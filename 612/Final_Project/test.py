# ============================================================
# Final Project – Spark ALS + Tag Genome Recommender
# Memory-safe pipeline for ~32M ratings on ~30 GB RAM machines
#
# Swap profiles (see profiles.py):
#   python test.py                      # uses profiles.ACTIVE
#   python test.py baseline_safe        # known-good fixed ALS
#   python test.py als_stronger         # fixed stronger ALS
#   python test.py tune_small           # sample grid + final fit (safe)
#   python test.py tune_small --retune  # redo grid even if JSON exists
#
# Checkpoints:
#   data/results/als_grid_results.json   per-combo + best + test RMSE
#
# Crash forensics:
#   data/logs/spark_run.log
#   data/logs/spark_last_stage.txt
# ============================================================

from __future__ import annotations

import atexit
import gc
import json
import os
import sys
import traceback
import warnings
from datetime import datetime
from pathlib import Path
from typing import Any

# Profile resolve before heavy imports so `python test.py --help` is instant.
from profiles import get_profile, parse_cli

PROFILE_NAME, FORCE_RETUNE = parse_cli()
PROFILE_NAME, CFG = get_profile(PROFILE_NAME)
SPARK_CFG = CFG["spark"]
ALS_CFG = CFG["als"]
TUNE_CFG = CFG.get("tune") or {"enabled": False}
MIN_FREE_RAM_GB = float(CFG["safety"]["min_free_ram_gb"])
RUN_ALS = bool(ALS_CFG["enabled"])
RUN_TUNE = bool(TUNE_CFG.get("enabled", False))

import psutil
from pyspark import StorageLevel
from pyspark.ml.evaluation import RegressionEvaluator
from pyspark.ml.recommendation import ALS
from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql.types import (
    StructType,
    StructField,
    IntegerType,
    LongType,
    FloatType,
    StringType,
)

warnings.filterwarnings("ignore")

# ------------------------------------------------------------
# Paths / logging
# ------------------------------------------------------------
DATA_DIR = Path(r"C:\Users\ganzs\Documents\CUNY_Assignments\612\Final_Project\data")
LOG_DIR = DATA_DIR / "logs"
SPARK_TMP_DIR = DATA_DIR / "spark_tmp"
RESULTS_DIR = DATA_DIR / "results"
GRID_RESULTS_PATH = RESULTS_DIR / "als_grid_results.json"

for _d in (LOG_DIR, SPARK_TMP_DIR, RESULTS_DIR):
    _d.mkdir(exist_ok=True)

LOG_PATH = LOG_DIR / "spark_run.log"
LAST_STAGE_PATH = LOG_DIR / "spark_last_stage.txt"
GB = 1024 ** 3
SPLIT_SEED = 50
SPLIT_TRAIN_FRACTION = 0.80


def _ts() -> str:
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def log(msg: str) -> None:
    """Print + append to spark_run.log with immediate flush (survives hard kills better)."""
    line = f"[{_ts()}] {msg}"
    print(line, flush=True)
    try:
        with open(LOG_PATH, "a", encoding="utf-8") as fh:
            fh.write(line + "\n")
            fh.flush()
            os.fsync(fh.fileno())
    except OSError as e:
        print(f"[log write failed] {e}", flush=True)


def set_stage(name: str) -> None:
    """Mark the stage that is about to run. If the PC freezes, this is the smoking gun."""
    try:
        LAST_STAGE_PATH.write_text(f"{_ts()} | STARTED: {name}\n", encoding="utf-8")
    except OSError:
        pass
    log(f"=== STAGE: {name} ===")


def finish_stage(name: str) -> None:
    try:
        LAST_STAGE_PATH.write_text(f"{_ts()} | FINISHED: {name}\n", encoding="utf-8")
    except OSError:
        pass
    log(f"=== DONE : {name} ===")


def mem_report(label: str) -> None:
    process = psutil.Process(os.getpid())
    rss = process.memory_info().rss / GB
    vm = psutil.virtual_memory()
    log(
        f"[mem] {label}: process={rss:.2f} GB | "
        f"system used={vm.used / GB:.2f}/{vm.total / GB:.2f} GB "
        f"({vm.percent:.0f}%) available={vm.available / GB:.2f} GB"
    )


def jvm_heap_report(spark: SparkSession, label: str) -> None:
    try:
        runtime = spark._jvm.java.lang.Runtime.getRuntime()
        max_heap = runtime.maxMemory() / GB
        total_heap = runtime.totalMemory() / GB
        free_heap = runtime.freeMemory() / GB
        used_heap = total_heap - free_heap
        log(
            f"[jvm] {label}: max={max_heap:.2f} GB total={total_heap:.2f} GB "
            f"used={used_heap:.2f} GB free_in_heap={free_heap:.2f} GB"
        )
    except Exception as e:
        log(f"[jvm] {label}: unavailable ({e})")


def guard_ram(label: str) -> None:
    """Soft-abort if free RAM is too low – avoids Windows hard freezes."""
    free = psutil.virtual_memory().available / GB
    if free < MIN_FREE_RAM_GB:
        raise MemoryError(
            f"Aborting before '{label}': only {free:.2f} GB free RAM "
            f"(threshold={MIN_FREE_RAM_GB} GB). Close browsers/apps and retry."
        )


def _combo_key(rank: int, reg: float, max_iter: int) -> tuple[int, float, int]:
    return (int(rank), round(float(reg), 6), int(max_iter))


def expand_grid(grid: dict[str, Any]) -> list[dict[str, Any]]:
    combos = []
    for rank in grid["rank"]:
        for reg in grid["reg_param"]:
            for max_iter in grid["max_iter"]:
                combos.append({
                    "rank": int(rank),
                    "regParam": float(reg),
                    "maxIter": int(max_iter),
                })
    return combos


def grid_results_valid(path: Path, expected: list[dict[str, Any]] | None = None) -> bool:
    """True if JSON has best_params and (optionally) every expected combo scored."""
    if not path.is_file() or path.stat().st_size < 20:
        return False
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
        best = payload.get("best_params") or {}
        if not all(k in best for k in ("rank", "maxIter", "regParam")):
            return False
        if expected is None:
            return True
        done = {
            _combo_key(r["rank"], r["regParam"], r["maxIter"])
            for r in (payload.get("all_results") or [])
            if "validation_rmse" in r
        }
        need = {_combo_key(c["rank"], c["regParam"], c["maxIter"]) for c in expected}
        return need <= done
    except (OSError, json.JSONDecodeError, TypeError, KeyError):
        return False


def load_grid_payload(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def save_grid_payload(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(exist_ok=True)
    tmp = path.with_suffix(".json.tmp")
    tmp.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    tmp.replace(path)


def refresh_best_params(payload: dict[str, Any]) -> None:
    results = [
        r for r in (payload.get("all_results") or [])
        if r.get("validation_rmse") is not None
    ]
    results.sort(key=lambda x: float(x["validation_rmse"]))
    payload["all_results"] = results
    if results:
        best = results[0]
        payload["best_params"] = {
            "rank": int(best["rank"]),
            "maxIter": int(best["maxIter"]),
            "regParam": float(best["regParam"]),
            "validation_rmse": float(best["validation_rmse"]),
        }


def make_als(rank: int, max_iter: int, reg_param: float) -> ALS:
    return ALS(
        userCol="userId",
        itemCol="movieId",
        ratingCol="rating",
        rank=int(rank),
        maxIter=int(max_iter),
        regParam=float(reg_param),
        implicitPrefs=bool(ALS_CFG["implicit_prefs"]),
        coldStartStrategy=str(ALS_CFG["cold_start_strategy"]),
        nonnegative=bool(ALS_CFG["nonnegative"]),
        numUserBlocks=int(ALS_CFG["num_user_blocks"]),
        numItemBlocks=int(ALS_CFG["num_item_blocks"]),
        seed=int(ALS_CFG["seed"]),
    )


def clear_spark_caches(spark: SparkSession) -> None:
    """Clear DataFrame cache + any persistent RDDs (Spark 3 and 4 safe)."""
    try:
        spark.catalog.clearCache()
    except Exception as e:
        log(f"Warning: catalog.clearCache failed: {e}")

    # PySpark 4 removed sc.getPersistentRDDs(); use the JVM SparkContext.
    try:
        jsc = spark.sparkContext._jsc.sc()
        persistent = jsc.getPersistentRDDs()
        # Java Map[Integer, RDD]
        if persistent is not None:
            for _id in list(persistent.keySet()):
                try:
                    persistent.get(_id).unpersist(True)
                except Exception:
                    pass
            log(f"Unpersisted {persistent.size()} JVM RDD(s).")
    except Exception as e:
        log(f"Warning: JVM getPersistentRDDs failed: {e}")


def stop_spark_cleanly(spark: SparkSession | None) -> None:
    if spark is None:
        return
    log("Cleaning up Spark resources...")
    try:
        clear_spark_caches(spark)
    except Exception as e:
        log(f"Warning while clearing cache: {e}")
    try:
        spark.stop()
        log("Spark session stopped.")
    except Exception as e:
        log(f"Warning while stopping Spark: {e}")
    gc.collect()
    mem_report("after cleanup")
    log("Cleanup complete.")


# Fresh log each run
try:
    LOG_PATH.write_text(f"=== spark_run.log started {_ts()} ===\n", encoding="utf-8")
except OSError:
    pass

# ------------------------------------------------------------
# Spark environment (important on Windows)
# ------------------------------------------------------------
os.environ["SPARK_LOCAL_HOSTNAME"] = "localhost"
os.environ["SPARK_LOCAL_IP"] = "127.0.0.1"
os.environ["PYSPARK_PYTHON"] = sys.executable
os.environ["PYSPARK_DRIVER_PYTHON"] = sys.executable

# Spark launch knobs come from the active profile.
os.environ["PYSPARK_SUBMIT_ARGS"] = (
    f"--master {SPARK_CFG['master']} "
    f"--driver-memory {SPARK_CFG['driver_memory']} "
    f"--conf spark.driver.maxResultSize={SPARK_CFG['max_result_size']} "
    "pyspark-shell"
)

log(f"Python executable: {sys.executable}")
log(f"Python version  : {sys.version.split()[0]}")
log(f"Profile         : {PROFILE_NAME} — {CFG.get('description', '')}")
log(
    f"Profile spark   : master={SPARK_CFG['master']} "
    f"driver={SPARK_CFG['driver_memory']} "
    f"shuffle={SPARK_CFG['shuffle_partitions']} "
    f"parallelism={SPARK_CFG['default_parallelism']}"
)
log(
    f"Profile ALS     : enabled={RUN_ALS} rank={ALS_CFG['rank']} "
    f"maxIter={ALS_CFG['max_iter']} regParam={ALS_CFG['reg_param']} "
    f"blocks={ALS_CFG['num_user_blocks']}/{ALS_CFG['num_item_blocks']}"
)
log(f"Tune            : enabled={RUN_TUNE} force_retune={FORCE_RETUNE}")
if RUN_TUNE:
    g = TUNE_CFG.get("grid", {})
    n_combo = len(expand_grid(g)) if g else 0
    log(
        f"Tune grid       : ranks={g.get('rank')} reg={g.get('reg_param')} "
        f"maxIter={g.get('max_iter')} ({n_combo} combos) "
        f"user_sample_mod={TUNE_CFG.get('user_sample_mod')} "
        f"val_fraction={TUNE_CFG.get('val_fraction')} "
        f"(manual loop, no TVS)"
    )
    log(f"Grid results    : {GRID_RESULTS_PATH}")
log(f"Min free RAM    : {MIN_FREE_RAM_GB} GB (soft abort threshold)")
log(f"Log file        : {LOG_PATH}")
log(f"Last-stage file : {LAST_STAGE_PATH}")
mem_report("startup")

spark: SparkSession | None = None


def _atexit_cleanup() -> None:
    # Safety net if process exits without hitting finally
    global spark
    if spark is not None:
        try:
            spark.stop()
        except Exception:
            pass
        spark = None


atexit.register(_atexit_cleanup)

try:
    set_stage("0_cleanup_leftover_session")
    existing = SparkSession.getActiveSession()
    if existing is not None:
        log("Found an existing Spark session – clearing cache and stopping it...")
        stop_spark_cleanly(existing)
        log("Leftover Spark session cleaned up.")
    else:
        log("No existing Spark session found – starting clean.")
    finish_stage("0_cleanup_leftover_session")

    set_stage("1_create_spark_session")
    guard_ram("create Spark session")
    spark = (
        SparkSession.builder
        .appName(f"MovieLens_ALS_Final[{PROFILE_NAME}]")
        .config("spark.driver.host", "127.0.0.1")
        .config("spark.driver.bindAddress", "127.0.0.1")
        .config("spark.sql.shuffle.partitions", str(SPARK_CFG["shuffle_partitions"]))
        .config("spark.default.parallelism", str(SPARK_CFG["default_parallelism"]))
        .config("spark.python.worker.reuse", "true")
        .config("spark.memory.fraction", str(SPARK_CFG["memory_fraction"]))
        .config("spark.memory.storageFraction", str(SPARK_CFG["storage_fraction"]))
        .config("spark.sql.adaptive.enabled", "true")
        .config("spark.sql.adaptive.coalescePartitions.enabled", "true")
        .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
        .config("spark.driver.maxResultSize", SPARK_CFG["max_result_size"])
        .config("spark.shuffle.spill.compress", "true")
        .config("spark.rdd.compress", "true")
        .config("spark.local.dir", str(SPARK_TMP_DIR))
        .getOrCreate()
    )
    SPARK_TMP_DIR.mkdir(exist_ok=True)
    spark.sparkContext.setLogLevel("ERROR")
    clear_spark_caches(spark)

    log(f"Spark version   : {spark.version}")
    log(f"Spark UI        : {spark.sparkContext.uiWebUrl}")
    spark.createDataFrame([(1, "a"), (2, "b")], ["id", "val"]).show()
    max_heap_gb = spark._jvm.java.lang.Runtime.getRuntime().maxMemory() / GB
    log(f"JVM maximum heap: {max_heap_gb:.2f} GB")
    mem_report("after Spark start")
    jvm_heap_report(spark, "after Spark start")
    finish_stage("1_create_spark_session")

    # ------------------------------------------------------------
    # Paths + schemas
    # ------------------------------------------------------------
    set_stage("2_load_csvs")
    csv_files = {
        "movies": DATA_DIR / "movies.csv",
        "ratings": DATA_DIR / "ratings.csv",
        "links": DATA_DIR / "links.csv",
    }
    for name, path in csv_files.items():
        if not path.exists():
            raise FileNotFoundError(f"Missing {name} file: {path}")
        log(f"{name:<8}: {path}")

    ratings_schema = StructType([
        StructField("userId", IntegerType(), nullable=False),
        StructField("movieId", IntegerType(), nullable=False),
        StructField("rating", FloatType(), nullable=False),
        StructField("timestamp", LongType(), nullable=False),
    ])
    movies_schema = StructType([
        StructField("movieId", IntegerType(), nullable=False),
        StructField("title", StringType(), nullable=False),
        StructField("genres", StringType(), nullable=True),
    ])
    links_schema = StructType([
        StructField("movieId", IntegerType(), nullable=False),
        StructField("imdbId", StringType(), nullable=True),
        StructField("tmdbId", IntegerType(), nullable=True),
    ])

    ratings_df = (
        spark.read.option("header", True).schema(ratings_schema)
        .csv(str(csv_files["ratings"]))
    )
    movies_df = (
        spark.read.option("header", True).schema(movies_schema)
        .csv(str(csv_files["movies"]))
    )
    links_df = (
        spark.read.option("header", True).schema(links_schema)
        .csv(str(csv_files["links"]))
    )

    n_movies = movies_df.count()
    n_links = links_df.count()
    log(f"Movies : {n_movies:,}")
    log(f"Links  : {n_links:,}")
    mem_report("after movies/links counts")

    ratings_df.printSchema()
    ratings_df.show(5, truncate=False)
    movies_df.printSchema()
    movies_df.show(5, truncate=False)
    links_df.printSchema()
    links_df.show(5, truncate=False)
    finish_stage("2_load_csvs")

    # ------------------------------------------------------------
    # Validate + clean ratings (single materialization)
    # ------------------------------------------------------------
    set_stage("3_validate_and_clean_ratings")
    guard_ram("validate/clean ratings")
    required_cols = ["userId", "movieId", "rating"]

    null_row = ratings_df.select([
        F.sum(F.col(c).isNull().cast("int")).alias(c) for c in required_cols
    ]).collect()[0]
    log(f"Null counts: userId={null_row['userId']}, movieId={null_row['movieId']}, rating={null_row['rating']}")

    stats = ratings_df.select(
        F.min("rating").alias("min_rating"),
        F.max("rating").alias("max_rating"),
        F.countDistinct("userId").alias("users"),
        F.countDistinct("movieId").alias("rated_movies"),
    ).collect()[0]
    log(
        f"Rating stats: min={stats['min_rating']} max={stats['max_rating']} "
        f"users={stats['users']:,} rated_movies={stats['rated_movies']:,}"
    )
    mem_report("after rating stats")

    # Drop timestamp early – not needed for ALS; saves ~25% row width
    ratings_clean = (
        ratings_df
        .select("userId", "movieId", "rating")
        .dropna()
        .dropDuplicates(["userId", "movieId"])
    )

    # Prefer disk spill over heap growth (safer on 30 GB Windows)
    ratings_clean = ratings_clean.persist(StorageLevel.MEMORY_AND_DISK)
    clean_pairs = ratings_clean.count()
    log(f"Rows after deduplication : {clean_pairs:,}")
    mem_report("after ratings_clean cache")
    jvm_heap_report(spark, "after ratings_clean cache")
    finish_stage("3_validate_and_clean_ratings")

    # ------------------------------------------------------------
    # Activity stats (cheap once ratings_clean is cached)
    # ------------------------------------------------------------
    set_stage("4_activity_percentiles")
    guard_ram("activity percentiles")

    user_activity = ratings_clean.groupBy("userId").agg(F.count("*").alias("rating_count"))
    u = user_activity.select(
        F.min("rating_count").alias("min"),
        F.expr("percentile_approx(rating_count, 0.25)").alias("p25"),
        F.expr("percentile_approx(rating_count, 0.50)").alias("median"),
        F.expr("percentile_approx(rating_count, 0.75)").alias("p75"),
        F.expr("percentile_approx(rating_count, 0.95)").alias("p95"),
        F.max("rating_count").alias("max"),
    ).collect()[0]
    log(
        f"User activity: min={u['min']} p25={u['p25']} median={u['median']} "
        f"p75={u['p75']} p95={u['p95']} max={u['max']}"
    )

    movie_activity = ratings_clean.groupBy("movieId").agg(F.count("*").alias("rating_count"))
    m = movie_activity.select(
        F.min("rating_count").alias("min"),
        F.expr("percentile_approx(rating_count, 0.25)").alias("p25"),
        F.expr("percentile_approx(rating_count, 0.50)").alias("median"),
        F.expr("percentile_approx(rating_count, 0.75)").alias("p75"),
        F.expr("percentile_approx(rating_count, 0.95)").alias("p95"),
        F.max("rating_count").alias("max"),
    ).collect()[0]
    log(
        f"Movie activity: min={m['min']} p25={m['p25']} median={m['median']} "
        f"p75={m['p75']} p95={m['p95']} max={m['max']}"
    )
    mem_report("after activity percentiles")
    finish_stage("4_activity_percentiles")

    # ------------------------------------------------------------
    # Train/test split – seeded single materialization (no parquet)
    # ------------------------------------------------------------
    set_stage("5_train_test_split_cache")
    guard_ram("train/test split")

    ratings_als = ratings_clean.withColumn(
        "_split",
        F.when(
            F.rand(seed=SPLIT_SEED) < SPLIT_TRAIN_FRACTION,
            F.lit("train"),
        ).otherwise(F.lit("test")),
    )
    ratings_als = ratings_als.persist(StorageLevel.MEMORY_AND_DISK)
    _ = ratings_als.count()
    mem_report("after ratings_als split cache")
    jvm_heap_report(spark, "after ratings_als split cache")

    log("Unpersisting ratings_clean (blocking)...")
    ratings_clean.unpersist(blocking=True)
    gc.collect()
    mem_report("after unpersist ratings_clean")
    finish_stage("5_train_test_split_cache")

    set_stage("6_train_test_counts")
    guard_ram("train/test counts")
    split_counts = {
        row["_split"]: int(row["n"])
        for row in (
            ratings_als.groupBy("_split")
            .count()
            .withColumnRenamed("count", "n")
            .collect()
        )
    }
    train_count = split_counts.get("train", 0)
    test_count = split_counts.get("test", 0)
    log(f"Training ratings: {train_count:,}")
    log(f"Testing ratings : {test_count:,}")
    log(f"Total ratings   : {train_count + test_count:,}")
    log(f"Split seed={SPLIT_SEED} train_fraction={SPLIT_TRAIN_FRACTION}")
    mem_report("after train/test counts")
    finish_stage("6_train_test_counts")

    train_df = ratings_als.filter(F.col("_split") == "train").drop("_split")
    test_df = ratings_als.filter(F.col("_split") == "test").drop("_split")

    # ------------------------------------------------------------
    # Cold-start diagnostics (LIGHT version)
    #
    # Old version did many distinct()+join()+count() passes over ~6–25M
    # rows and was the most likely freeze point.
    #
    # New version:
    #   1) distinct train movieIds only (~80k rows – tiny)
    #   2) ONE left-join of test against that set
    #   3) ONE groupBy for known vs cold test sizes
    #   4) cold users via a cheap left_anti on distinct IDs only
    # ------------------------------------------------------------
    set_stage("7_cold_start_train_movie_ids")
    guard_ram("cold-start train movie ids")

    train_movies = train_df.select("movieId").distinct()
    train_movies = train_movies.persist(StorageLevel.MEMORY_ONLY)
    n_train_movies = train_movies.count()
    log(f"Distinct train movies: {n_train_movies:,}")
    mem_report("after train_movies distinct")
    finish_stage("7_cold_start_train_movie_ids")

    set_stage("8_cold_start_test_movie_coverage")
    guard_ram("cold-start test coverage join")

    # Tag each test row: movie seen in train?  (single join + single groupBy)
    test_cov = (
        test_df
        .join(
            train_movies.withColumn("_seen", F.lit(1)),
            on="movieId",
            how="left",
        )
        .withColumn("is_cold_movie", F.col("_seen").isNull())
    )
    cov_rows = (
        test_cov.groupBy("is_cold_movie")
        .count()
        .withColumnRenamed("count", "n")
        .collect()
    )
    known_test_count = 0
    cold_test_count = 0
    for row in cov_rows:
        if row["is_cold_movie"]:
            cold_test_count = int(row["n"])
        else:
            known_test_count = int(row["n"])

    log(f"Evaluable test ratings : {known_test_count:,}")
    log(f"Cold-start test ratings: {cold_test_count:,}")
    if test_count:
        log(f"Evaluation coverage    : {known_test_count / test_count:.2%}")
    mem_report("after test coverage groupBy")
    finish_stage("8_cold_start_test_movie_coverage")

    set_stage("9_cold_start_user_movie_id_counts")
    guard_ram("cold-start id counts")

    # Distinct IDs only (small intermediate results, not full rating joins)
    train_users = train_df.select("userId").distinct()
    cold_users = (
        test_df.select("userId").distinct()
        .join(train_users, "userId", "left_anti")
        .count()
    )
    cold_movies = (
        test_df.select("movieId").distinct()
        .join(train_movies, "movieId", "left_anti")
        .count()
    )
    log(f"Cold-start users in test : {cold_users:,}")
    log(f"Cold-start movies in test: {cold_movies:,}")

    train_movies.unpersist(blocking=False)
    mem_report("after cold-start id counts")
    jvm_heap_report(spark, "after cold-start diagnostics")
    finish_stage("9_cold_start_user_movie_id_counts")

    coldStartStrategy = ALS_CFG["cold_start_strategy"]
    log(f"coldStartStrategy={coldStartStrategy!r} (for ALS when enabled)")
    mem_report("pipeline ready (ALS off)" if not RUN_ALS else "pipeline ready (ALS on)")

    # ------------------------------------------------------------
    # ALS: optional grid (train only) → final fit on full train → test
    # ------------------------------------------------------------
    if RUN_ALS:
        set_stage("10_als_persist_train_test")
        guard_ram("ALS persist train/test")

        train_df = train_df.persist(StorageLevel.MEMORY_AND_DISK)
        known_test_df = (
            test_df.join(
                train_df.select("movieId").distinct(),
                on="movieId",
                how="left_semi",
            ).persist(StorageLevel.MEMORY_AND_DISK)
        )
        log(f"train materialize: {train_df.count():,}")
        log(f"known_test materialize: {known_test_df.count():,}")
        log("Unpersisting ratings_als parent...")
        ratings_als.unpersist(blocking=True)
        gc.collect()
        mem_report("after ALS train/test persist")
        finish_stage("10_als_persist_train_test")

        # ---- Resolve hyperparameters (manual sample grid or fixed profile) ----
        best_rank = int(ALS_CFG["rank"])
        best_max_iter = int(ALS_CFG["max_iter"])
        best_reg = float(ALS_CFG["reg_param"])
        grid_payload: dict[str, Any] | None = None

        if RUN_TUNE:
            g = TUNE_CFG["grid"]
            expected_combos = expand_grid(g)
            need_grid = FORCE_RETUNE or not grid_results_valid(
                GRID_RESULTS_PATH, expected_combos
            )

            if need_grid:
                set_stage("11_manual_grid_on_user_sample")
                guard_ram("ALS sample grid")

                user_mod = int(TUNE_CFG.get("user_sample_mod", 10))
                val_frac = float(TUNE_CFG.get("val_fraction", 0.2))
                tune_seed = int(TUNE_CFG.get("seed", SPLIT_SEED))

                log(
                    f"Manual grid on ~1/{user_mod} of USERS (all their ratings). "
                    f"{len(expected_combos)} combos, local cores from profile. "
                    f"Test set is NOT used. Checkpoint after each combo."
                )
                mem_report("immediately before sample grid")
                jvm_heap_report(spark, "immediately before sample grid")

                # Deterministic user sample: keep full history for selected users
                tune_df = (
                    train_df
                    .filter(F.pmod(F.xxhash64("userId"), F.lit(user_mod)) == 0)
                    .persist(StorageLevel.MEMORY_AND_DISK)
                )
                n_tune = tune_df.count()
                n_tune_users = tune_df.select("userId").distinct().count()
                log(f"Tune sample ratings: {n_tune:,} | users: {n_tune_users:,}")

                # Hold out val rows inside the sample only
                tune_tagged = tune_df.withColumn(
                    "_tv",
                    F.when(
                        F.rand(seed=tune_seed) < (1.0 - val_frac),
                        F.lit("tr"),
                    ).otherwise(F.lit("va")),
                )
                tune_train = (
                    tune_tagged.filter(F.col("_tv") == "tr")
                    .drop("_tv")
                    .persist(StorageLevel.MEMORY_AND_DISK)
                )
                tune_val = (
                    tune_tagged.filter(F.col("_tv") == "va")
                    .drop("_tv")
                    .persist(StorageLevel.MEMORY_AND_DISK)
                )
                n_tr = tune_train.count()
                n_va = tune_val.count()
                log(f"Tune train/val split: train={n_tr:,} val={n_va:,}")

                # Movies seen in tune_train only (cold-start drop for val)
                tune_train_movies = (
                    tune_train.select("movieId").distinct()
                    .persist(StorageLevel.MEMORY_ONLY)
                )
                tune_val_known = (
                    tune_val.join(tune_train_movies, "movieId", "left_semi")
                    .persist(StorageLevel.MEMORY_AND_DISK)
                )
                n_va_known = tune_val_known.count()
                log(f"Tune val (known movies): {n_va_known:,}")

                evaluator_val = RegressionEvaluator(
                    metricName="rmse",
                    labelCol="rating",
                    predictionCol="prediction",
                )

                # Resume partial results if present (unless --retune)
                results: list[dict[str, Any]] = []
                if (
                    not FORCE_RETUNE
                    and GRID_RESULTS_PATH.is_file()
                    and GRID_RESULTS_PATH.stat().st_size > 20
                ):
                    try:
                        prev = load_grid_payload(GRID_RESULTS_PATH)
                        results = list(prev.get("all_results") or [])
                        log(f"Resuming grid with {len(results)} prior combo(s)")
                    except (OSError, json.JSONDecodeError, TypeError):
                        results = []

                done_keys = {
                    _combo_key(r["rank"], r["regParam"], r["maxIter"])
                    for r in results
                    if r.get("validation_rmse") is not None
                }

                grid_payload = {
                    "created_at": _ts(),
                    "profile": PROFILE_NAME,
                    "status": "in_progress",
                    "split": {
                        "seed": SPLIT_SEED,
                        "train_fraction": SPLIT_TRAIN_FRACTION,
                        "method": "F.rand(seed) row split (in-memory)",
                    },
                    "tune": {
                        "method": "manual_loop_user_sample",
                        "user_sample_mod": user_mod,
                        "val_fraction": val_frac,
                        "seed": tune_seed,
                        "tune_ratings": n_tune,
                        "tune_users": n_tune_users,
                        "tune_train_ratings": n_tr,
                        "tune_val_ratings": n_va,
                        "tune_val_known_ratings": n_va_known,
                        "grid": g,
                    },
                    "best_params": None,
                    "all_results": results,
                    "test_rmse": None,
                    "test_predictions": None,
                    "final_fit_at": None,
                }

                for i, combo in enumerate(expected_combos):
                    key = _combo_key(
                        combo["rank"], combo["regParam"], combo["maxIter"]
                    )
                    if key in done_keys:
                        log(
                            f"  grid[{i+1}/{len(expected_combos)}] SKIP "
                            f"rank={combo['rank']} maxIter={combo['maxIter']} "
                            f"regParam={combo['regParam']} (already scored)"
                        )
                        continue

                    set_stage(
                        f"11_grid_{i+1}_of_{len(expected_combos)}"
                        f"_r{combo['rank']}_reg{combo['regParam']}"
                    )
                    guard_ram(
                        f"grid combo {i+1}/{len(expected_combos)} "
                        f"rank={combo['rank']} reg={combo['regParam']}"
                    )
                    log(
                        f"  grid[{i+1}/{len(expected_combos)}] FIT "
                        f"rank={combo['rank']} maxIter={combo['maxIter']} "
                        f"regParam={combo['regParam']}"
                    )
                    mem_report(f"before grid combo {i+1}")

                    model_i = make_als(
                        combo["rank"], combo["maxIter"], combo["regParam"]
                    ).fit(tune_train)
                    preds_i = model_i.transform(tune_val_known)
                    val_rmse = float(evaluator_val.evaluate(preds_i))
                    log(
                        f"  grid[{i+1}/{len(expected_combos)}] val_RMSE={val_rmse:.4f}"
                    )

                    results.append({
                        "rank": combo["rank"],
                        "regParam": combo["regParam"],
                        "maxIter": combo["maxIter"],
                        "validation_rmse": val_rmse,
                        "finished_at": _ts(),
                    })
                    grid_payload["all_results"] = results
                    refresh_best_params(grid_payload)
                    save_grid_payload(GRID_RESULTS_PATH, grid_payload)
                    log(f"  checkpoint → {GRID_RESULTS_PATH}")

                    del model_i, preds_i
                    gc.collect()
                    mem_report(f"after grid combo {i+1}")
                    finish_stage(
                        f"11_grid_{i+1}_of_{len(expected_combos)}"
                        f"_r{combo['rank']}_reg{combo['regParam']}"
                    )

                refresh_best_params(grid_payload)
                grid_payload["status"] = "grid_complete"
                grid_payload["grid_finished_at"] = _ts()
                save_grid_payload(GRID_RESULTS_PATH, grid_payload)

                for i, row in enumerate(grid_payload["all_results"]):
                    mark = "  <-- best" if i == 0 else ""
                    log(
                        f"  grid final[{i}] rank={row['rank']} "
                        f"maxIter={row['maxIter']} regParam={row['regParam']} "
                        f"val_RMSE={row['validation_rmse']:.4f}{mark}"
                    )

                tune_val_known.unpersist(blocking=False)
                tune_train_movies.unpersist(blocking=False)
                tune_train.unpersist(blocking=True)
                tune_val.unpersist(blocking=True)
                tune_df.unpersist(blocking=True)
                gc.collect()
                mem_report("after sample grid complete")
                jvm_heap_report(spark, "after sample grid complete")
                finish_stage("11_manual_grid_on_user_sample")
            else:
                set_stage("11_load_grid_results")
                grid_payload = load_grid_payload(GRID_RESULTS_PATH)
                log(f"Loaded complete grid results from {GRID_RESULTS_PATH}")
                for i, row in enumerate(grid_payload.get("all_results") or []):
                    mark = "  <-- best" if i == 0 else ""
                    log(
                        f"  grid[{i}] rank={row['rank']} maxIter={row['maxIter']} "
                        f"regParam={row['regParam']} "
                        f"val_RMSE={row['validation_rmse']:.4f}{mark}"
                    )
                finish_stage("11_load_grid_results")

            best = grid_payload["best_params"]
            best_rank = int(best["rank"])
            best_max_iter = int(best["maxIter"])
            best_reg = float(best["regParam"])
            log(
                f"Using tuned params: rank={best_rank} maxIter={best_max_iter} "
                f"regParam={best_reg} "
                f"(val_RMSE={best.get('validation_rmse', 'n/a')})"
            )
        else:
            log(
                f"Fixed profile params (no grid): rank={best_rank} "
                f"maxIter={best_max_iter} regParam={best_reg}"
            )

        # ---- Final model: full train_df → evaluate on known test ----
        set_stage("12_final_als_fit")
        guard_ram("final ALS fit")
        log(
            f"FINAL ALS fit on FULL train: rank={best_rank} maxIter={best_max_iter} "
            f"regParam={best_reg} | profile={PROFILE_NAME} | "
            f"console may go quiet; watch spark_last_stage.txt"
        )
        mem_report("immediately before final ALS fit")
        jvm_heap_report(spark, "immediately before final ALS fit")
        final_als = make_als(best_rank, best_max_iter, best_reg)
        model = final_als.fit(train_df)
        log("Final ALS fit returned – model trained on full train_df.")
        mem_report("after final ALS fit")
        jvm_heap_report(spark, "after final ALS fit")
        finish_stage("12_final_als_fit")

        set_stage("13_final_als_evaluate")
        guard_ram("final ALS evaluate")
        predictions = model.transform(known_test_df)
        evaluator = RegressionEvaluator(
            metricName="rmse",
            labelCol="rating",
            predictionCol="prediction",
        )
        rmse = float(evaluator.evaluate(predictions))
        pred_n = int(predictions.count())
        log(f"Predictions evaluated: {pred_n:,}")
        log(f"Test RMSE            : {rmse:.4f}")
        log(f"Profile used         : {PROFILE_NAME}")
        log(
            f"Final params         : rank={best_rank} maxIter={best_max_iter} "
            f"regParam={best_reg}"
        )
        mem_report("after final ALS evaluate")
        finish_stage("13_final_als_evaluate")

        # Update results JSON with test metric when tuning
        if RUN_TUNE:
            if grid_payload is None and grid_results_valid(GRID_RESULTS_PATH):
                grid_payload = load_grid_payload(GRID_RESULTS_PATH)
            if grid_payload is not None:
                grid_payload["test_rmse"] = rmse
                grid_payload["test_predictions"] = pred_n
                grid_payload["final_fit_at"] = _ts()
                grid_payload["status"] = "complete"
                grid_payload["final_params"] = {
                    "rank": best_rank,
                    "maxIter": best_max_iter,
                    "regParam": best_reg,
                }
                save_grid_payload(GRID_RESULTS_PATH, grid_payload)
                log(f"Updated {GRID_RESULTS_PATH} with test_rmse={rmse:.4f}")
    else:
        log(
            f"ALS is DISABLED (profile={PROFILE_NAME}, als.enabled=False). "
            f"Pipeline stopped after cold-start diagnostics. "
            f"Try baseline_safe, als_stronger, or tune_small."
        )

    set_stage("99_success")
    log("Pipeline finished successfully.")
    finish_stage("99_success")

except MemoryError as e:
    log(f"SOFT ABORT (MemoryError): {e}")
    set_stage("FAILED_memory")
    raise
except Exception as e:
    log(f"FAILED with {type(e).__name__}: {e}")
    log(traceback.format_exc())
    try:
        LAST_STAGE_PATH.write_text(
            f"{_ts()} | FAILED during last stage – see spark_run.log\n",
            encoding="utf-8",
        )
    except OSError:
        pass
    raise
finally:
    stop_spark_cleanly(spark)
    spark = None
