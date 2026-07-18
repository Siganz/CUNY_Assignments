#!/usr/bin/env python3
"""
Project 6 – MovieLens Recommender on AWS
"""

import os
import sys
import time
import warnings
import boto3
import pandas as pd
from sklearn.model_selection import train_test_split

warnings.filterwarnings("ignore")

# ────────────────────────────────────────────────
# 1. CONFIGURATION  ← change these two lines
# ────────────────────────────────────────────────

S3_BUCKET = "shawn-ganz-s3-bucket"
S3_PREFIX = "data/"

# ────────────────────────────────────────────────
# 2. Environment for Spark on EC2
# ────────────────────────────────────────────────

os.environ["SPARK_LOCAL_HOSTNAME"] = "localhost"
os.environ["SPARK_LOCAL_IP"] = "127.0.0.1"
os.environ["PYSPARK_PYTHON"] = sys.executable
os.environ["PYSPARK_DRIVER_PYTHON"] = sys.executable


# ────────────────────────────────────────────────
# 3. S3 setup
# ────────────────────────────────────────────────

s3 = boto3.client("s3")

def load_parquet(name: str) -> pd.DataFrame:
    local_path = f"/tmp/{name}"

    s3.download_file(
        S3_BUCKET,
        f"{S3_PREFIX}{name}",
        local_path
    )

    return pd.read_parquet(local_path)

# ────────────────────────────────────────────────
# 4. Load data from S3
# ────────────────────────────────────────────────

movies_sample = load_parquet("movies_sample.parquet")
ratings_sample = load_parquet("ratings_sample.parquet")
links_sample = load_parquet("links_sample.parquet")

print(f"  movies : {len(movies_sample):,} rows")
print(f"  ratings: {len(ratings_sample):,} rows")
print(f"  links  : {len(links_sample):,} rows")

# ────────────────────────────────────────────────
# 5. Quick dataset summary
# ────────────────────────────────────────────────

n_ratings = len(ratings_sample)
n_users   = ratings_sample["userId"].nunique()
n_movies  = ratings_sample["movieId"].nunique()
density   = n_ratings / (n_users * n_movies)

print("\n→ Dataset summary")
print(f"  Ratings          : {n_ratings:,}")
print(f"  Users            : {n_users:,}")
print(f"  Movies           : {n_movies:,}")
print(f"  Matrix density   : {density:.6f}")

# ────────────────────────────────────────────────
# 6. Train / Test split (same as notebook)
# ────────────────────────────────────────────────

train_pd, test_pd = train_test_split(
    ratings_sample, test_size=0.2, random_state=42
)
print(f"\n→ Split: {len(train_pd):,} train  |  {len(test_pd):,} test")

# ────────────────────────────────────────────────
# 7. Baseline: Surprise SVD
# ────────────────────────────────────────────────

from surprise import Dataset, Reader, SVD, accuracy

reader = Reader(rating_scale=(0.5, 5.0))
train_data = Dataset.load_from_df(
    train_pd[["userId", "movieId", "rating"]], reader
)
trainset = train_data.build_full_trainset()
testset  = list(test_pd[["userId", "movieId", "rating"]].itertuples(index=False, name=None))

svd = SVD(
    n_factors=20,
    n_epochs=20,
    lr_all=0.01,
    reg_all=0.05,
    random_state=42
)

start = time.time()
svd.fit(trainset)
svd_predictions = svd.test(testset)
svd_runtime = time.time() - start
svd_rmse = accuracy.rmse(svd_predictions, verbose=False)

print(f"  Surprise SVD  RMSE = {svd_rmse:.4f}   runtime = {svd_runtime:.2f}s")

# ────────────────────────────────────────────────
# 8. Spark ALS
# ────────────────────────────────────────────────
print("\n→ Starting Spark and training ALS...")
from pyspark.sql import SparkSession
from pyspark.sql.functions import col, explode
from pyspark.ml.recommendation import ALS
from pyspark.ml.evaluation import RegressionEvaluator

spark = (
    SparkSession.builder
    .appName("project")
    .master("local[2]")
    .config("spark.driver.host", "127.0.0.1")
    .config("spark.driver.bindAddress", "127.0.0.1")
    .config("spark.sql.shuffle.partitions", "4")
    .config("spark.executorEnv.PYSPARK_PYTHON", sys.executable)
    .getOrCreate()
)
spark.sparkContext.setLogLevel("ERROR")

train_spark = spark.createDataFrame(train_pd).select(
    col("userId").cast("int"),
    col("movieId").cast("int"),
    col("rating").cast("float")
).cache()

test_spark = spark.createDataFrame(test_pd).select(
    col("userId").cast("int"),
    col("movieId").cast("int"),
    col("rating").cast("float")
).cache()

als = ALS(
    userCol="userId",
    itemCol="movieId",
    ratingCol="rating",
    rank=20,
    maxIter=10,
    regParam=0.1,
    coldStartStrategy="drop",
    nonnegative=True,
    seed=42
)

evaluator = RegressionEvaluator(
    metricName="rmse", labelCol="rating", predictionCol="prediction"
)

start = time.time()
als_model = als.fit(train_spark)
als_predictions = als_model.transform(test_spark)
als_rmse = evaluator.evaluate(als_predictions)
als_runtime = time.time() - start

print(f"  Spark ALS     RMSE = {als_rmse:.4f}   runtime = {als_runtime:.2f}s")

# ────────────────────────────────────────────────
# 9. Comparison table
# ────────────────────────────────────────────────
print("\n" + "=" * 70)
print("RESULTS COMPARISON")
print("=" * 70)
print(f"{'Model':<20} {'RMSE':>10} {'Runtime (s)':>12}")
print("-" * 44)
print(f"{'Surprise SVD':<20} {svd_rmse:>10.4f} {svd_runtime:>12.2f}")
print(f"{'Spark ALS':<20} {als_rmse:>10.4f} {als_runtime:>12.2f}")
print("=" * 70)

# ────────────────────────────────────────────────
# 10. Sample recommendations (Spark ALS)
# ────────────────────────────────────────────────
print("\n→ Generating sample recommendations (Spark ALS)...")
users_to_show = [3, 10, 16]

user_recs = als_model.recommendForAllUsers(5)
recs_exploded = (
    user_recs
    .select("userId", explode("recommendations").alias("rec"))
    .select(
        col("userId"),
        col("rec.movieId").alias("movieId"),
        col("rec.rating").alias("predicted_rating")
    )
)

movies_spark = spark.createDataFrame(movies_sample).select(
    col("movieId").cast("int"),
    col("title"),
    col("genres")
)

sample_recs = (
    recs_exploded
    .join(movies_spark, on="movieId", how="left")
    .filter(col("userId").isin(users_to_show))
    .orderBy("userId", col("predicted_rating").desc())
)

print("\nTop-5 recommendations for users 3, 10, 16:")
sample_recs.show(15, truncate=False)

results = pd.DataFrame({
    "Model": ["Surprise SVD", "Spark ALS"],
    "RMSE": [svd_rmse, als_rmse],
    "Runtime_Seconds": [svd_runtime, als_runtime]
})

# ────────────────────────────────────────────────
# 11. Export
# ────────────────────────────────────────────────

results.to_csv("/tmp/model_results.csv", index=False)

s3.upload_file(
    "/tmp/model_results.csv",
    S3_BUCKET,
    "results/model_results.csv"
)

# ────────────────────────────────────────────────
# 12. Clean up
# ────────────────────────────────────────────────
spark.stop()
print("\n✓ Done. Spark session stopped.")