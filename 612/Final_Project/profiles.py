# ============================================================
# Swap-friendly run profiles for test.py
#
# Usage:
#   python test.py                      # profiles.ACTIVE
#   python test.py baseline_safe
#   python test.py als_stronger
#   python test.py tune_small           # safe sample grid + final fit
#   python test.py tune_small --retune  # redo grid even if JSON exists
#   python test.py --help
# ============================================================

from __future__ import annotations

from copy import deepcopy
from typing import Any

# Default when you just run `python test.py` with no args.
ACTIVE = "baseline_safe"

# Shared Spark knobs (memory-safe on ~30 GB Windows) for fixed single-model runs.
_SPARK_SAFE = {
    "master": "local[4]",
    "driver_memory": "6g",
    "max_result_size": "1g",
    "shuffle_partitions": 8,
    "default_parallelism": 8,
    "memory_fraction": 0.5,
    "storage_fraction": 0.2,
}

# Tune profile: leave cores for Windows UI; smaller peaks.
_SPARK_TUNE = {
    "master": "local[2]",
    "driver_memory": "5g",
    "max_result_size": "1g",
    "shuffle_partitions": 4,
    "default_parallelism": 4,
    "memory_fraction": 0.5,
    "storage_fraction": 0.2,
}

_ALS_COMMON = {
    "num_user_blocks": 8,
    "num_item_blocks": 8,
    "seed": 50,
    "cold_start_strategy": "drop",
    "nonnegative": False,
    "implicit_prefs": False,
}

PROFILES: dict[str, dict[str, Any]] = {
    # Proven 2026-07-16: full ALS, RMSE 0.8092.
    "baseline_safe": {
        "description": "Known-good fixed ALS (rank=10, maxIter=5). No grid.",
        "spark": dict(_SPARK_SAFE),
        "safety": {"min_free_ram_gb": 3.5},
        "als": {
            "enabled": True,
            "rank": 10,
            "max_iter": 5,
            "reg_param": 0.10,
            **_ALS_COMMON,
        },
        "tune": {"enabled": False},
    },
    "prep_only": {
        "description": "Load/clean/split/cold-start only – no ALS fit.",
        "spark": dict(_SPARK_SAFE),
        "safety": {"min_free_ram_gb": 3.5},
        "als": {
            "enabled": False,
            "rank": 10,
            "max_iter": 5,
            "reg_param": 0.10,
            **_ALS_COMMON,
        },
        "tune": {"enabled": False},
    },
    # Proven 2026-07-16: RMSE 0.7998.
    "als_stronger": {
        "description": "Fixed stronger ALS (rank=20, maxIter=10). No grid.",
        "spark": dict(_SPARK_SAFE),
        "safety": {"min_free_ram_gb": 4.0},
        "als": {
            "enabled": True,
            "rank": 20,
            "max_iter": 10,
            "reg_param": 0.10,
            **_ALS_COMMON,
        },
        "tune": {"enabled": False},
    },
    # Safe grid: ~10% users sample, 6 combos, manual loop, local[2], then
    # ONE final fit on full train. Checkpoints after each combo.
    "tune_small": {
        "description": (
            "Safe hyperparam search: ~10% users, 6 combos, local[2], "
            "manual grid (no TVS auto-refit). Final model on full train."
        ),
        "spark": dict(_SPARK_TUNE),
        "safety": {"min_free_ram_gb": 5.0},
        # Fallback only; real final params come from grid JSON.
        "als": {
            "enabled": True,
            "rank": 20,
            "max_iter": 10,
            "reg_param": 0.10,
            **_ALS_COMMON,
        },
        "tune": {
            "enabled": True,
            # Keep all ratings for users where hash(userId) % mod == 0  (~1/mod)
            "user_sample_mod": 10,
            # Within the sample: train vs validation for scoring each combo
            "val_fraction": 0.2,
            "seed": 50,
            "grid": {
                "rank": [10, 20],
                "reg_param": [0.05, 0.1, 0.15],
                "max_iter": [10],
            },
        },
    },
}


def list_profiles() -> list[str]:
    return sorted(PROFILES.keys())


def get_profile(name: str | None = None) -> tuple[str, dict[str, Any]]:
    """Return (profile_name, deep-copied profile dict)."""
    key = (name or ACTIVE).strip()
    if key not in PROFILES:
        known = ", ".join(list_profiles())
        raise KeyError(f"Unknown profile {key!r}. Choose one of: {known}")
    return key, deepcopy(PROFILES[key])


def parse_cli(argv: list[str] | None = None) -> tuple[str, bool]:
    """
    Returns (profile_name, force_retune).

      python test.py
      python test.py tune_small
      python test.py tune_small --retune
      python test.py --profile als_stronger
    """
    import sys

    args = list(sys.argv[1:] if argv is None else argv)
    profile = ACTIVE
    retune = False

    if not args:
        return profile, retune

    i = 0
    while i < len(args):
        a = args[i]
        if a in ("-h", "--help"):
            names = ", ".join(list_profiles())
            print("Usage: python test.py [profile] [--retune]")
            print(f"Profiles: {names}")
            print(f"Default (ACTIVE): {ACTIVE}")
            print("  --retune   redo grid search even if results JSON exists")
            raise SystemExit(0)
        if a in ("--profile", "-p"):
            if i + 1 >= len(args):
                raise SystemExit("Missing profile name after --profile / -p")
            profile = args[i + 1]
            i += 2
            continue
        if a == "--retune":
            retune = True
            i += 1
            continue
        if a == "--resplit":
            raise SystemExit(
                "--resplit was removed (no parquet splits). "
                "Seeded F.rand(seed=50) is used each run."
            )
        if a.startswith("-"):
            raise SystemExit(f"Unknown flag: {a} (try --help)")
        profile = a
        i += 1

    return profile, retune


def resolve_profile_name(argv: list[str] | None = None) -> str:
    """Back-compat: profile name only."""
    return parse_cli(argv)[0]
