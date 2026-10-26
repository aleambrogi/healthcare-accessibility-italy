"""Audit the included municipal aggregates; no raw routing service is required."""
from pathlib import Path
import json
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
def main():
    data = ROOT / "data/processed"
    out = ROOT / "results/generated"
    out.mkdir(parents=True, exist_ok=True)
    summary = {}
    for level, code in (("comuni", "PRO_COM"), ("province", "COD_PROV"), ("regioni", "COD_REG")):
        frame = pd.read_csv(data/f"stress_{level}.csv")
        if frame[code].isna().any() or frame[code].duplicated().any():
            raise ValueError(f"Missing/duplicate identifiers: {level}")
        scores = frame[["total_index_avg", "stress_SSN_avg", "mobility_index_avg"]]
        values = scores.to_numpy(dtype=float)
        if np.isinf(values).any():
            raise ValueError(f"Infinite scores: {level}")
        summary[level] = {"rows": len(frame), "missing_scores": scores.isna().sum().to_dict(),
                          "unweighted_means": scores.mean().to_dict(),
                          "minima": scores.min().to_dict(), "maxima": scores.max().to_dict()}
        scores.describe().to_csv(out/f"descriptive_{level}.csv")
    (out/"data_audit.json").write_text(json.dumps(summary, indent=2)+"\n")
    print(json.dumps(summary, indent=2))

if __name__ == "__main__":
    main()
