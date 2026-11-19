#!/usr/bin/env python3
"""EPI-BONE: extract the daily Kaplan-Meier curves of the primary burden comparison (TriNetX analysis A01,
epilepsy vs migraine, days 1-90) from the stored export archive into one tidy CSV for Fig. 2a.

Input : A01_EPI_vs_MIG_fracture_early_90d_896749.zip (aggregate TriNetX export, unchanged; not in this repository)
Output: figures/data/F2A_km_days1_90.csv  (outcome, day, survival and 95% CI per cohort; nothing is recomputed)
"""
import csv
import io
import re
import sys
import zipfile
from pathlib import Path

import os

HERE = Path(__file__).resolve().parent
# The raw TriNetX export archive is not part of this repository (see README). Point EPIBONE_TRINETX_EXPORTS to the
# folder that holds it to repeat the extraction; the extracted table data/F2A_km_days1_90.csv is included.
ZIP = Path(os.environ.get("EPIBONE_TRINETX_EXPORTS", ".")) / "A01_EPI_vs_MIG_fracture_early_90d_896749.zip"
OUT = HERE.parent / "data/F2A_km_days1_90.csv"
OUTCOMES = {1: "Major osteoporotic fracture", 2: "Hip or proximal femur fracture", 8: "Falls", 9: "Head or face injury"}


def main():
    z = zipfile.ZipFile(ZIP)
    readme = z.read([n for n in z.namelist() if "Read_Me" in n][0]).decode("utf-8-sig")
    names = dict(re.findall(r"Outcome (\d+) Name: (.+)", readme))
    rows = []
    for k, label in OUTCOMES.items():
        text = z.read(f"Outcome_{k}_Result_b_KM_graph.csv").decode("utf-8-sig")
        lines = list(csv.reader(io.StringIO(text)))
        head = next(r for r in lines if r and r[0].startswith("Time"))
        assert [h.strip() for h in head[:7]] == [
            "Time (Days)", "Cohort 1: Survival Probability", "Cohort 1: Survival Probability 95 % CI Lower",
            "Cohort 1: Survival Probability 95 % CI Upper", "Cohort 2: Survival Probability",
            "Cohort 2: Survival Probability 95 % CI Lower", "Cohort 2: Survival Probability 95 % CI Upper"], head
        for r in lines:
            if r and re.match(r"^\d+(\.\d+)?$", r[0]) and 1 <= float(r[0]) <= 90:
                rows.append([label, names[str(k)].strip(), int(float(r[0]))] + [r[i] for i in range(1, 7)])
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["outcome", "trinetx_outcome_name", "day", "epilepsy_survival", "epilepsy_ci_low", "epilepsy_ci_high",
                    "migraine_survival", "migraine_ci_low", "migraine_ci_high"])
        w.writerows(rows)
    print("wrote", OUT, len(rows), "rows")


if __name__ == "__main__":
    sys.exit(main())
