#!/usr/bin/env python3
"""EPI-BONE: Benjamini-Hochberg q values for the two prespecified primary outcomes of the enzyme-inducer class
comparison (carbamazepine, phenytoin, or phenobarbital vs levetiracetam or lamotrigine).

The two primary outcomes are major osteoporotic fracture (R1) and head or face injury (R2). TriNetX returned the
risk-difference P value and the log-rank P value of each; the false discovery rate is controlled across the two
outcomes within each follow-up window and test. This script was written for this repository (19 November 2026) to
make that step checkable; it reads the aggregate table that also feeds Fig. 4a,b.

Usage: python trinetx/class_comparison/class_comparison_multiplicity.py
Input: figures/data/F5A_eic_class_by_outcome_window.csv
"""
import csv
from pathlib import Path

DATA = Path(__file__).resolve().parents[2] / "figures" / "data" / "F5A_eic_class_by_outcome_window.csv"
PRIMARY = {"R1": "Major osteoporotic fracture", "R2": "Head or face injury"}
WINDOWS = {"H1": "days 31-365", "H2": "days 31-730", "H3": "days 366-1825"}


def bh(pvalues):
    """Benjamini-Hochberg adjusted P values, in the input order."""
    m = len(pvalues)
    order = sorted(range(m), key=lambda i: pvalues[i])
    q = [None] * m
    running = 1.0
    for rank in range(m, 0, -1):
        i = order[rank - 1]
        running = min(running, pvalues[i] * m / rank)
        q[i] = running
    return q


def main():
    with open(DATA, encoding="utf-8-sig") as fh:
        rows = [r for r in csv.DictReader(fh) if r["outcome_key"] in PRIMARY and r["status"] == "estimable"]
    print(f"{'window':14s} {'test':16s} {'outcome':28s} {'P':>8s} {'BH q':>8s}")
    for h, label in WINDOWS.items():
        for column, test in (("rd_p", "risk difference"), ("logrank_p", "log-rank")):
            cell = [r for r in rows if r["horizon"] == h and r[column]]
            if len(cell) != len(PRIMARY):
                continue
            ps = [float(r[column]) for r in cell]
            for r, p, q in zip(cell, ps, bh(ps)):
                print(f"{label:14s} {test:16s} {PRIMARY[r['outcome_key']]:28s} {p:8.4f} {q:8.4f}")


if __name__ == "__main__":
    main()
