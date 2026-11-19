#!/usr/bin/env python3
"""Read-only reconciliation of archived TriNetX aggregate CSVs against saved live tables."""

from __future__ import annotations

import csv
import io
import json
import math
import os
import re
import zipfile
from pathlib import Path


PROJECT = Path(os.environ.get("EPIBONE_PROJECT_ROOT", "."))   # folder that holds 04_trinetx_exports/ and 05_results/
EXPORTS = PROJECT / "04_trinetx_exports"
RAW = EXPORTS / "raw_zips"
BURDEN = PROJECT / "05_results" / "burden_analysis_2026-08-23" / "EPIBONE_BURDEN_KEY_ESTIMATES_2026-08-23.csv"


def rows(path: Path):
    with path.open(newline="", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def numeric(value):
    if value is None:
        return None
    text = str(value).strip().replace("%", "").replace(",", "")
    if not text:
        return None
    if text.startswith("<"):
        return ("lt", float(text[1:]))
    try:
        return float(text)
    except ValueError:
        return None


def extract_pair(value):
    if not value:
        return None
    nums = re.findall(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?", str(value))
    return tuple(float(x) for x in nums[:2]) if len(nums) >= 2 else None


def section(table, label):
    for i, row in enumerate(table):
        if row and row[0].strip() == label:
            return i
    raise KeyError(label)


def extract_csv(zf, name):
    with zf.open(name) as f:
        return list(csv.reader(io.TextIOWrapper(f, encoding="utf-8-sig", newline="")))


def parse_moa(zf, index):
    table = extract_csv(zf, f"Outcome_{index}_Result_a_MOA_table.csv")
    i = section(table, "Cohort Statistics")
    c1, c2 = table[i + 2], table[i + 3]
    j = section(table, "Risk Difference")
    rd = table[j + 2]
    j = section(table, "Risk Ratio")
    rr = table[j + 2]
    j = section(table, "Odds Ratio")
    orow = table[j + 2]
    return {
        "n1": float(c1[2]), "events1": float(c1[3]), "risk1_pct": float(c1[4]) * 100,
        "n2": float(c2[2]), "events2": float(c2[3]), "risk2_pct": float(c2[4]) * 100,
        "rd_pct": float(rd[0]) * 100, "rd_lo_pct": float(rd[1]) * 100,
        "rd_hi_pct": float(rd[2]) * 100, "association_p": float(rd[4]),
        "rr": float(rr[0]), "rr_lo": float(rr[1]), "rr_hi": float(rr[2]),
        "or": float(orow[0]), "or_lo": float(orow[1]), "or_hi": float(orow[2]),
    }


def parse_km(zf, index):
    table = extract_csv(zf, f"Outcome_{index}_Result_b_KM_table.csv")
    i = section(table, "Cohort Statistics")
    c1, c2 = table[i + 2], table[i + 3]
    j = section(table, "Log-Rank Test")
    lr = table[j + 2]
    j = section(table, "Hazard Ratio")
    hr = table[j + 2]
    j = section(table, "Proportionality")
    ph = table[j + 2]
    return {
        "km_survival1_pct": float(c1[5]) * 100,
        "km_survival2_pct": float(c2[5]) * 100,
        "logrank_p": float(lr[2]), "hr": float(hr[0]), "hr_lo": float(hr[1]),
        "hr_hi": float(hr[2]), "proportionality_p": float(ph[2]),
    }


ALIASES = {
    "n1": ["n_at_risk_lcm", "n_lcm", "eligible_outcome_n1", "patients_1"],
    "events1": ["events_lcm", "events_n1", "events_1"],
    "risk1_pct": ["risk_lcm_pct", "risk_lcm", "risk_n1_pct", "risk_1_pct"],
    "n2": ["n_at_risk_lev", "n_lev", "eligible_outcome_n2", "patients_2"],
    "events2": ["events_lev", "events_n2", "events_2"],
    "risk2_pct": ["risk_lev_pct", "risk_lev", "risk_n2_pct", "risk_2_pct"],
    "rd_pct": ["risk_difference_pct", "risk_difference", "rd_pct"],
    "association_p": ["rd_p", "association_p", "p_value"],
    "rr": ["risk_ratio", "rr"], "or": ["odds_ratio", "or"],
    "km_survival1_pct": ["km_survival_lcm_pct", "km_survival_end_n1_pct"],
    "km_survival2_pct": ["km_survival_lev_pct", "km_survival_end_n2_pct"],
    "logrank_p": ["logrank_p"], "hr": ["hazard_ratio", "hr"],
    "proportionality_p": ["proportionality_p", "hr_proportionality_p", "ph_p"],
}

CI_ALIASES = {
    "rd": ["rd_ci95", "rd_95ci", "rd_95ci_pct"],
    "rr": ["rr_ci95", "rr_95ci"], "or": ["or_ci95", "or_95ci"],
    "hr": ["hr_ci95", "hr_95ci"],
}


def live_value(row, keys):
    for key in keys:
        if key in row and str(row[key]).strip() != "":
            return numeric(row[key])
    return None


def live_pair(row, keys):
    for key in keys:
        if key in row and str(row[key]).strip() != "":
            return extract_pair(row[key])
    return None


def compare_scalar(label, exported, live, tolerance, mismatches):
    if live is None:
        return 0
    if isinstance(live, tuple) and live and live[0] == "lt":
        if exported >= live[1]:
            mismatches.append({"field": label, "exported": exported, "saved": f"<{live[1]}"})
        return 1
    if abs(exported - live) > tolerance:
        mismatches.append({"field": label, "exported": exported, "saved": live})
    return 1


def compare_row(exported, live, context):
    mismatches = []
    checked = 0
    for field, keys in ALIASES.items():
        lv = live_value(live, keys)
        if lv is None:
            continue
        tol = 0.0 if field in {"n1", "n2", "events1", "events2"} else (0.00006 if field.endswith("_p") else 0.0006)
        checked += compare_scalar(field, exported[field], lv, tol, mismatches)
    for stem, keys in CI_ALIASES.items():
        pair = live_pair(live, keys)
        if pair is None:
            continue
        for suffix, lv in zip(("lo", "hi"), pair):
            field = f"{stem}_{suffix}" + ("_pct" if stem == "rd" else "")
            checked += compare_scalar(field, exported[field], lv, 0.0006, mismatches)
    for item in mismatches:
        item.update(context)
    return checked, mismatches


def by_outcome_id(path):
    return {row["outcome_id"]: row for row in rows(path)}


def by_run(path):
    out = {}
    for row in rows(path):
        out.setdefault(row["run_id"], []).append(row)
    return out


specs = [
    ("REPL2026", "R01", "REPL2026_P3_LCM_vs_LEV_PAPER9_d1_5y_133020.zip", EXPORTS / "EPIBONE_REPL2026_R01_LIVE_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("REPL2026", "R02", "REPL2026_P6_LCM_vs_LEV_PAPER9_d1_5y_834306.zip", EXPORTS / "EPIBONE_REPL2026_R02_LIVE_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("REPL2026", "R03", "REPL2026_NU_LCM_vs_LEV_PAPER9_d31_1y_317485.zip", EXPORTS / "EPIBONE_REPL2026_R03_LIVE_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("REPL2026", "R04", "REPL2026_NU_LCM_vs_LEV_PAPER9_d31_5y_750728.zip", EXPORTS / "EPIBONE_REPL2026_R04_LIVE_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("REPL2026", "R05", "REPL2026_LM3_LCM_vs_LEV_PAPER9_d91_5y_921354.zip", EXPORTS / "EPIBONE_REPL2026_R05_LIVE_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("REPL2026", "R06", "REPL2026_LM6_LCM_vs_LEV_PAPER9_d181_5y_865893.zip", EXPORTS / "EPIBONE_REPL2026_R06_LIVE_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("REPL2026", "R07", "REPL2026_P3_LCM_vs_LEV_NC10_d1_5y_713511.zip", EXPORTS / "EPIBONE_REPL2026_R07_LIVE_RESULTS_2026-08-22.csv", "NC", list(range(1, 11))),
    ("REPL2026", "R08", "REPL2026_NU_LCM_vs_LEV_NC10_d31_5y_152493.zip", EXPORTS / "EPIBONE_REPL2026_R08_LIVE_RESULTS_2026-08-22.csv", "NC", list(range(1, 11))),
    ("TTE2026", "A00", "REPL2026_LIB90_LCM_vs_LEV_PAPER9_d91_5y_984115.zip", EXPORTS / "EPIBONE_LIB90_LIVE_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("TTE2026", "A01", "TTE2026_LIB90_LCM_vs_LEV_PAPER9_d91_2y_686175.zip", EXPORTS / "EPIBONE_TTE2026_A01_LIB90_D91_2Y_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("TTE2026", "A02", "REPL2026_LITERAL_LCM_vs_LEV_PAPER9_d1_5y_976862.zip", EXPORTS / "EPIBONE_TTE2026_A02_LITERAL_D1_5Y_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("TTE2026", "A03", "TTE2026_NU_LCM_vs_LEV_PAPER9_d31_2y_980735.zip", EXPORTS / "EPIBONE_TTE2026_A03_NU_D31_2Y_RESULTS_2026-08-22.csv", "P", list(range(1, 10))),
    ("TTE2026", "A07", "TTE2026_NU_LCM_vs_LEV_NC10_d31_2y_457386.zip", EXPORTS / "EPIBONE_TTE2026_A07_NU_NC10_D31_2Y_RESULTS_2026-08-22.csv", "NC", list(range(1, 11))),
]

burden_rows = by_run(BURDEN)
burden_specs = [
    ("A01", "A01_EPI_vs_MIG_fracture_early_90d_896749.zip", [1, 2, 8, 9, 10]),
    ("A02", "A02_EPI_vs_MIG_F1_d91_365_866101.zip", [1, 2, 8, 9, 10]),
    ("A03", "A03_EPI_vs_MIG_F1_d366_1095_235349.zip", [1, 2, 8, 9, 10]),
    ("A06", "A06_EPI_vs_MIG_NC10_d1_365_130007.zip", list(range(1, 11))),
    ("A08", "A08_EPI_vs_MIG_CLINICAL6_d1_365_178284.zip", list(range(1, 7))),
]

results = []
all_mismatches = []
for program, run_id, archive, live_path, prefix, indices in specs:
    live = by_outcome_id(live_path)
    checked = available = suppressed = 0
    mismatches = []
    with zipfile.ZipFile(RAW / archive) as zf:
        names = set(zf.namelist())
        for index in indices:
            outcome_id = f"{prefix}{index:02d}"
            row = live[outcome_id]
            has = f"Outcome_{index}_Result_a_MOA_table.csv" in names and f"Outcome_{index}_Result_b_KM_table.csv" in names
            status = row.get("status", "available")
            if status == "privacy_suppressed":
                suppressed += 1
                if has:
                    mismatches.append({"program": program, "run_id": run_id, "outcome_id": outcome_id, "field": "suppression", "exported": "table_present", "saved": status})
                continue
            available += 1
            if not has:
                mismatches.append({"program": program, "run_id": run_id, "outcome_id": outcome_id, "field": "availability", "exported": "table_absent", "saved": status})
                continue
            exported = parse_moa(zf, index) | parse_km(zf, index)
            n, mm = compare_row(exported, row, {"program": program, "run_id": run_id, "outcome_id": outcome_id})
            checked += n
            mismatches.extend(mm)
    all_mismatches.extend(mismatches)
    results.append({"program": program, "run_id": run_id, "archive": f"raw_zips/{archive}", "available_rows_checked": available, "suppressed_rows_checked": suppressed, "numeric_fields_checked": checked, "mismatch_count": len(mismatches)})

for run_id, archive, indices in burden_specs:
    saved = burden_rows[run_id]
    checked = 0
    mismatches = []
    with zipfile.ZipFile(RAW / archive) as zf:
        for row, index in zip(saved, indices):
            exported = parse_moa(zf, index) | parse_km(zf, index)
            n, mm = compare_row(exported, row, {"program": "BURDEN", "run_id": run_id, "outcome_id": f"Outcome_{index}"})
            checked += n
            mismatches.extend(mm)
    all_mismatches.extend(mismatches)
    results.append({"program": "BURDEN", "run_id": run_id, "archive": f"raw_zips/{archive}", "available_rows_checked": len(indices), "suppressed_rows_checked": 0, "numeric_fields_checked": checked, "mismatch_count": len(mismatches)})

a09_live = rows(EXPORTS / "EPIBONE_BURDEN_A09_AGE65_LIVE_RESULTS_2026-08-23.csv")
checked = 0
mismatches = []
with zipfile.ZipFile(RAW / "A09_AGE65ATINDEX_EPI_vs_MIG_MOFHIP_d366_1095_698641.zip") as zf:
    for index, row in enumerate(a09_live, 1):
        exported = parse_moa(zf, index) | parse_km(zf, index)
        n, mm = compare_row(exported, row, {"program": "BURDEN", "run_id": "A09", "outcome_id": f"Outcome_{index}"})
        checked += n
        mismatches.extend(mm)
all_mismatches.extend(mismatches)
results.append({"program": "BURDEN", "run_id": "A09", "archive": "raw_zips/A09_AGE65ATINDEX_EPI_vs_MIG_MOFHIP_d366_1095_698641.zip", "available_rows_checked": 2, "suppressed_rows_checked": 0, "numeric_fields_checked": checked, "mismatch_count": len(mismatches)})

output = {
    "generated_on": "2026-08-23",
    "comparison_rule": "Exact counts/events; displayed-value rounding tolerance 0.0006 and p-value tolerance 0.00006; privacy-suppressed tables must be absent.",
    "runs_checked": len(results),
    "available_rows_checked": sum(x["available_rows_checked"] for x in results),
    "suppressed_rows_checked": sum(x["suppressed_rows_checked"] for x in results),
    "numeric_fields_checked": sum(x["numeric_fields_checked"] for x in results),
    "mismatch_count": len(all_mismatches),
    "runs": results,
    "mismatches": all_mismatches,
}
print(json.dumps(output, indent=2, ensure_ascii=False))
