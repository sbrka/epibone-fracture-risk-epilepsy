#!/usr/bin/env python3
"""Rebuild the PNU outcome ledger from validated TriNetX ZIP extractions.

Existing rows without a local extraction (for example the M43--M45 pilot rows)
are preserved.  Rows backed by a local extraction are deterministically replaced.
Suppressed or missing values are left blank; they are never converted to zero.
"""

from __future__ import annotations

import csv
import re
from pathlib import Path


BASE = Path(__file__).resolve().parent
EXTRACTED = BASE / "exports" / "extracted"
SOURCE_LEDGER = BASE / "EPIBONE_PNU_MATRIX_OUTCOME_RESULTS_2026-10-03.csv"
LEDGER = BASE / "EPIBONE_PNU_MATRIX_OUTCOME_RESULTS_2026-10-05.csv"

PRIVACY_FLOOR_QUALIFIED = {"M18", "M19", "M20", "M21", "M23", "M24"}

OUTCOMES = {
    1: ("P01", "Published primary composite — S72 + S12 + S52"),
    2: ("B06", "Broad major-fracture burden composite — S12/S22/S32/S42/S52/S62/S72/S82"),
    3: ("C01", "All-cause mortality"),
}

# Unavailable contrasts are represented explicitly so downstream matrix code
# cannot mistake an absent row for a zero event or null effect.  M08 and M31
# are retained here only as deterministic fallbacks; their validated exports
# replace these rows.  M42 never cleared the once-only Stage-0 count audit.
UNAVAILABLE_FALLBACKS = {
    "M08": (
        "CBZ",
        "LTG",
        "UNAVAILABLE_GOVERNED_TERMINAL_NO_RERUN",
        "Governed terminal failure; no rerun authorised; unavailable, never zero",
    ),
    "M31": (
        "OXC",
        "TPM",
        "UNAVAILABLE_GOVERNED_TERMINAL_NO_RERUN",
        "Governed terminal failure; no rerun authorised; unavailable, never zero",
    ),
    "M42": (
        "ZNS",
        "LEV",
        "UNAVAILABLE_PLATFORM_FAILURE_STAGE0_COUNT_NONRETURN",
        "Stage-0 cohort count was submitted once but the platform returned no numerator; Stage 1 was not started and the cell is unavailable, never zero",
    ),
}


def read_csv(path: Path) -> list[list[str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.reader(handle))


def section_values(rows: list[list[str]], title: str) -> list[str]:
    for i, row in enumerate(rows):
        if len(row) == 1 and row[0].strip() == title:
            if i + 2 >= len(rows):
                break
            return rows[i + 2]
    raise ValueError(f"Section {title!r} not found")


def cohort_rows(rows: list[list[str]]) -> tuple[list[str], list[str]]:
    for i, row in enumerate(rows):
        if len(row) >= 2 and row[0] == "Cohort" and row[1] == "Cohort Name":
            return rows[i + 1], rows[i + 2]
    raise ValueError("Cohort statistics not found")


def excluded_counts(rows: list[list[str]]) -> tuple[str, str]:
    pattern = re.compile(
        r"(\d+) patients in Cohort 1 and (\d+) patients in Cohort 2 "
        r"were excluded"
    )
    for row in rows:
        match = pattern.search(" ".join(row))
        if match:
            return match.group(1), match.group(2)
    return "", ""


def number(value: str, multiplier: float = 1.0) -> str:
    value = (value or "").strip()
    if not value:
        return ""
    try:
        result = float(value) * multiplier
    except ValueError:
        return value
    return f"{result:.10f}".rstrip("0").rstrip(".")


def drug_from_cohort(name: str, arm: str) -> str:
    match = re.search(rf"PNU_M\d+_{arm}_([A-Z0-9]+)_", name)
    if not match:
        raise ValueError(f"Cannot derive drug from cohort name {name!r}")
    return match.group(1)


def parse_outcome(directory: Path, outcome_number: int) -> dict[str, str]:
    moa = read_csv(directory / f"Outcome_{outcome_number}_Result_a_MOA_table.csv")
    km = read_csv(directory / f"Outcome_{outcome_number}_Result_b_KM_table.csv")
    moa1, moa2 = cohort_rows(moa)
    km1, km2 = cohort_rows(km)
    rd = section_values(moa, "Risk Difference")
    rr = section_values(moa, "Risk Ratio")
    odds = section_values(moa, "Odds Ratio")
    logrank = section_values(km, "Log-Rank Test")
    hazard = section_values(km, "Hazard Ratio")
    proportionality = section_values(km, "Proportionality")
    excluded1, excluded2 = excluded_counts(moa)

    match = re.fullmatch(r"(M\d{2})_(365|730)", directory.name)
    if not match:
        raise ValueError(f"Unexpected extraction directory {directory.name}")
    pair_id, horizon = match.groups()
    outcome_id, outcome_label = OUTCOMES[outcome_number]
    cohort1_name, cohort2_name = moa1[1], moa2[1]

    qualified = pair_id in PRIVACY_FLOOR_QUALIFIED
    return {
        "run_key": f"{pair_id}_{horizon}",
        "pair_id": pair_id,
        "horizon_days": horizon,
        "cohort_1_drug": drug_from_cohort(cohort1_name, "A"),
        "cohort_2_drug": drug_from_cohort(cohort2_name, "B"),
        "outcome_id": outcome_id,
        "outcome_label": outcome_label,
        "at_risk_n1": moa1[2],
        "at_risk_n2": moa2[2],
        "events_1": moa1[3],
        "events_2": moa2[3],
        "risk_1_pct": number(moa1[4], 100),
        "risk_2_pct": number(moa2[4], 100),
        "risk_difference_pct": number(rd[0], 100),
        "rd_ci_low_pct": number(rd[1], 100),
        "rd_ci_high_pct": number(rd[2], 100),
        "rd_z": number(rd[3]),
        "rd_p": number(rd[4]),
        "risk_ratio": number(rr[0]),
        "rr_ci_low": number(rr[1]),
        "rr_ci_high": number(rr[2]),
        "odds_ratio": number(odds[0]),
        "or_ci_low": number(odds[1]),
        "or_ci_high": number(odds[2]),
        "prior_outcome_excluded_n1": excluded1,
        "prior_outcome_excluded_n2": excluded2,
        "km_survival_1_pct": number(km1[5], 100),
        "km_survival_2_pct": number(km2[5], 100),
        "logrank_chi2": number(logrank[0]),
        "logrank_df": logrank[1],
        "logrank_p": number(logrank[2]),
        "hazard_ratio": number(hazard[0]),
        "hr_ci_low": number(hazard[1]),
        "hr_ci_high": number(hazard[2]),
        "ph_chi2": number(proportionality[0]),
        "ph_df": proportionality[1],
        "ph_p": number(proportionality[2]),
        "median_survival_1": km1[4] or "NA",
        "median_survival_2": km2[4] or "NA",
        "availability_status": (
            "QUALIFIED_PRIVACY_FLOOR_ARTIFACT" if qualified else "ESTIMABLE"
        ),
        "notes": (
            f"Parsed from validated local TriNetX extraction {directory.name}; "
            + (
                "qualified under the author-approved 2026-10-05 deviation because "
                "the balance-gate maximum was an exact 0-vs-10 privacy-floor artifact; "
                if qualified
                else ""
            )
            + "suppressed or absent fields remain unavailable, never zero"
        ),
    }


def unavailable_row(
    pair_id: str,
    horizon: str,
    drug1: str,
    drug2: str,
    outcome_number: int,
    status: str,
    note: str,
    fields: list[str],
) -> dict[str, str]:
    outcome_id, outcome_label = OUTCOMES[outcome_number]
    row = {field: "NA" for field in fields}
    row.update(
        {
            "run_key": f"{pair_id}_{horizon}",
            "pair_id": pair_id,
            "horizon_days": horizon,
            "cohort_1_drug": drug1,
            "cohort_2_drug": drug2,
            "outcome_id": outcome_id,
            "outcome_label": outcome_label,
            "availability_status": status,
            "notes": note,
        }
    )
    return row


def sort_key(row: dict[str, str]) -> tuple[int, int, int]:
    pair = int(row["pair_id"][1:])
    horizon_order = 0 if row["horizon_days"] == "730" else 1
    outcome_order = {"P01": 0, "B06": 1, "C01": 2}.get(row["outcome_id"], 9)
    return pair, horizon_order, outcome_order


def main() -> None:
    with SOURCE_LEDGER.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        fields = list(reader.fieldnames or [])
        existing = list(reader)
    if not fields:
        raise SystemExit("Outcome ledger has no header")

    rows_by_key = {(row["run_key"], row["outcome_id"]): row for row in existing}
    for pair_id, (drug1, drug2, status, note) in UNAVAILABLE_FALLBACKS.items():
        for horizon in ("730", "365"):
            for outcome_number in OUTCOMES:
                row = unavailable_row(
                    pair_id, horizon, drug1, drug2, outcome_number, status, note, fields
                )
                rows_by_key[(row["run_key"], row["outcome_id"])] = row
    parsed_directories = 0
    for directory in sorted(EXTRACTED.iterdir()):
        if not directory.is_dir() or not re.fullmatch(r"M\d{2}_(365|730)", directory.name):
            continue
        required = [directory / f"Outcome_{i}_Result_a_MOA_table.csv" for i in OUTCOMES]
        required += [directory / f"Outcome_{i}_Result_b_KM_table.csv" for i in OUTCOMES]
        if not all(path.exists() for path in required):
            continue
        parsed_directories += 1
        for outcome_number in OUTCOMES:
            row = parse_outcome(directory, outcome_number)
            rows_by_key[(row["run_key"], row["outcome_id"])] = row

    rows = sorted(rows_by_key.values(), key=sort_key)
    with LEDGER.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Parsed {parsed_directories} extraction directories; wrote {len(rows)} outcome rows")


if __name__ == "__main__":
    main()
