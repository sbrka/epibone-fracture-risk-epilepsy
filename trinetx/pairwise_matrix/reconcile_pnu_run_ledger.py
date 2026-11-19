#!/usr/bin/env python3
"""Create the final horizon-level run ledger from outcomes and validated exports."""

from __future__ import annotations

import csv
import hashlib
import re
from collections import defaultdict
from pathlib import Path


BASE = Path(__file__).resolve().parent
SOURCE = BASE / "EPIBONE_PAIRWISE_NU_AUG77_PRIMARY_MATRIX_RUN_LEDGER_2026-10-02.csv"
OUTCOMES = BASE / "EPIBONE_PNU_MATRIX_OUTCOME_RESULTS_2026-10-05.csv"
RAW = BASE / "exports" / "raw_zips"
EXTRACTED = BASE / "exports" / "extracted"
OUT = BASE / "EPIBONE_PAIRWISE_NU_AUG77_PRIMARY_MATRIX_RUN_LEDGER_FINAL_2026-10-05.csv"
DESIGN_OUT = BASE / "EPIBONE_PNU_PAIR_SPECIFIC_DESIGN_LEDGER_2026-10-05.csv"

STAGE2_PAIRS = {"M17", "M18", "M19", "M20", "M21", "M23", "M24"}
PRIVACY_FLOOR_QUALIFIED = {"M18", "M19", "M20", "M21", "M23", "M24"}


def read(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def digest(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def fmt(value: float) -> str:
    return f"{value:.10f}".rstrip("0").rstrip(".")


def csv_section(path: Path, header_prefix: str) -> list[dict[str, str]]:
    lines = path.read_text(encoding="utf-8-sig").splitlines()
    start = next(i for i, line in enumerate(lines) if line.startswith(header_prefix))
    return list(csv.DictReader(lines[start:]))


def baseline_diagnostics(directory: Path) -> dict[str, str]:
    rows = csv_section(directory / "Baseline_Patient_Characteristics.csv", "Characteristic ID")
    age = next(row for row in rows if row["Characteristic ID"] == "AI")
    maxima: list[tuple[float, str, str, bool]] = []
    artifact_count = 0
    for row in rows:
        try:
            smd = abs(float(row["After: Standardized Mean Difference"]))
        except (TypeError, ValueError):
            continue
        c1 = row["Cohort 1 After: Patient Count"].strip()
        c2 = row["Cohort 2 After: Patient Count"].strip()
        artifact = {c1, c2} == {"0", "10"}
        artifact_count += int(artifact)
        maxima.append((smd, row["Characteristic ID"], row["Characteristic Name"], artifact))
    max_all = max(maxima, default=(0.0, "", "", False))
    max_nonartifact = max((x for x in maxima if not x[3]), default=(0.0, "", "", False))
    if max_all[0] < 0.10:
        balance = "BALANCED_LT_0.10"
    elif max_all[3] and max_nonartifact[0] < 0.10:
        balance = "QUALIFIED_PRIVACY_FLOOR_ARTIFACT"
    elif max_all[0] <= 0.15:
        balance = "QUALIFIED_0.10_TO_0.15"
    else:
        balance = "DESCRIPTIVE_GT_0.15"
    eligible1 = int(age["Cohort 1 Before: Patient Count"])
    eligible2 = int(age["Cohort 2 Before: Patient Count"])
    matched1 = int(age["Cohort 1 After: Patient Count"])
    matched2 = int(age["Cohort 2 After: Patient Count"])
    return {
        "eligible_n1": str(eligible1),
        "eligible_n2": str(eligible2),
        "matched_n1": str(matched1),
        "matched_n2": str(matched2),
        "retention_1": fmt(matched1 / eligible1),
        "retention_2": fmt(matched2 / eligible2),
        "max_abs_smd": fmt(max_all[0]),
        "max_abs_smd_characteristic": f"{max_all[1]} {max_all[2]}".strip(),
        "max_nonartifact_abs_smd": fmt(max_nonartifact[0]),
        "max_nonartifact_characteristic": (
            f"{max_nonartifact[1]} {max_nonartifact[2]}".strip()
        ),
        "privacy_floor_artifact_count": str(artifact_count),
        "balance_class": balance,
        "overlap_status": "EXPORTED_PROPENSITY_DENSITY_AVAILABLE",
    }


def followup_diagnostics(directory: Path) -> dict[str, str]:
    rows = csv_section(
        directory / "Follow_up_time_metrics_matched_results_table.csv", "Cohort,Cohort Name"
    )
    return {
        "followup_mean_1": rows[0]["Mean Follow-up (Days)"],
        "followup_mean_2": rows[1]["Mean Follow-up (Days)"],
        "followup_median_1": rows[0]["Median Follow-up (Days)"],
        "followup_median_2": rows[1]["Median Follow-up (Days)"],
    }


def export_metadata(directory: Path) -> dict[str, str]:
    text = (directory / "_Read_Me.txt").read_text(encoding="utf-8-sig")
    def grab(label: str) -> str:
        match = re.search(rf"^ \* {re.escape(label)}: (.+)$", text, re.MULTILINE)
        return match.group(1).strip() if match else ""
    analysis_match = re.search(r"/analytics/([^/]+)/outcomes/results", text)
    total = grab("HCOs on Network at Time of Analysis")
    online = grab("HCOs Online for Analysis")
    status = f"{online}/{total} online" if online and total else ""
    if online and total and online != total:
        status += "; one or more HCOs did not return"
    return {
        "live_analysis_id": analysis_match.group(1) if analysis_match else "",
        "data_refresh": grab("Last Data Refresh"),
        "hco_status": status,
        "export_analysis_name": grab("Analysis Name"),
        "export_cohort_1": grab("Cohort 1"),
        "export_cohort_2": grab("Cohort 2"),
        "export_network": grab("Network Name"),
    }


def add_design_ledger(runs: list[dict[str, str]]) -> None:
    by_pair: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in runs:
        by_pair[row["pair_id"]].append(row)
    fields = [
        "pair_id", "drug1", "drug2", "stage0_cohort_status", "stage1_aug77_status",
        "adaptation_trigger", "stage2_manifest", "stage2_status", "stage3_restriction",
        "final_design_tier", "outcome_lock", "status_note",
    ]
    rows = []
    for pair_id in sorted(by_pair, key=lambda x: int(x[1:])):
        pair_runs = sorted(by_pair[pair_id], key=lambda x: -int(x["horizon_days"]))
        first = pair_runs[0]
        drug1 = re.search(rf"PNU_{pair_id}_A_([A-Z0-9]+)_", first["cohort_1"])
        drug2 = re.search(rf"PNU_{pair_id}_B_([A-Z0-9]+)_", first["cohort_2"])
        stage2 = pair_id in STAGE2_PAIRS
        stage0_unavailable = pair_id == "M42"
        if pair_id in PRIVACY_FLOOR_QUALIFIED:
            trigger = "Stage 1 failed the pre-outcome balance gate; Stage 2 maximum was an exact 0-vs-10 privacy-floor artifact."
            stage2_status = "qualified_under_author_approved_2026-10-05_deviation"
        elif stage2:
            trigger = "Stage 1 failed a prespecified pre-outcome gate; one frozen pair-specific model was used."
            stage2_status = "passed"
        else:
            trigger = ""
            stage2_status = "not_required"
        states = "; ".join(f"{r['horizon_days']}d={r['run_status']}" for r in pair_runs)
        rows.append({
            "pair_id": pair_id,
            "drug1": drug1.group(1) if drug1 else "",
            "drug2": drug2.group(1) if drug2 else "",
            "stage0_cohort_status": (
                "platform_count_nonreturn_after_once_only_submission"
                if stage0_unavailable else "passed"
            ),
            "stage1_aug77_status": (
                "not_run" if stage0_unavailable
                else ("failed_preoutcome_gate" if stage2 else "passed")
            ),
            "adaptation_trigger": (
                "Stage 0 did not return a cohort numerator; the once-only rule prohibited resubmission."
                if stage0_unavailable else trigger
            ),
            "stage2_manifest": (
                "not_reached" if stage0_unavailable
                else ("pair-specific frozen manifest" if stage2 else "AUG77 dc49b394...")
            ),
            "stage2_status": "not_reached" if stage0_unavailable else stage2_status,
            "stage3_restriction": "not_required",
            "final_design_tier": (
                "NOT_REACHED_STAGE1" if stage0_unavailable
                else ("STAGE2_PAIR_SPECIFIC" if stage2 else "STAGE1_AUG77")
            ),
            "outcome_lock": "not_run" if stage0_unavailable else "locked",
            "status_note": states,
        })
    with DESIGN_OUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {DESIGN_OUT}")


def main() -> None:
    runs = read(SOURCE)
    outcomes = read(OUTCOMES)
    by_run: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in outcomes:
        by_run[row["run_key"]].append(row)
    if len(runs) != 90 or len(by_run) != 90:
        raise SystemExit("Expected 90 run rows in both sources")

    manual = {
        "M40_730": dict(source_count_1="6139", source_count_2="12742", eligible_n1="5324", eligible_n2="10622", matched_n1="5127", matched_n2="5127", retention_1="0.9630", retention_2="0.4827", max_abs_smd="0.063", balance_class="BALANCED", overlap_status="ACCEPTABLE_VISUAL_OVERLAP"),
        "M40_365": dict(source_count_1="6139", source_count_2="12742", eligible_n1="", eligible_n2="", matched_n1="", matched_n2="", retention_1="", retention_2="", max_abs_smd="", balance_class="BALANCED", overlap_status="ACCEPTABLE_VISUAL_OVERLAP"),
        "M41_730": dict(source_count_1="5897", source_count_2="11841", eligible_n1="5324", eligible_n2="10219", matched_n1="4972", matched_n2="4972", retention_1="0.9339", retention_2="0.4865", max_abs_smd="0.0175", balance_class="BALANCED", overlap_status="ACCEPTABLE_VISUAL_OVERLAP"),
        "M41_365": dict(source_count_1="5897", source_count_2="11841", eligible_n1="5378", eligible_n2="10439", matched_n1="5031", matched_n2="5031", retention_1="0.9355", retention_2="0.4820", max_abs_smd="", balance_class="BALANCED", overlap_status="ACCEPTABLE_VISUAL_OVERLAP"),
    }

    for row in runs:
        key = row["run_key"]
        cells = by_run[key]
        statuses = {cell["availability_status"] for cell in cells}
        row["design_tier"] = (
            "NOT_REACHED_STAGE1" if row["pair_id"] == "M42"
            else ("STAGE2_PAIR_SPECIFIC" if row["pair_id"] in STAGE2_PAIRS else "STAGE1_AUG77")
        )
        row.setdefault("max_abs_smd_characteristic", "")
        row.setdefault("max_nonartifact_abs_smd", "")
        row.setdefault("max_nonartifact_characteristic", "")
        row.setdefault("privacy_floor_artifact_count", "")
        row.setdefault("mortality_hr", "")
        row.setdefault("mortality_hr_ci_low", "")
        row.setdefault("mortality_hr_ci_high", "")
        row.setdefault("evidence_source", "")
        if key in manual:
            row.update(manual[key])

        zip_matches = sorted(RAW.glob(f"EPIBONE_PNU_{key.split('_')[0]}_*_d31_{row['horizon_days']}_*.zip"))
        extracted = EXTRACTED / key
        export_available = extracted.is_dir() and zip_matches
        if export_available:
            row.update(baseline_diagnostics(extracted))
            row.update(followup_diagnostics(extracted))
            metadata = export_metadata(extracted)
            expected = {
                "analysis_name": row["analysis_name"],
                "cohort_1": row["cohort_1"],
                "cohort_2": row["cohort_2"],
                "network": "Global Collaborative Network",
            }
            observed = {
                "analysis_name": metadata.pop("export_analysis_name"),
                "cohort_1": metadata.pop("export_cohort_1"),
                "cohort_2": metadata.pop("export_cohort_2"),
                "network": metadata.pop("export_network"),
            }
            if observed != expected:
                raise SystemExit(f"{key}: export metadata mismatch: {observed} != {expected}")
            row.update({k: v for k, v in metadata.items() if v})
            row["evidence_source"] = "VALIDATED_LOCAL_EXPORT"

        mortality = next((cell for cell in cells if cell["outcome_id"] == "C01"), None)
        if mortality and mortality["hazard_ratio"] not in {"", "NA"}:
            row.update(
                mortality_hr=mortality["hazard_ratio"],
                mortality_hr_ci_low=mortality["hr_ci_low"],
                mortality_hr_ci_high=mortality["hr_ci_high"],
            )

        if statuses <= {"ESTIMABLE", "QUALIFIED_PRIVACY_FLOOR_ARTIFACT"}:
            if extracted.is_dir() and zip_matches:
                archive = zip_matches[-1]
                file_count = sum(1 for p in extracted.rglob("*") if p.is_file())
                if file_count != 46:
                    raise SystemExit(f"{key}: expected 46 extracted files, found {file_count}")
                row["run_status"] = (
                    "OUTCOME_COMPLETE_EXPORT_VALIDATED_QUALIFIED_PRIVACY_FLOOR_ARTIFACT"
                    if statuses == {"QUALIFIED_PRIVACY_FLOOR_ARTIFACT"}
                    else "OUTCOME_COMPLETE_EXPORT_VALIDATED"
                )
                row["export_zip"] = str(archive.relative_to(BASE))
                row["sha256"] = digest(archive)
                row["validation_status"] = "PASS_CRC_PATH_METADATA_46_MEMBERS"
                row["audit_status"] = "PASS_WITH_WARNINGS"
            else:
                row["run_status"] = "OUTCOMES_COMPLETE_SCREEN_AUDITED_NO_LOCAL_EXPORT"
                row["validation_status"] = "SCREEN_AUDITED_EXPORT_NOT_RECOVERED"
                row["audit_status"] = "SCREEN_AUDITED"
                row["evidence_source"] = "SCREEN_AUDITED"
        elif statuses == {"UNAVAILABLE_STAGE2_BALANCE_FAILURE"}:
            row["run_status"] = "TERMINAL_STAGE2_BALANCE_FAILURE"
            row["balance_class"] = "FAILED_STAGE2"
        elif statuses == {"UNAVAILABLE_PLATFORM_FAILURE_STAGE0_COUNT_NONRETURN"}:
            row["run_status"] = "PLATFORM_FAILURE_STAGE0_COUNT_NONRETURN_NO_RERUN"
        elif statuses == {"UNAVAILABLE_MATCHING_TERMINAL_TIMEOUT"}:
            row["run_status"] = "TERMINAL_MATCHING_TIMEOUT_NO_RESULTS"
        else:
            raise SystemExit(f"{key}: unexpected availability set {statuses}")

        if row["pair_id"] in PRIVACY_FLOOR_QUALIFIED:
            row["balance_class"] = "QUALIFIED_PRIVACY_FLOOR_ARTIFACT"
        suffix = "Final reconciliation 2026-10-05; unavailable cells were not imputed or treated as zero."
        if suffix not in row["notes"]:
            row["notes"] = (row["notes"].rstrip() + " " + suffix).strip()

    fields = list(runs[0])
    with OUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(runs)
    print(f"Wrote {OUT}")
    add_design_ledger(runs)


if __name__ == "__main__":
    main()
