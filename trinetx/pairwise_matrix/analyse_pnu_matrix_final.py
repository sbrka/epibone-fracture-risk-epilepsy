#!/usr/bin/env python3
"""Final reconciliation and multiplicity analysis for the PNU/AUG77 matrix.

Unavailable cells are retained explicitly.  BH is reported both across the
observed estimable tests and conservatively against the prespecified planned
family sizes (45 per endpoint/horizon; 180 globally).  No unavailable cell is
assigned a p value or treated as zero.
"""

from __future__ import annotations

import csv
from collections import Counter, defaultdict
from pathlib import Path


BASE = Path(__file__).resolve().parent
SOURCE = BASE / "EPIBONE_PNU_MATRIX_OUTCOME_RESULTS_2026-10-05.csv"
RUN_LEDGER = BASE / "EPIBONE_PAIRWISE_NU_AUG77_PRIMARY_MATRIX_RUN_LEDGER_FINAL_2026-10-05.csv"
OUT = BASE / "EPIBONE_PNU_MATRIX_FINAL_MULTIPLICITY_2026-10-05.csv"
SUMMARY = BASE / "EPIBONE_PNU_MATRIX_MULTIPLICITY_SUMMARY_2026-10-05.csv"
REPORT = BASE / "EPIBONE_PNU_MATRIX_FINAL_RECONCILIATION_AND_INFERENCE_2026-10-05.md"

ANALYSABLE = {"ESTIMABLE", "QUALIFIED_PRIVACY_FLOOR_ARTIFACT"}


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def pvalue(value: str) -> float | None:
    value = (value or "").strip()
    if not value or value == "NA":
        return None
    if value.startswith("<"):
        return float(value[1:])
    return float(value)


def bh(items: list[tuple[int, float]], m_total: int | None = None) -> dict[int, float]:
    ranked = sorted(items, key=lambda item: item[1])
    if not ranked:
        return {}
    m = m_total if m_total is not None else len(ranked)
    if m < len(ranked):
        raise ValueError("BH family size cannot be smaller than observed tests")
    adjusted: dict[int, float] = {}
    running = 1.0
    for rank in range(len(ranked), 0, -1):
        index, value = ranked[rank - 1]
        running = min(running, value * m / rank)
        adjusted[index] = min(running, 1.0)
    return adjusted


def fmt(value: float | None) -> str:
    return "" if value is None else f"{value:.10f}".rstrip("0").rstrip(".")


def main() -> None:
    rows = read_csv(SOURCE)
    run_ledger = {row["run_key"]: row for row in read_csv(RUN_LEDGER)}
    keys = {(row["run_key"], row["outcome_id"]) for row in rows}
    if len(rows) != 270 or len(keys) != 270:
        raise SystemExit(f"Expected 270 unique run/outcome rows; found {len(rows)} rows and {len(keys)} keys")

    run_keys = {row["run_key"] for row in rows}
    if len(run_keys) != 90:
        raise SystemExit(f"Expected 90 horizon-specific runs; found {len(run_keys)}")

    fracture = [
        i for i, row in enumerate(rows)
        if row["outcome_id"] in {"P01", "B06"}
        and row["availability_status"] in ANALYSABLE
    ]
    families: dict[tuple[str, str], list[int]] = defaultdict(list)
    for i in fracture:
        families[(rows[i]["outcome_id"], rows[i]["horizon_days"])].append(i)

    adjustments: dict[str, dict[int, float]] = defaultdict(dict)
    for indices in families.values():
        rd = [(i, pvalue(rows[i]["rd_p"])) for i in indices if pvalue(rows[i]["rd_p"]) is not None]
        lr = [(i, pvalue(rows[i]["logrank_p"])) for i in indices if pvalue(rows[i]["logrank_p"]) is not None]
        adjustments["rd_family_observed"].update(bh(rd))
        adjustments["lr_family_observed"].update(bh(lr))
        adjustments["rd_family_planned45"].update(bh(rd, 45))
        adjustments["lr_family_planned45"].update(bh(lr, 45))

    rd_global = [(i, pvalue(rows[i]["rd_p"])) for i in fracture if pvalue(rows[i]["rd_p"]) is not None]
    lr_global = [(i, pvalue(rows[i]["logrank_p"])) for i in fracture if pvalue(rows[i]["logrank_p"]) is not None]
    adjustments["rd_global_observed"] = bh(rd_global)
    adjustments["lr_global_observed"] = bh(lr_global)
    adjustments["rd_global_planned180"] = bh(rd_global, 180)
    adjustments["lr_global_planned180"] = bh(lr_global, 180)

    stage1_fracture = [
        i for i in fracture
        if run_ledger[rows[i]["run_key"]]["design_tier"] == "STAGE1_AUG77"
    ]
    stage1_families: dict[tuple[str, str], list[int]] = defaultdict(list)
    for i in stage1_fracture:
        stage1_families[(rows[i]["outcome_id"], rows[i]["horizon_days"])].append(i)
    for indices in stage1_families.values():
        rd = [(i, pvalue(rows[i]["rd_p"])) for i in indices if pvalue(rows[i]["rd_p"]) is not None]
        lr = [(i, pvalue(rows[i]["logrank_p"])) for i in indices if pvalue(rows[i]["logrank_p"]) is not None]
        adjustments["rd_family_stage1_only"].update(bh(rd))
        adjustments["lr_family_stage1_only"].update(bh(lr))
    adjustments["rd_global_stage1_only"] = bh([
        (i, pvalue(rows[i]["rd_p"])) for i in stage1_fracture if pvalue(rows[i]["rd_p"]) is not None
    ])
    adjustments["lr_global_stage1_only"] = bh([
        (i, pvalue(rows[i]["logrank_p"])) for i in stage1_fracture if pvalue(rows[i]["logrank_p"]) is not None
    ])

    added = [
        "design_tier",
        "family_role",
        "bh_rd_q_family_observed",
        "bh_logrank_q_family_observed",
        "bh_rd_q_family_planned45",
        "bh_logrank_q_family_planned45",
        "bh_rd_q_global_observed",
        "bh_logrank_q_global_observed",
        "bh_rd_q_global_planned180",
        "bh_logrank_q_global_planned180",
        "bh_rd_q_family_stage1_only",
        "bh_logrank_q_family_stage1_only",
        "bh_rd_q_global_stage1_only",
        "bh_logrank_q_global_stage1_only",
        "multiplicity_status",
    ]
    enriched: list[dict[str, str]] = []
    for i, source in enumerate(rows):
        row = dict(source)
        if row["outcome_id"] == "C01":
            role = "MORTALITY_DIAGNOSTIC_EXCLUDED"
        elif row["outcome_id"] == "P01" and row["horizon_days"] == "730":
            role = "PRIMARY_FAMILY"
        else:
            role = "KEY_SECONDARY_FAMILY"
        row.update({
            "design_tier": run_ledger[row["run_key"]]["design_tier"],
            "family_role": role,
            "bh_rd_q_family_observed": fmt(adjustments["rd_family_observed"].get(i)),
            "bh_logrank_q_family_observed": fmt(adjustments["lr_family_observed"].get(i)),
            "bh_rd_q_family_planned45": fmt(adjustments["rd_family_planned45"].get(i)),
            "bh_logrank_q_family_planned45": fmt(adjustments["lr_family_planned45"].get(i)),
            "bh_rd_q_global_observed": fmt(adjustments["rd_global_observed"].get(i)),
            "bh_logrank_q_global_observed": fmt(adjustments["lr_global_observed"].get(i)),
            "bh_rd_q_global_planned180": fmt(adjustments["rd_global_planned180"].get(i)),
            "bh_logrank_q_global_planned180": fmt(adjustments["lr_global_planned180"].get(i)),
            "bh_rd_q_family_stage1_only": fmt(adjustments["rd_family_stage1_only"].get(i)),
            "bh_logrank_q_family_stage1_only": fmt(adjustments["lr_family_stage1_only"].get(i)),
            "bh_rd_q_global_stage1_only": fmt(adjustments["rd_global_stage1_only"].get(i)),
            "bh_logrank_q_global_stage1_only": fmt(adjustments["lr_global_stage1_only"].get(i)),
            "multiplicity_status": (
                "ESTIMABLE_FRACTURE_CELL" if i in fracture
                else "DIAGNOSTIC_NOT_IN_FRACTURE_FAMILIES" if row["outcome_id"] == "C01"
                else "UNAVAILABLE_NOT_IMPUTED"
            ),
        })
        enriched.append(row)

    with OUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]) + added)
        writer.writeheader()
        writer.writerows(enriched)

    status = Counter(row["availability_status"] for row in rows)
    family_counts = {key: len(indices) for key, indices in sorted(families.items())}
    primary_pair_count = sum(
        row["availability_status"] in ANALYSABLE
        for row in rows
        if row["outcome_id"] == "P01" and row["horizon_days"] == "730"
    )

    summary_rows: list[dict[str, str | int]] = []
    for family, indices in sorted(families.items()):
        endpoint, horizon = family
        for test, pkey, qkey, stage1_qkey in (
            ("risk_difference", "rd_p", "rd_family_planned45", "rd_family_stage1_only"),
            ("log_rank", "logrank_p", "lr_family_planned45", "lr_family_stage1_only"),
        ):
            values = [(i, pvalue(rows[i][pkey])) for i in indices]
            values = [(i, p) for i, p in values if p is not None]
            stage1_values = [
                (i, p) for i, p in values
                if run_ledger[rows[i]["run_key"]]["design_tier"] == "STAGE1_AUG77"
            ]
            summary_rows.append({
                "family": f"{endpoint}_{horizon}",
                "role": "PRIMARY" if family == ("P01", "730") else "KEY_SECONDARY",
                "test": test,
                "estimable_tests": len(values),
                "planned_tests": 45,
                "nominal_p_lt_0.05": sum(p < 0.05 for _, p in values),
                "expected_nominal_under_null": fmt(0.05 * len(values)),
                "minimum_raw_p": fmt(min(p for _, p in values)) if values else "",
                "minimum_q_planned": fmt(min(adjustments[qkey][i] for i, _ in values)) if values else "",
                "q_lt_0.05_planned": sum(adjustments[qkey][i] < 0.05 for i, _ in values),
                "stage1_estimable_tests": len(stage1_values),
                "stage1_nominal_p_lt_0.05": sum(p < 0.05 for _, p in stage1_values),
                "stage1_expected_nominal_under_null": fmt(0.05 * len(stage1_values)),
                "stage1_minimum_q_within_tier": (
                    fmt(min(adjustments[stage1_qkey][i] for i, _ in stage1_values))
                    if stage1_values else ""
                ),
                "stage1_q_lt_0.05_within_tier": (
                    sum(adjustments[stage1_qkey][i] < 0.05 for i, _ in stage1_values)
                    if stage1_values else 0
                ),
            })
    for test, pkey, qkey, stage1_qkey in (
        ("risk_difference", "rd_p", "rd_global_planned180", "rd_global_stage1_only"),
        ("log_rank", "logrank_p", "lr_global_planned180", "lr_global_stage1_only"),
    ):
        values = [(i, pvalue(rows[i][pkey])) for i in fracture]
        values = [(i, p) for i, p in values if p is not None]
        stage1_values = [(i, p) for i, p in values if i in stage1_fracture]
        summary_rows.append({
            "family": "GLOBAL_FRACTURE_180", "role": "GLOBAL", "test": test,
            "estimable_tests": len(values), "planned_tests": 180,
            "nominal_p_lt_0.05": sum(p < 0.05 for _, p in values),
            "expected_nominal_under_null": fmt(0.05 * len(values)),
            "minimum_raw_p": fmt(min(p for _, p in values)) if values else "",
            "minimum_q_planned": fmt(min(adjustments[qkey][i] for i, _ in values)) if values else "",
            "q_lt_0.05_planned": sum(adjustments[qkey][i] < 0.05 for i, _ in values),
            "stage1_estimable_tests": len(stage1_values),
            "stage1_nominal_p_lt_0.05": sum(p < 0.05 for _, p in stage1_values),
            "stage1_expected_nominal_under_null": fmt(0.05 * len(stage1_values)),
            "stage1_minimum_q_within_tier": (
                fmt(min(adjustments[stage1_qkey][i] for i, _ in stage1_values))
                if stage1_values else ""
            ),
            "stage1_q_lt_0.05_within_tier": (
                sum(adjustments[stage1_qkey][i] < 0.05 for i, _ in stage1_values)
                if stage1_values else 0
            ),
        })
    with SUMMARY.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(summary_rows[0]))
        writer.writeheader()
        writer.writerows(summary_rows)

    primary = [(i, rows[i]) for i in families[("P01", "730")]]
    primary_rd = [
        (i, row) for i, row in primary
        if adjustments["rd_family_planned45"].get(i, 1) < 0.05
    ]
    primary_lr = [
        (i, row) for i, row in primary
        if adjustments["lr_family_planned45"].get(i, 1) < 0.05
    ]
    global_rd = [
        (i, rows[i]) for i in fracture
        if adjustments["rd_global_planned180"].get(i, 1) < 0.05
    ]
    global_lr = [
        (i, rows[i]) for i in fracture
        if adjustments["lr_global_planned180"].get(i, 1) < 0.05
    ]

    def signals(items: list[tuple[int, dict[str, str]]], qkey: str, pkey: str) -> str:
        if not items:
            return "- None."
        lines = []
        for i, row in sorted(items, key=lambda x: adjustments[qkey][x[0]]):
            lines.append(
                f"- {row['pair_id']} {row['cohort_1_drug']} vs {row['cohort_2_drug']}, "
                f"{row['outcome_id']} days 31–{row['horizon_days']}: "
                f"p={row[pkey]}, q={fmt(adjustments[qkey][i])}, "
                f"RD={row['risk_difference_pct']} percentage points, "
                f"RR={row['risk_ratio']} ({row['rr_ci_low']}–{row['rr_ci_high']}), "
                f"HR={row['hazard_ratio']} ({row['hr_ci_low']}–{row['hr_ci_high']})."
            )
        return "\n".join(lines)

    pair_lines = []
    for pair_num in range(1, 46):
        pair_id = f"M{pair_num:02d}"
        pair_rows = [row for row in rows if row["pair_id"] == pair_id]
        if len(pair_rows) != 6:
            raise SystemExit(f"{pair_id}: expected 6 rows, found {len(pair_rows)}")
        pair_statuses = {row["availability_status"] for row in pair_rows}
        run_rows = [run_ledger[f"{pair_id}_730"], run_ledger[f"{pair_id}_365"]]
        if "QUALIFIED_PRIVACY_FLOOR_ARTIFACT" in pair_statuses:
            final_state = "QUALIFIED_PRIVACY_FLOOR_ARTIFACT"
        elif pair_statuses <= ANALYSABLE:
            final_state = "ESTIMABLE"
        elif "UNAVAILABLE_MATCHING_TERMINAL_TIMEOUT" in pair_statuses and pair_statuses & ANALYSABLE:
            final_state = "PARTLY_ESTIMABLE; 365-day MATCHING_TIMEOUT"
        else:
            unavailable = sorted(s for s in pair_statuses if s not in ANALYSABLE)
            final_state = "UNAVAILABLE: " + ", ".join(unavailable)
        evidence = sorted({r.get("evidence_source", "") for r in run_rows if r.get("evidence_source", "")})
        primary_row = next(row for row in pair_rows if row["outcome_id"] == "P01" and row["horizon_days"] == "730")
        primary_index = rows.index(primary_row)
        primary_result = (
            f"RD {primary_row['risk_difference_pct']} pp; p={primary_row['rd_p']}; "
            f"q45={fmt(adjustments['rd_family_planned45'][primary_index])}"
            if primary_row["availability_status"] in ANALYSABLE else "unavailable"
        )
        pair_lines.append(
            f"| {pair_id} | {primary_row['cohort_1_drug']} vs {primary_row['cohort_2_drug']} | "
            f"{run_rows[0]['design_tier']} | {final_state} | {', '.join(evidence)} | {primary_result} |"
        )

    summary_lines = [
        "| Family | Test | Estimable | Nominal P<.05 | Expected | Minimum planned-denominator q | Stage-1-only q<.05 |",
        "|---|---:|---:|---:|---:|---:|---:|",
    ]
    for row in summary_rows:
        if row["family"] == "GLOBAL_FRACTURE_180":
            continue
        summary_lines.append(
            f"| {row['family']} | {row['test']} | {row['estimable_tests']} | "
            f"{row['nominal_p_lt_0.05']} | {row['expected_nominal_under_null']} | "
            f"{row['minimum_q_planned']} | {row['stage1_q_lt_0.05_within_tier']} |"
        )

    report = f"""# PNU/AUG77 matrix — final reconciliation and multiplicity analysis

## Completion and availability

- Planned structure: 45 drug pairs × 2 horizons × 3 outcomes = 270 cells across 90 horizon-specific runs.
- Reconciled table: {len(rows)} unique cells across {len(run_keys)} unique runs.
- Fully estimable cells: {status.get('ESTIMABLE', 0)}.
- Qualified privacy-floor-artifact cells: {status.get('QUALIFIED_PRIVACY_FLOOR_ARTIFACT', 0)}; these are retained in every multiplicity family under the author-approved 5 October 2026 deviation.
- Primary P01 days 31–730 pairs with an estimate: {primary_pair_count} of 45.
- Other unavailable cells: {sum(v for k, v in status.items() if k not in ANALYSABLE)}.
- Stage-0 platform count non-return (M42 at both horizons): {status.get('UNAVAILABLE_PLATFORM_FAILURE_STAGE0_COUNT_NONRETURN', 0)} cells.
- Matching terminal timeout (M45 at 365 days): {status.get('UNAVAILABLE_MATCHING_TERMINAL_TIMEOUT', 0)} cells.
- Unavailable cells are explicit and were never assigned zero events, null effects, or synthetic p values.
- Estimable fracture tests by family: {family_counts}.

## Multiplicity implementation

The prespecified primary family is P01 at days 31–730. P01 at days 31–365 and B06 at both horizons are key secondary families. C01 mortality is diagnostic and excluded. For each fracture family, BH was calculated across the observed estimable p values and also conservatively against the prespecified 45-test family size. Global BH was calculated across the observed estimable fracture tests and against the prespecified 180-test matrix. The planned-denominator results are used for the principal interpretation; observed-test q values are retained as transparent diagnostics.

## Nominal results versus chance expectation

{chr(10).join(summary_lines)}

The expectation is descriptive (0.05 × the number of estimable tests), not a formal model for the correlated pairwise tests. The Stage-1-only sensitivity applies BH within Stage-1 pairs only; no Stage-2 result contributes to that sensitivity.

## Primary family survivors — fixed-window risk difference (planned denominator 45)

{signals(primary_rd, 'rd_family_planned45', 'rd_p')}

## Primary family survivors — log-rank (planned denominator 45)

{signals(primary_lr, 'lr_family_planned45', 'logrank_p')}

## Global 180-test survivors — fixed-window risk difference

{signals(global_rd, 'rd_global_planned180', 'rd_p')}

## Global 180-test survivors — log-rank

{signals(global_lr, 'lr_global_planned180', 'logrank_p')}

## Final state of all 45 pairs

| Pair | Comparison | Design tier | Final state | Evidence | Primary P01 31–730 RD |
|---|---|---|---|---|---|
{chr(10).join(pair_lines)}

## Interpretation guardrails

- Family-level survival is not a causal drug ranking: pairs share drugs, horizons are correlated, and treatment selection remains clinically structured.
- Discordance between RD/RR and HR/log-rank, or a mortality imbalance, must be shown rather than averaged away.
- Every estimable value remains in the authoritative table regardless of SMD; SMD is a design diagnostic, not a post-outcome deletion rule.
- Suppressed and failed cells remain unavailable, never zero.
"""
    REPORT.write_text(report, encoding="utf-8")
    print(f"Wrote {OUT}")
    print(f"Wrote {SUMMARY}")
    print(f"Wrote {REPORT}")
    print(f"Primary planned-denominator survivors: RD={len(primary_rd)}, log-rank={len(primary_lr)}")
    print(f"Global planned-denominator survivors: RD={len(global_rd)}, log-rank={len(global_lr)}")


if __name__ == "__main__":
    main()
