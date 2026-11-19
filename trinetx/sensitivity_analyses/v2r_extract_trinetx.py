#!/usr/bin/env python3
"""Extract immutable V2R TriNetX ZIPs and independently recompute gates/BH."""

from __future__ import annotations

import csv
import io
import math
import re
import zipfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
LEDGER = ROOT / "04_trinetx_exports" / "EPIBONE_V2R_LIVE_RUN_LEDGER.csv"
OUT = ROOT / "05_results" / "v2r_reviewer_analyses_2026-09"


def clean(value):
    return (value or "").replace("\ufeff", "").strip()


def number(value):
    text = clean(value).replace(",", "")
    if not text or text in {"-", "—", "SUPPRESSED"}:
        return None
    try:
        return float(text)
    except ValueError:
        return None


def integer(value):
    parsed = number(value)
    return int(parsed) if parsed is not None else None


def zip_rows(zf, member):
    text = zf.read(member).decode("utf-8-sig", errors="replace")
    return list(csv.reader(io.StringIO(text)))


def section(rows, title):
    for i, row in enumerate(rows):
        if row and clean(row[0]) == title:
            j = i + 1
            while j < len(rows) and not any(clean(x) for x in rows[j]):
                j += 1
            if j >= len(rows):
                return []
            header = [clean(x) for x in rows[j]]
            result = []
            for values in rows[j + 1 :]:
                if not any(clean(x) for x in values):
                    break
                result.append(dict(zip(header, [clean(x) for x in values])))
            return result
    return []


def one(rows, title):
    found = section(rows, title)
    return found[0] if found else {}


def parse_readme(text):
    meta, outcomes = {}, {}
    for line in text.splitlines():
        match = re.match(r"^\s*\*\s+([^:]+):\s*(.*)$", clean(line))
        if not match:
            continue
        key, value = clean(match.group(1)), clean(match.group(2))
        out = re.match(r"Outcome\s+(\d+)\s+Name", key)
        if out:
            outcomes[int(out.group(1))] = value
        else:
            meta[key] = value
    return meta, outcomes


def parse_moa(zf, index):
    rows = zip_rows(zf, f"Outcome_{index}_Result_a_MOA_table.csv")
    cohorts = section(rows, "Cohort Statistics")
    rd, rr, odds = one(rows, "Risk Difference"), one(rows, "Risk Ratio"), one(rows, "Odds Ratio")
    return {
        "n1": integer(cohorts[0].get("Patients in Cohort")),
        "events1": integer(cohorts[0].get("Patients with Outcome")),
        "risk1": number(cohorts[0].get("Risk")),
        "n2": integer(cohorts[1].get("Patients in Cohort")),
        "events2": integer(cohorts[1].get("Patients with Outcome")),
        "risk2": number(cohorts[1].get("Risk")),
        "risk_difference": number(rd.get("Risk Difference")),
        "rd_ci_low": number(rd.get("95 % CI Lower")),
        "rd_ci_high": number(rd.get("95 % CI Upper")),
        "association_p": number(rd.get("p")),
        "risk_ratio": number(rr.get("Risk Ratio")),
        "rr_ci_low": number(rr.get("95 % CI Lower")),
        "rr_ci_high": number(rr.get("95 % CI Upper")),
        "odds_ratio": number(odds.get("Odds Ratio")),
        "or_ci_low": number(odds.get("95 % CI Lower")),
        "or_ci_high": number(odds.get("95 % CI Upper")),
    }


def parse_km(zf, index):
    rows = zip_rows(zf, f"Outcome_{index}_Result_b_KM_table.csv")
    logrank, hr, ph = one(rows, "Log-Rank Test"), one(rows, "Hazard Ratio"), one(rows, "Proportionality")
    return {
        "logrank_p": number(logrank.get("p")),
        "hazard_ratio": number(hr.get("Hazard Ratio")),
        "hr_ci_low": number(hr.get("95 % CI Lower")),
        "hr_ci_high": number(hr.get("95 % CI Upper")),
        "proportionality_p": number(ph.get("p")),
    }


def canonical_outcome(name, panel):
    text = name.lower()
    if panel == "NC10":
        found = re.search(r"nc\s*0?(\d+)", text)
        return f"NC{int(found.group(1)):02d}" if found else "NC_UNMAPPED"
    rules = [
        (1, "MOF", r"conventional major osteoporotic"),
        (2, "HIP", r"hip|proximal femur"),
        (3, "BROAD_FRACTURE", r"broad major-fracture"),
        (4, "FALLS", r"^falls?$"),
        (5, "HEAD_FACE_INJURY", r"head or face injury"),
        (6, "M81", r"without current pathological|\bm81\b"),
        (7, "M80", r"with current pathological|\bm80\b"),
        (8, "M85_8", r"osteopenia|m85[.]8"),
        (9, "S02", r"skull and facial fracture|\bs02\b"),
        (10, "S43_0", r"shoulder dislocation|s43[.]0"),
        (11, "VERTEBRAL", r"clinical vertebral"),
        (12, "FOREARM", r"distal forearm"),
        (13, "DXA", r"dxa performed"),
        (14, "VITAMIN_D", r"vitamin d measurement"),
    ]
    for order, code, pattern in rules:
        if re.search(pattern, text):
            return f"C{order:02d}_{code}"
    return "CORE_UNMAPPED"


def bh(values, total_tests):
    indexed = sorted(((p, i) for i, p in enumerate(values) if p is not None), reverse=True)
    out = [None] * len(values)
    running = 1.0
    ascending_rank = len(indexed)
    for p, i in indexed:
        running = min(running, p * total_tests / ascending_rank)
        out[i] = min(1.0, running)
        ascending_rank -= 1
    return out


def read_ledger():
    with LEDGER.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def main():
    records = []
    for ledger in read_ledger():
        if not clean(ledger.get("export_zip")):
            continue
        archive = ROOT / "04_trinetx_exports" / ledger["export_zip"]
        if not archive.is_file():
            continue
        panel = "NC10" if ledger["batch"] == "NC10" else "CORE_V2R"
        with zipfile.ZipFile(archive) as zf:
            names = set(zf.namelist())
            readme = zf.read("_Read_Me.txt").decode("utf-8-sig", errors="replace")
            meta, outcomes = parse_readme(readme)
            for index in sorted(outcomes):
                outcome_name = outcomes[index]
                canonical = canonical_outcome(outcome_name, panel)
                row = {
                    "stratum": ledger["stratum"], "window": ledger["window"],
                    "panel": panel, "batch": ledger["batch"],
                    "analysis_id": ledger["analysis_id"],
                    "analysis_name": meta.get("Analysis Name", ""),
                    "outcome_slot": index, "outcome_id": canonical,
                    "outcome_name": outcome_name,
                    "hcos_online": integer(ledger.get("hcos_online")),
                    "hcos_total": integer(ledger.get("hcos_total")),
                    "max_postmatch_asmd": number(ledger.get("max_asmd")),
                    "export_zip": ledger["export_zip"], "sha256": ledger["sha256"],
                    "availability": "privacy_suppressed_or_unavailable",
                }
                moa_member = f"Outcome_{index}_Result_a_MOA_table.csv"
                km_member = f"Outcome_{index}_Result_b_KM_table.csv"
                if moa_member in names:
                    row.update(parse_moa(zf, index))
                    row["availability"] = "available"
                if km_member in names:
                    row.update(parse_km(zf, index))
                row["event_support_gate"] = (
                    "PASS" if row.get("n1", 0) >= 100 and row.get("n2", 0) >= 100
                    and row.get("events1", 0) >= 21 and row.get("events2", 0) >= 21
                    else "FAIL_DESCRIPTIVE_ONLY"
                )
                ph = row.get("proportionality_p")
                row["primary_effect_measure"] = (
                    "RR_PH_FAILED" if ph is not None and ph < 0.05 else
                    "HR" if row.get("hazard_ratio") is not None else
                    "RR" if row.get("risk_ratio") is not None else "UNAVAILABLE"
                )
                row["provisional_hco_warning"] = "YES" if (
                    row["hcos_online"] is not None and row["hcos_total"] is not None
                    and row["hcos_online"] < row["hcos_total"]
                ) else "NO"
                records.append(row)

    core_groups = {}
    for i, row in enumerate(records):
        if row["panel"] == "CORE_V2R" and row["outcome_id"].startswith("C"):
            order = int(row["outcome_id"][1:3])
            if order <= 12:
                core_groups.setdefault((row["stratum"], row["window"]), []).append(i)
    for indices in core_groups.values():
        moa = [records[i].get("association_p") for i in indices]
        km = [records[i].get("logrank_p") for i in indices]
        q_moa, q_km = bh(moa, 12), bh(km, 12)
        q_moa_supported, q_km_supported = bh(moa, sum(x is not None for x in moa)), bh(
            km, sum(x is not None for x in km))
        for pos, i in enumerate(indices):
            records[i]["bh_q_association_12cell"] = q_moa[pos]
            records[i]["bh_q_logrank_12cell"] = q_km[pos]
            records[i]["bh_q_association_supported_only"] = q_moa_supported[pos]
            records[i]["bh_q_logrank_supported_only"] = q_km_supported[pos]

    nc_rows = []
    for key in sorted({(r["stratum"], r["window"], r["analysis_id"])
                       for r in records if r["panel"] == "NC10"}):
        group = [r for r in records if (r["stratum"], r["window"], r["analysis_id"]) == key]
        supported = [r for r in group if r["event_support_gate"] == "PASS" and
                     r.get("risk_ratio") is not None]
        geomean = math.exp(sum(math.log(r["risk_ratio"]) for r in supported) / len(supported)) \
            if supported else None
        direction = 1 if geomean is not None and geomean >= 1 else -1
        coherent_nonnull = sum(
            r.get("association_p") is not None and r["association_p"] < 0.05 and
            ((r["risk_ratio"] >= 1) == (direction >= 1)) for r in supported)
        gate = "BIAS_CHARACTERIZATION_ONLY" if (
            len(supported) >= 5 and coherent_nonnull >= math.ceil(len(supported) / 2)
        ) else "NO_COHERENT_CONTROL_GATE"
        nc_rows.append({
            "stratum": key[0], "window": key[1], "analysis_id": key[2],
            "supported_controls": len(supported), "geometric_mean_rr": geomean,
            "coherent_nominally_nonnull_controls": coherent_nonnull,
            "gate": gate,
        })

    OUT.mkdir(parents=True, exist_ok=True)
    fields = sorted({key for row in records for key in row})
    preferred = [
        "stratum", "window", "panel", "batch", "analysis_name", "analysis_id",
        "outcome_slot", "outcome_id", "outcome_name", "availability",
        "event_support_gate", "primary_effect_measure", "n1", "events1", "risk1",
        "n2", "events2", "risk2", "risk_difference", "rd_ci_low", "rd_ci_high",
        "association_p", "risk_ratio", "rr_ci_low", "rr_ci_high", "hazard_ratio",
        "hr_ci_low", "hr_ci_high", "logrank_p", "proportionality_p",
        "bh_q_association_12cell", "bh_q_logrank_12cell",
        "bh_q_association_supported_only", "bh_q_logrank_supported_only",
        "max_postmatch_asmd", "hcos_online", "hcos_total", "provisional_hco_warning",
        "export_zip", "sha256",
    ]
    fields = preferred + [x for x in fields if x not in preferred]
    with (OUT / "V2R_ESTIMATES.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, extrasaction="ignore")
        writer.writeheader(); writer.writerows(records)
    with (OUT / "V2R_NC10_SUMMARY.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(nc_rows[0]) if nc_rows else
                                ["stratum", "window", "analysis_id", "supported_controls",
                                 "geometric_mean_rr", "coherent_nominally_nonnull_controls", "gate"])
        writer.writeheader(); writer.writerows(nc_rows)
    print(f"Wrote {len(records)} outcome rows from {len(read_ledger())} ledger entries.")


if __name__ == "__main__":
    main()
