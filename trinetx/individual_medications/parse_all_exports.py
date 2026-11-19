"""Parse every validated V10 TriNetX export (Parts B and C) into one long table.
Read-only on the ZIPs; aggregate cohort-level TriNetX outputs only."""
import zipfile, re, io, csv, glob, os, sys
import pandas as pd
ROOT = os.path.dirname(os.path.abspath(__file__)) + "/.."
DRUG_BY_ID = {"6ad163c05d1de62a1c62cd22": "CBZ", "6ad20c58585251d70f910f22": "PHT"}

def blocks(txt):
    out = {}; key = None
    for line in txt.splitlines():
        s = line.strip().strip('﻿')
        if s in ('', '" "'): continue
        if s in ("Cohort Statistics", "Risk Difference", "Risk Ratio", "Odds Ratio", "Log-Rank Test", "Hazard Ratio", "Proportionality", "Notes"):
            key = s; out[key] = []; continue
        if key: out[key].append(s)
    return out

def rows(b, k):
    return list(csv.reader(b.get(k, [])))

def num(x):
    try: return float(x)
    except: return None

recs = []
zips = sorted(glob.glob(ROOT + "/**/raw_zips/*.zip", recursive=True))
for zp in zips:
    base = os.path.basename(zp)
    if base.startswith("REJECTED") or "excluded_duplicate" in zp: continue
    m = re.match(r"(.+?)__([0-9a-f]{24})__(\d{4}-\d{2}-\d{2})(__CSV_ONLY_FALLBACK)?\.zip", base)
    if not m: print("skip", base, file=sys.stderr); continue
    label, aid, date, fb = m.groups()
    label = label.replace("_RECOVERY", "")
    if label.startswith("D"): continue  # Part D (EIC/PNU) recoveries: verified vs screen values in RECOVERY_LOG.md
    if label == "B2_OXC_H1_RF10" and date == "2026-10-18": label = "B2_OXC_H3_RF10"  # window audit: ran days 366-1825
    parts = label.split("_")
    part = parts[0]
    if part == "B1" and parts[1] in ("H1", "H2", "H3"):
        label = f"B1_{DRUG_BY_ID[aid]}_" + "_".join(parts[1:])
    z = zipfile.ZipFile(zp)
    names = z.namelist()
    readme = z.read("_Read_Me.txt").decode("utf-8-sig", "replace") if "_Read_Me.txt" in names else ""
    onames = dict(re.findall(r"Outcome (\d+) Name: (.+)", readme))
    c1 = re.search(r"Cohort 1: (.+)", readme); c2 = re.search(r"Cohort 2: (.+)", readme)
    hco = re.search(r"HCOs Online for Analysis: (\d+)", readme)
    # matched N from follow-up table
    mN1 = mN2 = fu1 = fu2 = None
    fn = [n for n in names if n.startswith("Follow_up_time_metrics_matched_results_table")]
    if fn:
        t = z.read(fn[0]).decode("utf-8-sig", "replace")
        rr = [r for r in csv.reader(io.StringIO(t)) if r and r[0] in ("1", "2")]
        if len(rr) >= 2:
            fu1, fu2 = num(rr[0][2]), num(rr[1][2])
    nums = sorted({int(x) for x in re.findall(r"Outcome_(\d+)_Result", " ".join(names))} | {int(k) for k in onames})
    for k in nums:
        r = dict(zip_file=base, part=part, run=label, analysis_id=aid, export_date=date, csv_fallback=bool(fb),
                 cohort1=c1.group(1).strip() if c1 else None, cohort2=c2.group(1).strip() if c2 else None,
                 hco_online=hco.group(1) if hco else None, outcome_no=k, outcome=onames.get(str(k), f"Outcome {k}"),
                 matched_n1=mN1, matched_n2=mN2, fu_mean1=fu1, fu_mean2=fu2)
        moa = [n for n in names if n.startswith(f"Outcome_{k}_Result_a_MOA_table") and n.endswith(".csv")]
        km = [n for n in names if n.startswith(f"Outcome_{k}_Result_b_KM_table") and n.endswith(".csv")]
        if not moa:
            r["status"] = "SUPPRESSED_OR_UNAVAILABLE"; recs.append(r); continue
        b = blocks(z.read(moa[0]).decode("utf-8-sig", "replace"))
        ex = re.search(r"(\d+) patients in Cohort 1 and (\d+) patients in Cohort 2 were excluded", " ".join(b.get("Notes", [])))
        e1, e2 = (int(ex.group(1)), int(ex.group(2))) if ex else (0, 0)
        r.update(excl_prior1=e1, excl_prior2=e2)
        cs = [x for x in rows(b, "Cohort Statistics") if x and x[0] in ("1", "2")]
        if len(cs) == 2:
            r.update(at_risk1=num(cs[0][2]), events1=num(cs[0][3]), risk1=num(cs[0][4]),
                     at_risk2=num(cs[1][2]), events2=num(cs[1][3]), risk2=num(cs[1][4]),
                     matched_n1=num(cs[0][2]) + e1, matched_n2=num(cs[1][2]) + e2)
        rd = rows(b, "Risk Difference")
        if len(rd) > 1: r.update(rd=num(rd[1][0]), rd_lo=num(rd[1][1]), rd_hi=num(rd[1][2]), rd_p=num(rd[1][4]))
        rr_ = rows(b, "Risk Ratio")
        if len(rr_) > 1: r.update(rr=num(rr_[1][0]), rr_lo=num(rr_[1][1]), rr_hi=num(rr_[1][2]))
        if km:
            kb = blocks(z.read(km[0]).decode("utf-8-sig", "replace"))
            lr = rows(kb, "Log-Rank Test"); hr = rows(kb, "Hazard Ratio"); ph = rows(kb, "Proportionality")
            if len(lr) > 1: r["logrank_p"] = num(lr[1][2])
            if len(hr) > 1: r.update(hr=num(hr[1][0]), hr_lo=num(hr[1][1]), hr_hi=num(hr[1][2]))
            if len(ph) > 1: r["ph_p"] = num(ph[1][2])
        r["status"] = "ESTIMABLE"
        recs.append(r)
D = pd.DataFrame(recs)
# horizon & panel from label
D["horizon"] = D.run.str.extract(r"_(H[123])_")[0]
D["panel"] = D.run.str.extract(r"_(RF10|MECH10|AD5|NC10|CORE7|NC10_RECOVERY1)$")[0].fillna(D.run.str.extract(r"_(CORE7|NC10)")[0])
D["contrast"] = D.run.str.replace(r"_(H[123])_.*$", "", regex=True).str.replace(r"_(CORE7|NC10.*)$", "", regex=True)
# dedupe: prefer full export over CSV fallback
D = D.sort_values(["run", "outcome", "csv_fallback"]).drop_duplicates(["run", "outcome"], keep="first")
D.to_csv("V10_ALL_PARSED_CELLS.csv", index=False)
print(D.groupby(["contrast", "horizon", "panel"], dropna=False).size().to_string())
print(len(D), "rows from", D.zip_file.nunique(), "zips")
