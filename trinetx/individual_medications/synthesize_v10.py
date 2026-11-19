"""EPI-BONE v10 offline synthesis of the Codex live runs (Parts B, C).
Inputs: V10_ALL_PARSED_CELLS.csv (parse_all_exports.py), V10_WINDOW_AUDIT.csv,
screen-captured B3 H1 RF10 (export not captured; partB_live/B3_H1_RF10_CORRECTED_RESULTS_2026-10-15.md)."""
import pandas as pd, numpy as np
from scipy.stats import norm

d = pd.read_csv("V10_ALL_PARSED_CELLS.csv")
d["oc"] = d.outcome.str.replace("DVN_", "").str.extract(r"^([A-Z]+\d+)")[0]
# QC relabel: OXC "H1" RF10 export truly covers days 366-1825 (window audit) -> H3, descriptive
d.loc[d.zip_file.str.startswith("B2_OXC_H1_RF10") & d.zip_file.str.contains("2026-10-18"), "qc_note"] = "RELABELLED_H1_TO_H3_KM_AXIS_366_1826"
# screen-captured B3 H1 RF10 (corrected F7 card), live result verified, export unavailable
scr = [("R1", 2905, 53, 2868, 41, 1.276, .852, 1.912, .2359, 1.219, .811, 1.833, .3397),
       ("R2", 2324, 148, 2187, 130, 1.071, .853, 1.346, .5539, 1.016, .803, 1.286, .8938),
       ("F1", 2930, 40, 2938, 19, 2.111, 1.226, 3.636, .0058, 2.028, 1.175, 3.502, .0096),
       ("F2", 2601, 96, 2584, 76, 1.255, .933, 1.687, .1318, 1.200, .888, 1.621, .2345),
       ("F4", 3062, 23, 3033, 23, .991, .557, 1.762, .9742, .940, .528, 1.676, .8349),
       ("F7", 3019, 26, 3014, 24, 1.082, .622, 1.879, .7809, 1.024, .588, 1.784, .9319)]
add = pd.DataFrame([dict(contrast="B3", horizon="H1", panel="RF10", oc=o, outcome="DVN_" + o, at_risk1=a1, events1=e1,
        at_risk2=a2, events2=e2, rr=rr, rr_lo=l, rr_hi=h, rd_p=p, hr=hr, hr_lo=hl, hr_hi=hh, logrank_p=lp,
        matched_n1=3144, matched_n2=3144, status="SCREEN_CAPTURED_EXPORT_UNAVAILABLE") for o, a1, e1, a2, e2, rr, l, h, p, hr, hl, hh, lp in scr])
# B3 H1 RF10 now exported (recovery 21 Oct) and B3 H2 RF10 run on retry: screen values no longer needed
d.to_csv("DVN_RESULTS_ALL_CELLS.csv", index=False)

# ---- Holm across the 4 H-drugs per key outcome at H2 (RD p and log-rank p separately)
H = ["B1_CBZ", "B1_PHT", "B1_PHB", "B1_VPA"]; KEY = ["R1", "F2", "R2", "M1"]
def holm(p):
    p = np.asarray(p, float); o = np.argsort(p); adj = np.empty(len(p)); run = 0
    for i, k in enumerate(o): run = max(run, (len(p) - i) * p[k]); adj[k] = min(1, run)
    return adj
rows = []
for oc in KEY:
    g = d[(d.contrast.isin(H)) & (d.horizon == "H2") & (d.oc == oc)].set_index("contrast").reindex(H)
    for col in ("rd_p", "logrank_p"):
        pv = g[col].fillna(1.0).values  # suppressed cell stays in denominator as p=1
        for c, p, a in zip(H, g[col].values, holm(pv)):
            rows.append(dict(outcome=oc, horizon="H2", test=col, contrast=c, raw_p=p, holm_p=a,
                             rr=g.loc[c, "rr"], rr_lo=g.loc[c, "rr_lo"], rr_hi=g.loc[c, "rr_hi"], hr=g.loc[c, "hr"]))
M = pd.DataFrame(rows); M.to_csv("DVN_MULTIPLICITY.csv", index=False)

# ---- NC10 geometric means (selection/ascertainment diagnostic)
nc = d[d.panel.astype(str).str.startswith("NC10") & d.rr.notna()]
G = nc.groupby(["contrast", "horizon"], dropna=False).agg(n_estimable=("rr", "size"), gm_rr=("rr", lambda x: float(np.exp(np.log(x).mean())))).reset_index()
G.to_csv("DVN_NC10_GEOMEAN.csv", index=False)

# ---- readings per H-drug (prespecified)
def cell(c, h, oc, f="rr"):
    r = d[(d.contrast == c) & (d.horizon == h) & (d.oc == oc)]
    return r[f].iloc[0] if len(r) and pd.notna(r[f].iloc[0]) else np.nan
rd = []
for c in H:
    sup = []
    for oc in ("R1", "F2"):
        hp = M[(M.contrast == c) & (M.outcome == oc) & (M.test == "rd_p")].holm_p.iloc[0]
        if cell(c, "H2", oc) > 1 and hp < .05 and cell(c, "H1", oc) > 1: sup.append(oc)
    anyup = any(cell(c, "H2", oc) > 1 for oc in ("R1", "F2"))
    nom = any(cell(c, "H2", oc, "rd_p") < .05 and cell(c, "H2", oc) > 1 for oc in ("R1", "F2"))
    reading = "HIGHER_RISK_SUPPORTED" if sup else ("DIRECTIONAL_ONLY" if anyup else "NOT_SUPPORTED")
    mort = cell(c, "H2", "D1", "hr"); gm = G[(G.contrast == c) & (G.horizon == "H2")].gm_rr
    gm = gm.iloc[0] if len(gm) else np.nan
    cav = [] 
    if mort > 1.2: cav.append(f"mortality HR {mort:.2f}")
    if pd.notna(gm) and not (.8 <= gm <= 1.25): cav.append(f"NC10 GM {gm:.2f}")
    rd.append(dict(contrast=c, reading=reading, nominal_fracture_H2=nom, supported_outcomes=";".join(sup),
                   matched_per_arm=cell(c, "H2", "R1", "matched_n1"), mortality_hr_H2=mort, nc10_gm_H2=gm, caveat="; ".join(cav)))
R = pd.DataFrame(rd); R.to_csv("DVN_READINGS.csv", index=False)

# ---- Figure 4B forest data (same columns as interim file)
lab = {"R1": "Major osteoporotic fracture", "F2": "Broad fracture", "R2": "Head or face injury", "M1": "Falls", "D1": "All-cause mortality"}
fam = {"B1_CBZ": "hypothesis", "B1_PHT": "hypothesis", "B1_PHB": "hypothesis", "B1_VPA": "hypothesis",
       "B2_OXC": "descriptive", "B2_TPM": "descriptive", "B2_ZNS": "descriptive", "B3": "class (EIA3) vs NEUTRAL3", "B4": "PHT, loading-excluded"}
fr = []
for c in fam:
    for oc in lab:
        r = d[(d.contrast == c) & (d.horizon == "H2") & (d.oc == oc)]
        if not len(r): continue
        r = r.iloc[0]; met = "HR" if oc == "D1" else "RR"
        est, lo, hi = (r.hr, r.hr_lo, r.hr_hi) if met == "HR" else (r.rr, r.rr_lo, r.rr_hi)
        hp = M[(M.contrast == c) & (M.outcome == oc) & (M.test == "rd_p")].holm_p
        fr.append(dict(drug=c.split("_")[-1], family=fam[c], outcome=lab[oc], window_days="31-730", metric=met,
                       estimate=est, ci_low=lo, ci_high=hi, p=r.rd_p, holm_p=hp.iloc[0] if len(hp) else np.nan,
                       matched_per_arm=r.matched_n1, status=r.status))
F = pd.DataFrame(fr); F.to_csv("F4B_DRUG_VS_NEUTRAL3_FOREST.csv", index=False, float_format="%.4g")
pd.set_option("display.width", 220)
print(R.to_string(index=False))
print(M[M.test == "rd_p"].pivot(index="contrast", columns="outcome", values="holm_p").round(3))
print(G[G.horizon == "H2"].round(2).to_string(index=False))
