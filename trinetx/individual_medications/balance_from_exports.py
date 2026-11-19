"""Max post-match absolute SMD per DVN/BSEN contrast from Baseline_Patient_Characteristics.csv."""
import zipfile, glob, csv, re, os
import pandas as pd
rows = []
for zp in sorted(glob.glob("**/raw_zips/*.zip", recursive=True)):
    b = os.path.basename(zp)
    if b.startswith(("REJECTED", "D1", "D2")): continue
    z = zipfile.ZipFile(zp)
    if "Baseline_Patient_Characteristics.csv" not in z.namelist(): continue
    t = z.read("Baseline_Patient_Characteristics.csv").decode("utf-8-sig", "replace").splitlines()
    i = next(k for k, l in enumerate(t) if l.startswith("Characteristic ID"))
    data = list(csv.reader(t[i:]))
    h = data[0]
    sm = len(h) - 1
    c1a = [k for k, x in enumerate(h) if x.startswith("Cohort 1 After: Patient Count")][0]
    c2a = [k for k, x in enumerate(h) if x.startswith("Cohort 2 After: Patient Count")][0]
    mx = mxa = 0.0; n = None
    for r in data[1:]:
        try: s = abs(float(r[sm]))
        except: continue
        mx = max(mx, s)
        try: a1, a2 = float(r[c1a] or 0), float(r[c2a] or 0)
        except: a1 = a2 = 0
        if not (a1 <= 10 and a2 <= 10): mxa = max(mxa, s)
        if r[0] == "AI": n = r[c1a]
    rows.append(dict(zip=b, matched_n=n, max_smd=mx, max_smd_excl_privacy_floor=mxa))
D = pd.DataFrame(rows)
D["contrast"] = D.zip.str.extract(r"^(B[1-4](?:_[A-Z]{3})?|C3)")[0]
D.loc[D.zip.str.match(r"B1_H"), "contrast"] = D.zip.str.contains("6ad163c0").map({True: "B1_CBZ", False: "B1_PHT"})
S = D.groupby("contrast").agg(analyses=("zip", "size"), matched_min=("matched_n", "min"), matched_max=("matched_n", "max"),
                               max_smd=("max_smd", "max"), max_smd_excl_floor=("max_smd_excl_privacy_floor", "max")).reset_index()
S.to_csv("synthesis/DVN_BALANCE_FROM_EXPORTS.csv", index=False, float_format="%.4f")
print(S.to_string(index=False))
