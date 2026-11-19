"""Audit every export's actual outcome window from its KM graph time axis vs the label."""
import zipfile, glob, re, csv, os
EXP = {"H1": (31, 366), "H2": (31, 731), "H3": (366, 1826)}
out = []
for zp in sorted(glob.glob("**/raw_zips/*.zip", recursive=True)):
    b = os.path.basename(zp)
    if b.startswith("REJECTED") or "excluded" in zp: continue
    z = zipfile.ZipFile(zp)
    g = [x for x in z.namelist() if re.match(r"Outcome_\d+_Result_b_KM_graph\.csv$", x)]
    lo = hi = None
    for n in g:
        t = z.read(n).decode("utf-8-sig", "replace").splitlines()
        d = [float(r[0]) for r in csv.reader(t) if r and re.match(r"^\d+(\.\d+)?$", r[0])]
        if d: lo = min(d) if lo is None else min(lo, min(d)); hi = max(d) if hi is None else max(hi, max(d))
    h = re.search(r"_(H[123])_", b)
    exp = EXP.get(h.group(1)) if h else None
    ok = "n/a" if exp is None or lo is None else ("OK" if (round(lo), round(hi)) == exp else "MISMATCH")
    out.append((b, h.group(1) if h else "-", lo, hi, ok))
with open("synthesis/V10_WINDOW_AUDIT.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["zip", "label_horizon", "km_min_day", "km_max_day", "verdict"]); w.writerows(out)
for r in out:
    if r[4] != "OK": print(r)
print(sum(r[4] == "OK" for r in out), "OK of", len(out))
