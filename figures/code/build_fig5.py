#!/usr/bin/env python3
"""EPI-BONE, Fig. 5: factors associated with fracture and osteoporosis within epilepsy (All of Us).

a  Hazard ratios from the day-180 landmark: reported (blue) and calibrated to the empirical null (dark), with q values.
b  Major fracture by number of chronic conditions (reference: none).
Data: data/F5C_aou_determinants_v3.csv.
"""
import re

from nc_common import (Fig, LinAxis, LogAxis, INK, ACC, BLUE, GREY, LINE, GRID, W, rcsv, est, log, write_source, hci, vci,
                       xaxis, yaxis, header_rule, finish)


def parse_est(s):
    out = {}
    for name, key in (("as reported", "rep"), ("calibrated", "cal")):
        m = re.search(name + r"\s+([\d.]+)\s*\(([\d.]+)-([\d.]+)\)", s)
        out[key] = tuple(float(x) for x in m.groups()) if m else None
    return out


def parse_q(s):
    m1, m2 = re.search(r"q uncalibrated ([\d.]+)", s), re.search(r"q calibrated ([\d.]+|suppressed)", s)
    g = re.search(r"global P ([\d.]+); q ([\d.]+)", s)
    if g:
        return float("0" + g.group(2)), None, float("0" + g.group(1))
    qr = float(m1.group(1)) if m1 else None
    qc = None if not m2 else ("Suppressed" if m2.group(1) == "suppressed" else float(m2.group(1)))
    return qr, qc, None


def qfmt(q):
    if q is None:
        return "–"
    if isinstance(q, str):
        return q
    if q >= 0.995:
        return ">0.99"
    return f"{q:.3f}" if q < 0.1 else f"{q:.2f}"


def figure5():
    C = rcsv("F5C_aou_determinants_v3.csv")
    by = {r["row_label"]: r for r in C}
    f = Fig(330, "Fig. 5 | Factors associated with fracture and osteoporosis within epilepsy")
    f.tag(6, 14, "a", "Treatment, seizure markers, and multimorbidity")
    groups = [("Polytherapy, 2 or more antiseizure medications vs 0 or 1",
               [("Two or more ASMs, major_fracture", "Major fracture"), ("Two or more ASMs, osteoporosis", "Osteoporosis"),
                ("Two or more ASMs, osteopenia", "Osteopenia"), ("Two or more ASMs, vitamin_d_deficiency", "Vitamin D deficiency")]),
              ("Enzyme-inducing medication", [("Enzyme-inducing medication, major_fracture", "Major fracture"),
                                              ("Enzyme-inducing medication, osteoporosis", "Osteoporosis")]),
              ("Seizure-severity markers", [("Baseline status epilepticus, major_fracture", "Baseline status epilepticus, major fracture"),
                                            ("Seizure-record rate, major_fracture", "Seizure-record rate, major fracture"),
                                            ("Seizure-record rate, osteoporosis", "Seizure-record rate, osteoporosis"),
                                            ("Baseline convulsive seizures, major_fracture", "Baseline convulsive seizures, major fracture"),
                                            ("Baseline convulsive seizures, osteoporosis", "Baseline convulsive seizures, osteoporosis")]),
              ("Multimorbidity, per additional chronic condition", [("Per additional comorbid condition, major_fracture", "Major fracture"),
                                                                    ("Per additional comorbid condition, osteoporosis", "Osteoporosis")])]
    lab_x = 150
    ax = LogAxis(158, 262, 0.5, 3.2)
    xr, xc, xq1, xq2 = 272, 326, 384, 414
    yh = 34
    f.T(6, yh, "Exposure, outcome", 5.6, INK, weight=600)
    f.T(xr, yh - 7, "Reported", 5.6, INK, weight=600); f.T(xr, yh, "HR (95% CI)", 5.2, GREY)
    f.T(xc, yh - 7, "Calibrated", 5.6, INK, weight=600); f.T(xc, yh, "HR (95% CI)", 5.2, GREY)
    f.T(xq1, yh - 7, "q value", 5.6, INK, weight=600); f.T(xq1, yh, "Reported", 5.2, GREY); f.T(xq2, yh, "Calibrated", 5.2, GREY)
    header_rule(f, 6, 452, yh + 4)
    y = yh + 5
    ytop = y
    src = []
    for gname, items in groups:
        y += 12.5
        f.T(6, y, gname, 5.4, ACC, weight=600)
        for key, label in items:
            y += 11
            r = by[key]
            e_ = parse_est(r["estimate_ci"]); qr, qc, _ = parse_q(r["q"])
            f.T(lab_x, y + 1.9, label, 5.6, INK, anchor="end")
            e, lo, hi = e_["rep"]
            sig = hci(f, ax, y - 1.3, e, lo, hi, "square", BLUE, 2.0)
            f.T(xr, y + 1.9, est(e, lo, hi), 5.2, BLUE if sig else GREY, weight=600 if sig else 400)
            cal = e_["cal"]
            if cal:
                ce, cl, cu = cal
                csig = cl > 1 or cu < 1
                col = INK if csig else GREY
                f.L(ax(max(cl, ax.lo)), y + 2.7, ax(min(cu, ax.hi)), y + 2.7, col, 1.2)
                f.mark("square", ax(ce), y + 2.7, INK, csig, 1.6)
                f.T(xc, y + 1.9, est(ce, cl, cu), 5.2, INK if csig else GREY, weight=600 if csig else 400)
            else:
                f.T(xc, y + 1.9, "Not available" if "not available" in r["estimate_ci"] else "Not calibrated", 5.2, GREY, italic=True)
            b1 = isinstance(qr, float) and qr < 0.05
            b2 = isinstance(qc, float) and qc < 0.05
            f.T(xq1, y + 1.9, qfmt(qr), 5.2, INK if b1 else GREY, weight=600 if b1 else 400)
            f.T(xq2, y + 1.9, qfmt(qc), 5.2, INK if b2 else GREY, weight=600 if b2 else 400)
            log("Fig5", "a", key, f"reported {est(e, lo, hi)}; calibrated {est(*cal) if cal else 'none'}; q {qfmt(qr)} / {qfmt(qc)}", r["source"])
            src.append([gname, label, f"{e:.2f}", f"{lo:.2f}", f"{hi:.2f}"] + ([f"{v:.2f}" for v in cal] if cal else ["", "", ""]) + [qfmt(qr), qfmt(qc), r["n_events"]])
    ybot = y + 10
    xaxis(f, ax, ybot, (0.5, 1, 2, 3), "Hazard ratio (log scale)", grid_top=ytop, null=1)
    yk = ybot + 32
    f.mark("square", 9, yk - 2, BLUE, True, 2.0); f.T(15, yk, "Reported, 95% CI excludes 1", 5.6)
    f.mark("square", 104, yk - 2, INK, True, 1.6); f.T(110, yk, "Calibrated to the empirical null, 95% CI excludes 1", 5.6)
    f.mark("square", 252, yk - 2, BLUE, False, 2.0); f.T(258, yk, "95% CI includes 1", 5.6)
    write_source("Fig5a_determinants.csv", ["group", "row", "reported_hr", "reported_ci_low", "reported_ci_high", "calibrated_hr",
                                            "calibrated_ci_low", "calibrated_ci_high", "q_reported", "q_calibrated", "n_events"], src)

    # ------------------------------------------------------------------ b: number of chronic conditions
    ybt = yk + 24
    f.tag(6, ybt, "b", "Major fracture by number of chronic conditions")
    x0, x1, pt, pb = 60, 250, ybt + 16, ybt + 86
    axy = LogAxis(pb, pt, 0.8, 3.6)
    yaxis(f, axy, x0, (1, 2, 3), grid_to=x1, null=1)
    f.L(x0, pb, x1, pb, INK, 0.9)
    cats = [(None, "None"), ("1 vs 0 chronic conditions, major_fracture", "1"), ("2 vs 0 chronic conditions, major_fracture", "2"),
            ("3 or more vs 0 chronic conditions, major_fracture", "3 or more")]
    pts, src = [], []
    for k, (key, lab) in enumerate(cats):
        x = x0 + 26 + k * 46
        f.L(x, pb, x, pb + 3.5, INK, 0.8); f.T(x, pb + 10.6, lab, 5.7, INK, anchor="middle")
        if key is None:
            f.mark("square", x, axy(1), GREY, False, 2.0)
            f.T(x, axy(1) - 5, "Reference", 5.2, GREY, anchor="middle", italic=True)
            pts.append((x, axy(1)))
            src.append(["0 (reference)", "1", "", ""])
            continue
        r = by[key]
        e, lo, hi = parse_est(r["estimate_ci"])["rep"]
        pts.append((x, axy(e)))
        src.append([lab, f"{e:.2f}", f"{lo:.2f}", f"{hi:.2f}"])
    f.poly(pts, stroke=GREY, sw=0.8)
    f.mark("square", pts[0][0], pts[0][1], GREY, False, 2.0)
    for (key, lab), (x, _) in zip(cats[1:], pts[1:]):
        e, lo, hi = parse_est(by[key]["estimate_ci"])["rep"]
        sig = vci(f, x, axy, e, lo, hi, "square", BLUE, 2.0)
        f.T(x, axy(hi) - 3.4, est(e, lo, hi), 5.2, BLUE if sig else GREY, anchor="middle", weight=600 if sig else 400)
        log("Fig5", "b", key, est(e, lo, hi), by[key]["source"])
    _, _, gp = parse_q(by[cats[1][0]]["q"])
    f.T((x0 + x1) / 2, pb + 20.5, "Chronic conditions at the landmark, n", 6.5, INK, anchor="middle")
    f.T(36, (pt + pb) / 2, "Hazard ratio (log scale)", 6.5, INK, anchor="middle", rotate=-90)
    f.T(272, pt + 8, f"Global P = {gp:.3f}; not calibrated.", 5.6, INK)
    f.T(272, pt + 18, "Conditions counted: alcohol use disorder, cancer, cardiovascular disease,", 5.2, GREY, italic=True)
    f.T(272, pt + 25, "cerebrovascular disease, chronic kidney disease, depression, diabetes,", 5.2, GREY, italic=True)
    f.T(272, pt + 32, "obesity, and tobacco use.", 5.2, GREY, italic=True)
    log("Fig5", "b", "global test", f"Global P = {gp:.3f}", by[cats[1][0]]["source"])
    write_source("Fig5b_number_of_conditions.csv", ["chronic_conditions", "hazard_ratio", "ci_low", "ci_high"], src)
    return f


if __name__ == "__main__":
    finish(figure5(), "Figure5")
