#!/usr/bin/env python3
"""EPI-BONE, Fig. 3: injury or fragile bone.

a  Fracture and injury sites after bone-enriched matching: risk ratios at days 1-90 and days 366-1095.
b  Densitometry performed vs osteoporosis recorded (risk ratios, days 366-1095) across cohorts.
c  Adults with densitometry in the 3 years before diagnosis: hazard ratios, days 366-1095.
d  Negative-control outcomes: the 10 control risk ratios and their geometric mean in each cohort.
Data: data/F3_pathways_ascertainment.csv, data/F3_negative_controls.csv.
"""
import math

from nc_common import (Fig, LogAxis, INK, ACC, BLUE, GREY, LINE, GRID, TINT, WHITE, W, rcsv, fv, est, log,
                       write_source, hci, xaxis, yaxis, header_rule, finish)

NEUTRAL = "#f2f4f6"
BE = "T2_ALL_INCIDENT"


def limb(p0, p1, w0, w1):
    (x0, y0), (x1, y1) = p0, p1
    d = math.hypot(x1 - x0, y1 - y0)
    nx, ny = -(y1 - y0) / d, (x1 - x0) / d
    return [(x0 + nx * w0 / 2, y0 + ny * w0 / 2), (x1 + nx * w1 / 2, y1 + ny * w1 / 2),
            (x1 - nx * w1 / 2, y1 - ny * w1 / 2), (x0 - nx * w0 / 2, y0 - ny * w0 / 2)]


def body(f, cx, top):
    """Schematic front-view figure, 168 pt tall; outline first, then fills, so the parts read as one shape."""
    y = lambda v: top + v  # noqa: E731
    polys = [[(cx - 3.5, y(20)), (cx + 3.5, y(20)), (cx + 3.5, y(29)), (cx - 3.5, y(29))],
             [(cx - 19, y(28)), (cx + 19, y(28)), (cx + 21, y(34)), (cx + 16, y(64)), (cx + 15, y(84)), (cx + 18, y(97)),
              (cx - 18, y(97)), (cx - 15, y(84)), (cx - 16, y(64)), (cx - 21, y(34))]]
    circles = [(cx, y(11), 10.0)]
    for s in (-1, 1):
        polys += [limb((cx + s * 20, y(32)), (cx + s * 26, y(66)), 7.5, 6.5), limb((cx + s * 26, y(66)), (cx + s * 31, y(95)), 6.5, 5.0),
                  limb((cx + s * 9.5, y(95)), (cx + s * 10.5, y(132)), 15, 10), limb((cx + s * 10.5, y(132)), (cx + s * 10.5, y(163)), 9.5, 6.5),
                  [(cx + s * 7, y(162)), (cx + s * 16, y(162)), (cx + s * 18, y(168)), (cx + s * 7, y(168))]]
        circles += [(cx + s * 26, y(66), 3.4), (cx + s * 31.6, y(99.5), 3.6), (cx + s * 10.5, y(132), 5.0)]
    for pts in polys:
        f.poly(pts, stroke=LINE, sw=1.6, fill=NEUTRAL, closed=True)
    for x, yy, r in circles:
        f.C(x, yy, r, NEUTRAL, LINE, 1.6)
    for pts in polys:
        f.poly(pts, stroke="none", fill=NEUTRAL, closed=True)
    for x, yy, r in circles:
        f.C(x, yy, r - 0.4, NEUTRAL, NEUTRAL, 0.5)


def figure3():
    P, NC = rcsv("F3_pathways_ascertainment.csv"), rcsv("F3_negative_controls.csv")
    f = Fig(360, "Fig. 3 | Injury, skeletal fragility and bone-health investigation after a first recorded diagnosis of epilepsy")

    def get(code, win, outcome):
        return next(r for r in P if r["stratum_code"] == code and r["window_days"] == win and r["outcome"] == outcome)

    def rr(r):
        return fv(r["risk_ratio"]), fv(r["rr_ci_low"]), fv(r["rr_ci_high"])

    # ------------------------------------------------------------------ a: sites
    f.tag(6, 14, "a", "Fracture and injury sites after bone-enriched matching")
    cx, top = 48, 26
    body(f, cx, top)
    lab_x, c1, c2 = 92, 250, 274
    ax = LogAxis(184, 242, 1, 6)
    yh = 27
    f.T(lab_x, yh, "Site", 5.6, INK, weight=600)
    f.T(c1, yh - 7, "Risk ratio", 5.6, INK, weight=600)
    f.T(c1, yh, "1–90", 5.2, GREY); f.T(c2, yh, "366–1095", 5.2, GREY)
    header_rule(f, lab_x, 300, yh + 4)
    sites = [("Skull and facial fracture", "Skull or facial fracture", "typical seizure injury", (cx + 3, top + 9), 46),
             ("Shoulder dislocation", "Shoulder dislocation", "typical seizure injury", (cx + 20, top + 33), 68),
             ("Clinical vertebral fracture", "Clinical vertebral fracture", "osteoporosis-related site", (cx, top + 58), 90),
             ("Distal forearm fracture", "Distal forearm fracture", "classic fragility site", (cx + 30.2, top + 93), 112),
             ("Hip / proximal femur fracture", "Hip or proximal femur fracture", "osteoporosis-related site", (cx + 10, top + 100), 134)]
    comps = [("Conventional major osteoporotic fracture composite", "Major osteoporotic fracture", "hip, vertebra, humerus, forearm", None, 162),
             ("Broad major-fracture composite", "Broad fracture composite", "plus spine, thorax, pelvis, limbs", None, 184)]
    src = []
    for key, label, sub, mk, y in sites + comps:
        if mk:
            f.poly([mk, (84, y), (lab_x - 3, y)], stroke=INK, sw=0.6)
            f.C(mk[0], mk[1], 2.6, BLUE, WHITE, 0.8)
        f.T(lab_x, y - 0.6, label, 5.6, INK, weight=600)
        f.T(lab_x, y + 6.2, sub, 5.2, GREY, italic=True)
        for win, dy, col, xcol in (("1-90", -2.7, INK, c1), ("366-1095", 2.7, BLUE, c2)):
            r = get(BE, win, key)
            e, lo, hi = rr(r)
            hci(f, ax, y + dy, e, lo, hi, "circle", col, 2.0)
            f.T(xcol, y + 1.9, f"{e:.2f}", 5.4, col, weight=600)
            log("Fig3", "a", f"{label}, days {win}", f"{e:.2f}", r["source"])
            src.append([label, win, f"{e:.4f}", f"{lo:.4f}", f"{hi:.4f}", r["n_epilepsy"], r["n_migraine"], r["events_epilepsy"], r["events_migraine"]])
    f.L(lab_x, 148, 300, 148, GRID, 0.6)
    ybot = 196
    xaxis(f, ax, ybot, (1, 2, 4), "Risk ratio (log scale)", grid_top=yh + 5, null=1)
    f.mark("circle", 96, ybot + 9, INK, True, 2.0); f.T(101, ybot + 11, "Days 1–90", 5.6)
    f.mark("circle", 96, ybot + 18, BLUE, True, 2.0); f.T(101, ybot + 20, "Days 366–1095", 5.6)
    write_source("Fig3a_sites_bone_enriched.csv", ["outcome", "window_days", "risk_ratio", "ci_low", "ci_high", "n_epilepsy",
                                                   "n_migraine", "events_epilepsy", "events_migraine"], src)

    # ------------------------------------------------------------------ b: testing vs diagnosis
    f.tag(318, 14, "b", "Osteoporosis recorded vs densitometry")
    x0, x1, yt, yb = 348, 500, 30, 172
    axx, axy = LogAxis(x0, x1, 0.5, 2), LogAxis(yb, yt, 0.8, 2)
    f.R(x0, yt, axx(1) - x0, axy(1) - yt, TINT, "none")
    yaxis(f, axy, x0, (0.8, 1, 1.5, 2), grid_to=x1)
    xaxis(f, axx, yb, (0.5, 0.75, 1, 1.5, 2), "Densitometry performed, risk ratio", grid_top=yt)
    f.L(axx(1), yt, axx(1), yb, INK, 0.8, "2.4 2.2"); f.L(x0, axy(1), x1, axy(1), INK, 0.8, "2.4 2.2")
    f.T(332, (yt + yb) / 2, "Osteoporosis recorded, risk ratio", 6.5, INK, anchor="middle", rotate=-90)
    f.T(x0 + 4, yt + 7.5, "Recorded more often,", 5.2, ACC, italic=True); f.T(x0 + 4, yt + 13.8, "tested less often", 5.2, ACC, italic=True)
    DXA, OST = "DXA performed", "Osteoporosis without current pathological fracture"
    strata = [("T2_ALL_INCIDENT", ["All adults"], (-5, -4.5, "end")), ("T3_FEMALE", ["Women"], (-5, -4.5, "end")),
              ("T3_FEMALE_50PLUS", ["Women", "aged ≥50 y"], (6, 9.0, "start")), ("T3_MALE", ["Men"], (5, -4.5, "start")),
              ("S35_AGE65", ["Aged ≥65 y"], (-3, 18.5, "middle"))]
    src = []
    for code, label, (dx, dy, anchor) in strata:
        d, o = get(code, "366-1095", DXA), get(code, "366-1095", OST)
        (xe, xl, xh), (ye, yl, yh2) = rr(d), rr(o)
        f.L(axx(xl), axy(ye), axx(xh), axy(ye), INK, 1.2); f.L(axx(xe), axy(yl), axx(xe), axy(yh2), INK, 1.2)
        f.mark("circle", axx(xe), axy(ye), BLUE, True, 2.6)
        for j, part in enumerate(label):
            f.T(axx(xe) + dx, axy(ye) + dy + 6.6 * j, part, 5.6, INK, anchor=anchor, weight=600)
        label = " ".join(label)
        log("Fig3", "b", label, f"point at densitometry RR {est(xe, xl, xh)}, osteoporosis RR {est(ye, yl, yh2)} (no number printed)", d["source"])
        src.append([label, f"{xe:.4f}", f"{xl:.4f}", f"{xh:.4f}", f"{ye:.4f}", f"{yl:.4f}", f"{yh2:.4f}"])
    write_source("Fig3b_densitometry_vs_osteoporosis.csv", ["cohort", "densitometry_rr", "densitometry_ci_low", "densitometry_ci_high",
                                                            "osteoporosis_rr", "osteoporosis_ci_low", "osteoporosis_ci_high"], src)

    # ------------------------------------------------------------------ c: densitometry-tested cohort
    yc = 242
    f.tag(6, yc, "c", "Adults with densitometry before diagnosis")
    axc = LogAxis(124, 204, 0.7, 3)
    vx = 211
    yh = yc + 12
    f.T(6, yh + 3, "Outcome, days 366–1095", 5.6, INK, weight=600); f.T(vx, yh + 3, "HR (95% CI)", 5.6, INK, weight=600)
    header_rule(f, 6, 256, yh + 7)
    rows = [("Osteoporosis without current pathological fracture", "Osteoporosis recorded", False),
            ("Broad major-fracture composite", "Broad fracture composite", False),
            ("Head or face injury", "Head or face injury", False), ("Falls", "Falls", False),
            ("Conventional major osteoporotic fracture composite", "Major osteoporotic fracture", True),
            ("Vitamin D measurement", "Vitamin D measured (testing)", False)]
    y = yh + 9
    src = []
    for key, label, few in rows:
        y += 11
        r = get("T1_DXA3Y", "366-1095", key)
        e, lo, hi = fv(r["hazard_ratio"]), fv(r["hr_ci_low"]), fv(r["hr_ci_high"])
        f.T(118, y + 1.9, label, 5.6, GREY if few else INK, anchor="end", italic=few)
        sig = hci(f, axc, y, e, lo, hi, "square", BLUE, 2.0)
        f.T(vx, y + 1.9, est(e, lo, hi), 5.4, BLUE if sig else GREY, weight=600 if sig else 400)
        log("Fig3", "c", label, f"HR {est(e, lo, hi)}", r["source"])
        src.append([label, f"{e:.4f}", f"{lo:.4f}", f"{hi:.4f}", r["n_epilepsy"], r["n_migraine"], r["events_epilepsy"], r["events_migraine"], r["event_support_gate"]])
    ybc = y + 9
    xaxis(f, axc, ybc, (0.7, 1, 1.5, 2, 3), "Hazard ratio (log scale)", grid_top=yh + 8, null=1)
    write_source("Fig3c_densitometry_tested_cohort.csv", ["outcome", "hazard_ratio", "ci_low", "ci_high", "n_epilepsy", "n_migraine",
                                                          "events_epilepsy", "events_migraine", "event_support_gate"], src)

    # ------------------------------------------------------------------ d: negative controls
    f.tag(274, yc, "d", "Negative-control outcomes")
    axd = LogAxis(372, 462, 0.4, 1.3)
    yh = yc + 12
    f.T(274, yh + 3, "Cohort", 5.6, INK, weight=600); f.T(468, yh + 3, "Mean", 5.6, INK, weight=600)
    header_rule(f, 274, W - 6, yh + 7)
    cohorts = [("A06", "Primary cohort, days 1–365"), ("T2_ALL_INCIDENT", "Bone-enriched cohort"), ("T3_FEMALE", "Women"),
               ("T3_FEMALE_50PLUS", "Women aged ≥50 y"), ("T3_MALE", "Men")]
    y = yh + 9
    src = []
    for code, label in cohorts:
        y += 13.2
        rows_nc = [r for r in NC if r["stratum_code"] == code]
        vals = [fv(r["risk_ratio"]) for r in rows_nc]
        assert len(vals) == 10 and all(v is not None for v in vals), code
        gm = math.exp(sum(math.log(v) for v in vals) / len(vals))
        stated = fv(rows_nc[0]["stratum_geometric_mean_rr"])
        assert abs(gm - stated) < 0.0015, (code, gm, stated)
        f.T(366, y + 1.9, label, 5.6, INK, anchor="end")
        for k, v in enumerate(sorted(vals)):
            f.C(axd(v), y + (1.7 if k % 2 else -1.7), 1.5, WHITE, GREY, 0.7)
        f.mark("diamond", axd(stated), y, BLUE, True, 2.3)
        f.T(468, y + 1.9, f"{stated:.2f}", 5.4, BLUE, weight=600)
        log("Fig3", "d", label, f"{stated:.2f}", rows_nc[0]["source"])
        for r in rows_nc:
            src.append([label, r["window_days"], r["outcome"], r["risk_ratio"], r["rr_ci_low"], r["rr_ci_high"], f"{stated:.3f}"])
    ybd = ybc
    xaxis(f, axd, ybd, (0.5, 0.75, 1), "Risk ratio (log scale)", grid_top=yh + 8, null=1)
    f.C(278, ybd + 28, 1.5, WHITE, GREY, 0.7); f.T(283, ybd + 30, "One control outcome", 5.6)
    f.mark("diamond", 346, ybd + 28, BLUE, True, 2.3); f.T(352, ybd + 30, "Geometric mean of the 10 controls", 5.6)
    f.mark("square", 9, ybd + 28, BLUE, True, 2.0); f.T(15, ybd + 30, "95% CI excludes 1", 5.6)
    f.mark("square", 82, ybd + 28, BLUE, False, 2.0); f.T(88, ybd + 30, "95% CI includes 1", 5.6)
    write_source("Fig3d_negative_controls.csv", ["cohort", "window_days", "control_outcome", "risk_ratio", "ci_low", "ci_high",
                                                 "cohort_geometric_mean"], src)
    return f


if __name__ == "__main__":
    finish(figure3(), "Figure3")
