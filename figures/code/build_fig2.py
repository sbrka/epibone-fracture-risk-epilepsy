#!/usr/bin/env python3
"""EPI-BONE, Fig. 2: when the fracture and injury excess arises.

a  Kaplan-Meier cumulative incidence, days 1-90, epilepsy vs migraine (TriNetX analysis A01, daily curves).
b  Risk ratio in each follow-up window, five outcomes (square = hazard ratio).
c  Additional events per 1,000 person-years in each window (risk difference scaled to person-years).
d  Replication of the broad fracture composite over 3 years (TriNetX and All of Us).
Data: data/F2A_km_days1_90.csv, data/F2_burden_by_window.csv, data/F2_replication.csv.
"""
from nc_common import (Fig, LinAxis, LogAxis, INK, ACC, BLUE, GREY, GRID, TINT, W, rcsv, fv, est, log,
                       write_source, hci, vci, xaxis, yaxis, steps, header_rule, finish)

BAND_GREY = "#f2f4f6"      # canonical neutral for the comparator interval


def figure2():
    KM, B, R = rcsv("F2A_km_days1_90.csv"), rcsv("F2_burden_by_window.csv"), rcsv("F2_replication.csv")
    f = Fig(520, "Fig. 2 | Timing of fracture and injury risk after a first recorded diagnosis of epilepsy or migraine")

    def burden(outcome, win):
        return next(r for r in B if r["outcome"] == outcome and r["window_days"] == win)

    # ------------------------------------------------------------------ a: cumulative incidence, days 1-90
    f.tag(6, 14, "a", "Cumulative incidence during the first 90 days")
    kx = 306
    f.L(kx, 11.8, kx + 13, 11.8, BLUE, 1.5); f.T(kx + 16, 13.8, "Epilepsy", 5.6)
    f.L(kx + 48, 11.8, kx + 61, 11.8, GREY, 1.5); f.T(kx + 64, 13.8, "Migraine", 5.6)
    f.R(kx + 98, 8.4, 11, 6.8, TINT, "none"); f.T(kx + 112, 13.8, "95% confidence interval", 5.6)
    minis = [("Major osteoporotic fracture", 0.5, (0, 0.25, 0.5), 2),
             ("Hip or proximal femur fracture", 0.15, (0, 0.05, 0.10, 0.15), 2),
             ("Falls", 2.0, (0, 1, 2), 0),
             ("Head or face injury", 2.0, (0, 1, 2), 0)]
    ytop, ybot = 40, 112
    src = []
    for k, (name, ymax, ticks, nd) in enumerate(minis):
        x0 = 36 + k * 121.5
        x1 = x0 + 92
        xa, ya = LinAxis(x0, x1, 0, 90), LinAxis(ybot, ytop, 0, ymax)
        f.T(x0, 31, name, 5.6, INK, weight=600)
        yaxis(f, ya, x0, ticks, fmt=(lambda t, nd=nd: "0" if t == 0 else f"{t:.{nd}f}"), grid_to=x1)
        rows = [r for r in KM if r["outcome"] == name]
        assert len(rows) == 90 and [int(r["day"]) for r in rows] == list(range(1, 91)), name
        curves = {}
        for coh in ("migraine", "epilepsy"):
            pts = [(0, 0.0)] + [(int(r["day"]), 100 * (1 - fv(r[f"{coh}_survival"]))) for r in rows]
            up = [(0, 0.0)] + [(int(r["day"]), 100 * (1 - fv(r[f"{coh}_ci_low"]))) for r in rows]
            dn = [(0, 0.0)] + [(int(r["day"]), 100 * (1 - fv(r[f"{coh}_ci_high"]))) for r in rows]
            curves[coh] = (pts, up, dn)
        for coh, fill in (("migraine", BAND_GREY), ("epilepsy", TINT)):
            pts, up, dn = curves[coh]
            poly = [(xa(d), ya(min(v, ymax))) for d, v in steps(up)] + [(xa(d), ya(v)) for d, v in reversed(steps(dn))]
            f.poly(poly, stroke="none", fill=fill, closed=True)
        for coh, col in (("migraine", GREY), ("epilepsy", BLUE)):
            pts = curves[coh][0]
            assert max(v for _, v in pts) <= ymax, (name, coh)
            f.poly([(xa(d), ya(v)) for d, v in steps(pts)], stroke=col, sw=1.3)
        for r in rows:
            src.append([name, r["day"]] + [f"{100 * (1 - fv(r[c])):.5f}" for c in (
                "epilepsy_survival", "epilepsy_ci_high", "epilepsy_ci_low", "migraine_survival", "migraine_ci_high", "migraine_ci_low")])
        b = burden(name, "1-90")
        e, lo, hi = fv(b["estimate"]), fv(b["ci_low"]), fv(b["ci_high"])
        f.T(x0 + 4, ytop + 5.5, f"Risk ratio {est(e, lo, hi)}", 5.2, BLUE, weight=600)
        log("Fig2", "a", f"{name}, days 1-90", f"Risk ratio {est(e, lo, hi)}", b["source"])
        xaxis(f, xa, ybot, (0, 30, 60, 90))
    f.T(W / 2 + 12, ybot + 21, "Days since the first recorded diagnosis", 6.5, INK, anchor="middle")
    f.T(10.5, (ytop + ybot) / 2, "Cumulative incidence (%)", 6.5, INK, anchor="middle", rotate=-90)
    write_source("Fig2a_cumulative_incidence_days1_90.csv",
                 ["outcome", "day", "epilepsy_cuminc_pct", "epilepsy_ci_low", "epilepsy_ci_high",
                  "migraine_cuminc_pct", "migraine_ci_low", "migraine_ci_high"], src)

    # ------------------------------------------------------------------ b, c: by follow-up window
    cols = ["Hip or proximal femur fracture", "Major osteoporotic fracture", "Broad fracture composite", "Falls",
            "Head or face injury"]
    wins = [("1-90", "1–90"), ("91-365", "91–365"), ("366-1095", "366–1095")]
    yb = 152
    f.tag(6, yb, "b", "Risk ratio in each follow-up window")
    kx = 306
    f.mark("circle", kx + 3, yb - 2.2, BLUE, True, 2.2); f.T(kx + 9, yb - 0.2, "Risk ratio", 5.6)
    f.mark("square", kx + 51, yb - 2.2, BLUE, True, 2.0); f.T(kx + 57, yb - 0.2, "Hazard ratio", 5.6)
    b_top, b_bot = yb + 24, yb + 80
    yc = b_bot + 20
    f.tag(6, yc, "c", "Additional events per 1,000 person-years")
    c_top, c_bot = yc + 12, yc + 60
    axb = LogAxis(b_bot, b_top, 0.85, 8)
    axc = LinAxis(c_bot, c_top, 0, 40)
    src = []
    for k, name in enumerate(cols):
        x0 = 36 + k * 96
        x1 = x0 + 78
        px = [x0 + 13, x0 + 39, x0 + 65]
        f.T(x0, yb + 15, name, 5.6, INK, weight=600)
        first = k == 0
        yaxis(f, axb, x0, (1, 2, 4, 8), grid_to=x1, line=first, null=1)
        yaxis(f, axc, x0, (0, 10, 20, 30, 40), grid_to=x1, line=first)
        pts = []
        for (wkey, wlab), x in zip(wins, px):
            r = burden(name, wkey)
            e, lo, hi = fv(r["estimate"]), fv(r["ci_low"]), fv(r["ci_high"])
            pts.append((x, axb(e), e, lo, hi, r))
        f.poly([(x, y) for x, y, *_ in pts], stroke=GREY, sw=0.8)
        for (wkey, wlab), (x, y, e, lo, hi, r) in zip(wins, pts):
            is_hr = r["measure"] == "HR"
            vci(f, x, axb, e, lo, hi, "square" if is_hr else "circle", BLUE, 2.0 if is_hr else 2.2)
            f.T(x, axb(hi) - 3.2, f"{e:.2f}", 5.2, BLUE, anchor="middle", weight=600)
            log("Fig2", "b", f"{name}, days {wkey}", f"{e:.2f}", r["source"])
            # c: risk difference in the window (percentage points) scaled to events per 1,000 person-years
            kpy = 365.0 / fv(r["window_length_days"])
            ex = fv(r["excess_per_1000"]) * kpy
            dlo, dhi = fv(r["rd_ci_low"]), fv(r["rd_ci_high"])
            f.R(x - 6.5, axc(ex), 13, axc(0) - axc(ex), BLUE, "none")
            top = ex
            if dlo is not None and dhi is not None:
                dlo, dhi = dlo * 10 * kpy, dhi * 10 * kpy
                f.L(x, axc(dlo), x, axc(dhi), INK, 0.9)
                f.L(x - 2, axc(dlo), x + 2, axc(dlo), INK, 0.9); f.L(x - 2, axc(dhi), x + 2, axc(dhi), INK, 0.9)
                top = dhi
            f.T(x, axc(top) - 2.6, f"{ex:.1f}", 5.2, INK, anchor="middle")
            log("Fig2", "c", f"{name}, days {wkey}", f"{ex:.1f}", r["source"] + "; risk difference x 365/window length")
            f.L(x, c_bot, x, c_bot + 3.5, INK, 0.8)
            f.T(x, c_bot + 10.6, wlab, 5.7, INK, anchor="middle")
            src.append([name, wkey, r["measure"], f"{e:.4f}", f"{lo:.4f}", f"{hi:.4f}", f"{ex:.3f}",
                        "" if dlo is None else f"{dlo:.3f}", "" if dhi is None else f"{dhi:.3f}",
                        r["n_epilepsy"], r["n_migraine"], r["events_epilepsy"], r["events_migraine"]])
        f.L(x0, c_bot, x1, c_bot, INK, 0.9)
    f.T(10.5, (b_top + b_bot) / 2, "Ratio (log scale)", 6.5, INK, anchor="middle", rotate=-90)
    f.T(10.5, (c_top + c_bot) / 2, "Per 1,000 PY", 6.5, INK, anchor="middle", rotate=-90)
    f.T(W / 2 + 12, c_bot + 21, "Follow-up window (days since the first recorded diagnosis)", 6.5, INK, anchor="middle")
    write_source("Fig2bc_risk_ratio_and_excess_by_window.csv",
                 ["outcome", "window_days", "measure", "estimate", "ci_low", "ci_high", "excess_per_1000_person_years",
                  "excess_ci_low", "excess_ci_high", "n_epilepsy", "n_migraine", "events_epilepsy", "events_migraine"], src)

    # ------------------------------------------------------------------ d: replication
    yd = c_bot + 44
    f.tag(6, yd, "d", "Replication over 3 years: broad fracture composite")
    axd = LogAxis(346, 424, 0.75, 2.5)
    nx, ex_, rx, vx = 176, 246, 292, 432
    yh = yd + 12
    f.T(6, yh + 3, "Cohort and comparison", 5.6, INK, weight=600)
    f.T(nx, yh + 3, "Adults, n", 5.6, INK, weight=600); f.T(ex_, yh + 3, "Events", 5.6, INK, weight=600)
    f.T(rx, yh + 3, "Risk, %", 5.6, INK, weight=600); f.T(vx, yh + 3, "HR (95% CI)", 5.6, INK, weight=600)
    header_rule(f, 6, W - 6, yh + 7)
    rows = [("TriNetX network", "TriNetX network, vs migraine, days 366-1095", "Epilepsy vs migraine, days 366–1095"),
            ("All of Us Research Program", "All of Us replication, 3 y", "Epilepsy vs migraine, 3 years"),
            (None, "All of Us replication, competing risk", "Epilepsy vs migraine, death as competing event"),
            (None, "All of Us, strict 2-code phenotype", "Epilepsy vs migraine, strict 2-record definition"),
            (None, "All of Us, syncope comparator", "Epilepsy vs syncope, 3 years")]
    y = yh + 8
    src = []
    for grp, key, label in rows:
        if grp:
            y += 12; f.T(6, y, grp, 5.4, ACC, weight=600)
        y += 11
        r = next(r for r in R if r["cohort"] == key)
        f.T(14, y + 1.9, label, 5.6, INK)
        f.T(nx, y + 1.9, r["n"], 5.4, INK); f.T(ex_, y + 1.9, r["events"], 5.4, INK)
        f.T(rx, y + 1.9, r["risks"].replace("%", ""), 5.4, INK)
        e, lo, hi = fv(r["estimate"]), fv(r["ci_low"]), fv(r["ci_high"])
        sig = hci(f, axd, y, e, lo, hi, "square", BLUE, 2.0)
        f.T(vx, y + 1.9, est(e, lo, hi), 5.4, BLUE if sig else GREY, weight=600 if sig else 400)
        log("Fig2", "d", key, f"{r['measure']} {est(e, lo, hi)}; n {r['n']}; events {r['events']}; risks {r['risks']}", r["source"])
        src.append([label, r["comparator"], r["data_source"], r["measure"], r["estimate"], r["ci_low"], r["ci_high"], r["n"], r["events"], r["risks"]])
    ybot = y + 9
    xaxis(f, axd, ybot, (0.75, 1, 1.5, 2), "Hazard ratio (log scale)", grid_top=yh + 8, null=1)
    f.T(6, ybot + 21, "Adults, events and risks are given as epilepsy / comparator.", 5.2, GREY, italic=True)
    write_source("Fig2d_replication.csv", ["row", "comparator", "data_source", "measure", "estimate", "ci_low", "ci_high",
                                           "n_epilepsy_comparator", "events_epilepsy_comparator", "risks"], src)
    return f


if __name__ == "__main__":
    finish(figure2(), "Figure2")
