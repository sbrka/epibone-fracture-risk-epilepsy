#!/usr/bin/env python3
"""EPI-BONE, Fig. 4: antiseizure medication and fracture or injury risk.

a  Two-year risks (days 31-730) after starting an enzyme inducer vs levetiracetam or lamotrigine, with risk ratios.
b  The same comparison in each follow-up window (risk ratios; hazard ratios for days 366-1825).
c  Each medication vs a pooled non-inducing reference, days 31-730 (grid of estimates with intervals).
d  Lacosamide vs levetiracetam: published estimate and reconstructions by design.
Data: data/F5A_eic_class_by_outcome_window.csv, data/F4B_DRUG_VS_NEUTRAL3_FOREST.csv, data/F4_lcm_design_ladder.csv.
"""
from nc_common import (Fig, LinAxis, LogAxis, INK, ACC, BLUE, RED, GREY, LINE, GRID, TINT, WHITE, W, rcsv, fv, est, log,
                       write_source, hci, xaxis, header_rule, finish)

NEUTRAL = "#f2f4f6"


def figure4():
    A, M, L = rcsv("F5A_eic_class_by_outcome_window.csv"), rcsv("F4B_DRUG_VS_NEUTRAL3_FOREST.csv"), rcsv("F4_lcm_design_ladder.csv")
    f = Fig(600, "Fig. 4 | Fracture and injury risk after starting enzyme-inducing and individual antiseizure medications")

    def cls(key, h):
        return next(r for r in A if r["outcome_key"] == key and r["horizon"] == h)

    # ------------------------------------------------------------------ a, b: class comparison
    f.tag(6, 14, "a", "Two-year risk after starting treatment")
    f.tag(290, 14, "b", "Ratio in each follow-up window")
    rows = [("R2", "Head or face injury", True), ("F2", "Broad fracture composite", False), ("R1", "Major osteoporotic fracture", True),
            ("M1", "Falls", False), ("M4", "Osteoporosis recorded", False), ("A1", "Densitometry performed", False),
            ("D1", "All-cause mortality", False)]
    axa = LinAxis(116, 216, 0, 12)
    vx = 236
    yh = 30
    f.T(6, yh, "Outcome", 5.6, INK, weight=600); f.T(116, yh, "Risk, %", 5.6, INK, weight=600)
    f.T(vx, yh, "RR (95% CI)", 5.6, INK, weight=600)
    wins = [("H1", "Days 31–365", "RR"), ("H2", "Days 31–730", "RR"), ("H3", "Days 366–1825", "HR")]
    axs = [LogAxis(296 + k * 70, 350 + k * 70, 0.5, 3) for k in range(3)]
    for (h, lab, meas), ax in zip(wins, axs):
        f.T(ax.a0, yh - 7, lab, 5.6, INK, weight=600)
        f.T(ax.a0, yh, "Risk ratio" if meas == "RR" else "Hazard ratio", 5.2, GREY)
    header_rule(f, 6, W - 6, yh + 4)
    y = yh + 4
    src_a, src_b, na = [], [], []
    ytop = y + 2
    for key, label, primary in rows:
        y += 17
        f.T(110, y + 1.9, label, 5.6, INK, anchor="end", weight=600 if primary else 400)
        r2 = cls(key, "H2")
        ri, rr_ = fv(r2["risk_inducer_pct"]), fv(r2["risk_reference_pct"])
        f.R(axa(0), y - 5.6, axa(ri) - axa(0), 5, BLUE, "none"); f.R(axa(0), y + 0.6, axa(rr_) - axa(0), 5, LINE, "none")
        f.T(axa(ri) + 2.5, y - 1.3, f"{ri:.2f}", 5.2, BLUE, weight=600); f.T(axa(rr_) + 2.5, y + 4.9, f"{rr_:.2f}", 5.2, GREY)
        e, lo, hi = fv(r2["rr"]), fv(r2["rr_ci_low"]), fv(r2["rr_ci_high"])
        sig = lo > 1 or hi < 1
        f.T(vx, y + 1.9, est(e, lo, hi), 5.4, BLUE if sig else GREY, weight=600 if sig else 400)
        log("Fig4", "a", f"{label}, days 31-730", f"risks {ri:.2f} vs {rr_:.2f}; RR {est(e, lo, hi)}", r2["source"])
        src_a.append([label, f"{ri:.3f}", f"{rr_:.3f}", f"{e:.3f}", f"{lo:.3f}", f"{hi:.3f}", r2["n_inducer"], r2["n_reference"],
                      r2["events_inducer"], r2["events_reference"]])
        for (h, lab, meas), ax in zip(wins, axs):
            r = cls(key, h)
            if r["status"] != "estimable":
                na.append(((ax.a0 + ax.a1) / 2, y))
                src_b.append([label, lab, meas, "", "", "", r["status"]])
                continue
            k = "rr" if meas == "RR" else "hr"
            e, lo, hi = fv(r[k]), fv(r[f"{k}_ci_low"]), fv(r[f"{k}_ci_high"])
            hci(f, ax, y, e, lo, hi, "circle" if meas == "RR" else "square", BLUE, 2.2 if meas == "RR" else 2.0)
            log("Fig4", "b", f"{label}, {lab}", f"marker at {meas} {est(e, lo, hi)} (no number printed)", r["source"])
            src_b.append([label, lab, meas, f"{e:.3f}", f"{lo:.3f}", f"{hi:.3f}", r["status"]])
    ybot = y + 12
    xaxis(f, axa, ybot, (0, 4, 8, 12), "Adults with the outcome (%)")
    for ax, (h, lab, meas) in zip(axs, wins):
        xaxis(f, ax, ybot, (0.5, 1, 2, 3), None, grid_top=ytop, null=1)
    for x, yy in na:                                   # drawn after the grid so the label stays legible
        f.R(x - 19, yy - 4.2, 38, 8.4, WHITE, "none")
        f.T(x, yy + 1.9, "Not available", 5.2, GREY, anchor="middle", italic=True)
    f.T((axs[0].a0 + axs[2].a1) / 2, ybot + 20.5, "Ratio (log scale)", 6.5, INK, anchor="middle")
    yk = ybot + 32
    f.R(8, yk - 5, 10, 5, BLUE, "none"); f.T(21, yk, "Carbamazepine, phenytoin, or phenobarbital", 5.6)
    f.R(132, yk - 5, 10, 5, LINE, "none"); f.T(145, yk, "Levetiracetam or lamotrigine", 5.6)
    f.mark("circle", 300, yk - 2, BLUE, True, 2.2); f.T(306, yk, "95% CI excludes 1", 5.6)
    f.mark("circle", 366, yk - 2, BLUE, False, 2.2); f.T(372, yk, "95% CI includes 1", 5.6)
    f.mark("square", 432, yk - 2, BLUE, True, 2.0); f.T(438, yk, "Hazard ratio", 5.6)
    write_source("Fig4a_two_year_risks.csv", ["outcome", "risk_inducer_pct", "risk_reference_pct", "risk_ratio", "ci_low", "ci_high",
                                              "n_inducer", "n_reference", "events_inducer", "events_reference"], src_a)
    write_source("Fig4b_ratio_by_window.csv", ["outcome", "window", "measure", "estimate", "ci_low", "ci_high", "status"], src_b)

    # ------------------------------------------------------------------ c: medication grid
    yc = yk + 24
    f.tag(6, yc, "c", "Each medication vs levetiracetam, lamotrigine, or lacosamide, days 31–730")
    cols = [("Major osteoporotic fracture", "Major osteoporotic fracture", "Risk ratio"), ("Broad fracture", "Broad fracture composite", "Risk ratio"),
            ("Head or face injury", "Head or face injury", "Risk ratio"), ("Falls", "Falls", "Risk ratio"),
            ("All-cause mortality", "All-cause mortality", "Hazard ratio")]
    x0, cw, gap = 112, 76.4, 2.0
    yh = yc + 19
    f.T(6, yh, "Medication", 5.6, INK, weight=600)
    for k, (_, lab, meas) in enumerate(cols):
        f.T(x0 + k * (cw + gap), yh - 7, lab, 5.6, INK, weight=600); f.T(x0 + k * (cw + gap), yh, meas, 5.2, GREY)
    header_rule(f, 6, W - 6, yh + 4)
    meds = [("Hypothesis medications (Holm-corrected)", [("CBZ", "Carbamazepine"), ("PHT", "Phenytoin"), ("PHB", "Phenobarbital"), ("VPA", "Valproate")]),
            ("Descriptive", [("OXC", "Oxcarbazepine"), ("TPM", "Topiramate"), ("ZNS", "Zonisamide")])]
    y = yh + 5
    th = 16.0
    src = []
    for gname, items in meds:
        y += 10.5
        f.T(6, y, gname, 5.4, ACC, weight=600)
        y += 3
        for code, name in items:
            n = next(r["matched_per_arm"] for r in M if r["drug"] == code)
            f.T(6, y + 7.2, name, 5.6, INK)
            f.T(6, y + 13.6, f"n = {int(n):,} per arm", 5.2, GREY, italic=True)
            for k, (okey, lab, meas) in enumerate(cols):
                tx0 = x0 + k * (cw + gap)
                r = next((r for r in M if r["drug"] == code and r["outcome"] == okey), None)
                if r is None:
                    f.R(tx0, y, cw, th, WHITE, LINE, 0.6, rx=1.5, dash="2.4 2.2")
                    f.T(tx0 + cw / 2, y + 9.6, "Not run", 5.2, GREY, anchor="middle", italic=True)
                    src.append([name, lab, meas, "", "", "", "", "", "not run"])
                    continue
                e, lo, hi = fv(r["estimate"]), fv(r["ci_low"]), fv(r["ci_high"])
                holm = fv(r["holm_p"])
                sig = lo > 1 or hi < 1
                flag = holm is not None and holm < 0.05
                col = RED if flag else (BLUE if (sig and e > 1) else (INK if sig else GREY))
                fill = TINT if (sig and e > 1) else (NEUTRAL if sig else WHITE)
                f.R(tx0, y, cw, th, fill, LINE if not sig else col, 0.6, rx=1.5)
                f.T(tx0 + cw / 2, y + 6.9, est(e, lo, hi), 5.4, col, anchor="middle", weight=600 if sig else 400)
                ax = LogAxis(tx0 + 9, tx0 + cw - 9, 0.3, 3.3333)
                f.L(ax(1), y + 9.4, ax(1), y + 14.2, INK, 0.6)
                f.L(ax(lo), y + 11.8, ax(hi), y + 11.8, col, 1.2)
                f.C(ax(e), y + 11.8, 1.6, col if sig else WHITE, col, 0.7)
                log("Fig4", "c", f"{name}, {lab}", est(e, lo, hi) + (" (Holm P < 0.05)" if flag else ""), "F4B_DRUG_VS_NEUTRAL3_FOREST.csv")
                src.append([name, lab, meas, f"{e:.4f}", f"{lo:.4f}", f"{hi:.4f}", r["p"], r["holm_p"], r["status"]])
            y += th + 3
    yk = y + 9
    f.R(8, yk - 5.4, 12, 6.4, TINT, RED, 0.6, rx=1.5); f.T(24, yk, "Holm-adjusted P < 0.05", 5.6)
    f.R(104, yk - 5.4, 12, 6.4, TINT, BLUE, 0.6, rx=1.5); f.T(120, yk, "95% CI above 1", 5.6)
    f.R(178, yk - 5.4, 12, 6.4, NEUTRAL, INK, 0.6, rx=1.5); f.T(194, yk, "95% CI below 1", 5.6)
    f.R(252, yk - 5.4, 12, 6.4, WHITE, LINE, 0.6, rx=1.5); f.T(268, yk, "95% CI includes 1", 5.6)
    f.T(340, yk, "Bars show the 95% CI on a log scale from 0.3 to 3.3; tick, 1.", 5.2, GREY, italic=True)
    write_source("Fig4c_medication_grid.csv", ["medication", "outcome", "measure", "estimate", "ci_low", "ci_high", "p", "holm_p", "status"], src)

    # ------------------------------------------------------------------ d: lacosamide ladder
    yd = yk + 24
    f.tag(6, yd, "d", "Lacosamide vs levetiracetam: published signal and reconstructions")
    axd = LogAxis(322, 420, 0.4, 3.5)
    fx, nx, vx = 156, 226, 428
    yh = yd + 12
    f.T(6, yh + 3, "Design", 5.6, INK, weight=600); f.T(fx, yh + 3, "Follow-up, days", 5.6, INK, weight=600)
    f.T(nx, yh + 3, "Matched per arm, n", 5.6, INK, weight=600); f.T(vx, yh + 3, "HR (95% CI)", 5.6, INK, weight=600)
    header_rule(f, 6, W - 6, yh + 7)
    ladder = [(1, "Published estimate", "1–1825", True), (2, "3 or more prescriptions after day 0", "1–1825", False),
              (3, "6 or more prescriptions after day 0", "1–1825", False), (4, "Methods as written", "1–1825", False),
              (5, "90-day exposure window", "91–1825", False), (6, "3-month landmark", "91–1825", False),
              (7, "6-month landmark", "181–1825", False), (8, "New-user design", "31–1825", False), (9, "New-user design", "31–365", False),
              (20, "Corrected oral new-user design", "31–730", True), (21, "Corrected oral new-user design", "31–365", False)]
    lev = {int(r["order"]): r for r in L if r["pair"] == "LCM vs LEV"}
    pub = lev[1]
    pe, pl, ph = fv(pub["hr"]), fv(pub["hr_ci_low"]), fv(pub["hr_ci_high"])
    ytop = yh + 8
    ybot = ytop + 11 * len(ladder) + 6
    f.R(axd(pl), ytop, axd(ph) - axd(pl), ybot - ytop, TINT, "none")
    xaxis(f, axd, ybot, (0.5, 1, 2, 3), "Hazard ratio (log scale)", grid_top=ytop, null=1)
    f.L(axd(pe), ytop, axd(pe), ybot, ACC, 0.8, "2.4 2.2")
    y = ytop - 2
    src = []
    for order, label, fu, strong in ladder:
        y += 11
        r = lev[order]
        e, lo, hi = fv(r["hr"]), fv(r["hr_ci_low"]), fv(r["hr_ci_high"])
        f.T(6, y + 1.9, label, 5.6, INK, weight=600 if strong else 400)
        f.T(fx, y + 1.9, fu, 5.4, INK)
        f.T(nx, y + 1.9, " / ".join(f"{int(v):,}" for v in r["n_per_arm"].split(" / ")) if r["n_per_arm"] else "20,888 / 20,888", 5.4, INK)
        sig = hci(f, axd, y, e, lo, hi, "square", INK if order == 1 else BLUE, 2.0)
        f.T(vx, y + 1.9, est(e, lo, hi), 5.4, (INK if order == 1 else BLUE) if sig else GREY, weight=600 if (sig or strong) else 400)
        log("Fig4", "d", f"{label}, days {fu}", f"HR {est(e, lo, hi)}" + ("; 20,888 per arm from the published abstract" if order == 1 else ""), r["source"])
        src.append([label, fu, r["n_per_arm"], r["events"], f"{e:.4f}", f"{lo:.4f}", f"{hi:.4f}", r["design_group"]])
    f.T(6, ybot + 12, "Shaded band and dashed blue line: 95% CI and point estimate of the published study.", 5.2, GREY, italic=True)
    f.T(6, ybot + 19.5, "Outcome in every row: hip, cervical vertebra, or forearm fracture.", 5.2, GREY, italic=True)
    write_source("Fig4d_lacosamide_design_ladder.csv", ["design", "follow_up_days", "n_per_arm", "events", "hazard_ratio", "ci_low", "ci_high", "design_group"], src)
    return f


if __name__ == "__main__":
    finish(figure4(), "Figure4")
