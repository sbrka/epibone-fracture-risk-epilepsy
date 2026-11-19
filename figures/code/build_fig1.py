#!/usr/bin/env python3
"""EPI-BONE Fig. 1: Overview of study designs (schematic; no data file).

Tinted rounded boxes with accent strokes, small-caps grey row labels, grey detail text, white canvas; lower-case
panel letters. Every count and window below is copied from the Methods and Results of the manuscript and from the
frozen study specification.
"""
import os
import sys
from pathlib import Path

os.environ.setdefault("DYLD_FALLBACK_LIBRARY_PATH", "/opt/homebrew/lib")
from ambrose_lib import Fig, INK, ACC, BLUE, GREY, LINE, GRID, TINT, WHITE, W, tw, esc  # noqa: E402

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
NEUTRAL = "#f7f9fb"  # neutral fill of the matching boxes

# piecewise, not-to-scale day axis (anchor day -> x)
ANCH = [(-730, 100), (-365, 146), (-180, 170), (-61, 190), (-30, 204), (0, 232), (30, 246), (90, 262),
        (365, 308), (730, 352), (1095, 394), (1825, 440)]


def X(d):
    for (d0, x0), (d1, x1) in zip(ANCH, ANCH[1:]):
        if d0 <= d <= d1:
            return x0 + (d - d0) / (d1 - d0) * (x1 - x0)
    raise ValueError(d)


LABEL_X, RIGHT_X = 6, 450


def kicker(f, x, y, s, fill=GREY, size=5.2):
    """Small-caps row label with 0.08 letter spacing."""
    w = tw(s, size, 700) + 0.08 * max(len(s) - 1, 0)
    f.boxes.append((x, y - size * 0.70, x + w, y + size * 0.20, s))
    f.e.append(f'<text x="{x:.2f}" y="{y:.2f}" font-size="{size}" fill="{fill}" text-anchor="start" font-weight="700" '
               f'font-style="normal" letter-spacing="0.08">{esc(s)}</text>')


def box(f, x0, x1, yc, lines, h=None, fill=TINT, stroke=ACC, size=5.2, weight=400, color=INK):
    """Rounded box centred on yc with 1 or 2 centred text lines."""
    n = len(lines)
    h = h or (11 if n == 1 else 17)
    f.R(x0, yc - h / 2, x1 - x0, h, fill, stroke, 0.8, rx=2)
    for k, s in enumerate(lines):
        yy = yc + 1.9 if n == 1 else yc - 2.3 + k * 7.0
        f.T((x0 + x1) / 2, yy, s, size, color, anchor="middle", weight=weight)


def bar(f, d0, d1, yc, s, h=9):
    box(f, X(d0), X(d1), yc, [s], h=h, fill=WHITE, stroke=BLUE)


def day0(f, y0, y1):
    f.L(X(0), y0, X(0), y1, ACC, 1.4)


def right(f, y, lines, pointer):
    for k, s in enumerate(lines):
        f.T(RIGHT_X, y + k * 8, s, 5.6, ACC, weight=700)
    f.T(RIGHT_X, y + len(lines) * 8 + 1, pointer, 5.2, GREY)


def figure1():
    f = Fig(462, "Fig. 1 | Overview of study designs")

    # ============================== A. Burden (TriNetX) ==============================
    f.tag(6, 14, "A", "Fracture and injury burden after a first recorded epilepsy diagnosis (TriNetX)")
    yA = 30
    day0(f, yA - 10, yA + 66)
    kicker(f, LABEL_X, yA + 1.5, "ELIGIBILITY")
    box(f, X(-730), X(0) - 2, yA, ["Age ≥18 y · ≥2 epilepsy records", "No epilepsy, seizure, or ASM record in prior 2 y"])
    f.T(X(0) + 4, yA - 2.3, "Day 0: first recorded epilepsy diagnosis", 5.2, INK, weight=700)
    f.T(X(0) + 4, yA + 4.7, "Comparator: first recorded migraine diagnosis, defined the same way", 5.2, GREY)
    y = yA + 20
    kicker(f, LABEL_X, y + 1.5, "EXCLUSIONS")
    box(f, X(-730), X(0) - 2, y, ["Prior osteoporosis or major fracture", "Prior events of each outcome, per outcome"], fill=WHITE, stroke=LINE)
    y += 20
    kicker(f, LABEL_X, y + 1.5, "MATCHING")
    box(f, X(-365), X(0) - 2, y, ["1:1 matching", "37 covariates, days −365 to −1"], fill=NEUTRAL, stroke=LINE)
    f.T(X(0) + 4, y + 1.9, "252,822 per arm · maximum postmatch ASMD 0.04", 5.2, INK)
    y += 18
    kicker(f, LABEL_X, y + 1.5, "FOLLOW-UP")
    bar(f, 1, 90, y, "Days 1–90"); bar(f, 91, 365, y, "Days 91–365"); bar(f, 366, 1095, y, "Days 366–1095")
    y += 16
    kicker(f, LABEL_X, y + 1.5, "SENSITIVITY")
    f.T(X(-730), y + 1.9, "Bone-enriched model, 63 characteristics · adults aged ≥65 y, 49,067 per arm · women, women aged ≥50 y, men · densitometry in the prior 3 y, 1,624 per arm (exploratory)", 5.2, GREY)
    right(f, yA - 2, ["How large, and when?", "Injury or skeletal", "fragility?"], "Figs. 2 and 3")
    yEndA = y + 8
    f.L(6, yEndA + 6, W - 6, yEndA + 6, GRID, 0.6)

    # ============================== B. Replication (All of Us) ==============================
    yB0 = yEndA + 20
    f.tag(6, yB0, "B", "Replication of the burden comparison (All of Us Research Program)")
    yB = yB0 + 16
    day0(f, yB - 10, yB + 22)
    kicker(f, LABEL_X, yB + 1.5, "COMPARATORS")
    box(f, X(-730), X(0) - 2, yB, ["Migraine, 4,599 vs 38,740 adults, overlap weighted", "Syncope, 4,698 vs 16,531 adults"], fill=WHITE, stroke=LINE)
    f.T(X(0) + 4, yB - 2.3, "Day 0: first recorded epilepsy diagnosis", 5.2, INK, weight=700)
    f.T(X(0) + 4, yB + 4.7, "Epilepsy cohort compared with each comparator cohort", 5.2, GREY)
    y = yB + 18
    kicker(f, LABEL_X, y + 1.5, "FOLLOW-UP")
    bar(f, 1, 1095, y, "3 years · cause-specific and competing-risk hazard ratios")
    right(f, yB0 + 14, ["Does it replicate?"], "Fig. 2d")
    yEndB = y + 8
    f.L(6, yEndB + 6, W - 6, yEndB + 6, GRID, 0.6)

    # ============================== C. Medication (TriNetX) ==============================
    yC0 = yEndB + 20
    f.tag(6, yC0, "C", "Antiseizure medication target-trial emulations, oral new users (TriNetX)")
    yC = yC0 + 16
    day0(f, yC - 8, yC + 92)
    kicker(f, LABEL_X, yC + 1.5, "ELIGIBILITY")
    box(f, X(-730), X(0) - 2, yC, ["Age ≥18 y · ≥730 days of prior records"], fill=WHITE, stroke=LINE)
    f.T(X(0) + 4, yC + 1.9, "Day 0: first oral initiation of the study medication", 5.2, INK, weight=700)
    y = yC + 14
    kicker(f, LABEL_X, y + 1.5, "EPILEPSY RECORD")
    box(f, X(-365), X(-61), y, ["Days −365 to −61"], fill=WHITE, stroke=LINE); box(f, X(-30), X(0) - 2, y, ["−30 to −1"], fill=WHITE, stroke=LINE)
    f.T(X(0) + 4, y + 1.9, "Other antiseizure medications permitted · analyzed as initiated", 5.2, GREY)
    y += 17
    kicker(f, LABEL_X, y + 1.5, "WASHOUT")
    box(f, X(-365), X(0) - 2, y, ["No record of either study", "medication, by any route"], fill=WHITE, stroke=LINE)
    y += 17
    kicker(f, LABEL_X, y + 1.5, "MATCHING")
    box(f, X(-365), X(0) - 2, y, ["Frozen 77-characteristic model"], fill=NEUTRAL, stroke=LINE)
    y += 15
    kicker(f, LABEL_X, y + 6.5, "FOLLOW-UP")
    bar(f, 31, 365, y - 1, "Days 31–365", h=8); bar(f, 31, 730, y + 9, "Days 31–730", h=8); bar(f, 366, 1825, y + 19, "Days 366–1825", h=8)
    f.T(X(0) + 4, y + 30, "Days 0–30 excluded, to avoid events that prompted initiation", 5.2, GREY)
    y += 44
    kicker(f, LABEL_X, y + 1.5, "COMPARISONS")
    xm = (X(-730) + X(1825)) / 2
    bx0, bx1 = X(-730), xm - 4
    f.R(bx0, y - 12, bx1 - bx0, 30, TINT, ACC, 0.8, rx=2)
    kicker(f, bx0 + 5, y - 4, "PRIMARY", fill=BLUE)
    f.T(bx0 + 5, y + 5, "Carbamazepine, phenytoin, or phenobarbital", 5.6, INK, weight=700)
    f.T(bx0 + 5, y + 13, "vs levetiracetam or lamotrigine · 3,249–3,535 per arm", 5.2, GREY)
    cx0, cx1 = xm + 4, X(1825)
    f.R(cx0, y - 12, cx1 - cx0, 30, WHITE, INK, 0.8, rx=2)
    kicker(f, cx0 + 5, y - 4, "SECONDARY", fill=GREY)
    f.T(cx0 + 5, y + 5, "Each medication vs levetiracetam, lamotrigine, or lacosamide", 5.6, INK, weight=700)
    f.T(cx0 + 5, y + 13, "679–3,214 per arm · Holm correction across 4 hypothesis medications", 5.2, GREY)
    right(f, yC0 + 14, ["Does the medication", "matter?"], "Fig. 4")
    yEndC = y + 20
    f.L(6, yEndC + 6, W - 6, yEndC + 6, GRID, 0.6)

    # ============================== D. Determinants (All of Us) ==============================
    yD0 = yEndC + 20
    f.tag(6, yD0, "D", "Factors associated with fracture within epilepsy (All of Us), day-180 landmark")
    yD = yD0 + 16
    day0(f, yD - 10, yD + 40)
    kicker(f, LABEL_X, yD + 1.5, "COHORT")
    box(f, X(-730), X(0) - 2, yD, ["2,299 adults with 2 seizure-related records", "7–180 days apart; maintenance ASM started"], fill=WHITE, stroke=LINE)
    f.T(X(0) + 4, yD - 2.3, "Day 0: landmark, 180 days after the second record", 5.2, INK, weight=700)
    f.T(X(0) + 4, yD + 4.7, "Exposures measured at day 0", 5.2, GREY)
    y = yD + 18
    kicker(f, LABEL_X, y + 1.5, "EXPOSURES")
    f.T(X(0) + 4, y + 1.9, "≥2 ASMs vs 0–1 · enzyme-inducing ASM · seizure-severity markers · 9 chronic conditions", 5.2, INK)
    y += 14
    kicker(f, LABEL_X, y + 1.5, "FOLLOW-UP")
    bar(f, 1, 1825, y, "From the landmark to the last record · cause-specific Cox and Fine-Gray models")
    right(f, yD0 + 14, ["Which factors matter", "within epilepsy?"], "Fig. 5")
    yEndD = y + 8

    # ============================== shared axis ==============================
    ya = yEndD + 14
    f.L(X(-730), ya, X(1825), ya, INK, 0.9)
    for d in (-730, -365, -61, -30, 0, 30, 90, 365, 730, 1095, 1825):
        f.L(X(d), ya, X(d), ya + 4, INK, 0.8)
        lab = f"{d}".replace("-", "−")
        f.T(X(d), ya + 11.6, lab, 5.7, INK, anchor="middle", weight=700 if d == 0 else 400)
    f.T((X(-730) + X(1825)) / 2, ya + 22, "Days relative to day 0 (axis not to scale); day 0 is defined in each panel", 6.5, INK, anchor="middle")
    f.T(6, ya + 34, "ASM indicates antiseizure medication; ASMD, absolute standardized mean difference.", 5.2, GREY, italic=True)
    f.H = int(round(ya + 40))
    f.e[0] = f'<rect x="0" y="0" width="{W}" height="{f.H}" fill="{WHITE}"></rect>'
    return f


if __name__ == "__main__":
    fig = figure1()
    probs = fig.collisions()
    fig.save(HERE.parent / "main", "Figure1")
    print("wrote Figure1 H", fig.H, "collisions", len(probs))
    print("\n".join(probs))
