"""Shared helpers for the EPI-BONE figure set (Nature Communications submission, November 2026).

Design profile: 180 mm wide, Source Sans 3, one fixed palette and stroke ladder (see ambrose_lib.py). Every plotted
value is read from figures/data/*.csv; every printed number is logged in INVENTORY and the values of each panel are
written to figures/source_data/.
"""
import csv
import os
import sys
from pathlib import Path

os.environ.setdefault("DYLD_FALLBACK_LIBRARY_PATH", "/opt/homebrew/lib")
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
from ambrose_lib import Fig, LinAxis, LogAxis, INK, ACC, BLUE, RED, GREY, LINE, GRID, TINT, WHITE, W, tw  # noqa: E402,F401

DATA, MAIN, SRC = HERE.parent / "data", HERE.parent / "main", HERE.parent / "source_data"
INVENTORY = []          # (figure, panel, item, printed text, source)


def rcsv(name):
    with open(DATA / name, encoding="utf-8-sig") as fh:
        return list(csv.DictReader(fh))


def fv(x):
    try:
        return float(str(x).replace(",", ""))
    except (TypeError, ValueError):
        return None


def est(e, lo, hi, d=2):
    return f"{e:.{d}f} ({lo:.{d}f}–{hi:.{d}f})"


def log(fig, panel, item, text, source):
    INVENTORY.append((fig, panel, item, text, source))


def write_source(name, header, rows):
    SRC.mkdir(parents=True, exist_ok=True)
    with open(SRC / name, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(header)
        w.writerows(rows)


def hci(f, ax, y, e, lo, hi, kind="circle", color=BLUE, size=2.2, null=1.0, sig=None):
    """Horizontal interval + mark. Filled colour when the CI excludes the null, open grey otherwise."""
    if sig is None:
        sig = lo > null or hi < null
    col = color if sig else GREY
    lo_c, hi_c = max(lo, ax.lo), min(hi, ax.hi)
    f.L(ax(lo_c), y, ax(hi_c), y, col, 1.7 if sig else 1.2)
    if lo >= ax.lo:
        f.L(ax(lo), y - 1.8, ax(lo), y + 1.8, col, 0.9)
    else:
        f.arrow(ax(hi_c), y, ax(ax.lo), y, col, 0.9, head=2.6)
    if hi <= ax.hi:
        f.L(ax(hi), y - 1.8, ax(hi), y + 1.8, col, 0.9)
    else:
        f.arrow(ax(lo_c), y, ax(ax.hi), y, col, 0.9, head=2.6)
    f.mark(kind, ax(e), y, color, sig, size)
    return sig


def vci(f, x, ax, e, lo, hi, kind="circle", color=BLUE, size=2.2, null=1.0, sig=None):
    """Vertical interval + mark on a value->y axis."""
    if sig is None:
        sig = lo > null or hi < null
    col = color if sig else GREY
    f.L(x, ax(lo), x, ax(hi), col, 1.7 if sig else 1.2)
    f.L(x - 1.8, ax(lo), x + 1.8, ax(lo), col, 0.9)
    f.L(x - 1.8, ax(hi), x + 1.8, ax(hi), col, 0.9)
    f.mark(kind, x, ax(e), color, sig, size)
    return sig


def xaxis(f, ax, y, ticks, label=None, fmt=None, grid_top=None, null=None, label_dy=20.5):
    fmt = fmt or (lambda t: f"{t:g}")
    if grid_top is not None:
        for t in ticks:
            f.L(ax(t), grid_top, ax(t), y, GRID, 0.5)
        if null is not None:
            f.L(ax(null), grid_top, ax(null), y, INK, 0.8, "2.4 2.2")
    f.L(min(ax.a0, ax.a1), y, max(ax.a0, ax.a1), y, INK, 0.9)
    for t in ticks:
        f.L(ax(t), y, ax(t), y + 3.5, INK, 0.8)
        f.T(ax(t), y + 10.6, fmt(t), 5.7, INK, anchor="middle")
    if label:
        f.T((ax.a0 + ax.a1) / 2, y + label_dy, label, 6.5, INK, anchor="middle")


def yaxis(f, ax, x, ticks, fmt=None, grid_to=None, label=None, label_x=None, line=True, null=None):
    """Left axis for a value->y axis (ax.a0 = bottom y, ax.a1 = top y)."""
    fmt = fmt or (lambda t: f"{t:g}")
    for t in ticks:
        if grid_to is not None:
            f.L(x, ax(t), grid_to, ax(t), GRID, 0.5)
    if null is not None and grid_to is not None:
        f.L(x, ax(null), grid_to, ax(null), INK, 0.8, "2.4 2.2")
    if line:
        f.L(x, ax.a0, x, ax.a1, INK, 0.9)
        for t in ticks:
            f.L(x - 3.5, ax(t), x, ax(t), INK, 0.8)
            f.T(x - 5, ax(t) + 1.9, fmt(t), 5.7, INK, anchor="end")
    if label:
        f.T(label_x, (ax.a0 + ax.a1) / 2, label, 6.5, INK, anchor="middle", rotate=-90)


def steps(pts):
    """Right-continuous step path through (x, y) points."""
    out = [pts[0]]
    for x, y in pts[1:]:
        out.append((x, out[-1][1]))
        out.append((x, y))
    return out


def header_rule(f, x0, x1, y):
    f.L(x0, y, x1, y, LINE, 0.6)


def finish(f, name, pad=6.0):
    f.fit(pad)
    probs = f.collisions()
    f.save(MAIN, name)
    print(f"wrote {name}: H {f.H} pt = {f.H / W * 180:.1f} mm, collisions {len(probs)}")
    for p in probs:
        print("   ", p)
    return probs


def write_inventory(name):
    with open(HERE.parent / name, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["figure", "panel", "item", "printed", "source"])
        w.writerows(INVENTORY)
