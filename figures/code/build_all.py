#!/usr/bin/env python3
"""Build Figs. 1-5 of the EPI-BONE manuscript, the per-panel source data and the inventory of printed numbers.
Usage: python figures/code/build_all.py   (needs Pillow, cairosvg and the Source Sans 3 fonts; see README)."""
import shutil
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import nc_common as C  # noqa: E402
from build_fig1 import figure1  # noqa: E402
from build_fig2 import figure2  # noqa: E402
from build_fig3 import figure3  # noqa: E402
from build_fig4 import figure4  # noqa: E402
from build_fig5 import figure5  # noqa: E402

if __name__ == "__main__":
    bad = 0
    f1 = figure1()
    probs = f1.collisions(); f1.save(C.MAIN, "Figure1")
    print(f"wrote Figure1: H {f1.H} pt = {f1.H / C.W * 180:.1f} mm, collisions {len(probs)}"); bad += len(probs)
    for name, fn in (("Figure2", figure2), ("Figure3", figure3), ("Figure4", figure4), ("Figure5", figure5)):
        bad += len(C.finish(fn(), name))
    C.write_inventory("INVENTORY_printed_numbers.csv")
    print("inventory rows:", len(C.INVENTORY))
    sys.exit(1 if bad else 0)
