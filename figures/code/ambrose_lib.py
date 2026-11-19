"""Minimal SVG drawing primitives for the figure set (text, lines, rectangles, circles, marks, axes).
1 viewBox unit = 1 pt; the canvas is 510.24 units = 180 mm wide. Colours, type sizes and stroke widths follow one
fixed design profile. Lower-case bold panel letters, as used by Nature journals."""
import math, os
from pathlib import Path
from PIL import ImageFont

INK, ACC, BLUE, RED, GREY = "#2f3237", "#2f6bb3", "#1b5eae", "#c65a52", "#6b7280"
LINE, GRID, TINT, WHITE = "#c9ccd1", "#e7e6e1", "#eef4fb", "#ffffff"
W = 510.24
FONT_STACK = "'Source Sans 3', 'Myriad Pro', Arial, Helvetica, sans-serif"   # quoted: an unquoted '3' makes the CSS list invalid in browsers
_FILES = {(400, False): "SourceSans3-Regular.otf", (600, False): "SourceSans3-Semibold.otf", (700, False): "SourceSans3-Bold.otf",
          (400, True): "SourceSans3-It.otf", (600, True): "SourceSans3-SemiboldIt.otf", (700, True): "SourceSans3-BoldIt.otf"}
_cache = {}
MINUS = "−"
# Source Sans 3 (SIL Open Font License) is needed to measure text; set EPIBONE_FONT_DIR to the folder with the OTF files
FONT_DIR = Path(os.environ.get("EPIBONE_FONT_DIR", str(Path.home() / "Library/Fonts")))


def tw(s, size, weight=400, italic=False):
    k = (weight, italic)
    if k not in _cache: _cache[k] = ImageFont.truetype(str(FONT_DIR / _FILES[k]), 200)
    return _cache[k].getlength(s) / 200 * size


def esc(s): return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def num(v, d=2, sign=False):
    """Format a number with a true minus sign; never rounds away the source value beyond d decimals."""
    s = f"{v:+.{d}f}" if sign else f"{v:.{d}f}"
    return s.replace("-", MINUS)


class Fig:
    def __init__(self, H, title):
        self.H, self.title, self.e, self.boxes = H, title, [], []
        self.e.append(f'<rect x="0" y="0" width="{W}" height="{H}" fill="{WHITE}"></rect>')

    def T(self, x, y, s, size=5.6, fill=INK, anchor="start", weight=400, italic=False, check=True, rotate=None):
        w = tw(s, size, weight, italic)
        x0 = x if anchor == "start" else (x - w / 2 if anchor == "middle" else x - w)
        if check:
            if rotate is None:
                self.boxes.append((x0, y - size * 0.70, x0 + w, y + size * 0.20, s))
            elif rotate in (-90, 270):
                # rotated text: anchor middle at (x, y) runs vertically
                y0 = y - w / 2 if anchor == "middle" else (y - w if anchor == "start" else y)
                self.boxes.append((x - size * 0.70, y0, x + size * 0.20, y0 + w, s))
        tr = f' transform="rotate({rotate} {x:.2f} {y:.2f})"' if rotate is not None else ""
        self.e.append(f'<text x="{x:.2f}" y="{y:.2f}" font-size="{size}" fill="{fill}" text-anchor="{anchor}" font-weight="{weight}" '
                      f'font-style="{"italic" if italic else "normal"}" letter-spacing="0"{tr}>{esc(s)}</text>')
        return w

    def L(self, x1, y1, x2, y2, stroke=INK, sw=0.8, dash=None):
        d = f' stroke-dasharray="{dash}"' if dash else ""
        self.e.append(f'<line x1="{x1:.2f}" y1="{y1:.2f}" x2="{x2:.2f}" y2="{y2:.2f}" stroke="{stroke}" stroke-width="{sw}"{d}></line>')

    def R(self, x, y, w, h, fill="none", stroke=INK, sw=0.8, rx=0, dash=None):
        d = f' stroke-dasharray="{dash}"' if dash else ""
        st = f' stroke="{stroke}" stroke-width="{sw}"' if stroke != "none" else ' stroke="none"'
        self.e.append(f'<rect x="{x:.2f}" y="{y:.2f}" width="{w:.2f}" height="{h:.2f}" rx="{rx}" fill="{fill}"{st}{d}></rect>')

    def C(self, cx, cy, r, fill, stroke, sw=0.75):
        self.e.append(f'<circle cx="{cx:.2f}" cy="{cy:.2f}" r="{r}" fill="{fill}" stroke="{stroke}" stroke-width="{sw}"></circle>')

    def poly(self, pts, stroke=INK, sw=0.9, dash=None, fill="none", closed=False):
        d = f' stroke-dasharray="{dash}"' if dash else ""
        tag = "polygon" if closed else "polyline"
        st = f' stroke="{stroke}" stroke-width="{sw}"' if stroke != "none" else ' stroke="none"'
        self.e.append(f'<{tag} points="{" ".join(f"{x:.2f},{y:.2f}" for x, y in pts)}" fill="{fill}"{st} stroke-linejoin="round"{d}></{tag}>')

    def arrow(self, x1, y1, x2, y2, stroke=INK, sw=0.8, head=3.0):
        self.L(x1, y1, x2, y2, stroke, sw)
        a = math.atan2(y2 - y1, x2 - x1); h = head
        p1 = (x2 - h * math.cos(a - 0.45), y2 - h * math.sin(a - 0.45)); p2 = (x2 - h * math.cos(a + 0.45), y2 - h * math.sin(a + 0.45))
        self.poly([(x2, y2), p1, p2], stroke="none", fill=stroke, closed=True)

    def mark(self, kind, x, y, color, filled, size=2.0, open_stroke=None):
        fill = color if filled else WHITE
        stroke = color if filled else (open_stroke or GREY)
        if kind == "circle": self.C(x, y, size, fill, stroke, 0.75)
        elif kind == "square": self.R(x - size * 0.9, y - size * 0.9, 1.8 * size, 1.8 * size, fill, stroke, 0.75, rx=0.3)
        elif kind == "diamond": self.poly([(x, y - size * 1.3), (x + size * 1.3, y), (x, y + size * 1.3), (x - size * 1.3, y)], stroke=stroke, sw=0.75, fill=fill, closed=True)

    def half_square(self, x, y, s, color):
        """Square of side s centred at (x, y) with its left half filled (partial coverage)."""
        self.R(x - s / 2, y - s / 2, s / 2, s, color, "none")
        self.R(x - s / 2, y - s / 2, s, s, "none", color, 0.6)

    def tag(self, x, y, letter, title):
        self.T(x, y, letter.lower(), 8, ACC, weight=700); self.T(x + 9.0, y, title, 7, INK, weight=700)

    def fit(self, pad=6.0):
        """Trim the canvas height to the lowest text box plus pad (removes trailing whitespace)."""
        self.H = round(max(b[3] for b in self.boxes) + pad, 1)
        self.e[0] = f'<rect x="0" y="0" width="{W}" height="{self.H}" fill="{WHITE}"></rect>'

    def collisions(self):
        out = []
        b = self.boxes
        for i in range(len(b)):
            x0, y0, x1, y1, s = b[i]
            if x0 < 1 or x1 > W - 1 or y0 < 0 or y1 > self.H: out.append(f"OUT OF CANVAS: {s!r}")
            for j in range(i + 1, len(b)):
                a0, b0, a1, b1, t = b[j]
                if x0 < a1 - 0.3 and a0 < x1 - 0.3 and y0 < b1 - 0.3 and b0 < y1 - 0.3: out.append(f"OVERLAP: {s!r} x {t!r}")
        return out

    def save(self, out_dir, name):
        out_dir = Path(out_dir); out_dir.mkdir(parents=True, exist_ok=True)
        hmm = self.H / W * 180.0
        svg = (f'<?xml version="1.0" encoding="UTF-8"?>\n<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {self.H}" width="180.00mm" height="{hmm:.2f}mm" '
               f'font-family="{FONT_STACK}">\n<title>{esc(self.title)}</title>\n' + "\n".join(self.e) + "\n</svg>\n")
        p = out_dir / f"{name}.svg"; p.write_text(svg, encoding="utf-8")
        os.environ.setdefault("DYLD_FALLBACK_LIBRARY_PATH", "/opt/homebrew/lib")
        import cairosvg
        cairosvg.svg2pdf(url=str(p), write_to=str(out_dir / f"{name}.pdf"))
        cairosvg.svg2png(url=str(p), write_to=str(out_dir / f"{name}.png"), output_width=4252)
        return p


class LinAxis:
    def __init__(self, a0, a1, lo, hi): self.a0, self.a1, self.lo, self.hi = a0, a1, lo, hi
    def __call__(self, v): return self.a0 + (v - self.lo) / (self.hi - self.lo) * (self.a1 - self.a0)


class LogAxis:
    def __init__(self, a0, a1, lo, hi): self.a0, self.a1, self.lo, self.hi = a0, a1, lo, hi
    def __call__(self, v): return self.a0 + (math.log(v) - math.log(self.lo)) / (math.log(self.hi) - math.log(self.lo)) * (self.a1 - self.a0)


def ci(fig, ax, y, est, lo, hi, kind, null=0.0, color=BLUE, size=2.0, sig=None, cap=1.6):
    """Horizontal CI + marker. Filled colour when the CI excludes the null, open grey otherwise.
    Off-scale bounds end in an arrow at the axis limit (canonical grammar). Returns True if significant."""
    if sig is None: sig = (lo > null) or (hi < null)
    col = color if sig else GREY
    sw = 1.2 if sig else 0.9
    lo_c, hi_c = max(lo, ax.lo), min(hi, ax.hi)
    fig.L(ax(lo_c), y, ax(hi_c), y, col, sw)
    if lo >= ax.lo: fig.L(ax(lo), y - cap, ax(lo), y + cap, col, 0.8)
    else: fig.arrow(ax(hi_c), y, ax(ax.lo), y, col, 0.8, head=2.6)
    if hi <= ax.hi: fig.L(ax(hi), y - cap, ax(hi), y + cap, col, 0.8)
    else: fig.arrow(ax(lo_c), y, ax(ax.hi), y, col, 0.8, head=2.6)
    if ax.lo <= est <= ax.hi: fig.mark(kind, ax(est), y, color, sig, size)
    return sig


def xaxis(fig, ax, y, ticks, label=None, fmt=None, grid_top=None, null=None, size=5.7, label_dy=19.0):
    """Bottom axis with ticks, optional vertical grid lines from grid_top and a dashed null line."""
    fmt = fmt or (lambda t: f"{t:g}".replace("-", MINUS))
    if grid_top is not None:
        for t in ticks: fig.L(ax(t), grid_top, ax(t), y, GRID, 0.5)
        if null is not None: fig.L(ax(null), grid_top, ax(null), y, INK, 0.8, "2.4 2.2")
    fig.L(min(ax.a0, ax.a1), y, max(ax.a0, ax.a1), y, INK, 0.9)
    for t in ticks:
        fig.L(ax(t), y, ax(t), y + 3, INK, 0.8)
        fig.T(ax(t), y + 9.6, fmt(t), size, INK, anchor="middle")
    if label: fig.T((ax.a0 + ax.a1) / 2, y + label_dy, label, 6.5, INK, anchor="middle")


def legend_item(fig, x, y, kind, label, filled=True, color=BLUE, size=2.0, open_stroke=None, text_size=5.6):
    fig.mark(kind, x, y - 1.9, color, filled, size, open_stroke)
    return x + 5 + fig.T(x + 4.5, y, label, text_size, INK)
