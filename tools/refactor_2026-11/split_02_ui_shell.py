"""Étape 2 : extraire la coquille UI (header, sidebar, body) du monolithe.

Le bloc tabItems(...) reste encore inline dans ui_body.R ; il sera découpé
à l'étape 3.
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "app_v2_final.R"
text = SRC.read_text(encoding="utf-8")
lines = text.splitlines(keepends=True)


def find(marker, start=0, exact=False):
    for i in range(start, len(lines)):
        s = lines[i].strip()
        if (s == marker) if exact else (marker in lines[i]):
            return i
    raise SystemExit(f"Marqueur introuvable : {marker!r}")


def strip_trailing_comma(block: str) -> str:
    s = block.rstrip()
    if s.endswith(","):
        s = s[:-1].rstrip()
    return s


def fun(name: str, body: str) -> str:
    banner = f"# Extrait de app_v2_final.R — {name}\n"
    return f"{banner}{name} <- function() {{\n{body}\n}}\n"


page_start = find("ui <- dashboardPage(", exact=True)
header_start = find("dashboardHeader(", page_start, exact=True)
sidebar_comment = find("# ── Sidebar", header_start)
sidebar_start = find("dashboardSidebar(", sidebar_comment, exact=True)
body_comment = find("# ── Body", sidebar_start)
body_start = find("dashboardBody(", body_comment, exact=True)
body_close = find(") # ferme dashboardBody", body_start)
page_close = find(")", body_close + 1, exact=True)

header = strip_trailing_comma("".join(lines[header_start:sidebar_comment]))
sidebar = strip_trailing_comma("".join(lines[sidebar_start:body_comment]))
body = "".join(lines[body_start:body_close + 1]).rstrip()

ui_dir = ROOT / "app_parts" / "ui"
ui_dir.mkdir(parents=True, exist_ok=True)

(ui_dir / "ui_header.R").write_text(fun("ui_header", header), encoding="utf-8", newline="")
(ui_dir / "ui_sidebar.R").write_text(fun("ui_sidebar", sidebar), encoding="utf-8", newline="")
(ui_dir / "ui_body.R").write_text(fun("ui_body", body), encoding="utf-8", newline="")

(ui_dir / "ui.R").write_text(
    """# Assemblage de l'interface — NutriAnalyse Pro v2
source("app_parts/ui/ui_header.R", local = FALSE)
source("app_parts/ui/ui_sidebar.R", local = FALSE)
source("app_parts/ui/ui_body.R", local = FALSE)

ui <- dashboardPage(
  skin = "blue",
  ui_header(),
  ui_sidebar(),
  ui_body()
)
""",
    encoding="utf-8",
    newline="",
)

replacement = '# ── Interface : assemblage des morceaux UI ──────────────────────────────────\nsource("app_parts/ui/ui.R", local = FALSE)\n'
new_text = "".join(lines[:page_start]) + replacement + "".join(lines[page_close + 1:])
SRC.write_text(new_text, encoding="utf-8", newline="")

print(f"header  : {header.count(chr(10)) + 1} lignes -> app_parts/ui/ui_header.R")
print(f"sidebar : {sidebar.count(chr(10)) + 1} lignes -> app_parts/ui/ui_sidebar.R")
print(f"body    : {body.count(chr(10)) + 1} lignes -> app_parts/ui/ui_body.R")
print(f"monolithe : {new_text.count(chr(10)) + 1} lignes")
