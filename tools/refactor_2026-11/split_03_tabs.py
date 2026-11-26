"""Étape 3 : extraire chaque onglet substantiel de ui_body.R vers ui/tabs/.

Les bornes exactes viennent du parseur R (tools/list_tabs.R), pas d'un
découpage approximatif à la ligne. Les onglets d'une seule ligne restent
inline dans tabItems().
"""
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
RSCRIPT = Path(r"C:\Program Files\R\R-4.5.2\bin\Rscript.exe")
UI_BODY = ROOT / "app_parts" / "ui" / "ui_body.R"
TABS_DIR = ROOT / "app_parts" / "ui" / "tabs"
MIN_LINES = 5

out = subprocess.run(
    [str(RSCRIPT), "--vanilla", "tools/list_tabs.R"],
    cwd=ROOT, capture_output=True, text=True, check=True,
)
rows = []
for line in out.stdout.splitlines():
    if not line.strip():
        continue
    name, start, end = line.split("\t")
    rows.append((name, int(start), int(end)))

text = UI_BODY.read_text(encoding="utf-8")
lines = text.splitlines(keepends=True)
TABS_DIR.mkdir(parents=True, exist_ok=True)

extracted = []
replaced_lines = list(lines)

for name, start, end in sorted(rows, key=lambda r: r[1], reverse=True):
    if end - start + 1 < MIN_LINES:
        continue

    block = list(lines[start - 1:end])
    first = block[0].strip()
    if not first.startswith(f'tabItem(tabName="{name}"'):
        raise SystemExit(f"Frontière inattendue pour {name}: {first!r}")

    # Retirer la virgule finale de l'appel (elle appartient à tabItems()).
    last = block[-1].rstrip()
    if last.endswith(","):
        last = last[:-1].rstrip()
    block[-1] = last + "\n"

    header = f"# Extrait de app_parts/ui/ui_body.R — onglet {name}\n"
    fun = f"ui_tab_{name} <- function() {{\n{''.join(block)}}}\n"
    (TABS_DIR / f"tab_{name}.R").write_text(header + fun, encoding="utf-8", newline="")

    replaced_lines[start - 1:end] = [f"      ui_tab_{name}(),\n"]
    extracted.append((name, end - start + 1))

UI_BODY.write_text("".join(replaced_lines), encoding="utf-8", newline="")

(ROOT / "app_parts" / "ui" / "ui_tabs_index.R").write_text(
    """# Charge tous les onglets UI extraits (app_parts/ui/tabs/).
.tab_files <- sort(list.files("app_parts/ui/tabs", pattern = "\\\\.R$", full.names = TRUE))
invisible(lapply(.tab_files, source, local = FALSE))
rm(.tab_files)
""",
    encoding="utf-8",
    newline="",
)

print(f"{len(extracted)} onglets extraits :")
for name, n in extracted:
    print(f"  - {name:<14} {n:>4} lignes")
print(f"ui_body.R : {len(lines)} -> {len(replaced_lines)} lignes")
