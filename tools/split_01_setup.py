"""Étape 1 du refactor : extraire le setup et le CSS du monolithe.

Découpe app_v2_final.R (lignes 1-198 = setup, 199-968 = CSS) vers app_parts/.
Le reste du fichier (UI + serveur) est conservé tel quel pour l'instant.
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "app_v2_final.R"
text = SRC.read_text(encoding="utf-8")
lines = text.splitlines(keepends=True)


def block(start: int, end: int) -> str:
    """Lignes 1-indexées, bornes incluses."""
    return "".join(lines[start - 1:end])


parts = ROOT / "app_parts"
(parts).mkdir(exist_ok=True)

setup = block(1, 198)
css = block(199, 968)

(parts / "00_setup.R").write_text(setup, encoding="utf-8", newline="")
(parts / "01_css.R").write_text(css, encoding="utf-8", newline="")

header = """# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  NutriAnalyse Pro v2 — Orchestrateur                                        ║
# ║  Le code est découpé dans app_parts/ (setup, ui/, server/)                  ║
# ╚══════════════════════════════════════════════════════════════════════════════╝
# Lancement : shiny::runApp("app.R")

# ── Setup : options, packages, modules, moteurs ──────────────────────────────
source("app_parts/00_setup.R", local = FALSE)

# ── Feuille de style globale ─────────────────────────────────────────────────
source("app_parts/01_css.R", local = FALSE)

"""

rest = block(970, len(lines))
SRC.write_text(header + rest, encoding="utf-8", newline="")

print(f"setup   : {len(setup.splitlines())} lignes -> app_parts/00_setup.R")
print(f"css     : {len(css.splitlines())} lignes -> app_parts/01_css.R")
print(f"reste   : {len(rest.splitlines())} lignes dans app_v2_final.R")
