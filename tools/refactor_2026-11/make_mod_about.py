"""Étape module : convertir l'onglet À propos (UI statique) en module Shiny."""
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_about.R"
OUT = ROOT / "mod_about.R"

lines = UI_SRC.read_text(encoding="utf-8").splitlines()
start = next(i for i, l in enumerate(lines) if l.rstrip().endswith("<- function() {"))
end = max(i for i, l in enumerate(lines) if l.strip() == "}")
ui_body = "\n".join(lines[start + 1:end])

module = f"""# Module Shiny — Onglet À propos (contenu statique, aucun input/output)
# Aucun serveur n'est nécessaire : l'onglet ne contient aucun identifiant.

mod_about_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
