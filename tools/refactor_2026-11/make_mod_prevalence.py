"""Étape module : convertir l'onglet Prévalences SMART en module Shiny.

Le code UI et serveur existant est repris tel quel, avec :
  - les IDs de widgets préfixés par ns() côté UI ;
  - les widgets créés dans renderUI préfixés par ns() côté serveur.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_prevalence.R"
CORE_SRC = ROOT / "app_parts/server/server_prevalence.R"
EXPERT_SRC = ROOT / "app_parts/server/server_prevalence_expert.R"
OUT = ROOT / "mod_prevalence.R"


def read_lines(path):
    return path.read_text(encoding="utf-8").splitlines()


def strip_function_wrapper(lines):
    """Retourne le contenu entre `<- function() {` et la dernière ligne `}`."""
    start = next(i for i, l in enumerate(lines) if l.rstrip().endswith("<- function() {"))
    end = max(i for i, l in enumerate(lines) if l.strip() == "}")
    return lines[start + 1:end]


def ns_widgets(text, widget_names):
    pattern = re.compile(
        r"(" + "|".join(widget_names) + r")\(\s*\"([^\"]+)\""
    )
    return pattern.sub(r'\1(ns("\2")', text)


ui_body = "\n".join(strip_function_wrapper(read_lines(UI_SRC)))
ui_body = ns_widgets(
    ui_body,
    ["uiOutput", "plotlyOutput", "DTOutput", "downloadButton", "actionButton",
     "checkboxGroupInput", "selectInput"],
)

core_lines = read_lines(CORE_SRC)[1:]  # retire l'en-tête "# Extrait de ..."
expert_lines = read_lines(EXPERT_SRC)[1:]
core_body = "\n".join(core_lines)
expert_body = "\n".join(expert_lines)
core_body = ns_widgets(core_body, ["selectInput", "checkboxGroupInput"])
expert_body = ns_widgets(expert_body, ["selectInput", "checkboxGroupInput"])

module = f"""# Module Shiny — Prévalences SMART pondérées (design-based)
# UI et serveur namespacés (ns). Les réactifs partagés sont passés en arguments.

mod_prevalence_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}

mod_prevalence_server <- function(id, df_f, vars_cat, vars_num) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

{core_body}

{expert_body}
  }})
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
