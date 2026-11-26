"""Étape module : convertir l'onglet Exploration en module Shiny.

Seul le code vivant est conservé : les blocs manip_* et adv_* n'ont aucun
widget correspondant dans l'UI et ne peuvent donc jamais s'exécuter.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_explor.R"
SERVER_SRC = ROOT / "app_parts/server/server_exploration.R"
OUT = ROOT / "mod_explor.R"


def read_lines(path):
    return path.read_text(encoding="utf-8").splitlines()


def strip_function_wrapper(lines):
    start = next(i for i, l in enumerate(lines) if l.rstrip().endswith("<- function() {"))
    end = max(i for i, l in enumerate(lines) if l.strip() == "}")
    return lines[start + 1:end]


def ns_widgets(text, widget_names):
    pattern = re.compile(r"(" + "|".join(widget_names) + r")\(\s*\"([^\"]+)\"")
    return pattern.sub(r'\1(ns("\2")', text)


ui_body = "\n".join(strip_function_wrapper(read_lines(UI_SRC)))
ui_body = ns_widgets(
    ui_body,
    ["uiOutput", "plotlyOutput", "DTOutput", "downloadButton", "actionButton",
     "selectizeInput", "selectInput", "numericInput", "sliderInput"],
)

# Lignes 3 à 116 : seuls outputs/observers reliés à l'UI de l'onglet.
server_lines = read_lines(SERVER_SRC)
server_body = "\n".join(server_lines[2:116])
server_body = ns_widgets(server_body, ["selectizeInput", "selectInput"])

module = f"""# Module Shiny — Exploration interactive et statistiques descriptives
# UI et serveur namespacés (ns).
#
# Les blocs historiques manip_* et adv_* (manip_result, manip_add_var,
# manip_reset, adv_result, adv_data_r) ont été retirés : aucun widget
# correspondant n'existe dans l'interface, ils étaient donc inatteignables.

mod_explor_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}

mod_explor_server <- function(id, df_f, vars_cat, vars_num,
                              gpal, gprimary, gsecondary) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

{server_body}
  }})
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
