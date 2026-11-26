"""Étape module : convertir l'onglet Manipulation dplyr en module Shiny."""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_dplyr.R"
SERVER_SRC = ROOT / "app_parts/server/server_dplyr.R"
OUT = ROOT / "mod_dplyr.R"


def read_lines(path):
    return path.read_text(encoding="utf-8").splitlines()


def strip_function_wrapper(lines):
    start = next(i for i, l in enumerate(lines) if l.rstrip().endswith("<- function() {"))
    end = max(i for i, l in enumerate(lines) if l.strip() == "}")
    return lines[start + 1:end]


WIDGETS = ["uiOutput", "plotlyOutput", "DTOutput", "downloadButton", "actionButton",
           "selectizeInput", "selectInput", "checkboxInput", "checkboxGroupInput",
           "radioButtons", "numericInput", "sliderInput", "textInput",
           "textAreaInput", "tableOutput", "verbatimTextOutput"]


def ns_widgets(text):
    pattern = re.compile(r"(" + "|".join(WIDGETS) + r")\(\s*\"([^\"]+)\"")
    return pattern.sub(r'\1(ns("\2")', text)


ui_body = "\n".join(strip_function_wrapper(read_lines(UI_SRC)))
ui_body = ns_widgets(ui_body)
ui_body = ui_body.replace("conditionalPanel(", "conditionalPanel(ns = ns, ")

server_body = "\n".join(read_lines(SERVER_SRC)[1:])
server_body = ns_widgets(server_body)
# IDs construits dynamiquement pour les lignes transmute().
server_body = server_body.replace(
    'textInput(paste0("dplyr_tr_col_",i)',
    'textInput(ns(paste0("dplyr_tr_col_", i))',
)
server_body = server_body.replace(
    'textInput(paste0("dplyr_tr_expr_",i)',
    'textInput(ns(paste0("dplyr_tr_expr_", i))',
)

module = f"""# Module Shiny — Manipulation des données avec l'interface dplyr
# UI et serveur namespacés (ns). Les données de travail dplyr_data sont
# internes au module : les autres onglets ne sont pas affectés.

mod_dplyr_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}

mod_dplyr_server <- function(id, df_f, vars_cat, vars_num, gpal, gprimary) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

{server_body}
  }})
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
