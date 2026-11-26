"""Étape module : convertir l'onglet Sécurité alimentaire en module Shiny.

Les trois tranches serveur existantes (calculs, nouveaux outputs, lectures)
sont réunies dans un seul module, car elles partagent `sa_res`.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_secu_alim.R"
SERVER_SRCS = [
    ROOT / "app_parts/server/server_secu_alim.R",
    ROOT / "app_parts/server/server_secu_alim_nouveaux.R",
    ROOT / "app_parts/server/server_secu_alim_lectures.R",
]
OUT = ROOT / "mod_secu_alim.R"


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
     "radioButtons", "checkboxInput", "checkboxGroupInput", "selectInput",
     "numericInput", "sliderInput"],
)
# Les conditions des conditionalPanel doivent aussi être namespacées.
ui_body = ui_body.replace("conditionalPanel(", "conditionalPanel(ns = ns, ")

server_parts = []
for path in SERVER_SRCS:
    part = "\n".join(read_lines(path)[1:])
    part = ns_widgets(part, ["checkboxGroupInput", "selectInput", "numericInput", "sliderInput"])
    # Les helpers sa_select() et sa_select_any() reçoivent l'ID en argument.
    part = part.replace("selectInput(id, label", "selectInput(ns(id), label")
    server_parts.append(part)

server_body = "\n\n".join(server_parts)

module = f"""# Module Shiny — Sécurité alimentaire (FCS, rCSI, HDDS, HHS, LCSI, FIES, MDD-W)
# UI et serveur namespacés (ns). Les réactifs partagés sont passés en arguments.

mod_secu_alim_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}

mod_secu_alim_server <- function(id, df_f, vars_cat, vars_num) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

{server_body}
  }})
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
