"""Étape module : convertir l'onglet Tableau descriptif (descoms) en module Shiny."""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_descoms.R"
DESC_SERVER = ROOT / "app_parts/server/server_descriptive.R"
EXPERT_SERVER = ROOT / "server_desc_infer_expert.R"
OUT = ROOT / "mod_descoms.R"

WIDGETS = ["uiOutput", "plotlyOutput", "plotOutput", "DTOutput", "downloadButton",
           "actionButton", "selectizeInput", "selectInput", "checkboxInput",
           "checkboxGroupInput", "radioButtons", "numericInput", "sliderInput",
           "textInput", "tableOutput", "verbatimTextOutput"]


def read_lines(path):
    return path.read_text(encoding="utf-8").splitlines()


def ns_widgets(text):
    return re.sub(r"(" + "|".join(WIDGETS) + r")\(\s*\"([^\"]+)\"",
                  r'\1(ns("\2")', text)


lines = read_lines(UI_SRC)
start = next(i for i, l in enumerate(lines) if l.rstrip().endswith("<- function() {"))
end = max(i for i, l in enumerate(lines) if l.strip() == "}")
ui_body = "\n".join(lines[start + 1:end])
ui_body = ns_widgets(ui_body)
# La fonction expert reçoit le namespace du module.
ui_body = ui_body.replace("desc_expert_tabs()", "desc_expert_tabs(ns)")
ui_body = ui_body.replace('list(id = "desc_expert_tabs", type = "pills")',
                          'list(id = ns("desc_expert_tabs"), type = "pills")')

server_parts = ["\n".join(read_lines(DESC_SERVER)[1:])]
expert_lines = read_lines(EXPERT_SERVER)
section_b = next(i for i, l in enumerate(expert_lines) if "SECTION B : INFÉRENTIEL EXPERT" in l)
server_parts.append("\n".join(expert_lines[10:section_b - 2]))
server_body = ns_widgets("\n\n".join(server_parts))

module = f"""# Module Shiny — Tableau descriptif OMS + analyses descriptives expert
# Les onglets expert sont namespacés via desc_expert_tabs(ns).

mod_descoms_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}

mod_descoms_server <- function(id, df_f, vars_cat, vars_num, map_cols, gpal) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

{server_body}
  }})
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
