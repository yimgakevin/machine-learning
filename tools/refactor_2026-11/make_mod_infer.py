"""Étape module : convertir l'onglet Inférentiel (tests + expert) en module Shiny."""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_infer.R"
BASE_SERVER = ROOT / "app_parts/server/server_inferentiel.R"
EXPERT_SERVER = ROOT / "server_desc_infer_expert.R"
OUT = ROOT / "mod_infer.R"

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
ui_body = ui_body.replace("infer_expert_tabs()", "infer_expert_tabs(ns)")
ui_body = ui_body.replace('list(id = "infer_expert_tabs", type = "pills")',
                          'list(id = ns("infer_expert_tabs"), type = "pills")')
ui_body = ui_body.replace("conditionalPanel(", "conditionalPanel(ns = ns, ")

server_parts = ["\n".join(read_lines(BASE_SERVER)[1:])]
expert_lines = read_lines(EXPERT_SERVER)
section_b = next(i for i, l in enumerate(expert_lines) if "SECTION B : INFÉRENTIEL EXPERT" in l)
server_parts.append("\n".join(expert_lines[section_b - 2:]))
server_body = ns_widgets("\n\n".join(server_parts))

module = f"""# Module Shiny — Statistiques inférentielles (tests bivariés, régression,
# tests experts appariés, puissance, Bayes, TOST, tailles d'effet, synthèse).
# Les onglets expert sont namespacés via infer_expert_tabs(ns).

mod_infer_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}

mod_infer_server <- function(id, df_f, vars_cat, vars_num) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

{server_body}
  }})
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
