"""Étape module : convertir l'onglet Réduction de dimension en module Shiny.

Les trois tranches serveur sont réunies dans le module, dans l'ordre d'origine :
server_rdim.R, server_rdim_renderers.R, server_rdim_expert.R.
rdim_res devient local au module (aucun autre onglet ne l'utilise).
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
UI_SRC = ROOT / "app_parts/ui/tabs/tab_rdim.R"
SERVER_SRCS = [
    ROOT / "app_parts/server/server_rdim.R",
    ROOT / "app_parts/server/server_rdim_renderers.R",
    ROOT / "app_parts/server/server_rdim_expert.R",
]
OUT = ROOT / "mod_rdim.R"


def read_lines(path):
    return path.read_text(encoding="utf-8").splitlines()


def strip_function_wrapper(lines):
    start = next(i for i, l in enumerate(lines) if l.rstrip().endswith("<- function() {"))
    end = max(i for i, l in enumerate(lines) if l.strip() == "}")
    return lines[start + 1:end]


WIDGETS = ["uiOutput", "plotlyOutput", "plotOutput", "DTOutput", "downloadButton",
           "actionButton", "selectizeInput", "selectInput", "checkboxInput",
           "checkboxGroupInput", "radioButtons", "numericInput", "sliderInput",
           "textInput", "tableOutput", "verbatimTextOutput"]


def ns_widgets(text):
    pattern = re.compile(r"(" + "|".join(WIDGETS) + r")\(\s*\"([^\"]+)\"")
    return pattern.sub(r'\1(ns("\2")', text)


ui_body = "\n".join(strip_function_wrapper(read_lines(UI_SRC)))
ui_body = ns_widgets(ui_body)
ui_body = ui_body.replace("conditionalPanel(", "conditionalPanel(ns = ns, ")

server_parts = []
for path in SERVER_SRCS:
    part = "\n".join(read_lines(path)[1:])  # retire l'en-tête "# Extrait de ..."
    server_parts.append(ns_widgets(part))
server_body = "\n\n".join(server_parts)

module = f"""# Module Shiny — Réduction de dimension (ACP, MCA, FAMD, t-SNE, UMAP, HCPC, bootstrap)
# UI et serveur namespacés (ns).
# `rdim_res` est local au module : aucun autre onglet ne lit ce résultat.

mod_rdim_ui <- function(id) {{
  ns <- shiny::NS(id)

{ui_body}
}}

mod_rdim_server <- function(id, df_f, vars_cat, vars_num, mapped_data,
                            gpal, gprimary) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

    rdim_res <- reactiveVal(NULL)

{server_body}
  }})
}}
"""
OUT.write_text(module, encoding="utf-8", newline="")
print(f"{OUT.name} écrit : {len(module.splitlines())} lignes")
