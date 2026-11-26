"""Ajoute un paramètre ns optionnel aux fonctions UI expert et namespacent leurs IDs.

Le défaut `shiny::NS(NULL)` conserve le comportement actuel des appels globaux
(depuis l'onglet infer), tandis que les modules passent leur propre ns.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent.parent
PATH = ROOT / "ui_desc_infer_expert.R"

WIDGETS = ["uiOutput", "plotlyOutput", "plotOutput", "DTOutput", "downloadButton",
           "actionButton", "selectizeInput", "selectInput", "checkboxInput",
           "checkboxGroupInput", "radioButtons", "numericInput", "sliderInput",
           "textInput", "tableOutput", "verbatimTextOutput"]

text = PATH.read_text(encoding="utf-8")
for fn in ("desc_expert_tabs", "infer_expert_tabs"):
    text = text.replace(
        f"{fn} <- function() {{",
        f"{fn} <- function(ns = shiny::NS(NULL)) {{",
    )
text = re.sub(
    r"(" + "|".join(WIDGETS) + r")\(\s*\"([^\"]+)\"",
    r'\1(ns("\2")',
    text,
)
text = text.replace("conditionalPanel(", "conditionalPanel(ns = ns, ")
PATH.write_text(text, encoding="utf-8", newline="")
print(f"{PATH.name} mis à jour : {len(text.splitlines())} lignes")
