"""Génère mod_iycf.R à partir des extraits UI et du bloc serveur SENS."""
import re
from pathlib import Path

root = Path(__file__).resolve().parents[2]
ui_src = (root / "app_parts/ui/tabs/tab_iycf.R").read_text(encoding="utf-8")
srv_lines = (root / "app_parts/server/server_sens.R").read_text(encoding="utf-8").splitlines()

# ── UI ────────────────────────────────────────────────────────────────────────
ui = ui_src
ui = ui.replace("# Extrait de app_parts/ui/ui_body.R — onglet iycf\n", "")
ui = ui.replace(
    "ui_tab_iycf <- function() {\n      tabItem(tabName=\"iycf\",",
    "mod_iycf_ui <- function(id) {\n  ns <- shiny::NS(id)\n  tabItem(tabName = \"iycf\",",
)
ui = re.sub(
    r'\b(uiOutput|plotOutput|plotlyOutput|DT::DTOutput|checkboxInput|actionButton|downloadButton)\("([^"]+)"',
    r'\1(ns("\2")',
    ui,
)
ui = re.sub(r'tabsetPanel\(id="([^"]+)"', r'tabsetPanel(id = ns("\1")', ui)
if "mod_iycf_ui <- function(id)" not in ui or 'ns("iycf_age_ui")' not in ui:
    raise SystemExit("UI_IYCF_TRANSFORM_FAIL")

# ── Serveur ───────────────────────────────────────────────────────────────────
start = next(i for i, l in enumerate(srv_lines) if "# ── IYCF " in l)
end = next(i for i, l in enumerate(srv_lines) if "# ── ANÉMIE — module namespacé" in l)
block = srv_lines[start + 1:end]
while block and not block[-1].strip():
    block.pop()
block = ["  " + l if l.strip() else l for l in block]
srv = "\n".join(block) + "\n"

srv = srv.replace(
    "iycf_select <- function(id, label, selected) {",
    "iycf_select <- function(ns, id, label, selected) {",
)
srv = srv.replace(
    "selectInput(id, label, choices = c(",
    "selectInput(ns(id), label, choices = c(",
)
srv = srv.replace('iycf_select("', 'iycf_select(ns, "')
for input_id in ("iycf_stratvar", "iycf_equity_var", "iycf_focus"):
    srv = srv.replace(
        f'selectInput("{input_id}",',
        f'selectInput(ns("{input_id}"),',
    )
if "iycf_select(ns, \"iycf_age\"" not in srv or 'ns("iycf_focus")' not in srv:
    raise SystemExit("SERVER_IYCF_TRANSFORM_FAIL")

module = (
    "# Module Shiny — IYCF / ANJE (OMS/UNICEF 2021)\n"
    "# UI namespacée + serveur moduleServer. Réactif partagé : df_f.\n\n"
    + ui.rstrip()
    + "\n\nmod_iycf_server <- function(id, df_f) {\n"
    + "  moduleServer(id, function(input, output, session) {\n"
    + "    ns <- session$ns\n\n"
    + srv
    + "  })\n}\n"
)
(root / "mod_iycf.R").write_text(module, encoding="utf-8")
print("MOD_IYCF_GENERATED", len(module), "chars")
