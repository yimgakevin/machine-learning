"""Génère mod_zscore.R — conversion de l'onglet Z-scores en module Shiny.

Fusionne :
  * app_parts/ui/tabs/tab_zscore.R                 (UI, 9 sous-onglets)
  * app_parts/server/server_zscore_setup.R         (auto-détection des colonnes)
  * zscore_expert_integration.R                    (KPI expert, qualité, rapport)
  * app_parts/server/server_zscore.R               (calcul + diagnostics)
  * app_parts/server/server_muac_age.R             (MUAC + tranches d'âge)

Contraintes préservées :
  * zs_data reste un reactiveVal RACINE (partagé avec les autres onglets) ;
  * l'injection WHZ_calc/HAZ_calc/WAZ_calc dans raw_data() est conservée,
    avec le fallback ..row_id.. / seq_len(n_rs) quand n_rd != n_rs ;
  * tous les IDs UI (y compris ceux générés dans renderUI) sont namespacés.
"""
import re
from pathlib import Path

root = Path(__file__).resolve().parents[2]
P = root / "app_parts"
ui_src = (P / "ui/tabs/tab_zscore.R").read_text(encoding="utf-8")
setup = (P / "server/server_zscore_setup.R").read_text(encoding="utf-8")
zs = (P / "server/server_zscore.R").read_text(encoding="utf-8")
muac = (P / "server/server_muac_age.R").read_text(encoding="utf-8")
expert = (root / "zscore_expert_integration.R").read_text(encoding="utf-8")

# ── UI ────────────────────────────────────────────────────────────────────────
ui = ui_src.replace("# Extrait de app_parts/ui/ui_body.R — onglet zscore\n", "")
ui = ui.replace(
    "ui_tab_zscore <- function() {\n      tabItem(tabName=\"zscore\",",
    "mod_zscore_ui <- function(id) {\n  ns <- shiny::NS(id)\n  tabItem(tabName = \"zscore\",",
)
widgets = (r'\b(uiOutput|plotlyOutput|DTOutput|downloadButton|textInput|'
           r'actionButton|selectInput|checkboxInput)\(')
ui, n_ids = re.subn(widgets + r'\s*"([^"]+)"', r'\1(ns("\2")', ui)
ui, n_tabs = re.subn(r'tabsetPanel\(id\s*=\s*"([^"]+)"',
                     r'tabsetPanel(id = ns("\1")', ui)
if "mod_zscore_ui <- function(id)" not in ui or n_ids != 42 or n_tabs != 1:
    raise SystemExit(f"UI_ZSCORE_TRANSFORM_FAIL ids={n_ids} tabs={n_tabs}")
if 'tabItem(tabName = "zscore"' not in ui:
    raise SystemExit("UI_ZSCORE_TABITEM_FAIL")

# ── Corps serveur des quatre fichiers ─────────────────────────────────────────
setup_body = setup[setup.index("  # Auto-détection des colonnes"):]
zs_body = zs[zs.index("  # Calcul des Z-scores"):]
muac_body = muac[muac.index("  # ══════════════════════════════════════════════════════════════════"):]
expert_body = expert[expert.index("# REACTIVE VALUES"):]

# Namespace des UI dynamiques créées dans renderUI.
dyn = (r'\b(uiOutput|plotlyOutput|DTOutput|downloadButton|selectInput|'
       r'textInput|actionButton|checkboxInput)\(')
dyn_total = 0
for name in ("setup_body", "muac_body", "expert_body"):
    txt = locals()[name]
    txt, n1 = re.subn(dyn + r'\s*"([^"]+)"', r'\1(ns("\2")', txt)
    dyn_total += n1
    if name == "setup_body":
        setup_body = txt
    elif name == "muac_body":
        muac_body = txt
    else:
        expert_body = txt
if dyn_total != 10:
    raise SystemExit(f"DYN_UI_TRANSFORM_FAIL n={dyn_total}")
# Garde-fous : l'injection brute dans raw_data() doit rester intacte.
if "rd$WHZ_calc[seq_len(n_rs)] <- res$WHZ_calc" not in zs_body:
    raise SystemExit("RAW_DATA_FALLBACK_MISSING")
if "raw_data(rd)" not in zs_body:
    raise SystemExit("RAW_DATA_WRITE_MISSING")

body = "\n".join([
    setup_body.rstrip(), expert_body.rstrip(), zs_body.rstrip(), muac_body.rstrip(),
])
body = "\n".join(("    " + l if l.strip() else l) for l in body.splitlines())

module = (
    "# Module Shiny — Z-scores OMS 2006 (calcul, diagnostics, MUAC, âge, expert)\n"
    "# zs_data reste un reactiveVal racine (défini dans server_zscore_setup.R) :\n"
    "# il est partagé avec les autres onglets. L'injection WHZ_calc/HAZ_calc/\n"
    "# WAZ_calc dans raw_data() est conservée telle quelle, avec le fallback\n"
    "# ..row_id.. / seq_len(n_rs) lorsque le nombre de lignes diffère.\n\n"
    + ui.rstrip()
    + "\n\nmod_zscore_server <- function(id, df_f, raw_data, zs_data) {\n"
    + "  moduleServer(id, function(input, output, session) {\n"
    + "    ns <- session$ns\n\n"
    + body
    + "\n  })\n}\n"
)
(root / "mod_zscore.R").write_text(module, encoding="utf-8")

# ── Pont racine : zs_data partagé + appel du module ───────────────────────────
setup_bridge = (
    "# Extrait de app_v2_final.R — z-scores — réactif partagé et pont vers mod_zscore.R\n"
    "  # ══════════════════════════════════════════════════════════════════════════════\n"
    "  # Z-SCORES OMS 2006 — zs_data reste au scope RACINE (partagé entre onglets)\n"
    "  # ══════════════════════════════════════════════════════════════════════════════\n\n"
    "  zs_data <- reactiveVal(NULL)\n\n"
    '  mod_zscore_server("zscore", df_f = df_f, raw_data = raw_data, zs_data = zs_data)\n'
)
(P / "server/server_zscore_setup.R").write_text(setup_bridge, encoding="utf-8")

orch = (root / "app_v2_final.R").read_text(encoding="utf-8")
for f in ("server_zscore.R", "server_muac_age.R"):
    line = f'  source("app_parts/server/{f}", local = TRUE)\n'
    if line not in orch:
        raise SystemExit(f"ORCH_LINE_MISSING {f}")
    orch = orch.replace(line, "")
(root / "app_v2_final.R").write_text(orch, encoding="utf-8")

print(
    f"MOD_ZSCORE_GENERATED | ui={n_ids} tabs={n_tabs} dyn={dyn_total} "
    f"| module={len(module)} chars"
)
