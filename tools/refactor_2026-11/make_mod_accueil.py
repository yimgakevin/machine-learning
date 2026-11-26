"""Génère mod_accueil.R — conversion de l'onglet Accueil en module Shiny.

Contraintes préservées :
  * les 24 filtres f_* restent des IDs GLOBAUX (df_f les lit au scope racine) ;
  * event_data("plotly_click", source = "accueil_prev") reste au scope racine ;
  * update*() des filtres f_* utilisent le session racine (parent_session) ;
  * dashboard_filter_details reste un ID HTML nu (sélecteur CSS).
"""
import re
from pathlib import Path

root = Path(__file__).resolve().parents[2]
P = root / "app_parts"
ui_src = (P / "ui/tabs/tab_accueil.R").read_text(encoding="utf-8")
acc = (P / "server/server_accueil.R").read_text(encoding="utf-8")
dist = (P / "server/server_accueil_sa_distributions.R").read_text(encoding="utf-8")
cross = (P / "server/server_accueil_sa_crosstab.R").read_text(encoding="utf-8")
mapf = (P / "server/server_accueil_sa_map.R").read_text(encoding="utf-8")
report = (P / "server/server_accueil_sa_report.R").read_text(encoding="utf-8")
shell = (P / "server/server_dashboard_shell.R").read_text(encoding="utf-8")
core = (P / "server/server_core.R").read_text(encoding="utf-8")

# ── UI ────────────────────────────────────────────────────────────────────────
ui = ui_src.replace("# Extrait de app_v2_final.R — onglet accueil\n", "")
ui = ui.replace(
    'ui_tab_accueil <- function() {\n      tabItem(tabName="accueil",',
    'mod_accueil_ui <- function(id) {\n  ns <- shiny::NS(id)\n  tabItem(tabName = "accueil",',
)
ui, n_ui = re.subn(
    r'\b(uiOutput|plotlyOutput|actionButton)\("([^"]+)"',
    r'\1(ns("\2")',
    ui,
)
if "mod_accueil_ui <- function(id)" not in ui or n_ui != 20:
    raise SystemExit(f"UI_ACCUEIL_TRANSFORM_FAIL n={n_ui}")
if 'id = "dashboard_filter_details"' not in ui:
    raise SystemExit("UI_ACCUEIL_CSS_HOOK_MISSING")

# ── Serveur : extraits à déplacer ─────────────────────────────────────────────
acc_body = acc[acc.index("  output$accueil_badge"):]


def body_after(text, marker):
    return text[text.index(marker):]


dist_body = body_after(dist, "  output$accueil_sa_charts_row")
cross_body = body_after(cross, "  output$accueil_crosstab_row")
map_body = body_after(mapf, "  output$accueil_dv_map_row")
report_body = body_after(report, "  output$accueil_rapport_bar")

s1 = shell.index("  output$dashboard_filters <- renderUI({")
s2 = shell.index("  # Badge : nombre de filtres actifs")
s3 = shell.index("  # Reset tous les filtres")
s4 = shell.index("  # ═══ Synchronisation palette globale")
filters_block = shell[s1:s2]
badge_block = shell[s2:s3]
reset_block = shell[s3:s4]
shell_root = shell[:s1] + shell[s4:]

c1 = core.index("  # ── Filtres du dashboard (label réactif)")
c2 = core.index("  # ── Réactifs de base")
label_block = core[c1:c2]
core_root = core[:c1] + core[c2:]

# ── Transformations du corps du module ────────────────────────────────────────
# 1. Accueil : event_data reste au root, f_region est un réactif passé en argument.
n_ev = acc_body.count('event_data("plotly_click", source = "accueil_prev")')
acc_body = acc_body.replace(
    'observeEvent(event_data("plotly_click", source = "accueil_prev"), {',
    "observeEvent(accueil_click(), {",
).replace(
    'click <- event_data("plotly_click", source = "accueil_prev")',
    "click <- accueil_click()",
).replace("input$f_region", "f_region()").replace(
    'updateSelectInput(session, "f_region", selected = key)',
    'updateSelectInput(parent_session, "f_region", selected = key)',
)
if n_ev != 2 or 'event_data(' in acc_body:
    raise SystemExit(f"ACCUEIL_EVENT_TRANSFORM_FAIL n={n_ev}")

# 2. Blocs Sécurité Alimentaire : namespace des UI dynamiques.
dyn_ids = r'\b(downloadButton|selectInput|plotlyOutput|uiOutput|DT::DTOutput|DTOutput|leafletOutput)\(\s*'
for name, txt in [("dist", dist_body), ("cross", cross_body),
                  ("map", map_body), ("report", report_body)]:
    txt, n1 = re.subn(dyn_ids + r'"([^"]+)"', r'\1(ns("\2")', txt)
    txt, n2 = re.subn(dyn_ids + r'inputId\s*=\s*"([^"]+)"', r'\1(inputId = ns("\2")', txt)
    if n1 + n2 == 0:
        raise SystemExit(f"DYN_UI_TRANSFORM_FAIL {name}")
    if name == "dist":
        dist_body = txt
    elif name == "cross":
        cross_body = txt
    elif name == "map":
        map_body = txt
    else:
        report_body = txt

# 3. Filtres dashboard : les f_* restent nus, seul le badge est namespacé.
n_badge = filters_block.count('uiOutput("f_active_badge")')
filters_block = filters_block.replace(
    'uiOutput("f_active_badge")', 'uiOutput(ns("f_active_badge"))'
)
if n_badge != 1:
    raise SystemExit(f"BADGE_TRANSFORM_FAIL n={n_badge}")

# 4. Tous les update*() des filtres ciblent la session racine.
reset_block, n_upd = re.subn(r'(\bupdate[A-Za-z]+\()session,', r'\1parent_session,', reset_block)
if n_upd < 20:
    raise SystemExit(f"RESET_SESSION_TRANSFORM_FAIL n={n_upd}")

# ── Assemblage du module ──────────────────────────────────────────────────────
body = "\n".join([
    acc_body.rstrip(), dist_body.rstrip(), cross_body.rstrip(),
    map_body.rstrip(), report_body.rstrip(), filters_block.rstrip(),
    badge_block.rstrip(), reset_block.rstrip(), label_block.rstrip(),
])
body = "\n".join(("    " + l if l.strip() else l) for l in body.splitlines())

module = (
    "# Module Shiny — Accueil / tableau de bord (KPI, alertes, Sécurité alimentaire)\n"
    "# Les filtres f_* restent globaux (df_f les lit au scope racine) : le renderUI\n"
    "# du panneau de filtres ne leur applique donc PAS ns(). event_data('plotly_click',\n"
    "# source='accueil_prev') et les update*() des filtres sont fournis par le root.\n\n"
    + ui.rstrip()
    + "\n\nmod_accueil_server <- function(id, df_f, mapped_data, map_cols, vars_num,\n"
    + "                               vars_cat, filter_context, has_oms, f_region,\n"
    + "                               accueil_click, parent_session, gpal, lang) {\n"
    + "  moduleServer(id, function(input, output, session) {\n"
    + "    ns <- session$ns\n\n"
    + body
    + "\n  })\n}\n"
)
(root / "mod_accueil.R").write_text(module, encoding="utf-8")

# ── Fichiers racine réécrits ──────────────────────────────────────────────────
acc_bridge = (
    "# Extrait de app_v2_final.R — Accueil : pont racine vers mod_accueil.R\n"
    "  # ── ACCUEIL — module namespacé (mod_accueil.R) ─────────────────────────────\n"
    "  # event_data et les update*() des filtres doivent rester au scope racine :\n"
    "  # la source plotly 'accueil_prev' et les IDs f_* sont globaux.\n"
    '  accueil_prev_click <- event_data("plotly_click", source = "accueil_prev")\n'
    "  accueil_f_region <- reactive(input$f_region)\n\n"
    '  mod_accueil_server("accueil",\n'
    "    df_f = df_f, mapped_data = mapped_data, map_cols = map_cols,\n"
    "    vars_num = vars_num, vars_cat = vars_cat,\n"
    "    filter_context = filter_context, has_oms = has_oms,\n"
    "    f_region = accueil_f_region, accueil_click = accueil_prev_click,\n"
    "    parent_session = session, gpal = gpal, lang = lang)\n"
)
(P / "server/server_accueil.R").write_text(acc_bridge, encoding="utf-8")
(P / "server/server_dashboard_shell.R").write_text(shell_root, encoding="utf-8")
(P / "server/server_core.R").write_text(core_root, encoding="utf-8")

orch = (root / "app_v2_final.R").read_text(encoding="utf-8")
for f in ("server_accueil_sa_distributions.R", "server_accueil_sa_crosstab.R",
          "server_accueil_sa_map.R", "server_accueil_sa_report.R"):
    line = f'  source("app_parts/server/{f}", local = TRUE)\n'
    if line not in orch:
        raise SystemExit(f"ORCH_LINE_MISSING {f}")
    orch = orch.replace(line, "")
(root / "app_v2_final.R").write_text(orch, encoding="utf-8")

css = (P / "01_css.R").read_text(encoding="utf-8")
css = css.replace(
    "#dashboard_filters .control-label",
    "#dashboard_filters .control-label, [id$='-dashboard_filters'] .control-label",
).replace(
    "#dashboard_filters .f-section-hdr",
    "#dashboard_filters .f-section-hdr, [id$='-dashboard_filters'] .f-section-hdr",
).replace(
    "#dashboard_filters .checkbox label,",
    "#dashboard_filters .checkbox label, [id$='-dashboard_filters'] .checkbox label,",
).replace(
    "#dashboard_filters .radio label",
    "#dashboard_filters .radio label, [id$='-dashboard_filters'] .radio label",
)
(P / "01_css.R").write_text(css, encoding="utf-8")

print(
    f"MOD_ACCUEIL_GENERATED | ui={n_ui} event={n_ev} updates={n_upd} "
    f"| module={len(module)} chars"
)
