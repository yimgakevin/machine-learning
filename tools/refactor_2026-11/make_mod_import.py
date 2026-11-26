"""Génère mod_import.R et réécrit server_import.R pour l'onglet import.

Le store racine (raw_data / mapped_data / map_cols / df_f / vars_* /
filter_context / data_loaded) reste dans server_import.R : df_f lit les
24 filtres globaux f_* de la sidebar. Seules les entrées/sorties déclarées
dans tab_import.R (mappeur, Kobo, conformité OMS, aperçu) deviennent
namespacées dans mod_import.R.
"""
import re
from pathlib import Path

root = Path(__file__).resolve().parents[2]
ui_src = (root / "app_parts/ui/tabs/tab_import.R").read_text(encoding="utf-8")
imp_src = (root / "app_parts/server/server_import.R").read_text(encoding="utf-8")
map_src = (root / "app_parts/server/server_mappeur.R").read_text(encoding="utf-8")

# ── UI : namespace de tous les IDs de l'onglet ────────────────────────────────
ui = ui_src.replace("# Extrait de app_v2_final.R — onglet import\n", "")
ui = ui.replace(
    'ui_tab_import <- function() {\n      tabItem(tabName="import",',
    'mod_import_ui <- function(id) {\n  ns <- shiny::NS(id)\n  tabItem(tabName = "import",',
)
ui, n_ids = re.subn(
    r'\b(uiOutput|DTOutput|selectInput|textInput|passwordInput|actionButton|fileInput)\("([^"]+)"',
    r'\1(ns("\2")',
    ui,
)
ui, n_tab = re.subn(r'tabsetPanel\(id\s*=\s*"([^"]+)"', r'tabsetPanel(id = ns("\1")', ui)
ui, n_cond = re.subn(
    r'conditionalPanel\("(input\.kobo_server[^\"]*)"',
    r'conditionalPanel(ns = ns, "\1"',
    ui,
)
if "mod_import_ui <- function(id)" not in ui or n_ids < 25 or n_tab != 1 or n_cond != 1:
    raise SystemExit(f"UI_IMPORT_TRANSFORM_FAIL ids={n_ids} tab={n_tab} cond={n_cond}")

# ── Serveur : Parties à déplacer dans le module ───────────────────────────────
head, rest = imp_src.split("  # Appliquer le mappeur manuellement", 1)
imp_mod, tail = rest.split("  # Données filtrées", 1)
map_mod = map_src.split("  # ── MAPPEUR", 1)[1]
map_mod = "  # ── MAPPEUR" + map_mod

srv = "  # Appliquer le mappeur manuellement" + imp_mod
srv = srv.rstrip() + "\n\n" + map_mod.rstrip() + "\n"

# SelectInput générés dynamiquement : ils doivent porter le namespace.
srv, n_map = re.subn(
    r'selectInput\(paste0\("map_",\s*key\),',
    'selectInput(ns(paste0("map_", key)),',
    srv,
)
srv, n_uid = re.subn(
    r'selectInput\("kobo_form_uid",',
    'selectInput(ns("kobo_form_uid"),',
    srv,
)
if n_map != 1 or n_uid != 1:
    raise SystemExit(f"SERVER_IMPORT_TRANSFORM_FAIL map={n_map} uid={n_uid}")

# Indentation du corps pour refléter l'imbrication dans moduleServer().
srv = "\n".join(("  " + l if l.strip() else l) for l in srv.splitlines()) + "\n"

module = (
    "# Module Shiny — Import & Mappeur intelligent (Kobo / ODK / conformité OMS)\n"
    "# UI namespacée + serveur moduleServer.\n"
    "# Le store racine (raw_data, mapped_data, map_cols, df_f, …) reste dans\n"
    "# server_import.R : df_f dépend des 24 filtres globaux f_* de la sidebar.\n\n"
    + ui.rstrip()
    + "\n\nmod_import_server <- function(id, raw_data, mapped_data, map_cols) {\n"
    + "  moduleServer(id, function(input, output, session) {\n"
    + "    ns <- session$ns\n\n"
    + srv
    + "  })\n}\n"
)
(root / "mod_import.R").write_text(module, encoding="utf-8")

# ── Serveur racine : on garde upload_file + le store, on appelle le module ───
call = (
    "  # ── IMPORT — module namespacé (mod_import.R) ──────────────────────────────\n"
    '  mod_import_server("import", raw_data, mapped_data, map_cols)\n\n'
)
new_imp = head + call + "  # Données filtrées" + tail
(root / "app_parts/server/server_import.R").write_text(new_imp, encoding="utf-8")

print(
    f"MOD_IMPORT_GENERATED | ui_ids={n_ids} tabset={n_tab} cond={n_cond} "
    f"dyn_map={n_map} dyn_uid={n_uid} | module={len(module)} chars"
)
