"""Étape 4 : découper le corps du serveur en fichiers thématiques.

Chaque fichier contient une tranche brute du corps de server(), dans l'ordre
d'origine. L'orchestrateur les source avec local = TRUE, ce qui préserve
exactement le scope partagé (input, output, session, réactifs locaux).

Vérifications :
  - les ancres de début/fin correspondent aux lignes attendues ;
  - la concaténation des tranches est identique au corps d'origine.
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "app_v2_final.R"
SERVER_DIR = ROOT / "app_parts" / "server"

# (fichier, première ligne, dernière ligne) — lignes incluses, 1-indexées
CUTS = [
    ("server_core.R",                       24,   295),
    ("server_import.R",                    296,   687),
    ("server_dashboard_shell.R",           688,  1097),
    ("server_accueil.R",                  1098,  1658),
    ("server_accueil_sa_distributions.R", 1659,  2454),
    ("server_accueil_sa_crosstab.R",      2455,  3102),
    ("server_accueil_sa_map.R",           3103,  3526),
    ("server_accueil_sa_report.R",        3527,  3852),
    ("server_mappeur.R",                  3853,  4268),
    ("server_exploration.R",              4269,  4716),
    ("server_univarie.R",                 4717,  4908),
    ("server_bivarie.R",                  4909,  5150),
    ("server_descriptive.R",              5151,  5256),
    ("server_inferentiel.R",              5257,  5392),
    ("server_qualite_smart.R",            5393,  5664),
    ("server_prevalence.R",               5665,  5844),
    ("server_prevalence_expert.R",        5845,  6171),
    ("server_secu_alim.R",                6172,  6996),
    ("server_secu_alim_nouveaux.R",       6997,  7211),
    ("server_secu_alim_lectures.R",       7212,  7430),
    ("server_rdim.R",                     7431,  7853),
    ("server_rdim_renderers.R",           7854,  8856),
    ("server_rdim_expert.R",              8857,  9046),
    ("server_clustering.R",               9047,  9229),
    ("server_zscore_setup.R",             9230,  9262),
    ("server_sens.R",                     9263,  9874),
    ("server_zscore.R",                   9875, 10740),
    ("server_muac_age.R",                10741, 10934),
    ("server_oms.R",                     10935, 11073),
    ("server_modules.R",                 11074, 11117),
    ("server_dplyr.R",                   11118, 11676),
    ("server_modules_externes.R",        11677, 11768),
]

DESCRIPTIONS = {
    "server_core.R": "i18n, palette, authentification, réactifs de base, géo, chatbot, audit trail",
    "server_import.R": "import fichier, KoboToolbox / ODK",
    "server_dashboard_shell.R": "statut sidebar, filtres du dashboard, thème",
    "server_accueil.R": "KPI et cartes du tableau de bord d'accueil",
    "server_accueil_sa_distributions.R": "dashboard accueil — distributions rCSI / HDDS / FCS×rCSI",
    "server_accueil_sa_crosstab.R": "dashboard accueil — tableau croisé rCSI × FCS",
    "server_accueil_sa_map.R": "dashboard accueil — carte double vulnérabilité",
    "server_accueil_sa_report.R": "dashboard accueil — rapport automatique Word/HTML",
    "server_mappeur.R": "mappeur de colonnes et conformité OMS",
    "server_exploration.R": "exploration et statistiques complètes",
    "server_univarie.R": "analyses univariées",
    "server_bivarie.R": "analyses bivariées",
    "server_descriptive.R": "descriptive OMS",
    "server_inferentiel.R": "inférentiel unifié",
    "server_qualite_smart.R": "contrôle qualité SMART / plausibility check",
    "server_prevalence.R": "prévalences SMART pondérées",
    "server_prevalence_expert.R": "prévalences — comparaison designs, ICC, diagnostics",
    "server_secu_alim.R": "sécurité alimentaire — calculs FCS/rCSI/HDDS/HHS",
    "server_secu_alim_nouveaux.R": "sécurité alimentaire — composition, IPC, rounds",
    "server_secu_alim_lectures.R": "sécurité alimentaire — lectures expertes et exports",
    "server_rdim.R": "réduction de dimension — helpers et UI",
    "server_rdim_renderers.R": "réduction de dimension — graphiques factoextra et downloads",
    "server_rdim_expert.R": "réduction de dimension — t-SNE, UMAP, HCPC, bootstrap",
    "server_clustering.R": "clustering",
    "server_zscore_setup.R": "z-scores — réactif et intégration expert",
    "server_sens.R": "modules SENS — mortalité, IYCF, anémie, paludisme, WASH, ménage, handicap",
    "server_zscore.R": "z-scores OMS — calcul et diagnostics",
    "server_muac_age.R": "MUAC et z-scores par tranche d'âge",
    "server_oms.R": "indicateurs OMS / PAM",
    "server_modules.R": "cartographie v2 et modules Shiny formels",
    "server_dplyr.R": "manipulation dplyr",
    "server_modules_externes.R": "modules de recherche avancée et bases de données externes",
}

text = SRC.read_text(encoding="utf-8")
lines = text.splitlines(keepends=True)

assert lines[22].strip() == "server <- function(input, output, session) {", lines[22]
assert lines[11768].strip() == "}", repr(lines[11768])
assert lines[11770].strip() == "shinyApp(ui=ui, server=server)", lines[11770]

for i, (fname, start, end) in enumerate(CUTS):
    expected_next = CUTS[i + 1][1] if i + 1 < len(CUTS) else 11769
    assert end + 1 == expected_next, f"Trou entre {fname} et le suivant"

body = "".join(lines[23:11768])
slices = [(f, s, e, "".join(lines[s - 1:e])) for f, s, e in CUTS]
assert "".join(s[3] for s in slices) == body, "La concaténation ne couvre pas exactement le corps"

SERVER_DIR.mkdir(parents=True, exist_ok=True)
for fname, start, end, content in slices:
    header = f"# Extrait de app_v2_final.R — {DESCRIPTIONS[fname]}\n"
    (SERVER_DIR / fname).write_text(header + content, encoding="utf-8", newline="")

path_calls = "\n".join(
    f'  source("app_parts/server/{fname}", local = TRUE)' for fname, _, _, _ in slices
)

new_text = (
    "".join(lines[:22])
    + "server <- function(input, output, session) {\n\n"
    + "  # ── Le serveur est découpé par domaine dans app_parts/server/ ──────────────\n"
    + "  # local = TRUE : chaque fichier partage le scope de cette fonction.\n"
    + path_calls
    + "\n}\n\nshinyApp(ui=ui, server=server)\n"
)
SRC.write_text(new_text, encoding="utf-8", newline="")

readme = "# Serveur découpé\n\nChaque fichier est sourcé dans `server()` avec `local = TRUE`.\n\n"
readme += "\n".join(f"- `{f}` — {DESCRIPTIONS[f]} ({e - s + 1} lignes)" for f, s, e, _ in slices)
readme += "\n"
(SERVER_DIR / "README.md").write_text(readme, encoding="utf-8", newline="")

print(f"{len(slices)} fichiers serveur extraits")
for f, s, e, _ in slices:
    print(f"  {f:<38} {e - s + 1:>5} lignes")
print(f"app_v2_final.R : {len(lines)} -> {len(new_text.splitlines())} lignes")
