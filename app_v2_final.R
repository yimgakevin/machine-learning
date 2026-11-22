# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  NutriAnalyse Pro v2 — Orchestrateur                                        ║
# ║  Le code est découpé dans app_parts/ (setup, ui/, server/)                  ║
# ╚══════════════════════════════════════════════════════════════════════════════╝
# Lancement : shiny::runApp("app.R")

# ── Setup : options, packages, modules, moteurs ──────────────────────────────
source("app_parts/00_setup.R", local = FALSE)

# ── Feuille de style globale ─────────────────────────────────────────────────
source("app_parts/01_css.R", local = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
# ── Interface : assemblage des morceaux UI ──────────────────────────────────
source("app_parts/ui/ui.R", local = FALSE)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Le serveur est découpé par domaine dans app_parts/server/ ──────────────
  # local = TRUE : chaque fichier partage le scope de cette fonction.
  source("app_parts/server/server_core.R", local = TRUE)
  source("app_parts/server/server_import.R", local = TRUE)
  source("app_parts/server/server_dashboard_shell.R", local = TRUE)
  source("app_parts/server/server_accueil.R", local = TRUE)
  source("app_parts/server/server_accueil_sa_distributions.R", local = TRUE)
  source("app_parts/server/server_accueil_sa_crosstab.R", local = TRUE)
  source("app_parts/server/server_accueil_sa_map.R", local = TRUE)
  source("app_parts/server/server_accueil_sa_report.R", local = TRUE)
  source("app_parts/server/server_mappeur.R", local = TRUE)
  source("app_parts/server/server_exploration.R", local = TRUE)
  source("app_parts/server/server_univarie.R", local = TRUE)
  source("app_parts/server/server_bivarie.R", local = TRUE)
  source("app_parts/server/server_descriptive.R", local = TRUE)
  source("app_parts/server/server_inferentiel.R", local = TRUE)
  source("app_parts/server/server_qualite_smart.R", local = TRUE)
  source("app_parts/server/server_prevalence.R", local = TRUE)
  source("app_parts/server/server_prevalence_expert.R", local = TRUE)
  source("app_parts/server/server_secu_alim.R", local = TRUE)
  source("app_parts/server/server_secu_alim_nouveaux.R", local = TRUE)
  source("app_parts/server/server_secu_alim_lectures.R", local = TRUE)
  source("app_parts/server/server_rdim.R", local = TRUE)
  source("app_parts/server/server_rdim_renderers.R", local = TRUE)
  source("app_parts/server/server_rdim_expert.R", local = TRUE)
  source("app_parts/server/server_clustering.R", local = TRUE)
  source("app_parts/server/server_zscore_setup.R", local = TRUE)
  source("app_parts/server/server_sens.R", local = TRUE)
  source("app_parts/server/server_zscore.R", local = TRUE)
  source("app_parts/server/server_muac_age.R", local = TRUE)
  source("app_parts/server/server_oms.R", local = TRUE)
  source("app_parts/server/server_modules.R", local = TRUE)
  source("app_parts/server/server_dplyr.R", local = TRUE)
  source("app_parts/server/server_modules_externes.R", local = TRUE)
}

shinyApp(ui=ui, server=server)
