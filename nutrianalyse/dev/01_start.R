# =============================================================================
# dev/01_start.R — Initialisation du projet golem
# NutriAnalyse Pro v2 | YIMGA WANDJI KEVIN | DSAMID
# À exécuter UNE SEULE FOIS lors du setup initial
# =============================================================================

# Installer golem si nécessaire
if (!requireNamespace("golem", quietly = TRUE)) {
  install.packages("golem")
}

# ── 1. Remplir le DESCRIPTION ───────────────────────────────────────────────
golem::fill_desc(
  pkg_name        = "NutriAnalysePro",
  pkg_title       = "NutriAnalyse Pro v2 — Plateforme d'Analyse Nutritionnelle",
  pkg_description = paste(
    "Plateforme de référence pour l'analyse nutritionnelle, épidémiologique",
    "et de sécurité alimentaire selon les standards OMS 2006, UNICEF,",
    "SMART, PAM/FAO et CDC. 32+ modules analytiques et 4 agents IA Gemini."
  ),
  author_first_name = "YIMGA WANDJI",
  author_last_name  = "KEVIN",
  author_email      = "contact@dsamid.com",
  author_orcid      = "0009-0006-1000-7339",
  repo_url          = "https://github.com/yimgakevin/NutriAnalysePro"
)

# ── 2. Dépendances (ajouter les packages Imports) ──────────────────────────
usethis::use_package("shiny",          min_version = "1.8.0")
usethis::use_package("shinydashboard")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjs")
usethis::use_package("shinyauthr")
usethis::use_package("sodium")
usethis::use_package("DT")
usethis::use_package("plotly")
usethis::use_package("leaflet")
usethis::use_package("sf")
usethis::use_package("DBI")
usethis::use_package("RSQLite")
usethis::use_package("httr")
usethis::use_package("httr2",          type = "Suggests")
usethis::use_package("jsonlite")
usethis::use_package("golem",          min_version = "0.5.0")

# ── 3. Tests ─────────────────────────────────────────────────────────────────
usethis::use_testthat()
golem::use_recommended_tests()

# ── 4. Fichiers utilitaires golem ─────────────────────────────────────────────
golem::use_utils_ui()
golem::use_utils_server()

cat("\n✅ Projet golem initialisé avec succès !\n")
cat("   → Prochaine étape : dev/02_dev.R\n")
