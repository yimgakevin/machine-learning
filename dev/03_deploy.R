# =============================================================================
# dev/03_deploy.R — Déploiement de NutriAnalyse Pro v2
# YIMGA WANDJI KEVIN | DSAMID
# =============================================================================

# ── 1. Posit Connect ────────────────────────────────────────────────────────
# rsconnect::deployApp(
#   appDir  = ".",
#   appName = "NutriAnalysePro",
#   server  = "connect.posit.cloud"
# )

# ── 2. shinyapps.io ────────────────────────────────────────────────────────
# Générer le fichier app.R pour shinyapps.io
# golem::add_shinyappsio_file()
# rsconnect::deployApp(appName = "NutriAnalysePro")

# ── 3. Docker ────────────────────────────────────────────────────────────────
# Générer un Dockerfile optimisé (utilise renv pour la reproductibilité)
# golem::add_dockerfile_with_renv(
#   output_dir = "deploy",
#   from       = "rocker/r-ver:4.5.2",
#   sysreqs    = TRUE
# )
# Construire l'image Docker :
#   docker build -t nutrianalyse-pro:2.0.0 -f deploy/Dockerfile .
#   docker run -p 3838:3838 nutrianalyse-pro:2.0.0

# ── 4. Dockerfile simple (sans renv) ─────────────────────────────────────────
# golem::add_dockerfile(
#   output_dir = "deploy",
#   from       = "rocker/shiny-verse:4.5.2"
# )

# ── 5. ShinyProxy ────────────────────────────────────────────────────────────
# golem::add_shinyproxy_file()

# ── 6. Heroku ────────────────────────────────────────────────────────────────
# golem::add_heroku_file()

# ── 7. Vérification pré-déploiement ─────────────────────────────────────────
# devtools::check()
# devtools::test()
# cat("Tous les tests passent ? Si oui, déployez !\n")
