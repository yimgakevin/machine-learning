# =============================================================================
# dev/02_dev.R — Workflow de développement
# NutriAnalyse Pro v2 | YIMGA WANDJI KEVIN | DSAMID
# À exécuter pendant le développement
# =============================================================================

# ── Charger le package en mode dev ──────────────────────────────────────────
devtools::load_all(".")

# ── Lancer l'application ─────────────────────────────────────────────────────
run_app()

# ── Tests ─────────────────────────────────────────────────────────────────────
# devtools::test()               # Tous les tests
# devtools::test_active_file()   # Tests du fichier actif

# ── Vérification du package ─────────────────────────────────────────────────
# devtools::check()              # R CMD check complet
# devtools::check(vignettes = FALSE)  # Sans vignettes

# ── Ajouter un nouveau module ───────────────────────────────────────────────
# golem::add_module(name = "nouveau_module", with_test = TRUE)

# ── Ajouter une fonction utilitaire ──────────────────────────────────────────
# golem::add_fct("nom_fonction",  with_test = TRUE)
# golem::add_utils("nom_utils",   with_test = TRUE)

# ── Ajouter des ressources externes ─────────────────────────────────────────
# golem::add_css_file("custom")      # inst/app/www/custom.css
# golem::add_js_file("handlers")     # inst/app/www/handlers.js
# golem::add_js_handler("handlers")  # JS handler Shiny

# ── Documentation ────────────────────────────────────────────────────────────
# devtools::document()   # Générer la doc roxygen2
# pkgdown::build_site()  # Site de documentation

# ── Couverture des tests ─────────────────────────────────────────────────────
# covr::package_coverage()
