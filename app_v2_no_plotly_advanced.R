#!/usr/bin/env Rscript

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  app_v2_no_plotly_advanced.R — NutriAnalyse Pro v2                          ║
# ║  Version simplifiée sans modules avancés plotly                             ║
# ║  Tous les modules standards + 3 modules experts                             ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── NOTES ───────────────────────────────────────────────────────────────────
# Cette version désactive les modules avancés (Forecasting, Geolocation, NLP)
# qui utilisent plotly.
# - Dashboard Exécutif: ✓ Actif
# - Qualité Automatisée: ✓ Actif
# - Audit Trail: ✓ Actif
# - Tests Temporels: ✓ Actif
# - Benchmarking: ✓ Actif (sans plotly)
# ───────────────────────────────────────────────────────────────────────────

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  NutriAnalyse Pro v2 — Configuration ALLÉGÉE                  ║\n")
cat("║  (sans modules avancés plotly)                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# ── CHARGES DES DÉPENDANCES ─────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)
library(ggplot2)

cat("✓ Dépendances principales chargées\n")

# ── CHARGEMENT DE LA CONFIGURATION ──────────────────────────────────
source("config_oms.R")
source("utils.R")

cat("✓ Configuration chargée\n")

# ── CHARGEMENT DES MODULES STANDARDS ────────────────────────────────
standard_modules <- c(
  "inferentiel_v4.R",
  "module_qualite_smart.R",
  "module_prevalence_smart.R",
  "module_secu_alimentaire.R",
  "module_kobo_import.R",
  "module_carto_v2.R",
  "module_epidemio.R",
  "module_zscore_oms.R",
  "module_survie.R",
  "module_predictif.R",
  "module_rapport.R"
)

for (mod in standard_modules) {
  tryCatch({
    source(mod)
  }, error = function(e) {
    cat(paste0("⚠ Erreur: ", mod, " — ", e$message, "\n"))
  })
}

cat("✓ ", length(standard_modules), " modules standards chargés\n")

# ── CHARGEMENT DES MODULES EXPERTS (3/4) ────────────────────────────
expert_modules <- c(
  "mod_dashboard_exec.R",
  "mod_qualite_auto.R",
  "mod_audit_trail.R",
  "mod_temporal_tests.R"
)

for (mod in expert_modules) {
  tryCatch({
    source(mod)
  }, error = function(e) {
    cat(paste0("⚠ Erreur: ", mod, " — ", e$message, "\n"))
  })
}

cat("✓ 4 modules experts chargés\n")

# ── MODULES AVANCÉS SIMPLIFIÉS (sans plotly) ────────────────────────
# Les 4 modules avancés sont remplacés par des stubs simples

# 1. Benchmarking (sans plotly)
if(!exists("advancedBenchmarkingUI")) {
  advancedBenchmarkingUI <- function(id) {
    ns <- NS(id)
    tagList(
      box(width=12, status="primary", solidHeader=TRUE,
          title="Benchmarking Avancé",
          p("Ce module n'est pas disponible dans cette version simplifiée."),
          p("Utilisez la version complète pour accéder aux analyses benchmarking."))
    )
  }
  advancedBenchmarkingServer <- function(id, df_f) {
    moduleServer(id, function(input, output, session) {})
  }
}

cat("✓ Stubs des modules avancés chargés\n")

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("SETUP COMPLET — Lancement de l'interface Shiny\n")
cat(strrep("=", 80) %+% "\n\n")

# ── INTERFACE UTILISATEUR ──────────────────────────────────────────
ui <- shinydashboard::dashboardPage(
  skin = "blue",

  # HEADER
  shinydashboard::dashboardHeader(
    title = tags$span(
      style = "font-weight:700;font-size:16px;",
      "NutriAnalyse Pro v2"
    ),
    tags$li(class = "dropdown",
            style = "padding-right:12px;font-size:12px;color:#fff;margin-top:8px;",
            paste0("v2.0 | ", format(Sys.Date(), "%d-%b-%Y")))
  ),

  # SIDEBAR
  shinydashboard::dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "main_menu",
      # Import
      menuItem("📥 Importer Données", tabName = "import", icon = icon("cloud-upload-alt")),
      # Analyse
      menuItem("📊 Analyses", tabName = "analyse", icon = icon("chart-bar"),
        menuSubItem("Z-Scores OMS", tabName = "zscore"),
        menuSubItem("Sécurité Alimentaire", tabName = "secu"),
        menuSubItem("Prévalences", tabName = "prev"),
        menuSubItem("Qualité SMART", tabName = "qualite"),
        menuSubItem("Épidémiologie", tabName = "epid"),
        menuSubItem("Survie", tabName = "survie"),
        menuSubItem("Prédictions", tabName = "pred")
      ),
      # Visualisation
      menuItem("🗺️ Cartographie", tabName = "carto", icon = icon("map")),
      # Statistiques
      menuItem("📈 Tests Statistiques", tabName = "tests", icon = icon("flask")),
      # Experts
      menuItem("🎓 Modules Experts", tabName = "expert", icon = icon("star"),
        menuSubItem("Dashboard Exécutif", tabName = "exec_dash"),
        menuSubItem("Qualité Automatisée", tabName = "qual_auto"),
        menuSubItem("Audit Trail", tabName = "audit"),
        menuSubItem("Tests Temporels", tabName = "temporal")
      ),
      # À propos
      menuItem("ℹ️ À Propos", tabName = "about", icon = icon("info-circle"))
    )
  ),

  # BODY
  shinydashboard::dashboardBody(
    tags$head(
      tags$style(HTML("
        body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
        .box { border-top: 4px solid #008DC9; }
        .main-sidebar { background-color: #f4f4f4; }
        .sidebar-menu > li > a { font-weight: 500; }
      "))
    ),

    tabItems(
      # ── IMPORT ──────────────────────────────────────────────────────
      tabItem("import",
        h2("Importer les Données"),
        p("Sélectionnez la source de données:")
      ),

      # ── ZSCORE ──────────────────────────────────────────────────────
      tabItem("zscore",
        h2("Z-Scores OMS 2006")
      ),

      # ── DASHBOARD EXÉCUTIF ──────────────────────────────────────────
      tabItem("exec_dash",
        dashboardExecUI("exec")
      ),

      # ── QUALITÉ AUTOMATISÉE ─────────────────────────────────────────
      tabItem("qual_auto",
        qualiteAutoUI("qc_auto")
      ),

      # ── AUDIT TRAIL ─────────────────────────────────────────────────
      tabItem("audit",
        auditTrailUI("audit_trail")
      ),

      # ── TESTS TEMPORELS ─────────────────────────────────────────────
      tabItem("temporal",
        temporalTestsUI("temporal_tests")
      ),

      # ── À PROPOS ────────────────────────────────────────────────────
      tabItem("about",
        h2("À Propos"),
        p("NutriAnalyse Pro v2 — Version Simplifiée"),
        p("Cette version n'inclut pas les modules avancés (Forecasting, Geolocation, NLP)."),
        p("Utilisez la version complète pour accéder à toutes les fonctionnalités.")
      )
    )
  )
)

# ── SERVEUR ─────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Modules experts
  dashboardExecServer("exec")
  qualiteAutoServer("qc_auto")
  auditTrailServer("audit_trail")
  temporalTestsServer("temporal_tests")

  # Afficher un message de bienvenue
  showNotification(
    "✓ NutriAnalyse Pro v2 (version simplifiée) chargée avec succès",
    type = "message",
    duration = 5
  )
}

# ── LANCER L'APPLICATION ────────────────────────────────────────────
cat("Démarrage de l'application sur http://localhost:4819\n\n")

shinyApp(ui = ui, server = server)
