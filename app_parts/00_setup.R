# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  NutriAnalyse Pro v2 — Application Shiny Universelle                       ║
# ║  Style OMS/UNICEF | Niveau publication scientifique | Thèse de doctorat    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝
# Lancement : shiny::runApp("app.R")

# Les avertissements de version de packages ne doivent jamais interrompre le démarrage.
options(warn = 1)

# ── OPTIMISATION : Désactiver les messages de library() pour accélérer ───────
# Les fichiers sourcés appellent library() sur des packages déjà chargés,
# ce qui gaspille du temps en messages de masquage/attachement.
# On surcharge temporairement library() pour supprimer les messages redondants.
.original_library <- base::library
.loaded_marker <- new.env(parent = emptyenv())
library <- function(package, ...) {
  pkg_name <- tryCatch(as.character(substitute(package)), error = function(e) "")
  # Si character.only = TRUE, utiliser directement la valeur
  dots <- list(...)
  if (isTRUE(dots$character.only)) pkg_name <- package
  # Charger silencieusement
  suppressPackageStartupMessages(
    tryCatch(
      .original_library(pkg_name, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      error = function(e) message(sprintf("\u26a0\ufe0f Package %s non disponible", pkg_name))
    )
  )
}

# ── Chargement optimisé des packages ─────────────────────────────────────────
# Packages ESSENTIELS (chargés immédiatement — nécessaires pour l'UI/serveur)
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjs)
  library(tidyverse)
  library(scales)
  library(DT)
  library(plotly)
  library(leaflet)
  library(sf)
  library(digest)
  library(jsonlite)
  library(promises)
  library(future)
})

# Packages SECONDAIRES (chargés silencieusement — utilisés par les modules)
.load_quietly <- function(pkg) {
  suppressPackageStartupMessages(
    tryCatch(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
             error = function(e) message(sprintf("\u26a0\ufe0f %s non disponible: %s", pkg, conditionMessage(e))))
  )
}
pkgs_secondaires <- c(
  "ggiraph", "leaflet.extras", "binom", "gtsummary", "gt",
  "corrplot", "RColorBrewer", "viridis", "e1071", "readxl", "haven",
  "FactoMineR", "factoextra", "cluster", "rnaturalearth", "rnaturalearthdata",
  "rmarkdown", "officer", "flextable", "writexl", "ggdist",
  "base64enc", "knitr", "pagedown"
)
invisible(lapply(pkgs_secondaires, .load_quietly))

# ── Pandoc : indispensable aux exports Word / HTML / PDF ─────────────────────
# RStudio le fournit, mais un lancement par Rscript (LANCER_APP.bat) ne l'a pas.
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC"))) {
  .pandoc_candidates <- c(
    file.path(Sys.getenv("ProgramFiles"), "RStudio/resources/app/bin/pandoc"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/tools"),
    file.path(Sys.getenv("ProgramFiles"), "Positron/resources/app/bin/pandoc")
  )
  .pandoc_dir <- .pandoc_candidates[
    file.exists(file.path(.pandoc_candidates, "pandoc.exe"))
  ]
  if (length(.pandoc_dir) > 0) Sys.setenv(RSTUDIO_PANDOC = .pandoc_dir[1])
  rm(.pandoc_candidates, .pandoc_dir)
}

# ── Couche 0 : i18n (chargé en premier pour que tr() soit disponible globalement)
source("i18n.R")

# ── Couche 0 : Authentification & sécurité ───────────────────────────────────
source("auth_config.R")
source("safe_eval.R")

# ── Couche 1 : Configuration & utilitaires ───────────────────────────────────
source("config_oms.R")
source("measures.R")
source("utils.R")
source("rdim_validator.R")
source("module_rdim_expert.R")

# ── Couche 2 : Modules de données & analyses spécialisées ────────────────────
source("inferentiel_v4.R")
source("module_qualite_smart.R")
source("module_prevalence_smart.R")
source("module_secu_alimentaire.R")
source("module_mortalite.R")
source("module_iycf.R")
source("module_anemie.R")
source("module_paludisme.R")
source("module_wash.R")
source("module_menage_protection.R")
source("module_handicap_wg.R")
source("module_kobo_import.R")
source("module_zscore_oms.R")
source("zscore_expert_engine.R")

# ── Couche 3 : Cartographie ───────────────────────────────────────────────────
source("module_carto_v2.R")
source("ui_carto_v2.R")
source("ui_carto_pro.R")

# ── NIVEAU 3: IA INTELLIGENTE — 4 Agents Autonomes + Master Orchestrateur ────────
source("config_api_secure_v2.R")      # Configuration API Gemini sécurisée
source("context_manager_v3.R")        # Capture contexte utilisateur/données/analyses
source("gemini_api_v3.R")             # Wrapper API Gemini (streaming, caching, erreurs)
source("chat_agent.R")                # Agent Support & Q&A (nutrition, épidémiologie)
source("analysis_agent.R")            # Agent Analyses Autonomes (ACP, ACM, FAMD, tests)
source("report_agent.R")              # Agent Rapports Auto-Générés (4 styles)
source("diagnose_agent.R")            # Agent Diagnostic Erreurs Intelligentes
source("master_agent.R")              # Master Agent Orchestrateur Multi-Agent
source("rag_nutrition_kb.R")           # Base RAG : OMS/UNICEF/WFP/Lancet (nutrition, stats)
source("chatbot_ui_v4.R")             # Interface Chat IA Expert (v4 — analyse réelle)
source("chatbot_server_v4.R")         # Logique serveur Chat Expert (v4.2 — later + RAG)
source("expert_analyzer_engine.R")    # Exécution autonome des analyses
source("expert_interpreter.R")        # Interprétation intelligente des résultats

# ── CARTOGRAPHIE MONDIALE : Continent → Pays → Région → Département → Arrondissement
source("config_gadm_worldwide.R")  # Configuration GADM 50+ pays
source("gadm_loader.R")             # Chargeur intelligent + cache disque
source("carto_country_selector.R")  # Module UI/Server cascade géographique

# ── Couche 5 : Modules formels moduleServer (versions actives) ───────────────
# IMPORTANT : ne pas sourcer les anciens module_epidemio.R / module_survie.R etc.
# — seuls les mod_*.R ci-dessous sont actifs (versions moduleServer complètes)
source("mod_epidemio.R")
source("mod_survie.R")
source("mod_predictif.R")
source("mod_rapport.R")
source("mod_oms.R")               # Indicateurs OMS / PAM / UNICEF (module Shiny)
source("mod_uni.R")               # Analyse univariée (module Shiny)
source("mod_biv.R")               # Analyse bivariée (module Shiny)
source("mod_clust.R")             # Clustering K-means / CAH / DBSCAN (module Shiny)
source("mod_qualite.R")           # Contrôle qualité SMART / plausibility (module Shiny)
source("mod_prevalence.R")        # Prévalences SMART pondérées / DEFF (module Shiny)
source("mod_secu_alim.R")         # Sécurité alimentaire FCS/rCSI/HDDS/HHS (module Shiny)

# ── Modules Expert (Niveau Expert) ──────────────────────────────────────────
source("mod_dashboard_exec.R")    # Dashboard Exécutif KPIs OMS
source("mod_qualite_auto.R")      # Contrôle Qualité Automatisé
source("mod_audit_trail.R")       # Audit Trail & Traçabilité
source("mod_temporal_tests.R")    # Tests Statistiques Temporels
source("module_temporel_expert.R")  # Temporel expert : Continues, DID, Tendances, CUSUM, Bootstrap, Heatmap, Trajectoires
source("ui_temporel_expert.R")      # UI tabs expert temporel (7 onglets)

# ── Modules Expert Descriptif + Inférentiel ──────────────────────────────────
source("module_desc_expert.R")        # Descriptif expert : Bootstrap, Robuste, Normalité, Outliers, Distributions
source("module_infer_expert.R")       # Inférentiel expert : Appariés, Puissance, Bayes, TOST, Régression, Friedman
source("ui_desc_infer_expert.R")      # UI tabs expert descriptif + inférentiel
# ── Modules avancés — chargés au démarrage pour garantir le binding moduleServer ──
source("mod_advanced_forecasting.R")
source("mod_advanced_geolocation.R")
source("mod_advanced_nlp.R")
source("mod_advanced_benchmarking.R")

# ── Modules bases de données — chargement DIFFÉRÉ (lazy) ──
.lazy_modules <- list(
  database_sql = "mod_database_sql.R",
  dhis2        = "mod_dhis2.R",
  odk_central  = "mod_odk_central.R",
  redcap       = "mod_redcap.R"
)
.lazy_loaded <- new.env(parent = emptyenv())

load_lazy_module <- function(name) {
  if (!is.null(.lazy_loaded[[name]])) return(invisible(TRUE))
  f <- .lazy_modules[[name]]
  if (!is.null(f) && file.exists(f)) {
    tryCatch({
      source(f)
      .lazy_loaded[[name]] <- TRUE
      message(sprintf("\u2705 Module %s chargé à la demande", name))
    }, error = function(e) {
      message(sprintf("\u26a0\ufe0f Erreur chargement %s: %s", name, e$message))
    })
  }
}

# ── Moteurs experts cartographie (Tier 1-4) ─────────────────────────────────
source("carto_intelligence_engine.R")
source("carto_predictive_engine.R")
tryCatch(
  source("spatial_analysis_engine.R"),
  error = function(e) message("\u26a0\ufe0f spdep non installé — analyses spatiales désactivées")
)
source("gadm_predownload_all.R")
if (interactive()) gadm_cache_status()# zscore_expert_integration.R et carto_expert_integration.R
# sont sourcés dans le server (utilisent output/input)

# ── Restaurer library() original après chargement de tous les fichiers ──────
library <- .original_library
cat(sprintf("\u2705 Démarrage terminé — %d packages chargés\n", length(.packages())))

