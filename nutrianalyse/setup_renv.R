# ============================================================================
# setup_renv.R — À exécuter UNE SEULE FOIS dans RStudio pour figer les versions
# ============================================================================
# Lance simplement : source("setup_renv.R")
# ============================================================================

cat("📦 Initialisation renv...\n")

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Packages requis par NutriAnalyse Pro v2
REQUIRED_PACKAGES <- c(
  # Shiny
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  # Data
  "tidyverse", "scales", "DT", "readxl", "haven", "arrow",
  # Visualisation
  "plotly", "ggiraph", "ggdist", "corrplot", "RColorBrewer", "viridis",
  # Cartographie
  "leaflet", "leaflet.extras", "sf", "rnaturalearth", "rnaturalearthdata",
  # Stats
  "binom", "e1071", "FactoMineR", "factoextra", "cluster",
  # Rapports
  "gtsummary", "gt", "rmarkdown", "officer", "flextable", "writexl",
  # IA & API
  "httr", "httr2", "jsonlite", "digest",
  # Bases de données & Sources externes
  "DBI", "RSQLite", "RPostgres", "RMariaDB", "odbc",
  # Tests
  "testthat",
  # Authentification (à venir)
  "shinyauthr", "sodium",
  # Async (à venir)
  "promises", "future",
  # Progression UI
  "waiter"
)

# Installer les manquants
missing <- REQUIRED_PACKAGES[!sapply(REQUIRED_PACKAGES, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat(sprintf("📥 Installation de %d packages manquants...\n", length(missing)))
  install.packages(missing)
} else {
  cat("✅ Tous les packages requis sont installés\n")
}

# Initialiser renv et créer le lockfile
renv::init(project = ".", bare = FALSE, restart = FALSE)
renv::snapshot(project = ".", type = "implicit")

cat("\n✅ renv.lock créé — versions des packages figées\n")
cat("   → Pour restaurer sur une autre machine : renv::restore()\n")
cat("   → Pour mettre à jour : renv::update() puis renv::snapshot()\n")
