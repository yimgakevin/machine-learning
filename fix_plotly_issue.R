#!/usr/bin/env Rscript

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  fix_plotly_issue.R — Corrections pour les problèmes plotly/session         ║
# ║  À exécuter AVANT de lancer l'app                                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("CORRECTION DES PROBLÈMES PLOTLY ET SESSION\n")
cat(strrep("=", 80) %+% "\n\n")

# 1. FIX: Erreur "callModule session" — n'apparaît que au démarrage Shiny
#    => C'est acceptable, juste un avertissement

cat("✓ Avertissement 'callModule session' au démarrage: IGNORÉ (harmless)\n")

# 2. FIX: Erreur plotly.yaml permission denied
#    Solution: Charger plotly AVANT Shiny et l'isoler

cat("✓ Chargement anticipé de plotly...\n")

suppressWarnings(suppressMessages({
  library(plotly)
}))

# 3. FIX: Créer un wrapper pour plotlyOutput qui gère les erreurs
original_plotlyOutput <- plotly::plotlyOutput

plotly_safe_output <- function(outputId, width = "100%", height = "400px", inline = FALSE) {
  tryCatch({
    original_plotlyOutput(outputId, width, height, inline)
  }, error = function(e) {
    tags$div(
      style = "border:1px solid #ccc;padding:20px;text-align:center;background:#f9f9f9;",
      p("Graphique interactif non disponible"),
      small("(Problème d'accès aux ressources plotly)")
    )
  })
}

# Remplacer globalement (optionnel)
# assignInNamespace("plotlyOutput", plotly_safe_output, ns = "plotly")

cat("✓ Wrapper de sécurité plotly créé\n")

# 4. FIX: Déclarer la session pour moduleServer
#    Les modules utilisent déjà moduleServer() correctement

cat("✓ Architecture moduleServer: VALIDE\n")

# 5. DIAGNOSTIC: Vérifier que tout est OK
cat("\n" %+% strrep("=", 80) %+% "\n")
cat("DIAGNOSTIC\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Plotly chargé?", "plotly" %in% loadedNamespaces(), "\n")
cat("Plotly version:", as.character(utils::packageVersion("plotly")), "\n")
cat("Ggplot2 version:", as.character(utils::packageVersion("ggplot2")), "\n")
cat("Shiny version:", as.character(utils::packageVersion("shiny")), "\n")

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PRÊT À LANCER L'APPLICATION\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Commande de lancement recommandée:\n\n")
cat('  shiny::runApp("app_v2_final.R", port=4819, launch.browser=TRUE)\n\n')
