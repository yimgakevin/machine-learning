# Point d'entrée Shiny — NutriAnalyse Pro v2
# Permet `shiny::runApp()` et le bouton "Run App" de RStudio sans argument.
# $value est indispensable : source() renvoie une liste, pas l'objet shiny.appobj.
source("app_v2_final.R", local = TRUE)$value
