# Lance l'application NutriAnalyse Pro v2 en local.
# Port modifiable via la variable d'environnement NUTRI_PORT (défaut : 8765).
port <- suppressWarnings(as.integer(Sys.getenv("NUTRI_PORT", "8765")))
if (is.na(port)) port <- 8765L
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "— démarrage sur le port", port, "\n")
shiny::runApp(".", port = port, host = "127.0.0.1", launch.browser = TRUE)
