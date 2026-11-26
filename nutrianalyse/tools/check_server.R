# Exécute server() dans une session Shiny mock (shiny::testServer).
# Objectif : vérifier que tous les fichiers de app_parts/server/ se sourcent
# et s'enregistrent sans erreur, sans lancer de serveur HTTP.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
} else {
  getwd()
}
setwd(root)

source("app_v2_final.R", local = FALSE)

ok <- tryCatch({
  shiny::testServer(server, {
    cat("SERVER_SESSION_OK\n")
  })
  TRUE
}, error = function(e) {
  cat("SERVER_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_SERVER_OK\n") else quit(status = 1)
