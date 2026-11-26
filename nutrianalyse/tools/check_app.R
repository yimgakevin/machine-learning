# Vérification non destructive du chargement de l'application.
# Usage : Rscript tools/check_app.R
# Charge le setup, construit l'UI, définit le serveur — sans lancer le serveur HTTP.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
} else {
  getwd()
}
setwd(root)
cat("ROOT:", getwd(), "\n")

t0 <- Sys.time()
res <- tryCatch({
  source("app_v2_final.R", local = FALSE)
  list(ok = TRUE, err = NULL)
}, error = function(e) list(ok = FALSE, err = conditionMessage(e)))

if (!res$ok) {
  cat("CHECK_FAIL:", res$err, "\n")
  quit(status = 1)
}

checks <- c(
  ui_is_tag     = inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") || inherits(ui, "html"),
  server_is_fun = is.function(server),
  has_css       = exists("CSS") && nzchar(CSS)
)

print(checks)
cat("CHECK_OK |", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s\n")
if (!all(checks)) quit(status = 1)
