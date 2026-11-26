# Vérifie que l'onglet À propos est bien rendu par le module (UI statique).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_about_ui("about"))
if (!grepl('id="shiny-tab-about"', html, fixed = TRUE) || nchar(html) < 1000) {
  cat("MOD_ABOUT_UI_FAIL\n")
  quit(status = 1)
}
cat("MOD_ABOUT_UI_OK |", nchar(html), "caractères\n")
cat("CHECK_MOD_ABOUT_OK\n")
