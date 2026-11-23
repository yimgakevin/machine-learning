# Vérifie le module Handicap en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_handicap_ui("handicap"))
ids <- c(
  "handicap-wg_voir_ui", "handicap-wg_entendre_ui", "handicap-wg_marcher_ui",
  "handicap-wg_memoire_ui", "handicap-wg_soins_ui", "handicap-wg_comm_ui",
  "handicap-wg_threshold", "handicap-run_wg", "handicap-wg_tabs", "handicap-wg_kpi",
  "handicap-wg_domain_table", "handicap-wg_domain_plot", "handicap-wg_severity_plot"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_HANDICAP_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_HANDICAP_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(14)
n <- 40
d <- data.frame(
  handicap_voir = sample(1:4, n, replace = TRUE),
  handicap_entendre = sample(1:4, n, replace = TRUE),
  handicap_marcher = sample(1:4, n, replace = TRUE),
  handicap_memoire = sample(1:4, n, replace = TRUE),
  handicap_soins_personnels = sample(1:4, n, replace = TRUE),
  handicap_communication = sample(1:4, n, replace = TRUE),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(mod_handicap_server, args = list(df_f = shiny::reactive(d)), {
    session$setInputs(
      wg_voir = "handicap_voir", wg_entendre = "handicap_entendre",
      wg_marcher = "handicap_marcher", wg_memoire = "handicap_memoire",
      wg_soins = "handicap_soins_personnels", wg_comm = "handicap_communication",
      wg_threshold = 3, run_wg = 1
    )
    cat("wg_voir_ui     :", class(output$wg_voir_ui)[1], "\n")
    cat("wg_kpi         :", class(output$wg_kpi)[1], "\n")
    cat("wg_domain_tbl  :", class(output$wg_domain_table)[1], "\n")
    cat("wg_domain_plot :", class(output$wg_domain_plot)[1], "\n")
    cat("wg_severity    :", class(output$wg_severity_plot)[1], "\n")
  })
  TRUE
}, error = function(e) {
  cat("MOD_HANDICAP_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_HANDICAP_OK\n") else quit(status = 1)
