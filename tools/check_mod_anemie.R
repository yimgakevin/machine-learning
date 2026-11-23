# Vérifie le module Anémie en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_anemie_ui("anemie"))
ids <- c(
  "anemie-anem_hb_ui", "anemie-anem_alt_ui", "anemie-anem_smoke_ui", "anemie-anem_preg_ui",
  "anemie-anem_pop", "anemie-run_anem", "anemie-anem_tabs", "anemie-anem_kpi",
  "anemie-anem_summary_table", "anemie-anem_hb_dist", "anemie-anem_donut"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_ANEMIE_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_ANEMIE_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(15)
n <- 40
d <- data.frame(
  hemoglobine = round(rnorm(n, 10.5, 1.6), 1),
  altitude = sample(c(0, 500, 1500, 2500), n, replace = TRUE),
  fumeur = sample(c("oui", "non"), n, replace = TRUE),
  enceinte = sample(c("oui", "non"), n, replace = TRUE),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(mod_anemie_server, args = list(df_f = shiny::reactive(d)), {
    session$setInputs(
      anem_hb = "hemoglobine", anem_alt = "altitude", anem_smoke = "fumeur",
      anem_preg = "enceinte", anem_pop = "enfant_6_59", run_anem = 1
    )
    cat("anem_hb_ui     :", class(output$anem_hb_ui)[1], "\n")
    cat("anem_kpi       :", class(output$anem_kpi)[1], "\n")
    cat("anem_summary   :", class(output$anem_summary_table)[1], "\n")
    cat("anem_hb_dist   :", class(output$anem_hb_dist)[1], "\n")
    cat("anem_donut     :", class(output$anem_donut)[1], "\n")
  })
  TRUE
}, error = function(e) {
  cat("MOD_ANEMIE_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_ANEMIE_OK\n") else quit(status = 1)
