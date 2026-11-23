# Vérifie le module Mortalité en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_mortalite_ui("mortalite"))
ids <- c(
  "mortalite-mort_deaths_ui", "mortalite-mort_pop_start_ui", "mortalite-mort_pop_end_ui",
  "mortalite-mort_u5_pop_ui", "mortalite-mort_age_dec_ui", "mortalite-mort_cause_ui",
  "mortalite-mort_sex_dec_ui", "mortalite-mort_recall_ui", "mortalite-mort_recall_days",
  "mortalite-run_mort", "mortalite-mort_tabs", "mortalite-mort_kpi",
  "mortalite-mort_summary_table", "mortalite-mort_cause_plot",
  "mortalite-mort_cause_table", "mortalite-mort_pyramid_plot"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_MORTALITE_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_MORTALITE_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(11)
n <- 40
d <- data.frame(
  nb_deces = rbinom(n, 1, 0.15),
  pop_debut = sample(80:150, n, replace = TRUE),
  pop_fin = sample(80:150, n, replace = TRUE),
  taille_menage_0_5 = sample(1:4, n, replace = TRUE),
  age_decede = sample(c(1, 3, 12, 35, 60), n, replace = TRUE),
  cause_deces = sample(c("Paludisme", "Diarrhée", "Autre"), n, replace = TRUE),
  sexe_decede = rep(c("M", "F"), length.out = n),
  recall_period = rep(90, n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(mod_mortalite_server, args = list(df_f = shiny::reactive(d)), {
    session$setInputs(
      mort_deaths = "nb_deces", mort_pop_start = "pop_debut", mort_pop_end = "pop_fin",
      mort_u5_pop = "taille_menage_0_5", mort_age_dec = "age_decede",
      mort_cause = "cause_deces", mort_sex_dec = "sexe_decede",
      mort_recall = "recall_period", mort_recall_days = 90, run_mort = 1
    )
    cat("mort_deaths_ui :", class(output$mort_deaths_ui)[1], "\n")
    cat("mort_kpi       :", class(output$mort_kpi)[1], "\n")
    cat("mort_summary   :", class(output$mort_summary_table)[1], "\n")
    cat("mort_cause_plot:", class(output$mort_cause_plot)[1], "\n")
    cat("mort_cause_tbl :", class(output$mort_cause_table)[1], "\n")
    cat("mort_pyramid   :", class(output$mort_pyramid_plot)[1], "\n")
  })
  TRUE
}, error = function(e) {
  cat("MOD_MORTALITE_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_MORTALITE_OK\n") else quit(status = 1)
