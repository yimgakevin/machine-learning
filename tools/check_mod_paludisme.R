# Vérifie le module Paludisme en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_paludisme_ui("paludisme"))
ids <- c(
  "paludisme-palu_poss_ui", "paludisme-palu_use_ui", "paludisme-palu_access_ui",
  "paludisme-palu_nb_nets_ui", "paludisme-palu_hh_size_ui", "paludisme-palu_tdr_ui",
  "paludisme-palu_fever_ui", "paludisme-palu_strat_ui", "paludisme-run_palu",
  "paludisme-palu_tabs", "paludisme-palu_kpi", "paludisme-palu_cascade_plot",
  "paludisme-palu_by_fever", "paludisme-palu_by_group"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_PALUDISME_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_PALUDISME_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(12)
n <- 40
d <- data.frame(
  llin_possession = sample(c("oui", "non"), n, replace = TRUE),
  llin_utilisation = sample(c("oui", "non"), n, replace = TRUE),
  llin_acces = sample(c("oui", "non"), n, replace = TRUE),
  nb_moustiquaires = sample(0:3, n, replace = TRUE),
  taille_menage = sample(1:6, n, replace = TRUE),
  tdr_malaria = sample(c("positif", "negatif"), n, replace = TRUE),
  fievre = sample(c("oui", "non"), n, replace = TRUE),
  region = rep(c("Nord", "Sud"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(mod_paludisme_server, args = list(df_f = shiny::reactive(d)), {
    session$setInputs(
      palu_poss = "llin_possession", palu_use = "llin_utilisation",
      palu_access = "llin_acces", palu_nb_nets = "nb_moustiquaires",
      palu_hh_size = "taille_menage", palu_tdr = "tdr_malaria",
      palu_fever = "fievre", palu_strat = "region", run_palu = 1
    )
    cat("palu_poss_ui    :", class(output$palu_poss_ui)[1], "\n")
    cat("palu_kpi        :", class(output$palu_kpi)[1], "\n")
    cat("palu_cascade    :", class(output$palu_cascade_plot)[1], "\n")
    cat("palu_by_fever   :", class(output$palu_by_fever)[1], "\n")
    cat("palu_by_group   :", class(output$palu_by_group)[1], "\n")
  })
  TRUE
}, error = function(e) {
  cat("MOD_PALUDISME_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_PALUDISME_OK\n") else quit(status = 1)
