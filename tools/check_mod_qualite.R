# Vérifie le module qualité SMART en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_qualite_ui("qualite"))
ids <- c(
  "qualite-run_qualite", "qualite-sq_plausibility_html",
  "qualite-sq_flags_table", "qualite-sq_flags_plot",
  "qualite-sq_dp_table", "qualite-sq_dp_plot",
  "qualite-sq_age_summary", "qualite-sq_age_plot",
  "qualite-sq_team_table", "qualite-sq_team_plot",
  "qualite-sq_completude_table"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_QUALITE_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_QUALITE_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(7)
n <- 40
d <- data.frame(
  WHZ = rnorm(n, -0.4, 1.1),
  HAZ = rnorm(n, -0.7, 1.2),
  WAZ = rnorm(n, -0.5, 1.0),
  MUAC = round(rnorm(n, 135, 12)),
  sexe = rep(c("M", "F"), n / 2),
  age_mois = sample(6:59, n, replace = TRUE),
  poids = round(rnorm(n, 12, 3), 1),
  taille = round(rnorm(n, 85, 10), 1),
  equipe = rep(c("E1", "E2", "E3", "E4"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_qualite_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("sexe", "equipe")),
      vars_num = shiny::reactive(c("WHZ", "HAZ", "WAZ", "MUAC", "age_mois", "poids", "taille"))
    ),
    {
      session$setInputs(
        sq_col_whz = "WHZ", sq_col_haz = "HAZ", sq_col_waz = "WAZ",
        sq_col_muac = "MUAC", sq_col_sex = "sexe", sq_col_age = "age_mois",
        sq_col_weight = "poids", sq_col_height = "taille", sq_col_team = "equipe",
        sq_dp_var = "MUAC", run_qualite = 1
      )
      cat("sq_plausibility_html :", class(output$sq_plausibility_html)[1], "\n")
      cat("sq_flags_table       :", class(output$sq_flags_table)[1], "\n")
      cat("sq_flags_plot        :", class(output$sq_flags_plot)[1], "\n")
      cat("sq_dp_table          :", class(output$sq_dp_table)[1], "\n")
      cat("sq_dp_plot           :", class(output$sq_dp_plot)[1], "\n")
      cat("sq_age_summary       :", class(output$sq_age_summary)[1], "\n")
      cat("sq_age_plot          :", class(output$sq_age_plot)[1], "\n")
      cat("sq_team_table        :", class(output$sq_team_table)[1], "\n")
      cat("sq_team_plot         :", class(output$sq_team_plot)[1], "\n")
      cat("sq_completude_table  :", class(output$sq_completude_table)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_QUALITE_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_QUALITE_OK\n") else quit(status = 1)
