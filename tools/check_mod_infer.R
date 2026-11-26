# Vérifie le module inférentiel en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_infer_ui("infer"))
ids <- c(
  "infer-infer_x_ui", "infer-infer_y_ui", "infer-infer_alpha",
  "infer-run_infer", "infer-infer_summary_table", "infer-infer_all_cards",
  "infer-infer_plot", "infer-multi_outcome_ui", "infer-multi_forest",
  "infer-infer_expert_tabs", "infer-infer_paired_plot",
  "infer-infer_power_curve", "infer-infer_expert_summary_table"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_INFER_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_INFER_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(71)
n <- 60
d <- data.frame(
  GAM = rbinom(n, 1, 0.2),
  WHZ = rnorm(n, -0.3, 1.1),
  HAZ = rnorm(n, -0.6, 1.2),
  age_mois = sample(6:59, n, replace = TRUE),
  sexe = rep(c("M", "F"), n / 2),
  region = rep(c("Nord", "Sud"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_infer_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("GAM", "sexe", "region")),
      vars_num = shiny::reactive(c("WHZ", "HAZ", "age_mois"))
    ),
    {
      session$setInputs(
        infer_x = "sexe", infer_y = "WHZ", infer_alpha = 0.05,
        multi_out = "GAM", multi_vars = c("WHZ", "age_mois"),
        infer_paired_x1 = "WHZ", infer_paired_x2 = "HAZ",
        infer_paired_alpha = 0.05,
        infer_power_n = 60, infer_power_effect = 0.5,
        infer_power_alpha = 0.05, infer_power_k = 2,
        infer_power_type = "two.sample",
        run_infer = 1
      )
      cat("infer_type_badge   :", class(output$infer_type_badge)[1], "\n")
      cat("infer_summary_table:", class(output$infer_summary_table)[1], "\n")
      cat("infer_all_cards    :", class(output$infer_all_cards)[1], "\n")
      cat("infer_plot         :", class(output$infer_plot)[1], "\n")
      cat("multi_summary      :", class(output$multi_summary)[1], "\n")
      cat("multi_forest       :", class(output$multi_forest)[1], "\n")

      session$setInputs(run_infer_paired = 1)
      cat("infer_paired_plot  :", class(output$infer_paired_plot)[1], "\n")
      cat("infer_paired_cards :", class(output$infer_paired_cards)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_INFER_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_INFER_OK\n") else quit(status = 1)
