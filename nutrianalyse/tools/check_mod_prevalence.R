# Vérifie le module prévalences SMART en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_prevalence_ui("prevalence"))
ids <- c(
  "prevalence-run_prevalence", "prevalence-sp_design_info",
  "prevalence-sp_global_table_html", "prevalence-sp_forest_global",
  "prevalence-sp_strat_table", "prevalence-sp_forest_strat",
  "prevalence-sp_full_table", "prevalence-dl_smart_table",
  "prevalence-dl_smart_ena", "prevalence-sp_means_forest",
  "prevalence-sp_means_table", "prevalence-sp_cmp_inds",
  "prevalence-sp_cmp_forest", "prevalence-sp_cmp_table",
  "prevalence-sp_icc_table", "prevalence-sp_deff_bar",
  "prevalence-sp_diag_alerts", "prevalence-sp_cluster_sizes_hist",
  "prevalence-sp_cluster_sizes_table"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_PREVALENCE_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_PREVALENCE_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(11)
n <- 60
d <- data.frame(
  GAM = rbinom(n, 1, 0.1),
  SAM = rbinom(n, 1, 0.03),
  MAG_muac = rbinom(n, 1, 0.08),
  Stunting = rbinom(n, 1, 0.25),
  Underweight = rbinom(n, 1, 0.15),
  cluster = rep(paste0("C", 1:6), each = 10),
  region = rep(c("Nord", "Sud"), each = 30),
  poids_sond = runif(n, 0.5, 2),
  FCS = round(runif(n, 10, 80), 1),
  rCSI = round(runif(n, 0, 40), 1),
  HDDS = round(runif(n, 2, 12), 1),
  HHS = round(runif(n, 0, 6), 1),
  sexe = rep(c("M", "F"), n / 2),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_prevalence_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("cluster", "region", "sexe")),
      vars_num = shiny::reactive(c("FCS", "rCSI", "HDDS", "HHS", "poids_sond"))
    ),
    {
      session$setInputs(
        sp_col_cluster = "cluster",
        sp_col_strata = "region",
        sp_col_weights = "poids_sond",
        sp_indicators = c("GAM", "SAM", "MAG_muac", "Stunting", "Underweight"),
        sp_stratifications = c("region", "sexe"),
        sp_cmp_inds = c("GAM", "MAG_muac"),
        sp_means_sa_cols = c("FCS", "rCSI", "HDDS", "HHS"),
        sp_means_group_var = "",
        run_prevalence = 1
      )
      cat("sp_design_info       :", class(output$sp_design_info)[1], "\n")
      cat("sp_global_table_html :", class(output$sp_global_table_html)[1], "\n")
      cat("sp_forest_global     :", class(output$sp_forest_global)[1], "\n")
      cat("sp_strat_table       :", class(output$sp_strat_table)[1], "\n")
      cat("sp_full_table        :", class(output$sp_full_table)[1], "\n")
      cat("sp_means_forest      :", class(output$sp_means_forest)[1], "\n")
      cat("sp_means_table       :", class(output$sp_means_table)[1], "\n")
      cat("sp_cmp_forest        :", class(output$sp_cmp_forest)[1], "\n")
      cat("sp_cmp_table         :", class(output$sp_cmp_table)[1], "\n")
      cat("sp_icc_table         :", class(output$sp_icc_table)[1], "\n")
      cat("sp_deff_bar          :", class(output$sp_deff_bar)[1], "\n")
      cat("sp_diag_alerts       :", class(output$sp_diag_alerts)[1], "\n")
      cat("sp_cluster_sizes_hist:", class(output$sp_cluster_sizes_hist)[1], "\n")
      cat("sp_cluster_sizes_table:", class(output$sp_cluster_sizes_table)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_PREVALENCE_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_PREVALENCE_OK\n") else quit(status = 1)
