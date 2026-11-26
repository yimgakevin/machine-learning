# Vérifie le module sécurité alimentaire en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_secu_alim_ui("secu_alim"))
ids <- c(
  "secu_alim-sa_mode", "secu_alim-run_sa",
  "secu_alim-sa_summary_cards", "secu_alim-sa_verdict",
  "secu_alim-sa_fcs_stats", "secu_alim-sa_fcs_hist",
  "secu_alim-sa_rcsi_stats", "secu_alim-sa_hdds_groups",
  "secu_alim-sa_hhs_stats", "secu_alim-sa_severity_plot",
  "secu_alim-sa_strat_table", "secu_alim-sa_ipc_kpi_cards",
  "secu_alim-sa_household_table", "secu_alim-sa_dl_csv_hh"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_SECU_ALIM_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_SECU_ALIM_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(21)
n <- 40
d <- data.frame(
  FCS = round(runif(n, 10, 90), 1),
  rCSI = round(runif(n, 0, 40), 1),
  HDDS = sample(1:12, n, replace = TRUE),
  HHS = sample(0:6, n, replace = TRUE),
  lcsi_stress = rbinom(n, 1, 0.3),
  lcsi_crisis = rbinom(n, 1, 0.2),
  lcsi_emerg = rbinom(n, 1, 0.1),
  FIES = sample(0:8, n, replace = TRUE),
  MDDW = rbinom(n, 1, 0.55),
  region = rep(c("Nord", "Sud"), n / 2),
  sexe = rep(c("M", "F"), n / 2),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_secu_alim_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("region", "sexe")),
      vars_num = shiny::reactive(c("FCS", "rCSI", "HDDS", "HHS", "FIES"))
    ),
    {
      session$setInputs(
        sa_mode = "all",
        sa_fcs_precalc = "FCS",
        sa_rcsi_precalc = "rCSI",
        sa_hdds_precalc = "HDDS",
        sa_hhs_precalc = "HHS",
        sa_lcsi_stress = "lcsi_stress",
        sa_lcsi_crisis = "lcsi_crisis",
        sa_lcsi_emerg = "lcsi_emerg",
        sa_lcsi_class = "",
        sa_fies_col = "FIES",
        sa_mddw_col = "MDDW",
        sa_strat_var = "region",
        sa_ipc_group_var = "region",
        sa_hdds_cols = character(0),
        run_sa = 1
      )
      cat("sa_summary_cards :", class(output$sa_summary_cards)[1], "\n")
      cat("sa_verdict       :", class(output$sa_verdict)[1], "\n")
      cat("sa_fcs_stats     :", class(output$sa_fcs_stats)[1], "\n")
      cat("sa_fcs_hist      :", class(output$sa_fcs_hist)[1], "\n")
      cat("sa_rcsi_stats    :", class(output$sa_rcsi_stats)[1], "\n")
      cat("sa_hdds_groups   :", class(output$sa_hdds_groups)[1], "\n")
      cat("sa_hhs_stats     :", class(output$sa_hhs_stats)[1], "\n")
      cat("sa_severity_plot :", class(output$sa_severity_plot)[1], "\n")
      cat("sa_strat_table   :", class(output$sa_strat_table)[1], "\n")
      cat("sa_ipc_kpi_cards :", class(output$sa_ipc_kpi_cards)[1], "\n")
      cat("sa_household_table:", class(output$sa_household_table)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_SECU_ALIM_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_SECU_ALIM_OK\n") else quit(status = 1)
