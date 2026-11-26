# Vérifie le module Z-scores en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- paste(as.character(mod_zscore_ui("zscore")), collapse = "")
ids <- c(
  "zscore-zs_col_poids_ui", "zscore-zs_col_taille_ui", "zscore-zs_col_age_ui",
  "zscore-zs_col_sexe_ui", "zscore-zs_val_garcon", "zscore-zs_val_fille",
  "zscore-run_zscore", "zscore-zs_tabs", "zscore-zs_expert_cards",
  "zscore-zs_resume_table", "zscore-zs_strat_var", "zscore-zs_dist_whz",
  "zscore-zs_dist_haz", "zscore-zs_dist_waz", "zscore-zs_donut_gam",
  "zscore-zs_donut_retard", "zscore-zs_donut_insuf", "zscore-zs_stacked_bar",
  "zscore-zs_class_table", "zscore-zs_sex_whz", "zscore-zs_sex_haz",
  "zscore-zs_sex_waz", "zscore-zs_sex_table", "zscore-zs_dl_data",
  "zscore-zs_dl_xlsx", "zscore-zs_data_table", "zscore-zs_quality_expert_dashboard",
  "zscore-zs_flags_summary", "zscore-zs_flags_table", "zscore-zs_muac_kpi_row",
  "zscore-zs_muac_group_ui", "zscore-zs_muac_by_sex", "zscore-zs_muac_dist",
  "zscore-zs_muac_donut", "zscore-zs_muac_by_group", "zscore-zs_muac_vs_whz_panel",
  "zscore-zs_muac_table", "zscore-zs_age_indicator", "zscore-zs_age_boxplot",
  "zscore-zs_age_table", "zscore-zs_dl_rapport", "zscore-zs_dl_rapport_pdf",
  "zscore-zs_rapport_text"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_ZSCORE_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_ZSCORE_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(23)
n <- 60
d <- data.frame(
  region = rep(c("Nord", "Sud"), each = n / 2),
  sexe = rep(c("M", "F"), length.out = n),
  poids = round(runif(n, 7, 16), 1),
  taille = round(runif(n, 65, 105), 1),
  age_mois = sample(6:59, n, replace = TRUE),
  muac = round(runif(n, 100, 150), 0),
  stringsAsFactors = FALSE
)
raw <- shiny::reactiveVal(d)
zsd <- shiny::reactiveVal(NULL)

ok <- tryCatch({
  shiny::testServer(
    mod_zscore_server,
    args = list(df_f = shiny::reactive(d), raw_data = raw, zs_data = zsd),
    {
      session$setInputs(
        zs_col_poids = "poids", zs_col_taille = "taille",
        zs_col_age = "age_mois", zs_col_sexe = "sexe",
        zs_val_garcon = "M", zs_val_fille = "F",
        zs_strat_var = "global", zs_age_indicator = "WHZ_calc",
        zs_muac_group_var = "region", zs_muac_by_sex = FALSE
      )
      cat("col_poids_ui :", class(output$zs_col_poids_ui)[1], "\n")
      cat("col_taille_ui:", class(output$zs_col_taille_ui)[1], "\n")
      cat("col_age_ui   :", class(output$zs_col_age_ui)[1], "\n")
      cat("col_sexe_ui  :", class(output$zs_col_sexe_ui)[1], "\n")

      session$setInputs(run_zscore = 1)

      res <- zsd()
      if (is.null(res) || !all(c("WHZ_calc", "HAZ_calc", "WAZ_calc") %in% names(res))) {
        stop("zs_data non alimenté après run_zscore")
      }
      cat("zs_data      :", nrow(res), "lignes |",
          sum(!is.na(res$WHZ_calc)), "WHZ valides\n")
      if (!all(c("WHZ_calc", "HAZ_calc", "WAZ_calc") %in% names(raw()))) {
        stop("injection WHZ_calc/HAZ_calc/WAZ_calc absente de raw_data")
      }
      cat("raw_data     : injection OK (n =", nrow(raw()), ")\n")

      cat("resume_table :", class(output$zs_resume_table)[1], "\n")
      cat("dist_whz     :", class(output$zs_dist_whz)[1], "\n")
      cat("dist_haz     :", class(output$zs_dist_haz)[1], "\n")
      cat("dist_waz     :", class(output$zs_dist_waz)[1], "\n")
      cat("donut_gam    :", class(output$zs_donut_gam)[1], "\n")
      cat("stacked_bar  :", class(output$zs_stacked_bar)[1], "\n")
      cat("class_table  :", class(output$zs_class_table)[1], "\n")
      cat("sex_whz      :", class(output$zs_sex_whz)[1], "\n")
      cat("sex_table    :", class(output$zs_sex_table)[1], "\n")
      cat("data_table   :", class(output$zs_data_table)[1], "\n")
      cat("flags_summary:", class(output$zs_flags_summary)[1], "\n")
      cat("flags_table  :", class(output$zs_flags_table)[1], "\n")
      cat("expert_cards :", class(output$zs_expert_cards)[1], "\n")
      cat("quality_dash :", class(output$zs_quality_expert_dashboard)[1], "\n")
      cat("quality_dt   :", class(output$zs_quality_criteria_dt)[1], "\n")
      cat("rapport_text :", class(output$zs_rapport_text)[1], "\n")
      cat("muac_kpi     :", class(output$zs_muac_kpi_row)[1], "\n")
      cat("muac_dist    :", class(output$zs_muac_dist)[1], "\n")
      cat("muac_donut   :", class(output$zs_muac_donut)[1], "\n")
      cat("muac_by_group:", class(output$zs_muac_by_group)[1], "\n")
      cat("muac_vs_whz  :", class(output$zs_muac_vs_whz_panel)[1], "\n")
      cat("muac_table   :", class(output$zs_muac_table)[1], "\n")
      cat("age_boxplot  :", class(output$zs_age_boxplot)[1], "\n")
      cat("age_table    :", class(output$zs_age_table)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_ZSCORE_FAIL:", paste(conditionMessage(e), collapse = " | "), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_ZSCORE_OK\n") else quit(status = 1)
