# Vérifie le module dplyr en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_dplyr_ui("dplyr"))
ids <- c(
  "dplyr-dplyr_filter_ui", "dplyr-dplyr_apply_filter",
  "dplyr-dplyr_result_tbl", "dplyr-dplyr_dl",
  "dplyr-dplyr_code", "dplyr-dplyr_code_result",
  "dplyr-dplyr_plot_out", "dplyr-dplyr_pivot_dir",
  "dplyr-dplyr_across_fn", "dplyr-dplyr_slice_n",
  "dplyr-dplyr_transmute_ui"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_DPLYR_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_DPLYR_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(41)
n <- 40
d <- data.frame(
  WHZ = rnorm(n, -0.2, 1.1),
  age_mois = sample(6:59, n, replace = TRUE),
  poids = round(rnorm(n, 12, 3), 1),
  sexe = rep(c("M", "F"), n / 2),
  region = rep(c("Nord", "Sud"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_dplyr_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("sexe", "region")),
      vars_num = shiny::reactive(c("WHZ", "age_mois", "poids")),
      gpal = function(n, type = "cat") grDevices::rainbow(n),
      gprimary = function() "#0077B6"
    ),
    {
      session$setInputs(
        dplyr_f_var = "sexe",
        dplyr_f_vals = c("M", "F"),
        dplyr_sel_cols = c("WHZ", "sexe"),
        dplyr_new_col = "WHZ_pos",
        dplyr_expr = "WHZ > 0",
        dplyr_arr_col = "WHZ",
        dplyr_order = "desc",
        dplyr_grp_cols = "sexe",
        dplyr_sum_fns = c("n", "mean"),
        dplyr_sum_vars = "WHZ",
        dplyr_ren_old = "WHZ",
        dplyr_ren_new = "WHZ_renamed",
        dplyr_dist_cols = "sexe",
        dplyr_slice_type = "head",
        dplyr_slice_n = 10,
        dplyr_cnt_cols = "sexe",
        dplyr_cnt_wt = "",
        dplyr_count_mode = "count",
        dplyr_pivot_dir = "longer",
        dplyr_piv_long_cols = c("WHZ", "poids"),
        dplyr_pivot_names_to = "variable",
        dplyr_pivot_values_to = "valeur",
        dplyr_na_op = "drop",
        dplyr_na_cols = "WHZ",
        dplyr_across_verb = "summarise",
        dplyr_across_fn = "mean",
        dplyr_ac_cols = "WHZ",
        dplyr_ac_grp = "sexe",
        dplyr_px = "sexe",
        dplyr_py = "WHZ",
        dplyr_plot_type = "bar",
        dplyr_code = "df_f() |> head(5)",
        dplyr_apply_filter = 1
      )
      cat("dplyr_filter_ui  :", class(output$dplyr_filter_ui)[1], "\n")
      cat("dplyr_result_tbl :", class(output$dplyr_result_tbl)[1], "\n")
      cat("dplyr_nrow_ui    :", class(output$dplyr_nrow_ui)[1], "\n")
      cat("dplyr_stats_quick:", class(output$dplyr_stats_quick_ui)[1], "\n")
      cat("dplyr_code_result:", class(output$dplyr_code_result)[1], "\n")

      session$setInputs(dplyr_apply_mutate = 1)
      cat("apres mutate     :", class(output$dplyr_result_tbl)[1], "\n")

      session$setInputs(dplyr_plot_btn = 1)
      cat("dplyr_plot_out   :", class(output$dplyr_plot_out)[1], "\n")

      session$setInputs(dplyr_apply_summarise = 1)
      cat("apres summarise  :", class(output$dplyr_result_tbl)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_DPLYR_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_DPLYR_OK\n") else quit(status = 1)
