# Vérifie les modules uni et biv en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

# ── UI : IDs namespacés ───────────────────────────────────────────────────────
check_ui <- function(label, html, ids) {
  for (id in ids) {
    if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
      cat(label, "UI_FAIL: ID manquant", id, "\n")
      quit(status = 1)
    }
  }
  cat(label, "UI_OK |", length(ids), "IDs namespacés\n")
}

check_ui("MOD_UNI", as.character(mod_uni_ui("uni")), c(
  "uni-uni_type", "uni-uni_var_ui", "uni-uni_stats_tbl",
  "uni-uni_dl_plot", "uni-uni_plot_ui"
))
check_ui("MOD_BIV", as.character(mod_biv_ui("biv")), c(
  "biv-biv_type", "biv-biv_x_ui", "biv-biv_y_ui",
  "biv-biv_test_result", "biv-biv_dl_plot", "biv-biv_plot"
))

# ── Données synthétiques ──────────────────────────────────────────────────────
d <- data.frame(
  region = rep(c("A", "B"), 10),
  sexe = rep(c("M", "F"), 10),
  age_group5 = rep(c("0-5", "6-11"), 10),
  WHZ = c(-1.2, -2.5, 0.3, -3.1, -0.5, -2.2, 0.1, -1.8, -0.9, -2.7,
          0.4, -1.1, -2.9, 0.2, -1.6, -0.3, -2.4, 0.6, -1.4, -0.8),
  HAZ = c(-1.0, -2.1, -0.2, -2.8, -0.7, -1.9, -0.4, -2.3, -1.2, -2.6,
          -0.1, -1.5, -2.5, 0.1, -1.3, -0.6, -2.0, 0.3, -1.1, -0.9),
  WAZ = c(-1.1, -2.2, -0.1, -2.9, -0.6, -2.0, -0.3, -2.1, -1.0, -2.5,
          -0.2, -1.4, -2.7, 0.0, -1.2, -0.5, -2.2, 0.2, -1.0, -0.7),
  stringsAsFactors = FALSE
)
vars_cat_r <- shiny::reactive(c("region", "sexe", "age_group5"))
vars_num_r <- shiny::reactive(c("WHZ", "HAZ", "WAZ"))
gpal_r <- function(n, type = "cat") grDevices::hcl.colors(n, "viridis")

# ── Module uni ────────────────────────────────────────────────────────────────
ok_uni <- tryCatch({
  shiny::testServer(
    mod_uni_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = vars_cat_r,
      vars_cat_pie = vars_cat_r,
      vars_num = vars_num_r,
      gpal = gpal_r,
      gprimary = function() "#0077B6",
      gsecondary = function() "#F77F00",
      global_palette = shiny::reactive(NULL)
    ),
    {
      session$setInputs(
        uni_type = "histo", uni_var = "WHZ",
        uni_pal = "Auto (Global)", uni_lbl = TRUE
      )
      cat("uni_var_ui     :", class(output$uni_var_ui)[1], "\n")
      cat("uni_stats_tbl  :", class(output$uni_stats_tbl)[1], "\n")
      cat("uni_plot_ui    :", class(output$uni_plot_ui)[1], "\n")
      cat("uni_plot_out   :", class(output$uni_plot_out)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_UNI_FAIL:", conditionMessage(e), "\n")
  FALSE
})

# ── Module biv ────────────────────────────────────────────────────────────────
ok_biv <- tryCatch({
  shiny::testServer(
    mod_biv_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = vars_cat_r,
      vars_num = vars_num_r,
      gpal = gpal_r,
      global_palette = shiny::reactive(NULL)
    ),
    {
      session$setInputs(
        biv_type = "scatter", biv_x = "WHZ", biv_y = "HAZ",
        biv_z = "Aucune", biv_pal = "Auto (Global)"
      )
      cat("biv_x_ui         :", class(output$biv_x_ui)[1], "\n")
      cat("biv_y_ui         :", class(output$biv_y_ui)[1], "\n")
      cat("biv_test_result  :", class(output$biv_test_result)[1], "\n")
      cat("biv_interpretation:", class(output$biv_interpretation)[1], "\n")
      cat("biv_plot         :", class(output$biv_plot)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_BIV_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok_uni && ok_biv) cat("CHECK_MOD_UNI_BIV_OK\n") else quit(status = 1)
