# Vérifie le module Accueil en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_accueil_ui("accueil"))
ids <- c(
  "accueil-accueil_badge", "accueil-dash_filter_counter", "accueil-btn_reset_filters",
  "accueil-dashboard_filters_label", "accueil-dashboard_filters",
  "accueil-accueil_score_bar", "accueil-accueil_kpi_premium",
  "accueil-accueil_kpi_qualite", "accueil-accueil_kpi_secu", "accueil-accueil_alertes",
  "accueil-accueil_prev_chart", "accueil-accueil_gauge", "accueil-accueil_seuil_badge",
  "accueil-accueil_zscore", "accueil-accueil_rapport_bar",
  "accueil-accueil_sa_charts_row", "accueil-accueil_fcs_rcsi_scatter",
  "accueil-accueil_temporal_row", "accueil-accueil_crosstab_row",
  "accueil-accueil_dv_map_row"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_ACCUEIL_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
if (!grepl('id="dashboard_filter_details"', html, fixed = TRUE)) {
  cat("MOD_ACCUEIL_UI_FAIL: crochet CSS dashboard_filter_details perdu\n")
  quit(status = 1)
}
cat("MOD_ACCUEIL_UI_OK |", length(ids), "IDs namespacés + crochet CSS conservé\n")

set.seed(19)
n <- 50
d <- data.frame(
  region = rep(c("Nord", "Sud"), each = n / 2),
  dept = rep(c("D1", "D2"), length.out = n),
  sexe = sample(c("M", "F"), n, replace = TRUE),
  GAM = sample(c(TRUE, FALSE), n, replace = TRUE),
  SAM = sample(c(TRUE, FALSE), n, replace = TRUE),
  Stunting = sample(c(TRUE, FALSE), n, replace = TRUE),
  Underweight = sample(c(TRUE, FALSE), n, replace = TRUE),
  .whz = rnorm(n), .haz = rnorm(n), .waz = rnorm(n),
  .fcs = sample(0:12, n, replace = TRUE),
  .rcsi = sample(0:5, n, replace = TRUE),
  .hdds = sample(0:12, n, replace = TRUE),
  round = rep(c("R1", "R2"), length.out = n),
  stringsAsFactors = FALSE
)
m <- list(region = "region", dept = "dept", sexe = "sexe", whz = ".whz",
          haz = ".haz", waz = ".waz", fcs = ".fcs", rcsi = ".rcsi", hdds = ".hdds",
          survey = "round")

ok <- tryCatch({
  shiny::testServer(
    mod_accueil_server,
    args = list(
      df_f = shiny::reactive(d),
      mapped_data = shiny::reactive(d),
      map_cols = shiny::reactive(m),
      vars_num = shiny::reactive(c("GAM", "SAM", ".whz")),
      vars_cat = shiny::reactive(c("region", "sexe")),
      filter_context = shiny::reactive(list(data = d, n = nrow(d), n_total = nrow(d))),
      has_oms = shiny::reactive(TRUE),
      f_region = shiny::reactive(NULL),
      accueil_click = shiny::reactive(NULL),
      parent_session = shiny::MockShinySession$new(),
      gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis"),
      lang = shiny::reactive("fr")
    ),
    {
      session$setInputs(
        sa_grp_var = "region", fcs_rcsi_color_var = "region",
        temporal_grp_var = "region", crosstab_grp_var = "region", dv_map_level = "1"
      )
      filters_html <- paste(as.character(output$dashboard_filters), collapse = "")
      cat("filters_html  :", nchar(filters_html), "chars\n")
      if (!grepl('id="f_region"', filters_html, fixed = TRUE)) {
        stop("f_region n'est pas un ID global")
      }
      cat("badge         :", class(output$f_active_badge)[1], "\n")
      cat("filter_label  :", class(output$dashboard_filters_label)[1], "\n")
      cat("counter       :", class(output$dash_filter_counter)[1], "\n")
      cat("score_bar     :", class(output$accueil_score_bar)[1], "\n")
      cat("kpi_premium   :", class(output$accueil_kpi_premium)[1], "\n")
      cat("kpi_qualite   :", class(output$accueil_kpi_qualite)[1], "\n")
      cat("kpi_secu      :", class(output$accueil_kpi_secu)[1], "\n")
      cat("alertes       :", class(output$accueil_alertes)[1], "\n")
      cat("prev_chart    :", class(output$accueil_prev_chart)[1], "\n")
      cat("gauge         :", class(output$accueil_gauge)[1], "\n")
      cat("seuil_badge   :", class(output$accueil_seuil_badge)[1], "\n")
      cat("zscore        :", class(output$accueil_zscore)[1], "\n")
      cat("sa_charts_row :", class(output$accueil_sa_charts_row)[1], "\n")
      cat("fcs_rcsi      :", class(output$accueil_fcs_rcsi_scatter)[1], "\n")
      cat("temporal_row  :", class(output$accueil_temporal_row)[1], "\n")
      cat("crosstab_row  :", class(output$accueil_crosstab_row)[1], "\n")
      cat("rapport_bar   :", class(output$accueil_rapport_bar)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_ACCUEIL_FAIL:", paste(conditionMessage(e), collapse = " | "), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_ACCUEIL_OK\n") else quit(status = 1)
