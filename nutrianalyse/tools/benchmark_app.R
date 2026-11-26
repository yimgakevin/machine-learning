# Benchmark de l'application NutriAnalyse Pro v2
# Mesure : chargement, construction UI, calculs Z-scores, rendus de modules,
# démarrage HTTP réel.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

fmt <- function(x) sprintf("%.2f s", as.numeric(x, units = "secs"))
res <- list()

# ── 1. Chargement complet de l'application ────────────────────────────────────
t_load <- system.time({
  invisible(capture.output(source("app_v2_final.R", local = FALSE), type = "output"))
})
res$chargement <- t_load["elapsed"]
cat("CHARGEMENT_COMPLET |", fmt(t_load), "\n")

# ── 2. Construction des UI ────────────────────────────────────────────────────
t_ui_acc <- system.time(invisible(mod_accueil_ui("accueil")))
t_ui_zs  <- system.time(invisible(mod_zscore_ui("zscore")))
res$ui_accueil <- t_ui_acc["elapsed"]
res$ui_zscore  <- t_ui_zs["elapsed"]
cat("UI_ACCUEIL |", fmt(t_ui_acc), "| UI_ZSCORE |", fmt(t_ui_zs), "\n")

# ── 3. Calcul pur des Z-scores OMS ────────────────────────────────────────────
make_zs_data <- function(n) {
  set.seed(42)
  data.frame(
    region = rep(c("Nord", "Sud", "Est"), length.out = n),
    sexe = rep(c("M", "F"), length.out = n),
    poids = round(runif(n, 7, 16), 1),
    taille = round(runif(n, 65, 105), 1),
    age_mois = sample(6:59, n, replace = TRUE),
    muac = round(runif(n, 100, 150), 0),
    stringsAsFactors = FALSE
  )
}
calc_one <- function(d) {
  calcul_zscores_oms(d, col_poids = "poids", col_taille = "taille",
                     col_age = "age_mois", col_sexe = "sexe")
}
d500  <- make_zs_data(500)
d2000 <- make_zs_data(2000)
mb <- microbenchmark::microbenchmark(
  `500`  = calc_one(d500),
  `2000` = calc_one(d2000),
  times = 3
)
agg <- aggregate(time ~ expr, mb, median)
for (i in seq_len(nrow(agg))) {
  n <- as.character(agg$expr[i])
  res[[paste0("zscore_calc_", n)]] <- agg$time[i] / 1e9
  cat("CALC_ZSCORE_", n, " | ", fmt(agg$time[i] / 1e9), " (médiane 3 runs)\n", sep = "")
}

# ── 4. Module Z-scores : calcul + rendus ─────────────────────────────────────
d_zs <- make_zs_data(500)
raw_zs <- shiny::reactiveVal(d_zs)
zsd <- shiny::reactiveVal(NULL)
t_mod_zs <- system.time({
  shiny::testServer(
    mod_zscore_server,
    args = list(df_f = shiny::reactive(d_zs), raw_data = raw_zs, zs_data = zsd),
    {
      session$setInputs(
        zs_col_poids = "poids", zs_col_taille = "taille",
        zs_col_age = "age_mois", zs_col_sexe = "sexe",
        zs_val_garcon = "M", zs_val_fille = "F",
        zs_strat_var = "global", zs_age_indicator = "WHZ_calc",
        zs_muac_group_var = "region", zs_muac_by_sex = FALSE
      )
      session$setInputs(run_zscore = 1)
      stopifnot(!is.null(zsd()))
      invisible(output$zs_resume_table)
      invisible(output$zs_dist_whz)
      invisible(output$zs_class_table)
      invisible(output$zs_data_table)
      invisible(output$zs_flags_table)
      invisible(output$zs_muac_table)
      invisible(output$zs_age_table)
    }
  )
})
res$module_zscore_500 <- t_mod_zs["elapsed"]
cat("MODULE_ZSCORE_500 |", fmt(t_mod_zs), "(calcul + 7 rendus)\n")

# ── 5. Module Accueil : rendus KPI + graphiques ─────────────────────────────
set.seed(19)
n <- 500
d_acc <- data.frame(
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
mc <- list(region = "region", dept = "dept", sexe = "sexe", whz = ".whz",
           haz = ".haz", waz = ".waz", fcs = ".fcs", rcsi = ".rcsi",
           hdds = ".hdds", survey = "round")
t_mod_acc <- system.time({
  shiny::testServer(
    mod_accueil_server,
    args = list(
      df_f = shiny::reactive(d_acc), mapped_data = shiny::reactive(d_acc),
      map_cols = shiny::reactive(mc),
      vars_num = shiny::reactive(c("GAM", "SAM", ".whz")),
      vars_cat = shiny::reactive(c("region", "sexe")),
      filter_context = shiny::reactive(list(data = d_acc, n = nrow(d_acc), n_total = nrow(d_acc))),
      has_oms = shiny::reactive(TRUE), f_region = shiny::reactive(NULL),
      accueil_click = shiny::reactive(NULL),
      parent_session = shiny::MockShinySession$new(),
      gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis"),
      lang = shiny::reactive("fr")
    ),
    {
      session$setInputs(sa_grp_var = "region", fcs_rcsi_color_var = "region",
                        temporal_grp_var = "region", crosstab_grp_var = "region",
                        dv_map_level = "1")
      invisible(output$dashboard_filters)
      for (o in c("accueil_kpi_premium", "accueil_kpi_qualite", "accueil_kpi_secu",
                  "accueil_prev_chart", "accueil_sa_charts_row",
                  "accueil_crosstab_row", "accueil_dv_map_row")) {
        try(invisible(output[[o]]), silent = TRUE)
      }
    }
  )
})
res$module_accueil_500 <- t_mod_acc["elapsed"]
cat("MODULE_ACCUEIL_500 |", fmt(t_mod_acc), "(8 rendus)\n")

# ── 6. Démarrage HTTP réel ───────────────────────────────────────────────────
port <- 8765L
http_ok <- function(url) {
  tryCatch({
    h <- curl::new_handle(timeout = 1, connecttimeout = 1)
    r <- curl::curl_fetch_memory(url, handle = h)
    r$status_code == 200L
  }, error = function(e) FALSE, warning = function(w) FALSE)
}
t_http <- system.time({
  p <- callr::r_bg(
    function(port) {
      shiny::runApp(".", port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port), stdout = "|", stderr = "|"
  )
  on.exit(try(p$kill(), silent = TRUE), add = TRUE)
  ok <- FALSE
  for (i in seq_len(600)) {
    Sys.sleep(0.1)
    if (http_ok(sprintf("http://127.0.0.1:%d/", port))) { ok <- TRUE; break }
    if (!p$is_alive()) break
  }
  if (!ok) stop("L'application n'a pas répondu dans le délai imparti")
  invisible(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", port)))
  p$kill()
})
res$demarrage_http <- t_http["elapsed"]
cat("DEMARRAGE_HTTP |", fmt(t_http), "(processus fils + 1re réponse 200)\n")

# ── 7. Rapport ───────────────────────────────────────────────────────────────
report <- c(
  "# Benchmark — NutriAnalyse Pro v2",
  "",
  paste0("Date : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("R : ", R.version.string),
  paste0("Cœurs : ", parallel::detectCores()),
  "",
  "| Mesure | Durée |",
  "|---|---|",
  paste0("| Chargement complet (`source(app_v2_final.R)`) | ", fmt(res$chargement), " |"),
  paste0("| Construction UI Accueil | ", fmt(res$ui_accueil), " |"),
  paste0("| Construction UI Z-scores | ", fmt(res$ui_zscore), " |"),
  paste0("| Calcul Z-scores OMS — 500 lignes | ", fmt(res$zscore_calc_500), " |"),
  paste0("| Calcul Z-scores OMS — 2 000 lignes | ", fmt(res$zscore_calc_2000), " |"),
  paste0("| Module Z-scores — 500 lignes (calcul + 7 rendus) | ", fmt(res$module_zscore_500), " |"),
  paste0("| Module Accueil — 500 lignes (8 rendus) | ", fmt(res$module_accueil_500), " |"),
  paste0("| Démarrage HTTP + 1re réponse 200 | ", fmt(res$demarrage_http), " |"),
  ""
)
writeLines(report, "tools/benchmark_results.md", useBytes = TRUE)
cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
