build_filter_context <- function(data, reference_data = NULL) {
  n <- if (is.null(data)) 0L else nrow(data)
  n_total <- if (is.null(reference_data)) n else nrow(reference_data)
  list(
    data = data,
    n = as.integer(n),
    n_total = as.integer(n_total),
    share = if (n_total > 0) round(n / n_total * 100, 1) else NA_real_
  )
}

measure_severity <- function(value, indicator = "GAM") {
  if (length(value) == 0 || is.na(value) || !is.finite(value)) return("acceptable")
  s <- if (exists("OMS_SEUILS", inherits = TRUE)) OMS_SEUILS[[indicator]] else NULL
  if (is.null(s)) return("acceptable")
  if (value >= s$critique) "urgence"
  else if (value >= s$serieux) "critique"
  else if (value >= s$alerte) "serieux"
  else if (value >= s$acceptable) "alerte"
  else "acceptable"
}

measure_score_severity <- function(score) {
  if (length(score) == 0 || is.na(score) || !is.finite(score)) return("acceptable")
  if (score >= 80) "acceptable"
  else if (score >= 60) "alerte"
  else if (score >= 40) "serieux"
  else if (score >= 20) "critique"
  else "urgence"
}

measure_zscore_severity <- function(value) {
  if (length(value) == 0 || is.na(value) || !is.finite(value)) return("acceptable")
  if (value < -2) "critique"
  else if (value < -1) "serieux"
  else if (value < -0.5) "alerte"
  else "acceptable"
}

measure_binary <- function(data, column, label = column, indicator = column) {
  if (is.null(data) || !is.data.frame(data) || !column %in% names(data)) return(NULL)
  x <- data[[column]]
  if (is.logical(x)) {
    x <- x[!is.na(x)]
    cases <- sum(x)
  } else if (is.numeric(x)) {
    x <- x[!is.na(x)]
    cases <- sum(x > 0)
  } else {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    cases <- sum(tolower(x) %in% c("1", "true", "oui", "yes"))
  }
  n <- length(x)
  pct <- if (n > 0) round(cases / n * 100, 1) else NA_real_

  ci_low <- NA_real_
  ci_high <- NA_real_
  if (n > 0 && requireNamespace("binom", quietly = TRUE)) {
    ci <- tryCatch(binom::binom.wilson(cases, n), error = function(e) NULL)
    if (!is.null(ci)) {
      ci_low <- round(ci$lower * 100, 1)
      ci_high <- round(ci$upper * 100, 1)
    }
  }

  list(
    column = column,
    label = label,
    kind = "binary",
    n = n,
    cases = cases,
    pct = pct,
    ci_low = ci_low,
    ci_high = ci_high,
    severity = measure_severity(pct, indicator)
  )
}

measure_mean <- function(data, column, label = column) {
  if (is.null(data) || !is.data.frame(data) || !column %in% names(data)) return(NULL)
  x <- data[[column]]
  x <- x[is.finite(x)]
  n <- length(x)
  m <- if (n > 0) round(mean(x), 2) else NA_real_
  s <- if (n > 1) round(stats::sd(x), 2) else NA_real_
  list(
    column = column,
    label = label,
    kind = "mean",
    n = n,
    mean = m,
    sd = s,
    severity = measure_zscore_severity(m)
  )
}

measure_indicator <- function(data, column, label = column, indicator = column) {
  if (column %in% c(".whz", ".haz", ".waz")) {
    return(measure_mean(data, column, label))
  }
  measure_binary(data, column, label, indicator)
}

measure_ci_text <- function(ms, include_n = FALSE) {
  if (is.null(ms) || is.null(ms$pct) || is.na(ms$pct)) return("")
  txt <- sprintf("IC 95%% : [%.1f – %.1f%%]", ms$ci_low, ms$ci_high)
  if (isTRUE(include_n) && !is.na(ms$n)) txt <- paste0(txt, " \u00b7 n=", ms$n)
  txt
}

measure_global_score <- function(data,
                                 indicators = c("GAM", "SAM", "Stunting",
                                                "Underweight", "MUAC_GAM")) {
  if (is.null(data)) return(NULL)
  details <- list()
  for (ind in indicators) {
    ms <- measure_indicator(data, ind)
    if (is.null(ms)) next
    sev <- measure_severity(ms$pct, ind)
    score <- switch(sev,
      acceptable = 95, alerte = 75, serieux = 55,
      critique = 35, urgence = 15, 95
    )
    details[[length(details) + 1]] <- list(
      indicator = ind,
      value = ms$pct,
      severity = sev,
      score = score
    )
  }
  if (length(details) == 0) return(NULL)
  score_global <- round(mean(vapply(details, `[[`, numeric(1), "score")))
  list(
    score = score_global,
    severity = measure_score_severity(score_global),
    details = details
  )
}

measure_card_specs <- function(data,
                               indicators = c("GAM", "SAM", "Stunting",
                                              "Underweight", ".whz", ".haz",
                                              "MUAC_GAM", "MUAC_SAM")) {
  specs <- list(
    GAM = list(column = "GAM", indicator = "GAM",
               label = "GAM (Malnutrition Aiguë Globale)",
               icon = "exclamation-triangle", max_pct = 30),
    SAM = list(column = "SAM", indicator = "SAM",
               label = "SAM (Malnutrition Aiguë Sévère)",
               icon = "heartbeat", max_pct = 10),
    Stunting = list(column = "Stunting", indicator = "Stunting",
                    label = "Retard de croissance (HAZ < -2)",
                    icon = "child", max_pct = 60),
    Underweight = list(column = "Underweight", indicator = "Underweight",
                       label = "Insuffisance pondérale (WAZ < -2)",
                       icon = "weight", max_pct = 40),
    MUAC_GAM = list(column = "MUAC_GAM", indicator = "MUAC_GAM",
                    label = "MUAC GAM (PB < 125mm)",
                    icon = "ruler-horizontal", max_pct = 20),
    MUAC_SAM = list(column = "MUAC_SAM", indicator = "SAM",
                    label = "MUAC SAM (PB < 115mm)",
                    icon = "exclamation", max_pct = NULL),
    .whz = list(column = ".whz", indicator = ".whz",
                label = "WHZ moyen (Poids-pour-Taille)",
                icon = "chart-line", max_pct = NULL),
    .haz = list(column = ".haz", indicator = ".haz",
                label = "HAZ moyen (Taille-pour-Âge)",
                icon = "chart-line", max_pct = NULL)
  )
  out <- list()
  for (id in indicators) {
    spec <- specs[[id]]
    if (is.null(spec) || is.null(data) || !spec$column %in% names(data)) next
    ms <- measure_indicator(data, spec$column, spec$label, spec$indicator)
    if (is.null(ms)) next
    out[[length(out) + 1]] <- list(id = id, spec = spec, measure = ms)
  }
  out
}
