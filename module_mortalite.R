# ══════════════════════════════════════════════════════════════════════════════
# module_mortalite.R — Mortalité SMART (CMR / U5MR)
# Standards : SMART Methodology v2, Sphère, OMS
# Auteur : NutriAnalyse Pro v2
# ══════════════════════════════════════════════════════════════════════════════

# ── Seuils de référence ──────────────────────────────────────────────────────
MORTALITY_THRESHOLDS <- list(
  cmr  = list(normal = 0.5, alerte = 1.0, urgence = 2.0,
              labels = c("Normal (< 0.5)", "Élevé (0.5-1.0)", "Urgence (1.0-2.0)", "Hors contrôle (≥ 2.0)"),
              colors = c("#27ae60", "#f39c12", "#e74c3c", "#8e44ad")),
  u5mr = list(normal = 1.0, alerte = 2.0, urgence = 4.0,
              labels = c("Normal (< 1.0)", "Élevé (1.0-2.0)", "Urgence (2.0-4.0)", "Hors contrôle (≥ 4.0)"),
              colors = c("#27ae60", "#f39c12", "#e74c3c", "#8e44ad"))
)

# ── Calcul du CMR pondéré ────────────────────────────────────────────────────
#' @param d data.frame
#' @param col_deaths colonne nombre de décès (total ménage)
#' @param col_pop_start population début rappel
#' @param col_pop_end population fin rappel
#' @param col_recall_days durée du rappel en jours
#' @param col_cluster grappe (optionnel)
#' @param col_weight poids d'échantillonnage (optionnel)
calc_cmr <- function(d, col_deaths, col_pop_start, col_pop_end,
                     col_recall_days = NULL, recall_days = 90,
                     col_cluster = NULL, col_weight = NULL) {
  tryCatch({
    deaths   <- as.numeric(d[[col_deaths]])
    pop_s    <- as.numeric(d[[col_pop_start]])
    pop_e    <- as.numeric(d[[col_pop_end]])
    rdays    <- if (!is.null(col_recall_days)) as.numeric(d[[col_recall_days]]) else rep(recall_days, nrow(d))

    # Person-time mid-period
    mid_pop     <- (pop_s + pop_e) / 2
    person_time <- mid_pop * rdays

    valid <- !is.na(deaths) & !is.na(person_time) & person_time > 0
    total_deaths <- sum(deaths[valid], na.rm = TRUE)
    total_pt     <- sum(person_time[valid], na.rm = TRUE)

    cmr_rate <- total_deaths / total_pt * 10000

    # IC 95% (Poisson)
    ci_lo <- qpois(0.025, total_deaths) / total_pt * 10000
    ci_hi <- qpois(0.975, total_deaths) / total_pt * 10000

    # Classification
    severity <- case_when(
      cmr_rate < 0.5 ~ "Normal",
      cmr_rate < 1.0 ~ "Élevé",
      cmr_rate < 2.0 ~ "Urgence",
      TRUE           ~ "Hors contrôle"
    )

    list(
      rate      = round(cmr_rate, 4),
      ci_lower  = round(ci_lo, 4),
      ci_upper  = round(ci_hi, 4),
      severity  = severity,
      n_deaths  = total_deaths,
      n_hh      = sum(valid),
      person_time = round(total_pt),
      population  = round(sum(mid_pop[valid])),
      recall_days = round(mean(rdays[valid]))
    )
  }, error = function(e) list(rate = NA, error = conditionMessage(e)))
}

# ── Calcul du U5MR ───────────────────────────────────────────────────────────
calc_u5mr <- function(d, col_deaths, col_age_deceased, col_pop_u5,
                      col_pop_start = NULL, col_pop_end = NULL,
                      col_recall_days = NULL, recall_days = 90) {
  tryCatch({
    deaths_all <- as.numeric(d[[col_deaths]])
    age_dec    <- as.numeric(d[[col_age_deceased]])
    pop_u5     <- as.numeric(d[[col_pop_u5]])
    rdays      <- if (!is.null(col_recall_days)) as.numeric(d[[col_recall_days]]) else rep(recall_days, nrow(d))

    # Décès < 5 ans (si age décédé renseigné et < 5)
    u5_deaths <- ifelse(!is.na(deaths_all) & deaths_all > 0 & !is.na(age_dec) & age_dec < 5,
                        deaths_all, 0)

    person_time_u5 <- pop_u5 * rdays
    valid <- !is.na(person_time_u5) & person_time_u5 > 0

    total_u5_deaths <- sum(u5_deaths[valid], na.rm = TRUE)
    total_pt_u5     <- sum(person_time_u5[valid], na.rm = TRUE)

    u5mr_rate <- total_u5_deaths / total_pt_u5 * 10000

    ci_lo <- qpois(0.025, total_u5_deaths) / total_pt_u5 * 10000
    ci_hi <- qpois(0.975, total_u5_deaths) / total_pt_u5 * 10000

    severity <- case_when(
      u5mr_rate < 1.0 ~ "Normal",
      u5mr_rate < 2.0 ~ "Élevé",
      u5mr_rate < 4.0 ~ "Urgence",
      TRUE            ~ "Hors contrôle"
    )

    list(
      rate      = round(u5mr_rate, 4),
      ci_lower  = round(ci_lo, 4),
      ci_upper  = round(ci_hi, 4),
      severity  = severity,
      n_deaths  = total_u5_deaths,
      n_hh      = sum(valid),
      person_time = round(total_pt_u5),
      population_u5 = round(sum(pop_u5[valid], na.rm = TRUE))
    )
  }, error = function(e) list(rate = NA, error = conditionMessage(e)))
}

# ── Causes de décès ──────────────────────────────────────────────────────────
calc_cause_death <- function(d, col_cause, col_deaths = NULL) {
  tryCatch({
    causes <- d[[col_cause]]
    valid  <- !is.na(causes) & causes != ""

    freq <- as.data.frame(table(causes[valid]), stringsAsFactors = FALSE)
    names(freq) <- c("Cause", "N")
    freq <- freq |> dplyr::arrange(dplyr::desc(N)) |>
      dplyr::mutate(Pct = round(N / sum(N) * 100, 1))

    list(table = freq, n_total = sum(freq$N), n_missing = sum(!valid))
  }, error = function(e) list(table = data.frame(), error = conditionMessage(e)))
}

# ── Pyramide des décès par âge et sexe ───────────────────────────────────────
plot_mortality_pyramid <- function(d, col_age, col_sex) {
  tryCatch({
    df_plot <- data.frame(
      age = as.numeric(d[[col_age]]),
      sex = d[[col_sex]]
    ) |> dplyr::filter(!is.na(age) & !is.na(sex))

    df_plot$age_grp <- cut(df_plot$age,
                           breaks = c(0, 1, 5, 15, 30, 50, 70, Inf),
                           labels = c("< 1", "1-4", "5-14", "15-29", "30-49", "50-69", "70+"),
                           right = FALSE)

    counts <- df_plot |>
      dplyr::count(age_grp, sex) |>
      dplyr::mutate(n = ifelse(sex == "F", -n, n))

    ggplot2::ggplot(counts, ggplot2::aes(x = age_grp, y = n, fill = sex)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("M" = "#3498db", "F" = "#e74c3c"),
                                 labels = c("M" = "Masculin", "F" = "Féminin")) +
      ggplot2::labs(title = "Pyramide des décès par âge et sexe",
                    x = "Groupe d'âge", y = "Nombre de décès", fill = "Sexe") +
      ggplot2::theme_minimal()
  }, error = function(e) NULL)
}

# ── Graphique causes de décès ────────────────────────────────────────────────
plot_cause_death <- function(cause_result) {
  tryCatch({
    df <- cause_result$table
    if (is.null(df) || nrow(df) == 0) return(NULL)

    ggplot2::ggplot(df, ggplot2::aes(x = reorder(Cause, N), y = N)) +
      ggplot2::geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.85) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(N, " (", Pct, "%)")),
                         hjust = -0.1, size = 3.5) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Causes de décès",
                    x = "", y = "Nombre de décès") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) NULL)
}

# ── KPI Cards HTML ───────────────────────────────────────────────────────────
render_mortality_kpi <- function(cmr_res = NULL, u5mr_res = NULL) {
  cards <- list()

  make_card <- function(title, rate, ci_lo, ci_hi, severity, color, n_deaths) {
    sprintf(
      '<div style="display:inline-block;width:280px;margin:8px;padding:16px;
       background:%s;color:white;border-radius:12px;text-align:center;">
       <div style="font-size:13px;opacity:0.9;">%s</div>
       <div style="font-size:32px;font-weight:bold;">%.2f</div>
       <div style="font-size:11px;">IC 95%%: [%.2f — %.2f]</div>
       <div style="font-size:12px;margin-top:4px;">%s · %d décès</div>
       </div>',
      color, title, rate, ci_lo, ci_hi, severity, n_deaths
    )
  }

  if (!is.null(cmr_res) && !is.na(cmr_res$rate)) {
    col <- case_when(
      cmr_res$severity == "Normal" ~ "#27ae60",
      cmr_res$severity == "Élevé" ~ "#f39c12",
      cmr_res$severity == "Urgence" ~ "#e74c3c",
      TRUE ~ "#8e44ad"
    )
    cards <- c(cards, make_card("CMR (décès/10 000/jour)", cmr_res$rate,
                                cmr_res$ci_lower, cmr_res$ci_upper,
                                cmr_res$severity, col, cmr_res$n_deaths))
  }

  if (!is.null(u5mr_res) && !is.na(u5mr_res$rate)) {
    col <- case_when(
      u5mr_res$severity == "Normal" ~ "#27ae60",
      u5mr_res$severity == "Élevé" ~ "#f39c12",
      u5mr_res$severity == "Urgence" ~ "#e74c3c",
      TRUE ~ "#8e44ad"
    )
    cards <- c(cards, make_card("U5MR (décès/10 000/jour)", u5mr_res$rate,
                                u5mr_res$ci_lower, u5mr_res$ci_upper,
                                u5mr_res$severity, col, u5mr_res$n_deaths))
  }

  paste(cards, collapse = "\n")
}

# ── Tableau résumé mortalité ─────────────────────────────────────────────────
render_mortality_summary_table <- function(cmr_res, u5mr_res) {
  rows <- list()
  if (!is.null(cmr_res) && !is.na(cmr_res$rate)) {
    rows <- c(rows, list(data.frame(
      Indicateur = "CMR", Taux = cmr_res$rate,
      IC_inf = cmr_res$ci_lower, IC_sup = cmr_res$ci_upper,
      Décès = cmr_res$n_deaths, Population = cmr_res$population,
      `Person-time` = cmr_res$person_time, Classification = cmr_res$severity,
      check.names = FALSE
    )))
  }
  if (!is.null(u5mr_res) && !is.na(u5mr_res$rate)) {
    rows <- c(rows, list(data.frame(
      Indicateur = "U5MR", Taux = u5mr_res$rate,
      IC_inf = u5mr_res$ci_lower, IC_sup = u5mr_res$ci_upper,
      Décès = u5mr_res$n_deaths, Population = u5mr_res$population_u5,
      `Person-time` = u5mr_res$person_time, Classification = u5mr_res$severity,
      check.names = FALSE
    )))
  }
  if (length(rows) > 0) do.call(rbind, rows) else data.frame()
}

cat("module_mortalite.R chargé — CMR · U5MR · Causes · Pyramide\n")
