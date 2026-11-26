# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_survie.R — Analyse de survie complète                              ║
# ║  Kaplan-Meier · Log-rank · Cox · Hypothèses PH · Courbes ajustées          ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(survival)
library(survminer)
library(broom)

# ── Kaplan-Meier + test Log-rank ──────────────────────────────────────────────
run_km <- function(d, time_var, event_var, group_var = NULL, conf_level = 0.95) {
  tryCatch({
    d_km <- d |>
      select(all_of(c(time_var, event_var,
                       if (!is.null(group_var)) group_var))) |>
      drop_na() |>
      mutate(
        .time  = as.numeric(.data[[time_var]]),
        .event = as.numeric(as.character(.data[[event_var]])) %in% c(1, "1", "TRUE", "Yes", "Oui")
      ) |>
      filter(.time > 0)

    surv_obj <- Surv(d_km$.time, d_km$.event)

    # Formule
    if (!is.null(group_var)) {
      fm     <- as.formula(paste("surv_obj ~", group_var))
      km_fit <- survfit(fm, data = d_km, conf.int = conf_level)
      lr     <- survdiff(fm, data = d_km)
      lr_p   <- pchisq(lr$chisq, df = length(lr$obs) - 1, lower.tail = FALSE)
    } else {
      km_fit <- survfit(surv_obj ~ 1, data = d_km, conf.int = conf_level)
      lr     <- NULL; lr_p <- NULL
    }

    # Temps médian de survie
    summary_km <- summary(km_fit)$table

    list(
      fit       = km_fit,
      surv_obj  = surv_obj,
      d_km      = d_km,
      lr_p      = lr_p,
      lr_chi2   = if (!is.null(lr)) round(lr$chisq, 3) else NULL,
      n         = nrow(d_km),
      n_events  = sum(d_km$.event),
      group_var = group_var,
      time_var  = time_var,
      event_var = event_var,
      summary   = summary_km
    )
  }, error = function(e) list(error = e$message))
}

# ── Graphique Kaplan-Meier interactif ─────────────────────────────────────────
plot_km <- function(km_res, palette = NULL) {
  if (!is.null(km_res$error)) {
    return(plotly_empty() |> layout(title = list(text = km_res$error, x = 0.5)))
  }

  km_fit <- km_res$fit
  d_km   <- km_res$d_km

  # Extraire les données de la courbe
  km_df_raw <- broom::tidy(km_fit)
  km_df <- km_df_raw |>
    mutate(strata = if ("strata" %in% names(km_df_raw))
      gsub(".*=", "", strata) else "Global")

  nk <- length(unique(km_df$strata))
  if (is.null(palette))
    palette <- c("#c9161d", "#008DC9", "#4dac26", "#7b2d8b", "#e88400")[1:nk]

  # Titre avec log-rank si disponible
  subtitle <- if (!is.null(km_res$lr_p)) {
    paste0("Test Log-rank : χ²=", km_res$lr_chi2,
           "  p=", round(km_res$lr_p, 4),
           if (km_res$lr_p < 0.05) "  → Survies significativement différentes ✓"
           else "  → Pas de différence significative")
  } else {
    paste0("N=", km_res$n, " | Événements=", km_res$n_events)
  }

  p <- ggplot(km_df, aes(x = time, y = estimate, color = strata, fill = strata,
    text = paste0("Groupe: ", strata,
                  "\nTemps: ", round(time, 1),
                  "\nS(t): ", round(estimate, 3),
                  "\nIC95: [", round(conf.low, 3), "–", round(conf.high, 3), "]"))) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
    geom_step(linewidth = 1.2) +
    scale_color_manual(values = palette, name = km_res$group_var) +
    scale_fill_manual( values = palette, name = km_res$group_var) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_hline(yintercept = 0.5, linetype = "dashed",
               color = "gray60", linewidth = 0.7) +
    annotate("text", x = max(km_df$time, na.rm=TRUE) * 0.8, y = 0.52,
             label = "Survie médiane = 50%", color = "gray50", size = 3) +
    labs(
      x        = paste0("Temps (", km_res$time_var, ")"),
      y        = "Probabilité de survie S(t)",
      title    = "Courbe de Kaplan-Meier",
      subtitle = subtitle,
      caption  = paste0("Ruban = IC ", 95, "% | Tiret horizontal = médiane de survie")
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ggplotly(p, tooltip = "text") |>
    layout(legend = list(orientation = "h", y = -0.25))
}

# ── Modèle de Cox (régression des risques proportionnels) ────────────────────
run_cox <- function(d, time_var, event_var, covariates) {
  tryCatch({
    d_cox <- d |>
      select(all_of(c(time_var, event_var, covariates))) |>
      drop_na() |>
      mutate(
        .time  = as.numeric(.data[[time_var]]),
        .event = as.numeric(as.character(.data[[event_var]])) %in%
                   c(1, "1", "TRUE", "Yes", "Oui")
      ) |>
      filter(.time > 0)

    fm   <- as.formula(paste("Surv(.time,.event) ~",
                              paste(covariates, collapse = "+")))
    cox  <- coxph(fm, data = d_cox)
    tidy_cox <- broom::tidy(cox, exponentiate = TRUE, conf.int = TRUE)

    # Test de l'hypothèse de proportionnalité (Schoenfeld)
    ph_test <- tryCatch(cox.zph(cox), error = function(e) NULL)

    # Test global du modèle
    s <- summary(cox)

    list(
      model    = cox,
      tidy     = tidy_cox,
      ph_test  = ph_test,
      n        = s$n,
      n_events = s$nevent,
      logtest  = round(s$logtest["pvalue"], 4),     # LRT
      waldtest = round(s$waldtest["pvalue"], 4),    # Wald
      sctest   = round(s$sctest["pvalue"], 4),      # Score
      concordance = round(s$concordance["C"], 4),   # C-index (≈ AUC)
      r2_n     = round(s$rsq["rsq"], 4),            # Nagelkerke R²
      d_cox    = d_cox,
      h0       = "H₀ : Aucune variable n'est associée au risque (tous βᵢ = 0)",
      h1       = "H₁ : Au moins une variable est associée au risque d'événement"
    )
  }, error = function(e) list(error = e$message))
}

# ── Forest plot des HR du modèle de Cox ──────────────────────────────────────
plot_cox_forest <- function(cox_res) {
  if (!is.null(cox_res$error)) {
    return(plotly_empty() |> layout(title=list(text=cox_res$error, x=0.5)))
  }

  tidy_df <- cox_res$tidy |>
    filter(!is.na(estimate), is.finite(estimate), estimate < 20) |>
    mutate(
      Sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**",
                      p.value < .05 ~ "*",  p.value < .10 ~ ".",
                      TRUE ~ "NS"),
      Significatif = p.value < 0.05
    )

  p <- ggplot(tidy_df, aes(
    x    = estimate,
    y    = fct_reorder(term, estimate),
    color = Significatif,
    text  = paste0(term,
                   "\nHR = ", round(estimate, 3),
                   "\nIC95 = [", round(conf.low, 3), " – ", round(conf.high, 3), "]",
                   "\np = ", round(p.value, 4), "  ", Sig)
  )) +
    geom_vline(xintercept = 1, linetype = "dashed",
               color = "gray50", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.35, linewidth = 0.9) +
    geom_point(size = 3.5) +
    scale_color_manual(values = c("TRUE" = "#c9161d", "FALSE" = "#6c757d"),
                       labels = c("TRUE" = "Sig. (p<.05)", "FALSE" = "NS"),
                       name = "") +
    scale_x_log10() +
    labs(
      x        = "Hazard Ratio (HR) — Échelle logarithmique",
      y        = "",
      title    = "Forest plot — Modèle de Cox",
      subtitle = paste0("C-index = ", cox_res$concordance,
                        " | N = ", cox_res$n,
                        " | Événements = ", cox_res$n_events),
      caption  = "HR > 1 = augmente le risque | HR < 1 = diminue le risque | Barres = IC 95%"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")

  ggplotly(p, tooltip = "text")
}

# ── Test de l'hypothèse des risques proportionnels ───────────────────────────
plot_ph_test <- function(cox_res) {
  if (!is.null(cox_res$error) || is.null(cox_res$ph_test)) {
    return(plotly_empty())
  }

  ph  <- cox_res$ph_test
  df_ph <- as.data.frame(ph$table) |>
    rownames_to_column("Variable") |>
    rename(Chi2 = chisq, p_val = p) |>
    mutate(
      PH_OK = p_val > 0.05,
      Status = if_else(PH_OK, "✅ PH vérifié (p≥0.05)", "❌ PH violé (p<0.05)")
    )

  p <- ggplot(df_ph, aes(x = fct_reorder(Variable, p_val),
                          y = -log10(p_val),
                          fill = PH_OK,
    text = paste0(Variable, "\nχ²=", round(Chi2, 3),
                  "\np=", round(p_val, 4), "\n", Status))) +
    geom_col(alpha = 0.85, width = 0.65) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed",
               color = "#c9161d", linewidth = 0.8) +
    annotate("text", x = nrow(df_ph) * 0.7, y = -log10(0.05) + 0.1,
             label = "Seuil p=0.05", color = "#c9161d", size = 3) +
    scale_fill_manual(values = c("TRUE" = "#4dac26", "FALSE" = "#c9161d"),
                      labels = c("TRUE" = "PH vérifié", "FALSE" = "PH violé"),
                      name = "") +
    coord_flip() +
    labs(x = "", y = "-log10(p-valeur)",
         title = "Test de Schoenfeld — Hypothèse des risques proportionnels",
         subtitle = paste0("Global : p=", round(ph$table["GLOBAL","p"], 4),
                           if(ph$table["GLOBAL","p"] > 0.05)
                             " → Hypothèse PH vérifiée globalement ✓"
                           else " → Violation possible de l'hypothèse PH ⚠")) +
    theme_minimal(base_size = 11)

  ggplotly(p, tooltip = "text")
}

cat("module_survie.R chargé\n")
