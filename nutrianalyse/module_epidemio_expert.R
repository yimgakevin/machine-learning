# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_epidemio_expert.R — Épidémiologie analytique niveau expert         ║
# ║  Régression logistique (uni/multi), Poisson, Dose-réponse avancée,         ║
# ║  Score de propensité, Hétérogénéité I², Interaction, PAF avancé            ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── 1. RÉGRESSION LOGISTIQUE UNIVARIÉE (screening) ──────────────────────────

run_logistic_univariate <- function(df, outcome, exposures, conf_level = 0.95) {
  results <- lapply(exposures, function(expo) {
    tryCatch({
      df_mod <- df[complete.cases(df[[outcome]], df[[expo]]), ]
      if (nrow(df_mod) < 20) return(NULL)

      formula <- as.formula(paste0("`", outcome, "` ~ `", expo, "`"))
      mod <- glm(formula, data = df_mod, family = binomial(link = "logit"))
      s <- summary(mod)

      coefs <- broom::tidy(mod, conf.int = TRUE, conf.level = conf_level, exponentiate = TRUE)
      coefs_raw <- broom::tidy(mod, conf.int = TRUE, conf.level = conf_level)

      # Exclure l'intercept
      coefs <- coefs[coefs$term != "(Intercept)", ]
      coefs_raw <- coefs_raw[coefs_raw$term != "(Intercept)", ]

      if (nrow(coefs) == 0) return(NULL)

      # Pseudo-R² de McFadden
      null_deviance <- mod$null.deviance
      resid_deviance <- mod$deviance
      mcfadden_r2 <- 1 - resid_deviance / null_deviance

      # AIC
      aic <- AIC(mod)

      # LRT vs modèle nul
      null_mod <- glm(as.formula(paste0("`", outcome, "` ~ 1")),
                      data = df_mod, family = binomial)
      lrt <- anova(null_mod, mod, test = "Chisq")
      lrt_p <- lrt$`Pr(>Chi)`[2]

      tibble::tibble(
        Variable   = expo,
        Terme      = coefs$term,
        OR         = round(coefs$estimate, 3),
        `IC inf`   = round(coefs$conf.low, 3),
        `IC sup`   = round(coefs$conf.high, 3),
        Beta       = round(coefs_raw$estimate, 4),
        SE         = round(coefs_raw$std.error, 4),
        z          = round(coefs_raw$statistic, 3),
        `p-valeur` = signif(coefs_raw$p.value, 4),
        Sig = dplyr::case_when(
          coefs_raw$p.value < 0.001 ~ "***",
          coefs_raw$p.value < 0.01  ~ "**",
          coefs_raw$p.value < 0.05  ~ "*",
          coefs_raw$p.value < 0.10  ~ ".",
          TRUE ~ "NS"
        ),
        AIC         = round(aic, 1),
        `R² McF`    = round(mcfadden_r2, 4),
        `LRT p`     = signif(lrt_p, 4),
        N           = nrow(df_mod)
      )
    }, error = function(e) NULL)
  })

  dplyr::bind_rows(results)
}

# ── 2. RÉGRESSION LOGISTIQUE MULTIVARIÉE ────────────────────────────────────

run_logistic_multivariate <- function(df, outcome, predictors, conf_level = 0.95,
                                       stepwise = FALSE) {
  vars <- c(outcome, predictors)
  df_mod <- df[complete.cases(df[, vars, drop = FALSE]), ]
  if (nrow(df_mod) < 2 * length(predictors) + 10) return(NULL)

  formula_str <- paste0("`", outcome, "` ~ ",
                        paste0("`", predictors, "`", collapse = " + "))
  mod_full <- glm(as.formula(formula_str), data = df_mod, family = binomial)

  # Stepwise AIC si demandé
  mod_final <- if (stepwise) {
    tryCatch(MASS::stepAIC(mod_full, direction = "both", trace = FALSE),
             error = function(e) mod_full)
  } else mod_full

  # Coefficients avec OR
  coefs <- broom::tidy(mod_final, conf.int = TRUE, conf.level = conf_level,
                        exponentiate = TRUE)
  coefs_raw <- broom::tidy(mod_final, conf.int = TRUE, conf.level = conf_level)

  # Diagnostics
  null_dev <- mod_final$null.deviance
  resid_dev <- mod_final$deviance
  mcfadden <- 1 - resid_dev / null_dev

  # Nagelkerke R²
  n <- nrow(df_mod)
  cox_snell <- 1 - exp((resid_dev - null_dev) / n)
  nagelkerke <- cox_snell / (1 - exp(-null_dev / n))

  # Hosmer-Lemeshow
  hl <- tryCatch({
    pred <- fitted(mod_final)
    obs <- df_mod[[outcome]]
    # Découper en 10 groupes
    grps <- cut(pred, breaks = quantile(pred, probs = seq(0, 1, 0.1)),
                include.lowest = TRUE)
    tbl_hl <- data.frame(obs = obs, pred = pred, grp = grps) |>
      dplyr::group_by(grp) |>
      dplyr::summarise(
        n = dplyr::n(),
        obs_pos = sum(obs, na.rm = TRUE),
        exp_pos = sum(pred, na.rm = TRUE),
        .groups = "drop"
      )
    chi2 <- sum((tbl_hl$obs_pos - tbl_hl$exp_pos)^2 /
                  (tbl_hl$exp_pos * (1 - tbl_hl$exp_pos / tbl_hl$n)), na.rm = TRUE)
    p_hl <- 1 - pchisq(chi2, df = nrow(tbl_hl) - 2)
    list(chi2 = chi2, df = nrow(tbl_hl) - 2, p = p_hl)
  }, error = function(e) list(chi2 = NA, df = NA, p = NA))

  # VIF pour multicolinéarité
  vif_vals <- if (length(predictors) > 1) {
    tryCatch(car::vif(mod_final), error = function(e) NULL)
  } else NULL

  # AUC
  auc_val <- tryCatch({
    pred <- fitted(mod_final)
    roc_obj <- pROC::roc(df_mod[[outcome]], pred, quiet = TRUE)
    as.numeric(pROC::auc(roc_obj))
  }, error = function(e) NA)

  list(
    model = mod_final,
    coefficients = coefs,
    coefficients_raw = coefs_raw,
    diagnostics = list(
      N = nrow(df_mod),
      n_predictors = length(coef(mod_final)) - 1,
      AIC = round(AIC(mod_final), 1),
      BIC = round(BIC(mod_final), 1),
      null_deviance = round(null_dev, 1),
      residual_deviance = round(resid_dev, 1),
      McFadden_R2 = round(mcfadden, 4),
      Nagelkerke_R2 = round(nagelkerke, 4),
      AUC = round(auc_val, 4),
      HL_chi2 = round(hl$chi2, 3),
      HL_df = hl$df,
      HL_p = signif(hl$p, 3),
      stepwise = stepwise,
      formula_final = deparse(formula(mod_final))
    ),
    vif = vif_vals
  )
}

# Forest plot des OR
plot_or_forest <- function(logistic_res, title = "Odds Ratios ajustés") {
  if (is.null(logistic_res)) return(plotly::plotly_empty())

  coefs <- logistic_res$coefficients |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(
      sig = p.value < 0.05,
      label = paste0(term, "\nOR=", round(estimate, 2),
                     " [", round(conf.low, 2), ";", round(conf.high, 2), "]",
                     "\np=", signif(p.value, 3))
    )

  p <- ggplot2::ggplot(coefs, ggplot2::aes(
    x = estimate, y = reorder(term, estimate),
    xmin = conf.low, xmax = conf.high,
    color = sig, text = label)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
    ggplot2::geom_errorbarh(height = 0.25, linewidth = 0.9) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_color_manual(values = c("TRUE" = "#c9161d", "FALSE" = "#95a5a6"),
                                labels = c("TRUE" = "Significatif", "FALSE" = "NS"),
                                name = "") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Odds Ratio (échelle log)", y = "", title = title) +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p, tooltip = "text")
}

# ── 3. RÉGRESSION DE POISSON / BINOMIALE NÉGATIVE ───────────────────────────

run_poisson_regression <- function(df, outcome, predictors, offset_var = NULL,
                                    model_type = "poisson", conf_level = 0.95) {
  vars <- c(outcome, predictors)
  if (!is.null(offset_var)) vars <- c(vars, offset_var)
  df_mod <- df[complete.cases(df[, vars, drop = FALSE]), ]
  if (nrow(df_mod) < 20) return(NULL)

  # Formule
  formula_str <- paste0("`", outcome, "` ~ ",
                        paste0("`", predictors, "`", collapse = " + "))
  if (!is.null(offset_var)) {
    formula_str <- paste0(formula_str, " + offset(log(`", offset_var, "`))")
  }

  if (model_type == "poisson") {
    mod <- glm(as.formula(formula_str), data = df_mod, family = poisson)
  } else {
    mod <- MASS::glm.nb(as.formula(formula_str), data = df_mod)
  }

  # Coefficients (IRR = exp(beta))
  coefs <- broom::tidy(mod, conf.int = TRUE, conf.level = conf_level,
                        exponentiate = TRUE)
  coefs_raw <- broom::tidy(mod, conf.int = TRUE, conf.level = conf_level)

  # Test de surdispersion (Cameron & Trivedi)
  overdispersion <- tryCatch({
    resid_dev <- mod$deviance
    resid_df <- mod$df.residual
    disp_ratio <- resid_dev / resid_df
    p_disp <- pchisq(resid_dev, resid_df, lower.tail = FALSE)
    list(ratio = round(disp_ratio, 3), p = signif(p_disp, 3),
         overdispersed = disp_ratio > 1.5)
  }, error = function(e) list(ratio = NA, p = NA, overdispersed = NA))

  list(
    model = mod,
    model_type = model_type,
    coefficients = coefs,
    coefficients_raw = coefs_raw,
    diagnostics = list(
      N = nrow(df_mod),
      AIC = round(AIC(mod), 1),
      deviance = round(mod$deviance, 1),
      df_residual = mod$df.residual,
      overdispersion = overdispersion
    )
  )
}

# ── 4. ANALYSE DOSE-RÉPONSE AVANCÉE ────────────────────────────────────────

run_dose_response <- function(df, dose_var, outcome, n_doses = 5,
                               method = c("logistic", "spline"), conf_level = 0.95) {
  method <- match.arg(method)
  df_mod <- df[complete.cases(df[[dose_var]], df[[outcome]]), ]
  if (nrow(df_mod) < 30) return(NULL)

  # Créer des catégories de dose (quintiles)
  df_mod$dose_cat <- cut(df_mod[[dose_var]],
                          breaks = quantile(df_mod[[dose_var]],
                                            probs = seq(0, 1, length.out = n_doses + 1),
                                            na.rm = TRUE),
                          include.lowest = TRUE, labels = paste0("Q", 1:n_doses))
  df_mod$dose_cat <- relevel(df_mod$dose_cat, ref = "Q1")

  # Modèle par catégories
  formula_cat <- as.formula(paste0("`", outcome, "` ~ dose_cat"))
  mod_cat <- glm(formula_cat, data = df_mod, family = binomial)
  coefs_cat <- broom::tidy(mod_cat, conf.int = TRUE, conf.level = conf_level,
                             exponentiate = TRUE)

  # Modèle linéaire (tendance)
  formula_lin <- as.formula(paste0("`", outcome, "` ~ `", dose_var, "`"))
  mod_lin <- glm(formula_lin, data = df_mod, family = binomial)
  coefs_lin <- broom::tidy(mod_lin, conf.int = TRUE, conf.level = conf_level,
                             exponentiate = TRUE)

  # Modèle spline si demandé
  mod_spline <- NULL
  if (method == "spline") {
    mod_spline <- tryCatch({
      formula_sp <- as.formula(paste0("`", outcome, "` ~ splines::ns(`", dose_var, "`, df=3)"))
      glm(formula_sp, data = df_mod, family = binomial)
    }, error = function(e) NULL)
  }

  # Prévalence par quintile
  prev_by_dose <- df_mod |>
    dplyr::group_by(dose_cat) |>
    dplyr::summarise(
      N = dplyr::n(),
      Cas = sum(.data[[outcome]], na.rm = TRUE),
      Prevalence = round(mean(.data[[outcome]], na.rm = TRUE) * 100, 1),
      Dose_med = round(median(.data[[dose_var]], na.rm = TRUE), 2),
      .groups = "drop"
    )

  # Test de tendance p (LRT linéaire)
  lrt_trend <- anova(
    glm(as.formula(paste0("`", outcome, "` ~ 1")), data = df_mod, family = binomial),
    mod_lin, test = "Chisq"
  )

  list(
    model_cat = mod_cat,
    model_lin = mod_lin,
    model_spline = mod_spline,
    coefs_cat = coefs_cat,
    coefs_lin = coefs_lin,
    prevalence = prev_by_dose,
    trend_p = signif(lrt_trend$`Pr(>Chi)`[2], 4),
    dose_var = dose_var,
    outcome = outcome,
    aic_cat = round(AIC(mod_cat), 1),
    aic_lin = round(AIC(mod_lin), 1),
    aic_spline = if (!is.null(mod_spline)) round(AIC(mod_spline), 1) else NA
  )
}

# Plot dose-réponse
plot_dose_response <- function(dr_res) {
  if (is.null(dr_res)) return(plotly::plotly_empty())

  prev <- dr_res$prevalence

  p <- ggplot2::ggplot(prev, ggplot2::aes(
    x = Dose_med, y = Prevalence,
    text = paste0("Quintile: ", dose_cat, "\nDose médiane: ", Dose_med,
                  "\nPrévalence: ", Prevalence, "%\nN=", N))) +
    ggplot2::geom_col(ggplot2::aes(y = Prevalence), fill = "#3498db", alpha = 0.3, width = diff(range(prev$Dose_med)) / 8) +
    ggplot2::geom_line(color = "#e74c3c", linewidth = 1.2) +
    ggplot2::geom_point(color = "#e74c3c", size = 3) +
    ggplot2::labs(x = dr_res$dose_var, y = paste0("Prévalence ", dr_res$outcome, " (%)"),
                  title = "Courbe dose-réponse",
                  subtitle = paste0("Tendance p=", dr_res$trend_p,
                                    " | AIC lin=", dr_res$aic_lin,
                                    " cat=", dr_res$aic_cat)) +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p, tooltip = "text")
}

# ── 5. DÉTECTION D'INTERACTION / MODIFICATION D'EFFET ───────────────────────

run_interaction_test <- function(df, outcome, expo1, expo2, conf_level = 0.95) {
  df_mod <- df[complete.cases(df[[outcome]], df[[expo1]], df[[expo2]]), ]
  if (nrow(df_mod) < 30) return(NULL)

  # Modèle sans interaction
  formula_main <- as.formula(paste0("`", outcome, "` ~ `", expo1, "` + `", expo2, "`"))
  mod_main <- glm(formula_main, data = df_mod, family = binomial)

  # Modèle avec interaction
  formula_inter <- as.formula(paste0("`", outcome, "` ~ `", expo1, "` * `", expo2, "`"))
  mod_inter <- glm(formula_inter, data = df_mod, family = binomial)

  # LRT pour l'interaction
  lrt <- anova(mod_main, mod_inter, test = "Chisq")
  lrt_p <- lrt$`Pr(>Chi)`[2]

  # Coefficients du modèle avec interaction
  coefs <- broom::tidy(mod_inter, conf.int = TRUE, conf.level = conf_level,
                        exponentiate = TRUE)

  # Mesures d'interaction additives et multiplicatives
  # RERI (Relative Excess Risk due to Interaction)
  coef_raw <- coef(mod_inter)
  inter_term <- grep(":", names(coef_raw), value = TRUE)

  reri <- tryCatch({
    if (length(inter_term) == 1) {
      or_11 <- exp(sum(coef_raw[c(names(coef_raw)[2], names(coef_raw)[3], inter_term)]))
      or_10 <- exp(coef_raw[names(coef_raw)[2]])
      or_01 <- exp(coef_raw[names(coef_raw)[3]])
      reri_val <- or_11 - or_10 - or_01 + 1
      list(RERI = round(reri_val, 3),
           interpretation = dplyr::case_when(
             abs(reri_val) < 0.5 ~ "Pas d'interaction additive notable",
             reri_val > 0 ~ "Interaction additive positive (synergie)",
             TRUE ~ "Interaction additive négative (antagonisme)"
           ))
    } else NULL
  }, error = function(e) NULL)

  list(
    model_main = mod_main,
    model_inter = mod_inter,
    lrt_p = signif(lrt_p, 4),
    interaction_sig = lrt_p < 0.05,
    coefficients = coefs,
    reri = reri,
    aic_main = round(AIC(mod_main), 1),
    aic_inter = round(AIC(mod_inter), 1),
    expo1 = expo1, expo2 = expo2, outcome = outcome,
    conclusion = if (lrt_p < 0.05)
      paste0("Interaction SIGNIFICATIVE entre ", expo1, " et ", expo2,
             " (LRT p=", signif(lrt_p, 3), "). L'effet de ", expo1,
             " dépend du niveau de ", expo2, ".")
    else
      paste0("Pas d'interaction significative entre ", expo1, " et ", expo2,
             " (LRT p=", signif(lrt_p, 3), "). Effets indépendants.")
  )
}

# Tableau croisé des OR stratifiés (visualisation interaction)
plot_interaction_stratified <- function(df, outcome, expo, strat_var) {
  df_mod <- df[complete.cases(df[[outcome]], df[[expo]], df[[strat_var]]), ]
  if (nrow(df_mod) < 30) return(plotly::plotly_empty())

  strata <- unique(df_mod[[strat_var]])

  # [E3] Guard : analyse_2x2 peut ne pas être disponible dans certains contextes
  if (!exists("analyse_2x2", mode = "function")) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
             plotly::layout(title = "analyse_2x2() non disponible dans ce contexte"))
  }

  results <- lapply(strata, function(sv) {
    d_s <- df_mod[df_mod[[strat_var]] == sv, ]
    tryCatch({
      r <- analyse_2x2(d_s[[expo]], d_s[[outcome]])
      if (is.null(r)) return(NULL)
      tibble::tibble(Strate = as.character(sv), OR = r$or,
                     OR_lo = r$or_lo, OR_hi = r$or_hi, N = r$n)
    }, error = function(e) NULL)
  })
  df_plot <- dplyr::bind_rows(results)
  if (nrow(df_plot) == 0) return(plotly::plotly_empty())

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(
    x = OR, y = Strate, xmin = OR_lo, xmax = OR_hi,
    text = paste0(Strate, "\nOR=", round(OR, 2),
                  " [", round(OR_lo, 2), "-", round(OR_hi, 2), "]\nN=", N))) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
    ggplot2::geom_errorbarh(height = 0.25, linewidth = 0.9, color = "#008DC9") +
    ggplot2::geom_point(size = 4, color = "#c9161d") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "OR (échelle log)", y = strat_var,
                  title = paste0("OR de ", expo, " stratifié par ", strat_var)) +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p, tooltip = "text")
}

# ── 6. HÉTÉROGÉNÉITÉ (I² DE HIGGINS) ───────────────────────────────────────

calc_heterogeneity <- function(or_values, or_lo_values, or_hi_values, labels = NULL) {
  n_studies <- length(or_values)
  if (n_studies < 2) return(NULL)
  if (is.null(labels)) labels <- paste0("Strate_", seq_along(or_values))

  # Convertir en log-OR
  log_or <- log(or_values)
  # SE du log-OR à partir des IC
  z <- qnorm(0.975)
  se_log_or <- (log(or_hi_values) - log(or_lo_values)) / (2 * z)

  # Poids inverse de la variance
  w <- 1 / (se_log_or^2)
  w[!is.finite(w)] <- 0

  # Effet combiné (fixed effect)
  combined_log_or <- sum(w * log_or) / sum(w)
  se_combined <- 1 / sqrt(sum(w))
  combined_or <- exp(combined_log_or)
  combined_lo <- exp(combined_log_or - z * se_combined)
  combined_hi <- exp(combined_log_or + z * se_combined)

  # Q de Cochran
  Q <- sum(w * (log_or - combined_log_or)^2)
  df <- n_studies - 1
  Q_p <- 1 - pchisq(Q, df)

  # I² de Higgins
  I2 <- max(0, (Q - df) / Q * 100)

  # Tau² (DerSimonian-Laird)
  C <- sum(w) - sum(w^2) / sum(w)
  tau2 <- max(0, (Q - df) / C)

  # Effet combiné random effects (si I² > 0)
  if (tau2 > 0) {
    w_re <- 1 / (se_log_or^2 + tau2)
    combined_re_log <- sum(w_re * log_or) / sum(w_re)
    se_re <- 1 / sqrt(sum(w_re))
    re_or <- exp(combined_re_log)
    re_lo <- exp(combined_re_log - z * se_re)
    re_hi <- exp(combined_re_log + z * se_re)
  } else {
    re_or <- combined_or; re_lo <- combined_lo; re_hi <- combined_hi
  }

  list(
    n_strates = n_studies,
    # Effets fixes
    fe_or = round(combined_or, 3),
    fe_lo = round(combined_lo, 3),
    fe_hi = round(combined_hi, 3),
    # Effets aléatoires
    re_or = round(re_or, 3),
    re_lo = round(re_lo, 3),
    re_hi = round(re_hi, 3),
    # Hétérogénéité
    Q = round(Q, 3),
    Q_df = df,
    Q_p = signif(Q_p, 4),
    I2 = round(I2, 1),
    tau2 = round(tau2, 4),
    I2_interp = dplyr::case_when(
      I2 < 25 ~ "Hétérogénéité faible",
      I2 < 50 ~ "Hétérogénéité modérée",
      I2 < 75 ~ "Hétérogénéité substantielle",
      TRUE ~ "Hétérogénéité considérable"
    ),
    labels = labels,
    or_values = or_values,
    or_lo = or_lo_values,
    or_hi = or_hi_values
  )
}

# Forest plot avec diamant combiné
plot_meta_forest <- function(het_res) {
  if (is.null(het_res)) return(plotly::plotly_empty())

  df_study <- tibble::tibble(
    label = het_res$labels,
    OR = het_res$or_values,
    lo = het_res$or_lo,
    hi = het_res$or_hi,
    type = "Strate"
  )

  # Ajouter résumés
  df_summary <- tibble::tibble(
    label = c("Effets fixes", "Effets aléatoires"),
    OR = c(het_res$fe_or, het_res$re_or),
    lo = c(het_res$fe_lo, het_res$re_lo),
    hi = c(het_res$fe_hi, het_res$re_hi),
    type = "Résumé"
  )
  df_all <- dplyr::bind_rows(df_study, df_summary)
  df_all$label <- factor(df_all$label, levels = rev(df_all$label))

  p <- ggplot2::ggplot(df_all, ggplot2::aes(
    x = OR, y = label, xmin = lo, xmax = hi,
    shape = type, color = type,
    text = paste0(label, "\nOR=", round(OR, 2), " [", round(lo, 2), "-", round(hi, 2), "]"))) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
    ggplot2::geom_errorbarh(height = 0.2, linewidth = 0.8) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_color_manual(values = c("Strate" = "#008DC9", "Résumé" = "#c9161d"), name = "") +
    ggplot2::scale_shape_manual(values = c("Strate" = 16, "Résumé" = 18), name = "") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Odds Ratio (échelle log)", y = "",
                  title = "Forest plot méta-analytique",
                  subtitle = paste0("I²=", het_res$I2, "% — ", het_res$I2_interp,
                                    " | Q p=", het_res$Q_p)) +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p, tooltip = "text")
}

# ── 7. PAF AVANCÉ (MULTI-EXPOSITION) ───────────────────────────────────────

calc_paf_adjusted <- function(df, outcome, exposure, confounders = NULL,
                               conf_level = 0.95, seed = 7193) {
  # PAF ajusté via Bruzzi (1985) / régression logistique
  vars <- c(outcome, exposure, confounders)
  df_mod <- df[complete.cases(df[, vars, drop = FALSE]), ]
  if (nrow(df_mod) < 30) return(NULL)

  # Modèle ajusté
  formula_adj <- if (length(confounders) > 0) {
    paste0("`", outcome, "` ~ `", exposure, "` + ",
           paste0("`", confounders, "`", collapse = " + "))
  } else {
    paste0("`", outcome, "` ~ `", exposure, "`")
  }
  mod <- glm(as.formula(formula_adj), data = df_mod, family = binomial)

  # OR ajusté
  coefs <- broom::tidy(mod, conf.int = TRUE, conf.level = conf_level,
                        exponentiate = TRUE)
  expo_coef <- coefs[coefs$term == exposure | grepl(exposure, coefs$term), ]
  if (nrow(expo_coef) == 0) return(NULL)

  or_adj <- expo_coef$estimate[1]
  or_lo <- expo_coef$conf.low[1]
  or_hi <- expo_coef$conf.high[1]

  # Prévalence de l'exposition parmi les cas
  cases <- df_mod[df_mod[[outcome]] == 1, ]
  p_expo_cases <- mean(as.numeric(cases[[exposure]]) > 0, na.rm = TRUE)

  # PAF ajusté (méthode de Bruzzi)
  paf_adj <- p_expo_cases * (1 - 1 / or_adj)

  # [E5] IC bootstrap pour PAF — seed paramétrable
  set.seed(seed)
  B <- 1000
  boot_paf <- replicate(B, {
    idx <- sample(nrow(df_mod), nrow(df_mod), replace = TRUE)
    d_b <- df_mod[idx, ]
    tryCatch({
      mod_b <- glm(as.formula(formula_adj), data = d_b, family = binomial)
      or_b <- exp(coef(mod_b)[2])
      cases_b <- d_b[d_b[[outcome]] == 1, ]
      p_b <- mean(as.numeric(cases_b[[exposure]]) > 0, na.rm = TRUE)
      p_b * (1 - 1 / or_b)
    }, error = function(e) NA)
  })
  boot_paf <- boot_paf[is.finite(boot_paf)]
  alpha <- (1 - conf_level) / 2

  list(
    paf_brut = round(paf_adj, 4),
    paf_pct = round(paf_adj * 100, 1),
    paf_lo = round(quantile(boot_paf, alpha, na.rm = TRUE), 4),
    paf_hi = round(quantile(boot_paf, 1 - alpha, na.rm = TRUE), 4),
    or_adj = round(or_adj, 3),
    or_lo = round(or_lo, 3),
    or_hi = round(or_hi, 3),
    p_expo_cases = round(p_expo_cases, 3),
    n_cases = nrow(cases),
    n_total = nrow(df_mod),
    confounders = confounders,
    interpretation = paste0(
      "Si l'exposition '", exposure, "' était éliminée, environ ",
      round(paf_adj * 100, 1), "% [IC ", round(conf_level * 100), "%: ",
      round(quantile(boot_paf, alpha, na.rm = TRUE) * 100, 1), "–",
      round(quantile(boot_paf, 1 - alpha, na.rm = TRUE) * 100, 1),
      "%] des cas de '", outcome, "' pourraient être évités",
      if (length(confounders) > 0)
        paste0(" (ajusté pour ", paste(confounders, collapse = ", "), ")")
      else " (non ajusté)", "."
    )
  )
}

# ── 8. COURBE ROC POUR MODÈLE LOGISTIQUE ───────────────────────────────────

plot_logistic_roc <- function(logistic_res, df, outcome) {
  if (is.null(logistic_res)) return(plotly::plotly_empty())

  pred <- fitted(logistic_res$model)
  df_mod <- df[complete.cases(df[[outcome]]), ]
  obs <- df_mod[[outcome]][seq_along(pred)]

  roc_obj <- tryCatch(pROC::roc(obs, pred, quiet = TRUE), error = function(e) NULL)
  if (is.null(roc_obj)) return(plotly::plotly_empty())

  auc_val <- round(pROC::auc(roc_obj), 3)
  ci_auc <- tryCatch(pROC::ci.auc(roc_obj), error = function(e) NULL)

  df_roc <- tibble::tibble(
    Sensibilite = roc_obj$sensitivities,
    Specificite_1 = 1 - roc_obj$specificities
  )

  p <- ggplot2::ggplot(df_roc, ggplot2::aes(x = Specificite_1, y = Sensibilite)) +
    ggplot2::geom_line(color = "#c9161d", linewidth = 1.2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888") +
    ggplot2::annotate("text", x = 0.6, y = 0.3,
                      label = paste0("AUC = ", auc_val,
                                     if (!is.null(ci_auc))
                                       paste0("\n[", round(ci_auc[1], 3), "–", round(ci_auc[3], 3), "]")
                                     else ""),
                      size = 4, color = "#c9161d", fontface = "bold") +
    ggplot2::labs(x = "1 - Spécificité", y = "Sensibilité",
                  title = "Courbe ROC du modèle logistique") +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p)
}

cat("module_epidemio_expert.R chargé — 8 techniques épidémiologiques expert\n")
