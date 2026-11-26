# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_infer_expert.R — Statistiques inférentielles niveau expert         ║
# ║  Tests appariés, Puissance, Bayes Factor, Équivalence TOST, Régression    ║
# ║  diagnostics, Mesures répétées, IC bootstrap pour tailles d'effet          ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── 0. FALLBACK HELPERS (si non définis dans inferentiel_v4.R ou utils.R) ──

if (!exists("interp_cohen_d", mode = "function")) {
  interp_cohen_d <- function(d) {
    d <- abs(d)
    if (is.na(d)) "Indéterminé"
    else if (d < 0.2) "Négligeable"
    else if (d < 0.5) "Petit"
    else if (d < 0.8) "Moyen"
    else "Grand"
  }
}

if (!exists("interp_r_rank", mode = "function")) {
  interp_r_rank <- function(r) {
    r <- abs(r)
    if (is.na(r)) "Indéterminé"
    else if (r < 0.1) "Négligeable"
    else if (r < 0.3) "Petit"
    else if (r < 0.5) "Moyen"
    else "Grand"
  }
}

if (!exists("var_type", mode = "function")) {
  var_type <- function(x) {
    if (is.character(x) || is.factor(x)) {
      nd <- length(unique(na.omit(x)))
      if (nd == 2) "binaire" else if (nd <= 10) "cat_peu" else "cat_many"
    } else if (is.numeric(x)) {
      nd <- length(unique(na.omit(x)))
      if (nd == 2 && all(na.omit(x) %in% c(0, 1))) "binaire"
      else if (nd <= 10) "discrete"
      else "continue"
    } else "autre"
  }
}

# ── 1. TESTS APPARIÉS ──────────────────────────────────────────────────────

run_paired_tests <- function(x1, x2, var1_name = "X1", var2_name = "X2", alpha = 0.05) {
  x1 <- as.numeric(x1); x2 <- as.numeric(x2)
  complete <- complete.cases(x1, x2)
  x1 <- x1[complete]; x2 <- x2[complete]
  n <- length(x1)
  if (n < 5) return(NULL)

  diff_vals <- x1 - x2
  tests <- list()

  # Normalité de la différence
  norm_diff <- tryCatch(shapiro.test(diff_vals[1:min(5000, n)]),
                        error = function(e) NULL)
  is_normal <- if (!is.null(norm_diff)) norm_diff$p.value >= 0.05 else FALSE

  # 1a. t-test apparié
  if (is_normal) {
    res <- tryCatch(t.test(x1, x2, paired = TRUE), error = function(e) NULL)
    if (!is.null(res)) {
      # Cohen d pour mesures appariées (dz)
      dz <- mean(diff_vals) / sd(diff_vals)
      tests[["t_apparie"]] <- list(
        nom = "t-test apparié",
        h0 = paste0("H0 : mu_diff = 0 (pas de différence entre ", var1_name, " et ", var2_name, ")"),
        h1 = "H1 : mu_diff ≠ 0",
        conditions = list(
          list(nom = "Appariement", ok = TRUE, detail = paste0("n paires = ", n, " ✓")),
          list(nom = "Normalité des différences", ok = TRUE,
               detail = paste0("Shapiro-Wilk p=", round(norm_diff$p.value, 4), " ✓"))
        ),
        valide = TRUE,
        stat = round(res$statistic, 3), stat_label = "t",
        df = round(res$parameter, 1), p_val = round(res$p.value, 4),
        ci = paste0("[", round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]"),
        effet_nom = "Cohen dz (apparié)", effet_val = round(abs(dz), 4),
        effet_interp = interp_cohen_d(abs(dz)),
        diff_moyenne = round(mean(diff_vals), 4),
        conclusion = if (res$p.value < alpha)
          paste0("Rejet de H0 (p=", round(res$p.value, 4), ") — Différence significative (dz=", round(abs(dz), 3), ")")
        else
          paste0("Non rejet de H0 (p=", round(res$p.value, 4), ") — Pas de différence significative"),
        sig = res$p.value < alpha
      )
    }
  } else {
    tests[["t_apparie"]] <- list(
      nom = "t-test apparié",
      conditions = list(
        list(nom = "Appariement", ok = TRUE, detail = paste0("n paires = ", n, " ✓")),
        list(nom = "Normalité des différences", ok = FALSE,
             detail = if (!is.null(norm_diff))
               paste0("Shapiro-Wilk p=", round(norm_diff$p.value, 4), " ✗")
             else "Test non disponible ✗")
      ),
      valide = FALSE,
      raison = "Différences non normales — utiliser Wilcoxon signé"
    )
  }

  # 1b. Wilcoxon signed-rank (toujours applicable)
  res <- tryCatch(wilcox.test(x1, x2, paired = TRUE, exact = FALSE, conf.int = TRUE),
                  error = function(e) NULL)
  if (!is.null(res)) {
    # Rank-biserial correlation pour mesures appariées
    r_rb <- tryCatch({
      ranks <- rank(abs(diff_vals))
      T_plus <- sum(ranks[diff_vals > 0])
      T_minus <- sum(ranks[diff_vals < 0])
      (T_plus - T_minus) / (T_plus + T_minus)
    }, error = function(e) NA)

    tests[["wilcoxon_signe"]] <- list(
      nom = "Test de Wilcoxon signé (apparié)",
      h0 = paste0("H0 : Médiane des différences = 0"),
      h1 = "H1 : Médiane des différences ≠ 0",
      conditions = list(
        list(nom = "Appariement", ok = TRUE, detail = paste0("n paires = ", n, " ✓")),
        list(nom = "Non paramétrique", ok = TRUE, detail = "Aucune hypothèse de normalité ✓")
      ),
      valide = TRUE,
      stat = round(res$statistic, 3), stat_label = "V",
      df = "—", p_val = round(res$p.value, 4),
      ci = if (!is.null(res$conf.int))
        paste0("[", round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
      else NULL,
      effet_nom = "r rank-biserial", effet_val = round(abs(r_rb), 4),
      effet_interp = interp_r_rank(abs(r_rb)),
      conclusion = if (res$p.value < alpha)
        paste0("Rejet de H0 (p=", round(res$p.value, 4), ") — Différence significative")
      else
        paste0("Non rejet de H0 (p=", round(res$p.value, 4), ") — Pas de différence significative"),
      sig = res$p.value < alpha
    )
  }

  # 1c. Sign test (test des signes — très robuste)
  n_pos <- sum(diff_vals > 0)
  n_neg <- sum(diff_vals < 0)
  n_nz <- n_pos + n_neg
  sign_p <- if (n_nz > 0) 2 * min(pbinom(n_pos, n_nz, 0.5), 1 - pbinom(n_pos - 1, n_nz, 0.5)) else NA

  if (!is.na(sign_p)) {
    tests[["sign"]] <- list(
      nom = "Test des signes",
      h0 = "H0 : Probabilité (X1 > X2) = 0.5",
      h1 = "H1 : Probabilité (X1 > X2) ≠ 0.5",
      conditions = list(
        list(nom = "Appariement", ok = TRUE, detail = paste0("n paires = ", n, " ✓")),
        list(nom = "Minimal", ok = TRUE, detail = "Aucune hypothèse distributionnelle ✓")
      ),
      valide = TRUE,
      stat = paste0(n_pos, "+/", n_neg, "-"), stat_label = "Signes",
      df = "—", p_val = round(sign_p, 4),
      ci = NULL,
      effet_nom = "Proportion +", effet_val = round(n_pos / n_nz, 4),
      effet_interp = paste0(round(n_pos / n_nz * 100, 1), "% en faveur de ", var1_name),
      conclusion = if (sign_p < alpha)
        paste0("Rejet de H0 (p=", round(sign_p, 4), ") — Tendance directionnelle significative")
      else
        paste0("Non rejet de H0 (p=", round(sign_p, 4), ") — Pas de tendance significative"),
      sig = sign_p < alpha
    )
  }

  tests
}

# ── 2. ANALYSE DE PUISSANCE POST-HOC ───────────────────────────────────────

calc_power_analysis <- function(test_type, n, effect_size, alpha = 0.05, k = NULL) {
  result <- tryCatch({
    if (test_type == "t2") {
      # t-test deux groupes indépendants
      pwr::pwr.t.test(n = n, d = effect_size, sig.level = alpha, type = "two.sample")
    } else if (test_type == "t_paired") {
      pwr::pwr.t.test(n = n, d = effect_size, sig.level = alpha, type = "paired")
    } else if (test_type == "anova") {
      # f = eta2_to_f
      f <- sqrt(effect_size / (1 - effect_size))
      pwr::pwr.anova.test(k = k, n = n, f = f, sig.level = alpha)
    } else if (test_type == "chi2") {
      pwr::pwr.chisq.test(w = effect_size, N = n, df = k, sig.level = alpha)
    } else if (test_type == "cor") {
      pwr::pwr.r.test(n = n, r = effect_size, sig.level = alpha)
    } else {
      return(NULL)
    }
  }, error = function(e) NULL)

  if (is.null(result)) return(NULL)

  power <- round(result$power, 4)
  tibble::tibble(
    `Type de test` = test_type,
    N = n,
    `Taille d'effet` = round(effect_size, 4),
    Alpha = alpha,
    `Puissance (1-β)` = power,
    `Puissance (%)` = paste0(round(power * 100, 1), "%"),
    Interpretation = dplyr::case_when(
      power >= 0.90 ~ "Excellente (≥90%) — Très faible risque de Type II",
      power >= 0.80 ~ "Adéquate (80-90%) — Risque de Type II acceptable",
      power >= 0.60 ~ "Insuffisante (60-80%) — Risque élevé de faux négatifs",
      TRUE ~ "Très insuffisante (<60%) — Résultat négatif non interprétable"
    ),
    `N requis (80%)` = tryCatch({
      if (test_type == "t2") ceiling(pwr::pwr.t.test(d = effect_size, sig.level = alpha, power = 0.80, type = "two.sample")$n)
      else if (test_type == "t_paired") ceiling(pwr::pwr.t.test(d = effect_size, sig.level = alpha, power = 0.80, type = "paired")$n)
      else if (test_type == "cor") ceiling(pwr::pwr.r.test(r = effect_size, sig.level = alpha, power = 0.80)$n)
      else if (test_type == "anova") ceiling(pwr::pwr.anova.test(k = k, f = sqrt(effect_size / (1 - effect_size)), sig.level = alpha, power = 0.80)$n)
      else NA
    }, error = function(e) NA)
  )
}

# Courbe de puissance
plot_power_curve <- function(test_type, effect_size, alpha = 0.05, k = NULL,
                             n_range = seq(10, 500, by = 10)) {
  powers <- sapply(n_range, function(n) {
    res <- calc_power_analysis(test_type, n, effect_size, alpha, k)
    if (is.null(res)) NA else res$`Puissance (1-β)`
  })

  df <- tibble::tibble(N = n_range, Puissance = powers) |> tidyr::drop_na()
  if (nrow(df) == 0) return(plotly::plotly_empty())

  # Trouver N pour 80% de puissance
  n80 <- df$N[which.min(abs(df$Puissance - 0.80))]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = N, y = Puissance,
    text = paste0("N=", N, "\nPuissance=", round(Puissance * 100, 1), "%"))) +
    ggplot2::geom_line(color = "#2980b9", linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", color = "#e74c3c", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = n80, linetype = "dotted", color = "#27ae60", linewidth = 0.6) +
    ggplot2::annotate("text", x = n80 + 10, y = 0.82,
                      label = paste0("N≈", n80, " pour 80%"),
                      hjust = 0, size = 3.5, color = "#27ae60") +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(x = "Taille d'échantillon (par groupe)", y = "Puissance (1-β)",
                  title = paste0("Courbe de puissance — effet=", round(effect_size, 3),
                                 " α=", alpha)) +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p, tooltip = "text")
}

# ── 3. BAYES FACTOR ────────────────────────────────────────────────────────

calc_bayes_factor <- function(d, var_x, var_y, test_type = "auto") {
  results <- list()

  if (test_type %in% c("auto", "t")) {
    # BF pour t-test (deux groupes)
    bf <- tryCatch({
      grps <- split(d[[var_y]], d[[var_x]])
      grps <- grps[sapply(grps, function(g) sum(!is.na(g)) >= 3)]
      if (length(grps) == 2) {
        bf_obj <- BayesFactor::ttestBF(x = na.omit(grps[[1]]), y = na.omit(grps[[2]]))
        bf_val <- BayesFactor::extractBF(bf_obj)$bf
        list(
          test = "t-test bayésien",
          bf10 = bf_val,
          bf01 = 1 / bf_val,
          interpretation = interpret_bf(bf_val)
        )
      } else NULL
    }, error = function(e) NULL)
    if (!is.null(bf)) results[["t_test"]] <- bf
  }

  if (test_type %in% c("auto", "anova")) {
    # BF pour ANOVA
    bf <- tryCatch({
      n_grp <- length(unique(na.omit(d[[var_x]])))
      if (n_grp >= 3 && is.numeric(d[[var_y]])) {
        df_bf <- data.frame(y = d[[var_y]], x = as.factor(d[[var_x]])) |> na.omit()
        bf_obj <- BayesFactor::anovaBF(y ~ x, data = df_bf)
        bf_val <- BayesFactor::extractBF(bf_obj)$bf
        list(
          test = "ANOVA bayésienne",
          bf10 = bf_val,
          bf01 = 1 / bf_val,
          interpretation = interpret_bf(bf_val)
        )
      } else NULL
    }, error = function(e) NULL)
    if (!is.null(bf)) results[["anova"]] <- bf
  }

  if (test_type %in% c("auto", "cor")) {
    # BF pour corrélation
    bf <- tryCatch({
      if (is.numeric(d[[var_x]]) && is.numeric(d[[var_y]])) {
        complete <- complete.cases(d[[var_x]], d[[var_y]])
        bf_obj <- BayesFactor::correlationBF(d[[var_x]][complete], d[[var_y]][complete])
        bf_val <- BayesFactor::extractBF(bf_obj)$bf
        list(
          test = "Corrélation bayésienne",
          bf10 = bf_val,
          bf01 = 1 / bf_val,
          interpretation = interpret_bf(bf_val)
        )
      } else NULL
    }, error = function(e) NULL)
    if (!is.null(bf)) results[["cor"]] <- bf
  }

  results
}

interpret_bf <- function(bf10) {
  if (is.na(bf10) || !is.finite(bf10)) return("Non calculable")
  abf <- abs(bf10)
  if (abf > 100) "Preuve décisive pour H1"
  else if (abf > 30) "Preuve très forte pour H1"
  else if (abf > 10) "Preuve forte pour H1"
  else if (abf > 3) "Preuve modérée pour H1"
  else if (abf > 1) "Preuve anecdotique pour H1"
  else if (abf > 1/3) "Preuve anecdotique pour H0"
  else if (abf > 1/10) "Preuve modérée pour H0"
  else if (abf > 1/30) "Preuve forte pour H0"
  else "Preuve très forte pour H0"
}

format_bf_results <- function(bf_list) {
  if (length(bf_list) == 0) return(NULL)
  rows <- lapply(bf_list, function(bf) {
    tibble::tibble(
      Test = bf$test,
      `BF10` = signif(bf$bf10, 4),
      `BF01` = signif(bf$bf01, 4),
      `log10(BF10)` = round(log10(bf$bf10), 3),
      Interpretation = bf$interpretation,
      Direction = if (bf$bf10 > 1) "En faveur de H1" else "En faveur de H0"
    )
  })
  dplyr::bind_rows(rows)
}

# ── 4. TEST D'ÉQUIVALENCE (TOST) ───────────────────────────────────────────

run_tost_test <- function(x1, x2, var1_name = "X1", var2_name = "X2",
                          delta = NULL, alpha = 0.05) {
  x1 <- na.omit(as.numeric(x1))
  x2 <- na.omit(as.numeric(x2))
  n1 <- length(x1); n2 <- length(x2)
  if (n1 < 5 || n2 < 5) return(NULL)

  # Si delta non spécifié, utiliser 0.5 × SD poolé (marge cliniquement pertinente)
  if (is.null(delta)) {
    sp <- sqrt(((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2))
    delta <- 0.5 * sp
  }

  diff_means <- mean(x1) - mean(x2)
  se_diff <- sqrt(var(x1) / n1 + var(x2) / n2)
  df_approx <- (var(x1) / n1 + var(x2) / n2)^2 /
    ((var(x1) / n1)^2 / (n1 - 1) + (var(x2) / n2)^2 / (n2 - 1))

  # TOST : deux tests unilatéraux
  # Test 1 : H0: diff ≤ -delta vs H1: diff > -delta
  t1 <- (diff_means - (-delta)) / se_diff
  p1 <- pt(t1, df_approx, lower.tail = FALSE)

  # Test 2 : H0: diff ≥ +delta vs H1: diff < +delta
  t2 <- (diff_means - delta) / se_diff
  p2 <- pt(t2, df_approx, lower.tail = TRUE)

  # p-value TOST = max(p1, p2)
  p_tost <- max(p1, p2)

  # IC 90% (pour test d'équivalence bilatéral à alpha=0.05)
  ci_90 <- diff_means + c(-1, 1) * qt(1 - alpha, df_approx) * se_diff

  list(
    nom = "Test TOST (Two One-Sided Tests)",
    description = paste0("Test d'équivalence entre ", var1_name, " et ", var2_name),
    delta = round(delta, 4),
    diff_moyennes = round(diff_means, 4),
    se = round(se_diff, 4),
    t1 = round(t1, 3), p1 = round(p1, 4),
    t2 = round(t2, 3), p2 = round(p2, 4),
    p_tost = round(p_tost, 4),
    ci_90 = paste0("[", round(ci_90[1], 3), " ; ", round(ci_90[2], 3), "]"),
    equivalent = p_tost < alpha,
    conclusion = if (p_tost < alpha)
      paste0("Les deux groupes sont ÉQUIVALENTS (p_TOST=", round(p_tost, 4),
             " < α=", alpha, ") — IC 90% inclus dans [-", round(delta, 2), " ; +", round(delta, 2), "]")
    else
      paste0("Équivalence NON démontrée (p_TOST=", round(p_tost, 4),
             " ≥ α=", alpha, ") — IC 90% dépasse les bornes d'équivalence")
  )
}

# Visualisation TOST
plot_tost <- function(tost_res) {
  if (is.null(tost_res)) return(plotly::plotly_empty())

  ci <- as.numeric(stringr::str_extract_all(tost_res$ci_90, "-?[0-9.]+")[[1]])
  delta <- tost_res$delta

  df <- tibble::tibble(
    y = 1,
    diff = tost_res$diff_moyennes,
    ci_low = ci[1],
    ci_high = ci[2]
  )

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -delta, xmax = delta, ymin = 0.5, ymax = 1.5),
                       fill = "#27ae60", alpha = 0.15) +
    ggplot2::geom_vline(xintercept = c(-delta, delta), linetype = "dashed",
                        color = "#e74c3c", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
    ggplot2::geom_errorbarh(ggplot2::aes(y = y, xmin = ci_low, xmax = ci_high),
                            height = 0.2, linewidth = 1.2, color = "#2980b9") +
    ggplot2::geom_point(ggplot2::aes(x = diff, y = y), size = 4, color = "#2980b9") +
    ggplot2::annotate("text", x = -delta, y = 1.6, label = paste0("-Δ=", round(-delta, 2)),
                      size = 3.5, color = "#e74c3c") +
    ggplot2::annotate("text", x = delta, y = 1.6, label = paste0("+Δ=", round(delta, 2)),
                      size = 3.5, color = "#e74c3c") +
    ggplot2::scale_y_continuous(limits = c(0, 2)) +
    ggplot2::labs(x = "Différence des moyennes", y = "",
                  title = "Test d'équivalence TOST",
                  subtitle = paste0("Zone verte = bornes d'équivalence | ",
                                    if (tost_res$equivalent) "ÉQUIVALENT ✓" else "Non équivalent ✗")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())

  plotly::ggplotly(p)
}

# ── 5. RÉGRESSION MULTIPLE + DIAGNOSTICS ────────────────────────────────────

run_regression_diagnostics <- function(df, outcome, predictors) {
  vars <- c(outcome, predictors)
  df_mod <- df |> dplyr::select(dplyr::all_of(vars)) |> tidyr::drop_na()
  if (nrow(df_mod) < length(vars) + 5) return(NULL)

  formula_str <- paste0("`", outcome, "` ~ ", paste0("`", predictors, "`", collapse = " + "))
  mod <- lm(as.formula(formula_str), data = df_mod)
  s <- summary(mod)

  # Coefficients avec IC
  coefs <- broom::tidy(mod, conf.int = TRUE)
  coefs$Significatif <- coefs$p.value < 0.05

  # VIF (Variance Inflation Factor)
  vif_vals <- if (length(predictors) > 1) {
    tryCatch({
      v <- car::vif(mod)
      if (is.matrix(v)) v[, "GVIF"] else v
    }, error = function(e) NULL)
  } else NULL

  # Diagnostics globaux
  diag <- broom::glance(mod)

  # Cook's distance
  cooks <- cooks.distance(mod)
  n_influential <- sum(cooks > 4 / nrow(df_mod))

  # Durbin-Watson (autocorrélation résidus)
  dw <- tryCatch(car::durbinWatsonTest(mod), error = function(e) NULL)

  # Breusch-Pagan (hétéroscédasticité)
  bp <- tryCatch(lmtest::bptest(mod), error = function(e) NULL)

  # Shapiro-Wilk sur résidus
  resid_sw <- tryCatch(shapiro.test(residuals(mod)[1:min(5000, length(residuals(mod)))]),
                        error = function(e) NULL)

  list(
    model = mod,
    coefficients = coefs,
    vif = vif_vals,
    diagnostics = list(
      R2 = round(diag$r.squared, 4),
      R2_adj = round(diag$adj.r.squared, 4),
      F_stat = round(diag$statistic, 3),
      F_pval = signif(diag$p.value, 4),
      AIC = round(AIC(mod), 2),
      BIC = round(BIC(mod), 2),
      RMSE = round(sqrt(mean(residuals(mod)^2)), 4),
      MAE = round(mean(abs(residuals(mod))), 4),
      n_obs = nrow(df_mod),
      n_influential_cook = n_influential,
      durbin_watson = if (!is.null(dw)) round(dw$dw, 4) else NA,
      dw_pval = if (!is.null(dw)) round(dw$p, 4) else NA,
      bp_stat = if (!is.null(bp)) round(bp$statistic, 3) else NA,
      bp_pval = if (!is.null(bp)) signif(bp$p.value, 3) else NA,
      resid_normal_p = if (!is.null(resid_sw)) round(resid_sw$p.value, 4) else NA
    ),
    residuals = residuals(mod),
    fitted = fitted(mod),
    cooks = cooks
  )
}

# Forest plot des coefficients
plot_regression_forest <- function(reg_res) {
  if (is.null(reg_res)) return(plotly::plotly_empty())

  coefs <- reg_res$coefficients |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(
      color = dplyr::if_else(Significatif, "#e74c3c", "#95a5a6"),
      label = paste0(term, "\nβ=", round(estimate, 3),
                     " [", round(conf.low, 3), ";", round(conf.high, 3), "]",
                     "\np=", signif(p.value, 3))
    )

  p <- ggplot2::ggplot(coefs,
    ggplot2::aes(x = estimate, y = reorder(term, abs(estimate)),
                 xmin = conf.low, xmax = conf.high,
                 color = Significatif, text = label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_errorbarh(height = 0.2, linewidth = 0.8) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#95a5a6"),
                                labels = c("TRUE" = "Significatif", "FALSE" = "Non significatif")) +
    ggplot2::labs(x = "Coefficient (β)", y = "",
                  title = "Coefficients de régression avec IC 95%") +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p, tooltip = "text")
}

# Diagnostic plots (4-en-1)
plot_regression_diagnostics <- function(reg_res) {
  if (is.null(reg_res)) return(plotly::plotly_empty())

  mod <- reg_res$model
  df_diag <- tibble::tibble(
    fitted = reg_res$fitted,
    residuals = reg_res$residuals,
    std_resid = rstandard(mod),
    cooks = reg_res$cooks,
    idx = seq_along(reg_res$residuals)
  )

  # Résidus vs Fitted
  p1 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db", size = 1.5) +
    ggplot2::geom_hline(yintercept = 0, color = "#e74c3c", linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#e74c3c", linewidth = 0.7) +
    ggplot2::labs(x = "Valeurs ajustées", y = "Résidus", title = "Résidus vs Ajustées") +
    ggplot2::theme_minimal(base_size = 9)

  # QQ-plot résidus
  p2 <- ggplot2::ggplot(df_diag, ggplot2::aes(sample = std_resid)) +
    ggplot2::stat_qq(color = "#3498db", alpha = 0.5, size = 1.5) +
    ggplot2::stat_qq_line(color = "#e74c3c", linewidth = 0.7) +
    ggplot2::labs(x = "Quantiles théoriques", y = "Résidus standardisés",
                  title = "QQ-plot des résidus") +
    ggplot2::theme_minimal(base_size = 9)

  # Scale-Location
  p3 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = fitted, y = sqrt(abs(std_resid)))) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db", size = 1.5) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#e74c3c", linewidth = 0.7) +
    ggplot2::labs(x = "Valeurs ajustées", y = "√|Résidus std|",
                  title = "Scale-Location") +
    ggplot2::theme_minimal(base_size = 9)

  # Cook's distance
  cook_threshold <- 4 / length(reg_res$cooks)
  p4 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = idx, y = cooks)) +
    ggplot2::geom_col(fill = dplyr::if_else(df_diag$cooks > cook_threshold, "#e74c3c", "#3498db"),
                      width = 0.8) +
    ggplot2::geom_hline(yintercept = cook_threshold, linetype = "dashed", color = "#e74c3c") +
    ggplot2::labs(x = "Observation", y = "Distance de Cook",
                  title = "Influence (Cook)") +
    ggplot2::theme_minimal(base_size = 9)

  # Combiner (patchwork ou gridExtra)
  combined <- tryCatch({
    if (requireNamespace("patchwork", quietly = TRUE)) {
      (p1 + p2) / (p3 + p4)
    } else if (requireNamespace("gridExtra", quietly = TRUE)) {
      gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
    } else {
      p1
    }
  }, error = function(e) p1)

  combined
}

# ── 6. MESURES RÉPÉTÉES ────────────────────────────────────────────────────

run_friedman_test <- function(df, measures, id_var = NULL, alpha = 0.05) {
  # measures = vecteur de noms de colonnes (mesures répétées)
  if (length(measures) < 3) return(NULL)

  df_sel <- df |> dplyr::select(dplyr::all_of(measures)) |> tidyr::drop_na()
  if (nrow(df_sel) < 5) return(NULL)

  mat <- as.matrix(df_sel)

  res <- tryCatch(friedman.test(mat), error = function(e) NULL)
  if (is.null(res)) return(NULL)

  n <- nrow(mat); k <- ncol(mat)
  # Kendall's W (coefficient de concordance)
  W <- res$statistic / (n * (k - 1))

  # Post-hoc pairwise Wilcoxon avec correction
  posthoc <- NULL
  if (res$p.value < alpha) {
    posthoc <- tryCatch({
      pvals <- matrix(NA, k, k, dimnames = list(measures, measures))
      for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
          wt <- wilcox.test(mat[, i], mat[, j], paired = TRUE, exact = FALSE)
          pvals[i, j] <- pvals[j, i] <- wt$p.value
        }
      }
      # Correction BH
      upper_p <- pvals[upper.tri(pvals)]
      adj_p <- p.adjust(upper_p, method = "BH")
      idx <- which(upper.tri(pvals), arr.ind = TRUE)
      rows <- list()
      for (l in seq_along(adj_p)) {
        rows[[l]] <- tibble::tibble(
          `Paire` = paste0(measures[idx[l, 1]], " vs ", measures[idx[l, 2]]),
          `p brute` = round(upper_p[l], 4),
          `p ajustée (BH)` = round(adj_p[l], 4),
          Significatif = if (adj_p[l] < alpha) "Oui" else "Non"
        )
      }
      dplyr::bind_rows(rows)
    }, error = function(e) NULL)
  }

  list(
    nom = "Test de Friedman (mesures répétées non paramétriques)",
    h0 = "H0 : Distributions identiques pour toutes les mesures",
    h1 = "H1 : Au moins une mesure diffère",
    stat = round(res$statistic, 3),
    df = res$parameter,
    p_val = round(res$p.value, 4),
    W_kendall = round(W, 4),
    W_interp = dplyr::case_when(
      W >= 0.70 ~ "Concordance forte",
      W >= 0.40 ~ "Concordance modérée",
      W >= 0.10 ~ "Concordance faible",
      TRUE ~ "Concordance très faible"
    ),
    n_sujets = n,
    k_mesures = k,
    posthoc = posthoc,
    sig = res$p.value < alpha,
    conclusion = if (res$p.value < alpha)
      paste0("Rejet de H0 (χ²=", round(res$statistic, 3), ", p=", round(res$p.value, 4),
             ") — Au moins une mesure diffère (W=", round(W, 3), ")")
    else
      paste0("Non rejet de H0 (p=", round(res$p.value, 4),
             ") — Pas de différence entre les mesures")
  )
}

# ── 7. IC BOOTSTRAP POUR TAILLES D'EFFET ───────────────────────────────────

calc_effect_size_ci <- function(d, var_x, var_y, B = 2000, conf = 0.95, seed = 6391) {
  x <- d[[var_x]]; y <- d[[var_y]]

  tp_x <- var_type(x)
  is_cat_x <- tp_x %in% c("binaire", "cat_peu", "cat_many")
  is_num_y <- var_type(y) %in% c("continue", "discrete")

  if (!is_cat_x || !is_num_y) return(NULL)

  grps <- split(y, x)
  grps <- grps[sapply(grps, function(g) sum(!is.na(g)) >= 3)]
  n_grp <- length(grps)

  if (n_grp == 2) {
    g1 <- na.omit(grps[[1]]); g2 <- na.omit(grps[[2]])

    # Bootstrap Cohen's d
    set.seed(seed)
    boot_d <- replicate(B, {
      b1 <- sample(g1, length(g1), replace = TRUE)
      b2 <- sample(g2, length(g2), replace = TRUE)
      sp <- sqrt(((length(b1) - 1) * var(b1) + (length(b2) - 1) * var(b2)) / (length(b1) + length(b2) - 2))
      if (sp == 0) NA else abs(mean(b1) - mean(b2)) / sp
    })
    boot_d <- boot_d[is.finite(boot_d)]

    alpha <- (1 - conf) / 2
    d_obs <- abs(mean(g1) - mean(g2)) /
      sqrt(((length(g1) - 1) * var(g1) + (length(g2) - 1) * var(g2)) / (length(g1) + length(g2) - 2))

    # Glass's delta (utilise SD du groupe contrôle)
    glass_delta <- abs(mean(g1) - mean(g2)) / sd(g2)

    # Hedges' g (correction pour petits échantillons)
    J <- 1 - 3 / (4 * (length(g1) + length(g2) - 2) - 1)
    hedges_g <- d_obs * J

    # Common Language Effect Size (CLES)
    cles <- mean(outer(g1, g2, ">")) + 0.5 * mean(outer(g1, g2, "=="))

    tibble::tibble(
      `Taille d'effet` = c("Cohen d", "Hedges g (corrigé)", "Glass Δ", "CLES (prob. supériorité)"),
      Estimation = round(c(d_obs, hedges_g, glass_delta, cles), 4),
      `IC inf` = round(c(quantile(boot_d, alpha), NA, NA, NA), 4),
      `IC sup` = round(c(quantile(boot_d, 1 - alpha), NA, NA, NA), 4),
      Interpretation = c(
        interp_cohen_d(d_obs),
        interp_cohen_d(hedges_g),
        interp_cohen_d(glass_delta),
        paste0(round(cles * 100, 1), "% de chance qu'un obs. de ", names(grps)[1],
               " > obs. de ", names(grps)[2])
      )
    )
  } else NULL
}

# ── 8. CORRECTIONS POUR COMPARAISONS MULTIPLES ─────────────────────────────

apply_multiple_corrections <- function(p_values, labels = NULL) {
  if (is.null(labels)) labels <- paste0("Test ", seq_along(p_values))
  n <- length(p_values)

  methods <- c("bonferroni", "holm", "hochberg", "BH", "BY")
  results <- tibble::tibble(Test = labels, `p brute` = round(p_values, 4))

  for (m in methods) {
    adj <- p.adjust(p_values, method = m)
    results[[paste0("p_", m)]] <- round(adj, 4)
  }

  # Ajouter colonnes de significativité
  results$`Sig (brute)` <- p_values < 0.05
  results$`Sig (BH)` <- p.adjust(p_values, "BH") < 0.05
  results$`Sig (Bonf)` <- p.adjust(p_values, "bonferroni") < 0.05

  attr(results, "n_tests") <- n
  attr(results, "n_sig_raw") <- sum(p_values < 0.05)
  attr(results, "n_sig_bh") <- sum(p.adjust(p_values, "BH") < 0.05)
  attr(results, "n_sig_bonf") <- sum(p.adjust(p_values, "bonferroni") < 0.05)

  results
}

# ── 9. TESTS NON-PARAMÉTRIQUES SUPPLÉMENTAIRES ─────────────────────────────

run_brunner_munzel <- function(x1, x2, alpha = 0.05) {
  # Test de Brunner-Munzel : alternative robuste à Mann-Whitney
  # Ne requiert pas l'hypothèse de distributions de même forme
  x1 <- na.omit(as.numeric(x1))
  x2 <- na.omit(as.numeric(x2))
  n1 <- length(x1); n2 <- length(x2)
  if (n1 < 5 || n2 < 5) return(NULL)

  res <- tryCatch({
    lawstat::brunner.munzel.test(x1, x2)
  }, error = function(e) NULL)

  if (is.null(res)) return(NULL)

  list(
    nom = "Test de Brunner-Munzel",
    h0 = "H0 : P(X1 > X2) = 0.5 (stochastic equality)",
    h1 = "H1 : P(X1 > X2) ≠ 0.5",
    stat = round(res$statistic, 3),
    df = round(res$parameter, 1),
    p_val = round(res$p.value, 4),
    estimate = round(res$estimate, 4),
    ci = if (!is.null(res$conf.int))
      paste0("[", round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
    else NULL,
    note = "Plus robuste que Mann-Whitney : ne suppose pas des distributions de même forme",
    sig = res$p.value < alpha,
    conclusion = if (res$p.value < alpha)
      paste0("Dominance stochastique significative (p=", round(res$p.value, 4), ")")
    else
      paste0("Pas de dominance stochastique (p=", round(res$p.value, 4), ")")
  )
}

# ── 10. TABLEAU RÉCAPITULATIF EXPERT ────────────────────────────────────────

format_expert_summary <- function(frequentist_tests, bf_results = NULL,
                                   power_results = NULL, tost_result = NULL) {
  summary_parts <- list()

  # Résumé fréquentiste
  if (length(frequentist_tests) > 0) {
    freq_tbl <- purrr::map_dfr(frequentist_tests, function(t) {
      if (!isTRUE(t$valide)) return(NULL)
      tibble::tibble(
        Approche = "Fréquentiste",
        Test = t$nom,
        Statistique = as.character(t$stat),
        `p-valeur` = as.character(t$p_val),
        `Taille d'effet` = paste0(t$effet_nom, " = ", t$effet_val),
        Decision = if (isTRUE(t$sig)) "Significatif" else "Non significatif"
      )
    })
    summary_parts[["freq"]] <- freq_tbl
  }

  # Résumé bayésien
  if (!is.null(bf_results) && nrow(bf_results) > 0) {
    bf_tbl <- bf_results |>
      dplyr::transmute(
        Approche = "Bayésien",
        Test = Test,
        Statistique = paste0("BF10=", BF10),
        `p-valeur` = "—",
        `Taille d'effet` = paste0("log10(BF)=", `log10(BF10)`),
        Decision = Interpretation
      )
    summary_parts[["bayes"]] <- bf_tbl
  }

  # Résumé puissance
  if (!is.null(power_results)) {
    pow_tbl <- power_results |>
      dplyr::transmute(
        Approche = "Puissance",
        Test = `Type de test`,
        Statistique = paste0("1-β=", `Puissance (%)`),
        `p-valeur` = "—",
        `Taille d'effet` = as.character(`Taille d'effet`),
        Decision = Interpretation
      )
    summary_parts[["power"]] <- pow_tbl
  }

  # Résumé TOST
  if (!is.null(tost_result)) {
    tost_tbl <- tibble::tibble(
      Approche = "Équivalence",
      Test = "TOST",
      Statistique = paste0("Δ=±", tost_result$delta),
      `p-valeur` = as.character(tost_result$p_tost),
      `Taille d'effet` = paste0("Diff=", tost_result$diff_moyennes),
      Decision = if (tost_result$equivalent) "Équivalent" else "Non équivalent"
    )
    summary_parts[["tost"]] <- tost_tbl
  }

  dplyr::bind_rows(summary_parts)
}

cat("module_infer_expert.R chargé — 10 techniques inférentielles expert\n")
