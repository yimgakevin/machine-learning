# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_desc_expert.R — Statistiques descriptives niveau expert            ║
# ║  Bootstrap IC, Statistiques robustes, Ajustement de distributions,         ║
# ║  Détection outliers, Batterie de normalité, Profil complet par groupe      ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── 1. BOOTSTRAP IC POUR STATISTIQUES DESCRIPTIVES ──────────────────────────

calc_bootstrap_ci <- function(x, stat_fn, B = 2000, conf = 0.95, seed = 4827) {
  x <- na.omit(as.numeric(x))
  if (length(x) < 5) return(list(estimate = NA, lower = NA, upper = NA, se = NA))

  set.seed(seed)
  boot_vals <- replicate(B, {
    s <- sample(x, length(x), replace = TRUE)
    stat_fn(s)
  })
  boot_vals <- boot_vals[is.finite(boot_vals)]
  if (length(boot_vals) < 10) return(list(estimate = NA, lower = NA, upper = NA, se = NA))

  alpha <- (1 - conf) / 2
  list(
    estimate = stat_fn(x),
    lower    = quantile(boot_vals, alpha, na.rm = TRUE),
    upper    = quantile(boot_vals, 1 - alpha, na.rm = TRUE),
    se       = sd(boot_vals, na.rm = TRUE)
  )
}

calc_stats_bootstrap <- function(x, B = 2000, conf = 0.95) {
  x <- na.omit(as.numeric(x))
  if (length(x) < 5) return(NULL)

  stats_fns <- list(
    Moyenne       = mean,
    Mediane       = median,
    `Ecart-type`  = sd,
    `Moyenne tronquee 10%` = function(v) mean(v, trim = 0.10),
    `MAD`         = mad,
    Skewness      = function(v) e1071::skewness(v),
    Kurtosis      = function(v) e1071::kurtosis(v),
    `CV (%)`      = function(v) sd(v) / mean(v) * 100,
    Q1            = function(v) quantile(v, 0.25),
    Q3            = function(v) quantile(v, 0.75)
  )

  results <- lapply(names(stats_fns), function(nm) {
    res <- calc_bootstrap_ci(x, stats_fns[[nm]], B = B, conf = conf)
    tibble::tibble(
      Statistique = nm,
      Estimation  = round(res$estimate, 4),
      `IC inf`    = round(res$lower, 4),
      `IC sup`    = round(res$upper, 4),
      `Err. std. bootstrap` = round(res$se, 4)
    )
  })

  dplyr::bind_rows(results)
}

# ── 2. STATISTIQUES ROBUSTES ────────────────────────────────────────────────

calc_robust_stats <- function(x) {
  x <- na.omit(as.numeric(x))
  n <- length(x)
  if (n < 5) return(NULL)

  # Moyenne tronquée à différents niveaux
  trim_05 <- mean(x, trim = 0.05)
  trim_10 <- mean(x, trim = 0.10)
  trim_20 <- mean(x, trim = 0.20)

  # Winsorisée
  winsorize <- function(v, fraction = 0.05) {
    lim <- quantile(v, c(fraction, 1 - fraction))
    v[v < lim[1]] <- lim[1]
    v[v > lim[2]] <- lim[2]
    v
  }
  winsor_mean <- mean(winsorize(x, 0.05))

  # MAD (Median Absolute Deviation)
  mad_val <- mad(x, constant = 1.4826)

  # Huber M-estimator

  huber_res <- tryCatch({
    MASS::huber(x)
  }, error = function(e) list(mu = NA, s = NA))

  # IQR-based location
  iqr_val <- IQR(x)
  midhinge <- (quantile(x, 0.25) + quantile(x, 0.75)) / 2

  # Comparaison robuste vs classique
  mean_classic <- mean(x)
  median_val <- median(x)
  sd_classic <- sd(x)

  tibble::tibble(
    Mesure = c(
      "Moyenne classique", "Médiane", "Mi-charnière (midhinge)",
      "Moyenne tronquée 5%", "Moyenne tronquée 10%", "Moyenne tronquée 20%",
      "Moyenne winsorisée 5%",
      "Huber M-estimateur (μ)", "Huber M-estimateur (s)",
      "Écart-type classique", "MAD (× 1.4826)", "IQR",
      "CV classique (%)", "CV robuste (MAD/Médiane %)",
      "Sensibilité (|Moy - Méd| / σ)"
    ),
    Valeur = round(c(
      mean_classic, median_val, as.numeric(midhinge),
      trim_05, trim_10, trim_20,
      winsor_mean,
      huber_res$mu, huber_res$s,
      sd_classic, mad_val, iqr_val,
      sd_classic / mean_classic * 100,
      mad_val / median_val * 100,
      abs(mean_classic - median_val) / sd_classic
    ), 4),
    Type = c(
      "Classique", "Classique", "Robuste",
      "Robuste", "Robuste", "Robuste",
      "Robuste",
      "Robuste", "Robuste",
      "Classique", "Robuste", "Robuste",
      "Classique", "Robuste",
      "Diagnostic"
    ),
    Interpretation = c(
      "", "", "Moyenne des quartiles Q1 et Q3",
      "Exclut 5% extrêmes de chaque côté",
      "Exclut 10% extrêmes de chaque côté",
      "Exclut 20% extrêmes de chaque côté",
      "Ramène les extrêmes aux seuils 5%/95%",
      "Localisation robuste aux outliers",
      "Dispersion robuste (échelle)",
      "", "Écart médian absolu normalisé",
      "Intervalle interquartile",
      "", "CV basé sur mesures robustes",
      if (abs(mean_classic - median_val) / sd_classic > 0.2)
        "Distribution probablement asymétrique"
      else "Distribution relativement symétrique"
    )
  )
}

# ── 3. BATTERIE DE TESTS DE NORMALITÉ ───────────────────────────────────────

calc_normality_battery <- function(x, alpha = 0.05) {
  x <- na.omit(as.numeric(x))
  n <- length(x)
  if (n < 8) return(NULL)

  results <- list()

  # 1. Shapiro-Wilk (n ≤ 5000)
  x_sw <- if (n > 5000) x[1:5000] else x
  sw <- tryCatch(shapiro.test(x_sw), error = function(e) NULL)
  if (!is.null(sw)) {
    results[[length(results) + 1]] <- tibble::tibble(
      Test = "Shapiro-Wilk",
      Statistique = round(sw$statistic, 5),
      `p-valeur` = signif(sw$p.value, 4),
      Decision = if (sw$p.value < alpha) "Rejet H0" else "Non rejet H0",
      Conclusion = if (sw$p.value < alpha) "Non normale" else "Normale",
      Note = paste0("Puissant pour n < 2000. N utilisé = ", length(x_sw))
    )
  }

  # 2. Anderson-Darling
  ad <- tryCatch(nortest::ad.test(x), error = function(e) NULL)
  if (!is.null(ad)) {
    results[[length(results) + 1]] <- tibble::tibble(
      Test = "Anderson-Darling",
      Statistique = round(ad$statistic, 5),
      `p-valeur` = signif(ad$p.value, 4),
      Decision = if (ad$p.value < alpha) "Rejet H0" else "Non rejet H0",
      Conclusion = if (ad$p.value < alpha) "Non normale" else "Normale",
      Note = "Sensible aux queues de distribution"
    )
  }

  # 3. Kolmogorov-Smirnov (Lilliefors)
  ks <- tryCatch(nortest::lillie.test(x), error = function(e) NULL)
  if (!is.null(ks)) {
    results[[length(results) + 1]] <- tibble::tibble(
      Test = "Lilliefors (KS corrigé)",
      Statistique = round(ks$statistic, 5),
      `p-valeur` = signif(ks$p.value, 4),
      Decision = if (ks$p.value < alpha) "Rejet H0" else "Non rejet H0",
      Conclusion = if (ks$p.value < alpha) "Non normale" else "Normale",
      Note = "Version corrigée de KS pour paramètres estimés"
    )
  }

  # 4. Cramér-von Mises
  cvm <- tryCatch(nortest::cvm.test(x), error = function(e) NULL)
  if (!is.null(cvm)) {
    results[[length(results) + 1]] <- tibble::tibble(
      Test = "Cramér-von Mises",
      Statistique = round(cvm$statistic, 5),
      `p-valeur` = signif(cvm$p.value, 4),
      Decision = if (cvm$p.value < alpha) "Rejet H0" else "Non rejet H0",
      Conclusion = if (cvm$p.value < alpha) "Non normale" else "Normale",
      Note = "Poids égal sur toute la distribution"
    )
  }

  # 5. Jarque-Bera (basé sur skewness + kurtosis)
  jb <- tryCatch(tseries::jarque.bera.test(x), error = function(e) NULL)
  if (!is.null(jb)) {
    results[[length(results) + 1]] <- tibble::tibble(
      Test = "Jarque-Bera",
      Statistique = round(jb$statistic, 5),
      `p-valeur` = signif(jb$p.value, 4),
      Decision = if (jb$p.value < alpha) "Rejet H0" else "Non rejet H0",
      Conclusion = if (jb$p.value < alpha) "Non normale" else "Normale",
      Note = paste0("Skew=", round(e1071::skewness(x), 3),
                    " Kurt=", round(e1071::kurtosis(x), 3))
    )
  }

  # 6. D'Agostino-Pearson (skewness + kurtosis combinés)
  dp <- tryCatch({
    sk <- e1071::skewness(x)
    ku <- e1071::kurtosis(x)
    # Z-scores approximatifs pour skewness et kurtosis
    z_sk <- sk / sqrt(6 / n)
    z_ku <- ku / sqrt(24 / n)
    chi2 <- z_sk^2 + z_ku^2
    p_val <- 1 - pchisq(chi2, df = 2)
    list(statistic = chi2, p.value = p_val)
  }, error = function(e) NULL)
  if (!is.null(dp)) {
    results[[length(results) + 1]] <- tibble::tibble(
      Test = "D'Agostino-Pearson (K²)",
      Statistique = round(dp$statistic, 5),
      `p-valeur` = signif(dp$p.value, 4),
      Decision = if (dp$p.value < alpha) "Rejet H0" else "Non rejet H0",
      Conclusion = if (dp$p.value < alpha) "Non normale" else "Normale",
      Note = "Combine asymétrie et aplatissement (χ², ddl=2)"
    )
  }

  if (length(results) == 0) return(NULL)

  tbl <- dplyr::bind_rows(results)

  # Verdict global (vote majoritaire)
  n_normal <- sum(tbl$Conclusion == "Normale")
  n_non    <- sum(tbl$Conclusion == "Non normale")
  n_total  <- nrow(tbl)
  verdict <- if (n_normal > n_non) {
    paste0("NORMALE (", n_normal, "/", n_total, " tests concordants)")
  } else {
    paste0("NON NORMALE (", n_non, "/", n_total, " tests concordants)")
  }

  attr(tbl, "verdict") <- verdict
  attr(tbl, "n_obs") <- n
  tbl
}

# ── 4. DÉTECTION D'OUTLIERS (MULTI-MÉTHODES) ────────────────────────────────

calc_outlier_detection <- function(x, var_name = "x") {
  x_clean <- na.omit(as.numeric(x))
  n <- length(x_clean)
  if (n < 10) return(NULL)

  results <- list()

  # 1. Méthode IQR (Tukey)
  q1 <- quantile(x_clean, 0.25)
  q3 <- quantile(x_clean, 0.75)
  iqr <- q3 - q1
  lower_mild <- q1 - 1.5 * iqr
  upper_mild <- q3 + 1.5 * iqr
  lower_extreme <- q1 - 3.0 * iqr
  upper_extreme <- q3 + 3.0 * iqr
  n_mild <- sum(x_clean < lower_mild | x_clean > upper_mild)
  n_extreme <- sum(x_clean < lower_extreme | x_clean > upper_extreme)

  results[["iqr"]] <- tibble::tibble(
    Methode = "IQR (Tukey)",
    Seuil = paste0("[", round(lower_mild, 2), " ; ", round(upper_mild, 2), "]"),
    `N outliers modérés` = n_mild,
    `N outliers extrêmes` = n_extreme,
    `% outliers` = round(n_mild / n * 100, 1),
    Interpretation = dplyr::case_when(
      n_mild / n > 0.10 ~ "Proportion élevée (>10%)",
      n_mild / n > 0.05 ~ "Proportion modérée (5-10%)",
      TRUE ~ "Proportion acceptable (<5%)"
    )
  )

  # 2. Méthode Z-score
  z_scores <- (x_clean - mean(x_clean)) / sd(x_clean)
  n_z2 <- sum(abs(z_scores) > 2)
  n_z3 <- sum(abs(z_scores) > 3)

  results[["zscore"]] <- tibble::tibble(
    Methode = "Z-score",
    Seuil = "|Z| > 2 et |Z| > 3",
    `N outliers modérés` = n_z2,
    `N outliers extrêmes` = n_z3,
    `% outliers` = round(n_z2 / n * 100, 1),
    Interpretation = paste0(
      round(n_z2 / n * 100, 1), "% au-delà de 2σ (attendu ~5%), ",
      round(n_z3 / n * 100, 1), "% au-delà de 3σ (attendu ~0.3%)"
    )
  )

  # 3. Méthode MAD (Median Absolute Deviation)
  med <- median(x_clean)
  mad_val <- mad(x_clean, constant = 1.4826)
  if (mad_val > 0) {
    mad_scores <- abs(x_clean - med) / mad_val
    n_mad <- sum(mad_scores > 3)
    results[["mad"]] <- tibble::tibble(
      Methode = "MAD (robuste)",
      Seuil = "|X - médiane| / MAD > 3",
      `N outliers modérés` = n_mad,
      `N outliers extrêmes` = sum(mad_scores > 4),
      `% outliers` = round(n_mad / n * 100, 1),
      Interpretation = "Robuste aux outliers existants (contrairement au Z-score)"
    )
  }

  # 4. Test de Grubbs (outlier unique le plus extrême)
  grubbs <- tryCatch({
    outliers::grubbs.test(x_clean)
  }, error = function(e) NULL)
  if (!is.null(grubbs)) {
    results[["grubbs"]] <- tibble::tibble(
      Methode = "Grubbs (max outlier)",
      Seuil = paste0("G = ", round(grubbs$statistic[1], 3)),
      `N outliers modérés` = if (grubbs$p.value < 0.05) 1L else 0L,
      `N outliers extrêmes` = if (grubbs$p.value < 0.01) 1L else 0L,
      `% outliers` = round(if (grubbs$p.value < 0.05) 1 / n * 100 else 0, 2),
      Interpretation = paste0("p=", signif(grubbs$p.value, 3),
                              if (grubbs$p.value < 0.05) " → Outlier significatif"
                              else " → Pas d'outlier significatif")
    )
  }

  # 5. Percentiles extrêmes
  p01 <- quantile(x_clean, 0.01)
  p05 <- quantile(x_clean, 0.05)
  p95 <- quantile(x_clean, 0.95)
  p99 <- quantile(x_clean, 0.99)
  n_p01 <- sum(x_clean < p01 | x_clean > p99)

  results[["percentile"]] <- tibble::tibble(
    Methode = "Percentiles extrêmes",
    Seuil = paste0("P1=", round(p01, 2), " P5=", round(p05, 2),
                   " P95=", round(p95, 2), " P99=", round(p99, 2)),
    `N outliers modérés` = sum(x_clean < p05 | x_clean > p95),
    `N outliers extrêmes` = n_p01,
    `% outliers` = round(n_p01 / n * 100, 1),
    Interpretation = "Valeurs au-delà du 1er/99ème percentile"
  )

  dplyr::bind_rows(results)
}

# Visualisation des outliers
plot_outlier_diagnostic <- function(x, var_name = "x") {
  x_clean <- na.omit(as.numeric(x))
  n <- length(x_clean)
  if (n < 10) return(plotly::plotly_empty())

  df <- tibble::tibble(
    idx  = seq_along(x_clean),
    val  = x_clean,
    z    = (x_clean - mean(x_clean)) / sd(x_clean),
    mad_score = abs(x_clean - median(x_clean)) / mad(x_clean, constant = 1.4826)
  )

  q1 <- quantile(x_clean, 0.25); q3 <- quantile(x_clean, 0.75)
  iqr <- q3 - q1
  df$type <- dplyr::case_when(
    abs(df$z) > 3 | df$val < (q1 - 3 * iqr) | df$val > (q3 + 3 * iqr) ~ "Extrême",
    abs(df$z) > 2 | df$val < (q1 - 1.5 * iqr) | df$val > (q3 + 1.5 * iqr) ~ "Modéré",
    TRUE ~ "Normal"
  )
  df$type <- factor(df$type, levels = c("Normal", "Modéré", "Extrême"))

  colors <- c("Normal" = "#95a5a6", "Modéré" = "#f39c12", "Extrême" = "#e74c3c")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = idx, y = val, color = type,
                                          text = paste0("Obs #", idx, "\nValeur=", round(val, 2),
                                                        "\nZ=", round(z, 2)))) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::geom_hline(yintercept = mean(x_clean), color = "#2980b9",
                        linetype = "dashed", linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = q1 - 1.5 * iqr, color = "#f39c12",
                        linetype = "dotted", linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = q3 + 1.5 * iqr, color = "#f39c12",
                        linetype = "dotted", linewidth = 0.6) +
    ggplot2::scale_color_manual(values = colors, name = "Classification") +
    ggplot2::labs(x = "Observation", y = var_name,
                  title = paste0("Diagnostic outliers — ", var_name)) +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p, tooltip = "text")
}

# ── 5. AJUSTEMENT DE DISTRIBUTIONS ──────────────────────────────────────────

calc_distribution_fit <- function(x) {
  x <- na.omit(as.numeric(x))
  n <- length(x)
  if (n < 20) return(NULL)

  results <- list()

  # Normale
  fit_norm <- tryCatch({
    MASS::fitdistr(x, "normal")
  }, error = function(e) NULL)
  if (!is.null(fit_norm)) {
    ks <- tryCatch(
      ks.test(x, "pnorm", mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"]),
      error = function(e) list(statistic = NA, p.value = NA))
    results[["Normale"]] <- tibble::tibble(
      Distribution = "Normale",
      Parametres = paste0("μ=", round(fit_norm$estimate["mean"], 3),
                          " σ=", round(fit_norm$estimate["sd"], 3)),
      AIC = round(-2 * fit_norm$loglik + 2 * 2, 2),
      BIC = round(-2 * fit_norm$loglik + log(n) * 2, 2),
      LogLik = round(fit_norm$loglik, 2),
      KS_stat = round(ks$statistic, 4),
      KS_pval = signif(ks$p.value, 3),
      Adequation = if (!is.na(ks$p.value) && ks$p.value >= 0.05) "Adéquate" else "Inadéquate"
    )
  }

  # Log-Normale (si toutes les valeurs > 0)
  if (all(x > 0)) {
    fit_ln <- tryCatch({
      MASS::fitdistr(x, "lognormal")
    }, error = function(e) NULL)
    if (!is.null(fit_ln)) {
      ks <- tryCatch(
        ks.test(x, "plnorm", meanlog = fit_ln$estimate["meanlog"],
                sdlog = fit_ln$estimate["sdlog"]),
        error = function(e) list(statistic = NA, p.value = NA))
      results[["Log-Normale"]] <- tibble::tibble(
        Distribution = "Log-Normale",
        Parametres = paste0("μlog=", round(fit_ln$estimate["meanlog"], 3),
                            " σlog=", round(fit_ln$estimate["sdlog"], 3)),
        AIC = round(-2 * fit_ln$loglik + 2 * 2, 2),
        BIC = round(-2 * fit_ln$loglik + log(n) * 2, 2),
        LogLik = round(fit_ln$loglik, 2),
        KS_stat = round(ks$statistic, 4),
        KS_pval = signif(ks$p.value, 3),
        Adequation = if (!is.na(ks$p.value) && ks$p.value >= 0.05) "Adéquate" else "Inadéquate"
      )
    }

    # Gamma
    fit_gam <- tryCatch({
      MASS::fitdistr(x, "gamma")
    }, error = function(e) NULL)
    if (!is.null(fit_gam)) {
      ks <- tryCatch(
        ks.test(x, "pgamma", shape = fit_gam$estimate["shape"],
                rate = fit_gam$estimate["rate"]),
        error = function(e) list(statistic = NA, p.value = NA))
      results[["Gamma"]] <- tibble::tibble(
        Distribution = "Gamma",
        Parametres = paste0("shape=", round(fit_gam$estimate["shape"], 3),
                            " rate=", round(fit_gam$estimate["rate"], 3)),
        AIC = round(-2 * fit_gam$loglik + 2 * 2, 2),
        BIC = round(-2 * fit_gam$loglik + log(n) * 2, 2),
        LogLik = round(fit_gam$loglik, 2),
        KS_stat = round(ks$statistic, 4),
        KS_pval = signif(ks$p.value, 3),
        Adequation = if (!is.na(ks$p.value) && ks$p.value >= 0.05) "Adéquate" else "Inadéquate"
      )
    }

    # Weibull
    fit_wb <- tryCatch({
      MASS::fitdistr(x, "weibull")
    }, error = function(e) NULL)
    if (!is.null(fit_wb)) {
      ks <- tryCatch(
        ks.test(x, "pweibull", shape = fit_wb$estimate["shape"],
                scale = fit_wb$estimate["scale"]),
        error = function(e) list(statistic = NA, p.value = NA))
      results[["Weibull"]] <- tibble::tibble(
        Distribution = "Weibull",
        Parametres = paste0("shape=", round(fit_wb$estimate["shape"], 3),
                            " scale=", round(fit_wb$estimate["scale"], 3)),
        AIC = round(-2 * fit_wb$loglik + 2 * 2, 2),
        BIC = round(-2 * fit_wb$loglik + log(n) * 2, 2),
        LogLik = round(fit_wb$loglik, 2),
        KS_stat = round(ks$statistic, 4),
        KS_pval = signif(ks$p.value, 3),
        Adequation = if (!is.na(ks$p.value) && ks$p.value >= 0.05) "Adéquate" else "Inadéquate"
      )
    }

    # Exponentielle
    fit_exp <- tryCatch({
      MASS::fitdistr(x, "exponential")
    }, error = function(e) NULL)
    if (!is.null(fit_exp)) {
      ks <- tryCatch(
        ks.test(x, "pexp", rate = fit_exp$estimate["rate"]),
        error = function(e) list(statistic = NA, p.value = NA))
      results[["Exponentielle"]] <- tibble::tibble(
        Distribution = "Exponentielle",
        Parametres = paste0("rate=", round(fit_exp$estimate["rate"], 4)),
        AIC = round(-2 * fit_exp$loglik + 2 * 1, 2),
        BIC = round(-2 * fit_exp$loglik + log(n) * 1, 2),
        LogLik = round(fit_exp$loglik, 2),
        KS_stat = round(ks$statistic, 4),
        KS_pval = signif(ks$p.value, 3),
        Adequation = if (!is.na(ks$p.value) && ks$p.value >= 0.05) "Adéquate" else "Inadéquate"
      )
    }
  }

  if (length(results) == 0) return(NULL)
  tbl <- dplyr::bind_rows(results) |> dplyr::arrange(AIC)
  attr(tbl, "best") <- tbl$Distribution[1]
  tbl
}

# Visualisation de l'ajustement
plot_distribution_fit <- function(x, var_name = "x") {
  x <- na.omit(as.numeric(x))
  if (length(x) < 20) return(plotly::plotly_empty())

  df <- tibble::tibble(val = x)
  x_seq <- seq(min(x) - 0.5 * sd(x), max(x) + 0.5 * sd(x), length.out = 300)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = val)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 35, fill = "#3498db", color = "white", alpha = 0.5) +
    ggplot2::geom_density(color = "#2c3e50", linewidth = 1.1, linetype = "solid")

  # Superposer les distributions ajustées
  colors <- c("#e74c3c", "#27ae60", "#f39c12", "#8e44ad", "#1abc9c")
  dists_added <- 0

  # Normale
  fit <- tryCatch(MASS::fitdistr(x, "normal"), error = function(e) NULL)
  if (!is.null(fit)) {
    dists_added <- dists_added + 1
    d_df <- tibble::tibble(x = x_seq,
      y = dnorm(x_seq, fit$estimate["mean"], fit$estimate["sd"]))
    p <- p + ggplot2::geom_line(data = d_df, ggplot2::aes(x = x, y = y),
                                color = colors[1], linewidth = 0.8, linetype = "dashed")
  }

  if (all(x > 0)) {
    # Log-Normale
    fit <- tryCatch(MASS::fitdistr(x, "lognormal"), error = function(e) NULL)
    if (!is.null(fit)) {
      dists_added <- dists_added + 1
      d_df <- tibble::tibble(x = x_seq[x_seq > 0],
        y = dlnorm(x_seq[x_seq > 0], fit$estimate["meanlog"], fit$estimate["sdlog"]))
      p <- p + ggplot2::geom_line(data = d_df, ggplot2::aes(x = x, y = y),
                                  color = colors[2], linewidth = 0.8, linetype = "dashed")
    }

    # Gamma
    fit <- tryCatch(MASS::fitdistr(x, "gamma"), error = function(e) NULL)
    if (!is.null(fit)) {
      dists_added <- dists_added + 1
      d_df <- tibble::tibble(x = x_seq[x_seq > 0],
        y = dgamma(x_seq[x_seq > 0], fit$estimate["shape"], fit$estimate["rate"]))
      p <- p + ggplot2::geom_line(data = d_df, ggplot2::aes(x = x, y = y),
                                  color = colors[3], linewidth = 0.8, linetype = "dashed")
    }

    # Weibull
    fit <- tryCatch(MASS::fitdistr(x, "weibull"), error = function(e) NULL)
    if (!is.null(fit)) {
      dists_added <- dists_added + 1
      d_df <- tibble::tibble(x = x_seq[x_seq > 0],
        y = dweibull(x_seq[x_seq > 0], fit$estimate["shape"], fit$estimate["scale"]))
      p <- p + ggplot2::geom_line(data = d_df, ggplot2::aes(x = x, y = y),
                                  color = colors[4], linewidth = 0.8, linetype = "dashed")
    }
  }

  p <- p +
    ggplot2::labs(x = var_name, y = "Densité",
                  title = paste0("Ajustement de distributions — ", var_name),
                  caption = "Rouge=Normale | Vert=Log-N | Orange=Gamma | Violet=Weibull | Noir=KDE") +
    ggplot2::theme_minimal(base_size = 11)

  plotly::ggplotly(p)
}

# ── 6. PROFIL DESCRIPTIF COMPLET PAR GROUPE ─────────────────────────────────

calc_group_profile <- function(df, num_vars, group_var) {
  if (!group_var %in% names(df)) return(NULL)
  num_vars <- intersect(num_vars, names(df))
  if (length(num_vars) == 0) return(NULL)

  df_long <- df |>
    dplyr::select(dplyr::all_of(c(group_var, num_vars))) |>
    tidyr::pivot_longer(-dplyr::all_of(group_var), names_to = "Variable", values_to = "Valeur")

  df_long |>
    dplyr::group_by(.data[[group_var]], Variable) |>
    dplyr::summarise(
      N       = sum(!is.na(Valeur)),
      Moyenne = round(mean(Valeur, na.rm = TRUE), 2),
      Mediane = round(median(Valeur, na.rm = TRUE), 2),
      `Ec-type` = round(sd(Valeur, na.rm = TRUE), 2),
      `CV (%)` = round(sd(Valeur, na.rm = TRUE) / mean(Valeur, na.rm = TRUE) * 100, 1),
      Min     = round(min(Valeur, na.rm = TRUE), 2),
      Q1      = round(quantile(Valeur, 0.25, na.rm = TRUE), 2),
      Q3      = round(quantile(Valeur, 0.75, na.rm = TRUE), 2),
      Max     = round(max(Valeur, na.rm = TRUE), 2),
      Skew    = round(e1071::skewness(Valeur, na.rm = TRUE), 3),
      Kurt    = round(e1071::kurtosis(Valeur, na.rm = TRUE), 3),
      `NA (%)` = round(sum(is.na(Valeur)) / dplyr::n() * 100, 1),
      .groups = "drop"
    )
}

# Heatmap des moyennes par groupe
plot_group_heatmap <- function(df, num_vars, group_var) {
  prof <- calc_group_profile(df, num_vars, group_var)
  if (is.null(prof) || nrow(prof) == 0) return(plotly::plotly_empty())

  # Normaliser les moyennes pour la heatmap (0-1 par variable)
  prof_norm <- prof |>
    dplyr::group_by(Variable) |>
    dplyr::mutate(
      Moy_norm = (Moyenne - min(Moyenne, na.rm = TRUE)) /
        max(1e-10, max(Moyenne, na.rm = TRUE) - min(Moyenne, na.rm = TRUE))
    ) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(prof_norm,
    ggplot2::aes(x = Variable, y = .data[[group_var]], fill = Moy_norm,
                 text = paste0(.data[[group_var]], "\n", Variable,
                               "\nMoy=", Moyenne, " ± ", `Ec-type`,
                               "\nCV=", `CV (%)`, "%"))) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = Moyenne), size = 3, color = "white", fontface = "bold") +
    ggplot2::scale_fill_gradient2(low = "#2980b9", mid = "#f39c12", high = "#e74c3c",
                                  midpoint = 0.5, name = "Niveau\n(normalisé)") +
    ggplot2::labs(x = "", y = "", title = paste0("Profil descriptif par ", group_var)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  plotly::ggplotly(p, tooltip = "text")
}

# ── 7. MATRICE DE CORRÉLATION AVANCÉE ───────────────────────────────────────

calc_correlation_matrix_advanced <- function(df, vars, method = "spearman", p_adjust = "BH") {
  df_sel <- df |> dplyr::select(dplyr::all_of(vars)) |> tidyr::drop_na()
  n <- nrow(df_sel)
  p <- length(vars)
  if (n < 5 || p < 2) return(NULL)

  # Matrice de corrélation + p-valeurs
  cor_mat <- matrix(NA, p, p, dimnames = list(vars, vars))
  pval_mat <- matrix(NA, p, p, dimnames = list(vars, vars))

  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (i == j) {
        cor_mat[i, j] <- 1
        pval_mat[i, j] <- 0
      } else if (j > i) {
        test <- tryCatch(
          cor.test(df_sel[[vars[i]]], df_sel[[vars[j]]], method = method),
          error = function(e) NULL
        )
        if (!is.null(test)) {
          cor_mat[i, j] <- cor_mat[j, i] <- test$estimate
          pval_mat[i, j] <- pval_mat[j, i] <- test$p.value
        }
      }
    }
  }

  # Ajustement multiple des p-valeurs
  upper_idx <- which(upper.tri(pval_mat), arr.ind = TRUE)
  raw_p <- pval_mat[upper_idx]
  adj_p <- p.adjust(raw_p, method = p_adjust)
  pval_adj_mat <- pval_mat
  for (k in seq_along(adj_p)) {
    i <- upper_idx[k, 1]; j <- upper_idx[k, 2]
    pval_adj_mat[i, j] <- pval_adj_mat[j, i] <- adj_p[k]
  }

  list(
    cor = round(cor_mat, 4),
    pval = pval_mat,
    pval_adj = pval_adj_mat,
    n = n,
    method = method,
    p_adjust = p_adjust,
    n_tests = length(raw_p),
    n_sig_raw = sum(raw_p < 0.05, na.rm = TRUE),
    n_sig_adj = sum(adj_p < 0.05, na.rm = TRUE)
  )
}

# Tableau long des corrélations significatives
format_cor_table <- function(cor_res) {
  if (is.null(cor_res)) return(NULL)
  vars <- rownames(cor_res$cor)
  p <- length(vars)
  rows <- list()

  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      rows[[length(rows) + 1]] <- tibble::tibble(
        `Var 1` = vars[i],
        `Var 2` = vars[j],
        r = cor_res$cor[i, j],
        `p brute` = signif(cor_res$pval[i, j], 3),
        `p ajustée` = signif(cor_res$pval_adj[i, j], 3),
        Force = dplyr::case_when(
          abs(cor_res$cor[i, j]) >= 0.70 ~ "Très forte",
          abs(cor_res$cor[i, j]) >= 0.50 ~ "Forte",
          abs(cor_res$cor[i, j]) >= 0.30 ~ "Modérée",
          abs(cor_res$cor[i, j]) >= 0.10 ~ "Faible",
          TRUE ~ "Négligeable"
        ),
        Significative = if (cor_res$pval_adj[i, j] < 0.05) "Oui" else "Non"
      )
    }
  }

  dplyr::bind_rows(rows) |> dplyr::arrange(dplyr::desc(abs(r)))
}

cat("module_desc_expert.R chargé — 7 techniques descriptives expert\n")
