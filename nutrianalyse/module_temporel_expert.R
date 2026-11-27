# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_temporel_expert.R — Fonctions Analytiques Temporelles Avancées    ║
# ║  7 Techniques Expert pour Analyse Longitudinale Multi-Vagues              ║
# ║  NutriAnalyse Pro v2                                                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# 1. COMPARAISON MOYENNES CONTINUES INTER-VAGUES
#    t-test / Wilcoxon + Cohen d + Bootstrap IC
# ══════════════════════════════════════════════════════════════════════════════

#' Compare continuous indicators across survey rounds
#' @param d data.frame with round column and numeric indicators
#' @param round_col name of the round/wave column
#' @param r1,r2 round labels for comparison
#' @param indicators character vector of numeric column names
#' @param conf_level confidence level (default 0.95)
#' @return data.frame with test results per indicator
run_continuous_comparison <- function(d, round_col, r1, r2, indicators, conf_level = 0.95) {
  d1 <- d[d[[round_col]] == r1 & !is.na(d[[round_col]]), ]
  d2 <- d[d[[round_col]] == r2 & !is.na(d[[round_col]]), ]

  # [P2] Filtrer uniquement les colonnes numériques existantes
  indicators <- indicators[indicators %in% names(d) & sapply(indicators, function(v) is.numeric(d[[v]]))]
  if (length(indicators) == 0) {
    return(data.frame(Variable = character(), n_V1 = integer(), n_V2 = integer(),
                      Moy_V1 = numeric(), ET_V1 = numeric(), Med_V1 = numeric(),
                      Moy_V2 = numeric(), ET_V2 = numeric(), Med_V2 = numeric(),
                      Diff_moy = numeric(), IC_lo = numeric(), IC_hi = numeric(),
                      t_stat = numeric(), p_t = numeric(),
                      W_stat = numeric(), p_wilcox = numeric(),
                      Cohen_d = numeric(), d_IC_lo = numeric(), d_IC_hi = numeric(),
                      Effet = character(), stringsAsFactors = FALSE))
  }

  results <- lapply(indicators, function(var) {
    x1 <- na.omit(d1[[var]]); x2 <- na.omit(d2[[var]])
    n1 <- length(x1); n2 <- length(x2)
    if (n1 < 5 || n2 < 5) {
      return(data.frame(Variable = var, n_V1 = n1, n_V2 = n2,
                        Moy_V1 = NA, ET_V1 = NA, Med_V1 = NA,
                        Moy_V2 = NA, ET_V2 = NA, Med_V2 = NA,
                        Diff_moy = NA, IC_lo = NA, IC_hi = NA,
                        t_stat = NA, p_t = NA,
                        W_stat = NA, p_wilcox = NA,
                        Cohen_d = NA, d_IC_lo = NA, d_IC_hi = NA,
                        Effet = "Effectifs insuffisants",
                        stringsAsFactors = FALSE))
    }

    m1 <- mean(x1); m2 <- mean(x2)
    s1 <- sd(x1); s2 <- sd(x2)

    # Welch t-test
    tt <- tryCatch(t.test(x2, x1, conf.level = conf_level), error = function(e) NULL)
    # Wilcoxon rank-sum
    wt <- tryCatch(suppressWarnings(wilcox.test(x2, x1, conf.int = TRUE, conf.level = conf_level)),
                   error = function(e) NULL)

    # Cohen's d (pooled SD)
    s_pool <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    cohen_d <- if (s_pool > 0) (m2 - m1) / s_pool else NA

    # Bootstrap CI for Cohen's d
    d_ci <- tryCatch({
      boot_d <- replicate(1000, {
        bx1 <- sample(x1, n1, replace = TRUE)
        bx2 <- sample(x2, n2, replace = TRUE)
        sp <- sqrt(((n1 - 1) * sd(bx1)^2 + (n2 - 1) * sd(bx2)^2) / (n1 + n2 - 2))
        if (sp > 0) (mean(bx2) - mean(bx1)) / sp else NA
      })
      quantile(boot_d, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)
    }, error = function(e) c(NA, NA))

    effet <- if (is.na(cohen_d)) "Indéterminé"
             else if (abs(cohen_d) < 0.2) "Négligeable"
             else if (abs(cohen_d) < 0.5) "Petit"
             else if (abs(cohen_d) < 0.8) "Moyen"
             else "Grand"

    data.frame(
      Variable = var, n_V1 = n1, n_V2 = n2,
      Moy_V1 = round(m1, 2), ET_V1 = round(s1, 2), Med_V1 = round(median(x1), 2),
      Moy_V2 = round(m2, 2), ET_V2 = round(s2, 2), Med_V2 = round(median(x2), 2),
      Diff_moy = round(m2 - m1, 2),
      IC_lo = if (!is.null(tt)) round(tt$conf.int[1], 2) else NA,
      IC_hi = if (!is.null(tt)) round(tt$conf.int[2], 2) else NA,
      t_stat = if (!is.null(tt)) round(tt$statistic, 3) else NA,
      p_t = if (!is.null(tt)) tt$p.value else NA,
      W_stat = if (!is.null(wt)) round(wt$statistic, 1) else NA,
      p_wilcox = if (!is.null(wt)) wt$p.value else NA,
      Cohen_d = round(cohen_d, 3),
      d_IC_lo = round(d_ci[1], 3), d_IC_hi = round(d_ci[2], 3),
      Effet = effet,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, results)
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. DIFFERENCE-IN-DIFFERENCES (DID)
#    Impact evaluation: (treatment vs control) × (before vs after)
# ══════════════════════════════════════════════════════════════════════════════

#' Difference-in-Differences estimator
#' @param d data.frame
#' @param round_col column identifying rounds
#' @param r1,r2 baseline and endline round labels
#' @param outcome numeric outcome variable name
#' @param group_col column identifying treatment/control groups
#' @param treat_label label for treatment group
#' @param control_label label for control group
#' @return list with DID estimate, SE, CI, model summary
run_did_analysis <- function(d, round_col, r1, r2, outcome, group_col,
                              treat_label, control_label) {
  # [P4] Validation : les labels traitement et contrôle doivent être différents

  if (treat_label == control_label) {
    return(list(error = "Les labels traitement et contrôle sont identiques"))
  }

  dd <- d[d[[round_col]] %in% c(r1, r2) & d[[group_col]] %in% c(treat_label, control_label), ]
  dd <- dd[!is.na(dd[[outcome]]) & !is.na(dd[[round_col]]) & !is.na(dd[[group_col]]), ]

  if (nrow(dd) < 20) return(list(error = "Effectifs insuffisants (n < 20)"))

  dd$post <- as.integer(dd[[round_col]] == r2)
  dd$treat <- as.integer(dd[[group_col]] == treat_label)
  dd$y <- dd[[outcome]]

  # DID regression: y = β0 + β1·post + β2·treat + β3·post×treat + ε
  fit <- tryCatch(lm(y ~ post * treat, data = dd), error = function(e) NULL)
  if (is.null(fit)) return(list(error = "Échec de la régression DID"))

  sm <- summary(fit)
  ci <- confint(fit)
  coefs <- broom::tidy(fit, conf.int = TRUE)

  # Means by cell
  cells <- dd |>
    dplyr::group_by(treat, post) |>
    dplyr::summarise(mean_y = mean(y, na.rm = TRUE),
                     sd_y = sd(y, na.rm = TRUE),
                     n = dplyr::n(), .groups = "drop")
  cells$Groupe <- ifelse(cells$treat == 1, treat_label, control_label)
  cells$Periode <- ifelse(cells$post == 1, r2, r1)

  # DID estimate is the interaction coefficient
  did_row <- coefs[coefs$term == "post:treat", ]
  did_est <- if (nrow(did_row) > 0) did_row$estimate else NA
  did_se  <- if (nrow(did_row) > 0) did_row$std.error else NA
  did_p   <- if (nrow(did_row) > 0) did_row$p.value else NA
  did_lo  <- if (nrow(did_row) > 0) did_row$conf.low else NA
  did_hi  <- if (nrow(did_row) > 0) did_row$conf.high else NA

  list(
    did_estimate = round(did_est, 3),
    did_se = round(did_se, 3),
    did_ci = c(round(did_lo, 3), round(did_hi, 3)),
    did_p = did_p,
    r_squared = round(sm$r.squared, 4),
    adj_r_squared = round(sm$adj.r.squared, 4),
    coefficients = coefs,
    cells = cells,
    n_total = nrow(dd),
    model = fit,
    outcome = outcome,
    group_col = group_col,
    treat_label = treat_label,
    control_label = control_label,
    r1 = r1, r2 = r2
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. TREND ANALYSIS MULTI-ROUNDS
#    Cochran-Armitage (proportions), Jonckheere-Terpstra (continu)
# ══════════════════════════════════════════════════════════════════════════════

#' Multi-round trend analysis for proportions and continuous indicators
#' @param d data.frame
#' @param round_col column identifying rounds (must have natural ordering)
#' @param indicators character vector of variable names
#' @param round_order optional explicit ordering of rounds
#' @return list with trend results per indicator
run_trend_analysis <- function(d, round_col, indicators, round_order = NULL) {
  rounds <- if (!is.null(round_order)) round_order
            else sort(unique(na.omit(d[[round_col]])))
  n_rounds <- length(rounds)

  if (n_rounds < 2) return(list(error = "Au moins 2 vagues nécessaires"))

  results <- lapply(indicators, function(var) {
    vals <- d[[var]]
    is_binary <- all(na.omit(vals) %in% c(0, 1))

    # Per-round stats
    round_stats <- lapply(seq_along(rounds), function(i) {
      x <- vals[d[[round_col]] == rounds[i] & !is.na(d[[round_col]])]
      x <- na.omit(x)
      n <- length(x)
      if (n == 0) return(NULL)
      if (is_binary) {
        k <- sum(x == 1)
        p <- k / n
        se <- sqrt(p * (1 - p) / n)
        list(round = rounds[i], order = i, n = n, k = k,
             estimate = round(p * 100, 2), se = round(se * 100, 2),
             ci_lo = round((p - 1.96 * se) * 100, 2),
             ci_hi = round((p + 1.96 * se) * 100, 2),
             type = "proportion")
      } else {
        m <- mean(x); s <- sd(x); se <- s / sqrt(n)
        list(round = rounds[i], order = i, n = n,
             estimate = round(m, 2), se = round(se, 2),
             ci_lo = round(m - 1.96 * se, 2),
             ci_hi = round(m + 1.96 * se, 2),
             type = "continuous")
      }
    })
    round_stats <- round_stats[!sapply(round_stats, is.null)]
    if (length(round_stats) < 2) return(NULL)

    type <- round_stats[[1]]$type
    estimates <- sapply(round_stats, `[[`, "estimate")

    # Trend tests
    trend_result <- list()

    if (is_binary && n_rounds >= 2) {
      # Cochran-Armitage trend test
      ns <- sapply(round_stats, `[[`, "n")
      ks <- sapply(round_stats, `[[`, "k")
      ca <- tryCatch({
        tab <- rbind(ks, ns - ks)
        prop.trend.test(ks, ns)
      }, error = function(e) NULL)
      if (!is.null(ca)) {
        trend_result$cochran_armitage <- list(
          chi2 = round(ca$statistic, 3),
          p_value = ca$p.value,
          method = "Cochran-Armitage"
        )
      }
    }

    if (!is_binary && n_rounds >= 3) {
      # [P1] Jonckheere-Terpstra trend test — guard si clinfun non installé
      jt <- tryCatch({
        if (!requireNamespace("clinfun", quietly = TRUE)) stop("clinfun non disponible")
        groups <- factor(d[[round_col]], levels = rounds)
        valid <- !is.na(groups) & !is.na(vals)
        clinfun::jonckheere.test(vals[valid], as.numeric(groups[valid]), alternative = "two.sided")
      }, error = function(e) NULL)
      if (!is.null(jt)) {
        trend_result$jonckheere <- list(
          statistic = round(jt$statistic, 3),
          p_value = jt$p.value,
          method = "Jonckheere-Terpstra"
        )
      }
    }

    # [P3] Linear trend — seulement si 3+ rounds (2 points → R²=1, p=NaN)
    ord <- seq_along(round_stats)
    if (length(ord) >= 3) {
      lt <- tryCatch({
        fit <- lm(estimates ~ ord)
        sm <- summary(fit)
        list(
          slope = round(coef(fit)[2], 3),
          slope_se = round(sm$coefficients[2, 2], 3),
          r_squared = round(sm$r.squared, 3),
          p_value = sm$coefficients[2, 4],
          method = "Régression linéaire"
        )
      }, error = function(e) NULL)
      if (!is.null(lt)) trend_result$linear <- lt
    }

    # Direction assessment
    first_est <- estimates[1]
    last_est  <- estimates[length(estimates)]
    pct_change <- if (first_est != 0) round((last_est - first_est) / abs(first_est) * 100, 1) else NA
    direction <- if (last_est > first_est) "↗ Tendance haussière"
                 else if (last_est < first_est) "↘ Tendance baissière"
                 else "→ Stable"

    list(
      variable = var,
      type = type,
      n_rounds = length(round_stats),
      round_stats = round_stats,
      estimates = estimates,
      first = first_est, last = last_est,
      pct_change = pct_change,
      direction = direction,
      trend_tests = trend_result
    )
  })

  results <- results[!sapply(results, is.null)]
  names(results) <- sapply(results, `[[`, "variable")
  list(results = results, rounds = rounds, n_rounds = n_rounds)
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. CUSUM — Cumulative Sum Control Chart (Early Warning)
#    Détection de décalages par rapport à un seuil de référence
# ══════════════════════════════════════════════════════════════════════════════

#' CUSUM analysis for early warning detection
#' @param d data.frame
#' @param round_col round column
#' @param indicator indicator variable name
#' @param zone_col optional zone column for zone-level CUSUM
#' @param target_mean reference mean (NULL = use overall mean)
#' @param k allowance parameter (default 0.5 SD)
#' @param h decision interval (default 4 SD)
#' @return list with CUSUM values and alerts
run_cusum_analysis <- function(d, round_col, indicator, zone_col = NULL,
                                target_mean = NULL, k = 0.5, h = 4) {
  rounds <- sort(unique(na.omit(d[[round_col]])))
  is_binary <- all(na.omit(d[[indicator]]) %in% c(0, 1))

  do_cusum <- function(sub_d, label = "Global") {
    stats_by_round <- lapply(rounds, function(r) {
      x <- sub_d[[indicator]][sub_d[[round_col]] == r & !is.na(sub_d[[round_col]])]
      x <- na.omit(x)
      n <- length(x)
      if (n == 0) return(NULL)
      if (is_binary) list(round = r, n = n, value = mean(x) * 100)
      else list(round = r, n = n, value = mean(x))
    })
    stats_by_round <- stats_by_round[!sapply(stats_by_round, is.null)]
    if (length(stats_by_round) < 2) return(NULL)

    values <- sapply(stats_by_round, `[[`, "value")
    ns <- sapply(stats_by_round, `[[`, "n")
    rnames <- sapply(stats_by_round, `[[`, "round")

    mu0 <- if (!is.null(target_mean)) target_mean else mean(values)
    sigma <- sd(values)
    if (sigma == 0) sigma <- 1

    # Two-sided CUSUM
    K <- k * sigma
    H <- h * sigma
    S_hi <- numeric(length(values)); S_lo <- numeric(length(values))
    alert_hi <- logical(length(values)); alert_lo <- logical(length(values))

    for (i in seq_along(values)) {
      prev_hi <- if (i > 1) S_hi[i - 1] else 0
      prev_lo <- if (i > 1) S_lo[i - 1] else 0
      S_hi[i] <- max(0, prev_hi + (values[i] - mu0) - K)
      S_lo[i] <- max(0, prev_lo - (values[i] - mu0) - K)
      alert_hi[i] <- S_hi[i] > H
      alert_lo[i] <- S_lo[i] > H
    }

    data.frame(
      Zone = label, Round = rnames, n = ns,
      Valeur = round(values, 2),
      Ref = round(mu0, 2),
      Ecart = round(values - mu0, 2),
      CUSUM_haut = round(S_hi, 2),
      CUSUM_bas = round(S_lo, 2),
      Seuil_H = round(H, 2),
      Alerte_hausse = alert_hi,
      Alerte_baisse = alert_lo,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(zone_col) && zone_col %in% names(d)) {
    zones <- sort(unique(na.omit(d[[zone_col]])))
    cusum_list <- lapply(zones, function(z) {
      sub <- d[d[[zone_col]] == z & !is.na(d[[zone_col]]), ]
      do_cusum(sub, label = as.character(z))
    })
    cusum_df <- do.call(rbind, cusum_list[!sapply(cusum_list, is.null)])
  } else {
    cusum_df <- do_cusum(d, label = "Global")
  }

  if (is.null(cusum_df) || nrow(cusum_df) == 0) {
    return(list(error = "Données insuffisantes pour le CUSUM"))
  }

  n_alerts <- sum(cusum_df$Alerte_hausse | cusum_df$Alerte_baisse)

  list(
    cusum = cusum_df,
    n_alerts = n_alerts,
    indicator = indicator,
    is_binary = is_binary,
    k = k, h = h
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. BOOTSTRAP TEMPORAL — IC Bootstrap pour différences inter-vagues
#    Percentile + BCa bootstrap
# ══════════════════════════════════════════════════════════════════════════════

#' Bootstrap confidence intervals for inter-wave differences
#' @param d data.frame
#' @param round_col round column
#' @param r1,r2 round labels
#' @param indicators variable names
#' @param n_boot number of bootstrap resamples (default 2000)
#' @param conf_level confidence level
#' @return data.frame with bootstrap results
run_bootstrap_temporal <- function(d, round_col, r1, r2, indicators,
                                    n_boot = 2000, conf_level = 0.95) {
  d1 <- d[d[[round_col]] == r1 & !is.na(d[[round_col]]), ]
  d2 <- d[d[[round_col]] == r2 & !is.na(d[[round_col]]), ]
  alpha <- 1 - conf_level

  results <- lapply(indicators, function(var) {
    x1 <- na.omit(d1[[var]]); x2 <- na.omit(d2[[var]])
    n1 <- length(x1); n2 <- length(x2)
    if (n1 < 10 || n2 < 10) {
      return(data.frame(Variable = var, n_V1 = n1, n_V2 = n2,
                        Stat_V1 = NA, Stat_V2 = NA,
                        Diff_obs = NA,
                        Boot_moy = NA, Boot_ET = NA,
                        IC_perc_lo = NA, IC_perc_hi = NA,
                        IC_bca_lo = NA, IC_bca_hi = NA,
                        Biais = NA, p_boot = NA,
                        stringsAsFactors = FALSE))
    }

    is_binary <- all(c(x1, x2) %in% c(0, 1))
    stat_fn <- if (is_binary) function(x) mean(x) * 100 else mean

    obs1 <- stat_fn(x1); obs2 <- stat_fn(x2)
    obs_diff <- obs2 - obs1

    # Bootstrap differences
    boot_diffs <- replicate(n_boot, {
      b1 <- sample(x1, n1, replace = TRUE)
      b2 <- sample(x2, n2, replace = TRUE)
      stat_fn(b2) - stat_fn(b1)
    })

    boot_mean <- mean(boot_diffs)
    boot_se <- sd(boot_diffs)
    bias <- boot_mean - obs_diff

    # Percentile CI
    perc_ci <- quantile(boot_diffs, c(alpha / 2, 1 - alpha / 2))

    # BCa CI (simplified)
    z0 <- qnorm(mean(boot_diffs < obs_diff))
    jack_vals <- sapply(seq_along(c(x1, x2)), function(i) {
      if (i <= n1) {
        j1 <- x1[-i]; j2 <- x2
      } else {
        j1 <- x1; j2 <- x2[-(i - n1)]
      }
      if (length(j1) < 2 || length(j2) < 2) return(NA)
      stat_fn(j2) - stat_fn(j1)
    })
    jack_vals <- na.omit(jack_vals)
    jack_mean <- mean(jack_vals)
    a <- if (length(jack_vals) > 2) {
      num <- sum((jack_mean - jack_vals)^3)
      den <- 6 * (sum((jack_mean - jack_vals)^2))^1.5
      if (den != 0) num / den else 0
    } else 0

    bca_ci <- tryCatch({
      z_alpha <- qnorm(c(alpha / 2, 1 - alpha / 2))
      adj <- sapply(z_alpha, function(za) {
        pnorm(z0 + (z0 + za) / (1 - a * (z0 + za)))
      })
      adj[adj < 0.001] <- 0.001; adj[adj > 0.999] <- 0.999
      quantile(boot_diffs, adj)
    }, error = function(e) perc_ci)

    # Bootstrap p-value (H0: diff = 0)
    p_boot <- 2 * min(mean(boot_diffs >= 0), mean(boot_diffs <= 0))

    data.frame(
      Variable = var, n_V1 = n1, n_V2 = n2,
      Stat_V1 = round(obs1, 2), Stat_V2 = round(obs2, 2),
      Diff_obs = round(obs_diff, 2),
      Boot_moy = round(boot_mean, 2), Boot_ET = round(boot_se, 2),
      IC_perc_lo = round(perc_ci[1], 2), IC_perc_hi = round(perc_ci[2], 2),
      IC_bca_lo = round(bca_ci[1], 2), IC_bca_hi = round(bca_ci[2], 2),
      Biais = round(bias, 3), p_boot = round(p_boot, 4),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. HEATMAP TEMPORELLE MULTIVARIÉE
#    Multi-indicateur × Multi-round avec significativité
# ══════════════════════════════════════════════════════════════════════════════

#' Build multi-indicator × multi-round summary matrix
#' @param d data.frame
#' @param round_col round column
#' @param indicators variable names
#' @param round_order optional explicit round ordering
#' @return list with matrices for values, pairwise p-values, pairwise changes
build_temporal_heatmap <- function(d, round_col, indicators, round_order = NULL) {
  rounds <- if (!is.null(round_order)) round_order
            else sort(unique(na.omit(d[[round_col]])))

  # Value matrix: indicator × round
  val_mat <- matrix(NA, nrow = length(indicators), ncol = length(rounds),
                    dimnames = list(indicators, rounds))
  n_mat <- val_mat

  for (var in indicators) {
    is_binary <- all(na.omit(d[[var]]) %in% c(0, 1))
    for (r in rounds) {
      x <- d[[var]][d[[round_col]] == r & !is.na(d[[round_col]])]
      x <- na.omit(x)
      n_mat[var, r] <- length(x)
      if (length(x) > 0) {
        val_mat[var, r] <- if (is_binary) round(mean(x) * 100, 1) else round(mean(x), 1)
      }
    }
  }

  # Pairwise change matrix (consecutive rounds)
  if (length(rounds) >= 2) {
    pairs <- paste0(rounds[-length(rounds)], " → ", rounds[-1])
    change_mat <- matrix(NA, nrow = length(indicators), ncol = length(pairs),
                         dimnames = list(indicators, pairs))
    p_mat <- change_mat
    sig_mat <- matrix("", nrow = length(indicators), ncol = length(pairs),
                      dimnames = list(indicators, pairs))

    for (var in indicators) {
      is_binary <- all(na.omit(d[[var]]) %in% c(0, 1))
      for (j in seq_along(pairs)) {
        v1 <- val_mat[var, rounds[j]]
        v2 <- val_mat[var, rounds[j + 1]]
        if (!is.na(v1) && !is.na(v2)) {
          change_mat[var, pairs[j]] <- round(v2 - v1, 1)
        }

        # Significance test
        x1 <- na.omit(d[[var]][d[[round_col]] == rounds[j]])
        x2 <- na.omit(d[[var]][d[[round_col]] == rounds[j + 1]])
        if (length(x1) >= 5 && length(x2) >= 5) {
          p_val <- tryCatch({
            if (is_binary) {
              ct <- chisq.test(matrix(c(sum(x1 == 1), sum(x1 == 0),
                                        sum(x2 == 1), sum(x2 == 0)),
                                      nrow = 2, byrow = TRUE), correct = TRUE)
              ct$p.value
            } else {
              suppressWarnings(wilcox.test(x2, x1)$p.value)
            }
          }, error = function(e) NA)
          p_mat[var, pairs[j]] <- round(p_val, 4)
          sig_mat[var, pairs[j]] <- if (is.na(p_val)) ""
                                     else if (p_val < 0.001) "***"
                                     else if (p_val < 0.01) "**"
                                     else if (p_val < 0.05) "*"
                                     else ""
        }
      }
    }
  } else {
    change_mat <- NULL; p_mat <- NULL; sig_mat <- NULL
  }

  list(
    values = val_mat,
    n = n_mat,
    changes = change_mat,
    p_values = p_mat,
    significance = sig_mat,
    rounds = rounds,
    indicators = indicators
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 7. PROFIL ÉVOLUTIF PAR ZONE — Slope Plots & Spaghetti
#    Trajectoires zone-level avec identification des anomalies
# ══════════════════════════════════════════════════════════════════════════════

#' Compute zone-level trajectories across rounds
#' @param d data.frame
#' @param round_col round column
#' @param indicator indicator variable
#' @param zone_col zone column
#' @param round_order optional round ordering
#' @return list with zone trajectories and anomaly flags
run_zone_trajectory <- function(d, round_col, indicator, zone_col,
                                 round_order = NULL) {
  rounds <- if (!is.null(round_order)) round_order
            else sort(unique(na.omit(d[[round_col]])))
  zones <- sort(unique(na.omit(d[[zone_col]])))
  is_binary <- all(na.omit(d[[indicator]]) %in% c(0, 1))

  trajectories <- lapply(zones, function(z) {
    sub <- d[d[[zone_col]] == z & !is.na(d[[zone_col]]), ]
    vals <- sapply(rounds, function(r) {
      x <- sub[[indicator]][sub[[round_col]] == r & !is.na(sub[[round_col]])]
      x <- na.omit(x)
      if (length(x) == 0) return(NA)
      if (is_binary) mean(x) * 100 else mean(x)
    })
    ns <- sapply(rounds, function(r) {
      sum(!is.na(sub[[indicator]][sub[[round_col]] == r & !is.na(sub[[round_col]])]))
    })
    data.frame(Zone = z, Round = rounds, Valeur = round(vals, 2),
               n = ns, stringsAsFactors = FALSE)
  })

  traj_df <- do.call(rbind, trajectories)

  # National average trajectory
  nat_vals <- sapply(rounds, function(r) {
    x <- d[[indicator]][d[[round_col]] == r & !is.na(d[[round_col]])]
    x <- na.omit(x)
    if (length(x) == 0) return(NA)
    if (is_binary) mean(x) * 100 else mean(x)
  })
  nat_df <- data.frame(Zone = "Moyenne nationale", Round = rounds,
                        Valeur = round(nat_vals, 2), n = NA,
                        stringsAsFactors = FALSE)

  # [P5] Anomaly detection: filter zones with data in all rounds
  if (length(rounds) >= 2) {
    zone_changes <- sapply(zones, function(z) {
      zv <- traj_df$Valeur[traj_df$Zone == z]
      if (any(is.na(zv))) return(NA)
      zv[length(zv)] - zv[1]
    })
    nat_change <- nat_vals[length(nat_vals)] - nat_vals[1]
    divergence <- abs(zone_changes - nat_change)
    anomalies <- data.frame(
      Zone = zones, Changement = round(zone_changes, 2),
      Changement_national = round(nat_change, 2),
      Divergence = round(divergence, 2),
      stringsAsFactors = FALSE
    )
    # Filtrer les zones sans données complètes
    anomalies <- anomalies[!is.na(anomalies$Changement) & !is.na(anomalies$Divergence), ]
    anomalies <- anomalies[order(-anomalies$Divergence), ]
    anomalies$Rang <- seq_len(nrow(anomalies))
  } else {
    anomalies <- NULL
  }

  list(
    trajectories = traj_df,
    national = nat_df,
    anomalies = anomalies,
    rounds = rounds,
    zones = zones,
    indicator = indicator,
    is_binary = is_binary
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# PLOTTING FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

#' Forest plot for continuous comparisons
plot_continuous_forest <- function(res_df) {
  req_cols <- c("Variable", "Diff_moy", "IC_lo", "IC_hi", "p_t")
  if (!all(req_cols %in% names(res_df))) return(NULL)
  df <- res_df[!is.na(res_df$Diff_moy), ]
  if (nrow(df) == 0) return(NULL)

  df$Sig <- ifelse(!is.na(df$p_t) & df$p_t < 0.05, "Significatif", "Non significatif")

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = Diff_moy, y = reorder(Variable, abs(Diff_moy)), color = Sig,
    text = paste0(Variable, "\nDiff: ", round(Diff_moy, 2),
                  "\nIC: [", IC_lo, " ; ", IC_hi, "]",
                  "\np = ", round(p_t, 4)))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = IC_lo, xmax = IC_hi),
                            height = 0.25, linewidth = 0.8) +
    ggplot2::geom_point(size = 4) +
    ggplot2::scale_color_manual(values = c("Significatif" = "#c9161d",
                                            "Non significatif" = "#008DC9")) +
    ggplot2::labs(x = "Différence des moyennes (V2 - V1)", y = "", color = "") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  plotly::ggplotly(p, tooltip = "text") |> plotly::layout(hovermode = "closest")
}

#' DID visualization
plot_did <- function(did_res) {
  if (is.null(did_res) || !is.null(did_res$error)) return(NULL)
  cells <- did_res$cells

  p <- ggplot2::ggplot(cells, ggplot2::aes(
    x = Periode, y = mean_y, color = Groupe, group = Groupe,
    text = paste0(Groupe, " — ", Periode,
                  "\nMoyenne: ", round(mean_y, 2),
                  "\nET: ", round(sd_y, 2),
                  "\nn = ", n))) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_y - 1.96 * sd_y / sqrt(n),
                                         ymax = mean_y + 1.96 * sd_y / sqrt(n)),
                           width = 0.1) +
    ggplot2::scale_color_manual(values = c("#008DC9", "#e88400")) +
    ggplot2::labs(x = "", y = did_res$outcome, color = "") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  plotly::ggplotly(p, tooltip = "text")
}

#' Trend plot (multi-round)
plot_trend <- function(trend_res, var_name) {
  if (is.null(trend_res) || !is.null(trend_res$error)) return(NULL)
  r <- trend_res$results[[var_name]]
  if (is.null(r)) return(NULL)

  df <- do.call(rbind, lapply(r$round_stats, function(s) {
    data.frame(Round = s$round, Valeur = s$estimate,
               CI_lo = s$ci_lo, CI_hi = s$ci_hi, n = s$n,
               stringsAsFactors = FALSE)
  }))
  df$Order <- seq_len(nrow(df))

  ylab <- if (r$type == "proportion") "Prévalence (%)" else "Moyenne"

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = Order, y = Valeur,
    text = paste0(Round, "\n", ylab, ": ", Valeur,
                  "\nIC: [", CI_lo, " ; ", CI_hi, "]\nn = ", n))) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lo, ymax = CI_hi),
                         alpha = 0.2, fill = "#008DC9") +
    ggplot2::geom_line(color = "#008DC9", linewidth = 1.2) +
    ggplot2::geom_point(color = "#00426A", size = 4) +
    ggplot2::scale_x_continuous(breaks = df$Order, labels = df$Round) +
    ggplot2::labs(x = "", y = ylab, title = var_name) +
    ggplot2::theme_minimal(base_size = 12)

  # Add trend line if linear fit exists
  if (!is.null(r$trend_tests$linear)) {
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                                   se = FALSE, linetype = "dashed",
                                   color = "#e88400", linewidth = 0.8)
  }

  plotly::ggplotly(p, tooltip = "text")
}

#' CUSUM chart
plot_cusum <- function(cusum_res) {
  if (is.null(cusum_res) || !is.null(cusum_res$error)) return(NULL)
  df <- cusum_res$cusum

  # CUSUM_haut
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Round, group = Zone)) +
    ggplot2::geom_hline(yintercept = df$Seuil_H[1], linetype = "dashed",
                        color = "#c9161d", linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = CUSUM_haut, color = Zone), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = CUSUM_haut, color = Zone,
                                      shape = Alerte_hausse), size = 3) +
    ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                                guide = "none") +
    ggplot2::labs(x = "", y = "CUSUM (hausse)", color = "Zone",
                  title = paste0("CUSUM — ", cusum_res$indicator)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  plotly::ggplotly(p)
}

#' Spaghetti / slope plot for zone trajectories
plot_zone_trajectory <- function(traj_res) {
  if (is.null(traj_res)) return(NULL)
  traj <- traj_res$trajectories
  nat <- traj_res$national

  all_df <- rbind(traj[, c("Zone", "Round", "Valeur")],
                  nat[, c("Zone", "Round", "Valeur")])
  all_df$Round_f <- factor(all_df$Round, levels = traj_res$rounds)
  all_df$is_nat <- all_df$Zone == "Moyenne nationale"

  p <- ggplot2::ggplot(all_df, ggplot2::aes(
    x = Round_f, y = Valeur, color = Zone, group = Zone,
    linewidth = is_nat,
    text = paste0(Zone, " — ", Round, "\nValeur: ", Valeur))) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_linewidth_manual(values = c("FALSE" = 0.7, "TRUE" = 2), guide = "none") +
    ggplot2::labs(x = "", y = if (traj_res$is_binary) "Prévalence (%)" else "Moyenne",
                  color = "Zone",
                  title = paste0("Trajectoires — ", traj_res$indicator)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  plotly::ggplotly(p, tooltip = "text")
}

#' Temporal heatmap plot
plot_temporal_heatmap <- function(hm_res, show = "values") {
  if (is.null(hm_res)) return(NULL)

  mat <- if (show == "changes" && !is.null(hm_res$changes)) hm_res$changes
         else hm_res$values
  if (is.null(mat)) return(NULL)

  # Convert to long format
  df <- expand.grid(Indicateur = rownames(mat), Periode = colnames(mat),
                    stringsAsFactors = FALSE)
  df$Valeur <- as.vector(mat)

  # Add significance stars if showing changes
  if (show == "changes" && !is.null(hm_res$significance)) {
    sig <- as.vector(hm_res$significance)
    df$label <- paste0(df$Valeur, sig)
  } else {
    df$label <- as.character(df$Valeur)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = Periode, y = Indicateur, fill = Valeur,
    text = paste0(Indicateur, " — ", Periode, "\nValeur: ", Valeur))) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.5, color = "black") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   panel.grid = ggplot2::element_blank())

  if (show == "changes") {
    p <- p + ggplot2::scale_fill_gradient2(low = "#4dac26", mid = "white",
                                            high = "#c9161d", midpoint = 0,
                                            na.value = "gray90",
                                            name = "Variation")
  } else {
    p <- p + ggplot2::scale_fill_viridis_c(option = "D", na.value = "gray90",
                                             name = "Valeur")
  }

  plotly::ggplotly(p, tooltip = "text")
}

cat("✅ module_temporel_expert.R chargé — 7 techniques + 7 plots\n")
