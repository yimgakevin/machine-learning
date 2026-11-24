# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_expert_helpers.R — Socle « niveau expert » partagé par les modules     ║
# ║  IC Wilson / design-based, qualité, tailles d'effet, exports, sélecteurs.   ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

EXPERT_HAS_SURVEY <- requireNamespace("survey", quietly = TRUE)

# ── 1. Conversion binaire robuste ───────────────────────────────────────────
# Retourne un vecteur entier 0/1, NA quand la modalité n'est pas interprétable.
expert_to_binary <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) {
    u <- unique(stats::na.omit(x))
    if (length(u) > 0 && all(u %in% c(0, 1))) return(as.integer(x))
    return(rep(NA_integer_, length(x)))
  }
  v <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(v))
  out[v %in% c("oui", "yes", "1", "true", "vrai", "positif", "positive", "pos")] <- 1L
  out[v %in% c("non", "no", "0", "false", "faux", "négatif", "negatif", "negative", "neg")] <- 0L
  out
}

# ── 2. IC Wilson vectorisé ──────────────────────────────────────────────────
expert_wilson <- function(k, n, level = 0.95) {
  k <- as.numeric(k); n <- as.numeric(n)
  p <- ifelse(n > 0, k / n, NA_real_)
  z <- stats::qnorm(1 - (1 - level) / 2)
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  marge <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  cbind(lower = pmax(0, centre - marge), upper = pmin(1, centre + marge))
}

# ── 3. Plan de sondage ──────────────────────────────────────────────────────
# Construit un svydesign sur des colonnes temporaires (.psu/.strata/.w) pour
# rester robuste aux noms de colonnes non syntaxiques.
expert_design <- function(d, cluster = NULL, strata = NULL, weights = NULL) {
  if (!EXPERT_HAS_SURVEY) return(NULL)
  d2 <- as.data.frame(d)
  if (nrow(d2) < 5) return(NULL)
  ok <- rep(TRUE, nrow(d2))
  if (!is.null(cluster) && cluster %in% names(d2)) ok <- ok & !is.na(d2[[cluster]])
  if (!is.null(strata) && strata %in% names(d2)) ok <- ok & !is.na(d2[[strata]])
  d2 <- d2[ok, , drop = FALSE]
  if (nrow(d2) < 5) return(NULL)
  d2$.psu <- if (!is.null(cluster) && cluster %in% names(d2))
    as.factor(d2[[cluster]]) else factor(rep(1L, nrow(d2)))
  if (!is.null(strata) && strata %in% names(d2)) d2$.strata <- as.factor(d2[[strata]])
  d2$.w <- if (!is.null(weights) && weights %in% names(d2)) {
    w <- suppressWarnings(as.numeric(d2[[weights]]))
    w[!is.finite(w) | w <= 0] <- 1
    w
  } else rep(1, nrow(d2))
  old <- options(survey.lonely.psu = "adjust")
  on.exit(options(old), add = TRUE)
  tryCatch({
    if (!is.null(strata) && strata %in% names(d2)) {
      survey::svydesign(ids = ~.psu, strata = ~.strata, weights = ~.w,
                        data = d2, nest = TRUE)
    } else {
      survey::svydesign(ids = ~.psu, weights = ~.w, data = d2)
    }
  }, error = function(e) NULL)
}

# ── 4. Prévalence + IC (design-based si demandé, Wilson sinon) ──────────────
expert_prev <- function(d, var, label = var, cluster = NULL, strata = NULL,
                        weights = NULL, level = 0.95) {
  empty <- tibble::tibble(
    Indicateur = label, N = 0L, Cas = 0L, `Prév. (%)` = NA_real_,
    `IC bas (%)` = NA_real_, `IC haut (%)` = NA_real_,
    DEFF = NA_real_, Methode = "Non calculable"
  )
  if (is.null(var) || !var %in% names(d)) return(empty)
  d2 <- as.data.frame(d)
  d2$.y <- expert_to_binary(d2[[var]])
  d2 <- d2[!is.na(d2$.y), , drop = FALSE]
  n <- nrow(d2); k <- sum(d2$.y == 1)
  if (n == 0) return(empty)
  use_design <- !is.null(cluster) || !is.null(strata) || !is.null(weights)
  if (use_design) {
    des <- expert_design(d2, cluster, strata, weights)
    if (!is.null(des)) {
      est <- tryCatch({
        m <- survey::svymean(~.y, des, na.rm = TRUE)
        p <- as.numeric(stats::coef(m))[1]
        se <- as.numeric(survey::SE(m))[1]
        ci <- tryCatch(as.numeric(stats::confint(
          survey::svyciprop(~.y, des, method = "logit"))),
          error = function(e) c(max(0, p - 1.96 * se), min(1, p + 1.96 * se)))
        if (length(ci) < 2 || any(!is.finite(ci))) ci <- c(max(0, p - 1.96 * se), min(1, p + 1.96 * se))
        deff <- if (is.finite(se) && se > 0 && p > 0 && p < 1) se^2 / (p * (1 - p) / n) else NA_real_
        tibble::tibble(
          Indicateur = label, N = n, Cas = k,
          `Prév. (%)` = round(p * 100, 2),
          `IC bas (%)` = round(max(0, ci[1]) * 100, 1),
          `IC haut (%)` = round(min(1, ci[2]) * 100, 1),
          DEFF = round(deff, 2), Methode = "Design-based"
        )
      }, error = function(e) NULL)
      if (!is.null(est)) return(est)
    }
  }
  ic <- expert_wilson(k, n, level)
  tibble::tibble(
    Indicateur = label, N = n, Cas = k,
    `Prév. (%)` = round(k / n * 100, 2),
    `IC bas (%)` = round(ic[1, "lower"] * 100, 1),
    `IC haut (%)` = round(ic[1, "upper"] * 100, 1),
    DEFF = NA_real_, Methode = "Wilson"
  )
}

# Prévalences pour plusieurs indicateurs (vecteur nommé var → label)
expert_prev_multi <- function(d, vars, labels = NULL, cluster = NULL,
                              strata = NULL, weights = NULL) {
  labs <- names(vars)
  keep <- vars %in% names(d)
  vars <- vars[keep]
  if (length(vars) == 0) return(tibble::tibble())
  labels <- if (is.null(labels)) {
    if (!is.null(labs)) labs[keep] else vars
  } else labels[keep]
  purrr::map_dfr(seq_along(vars), function(i)
    expert_prev(d, vars[i], labels[[i]], cluster, strata, weights))
}

# Prévalences par strate + test d'association
expert_prev_by <- function(d, var, by, label = var, cluster = NULL,
                           strata = NULL, weights = NULL) {
  if (is.null(by) || !by %in% names(d) || !var %in% names(d)) return(tibble::tibble())
  d2 <- as.data.frame(d)
  d2$.y <- expert_to_binary(d2[[var]])
  d2 <- d2[!is.na(d2$.y) & !is.na(d2[[by]]), , drop = FALSE]
  if (nrow(d2) == 0) return(tibble::tibble())
  groups <- sort(unique(as.character(d2[[by]])))
  purrr::map_dfr(groups, function(g) {
    sub <- d2[as.character(d2[[by]]) == g, , drop = FALSE]
    res <- expert_prev(sub, ".y", label, cluster = cluster, strata = strata, weights = weights)
    res$Strate <- by
    res$Groupe <- g
    res
  })
}

# ── 5. Test d'association + V de Cramér ─────────────────────────────────────
expert_strat_test <- function(d, var, by) {
  if (is.null(by) || !by %in% names(d) || !var %in% names(d)) return(NULL)
  y <- expert_to_binary(d[[var]])
  g <- as.character(d[[by]])
  keep <- !is.na(y) & !is.na(g)
  if (sum(keep) < 5) return(NULL)
  tab <- table(g[keep], y[keep])
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NULL)
  n <- sum(tab)
  test <- tryCatch({
    if (any(chisq.test(tab)$expected < 5)) {
      ft <- suppressWarnings(stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000))
      list(nom = "Fisher exact (simulé)", stat = NA_real_, df = NA_real_, p = ft$p.value)
    } else {
      ct <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
      list(nom = "Chi² de Pearson", stat = as.numeric(ct$statistic),
           df = as.numeric(ct$parameter), p = ct$p.value)
    }
  }, error = function(e) NULL)
  if (is.null(test)) return(NULL)
  v <- tryCatch(sqrt(as.numeric(suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic) /
                       (n * (min(dim(tab)) - 1))), error = function(e) NA_real_)
  tibble::tibble(
    Strate = by, Test = test$nom, Statistique = round(test$stat, 3),
    ddl = test$df, p = test$p, `V de Cramér` = round(v, 3), N = n
  )
}

# ── 6. Taux d'incidence (mortalité) avec IC et test ─────────────────────────
# Rate pour `scale` unités de temps-personne (10 000 jours-personne par défaut).
expert_rate <- function(d, events, time, label = "Taux", cluster = NULL,
                        strata = NULL, weights = NULL, scale = 10000) {
  empty <- tibble::tibble(
    Indicateur = label, Deces = 0, `Temps-personne` = 0, `Taux` = NA_real_,
    `IC bas` = NA_real_, `IC haut` = NA_real_, Methode = "Non calculable"
  )
  if (!all(c(events, time) %in% names(d))) return(empty)
  d2 <- as.data.frame(d)
  ev <- suppressWarnings(as.numeric(d2[[events]]))
  pt <- suppressWarnings(as.numeric(d2[[time]]))
  keep <- !is.na(ev) & !is.na(pt) & pt > 0
  d2 <- d2[keep, , drop = FALSE]; ev <- ev[keep]; pt <- pt[keep]
  if (length(ev) == 0) return(empty)
  use_design <- !is.null(cluster) || !is.null(strata) || !is.null(weights)
  if (use_design) {
    d3 <- d2; d3$.ev <- ev; d3$.pt <- pt
    des <- expert_design(d3, cluster, strata, weights)
    if (!is.null(des)) {
      est <- tryCatch({
        r <- survey::svyratio(~.ev, ~.pt, des)
        ci <- stats::confint(r)
        tibble::tibble(
          Indicateur = label, Deces = sum(ev), `Temps-personne` = round(sum(pt)),
          `Taux` = round(as.numeric(stats::coef(r))[1] * scale, 4),
          `IC bas` = round(ci[1, 1] * scale, 4),
          `IC haut` = round(ci[1, 2] * scale, 4),
          Methode = "Design-based (svyratio)"
        )
      }, error = function(e) NULL)
      if (!is.null(est)) return(est)
    }
  }
  k <- sum(ev); tot <- sum(pt)
  tibble::tibble(
    Indicateur = label, Deces = k, `Temps-personne` = round(tot),
    `Taux` = round(k / tot * scale, 4),
    `IC bas` = round(stats::qpois(0.025, k) / tot * scale, 4),
    `IC haut` = round(stats::qpois(0.975, k) / tot * scale, 4),
    Methode = "Poisson exact"
  )
}

expert_rate_by <- function(d, events, time, by, label = "Taux", cluster = NULL,
                           strata = NULL, weights = NULL, scale = 10000) {
  if (is.null(by) || !by %in% names(d)) return(list(table = tibble::tibble(), test = NULL))
  d2 <- as.data.frame(d)
  ev <- suppressWarnings(as.numeric(d2[[events]]))
  pt <- suppressWarnings(as.numeric(d2[[time]]))
  keep <- !is.na(ev) & !is.na(pt) & pt > 0 & !is.na(d2[[by]])
  if (sum(keep) < 5) return(list(table = tibble::tibble(), test = NULL))
  d2 <- d2[keep, , drop = FALSE]
  groups <- sort(unique(as.character(d2[[by]])))
  tab <- purrr::map_dfr(groups, function(g) {
    sub <- d2[as.character(d2[[by]]) == g, , drop = FALSE]
    r <- expert_rate(sub, events, time, label, cluster, strata, weights, scale)
    r$Groupe <- g; r
  })
  test <- tryCatch({
    fit <- stats::glm(stats::as.formula(paste0("`", events, "` ~ `", by, "`")),
                      data = d2, family = stats::quasipoisson(),
                      offset = log(pmax(pt, 1e-9)))
    a <- stats::anova(fit, test = "F")
    p <- a[["Pr(>F)"]][2]
    tibble::tibble(Test = "GLM quasi-Poisson (offset temps-personne)",
                   Statistique = round(a[["F"]][2], 3), ddl = a[["Df"]][2],
                   p = p, N = nrow(d2))
  }, error = function(e) NULL)
  list(table = tab, test = test)
}

# ── 7. Contrôle qualité générique ───────────────────────────────────────────
expert_quality <- function(d, vars) {
  vars <- vars[vars %in% names(d)]
  if (length(vars) == 0) return(tibble::tibble())
  purrr::map_dfr(vars, function(v) {
    x <- d[[v]]
    n <- length(x)
    if (is.character(x)) {
      blank <- is.na(x) | !nzchar(trimws(x))
      n_val <- sum(!blank)
      n_uniq <- dplyr::n_distinct(x[!blank])
      mn <- NA_real_; mx <- NA_real_; out <- NA_integer_; neg <- NA_integer_
    } else {
      n_val <- sum(!is.na(x))
      n_uniq <- dplyr::n_distinct(x[!is.na(x)])
      xn <- suppressWarnings(as.numeric(x))
      mn <- if (n_val > 0) suppressWarnings(min(xn, na.rm = TRUE)) else NA_real_
      mx <- if (n_val > 0) suppressWarnings(max(xn, na.rm = TRUE)) else NA_real_
      q <- if (n_val > 3) stats::quantile(xn, c(.25, .75), na.rm = TRUE) else c(NA, NA)
      iqr <- q[2] - q[1]
      out <- if (is.finite(iqr) && iqr > 0)
        sum(xn < q[1] - 3 * iqr | xn > q[2] + 3 * iqr, na.rm = TRUE) else NA_integer_
      neg <- sum(xn < 0, na.rm = TRUE)
    }
    tibble::tibble(
      Variable = v,
      Type = if (is.numeric(x)) "Numérique" else if (is.logical(x)) "Logique" else "Catégorielle",
      N = n, `N valides` = n_val, `Manquants` = n - n_val,
      `% manquants` = if (n > 0) round((n - n_val) / n * 100, 1) else NA_real_,
      `Modalités` = n_uniq, Min = mn, Max = mx,
      `Valeurs extrêmes (3 IQR)` = out, `Valeurs négatives` = neg
    )
  })
}

# ── 8. Statistiques expertes univariées (avec plan de sondage) ──────────────
expert_uni_table <- function(d, var, cluster = NULL, strata = NULL, weights = NULL) {
  out <- list(summary = tibble::tibble(), modalities = tibble::tibble())
  if (is.null(var) || !var %in% names(d)) return(out)
  x <- d[[var]]
  use_design <- !is.null(cluster) || !is.null(strata) || !is.null(weights)
  if (is.numeric(x)) {
    s <- expert_num_summary(x)
    if (use_design) {
      d2 <- as.data.frame(d); d2$.x <- suppressWarnings(as.numeric(x))
      des <- expert_design(d2[!is.na(d2$.x), , drop = FALSE], cluster, strata, weights)
      if (!is.null(des)) {
        est <- tryCatch({
          m <- survey::svymean(~.x, des, na.rm = TRUE)
          ci <- stats::confint(m)
          tibble::tibble(
            Mesure = c("Moyenne pondérée", "IC95 pondéré — bas", "IC95 pondéré — haut"),
            Valeur = c(round(as.numeric(stats::coef(m))[1], 3),
                       round(ci[1, 1], 3), round(ci[1, 2], 3))
          )
        }, error = function(e) NULL)
        if (!is.null(est)) s <- dplyr::bind_rows(s, est)
      }
    }
    out$summary <- s
    return(out)
  }
  xc <- as.character(x)
  lev <- sort(unique(xc[!is.na(xc)]))
  out$modalities <- purrr::map_dfr(lev, function(g) {
    d2 <- as.data.frame(d); d2$.y <- as.integer(!is.na(xc) & xc == g)
    r <- expert_prev(d2, ".y", g, cluster, strata, weights)
    r$Modalite <- g; r
  })
  out$modalities$N_total <- sum(!is.na(xc))
  out
}

# ── 10. Tests multiples avec correction (module Bivarié) ────────────────────
expert_multi_tests <- function(d, y, x_vars) {
  x_vars <- setdiff(x_vars[x_vars %in% names(d)], y)
  if (length(x_vars) == 0) return(tibble::tibble())
  ty <- var_type(d[[y]])
  rows <- purrr::map_dfr(x_vars, function(v) {
    tx <- var_type(d[[v]])
    res <- tryCatch({
      if (ty %in% c("binaire", "cat_peu", "cat_many") && tx %in% c("continue", "discrete")) {
        auto_test(d, y, v, ty, tx)
      } else {
        auto_test(d, v, y, tx, ty)
      }
    }, error = function(e) NULL)
    if (is.null(res) || is.null(res$p) || length(res$p) == 0 || is.na(res$p)) return(NULL)
    tibble::tibble(Variable = v, Test = res$nom,
                   Statistique = suppressWarnings(as.numeric(res$stat)),
                   ddl = suppressWarnings(as.numeric(res$df)), p = res$p)
  })
  if (nrow(rows) == 0) return(rows)
  rows$`p ajusté (BH)` <- expert_adjust_p(rows$p, "BH")
  rows$`Significatif 5 %` <- rows$`p ajusté (BH)` < 0.05
  rows[order(rows$`p ajusté (BH)`, rows$p), , drop = FALSE]
}

# ── 12. Clustering : ARI et stabilité bootstrap ─────────────────────────────
expert_ari <- function(a, b) {
  tab <- table(a, b); n <- sum(tab)
  if (n < 2) return(NA_real_)
  comb2 <- function(x) sum(choose(x, 2))
  idx <- comb2(tab)
  ra <- comb2(rowSums(tab)); rb <- comb2(colSums(tab))
  expected <- ra * rb / choose(n, 2)
  max_idx <- (ra + rb) / 2
  if (isTRUE(all.equal(max_idx, expected))) return(1)
  (idx - expected) / (max_idx - expected)
}

expert_recluster <- function(d_scaled, method, k, eps = 0.5, minPts = 5) {
  tryCatch({
    if (method == "kmeans") {
      stats::kmeans(d_scaled, centers = k, nstart = 10)$cluster
    } else if (method == "hclust") {
      stats::cutree(stats::hclust(stats::dist(d_scaled), method = "ward.D2"), k = k)
    } else {
      if (!requireNamespace("dbscan", quietly = TRUE)) return(NULL)
      dbscan::dbscan(d_scaled, eps = eps, minPts = minPts)$cluster
    }
  }, error = function(e) NULL)
}

expert_bootstrap_stability <- function(d_scaled, clusters, method, k, eps = 0.5,
                                       B = 30, seed = 2026) {
  n <- nrow(d_scaled)
  if (n < 5 || length(clusters) != n) return(NULL)
  set.seed(seed)
  aris <- numeric(B)
  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    cb <- expert_recluster(d_scaled[idx, , drop = FALSE], method, k, eps)
    aris[b] <- if (is.null(cb) || length(cb) != n) NA_real_ else expert_ari(clusters[idx], cb)
  }
  list(mean_ari = mean(aris, na.rm = TRUE), sd_ari = stats::sd(aris, na.rm = TRUE),
       aris = aris, B = sum(!is.na(aris)))
}

# ── 13. Statistiques numériques complètes (module Univarié) ─────────────────
expert_num_summary <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  n <- length(x); nv <- sum(!is.na(x)); xv <- x[!is.na(x)]
  if (nv == 0) return(tibble::tibble(Mesure = "N", Valeur = 0))
  m <- mean(xv); s <- stats::sd(xv); se <- s / sqrt(nv)
  tcrit <- stats::qt(0.975, df = max(1, nv - 1))
  q <- stats::quantile(xv, c(.25, .5, .75), na.rm = TRUE)
  iqr <- q[3] - q[1]
  out <- if (is.finite(iqr) && iqr > 0) sum(xv < q[1] - 3 * iqr | xv > q[3] + 3 * iqr) else 0L
  shp <- if (nv >= 3 && nv <= 5000) tryCatch(stats::shapiro.test(xv)$p.value, error = function(e) NA_real_) else NA_real_
  sk <- if (nv > 2) mean((xv - m)^3) / (mean((xv - m)^2)^1.5) else NA_real_
  ku <- if (nv > 3) mean((xv - m)^4) / (mean((xv - m)^2)^2) - 3 else NA_real_
  tibble::tibble(
    Mesure = c("N", "N valides", "N manquants", "Moyenne", "Écart-type",
               "Erreur standard", "IC95 moyenne — bas", "IC95 moyenne — haut",
               "Médiane", "Q1", "Q3", "IQR", "Min", "Max", "Étendue",
               "Asymétrie", "Aplatissement", "Valeurs extrêmes (3 IQR)",
               "Shapiro-Wilk p"),
    Valeur = c(n, nv, n - nv, round(m, 3), round(s, 3), round(se, 3),
               round(m - tcrit * se, 3), round(m + tcrit * se, 3),
               round(q[2], 3), round(q[1], 3), round(q[3], 3), round(iqr, 3),
               round(min(xv), 3), round(max(xv), 3), round(diff(range(xv)), 3),
               round(sk, 3), round(ku, 3), out, round(shp, 4))
  )
}

# ── 8. Tailles d'effet bivariées ────────────────────────────────────────────
expert_effect_sizes <- function(d, x, y) {
  if (is.null(x) || is.null(y) || !all(c(x, y) %in% names(d))) return(tibble::tibble())
  dx <- d[[x]]; dy <- d[[y]]
  tx <- var_type(dx); ty <- var_type(dy)
  num_x <- is.numeric(dx); num_y <- is.numeric(dy)
  rows <- list()
  if (!num_x && !num_y) {
    keep <- !is.na(dx) & !is.na(dy)
    tab <- table(dx[keep], dy[keep])
    if (nrow(tab) > 1 && ncol(tab) > 1) {
      ct <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
      v <- sqrt(as.numeric(ct$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
      rows[[length(rows) + 1]] <- tibble::tibble(
        Mesure = c("Chi²", "ddl", "p", "V de Cramér", "N"),
        Valeur = c(round(as.numeric(ct$statistic), 3), as.numeric(ct$parameter),
                   round(ct$p.value, 4), round(v, 3), sum(tab)))
    }
  } else if (num_x != num_y) {
    g <- if (num_x) dy else dx
    v <- if (num_x) dx else dy
    keep <- !is.na(g) & !is.na(v)
    g <- g[keep]; v <- v[keep]
    lv <- unique(as.character(g))
    if (length(lv) == 2) {
      a <- v[as.character(g) == lv[1]]; b <- v[as.character(g) == lv[2]]
      sp <- sqrt(((length(a) - 1) * stats::var(a) + (length(b) - 1) * stats::var(b)) /
                   max(1, length(a) + length(b) - 2))
      dcohen <- if (is.finite(sp) && sp > 0) (mean(a) - mean(b)) / sp else NA_real_
      wt <- tryCatch(suppressWarnings(stats::wilcox.test(a, b)$p.value), error = function(e) NA_real_)
      rows[[length(rows) + 1]] <- tibble::tibble(
        Mesure = c("Moyenne groupe 1", "Moyenne groupe 2", "Différence",
                   "d de Cohen", "p Wilcoxon", "N"),
        Valeur = c(round(mean(a), 3), round(mean(b), 3), round(mean(a) - mean(b), 3),
                   round(dcohen, 3), round(wt, 4), length(v)))
    } else {
      kw <- tryCatch(suppressWarnings(stats::kruskal.test(v ~ g)$p.value), error = function(e) NA_real_)
      rows[[length(rows) + 1]] <- tibble::tibble(
        Mesure = c("Groupes", "p Kruskal-Wallis", "N"),
        Valeur = c(length(lv), round(kw, 4), length(v)))
    }
  } else {
    keep <- !is.na(dx) & !is.na(dy)
    if (sum(keep) >= 3) {
      pr <- suppressWarnings(stats::cor.test(dx[keep], dy[keep], method = "pearson"))
      sr <- suppressWarnings(stats::cor.test(dx[keep], dy[keep], method = "spearman", exact = FALSE))
      rows[[length(rows) + 1]] <- tibble::tibble(
        Mesure = c("r Pearson", "p Pearson", "ρ Spearman", "p Spearman", "N"),
        Valeur = c(round(pr$estimate, 3), round(pr$p.value, 4),
                   round(sr$estimate, 3), round(sr$p.value, 4), sum(keep)))
    }
  }
  if (length(rows) == 0) tibble::tibble() else dplyr::bind_rows(rows)
}

# ── 9. Correction multi-tests ───────────────────────────────────────────────
expert_adjust_p <- function(p, method = "BH") {
  p <- as.numeric(p)
  method <- match.arg(method, c("BH", "holm", "bonferroni", "none"))
  if (method == "none") return(p)
  stats::p.adjust(p, method = method)
}

# ── 10. Sélecteurs & contrôles de design ────────────────────────────────────
expert_col_select <- function(ns, id, label, d, candidates = character(),
                              selected = NULL, allow_empty = TRUE) {
  cols <- names(d)
  hit <- intersect(candidates, cols)
  if (is.null(selected)) selected <- if (length(hit) > 0) hit[1] else ""
  if (!selected %in% cols) selected <- ""
  choices <- if (allow_empty) c("—" = "", stats::setNames(cols, cols)) else stats::setNames(cols, cols)
  shiny::selectInput(ns(id), label, choices = choices, selected = selected,
                     width = "100%")
}

expert_design_controls_ui <- function(ns, prefix) {
  shiny::tagList(
    shiny::checkboxInput(ns(paste0(prefix, "_use_design")),
                         "Utiliser le plan de sondage", value = FALSE),
    shiny::uiOutput(ns(paste0(prefix, "_cluster_ui"))),
    shiny::uiOutput(ns(paste0(prefix, "_strata_ui"))),
    shiny::uiOutput(ns(paste0(prefix, "_weights_ui")))
  )
}

expert_design_controls_server <- function(input, output, session, prefix, df_reactive) {
  ns <- session$ns
  output[[paste0(prefix, "_cluster_ui")]] <- shiny::renderUI({
    d <- df_reactive(); shiny::req(d)
    expert_col_select(ns, paste0(prefix, "_cluster"), "Cluster / grappe", d,
                      c("cluster", "cluster_id", "grappe", "psu",
                        "primary_sampling_unit", "code_cluster"))
  })
  output[[paste0(prefix, "_strata_ui")]] <- shiny::renderUI({
    d <- df_reactive(); shiny::req(d)
    expert_col_select(ns, paste0(prefix, "_strata"), "Strates", d,
                      c("strate", "strata", "stratum", "sampling_strata", "region"))
  })
  output[[paste0(prefix, "_weights_ui")]] <- shiny::renderUI({
    d <- df_reactive(); shiny::req(d)
    expert_col_select(ns, paste0(prefix, "_weights"), "Poids de sondage", d,
                      c("poids_echantillon", "sampling_weight", "survey_weight",
                        "design_weight", "poids_pondere", "weights", "weight"))
  })
}

expert_design_args <- function(input, prefix) {
  g <- function(id) {
    v <- input[[paste0(prefix, "_", id)]]
    if (!is.null(v) && nzchar(v)) v else NULL
  }
  use <- isTRUE(input[[paste0(prefix, "_use_design")]])
  if (!use) return(list(cluster = NULL, strata = NULL, weights = NULL))
  list(cluster = g("cluster"), strata = g("strata"), weights = g("weights"))
}

# ── 11. Exports CSV / Excel mutualisés ──────────────────────────────────────
expert_exports_ui <- function(ns, prefix) {
  shiny::tagList(
    shiny::tags$div(class = "section-header", "Exporter les résultats"),
    shiny::tags$p(style = "font-size:12px;color:#555;",
                  "CSV = concaténation de tous les tableaux (colonne Table). ",
                  "Excel = un onglet par tableau."),
    shiny::downloadButton(ns(paste0(prefix, "_dl_csv")), "CSV (tous tableaux)",
                          style = "background:#0077B6;color:white;border:none;border-radius:8px;"),
    shiny::downloadButton(ns(paste0(prefix, "_dl_xlsx")), "Excel (xlsx)",
                          style = "background:#27ae60;color:white;border:none;border-radius:8px;"),
    shiny::br(), shiny::br(),
    shiny::uiOutput(ns(paste0(prefix, "_export_info")))
  )
}

expert_export_server <- function(input, output, session, prefix, tables) {
  clean_tables <- function() {
    tl <- tryCatch(tables(), error = function(e) NULL)
    if (is.null(tl)) return(list())
    tl[vapply(tl, function(x) !is.null(x) && nrow(as.data.frame(x)) > 0, logical(1))]
  }
  output[[paste0(prefix, "_dl_csv")]] <- shiny::downloadHandler(
    filename = function() paste0(prefix, "_", Sys.Date(), ".csv"),
    content = function(f) {
      tl <- clean_tables()
      shiny::req(length(tl) > 0)
      out <- dplyr::bind_rows(lapply(names(tl), function(nm) {
        d <- as.data.frame(tl[[nm]]); d$Table <- nm; d
      }))
      utils::write.csv(out, f, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output[[paste0(prefix, "_dl_xlsx")]] <- shiny::downloadHandler(
    filename = function() paste0(prefix, "_", Sys.Date(), ".xlsx"),
    content = function(f) {
      tl <- clean_tables(); shiny::req(length(tl) > 0)
      writexl::write_xlsx(tl, path = f)
    }
  )
  output[[paste0(prefix, "_export_info")]] <- shiny::renderUI({
    tl <- clean_tables()
    shiny::tags$div(style = "font-size:12px;color:#555;",
      shiny::tags$b(length(tl), " tableau(x) prêt(s) : "),
      paste(names(tl), collapse = ", "))
  })
}

# ── 12. Affichage DT homogène ───────────────────────────────────────────────
expert_dt <- function(tbl, page_length = 15) {
  DT::datatable(tbl, rownames = FALSE, filter = "top", class = "compact stripe hover",
                options = list(pageLength = page_length, dom = "ftp",
                               scrollX = TRUE, autoWidth = TRUE))
}

# ── 13. Petit graphique en barres avec IC (forest) ──────────────────────────
expert_prev_plot <- function(tbl, title = "Prévalences et IC 95 %") {
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  df <- tbl
  df$Label <- if ("Groupe" %in% names(df) && any(!is.na(df$Groupe)))
    paste0(df$Indicateur, " — ", df$Groupe) else df$Indicateur
  df$Label <- truncate_label(df$Label, 40)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = `Prév. (%)`, y = stats::reorder(Label, `Prév. (%)`))) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = `IC bas (%)`, xmax = `IC haut (%)`),
                           width = .25, color = "#0077B6", linewidth = .7,
                           orientation = "y") +
    ggplot2::geom_point(size = 2.8, color = "#C0392B") +
    ggplot2::labs(x = "Prévalence (%)", y = "", title = title) +
    ggplot2::theme_minimal(base_size = 11)
  plotly::ggplotly(p, tooltip = c("x", "y")) |>
    plotly::layout(margin = list(l = 10, r = 10))
}

cat("mod_expert_helpers.R chargé — IC, design, qualité, effets, exports\n")
