# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  inferentiel_v4.R — Module inférentiel unifié : TOUS les tests à la fois  ║
# ║  Sélectionner X + Y → Vérification conditions → Résultats + Conclusion    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Prérequis conditions communes ────────────────────────────────────────────
check_normality <- function(x, label = "X") {
  x_clean <- na.omit(x)
  n <- length(x_clean)
  if (n < 3 || n > 5000) x_clean <- x_clean[1:min(5000, n)]
  if (length(x_clean) < 3) {
    return(list(ok = FALSE, W = NA, p = NA,
                detail = paste0(label, " : n < 3, test impossible")))
  }
  sw <- tryCatch(shapiro.test(x_clean), error = function(e) NULL)
  if (is.null(sw)) {
    return(list(ok = FALSE, W = NA, p = NA,
                detail = paste0(label, " : erreur Shapiro-Wilk")))
  }
  list(
    ok     = sw$p.value >= 0.05,
    W      = round(sw$statistic, 4),
    p      = round(sw$p.value, 4),
    detail = paste0(label, " : Shapiro-Wilk W=", round(sw$statistic, 4),
                    " p=", round(sw$p.value, 4),
                    if (sw$p.value >= 0.05) " → Normale ✓" else " → Non normale ✗")
  )
}

check_homoscedasticity <- function(y, x) {
  p_val <- tryCatch({
    car::leveneTest(y ~ as.factor(x))$`Pr(>F)`[1]
  }, error = function(e) NA)
  if (is.na(p_val)) {
    return(list(ok = FALSE, p = NA,
                detail = "Test de Levene non applicable"))
  }
  list(
    ok     = p_val >= 0.05,
    p      = round(p_val, 4),
    detail = paste0("Levene p=", round(p_val, 4),
                    if (p_val >= 0.05) " → Variances homogènes ✓"
                    else " → Variances hétérogènes ✗")
  )
}

# ── Interprétation taille d'effet ────────────────────────────────────────────
interp_cramer <- function(v) {
  if (is.na(v)) return("N/A")
  if (v < 0.10) "Très faible (V<0.10)"
  else if (v < 0.20) "Faible (V<0.20)"
  else if (v < 0.40) "Modéré (V<0.40)"
  else "Fort (V≥0.40)"
}

interp_cohen_d <- function(d) {
  if (is.na(d)) return("N/A")
  if (d < 0.20) "Très faible (d<0.20)"
  else if (d < 0.50) "Faible (d<0.50)"
  else if (d < 0.80) "Moyen (d<0.80)"
  else "Fort (d≥0.80)"
}

interp_eta2 <- function(e) {
  if (is.na(e)) return("N/A")
  if (e < 0.01) "Très faible (η²<0.01)"
  else if (e < 0.06) "Faible (η²<0.06)"
  else if (e < 0.14) "Moyen (η²<0.14)"
  else "Fort (η²≥0.14)"
}

interp_eps2 <- function(e) {
  if (is.na(e)) return("N/A")
  if (e < 0.01) "Très faible"
  else if (e < 0.06) "Faible"
  else if (e < 0.14) "Moyen"
  else "Fort"
}

interp_cor <- function(r) {
  if (is.na(r)) return("N/A")
  ar <- abs(r)
  lbl <- if (ar < 0.10) "Négligeable"
         else if (ar < 0.30) "Faible"
         else if (ar < 0.50) "Modérée"
         else if (ar < 0.70) "Forte"
         else "Très forte"
  dir <- if (r > 0) "positive" else "négative"
  paste0(lbl, " (", dir, ")")
}

interp_r_rank <- function(r) {
  if (is.na(r)) return("N/A")
  if (r < 0.10) "Très faible"
  else if (r < 0.30) "Faible"
  else if (r < 0.50) "Moyen"
  else "Fort"
}

# ── Fonction principale : lancer TOUS les tests ─────────────────────────────
run_all_tests <- function(d, var_x, var_y, alpha = 0.05) {
  x <- d[[var_x]]; y <- d[[var_y]]
  tp_x <- var_type(x); tp_y <- var_type(y)
  tests <- list()

  # Déterminer la combinaison
  is_cat_x <- tp_x %in% c("binaire", "cat_peu", "cat_many")
  is_cat_y <- tp_y %in% c("binaire", "cat_peu", "cat_many")
  is_num_x <- tp_x %in% c("continue", "discrete")
  is_num_y <- tp_y %in% c("continue", "discrete")

  # Si Quanti × Quali, inverser pour avoir Quali × Quanti
  if (is_num_x && is_cat_y) {
    tmp <- var_x; var_x <- var_y; var_y <- tmp
    x <- d[[var_x]]; y <- d[[var_y]]
    tp_x <- var_type(x); tp_y <- var_type(y)
    is_cat_x <- TRUE; is_cat_y <- FALSE; is_num_x <- FALSE; is_num_y <- TRUE
  }

  n_grp <- length(unique(na.omit(x)))

  # ╔═══════════════════════════════════════════════════════════════════════╗
  # ║  CAS 1 : QUALI × QUALI                                             ║
  # ╚═══════════════════════════════════════════════════════════════════════╝
  if (is_cat_x && is_cat_y) {
    tbl <- table(x, y)
    niv_x <- nrow(tbl); niv_y <- ncol(tbl)
    n_tot <- sum(tbl)

    # Chi² simulé pour V de Cramér dans tous les cas
    chi2_sim <- tryCatch(chisq.test(tbl, correct = FALSE), error = function(e) NULL)
    min_exp  <- if (!is.null(chi2_sim)) min(chi2_sim$expected) else 0
    cramer_v <- if (!is.null(chi2_sim))
      round(sqrt(as.numeric(chi2_sim$statistic) / (n_tot * (min(niv_x, niv_y) - 1))), 4)
    else NA

    # ── Test 1 : Chi² de Pearson ──
    chi2_conds <- list(
      list(nom = "Type de variables", ok = TRUE,
           detail = "Deux variables qualitatives ✓"),
      list(nom = "Effectifs attendus ≥ 5", ok = (min_exp >= 5),
           detail = paste0("Min. attendu = ", round(min_exp, 1),
                           if (min_exp >= 5) " ≥ 5 ✓" else " < 5 ✗")),
      list(nom = "Indépendance des observations", ok = TRUE,
           detail = "Supposée par le plan d'enquête ✓")
    )
    chi2_valide <- all(sapply(chi2_conds, \(c) c$ok))

    if (chi2_valide && !is.null(chi2_sim)) {
      tests[["chi2"]] <- list(
        nom        = "Test du Chi² de Pearson",
        h0         = paste0("H₀ : Indépendance entre « ", var_x, " » et « ", var_y, " »"),
        h1         = paste0("H₁ : Association significative entre « ", var_x, " » et « ", var_y, " »"),
        conditions = chi2_conds,
        valide     = TRUE,
        stat       = round(as.numeric(chi2_sim$statistic), 3),
        stat_label = "χ²",
        df         = chi2_sim$parameter,
        p_val      = round(chi2_sim$p.value, 4),
        ci         = NULL,
        effet_nom  = "V de Cramér",
        effet_val  = cramer_v,
        effet_interp = interp_cramer(cramer_v),
        conclusion = if (chi2_sim$p.value < alpha)
          paste0("Rejet de H₀ (p=", round(chi2_sim$p.value, 4), " < α=", alpha,
                 ") — Association significative (V=", cramer_v, ")")
        else
          paste0("Non rejet de H₀ (p=", round(chi2_sim$p.value, 4), " ≥ α=", alpha,
                 ") — Pas d'association significative"),
        sig = chi2_sim$p.value < alpha
      )
    } else {
      tests[["chi2"]] <- list(
        nom        = "Test du Chi² de Pearson",
        conditions = chi2_conds,
        valide     = FALSE,
        raison     = "Effectifs attendus < 5 — Chi² non fiable"
      )
    }

    # ── Test 2 : Fisher exact ──
    fisher_res <- tryCatch(
      fisher.test(tbl, simulate.p.value = TRUE, B = 10000),
      error = function(e) NULL
    )
    fisher_conds <- list(
      list(nom = "Type de variables", ok = TRUE,
           detail = "Deux variables qualitatives ✓"),
      list(nom = "Applicabilité", ok = !is.null(fisher_res),
           detail = if (!is.null(fisher_res)) "Test exact toujours applicable ✓"
                    else "Erreur de calcul ✗"),
      list(nom = "Indépendance des observations", ok = TRUE,
           detail = "Supposée par le plan d'enquête ✓")
    )

    if (!is.null(fisher_res)) {
      tests[["fisher"]] <- list(
        nom        = "Test exact de Fisher",
        h0         = paste0("H₀ : Indépendance entre « ", var_x, " » et « ", var_y, " »"),
        h1         = paste0("H₁ : Association significative entre « ", var_x, " » et « ", var_y, " »"),
        conditions = fisher_conds,
        valide     = TRUE,
        stat       = "—",
        stat_label = "—",
        df         = "—",
        p_val      = round(fisher_res$p.value, 4),
        ci         = NULL,
        effet_nom  = "V de Cramér",
        effet_val  = cramer_v,
        effet_interp = interp_cramer(cramer_v),
        conclusion = if (fisher_res$p.value < alpha)
          paste0("Rejet de H₀ (p=", round(fisher_res$p.value, 4), " < α=", alpha,
                 ") — Association significative (V=", cramer_v, ")")
        else
          paste0("Non rejet de H₀ (p=", round(fisher_res$p.value, 4), " ≥ α=", alpha,
                 ") — Pas d'association significative"),
        sig = fisher_res$p.value < alpha
      )
    }

  # ╔═══════════════════════════════════════════════════════════════════════╗
  # ║  CAS 2 : QUALI (2 groupes) × QUANTI                                ║
  # ╚═══════════════════════════════════════════════════════════════════════╝
  } else if (is_cat_x && is_num_y && n_grp == 2) {
    grps <- split(y, x)
    grps <- grps[sapply(grps, function(g) sum(!is.na(g)) >= 2)]
    if (length(grps) < 2) return(tests)
    g1 <- na.omit(grps[[1]]); g2 <- na.omit(grps[[2]])
    n1 <- length(g1); n2 <- length(g2)
    grp_names <- names(grps)

    # Pré-tests
    norm_y <- check_normality(y, var_y)
    homosc <- check_homoscedasticity(y, x)

    # Cohen d helper
    calc_cohen_d <- function(g1, g2) {
      n1 <- length(g1); n2 <- length(g2)
      sp <- sqrt(((n1 - 1) * var(g1) + (n2 - 1) * var(g2)) / (n1 + n2 - 2))
      if (sp == 0) return(NA)
      abs(mean(g1) - mean(g2)) / sp
    }

    # ── Test 1 : t-test Student (var.equal = TRUE) ──
    student_conds <- list(
      list(nom = "Normalité", ok = norm_y$ok, detail = norm_y$detail),
      list(nom = "Homoscédasticité", ok = homosc$ok, detail = homosc$detail),
      list(nom = "Indépendance", ok = TRUE, detail = "Groupes indépendants ✓"),
      list(nom = "2 groupes", ok = TRUE, detail = paste0(n_grp, " groupes : ",
           paste(grp_names, collapse = " vs "), " ✓"))
    )
    student_valide <- norm_y$ok && homosc$ok

    if (student_valide) {
      res <- tryCatch(t.test(g1, g2, var.equal = TRUE), error = function(e) NULL)
      if (!is.null(res)) {
        cd <- calc_cohen_d(g1, g2)
        tests[["student"]] <- list(
          nom = "t-test de Student (variances égales)",
          h0 = paste0("H₀ : μ₁ = μ₂ (moyennes de « ", var_y, " » égales dans les 2 groupes)"),
          h1 = paste0("H₁ : μ₁ ≠ μ₂ (différence des moyennes)"),
          conditions = student_conds, valide = TRUE,
          stat = round(res$statistic, 3), stat_label = "t",
          df = round(res$parameter, 1), p_val = round(res$p.value, 4),
          ci = paste0("[", round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]"),
          effet_nom = "Cohen d", effet_val = round(cd, 4),
          effet_interp = interp_cohen_d(cd),
          conclusion = if (res$p.value < alpha)
            paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                   ") — Différence significative (d=", round(cd, 3), ")")
          else
            paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                   ") — Pas de différence significative"),
          sig = res$p.value < alpha
        )
      }
    } else {
      tests[["student"]] <- list(
        nom = "t-test de Student (variances égales)",
        conditions = student_conds, valide = FALSE,
        raison = paste0(
          if (!norm_y$ok) "Distribution non normale. " else "",
          if (!homosc$ok) "Variances non homogènes." else ""
        )
      )
    }

    # ── Test 2 : t-test Welch (var.equal = FALSE) ──
    welch_conds <- list(
      list(nom = "Normalité", ok = norm_y$ok, detail = norm_y$detail),
      list(nom = "Homoscédasticité", ok = TRUE,
           detail = "Welch ne requiert PAS l'homoscédasticité ✓"),
      list(nom = "Indépendance", ok = TRUE, detail = "Groupes indépendants ✓")
    )
    welch_valide <- norm_y$ok  # Welch ne nécessite que la normalité

    if (welch_valide) {
      res <- tryCatch(t.test(g1, g2, var.equal = FALSE), error = function(e) NULL)
      if (!is.null(res)) {
        cd <- calc_cohen_d(g1, g2)
        tests[["welch"]] <- list(
          nom = "t-test de Welch (variances inégales)",
          h0 = paste0("H₀ : μ₁ = μ₂ (moyennes de « ", var_y, " » égales)"),
          h1 = paste0("H₁ : μ₁ ≠ μ₂ (différence des moyennes)"),
          conditions = welch_conds, valide = TRUE,
          stat = round(res$statistic, 3), stat_label = "t",
          df = round(res$parameter, 1), p_val = round(res$p.value, 4),
          ci = paste0("[", round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]"),
          effet_nom = "Cohen d", effet_val = round(cd, 4),
          effet_interp = interp_cohen_d(cd),
          conclusion = if (res$p.value < alpha)
            paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                   ") — Différence significative (d=", round(cd, 3), ")")
          else
            paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                   ") — Pas de différence significative"),
          sig = res$p.value < alpha
        )
      }
    } else {
      tests[["welch"]] <- list(
        nom = "t-test de Welch (variances inégales)",
        conditions = welch_conds, valide = FALSE,
        raison = "Distribution non normale — test paramétrique non recommandé"
      )
    }

    # ── Test 3 : Mann-Whitney-Wilcoxon (toujours applicable) ──
    mw_conds <- list(
      list(nom = "Type", ok = TRUE, detail = "Variable continue + 2 groupes ✓"),
      list(nom = "Indépendance", ok = TRUE, detail = "Observations indépendantes ✓"),
      list(nom = "Non paramétrique", ok = TRUE,
           detail = "Aucune hypothèse de normalité requise ✓")
    )
    res <- tryCatch(wilcox.test(g1, g2, exact = FALSE, conf.int = TRUE),
                    error = function(e) NULL)
    if (!is.null(res)) {
      n_tot <- n1 + n2
      r_eff <- tryCatch(abs(qnorm(res$p.value / 2)) / sqrt(n_tot),
                         error = function(e) NA)
      tests[["mannwhitney"]] <- list(
        nom = "Test de Mann-Whitney-Wilcoxon",
        h0 = paste0("H₀ : Distributions identiques de « ", var_y, " » dans les 2 groupes"),
        h1 = paste0("H₁ : Distributions (ou médianes) différentes"),
        conditions = mw_conds, valide = TRUE,
        stat = round(res$statistic, 3), stat_label = "W",
        df = "—", p_val = round(res$p.value, 4),
        ci = if (!is.null(res$conf.int))
          paste0("[", round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
        else NULL,
        effet_nom = "r de rang", effet_val = round(r_eff, 4),
        effet_interp = interp_r_rank(r_eff),
        conclusion = if (res$p.value < alpha)
          paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                 ") — Différence significative entre les groupes")
        else
          paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                 ") — Pas de différence significative"),
        sig = res$p.value < alpha
      )
    }

  # ╔═══════════════════════════════════════════════════════════════════════╗
  # ║  CAS 3 : QUALI (k ≥ 3 groupes) × QUANTI                           ║
  # ╚═══════════════════════════════════════════════════════════════════════╝
  } else if (is_cat_x && is_num_y && n_grp >= 3) {
    norm_y <- check_normality(y, var_y)
    homosc <- check_homoscedasticity(y, x)

    # ── Test 1 : ANOVA classique ──
    anova_conds <- list(
      list(nom = "Normalité", ok = norm_y$ok, detail = norm_y$detail),
      list(nom = "Homoscédasticité", ok = homosc$ok, detail = homosc$detail),
      list(nom = "Indépendance", ok = TRUE, detail = "Observations indépendantes ✓"),
      list(nom = "k ≥ 3 groupes", ok = TRUE, detail = paste0(n_grp, " groupes ✓"))
    )
    anova_valide <- norm_y$ok && homosc$ok

    if (anova_valide) {
      res <- tryCatch({
        mod <- aov(y ~ as.factor(x))
        s   <- summary(mod)[[1]]
        list(mod = mod, s = s)
      }, error = function(e) NULL)

      if (!is.null(res)) {
        s <- res$s
        eta2 <- round(s[1, "Sum Sq"] / sum(s[, "Sum Sq"]), 4)
        f_val <- round(s[1, "F value"], 3)
        p_val <- s[1, "Pr(>F)"]
        df_str <- paste0(s[1, "Df"], ";", s[2, "Df"])

        # Post-hoc Tukey si significatif
        posthoc_txt <- NULL
        if (p_val < alpha) {
          tk <- tryCatch({
            TukeyHSD(res$mod)[[1]]
          }, error = function(e) NULL)
          if (!is.null(tk)) {
            sig_pairs <- which(tk[, "p adj"] < alpha)
            if (length(sig_pairs) > 0) {
              posthoc_txt <- paste0("Post-hoc Tukey HSD : ",
                paste(rownames(tk)[sig_pairs], " (p=",
                      round(tk[sig_pairs, "p adj"], 4), ")",
                      collapse = " | "))
            } else {
              posthoc_txt <- "Post-hoc Tukey HSD : Aucune paire significative"
            }
          }
        }

        tests[["anova"]] <- list(
          nom = "ANOVA à un facteur (Fisher)",
          h0 = paste0("H₀ : μ₁ = μ₂ = ... = μₖ (toutes les moyennes égales)"),
          h1 = paste0("H₁ : Au moins une moyenne diffère pour « ", var_y, " »"),
          conditions = anova_conds, valide = TRUE,
          stat = f_val, stat_label = "F",
          df = df_str, p_val = round(p_val, 4), ci = NULL,
          effet_nom = "η² (eta carré)", effet_val = eta2,
          effet_interp = interp_eta2(eta2),
          posthoc = posthoc_txt,
          conclusion = if (p_val < alpha)
            paste0("Rejet de H₀ (p=", round(p_val, 4), " < α=", alpha,
                   ") — Au moins un groupe diffère (η²=", eta2, ")")
          else
            paste0("Non rejet de H₀ (p=", round(p_val, 4), " ≥ α=", alpha,
                   ") — Pas de différence entre les groupes"),
          sig = p_val < alpha
        )
      }
    } else {
      tests[["anova"]] <- list(
        nom = "ANOVA à un facteur (Fisher)",
        conditions = anova_conds, valide = FALSE,
        raison = paste0(
          if (!norm_y$ok) "Distribution non normale. " else "",
          if (!homosc$ok) "Variances non homogènes." else ""
        )
      )
    }

    # ── Test 2 : ANOVA de Welch ──
    welch_anova_conds <- list(
      list(nom = "Normalité", ok = norm_y$ok, detail = norm_y$detail),
      list(nom = "Homoscédasticité", ok = TRUE,
           detail = "Welch ANOVA ne requiert PAS l'homoscédasticité ✓"),
      list(nom = "Indépendance", ok = TRUE, detail = "Observations indépendantes ✓")
    )
    welch_anova_valide <- norm_y$ok

    if (welch_anova_valide) {
      res <- tryCatch(oneway.test(y ~ as.factor(x)), error = function(e) NULL)
      if (!is.null(res)) {
        # Calculer eta² approximatif via somme des carrés
        eta2_approx <- tryCatch({
          mod <- aov(y ~ as.factor(x))
          s <- summary(mod)[[1]]
          round(s[1, "Sum Sq"] / sum(s[, "Sum Sq"]), 4)
        }, error = function(e) NA)

        tests[["welch_anova"]] <- list(
          nom = "ANOVA de Welch (variances inégales)",
          h0 = "H₀ : Toutes les moyennes sont égales",
          h1 = paste0("H₁ : Au moins une moyenne diffère pour « ", var_y, " »"),
          conditions = welch_anova_conds, valide = TRUE,
          stat = round(res$statistic, 3), stat_label = "F",
          df = paste0(round(res$parameter[1]), ";", round(res$parameter[2], 1)),
          p_val = round(res$p.value, 4), ci = NULL,
          effet_nom = "η² (approx.)", effet_val = eta2_approx,
          effet_interp = interp_eta2(eta2_approx),
          conclusion = if (res$p.value < alpha)
            paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                   ") — Différence significative entre les groupes")
          else
            paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                   ") — Pas de différence entre les groupes"),
          sig = res$p.value < alpha
        )
      }
    } else {
      tests[["welch_anova"]] <- list(
        nom = "ANOVA de Welch (variances inégales)",
        conditions = welch_anova_conds, valide = FALSE,
        raison = "Distribution non normale — test paramétrique non recommandé"
      )
    }

    # ── Test 3 : Kruskal-Wallis (toujours applicable) ──
    kw_conds <- list(
      list(nom = "Type", ok = TRUE, detail = "Variable continue + ≥ 3 groupes ✓"),
      list(nom = "Indépendance", ok = TRUE, detail = "Observations indépendantes ✓"),
      list(nom = "Non paramétrique", ok = TRUE,
           detail = "Aucune hypothèse de normalité ou homoscédasticité ✓")
    )
    res <- tryCatch(kruskal.test(y ~ as.factor(x)), error = function(e) NULL)
    if (!is.null(res)) {
      n_tot <- sum(!is.na(y)); k <- n_grp
      eps2 <- round(max(0, (res$statistic - k + 1) / (n_tot - k)), 4)

      # Post-hoc Dunn si significatif
      posthoc_txt <- NULL
      if (res$p.value < alpha) {
        ph <- tryCatch({
          pw <- pairwise.wilcox.test(y, x, p.adjust.method = "BH", exact = FALSE)
          mat <- pw$p.value
          sig_idx <- which(mat < alpha, arr.ind = TRUE)
          if (nrow(sig_idx) > 0) {
            pairs <- apply(sig_idx, 1, function(idx) {
              paste0(rownames(mat)[idx[1]], " vs ", colnames(mat)[idx[2]],
                     " (p=", round(mat[idx[1], idx[2]], 4), ")")
            })
            paste0("Post-hoc Dunn (BH) : ", paste(unique(pairs), collapse = " | "))
          } else {
            "Post-hoc Dunn (BH) : Aucune paire significative"
          }
        }, error = function(e) NULL)
        posthoc_txt <- ph
      }

      tests[["kruskal"]] <- list(
        nom = "Test de Kruskal-Wallis",
        h0 = paste0("H₀ : Distributions identiques pour tous les niveaux de « ", var_x, " »"),
        h1 = paste0("H₁ : Au moins un groupe diffère pour « ", var_y, " »"),
        conditions = kw_conds, valide = TRUE,
        stat = round(res$statistic, 3), stat_label = "H",
        df = res$parameter, p_val = round(res$p.value, 4), ci = NULL,
        effet_nom = "ε² de Kelley", effet_val = eps2,
        effet_interp = interp_eps2(eps2),
        posthoc = posthoc_txt,
        conclusion = if (res$p.value < alpha)
          paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                 ") — Au moins un groupe diffère (ε²=", eps2, ")")
        else
          paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                 ") — Pas de différence entre les groupes"),
        sig = res$p.value < alpha
      )
    }

  # ╔═══════════════════════════════════════════════════════════════════════╗
  # ║  CAS 4 : QUANTI × QUANTI                                           ║
  # ╚═══════════════════════════════════════════════════════════════════════╝
  } else if (is_num_x && is_num_y) {
    norm_x <- check_normality(x, var_x)
    norm_y <- check_normality(y, var_y)

    # ── Test 1 : Pearson ──
    pearson_conds <- list(
      list(nom = "Normalité X", ok = norm_x$ok, detail = norm_x$detail),
      list(nom = "Normalité Y", ok = norm_y$ok, detail = norm_y$detail),
      list(nom = "Relation linéaire", ok = TRUE,
           detail = "Vérifier visuellement sur le nuage de points")
    )
    pearson_valide <- norm_x$ok && norm_y$ok

    if (pearson_valide) {
      res <- tryCatch(cor.test(x, y, method = "pearson"), error = function(e) NULL)
      if (!is.null(res)) {
        tests[["pearson"]] <- list(
          nom = "Corrélation de Pearson",
          h0 = paste0("H₀ : r = 0 (pas de corrélation linéaire)"),
          h1 = paste0("H₁ : r ≠ 0 (corrélation linéaire significative)"),
          conditions = pearson_conds, valide = TRUE,
          stat = round(res$statistic, 3), stat_label = "t",
          df = round(res$parameter, 0), p_val = round(res$p.value, 4),
          ci = if (!is.null(res$conf.int))
            paste0("[", round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
          else NULL,
          effet_nom = "r de Pearson", effet_val = round(res$estimate, 4),
          effet_interp = interp_cor(res$estimate),
          conclusion = if (res$p.value < alpha)
            paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                   ") — Corrélation significative (r=", round(res$estimate, 3), ")")
          else
            paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                   ") — Pas de corrélation linéaire significative"),
          sig = res$p.value < alpha
        )
      }
    } else {
      tests[["pearson"]] <- list(
        nom = "Corrélation de Pearson",
        conditions = pearson_conds, valide = FALSE,
        raison = paste0(
          if (!norm_x$ok) paste0(var_x, " non normale. ") else "",
          if (!norm_y$ok) paste0(var_y, " non normale.") else ""
        )
      )
    }

    # ── Test 2 : Spearman ──
    spearman_conds <- list(
      list(nom = "Type", ok = TRUE, detail = "Deux variables quantitatives ✓"),
      list(nom = "Relation monotone", ok = TRUE,
           detail = "Mesure l'association monotone (pas nécessairement linéaire) ✓"),
      list(nom = "Non paramétrique", ok = TRUE,
           detail = "Aucune hypothèse de normalité requise ✓")
    )
    res <- tryCatch(cor.test(x, y, method = "spearman", exact = FALSE),
                    error = function(e) NULL)
    if (!is.null(res)) {
      tests[["spearman"]] <- list(
        nom = "Corrélation de Spearman",
        h0 = paste0("H₀ : ρ = 0 (pas d'association monotone)"),
        h1 = paste0("H₁ : ρ ≠ 0 (association monotone significative)"),
        conditions = spearman_conds, valide = TRUE,
        stat = round(res$statistic, 3), stat_label = "S",
        df = "—", p_val = round(res$p.value, 4), ci = NULL,
        effet_nom = "ρ de Spearman", effet_val = round(res$estimate, 4),
        effet_interp = interp_cor(res$estimate),
        conclusion = if (res$p.value < alpha)
          paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                 ") — Corrélation monotone significative (ρ=", round(res$estimate, 3), ")")
        else
          paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                 ") — Pas d'association monotone significative"),
        sig = res$p.value < alpha
      )
    }

    # ── Test 3 : Kendall ──
    kendall_conds <- list(
      list(nom = "Type", ok = TRUE, detail = "Deux variables quantitatives ✓"),
      list(nom = "Non paramétrique", ok = TRUE,
           detail = "Robuste aux ex-aequo et petits échantillons ✓")
    )
    res <- tryCatch(cor.test(x, y, method = "kendall", exact = FALSE),
                    error = function(e) NULL)
    if (!is.null(res)) {
      tests[["kendall"]] <- list(
        nom = "Corrélation de Kendall",
        h0 = paste0("H₀ : τ = 0 (pas de concordance)"),
        h1 = paste0("H₁ : τ ≠ 0 (concordance significative)"),
        conditions = kendall_conds, valide = TRUE,
        stat = round(res$statistic, 3), stat_label = "z",
        df = "—", p_val = round(res$p.value, 4), ci = NULL,
        effet_nom = "τ de Kendall", effet_val = round(res$estimate, 4),
        effet_interp = interp_cor(res$estimate),
        conclusion = if (res$p.value < alpha)
          paste0("Rejet de H₀ (p=", round(res$p.value, 4), " < α=", alpha,
                 ") — Concordance significative (τ=", round(res$estimate, 3), ")")
        else
          paste0("Non rejet de H₀ (p=", round(res$p.value, 4), " ≥ α=", alpha,
                 ") — Pas de concordance significative"),
        sig = res$p.value < alpha
      )
    }
  }

  tests
}

# ── Rendu HTML : Tableau récapitulatif ──────────────────────────────────────
render_summary_df <- function(tests) {
  if (length(tests) == 0) return(data.frame(Message = "Aucun test applicable"))
  map_dfr(tests, function(t) {
    tibble(
      Test     = t$nom,
      Valide   = if (isTRUE(t$valide)) "✅ Oui" else "❌ Non",
      Stat     = if (isTRUE(t$valide)) as.character(t$stat) else "—",
      ddl      = if (isTRUE(t$valide)) as.character(t$df) else "—",
      `p-valeur` = if (isTRUE(t$valide)) as.character(t$p_val) else "—",
      `Taille d'effet` = if (isTRUE(t$valide))
        paste0(t$effet_nom, " = ", t$effet_val) else "—",
      Décision = if (isTRUE(t$valide))
        (if (isTRUE(t$sig)) "🔴 Significatif" else "🟢 Non significatif")
      else "⬜ Non applicable"
    )
  })
}

# ── Rendu HTML : Carte détaillée d'un test ──────────────────────────────────
render_test_card <- function(t) {
  if (is.null(t)) return(NULL)

  valide <- isTRUE(t$valide)
  badge_col <- if (valide) "#27ae60" else "#e74c3c"
  badge_txt <- if (valide) "✅ CONDITIONS REMPLIES" else "❌ CONDITIONS NON REMPLIES"

  # Conditions HTML
  cond_html <- paste(sapply(t$conditions, function(c) {
    ico <- if (isTRUE(c$ok)) "✅" else "❌"
    paste0("<div style='display:flex;gap:8px;padding:5px 0;border-bottom:0.5px solid #eee;font-size:12px;'>",
           "<span style='min-width:20px;'>", ico, "</span>",
           "<div><b style='color:#333;'>", c$nom, "</b><br>",
           "<span style='color:#666;'>", c$detail, "</span></div></div>")
  }), collapse = "")

  # Début de la carte
  card <- tags$div(
    style = "border:1px solid #ddd;border-radius:10px;margin-bottom:16px;overflow:hidden;background:white;",

    # En-tête
    tags$div(
      style = paste0("background:linear-gradient(135deg,#f0f8ff,#e8f4fd);",
                      "padding:12px 16px;border-bottom:1px solid #ddd;",
                      "display:flex;justify-content:space-between;align-items:center;"),
      tags$h5(style = "margin:0;color:#00426A;font-size:14px;font-weight:700;", t$nom),
      tags$span(style = paste0("background:", badge_col, ";color:white;padding:3px 10px;",
                               "border-radius:12px;font-size:11px;font-weight:600;"),
                badge_txt)
    ),

    # Conditions
    tags$div(
      style = "padding:10px 16px;background:#fafafa;",
      tags$div(style = "font-size:11px;font-weight:700;color:#888;text-transform:uppercase;letter-spacing:.8px;margin-bottom:6px;",
               "CONDITIONS D'APPLICATION"),
      HTML(cond_html)
    )
  )

  if (!valide) {
    # Test non valide : afficher la raison
    card <- tagAppendChildren(card,
      tags$div(
        style = "padding:10px 16px;background:#fef2f2;border-top:1px solid #fdd;",
        tags$span(style = "font-size:12px;color:#b91c1c;font-weight:600;",
                  paste0("⚠ ", t$raison %||% "Conditions non remplies"))
      )
    )
  } else {
    # Test valide : hypothèses + résultats + conclusion
    sig <- isTRUE(t$sig)
    col <- if (sig) "#c9161d" else "#27ae60"

    card <- tagAppendChildren(card,
      # H0 / H1
      tags$div(
        style = "padding:10px 16px;border-top:1px solid #eee;",
        tags$div(style = "font-size:12px;color:#333;margin-bottom:3px;",
                 tags$b("H₀ : "), gsub("H₀ : ", "", t$h0)),
        tags$div(style = "font-size:12px;color:#333;",
                 tags$b("H₁ : "), gsub("H₁ : ", "", t$h1))
      ),

      # Résultats chiffrés
      tags$div(
        style = "padding:10px 16px;display:flex;gap:12px;flex-wrap:wrap;",

        tags$div(style = "flex:1;min-width:100px;text-align:center;background:#f8f9fa;border-radius:8px;padding:10px;",
          tags$div(style = paste0("font-size:22px;font-weight:700;color:", col, ";"), t$stat),
          tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;",
                   paste0(t$stat_label, " (ddl=", t$df, ")"))
        ),

        tags$div(style = "flex:1;min-width:100px;text-align:center;background:#f8f9fa;border-radius:8px;padding:10px;",
          tags$div(style = paste0("font-size:22px;font-weight:700;color:", col, ";"), t$p_val),
          tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "p-valeur")
        ),

        tags$div(style = "flex:2;min-width:150px;background:#f8f9fa;border-radius:8px;padding:10px;",
          tags$div(style = "font-size:10px;font-weight:700;color:#888;text-transform:uppercase;",
                   t$effet_nom),
          tags$div(style = paste0("font-size:18px;font-weight:700;color:", col, ";"),
                   t$effet_val),
          tags$div(style = "font-size:11px;color:#555;", t$effet_interp)
        )
      ),

      # IC 95% si disponible
      if (!is.null(t$ci)) {
        tags$div(
          style = "padding:4px 16px 8px;",
          tags$span(style = "font-size:12px;color:#555;",
                    tags$b("IC 95% : "), t$ci)
        )
      },

      # Post-hoc si disponible
      if (!is.null(t$posthoc)) {
        tags$div(
          style = "padding:6px 16px;background:#fffbeb;border-top:1px solid #fef3c7;",
          tags$span(style = "font-size:11px;color:#92400e;", t$posthoc)
        )
      },

      # Conclusion
      tags$div(
        style = paste0("padding:12px 16px;border-top:2px solid ", col, ";",
                       "background:", if (sig) "#fef2f2" else "#f0fdf4", ";"),
        tags$strong(style = paste0("color:", col, ";font-size:13px;"),
                    if (sig) "🔴 CONCLUSION" else "🟢 CONCLUSION"),
        tags$br(),
        tags$span(style = "font-size:12px;color:#333;", t$conclusion)
      )
    )
  }

  card
}

# ── Rendu HTML : Toutes les cartes ──────────────────────────────────────────
render_all_cards <- function(tests) {
  if (length(tests) == 0) {
    return(tags$div(
      style = "text-align:center;padding:40px;color:#888;",
      tags$h4("Aucun test applicable pour cette combinaison de variables")
    ))
  }
  tagList(lapply(tests, render_test_card))
}

# ── Graphique adapté au type de combinaison ─────────────────────────────────
make_infer_plot <- function(d, var_x, var_y, alpha = 0.05) {
  x <- d[[var_x]]; y <- d[[var_y]]
  tp_x <- var_type(x); tp_y <- var_type(y)
  is_cat_x <- tp_x %in% c("binaire", "cat_peu", "cat_many")
  is_cat_y <- tp_y %in% c("binaire", "cat_peu", "cat_many")
  is_num_x <- tp_x %in% c("continue", "discrete")
  is_num_y <- tp_y %in% c("continue", "discrete")

  # Inverser si Quanti × Quali
  if (is_num_x && is_cat_y) {
    tmp <- var_x; var_x <- var_y; var_y <- tmp
    is_cat_x <- TRUE; is_num_y <- TRUE
  }

  n_grp <- length(unique(na.omit(d[[var_x]])))

  tryCatch({
    # Quali × Quali → barplot empilé
    if (is_cat_x && is_cat_y) {
      cnt <- d |> count(.data[[var_x]], .data[[var_y]]) |>
        group_by(.data[[var_x]]) |> mutate(pct = round(n / sum(n) * 100, 1)) |> ungroup()
      niv <- length(unique(na.omit(d[[var_y]])))
      p <- ggplot(cnt, aes(x = .data[[var_x]], y = pct, fill = as.character(.data[[var_y]]),
                            text = paste0(.data[[var_x]], " | ", .data[[var_y]], "\n", pct, "%"))) +
        geom_col(position = position_fill(), alpha = .85) +
        geom_text(aes(label = paste0(pct, "%")),
                  position = position_fill(vjust = .5), size = 3, color = "white", fontface = "bold") +
        scale_fill_manual(values = get_pal(niv, "cat"), name = var_y) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = var_x, y = "Proportion", title = paste0(var_x, " × ", var_y)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1))
      ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.3))

    # Quali × Quanti → Violin + Boxplot
    } else if (is_cat_x && is_num_y) {
      niv <- length(unique(na.omit(d[[var_x]])))
      p <- ggplot(d, aes(x = .data[[var_x]], y = .data[[var_y]], fill = .data[[var_x]],
                          text = paste0(.data[[var_x]], "\n", var_y, "=", round(.data[[var_y]], 2)))) +
        geom_violin(alpha = .35, trim = FALSE, linewidth = .6) +
        geom_boxplot(fill = "white", alpha = .7, width = .2,
                     outlier.shape = 21, outlier.color = "#c9161d", outlier.size = 2) +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#c9161d") +
        stat_summary(fun.data = mean_se, geom = "errorbar", width = .15,
                     color = "#c9161d", linewidth = .8) +
        scale_fill_manual(values = get_pal(niv, "cat")) +
        labs(x = var_x, y = var_y, title = paste0(var_y, " par ", var_x)) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
      ggplotly(p, tooltip = "text")

    # Quanti × Quanti → Scatter
    } else if (is_num_x && is_num_y) {
      rho <- tryCatch(cor.test(d[[var_x]], d[[var_y]], method = "spearman", exact = FALSE),
                      error = function(e) NULL)
      p <- ggplot(d, aes(x = .data[[var_x]], y = .data[[var_y]],
                          text = paste0(var_x, "=", round(.data[[var_x]], 2),
                                        "\n", var_y, "=", round(.data[[var_y]], 2)))) +
        geom_point(alpha = .45, size = 1.8, color = "#008DC9") +
        geom_smooth(method = "lm", se = TRUE, color = "#c9161d", linewidth = 1,
                    fill = "#c9161d", alpha = .12) +
        geom_smooth(method = "loess", se = FALSE, color = "#4dac26",
                    linewidth = .8, linetype = "dashed") +
        labs(x = var_x, y = var_y,
             title = paste0(var_x, " vs ", var_y),
             subtitle = if (!is.null(rho))
               paste0("ρ=", round(rho$estimate, 3), "  p=", round(rho$p.value, 4))
             else "",
             caption = "Rouge=régression linéaire | Vert=LOESS") +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text")

    } else {
      plotly_empty() |> layout(title = list(text = "Type de variables non pris en charge", x = .5))
    }
  }, error = function(e)
    plotly_empty() |> layout(title = list(text = paste("Erreur graphique:", e$message), x = .5))
  )
}

# ── Badge type de combinaison ───────────────────────────────────────────────
detect_combination_badge <- function(var_x, var_y, d) {
  tp_x <- var_type(d[[var_x]]); tp_y <- var_type(d[[var_y]])
  is_cat_x <- tp_x %in% c("binaire", "cat_peu", "cat_many")
  is_cat_y <- tp_y %in% c("binaire", "cat_peu", "cat_many")
  is_num_x <- tp_x %in% c("continue", "discrete")
  is_num_y <- tp_y %in% c("continue", "discrete")

  if (is_cat_x && is_cat_y) {
    n_x <- length(unique(na.omit(d[[var_x]])))
    n_y <- length(unique(na.omit(d[[var_y]])))
    lbl <- paste0("Quali × Quali (", n_x, "×", n_y, " niveaux)")
    col <- "#8E44AD"
  } else if ((is_cat_x && is_num_y) || (is_num_x && is_cat_y)) {
    cat_var <- if (is_cat_x) var_x else var_y
    n_g <- length(unique(na.omit(d[[cat_var]])))
    lbl <- paste0("Quali × Quanti (", n_g, " groupes)")
    col <- "#2980B9"
  } else if (is_num_x && is_num_y) {
    lbl <- "Quanti × Quanti"
    col <- "#27AE60"
  } else {
    lbl <- paste0("Types : ", tp_x, " × ", tp_y)
    col <- "#7f8c8d"
  }

  tags$div(
    style = paste0("background:", col, ";color:white;padding:8px 14px;border-radius:8px;",
                   "text-align:center;font-weight:700;font-size:13px;margin:8px 0;"),
    lbl
  )
}

cat("inferentiel_v4.R chargé — Module unifié de tests statistiques\n")
