# module_predictif.R ── Modélisation prédictive expert (v2)
# ═══════════════════════════════════════════════════════════════════════════════
# Binaire   : GLM (logistique) | Random Forest | XGBoost | SVM (RBF) | k-NN
# Multiclasse : Multinomiale | Random Forest | XGBoost
# Métriques : AUC · PR-AUC · Accuracy · Balanced Accuracy · Sensibilité
#             Spécificité · VPP · VPN · F1 · MCC · Kappa · Youden · Brier · LogLoss
# Courbes   : ROC · PR · Gain/Lift · Calibration · Learning curves
# Importance: caret varImp · Permutation importance
# ═══════════════════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  for (pkg in c("pROC", "caret", "ROSE", "ResourceSelection", "nnet", "kernlab")) {
    tryCatch(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
             error = function(e) message("\u26a0\ufe0f ", pkg, " non disponible"))
  }
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ═══════════════════════════════════════════════════════════════════════════════
# 1. PIPELINE PRINCIPAL
# ═══════════════════════════════════════════════════════════════════════════════
run_predictive_pipeline <- function(d, outcome, predictors,
                                    k_folds   = 5,
                                    seed      = 2847,
                                    models    = c("glm", "rf", "xgb"),
                                    train_pct = 0.70,
                                    allow_parallel = TRUE) {
  set.seed(seed)

  d_ml <- d |>
    select(all_of(c(outcome, predictors))) |>
    drop_na() |>
    mutate(across(where(is.character), as.factor))

  y_vals    <- unique(na.omit(d_ml[[outcome]]))
  n_classes <- length(y_vals)
  if (n_classes < 2) stop("La variable cible doit avoir au moins 2 niveaux")
  is_binary <- (n_classes == 2)

  if (!is.factor(d_ml[[outcome]])) d_ml[[outcome]] <- factor(d_ml[[outcome]])

  if (is_binary) {
    d_ml[[outcome]] <- factor(d_ml[[outcome]], labels = c("Negatif", "Positif"))
    class_levels    <- c("Negatif", "Positif")
  } else {
    lvls  <- levels(d_ml[[outcome]])
    clean <- make.names(lvls)
    levels(d_ml[[outcome]]) <- clean
    class_levels <- clean
  }

  # Renommer la cible en .y pour eviter les conflits de scoping dans caret
  names(d_ml)[names(d_ml) == outcome] <- ".y"
  fml <- .y ~ .

  idx_train  <- createDataPartition(d_ml[[".y"]], p = train_pct, list = FALSE)
  train_data <- d_ml[idx_train, ]
  test_data  <- d_ml[-idx_train, ]

  ct              <- table(train_data[[".y"]])
  imbalance_ratio <- max(ct) / min(ct)

  train_bal <- if (is_binary && imbalance_ratio > 2) {
    tryCatch(ovun.sample(.y ~ ., data = train_data, method = "both", seed = seed)$data,
             error = function(e) train_data)
  } else train_data

  # Controle CV
  if (is_binary) {
    ctrl   <- trainControl(method = "cv", number = k_folds, classProbs = TRUE,
                           summaryFunction = twoClassSummary, savePredictions = "final")
    metric <- "ROC"
  } else {
    ctrl   <- trainControl(method = "cv", number = k_folds, classProbs = TRUE,
                           savePredictions = "final")
    metric <- "Accuracy"
  }

  # Parallélisation de la validation croisée (aucun impact sur les résultats)
  .par_registered <- function() {
    isTRUE(foreach::getDoParRegistered()) &&
      !identical(foreach::getDoParName(), "doSEQ")
  }
  if (isTRUE(allow_parallel) && nrow(train_bal) >= 2500) {
    if (.par_registered()) {
      ctrl$allowParallel <- TRUE
    } else {
      nc <- max(1L, parallel::detectCores() - 1L)
      if (nc >= 2) {
        .cl <- parallel::makeCluster(nc, rscript_args = c("--vanilla"))
        parallel::clusterCall(.cl, function(paths) { .libPaths(paths); NULL }, .libPaths())
        doParallel::registerDoParallel(.cl)
        on.exit({
          try(parallel::stopCluster(.cl), silent = TRUE)
          foreach::registerDoSEQ()
        }, add = TRUE)
        ctrl$allowParallel <- TRUE
      }
    }
  }

  # Grilles de tuning
  rf_grid  <- expand.grid(
    mtry = unique(pmax(1L, c(2L, floor(sqrt(length(predictors))),
                              floor(length(predictors) / 3L))))
  )
  xgb_grid <- expand.grid(
    nrounds = c(100, 200), max_depth = c(3, 6), eta = c(0.1),
    gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8
  )
  svm_grid <- expand.grid(sigma = c(0.02, 0.05), C = c(1, 5))
  knn_grid <- expand.grid(k = c(3, 5, 7))

  results <- list()

  # Helper : evaluation sur le jeu de test
  .eval <- function(fit) {
    if (is.null(fit)) return(NULL)
    pc <- tryCatch(predict(fit, test_data), error = function(e) NULL)
    if (is.null(pc)) return(NULL)
    if (is_binary) {
      pp <- tryCatch(predict(fit, test_data, type = "prob")[, "Positif"],
                     error = function(e) NULL)
      ro <- tryCatch(roc(test_data[[".y"]], pp,
                         levels = c("Negatif", "Positif"), quiet = TRUE),
                     error = function(e) NULL)
      cm <- tryCatch(confusionMatrix(pc, test_data[[".y"]], positive = "Positif"),
                     error = function(e) NULL)
      list(fit = fit, roc = ro, cm = cm, pred_prob = pp, pred_class = pc,
           auc = if (!is.null(ro)) as.numeric(auc(ro)) else NA_real_)
    } else {
      pp <- tryCatch(predict(fit, test_data, type = "prob"), error = function(e) NULL)
      cm <- tryCatch(confusionMatrix(pc, test_data[[".y"]]), error = function(e) NULL)
      ro <- tryCatch(multiclass.roc(test_data[[".y"]], pp, quiet = TRUE),
                     error = function(e) NULL)
      list(fit = fit, roc = ro, cm = cm, pred_prob = pp, pred_class = pc,
           auc = if (!is.null(ro)) as.numeric(ro$auc) else NA_real_)
    }
  }

  # --- Regression logistique / Multinomiale ---
  if ("glm" %in% models) {
    if (is_binary) {
      fit <- tryCatch(train(fml, data = train_bal, method = "glm", family = "binomial",
                            trControl = ctrl, metric = metric), error = function(e) NULL)
    } else {
      fit <- tryCatch(train(fml, data = train_bal, method = "multinom",
                            trace = FALSE, MaxNWts = 5000,
                            trControl = ctrl, metric = metric), error = function(e) NULL)
    }
    ev <- .eval(fit)
    if (!is.null(ev)) results[[if (is_binary) "Logistique" else "Multinomiale"]] <- ev
  }

  # --- Random Forest ---
  if ("rf" %in% models) {
    fit <- tryCatch(train(fml, data = train_bal, method = "rf", ntree = 200,
                          trControl = ctrl, tuneGrid = rf_grid, metric = metric),
                    error = function(e) NULL)
    ev <- .eval(fit)
    if (!is.null(ev)) results[["Random Forest"]] <- ev
  }

  # --- XGBoost ---
  if ("xgb" %in% models) {
    fit <- tryCatch(train(fml, data = train_bal, method = "xgbTree", verbosity = 0,
                          trControl = ctrl, tuneGrid = xgb_grid, metric = metric),
                    error = function(e) NULL)
    ev <- .eval(fit)
    if (!is.null(ev)) results[["XGBoost"]] <- ev
  }

  # --- SVM (noyau RBF) ---
  if ("svm" %in% models) {
    fit <- tryCatch(train(fml, data = train_bal, method = "svmRadial",
                          preProcess = c("center", "scale"),
                          trControl = ctrl, tuneGrid = svm_grid, metric = metric),
                    error = function(e) NULL)
    ev <- .eval(fit)
    if (!is.null(ev)) results[["SVM (RBF)"]] <- ev
  }

  # --- k-NN ---
  if ("knn" %in% models) {
    fit <- tryCatch(train(fml, data = train_bal, method = "knn",
                          preProcess = c("center", "scale"),
                          trControl = ctrl, tuneGrid = knn_grid, metric = metric),
                    error = function(e) NULL)
    ev <- .eval(fit)
    if (!is.null(ev)) results[["k-NN"]] <- ev
  }

  .codes_map <- c("Logistique" = "glm", "Multinomiale" = "glm",
                  "Random Forest" = "rf", "XGBoost" = "xgb",
                  "SVM (RBF)" = "svm", "k-NN" = "knn")
  list(
    results         = results,
    model_codes     = unname(.codes_map[names(results)]),
    train_data      = train_bal,
    test_data       = test_data,
    outcome         = outcome,
    outcome_col     = ".y",
    predictors      = predictors,
    is_binary       = is_binary,
    n_classes       = n_classes,
    class_levels    = class_levels,
    original_levels = as.character(y_vals),
    imbalance_ratio = round(imbalance_ratio, 2),
    n_train         = nrow(train_bal),
    n_test          = nrow(test_data),
    k_folds         = k_folds
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# 2. HELPERS PR (Precision-Rappel)
# ═══════════════════════════════════════════════════════════════════════════════
.pr_points <- function(obs, pred) {
  ord   <- order(pred, decreasing = TRUE)
  obs_s <- obs[ord]
  n_pos <- sum(obs)
  if (n_pos == 0) return(tibble(precision = numeric(0), recall = numeric(0)))
  tp <- cumsum(obs_s)
  fp <- cumsum(1 - obs_s)
  tibble(precision = tp / (tp + fp), recall = tp / n_pos)
}

.pr_auc <- function(pts) {
  if (is.null(pts) || nrow(pts) < 2) return(NA_real_)
  ord <- order(pts$recall)
  r <- pts$recall[ord]; p <- pts$precision[ord]
  uid <- !duplicated(r); r <- r[uid]; p <- p[uid]
  if (length(r) < 2) return(NA_real_)
  sum(diff(r) * (head(p, -1) + tail(p, -1)) / 2, na.rm = TRUE)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 3. METRIQUES (15 binaire · 9 multiclasse)
# ═══════════════════════════════════════════════════════════════════════════════
metrics_table <- function(pipeline_res) {
  res <- pipeline_res$results
  if (length(res) == 0) return(data.frame())
  oc <- pipeline_res$outcome_col %||% ".y"

  if (pipeline_res$is_binary) {
    purrr::map_dfr(names(res), function(nm) {
      r  <- res[[nm]]; cm <- r$cm
      if (is.null(cm)) return(tibble(Modele = nm))
      tb <- tryCatch(cm$table, error = function(e) NULL)
      if (is.null(tb)) return(tibble(Modele = nm))

      TP <- unname(tryCatch(tb["Positif", "Positif"], error = function(e) NA_real_))
      TN <- unname(tryCatch(tb["Negatif", "Negatif"], error = function(e) NA_real_))
      FP <- unname(tryCatch(tb["Positif", "Negatif"], error = function(e) NA_real_))
      FN <- unname(tryCatch(tb["Negatif", "Positif"], error = function(e) NA_real_))

      sens <- unname(cm$byClass["Sensitivity"])
      spec <- unname(cm$byClass["Specificity"])
      ppv  <- unname(cm$byClass["Pos Pred Value"])
      npv  <- unname(cm$byClass["Neg Pred Value"])
      f1   <- unname(cm$byClass["F1"])
      acc  <- unname(cm$overall["Accuracy"])
      kap  <- unname(cm$overall["Kappa"])

      # Matthews Correlation Coefficient
      denom_mcc <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
      mcc <- if (!is.na(denom_mcc) && denom_mcc > 0) (TP * TN - FP * FN) / denom_mcc
             else NA_real_

      # Brier Score + Log Loss
      obs   <- as.numeric(pipeline_res$test_data[[oc]]) - 1
      pp    <- r$pred_prob
      brier <- if (!is.null(pp)) mean((pp - obs)^2, na.rm = TRUE) else NA_real_
      eps   <- 1e-15
      pp_c  <- if (!is.null(pp)) pmax(pmin(pp, 1 - eps), eps) else NULL
      ll    <- if (!is.null(pp_c))
                 -mean(obs * log(pp_c) + (1 - obs) * log(1 - pp_c), na.rm = TRUE)
               else NA_real_

      # PR-AUC
      pts  <- if (!is.null(pp)) .pr_points(obs, pp) else NULL
      pr_a <- .pr_auc(pts)

      tibble(
        Modele       = nm,
        AUC          = round(r$auc %||% NA_real_, 4),
        PR_AUC       = round(pr_a, 4),
        Accuracy     = round(acc, 4),
        Balanced_Acc = round((sens + spec) / 2, 4),
        Sensibilite  = round(sens, 4),
        Specificite  = round(spec, 4),
        VPP          = round(ppv, 4),
        VPN          = round(npv, 4),
        F1           = round(f1, 4),
        MCC          = round(mcc, 4),
        Kappa        = round(kap, 4),
        Youden       = round(sens + spec - 1, 4),
        Brier        = round(brier, 4),
        LogLoss      = round(ll, 4)
      )
    })
  } else {
    purrr::map_dfr(names(res), function(nm) {
      r  <- res[[nm]]; cm <- r$cm
      if (is.null(cm)) return(tibble(Modele = nm))
      bc <- if (!is.null(cm$byClass) && is.matrix(cm$byClass))
              colMeans(cm$byClass, na.rm = TRUE)
            else if (!is.null(cm$byClass)) cm$byClass
            else setNames(rep(NA, 11),
                          c("Sensitivity", "Specificity", "Pos Pred Value",
                            "Neg Pred Value", "Precision", "Recall", "F1",
                            "Prevalence", "Detection Rate",
                            "Detection Prevalence", "Balanced Accuracy"))
      tibble(
        Modele            = nm,
        AUC_macro         = round(r$auc %||% NA_real_, 4),
        Accuracy          = round(unname(cm$overall["Accuracy"]), 4),
        Balanced_Acc      = round(unname(bc["Balanced Accuracy"]) %||% NA_real_, 4),
        Sensibilite_macro = round(unname(bc["Sensitivity"]) %||% NA_real_, 4),
        Specificite_macro = round(unname(bc["Specificity"]) %||% NA_real_, 4),
        VPP_macro         = round(unname(bc["Pos Pred Value"]) %||% NA_real_, 4),
        F1_macro          = round(unname(bc["F1"]) %||% NA_real_, 4),
        Kappa             = round(unname(cm$overall["Kappa"]), 4)
      )
    })
  }
}

# Metriques par classe (multiclasse)
metrics_per_class <- function(pipeline_res, model_name = NULL) {
  if (pipeline_res$is_binary) return(NULL)
  res <- pipeline_res$results
  if (is.null(model_name) || !model_name %in% names(res)) model_name <- names(res)[1]
  r <- res[[model_name]]; if (is.null(r) || is.null(r$cm)) return(NULL)
  bc <- r$cm$byClass
  if (is.null(bc) || !is.matrix(bc)) return(NULL)
  as.data.frame(bc) |>
    tibble::rownames_to_column("Classe") |>
    mutate(Classe = gsub("Class: ", "", Classe)) |>
    mutate(across(where(is.numeric), ~round(., 4))) |>
    as_tibble()
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. COURBES ROC COMPAREES
# ═══════════════════════════════════════════════════════════════════════════════
plot_roc_compare <- function(pipeline_res) {
  res <- pipeline_res$results
  if (length(res) == 0 || !pipeline_res$is_binary) return(plotly_empty())
  df_roc <- purrr::map_dfr(names(res), function(nm) {
    r <- res[[nm]]$roc
    if (is.null(r) || !inherits(r, "roc")) return(NULL)
    tibble(FPR = 1 - r$specificities, TPR = r$sensitivities,
           Modele = paste0(nm, " (AUC=", round(res[[nm]]$auc, 3), ")"))
  })
  if (nrow(df_roc) == 0) return(plotly_empty())
  nk <- length(unique(df_roc$Modele))
  pal <- c("#008DC9", "#c9161d", "#4dac26", "#7b2d8b", "#e88400")
  p <- ggplot(df_roc, aes(x = FPR, y = TPR, color = Modele)) +
    geom_line(linewidth = 1.3) +
    geom_abline(linetype = "dashed", color = "gray60") +
    scale_color_manual(values = pal[seq_len(nk)]) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Taux de faux positifs (1 \u2013 Sp\u00e9cificit\u00e9)",
         y = "Sensibilit\u00e9 (Taux de vrais positifs)",
         title = "Courbes ROC compar\u00e9es") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.25))
}

# ROC avec IC bootstrap (un seul modele)
plot_roc_ci <- function(pipeline_res, model_name = NULL, n_boot = 500) {
  if (!pipeline_res$is_binary) return(plotly_empty())
  res <- pipeline_res$results
  if (is.null(model_name) || !model_name %in% names(res)) model_name <- names(res)[1]
  r <- res[[model_name]]
  if (is.null(r) || is.null(r$roc)) return(plotly_empty())
  ci_obj <- tryCatch(ci.se(r$roc, specificities = seq(0, 1, 0.02), boot.n = n_boot,
                           progress = "none"),
                     error = function(e) NULL)
  if (is.null(ci_obj)) return(plotly_empty())
  df_ci <- tibble(
    FPR     = 1 - as.numeric(rownames(ci_obj)),
    TPR_lo  = ci_obj[, 1],
    TPR_med = ci_obj[, 2],
    TPR_hi  = ci_obj[, 3]
  )
  p <- ggplot(df_ci, aes(x = FPR)) +
    geom_ribbon(aes(ymin = TPR_lo, ymax = TPR_hi), fill = "#008DC9", alpha = 0.2) +
    geom_line(aes(y = TPR_med), color = "#008DC9", linewidth = 1.3) +
    geom_abline(linetype = "dashed", color = "gray60") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "1 \u2013 Sp\u00e9cificit\u00e9", y = "Sensibilit\u00e9",
         title = paste0("ROC avec IC 95% bootstrap \u2014 ", model_name),
         subtitle = paste0("AUC = ", round(r$auc, 3),
                           " [", round(ci.auc(r$roc)[1], 3), "\u2013",
                           round(ci.auc(r$roc)[3], 3), "]")) +
    theme_minimal(base_size = 11)
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 5. COURBES PRECISION-RAPPEL (PR)
# ═══════════════════════════════════════════════════════════════════════════════
plot_pr_curve <- function(pipeline_res) {
  if (!pipeline_res$is_binary) return(plotly_empty())
  res <- pipeline_res$results
  oc  <- pipeline_res$outcome_col %||% ".y"
  obs <- as.numeric(pipeline_res$test_data[[oc]]) - 1
  prev <- mean(obs)
  df_pr <- purrr::map_dfr(names(res), function(nm) {
    r <- res[[nm]]; if (is.null(r$pred_prob)) return(NULL)
    pts <- .pr_points(obs, r$pred_prob)
    pts |> mutate(Modele = paste0(nm, " (PR-AUC=", round(.pr_auc(pts), 3), ")"))
  })
  if (nrow(df_pr) == 0) return(plotly_empty())
  nk  <- length(unique(df_pr$Modele))
  pal <- c("#008DC9", "#c9161d", "#4dac26", "#7b2d8b", "#e88400")
  p <- ggplot(df_pr, aes(x = recall, y = precision, color = Modele)) +
    geom_line(linewidth = 1.3, na.rm = TRUE) +
    geom_hline(yintercept = prev, linetype = "dashed", color = "gray50") +
    annotate("text", x = 0.7, y = prev + 0.04,
             label = paste0("Baseline (", round(prev * 100, 1), "%)"),
             color = "gray50", size = 3) +
    scale_color_manual(values = pal[seq_len(nk)]) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Rappel (Recall)", y = "Pr\u00e9cision (PPV)",
         title = "Courbes Pr\u00e9cision-Rappel compar\u00e9es") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.25))
}

# ═══════════════════════════════════════════════════════════════════════════════
# 6. GAIN CUMULE & LIFT
# ═══════════════════════════════════════════════════════════════════════════════
plot_gain_lift <- function(pipeline_res, model_name = NULL) {
  if (!pipeline_res$is_binary) return(plotly_empty())
  res <- pipeline_res$results
  if (is.null(model_name) || !model_name %in% names(res)) model_name <- names(res)[1]
  r <- res[[model_name]]; if (is.null(r)) return(plotly_empty())
  oc    <- pipeline_res$outcome_col %||% ".y"
  obs   <- as.numeric(pipeline_res$test_data[[oc]]) - 1
  n     <- length(obs); n_pos <- sum(obs)
  ord   <- order(r$pred_prob, decreasing = TRUE)
  obs_s <- obs[ord]
  pct   <- seq_len(n) / n
  gain  <- cumsum(obs_s) / n_pos
  lift  <- gain / pct
  df    <- tibble(pct = c(0, pct), gain = c(0, gain), lift = c(NA_real_, lift))

  p_g <- ggplot(df, aes(x = pct, y = gain)) +
    geom_line(color = "#008DC9", linewidth = 1.3, na.rm = TRUE) +
    geom_abline(linetype = "dashed", color = "gray60") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "% Population cibl\u00e9e", y = "% Cas positifs captur\u00e9s",
         title = paste0("Gain cumul\u00e9 \u2014 ", model_name)) +
    theme_minimal(base_size = 11)

  p_l <- ggplot(df[-1, ], aes(x = pct, y = lift)) +
    geom_line(color = "#c9161d", linewidth = 1.3, na.rm = TRUE) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "% Population cibl\u00e9e", y = "Lift",
         title = paste0("Courbe de Lift \u2014 ", model_name)) +
    theme_minimal(base_size = 11)

  subplot(ggplotly(p_g), ggplotly(p_l), nrows = 1, shareX = FALSE,
          titleX = TRUE, margin = 0.06)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 7. CALIBRATION (Hosmer-Lemeshow)
# ═══════════════════════════════════════════════════════════════════════════════
plot_calibration <- function(pipeline_res, model_name = NULL) {
  if (!pipeline_res$is_binary) return(plotly_empty())
  res <- pipeline_res$results
  if (is.null(model_name)) model_name <- names(res)[1]
  r <- res[[model_name]]; if (is.null(r)) return(plotly_empty())
  oc  <- pipeline_res$outcome_col %||% ".y"
  obs <- as.numeric(pipeline_res$test_data[[oc]]) - 1
  pp  <- r$pred_prob
  df  <- tibble(pred = pp, obs = obs) |>
    mutate(decile = ntile(pred, 10)) |>
    group_by(decile) |>
    summarise(moy_predit = mean(pred), moy_observe = mean(obs), n = n(), .groups = "drop")
  hl     <- tryCatch(hoslem.test(obs, pp), error = function(e) NULL)
  hl_lab <- if (!is.null(hl)) paste0("Hosmer-Lemeshow p = ", round(hl$p.value, 4)) else ""
  p <- ggplot(df, aes(x = moy_predit, y = moy_observe)) +
    geom_abline(linetype = "dashed", color = "gray60") +
    geom_line(color = "#008DC9", linewidth = 1.1) +
    geom_point(aes(size = n), color = "#008DC9", alpha = 0.85) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Probabilit\u00e9 pr\u00e9dite", y = "Proportion observ\u00e9e",
         title = paste0("Calibration \u2014 ", model_name), subtitle = hl_lab) +
    theme_minimal(base_size = 11)
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 8. IMPORTANCE VARIABLES (caret varImp)
# ═══════════════════════════════════════════════════════════════════════════════
plot_importance <- function(pipeline_res, model_name = NULL) {
  res <- pipeline_res$results
  if (is.null(model_name) || !model_name %in% names(res)) model_name <- names(res)[1]
  r <- res[[model_name]]; if (is.null(r)) return(plotly_empty())
  tryCatch({
    imp <- varImp(r$fit)$importance |> tibble::rownames_to_column("Variable")
    y_var <- if ("Overall" %in% names(imp)) {
      "Overall"
    } else {
      imp$MoyImportance <- rowMeans(imp[, -1, drop = FALSE], na.rm = TRUE)
      "MoyImportance"
    }
    imp <- imp |> arrange(desc(.data[[y_var]])) |> slice_head(n = 20)
    p <- ggplot(imp, aes(y = fct_reorder(Variable, .data[[y_var]]),
                         x = .data[[y_var]], fill = .data[[y_var]])) +
      geom_col(alpha = 0.88) +
      scale_fill_gradientn(colors = c("#e3f2fd", "#008DC9", "#00426A")) +
      labs(y = "", x = "Importance",
           title = paste0("Importance des variables \u2014 ", model_name)) +
      theme_minimal(base_size = 11) + theme(legend.position = "none")
    ggplotly(p)
  }, error = function(e) plotly_empty())
}

importance_table <- function(pipeline_res, model_name = NULL) {
  res <- pipeline_res$results
  if (is.null(model_name) || !model_name %in% names(res)) model_name <- names(res)[1]
  r <- res[[model_name]]; if (is.null(r)) return(NULL)
  tryCatch({
    imp <- varImp(r$fit)$importance |> tibble::rownames_to_column("Variable")
    if ("Overall" %in% names(imp)) {
      imp |> arrange(desc(Overall)) |>
        mutate(Rang = row_number(), Importance = round(Overall, 2)) |>
        select(Rang, Variable, Importance)
    } else {
      imp$Importance <- round(rowMeans(imp[, -1, drop = FALSE], na.rm = TRUE), 2)
      imp |> arrange(desc(Importance)) |>
        mutate(Rang = row_number()) |>
        select(Rang, Variable, Importance)
    }
  }, error = function(e) NULL)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 9. IMPORTANCE PAR PERMUTATION
# ═══════════════════════════════════════════════════════════════════════════════
calc_permutation_importance <- function(pipeline_res, model_name = NULL, n_perm = 10) {
  res <- pipeline_res$results
  if (is.null(model_name) || !model_name %in% names(res)) model_name <- names(res)[1]
  r   <- res[[model_name]]; if (is.null(r)) return(NULL)
  td  <- pipeline_res$test_data
  oc  <- pipeline_res$outcome_col %||% ".y"
  preds <- intersect(pipeline_res$predictors, names(td))
  is_b  <- pipeline_res$is_binary

  set.seed(2847)
  if (nrow(td) > 400) td <- td[sample(seq_len(nrow(td)), 400), ]

  score_td <- function(dat) {
    if (is_b) {
      pp <- predict(r$fit, dat, type = "prob")[, "Positif"]
      as.numeric(auc(roc(dat[[oc]], pp, quiet = TRUE)))
    } else {
      mean(predict(r$fit, dat) == dat[[oc]])
    }
  }
  base_metric <- tryCatch(score_td(td), error = function(e) if (is_b) r$auc else as.numeric(r$cm$overall["Accuracy"]))
  metric_nm   <- if (is_b) "AUC" else "Accuracy"

  out <- purrr::map_dfr(preds, function(var) {
    drops <- replicate(n_perm, {
      td_p        <- td
      td_p[[var]] <- sample(td_p[[var]])
      m_perm <- tryCatch(score_td(td_p), error = function(e) base_metric)
      base_metric - m_perm
    })
    tibble(Variable = var, Importance = mean(drops), SD = sd(drops), Metrique = metric_nm)
  }) |> arrange(desc(Importance))
  attr(out, "n_perm") <- n_perm
  out
}

plot_permutation_importance <- function(perm_imp, model_name = "") {
  if (is.null(perm_imp) || nrow(perm_imp) == 0) return(plotly_empty())
  df <- perm_imp |> filter(!is.na(Importance)) |> slice_head(n = 20)
  metric_nm <- if ("Metrique" %in% names(df)) df$Metrique[1] else "Score"
  n_perm <- attr(perm_imp, "n_perm") %||% nrow(df)
  p <- ggplot(df, aes(y = reorder(Variable, Importance), x = Importance)) +
    geom_col(aes(fill = Importance), alpha = 0.88) +
    geom_errorbarh(aes(xmin = Importance - SD, xmax = Importance + SD),
                   height = 0.3, color = "gray40") +
    scale_fill_gradientn(colors = c("#ffeaa7", "#fdcb6e", "#e17055")) +
    labs(y = "", x = paste0("Baisse de ", metric_nm),
         title = paste0("Importance par permutation \u2014 ", model_name),
         subtitle = paste0(nrow(perm_imp), " variables \u00b7 ", n_perm, " permutations")) +
    theme_minimal(base_size = 11) + theme(legend.position = "none")
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 10. COURBES D'APPRENTISSAGE (Learning curves)
# ═══════════════════════════════════════════════════════════════════════════════
calc_learning_curves <- function(pipeline_res, model_type = "glm",
                                 sizes = c(0.2, 0.4, 0.6, 0.8, 1.0),
                                 k_folds = 3, seed = 2847) {
  set.seed(seed)
  d_train <- pipeline_res$train_data
  is_b    <- pipeline_res$is_binary
  n_total <- nrow(d_train)
  fml     <- .y ~ .

  caret_method <- switch(model_type,
    glm = if (is_b) "glm" else "multinom",
    rf  = "rf", xgb = "xgbTree", svm = "svmRadial", knn = "knn",
    if (is_b) "glm" else "multinom"
  )

  ctrl_cv <- if (is_b) {
    trainControl(method = "cv", number = k_folds, classProbs = TRUE,
                 summaryFunction = twoClassSummary, savePredictions = "final")
  } else {
    trainControl(method = "cv", number = k_folds, savePredictions = "final")
  }
  metric <- if (is_b) "ROC" else "Accuracy"

  purrr::map_dfr(sizes, function(s) {
    n_s   <- max(40, round(s * n_total))
    idx   <- sample(seq_len(n_total), min(n_s, n_total))
    d_sub <- d_train[idx, ]

    add_args <- list()
    if (caret_method %in% c("svmRadial", "knn"))
      add_args$preProcess <- c("center", "scale")
    if (caret_method == "glm" && is_b) add_args$family <- "binomial"
    if (caret_method == "multinom") {
      add_args$trace <- FALSE; add_args$MaxNWts <- 5000
    }
    if (caret_method == "xgbTree") add_args$verbosity <- 0

    # Hyperparametres fixes (pas de tuning) : objectif = tendance avec la taille
    if (caret_method == "rf")
      add_args$tuneGrid <- data.frame(mtry = max(1L, floor(sqrt(length(pipeline_res$predictors)))))
    if (caret_method == "svmRadial")
      add_args$tuneGrid <- data.frame(sigma = 0.05, C = 1)
    if (caret_method == "knn")
      add_args$tuneGrid <- data.frame(k = 5)
    if (caret_method == "xgbTree")
      add_args$tuneGrid <- data.frame(nrounds = 100, max_depth = 3, eta = 0.1,
                                      gamma = 0, colsample_bytree = 0.8,
                                      min_child_weight = 1, subsample = 0.8)

    # Score entrainement (resubstitution)
    ctrl_none <- trainControl(method = "none", classProbs = TRUE)
    fit_tr <- tryCatch(
      do.call(train, c(list(fml, data = d_sub, method = caret_method,
                            trControl = ctrl_none, metric = metric), add_args)),
      error = function(e) NULL)

    train_sc <- tryCatch({
      if (!is.null(fit_tr)) {
        if (is_b) {
          pp_tr <- predict(fit_tr, d_sub, type = "prob")[, "Positif"]
          as.numeric(auc(roc(d_sub[[".y"]], pp_tr, quiet = TRUE)))
        } else {
          mean(predict(fit_tr, d_sub) == d_sub[[".y"]])
        }
      } else NA_real_
    }, error = function(e) NA_real_)

    # Score validation croisee
    fit_cv <- tryCatch(
      do.call(train, c(list(fml, data = d_sub, method = caret_method,
                            trControl = ctrl_cv, metric = metric), add_args)),
      error = function(e) NULL)

    cv_sc <- tryCatch({
      if (!is.null(fit_cv)) {
        br <- fit_cv$results[which.max(fit_cv$results[[metric]]), , drop = FALSE]
        br[[metric]]
      } else NA_real_
    }, error = function(e) NA_real_)

    tibble(Size = n_s, Pct = s, Train = train_sc, CV = cv_sc)
  })
}

plot_learning_curves <- function(lc_data, model_name = "", metric_name = "AUC") {
  if (is.null(lc_data) || nrow(lc_data) == 0) return(plotly_empty())
  dl <- lc_data |>
    tidyr::pivot_longer(c(Train, CV), names_to = "Type", values_to = "Score") |>
    mutate(Type = dplyr::recode(Type,
                                "Train" = "Entra\u00eenement",
                                "CV"    = "Validation crois\u00e9e"))
  # Diagnostic biais-variance
  gap <- lc_data |>
    filter(!is.na(Train) & !is.na(CV)) |>
    slice_tail(n = 1) |>
    mutate(gap = abs(Train - CV)) |>
    pull(gap)
  subtitle <- if (length(gap) > 0 && !is.na(gap[1])) {
    if (gap[1] < 0.03)      "Convergence \u2014 faible variance"
    else if (gap[1] < 0.10) "L\u00e9ger surapprentissage \u2014 variance mod\u00e9r\u00e9e"
    else                     "Surapprentissage \u2014 variance \u00e9lev\u00e9e"
  } else ""

  p <- ggplot(dl, aes(x = Size, y = Score, color = Type)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    geom_point(size = 2.5, na.rm = TRUE) +
    scale_color_manual(values = c("Entra\u00eenement" = "#c9161d",
                                  "Validation crois\u00e9e" = "#008DC9")) +
    labs(x = "Taille de l'\u00e9chantillon d'entra\u00eenement",
         y = metric_name, color = "",
         title = paste0("Courbes d'apprentissage \u2014 ", model_name),
         subtitle = subtitle) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.25))
}

# ═══════════════════════════════════════════════════════════════════════════════
# 11. COMPARAISON CV PAR FOLD (heatmap)
# ═══════════════════════════════════════════════════════════════════════════════
plot_cv_fold_heatmap <- function(pipeline_res) {
  res <- pipeline_res$results
  if (length(res) == 0) return(plotly_empty())
  is_b <- pipeline_res$is_binary
  metric_col <- if (is_b) "ROC" else "Accuracy"

  df_folds <- purrr::map_dfr(names(res), function(nm) {
    r <- res[[nm]]
    if (is.null(r$fit) || is.null(r$fit$resample)) return(NULL)
    rs <- r$fit$resample
    if (!metric_col %in% names(rs)) return(NULL)
    rs |>
      mutate(Modele = nm, Fold = row_number()) |>
      select(Modele, Fold, Score = all_of(metric_col))
  })
  if (nrow(df_folds) == 0) return(plotly_empty())

  p <- ggplot(df_folds, aes(x = factor(Fold), y = Modele, fill = Score)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = round(Score, 3)), size = 3.5, fontface = "bold") +
    scale_fill_gradient2(low = "#e74c3c", mid = "#f39c12", high = "#27ae60",
                         midpoint = median(df_folds$Score, na.rm = TRUE)) +
    labs(x = "Fold", y = "", fill = metric_col,
         title = paste0("Performance par fold (", metric_col, ")")) +
    theme_minimal(base_size = 11) + theme(legend.position = "right")
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 12. DISTRIBUTION DES PROBABILITES PREDITES
# ═══════════════════════════════════════════════════════════════════════════════
plot_pred_distribution <- function(pipeline_res, model_name = NULL) {
  if (!pipeline_res$is_binary) return(plotly_empty())
  res <- pipeline_res$results
  if (is.null(model_name) || !model_name %in% names(res)) model_name <- names(res)[1]
  r <- res[[model_name]]; if (is.null(r)) return(plotly_empty())
  oc  <- pipeline_res$outcome_col %||% ".y"
  df <- tibble(
    Prob   = r$pred_prob,
    Classe = as.character(pipeline_res$test_data[[oc]])
  )
  p <- ggplot(df, aes(x = Prob, fill = Classe)) +
    geom_density(alpha = 0.55) +
    scale_fill_manual(values = c("Negatif" = "#008DC9", "Positif" = "#c9161d")) +
    labs(x = "Probabilit\u00e9 pr\u00e9dite (Positif)",
         y = "Densit\u00e9",
         title = paste0("Distribution des probabilit\u00e9s \u2014 ", model_name)) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p)
}

cat("module_predictif.R v2 charg\u00e9 (GLM|RF|XGBoost|SVM|k-NN \u00b7 15 m\u00e9triques \u00b7 ROC/PR/Lift \u00b7 Permutation \u00b7 Learning curves)\n")

# ═══════════════════════════════════════════════════════════════════════════════
# STABILITE MULTI-SEEDS
# ═══════════════════════════════════════════════════════════════════════════════
run_pipeline_seeds <- function(d, outcome, predictors, k_folds = 5,
                               models = c("glm", "rf", "xgb"),
                               train_pct = 0.70, n_seeds = 5, seed_base = 2847,
                               allow_parallel = TRUE) {
  .par_registered <- function() {
    isTRUE(foreach::getDoParRegistered()) &&
      !identical(foreach::getDoParName(), "doSEQ")
  }
  .cl_seeds <- NULL
  if (isTRUE(allow_parallel) && nrow(d) >= 2500 && !.par_registered()) {
    nc <- max(1L, parallel::detectCores() - 1L)
    if (nc >= 2) {
      .cl_seeds <- parallel::makeCluster(nc, rscript_args = c("--vanilla"))
      parallel::clusterCall(.cl_seeds, function(paths) { .libPaths(paths); NULL }, .libPaths())
      doParallel::registerDoParallel(.cl_seeds)
      on.exit({
        try(parallel::stopCluster(.cl_seeds), silent = TRUE)
        foreach::registerDoSEQ()
      }, add = TRUE)
    }
  }
  per_seed <- list()
  for (i in seq_len(n_seeds)) {
    seed <- seed_base + (i - 1L) * 17L
    r <- tryCatch(run_predictive_pipeline(d, outcome, predictors,
      k_folds = k_folds, seed = seed, models = models, train_pct = train_pct,
      allow_parallel = TRUE),
      error = function(e) NULL)
    if (is.null(r)) next
    for (nm in names(r$results)) {
      ev <- r$results[[nm]]
      sc <- if (!is.null(ev$auc) && is.finite(ev$auc)) ev$auc else NA_real_
      if (is.finite(sc))
        per_seed[[length(per_seed) + 1L]] <-
          data.frame(Modele = nm, Seed = seed, Score = sc)
    }
  }
  per_seed <- do.call(rbind, per_seed)
  if (is.null(per_seed) || nrow(per_seed) == 0) return(NULL)
  is_bin <- length(unique(na.omit(d[[outcome]]))) == 2
  met <- if (is_bin) "AUC" else "Accuracy"
  summ <- per_seed |>
    group_by(Modele) |>
    summarise(N_seeds = n(),
              Score_moyen = mean(Score, na.rm = TRUE),
              Score_sd = sd(Score, na.rm = TRUE),
              Score_min = min(Score), Score_max = max(Score),
              CV = Score_sd / pmax(Score_moyen, 1e-9), .groups = "drop") |>
    arrange(desc(Score_moyen))
  list(per_seed = per_seed, summary = summ, metric = met)
}

plot_stability_seeds <- function(seeds_res) {
  if (is.null(seeds_res)) return(plotly_empty())
  ps <- seeds_res$per_seed; met <- seeds_res$metric
  order_mod <- seeds_res$summary$Modele
  ps$Modele <- factor(ps$Modele, levels = order_mod)
  pal <- c("#008DC9", "#c9161d", "#4dac26", "#7b2d8b", "#e88400")
  p <- ggplot(ps, aes(x = Modele, y = Score, fill = Modele)) +
    geom_boxplot(alpha = 0.55, outlier.shape = NA, width = 0.55) +
    geom_jitter(width = 0.08, size = 2.2, alpha = 0.75, color = "#00426A") +
    scale_fill_manual(values = pal[seq_len(length(order_mod))], guide = "none") +
    labs(x = "", y = paste0(met, " (jeu de test)"),
         title = paste0("Stabilit\u00e9 des mod\u00e8les sur ", length(unique(ps$Seed)),
                        " seeds (split train/test al\u00e9atoires)")) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", color = "#00426A", size = 13))
  p
}

# ═══════════════════════════════════════════════════════════════════════════════
# VERSIONS gg PLOT QUALITE PUBLICATION (PNG 300 dpi / PDF)
# ═══════════════════════════════════════════════════════════════════════════════
.gg_theme_pub <- function() {
  theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", color = "#00426A", size = 14),
          plot.subtitle = element_text(color = "#008DC9", size = 10),
          plot.caption  = element_text(color = "#888", size = 8, hjust = 0),
          legend.position = "bottom",
          plot.background = element_rect(fill = "white", color = NA))
}

gg_roc_simple <- function(pipeline_res) {
  res <- pipeline_res$results
  df_roc <- purrr::map_dfr(names(res), function(nm) {
    r <- res[[nm]]$roc
    if (is.null(r) || !inherits(r, "roc")) return(NULL)
    tibble(FPR = 1 - r$specificities, TPR = r$sensitivities,
           Modele = paste0(nm, " (AUC=", round(res[[nm]]$auc, 3), ")"))
  })
  if (nrow(df_roc) == 0) return(NULL)
  nk <- length(unique(df_roc$Modele))
  pal <- c("#008DC9", "#c9161d", "#4dac26", "#7b2d8b", "#e88400")
  ggplot(df_roc, aes(x = FPR, y = TPR, color = Modele)) +
    geom_line(linewidth = 1.3) +
    geom_abline(linetype = "dashed", color = "gray60") +
    scale_color_manual(values = pal[seq_len(nk)]) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Taux de faux positifs", y = "Sensibilit\u00e9",
         title = "Courbes ROC compar\u00e9es",
         caption = "Source : pipeline pr\u00e9dictif NutriAnalyse \u00b7 jeu de test") +
    .gg_theme_pub()
}

gg_pr_simple <- function(pipeline_res) {
  res <- pipeline_res$results
  oc  <- pipeline_res$outcome_col %||% ".y"
  obs <- as.numeric(pipeline_res$test_data[[oc]]) - 1
  df_pr <- purrr::map_dfr(names(res), function(nm) {
    pts <- tryCatch(.pr_points(obs, res[[nm]]$pred_prob), error = function(e) NULL)
    if (is.null(pts)) return(NULL)
    aucv <- tryCatch(.pr_auc(pts), error = function(e) NA)
    tibble(Recall = pts$recall, Precision = pts$precision,
           Modele = paste0(nm, " (PR-AUC=", round(aucv, 3), ")"))
  })
  if (nrow(df_pr) == 0) return(NULL)
  nk <- length(unique(df_pr$Modele))
  pal <- c("#008DC9", "#c9161d", "#4dac26", "#7b2d8b", "#e88400")
  ggplot(df_pr, aes(x = Recall, y = Precision, color = Modele)) +
    geom_line(linewidth = 1.3) +
    scale_color_manual(values = pal[seq_len(nk)]) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Rappel", y = "Pr\u00e9cision", title = "Courbes Precision-Recall",
         caption = "Source : pipeline pr\u00e9dictif NutriAnalyse \u00b7 jeu de test") +
    .gg_theme_pub()
}

gg_calib_simple <- function(pipeline_res, model_name = NULL, n_bins = 10) {
  res <- pipeline_res$results
  if (is.null(model_name)) model_name <- names(res)[1]
  r <- res[[model_name]]
  if (is.null(r) || is.null(r$pred_prob) || is.null(r$pred_class)) return(NULL)
  oc  <- pipeline_res$outcome_col %||% ".y"
  obs <- as.numeric(pipeline_res$test_data[[oc]]) - 1
  bins <- cut(r$pred_prob, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)
  df_c <- data.frame(
    prev_pred = tapply(r$pred_prob, bins, mean),
    prev_obs  = tapply(obs, bins, mean)) |>
    tibble::rownames_to_column() |>
    na.omit()
  if (nrow(df_c) == 0) return(NULL)
  ggplot(df_c, aes(x = prev_pred, y = prev_obs)) +
    geom_point(size = 2.6, color = "#008DC9") +
    geom_line(color = "#008DC9", linewidth = 0.9) +
    geom_abline(linetype = "dashed", color = "gray60") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Probabilit\u00e9 pr\u00e9dite", y = "Fr\u00e9quence observ\u00e9e",
         title = paste0("Calibration \u2014 ", model_name),
         caption = "Source : pipeline pr\u00e9dictif NutriAnalyse \u00b7 jeu de test") +
    .gg_theme_pub()
}

gg_imp_simple <- function(pipeline_res, model_name = NULL, top_n = 15) {
  it <- importance_table(pipeline_res, model_name)
  if (is.null(it) || nrow(it) == 0) return(NULL)
  nm <- "Variable"; v <- "Importance"
  it <- it[order(-it$Importance), ][seq_len(min(top_n, nrow(it))), ]
  ggplot(it, aes(x = reorder(.data[[nm]], .data[[v]]), y = .data[[v]])) +
    geom_col(fill = "#008DC9", alpha = 0.85) +
    coord_flip() +
    labs(x = "", y = "Importance",
         title = paste0("Importance des variables \u2014 ",
           ifelse(is.null(model_name), "meilleur mod\u00e8le", model_name)),
         caption = "Source : pipeline pr\u00e9dictif NutriAnalyse") +
    .gg_theme_pub() + theme(legend.position = "none")
}

#' Rapport PDF multi-pages qualite publication
rapport_predictions_pdf <- function(pipeline_res, file, model_name = NULL) {
  res <- pipeline_res
  ok <- tryCatch({
    pdf(file, width = 11, height = 8.5, useDingbats = FALSE)
    grid::grid.newpage()
    grid::grid.text("Rapport de mod\u00e9lisation pr\u00e9dictive", y = 0.72,
      gp = grid::gpar(fontsize = 22, fontface = "bold", col = "#00426A"))
    fr <- if (!is.null(res$pays_lbl)) res$pays_lbl else ""
    grid::grid.text(paste0(
      "Cible : ", res$outcome_col, " \u00b7 ",
      if (res$is_binary) "Classification binaire"
      else paste0("Multiclasse (", res$n_classes, " classes)"),
      " \u00b7 ", res$n_train, " obs. train / ", res$n_test, " test \u00b7 ",
      res$k_folds, "-fold CV\nMod\u00e8les : ",
      paste(names(res$results), collapse = " \u00b7 "),
      if (nzchar(fr)) paste0("\n", fr)), y = 0.58,
      gp = grid::gpar(fontsize = 12, col = "#333"))
    grid::grid.text(paste0("NutriAnalyse Pro v2 \u00b7 ",
      format(Sys.Date(), "%d/%m/%Y")),
      y = 0.08, gp = grid::gpar(fontsize = 9, col = "#888"))
    for (p in list(gg_roc_simple(res), gg_pr_simple(res),
                   gg_calib_simple(res, model_name), gg_imp_simple(res, model_name))) {
      if (!is.null(p)) print(p)
    }
    mt <- metrics_table(res)
    if (nrow(mt) > 0) {
      mt <- mutate(mt, across(where(is.numeric), ~round(., 4)))
      print(gridExtra::grid.table(mt, rows = NULL,
        theme = gridExtra::ttheme_minimal(base_size = 9)))
    }
    TRUE
  }, error = function(e) FALSE)
  tryCatch(dev.off(), error = function(e) NULL)
  ok
}

# ═══════════════════════════════════════════════════════════════════════════════
# SCORING BATCH (appliquer le modele a de nouvelles donnees)
# ═══════════════════════════════════════════════════════════════════════════════
score_new_data <- function(fit, newd, predictors) {
  miss <- setdiff(predictors, names(newd))
  if (length(miss) > 0) stop("Colonnes manquantes : ", paste(miss, collapse = ", "))
  tr <- fit$trainingData
  for (v in predictors) {
    if (v %in% names(tr) && is.factor(tr[[v]]) && v %in% names(newd)) {
      newd[[v]] <- factor(as.character(newd[[v]]), levels = levels(tr[[v]]))
    }
  }
  pc <- tryCatch(predict(fit, newd), error = function(e) NULL)
  if (is.null(pc)) stop("La prediction a echoue (verifiez les types de colonnes)")
  if ("Positif" %in% levels(pc)) {
    pp <- tryCatch(predict(fit, newd, type = "prob")[, "Positif"],
                   error = function(e) NA_real_)
  } else pp <- NA_real_
  data.frame(Prediction = as.character(pc), Probabilite = round(pp, 4))
}

# ═══════════════════════════════════════════════════════════════════════════════
# PARTIAL DEPENDENCE (PDP)
# ═══════════════════════════════════════════════════════════════════════════════
calc_pdp <- function(fit, train_data, var, n_grid = 20) {
  if (is.factor(train_data[[var]])) {
    vals <- levels(droplevels(factor(train_data[[var]])))
  } else {
    x <- train_data[[var]][is.finite(train_data[[var]])]
    if (length(x) == 0) return(NULL)
    vals <- unique(round(seq(min(x), max(x), length.out = n_grid), 3))
  }
  if (length(vals) < 2) return(NULL)
  nd <- train_data[rep(1, length(vals)), , drop = FALSE]
  nd[[var]] <- vals
  pp <- tryCatch(predict(fit, nd, type = "prob")[, "Positif"], error = function(e) NULL)
  if (is.null(pp)) return(NULL)
  data.frame(Val = vals, Prev = round(pp, 4))
}

plot_pdp <- function(pdp_df, var, lbl = NULL) {
  if (is.null(pdp_df)) return(plotly_empty())
  p <- ggplot(pdp_df, aes(x = Val, y = Prev)) +
    geom_line(color = "#008DC9", linewidth = 1.1) +
    geom_point(color = "#00426A", size = 2.2) +
    geom_hline(yintercept = mean(pdp_df$Prev), linetype = "dashed", color = "gray60") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = ifelse(is.null(lbl), var, lbl), y = "Probabilit\u00e9 pr\u00e9dite",
         title = paste0("Effet marginal \u2014 ", var),
         subtitle = "Partial dependence (autres variables fixes \u00e0 leur valeur de reference)",
         caption = "Source : pipeline pr\u00e9dictif NutriAnalyse") +
    .gg_theme_pub()
  ggplotly(p)
}
