# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  server_epidemio_expert.R — Server logic expert épidémiologie              ║
# ║  À sourcer dans le moduleServer de epidemioServer (local = TRUE)           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 1 : SCREENING LOGISTIQUE UNIVARIÉ
# ═══════════════════════════════════════════════════════════════════════════════
output$epid_screen_outcome_ui <- renderUI({
  vc <- vars_cat(); req(length(vc) > 0)
  selectInput(ns("epid_screen_outcome"), "Outcome (binaire) :", choices = vc, width = "100%")
})

output$epid_screen_vars_ui <- renderUI({
  va <- vars_all(); req(length(va) > 0)
  selectizeInput(ns("epid_screen_vars"), "Variables à tester :",
                 choices = va, selected = head(va, 10), multiple = TRUE, width = "100%")
})

epid_screen_res <- reactiveVal(NULL)

observeEvent(input$run_screen, {
  req(input$epid_screen_outcome, input$epid_screen_vars)
  d <- df_f()
  withProgress(message = "Screening logistique univarié...", value = 0.3, {
    res <- tryCatch(
      run_logistic_univariate(d, input$epid_screen_outcome,
                              setdiff(input$epid_screen_vars, input$epid_screen_outcome),
                              conf_level = input$epid_screen_conf),
      error = function(e) NULL)
    incProgress(0.7)
  })
  epid_screen_res(res)
  if (!is.null(res) && nrow(res) > 0)
    showNotification(paste0("✅ ", nrow(res), " variables screenées"), type = "message")
})

output$epid_screen_table <- renderDT({
  req(epid_screen_res(), nrow(epid_screen_res()) > 0)
  datatable(epid_screen_res(), rownames = FALSE,
            options = list(pageLength = 20, dom = "ftp", scrollX = TRUE,
                           order = list(list(8, "asc"))),
            class = "compact stripe hover") |>
    formatStyle("OR", color = styleInterval(1, c("#4dac26", "#c9161d")), fontWeight = "bold") |>
    formatStyle("Sig", fontWeight = "bold",
                color = styleEqual(c("***","**","*",".","NS"),
                                   c("#c9161d","#c9161d","#e88400","#888","#aaa")))
})

output$epid_screen_forest <- renderPlotly({
  req(epid_screen_res(), nrow(epid_screen_res()) > 0)
  res <- epid_screen_res() |> dplyr::filter(!is.na(OR))

  p <- ggplot(res, aes(x = OR, y = reorder(Variable, OR),
                        xmin = `IC inf`, xmax = `IC sup`,
                        color = `p-valeur` < 0.05,
                        text = paste0(Variable, "\nOR=", OR, " [", `IC inf`, "-", `IC sup`, "]",
                                      "\np=", `p-valeur`, " ", Sig))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
    geom_errorbarh(height = 0.25, linewidth = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = c("TRUE" = "#c9161d", "FALSE" = "#95a5a6"),
                       labels = c("TRUE" = "p<0.05", "FALSE" = "NS"), name = "") +
    scale_x_log10() +
    labs(x = "OR (échelle log)", y = "", title = "Screening — OR univariés") +
    theme_minimal(base_size = 11)
  ggplotly(p, tooltip = "text")
})

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 2 : LOGISTIQUE MULTIVARIÉE
# ═══════════════════════════════════════════════════════════════════════════════
output$epid_multi_outcome_ui <- renderUI({
  vc <- vars_cat(); req(length(vc) > 0)
  selectInput(ns("epid_multi_outcome"), "Outcome :", choices = vc, width = "100%")
})

output$epid_multi_pred_ui <- renderUI({
  va <- vars_all(); req(length(va) > 0)
  selectizeInput(ns("epid_multi_pred"), "Prédicteurs :",
                 choices = va, selected = head(va, 5), multiple = TRUE, width = "100%")
})

epid_multi_res <- reactiveVal(NULL)

observeEvent(input$run_multi_logistic, {
  req(input$epid_multi_outcome, input$epid_multi_pred)
  d <- df_f()
  preds <- setdiff(input$epid_multi_pred, input$epid_multi_outcome)
  req(length(preds) >= 1)
  withProgress(message = "Régression logistique multivariée...", value = 0.3, {
    res <- tryCatch(
      run_logistic_multivariate(d, input$epid_multi_outcome, preds,
                                conf_level = input$epid_multi_conf,
                                stepwise = input$epid_multi_stepwise),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL })
    incProgress(0.7)
  })
  epid_multi_res(res)
  if (!is.null(res)) showNotification("✅ Modèle ajusté", type = "message")
})

output$epid_multi_coef_table <- renderDT({
  req(epid_multi_res())
  coefs <- epid_multi_res()$coefficients |>
    dplyr::mutate(across(where(is.numeric), ~round(., 4)))
  datatable(coefs, rownames = FALSE, options = list(dom = "t", pageLength = 20),
            class = "compact stripe hover") |>
    formatStyle("estimate", color = styleInterval(1, c("#4dac26", "#c9161d")), fontWeight = "bold")
})

output$epid_multi_forest <- renderPlotly({
  req(epid_multi_res())
  plot_or_forest(epid_multi_res(), "OR ajustés — Modèle multivarié")
})

output$epid_multi_roc <- renderPlotly({
  req(epid_multi_res(), input$epid_multi_outcome)
  d <- df_f()
  plot_logistic_roc(epid_multi_res(), d, input$epid_multi_outcome)
})

output$epid_multi_summary <- renderUI({
  req(epid_multi_res())
  diag <- epid_multi_res()$diagnostics
  auc_col <- if (!is.na(diag$AUC) && diag$AUC >= 0.7) "#27ae60" else "#f39c12"
  tags$div(
    tags$div(style = paste0("border:2px solid ", auc_col,
                            ";border-radius:8px;padding:10px;text-align:center;margin-bottom:8px;"),
      tags$div(style = paste0("font-size:22px;font-weight:700;color:", auc_col, ";"),
               if (!is.na(diag$AUC)) diag$AUC else "—"),
      tags$div(style = "font-size:10px;color:#888;", "AUC")
    ),
    tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
      paste0("R² McF=", diag$McFadden_R2), tags$br(),
      paste0("R² Nag=", diag$Nagelkerke_R2), tags$br(),
      paste0("AIC=", diag$AIC), tags$br(),
      paste0("N=", diag$N)
    )
  )
})

output$epid_multi_diag_table <- renderDT({
  req(epid_multi_res())
  diag <- epid_multi_res()$diagnostics
  tbl <- tibble::tibble(
    Diagnostic = c("N observations", "N prédicteurs", "AIC", "BIC",
                   "Déviance nulle", "Déviance résiduelle",
                   "R² McFadden", "R² Nagelkerke", "AUC",
                   "Hosmer-Lemeshow χ²", "HL ddl", "HL p-valeur",
                   "Stepwise", "Formule finale"),
    Valeur = as.character(c(diag$N, diag$n_predictors, diag$AIC, diag$BIC,
                            diag$null_deviance, diag$residual_deviance,
                            diag$McFadden_R2, diag$Nagelkerke_R2, diag$AUC,
                            diag$HL_chi2, diag$HL_df, diag$HL_p,
                            diag$stepwise, diag$formula_final))
  )
  datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 20),
            class = "compact stripe hover")
})

output$epid_multi_diag_interp <- renderUI({
  req(epid_multi_res())
  diag <- epid_multi_res()$diagnostics
  tags$div(style = "font-size:12px;",
    if (!is.na(diag$HL_p)) tags$div(
      style = paste0("border-left:4px solid ", if (diag$HL_p >= 0.05) "#27ae60" else "#e74c3c",
                     ";padding:8px 12px;margin-bottom:8px;"),
      tags$b("Hosmer-Lemeshow : "),
      if (diag$HL_p >= 0.05) "Bonne calibration du modèle ✓"
      else "Calibration inadéquate ✗"
    ),
    if (!is.na(diag$AUC)) tags$div(
      style = "border-left:4px solid #2980b9;padding:8px 12px;",
      tags$b("Discrimination : "),
      dplyr::case_when(
        diag$AUC >= 0.9 ~ "Excellente (AUC ≥ 0.9)",
        diag$AUC >= 0.8 ~ "Bonne (0.8–0.9)",
        diag$AUC >= 0.7 ~ "Acceptable (0.7–0.8)",
        diag$AUC >= 0.6 ~ "Faible (0.6–0.7)",
        TRUE ~ "Pas de discrimination (<0.6)"
      )
    )
  )
})

output$epid_multi_vif_table <- renderDT({
  req(epid_multi_res())
  vif_vals <- epid_multi_res()$vif
  req(!is.null(vif_vals))
  tbl <- tibble::tibble(Variable = names(vif_vals), VIF = round(vif_vals, 3),
    Diagnostic = dplyr::case_when(vif_vals > 10 ~ "SÉVÈRE", vif_vals > 5 ~ "Modérée",
                                   vif_vals > 2 ~ "Légère", TRUE ~ "OK"))
  datatable(tbl, rownames = FALSE, options = list(dom = "t"), class = "compact stripe hover") |>
    formatStyle("Diagnostic",
      color = styleEqual(c("SÉVÈRE","Modérée","Légère","OK"),
                         c("#e74c3c","#f39c12","#f1c40f","#27ae60")), fontWeight = "bold")
})

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 3 : RÉGRESSION DE POISSON / BN
# ═══════════════════════════════════════════════════════════════════════════════
output$epid_pois_outcome_ui <- renderUI({
  d <- df_f(); req(d)
  vn <- names(d)[sapply(d, is.numeric)]
  selectInput(ns("epid_pois_outcome"), "Variable de comptage (Y) :", choices = vn, width = "100%")
})
output$epid_pois_pred_ui <- renderUI({
  va <- vars_all()
  selectizeInput(ns("epid_pois_pred"), "Prédicteurs :",
                 choices = va, selected = head(va, 3), multiple = TRUE, width = "100%")
})
output$epid_pois_offset_ui <- renderUI({
  d <- df_f(); req(d)
  vn <- names(d)[sapply(d, is.numeric)]
  selectInput(ns("epid_pois_offset"), "Offset (optionnel) :",
              choices = c("Aucun" = "", vn), selected = "", width = "100%")
})

epid_pois_res <- reactiveVal(NULL)

observeEvent(input$run_poisson, {
  req(input$epid_pois_outcome, input$epid_pois_pred)
  d <- df_f()
  offset_v <- if (input$epid_pois_offset != "") input$epid_pois_offset else NULL
  withProgress(message = "Régression Poisson/BN...", value = 0.5, {
    res <- tryCatch(
      run_poisson_regression(d, input$epid_pois_outcome,
                             setdiff(input$epid_pois_pred, input$epid_pois_outcome),
                             offset_var = offset_v,
                             model_type = input$epid_pois_type),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL })
    incProgress(0.5)
  })
  epid_pois_res(res)
  if (!is.null(res)) showNotification("✅ Modèle Poisson/BN ajusté", type = "message")
})

output$epid_pois_coef_table <- renderDT({
  req(epid_pois_res())
  coefs <- epid_pois_res()$coefficients |>
    dplyr::mutate(across(where(is.numeric), ~round(., 4))) |>
    dplyr::rename(IRR = estimate)
  datatable(coefs, rownames = FALSE, options = list(dom = "t", pageLength = 20),
            class = "compact stripe hover") |>
    formatStyle("IRR", fontWeight = "bold",
                color = styleInterval(1, c("#4dac26", "#c9161d")))
})

output$epid_pois_diag <- renderUI({
  req(epid_pois_res())
  diag <- epid_pois_res()$diagnostics
  od <- diag$overdispersion
  tags$div(style = "display:flex;gap:12px;flex-wrap:wrap;",
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:10px;",
      tags$div(style = "font-size:18px;font-weight:700;", diag$AIC),
      tags$div(style = "font-size:10px;color:#888;", "AIC")),
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:10px;",
      tags$div(style = "font-size:18px;font-weight:700;", round(diag$deviance, 1)),
      tags$div(style = "font-size:10px;color:#888;", paste0("Déviance (ddl=", diag$df_residual, ")"))),
    tags$div(style = paste0("flex:1;min-width:120px;text-align:center;border-radius:8px;padding:10px;",
                            "background:", if (isTRUE(od$overdispersed)) "#fef2f2" else "#f0fdf4", ";"),
      tags$div(style = paste0("font-size:18px;font-weight:700;color:",
                              if (isTRUE(od$overdispersed)) "#e74c3c" else "#27ae60", ";"),
               od$ratio),
      tags$div(style = "font-size:10px;color:#888;",
               if (isTRUE(od$overdispersed)) "Surdispersion !" else "Dispersion OK"))
  )
})

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 4 : DOSE-RÉPONSE
# ═══════════════════════════════════════════════════════════════════════════════
output$epid_dr_dose_ui <- renderUI({
  d <- df_f(); vn <- names(d)[sapply(d, is.numeric)]
  selectInput(ns("epid_dr_dose"), "Variable dose (continue) :", choices = vn, width = "100%")
})
output$epid_dr_outcome_ui <- renderUI({
  vc <- vars_cat()
  selectInput(ns("epid_dr_outcome"), "Outcome (binaire) :", choices = vc, width = "100%")
})

epid_dr_res <- reactiveVal(NULL)

observeEvent(input$run_dose_response, {
  req(input$epid_dr_dose, input$epid_dr_outcome)
  d <- df_f()
  withProgress(message = "Analyse dose-réponse...", value = 0.5, {
    res <- tryCatch(
      run_dose_response(d, input$epid_dr_dose, input$epid_dr_outcome,
                        n_doses = input$epid_dr_ndoses),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL })
    incProgress(0.5)
  })
  epid_dr_res(res)
})

output$epid_dr_plot <- renderPlotly({
  req(epid_dr_res())
  plot_dose_response(epid_dr_res())
})

output$epid_dr_table <- renderDT({
  req(epid_dr_res())
  coefs <- epid_dr_res()$coefs_cat |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(across(where(is.numeric), ~round(., 3))) |>
    dplyr::rename(OR = estimate)
  datatable(coefs, rownames = FALSE, options = list(dom = "t"), class = "compact stripe hover") |>
    formatStyle("OR", fontWeight = "bold", color = styleInterval(1, c("#4dac26", "#c9161d")))
})

output$epid_dr_summary <- renderUI({
  req(epid_dr_res())
  dr <- epid_dr_res()
  sig <- !is.na(dr$trend_p) && dr$trend_p < 0.05
  col <- if (sig) "#c9161d" else "#27ae60"
  tags$div(style = paste0("border:2px solid ", col, ";border-radius:8px;padding:12px;"),
    tags$div(style = paste0("font-weight:700;color:", col, ";font-size:13px;"),
             if (sig) "Tendance dose-réponse SIGNIFICATIVE" else "Pas de tendance significative"),
    tags$div(style = "font-size:12px;color:#555;margin-top:4px;",
             paste0("LRT tendance p=", dr$trend_p,
                    " | AIC linéaire=", dr$aic_lin,
                    " catégoriel=", dr$aic_cat)))
})

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 5 : INTERACTION
# ═══════════════════════════════════════════════════════════════════════════════
output$epid_inter_outcome_ui <- renderUI({
  vc <- vars_cat()
  selectInput(ns("epid_inter_outcome"), "Outcome :", choices = vc, width = "100%")
})
output$epid_inter_expo1_ui <- renderUI({
  va <- vars_all()
  selectInput(ns("epid_inter_expo1"), "Exposition 1 :", choices = va, width = "100%")
})
output$epid_inter_expo2_ui <- renderUI({
  va <- vars_all()
  sel <- if (length(va) > 1) va[2] else va[1]
  selectInput(ns("epid_inter_expo2"), "Exposition 2 (modificateur) :", choices = va, selected = sel, width = "100%")
})

epid_inter_res <- reactiveVal(NULL)

observeEvent(input$run_interaction, {
  req(input$epid_inter_outcome, input$epid_inter_expo1, input$epid_inter_expo2)
  d <- df_f()
  withProgress(message = "Test d'interaction...", value = 0.5, {
    res <- tryCatch(
      run_interaction_test(d, input$epid_inter_outcome,
                           input$epid_inter_expo1, input$epid_inter_expo2),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL })
    incProgress(0.5)
  })
  epid_inter_res(res)
})

output$epid_inter_result <- renderUI({
  req(epid_inter_res())
  res <- epid_inter_res()
  col <- if (res$interaction_sig) "#c9161d" else "#27ae60"
  tags$div(
    tags$div(style = paste0("border:2px solid ", col, ";border-radius:8px;padding:12px;margin-bottom:10px;"),
      tags$div(style = paste0("font-weight:700;color:", col, ";font-size:13px;"),
               if (res$interaction_sig) "INTERACTION SIGNIFICATIVE" else "Pas d'interaction"),
      tags$div(style = "font-size:12px;color:#555;margin-top:4px;",
               paste0("LRT p=", res$lrt_p, " | AIC sans=", res$aic_main, " avec=", res$aic_inter)),
      tags$div(style = "font-size:12px;color:#333;margin-top:4px;", res$conclusion)
    ),
    if (!is.null(res$reri)) tags$div(
      style = "background:#f8f9fa;border-radius:6px;padding:10px;font-size:12px;",
      tags$b("RERI (interaction additive) = "), res$reri$RERI, tags$br(),
      res$reri$interpretation
    )
  )
})

output$epid_inter_plot <- renderPlotly({
  req(input$epid_inter_outcome, input$epid_inter_expo1, input$epid_inter_expo2)
  d <- df_f()
  plot_interaction_stratified(d, input$epid_inter_outcome,
                              input$epid_inter_expo1, input$epid_inter_expo2)
})

output$epid_inter_coef_table <- renderDT({
  req(epid_inter_res())
  coefs <- epid_inter_res()$coefficients |>
    dplyr::mutate(across(where(is.numeric), ~round(., 4)))
  datatable(coefs, rownames = FALSE, options = list(dom = "t"), class = "compact stripe hover")
})

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 6 : HÉTÉROGÉNÉITÉ I²
# ═══════════════════════════════════════════════════════════════════════════════
output$epid_het_expo_ui <- renderUI({
  vc <- vars_cat()
  selectInput(ns("epid_het_expo"), "Exposition :", choices = vc, width = "100%")
})
output$epid_het_outcome_ui <- renderUI({
  vc <- vars_cat()
  selectInput(ns("epid_het_outcome"), "Outcome :", choices = vc, width = "100%")
})
output$epid_het_strata_ui <- renderUI({
  vc <- vars_cat()
  selectInput(ns("epid_het_strata"), "Variable de stratification :", choices = vc, width = "100%")
})

epid_het_res <- reactiveVal(NULL)

observeEvent(input$run_heterogeneity, {
  req(input$epid_het_expo, input$epid_het_outcome, input$epid_het_strata)
  d <- df_f()
  withProgress(message = "Calcul hétérogénéité I²...", value = 0.3, {
    strata_vals <- unique(na.omit(d[[input$epid_het_strata]]))
    req(length(strata_vals) >= 2)

    # [E4] Utiliser lapply au lieu de purrr::map pour éviter la dépendance
    strata_results <- lapply(strata_vals, function(sv) {
      d_s <- d |> dplyr::filter(.data[[input$epid_het_strata]] == sv)
      tryCatch({
        r <- analyse_2x2(d_s[[input$epid_het_expo]], d_s[[input$epid_het_outcome]])
        if (is.null(r)) return(NULL)
        list(label = as.character(sv), or = r$or, or_lo = r$or_lo, or_hi = r$or_hi)
      }, error = function(e) NULL)
    })
    strata_results <- strata_results[!sapply(strata_results, is.null)]
    req(length(strata_results) >= 2)

    res <- calc_heterogeneity(
      or_values = sapply(strata_results, `[[`, "or"),
      or_lo_values = sapply(strata_results, `[[`, "or_lo"),
      or_hi_values = sapply(strata_results, `[[`, "or_hi"),
      labels = sapply(strata_results, `[[`, "label")
    )
    incProgress(0.7)
    epid_het_res(res)
  })
  if (!is.null(epid_het_res())) showNotification("✅ I² calculé", type = "message")
})

output$epid_het_forest <- renderPlotly({
  req(epid_het_res())
  plot_meta_forest(epid_het_res())
})

output$epid_het_summary <- renderUI({
  req(epid_het_res())
  res <- epid_het_res()
  col <- if (res$I2 < 50) "#27ae60" else if (res$I2 < 75) "#f39c12" else "#e74c3c"
  tags$div(
    tags$div(style = paste0("border:2px solid ", col, ";border-radius:8px;padding:10px;text-align:center;"),
      tags$div(style = paste0("font-size:24px;font-weight:700;color:", col, ";"),
               paste0(res$I2, "%")),
      tags$div(style = "font-size:10px;color:#888;", "I² de Higgins"),
      tags$div(style = "font-size:11px;color:#555;", res$I2_interp)
    ),
    tags$div(style = "font-size:11px;color:#555;margin-top:8px;line-height:1.6;",
      paste0("Q=", res$Q, " p=", res$Q_p), tags$br(),
      paste0("τ²=", res$tau2), tags$br(),
      paste0("OR FE=", res$fe_or, " [", res$fe_lo, "-", res$fe_hi, "]"), tags$br(),
      paste0("OR RE=", res$re_or, " [", res$re_lo, "-", res$re_hi, "]")
    )
  )
})

output$epid_het_table <- renderDT({
  req(epid_het_res())
  res <- epid_het_res()
  tbl <- tibble::tibble(Strate = res$labels, OR = res$or_values,
                         `IC inf` = res$or_lo, `IC sup` = res$or_hi)
  datatable(tbl, rownames = FALSE, options = list(dom = "t"), class = "compact stripe hover") |>
    formatRound(c("OR", "IC inf", "IC sup"), 3)
})

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 7 : PAF AJUSTÉ
# ═══════════════════════════════════════════════════════════════════════════════
output$epid_paf_expo_ui <- renderUI({
  vc <- vars_cat()
  selectInput(ns("epid_paf_expo"), "Exposition :", choices = vc, width = "100%")
})
output$epid_paf_outcome_ui <- renderUI({
  vc <- vars_cat()
  selectInput(ns("epid_paf_outcome"), "Outcome :", choices = vc, width = "100%")
})
output$epid_paf_conf_ui <- renderUI({
  va <- vars_all()
  selectizeInput(ns("epid_paf_conf"), "Facteurs de confusion :",
                 choices = va, selected = NULL, multiple = TRUE, width = "100%")
})

epid_paf_res <- reactiveVal(NULL)

observeEvent(input$run_paf_adj, {
  req(input$epid_paf_expo, input$epid_paf_outcome)
  d <- df_f()
  conf <- if (length(input$epid_paf_conf) > 0) input$epid_paf_conf else NULL
  withProgress(message = "Calcul PAF ajusté + bootstrap...", value = 0.3, {
    res <- tryCatch(
      calc_paf_adjusted(d, input$epid_paf_outcome, input$epid_paf_expo,
                        confounders = conf),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL })
    incProgress(0.7)
  })
  epid_paf_res(res)
})

output$epid_paf_result <- renderUI({
  req(epid_paf_res())
  res <- epid_paf_res()
  col <- if (res$paf_brut > 0.1) "#c9161d" else if (res$paf_brut > 0) "#f39c12" else "#27ae60"
  tags$div(
    tags$div(style = "display:flex;gap:16px;flex-wrap:wrap;margin-bottom:12px;",
      tags$div(style = paste0("flex:1;min-width:150px;text-align:center;border:2px solid ", col,
                              ";border-radius:10px;padding:14px;"),
        tags$div(style = paste0("font-size:28px;font-weight:700;color:", col, ";"),
                 paste0(res$paf_pct, "%")),
        tags$div(style = "font-size:10px;color:#888;", "PAF ajusté"),
        tags$div(style = "font-size:11px;color:#555;",
                 paste0("[", round(res$paf_lo * 100, 1), "% – ", round(res$paf_hi * 100, 1), "%]"))
      ),
      tags$div(style = "flex:1;min-width:150px;text-align:center;background:#f8f9fa;border-radius:10px;padding:14px;",
        tags$div(style = "font-size:28px;font-weight:700;color:#2980b9;", res$or_adj),
        tags$div(style = "font-size:10px;color:#888;", "OR ajusté"),
        tags$div(style = "font-size:11px;color:#555;",
                 paste0("[", res$or_lo, " – ", res$or_hi, "]"))
      ),
      tags$div(style = "flex:1;min-width:150px;text-align:center;background:#f8f9fa;border-radius:10px;padding:14px;",
        tags$div(style = "font-size:28px;font-weight:700;", res$n_cases),
        tags$div(style = "font-size:10px;color:#888;", "Cas / N total"),
        tags$div(style = "font-size:11px;color:#555;", paste0("/ ", res$n_total))
      )
    ),
    tags$div(style = "background:#f0f8ff;border-left:4px solid #008DC9;padding:12px;border-radius:0 8px 8px 0;font-size:12px;",
      res$interpretation)
  )
})

output$epid_paf_details <- renderUI({
  req(epid_paf_res())
  res <- epid_paf_res()
  tags$div(style = "font-size:12px;color:#555;line-height:1.7;",
    tags$b("Détails techniques :"), tags$br(),
    paste0("Méthode : Bruzzi (1985) — PAF = p(expo|cas) × (1 - 1/OR)"), tags$br(),
    paste0("P(expo|cas) = ", res$p_expo_cases), tags$br(),
    if (length(res$confounders) > 0)
      paste0("Ajusté pour : ", paste(res$confounders, collapse = ", "))
    else "Non ajusté (brut)", tags$br(),
    paste0("IC bootstrap : 1000 rééchantillonnages")
  )
})

cat("server_epidemio_expert.R chargé — Server épidémiologie expert\n")
