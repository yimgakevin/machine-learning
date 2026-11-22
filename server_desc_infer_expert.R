# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  server_desc_infer_expert.R — Server logic pour modules expert             ║
# ║  Descriptif : Bootstrap, Robuste, Normalité, Outliers, Distributions,      ║
# ║               Profil groupes, Corrélation avancée                          ║
# ║  Inférentiel : Appariés, Puissance, Bayes, TOST, Régression, Friedman,    ║
# ║                Tailles d'effet, Synthèse                                   ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION A : DESCRIPTIF EXPERT
# ═══════════════════════════════════════════════════════════════════════════════

# ── A1 : Bootstrap IC ─────────────────────────────────────────────────────────
output$desc_boot_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("desc_boot_var", "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_boot_res <- reactiveVal(NULL)

observeEvent(input$run_desc_boot, {
  req(input$desc_boot_var)
  d <- df_f()
  req(input$desc_boot_var %in% names(d))
  withProgress(message = "Bootstrap en cours...", value = 0.5, {
    res <- tryCatch(
      calc_stats_bootstrap(d[[input$desc_boot_var]],
                           B = input$desc_boot_B,
                           conf = input$desc_boot_conf),
      error = function(e) NULL)
    incProgress(0.5)
  })
  desc_boot_res(res)
})

output$desc_boot_table <- renderDT({
  req(desc_boot_res())
  datatable(desc_boot_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 15, ordering = FALSE),
            class = "compact stripe hover") |>
    formatRound(c("Estimation", "IC inf", "IC sup", "Err. std. bootstrap"), 4)
})

output$desc_boot_comparison <- renderUI({
  req(desc_boot_res())
  res <- desc_boot_res()
  moy <- res$Estimation[res$Statistique == "Moyenne"]
  med <- res$Estimation[res$Statistique == "Mediane"]
  trim <- res$Estimation[res$Statistique == "Moyenne tronquee 10%"]
  tags$div(style = "display:flex;gap:12px;flex-wrap:wrap;",
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:12px;",
      tags$div(style = "font-size:22px;font-weight:700;color:#2980b9;", moy),
      tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "Moyenne")),
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:12px;",
      tags$div(style = "font-size:22px;font-weight:700;color:#27ae60;", med),
      tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "Médiane")),
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:12px;",
      tags$div(style = "font-size:22px;font-weight:700;color:#8e44ad;", trim),
      tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "Tronquée 10%"))
  )
})

# ── A2 : Statistiques robustes ────────────────────────────────────────────────
output$desc_robust_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("desc_robust_var", "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_robust_res <- reactiveVal(NULL)

observeEvent(input$run_desc_robust, {
  req(input$desc_robust_var)
  d <- df_f()
  req(input$desc_robust_var %in% names(d))
  res <- tryCatch(calc_robust_stats(d[[input$desc_robust_var]]), error = function(e) NULL)
  desc_robust_res(res)
})

output$desc_robust_table <- renderDT({
  req(desc_robust_res())
  datatable(desc_robust_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 20, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("Type",
      backgroundColor = styleEqual(c("Classique", "Robuste", "Diagnostic"),
                                    c("#e8f4fd", "#f0fdf4", "#fef9e7")),
      fontWeight = "bold")
})

# ── A3 : Batterie de normalité ────────────────────────────────────────────────
output$desc_norm_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("desc_norm_var", "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_norm_res <- reactiveVal(NULL)

observeEvent(input$run_desc_norm, {
  req(input$desc_norm_var)
  d <- df_f()
  req(input$desc_norm_var %in% names(d))
  withProgress(message = "Tests de normalité...", value = 0.5, {
    res <- tryCatch(
      calc_normality_battery(d[[input$desc_norm_var]], alpha = input$desc_norm_alpha),
      error = function(e) NULL)
    incProgress(0.5)
  })
  desc_norm_res(res)
})

output$desc_norm_table <- renderDT({
  req(desc_norm_res())
  datatable(desc_norm_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 10, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("Conclusion",
      color = styleEqual(c("Normale", "Non normale"), c("#27ae60", "#e74c3c")),
      fontWeight = "bold")
})

output$desc_norm_verdict <- renderUI({
  req(desc_norm_res())
  verdict <- attr(desc_norm_res(), "verdict")
  n_obs <- attr(desc_norm_res(), "n_obs")
  is_normal <- grepl("^NORMALE", verdict)
  col <- if (is_normal) "#27ae60" else "#e74c3c"
  ico <- if (is_normal) "check-circle" else "times-circle"
  tags$div(
    style = paste0("background:", col, "15;border:2px solid ", col,
                   ";border-radius:8px;padding:12px;text-align:center;"),
    tags$div(style = paste0("color:", col, ";font-size:14px;font-weight:700;"),
             icon(ico), " ", verdict),
    tags$div(style = "font-size:11px;color:#555;margin-top:4px;",
             paste0("N = ", n_obs, " observations"))
  )
})

output$desc_norm_qqplot <- renderPlotly({
  req(input$desc_norm_var)
  d <- df_f()
  req(input$desc_norm_var %in% names(d))
  x <- na.omit(as.numeric(d[[input$desc_norm_var]]))
  df_qq <- tibble::tibble(val = x)
  sw <- tryCatch(shapiro.test(x[1:min(5000, length(x))]), error = function(e) NULL)

  p <- ggplot(df_qq, aes(sample = val)) +
    stat_qq(color = "#3498db", alpha = 0.5, size = 1.5) +
    stat_qq_line(color = "#e74c3c", linewidth = 0.8) +
    labs(x = "Quantiles théoriques", y = "Quantiles observés",
         title = paste0("QQ-plot — ", input$desc_norm_var),
         subtitle = if (!is.null(sw))
           paste0("Shapiro-Wilk W=", round(sw$statistic, 4), " p=", round(sw$p.value, 4))
         else "") +
    theme_minimal(base_size = 11)
  ggplotly(p)
})

# ── A4 : Détection outliers ──────────────────────────────────────────────────
output$desc_outlier_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("desc_outlier_var", "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_outlier_res <- reactiveVal(NULL)

observeEvent(input$run_desc_outlier, {
  req(input$desc_outlier_var)
  d <- df_f()
  req(input$desc_outlier_var %in% names(d))
  res <- tryCatch(calc_outlier_detection(d[[input$desc_outlier_var]], input$desc_outlier_var),
                  error = function(e) NULL)
  desc_outlier_res(res)
})

output$desc_outlier_table <- renderDT({
  req(desc_outlier_res())
  datatable(desc_outlier_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 10, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("% outliers",
      color = styleInterval(c(5, 10), c("#27ae60", "#f39c12", "#e74c3c")),
      fontWeight = "bold")
})

output$desc_outlier_plot <- renderPlotly({
  req(input$desc_outlier_var)
  d <- df_f()
  req(input$desc_outlier_var %in% names(d))
  plot_outlier_diagnostic(d[[input$desc_outlier_var]], input$desc_outlier_var)
})

# ── A5 : Ajustement de distributions ─────────────────────────────────────────
output$desc_distfit_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("desc_distfit_var", "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_distfit_res <- reactiveVal(NULL)

observeEvent(input$run_desc_distfit, {
  req(input$desc_distfit_var)
  d <- df_f()
  req(input$desc_distfit_var %in% names(d))
  withProgress(message = "Ajustement des distributions...", value = 0.5, {
    res <- tryCatch(calc_distribution_fit(d[[input$desc_distfit_var]]),
                    error = function(e) NULL)
    incProgress(0.5)
  })
  desc_distfit_res(res)
})

output$desc_distfit_table <- renderDT({
  req(desc_distfit_res())
  datatable(desc_distfit_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 10, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("Adequation",
      color = styleEqual(c("Adéquate", "Inadéquate"), c("#27ae60", "#e74c3c")),
      fontWeight = "bold") |>
    formatRound(c("AIC", "BIC", "LogLik"), 2)
})

output$desc_distfit_best <- renderUI({
  req(desc_distfit_res())
  best <- attr(desc_distfit_res(), "best")
  tags$div(
    style = "background:#27ae60;color:white;border-radius:8px;padding:10px;text-align:center;",
    tags$div(style = "font-size:11px;text-transform:uppercase;", "Meilleur ajustement"),
    tags$div(style = "font-size:16px;font-weight:700;", best)
  )
})

output$desc_distfit_plot <- renderPlotly({
  req(input$desc_distfit_var)
  d <- df_f()
  req(input$desc_distfit_var %in% names(d))
  plot_distribution_fit(d[[input$desc_distfit_var]], input$desc_distfit_var)
})

# ── A6 : Profil par groupe ───────────────────────────────────────────────────
output$desc_profile_grp_ui <- renderUI({
  vc <- vars_cat(); req(length(vc) > 0)
  selectInput("desc_profile_grp", "Grouper par :", choices = vc, selected = vc[1], width = "100%")
})

output$desc_profile_vars_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectizeInput("desc_profile_vars", "Variables numériques :",
                 choices = vn, selected = head(vn, 8), multiple = TRUE, width = "100%")
})

desc_profile_res <- reactiveVal(NULL)

observeEvent(input$run_desc_profile, {
  req(input$desc_profile_grp, input$desc_profile_vars)
  d <- df_f()
  res <- tryCatch(calc_group_profile(d, input$desc_profile_vars, input$desc_profile_grp),
                  error = function(e) NULL)
  desc_profile_res(res)
})

output$desc_profile_table <- renderDT({
  req(desc_profile_res())
  datatable(desc_profile_res(), rownames = FALSE,
            options = list(dom = "tip", pageLength = 20, scrollX = TRUE),
            class = "compact stripe hover") |>
    formatRound(c("Moyenne", "Mediane", "Ec-type", "CV (%)", "Min", "Q1", "Q3", "Max", "Skew", "Kurt"), 2)
})

output$desc_profile_heatmap <- renderPlotly({
  req(input$desc_profile_grp, input$desc_profile_vars)
  d <- df_f()
  plot_group_heatmap(d, input$desc_profile_vars, input$desc_profile_grp)
})

# ── A7 : Corrélation avancée ─────────────────────────────────────────────────
output$desc_cor_adv_vars_ui <- renderUI({
  vn <- vars_num(); req(length(vn) >= 2)
  selectizeInput("desc_cor_adv_vars", "Variables :",
                 choices = vn, selected = head(vn, 8), multiple = TRUE, width = "100%")
})

desc_cor_adv_res <- reactiveVal(NULL)

observeEvent(input$run_desc_cor_adv, {
  req(input$desc_cor_adv_vars, length(input$desc_cor_adv_vars) >= 2)
  d <- df_f()
  withProgress(message = "Calcul des corrélations...", value = 0.5, {
    res <- tryCatch(
      calc_correlation_matrix_advanced(d, input$desc_cor_adv_vars,
                                       method = input$desc_cor_method,
                                       p_adjust = input$desc_cor_padj),
      error = function(e) NULL)
    incProgress(0.5)
  })
  desc_cor_adv_res(res)
})

output$desc_cor_adv_table <- renderDT({
  req(desc_cor_adv_res())
  tbl <- format_cor_table(desc_cor_adv_res())
  req(tbl)
  datatable(tbl, rownames = FALSE,
            options = list(dom = "tip", pageLength = 20, scrollX = TRUE),
            class = "compact stripe hover") |>
    formatRound("r", 4) |>
    formatStyle("Significative",
      color = styleEqual(c("Oui", "Non"), c("#27ae60", "#95a5a6")),
      fontWeight = "bold") |>
    formatStyle("Force",
      backgroundColor = styleEqual(
        c("Très forte", "Forte", "Modérée", "Faible", "Négligeable"),
        c("#e74c3c30", "#f39c1230", "#f1c40f30", "#3498db20", "#ecf0f120")))
})

output$desc_cor_adv_summary <- renderUI({
  req(desc_cor_adv_res())
  res <- desc_cor_adv_res()
  tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
    tags$b("Résumé :"), tags$br(),
    paste0("N = ", res$n, " observations"), tags$br(),
    paste0(res$n_tests, " paires testées"), tags$br(),
    paste0(res$n_sig_raw, " significatives (brut)"), tags$br(),
    paste0(res$n_sig_adj, " significatives (après ", res$p_adjust, ")")
  )
})

output$desc_cor_adv_plot <- renderPlot({
  req(desc_cor_adv_res())
  res <- desc_cor_adv_res()
  # Masquer les non-significatives
  sig_mask <- res$pval_adj < 0.05
  cor_display <- res$cor
  cor_display[!sig_mask & !diag(nrow(sig_mask))] <- NA

  corrplot::corrplot(res$cor, method = "color", type = "upper",
                     addCoef.col = "black", number.cex = 0.65,
                     tl.cex = 0.72, tl.col = "#00426A", tl.srt = 45,
                     col = colorRampPalette(c("#e74c3c", "white", "#2980b9"))(200),
                     p.mat = res$pval_adj, sig.level = 0.05,
                     insig = "cross", pch.cex = 1.5,
                     mar = c(0, 0, 2, 0),
                     title = paste0("Corrélation ", res$method, " (× = non sig. après ", res$p_adjust, ")"))
}, height = 450)


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION B : INFÉRENTIEL EXPERT
# ═══════════════════════════════════════════════════════════════════════════════

# ── B1 : Tests appariés ──────────────────────────────────────────────────────
output$infer_paired_x1_ui <- renderUI({
  vn <- vars_num(); req(length(vn) >= 2)
  selectInput("infer_paired_x1", "Mesure 1 :", choices = vn, selected = vn[1], width = "100%")
})

output$infer_paired_x2_ui <- renderUI({
  vn <- vars_num(); req(length(vn) >= 2)
  sel <- if (length(vn) > 1) vn[2] else vn[1]
  selectInput("infer_paired_x2", "Mesure 2 :", choices = vn, selected = sel, width = "100%")
})

infer_paired_res <- reactiveVal(NULL)

observeEvent(input$run_infer_paired, {
  req(input$infer_paired_x1, input$infer_paired_x2)
  d <- df_f()
  req(input$infer_paired_x1 %in% names(d), input$infer_paired_x2 %in% names(d))
  withProgress(message = "Tests appariés...", value = 0.5, {
    res <- tryCatch(
      run_paired_tests(d[[input$infer_paired_x1]], d[[input$infer_paired_x2]],
                       input$infer_paired_x1, input$infer_paired_x2,
                       alpha = input$infer_paired_alpha),
      error = function(e) NULL)
    incProgress(0.5)
  })
  infer_paired_res(res)
})

output$infer_paired_cards <- renderUI({
  req(infer_paired_res())
  render_all_cards(infer_paired_res())
})

output$infer_paired_plot <- renderPlotly({
  req(input$infer_paired_x1, input$infer_paired_x2)
  d <- df_f()
  x1 <- d[[input$infer_paired_x1]]; x2 <- d[[input$infer_paired_x2]]
  complete <- complete.cases(x1, x2)
  diff_vals <- x1[complete] - x2[complete]
  df_diff <- tibble::tibble(diff = diff_vals)

  p <- ggplot(df_diff, aes(x = diff)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "#3498db", color = "white", alpha = 0.6) +
    geom_density(color = "#2c3e50", linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
    geom_vline(xintercept = mean(diff_vals), linetype = "dotted",
               color = "#27ae60", linewidth = 0.8) +
    labs(x = paste0("Différence (", input$infer_paired_x1, " - ", input$infer_paired_x2, ")"),
         y = "Densité",
         title = "Distribution des différences appariées",
         caption = "Rouge = 0 | Vert = moyenne") +
    theme_minimal(base_size = 11)
  ggplotly(p)
})

# ── B2 : Puissance statistique ──────────────────────────────────────────────
infer_power_res <- reactiveVal(NULL)

observeEvent(input$run_infer_power, {
  k_val <- if (input$infer_power_type %in% c("anova", "chi2")) input$infer_power_k else NULL
  res <- tryCatch(
    calc_power_analysis(input$infer_power_type, input$infer_power_n,
                        input$infer_power_effect, input$infer_power_alpha, k_val),
    error = function(e) NULL)
  infer_power_res(res)
})

output$infer_power_result <- renderUI({
  req(infer_power_res())
  res <- infer_power_res()
  power_pct <- as.numeric(sub("%", "", res$`Puissance (%)`))
  col <- if (power_pct >= 80) "#27ae60" else if (power_pct >= 60) "#f39c12" else "#e74c3c"
  tags$div(
    style = paste0("background:", col, "15;border:2px solid ", col,
                   ";border-radius:8px;padding:12px;text-align:center;"),
    tags$div(style = paste0("color:", col, ";font-size:24px;font-weight:700;"),
             res$`Puissance (%)`),
    tags$div(style = "font-size:11px;color:#555;", res$Interpretation),
    if (!is.na(res$`N requis (80%)`))
      tags$div(style = "font-size:11px;color:#2980b9;margin-top:4px;",
               paste0("N minimum pour 80% : ", res$`N requis (80%)`, " par groupe"))
  )
})

output$infer_power_curve <- renderPlotly({
  req(input$infer_power_type, input$infer_power_effect)
  k_val <- if (input$infer_power_type %in% c("anova", "chi2")) input$infer_power_k else NULL
  plot_power_curve(input$infer_power_type, input$infer_power_effect,
                   input$infer_power_alpha, k_val)
})

output$infer_power_grid <- renderDT({
  req(input$infer_power_type, input$infer_power_effect)
  k_val <- if (input$infer_power_type %in% c("anova", "chi2")) input$infer_power_k else NULL

  n_vals <- c(20, 30, 50, 75, 100, 150, 200, 300, 500)
  eff_vals <- c(0.2, 0.3, 0.5, 0.8, 1.0)

  grid <- expand.grid(N = n_vals, Effet = eff_vals)
  grid$Puissance <- mapply(function(n, e) {
    res <- calc_power_analysis(input$infer_power_type, n, e, input$infer_power_alpha, k_val)
    if (is.null(res)) NA else round(res$`Puissance (1-β)` * 100, 1)
  }, grid$N, grid$Effet)

  grid_wide <- grid |>
    tidyr::pivot_wider(names_from = Effet, values_from = Puissance,
                       names_prefix = "d=")
  datatable(grid_wide, rownames = FALSE,
            options = list(dom = "t", pageLength = 15),
            class = "compact stripe hover") |>
    formatStyle(names(grid_wide)[-1],
      backgroundColor = styleInterval(c(60, 80), c("#fef2f2", "#fef9e7", "#f0fdf4")),
      fontWeight = "bold")
})

# ── B3 : Bayes Factor ───────────────────────────────────────────────────────
output$infer_bf_x_ui <- renderUI({
  all_vars <- c(vars_cat(), vars_num())
  selectInput("infer_bf_x", "Variable X :", choices = all_vars,
              selected = all_vars[1], width = "100%")
})

output$infer_bf_y_ui <- renderUI({
  all_vars <- c(vars_num(), vars_cat())
  sel <- if (length(all_vars) > 1) all_vars[2] else all_vars[1]
  selectInput("infer_bf_y", "Variable Y :", choices = all_vars,
              selected = sel, width = "100%")
})

infer_bf_res <- reactiveVal(NULL)

observeEvent(input$run_infer_bf, {
  req(input$infer_bf_x, input$infer_bf_y)
  d <- df_f()
  req(input$infer_bf_x %in% names(d), input$infer_bf_y %in% names(d))
  withProgress(message = "Calcul Bayes Factor...", value = 0.5, {
    res <- tryCatch(
      calc_bayes_factor(d, input$infer_bf_x, input$infer_bf_y),
      error = function(e) NULL)
    incProgress(0.5)
  })
  infer_bf_res(res)
})

output$infer_bf_table <- renderDT({
  req(infer_bf_res())
  tbl <- format_bf_results(infer_bf_res())
  req(tbl)
  datatable(tbl, rownames = FALSE,
            options = list(dom = "t", pageLength = 10),
            class = "compact stripe hover") |>
    formatStyle("Direction",
      color = styleEqual(c("En faveur de H1", "En faveur de H0"),
                          c("#e74c3c", "#27ae60")),
      fontWeight = "bold")
})

output$infer_bf_interpretation <- renderUI({
  req(infer_bf_res())
  bf_list <- infer_bf_res()
  cards <- lapply(bf_list, function(bf) {
    col <- if (bf$bf10 > 3) "#e74c3c" else if (bf$bf10 > 1) "#f39c12" else "#27ae60"
    tags$div(
      style = paste0("border:2px solid ", col, ";border-radius:8px;padding:12px;margin-bottom:8px;"),
      tags$div(style = paste0("font-weight:700;color:", col, ";font-size:13px;"), bf$test),
      tags$div(style = "font-size:12px;color:#333;margin-top:4px;",
        paste0("BF10 = ", signif(bf$bf10, 3), " → ", bf$interpretation)),
      tags$div(style = "font-size:11px;color:#555;margin-top:2px;",
        paste0("log10(BF) = ", round(log10(bf$bf10), 2)))
    )
  })
  tagList(cards)
})

# ── B4 : Équivalence TOST ───────────────────────────────────────────────────
output$infer_tost_grp_ui <- renderUI({
  vc <- vars_cat(); req(length(vc) > 0)
  selectInput("infer_tost_grp", "Variable de groupe (2 niveaux) :",
              choices = vc, selected = vc[1], width = "100%")
})

output$infer_tost_y_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("infer_tost_y", "Variable continue :",
              choices = vn, selected = vn[1], width = "100%")
})

infer_tost_res <- reactiveVal(NULL)

observeEvent(input$run_infer_tost, {
  req(input$infer_tost_grp, input$infer_tost_y)
  d <- df_f()
  req(input$infer_tost_grp %in% names(d), input$infer_tost_y %in% names(d))

  grps <- split(d[[input$infer_tost_y]], d[[input$infer_tost_grp]])
  grps <- grps[sapply(grps, function(g) sum(!is.na(g)) >= 5)]
  req(length(grps) == 2)

  delta <- if (!is.na(input$infer_tost_delta) && input$infer_tost_delta > 0)
    input$infer_tost_delta else NULL

  res <- tryCatch(
    run_tost_test(grps[[1]], grps[[2]], names(grps)[1], names(grps)[2],
                  delta = delta, alpha = input$infer_tost_alpha),
    error = function(e) NULL)
  infer_tost_res(res)
})

output$infer_tost_result <- renderUI({
  req(infer_tost_res())
  res <- infer_tost_res()
  col <- if (res$equivalent) "#27ae60" else "#e74c3c"
  ico <- if (res$equivalent) "check-circle" else "times-circle"

  tags$div(
    style = paste0("border:2px solid ", col, ";border-radius:10px;padding:16px;"),
    tags$div(style = paste0("font-size:15px;font-weight:700;color:", col, ";margin-bottom:8px;"),
             icon(ico), " ", if (res$equivalent) "GROUPES ÉQUIVALENTS" else "ÉQUIVALENCE NON DÉMONTRÉE"),
    tags$div(style = "display:flex;gap:16px;flex-wrap:wrap;margin-bottom:8px;",
      tags$div(style = "flex:1;text-align:center;background:#f8f9fa;border-radius:6px;padding:8px;",
        tags$div(style = "font-size:18px;font-weight:700;", res$diff_moyennes),
        tags$div(style = "font-size:10px;color:#888;", "Diff. moyennes")),
      tags$div(style = "flex:1;text-align:center;background:#f8f9fa;border-radius:6px;padding:8px;",
        tags$div(style = "font-size:18px;font-weight:700;", paste0("±", res$delta)),
        tags$div(style = "font-size:10px;color:#888;", "Marge Δ")),
      tags$div(style = "flex:1;text-align:center;background:#f8f9fa;border-radius:6px;padding:8px;",
        tags$div(style = paste0("font-size:18px;font-weight:700;color:", col, ";"), res$p_tost),
        tags$div(style = "font-size:10px;color:#888;", "p TOST"))
    ),
    tags$div(style = "font-size:12px;color:#333;", paste0("IC 90% : ", res$ci_90)),
    tags$div(style = "font-size:12px;color:#555;margin-top:4px;", res$conclusion)
  )
})

output$infer_tost_plot <- renderPlotly({
  req(infer_tost_res())
  plot_tost(infer_tost_res())
})

# ── B5 : Régression + Diagnostics ───────────────────────────────────────────
output$infer_reg_outcome_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("infer_reg_outcome", "Variable réponse (Y) :",
              choices = vn, selected = vn[1], width = "100%")
})

output$infer_reg_pred_ui <- renderUI({
  vn <- vars_num(); req(length(vn) >= 2)
  sel <- setdiff(vn, input$infer_reg_outcome)
  selectizeInput("infer_reg_pred", "Prédicteurs (X) :",
                 choices = sel, selected = head(sel, 5), multiple = TRUE, width = "100%")
})

infer_reg_res <- reactiveVal(NULL)

observeEvent(input$run_infer_reg, {
  req(input$infer_reg_outcome, input$infer_reg_pred)
  d <- df_f()
  withProgress(message = "Ajustement de la régression...", value = 0.5, {
    res <- tryCatch(
      run_regression_diagnostics(d, input$infer_reg_outcome, input$infer_reg_pred),
      error = function(e) NULL)
    incProgress(0.5)
  })
  infer_reg_res(res)
})

output$infer_reg_coef_table <- renderDT({
  req(infer_reg_res())
  coefs <- infer_reg_res()$coefficients |>
    dplyr::mutate(across(where(is.numeric), ~round(., 4)))
  datatable(coefs, rownames = FALSE,
            options = list(dom = "t", pageLength = 20),
            class = "compact stripe hover") |>
    formatStyle("Significatif",
      color = styleEqual(c(TRUE, FALSE), c("#e74c3c", "#95a5a6")),
      fontWeight = "bold")
})

output$infer_reg_forest <- renderPlotly({
  req(infer_reg_res())
  plot_regression_forest(infer_reg_res())
})

output$infer_reg_diag_table <- renderDT({
  req(infer_reg_res())
  diag <- infer_reg_res()$diagnostics
  tbl <- tibble::tibble(
    Diagnostic = c("R²", "R² ajusté", "F global", "p (modèle)", "AIC", "BIC",
                   "RMSE", "MAE", "N observations", "N influents (Cook)",
                   "Durbin-Watson", "DW p-val", "Breusch-Pagan χ²", "BP p-val",
                   "Shapiro résidus p"),
    Valeur = as.character(c(diag$R2, diag$R2_adj, diag$F_stat, diag$F_pval,
                            diag$AIC, diag$BIC, diag$RMSE, diag$MAE, diag$n_obs,
                            diag$n_influential_cook, diag$durbin_watson, diag$dw_pval,
                            diag$bp_stat, diag$bp_pval, diag$resid_normal_p)),
    Interpretation = c(
      paste0(round(diag$R2 * 100, 1), "% de variance expliquée"),
      paste0("Corrigé pour ", length(input$infer_reg_pred), " prédicteurs"),
      if (diag$F_pval < 0.05) "Modèle globalement significatif" else "Modèle non significatif",
      "", "", "",
      paste0("Erreur moyenne = ", diag$RMSE),
      paste0("Erreur absolue moyenne = ", diag$MAE),
      "", paste0(diag$n_influential_cook, " obs. avec Cook > 4/n"),
      if (!is.na(diag$durbin_watson)) {
        if (abs(diag$durbin_watson - 2) < 0.5) "Pas d'autocorrélation" else "Autocorrélation possible"
      } else "",
      "",
      if (!is.na(diag$bp_pval)) {
        if (diag$bp_pval >= 0.05) "Homoscédasticité ✓" else "Hétéroscédasticité ✗"
      } else "",
      "",
      if (!is.na(diag$resid_normal_p)) {
        if (diag$resid_normal_p >= 0.05) "Résidus normaux ✓" else "Résidus non normaux ✗"
      } else ""
    )
  )
  datatable(tbl, rownames = FALSE,
            options = list(dom = "t", pageLength = 20),
            class = "compact stripe hover")
})

output$infer_reg_diag_plots <- renderPlot({
  req(infer_reg_res())
  plot_regression_diagnostics(infer_reg_res())
}, height = 600)

output$infer_reg_vif_table <- renderDT({
  req(infer_reg_res())
  vif_vals <- infer_reg_res()$vif
  req(!is.null(vif_vals))
  tbl <- tibble::tibble(
    Variable = names(vif_vals),
    VIF = round(vif_vals, 3),
    `Tolérance (1/VIF)` = round(1 / vif_vals, 3),
    Diagnostic = dplyr::case_when(
      vif_vals > 10 ~ "Multicolinéarité SÉVÈRE",
      vif_vals > 5  ~ "Multicolinéarité modérée",
      vif_vals > 2  ~ "Légère corrélation",
      TRUE ~ "Acceptable"
    )
  )
  datatable(tbl, rownames = FALSE,
            options = list(dom = "t", pageLength = 20),
            class = "compact stripe hover") |>
    formatStyle("Diagnostic",
      color = styleEqual(
        c("Multicolinéarité SÉVÈRE", "Multicolinéarité modérée", "Légère corrélation", "Acceptable"),
        c("#e74c3c", "#f39c12", "#f1c40f", "#27ae60")),
      fontWeight = "bold")
})

output$infer_reg_vif_interp <- renderUI({
  req(infer_reg_res())
  vif_vals <- infer_reg_res()$vif
  if (is.null(vif_vals)) return(NULL)
  max_vif <- max(vif_vals)
  col <- if (max_vif > 10) "#e74c3c" else if (max_vif > 5) "#f39c12" else "#27ae60"
  tags$div(style = paste0("border-left:4px solid ", col, ";padding:8px 12px;background:", col, "10;border-radius:4px;"),
    tags$div(style = paste0("color:", col, ";font-weight:700;"),
             paste0("VIF max = ", round(max_vif, 2))),
    tags$div(style = "font-size:11px;color:#555;",
      if (max_vif > 10) "Multicolinéarité sévère — envisager de retirer une variable"
      else if (max_vif > 5) "Multicolinéarité modérée — vérifier les corrélations"
      else "Pas de problème de multicolinéarité")
  )
})

output$infer_reg_summary <- renderUI({
  req(infer_reg_res())
  diag <- infer_reg_res()$diagnostics
  col <- if (diag$R2_adj > 0.5) "#27ae60" else if (diag$R2_adj > 0.2) "#f39c12" else "#e74c3c"
  tags$div(
    style = paste0("border:2px solid ", col, ";border-radius:8px;padding:10px;text-align:center;"),
    tags$div(style = paste0("font-size:22px;font-weight:700;color:", col, ";"),
             paste0(round(diag$R2_adj * 100, 1), "%")),
    tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "R² ajusté"),
    tags$div(style = "font-size:11px;color:#555;margin-top:4px;",
             paste0("F=", diag$F_stat, " p=", diag$F_pval))
  )
})

# ── B6 : Mesures répétées ───────────────────────────────────────────────────
output$infer_repeat_vars_ui <- renderUI({
  vn <- vars_num(); req(length(vn) >= 3)
  selectizeInput("infer_repeat_vars", "Mesures répétées (≥ 3) :",
                 choices = vn, selected = head(vn, 3), multiple = TRUE, width = "100%")
})

infer_repeat_res <- reactiveVal(NULL)

observeEvent(input$run_infer_repeat, {
  req(input$infer_repeat_vars, length(input$infer_repeat_vars) >= 3)
  d <- df_f()
  withProgress(message = "Test de Friedman...", value = 0.5, {
    res <- tryCatch(
      run_friedman_test(d, input$infer_repeat_vars, alpha = input$infer_repeat_alpha),
      error = function(e) NULL)
    incProgress(0.5)
  })
  infer_repeat_res(res)
})

output$infer_repeat_result <- renderUI({
  req(infer_repeat_res())
  res <- infer_repeat_res()
  col <- if (res$sig) "#e74c3c" else "#27ae60"
  tags$div(
    style = paste0("border:2px solid ", col, ";border-radius:10px;padding:14px;"),
    tags$div(style = paste0("font-size:14px;font-weight:700;color:", col, ";"),
             res$nom),
    tags$div(style = "display:flex;gap:12px;margin-top:8px;flex-wrap:wrap;",
      tags$div(style = "flex:1;text-align:center;background:#f8f9fa;border-radius:6px;padding:8px;",
        tags$div(style = "font-size:18px;font-weight:700;", res$stat),
        tags$div(style = "font-size:10px;color:#888;", paste0("χ² (ddl=", res$df, ")"))),
      tags$div(style = "flex:1;text-align:center;background:#f8f9fa;border-radius:6px;padding:8px;",
        tags$div(style = paste0("font-size:18px;font-weight:700;color:", col, ";"), res$p_val),
        tags$div(style = "font-size:10px;color:#888;", "p-valeur")),
      tags$div(style = "flex:1;text-align:center;background:#f8f9fa;border-radius:6px;padding:8px;",
        tags$div(style = "font-size:18px;font-weight:700;", res$W_kendall),
        tags$div(style = "font-size:10px;color:#888;", paste0("W Kendall — ", res$W_interp)))
    ),
    tags$div(style = "font-size:12px;color:#333;margin-top:8px;", res$conclusion),
    tags$div(style = "font-size:11px;color:#555;",
             paste0("N=", res$n_sujets, " sujets × ", res$k_mesures, " mesures"))
  )
})

output$infer_repeat_posthoc <- renderDT({
  req(infer_repeat_res())
  ph <- infer_repeat_res()$posthoc
  req(!is.null(ph))
  datatable(ph, rownames = FALSE,
            options = list(dom = "t", pageLength = 20),
            class = "compact stripe hover") |>
    formatStyle("Significatif",
      color = styleEqual(c("Oui", "Non"), c("#e74c3c", "#95a5a6")),
      fontWeight = "bold")
})

output$infer_repeat_plot <- renderPlotly({
  req(input$infer_repeat_vars)
  d <- df_f()
  vars <- intersect(input$infer_repeat_vars, names(d))
  req(length(vars) >= 3)

  df_long <- d |>
    dplyr::select(dplyr::all_of(vars)) |>
    tidyr::pivot_longer(everything(), names_to = "Mesure", values_to = "Valeur")

  p <- ggplot(df_long, aes(x = Mesure, y = Valeur, fill = Mesure)) +
    geom_violin(alpha = 0.4, trim = FALSE) +
    geom_boxplot(width = 0.15, fill = "white", alpha = 0.8,
                 outlier.shape = 21, outlier.color = "#e74c3c") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#e74c3c") +
    labs(x = "", y = "Valeur", title = "Distribution par mesure") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  ggplotly(p)
})

# ── B7 : Tailles d'effet avec IC ────────────────────────────────────────────
output$infer_es_x_ui <- renderUI({
  vc <- vars_cat(); req(length(vc) > 0)
  selectInput("infer_es_x", "Variable de groupe :", choices = vc, selected = vc[1], width = "100%")
})

output$infer_es_y_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput("infer_es_y", "Variable continue :", choices = vn, selected = vn[1], width = "100%")
})

infer_es_res <- reactiveVal(NULL)

observeEvent(input$run_infer_es, {
  req(input$infer_es_x, input$infer_es_y)
  d <- df_f()
  req(input$infer_es_x %in% names(d), input$infer_es_y %in% names(d))
  withProgress(message = "Calcul tailles d'effet + bootstrap...", value = 0.5, {
    res <- tryCatch(
      calc_effect_size_ci(d, input$infer_es_x, input$infer_es_y, B = input$infer_es_B),
      error = function(e) NULL)
    incProgress(0.5)
  })
  infer_es_res(res)
})

output$infer_es_table <- renderDT({
  req(infer_es_res())
  datatable(infer_es_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 10),
            class = "compact stripe hover") |>
    formatRound(c("Estimation", "IC inf", "IC sup"), 4)
})

output$infer_es_interpretation <- renderUI({
  req(infer_es_res())
  es <- infer_es_res()
  d_val <- es$Estimation[es$`Taille d'effet` == "Cohen d"]
  cles <- es$Estimation[es$`Taille d'effet` == "CLES (prob. supériorité)"]

  tags$div(style = "display:flex;gap:12px;flex-wrap:wrap;",
    tags$div(style = "flex:1;min-width:150px;border:2px solid #2980b9;border-radius:8px;padding:12px;text-align:center;",
      tags$div(style = "font-size:24px;font-weight:700;color:#2980b9;", round(d_val, 3)),
      tags$div(style = "font-size:10px;color:#888;", "Cohen d"),
      tags$div(style = "font-size:11px;color:#555;", interp_cohen_d(d_val))),
    tags$div(style = "flex:1;min-width:150px;border:2px solid #27ae60;border-radius:8px;padding:12px;text-align:center;",
      tags$div(style = "font-size:24px;font-weight:700;color:#27ae60;",
               paste0(round(cles * 100, 1), "%")),
      tags$div(style = "font-size:10px;color:#888;", "CLES"),
      tags$div(style = "font-size:11px;color:#555;", "Prob. supériorité"))
  )
})

# ── B8 : Synthèse expert ────────────────────────────────────────────────────
# Collecte automatique des p-values pour corrections multiples
expert_p_values <- reactiveVal(list())

# Observer qui collecte les p-values de tous les tests
observe({
  p_list <- list()

  # Depuis tests inférentiels classiques
  res <- infer_results()
  if (!is.null(res) && is.null(res$error)) {
    for (nm in names(res)) {
      if (isTRUE(res[[nm]]$valide)) {
        p_list[[res[[nm]]$nom]] <- res[[nm]]$p_val
      }
    }
  }

  # Depuis tests appariés
  paired <- infer_paired_res()
  if (!is.null(paired)) {
    for (nm in names(paired)) {
      if (isTRUE(paired[[nm]]$valide)) {
        p_list[[paired[[nm]]$nom]] <- paired[[nm]]$p_val
      }
    }
  }

  if (length(p_list) > 0) expert_p_values(p_list)
})

output$infer_expert_corrections_table <- renderDT({
  req(length(expert_p_values()) > 1)
  p_vals <- unlist(expert_p_values())
  tbl <- apply_multiple_corrections(p_vals, names(p_vals))
  datatable(tbl, rownames = FALSE,
            options = list(dom = "t", pageLength = 20, scrollX = TRUE),
            class = "compact stripe hover") |>
    formatStyle("Sig (Bonf)",
      color = styleEqual(c(TRUE, FALSE), c("#e74c3c", "#95a5a6")),
      fontWeight = "bold")
})

output$infer_expert_summary_table <- renderDT({
  # Collecter tous les résultats disponibles
  freq_tests <- infer_results()
  bf_res <- infer_bf_res()
  power_res <- infer_power_res()
  tost_res <- infer_tost_res()

  bf_formatted <- if (!is.null(bf_res)) format_bf_results(bf_res) else NULL

  tbl <- format_expert_summary(
    freq_tests %||% list(),
    bf_formatted,
    power_res,
    tost_res
  )

  req(nrow(tbl) > 0)
  datatable(tbl, rownames = FALSE,
            options = list(dom = "t", pageLength = 30, scrollX = TRUE),
            class = "compact stripe hover") |>
    formatStyle("Approche",
      backgroundColor = styleEqual(
        c("Fréquentiste", "Bayésien", "Puissance", "Équivalence"),
        c("#e8f4fd", "#fef2f2", "#fef9e7", "#f0fdf4")),
      fontWeight = "bold")
})

output$dl_expert_synthesis <- downloadHandler(
  filename = function() paste0("synthese_expert_", Sys.Date(), ".csv"),
  content = function(file) {
    freq_tests <- infer_results() %||% list()
    bf_formatted <- if (!is.null(infer_bf_res())) format_bf_results(infer_bf_res()) else NULL
    tbl <- format_expert_summary(freq_tests, bf_formatted, infer_power_res(), infer_tost_res())
    write.csv(tbl, file, row.names = FALSE)
  }
)

cat("server_desc_infer_expert.R chargé — Logic serveur descriptif + inférentiel expert\n")
