# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  server_temporel_expert.R — Logique Serveur Expert Temporel               ║
# ║  Sourcé avec local=TRUE dans mod_temporal_tests.R / moduleServer()        ║
# ║  Accède à: input, output, session, ns, df_f(), map_cols(), survey_col()   ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Helpers partagés ──────────────────────────────────────────────────────────
vars_num_temp <- reactive({
  d <- df_f(); req(d)
  names(d)[sapply(d, is.numeric)]
})

vars_all_temp <- reactive({
  d <- df_f(); req(d)
  names(d)
})

vars_cat_temp <- reactive({
  d <- df_f(); req(d)
  names(d)[sapply(d, function(x) is.character(x) || is.factor(x) ||
                    (is.numeric(x) && dplyr::n_distinct(x) <= 20))]
})

# Vagues actuelles sélectionnées (partagées depuis module parent)
current_r1 <- reactive({ input$temporal_r1 })
current_r2 <- reactive({ input$temporal_r2 })
current_round_col <- reactive({ input$temporal_vague_col })

# ═══════════════════════════════════════════════════════════════════════════════
# TAB 1: COMPARAISON MOYENNES CONTINUES
# ═══════════════════════════════════════════════════════════════════════════════

output$temp_cont_vars_ui <- renderUI({
  vn <- vars_num_temp(); req(length(vn) > 0)
  # [S1] Pré-sélection heuristique : d'abord map_cols, puis regex patterns
  m <- tryCatch(map_cols(), error = function(e) list())
  mapped <- unlist(m[c("fcs", "muac", "poids", "taille", "age")], use.names = FALSE)
  mapped <- mapped[!is.null(mapped) & mapped %in% vn]
  if (length(mapped) == 0) {
    # Fallback regex : patterns courants dans les enquêtes SMART/nutrition
    patt <- "fcs|muac|poids|weight|taille|height|age|rcsi|hhs|hdds|brachial|conso|score"
    mapped <- grep(patt, vn, ignore.case = TRUE, value = TRUE)
  }
  sel <- if (length(mapped) > 0) mapped[1:min(5, length(mapped))] else vn[1:min(3, length(vn))]
  selectizeInput(ns("temp_cont_vars"), "Variables numériques :",
                 choices = vn, selected = sel,
                 multiple = TRUE, width = "100%",
                 options = list(maxItems = 15, placeholder = "Sélectionner..."))
})

temp_cont_res <- reactiveVal(NULL)

observeEvent(input$run_temp_continu, {
  req(current_round_col(), current_r1(), current_r2(), input$temp_cont_vars)
  # [S3] Validation : vagues différentes
  validate(need(current_r1() != current_r2(), "Veuillez sélectionner deux vagues différentes."))
  d <- df_f(); req(d)

  withProgress(message = "Comparaison des moyennes...", value = 0.3, {
    res <- tryCatch(
      run_continuous_comparison(d, current_round_col(), current_r1(), current_r2(),
                                input$temp_cont_vars, input$temp_cont_conf),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL }
    )
    incProgress(0.7)
  })
  temp_cont_res(res)
  if (!is.null(res)) showNotification("✅ Comparaison terminée", type = "message")
})

output$temp_cont_table <- renderDT({
  res <- temp_cont_res(); req(res)
  df_show <- res[, c("Variable", "n_V1", "Moy_V1", "ET_V1", "n_V2", "Moy_V2",
                       "ET_V2", "Diff_moy", "IC_lo", "IC_hi", "p_t", "p_wilcox",
                       "Cohen_d", "d_IC_lo", "d_IC_hi", "Effet")]
  df_show$p_t <- format.pval(df_show$p_t, digits = 4)
  df_show$p_wilcox <- format.pval(df_show$p_wilcox, digits = 4)

  datatable(df_show, rownames = FALSE, filter = "top",
            options = list(pageLength = 15, dom = "ftp", scrollX = TRUE,
                           columnDefs = list(list(className = "dt-center", targets = "_all"))),
            class = "compact stripe hover") |>
    formatStyle("Effet",
                backgroundColor = styleEqual(
                  c("Négligeable", "Petit", "Moyen", "Grand", "Effectifs insuffisants"),
                  c("#e8f5e9", "#fff9c4", "#ffe0b2", "#ffcdd2", "#f5f5f5")),
                fontWeight = "bold")
})

output$temp_cont_forest <- renderPlotly({
  res <- temp_cont_res(); req(res)
  plot_continuous_forest(res)
})

output$temp_cont_summary <- renderUI({
  res <- temp_cont_res(); req(res)
  n_sig <- sum(!is.na(res$p_t) & res$p_t < 0.05)
  n_total <- nrow(res)
  col <- if (n_sig > 0) "#e88400" else "#4dac26"

  tags$div(style = paste0("border-left:4px solid ", col,
                            ";padding:12px 16px;background:#f8f9fa;border-radius:6px;margin-top:12px;"),
    tags$strong(style = paste0("color:", col, ";"),
                icon("chart-bar"), paste0(" ", n_sig, "/", n_total,
                                           " variables avec différence significative (α=0.05)")),
    tags$br(),
    tags$span(style = "font-size:12px;color:#666;",
              "Tests utilisés : Welch t-test (paramétrique) + Wilcoxon rank-sum (non-paramétrique). ",
              "Tailles d'effet : Cohen d avec IC bootstrap (1000 rééchantillonnages).")
  )
})


# ═══════════════════════════════════════════════════════════════════════════════
# TAB 2: DIFFERENCE-IN-DIFFERENCES
# ═══════════════════════════════════════════════════════════════════════════════

output$did_outcome_ui <- renderUI({
  vn <- vars_num_temp(); req(length(vn) > 0)
  selectInput(ns("did_outcome"), "Variable résultat :", choices = vn, width = "100%")
})

output$did_group_ui <- renderUI({
  vc <- vars_cat_temp(); req(length(vc) > 0)
  selectInput(ns("did_group_col"), "Colonne groupe (traitement/contrôle) :",
              choices = vc, width = "100%")
})

output$did_treat_ui <- renderUI({
  d <- df_f(); req(d, input$did_group_col)
  col <- input$did_group_col
  req(col %in% names(d))
  vals <- sort(unique(na.omit(d[[col]])))
  selectInput(ns("did_treat_label"), "Label du groupe traitement :",
              choices = vals, selected = vals[1], width = "100%")
})

output$did_control_ui <- renderUI({
  d <- df_f(); req(d, input$did_group_col)
  col <- input$did_group_col
  req(col %in% names(d))
  vals <- sort(unique(na.omit(d[[col]])))
  sel <- if (length(vals) >= 2) vals[2] else vals[1]
  selectInput(ns("did_control_label"), "Label du groupe contrôle :",
              choices = vals, selected = sel, width = "100%")
})

did_res <- reactiveVal(NULL)

observeEvent(input$run_did, {
  req(current_round_col(), current_r1(), current_r2(),
      input$did_outcome, input$did_group_col,
      input$did_treat_label, input$did_control_label)
  # [S3] Validation : vagues et groupes différents
  validate(
    need(current_r1() != current_r2(), "Veuillez sélectionner deux vagues différentes."),
    need(input$did_treat_label != input$did_control_label, "Les groupes traitement et contrôle doivent être différents.")
  )
  d <- df_f(); req(d)

  withProgress(message = "Estimation DID...", value = 0.3, {
    res <- tryCatch(
      run_did_analysis(d, current_round_col(), current_r1(), current_r2(),
                        input$did_outcome, input$did_group_col,
                        input$did_treat_label, input$did_control_label),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL }
    )
    incProgress(0.7)
  })
  did_res(res)
  if (!is.null(res) && is.null(res$error)) showNotification("✅ DID estimé", type = "message")
  if (!is.null(res$error)) showNotification(paste("⚠️", res$error), type = "warning")
})

output$did_estimate_box <- renderUI({
  res <- did_res(); req(res, is.null(res$error))
  p_col <- if (!is.na(res$did_p) && res$did_p < 0.05) "#c9161d" else "#4dac26"
  sig_txt <- if (!is.na(res$did_p) && res$did_p < 0.05) "Significatif" else "Non significatif"

  tags$div(style = paste0("background:linear-gradient(135deg, ", p_col, "15, white);",
                            "border:2px solid ", p_col, ";border-radius:12px;padding:20px;margin-bottom:16px;"),
    fluidRow(
      column(4, style = "text-align:center;",
        tags$div(style = "font-size:12px;color:#666;", "Estimateur DID"),
        tags$div(style = paste0("font-size:28px;font-weight:800;color:", p_col, ";"),
                 res$did_estimate)
      ),
      column(4, style = "text-align:center;",
        tags$div(style = "font-size:12px;color:#666;", "IC 95%"),
        tags$div(style = "font-size:16px;font-weight:600;color:#00426A;",
                 paste0("[", res$did_ci[1], " ; ", res$did_ci[2], "]"))
      ),
      column(4, style = "text-align:center;",
        tags$div(style = "font-size:12px;color:#666;", "p-value"),
        tags$div(style = paste0("font-size:16px;font-weight:700;color:", p_col, ";"),
                 paste0(format.pval(res$did_p, digits = 4), " (", sig_txt, ")"))
      )
    )
  )
})

output$did_coef_table <- renderDT({
  res <- did_res(); req(res, is.null(res$error))
  coefs <- res$coefficients
  coefs$estimate <- round(coefs$estimate, 4)
  coefs$std.error <- round(coefs$std.error, 4)
  coefs$statistic <- round(coefs$statistic, 3)
  coefs$p.value <- format.pval(coefs$p.value, digits = 4)
  datatable(coefs, rownames = FALSE, options = list(dom = "t", pageLength = 10),
            class = "compact stripe")
})

output$did_cells_table <- renderDT({
  res <- did_res(); req(res, is.null(res$error))
  cells <- res$cells
  cells$mean_y <- round(cells$mean_y, 2)
  cells$sd_y <- round(cells$sd_y, 2)
  datatable(cells[, c("Groupe", "Periode", "mean_y", "sd_y", "n")],
            rownames = FALSE, options = list(dom = "t", pageLength = 10),
            class = "compact stripe",
            colnames = c("Groupe", "Période", "Moyenne", "ET", "n"))
})

output$did_plot <- renderPlotly({
  res <- did_res(); req(res, is.null(res$error))
  plot_did(res)
})

output$did_summary <- renderUI({
  res <- did_res(); req(res, is.null(res$error))
  sig <- if (!is.na(res$did_p) && res$did_p < 0.05) "significatif" else "non significatif"

  tags$div(style = "border-left:4px solid #00426A;padding:12px;background:#f8f9fa;border-radius:6px;margin-top:12px;",
    tags$strong(style = "color:#00426A;", icon("info-circle"), " Interprétation DID"),
    tags$p(style = "font-size:13px;margin-top:6px;",
      paste0("L'estimateur DID de ", res$did_estimate, " (IC95: [",
             res$did_ci[1], " ; ", res$did_ci[2], "]) est ", sig,
             " (p=", format.pval(res$did_p, digits = 4), "). "),
      paste0("Le modèle explique ", res$r_squared * 100, "% de la variance (R²=",
             res$r_squared, ", R² ajusté=", res$adj_r_squared, "). "),
      paste0("N total = ", res$n_total, ".")
    ),
    tags$p(style = "font-size:11px;color:#666;margin-top:4px;",
           "Modèle : Y = β₀ + β₁·Post + β₂·Traitement + β₃·Post×Traitement. ",
           "L'estimateur DID est β₃ (terme d'interaction).")
  )
})


# ═══════════════════════════════════════════════════════════════════════════════
# TAB 3: ANALYSE DE TENDANCE MULTI-ROUNDS
# ═══════════════════════════════════════════════════════════════════════════════

output$trend_vars_ui <- renderUI({
  vn <- c(vars_num_temp())
  req(length(vn) > 0)
  selectizeInput(ns("trend_vars"), "Variables à analyser :",
                 choices = vn, selected = vn[1:min(5, length(vn))],
                 multiple = TRUE, width = "100%",
                 options = list(maxItems = 20, placeholder = "Sélectionner..."))
})

output$trend_order_ui <- renderUI({
  d <- df_f(); rc <- current_round_col(); req(d, rc)
  rounds <- sort(unique(na.omit(d[[rc]])))
  tags$div(
    tags$label("Ordre des vagues :", style = "font-weight:600;"),
    tags$p(style = "font-size:11px;color:#888;",
           paste0("Détecté : ", paste(rounds, collapse = " → "))),
    tags$p(style = "font-size:10px;color:#aaa;",
           "L'ordre est basé sur le tri alphabétique/numérique.")
  )
})

trend_res <- reactiveVal(NULL)

observeEvent(input$run_trend, {
  req(current_round_col(), input$trend_vars)
  d <- df_f(); req(d)

  withProgress(message = "Analyse de tendance...", value = 0.3, {
    res <- tryCatch(
      run_trend_analysis(d, current_round_col(), input$trend_vars),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL }
    )
    incProgress(0.7)
  })
  trend_res(res)
  if (!is.null(res) && is.null(res$error)) showNotification("✅ Tendances analysées", type = "message")
})

output$trend_summary_table <- renderDT({
  res <- trend_res(); req(res, is.null(res$error))

  df_summ <- do.call(rbind, lapply(res$results, function(r) {
    # Get best trend test
    tt <- r$trend_tests
    best_p <- NA; best_method <- "—"
    if (!is.null(tt$cochran_armitage)) { best_p <- tt$cochran_armitage$p_value; best_method <- "Cochran-Armitage" }
    if (!is.null(tt$jonckheere)) { best_p <- tt$jonckheere$p_value; best_method <- "Jonckheere-Terpstra" }
    lin_p <- if (!is.null(tt$linear)) tt$linear$p_value else NA
    slope <- if (!is.null(tt$linear)) tt$linear$slope else NA
    r2 <- if (!is.null(tt$linear)) tt$linear$r_squared else NA

    data.frame(
      Variable = r$variable, Type = r$type, Rounds = r$n_rounds,
      Premier = r$first, Dernier = r$last,
      `Variation (%)` = if (!is.na(r$pct_change)) paste0(r$pct_change, "%") else "—",
      Direction = r$direction,
      Pente = if (!is.na(slope)) slope else NA,
      `R²` = if (!is.na(r2)) r2 else NA,
      `p (tendance)` = if (!is.na(best_p)) format.pval(best_p, digits = 4) else "—",
      `p (linéaire)` = if (!is.na(lin_p)) format.pval(lin_p, digits = 4) else "—",
      Methode = best_method,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }))

  datatable(df_summ, rownames = FALSE, filter = "top",
            options = list(pageLength = 20, dom = "ftp", scrollX = TRUE,
                           columnDefs = list(list(className = "dt-center", targets = "_all"))),
            class = "compact stripe hover") |>
    formatStyle("Direction",
                color = styleEqual(c("↗ Tendance haussière", "↘ Tendance baissière", "→ Stable"),
                                    c("#c9161d", "#4dac26", "#666")),
                fontWeight = "bold")
})

output$trend_var_selector_ui <- renderUI({
  res <- trend_res(); req(res, is.null(res$error))
  vars <- names(res$results)
  selectInput(ns("trend_plot_var"), "Variable à visualiser :",
              choices = vars, selected = vars[1], width = "300px")
})

output$trend_plot <- renderPlotly({
  res <- trend_res(); req(res, is.null(res$error), input$trend_plot_var)
  plot_trend(res, input$trend_plot_var)
})

output$trend_interpretation <- renderUI({
  res <- trend_res(); req(res, is.null(res$error))

  interps <- lapply(res$results, function(r) {
    lin <- r$trend_tests$linear
    ca <- r$trend_tests$cochran_armitage
    jt <- r$trend_tests$jonckheere

    test_txt <- if (!is.null(ca)) {
      paste0("Cochran-Armitage χ²=", ca$chi2, ", p=", format.pval(ca$p_value, digits = 4))
    } else if (!is.null(jt)) {
      paste0("Jonckheere-Terpstra JT=", jt$statistic, ", p=", format.pval(jt$p_value, digits = 4))
    } else ""

    lin_txt <- if (!is.null(lin)) {
      paste0("Régression linéaire : pente=", lin$slope, ", R²=", lin$r_squared,
             ", p=", format.pval(lin$p_value, digits = 4))
    } else ""

    col <- if (grepl("haussière", r$direction)) "#e88400"
           else if (grepl("baissière", r$direction)) "#4dac26"
           else "#666"

    tags$div(style = paste0("border-left:4px solid ", col,
                             ";padding:8px 14px;background:#f8f9fa;border-radius:6px;margin-bottom:6px;"),
      tags$strong(style = paste0("color:", col, ";"), r$variable, " : "),
      tags$span(paste0(r$direction, " — de ", r$first, " à ", r$last)),
      if (nchar(test_txt) > 0) tags$span(style = "font-size:11px;color:#666;display:block;", test_txt),
      if (nchar(lin_txt) > 0) tags$span(style = "font-size:11px;color:#666;display:block;", lin_txt)
    )
  })
  tags$div(
    tags$h5(style = "color:#00426A;font-weight:700;margin:12px 0 8px;",
            icon("comment-medical"), " Interprétation"),
    tagList(interps)
  )
})


# ═══════════════════════════════════════════════════════════════════════════════
# TAB 4: CUSUM — ALERTE PRÉCOCE
# ═══════════════════════════════════════════════════════════════════════════════

output$cusum_var_ui <- renderUI({
  vn <- vars_num_temp(); req(length(vn) > 0)
  selectInput(ns("cusum_var"), "Indicateur :", choices = vn, width = "100%")
})

output$cusum_zone_ui <- renderUI({
  vc <- vars_cat_temp(); req(length(vc) > 0)
  choices <- c("(Global)" = "__none__", vc)
  selectInput(ns("cusum_zone_col"), "Stratification par zone :",
              choices = choices, selected = "__none__", width = "100%")
})

cusum_res <- reactiveVal(NULL)

observeEvent(input$run_cusum, {
  req(current_round_col(), input$cusum_var)
  d <- df_f(); req(d)

  zone_col <- if (input$cusum_zone_col == "__none__") NULL else input$cusum_zone_col
  target <- if (is.na(input$cusum_target)) NULL else input$cusum_target

  withProgress(message = "Calcul CUSUM...", value = 0.3, {
    res <- tryCatch(
      run_cusum_analysis(d, current_round_col(), input$cusum_var,
                          zone_col = zone_col, target_mean = target,
                          k = input$cusum_k, h = input$cusum_h),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL }
    )
    incProgress(0.7)
  })
  cusum_res(res)
  if (!is.null(res) && is.null(res$error)) showNotification("✅ CUSUM calculé", type = "message")
})

output$cusum_alert_box <- renderUI({
  res <- cusum_res(); req(res, is.null(res$error))
  n_alerts <- res$n_alerts
  col <- if (n_alerts > 0) "#c9161d" else "#4dac26"
  icon_nm <- if (n_alerts > 0) "exclamation-circle" else "check-circle"

  tags$div(style = paste0("background:", col, "15;border:2px solid ", col,
                            ";border-radius:10px;padding:14px;margin-bottom:14px;text-align:center;"),
    tags$span(style = paste0("font-size:18px;font-weight:800;color:", col, ";"),
              icon(icon_nm),
              if (n_alerts > 0) paste0(" ", n_alerts, " ALERTE(S) détectée(s)")
              else " Aucune alerte — situation sous contrôle")
  )
})

output$cusum_plot <- renderPlotly({
  res <- cusum_res(); req(res, is.null(res$error))
  plot_cusum(res)
})

output$cusum_table <- renderDT({
  res <- cusum_res(); req(res, is.null(res$error))
  df <- res$cusum
  datatable(df, rownames = FALSE, filter = "top",
            options = list(pageLength = 20, dom = "ftp", scrollX = TRUE,
                           columnDefs = list(list(className = "dt-center", targets = "_all"))),
            class = "compact stripe hover") |>
    formatStyle("Alerte_hausse",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#ffcdd2", "white")),
                fontWeight = "bold") |>
    formatStyle("Alerte_baisse",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#ffcdd2", "white")),
                fontWeight = "bold")
})

output$cusum_summary <- renderUI({
  res <- cusum_res(); req(res, is.null(res$error))
  tags$div(style = "border-left:4px solid #00426A;padding:12px;background:#f8f9fa;border-radius:6px;margin-top:12px;",
    tags$strong(style = "color:#00426A;", icon("info-circle"), " Méthode CUSUM"),
    tags$p(style = "font-size:12px;margin-top:6px;",
      paste0("Le CUSUM (Cumulative Sum) détecte les décalages persistants par rapport ",
             "à une valeur de référence. Paramètres : k=", res$k,
             " (tolérance), h=", res$h, " (seuil de décision). "),
      "Un signal d'alerte (▲) indique que la somme cumulée des écarts dépasse le seuil H, ",
      "suggérant un changement systématique de l'indicateur."
    )
  )
})


# ═══════════════════════════════════════════════════════════════════════════════
# TAB 5: BOOTSTRAP TEMPORAL
# ═══════════════════════════════════════════════════════════════════════════════

output$boot_temp_vars_ui <- renderUI({
  vn <- vars_num_temp(); req(length(vn) > 0)
  selectizeInput(ns("boot_temp_vars"), "Variables :",
                 choices = vn, selected = vn[1:min(5, length(vn))],
                 multiple = TRUE, width = "100%",
                 options = list(maxItems = 15, placeholder = "Sélectionner..."))
})

boot_temp_res <- reactiveVal(NULL)

observeEvent(input$run_boot_temp, {
  req(current_round_col(), current_r1(), current_r2(), input$boot_temp_vars)
  validate(need(current_r1() != current_r2(), "Veuillez sélectionner deux vagues différentes."))
  d <- df_f(); req(d)

  withProgress(message = "Bootstrap en cours...", value = 0.1, {
    res <- tryCatch(
      run_bootstrap_temporal(d, current_round_col(), current_r1(), current_r2(),
                              input$boot_temp_vars, input$boot_temp_n, input$boot_temp_conf),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL }
    )
    incProgress(0.9)
  })
  boot_temp_res(res)
  if (!is.null(res)) showNotification("✅ Bootstrap terminé", type = "message")
})

output$boot_temp_table <- renderDT({
  res <- boot_temp_res(); req(res)
  df_show <- res
  df_show$p_boot <- format.pval(df_show$p_boot, digits = 4)

  datatable(df_show, rownames = FALSE, filter = "top",
            options = list(pageLength = 15, dom = "ftp", scrollX = TRUE,
                           columnDefs = list(list(className = "dt-center", targets = "_all"))),
            class = "compact stripe hover")
})

output$boot_temp_forest <- renderPlotly({
  res <- boot_temp_res(); req(res)
  df <- res[!is.na(res$Diff_obs), ]
  req(nrow(df) > 0)

  df$Sig <- ifelse(df$p_boot < 0.05, "Significatif", "Non significatif")

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = Diff_obs, y = reorder(Variable, abs(Diff_obs)), color = Sig,
    text = paste0(Variable, "\nDiff: ", Diff_obs,
                  "\nIC BCa: [", IC_bca_lo, " ; ", IC_bca_hi, "]",
                  "\np boot = ", p_boot))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = IC_bca_lo, xmax = IC_bca_hi),
                            height = 0.25, linewidth = 0.8) +
    ggplot2::geom_point(size = 4) +
    ggplot2::scale_color_manual(values = c("Significatif" = "#c9161d",
                                            "Non significatif" = "#008DC9")) +
    ggplot2::labs(x = "Différence (V2 - V1)", y = "", color = "") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  plotly::ggplotly(p, tooltip = "text") |> plotly::layout(hovermode = "closest")
})

output$boot_temp_summary <- renderUI({
  res <- boot_temp_res(); req(res)
  n_sig <- sum(res$p_boot < 0.05, na.rm = TRUE)

  tags$div(style = "border-left:4px solid #6c3483;padding:12px;background:#f8f9fa;border-radius:6px;margin-top:12px;",
    tags$strong(style = "color:#6c3483;", icon("random"), " Méthode Bootstrap"),
    tags$p(style = "font-size:12px;margin-top:6px;",
      paste0(n_sig, "/", nrow(res), " variables significatives (p bootstrap < 0.05). "),
      paste0("Rééchantillonnages : ", input$boot_temp_n, ". "),
      "Deux types d'IC sont rapportés : percentile (classique) et BCa (bias-corrected ",
      "and accelerated, corrigé pour le biais et l'asymétrie). ",
      "Le BCa est généralement préféré car plus robuste."
    )
  )
})


# ═══════════════════════════════════════════════════════════════════════════════
# TAB 6: HEATMAP TEMPORELLE MULTIVARIÉE
# ═══════════════════════════════════════════════════════════════════════════════

output$hm_vars_ui <- renderUI({
  vn <- vars_num_temp(); req(length(vn) > 0)
  selectizeInput(ns("hm_vars"), "Indicateurs :",
                 choices = vn, selected = vn[1:min(8, length(vn))],
                 multiple = TRUE, width = "100%",
                 options = list(maxItems = 25, placeholder = "Sélectionner..."))
})

hm_res <- reactiveVal(NULL)

observeEvent(input$run_heatmap, {
  req(current_round_col(), input$hm_vars)
  d <- df_f(); req(d)

  withProgress(message = "Construction de la heatmap...", value = 0.3, {
    res <- tryCatch(
      build_temporal_heatmap(d, current_round_col(), input$hm_vars),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL }
    )
    incProgress(0.7)
  })
  hm_res(res)
  if (!is.null(res)) showNotification("✅ Heatmap générée", type = "message")
})

output$hm_plot <- renderPlotly({
  res <- hm_res(); req(res)
  plot_temporal_heatmap(res, show = input$hm_show)
})

output$hm_summary <- renderUI({
  res <- hm_res(); req(res)

  if (!is.null(res$significance) && !is.null(res$changes)) {
    n_sig <- sum(res$significance != "", na.rm = TRUE)
    n_total <- sum(!is.na(res$changes))
    tags$div(style = "border-left:4px solid #008DC9;padding:12px;background:#f8f9fa;border-radius:6px;margin-top:12px;",
      tags$strong(style = "color:#008DC9;", icon("th"), " Résumé"),
      tags$p(style = "font-size:12px;margin-top:6px;",
        paste0(n_sig, "/", n_total, " transitions significatives (p < 0.05). "),
        paste0(length(res$indicators), " indicateurs × ", length(res$rounds), " vagues. "),
        "Tests : Chi² (binaire) ou Wilcoxon (continu) entre vagues consécutives. ",
        "Les étoiles (*, **, ***) indiquent le niveau de significativité."
      )
    )
  }
})


# ═══════════════════════════════════════════════════════════════════════════════
# TAB 7: PROFIL ÉVOLUTIF PAR ZONE — TRAJECTOIRES
# ═══════════════════════════════════════════════════════════════════════════════

output$traj_var_ui <- renderUI({
  vn <- vars_num_temp(); req(length(vn) > 0)
  selectInput(ns("traj_var"), "Indicateur :", choices = vn, width = "100%")
})

output$traj_zone_ui <- renderUI({
  d <- df_f(); req(d)
  m <- tryCatch(map_cols(), error = function(e) list())
  vc <- vars_cat_temp()
  # [S2] Prioriser : d'abord map_cols, puis regex patterns géographiques
  prio <- c(m$region, m$dept)
  prio <- prio[!sapply(prio, is.null) & prio %in% names(d)]
  if (length(prio) == 0) {
    geo_patt <- "region|province|departement|district|zone|commune|county|state|admin|strate"
    prio <- grep(geo_patt, vc, ignore.case = TRUE, value = TRUE)
  }
  sel <- if (length(prio) > 0) prio[1] else if (length(vc) > 0) vc[1] else NULL
  selectInput(ns("traj_zone_col"), "Colonne zone :",
              choices = vc, selected = sel, width = "100%")
})

traj_res <- reactiveVal(NULL)

observeEvent(input$run_trajectory, {
  req(current_round_col(), input$traj_var, input$traj_zone_col)
  d <- df_f(); req(d)

  withProgress(message = "Calcul des trajectoires...", value = 0.3, {
    res <- tryCatch(
      run_zone_trajectory(d, current_round_col(), input$traj_var, input$traj_zone_col),
      error = function(e) { showNotification(paste("❌", e$message), type = "error"); NULL }
    )
    incProgress(0.7)
  })
  traj_res(res)
  if (!is.null(res)) showNotification("✅ Trajectoires calculées", type = "message")
})

output$traj_plot <- renderPlotly({
  res <- traj_res(); req(res)
  plot_zone_trajectory(res)
})

output$traj_anomaly_table <- renderDT({
  res <- traj_res(); req(res, !is.null(res$anomalies))
  datatable(res$anomalies, rownames = FALSE,
            options = list(pageLength = 10, dom = "ftp",
                           columnDefs = list(list(className = "dt-center", targets = "_all"))),
            class = "compact stripe hover") |>
    formatStyle("Divergence",
                background = styleColorBar(range(res$anomalies$Divergence, na.rm = TRUE), "#ffe0b2"),
                backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",
                backgroundPosition = "center")
})

output$traj_summary <- renderUI({
  res <- traj_res(); req(res)
  top_zone <- if (!is.null(res$anomalies) && nrow(res$anomalies) > 0) {
    res$anomalies$Zone[1]
  } else "—"

  tags$div(style = "border-left:4px solid #2e7d32;padding:12px;background:#f8f9fa;border-radius:6px;margin-top:12px;",
    tags$strong(style = "color:#2e7d32;", icon("route"), " Analyse des trajectoires"),
    tags$p(style = "font-size:12px;margin-top:6px;",
      paste0(length(res$zones), " zones suivies sur ", length(res$rounds), " vagues. "),
      paste0("La zone la plus divergente de la tendance nationale est : "),
      tags$strong(top_zone), ". ",
      "Les zones divergentes peuvent nécessiter une investigation ciblée ",
      "ou une adaptation des interventions."
    )
  )
})

cat("✅ server_temporel_expert.R chargé — 7 serveurs expert\n")
