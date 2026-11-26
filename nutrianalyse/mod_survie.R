# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_survie.R — Module Shiny Expert : Analyse de survie                    ║
# ║  Kaplan-Meier · Log-rank · Cox multivariable · Hypothèse PH · Forest      ║
# ║  Table de vie · Survie médiane · Comparaisons multiples · Exports          ║
# ║  Niveau: Doctoral Research Grade                                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

source("module_survie.R")

# ── UI ────────────────────────────────────────────────────────────────────────
survieUI <- function(id, lang = "fr") {
  ns <- NS(id)

  tagList(
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #008DC9; border-radius: 6px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#008DC9;",
              icon("hourglass-half"), tr("surv_title", lang)),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             tr("surv_desc", lang),
             )
    ),

    fluidRow(
      box(width = 3, status = "primary", solidHeader = TRUE,
          title = tags$span(icon("cog"), " ", tr("surv_config", lang)),
          uiOutput(ns("sel_time")),
          uiOutput(ns("sel_event")),
          uiOutput(ns("sel_group")),
          hr(),
          tags$div(class = "section-header", tr("surv_covariates", lang)),
          uiOutput(ns("sel_covariates")),
          hr(),
          actionButton(ns("run_km"), tr("surv_btn_km", lang),
                       class = "btn btn-oms btn-block", width = "100%",
                       style = "font-size:13px;padding:10px;margin-bottom:8px;"),
          actionButton(ns("run_cox"), tr("surv_btn_cox", lang),
                       class = "btn btn-oms btn-block", width = "100%",
                       style = "font-size:13px;padding:10px;background:#1D9E75 !important;")
      ),

      box(width = 9, status = "info", solidHeader = TRUE,
          title = tags$span(icon("chart-line"), " ", tr("surv_results", lang)),
          tabsetPanel(
            id = ns("surv_tabs"),

            # ── Onglet 1 : Kaplan-Meier ─────────────────────────────────────
            tabPanel(tags$span(icon("chart-area"), " ", tr("surv_tab_km", lang)),
              br(),
              plotlyOutput(ns("km_plot"), height = "500px"),
              br(),
              fluidRow(
                column(6, uiOutput(ns("km_summary"))),
                column(6, uiOutput(ns("km_interpretation")))
              )
            ),

            # ── Onglet 2 : Table de vie ─────────────────────────────────────
            tabPanel(tags$span(icon("table"), " ", tr("surv_tab_life", lang)),
              br(),
              tags$div(style = "font-size:12px;color:#666;margin-bottom:10px;",
                tr("surv_life_note", lang)),
              DTOutput(ns("life_table")),
              br(),
              uiOutput(ns("median_survival_cards"))
            ),

            # ── Onglet 3 : Cox — Forest plot ────────────────────────────────
            tabPanel(tags$span(icon("tree"), " ", tr("surv_tab_cox", lang)),
              br(),
              plotlyOutput(ns("cox_forest"), height = "500px"),
              br(),
              fluidRow(
                column(8, DTOutput(ns("cox_table"))),
                column(4, uiOutput(ns("cox_model_summary")))
              )
            ),

            # ── Onglet 4 : Hypothèse PH ────────────────────────────────────
            tabPanel(tags$span(icon("check-double"), " ", tr("surv_tab_ph", lang)),
              br(),
              plotlyOutput(ns("ph_plot"), height = "400px"),
              br(),
              uiOutput(ns("ph_summary")),
              br(),
              plotOutput(ns("schoenfeld_residuals"), height = "400px")
            ),

            # ── Onglet 5 : Diagnostics Cox ──────────────────────────────────
            tabPanel(tags$span(icon("stethoscope"), " ", tr("surv_tab_diag", lang)),
              br(),
              fluidRow(
                column(6,
                  tags$div(class = "section-header", tr("surv_martingale", lang)),
                  plotlyOutput(ns("martingale_plot"), height = "350px")
                ),
                column(6,
                  tags$div(class = "section-header", tr("surv_deviance", lang)),
                  plotlyOutput(ns("deviance_plot"), height = "350px")
                )
              ),
              br(),
              uiOutput(ns("diagnostics_summary"))
            ),

            # ── Onglet 6 : Export ───────────────────────────────────────────
            tabPanel(tags$span(icon("download"), " Export"),
              br(),
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = tags$span(icon("download"), " ", tr("surv_export_title", lang)),
                    fluidRow(
                      column(3, downloadButton(ns("dl_km_csv"), tr("surv_dl_km_csv", lang),
                        style = "background:#008DC9;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                      column(3, downloadButton(ns("dl_cox_csv"), tr("surv_dl_cox_csv", lang),
                        style = "background:#4dac26;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                      column(3, downloadButton(ns("dl_km_png"), tr("surv_dl_km_png", lang),
                        style = "background:#e88400;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                      column(3, downloadButton(ns("dl_excel"), tr("surv_dl_excel", lang),
                        style = "background:#7b2d8b;color:white;border:none;width:100%;border-radius:6px;padding:8px;"))
                    )
                )
              )
            )
          )
      )
    )
  )
}

# ── SERVER ────────────────────────────────────────────────────────────────────
survieServer <- function(id, df_f, map_cols, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vars_num <- reactive({ d <- df_f(); req(d); names(d)[sapply(d, is.numeric)] })
    vars_cat <- reactive({
      d <- df_f(); req(d)
      names(d)[sapply(d, function(x) is.factor(x) || is.character(x) ||
                (is.numeric(x) && all(x %in% c(0, 1, NA))))]
    })

    output$sel_time  <- renderUI(selectInput(ns("time_var"), "Variable temps :", choices = vars_num(), width = "100%"))
    output$sel_event <- renderUI(selectInput(ns("event_var"), "Variable événement (0/1) :", choices = vars_cat(), width = "100%"))
    output$sel_group <- renderUI(selectInput(ns("group_var"), "Variable groupe (optionnel) :",
                                             choices = c("(aucun)" = "", vars_cat()), width = "100%"))
    output$sel_covariates <- renderUI({
      all_vars <- c(vars_num(), vars_cat())
      checkboxGroupInput(ns("cox_covars"), NULL,
                         choices = setdiff(all_vars, c(input$time_var, input$event_var)), selected = NULL)
    })

    # ════════════════════════════════════════════════════════════════════════
    # KAPLAN-MEIER
    # ════════════════════════════════════════════════════════════════════════
    km_res <- reactiveVal(NULL)

    observeEvent(input$run_km, {
      d <- df_f(); req(d, input$time_var, input$event_var)
      grp <- if (!is.null(input$group_var) && input$group_var != "") input$group_var else NULL
      withProgress(message = "Calcul Kaplan-Meier...", {
        tryCatch({
          r <- run_km(d, input$time_var, input$event_var, grp)
          if (!is.null(r$error)) showNotification(paste("❌", r$error), type = "error")
          else { km_res(r); showNotification("✅ Kaplan-Meier calculé", type = "message") }
        }, error = function(e) showNotification(paste("❌", e$message), type = "error"))
      })
    })

    output$km_plot <- renderPlotly({
      r <- km_res(); req(!is.null(r))
      p <- plot_km(r)
      if (inherits(p, "gg")) ggplotly(p) else p
    })

    output$km_summary <- renderUI({
      r <- km_res(); req(!is.null(r))
      logrank_ui <- if (!is.null(r$lr_p)) {
        sig <- r$lr_p < 0.05
        tags$div(class = if (sig) "alerte-box alerte-crit" else "alerte-box alerte-ok",
          tags$b("Test Log-rank"), tags$br(),
          paste0("Chi² = ", r$lr_chi2, " | p = ", format.pval(r$lr_p, digits = 4)),
          tags$br(),
          tags$span(style = "font-size:12px;",
            if (sig) "→ Survies significativement différentes entre groupes"
            else "→ Pas de différence significative entre groupes")
        )
      }
      tags$div(style = "background:#f8f9fa;padding:14px;border-radius:8px;border-left:4px solid #008DC9;",
        tags$h6(style = "font-weight:700;color:#00426A;margin:0 0 8px;", "Résumé"),
        tags$p(tags$b("N = "), r$n, " | ", tags$b("Événements = "), r$n_events,
               " | ", tags$b("Taux événement = "), round(r$n_events / r$n * 100, 1), "%"),
        logrank_ui
      )
    })

    output$km_interpretation <- renderUI({
      r <- km_res(); req(!is.null(r))
      cens_rate <- round((1 - r$n_events / r$n) * 100, 1)
      tags$div(style = "background:#fff3e0;padding:14px;border-radius:8px;border-left:4px solid #e88400;",
        tags$h6(style = "font-weight:700;color:#e88400;margin:0 0 8px;", icon("lightbulb"), " Interprétation"),
        tags$p(style = "font-size:12px;line-height:1.6;",
          paste0("Le taux de censure est de ", cens_rate, "%. "),
          if (cens_rate > 50) "⚠ Taux de censure élevé — la survie médiane peut être peu fiable."
          else "Le taux de censure est raisonnable pour une estimation fiable.",
          tags$br(),
          if (!is.null(r$lr_p) && r$lr_p < 0.05)
            "Les courbes de survie diffèrent significativement entre les groupes. Le test de Cox pourra préciser les facteurs contributifs."
          else if (!is.null(r$lr_p))
            "Les courbes de survie ne diffèrent pas significativement. Un modèle de Cox peut néanmoins être exploré pour l'ajustement."
        )
      )
    })

    # ── Table de vie ────────────────────────────────────────────────────────
    output$life_table <- renderDT({
      r <- km_res(); req(!is.null(r))
      tryCatch({
        tidy_km <- broom::tidy(r$fit) |>
          mutate(
            `S(t)` = round(estimate, 4),
            `IC bas` = round(conf.low, 4),
            `IC haut` = round(conf.high, 4),
            `H(t)` = round(-log(estimate), 4),
            Strate = if ("strata" %in% names(broom::tidy(r$fit))) gsub(".*=", "", strata) else "Global"
          ) |>
          select(Strate, Temps = time, `N à risque` = n.risk, `Événements` = n.event,
                 `Censurés` = n.censor, `S(t)`, `IC bas`, `IC haut`, `H(t)`)
        tidy_km |>
          datatable(rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
                    class = "compact stripe hover") |>
          formatStyle("S(t)", background = styleColorBar(c(0, 1), "#e3f2fd"),
                      backgroundSize = "100% 70%") |>
          formatRound(c("S(t)", "IC bas", "IC haut", "H(t)"), 4)
      }, error = function(e) NULL)
    })

    output$median_survival_cards <- renderUI({
      r <- km_res(); req(!is.null(r))
      sm <- r$summary
      if (is.null(sm)) return(NULL)
      if (is.matrix(sm)) {
        cards <- lapply(seq_len(nrow(sm)), function(i) {
          rn <- rownames(sm)[i]
          med <- sm[i, "median"]
          lo <- sm[i, "0.95LCL"]; hi <- sm[i, "0.95UCL"]
          column(4, tags$div(class = "stat-card",
            tags$p(class = "sc-lbl", gsub(".*=", "", rn)),
            tags$p(class = "sc-val", if (is.na(med)) "NR" else round(med, 1)),
            tags$p(class = "sc-interp",
              if (is.na(med)) "Médiane non atteinte"
              else paste0("IC95 [", round(lo, 1), " – ", round(hi, 1), "]"))
          ))
        })
        do.call(fluidRow, cards)
      } else {
        med <- sm["median"]
        tags$div(class = "stat-card", style = "max-width:300px;",
          tags$p(class = "sc-lbl", "Survie médiane"),
          tags$p(class = "sc-val", if (is.na(med)) "NR" else round(med, 1)),
          tags$p(class = "sc-interp",
            if (is.na(med)) "Médiane non atteinte"
            else paste0("IC95 [", round(sm["0.95LCL"], 1), " – ", round(sm["0.95UCL"], 1), "]"))
        )
      }
    })

    # ════════════════════════════════════════════════════════════════════════
    # COX
    # ════════════════════════════════════════════════════════════════════════
    cox_res <- reactiveVal(NULL)

    observeEvent(input$run_cox, {
      d <- df_f(); req(d, input$time_var, input$event_var, input$cox_covars)
      req(length(input$cox_covars) >= 1)
      withProgress(message = "Modèle de Cox...", {
        tryCatch({
          r <- run_cox(d, input$time_var, input$event_var, input$cox_covars)
          if (!is.null(r$error)) showNotification(paste("❌", r$error), type = "error")
          else { cox_res(r); showNotification("✅ Cox ajusté", type = "message") }
        }, error = function(e) showNotification(paste("❌", e$message), type = "error"))
      })
    })

    output$cox_forest <- renderPlotly({
      r <- cox_res(); req(!is.null(r))
      p <- plot_cox_forest(r)
      if (inherits(p, "gg")) ggplotly(p) else p
    })

    output$cox_table <- renderDT({
      r <- cox_res(); req(!is.null(r), !is.null(r$tidy))
      r$tidy |>
        mutate(
          Sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**",
                          p.value < .05 ~ "*", p.value < .10 ~ ".", TRUE ~ "NS"),
          across(where(is.numeric), ~round(., 3))
        ) |>
        datatable(rownames = FALSE, options = list(dom = "t", pageLength = 20),
                  class = "compact stripe hover") |>
        formatStyle("p.value", color = styleInterval(0.05, c("#c9161d", "#4dac26")), fontWeight = "bold") |>
        formatStyle("Sig", fontWeight = "bold")
    })

    output$cox_model_summary <- renderUI({
      r <- cox_res(); req(!is.null(r))
      n_sig <- sum(r$tidy$p.value < 0.05, na.rm = TRUE)
      tags$div(style = "background:#f0f8ff;padding:14px;border-radius:8px;border-left:4px solid #008DC9;",
        tags$h6(style = "font-weight:700;color:#00426A;margin:0 0 10px;", "Performance du modèle"),
        tags$div(class = "stat-card",
          tags$p(class = "sc-lbl", "C-index (concordance)"),
          tags$p(class = "sc-val", r$concordance),
          tags$p(class = "sc-interp",
            if (r$concordance > 0.7) "Bonne discrimination" else "Discrimination modeste")
        ),
        tags$div(class = "stat-card",
          tags$p(class = "sc-lbl", "Nagelkerke R²"),
          tags$p(class = "sc-val", r$r2_n)
        ),
        tags$div(class = "stat-card",
          tags$p(class = "sc-lbl", "Variables significatives"),
          tags$p(class = "sc-val", paste0(n_sig, "/", nrow(r$tidy)))
        ),
        tags$div(style = "margin-top:10px;font-size:11px;color:#666;",
          paste0("LRT p = ", r$logtest, " | Wald p = ", r$waldtest, " | Score p = ", r$sctest)
        )
      )
    })

    # ── Hypothèse PH ────────────────────────────────────────────────────────
    output$ph_plot <- renderPlotly({
      r <- cox_res(); req(!is.null(r))
      p <- tryCatch(plot_ph_test(r), error = function(e) NULL)
      if (inherits(p, "gg")) ggplotly(p) else if (!is.null(p)) p
    })

    output$ph_summary <- renderUI({
      r <- cox_res(); req(!is.null(r), !is.null(r$ph_test))
      ph <- r$ph_test
      global_p <- tryCatch(ph$table["GLOBAL", "p"], error = function(e) NA)
      if (is.na(global_p)) return(tags$div(style = "padding:10px;color:#888;", "Test PH non disponible"))

      verdict <- if (global_p < 0.05) {
        tags$div(class = "alerte-box alerte-crit",
          tags$strong("⚠ Hypothèse PH violée (p < 0.05)"), tags$br(),
          tags$span(style = "font-size:12px;",
            "Le modèle de Cox peut ne pas être approprié. Considérez un modèle stratifié, ",
            "des covariables dépendantes du temps, ou un modèle AFT (Accelerated Failure Time)."))
      } else {
        tags$div(class = "alerte-box alerte-ok",
          tags$strong("✓ Hypothèse PH respectée (p ≥ 0.05)"), tags$br(),
          tags$span(style = "font-size:12px;", "Le modèle de Cox est valide."))
      }

      tagList(
        tags$div(style = "background:#f8f9fa;border-radius:8px;padding:12px;",
          tags$h6(style = "font-weight:700;color:#00426A;margin:0 0 8px;",
                  icon("stethoscope"), " Test de Schoenfeld"),
          tags$table(
            style = "width:100%;font-size:12px;border-collapse:collapse;",
            tags$thead(tags$tr(
              lapply(c("Variable", "Chi²", "df", "p-value"), function(h)
                tags$th(style = "padding:6px;background:#008DC9;color:white;text-align:left;", h))
            )),
            tags$tbody(
              lapply(rownames(ph$table), function(rn) {
                vals <- ph$table[rn, ]
                tags$tr(
                  tags$td(style = "padding:5px;border-bottom:1px solid #eee;font-weight:600;", rn),
                  tags$td(style = "padding:5px;border-bottom:1px solid #eee;", round(vals[1], 3)),
                  tags$td(style = "padding:5px;border-bottom:1px solid #eee;", vals[2]),
                  tags$td(style = paste0("padding:5px;border-bottom:1px solid #eee;font-weight:600;color:",
                                          if (vals[3] < 0.05) "#c9161d" else "#4dac26", ";"),
                          format.pval(vals[3], digits = 3))
                )
              })
            )
          ),
          verdict
        )
      )
    })

    output$schoenfeld_residuals <- renderPlot({
      r <- cox_res(); req(!is.null(r), !is.null(r$ph_test))
      tryCatch({
        n_vars <- nrow(r$ph_test$table) - 1
        if (n_vars > 0 && n_vars <= 6) {
          par(mfrow = c(ceiling(n_vars / 2), min(n_vars, 2)), mar = c(4, 4, 2, 1))
          plot(r$ph_test, col = "#008DC9", lwd = 2)
        }
      }, error = function(e) NULL)
    })

    # ── Diagnostics Cox ─────────────────────────────────────────────────────
    output$martingale_plot <- renderPlotly({
      r <- cox_res(); req(!is.null(r))
      tryCatch({
        resid_m <- residuals(r$model, type = "martingale")
        fitted_v <- predict(r$model, type = "lp")
        df_res <- tibble(Fitted = fitted_v, Residual = resid_m)
        p <- ggplot(df_res, aes(x = Fitted, y = Residual)) +
          geom_point(alpha = 0.5, color = "#008DC9") +
          geom_smooth(method = "loess", color = "#c9161d", se = FALSE, linewidth = 1) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(x = "Prédicteur linéaire", y = "Résidu de Martingale",
               title = "Résidus de Martingale vs Prédicteur linéaire") +
          theme_minimal(base_size = 11)
        ggplotly(p)
      }, error = function(e) plotly_empty())
    })

    output$deviance_plot <- renderPlotly({
      r <- cox_res(); req(!is.null(r))
      tryCatch({
        resid_d <- residuals(r$model, type = "deviance")
        df_res <- tibble(Index = seq_along(resid_d), Residual = resid_d)
        p <- ggplot(df_res, aes(x = Index, y = Residual,
                                text = paste0("Obs: ", Index, "\nRésidu: ", round(Residual, 3)))) +
          geom_point(alpha = 0.5, color = "#008DC9") +
          geom_hline(yintercept = c(-2, 0, 2), linetype = c("dashed", "solid", "dashed"),
                     color = c("#c9161d", "#888", "#c9161d")) +
          labs(x = "Observation", y = "Résidu de Déviance",
               title = "Résidus de Déviance (outliers: |r| > 2)") +
          theme_minimal(base_size = 11)
        ggplotly(p, tooltip = "text")
      }, error = function(e) plotly_empty())
    })

    output$diagnostics_summary <- renderUI({
      r <- cox_res(); req(!is.null(r))
      tryCatch({
        resid_d <- residuals(r$model, type = "deviance")
        n_outliers <- sum(abs(resid_d) > 2)
        pct_outliers <- round(n_outliers / length(resid_d) * 100, 1)
        tags$div(style = "background:#f0f8ff;padding:14px;border-radius:8px;border-left:4px solid #008DC9;",
          tags$h6(style = "font-weight:700;color:#00426A;margin:0 0 8px;", "Synthèse diagnostique"),
          tags$p(style = "font-size:12px;",
            paste0(n_outliers, " observations (", pct_outliers, "%) avec résidu de déviance > 2."),
            tags$br(),
            if (pct_outliers > 5) "⚠ Proportion élevée d'outliers — vérifiez les données."
            else "✓ Peu d'outliers — le modèle s'ajuste bien aux données."
          )
        )
      }, error = function(e) NULL)
    })

    # ════════════════════════════════════════════════════════════════════════
    # EXPORTS
    # ════════════════════════════════════════════════════════════════════════
    output$dl_km_csv <- downloadHandler(
      filename = function() paste0("table_vie_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- km_res(); req(!is.null(r))
        tryCatch({
          tidy_km <- broom::tidy(r$fit)
          write.csv(tidy_km, file, row.names = FALSE)
        }, error = function(e) writeLines(paste("Erreur:", e$message), file))
      }
    )

    output$dl_cox_csv <- downloadHandler(
      filename = function() paste0("cox_resultats_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- cox_res(); req(!is.null(r))
        write.csv(r$tidy, file, row.names = FALSE)
      }
    )

    output$dl_km_png <- downloadHandler(
      filename = function() paste0("kaplan_meier_", Sys.Date(), ".png"),
      content = function(file) {
        r <- km_res(); req(!is.null(r))
        tryCatch({
          km_df <- broom::tidy(r$fit) |>
            mutate(strata = if ("strata" %in% names(broom::tidy(r$fit))) gsub(".*=", "", strata) else "Global")
          p <- ggplot(km_df, aes(x = time, y = estimate, color = strata, fill = strata)) +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
            geom_step(linewidth = 1.2) +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60") +
            scale_y_continuous(labels = scales::percent) +
            labs(x = "Temps", y = "S(t)", title = "Courbe de Kaplan-Meier") +
            theme_minimal(base_size = 12) + theme(legend.position = "bottom")
          ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
        }, error = function(e) NULL)
      }
    )

    output$dl_excel <- downloadHandler(
      filename = function() paste0("survie_complet_", Sys.Date(), ".xlsx"),
      content = function(file) {
        sheets <- list()
        r_km <- km_res()
        if (!is.null(r_km)) {
          sheets[["Table de vie"]] <- tryCatch(broom::tidy(r_km$fit), error = function(e) tibble(Erreur = e$message))
          sheets[["Resume KM"]] <- tibble(N = r_km$n, Evenements = r_km$n_events,
                                           Logrank_p = r_km$lr_p %||% NA)
        }
        r_cx <- cox_res()
        if (!is.null(r_cx)) {
          sheets[["Cox HR"]] <- r_cx$tidy
          sheets[["Cox Perf"]] <- tibble(Concordance = r_cx$concordance, R2 = r_cx$r2_n,
                                          LRT_p = r_cx$logtest, Wald_p = r_cx$waldtest)
          if (!is.null(r_cx$ph_test)) {
            sheets[["Test PH"]] <- as.data.frame(r_cx$ph_test$table) |> tibble::rownames_to_column("Variable")
          }
        }
        if (length(sheets) == 0) sheets[["Info"]] <- tibble(Message = "Lancez au moins une analyse")
        writexl::write_xlsx(sheets, path = file)
      }
    )
  })
}
