# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_advanced_forecasting.R — Forecasting ARIMA/Prophet                    ║
# ║  Prévisions sur séries temporelles avec intervalles de confiance 95%       ║
# ║  Variables et dates dynamiques selon les données importées                 ║
# ║  Niveau: Doctoral Research Grade                                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(shiny)
library(tidyverse)
library(forecast)
library(tseries)
library(plotly)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

advancedForecastingUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #008DC9; border-radius: 6px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#008DC9;",
              icon("chart-line"), tr("fore_title", lang)),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             tr("fore_desc", lang),
             )
    ),
    fluidRow(
      box(width = 3, status = "info", solidHeader = TRUE, title = tr("fore_params", lang),
          uiOutput(ns("sel_date_col")),
          uiOutput(ns("sel_value_col")),
          uiOutput(ns("sel_agg_fn")),
          hr(),
          selectInput(ns("fc_temporal"), tr("fore_temporal_grp", lang),
                      choices = setNames(c("month","quarter","week"),
                                         c(tr("fore_monthly",lang),tr("fore_quarterly",lang),tr("fore_weekly",lang))),
                      selected = "month", width = "100%"),
          sliderInput(ns("fc_horizon"), tr("fore_horizon", lang),
                      min = 3, max = 36, value = 12, step = 3, width = "100%"),
          radioButtons(ns("fc_method"), tr("fore_method", lang),
                       choices = setNames(c("arima","ets"), c(tr("fore_arima",lang), tr("fore_ets",lang))),
                       selected = "arima", inline = FALSE),
          hr(),
          tags$div(class = "section-header", tr("fore_diagnostics", lang)),
          checkboxInput(ns("fc_adf_test"), tr("fore_adf_test", lang), value = TRUE),
          checkboxInput(ns("fc_acf_plot"), "ACF/PACF plots", value = FALSE),
          checkboxInput(ns("fc_residuals"), tr("fore_residuals", lang), value = TRUE),
          hr(),
          actionButton(ns("fc_run"), tr("fore_btn_run", lang),
                       class = "btn btn-oms btn-block", width = "100%",
                       style = "font-size:13px;padding:10px;"),
          br(),
          uiOutput(ns("data_info"))
      ),
      box(width = 9, status = "primary", solidHeader = TRUE,
          title = tr("fore_main_title", lang),
          plotlyOutput(ns("fc_forecast_plot"), height = "400px")
      )
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tags$span(icon("table"), " ", tr("fore_table_title", lang)),
          DTOutput(ns("fc_forecast_table"))
      ),
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tags$span(icon("flask"), " ", tr("fore_diag_title", lang)),
          div(style = "max-height:400px;overflow-y:auto;font-size:12px;",
              verbatimTextOutput(ns("fc_diagnostics")))
      )
    ),
    fluidRow(
      box(width = 6, status = "warning", solidHeader = TRUE,
          title = tr("fore_acf_title", lang),
          plotOutput(ns("fc_acf_plot"), height = "350px")
      ),
      box(width = 6, status = "warning", solidHeader = TRUE,
          title = tr("fore_resid_title", lang),
          plotOutput(ns("fc_residuals_plot"), height = "350px")
      )
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = tags$span(icon("download"), " ", tr("fore_export", lang)),
          fluidRow(
            column(3, downloadButton(ns("fc_dl_csv"), tr("fore_dl_csv", lang),
                                     style = "background:#008DC9;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
            column(3, downloadButton(ns("fc_dl_plot"), tr("fore_dl_png", lang),
                                     style = "background:#4dac26;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
            column(3, downloadButton(ns("fc_dl_report"), tr("fore_dl_html", lang),
                                     style = "background:#e88400;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
            column(3, downloadButton(ns("fc_dl_json"), tr("fore_dl_json", lang),
                                     style = "background:#9b59b6;color:white;border:none;width:100%;border-radius:6px;padding:8px;"))
          )
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

advancedForecastingServer <- function(id, data_reactive, map_cols, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Parsing de dates tolérant (ISO, FR, US, année seule) ────────────────
    .as_date_flex <- function(x) {
      if (inherits(x, "Date")) return(x)
      if (inherits(x, "POSIXct")) return(as.Date(x))
      out <- suppressWarnings(lubridate::parse_date_time(
        as.character(x),
        orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY",
                   "Y-m-d", "d/m/Y", "m/d/Y", "Y/m/d", "Ym", "Y")
      ))
      as.Date(out)
    }

    # ── Détection dynamique des colonnes ────────────────────────────────────
    date_cols <- reactive({
      d <- data_reactive(); req(d)
      # Colonnes qui sont Date, POSIXct ou parseable en date
      candidates <- names(d)[sapply(d, function(x) {
        if (inherits(x, "Date") || inherits(x, "POSIXct")) return(TRUE)
        if (!is.character(x)) return(FALSE)
        vals <- head(na.omit(x), 20)
        if (length(vals) == 0) return(FALSE)
        any(!is.na(.as_date_flex(vals)))
      })]
      # Aussi chercher par nom
      name_match <- grep("date|jour|day|month|year|mois|annee|time|periode",
                         names(d), ignore.case = TRUE, value = TRUE)
      unique(c(candidates, name_match))
    })

    num_cols <- reactive({
      d <- data_reactive(); req(d)
      names(d)[sapply(d, is.numeric)]
    })

    output$sel_date_col <- renderUI({
      cols <- date_cols()
      if (length(cols) == 0) {
        return(tags$div(class = "alerte-box alerte-crit",
          "Aucune colonne de date détectée dans vos données."))
      }
      selectInput(ns("date_col"), "Colonne de date :", choices = cols, width = "100%")
    })

    output$sel_value_col <- renderUI({
      cols <- num_cols()
      if (length(cols) == 0) {
        return(tags$div(class = "alerte-box alerte-crit",
          "Aucune colonne numérique détectée."))
      }
      selectInput(ns("value_col"), "Variable à prévoir :", choices = cols, width = "100%")
    })

    output$sel_agg_fn <- renderUI({
      selectInput(ns("agg_fn"), "Agrégation temporelle :",
                  choices = c("Moyenne" = "mean", "Somme" = "sum",
                              "Médiane" = "median", "Comptage" = "n"),
                  selected = "mean", width = "100%")
    })

    output$data_info <- renderUI({
      d <- data_reactive()
      if (is.null(d)) return(NULL)
      tags$div(style = "font-size:11px;color:#666;margin-top:8px;",
        paste0(nrow(d), " observations · ", length(num_cols()), " var. numériques · ",
               length(date_cols()), " colonnes de date détectées"))
    })

    # ── Préparation des données temporelles ─────────────────────────────────
    ts_data <- reactive({
      d <- data_reactive()
      req(d, input$date_col, input$value_col, input$agg_fn)
      req(input$date_col %in% names(d), input$value_col %in% names(d))

      tryCatch({
        d_ts <- d |>
          mutate(.date = .as_date_flex(.data[[input$date_col]])) |>
          filter(!is.na(.date), !is.na(.data[[input$value_col]])) |>
          arrange(.date)

        if (nrow(d_ts) < 6) return(NULL)

        # Agréger par période
        freq_label <- input$fc_temporal
        d_agg <- d_ts |>
          mutate(.period = switch(freq_label,
            "month" = floor_date(.date, "month"),
            "quarter" = floor_date(.date, "quarter"),
            "week" = floor_date(.date, "week")
          )) |>
          group_by(.period) |>
          summarise(
            value = switch(input$agg_fn,
              "mean" = mean(.data[[input$value_col]], na.rm = TRUE),
              "sum" = sum(.data[[input$value_col]], na.rm = TRUE),
              "median" = median(.data[[input$value_col]], na.rm = TRUE),
              "n" = n()
            ),
            n_obs = n(),
            .groups = "drop"
          ) |>
          rename(date = .period) |>
          filter(!is.na(value)) |>
          arrange(date)

        if (nrow(d_agg) < 6) return(NULL)
        d_agg
      }, error = function(e) NULL)
    })

    # ── Modèle de prévision ─────────────────────────────────────────────────
    forecast_result <- eventReactive(input$fc_run, {
      req(ts_data())

      withProgress(message = "Ajustement du modèle...", value = 0, {
        d <- ts_data()

        if (nrow(d) < 6) {
          showNotification("⚠️ Besoin d'au moins 6 périodes pour les prévisions",
                          type = "warning", duration = 8)
          return(NULL)
        }

        # Fréquence
        freq <- switch(input$fc_temporal,
          "month" = 12, "quarter" = 4, "week" = 52
        )
        ts_obj <- ts(d$value, frequency = min(freq, nrow(d) - 1))

        # ADF test
        adf_test <- tryCatch(tseries::adf.test(d$value), error = function(e) NULL)

        incProgress(0.3, detail = paste0("Ajustement ", input$fc_method, "..."))

        # Modèle
        model_fit <- tryCatch({
          if (input$fc_method == "arima") {
            auto.arima(ts_obj, stepwise = TRUE, approximation = FALSE,
                       max.d = 2, max.D = 1, seasonal = TRUE)
          } else {
            ets(ts_obj)
          }
        }, error = function(e) NULL)

        incProgress(0.9, detail = "Génération prévisions")

        if (is.null(model_fit)) {
          showNotification("❌ Impossible d'ajuster le modèle. Essayez avec plus de données ou une autre méthode.",
                          type = "error")
          return(NULL)
        }

        list(
          data = d,
          model = model_fit,
          method = input$fc_method,
          adf_test = adf_test,
          ts_obj = ts_obj,
          h = input$fc_horizon,
          var_name = input$value_col,
          agg_fn = input$agg_fn,
          temporal = input$fc_temporal
        )
      })
    })

    # ── Graphique principal ─────────────────────────────────────────────────
    output$fc_forecast_plot <- renderPlotly({
      req(forecast_result())
      res <- forecast_result()
      if (is.null(res$model)) return(plotly_empty())

      fc <- forecast::forecast(res$model, h = res$h)

      by_unit <- switch(res$temporal, "month" = "month", "quarter" = "3 months", "week" = "week")

      df_hist <- tibble(date = res$data$date, value = res$data$value, type = "Historique")
      future_dates <- seq(tail(res$data$date, 1), by = by_unit, length.out = res$h + 1)[-1]
      df_fc <- tibble(
        date = future_dates[1:length(fc$mean)],
        value = as.numeric(fc$mean),
        lower = as.numeric(fc$lower[, 2]),
        upper = as.numeric(fc$upper[, 2]),
        type = "Prévision"
      )

      p <- ggplot() +
        geom_line(data = df_hist, aes(x = date, y = value, color = "Historique"), linewidth = 1) +
        geom_line(data = df_fc, aes(x = date, y = value, color = "Prévision"), linewidth = 1, linetype = "dashed") +
        geom_ribbon(data = df_fc, aes(x = date, ymin = lower, ymax = upper, fill = "IC 95%"), alpha = 0.2) +
        scale_color_manual(values = c("Historique" = "#008DC9", "Prévision" = "#e88400")) +
        scale_fill_manual(values = c("IC 95%" = "#e88400")) +
        labs(title = paste0("Prévisions — ", res$var_name, " (", res$agg_fn, ", ", res$temporal, ")"),
             subtitle = paste0("Méthode : ", toupper(res$method), " | Horizon : ", res$h, " périodes"),
             x = "Date", y = res$var_name, color = "", fill = "") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")

      ggplotly(p)
    })

    # ── Tableau de prévisions ───────────────────────────────────────────────
    output$fc_forecast_table <- renderDT({
      req(forecast_result())
      res <- forecast_result()
      if (is.null(res$model)) return(NULL)

      fc <- forecast::forecast(res$model, h = res$h)
      by_unit <- switch(res$temporal, "month" = "month", "quarter" = "3 months", "week" = "week")
      future_dates <- seq(tail(res$data$date, 1), by = by_unit, length.out = res$h + 1)[-1]

      tibble(
        Date = future_dates[1:length(fc$mean)],
        Prévision = round(as.numeric(fc$mean), 2),
        `IC 80% Bas` = round(as.numeric(fc$lower[, 1]), 2),
        `IC 80% Haut` = round(as.numeric(fc$upper[, 1]), 2),
        `IC 95% Bas` = round(as.numeric(fc$lower[, 2]), 2),
        `IC 95% Haut` = round(as.numeric(fc$upper[, 2]), 2)
      ) |>
        datatable(rownames = FALSE, options = list(dom = "tp", pageLength = 12),
                  class = "compact stripe hover")
    })

    # ── Diagnostics ─────────────────────────────────────────────────────────
    output$fc_diagnostics <- renderPrint({
      req(forecast_result())
      res <- forecast_result()

      cat("═══════════════════════════════════════════════════════════\n")
      cat("DIAGNOSTICS —", toupper(res$method), "\n")
      cat("Variable :", res$var_name, "| Agrégation :", res$agg_fn, "\n")
      cat("═══════════════════════════════════════════════════════════\n\n")

      if (!is.null(res$adf_test)) {
        cat("TEST DE STATIONNARITÉ (ADF Test)\n")
        cat("H0: Série non-stationnaire | α = 0.05\n")
        cat("p-value:", round(res$adf_test$p.value, 4), "\n")
        if (res$adf_test$p.value < 0.05)
          cat("✅ Série stationnaire (différenciation non requise)\n\n")
        else
          cat("⚠️  Série non-stationnaire (différenciation recommandée)\n\n")
      }

      if (!is.null(res$model)) {
        cat("MODÈLE SÉLECTIONNÉ\n")
        if (res$method == "arima") {
          cat("Formule: ARIMA(", paste0(res$model$arma[1:3], collapse = ","), ")\n")
        } else {
          cat("Méthode: ETS\n")
        }
        cat("AIC:", round(AIC(res$model), 2), "\n")
        cat("BIC:", round(BIC(res$model), 2), "\n\n")

        cat("TESTS DE RÉSIDUS\n")
        lb_test <- Box.test(residuals(res$model), lag = min(10, nrow(res$data) - 1), type = "Ljung-Box")
        cat("Ljung-Box p-value:", round(lb_test$p.value, 4), "\n")
        if (lb_test$p.value > 0.05)
          cat("  ✅ Pas d'autocorrélation résiduelle\n")
        else
          cat("  ⚠️  Autocorrélation détectée dans les résidus\n")

        sw_test <- tryCatch(shapiro.test(head(residuals(res$model), 5000)), error = function(e) NULL)
        if (!is.null(sw_test)) {
          cat("\nShapiro-Wilk (normalité résidus) p-value:", round(sw_test$p.value, 4), "\n")
          if (sw_test$p.value > 0.05)
            cat("  ✅ Résidus normalement distribués\n")
          else
            cat("  ⚠️  Résidus non-normaux\n")
        }
      }

      cat("\nNOMBRE DE PÉRIODES:", nrow(res$data), "\n")
      cat("HORIZON PRÉVISIONS:", res$h, "périodes\n")
    })

    # ── ACF/PACF ────────────────────────────────────────────────────────────
    output$fc_acf_plot <- renderPlot({
      req(forecast_result(), input$fc_acf_plot)
      res <- forecast_result()
      par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
      acf(res$ts_obj, main = "ACF — Autocorrélation", col = "#008DC9", lwd = 2)
      pacf(res$ts_obj, main = "PACF — Autocorrélation partielle", col = "#008DC9", lwd = 2)
      par(mfrow = c(1, 1))
    })

    # ── Résidus ─────────────────────────────────────────────────────────────
    output$fc_residuals_plot <- renderPlot({
      req(forecast_result(), input$fc_residuals)
      res <- forecast_result()
      if (is.null(res$model)) return(NULL)

      resids <- residuals(res$model)
      par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
      plot(resids, main = "Résidus au fil du temps", ylab = "Résidu", xlab = "Temps",
           col = "#008DC9", type = "l")
      abline(h = 0, col = "#c9161d", lty = 2)
      hist(resids, main = "Distribution résidus", xlab = "Résidu", breaks = 20, col = "#008DC9", border = "white")
      qqnorm(resids, main = "Q-Q Plot (normalité)", col = "#008DC9")
      qqline(resids, col = "#c9161d", lwd = 2)
      acf(resids, main = "ACF résidus", col = "#008DC9", lwd = 2)
      par(mfrow = c(1, 1))
    })

    # ── Downloads ───────────────────────────────────────────────────────────
    output$fc_dl_csv <- downloadHandler(
      filename = function() paste0("previsions_", input$value_col, "_", Sys.Date(), ".csv"),
      content = function(file) {
        req(forecast_result())
        res <- forecast_result()
        fc <- forecast::forecast(res$model, h = res$h)
        by_unit <- switch(res$temporal, "month" = "month", "quarter" = "3 months", "week" = "week")
        future_dates <- seq(tail(res$data$date, 1), by = by_unit, length.out = res$h + 1)[-1]
        df <- data.frame(
          Date = future_dates[1:length(fc$mean)],
          Prevision = round(as.numeric(fc$mean), 2),
          IC_95_Bas = round(as.numeric(fc$lower[, 2]), 2),
          IC_95_Haut = round(as.numeric(fc$upper[, 2]), 2)
        )
        write.csv(df, file, row.names = FALSE)
      }
    )

    output$fc_dl_plot <- downloadHandler(
      filename = function() paste0("previsions_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(forecast_result())
        res <- forecast_result()
        fc <- forecast::forecast(res$model, h = res$h)
        png(file, width = 1200, height = 600, res = 150)
        plot(fc, main = paste0("Prévisions — ", res$var_name),
             xlab = "Temps", ylab = res$var_name, col = "#008DC9", fcol = "#e88400")
        dev.off()
      }
    )

    output$fc_dl_report <- downloadHandler(
      filename = function() paste0("rapport_previsions_", Sys.Date(), ".html"),
      content = function(file) {
        req(forecast_result())
        res <- forecast_result()
        fc <- forecast::forecast(res$model, h = res$h)
        by_unit <- switch(res$temporal, "month" = "month", "quarter" = "3 months", "week" = "week")
        future_dates <- seq(tail(res$data$date, 1), by = by_unit, length.out = res$h + 1)[-1]
        html_content <- paste0(
          "<html><head><meta charset='UTF-8'><style>body{font-family:Calibri,sans-serif;max-width:800px;margin:auto;padding:20px}",
          "table{border-collapse:collapse;width:100%}th,td{border:1px solid #ddd;padding:8px;text-align:right}",
          "th{background:#008DC9;color:white}</style></head><body>",
          "<h1>Rapport de Prévisions Temporelles</h1>",
          "<p><b>Variable :</b> ", res$var_name, " | <b>Agrégation :</b> ", res$agg_fn,
          " | <b>Méthode :</b> ", toupper(res$method), "</p>",
          "<p><b>Périodes historiques :</b> ", nrow(res$data), " | <b>Horizon :</b> ", res$h, "</p>",
          "<h2>Prévisions</h2><table><tr><th>Date</th><th>Prévision</th><th>IC 95%</th></tr>",
          paste0("<tr><td>", future_dates[1:length(fc$mean)], "</td><td>",
                 round(as.numeric(fc$mean), 2), "</td><td>",
                 round(as.numeric(fc$lower[, 2]), 2), " — ",
                 round(as.numeric(fc$upper[, 2]), 2), "</td></tr>", collapse = ""),
          "</table><p style='color:#888;font-size:11px;margin-top:20px;'>",
          "Généré par NutriAnalyse Pro v2 le ", format(Sys.Date(), "%d/%m/%Y"),
          "</p></body></html>")
        writeLines(html_content, file)
      }
    )

    output$fc_dl_json <- downloadHandler(
      filename = function() paste0("previsions_", input$value_col, "_", Sys.Date(), ".json"),
      content = function(file) {
        req(forecast_result())
        res <- forecast_result()
        fc <- forecast::forecast(res$model, h = res$h)
        by_unit <- switch(res$temporal, "month" = "month", "quarter" = "3 months", "week" = "week")
        future_dates <- seq(tail(res$data$date, 1), by = by_unit, length.out = res$h + 1)[-1]
        json_list <- list(
          variable = res$var_name,
          method = res$method,
          aggregation = res$agg_fn,
          aic = round(AIC(res$model), 2),
          observations = nrow(res$data),
          forecasts = list(
            dates = as.character(future_dates[1:length(fc$mean)]),
            values = round(as.numeric(fc$mean), 2),
            ci_lower = round(as.numeric(fc$lower[, 2]), 2),
            ci_upper = round(as.numeric(fc$upper[, 2]), 2)
          )
        )
        write_json(json_list, file, pretty = TRUE)
      }
    )
  })
}
