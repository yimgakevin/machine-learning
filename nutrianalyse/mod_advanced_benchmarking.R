# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_advanced_benchmarking.R — Benchmarking & Analyse Comparative          ║
# ║  Détection dynamique indicateurs | Calculs réels | Référence custom        ║
# ║  Niveau: Doctoral Research Grade — v2 Dynamic                             ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(DT)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

advancedBenchmarkingUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #008DC9; border-radius: 6px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#008DC9;",
              icon("chart-column"), tr("bench_title", lang)),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             tr("bench_desc", lang),
             )
    ),
    fluidRow(
      box(width = 3, status = "info", solidHeader = TRUE, title = tr("bench_config", lang),
          # --- Données de référence ---
          tags$div(class = "section-header", tr("bench_ref_hdr", lang)),
          selectInput(ns("bench_ref_source"), tr("bench_ref_source", lang),
                      choices = setNames(c("internal","upload"), c(tr("bench_ref_internal",lang),tr("bench_ref_upload",lang))),
                      width = "100%"),

          # Branche interne : grouper ses propres données
          conditionalPanel(
            sprintf("input['%s'] == 'internal'", ns("bench_ref_source")),
            uiOutput(ns("bench_group_col_ui")),
            uiOutput(ns("bench_ref_group_ui"))
          ),

          # Branche upload
          conditionalPanel(
            sprintf("input['%s'] == 'upload'", ns("bench_ref_source")),
            fileInput(ns("bench_upload"), tr("bench_ref_file", lang),
                      accept = c(".csv", ".xlsx"), width = "100%"),
            tags$small(style = "color:#666;",
                       tr("bench_ref_note", lang))
          ),
          hr(),

          # --- Indicateurs numériques (dynamiques) ---
          tags$div(class = "section-header", tr("bench_indicators_hdr", lang)),
          uiOutput(ns("bench_indicators_ui")),
          hr(),

          # --- Fonction d'agrégation ---
          selectInput(ns("bench_agg_fn"), tr("bench_agg_label", lang),
                      choices = setNames(c("mean","median","sum"), c(tr("bench_agg_mean",lang),tr("bench_agg_median",lang),tr("bench_agg_sum",lang))),
                      selected = "mean", width = "100%"),
          hr(),
          actionButton(ns("bench_run"), tr("bench_btn_run", lang),
                       class = "btn btn-oms btn-block", width = "100%",
                       style = "font-size:13px;padding:10px;"),
          uiOutput(ns("bench_status"))
      ),
      box(width = 9, status = "primary", solidHeader = TRUE,
          title = tr("bench_main_title", lang),
          plotlyOutput(ns("bench_comparison_plot"), height = "500px")
      )
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tr("bench_gap_title", lang),
          plotlyOutput(ns("bench_gap_plot"), height = "400px")
      ),
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tr("bench_percentile_title", lang),
          plotlyOutput(ns("bench_percentile_plot"), height = "400px")
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = tr("bench_detailed_title", lang),
          DTOutput(ns("bench_detailed_table"))
      )
    ),
    fluidRow(
      box(width = 6, status = "warning", solidHeader = TRUE,
          title = tr("bench_rankings_title", lang),
          DTOutput(ns("bench_rankings_table"))
      ),
      box(width = 6, status = "warning", solidHeader = TRUE,
          title = tr("bench_zscore_title", lang),
          plotlyOutput(ns("bench_zscore_plot"), height = "400px")
      )
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = tags$span(icon("download"), " Exports comparatifs"),
          fluidRow(
            column(3, downloadButton(ns("bench_dl_comparison"), "📊 Tableau comparatif",
                                     style = "background:#008DC9;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("bench_dl_gaps"), "📈 Gap analysis",
                                     style = "background:#4dac26;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("bench_dl_rankings"), "🏆 Rankings",
                                     style = "background:#e88400;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("bench_dl_report"), "📄 Rapport HTML",
                                     style = "background:#9b59b6;color:white;border:none;width:100%;"))
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "Rapport d'analyse comparative",
          verbatimTextOutput(ns("bench_report"))
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

advancedBenchmarkingServer <- function(id, data_reactive, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Helpers ──────────────────────────────────────────────────────────────
    numeric_cols <- reactive({
      d <- data_reactive(); req(d)
      names(d)[sapply(d, is.numeric)]
    })

    categorical_cols <- reactive({
      d <- data_reactive(); req(d)
      names(d)[sapply(d, function(x) is.character(x) || is.factor(x))]
    })

    agg_function <- reactive({
      switch(input$bench_agg_fn %||% "mean",
             "mean"   = \(x) mean(x, na.rm = TRUE),
             "median" = \(x) median(x, na.rm = TRUE),
             "sum"    = \(x) sum(x, na.rm = TRUE))
    })

    # ── UI dynamique : indicateurs numériques ────────────────────────────────
    output$bench_indicators_ui <- renderUI({
      req(numeric_cols())
      nc <- numeric_cols()
      # Pré-sélectionner les 3 premiers
      pre_sel <- head(nc, min(3, length(nc)))
      checkboxGroupInput(ns("bench_indicators"), NULL,
                         choices = nc, selected = pre_sel, width = "100%")
    })

    # ── UI dynamique : colonne de groupement ─────────────────────────────────
    output$bench_group_col_ui <- renderUI({
      req(categorical_cols())
      cc <- categorical_cols()
      selectInput(ns("bench_group_col"), "Grouper par :",
                  choices = c("— Sélectionner —" = "", cc),
                  width = "100%")
    })

    # ── UI dynamique : choisir le groupe de référence ────────────────────────
    output$bench_ref_group_ui <- renderUI({
      req(input$bench_group_col, input$bench_group_col != "")
      d <- data_reactive(); req(d)
      levels_vec <- sort(unique(as.character(d[[input$bench_group_col]])))
      selectInput(ns("bench_ref_level"), "Groupe de référence :",
                  choices = c("Moyenne globale" = "__global__", levels_vec),
                  width = "100%")
    })

    # ── Message d'aide : aucun indicateur sélectionné ───────────────────────
    output$bench_status <- renderUI({
      d <- data_reactive()
      if (is.null(d)) return(NULL)
      inds <- intersect(input$bench_indicators %||% character(0), names(d))
      if (length(inds) == 0) {
        tags$p("Sélectionnez au moins un indicateur numérique.",
               style = "color:#c9161d;font-size:11px;margin-top:6px;")
      }
    })

    # ── Données de référence uploadées ───────────────────────────────────────
    uploaded_ref <- reactive({
      req(input$bench_upload)
      ext <- tools::file_ext(input$bench_upload$name)
      tryCatch({
        if (ext == "csv") {
          read.csv(input$bench_upload$datapath, stringsAsFactors = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          readxl::read_excel(input$bench_upload$datapath)
        } else {
          showNotification("Format non supporté. Utilisez CSV ou Excel.", type = "error")
          NULL
        }
      }, error = function(e) {
        showNotification(paste("Erreur lecture fichier :", e$message), type = "error")
        NULL
      })
    })

    # ══════════════════════════════════════════════════════════════════════════
    # Calculs comparatifs (DEPUIS les vraies données)
    # ══════════════════════════════════════════════════════════════════════════

    # Résumé de vos données par groupe
    your_summary <- reactive({
      req(data_reactive(), input$bench_indicators, input$bench_run)
      d <- data_reactive()
      inds <- intersect(input$bench_indicators, names(d))
      if (length(inds) == 0) return(NULL)
      fn <- agg_function()

      if (input$bench_ref_source == "internal" &&
          !is.null(input$bench_group_col) && input$bench_group_col != "") {
        # Groupé
        d |>
          group_by(.group = .data[[input$bench_group_col]]) |>
          summarise(across(all_of(inds), fn), .groups = "drop") |>
          rename(groupe = .group)
      } else {
        # Global (une seule ligne)
        d |>
          summarise(across(all_of(inds), fn)) |>
          mutate(groupe = "Votre enquête") |>
          relocate(groupe)
      }
    })

    # Référence
    reference_summary <- reactive({
      req(input$bench_indicators, input$bench_run)
      d <- data_reactive(); req(d)
      inds <- intersect(input$bench_indicators, names(d))
      if (length(inds) == 0) return(NULL)
      fn <- agg_function()

      if (input$bench_ref_source == "upload") {
        # Fichier uploadé
        ref <- uploaded_ref(); req(ref)
        common_inds <- intersect(inds, names(ref))
        if (length(common_inds) == 0) return(NULL)
        ref |>
          summarise(across(all_of(common_inds), fn)) |>
          mutate(groupe = "Référence externe") |>
          relocate(groupe)

      } else {
        # Interne : moyenne globale ou un groupe spécifique
        ref_level <- input$bench_ref_level %||% "__global__"

        if (ref_level == "__global__") {
          d |>
            summarise(across(all_of(inds), fn)) |>
            mutate(groupe = "Moyenne globale") |>
            relocate(groupe)
        } else {
          req(input$bench_group_col, input$bench_group_col != "")
          d |>
            filter(.data[[input$bench_group_col]] == ref_level) |>
            summarise(across(all_of(inds), fn)) |>
            mutate(groupe = paste0("Réf: ", ref_level)) |>
            relocate(groupe)
        }
      }
    })

    # Données combinées
    comparison_data <- reactive({
      req(your_summary(), reference_summary())
      ys <- your_summary()
      rs <- reference_summary()
      common_cols <- intersect(names(ys), names(rs))
      bind_rows(
        ys |> select(all_of(common_cols)) |> mutate(source = "Vos données"),
        rs |> select(all_of(common_cols)) |> mutate(source = "Référence")
      )
    })

    # Indicateurs communs effectivement utilisés
    used_indicators <- reactive({
      req(comparison_data())
      cd <- comparison_data()
      setdiff(names(cd), c("groupe", "source"))
    })

    # ── Graphique de comparaison ─────────────────────────────────────────────
    output$bench_comparison_plot <- renderPlotly({
      req(comparison_data(), used_indicators())

      plot_data <- comparison_data() |>
        pivot_longer(all_of(used_indicators()),
                     names_to = "indicateur", values_to = "valeur")

      p <- ggplot(plot_data, aes(x = indicateur, y = valeur, fill = groupe)) +
        geom_col(position = "dodge", alpha = 0.8) +
        facet_wrap(~source, scales = "free_y") +
        labs(title = "Comparaison indicateurs", x = NULL, y = "Valeur", fill = "Groupe") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p)
    })

    # ── Gap analysis ─────────────────────────────────────────────────────────
    gap_data_reactive <- reactive({
      req(your_summary(), reference_summary(), used_indicators())
      inds <- used_indicators()

      ys <- your_summary()
      rs <- reference_summary()

      # Si votre résumé a plusieurs groupes, prendre la moyenne
      your_vals <- ys |> summarise(across(any_of(inds), \(x) mean(x, na.rm = TRUE)))
      ref_vals  <- rs |> summarise(across(any_of(inds), \(x) mean(x, na.rm = TRUE)))

      common <- intersect(names(your_vals), names(ref_vals))
      common <- intersect(common, inds)

      tibble(
        indicateur  = common,
        votre_valeur = as.numeric(your_vals[1, common]),
        reference    = as.numeric(ref_vals[1, common]),
        ecart        = as.numeric(your_vals[1, common]) - as.numeric(ref_vals[1, common])
      )
    })

    output$bench_gap_plot <- renderPlotly({
      req(gap_data_reactive())
      gd <- gap_data_reactive()

      p <- ggplot(gd, aes(x = reorder(indicateur, abs(ecart)), y = ecart,
                           fill = ifelse(ecart > 0, "Supérieur", "Inférieur"))) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = round(ecart, 2)), vjust = -0.5, size = 3) +
        scale_fill_manual(values = c("Supérieur" = "#4dac26", "Inférieur" = "#c9161d")) +
        labs(title = "Écarts vs référence", x = NULL, y = "Différence", fill = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p)
    })

    # ── Percentiles ──────────────────────────────────────────────────────────
    output$bench_percentile_plot <- renderPlotly({
      req(your_summary(), reference_summary(), used_indicators())
      inds <- used_indicators()

      ys <- your_summary()
      rs <- reference_summary()

      # Besoin de plusieurs groupes pour calculer un percentile
      if (nrow(ys) < 3 && nrow(rs) < 3) {
        return(plotly_empty() |>
                 layout(title = "Percentiles : insuffisamment de groupes pour calculer"))
      }

      # Pool de valeurs de référence (si interne groupé, utiliser your_summary)
      ref_pool <- if (nrow(ys) >= 3) ys else rs

      # Votre valeur globale
      your_global <- ys |>
        summarise(across(any_of(inds), \(x) mean(x, na.rm = TRUE)))

      pct_data <- tibble(
        indicateur = inds,
        percentile = sapply(inds, function(ind) {
          if (!ind %in% names(ref_pool) || !ind %in% names(your_global)) return(NA_real_)
          ref_vals <- ref_pool[[ind]]
          your_val <- your_global[[ind]]
          ecdf(ref_vals)(your_val) * 100
        })
      ) |> filter(!is.na(percentile))

      if (nrow(pct_data) == 0) return(plotly_empty())

      p <- ggplot(pct_data, aes(x = reorder(indicateur, percentile), y = percentile,
                                 fill = percentile)) +
        geom_col(alpha = 0.8) +
        geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.5) +
        geom_text(aes(label = round(percentile, 0)), vjust = -0.5, size = 3) +
        scale_fill_viridis_c() +
        labs(title = "Percentiles de votre enquête vs distribution de référence",
             x = NULL, y = "Percentile (%)", fill = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p)
    })

    # ── Tableau détaillé ─────────────────────────────────────────────────────
    output$bench_detailed_table <- renderDT({
      req(comparison_data())
      comparison_data() |>
        mutate(across(where(is.numeric), \(x) round(x, 2))) |>
        datatable(rownames = FALSE, options = list(dom = "tp", pageLength = 20),
                  class = "compact stripe")
    })

    # ── Rankings ─────────────────────────────────────────────────────────────
    output$bench_rankings_table <- renderDT({
      req(your_summary(), used_indicators())
      ys <- your_summary()
      inds <- used_indicators()

      if (nrow(ys) < 2) {
        return(datatable(
          tibble(Info = "Rankings nécessitent au moins 2 groupes. Sélectionnez une variable de groupement."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }

      rank_data <- ys |>
        mutate(across(all_of(inds), \(x) rank(desc(x), na.last = "keep"),
                      .names = "rank_{.col}")) |>
        mutate(score_moyen = rowMeans(across(starts_with("rank_")), na.rm = TRUE)) |>
        arrange(score_moyen) |>
        mutate(position = row_number()) |>
        select(position, groupe, score_moyen)

      rank_data |>
        rename(Position = position, Groupe = groupe, `Score ranking` = score_moyen) |>
        datatable(rownames = FALSE, options = list(dom = "tp"), class = "compact stripe")
    })

    # ── Z-scores ─────────────────────────────────────────────────────────────
    output$bench_zscore_plot <- renderPlotly({
      req(gap_data_reactive(), your_summary(), used_indicators())
      inds <- used_indicators()
      ys <- your_summary()

      # Besoin de variation pour calculer un z-score
      if (nrow(ys) < 3) {
        return(plotly_empty() |>
                 layout(title = "Z-scores : insuffisamment de groupes"))
      }

      ref_mean <- ys |> summarise(across(any_of(inds), \(x) mean(x, na.rm = TRUE)))
      ref_sd   <- ys |> summarise(across(any_of(inds), \(x) sd(x, na.rm = TRUE)))

      # Votre valeur globale
      your_global <- data_reactive() |>
        summarise(across(any_of(inds), \(x) agg_function()(x)))

      zs <- tibble(
        indicateur = inds,
        zscore = sapply(inds, function(ind) {
          s <- ref_sd[[ind]]
          if (is.na(s) || s == 0) return(NA_real_)
          (your_global[[ind]] - ref_mean[[ind]]) / s
        })
      ) |> filter(!is.na(zscore))

      if (nrow(zs) == 0) return(plotly_empty())

      p <- ggplot(zs, aes(x = reorder(indicateur, zscore), y = zscore,
                           fill = ifelse(zscore > 0, "Au-dessus", "En-dessous"))) +
        geom_col(alpha = 0.8) +
        geom_hline(yintercept = 0, linewidth = 1) +
        geom_hline(yintercept = c(-1, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
        scale_fill_manual(values = c("Au-dessus" = "#4dac26", "En-dessous" = "#c9161d")) +
        labs(title = tr("bench_zscore_title", lang), x = NULL, y = "Z-score", fill = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p)
    })

    # ── Rapport ──────────────────────────────────────────────────────────────
    output$bench_report <- renderPrint({
      req(gap_data_reactive())
      gd <- gap_data_reactive()

      cat("═══════════════════════════════════════════════════════════\n")
      cat("RAPPORT D'ANALYSE COMPARATIVE\n")
      cat("═══════════════════════════════════════════════════════════\n\n")

      cat("SOURCE RÉFÉRENCE :",
          if (input$bench_ref_source == "upload") "Fichier externe" else "Données internes",
          "\n")
      cat("AGRÉGATION       :", input$bench_agg_fn, "\n")
      cat("INDICATEURS      :", paste(gd$indicateur, collapse = ", "), "\n\n")

      cat("RÉSUMÉ DES ÉCARTS\n")
      print(as.data.frame(gd |> mutate(across(where(is.numeric), \(x) round(x, 3)))))

      sup <- sum(gd$ecart > 0)
      inf <- sum(gd$ecart < 0)
      cat("\n→ ", sup, "indicateur(s) au-dessus de la référence\n")
      cat("→ ", inf, "indicateur(s) en-dessous de la référence\n\n")

      if (inf > 0) {
        worst <- gd |> filter(ecart < 0) |> arrange(ecart) |> head(3)
        cat("INDICATEURS PRIORITAIRES (plus grands écarts négatifs) :\n")
        for (i in seq_len(nrow(worst))) {
          cat("  • ", worst$indicateur[i], ": écart =", round(worst$ecart[i], 2), "\n")
        }
      }
    })

    # ── Downloads ────────────────────────────────────────────────────────────
    output$bench_dl_comparison <- downloadHandler(
      filename = function() paste0("comparaison_", Sys.Date(), ".csv"),
      content = function(file) {
        req(comparison_data())
        write.csv(comparison_data(), file, row.names = FALSE)
      }
    )

    output$bench_dl_gaps <- downloadHandler(
      filename = function() paste0("gap_analysis_", Sys.Date(), ".csv"),
      content = function(file) {
        req(gap_data_reactive())
        write.csv(gap_data_reactive(), file, row.names = FALSE)
      }
    )

    output$bench_dl_rankings <- downloadHandler(
      filename = function() paste0("rankings_", Sys.Date(), ".csv"),
      content = function(file) {
        req(your_summary(), used_indicators())
        ys <- your_summary()
        inds <- used_indicators()
        if (nrow(ys) < 2) {
          write.csv(tibble(info = "Pas assez de groupes"), file, row.names = FALSE)
          return()
        }
        rank_data <- ys |>
          mutate(across(all_of(inds), \(x) rank(desc(x), na.last = "keep"),
                        .names = "rank_{.col}")) |>
          mutate(score_moyen = rowMeans(across(starts_with("rank_")), na.rm = TRUE)) |>
          arrange(score_moyen)
        write.csv(rank_data, file, row.names = FALSE)
      }
    )

    output$bench_dl_report <- downloadHandler(
      filename = function() paste0("rapport_benchmarking_", Sys.Date(), ".html"),
      content = function(file) {
        req(gap_data_reactive(), comparison_data())
        gd <- gap_data_reactive()
        cd <- comparison_data()

        tbl_rows <- paste(
          apply(gd |> mutate(across(where(is.numeric), \(x) round(x, 3))), 1, function(r) {
            paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")
          }),
          collapse = "\n"
        )

        html <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
          "<style>body{font-family:sans-serif;margin:20px;}",
          "table{border-collapse:collapse;width:100%;}",
          "th,td{border:1px solid #ddd;padding:8px;text-align:left;}",
          "th{background:#008DC9;color:white;}</style></head><body>",
          "<h1>Rapport de Benchmarking</h1>",
          "<p>Date : ", Sys.Date(), "</p>",
          "<p>Source : ", input$bench_ref_source, " | Agrégation : ", input$bench_agg_fn, "</p>",
          "<h2>Écarts vs Référence</h2>",
          "<table><tr><th>Indicateur</th><th>Votre valeur</th><th>Référence</th><th>Écart</th></tr>",
          tbl_rows,
          "</table>",
          "<h2>Nombre de groupes comparés</h2>",
          "<p>", nrow(cd), " lignes dans le tableau de comparaison.</p>",
          "</body></html>"
        )
        write(html, file)
      }
    )
  })
}
