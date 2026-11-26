# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_epidemio.R — Module Shiny Expert : Analyses épidémiologiques          ║
# ║  Tableau 2×2 · RR · OR · PAF · Multi-exposition · Cochran-Armitage         ║
# ║  Stratification · Mantel-Haenszel · Forest plot · Exports complets         ║
# ║  Niveau: Doctoral Research Grade                                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

source("module_epidemio.R")
source("module_epidemio_expert.R")
source("ui_epidemio_expert.R")

# ── UI ────────────────────────────────────────────────────────────────────────
epidemioUI <- function(id, lang = "fr") {
  ns <- NS(id)

  tagList(
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #008DC9; border-radius: 6px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#008DC9;",
              icon("virus"), tr("epid_title", lang)),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             tr("epid_desc", lang),
             )
    ),

    do.call(tabsetPanel, c(
      list(id = ns("epid_tabs")),
      list(

      # ── Onglet 1 : Analyse 2×2 ────────────────────────────────────────────
      tabPanel(tags$span(icon("table"), " ", tr("epid_tab_2x2", lang)),
        br(),
        fluidRow(
          box(width = 3, status = "primary", solidHeader = TRUE,
              title = tags$span(icon("cog"), " ", tr("epid_params", lang)),
              uiOutput(ns("sel_exposition")),
              uiOutput(ns("sel_outcome")),
              sliderInput(ns("conf_level"), tr("epid_conf_level", lang),
                          min = 0.80, max = 0.99, value = 0.95, step = 0.01),
              hr(),
              actionButton(ns("run_2x2"), tr("epid_btn_analyse", lang),
                           class = "btn btn-oms btn-block", width = "100%",
                           style = "font-size:13px;padding:10px;"),
              hr(),
              tags$div(class = "section-header", tr("epid_interp", lang)),
              uiOutput(ns("interpretation_2x2"))
          ),
          box(width = 9, status = "info", solidHeader = TRUE,
              title = tags$span(icon("chart-bar"), " ", tr("epid_result_2x2", lang)),
              uiOutput(ns("res_2x2_html")),
              br(),
              DTOutput(ns("res_2x2_table")),
              br(),
              plotlyOutput(ns("plot_mesures_2x2"), height = "350px")
          )
        )
      ),

      # ── Onglet 2 : Multi-exposition ───────────────────────────────────────
      tabPanel(tags$span(icon("list"), " ", tr("epid_tab_multi", lang)),
        br(),
        fluidRow(
          box(width = 3, status = "warning", solidHeader = TRUE,
              title = tags$span(icon("list"), " ", tr("epid_variables", lang)),
              uiOutput(ns("sel_expositions_multi")),
              uiOutput(ns("sel_outcome_multi")),
              sliderInput(ns("conf_multi"), tr("epid_confiance", lang), 0.80, 0.99, 0.95, 0.01),
              hr(),
              checkboxInput(ns("multi_sort_or"), "Trier par OR", TRUE),
              checkboxInput(ns("multi_show_sig"), "Significatifs seulement", FALSE),
              hr(),
              actionButton(ns("run_multi"), tr("epid_btn_multi", lang),
                           class = "btn btn-oms btn-block", width = "100%",
                           style = "font-size:13px;padding:10px;")
          ),
          box(width = 9, status = "info", solidHeader = TRUE,
              title = tags$span(icon("chart-bar"), " ", tr("epid_forest_title", lang)),
              plotlyOutput(ns("forest_or"), height = "500px"),
              br(),
              DTOutput(ns("table_multi")),
              br(),
              uiOutput(ns("multi_summary"))
          )
        )
      ),

      # ── Onglet 3 : Cochran-Armitage (tendance) ───────────────────────────
      tabPanel(tags$span(icon("chart-line"), " ", tr("epid_tab_trend", lang)),
        br(),
        fluidRow(
          box(width = 3, status = "success", solidHeader = TRUE,
              title = tags$span(icon("sort-amount-up"), " ", tr("epid_params", lang)),
              uiOutput(ns("sel_var_ord")),
              uiOutput(ns("sel_outcome_ca")),
              hr(),
              tags$div(
                style = "background:#f0f8ff;border-left:3px solid #008DC9;padding:10px;border-radius:0 6px 6px 0;font-size:12px;",
                tags$b("Test de Cochran-Armitage"), tags$br(),
                tr("epid_ca_desc", lang),
                tags$br(),
                tags$span(style = "color:#888;", tr("epid_ca_example", lang))
              ),
              hr(),
              actionButton(ns("run_ca"), tr("epid_btn_trend", lang),
                           class = "btn btn-oms btn-block", width = "100%",
                           style = "font-size:13px;padding:10px;")
          ),
          box(width = 9, status = "info", solidHeader = TRUE,
              title = tr("epid_ca_title", lang),
              uiOutput(ns("ca_result_html")),
              br(),
              plotlyOutput(ns("ca_trend_plot"), height = "400px"),
              br(),
              DTOutput(ns("ca_table"))
          )
        )
      ),

      # ── Onglet 4 : Stratification Mantel-Haenszel ────────────────────────
      tabPanel(tags$span(icon("layer-group"), " ", tr("epid_tab_mh", lang)),
        br(),
        fluidRow(
          box(width = 3, status = "danger", solidHeader = TRUE,
              title = tags$span(icon("layer-group"), " ", tr("epid_params", lang)),
              uiOutput(ns("sel_expo_mh")),
              uiOutput(ns("sel_out_mh")),
              uiOutput(ns("sel_strata_mh")),
              hr(),
              tags$div(
                style = "background:#fff3e0;border-left:3px solid #e88400;padding:10px;border-radius:0 6px 6px 0;font-size:12px;",
                tags$b("Stratification Mantel-Haenszel"), tags$br(),
                tr("epid_mh_desc", lang),
                
              ),
              hr(),
              actionButton(ns("run_mh"), tr("epid_btn_mh", lang),
                           class = "btn btn-oms btn-block", width = "100%",
                           style = "font-size:13px;padding:10px;")
          ),
          box(width = 9, status = "info", solidHeader = TRUE,
              title = tr("epid_mh_title", lang),
              uiOutput(ns("mh_result_html")),
              br(),
              plotlyOutput(ns("mh_forest"), height = "400px"),
              br(),
              DTOutput(ns("mh_strata_table"))
          )
        )
      ),

      # ── Onglet 5 : Export ─────────────────────────────────────────────────
      tabPanel(tags$span(icon("download"), " ", tr("epid_tab_export", lang)),
        br(),
        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE,
              title = tags$span(icon("download"), " ", tr("epid_export_title", lang)),
              fluidRow(
                column(3, downloadButton(ns("dl_csv"), tr("epid_dl_csv", lang),
                  style = "background:#008DC9;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                column(3, downloadButton(ns("dl_multi_csv"), tr("epid_dl_multi", lang),
                  style = "background:#4dac26;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                column(3, downloadButton(ns("dl_forest_png"), tr("epid_dl_forest", lang),
                  style = "background:#e88400;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                column(3, downloadButton(ns("dl_excel"), tr("epid_dl_excel", lang),
                  style = "background:#7b2d8b;color:white;border:none;width:100%;border-radius:6px;padding:8px;"))
              ),
              br(),
              tags$div(style = "font-size:12px;color:#888;",
                tr("epid_export_note", lang))
          )
        )
      )
      ),

      # ── Onglets 6-12 : EXPERT ──────────────────────────────────────────────
      epid_expert_tabs(ns)
    ))
  )
}

# ── SERVER ────────────────────────────────────────────────────────────────────
epidemioServer <- function(id, df_f, map_cols, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Réactifs communs ────────────────────────────────────────────────────
    vars_cat <- reactive({
      d <- df_f(); req(d)
      noms <- names(d)[sapply(d, function(x) {
        (is.factor(x) || is.character(x) || is.logical(x) ||
         (is.numeric(x) && all(x %in% c(0, 1, NA))))
      })]
      noms[noms != ""]
    })

    vars_all <- reactive({
      d <- df_f(); req(d)
      names(d)[sapply(d, function(x) {
        is.factor(x) || is.character(x) || is.numeric(x)
      })]
    })

    # ── UI dynamiques ───────────────────────────────────────────────────────
    output$sel_exposition <- renderUI(selectInput(ns("var_expo"), "Variable exposition :", choices = vars_cat(), width = "100%"))
    output$sel_outcome    <- renderUI(selectInput(ns("var_out"), "Variable outcome :", choices = vars_cat(), width = "100%"))
    output$sel_expositions_multi <- renderUI(
      checkboxGroupInput(ns("vars_multi"), "Expositions à tester :", choices = vars_cat(), selected = head(vars_cat(), 5))
    )
    output$sel_outcome_multi <- renderUI(selectInput(ns("var_out_multi"), "Outcome :", choices = vars_cat(), width = "100%"))
    output$sel_var_ord    <- renderUI(selectInput(ns("var_ord"), "Variable ordinale :", choices = vars_all(), width = "100%"))
    output$sel_outcome_ca <- renderUI(selectInput(ns("var_out_ca"), "Outcome (binaire) :", choices = vars_cat(), width = "100%"))
    output$sel_expo_mh    <- renderUI(selectInput(ns("expo_mh"), "Exposition :", choices = vars_cat(), width = "100%"))
    output$sel_out_mh     <- renderUI(selectInput(ns("out_mh"), "Outcome :", choices = vars_cat(), width = "100%"))
    output$sel_strata_mh  <- renderUI(selectInput(ns("strata_mh"), "Variable de stratification :", choices = vars_cat(), width = "100%"))

    # ════════════════════════════════════════════════════════════════════════
    # ONGLET 1 : Analyse 2×2
    # ════════════════════════════════════════════════════════════════════════
    res_2x2 <- reactiveVal(NULL)

    observeEvent(input$run_2x2, {
      d <- df_f(); req(d, input$var_expo, input$var_out)
      req(input$var_expo %in% names(d), input$var_out %in% names(d))
      withProgress(message = "Calcul tableau 2×2...", {
        tryCatch({
          r <- analyse_2x2(d[[input$var_expo]], d[[input$var_out]], conf_level = input$conf_level)
          res_2x2(r)
          if (is.null(r)) showNotification("⚠️ Tableau 2×2 impossible (données insuffisantes)", type = "warning")
          else showNotification("✅ Analyse 2×2 terminée", type = "message")
        }, error = function(e) showNotification(paste("❌ Erreur :", e$message), type = "error"))
      })
    })

    output$res_2x2_html <- renderUI({
      r <- res_2x2()
      if (is.null(r)) return(tags$p("Cliquez sur 'Analyser' pour lancer.", style = "color:#999;"))
      render_2x2_html(r, input$var_expo, input$var_out)
    })

    output$res_2x2_table <- renderDT({
      r <- res_2x2(); req(!is.null(r))
      tibble(
        Mesure = c("Risque Relatif (RR)", "Odds Ratio (OR)", "PAF (%)",
                   "FAE (%)", "Risque Attribuable", "NNT",
                   "Chi² p-value", "Fisher p-value"),
        Valeur = c(round(r$rr, 3), round(r$or, 3),
                   round(r$paf * 100, 1), round(r$fae * 100, 1),
                   round(r$rar * 100, 1), r$nnt,
                   format.pval(r$chi2_p, digits = 4),
                   format.pval(r$fish_p, digits = 4)),
        `IC bas` = c(round(r$rr_lo, 3), round(r$or_lo, 3),
                     round(r$paf_lo * 100, 1), NA, NA, NA, NA, NA),
        `IC haut` = c(round(r$rr_hi, 3), round(r$or_hi, 3),
                      round(r$paf_hi * 100, 1), NA, NA, NA, NA, NA)
      ) |>
        datatable(rownames = FALSE, options = list(dom = "t", pageLength = 10),
                  class = "compact stripe hover") |>
        formatStyle("Mesure", fontWeight = "bold")
    })

    # Graphique mesures d'association
    output$plot_mesures_2x2 <- renderPlotly({
      r <- res_2x2(); req(!is.null(r))
      df_plot <- tibble(
        Mesure = c("RR", "OR"),
        Valeur = c(r$rr, r$or),
        IC_lo = c(r$rr_lo, r$or_lo),
        IC_hi = c(r$rr_hi, r$or_hi)
      )
      p <- ggplot(df_plot, aes(x = Valeur, y = Mesure,
                               xmin = IC_lo, xmax = IC_hi,
                               color = Valeur > 1,
                               text = paste0(Mesure, " = ", round(Valeur, 3),
                                             "\nIC [", round(IC_lo, 3), " – ", round(IC_hi, 3), "]"))) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
        geom_errorbarh(height = 0.3, linewidth = 1) +
        geom_point(size = 5) +
        scale_color_manual(values = c("TRUE" = "#c9161d", "FALSE" = "#4dac26"), guide = "none") +
        scale_x_log10() +
        labs(x = "Valeur (échelle log)", y = "", title = "Mesures d'association avec IC") +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text")
    })

    output$interpretation_2x2 <- renderUI({
      r <- res_2x2()
      if (is.null(r)) return(tags$p(style = "color:#999;font-size:12px;", "En attente..."))
      sig <- r$chi2_p < 0.05
      tags$div(style = "font-size:12px;line-height:1.6;",
        tags$div(class = if (sig) "alerte-box alerte-crit" else "alerte-box alerte-ok",
          if (sig) tags$b("🔴 Association significative") else tags$b("🟢 Pas d'association significative"),
          tags$br(),
          paste0("Chi² p = ", format.pval(r$chi2_p, digits = 3))
        ),
        tags$p(
          if (r$rr > 1) paste0("L'exposition multiplie le risque par ", round(r$rr, 2), " (facteur de risque).")
          else if (r$rr < 1) paste0("L'exposition réduit le risque de ", round((1 - r$rr) * 100, 0), "% (facteur protecteur).")
          else "Aucun effet détecté."
        ),
        if (!is.null(r$paf) && r$paf > 0) tags$p(
          paste0("Si cette exposition était éliminée, ", round(r$paf * 100, 1),
                 "% des cas pourraient être évités dans la population.")
        )
      )
    })

    # ════════════════════════════════════════════════════════════════════════
    # ONGLET 2 : Multi-exposition
    # ════════════════════════════════════════════════════════════════════════
    res_multi <- reactiveVal(NULL)

    observeEvent(input$run_multi, {
      d <- df_f(); req(d, input$vars_multi, input$var_out_multi)
      withProgress(message = "Analyse multi-exposition...", {
        tryCatch({
          r <- multi_exposition(d, input$vars_multi, input$var_out_multi, input$conf_multi)
          if (input$multi_sort_or && !is.null(r)) r <- r |> arrange(desc(OR))
          if (input$multi_show_sig && !is.null(r)) r <- r |> filter(Sig != "NS")
          res_multi(r)
          showNotification(paste0("✅ ", length(input$vars_multi), " expositions analysées"), type = "message")
        }, error = function(e) showNotification(paste("❌ Erreur :", e$message), type = "error"))
      })
    })

    output$forest_or <- renderPlotly({
      r <- res_multi(); req(!is.null(r), is.data.frame(r), nrow(r) > 0)
      r_plot <- r |> filter(!is.na(OR)) |> arrange(OR)
      p <- ggplot(r_plot, aes(
        x = OR, y = fct_inorder(Exposition),
        xmin = as.numeric(gsub("\\[(.+)–.+\\]", "\\1", `IC95 OR`)),
        xmax = as.numeric(gsub("\\[.+–(.+)\\]", "\\1", `IC95 OR`)),
        text = paste0(Exposition, "\nOR = ", round(OR, 2),
                      " ", `IC95 OR`, "\np = ", `p Chi²`, " ", Sig)
      )) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
        geom_errorbarh(height = 0.3, linewidth = 0.9, color = "#008DC9") +
        geom_point(aes(color = OR > 1), size = 3.5) +
        scale_color_manual(values = c("TRUE" = "#c9161d", "FALSE" = "#4dac26"), guide = "none") +
        labs(x = "Odds Ratio (échelle log)", y = "",
             title = "Forest plot — Odds Ratios multi-exposition") +
        scale_x_log10() +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text")
    })

    output$table_multi <- renderDT({
      r <- res_multi(); req(!is.null(r), is.data.frame(r))
      r |> mutate(across(where(is.numeric), ~round(., 3))) |>
        datatable(rownames = FALSE, options = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
                  class = "compact stripe hover") |>
        formatStyle("OR", color = styleInterval(1, c("#4dac26", "#c9161d")), fontWeight = "bold") |>
        formatStyle("Sig", fontWeight = "bold",
                    color = styleEqual(c("***", "**", "*", ".", "NS"),
                                       c("#c9161d", "#c9161d", "#e88400", "#888", "#aaa")))
    })

    output$multi_summary <- renderUI({
      r <- res_multi(); req(!is.null(r), nrow(r) > 0)
      n_sig <- sum(r$Sig != "NS", na.rm = TRUE)
      top_or <- r |> filter(!is.na(OR)) |> slice_max(OR, n = 1)
      tags$div(style = "background:#f0f8ff;border-left:4px solid #008DC9;padding:12px;border-radius:0 8px 8px 0;",
        tags$b(paste0(n_sig, "/", nrow(r), " expositions significatives")),
        tags$br(),
        if (nrow(top_or) > 0) tags$span(style = "font-size:12px;",
          paste0("Exposition la plus forte : ", top_or$Exposition[1],
                 " (OR = ", round(top_or$OR[1], 2), ")"))
      )
    })

    # ════════════════════════════════════════════════════════════════════════
    # ONGLET 3 : Cochran-Armitage
    # ════════════════════════════════════════════════════════════════════════
    ca_res <- reactiveVal(NULL)

    observeEvent(input$run_ca, {
      d <- df_f(); req(d, input$var_ord, input$var_out_ca)
      withProgress(message = "Test de tendance Cochran-Armitage...", {
        tryCatch({
          r <- cochran_armitage_test(d, input$var_ord, input$var_out_ca)
          ca_res(r)
          showNotification("✅ Test de tendance terminé", type = "message")
        }, error = function(e) showNotification(paste("❌ Erreur :", e$message), type = "error"))
      })
    })

    output$ca_result_html <- renderUI({
      r <- ca_res()
      if (is.null(r)) return(tags$p("Cliquez sur 'Test de tendance' pour lancer.", style = "color:#999;"))
      sig <- !is.na(r$p) && r$p < 0.05
      tags$div(
        tags$div(class = if (sig) "alerte-box alerte-crit" else "alerte-box alerte-ok",
          tags$b(paste0("Test de Cochran-Armitage : Z = ", r$Z, ", p = ", r$p)),
          tags$br(), tags$span(style = "font-size:12px;", r$interpretation)
        ),
        tags$div(style = "background:#f8f9fa;padding:10px;border-radius:6px;margin-top:10px;font-size:12px;",
          tags$b("Hypothèses :"), tags$br(),
          "H₀ : Pas de tendance linéaire entre la variable ordinale et l'outcome", tags$br(),
          "H₁ : Relation dose-réponse significative"
        )
      )
    })

    output$ca_trend_plot <- renderPlotly({
      d <- df_f(); req(d, input$var_ord, input$var_out_ca)
      tryCatch({
        tbl <- table(d[[input$var_ord]], d[[input$var_out_ca]])
        if (ncol(tbl) < 2) return(plotly_empty())
        df_trend <- tibble(
          Niveau = rownames(tbl),
          Proportion = tbl[, 2] / rowSums(tbl),
          N = rowSums(tbl)
        ) |> mutate(Niveau = fct_inorder(Niveau))

        p <- ggplot(df_trend, aes(x = Niveau, y = Proportion, group = 1,
                                  text = paste0(Niveau, "\nP = ", round(Proportion * 100, 1), "%\nN = ", N))) +
          geom_col(fill = "#008DC9", alpha = 0.4, width = 0.6) +
          geom_line(color = "#c9161d", linewidth = 1.2) +
          geom_point(color = "#c9161d", size = 3) +
          scale_y_continuous(labels = scales::percent) +
          labs(x = input$var_ord, y = paste0("Proportion ", input$var_out_ca, " = Oui"),
               title = "Tendance dose-réponse") +
          theme_minimal(base_size = 11)
        ggplotly(p, tooltip = "text")
      }, error = function(e) plotly_empty())
    })

    output$ca_table <- renderDT({
      d <- df_f(); req(d, input$var_ord, input$var_out_ca)
      tryCatch({
        tbl <- table(d[[input$var_ord]], d[[input$var_out_ca]])
        if (ncol(tbl) < 2) return(NULL)
        tibble(Niveau = rownames(tbl), Cas = tbl[, 2], Total = rowSums(tbl),
               `Proportion (%)` = round(tbl[, 2] / rowSums(tbl) * 100, 1)) |>
          datatable(rownames = FALSE, options = list(dom = "t"), class = "compact stripe hover")
      }, error = function(e) NULL)
    })

    # ════════════════════════════════════════════════════════════════════════
    # ONGLET 4 : Stratification Mantel-Haenszel
    # ════════════════════════════════════════════════════════════════════════
    mh_res <- reactiveVal(NULL)

    observeEvent(input$run_mh, {
      d <- df_f(); req(d, input$expo_mh, input$out_mh, input$strata_mh)
      withProgress(message = "Analyse Mantel-Haenszel...", {
        tryCatch({
          strata_vals <- unique(na.omit(d[[input$strata_mh]]))
          req(length(strata_vals) >= 2)

          strata_results <- purrr::map_dfr(strata_vals, function(sv) {
            d_s <- d |> filter(.data[[input$strata_mh]] == sv)
            tryCatch({
              r <- analyse_2x2(d_s[[input$expo_mh]], d_s[[input$out_mh]])
              if (is.null(r)) return(NULL)
              tibble(Strate = as.character(sv), N = r$n,
                     OR = round(r$or, 3), OR_lo = round(r$or_lo, 3), OR_hi = round(r$or_hi, 3),
                     RR = round(r$rr, 3), p_chi2 = round(r$chi2_p, 4))
            }, error = function(e) NULL)
          })

          # OR Mantel-Haenszel via epiR ou manuellem
          tbl_3d <- tryCatch({
            table(d[[input$expo_mh]], d[[input$out_mh]], d[[input$strata_mh]])
          }, error = function(e) NULL)

          mh_test <- if (!is.null(tbl_3d) && length(dim(tbl_3d)) == 3) {
            tryCatch(mantelhaen.test(tbl_3d), error = function(e) NULL)
          }

          mh_res(list(strata = strata_results, mh_test = mh_test,
                      expo = input$expo_mh, out = input$out_mh, strat = input$strata_mh))
          showNotification(paste0("✅ ", length(strata_vals), " strates analysées"), type = "message")
        }, error = function(e) showNotification(paste("❌ Erreur :", e$message), type = "error"))
      })
    })

    output$mh_result_html <- renderUI({
      r <- mh_res()
      if (is.null(r)) return(tags$p("Cliquez sur 'Analyse stratifiée' pour lancer.", style = "color:#999;"))
      mh <- r$mh_test
      tags$div(
        if (!is.null(mh)) tags$div(
          class = if (mh$p.value < 0.05) "alerte-box alerte-crit" else "alerte-box alerte-ok",
          tags$b(paste0("OR Mantel-Haenszel commun = ", round(as.numeric(mh$estimate), 3))),
          tags$br(),
          tags$span(style = "font-size:12px;",
            paste0("Test MH : Chi² = ", round(mh$statistic, 3),
                   ", p = ", format.pval(mh$p.value, digits = 4),
                   if (mh$p.value < 0.05) " → Association significative après ajustement"
                   else " → Pas d'association après ajustement"))
        ),
        tags$div(style = "margin-top:10px;font-size:12px;background:#f8f9fa;padding:10px;border-radius:6px;",
          tags$b("Interprétation : "),
          paste0("L'analyse stratifiée sur '", r$strat, "' permet de contrôler la confusion. ",
                 "Si l'OR MH diffère de l'OR brut, la variable '", r$strat, "' est un facteur de confusion."))
      )
    })

    output$mh_forest <- renderPlotly({
      r <- mh_res(); req(!is.null(r), nrow(r$strata) > 0)
      df_plot <- r$strata |> filter(!is.na(OR))
      mh_or <- if (!is.null(r$mh_test)) as.numeric(r$mh_test$estimate) else NA

      p <- ggplot(df_plot, aes(x = OR, y = fct_reorder(Strate, OR),
                               xmin = OR_lo, xmax = OR_hi,
                               text = paste0("Strate: ", Strate, "\nOR = ", OR,
                                             " [", OR_lo, "–", OR_hi, "]\np = ", p_chi2))) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
        { if (!is.na(mh_or)) geom_vline(xintercept = mh_or, linetype = "dotted", color = "#7b2d8b", linewidth = 1) } +
        geom_errorbarh(height = 0.3, linewidth = 0.9, color = "#008DC9") +
        geom_point(aes(color = OR > 1), size = 4) +
        scale_color_manual(values = c("TRUE" = "#c9161d", "FALSE" = "#4dac26"), guide = "none") +
        scale_x_log10() +
        labs(x = "OR (échelle log)", y = "Strate",
             title = "Forest plot par strate — Mantel-Haenszel",
             subtitle = if (!is.na(mh_or)) paste0("Ligne pointillée = OR MH commun (", round(mh_or, 3), ")") else "") +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text")
    })

    output$mh_strata_table <- renderDT({
      r <- mh_res(); req(!is.null(r))
      r$strata |>
        datatable(rownames = FALSE, options = list(dom = "t", pageLength = 20),
                  class = "compact stripe hover") |>
        formatStyle("OR", color = styleInterval(1, c("#4dac26", "#c9161d")), fontWeight = "bold")
    })

    # ════════════════════════════════════════════════════════════════════════
    # EXPERT TABS — Server logic
    # ════════════════════════════════════════════════════════════════════════
    source("server_epidemio_expert.R", local = TRUE)

    # ════════════════════════════════════════════════════════════════════════
    # EXPORTS
    # ════════════════════════════════════════════════════════════════════════
    output$dl_csv <- downloadHandler(
      filename = function() paste0("tableau_2x2_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- res_2x2(); req(!is.null(r))
        df <- tibble(Mesure = c("RR","OR","PAF","FAE","NNT","Chi2_p","Fisher_p"),
                     Valeur = c(r$rr, r$or, r$paf, r$fae, r$nnt, r$chi2_p, r$fish_p),
                     IC_lo = c(r$rr_lo, r$or_lo, r$paf_lo, NA, NA, NA, NA),
                     IC_hi = c(r$rr_hi, r$or_hi, r$paf_hi, NA, NA, NA, NA))
        write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_multi_csv <- downloadHandler(
      filename = function() paste0("multi_exposition_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- res_multi(); req(!is.null(r))
        write.csv(r, file, row.names = FALSE)
      }
    )

    output$dl_forest_png <- downloadHandler(
      filename = function() paste0("forest_plot_", Sys.Date(), ".png"),
      content = function(file) {
        r <- res_multi(); req(!is.null(r), nrow(r) > 0)
        r_plot <- r |> filter(!is.na(OR)) |> arrange(OR)
        p <- ggplot(r_plot, aes(x = OR, y = fct_inorder(Exposition))) +
          geom_vline(xintercept = 1, linetype = "dashed", color = "#888") +
          geom_point(aes(color = OR > 1), size = 3.5) +
          scale_color_manual(values = c("TRUE" = "#c9161d", "FALSE" = "#4dac26"), guide = "none") +
          scale_x_log10() +
          labs(x = "Odds Ratio", y = "", title = "Forest plot — Multi-exposition") +
          theme_minimal(base_size = 12)
        ggsave(file, plot = p, width = 10, height = max(4, nrow(r_plot) * 0.5), dpi = 200)
      }
    )

    output$dl_excel <- downloadHandler(
      filename = function() paste0("epidemio_complet_", Sys.Date(), ".xlsx"),
      content = function(file) {
        sheets <- list()
        r2x2 <- res_2x2()
        if (!is.null(r2x2)) {
          sheets[["Tableau 2x2"]] <- tibble(
            Mesure = c("RR","OR","PAF","FAE","NNT","Chi2_p","Fisher_p"),
            Valeur = c(r2x2$rr, r2x2$or, r2x2$paf, r2x2$fae, r2x2$nnt, r2x2$chi2_p, r2x2$fish_p))
        }
        rm <- res_multi()
        if (!is.null(rm)) sheets[["Multi-exposition"]] <- rm
        r_ca <- ca_res()
        if (!is.null(r_ca)) sheets[["Cochran-Armitage"]] <- tibble(Z = r_ca$Z, p = r_ca$p, Interp = r_ca$interpretation)
        r_mh <- mh_res()
        if (!is.null(r_mh) && !is.null(r_mh$strata)) sheets[["Mantel-Haenszel"]] <- r_mh$strata
        if (length(sheets) == 0) sheets[["Info"]] <- tibble(Message = "Lancez au moins une analyse")
        writexl::write_xlsx(sheets, path = file)
      }
    )
  })
}
