# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_rapport.R — Module Shiny Expert : Rapport exportable                  ║
# ║  Word OMS · PDF · Excel multi-feuilles · Aperçu dynamique riche           ║
# ║  Sections configurables · Graphiques intégrés · Métadonnées               ║
# ║  Niveau: Doctoral Research Grade                                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

source("module_rapport.R")

# ── UI ────────────────────────────────────────────────────────────────────────
rapportUI <- function(id, lang = "fr") {
  ns <- NS(id)

  tagList(
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #008DC9; border-radius: 6px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#008DC9;",
              icon("file-alt"), tr("rapp_title", lang)),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             tr("rapp_desc", lang),
             )
    ),

    fluidRow(
      # ── Panneau de configuration ──────────────────────────────────────────
      box(width = 4, status = "primary", solidHeader = TRUE,
          title = tags$span(icon("cog"), " ", tr("rapp_config_title", lang)),

          tags$div(class = "section-header", tr("rapp_general_info", lang)),
          textInput(ns("rpt_title"), tr("rapp_report_title", lang), tr("rapp_default_title", lang), width = "100%"),
          fluidRow(
            column(6, textInput(ns("rpt_auteur"), tr("rapp_author", lang), "", width = "100%")),
            column(6, textInput(ns("rpt_org"), tr("rapp_org", lang), "DSAMID", width = "100%"))
          ),
          fluidRow(
            column(6, textInput(ns("rpt_zone"), tr("rapp_zone", lang), "", width = "100%")),
            column(6, textInput(ns("rpt_date"), tr("rapp_period", lang), format(Sys.Date(), "%B %Y"), width = "100%"))
          ),

          hr(),
          tags$div(class = "section-header", tr("rapp_sections_hdr", lang)),
          checkboxGroupInput(ns("rpt_sections"), NULL,
            choices = setNames(
              c("resume","descriptif","qualite","prevalence","secu_alim","indicateurs","zscore","inferentiel","epidemio","survie","predictif","carto","conclusion"),
              c(tr("rapp_sec_resume",lang),tr("rapp_sec_desc",lang),tr("rapp_sec_qualite",lang),
                tr("rapp_sec_prev",lang),tr("rapp_sec_secu",lang),tr("rapp_sec_indic",lang),
                tr("rapp_sec_zscore",lang),tr("rapp_sec_infer",lang),tr("rapp_sec_epid",lang),
                tr("rapp_sec_surv",lang),tr("rapp_sec_pred",lang),tr("rapp_sec_carto",lang),
                tr("rapp_sec_concl",lang))),
            selected = c("resume", "descriptif", "indicateurs", "inferentiel", "conclusion")
          ),

          hr(),
          tags$div(class = "section-header", tr("rapp_options_hdr", lang)),
          checkboxInput(ns("rpt_include_plots"), tr("rapp_incl_plots", lang), TRUE),
          checkboxInput(ns("rpt_include_interp"), tr("rapp_incl_interp", lang), TRUE),
          checkboxInput(ns("rpt_include_refs"), tr("rapp_incl_refs", lang), TRUE),

          hr(),
          tags$div(class = "section-header", tr("rapp_formats_hdr", lang)),
          fluidRow(
            column(6,
              downloadButton(ns("dl_word"), tr("rapp_dl_word", lang),
                style = "background:#008DC9;color:white;border:none;border-radius:6px;padding:10px;width:100%;font-weight:600;font-size:13px;")),
            column(6,
              downloadButton(ns("dl_pdf"), tr("rapp_dl_pdf", lang),
                style = "background:#c9161d;color:white;border:none;border-radius:6px;padding:10px;width:100%;font-weight:600;font-size:13px;"))
          ),
          br(),
          fluidRow(
            column(6,
              downloadButton(ns("dl_excel_full"), tr("rapp_dl_excel", lang),
                style = "background:#4dac26;color:white;border:none;border-radius:6px;padding:10px;width:100%;font-weight:600;font-size:13px;")),
            column(6,
              downloadButton(ns("dl_html"), tr("rapp_dl_html", lang),
                style = "background:#e88400;color:white;border:none;border-radius:6px;padding:10px;width:100%;font-weight:600;font-size:13px;"))
          )
      ),

      # ── Aperçu dynamique ──────────────────────────────────────────────────
      box(width = 8, status = "info", solidHeader = TRUE,
          title = tags$span(icon("eye"), " ", tr("rapp_preview_title", lang)),

          tabsetPanel(
            id = ns("preview_tabs"),

            tabPanel(tags$span(icon("file-alt"), " ", tr("rapp_tab_preview", lang)),
              br(),
              uiOutput(ns("rapport_preview"))
            ),

            tabPanel(tags$span(icon("chart-bar"), " ", tr("rapp_tab_stats", lang)),
              br(),
              uiOutput(ns("stats_key_cards")),
              br(),
              plotlyOutput(ns("preview_plot_prev"), height = "350px")
            ),

            tabPanel(tags$span(icon("list-check"), " ", tr("rapp_tab_checklist", lang)),
              br(),
              uiOutput(ns("checklist_ui"))
            )
          )
      )
    )
  )
}

# ── SERVER ────────────────────────────────────────────────────────────────────
rapportServer <- function(id, df_f, map_cols, mapped_data, lang = reactive("fr"),
                           vars_num_fn = NULL, has_oms_fn = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ════════════════════════════════════════════════════════════════════════
    # APERÇU DYNAMIQUE RICHE
    # ════════════════════════════════════════════════════════════════════════
    output$rapport_preview <- renderUI({
      df <- mapped_data()
      if (is.null(df)) {
        return(tags$div(style = "color:#888;padding:40px;text-align:center;",
          icon("cloud-upload-alt", style = "font-size:48px;color:#ccc;"), tags$br(), tags$br(),
          "Importez des données pour générer un rapport."))
      }
      d <- df_f()
      n_sections <- length(input$rpt_sections)

      # Header
      header <- tags$div(
        style = "background:linear-gradient(135deg,#00426A,#008DC9);color:white;padding:20px;border-radius:10px;margin-bottom:16px;",
        tags$h3(style = "margin:0;font-weight:700;", input$rpt_title),
        tags$p(style = "margin:6px 0 0;opacity:.85;font-size:13px;",
          if (nchar(input$rpt_auteur) > 0) paste0("Par ", input$rpt_auteur) else "",
          if (nchar(input$rpt_org) > 0) paste0(" — ", input$rpt_org) else "",
          if (nchar(input$rpt_zone) > 0) paste0(" | Zone : ", input$rpt_zone) else "",
          paste0(" | Période : ", input$rpt_date)
        ),
        tags$p(style = "margin:4px 0 0;font-size:11px;opacity:.65;",
          paste0(nrow(d), " observations · ", ncol(d), " variables · ",
                 n_sections, " sections sélectionnées · ",
                 format(Sys.Date(), "%d %B %Y")))
      )

      # Résumé exécutif
      resume_ui <- NULL
      if ("resume" %in% input$rpt_sections && "GAM" %in% names(d)) {
        gam_p <- round(mean(d$GAM, na.rm = TRUE) * 100, 1)
        sam_p <- if ("SAM" %in% names(d)) round(mean(d$SAM, na.rm = TRUE) * 100, 1) else NA
        sev <- if (gam_p >= 15) list(lbl = "CRITIQUE", col = "#c9161d", bg = "#ffebee")
               else if (gam_p >= 10) list(lbl = "SÉRIEUX", col = "#e88400", bg = "#fff3e0")
               else if (gam_p >= 5) list(lbl = "ALERTE", col = "#f9c734", bg = "#fffde7")
               else list(lbl = "ACCEPTABLE", col = "#4dac26", bg = "#e8f5e9")

        resume_ui <- tags$div(
          style = paste0("background:", sev$bg, ";border-left:5px solid ", sev$col,
                         ";padding:16px;border-radius:0 10px 10px 0;margin-bottom:14px;"),
          tags$h5(style = "margin:0 0 8px;font-weight:700;color:#00426A;", "Résumé exécutif"),
          fluidRow(
            column(4, tags$div(class = "stat-card", style = "text-align:center;",
              tags$p(class = "sc-lbl", "GAM (MAG)"),
              tags$p(class = "sc-val", style = paste0("color:", sev$col, ";"), paste0(gam_p, "%")),
              tags$span(class = "zs-severity-badge", style = paste0("background:", sev$col, ";color:white;"), sev$lbl)
            )),
            column(4, if (!is.na(sam_p)) tags$div(class = "stat-card", style = "text-align:center;",
              tags$p(class = "sc-lbl", "SAM (MAS)"),
              tags$p(class = "sc-val", style = paste0("color:", if (sam_p >= 2) "#c9161d" else "#4dac26", ";"),
                     paste0(sam_p, "%"))
            )),
            column(4, tags$div(class = "stat-card", style = "text-align:center;",
              tags$p(class = "sc-lbl", "Échantillon"),
              tags$p(class = "sc-val", format(nrow(d), big.mark = " "))
            ))
          ),
          if (input$rpt_include_interp) tags$p(style = "font-size:12px;margin-top:8px;color:#555;",
            interpreter_prevalence("GAM", gam_p))
        )
      }

      # Sections incluses
      sections_ui <- tags$div(
        style = "background:#f8f9fa;padding:14px;border-radius:8px;margin-bottom:14px;",
        tags$h6(style = "font-weight:700;color:#00426A;margin:0 0 8px;", icon("list-check"), " Sections incluses"),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:6px;",
          lapply(input$rpt_sections, function(s) {
            lbl <- c(resume = "Résumé", descriptif = "Descriptif", qualite = "Qualité SMART",
                     prevalence = "Prévalences", secu_alim = "Sécu. Alim.", indicateurs = "OMS/PAM",
                     zscore = "Z-scores", inferentiel = "Inférentiel", epidemio = "Épidémiologie",
                     survie = "Survie", predictif = "Prédictif", carto = "Cartographie",
                     conclusion = "Conclusion")
            tags$span(class = "badge-info", style = "font-size:11px;", lbl[s] %||% s)
          })
        )
      )

      # Descriptif
      desc_ui <- NULL
      if ("descriptif" %in% input$rpt_sections) {
        vars_num <- names(d)[sapply(d, is.numeric) & !startsWith(names(d), ".")]
        vars_cat_names <- names(d)[sapply(d, function(x) is.factor(x) || is.character(x))]
        desc_ui <- tags$div(
          style = "background:white;border:1px solid #e0e8f0;border-radius:8px;padding:14px;margin-bottom:14px;",
          tags$h6(style = "font-weight:700;color:#00426A;margin:0 0 8px;", "Statistiques descriptives"),
          tags$p(style = "font-size:12px;color:#666;",
            paste0(length(vars_num), " variables numériques · ",
                   length(vars_cat_names), " variables catégorielles · ",
                   sum(is.na(d)), " valeurs manquantes au total"))
        )
      }

      tagList(header, resume_ui, sections_ui, desc_ui,
        tags$div(style = "background:#fff3e0;border-left:3px solid #e88400;padding:10px;border-radius:0 6px 6px 0;",
          tags$span(style = "font-size:12px;color:#e88400;",
            icon("info-circle"), " Le rapport complet sera généré au format sélectionné avec tableaux, graphiques et interprétations automatiques.")))
    })

    # ── Statistiques clés ───────────────────────────────────────────────────
    output$stats_key_cards <- renderUI({
      d <- df_f(); req(!is.null(d))
      vars_num <- names(d)[sapply(d, is.numeric) & !startsWith(names(d), ".")]
      n_na <- sum(is.na(d))
      pct_na <- round(n_na / (nrow(d) * ncol(d)) * 100, 1)

      fluidRow(
        column(3, tags$div(class = "stat-card", style = "text-align:center;",
          tags$p(class = "sc-lbl", "Observations"), tags$p(class = "sc-val", format(nrow(d), big.mark = " ")))),
        column(3, tags$div(class = "stat-card", style = "text-align:center;",
          tags$p(class = "sc-lbl", "Variables"), tags$p(class = "sc-val", ncol(d)))),
        column(3, tags$div(class = "stat-card", style = "text-align:center;",
          tags$p(class = "sc-lbl", "% Manquant"),
          tags$p(class = "sc-val", style = paste0("color:", if (pct_na > 10) "#c9161d" else "#4dac26", ";"),
                 paste0(pct_na, "%")))),
        column(3, tags$div(class = "stat-card", style = "text-align:center;",
          tags$p(class = "sc-lbl", "Sections"), tags$p(class = "sc-val", length(input$rpt_sections))))
      )
    })

    output$preview_plot_prev <- renderPlotly({
      d <- df_f(); req(!is.null(d), "GAM" %in% names(d))
      tryCatch({
        inds <- intersect(c("GAM", "SAM", "MAM", "Stunting", "Underweight"), names(d))
        req(length(inds) > 0)
        prev_df <- purrr::map_dfr(inds, function(v) {
          ic <- binom::binom.confint(sum(d[[v]], na.rm = TRUE), sum(!is.na(d[[v]])), methods = "wilson")
          tibble(Indicateur = v, Prevalence = round(ic$mean * 100, 1),
                 IC_lo = round(ic$lower * 100, 1), IC_hi = round(ic$upper * 100, 1))
        })
        p <- ggplot(prev_df, aes(x = fct_reorder(Indicateur, Prevalence), y = Prevalence,
                                  fill = Prevalence,
                                  text = paste0(Indicateur, ": ", Prevalence, "% [", IC_lo, "–", IC_hi, "]"))) +
          geom_col(alpha = 0.88) +
          geom_errorbar(aes(ymin = IC_lo, ymax = IC_hi), width = 0.2, color = "gray40") +
          geom_hline(yintercept = 15, linetype = "dashed", color = "#c9161d") +
          scale_fill_gradientn(colors = c("#4dac26", "#f9c734", "#e88400", "#c9161d"), guide = "none") +
          labs(x = "", y = "Prévalence (%)", title = "Indicateurs nutritionnels") +
          theme_minimal(base_size = 11) +
          coord_flip()
        ggplotly(p, tooltip = "text")
      }, error = function(e) plotly_empty())
    })

    # ── Checklist ───────────────────────────────────────────────────────────
    output$checklist_ui <- renderUI({
      d <- df_f()
      items <- list(
        list("Données importées", !is.null(d)),
        list("Titre renseigné", nchar(input$rpt_title) > 5),
        list("Auteur renseigné", nchar(input$rpt_auteur) > 0),
        list("Zone géographique", nchar(input$rpt_zone) > 0),
        list("Au moins 3 sections", length(input$rpt_sections) >= 3),
        list("Variables GAM/SAM détectées", !is.null(d) && "GAM" %in% names(d)),
        list("Plus de 100 observations", !is.null(d) && nrow(d) >= 100),
        list("Moins de 20% de NA", !is.null(d) && sum(is.na(d)) / (nrow(d) * ncol(d)) < 0.2)
      )
      tags$div(style = "padding:10px;",
        tags$h6(style = "font-weight:700;color:#00426A;margin:0 0 12px;", "Checklist avant export"),
        lapply(items, function(item) {
          ok <- item[[2]]
          tags$div(
            style = paste0("display:flex;align-items:center;gap:10px;padding:8px 12px;margin-bottom:4px;",
                           "border-radius:6px;background:", if (ok) "#e8f5e9" else "#fff3e0", ";"),
            tags$span(style = paste0("font-size:16px;color:", if (ok) "#4dac26" else "#e88400", ";"),
                      if (ok) "✓" else "○"),
            tags$span(style = paste0("font-size:13px;color:", if (ok) "#2e7d32" else "#e65100", ";"), item[[1]])
          )
        }),
        tags$div(style = "margin-top:12px;font-size:12px;color:#888;",
          paste0(sum(sapply(items, function(x) x[[2]])), "/", length(items), " éléments validés"))
      )
    })

    # ════════════════════════════════════════════════════════════════════════
    # EXPORTS
    # ════════════════════════════════════════════════════════════════════════

    # ── Word ────────────────────────────────────────────────────────────────
    output$dl_word <- downloadHandler(
      filename = function() paste0(gsub(" ", "_", input$rpt_title %||% "rapport"), "_", Sys.Date(), ".docx"),
      content = function(f) {
        d <- df_f()
        withProgress(message = "Génération Word OMS...", {
          tryCatch({
            doc <- generate_word_report(
              d = d, map_cols_res = map_cols(),
              titre = input$rpt_title, auteur = input$rpt_auteur,
              organisation = input$rpt_org, zone = input$rpt_zone,
              periode = input$rpt_date, sections = input$rpt_sections)
            print(doc, target = f)
            showNotification("✅ Rapport Word généré", type = "message")
          }, error = function(e) {
            tryCatch({
              doc <- officer::read_docx() |>
                officer::body_add_par(input$rpt_title, style = "heading 1") |>
                officer::body_add_par(paste0("Date : ", Sys.Date())) |>
                officer::body_add_par(paste("Observations :", nrow(d))) |>
                officer::body_add_par(paste("Erreur rencontrée :", e$message))
              print(doc, target = f)
            }, error = function(e2) writeLines(paste("Erreur:", e2$message), f))
          })
        })
      }
    )

    # ── PDF ─────────────────────────────────────────────────────────────────
    output$dl_pdf <- downloadHandler(
      filename = function() paste0(gsub(" ", "_", input$rpt_title %||% "rapport"), "_", Sys.Date(), ".pdf"),
      content = function(f) {
        d <- df_f()
        tryCatch({
          rmd <- generate_report_rmd(d, map_cols(), NULL, "html_document")
          tmp <- tempfile(fileext = ".Rmd"); writeLines(rmd, tmp)
          html <- tempfile(fileext = ".html")
          env <- new.env(parent = globalenv()); env$df <- d; env$m <- map_cols()
          rmarkdown::render(tmp, output_file = html, envir = env, quiet = TRUE)
          pagedown::chrome_print(html, output = f, timeout = 120)
        }, error = function(e) writeLines(paste("Erreur PDF:", e$message), f))
      }
    )

    # ── HTML ────────────────────────────────────────────────────────────────
    output$dl_html <- downloadHandler(
      filename = function() paste0(gsub(" ", "_", input$rpt_title %||% "rapport"), "_", Sys.Date(), ".html"),
      content = function(f) {
        d <- df_f()
        tryCatch({
          rmd <- generate_report_rmd(d, map_cols(), NULL, "html_document")
          tmp <- tempfile(fileext = ".Rmd"); writeLines(rmd, tmp)
          env <- new.env(parent = globalenv()); env$df <- d; env$m <- map_cols()
          rmarkdown::render(tmp, output_file = f, envir = env, quiet = TRUE)
        }, error = function(e) writeLines(paste("Erreur HTML:", e$message), f))
      }
    )

    # ── Excel complet ───────────────────────────────────────────────────────
    output$dl_excel_full <- downloadHandler(
      filename = function() paste0("data_complete_", Sys.Date(), ".xlsx"),
      content = function(f) {
        d <- df_f()
        sheets <- list()

        # Métadonnées
        sheets[["Métadonnées"]] <- tibble(
          Champ = c("Titre", "Auteur", "Organisation", "Zone", "Période", "Date rapport",
                    "N observations", "N variables", "Sections"),
          Valeur = c(input$rpt_title, input$rpt_auteur, input$rpt_org, input$rpt_zone,
                     input$rpt_date, as.character(Sys.Date()),
                     as.character(nrow(d)), as.character(ncol(d)),
                     paste(input$rpt_sections, collapse = ", ")))

        # Données
        sheets[["Données"]] <- d |> select(!starts_with("."))

        # Statistiques descriptives
        vn <- if (!is.null(vars_num_fn)) vars_num_fn() else names(d)[sapply(d, is.numeric)]
        if (length(vn) > 0) {
          sheets[["Statistiques"]] <- tryCatch(
            purrr::map_dfr(head(vn, 25), function(v) {
              tibble(Variable = v, N = sum(!is.na(d[[v]])),
                     Moyenne = round(mean(d[[v]], na.rm = TRUE), 2),
                     Médiane = round(median(d[[v]], na.rm = TRUE), 2),
                     SD = round(sd(d[[v]], na.rm = TRUE), 2),
                     Min = round(min(d[[v]], na.rm = TRUE), 2),
                     Max = round(max(d[[v]], na.rm = TRUE), 2),
                     `% NA` = round(mean(is.na(d[[v]])) * 100, 1))
            }), error = function(e) tibble(Message = e$message))
        }

        # Indicateurs OMS
        oms_check <- if (!is.null(has_oms_fn)) has_oms_fn() else ("GAM" %in% names(d))
        if (oms_check) {
          inds <- intersect(c("GAM", "SAM", "MAM", "Stunting", "Sev_Stunting",
                               "Underweight", "MUAC_GAM", "MUAC_SAM"), names(d))
          if (length(inds) > 0) {
            sheets[["Indicateurs OMS"]] <- tryCatch(
              purrr::map_dfr(inds, function(v) {
                ic <- binom::binom.confint(sum(d[[v]], na.rm = TRUE),
                                            sum(!is.na(d[[v]])), methods = "wilson")
                tibble(Indicateur = v,
                       Prevalence_pct = round(ic$mean * 100, 1),
                       IC95_lo = round(ic$lower * 100, 1),
                       IC95_hi = round(ic$upper * 100, 1),
                       N = sum(!is.na(d[[v]])),
                       Cas = sum(d[[v]], na.rm = TRUE))
              }), error = function(e) tibble(Message = e$message))
          }
        }

        # Valeurs manquantes
        na_summary <- tibble(
          Variable = names(d),
          N_manquant = sapply(d, function(x) sum(is.na(x))),
          Pct_manquant = round(sapply(d, function(x) mean(is.na(x))) * 100, 1)
        ) |> filter(N_manquant > 0) |> arrange(desc(Pct_manquant))
        if (nrow(na_summary) > 0) sheets[["Valeurs manquantes"]] <- na_summary

        writexl::write_xlsx(sheets, path = f)
        showNotification("✅ Export Excel complet généré", type = "message")
      }
    )
  })
}
