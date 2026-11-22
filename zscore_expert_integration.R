# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  zscore_expert_integration.R — Intégration complète du moteur expert dans Shiny
# ║  À sourcer dans le server() de app_v2_final.R                              ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# REACTIVE VALUES
# ══════════════════════════════════════════════════════════════════════════════

zs_expert_quality <- reactiveVal(NULL)
zs_expert_narratives <- reactiveVal(NULL)
zs_expert_report <- reactiveVal(NULL)

# ══════════════════════════════════════════════════════════════════════════════
# AFTER Z-SCORE CALCULATION
# ══════════════════════════════════════════════════════════════════════════════

# Observer qui déclenche l'analyse expert après calcul des Z-scores
# Protégé par tryCatch pour éviter de crasher l'app si les données sont invalides
observe({
  res <- zs_data()
  req(res)

  tryCatch({
    # Vérifier qu'il y a au moins des Z-scores valides
    whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
    haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
    waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]

    if (length(whz) < 5 && length(haz) < 5 && length(waz) < 5) {
      showNotification("Trop peu de Z-scores valides pour l'analyse expert.", type = "warning")
      return()
    }

    # Calculer la qualité avec tous les critères
    quality <- calc_quality_score_expert(res,
      col_whz = "WHZ_calc",
      col_haz = "HAZ_calc",
      col_waz = "WAZ_calc",
      col_age = isolate(input$zs_col_age),
      col_sexe = isolate(input$zs_col_sexe),
      col_team = NULL
    )

    zs_expert_quality(quality)

    # ─────────────────────────────────────────────────────────────────
    # Générer les narratifs (avec protection NA)
    # ─────────────────────────────────────────────────────────────────

    safe_pct <- function(x, threshold) {
      if (length(x) == 0) return(0)
      round(100 * mean(x < threshold, na.rm = TRUE), 1)
    }

    pct_gam <- safe_pct(whz, -2)
    pct_sam <- safe_pct(whz, -3)
    pct_haz <- safe_pct(haz, -2)
    pct_haz_sev <- safe_pct(haz, -3)
    pct_waz <- safe_pct(waz, -2)
    pct_waz_sev <- safe_pct(waz, -3)

    narr_gam <- gen_narrative_kpi("GAM",
      pct_prevalence = pct_gam,
      pct_severe = pct_sam,
      pct_moderate = pct_gam - pct_sam,
      mean_z = mean(whz, na.rm = TRUE),
      sd_z = if (length(whz) > 1) sd(whz, na.rm = TRUE) else NA_real_,
      n_flags = sum(res$flag_WHZ, na.rm = TRUE),
      n_total = nrow(res),
      quality_score = quality$score_global
    )

    narr_haz <- gen_narrative_kpi("HAZ",
      pct_prevalence = pct_haz,
      pct_severe = pct_haz_sev,
      pct_moderate = pct_haz - pct_haz_sev,
      mean_z = mean(haz, na.rm = TRUE),
      sd_z = if (length(haz) > 1) sd(haz, na.rm = TRUE) else NA_real_,
      n_flags = sum(res$flag_HAZ, na.rm = TRUE),
      n_total = nrow(res),
      quality_score = quality$score_global
    )

    narr_waz <- gen_narrative_kpi("WAZ",
      pct_prevalence = pct_waz,
      pct_severe = pct_waz_sev,
      pct_moderate = pct_waz - pct_waz_sev,
      mean_z = mean(waz, na.rm = TRUE),
      sd_z = if (length(waz) > 1) sd(waz, na.rm = TRUE) else NA_real_,
      n_flags = sum(res$flag_WAZ, na.rm = TRUE),
      n_total = nrow(res),
      quality_score = quality$score_global
    )

    zs_expert_narratives(list(
      GAM = narr_gam,
      HAZ = narr_haz,
      WAZ = narr_waz,
      pct_gam = pct_gam,
      pct_haz = pct_haz,
      pct_waz = pct_waz,
      pct_sam = pct_sam,
      pct_haz_sev = pct_haz_sev,
      pct_waz_sev = pct_waz_sev
    ))

    # Générer le rapport complet
    rapport <- gen_rapport_expert(res, NULL, quality,
      col_sexe = isolate(input$zs_col_sexe),
      col_age = isolate(input$zs_col_age)
    )

    zs_expert_report(rapport)

  }, error = function(e) {
    showNotification(
      paste("Erreur analyse expert Z-scores :", e$message),
      type = "error", duration = 10
    )
    message("[zscore_expert] Erreur dans observe : ", e$message)
  })
})

# ══════════════════════════════════════════════════════════════════════════════
# OUTPUT: KPI CARDS EXPERT AVEC NARRATIF
# ══════════════════════════════════════════════════════════════════════════════

output$zs_expert_cards <- renderUI({
  res <- zs_data()
  quality <- zs_expert_quality()
  narr <- zs_expert_narratives()
  req(res, quality, narr)

  whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
  haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
  waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]

  tags$div(class = "zs-kpi-grid",
    # ─ CARD 1: GAM ─
    tags$div(class = "zs-kpi-card",
      style = "border: 2px solid #d32f2f; position: relative; overflow: hidden;",
      tags$div(style = "position: absolute; top: -8px; right: -8px; font-size: 72px; opacity: 0.06; color: #d32f2f;",
        icon("exclamation-triangle")),
      tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px;",
        tags$div(style = "width:36px; height:36px; border-radius:10px; background:#d32f2f; display:flex; align-items:center; justify-content:center; color:white;",
          icon("exclamation-triangle"))
        ,
        tags$div(
          tags$div(class = "zs-title", "Malnutrition Aiguë (WHZ < -2)"),
          tags$div(class = paste("zs-severity-badge", if (narr$pct_gam >= 15) "zs-severity-critique" else "zs-severity-acceptable"),
            if (narr$pct_gam >= 15) "CRITIQUE ≥15%" else "ACCEPTABLE <5%")
        )
      ),
      tags$div(style = "margin: 8px 0;",
        tags$span(class = "zs-value", style = "color: #d32f2f;", paste0(narr$pct_gam, "%"))
      ),
      # Sparkline inline
      plotlyOutput("zs_spark_gam", height = "35px", width = "100%"),
      # Décomposition
      tags$div(class = "zs-decomp",
        tags$div(class = "zs-decomp-row",
          tags$span(class = "zs-decomp-label", paste0("SMA (< -3): ", narr$pct_sam, "%")),
          tags$div(class = "zs-mini-bar-track",
            tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min(narr$pct_sam * 4.5, 100), "%; background: #d32f2f;"))
          )
        ),
        tags$div(class = "zs-decomp-row",
          tags$span(class = "zs-decomp-label", paste0("MAM (-3 à -2): ", round(narr$pct_gam - narr$pct_sam, 1), "%")),
          tags$div(class = "zs-mini-bar-track",
            tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min((narr$pct_gam - narr$pct_sam) * 4.5, 100), "%; background: #f57c00;"))
          )
        )
      ),
      # Narratif
      tags$div(style = "margin-top: 12px; padding-top: 10px; border-top: 1px dashed #e8edf2; font-size: 12px; line-height: 1.5; color: #5a6a7a;",
        HTML(narr$GAM)
      )
    ),

    # ─ CARD 2: HAZ ─
    tags$div(class = "zs-kpi-card",
      style = "border: 2px solid #388e3c; position: relative; overflow: hidden;",
      tags$div(style = "position: absolute; top: -8px; right: -8px; font-size: 72px; opacity: 0.06; color: #388e3c;",
        icon("arrow-up")),
      tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px;",
        tags$div(style = "width:36px; height:36px; border-radius:10px; background:#388e3c; display:flex; align-items:center; justify-content:center; color:white;",
          icon("arrow-up")),
        tags$div(
          tags$div(class = "zs-title", "Retard de Croissance (HAZ < -2)"),
          tags$div(class = paste("zs-severity-badge", if (narr$pct_haz >= 40) "zs-severity-critique" else "zs-severity-acceptable"),
            if (narr$pct_haz >= 40) "CRITIQUE ≥40%" else "ACCEPTABLE <20%")
        )
      ),
      tags$div(style = "margin: 8px 0;",
        tags$span(class = "zs-value", style = "color: #388e3c;", paste0(narr$pct_haz, "%"))
      ),
      plotlyOutput("zs_spark_haz", height = "35px", width = "100%"),
      tags$div(class = "zs-decomp",
        tags$div(class = "zs-decomp-row",
          tags$span(class = "zs-decomp-label", paste0("Sévère (< -3): ", narr$pct_haz_sev, "%")),
          tags$div(class = "zs-mini-bar-track",
            tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min(narr$pct_haz_sev * 2, 100), "%; background: #d32f2f;"))
          )
        ),
        tags$div(class = "zs-decomp-row",
          tags$span(class = "zs-decomp-label", paste0("Modéré (-3 à -2): ", round(narr$pct_haz - narr$pct_haz_sev, 1), "%")),
          tags$div(class = "zs-mini-bar-track",
            tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min((narr$pct_haz - narr$pct_haz_sev) * 2, 100), "%; background: #f57c00;"))
          )
        )
      ),
      tags$div(style = "margin-top: 12px; padding-top: 10px; border-top: 1px dashed #e8edf2; font-size: 12px; line-height: 1.5; color: #5a6a7a;",
        HTML(narr$HAZ)
      )
    ),

    # ─ CARD 3: WAZ ─
    tags$div(class = "zs-kpi-card",
      style = "border: 2px solid #7b1fa2; position: relative; overflow: hidden;",
      tags$div(style = "position: absolute; top: -8px; right: -8px; font-size: 72px; opacity: 0.06; color: #7b1fa2;",
        icon("weight")),
      tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px;",
        tags$div(style = "width:36px; height:36px; border-radius:10px; background:#7b1fa2; display:flex; align-items:center; justify-content:center; color:white;",
          icon("weight")),
        tags$div(
          tags$div(class = "zs-title", "Insuffisance Pondérale (WAZ < -2)"),
          tags$div(class = paste("zs-severity-badge", if (narr$pct_waz >= 30) "zs-severity-critique" else "zs-severity-acceptable"),
            if (narr$pct_waz >= 30) "CRITIQUE ≥30%" else "ACCEPTABLE <10%")
        )
      ),
      tags$div(style = "margin: 8px 0;",
        tags$span(class = "zs-value", style = "color: #7b1fa2;", paste0(narr$pct_waz, "%"))
      ),
      plotlyOutput("zs_spark_waz", height = "35px", width = "100%"),
      tags$div(class = "zs-decomp",
        tags$div(class = "zs-decomp-row",
          tags$span(class = "zs-decomp-label", paste0("Sévère (< -3): ", narr$pct_waz_sev, "%")),
          tags$div(class = "zs-mini-bar-track",
            tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min(narr$pct_waz_sev * 2.2, 100), "%; background: #d32f2f;"))
          )
        ),
        tags$div(class = "zs-decomp-row",
          tags$span(class = "zs-decomp-label", paste0("Modéré (-3 à -2): ", round(narr$pct_waz - narr$pct_waz_sev, 1), "%")),
          tags$div(class = "zs-mini-bar-track",
            tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min((narr$pct_waz - narr$pct_waz_sev) * 2.2, 100), "%; background: #f57c00;"))
          )
        )
      ),
      tags$div(style = "margin-top: 12px; padding-top: 10px; border-top: 1px dashed #e8edf2; font-size: 12px; line-height: 1.5; color: #5a6a7a;",
        HTML(narr$WAZ)
      )
    )
  )
})

# Sparklines outputs
output$zs_spark_gam <- renderPlotly({
  res <- zs_data(); req(res)
  p <- gen_sparkline_z(res$WHZ_calc, "#d32f2f")
  if (is.null(p)) return(plotly_empty())
  ggplotly(p, tooltip = FALSE) |> layout(margin = list(t = 0, b = 0, l = 0, r = 0))
})

output$zs_spark_haz <- renderPlotly({
  res <- zs_data(); req(res)
  p <- gen_sparkline_z(res$HAZ_calc, "#388e3c")
  if (is.null(p)) return(plotly_empty())
  ggplotly(p, tooltip = FALSE) |> layout(margin = list(t = 0, b = 0, l = 0, r = 0))
})

output$zs_spark_waz <- renderPlotly({
  res <- zs_data(); req(res)
  p <- gen_sparkline_z(res$WAZ_calc, "#7b1fa2")
  if (is.null(p)) return(plotly_empty())
  ggplotly(p, tooltip = FALSE) |> layout(margin = list(t = 0, b = 0, l = 0, r = 0))
})

# ══════════════════════════════════════════════════════════════════════════════
# OUTPUT: QUALITY DASHBOARD EXPERT
# ══════════════════════════════════════════════════════════════════════════════

output$zs_quality_expert_dashboard <- renderUI({
  quality <- zs_expert_quality()
  req(quality)

  tags$div(
    # Score principal
    tags$div(style = paste0("background:white;border-radius:12px;padding:22px;border:1px solid #e8edf2;",
                             "margin-bottom:16px;box-shadow:0 2px 8px rgba(0,0,0,.06);display:flex;align-items:center;gap:16px;"),
      tags$div(style = paste0("width:100px;height:100px;border-radius:50%;background:", quality$color,
                               ";display:flex;align-items:center;justify-content:center;",
                               "font-weight:800;font-size:32px;color:white;"),
        paste0(round(quality$score_global), "/100")
      ),
      tags$div(
        tags$div(style = "font-size:14px;font-weight:700;color:#00426A;margin-bottom:4px;",
          "Score de qualité global"),
        tags$div(style = paste0("font-size:20px;font-weight:800;color:", quality$color, ";margin:8px 0;"),
          quality$classification),
        tags$div(style = "font-size:11px;color:#666;", "Basé sur 10 critères OMS standardisés")
      )
    ),

    # Critères détaillés en tableau
    tags$div(style = "background:white;border-radius:12px;padding:16px;border:1px solid #e8edf2;",
      tags$div(style = "font-size:12px;font-weight:600;color:#00426A;margin-bottom:12px;text-transform:uppercase;letter-spacing:1px;",
        icon("list-check"), " Détail des 10 critères"),
      DTOutput("zs_quality_criteria_dt")
    )
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# OUTPUT: RAPPORT EXPERT TEXTE
# ══════════════════════════════════════════════════════════════════════════════

output$zs_rapport_text <- renderUI({
  rapport <- zs_expert_report()
  req(rapport)

  tags$div(style = "background:white;border-radius:12px;padding:20px;font-size:13px;line-height:1.8;color:#333;",
    HTML(markdown::markdownToHTML(text = rapport))
  )
})

# Download rapport Markdown
output$zs_dl_rapport <- downloadHandler(
  filename = function() paste0("zscores_rapport_expert_", Sys.Date(), ".md"),
  content = function(file) {
    rapport <- zs_expert_report()
    writeLines(rapport, file)
  }
)
# Download rapport PDF
output$zs_dl_rapport_pdf <- downloadHandler(
  filename = function() paste0("zscores_rapport_expert_", Sys.Date(), ".pdf"),
  content = function(file) {
    rapport <- zs_expert_report()
    req(rapport)
    # Écrire le markdown dans un fichier temporaire puis convertir en PDF via rmarkdown
    tmp_md <- tempfile(fileext = ".md")
    writeLines(rapport, tmp_md)
    tryCatch({
      rmarkdown::render(tmp_md, output_format = rmarkdown::pdf_document(), output_file = file, quiet = TRUE)
    }, error = function(e) {
      # Fallback : enregistrer en .md si la conversion PDF échoue (LaTeX manquant, etc.)
      file.copy(tmp_md, file)
      showNotification(
        paste("Export PDF impossible (LaTeX requis). Fichier Markdown exporté à la place.", e$message),
        type = "warning", duration = 8
      )
    })
  }
)

# Output: Quality criteria datatable (extracted from renderUI for stability)
output$zs_quality_criteria_dt <- renderDT({
  quality <- zs_expert_quality()
  req(quality, quality$criteria_scores)
  datatable(quality$criteria_scores,
    rownames = FALSE,
    options = list(dom = "t", pageLength = 10, scrollX = TRUE),
    class = "compact stripe") |>
    formatStyle("Score",
      backgroundColor = styleInterval(c(60, 75, 90), c("#ffcdd2", "#fff9c4", "#c8e6c9", "#81c784")))
})

