# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  carto_expert_integration.R — Intégration Shiny des moteurs experts        ║
# ║  Tier 1 : Jenks breaks enhancement                                        ║
# ║  (Tier 2-4 handlers sont dans carto_server_outputs.R)                     ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# TIER 1 : CHOROPLÈTHE EXPERT (amélioration Jenks)
# ══════════════════════════════════════════════════════════════════════════════

observe({
  req(mapped_data())
  d <- df_f()
  req(d)

  ind <- isolate(input$carto_indicateur)

  if (!is.null(ind) && ind %in% names(d)) {
    breaks <- tryCatch(
      calc_jenks_breaks(d[[ind]], n_classes = 5),
      error = function(e) NULL
    )
    session$userData$jenks_breaks <- breaks
  }
})

# ══════════════════════════════════════════════════════════════════════════════
# Résumé Moran's I (output unique, pas dans carto_server_outputs.R)
# ══════════════════════════════════════════════════════════════════════════════

output$carto_morans_summary <- renderUI({
  res <- tryCatch(carto_morans_result(), error = function(e) NULL)
  if (is.null(res)) return(NULL)

  tags$div(style = "background:white;padding:16px;border-radius:8px;border-left:4px solid #008DC9;",
    tags$h4("Statistique de Moran (Auto-corrélation spatiale)", style = "margin-top:0;"),
    tags$table(style = "width:100%;",
      tags$tr(
        tags$td(style = "font-weight:700;width:40%;", "Moran's I :"),
        tags$td(if (!is.null(res$I_value)) round(res$I_value, 4) else "N/A")
      ),
      tags$tr(
        tags$td(style = "font-weight:700;", "P-value :"),
        tags$td(if (!is.null(res$p_value)) round(res$p_value, 4) else "N/A")
      ),
      tags$tr(
        tags$td(style = "font-weight:700;", "Résultat :"),
        tags$td(tags$strong(if (!is.null(res$interpretation)) res$interpretation else "—"))
      )
    )
  )
})
