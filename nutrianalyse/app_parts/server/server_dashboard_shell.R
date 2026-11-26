# Extrait de app_v2_final.R — statut sidebar, filtres du dashboard, thème
  # ── SIDEBAR STATUS ──────────────────────────────────────────────────────────
  output$sidebar_status <- renderUI({
    df <- mapped_data()
    if (is.null(df)) {
      tags$div(style="text-align:center;color:#5a9ec4;font-size:11px;margin-top:6px;",
               "Aucune donnée · Importez un fichier ci-dessus")
    } else {
      tags$div(style="margin-top:6px;display:flex;flex-wrap:wrap;gap:4px;",
        tags$span(class="badge-ok", paste0(nrow(df)," obs.")),
        tags$span(class="badge-info", paste0(ncol(df)," vars")),
        if(has_oms()) tags$span(class="badge-ok","✓ OMS")
        else tags$span(class="badge-warn","Mapper OMS")
      )
    }
  })

  output$header_status <- renderUI({
    df <- mapped_data()
    if (is.null(df)) tags$span("Aucune donnée")
    else tags$span(nrow(df), " observations |", ncol(df), " variables",
                   if(has_oms()) " | ✓ Indicateurs OMS calculés" else " | Mapper les colonnes OMS")
  })

  # ═══ Synchronisation palette globale → sélecteurs locaux ═══
  # ── Synchro palette couleurs vers les modules d'exploration ──
  # (uni_pal / biv_pal sont désormais synchronisés dans leurs modules respectifs)

  # ── Thème d'interface — injection CSS dynamique complète ──
  observeEvent(input$ui_theme, {
    tn <- input$ui_theme
    if (is.null(tn) || !tn %in% UI_THEME_CHOICES) return()

    css_full <- build_theme_css(tn)
    # Échapper les quotes simples pour le JS
    css_escaped <- gsub("'", "\\\\'", gsub("[\n\r]+", " ", css_full))

    shinyjs::runjs(sprintf(
      "var el=document.getElementById('dynamic-ui-theme');
       if(!el){el=document.createElement('style');el.id='dynamic-ui-theme';document.head.appendChild(el);}
       el.textContent='%s';",
      css_escaped
    ))

    showNotification(
      sprintf("\U0001f3a8 Th\u00e8me d'interface : %s", tn),
      type = "message", duration = 2
    )
  }, ignoreInit = TRUE)

  # Déclencheur historique conservé pour df_f()
  dash_filter_trigger <- reactiveVal(0L)

