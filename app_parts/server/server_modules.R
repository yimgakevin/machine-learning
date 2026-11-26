# Extrait de app_v2_final.R — cartographie v2 et modules Shiny formels
  # ── CARTOGRAPHIE v2 ──────────────────────────────────────────────────────────
  cat("[CARTO] call reached
")
  server_carto_pro(input, output, session, df_f, map_cols)
  cat("[CARTO] call returned
")


  # ── MODULES FORMELS (moduleServer) ──────────────────────────────────────────
  # Wrappers renderUI i18n — reconstruisent le module UI quand la langue change
  output$epidemio_ui_dyn  <- renderUI({ epidemioUI("epidemio_mod",   lang()) })
  output$survie_ui_dyn    <- renderUI({ survieUI("survie_mod",       lang()) })
  output$predictif_ui_dyn <- renderUI({ predictifUI("predictif_mod", lang()) })
  output$rapport_ui_dyn   <- renderUI({ rapportUI("rapport_mod",     lang()) })

  epidemioServer("epidemio_mod",   df_f, map_cols,  lang = lang)
  survieServer("survie_mod",       df_f, map_cols,  lang = lang)
  predictifServer("predictif_mod", df_f, map_cols,  lang = lang)
  rapportServer("rapport_mod", df_f, map_cols, mapped_data,
                vars_num_fn = vars_num, has_oms_fn = has_oms, lang = lang)

  # ── MODULES EXPERT (moduleServer) ──────────────────────────────────────────
  output$exec_dash_ui_dyn      <- renderUI({ dashboardExecUI("exec_dash_mod",     lang()) })
  output$qualite_auto_ui_dyn   <- renderUI({ qualiteAutoUI("qualite_auto_mod",    lang()) })
  output$audit_trail_ui_dyn    <- renderUI({ auditTrailUI("audit_trail_mod",      lang()) })
  output$temporal_tests_ui_dyn <- renderUI({ temporalTestsUI("temporal_tests_mod",lang()) })

  dashboardExecServer("exec_dash_mod", df_f, map_cols, mapped_data, has_oms,
                      audit_log = add_audit_entry, lang = lang,
                      filter_context = filter_context)
  qualiteAutoServer("qualite_auto_mod", df_f, map_cols,
                    audit_log = add_audit_entry, lang = lang)
  auditTrailServer("audit_trail_mod", df_f, mapped_data, audit_log_rv, lang = lang)
  temporalTestsServer("temporal_tests_mod", df_f, map_cols,
                      audit_log = add_audit_entry, lang = lang)

  # Auto-log navigation entre onglets
  observeEvent(input$menu, {
    add_audit_entry(list(
      timestamp = Sys.time(), action = "navigation",
      module = "Navigation", details = paste0("Onglet: ", input$menu)
    ))
  })

