# Extrait de app_v2_final.R — modules de recherche avancée et bases de données externes
  # ── Modules avancés (Recherche) — chargés au démarrage ─────────────────────
  output$forecasting_ui_dyn  <- renderUI({ advancedForecastingUI("forecasting", lang()) })
  output$geolocation_ui_dyn  <- renderUI({ advancedGeolocationUI("geolocation", lang()) })
  output$nlp_ui_dyn          <- renderUI({ advancedNLPUI("nlp", lang()) })
  output$benchmarking_ui_dyn <- renderUI({ advancedBenchmarkingUI("benchmarking", lang()) })

  # Server avancés — initialisés directement (pas dans un observe)
  advancedForecastingServer("forecasting", df_f, map_cols, lang = lang)
  advancedGeolocationServer("geolocation", df_f, lang = lang)
  advancedNLPServer("nlp", df_f, lang = lang)
  advancedBenchmarkingServer("benchmarking", df_f, lang = lang)

  # ── BASES DE DONNÉES & SOURCES EXTERNES ────────────────────────────────
  output$db_sql_ui_dyn <- renderUI({
    load_lazy_module("database_sql")
    if (exists("databaseSQLUI", mode = "function")) {
      databaseSQLUI("db_sql_mod", lang())
    } else {
      tags$div(class = "nutri-empty-state",
        tags$h4(icon("exclamation-triangle"), " Module SQL non disponible"),
        tags$p("Erreur au chargement de mod_database_sql.R"))
    }
  })
  output$db_dhis2_ui_dyn <- renderUI({
    load_lazy_module("dhis2")
    if (exists("dhis2UI", mode = "function")) {
      dhis2UI("db_dhis2_mod", lang())
    } else {
      tags$div(class = "nutri-empty-state",
        tags$h4(icon("exclamation-triangle"), " Module DHIS2 non disponible"),
        tags$p("Erreur au chargement de mod_dhis2.R"))
    }
  })
  output$db_odk_ui_dyn <- renderUI({
    load_lazy_module("odk_central")
    if (exists("odkCentralUI", mode = "function")) {
      odkCentralUI("db_odk_mod", lang())
    } else {
      tags$div(class = "nutri-empty-state",
        tags$h4(icon("exclamation-triangle"), " Module ODK Central non disponible"),
        tags$p("Erreur au chargement de mod_odk_central.R"))
    }
  })
  output$db_redcap_ui_dyn <- renderUI({
    load_lazy_module("redcap")
    if (exists("redcapUI", mode = "function")) {
      redcapUI("db_redcap_mod", lang())
    } else {
      tags$div(class = "nutri-empty-state",
        tags$h4(icon("exclamation-triangle"), " Module REDCap non disponible"),
        tags$p("Erreur au chargement de mod_redcap.R"))
    }
  })

  # Les modules Server sont initialisés via un observer déclenché à la première visite
  # pour garantir que le lazy loading est fait avant l'appel Server
  .db_servers_init <- reactiveVal(FALSE)
  observe({
    req(input$menu %in% c("db_sql", "db_dhis2", "db_odk", "db_redcap"))
    if (.db_servers_init()) return()
    .db_servers_init(TRUE)

    load_lazy_module("database_sql")
    load_lazy_module("dhis2")
    load_lazy_module("odk_central")
    load_lazy_module("redcap")

    if (exists("databaseSQLServer", mode = "function"))
      databaseSQLServer("db_sql_mod",
        mapped_data = reactive(mapped_data()),
        set_mapped_data = function(df) mapped_data(df),
        lang = lang)

    if (exists("dhis2Server", mode = "function"))
      dhis2Server("db_dhis2_mod",
        mapped_data = reactive(mapped_data()),
        set_mapped_data = function(df) mapped_data(df),
        lang = lang)

    if (exists("odkCentralServer", mode = "function"))
      odkCentralServer("db_odk_mod",
        mapped_data = reactive(mapped_data()),
        set_mapped_data = function(df) mapped_data(df),
        lang = lang)

    if (exists("redcapServer", mode = "function"))
      redcapServer("db_redcap_mod",
        mapped_data = reactive(mapped_data()),
        set_mapped_data = function(df) mapped_data(df),
        lang = lang)
  })

