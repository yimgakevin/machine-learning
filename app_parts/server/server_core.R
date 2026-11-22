# Extrait de app_v2_final.R — i18n, palette, authentification, réactifs de base, géo, chatbot, audit trail

  # ── i18n — Langue active ─────────────────────────────────────────────────────
  # TRANSLATIONS et tr() sont déjà chargés globalement via source("i18n.R") en tête de fichier
  lang <- reactiveVal("fr")

  observeEvent(input$lang_fr, { lang("fr") }, ignoreInit = TRUE)
  observeEvent(input$lang_en, { lang("en") }, ignoreInit = TRUE)

  # Highlight du bouton actif
  observe({
    l <- lang()
    if (l == "fr") {
      shinyjs::runjs("
        document.getElementById('lang_fr').style.background='#D4A843';document.getElementById('lang_fr').style.color='#0B3954';
        document.getElementById('lang_en').style.background='rgba(255,255,255,.2)';
      ")
    } else {
      shinyjs::runjs("
        document.getElementById('lang_fr').style.background='rgba(255,255,255,.2)';
        document.getElementById('lang_en').style.background='#D4A843';document.getElementById('lang_en').style.color='#0B3954';
      ")
    }
  })

  # ── Palette globale — reactive helper ──────────────────────────────────────
  # Raccourci : gpal(n, type) retourne n couleurs selon le thème choisi
  gpal <- function(n, type = "cat") {
    pn <- input$global_palette
    if (is.null(pn)) pn <- "OMS officiel"
    get_global_pal(n, type, pn)
  }
  # Nom de la palette active (pour passer aux fonctions)
  gpal_name <- reactive({
    pn <- input$global_palette
    if (is.null(pn)) "OMS officiel" else pn
  })
  # Couleur primaire et secondaire du thème actif
  gprimary   <- function() get_primary_color(gpal_name())
  gsecondary <- function() get_secondary_color(gpal_name())

  # ── Outputs i18n — sidebar upload ──────────────────────────────────────────
  output$sidebar_upload_label <- renderUI({ tr("sidebar_upload", lang()) })
  output$sidebar_btn_label    <- renderUI({ tr("sidebar_btn",    lang()) })
  output$sidebar_formats_label<- renderUI({ tr("sidebar_formats",lang()) })
  output$sidebar_geo_label    <- renderUI({
    tags$span(tags$span("🌐"), "  ", tr("sidebar_geo_config", lang()))
  })

  # ── Outputs i18n — login page ──────────────────────────────────────────────
  output$login_subtitle_out <- renderUI({ tr("login_subtitle",lang()) })
  output$login_user_out     <- renderUI({ tr("login_user",    lang()) })
  output$login_pass_out     <- renderUI({ tr("login_pass",    lang()) })
  output$login_btn_out      <- renderUI({ tr("login_btn",     lang()) })
  output$login_error_out    <- renderUI({ tr("login_error",   lang()) })
  output$login_footer_out   <- renderUI({ tr("login_footer",  lang()) })

  # ── Menu dynamique i18n ────────────────────────────────────────────────────
  output$dynamic_menu <- renderMenu({
    l <- lang()
    sidebarMenu(
      id = "menu",
      menuItem(tr("nav_dashboard",  l), tabName="accueil",    icon=icon("tachometer-alt"), selected=TRUE),
      menuItem(tr("nav_import",     l), tabName="import",     icon=icon("magic")),
      menuItem(tr("nav_explor",     l), tabName="explor",     icon=icon("search")),
      menuItem(tr("nav_dplyr",      l), tabName="dplyr",      icon=icon("filter")),
      menuItem(tr("nav_graphs",     l), icon=icon("chart-bar"),
        menuSubItem(tr("nav_uni",     l), tabName="uni",      icon=icon("chart-bar")),
        menuSubItem(tr("nav_biv",     l), tabName="biv",      icon=icon("braille")),
        menuSubItem(tr("nav_descoms", l), tabName="descoms",  icon=icon("table"))
      ),
      menuItem(tr("nav_infer",      l), tabName="infer",      icon=icon("flask")),
      menuItem(tr("nav_rdim",       l), tabName="rdim",       icon=icon("sitemap")),
      menuItem(tr("nav_clust",      l), tabName="clust",      icon=icon("object-group")),
      menuItem(tr("nav_qualite",    l), tabName="qualite",    icon=icon("check-double")),
      menuItem(tr("nav_prevalence", l), tabName="prevalence", icon=icon("chart-line")),
      menuItem(tr("nav_secu",       l), tabName="secu_alim",  icon=icon("utensils")),
      menuItem(tr("nav_mortalite", l), tabName="mortalite",  icon=icon("skull-crossbones")),
      menuItem(tr("nav_iycf",     l), tabName="iycf",       icon=icon("baby")),
      menuItem(tr("nav_anemie",   l), tabName="anemie",     icon=icon("tint")),
      menuItem(tr("nav_paludisme",l), tabName="paludisme",  icon=icon("bug")),
      menuItem(tr("nav_wash",     l), tabName="wash",       icon=icon("shower")),
      menuItem(tr("nav_menage",   l), tabName="menage",     icon=icon("house-user")),
      menuItem(tr("nav_handicap", l), tabName="handicap",   icon=icon("wheelchair")),
      menuItem(tr("nav_zscore",     l), tabName="zscore",     icon=icon("calculator")),
      menuItem(tr("nav_oms",        l), tabName="oms",        icon=icon("heartbeat")),
      menuItem(tr("nav_carto",      l), tabName="carto",      icon=icon("map")),
      menuItem(tr("nav_epidemio",   l), tabName="epidemio",   icon=icon("virus")),
      menuItem(tr("nav_survie",     l), tabName="survie",     icon=icon("hourglass-half")),
      menuItem(tr("nav_predictif",  l), tabName="predictif",  icon=icon("brain")),
      menuItem(tr("nav_rapport",    l), tabName="rapport",    icon=icon("download")),
      tags$hr(style="border-color:rgba(255,255,255,.15);margin:6px 12px;"),
      menuItem(tr("nav_expert",     l), icon=icon("star"), startExpanded=FALSE,
        menuSubItem(tr("nav_exec_dash",    l), tabName="exec_dash",      icon=icon("tachometer-alt")),
        menuSubItem(tr("nav_qualite_auto", l), tabName="qualite_auto",   icon=icon("shield-alt")),
        menuSubItem(tr("nav_audit",        l), tabName="audit_trail",    icon=icon("history")),
        menuSubItem(tr("nav_temporal",     l), tabName="temporal_tests", icon=icon("chart-line"))
      ),
      tags$hr(style="border-color:rgba(255,255,255,.15);margin:6px 12px;"),
      menuItem(tr("nav_advanced",   l), icon=icon("flask"), startExpanded=FALSE,
        menuSubItem(tr("nav_forecasting", l), tabName="advanced_forecasting", icon=icon("line-chart")),
        menuSubItem(tr("nav_geoloc",      l), tabName="advanced_geolocation", icon=icon("map")),
        menuSubItem(tr("nav_nlp",         l), tabName="advanced_nlp",         icon=icon("comments")),
        menuSubItem(tr("nav_benchmark",   l), tabName="advanced_benchmarking",icon=icon("chart-bar"))
      ),
      tags$hr(style="border-color:rgba(255,255,255,.15);margin:6px 12px;"),
      menuItem(tr("nav_databases",  l), icon=icon("database"), startExpanded=FALSE,
        menuSubItem(tr("nav_db_sql",    l), tabName="db_sql",    icon=icon("table")),
        menuSubItem(tr("nav_dhis2",     l), tabName="db_dhis2",  icon=icon("globe")),
        menuSubItem(tr("nav_odk",       l), tabName="db_odk",    icon=icon("wpforms")),
        menuSubItem(tr("nav_redcap",    l), tabName="db_redcap", icon=icon("hospital"))
      ),
      menuItem(tr("nav_about",      l), tabName="about", icon=icon("info-circle"))
    )
  })

  # ── Filtres du dashboard (label réactif) ────────────────────────────────────
  output$dashboard_filters_label <- renderUI({
    tags$small(
      style = "color:#5a9ec4;text-transform:uppercase;font-size:10px;letter-spacing:1px;",
      tr("sidebar_filters", lang())
    )
  })

  # ── Réactifs de base (déclarés EN PREMIER — utilisés par tous les modules) ───
  raw_data    <- reactiveVal(NULL)
  mapped_data <- reactiveVal(NULL)
  map_cols    <- reactiveVal(NULL)
  rdim_res    <- reactiveVal(NULL)
  clust_res   <- reactiveVal(NULL)

  # ── AUTHENTIFICATION ────────────────────────────────────────────────────────
  credentials <- shinyauthr::loginServer(
    id            = "login",
    data          = USER_BASE,
    user_col      = user,
    pwd_col       = password,
    sodium_hashed = TRUE,
    log_out       = reactive(input$logout_btn)
  )

  # Afficher l'app uniquement après connexion réussie
  observe({
    req(credentials()$user_auth)
    shinyjs::show("main_app_content")
  })

  # Infos utilisateur connecté
  current_user <- reactive({
    req(credentials()$user_auth)
    info <- USER_BASE[USER_BASE$user == credentials()$info$user, ]
    role <- info$role[1]
    list(
      user  = credentials()$info$user,
      name  = info$name[1],
      role  = role,
      perms = ROLE_PERMISSIONS[[role]]
    )
  })

  # Badge utilisateur dans le header
  output$header_status <- renderUI({
    if (!isTRUE(credentials()$user_auth)) return(NULL)
    u <- current_user()
    tags$span(
      tags$span(style = paste0("color:", u$perms$color, ";font-weight:600;"),
                u$perms$label),
      tags$span(style = "opacity:.7;margin-left:6px;", u$name),
      actionLink("logout_btn",
                 label     = tags$span(icon("sign-out-alt"), "Déconnexion"),
                 style     = "color:rgba(255,255,255,.7);margin-left:12px;font-size:11px;")
    )
  })

  # Garde — masquer le bouton IA si rôle lecteur
  observe({
    req(credentials()$user_auth)
    u <- current_user()
    if (!isTRUE(u$perms$can_ia)) {
      shinyjs::hide("ai_float_btn")
    }
  })

  # ── SOURCE : Intégration Shiny cartographie (local=TRUE = scope server) ──────
  source("carto_server_outputs.R",   local = TRUE)
  source("carto_expert_integration.R", local = TRUE)
  source("carto_server_pro.R",       local = TRUE)

  # ── SOURCE : Server expert descriptif + inférentiel ────────────────────────
  source("server_desc_infer_expert.R", local = TRUE)

  # ── SÉLECTEUR GÉOGRAPHIQUE MONDIAL ──────────────────────────────────────────
  selected_geo <- carto_country_selector_Server("country_selector")

  observe({
    geo_config <- selected_geo()
    if (isTRUE(geo_config$is_complete)) {
      message(sprintf("✅ GÉOGRAPHIE: %s → %s → %s",
                      geo_config$country_name,
                      geo_config$region,
                      geo_config$department))
    }
  })

  # ── PAYS CARTOGRAPHIQUE : le sélecteur pilote load_gadm() ───────────────────
  observeEvent(selected_geo()$country_code, {
    cc <- selected_geo()$country_code
    if (!is.null(cc) && nzchar(cc)) {
      set_carto_country(cc)
      showNotification(
        sprintf("🗺️ Cartographie alignée sur : %s", get_country_name(cc)),
        type = "message", duration = 3)
    }
  }, ignoreInit = FALSE)

  # ── DÉTECTION AUTOMATIQUE DU PAYS depuis les zones des données ──────────────
  observeEvent(mapped_data(), {
    d <- mapped_data()
    if (is.null(d) || !nrow(d)) return()
    mc <- map_cols()
    col_geo <- if (!is.null(mc$region) && mc$region %in% names(d)) mc$region
               else if (!is.null(mc$dept) && mc$dept %in% names(d)) mc$dept
               else NULL
    if (is.null(col_geo)) return()
    det <- tryCatch(auto_detect_country(d[[col_geo]]), error = function(e) NULL)
    if (!is.null(det)) {
      set_carto_country(det$country)
      showNotification(
        sprintf("🌍 Pays détecté depuis les données : %s (%d %% des zones reconnues)",
                get_country_name(det$country), round(det$ratio * 100)),
        type = "message", duration = 5)
    }
  }, ignoreInit = TRUE)

  # ── NIVEAU 3: CHATBOT IA — Gemini multi-tour + Boutons d'exécution ──────────
  chat_outputs <- chatbot_server("ai_chat",
                                 df          = reactive(mapped_data()),
                                 col_mapping = reactive(map_cols()))

  # Navigation vers l'onglet suggéré par l'IA
  observeEvent(input[["ai_chat-goto_tab"]], {
    tab_target <- input[["ai_chat-goto_tab"]]
    if (!is.null(tab_target) && nzchar(tab_target)) {
      shinydashboard::updateTabItems(session, "menu", selected = tab_target)
      showNotification(sprintf("📍 Navigation : %s", tab_target),
                       type = "message", duration = 3)
    }
  })

  # ── Audit Trail — Journal d'analyse horodaté (Expert) ───────────────────
  audit_log_rv <- reactiveVal(list())

  # Fonction pour ajouter une entrée au journal (isolate pour usage hors contexte réactif)
  add_audit_entry <- function(entry) {
    current <- isolate(audit_log_rv())
    current[[length(current) + 1]] <- entry
    audit_log_rv(current)
  }

  # Auto-log de la session au démarrage (flag pour exécution unique)
  .session_logged <- reactiveVal(FALSE)
  observe({
    if (!isolate(.session_logged())) {
      add_audit_entry(list(
        timestamp = Sys.time(),
        action = "session_start",
        module = "Système",
        details = "Session démarrée — NutriAnalyse Pro v2 Expert"
      ))
      .session_logged(TRUE)
    }
  })

