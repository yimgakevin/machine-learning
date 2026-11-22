# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  NutriAnalyse Pro v2 — Orchestrateur                                        ║
# ║  Le code est découpé dans app_parts/ (setup, ui/, server/)                  ║
# ╚══════════════════════════════════════════════════════════════════════════════╝
# Lancement : shiny::runApp("app.R")

# ── Setup : options, packages, modules, moteurs ──────────────────────────────
source("app_parts/00_setup.R", local = FALSE)

# ── Feuille de style globale ─────────────────────────────────────────────────
source("app_parts/01_css.R", local = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
# ── Interface : assemblage des morceaux UI ──────────────────────────────────
source("app_parts/ui/ui.R", local = FALSE)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

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

  # ── IMPORT ──────────────────────────────────────────────────────────────────
  observeEvent(input$upload_file, {
    req(input$upload_file)
    showNotification("⏳ Importation en cours...", id="notif_import", duration=NULL)
    df <- import_data(input$upload_file$datapath, input$upload_file$name)
    if (!is.list(df) || is.data.frame(df)) {
      raw_data(as.data.frame(df))
      m <- auto_detect(df)
      map_cols(m)
      df_enrich <- tryCatch(calc_indicateurs(df, m), error=function(e) {
        message("[IMPORT] calc_indicateurs ERREUR: ", e$message)
        df
      })
      # Filet de sécurité : créer .fcs/.rcsi/.hdds si calc_indicateurs a échoué
      df_enrich <- tryCatch(ensure_sa_columns(df_enrich), error=function(e) df_enrich)
      message("[IMPORT] Après enrichissement: .fcs=", ".fcs" %in% names(df_enrich),
              " | .rcsi=", ".rcsi" %in% names(df_enrich),
              " | ncol=", ncol(df_enrich))
      mapped_data(df_enrich)
      removeNotification("notif_import")
      showNotification(paste0("✅ ",nrow(df)," lignes · ",ncol(df)," variables importées"),
                       type="message", duration=4)
      # Audit trail
      add_audit_entry(list(
        timestamp = Sys.time(), action = "import",
        module = "Import", details = paste0("Fichier: ", input$upload_file$name,
          " — ", nrow(df), " lignes × ", ncol(df), " colonnes")
      ))
    } else {
      removeNotification("notif_import")
      showNotification(paste("❌ Erreur:", df$error), type="error", duration=6)
      add_audit_entry(list(
        timestamp = Sys.time(), action = "error",
        module = "Import", details = paste0("Erreur import: ", df$error)
      ))
    }
  })

  # Appliquer le mappeur manuellement
  observeEvent(input$apply_map, {
    df <- raw_data(); req(df)
    m  <- build_manual_map()
    map_cols(m)
    df_enrich <- tryCatch(calc_indicateurs(df, m), error=function(e) df)
    df_enrich <- tryCatch(ensure_sa_columns(df_enrich), error=function(e) df_enrich)
    mapped_data(df_enrich)
    showNotification("✅ Mappeur appliqué — indicateurs recalculés", type="message", duration=3)
  })

  build_manual_map <- function() {
    m <- map_cols()
    if (is.null(m)) return(auto_detect(raw_data()))
    for (k in names(OMS_DICT)) {
      inp_id <- paste0("map_", k)
      if (!is.null(input[[inp_id]]) && input[[inp_id]] != "")
        m[[k]] <- input[[inp_id]]
    }
    m
  }

  # ══════════════════════════════════════════════════════════════════
  # KOBOTOOLBOX / ODK — Import API et fichier
  # ══════════════════════════════════════════════════════════════════

  kobo_state <- reactiveValues(
    connected = FALSE, forms = NULL, selected_uid = NULL,
    kobo_data = NULL, mapping = NULL
  )

  # URL effective du serveur
  kobo_server_url <- reactive({
    if (input$kobo_server == "custom") input$kobo_custom_url %||% ""
    else input$kobo_server
  })

  # Test de connexion
  observeEvent(input$kobo_test, {
    req(input$kobo_token)
    url <- kobo_server_url()
    showNotification("🔌 Test de connexion...", id = "kobo_notif", duration = NULL)
    res <- kobo_test_connection(url, input$kobo_token)
    removeNotification("kobo_notif")
    kobo_state$connected <- res$ok
    showNotification(
      if (res$ok) paste0("✅ ", res$msg) else paste0("❌ ", res$msg),
      type = if (res$ok) "message" else "error", duration = 4
    )
  })

  output$kobo_connection_status <- renderUI({
    if (isTRUE(kobo_state$connected)) {
      tags$div(style = "background:#f0fdf4;border:1px solid #86efac;border-radius:6px;padding:8px;margin-top:8px;text-align:center;",
               tags$span(style = "color:#16a34a;font-weight:700;font-size:12px;", "✅ Connecté"))
    } else NULL
  })

  # Lister les formulaires
  observeEvent(input$kobo_list_forms, {
    req(input$kobo_token)
    url <- kobo_server_url()
    showNotification("📋 Chargement des formulaires...", id = "kobo_notif", duration = NULL)
    res <- kobo_list_forms(url, input$kobo_token)
    removeNotification("kobo_notif")
    if (res$ok) {
      kobo_state$forms <- res$forms
      kobo_state$connected <- TRUE
      showNotification(paste0("✅ ", res$count, " formulaire(s) trouvé(s)"),
                       type = "message", duration = 4)
    } else {
      showNotification(paste0("❌ ", res$msg), type = "error", duration = 5)
    }
  })

  # Afficher la liste des formulaires
  output$kobo_forms_list <- renderUI({
    forms <- kobo_state$forms
    if (is.null(forms) || length(forms) == 0) {
      return(tags$div(style = "text-align:center;padding:30px;color:#888;",
                      tags$p("Connectez-vous et cliquez « Lister les formulaires »"),
                      tags$p(style = "font-size:11px;",
                             "Serveurs supportés : OCHA, KoboToolbox.org, ou personnalisé")))
    }
    tagList(lapply(forms, render_kobo_form_card))
  })

  # Sélecteur de formulaire
  output$kobo_form_select_ui <- renderUI({
    forms <- kobo_state$forms
    if (is.null(forms) || length(forms) == 0) return(NULL)
    choices <- setNames(
      sapply(forms, function(f) f$uid),
      sapply(forms, function(f) paste0(f$name, " (", f$submissions, " sub.)"))
    )
    selectInput("kobo_form_uid", "Formulaire :", choices = choices, width = "100%")
  })

  # Télécharger les données
  observeEvent(input$kobo_download, {
    req(input$kobo_token, input$kobo_form_uid)
    url <- kobo_server_url()
    showNotification("📥 Téléchargement en cours...", id = "kobo_dl", duration = NULL)

    res <- kobo_download_data(url, input$kobo_token, input$kobo_form_uid)
    removeNotification("kobo_dl")

    if (res$ok) {
      kobo_state$kobo_data <- res$data

      # Auto-mapping
      kobo_state$mapping <- kobo_auto_map(names(res$data))

      # Charger dans l'app comme données principales
      raw_data(as.data.frame(res$data))
      m <- auto_detect(res$data)
      map_cols(m)
      df_enrich <- tryCatch(calc_indicateurs(res$data, m), error = function(e) res$data)
      df_enrich <- tryCatch(ensure_sa_columns(df_enrich), error = function(e) df_enrich)
      mapped_data(as.data.frame(df_enrich))

      showNotification(
        paste0("✅ ", res$n_rows, " lignes × ", res$n_cols, " colonnes importées depuis KoboToolbox"),
        type = "message", duration = 5)
    } else {
      showNotification(paste0("❌ ", res$msg), type = "error", duration = 5)
    }
  })

  output$kobo_download_status <- renderUI({
    d <- kobo_state$kobo_data
    if (is.null(d)) return(NULL)
    tags$div(style = "background:#f0fdf4;border:1px solid #86efac;border-radius:6px;padding:10px;",
      tags$div(style = "font-weight:700;color:#16a34a;font-size:13px;",
               paste0("✅ ", nrow(d), " lignes × ", ncol(d), " colonnes")),
      tags$div(style = "font-size:11px;color:#555;margin-top:4px;",
               "Données chargées dans l'application")
    )
  })

  # Mapping automatique
  output$kobo_mapping_html <- renderUI({
    mapping <- kobo_state$mapping
    if (is.null(mapping)) {
      return(tags$p(style = "color:#888;font-size:12px;",
                    "Le mapping sera affiché après le téléchargement"))
    }
    mapping_df <- kobo_mapping_summary(mapping)
    render_kobo_mapping_html(mapping_df)
  })

  # Aperçu des données Kobo
  output$kobo_preview_table <- renderDT({
    d <- kobo_state$kobo_data
    req(d)
    datatable(head(d, 50), rownames = FALSE,
              options = list(dom = "tp", pageLength = 10, scrollX = TRUE),
              class = "compact stripe hover")
  })

  # ── Import fichier ODK Export ──
  observeEvent(input$kobo_file_import, {
    req(input$kobo_file_upload)
    showNotification("📥 Import et nettoyage...", id = "kobo_file_notif", duration = NULL)

    df <- import_kobo_file(input$kobo_file_upload$datapath)
    removeNotification("kobo_file_notif")

    if (!is.null(df) && nrow(df) > 0) {
      kobo_state$kobo_data <- df
      kobo_state$mapping <- kobo_auto_map(names(df))

      # Charger dans l'app
      raw_data(as.data.frame(df))
      m <- auto_detect(df)
      map_cols(m)
      df_enrich <- tryCatch(calc_indicateurs(df, m), error = function(e) df)
      df_enrich <- tryCatch(ensure_sa_columns(df_enrich), error = function(e) df_enrich)
      mapped_data(as.data.frame(df_enrich))

      showNotification(
        paste0("✅ ", nrow(df), " lignes × ", ncol(df),
               " colonnes importées (colonnes système supprimées)"),
        type = "message", duration = 5)
    } else {
      showNotification("❌ Erreur lors de l'import du fichier", type = "error", duration = 5)
    }
  })

  output$kobo_file_status <- renderUI({
    d <- kobo_state$kobo_data
    if (is.null(d)) return(NULL)
    tags$div(style = "background:#f0fdf4;border:1px solid #86efac;border-radius:6px;padding:10px;margin-top:8px;",
      tags$div(style = "font-weight:700;color:#16a34a;", paste0("✅ ", nrow(d), " × ", ncol(d))),
      tags$div(style = "font-size:11px;color:#555;", "Colonnes système nettoyées")
    )
  })

  output$kobo_file_mapping <- renderUI({
    mapping <- kobo_state$mapping
    if (is.null(mapping)) return(NULL)
    mapping_df <- kobo_mapping_summary(mapping)
    tags$div(style = "margin-top:10px;",
      tags$div(class = "section-header", "Variables SMART détectées"),
      render_kobo_mapping_html(mapping_df)
    )
  })

  output$kobo_file_preview <- renderDT({
    d <- kobo_state$kobo_data
    req(d)
    datatable(head(d, 50), rownames = FALSE,
              options = list(dom = "tp", pageLength = 10, scrollX = TRUE),
              class = "compact stripe hover")
  })

  # Données filtrées
  df_f <- reactive({
    df <- mapped_data(); req(df)
    m  <- map_cols()
    d  <- df
    # Écouter les changements des filtres dashboard (via le trigger)
    dash_filter_trigger()

    # ── §1 GÉOGRAPHIE ─────────────────────────────────────────────────
    if (!is.null(m$region) && m$region %in% names(d) && !is.null(input$f_region) && length(input$f_region) > 0)
      if (input$f_region != "Toutes")
        d <- filter(d, .data[[m$region]] == input$f_region)

    if (!is.null(m$dept) && m$dept %in% names(d) && !is.null(input$f_dept) && length(input$f_dept) > 0)
      if (input$f_dept != "Tous")
        d <- filter(d, .data[[m$dept]] == input$f_dept)

    zone_col <- detect_col(names(d), c("zone","milieu","area","type_zone","setting","camp"))
    if (!is.null(zone_col) && !is.null(input$f_zone) && length(input$f_zone) > 0)
      d <- filter(d, as.character(.data[[zone_col]]) %in% input$f_zone)

    cluster_col <- detect_col(names(d), c("cluster","grappe","psu","cluster_id","clust"))
    if (!is.null(cluster_col) && !is.null(input$f_cluster) && length(input$f_cluster) > 0)
      d <- filter(d, as.character(.data[[cluster_col]]) %in% input$f_cluster)

    strate_col <- detect_col(names(d), c("strate","strata","stratum","strat"))
    if (!is.null(strate_col) && !is.null(input$f_strate) && length(input$f_strate) > 0)
      d <- filter(d, as.character(.data[[strate_col]]) %in% input$f_strate)

    # Vague / Round d'enquête
    if (!is.null(m$survey) && m$survey %in% names(d) &&
        !is.null(input$f_survey_round) && length(input$f_survey_round) > 0)
      d <- filter(d, as.character(.data[[m$survey]]) %in% input$f_survey_round)

    # ── §2 DÉMOGRAPHIE ────────────────────────────────────────────────
    if (!is.null(m$sexe) && m$sexe %in% names(d) && !is.null(input$f_sexe) && length(input$f_sexe) > 0)
      if (input$f_sexe != "Tous")
        d <- filter(d, .data[[m$sexe]] == input$f_sexe)

    if (!is.null(m$depl) && m$depl %in% names(d) && !is.null(input$f_depl))
      if (length(input$f_depl) > 0)
        d <- filter(d, as.character(.data[[m$depl]]) %in% input$f_depl)

    if (!is.null(m$age_m) && ".age" %in% names(d) && !is.null(input$f_age))
      d <- filter(d, is.na(.age) | between(.age, input$f_age[1], input$f_age[2]))

    age_grp_col <- detect_col(names(d), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
    if (!is.null(age_grp_col) && !is.null(input$f_age_grp) && length(input$f_age_grp) > 0)
      d <- filter(d, as.character(.data[[age_grp_col]]) %in% input$f_age_grp)

    # ── §3 INDICATEURS OMS ────────────────────────────────────────────
    if ("nutr_cat" %in% names(d) && !is.null(input$f_nutr) && length(input$f_nutr) > 0)
      d <- filter(d, as.character(nutr_cat) %in% input$f_nutr)

    if ("GAM" %in% names(d) && !is.null(input$f_gam) && input$f_gam != "Tous")
      d <- filter(d, GAM == as.logical(as.integer(input$f_gam)))

    if ("SAM" %in% names(d) && !is.null(input$f_sam) && input$f_sam != "Tous")
      d <- filter(d, SAM == as.logical(as.integer(input$f_sam)))

    if ("MAM" %in% names(d) && !is.null(input$f_mam) && input$f_mam != "Tous")
      d <- filter(d, MAM == as.logical(as.integer(input$f_mam)))

    if ("Stunting" %in% names(d) && !is.null(input$f_stunting) && input$f_stunting != "Tous")
      d <- filter(d, Stunting == as.logical(as.integer(input$f_stunting)))

    if ("Underweight" %in% names(d) && !is.null(input$f_underweight) && input$f_underweight != "Tous")
      d <- filter(d, Underweight == as.logical(as.integer(input$f_underweight)))

    oedeme_col <- detect_col(names(d), c("oedeme","oedema","edema","oedèmes","oedème"))
    if (!is.null(oedeme_col) && !is.null(input$f_oedeme) && input$f_oedeme != "Tous")
      d <- filter(d, as.character(.data[[oedeme_col]]) == input$f_oedeme)

    # ── §4 ANTHROPOMÉTRIE ─────────────────────────────────────────────
    if (".muac" %in% names(d) && !is.null(input$f_muac))
      d <- filter(d, is.na(.muac) | between(.muac, input$f_muac[1], input$f_muac[2]))

    if (".whz" %in% names(d) && !is.null(input$f_whz))
      d <- filter(d, is.na(.whz) | between(.whz, input$f_whz[1], input$f_whz[2]))

    if (".haz" %in% names(d) && !is.null(input$f_haz))
      d <- filter(d, is.na(.haz) | between(.haz, input$f_haz[1], input$f_haz[2]))

    if (".waz" %in% names(d) && !is.null(input$f_waz))
      d <- filter(d, is.na(.waz) | between(.waz, input$f_waz[1], input$f_waz[2]))

    # ── §5 QUALITÉ / ÉCHANTILLONNAGE ──────────────────────────────────
    if (!is.null(input$f_excl_na) && length(input$f_excl_na) > 0)
      d <- drop_na(d, any_of(input$f_excl_na))

    if (isTRUE(input$f_no_outlier)) {
      for (z_col in intersect(c(".whz",".haz",".waz"), names(d))) {
        q   <- quantile(d[[z_col]], c(.25,.75), na.rm=TRUE)
        iqr <- q[2] - q[1]
        d   <- filter(d, is.na(.data[[z_col]]) |
                         between(.data[[z_col]], q[1] - 3*iqr, q[2] + 3*iqr))
      }
    }

    if (!is.null(input$f_top_n) && input$f_top_n > 0)
      d <- slice_head(d, n = as.integer(input$f_top_n))

    d
  })

  # Types de variables
  vars_num <- reactive({
    df <- df_f(); req(df)
    v <- names(df)[vapply(df, is.numeric, logical(1)) & !startsWith(names(df), ".")]
    if (length(v) == 0) return(character(0))
    v[vapply(v, function(x) sum(!is.na(df[[x]])) > 2, logical(1))]
  })
  vars_cat <- reactive({
    df <- df_f(); req(df)
    nms <- names(df)
    if (length(nms) == 0) return(character(0))
    nms[vapply(nms, function(n) {
      x <- df[[n]]
      !startsWith(n, ".") && (is.factor(x) || is.character(x)) &&
      length(unique(na.omit(x))) >= 2 && length(unique(na.omit(x))) <= 50
    }, logical(1))]
  })
  vars_cat_pie <- reactive({
    vc <- vars_cat(); df <- df_f()
    if (length(vc) == 0) return(character(0))
    vc[vapply(vc, function(v) length(unique(na.omit(df[[v]]))) <= 8, logical(1))]
  })
  has_oms <- reactive("GAM" %in% names(df_f()))

  filter_context <- reactive({
    d <- df_f(); req(d)
    build_filter_context(d, mapped_data())
  })

  output$data_loaded <- reactive(!is.null(mapped_data()))
  outputOptions(output, "data_loaded", suspendWhenHidden=FALSE)
  # (kw_sig supprimé — remplacé par module unifié inferentiel_v4)

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

  output$dashboard_filters <- renderUI({
    df <- mapped_data(); req(df)
    m <- map_cols(); req(m)
    out  <- list()
    idx  <- 1

    # ── Helper pour créer un en-tête de section ──────────────────────
    section_hdr <- function(label)
      tags$div(class="f-section-hdr",
               style="margin:8px 0 3px;font-size:9px;text-transform:uppercase;
                      letter-spacing:1px;color:#4da8d4;border-bottom:1px solid
                      rgba(255,255,255,.15);padding-bottom:3px;", label)

    # ════════════════════════════════════════════
    # §1 — GÉOGRAPHIE
    # ════════════════════════════════════════════
    geo_items <- list()
    if (!is.null(m$region) && m$region %in% names(df)) {
      vals <- c("Toutes", sort(unique(na.omit(as.character(df[[m$region]])))))
      geo_items <- c(geo_items, list(
        selectInput("f_region","🗺️ Région :", choices=vals, selected="Toutes", width="100%")
      ))
    }
    if (!is.null(m$dept) && m$dept %in% names(df)) {
      vals <- c("Tous", sort(unique(na.omit(as.character(df[[m$dept]])))))
      geo_items <- c(geo_items, list(
        selectInput("f_dept","📍 Département :", choices=vals, selected="Tous", width="100%")
      ))
    }
    # Zone (urbain/rural/camp) — auto-détection
    zone_col <- detect_col(names(df), c("zone","milieu","area","type_zone","setting","camp"))
    if (!is.null(zone_col)) {
      vals <- sort(unique(na.omit(as.character(df[[zone_col]]))))
      geo_items <- c(geo_items, list(
        selectizeInput("f_zone","🏘️ Zone :", choices=c("Toutes"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Toutes les zones"))
      ))
    }
    # Cluster / Grappe
    cluster_col <- detect_col(names(df), c("cluster","grappe","psu","cluster_id","clust"))
    if (!is.null(cluster_col) && length(unique(df[[cluster_col]])) <= 200) {
      vals <- sort(unique(na.omit(as.character(df[[cluster_col]]))))
      geo_items <- c(geo_items, list(
        selectizeInput("f_cluster","🎯 Cluster/Grappe :", choices=c("Tous"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Tous",maxItems=50))
      ))
    }
    # Strate
    strate_col <- detect_col(names(df), c("strate","strata","stratum","strat"))
    if (!is.null(strate_col)) {
      vals <- sort(unique(na.omit(as.character(df[[strate_col]]))))
      geo_items <- c(geo_items, list(
        selectizeInput("f_strate","🗂️ Strate :", choices=c("Toutes"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Toutes"))
      ))
    }
    # Vague / Round d'enquête — auto-détection sur m$survey
    if (!is.null(m$survey) && m$survey %in% names(df)) {
      vals_sv <- sort(unique(na.omit(as.character(df[[m$survey]]))))
      if (length(vals_sv) >= 2) {
        geo_items <- c(geo_items, list(
          selectizeInput("f_survey_round", paste0("\U0001f504 ", m$survey, " :"),
                         choices = c("Toutes" = "", vals_sv),
                         selected = "", multiple = TRUE, width = "100%",
                         options = list(placeholder = "Toutes les vagues"))
        ))
      }
    }
    if (length(geo_items) > 0) {
      out[[idx]] <- tagList(section_hdr("📌 Géographie & Enquête"), geo_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §2 — DÉMOGRAPHIE
    # ════════════════════════════════════════════
    demo_items <- list()
    if (!is.null(m$sexe) && m$sexe %in% names(df)) {
      vals <- sort(unique(na.omit(as.character(df[[m$sexe]]))))
      demo_items <- c(demo_items, list(
        selectInput("f_sexe","👤 Sexe :", choices=c("Tous",vals), selected="Tous", width="100%")
      ))
    }
    if (!is.null(m$depl) && m$depl %in% names(df)) {
      vals <- sort(unique(na.omit(as.character(df[[m$depl]]))))
      demo_items <- c(demo_items, list(
        selectizeInput("f_depl","🏕️ Déplacement :", choices=c("Tous"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Tous statuts"))
      ))
    }
    if (".age" %in% names(df)) {
      r <- range(df$.age, na.rm=TRUE)
      demo_items <- c(demo_items, list(
        sliderInput("f_age","👶 Âge (mois) :",
                    min=floor(r[1]), max=ceiling(r[2]),
                    value=c(floor(r[1]),ceiling(r[2])), step=1, width="100%")
      ))
    }
    # Groupe d'âge
    age_grp_col <- detect_col(names(df), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
    if (!is.null(age_grp_col)) {
      vals <- sort(unique(na.omit(as.character(df[[age_grp_col]]))))
      demo_items <- c(demo_items, list(
        checkboxGroupInput("f_age_grp","📅 Groupe d'âge :",
                           choices=vals, selected=vals, inline=TRUE)
      ))
    }
    if (length(demo_items) > 0) {
      out[[idx]] <- tagList(section_hdr("👥 Démographie"), demo_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §3 — INDICATEURS OMS
    # ════════════════════════════════════════════
    oms_items <- list()
    if ("nutr_cat" %in% names(df)) {
      oms_items <- c(oms_items, list(
        checkboxGroupInput("f_nutr","📊 Statut nutritionnel :",
          choices=c("Normal","MAM","SAM"), selected=c("Normal","MAM","SAM"), inline=TRUE)
      ))
    }
    if ("GAM" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_gam","🔴 GAM :",
          choices=c("Tous","Cas GAM"="1","Non GAM"="0"), selected="Tous", width="100%")
      ))
    }
    if ("SAM" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_sam","🚨 SAM :",
          choices=c("Tous","Cas SAM"="1","Non SAM"="0"), selected="Tous", width="100%")
      ))
    }
    if ("MAM" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_mam","🟠 MAM :",
          choices=c("Tous","Cas MAM"="1","Non MAM"="0"), selected="Tous", width="100%")
      ))
    }
    if ("Stunting" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_stunting","📏 Stunting :",
          choices=c("Tous","Stunté"="1","Non stunté"="0"), selected="Tous", width="100%")
      ))
    }
    if ("Underweight" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_underweight","⚖️ Underweight :",
          choices=c("Tous","Insuffisance pondérale"="1","Normal"="0"),
          selected="Tous", width="100%")
      ))
    }
    oedeme_col <- detect_col(names(df), c("oedeme","oedema","edema","oedèmes","oedème"))
    if (!is.null(oedeme_col)) {
      vals_o <- sort(unique(na.omit(as.character(df[[oedeme_col]]))))
      oms_items <- c(oms_items, list(
        selectInput("f_oedeme","💧 Œdèmes :",
          choices=c("Tous",vals_o), selected="Tous", width="100%")
      ))
    }
    if (length(oms_items) > 0) {
      out[[idx]] <- tagList(section_hdr("🏥 Indicateurs OMS"), oms_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §4 — ANTHROPOMÉTRIE
    # ════════════════════════════════════════════
    anthro_items <- list()
    if (".muac" %in% names(df)) {
      r <- range(df$.muac, na.rm=TRUE)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_muac","📐 MUAC (mm) :",
                    min=floor(r[1]), max=ceiling(r[2]),
                    value=c(floor(r[1]),ceiling(r[2])), step=1, width="100%")
      ))
    }
    if (".whz" %in% names(df)) {
      r <- round(range(df$.whz, na.rm=TRUE), 1)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_whz","⚖️ WHZ (z-score) :",
                    min=r[1], max=r[2], value=r, step=0.1, width="100%")
      ))
    }
    if (".haz" %in% names(df)) {
      r <- round(range(df$.haz, na.rm=TRUE), 1)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_haz","📏 HAZ (z-score) :",
                    min=r[1], max=r[2], value=r, step=0.1, width="100%")
      ))
    }
    if (".waz" %in% names(df)) {
      r <- round(range(df$.waz, na.rm=TRUE), 1)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_waz","🏋️ WAZ (z-score) :",
                    min=r[1], max=r[2], value=r, step=0.1, width="100%")
      ))
    }
    if (length(anthro_items) > 0) {
      out[[idx]] <- tagList(section_hdr("📐 Anthropométrie"), anthro_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §5 — QUALITÉ / ÉCHANTILLONNAGE
    # ════════════════════════════════════════════
    qual_items <- list()
    # Variables clés disponibles pour drop_na
    cles <- intersect(c(m$region,m$dept,m$sexe,m$age_m,m$muac,m$whz,m$haz,m$waz),
                      names(df))
    cles <- cles[!is.na(cles)]
    if (length(cles) > 0) {
      qual_items <- c(qual_items, list(
        checkboxGroupInput("f_excl_na","🚫 Exclure NA sur :",
          choices=setNames(cles, cles), selected=NULL, inline=FALSE)
      ))
    }
    qual_items <- c(qual_items, list(
      numericInput("f_top_n","✂️ Top-N lignes (0=tous) :",
                   value=0, min=0, step=100, width="100%"),
      checkboxInput("f_no_outlier","❌ Exclure outliers z±3 (WHZ/HAZ/WAZ)",
                    value=FALSE)
    ))
    out[[idx]] <- tagList(section_hdr("🔬 Qualité & Échantillonnage"), qual_items)
    idx <- idx + 1

    # ════════════════════════════════════════════
    # Badge filtres actifs
    # ════════════════════════════════════════════
    out[[idx]] <- tags$div(style="margin-top:6px;",
      uiOutput("f_active_badge")
    )

    tagList(out)
  })

  # Badge : nombre de filtres actifs
  output$f_active_badge <- renderUI({
    df_all <- mapped_data(); req(df_all); m <- map_cols()
    n_actif <- 0
    if (!is.null(input$f_region) && input$f_region != "Toutes") n_actif <- n_actif + 1
    if (!is.null(input$f_dept)   && input$f_dept   != "Tous")   n_actif <- n_actif + 1
    if (!is.null(input$f_zone)   && length(input$f_zone)   > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_cluster)&& length(input$f_cluster)> 0) n_actif <- n_actif + 1
    if (!is.null(input$f_strate)        && length(input$f_strate)        > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_survey_round)  && length(input$f_survey_round)  > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_sexe)   && input$f_sexe   != "Tous")   n_actif <- n_actif + 1
    if (!is.null(input$f_depl)   && length(input$f_depl)   > 0) n_actif <- n_actif + 1
    if (".age" %in% names(df_all) && !is.null(input$f_age)) {
      r <- range(df_all$.age, na.rm=TRUE)
      if (input$f_age[1] > floor(r[1]) || input$f_age[2] < ceiling(r[2]))
        n_actif <- n_actif + 1
    }
    if (!is.null(input$f_age_grp) && length(input$f_age_grp) > 0) {
      age_grp_col <- detect_col(names(df_all), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
      if (!is.null(age_grp_col)) {
        all_vals <- sort(unique(na.omit(as.character(df_all[[age_grp_col]]))))
        if (!setequal(input$f_age_grp, all_vals)) n_actif <- n_actif + 1
      }
    }
    if (!is.null(input$f_nutr) && length(input$f_nutr) < 3) n_actif <- n_actif + 1
    if (!is.null(input$f_gam)  && input$f_gam  != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_sam)  && input$f_sam  != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_mam)  && input$f_mam  != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_stunting)    && input$f_stunting    != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_underweight) && input$f_underweight != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_oedeme)      && input$f_oedeme      != "Tous") n_actif <- n_actif + 1
    for (sl in c("f_muac","f_whz","f_haz","f_waz")) {
      col <- switch(sl, f_muac=".muac", f_whz=".whz", f_haz=".haz", f_waz=".waz")
      if (col %in% names(df_all) && !is.null(input[[sl]])) {
        r <- range(df_all[[col]], na.rm=TRUE)
        if (input[[sl]][1] > r[1] || input[[sl]][2] < r[2]) n_actif <- n_actif + 1
      }
    }
    if (!is.null(input$f_excl_na) && length(input$f_excl_na) > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_top_n)   && input$f_top_n > 0)           n_actif <- n_actif + 1
    if (isTRUE(input$f_no_outlier))                                n_actif <- n_actif + 1

    if (n_actif == 0)
      tags$div(style="font-size:10px;color:#64748b;text-align:center;",
               "Aucun filtre actif")
    else
      tags$div(style="text-align:center;",
        tags$span(style=paste0("background:#D98B14;color:white;border-radius:20px;",
                               "padding:2px 10px;font-size:11px;font-weight:600;"),
                  paste0("🔽 ", n_actif, " filtre", if(n_actif>1)"s" else "", " actif",
                         if(n_actif>1)"s" else ""))
      )
  })

  # Reset tous les filtres
  observeEvent(input$btn_reset_filters, {
    df <- mapped_data(); req(df); m <- map_cols()

    # § Géographie
    if (!is.null(m$region) && m$region %in% names(df))
      updateSelectInput(session, "f_region", selected="Toutes")
    if (!is.null(m$dept) && m$dept %in% names(df))
      updateSelectInput(session, "f_dept", selected="Tous")
    updateSelectizeInput(session, "f_zone",    selected=character(0))
    updateSelectizeInput(session, "f_cluster",       selected=character(0))
    updateSelectizeInput(session, "f_strate",        selected=character(0))
    updateSelectizeInput(session, "f_survey_round",  selected=character(0))

    # § Démographie
    if (!is.null(m$sexe) && m$sexe %in% names(df))
      updateSelectInput(session, "f_sexe", selected="Tous")
    updateSelectizeInput(session, "f_depl", selected=character(0))
    if (".age" %in% names(df)) {
      r <- range(df$.age, na.rm=TRUE)
      updateSliderInput(session, "f_age", value=c(floor(r[1]), ceiling(r[2])))
    }
    age_grp_col <- detect_col(names(df), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
    if (!is.null(age_grp_col)) {
      all_vals <- sort(unique(na.omit(as.character(df[[age_grp_col]]))))
      updateCheckboxGroupInput(session, "f_age_grp", selected=all_vals)
    }

    # § Indicateurs OMS
    if ("nutr_cat" %in% names(df))
      updateCheckboxGroupInput(session, "f_nutr", selected=c("Normal","MAM","SAM"))
    if ("GAM"         %in% names(df)) updateSelectInput(session, "f_gam",         selected="Tous")
    if ("SAM"         %in% names(df)) updateSelectInput(session, "f_sam",         selected="Tous")
    if ("MAM"         %in% names(df)) updateSelectInput(session, "f_mam",         selected="Tous")
    if ("Stunting"    %in% names(df)) updateSelectInput(session, "f_stunting",    selected="Tous")
    if ("Underweight" %in% names(df)) updateSelectInput(session, "f_underweight", selected="Tous")
    oedeme_col <- detect_col(names(df), c("oedeme","oedema","edema","oedèmes","oedème"))
    if (!is.null(oedeme_col))         updateSelectInput(session, "f_oedeme",      selected="Tous")

    # § Anthropométrie
    for (info in list(list("f_muac",".muac",1), list("f_whz",".whz",.1),
                      list("f_haz",".haz",.1),  list("f_waz",".waz",.1))) {
      id <- info[[1]]; col <- info[[2]]
      if (col %in% names(df)) {
        r <- range(df[[col]], na.rm=TRUE)
        updateSliderInput(session, id, value=r)
      }
    }

    # § Qualité
    updateCheckboxGroupInput(session, "f_excl_na", selected=NULL)
    updateNumericInput(session, "f_top_n", value=0)
    updateCheckboxInput(session, "f_no_outlier", value=FALSE)

    showNotification("✅ Tous les filtres réinitialisés", type="message", duration=2)
  })

  # ═══ Synchronisation palette globale → sélecteurs locaux ═══
  # ── Synchro palette couleurs vers les modules d'exploration ──
  observeEvent(input$global_palette, {
    pn <- input$global_palette
    if (!is.null(pn) && pn %in% PALETTE_CHOICES) {
      updateSelectInput(session, "uni_pal", selected = pn)
      updateSelectInput(session, "biv_pal", selected = pn)
    }
  }, ignoreInit = TRUE)

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

  # ── ACCUEIL ─────────────────────────────────────────────────────────────────
  output$accueil_badge <- renderUI({
    df <- mapped_data()
    if (is.null(df)) {
      tags$div(style="text-align:right;",
        tags$div(style="font-size:36px;opacity:.5;","⬆"),
        tags$p(style="font-size:13px;opacity:.8;","Importez un fichier pour commencer"))
    } else {
      tags$div(style="text-align:right;",
        tags$span(class="badge-ok",style="font-size:14px;padding:6px 14px;",
                  paste0("✓ ",nrow(df)," obs. · ",ncol(df)," variables")),
        br(), br(),
        if(has_oms()) tags$span(class="badge-ok",style="font-size:13px;padding:5px 12px;",
                                "✓ Indicateurs OMS calculés")
        else tags$span(class="badge-warn",style="font-size:13px;padding:5px 12px;",
                       "⚠ Mapper les colonnes OMS")
      )
    }
  })

  output$accueil_alertes <- renderUI({
    if (!has_oms()) return(NULL)
    d <- df_f()
    inds <- list(
      list(v="GAM", lbl="GAM", seuil=15),
      list(v="SAM", lbl="SAM", seuil=2),
      list(v="Stunting", lbl="Stunting", seuil=30)
    )
    alertes <- lapply(inds, function(ind) {
      if (!ind$v %in% names(d)) return(NULL)
      p <- round(mean(d[[ind$v]], na.rm=TRUE)*100, 1)
      if (is.na(p)) return(NULL)
      sev <- alerte_label(p)
      cls <- if(p < 5)"alerte-ok" else if(p < 10)"alerte-alerte" else if(p < 15)"alerte-serieux" else "alerte-crit"
      tags$div(class=paste("alerte-box", cls),
        tags$strong(paste0(sev, " — ", ind$lbl, " : ", p, "%")),
        " | Seuil OMS critique : ", ind$seuil, "% | Situation actuelle : ",
        if(p >= ind$seuil) "⚠️ Au-dessus du seuil critique" else "✓ En dessous du seuil critique"
      )
    })
    fluidRow(column(12, tagList(alertes)))
  })

  # ── Helper : créer une KPI card premium (accueil) ──
  make_nutri_kpi <- function(value, label, subtitle = "", icon_name = "chart-bar",
                            icon_bg = "#0077B6", severity = "acceptable",
                            progress_pct = NULL) {
    sev_colors <- list(
      acceptable = list(text = "#166534", badge_cls = "kpi-badge-acceptable"),
      alerte     = list(text = "#854d0e", badge_cls = "kpi-badge-alerte"),
      serieux    = list(text = "#9a3412", badge_cls = "kpi-badge-serieux"),
      critique   = list(text = "#991b1b", badge_cls = "kpi-badge-critique"),
      urgence    = list(text = "#7f1d1d", badge_cls = "kpi-badge-urgence")
    )
    sc <- sev_colors[[severity]] %||% sev_colors$acceptable
    bar_colors <- list(acceptable="#2E7D52", alerte="#D4A843", serieux="#D98B14",
                       critique="#BF3636", urgence="#5C1A1A")

    progress_html <- if (!is.null(progress_pct)) {
      pct <- min(100, max(0, progress_pct))
      bc <- bar_colors[[severity]] %||% "#0077B6"
      tags$div(class = "kpi-progress",
        tags$div(class = "kpi-progress-fill",
                 style = sprintf("width:%.0f%%;background:%s;", pct, bc)))
    }

    tags$div(class = paste("nutri-kpi-card", paste0("sev-", severity)),
      tags$span(class = "kpi-bg-icon", icon(icon_name)),
      tags$div(class = "kpi-header",
        tags$div(class = "kpi-icon", style = sprintf("background:%s;", icon_bg),
                 icon(icon_name)),
        tags$div(class = "kpi-label", label)
      ),
      tags$div(class = "kpi-value", style = sprintf("color:%s;", sc$text), value),
      if (nzchar(subtitle)) tags$p(class = "kpi-sub", subtitle),
      tags$span(class = paste("kpi-badge", sc$badge_cls),
                toupper(severity)),
      progress_html
    )
  }

  # ── Helper : palette des sévérités ──
  sev_icon_bg <- list(acceptable="#2E7D52", alerte="#D4A843",
                      serieux="#D98B14", critique="#BF3636", urgence="#5C1A1A")

  # ═══ Score barre horizontale ═══
  output$accueil_score_bar <- renderUI({
    if (!has_oms()) return(NULL)
    d <- filter_context()$data
    score <- measure_global_score(d)
    if (is.null(score)) return(NULL)
    score_global <- score$score
    sev_global <- score$severity
    sev_label <- toupper(sev_global)
    sev_col <- sev_icon_bg[[sev_global]]
    marker_pct <- 100 - score_global  # inversé : 100=bon (gauche), 0=mauvais (droite)

    tags$div(class = "nutri-score-bar-container",
      fluidRow(
        column(3,
          tags$div(style="text-align:center;",
            tags$div(style=sprintf("font-size:42px;font-weight:800;color:%s;line-height:1;", sev_col),
                     score_global),
            tags$div(style="font-size:12px;font-weight:600;color:#64748b;margin-top:4px;",
                     "SCORE GLOBAL"),
            tags$span(class=paste0("kpi-badge kpi-badge-",sev_global),
                      style="margin-top:6px;", sev_label)
          )
        ),
        column(9,
          tags$div(style="padding-top:12px;",
            tags$div(class = "nutri-score-bar",
              tags$div(class = "nutri-score-marker",
                       style = sprintf("left:%.0f%%;", marker_pct))
            ),
            tags$div(class = "nutri-score-labels",
              tags$span("Urgence"), tags$span("Critique"),
              tags$span("Sérieux"), tags$span("Alerte"),
              tags$span("Acceptable")
            )
          )
        )
      )
    )
  })

  # ═══ KPI Cards Premium ═══
  output$accueil_kpi_premium <- renderUI({
    df <- mapped_data()
    if (is.null(df)) {
      return(tags$div(class="nutri-empty-state",
        tags$div(class="empty-icon", icon("cloud-upload-alt")),
        tags$h3("Importez un jeu de données pour commencer"),
        tags$p("Formats : CSV \u00b7 Excel (.xlsx) \u00b7 SPSS (.sav) \u00b7 Stata (.dta) \u00b7 Parquet"),
        tags$p("Cliquez sur ", tags$strong("Importer un fichier"), " dans la barre latérale")
      ))
    }
    d <- filter_context()$data
    cards <- list()

    # Card 1 : Observations
    cards[[1]] <- make_nutri_kpi(
      value = format(nrow(d), big.mark = " "),
      label = "Observations",
      subtitle = sprintf("%d variables \u00b7 %d numériques \u00b7 %d catégorielles",
                         length(vars_num()) + length(vars_cat()), length(vars_num()), length(vars_cat())),
      icon_name = "database", icon_bg = "#0077B6", severity = "acceptable"
    )

    for (card in measure_card_specs(d, c("GAM", "SAM", "Stunting", "Underweight", ".whz", ".haz", "MUAC_GAM"))) {
      ms <- card$measure
      spec <- card$spec
      subtitle <- if (identical(card$id, "Stunting")) {
        sprintf("Seuil OMS sérieux : 40%%")
      } else if (identical(card$id, "Underweight")) {
        sprintf("Seuil OMS sérieux : 30%%")
      } else if (identical(card$id, "MUAC_GAM")) {
        "Périmètre brachial < 125mm"
      } else if (identical(ms$kind, "mean")) {
        sprintf("± %.2f SD · Réf. OMS : 0 ± 1", ms$sd)
      } else {
        measure_ci_text(ms)
      }
      value <- if (identical(ms$kind, "mean")) ms$mean else paste0(ms$pct, "%")
      progress <- if (identical(ms$kind, "binary") && !is.null(spec$max_pct)) {
        min(100, ms$pct / spec$max_pct * 100)
      } else NULL
      cards[[length(cards) + 1]] <- make_nutri_kpi(
        value = value, label = spec$label, subtitle = subtitle,
        icon_name = spec$icon, icon_bg = sev_icon_bg[[ms$severity]],
        severity = ms$severity, progress_pct = progress
      )
    }

    tags$div(class = "nutri-kpi-grid", tagList(cards))
  })

  # ═══ Compteur d'observations filtré ═══
  output$dash_filter_counter <- renderUI({
    ctx <- filter_context()
    tags$span(style = "font-size:11px;color:#64748b;",
      format(ctx$n, big.mark = " "), " / ",
      format(ctx$n_total, big.mark = " "), " obs."
    )
  })
  outputOptions(output, "dash_filter_counter", suspendWhenHidden = FALSE)

  # ═══ KPI Qualité des données + Démographie ═══
  output$accueil_kpi_qualite <- renderUI({
    df <- mapped_data()
    if (is.null(df)) return(NULL)
    d <- filter_context()$data; m <- map_cols()
    cards <- list()

    # Complétude globale
    total_cells <- nrow(d) * ncol(d)
    na_cells <- sum(is.na(d))
    completeness <- round((1 - na_cells / total_cells) * 100, 1)
    sev_comp <- if(completeness >= 95) "acceptable" else if(completeness >= 85) "alerte" else if(completeness >= 70) "serieux" else "critique"
    # Colonnes les plus incomplètes
    na_by_col <- sort(colMeans(is.na(d)) * 100, decreasing = TRUE)
    worst_cols <- head(na_by_col[na_by_col > 0], 3)
    worst_text <- if(length(worst_cols) > 0) {
      paste(sprintf("%s (%.0f%%)", names(worst_cols), worst_cols), collapse = " \u00b7 ")
    } else "Aucune colonne avec valeurs manquantes"

    cards[[1]] <- make_nutri_kpi(
      value = paste0(completeness, "%"),
      label = "Complétude des données",
      subtitle = paste0("\u26a0 Plus incomplets : ", worst_text),
      icon_name = "check-double", icon_bg = sev_icon_bg[[sev_comp]],
      severity = sev_comp, progress_pct = completeness
    )

    # Nombre de doublons
    n_dup <- sum(duplicated(d))
    sev_dup <- if(n_dup == 0) "acceptable" else if(n_dup / nrow(d) < 0.02) "alerte" else "serieux"
    cards[[length(cards)+1]] <- make_nutri_kpi(
      value = format(n_dup, big.mark = " "),
      label = "Lignes dupliquées",
      subtitle = sprintf("%.1f%% du total \u00b7 %s lignes uniques",
                         n_dup / nrow(d) * 100, format(nrow(d) - n_dup, big.mark = " ")),
      icon_name = "clone", icon_bg = sev_icon_bg[[sev_dup]],
      severity = sev_dup
    )

    # Ratio filles/garçons (démographie)
    if (!is.null(m$sexe) && m$sexe %in% names(d)) {
      sex_tbl <- table(d[[m$sexe]], useNA = "no")
      if (length(sex_tbl) >= 2) {
        sex_vals <- sort(sex_tbl, decreasing = TRUE)
        ratio_txt <- paste(sprintf("%s: %s (%.0f%%)",
                                   names(sex_vals), format(as.integer(sex_vals), big.mark=" "),
                                   as.integer(sex_vals)/sum(sex_vals)*100), collapse = " \u00b7 ")
        # Vérifier l'équilibre (50/50 idéal)
        max_pct <- max(as.integer(sex_vals)) / sum(sex_vals) * 100
        sev_sex <- if(max_pct <= 55) "acceptable" else if(max_pct <= 60) "alerte" else "serieux"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = sprintf("%.0f/%.0f", as.integer(sex_vals[1])/sum(sex_vals)*100,
                          as.integer(sex_vals[2])/sum(sex_vals)*100),
          label = "Ratio par sexe",
          subtitle = ratio_txt,
          icon_name = "venus-mars", icon_bg = sev_icon_bg[[sev_sex]],
          severity = sev_sex
        )
      }
    }

    # Âge moyen
    if (".age" %in% names(d)) {
      age_m <- round(mean(d$.age, na.rm = TRUE), 1)
      age_sd <- round(sd(d$.age, na.rm = TRUE), 1)
      age_range <- range(d$.age, na.rm = TRUE)
      cards[[length(cards)+1]] <- make_nutri_kpi(
        value = paste0(age_m, " mois"),
        label = "\u00c2ge moyen",
        subtitle = sprintf("\u00b1 %.1f SD \u00b7 Min: %.0f \u00b7 Max: %.0f mois",
                           age_sd, age_range[1], age_range[2]),
        icon_name = "baby", icon_bg = "#0077B6", severity = "acceptable"
      )
    }

    # Nombre de régions/clusters
    if (!is.null(m$region) && m$region %in% names(d)) {
      n_regions <- length(unique(na.omit(d[[m$region]])))
      avg_per_region <- round(nrow(d) / n_regions)
      sev_n <- if(avg_per_region >= 25) "acceptable" else if(avg_per_region >= 15) "alerte" else "serieux"
      cards[[length(cards)+1]] <- make_nutri_kpi(
        value = n_regions,
        label = "Zones / Régions",
        subtitle = sprintf("~%s obs/zone \u00b7 colonne : %s",
                           format(avg_per_region, big.mark = " "), m$region),
        icon_name = "map-marked-alt", icon_bg = sev_icon_bg[[sev_n]],
        severity = sev_n
      )
    }

    # MUAC moyen
    if (".muac" %in% names(d)) {
      muac_m <- round(mean(d$.muac, na.rm=TRUE), 1)
      muac_sd <- round(sd(d$.muac, na.rm=TRUE), 1)
      sev_muac <- if(muac_m >= 135) "acceptable" else if(muac_m >= 125) "alerte" else if(muac_m >= 115) "serieux" else "critique"
      cards[[length(cards)+1]] <- make_nutri_kpi(
        value = paste0(muac_m, " mm"),
        label = "MUAC moyen (Périmètre brachial)",
        subtitle = sprintf("\u00b1 %.1f mm \u00b7 < 125mm = MAG \u00b7 < 115mm = MAS", muac_sd),
        icon_name = "ruler", icon_bg = sev_icon_bg[[sev_muac]],
        severity = sev_muac
      )
    }

    if (length(cards) == 0) return(NULL)
    tagList(
      tags$div(class = "nutri-section-title", icon("clipboard-check"),
               " Qualité des données & Démographie"),
      tags$div(class = "nutri-kpi-grid", tagList(cards))
    )
  })

  # ═══ KPI Sécurité alimentaire (auto-détecté) ═══
  output$accueil_kpi_secu <- renderUI({
    d <- filter_context()$data
    if (is.null(d)) return(NULL)
    cards <- list()
    cols_lower <- tolower(names(d))

    # Helper : préfère la colonne interne calculée (ex: .fcs) sinon cherche dans les noms
    pick_sa_col <- function(d, internal_col, patterns, numeric_only = TRUE) {
      if (internal_col %in% names(d) && is.numeric(d[[internal_col]]) && !all(is.na(d[[internal_col]])))
        return(internal_col)
      candidates <- names(d)[grepl(patterns, tolower(names(d)))]
      # Exclure les colonnes internes (commençant par ".") et les catégories (_cat)
      candidates <- candidates[!grepl("^\\.|_cat$", candidates)]
      if (numeric_only) candidates <- candidates[vapply(candidates, function(v) {
        tryCatch(isTRUE(is.numeric(d[[v]])), error = function(e) FALSE)
      }, logical(1))]
      if (length(candidates) > 0) return(candidates[1])
      NULL
    }

    # FCS (Food Consumption Score)
    fcs_col <- pick_sa_col(d, ".fcs", "^fcs$|^fcs_score$|food.?consumption.?score|^score.*fcs|^sca$")
    if (!is.null(fcs_col)) {
      fcs_vals <- na.omit(d[[fcs_col]])
      if (length(fcs_vals) > 0 && is.numeric(fcs_vals)) {
        m_fcs <- round(mean(fcs_vals), 1)
        poor <- round(mean(fcs_vals <= 21) * 100, 1)
        border <- round(mean(fcs_vals > 21 & fcs_vals <= 35) * 100, 1)
        acceptable <- round(mean(fcs_vals > 35) * 100, 1)
        sev <- if(poor >= 40) "critique" else if(poor >= 20) "serieux" else if(poor >= 10) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_fcs, label = "FCS moyen (Consommation Alimentaire)",
          subtitle = sprintf("Pauvre \u226421: %.1f%% \u00b7 Limite 21-35: %.1f%% \u00b7 Acceptable >35: %.1f%%",
                             poor, border, acceptable),
          icon_name = "utensils", icon_bg = sev_icon_bg[[sev]],
          severity = sev, progress_pct = min(100, acceptable)
        )
      }
    }

    # rCSI (reduced Coping Strategy Index) — utilise la colonne calculée .rcsi en priorité
    rcsi_col <- pick_sa_col(d, ".rcsi", "^rcsi$|^rcsi_score$|^r_csi$|^rcsiscore$")
    if (!is.null(rcsi_col)) {
      rcsi_vals <- na.omit(d[[rcsi_col]])
      if (length(rcsi_vals) > 0 && is.numeric(rcsi_vals)) {
        m_rcsi <- round(mean(rcsi_vals), 1)
        high <- round(mean(rcsi_vals >= 19) * 100, 1)
        sev <- if(high >= 50) "critique" else if(high >= 30) "serieux" else if(high >= 15) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_rcsi, label = "rCSI moyen (Strat\u00e9gies d'Adaptation)",
          subtitle = sprintf("Niveau \u00e9lev\u00e9 (\u226519) : %.1f%% \u00b7 Plus le score est haut, plus la situation est grave", high),
          icon_name = "hand-holding-heart", icon_bg = sev_icon_bg[[sev]],
          severity = sev
        )
      }
    }

    # HDDS (Household Dietary Diversity Score) — utilise .hdds en priorité
    hdds_col <- pick_sa_col(d, ".hdds", "^hdds$|^hdds_score$|dietary.?diversity.?score|^dds$|^sdam$")
    if (!is.null(hdds_col)) {
      hdds_vals <- na.omit(d[[hdds_col]])
      if (length(hdds_vals) > 0 && is.numeric(hdds_vals)) {
        m_hdds <- round(mean(hdds_vals), 1)
        low <- round(mean(hdds_vals <= 3) * 100, 1)
        sev <- if(low >= 50) "critique" else if(low >= 30) "serieux" else if(low >= 15) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_hdds, label = "HDDS moyen (Diversit\u00e9 Alimentaire)",
          subtitle = sprintf("Faible diversit\u00e9 (\u22643 groupes) : %.1f%%", low),
          icon_name = "carrot", icon_bg = sev_icon_bg[[sev]],
          severity = sev
        )
      }
    }

    # HHS (Household Hunger Scale) — utilise .hhs en priorité
    hhs_col <- pick_sa_col(d, ".hhs", "^hhs$|^hhs_score$|^hhs_total$|household.?hunger.?scale")
    if (!is.null(hhs_col)) {
      hhs_vals <- na.omit(d[[hhs_col]])
      if (length(hhs_vals) > 0 && is.numeric(hhs_vals)) {
        m_hhs <- round(mean(hhs_vals), 1)
        severe <- round(mean(hhs_vals >= 4) * 100, 1)
        sev <- if(severe >= 20) "critique" else if(severe >= 10) "serieux" else if(severe >= 5) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_hhs, label = "HHS moyen (\u00c9chelle de la Faim)",
          subtitle = sprintf("Faim s\u00e9v\u00e8re (\u22654) : %.1f%% \u00b7 Seuils : 0-1 Peu/Pas \u00b7 2-3 Mod\u00e9r\u00e9e \u00b7 4-6 S\u00e9v\u00e8re", severe),
          icon_name = "frown", icon_bg = sev_icon_bg[[sev]],
          severity = sev
        )
      }
    }

    # ── MUAC (Périmètre Brachial) — carte KPI dashboard ──
    muac_col <- intersect(c(".muac", "muac", "MUAC", "pb", "PB", "muac_mm"), names(d))
    if (length(muac_col) > 0) {
      muac_vals <- na.omit(as.numeric(d[[muac_col[1]]]))
      if (length(muac_vals) >= 5) {
        pct_mag <- round(mean(muac_vals < 125) * 100, 1)
        pct_mas <- round(mean(muac_vals < 115) * 100, 1)
        muac_mean <- round(mean(muac_vals), 1)
        sev_muac <- if (pct_mag >= 15) "critique" else if (pct_mag >= 10) "serieux" else if (pct_mag >= 5) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = paste0(pct_mag, "%"), label = "MAG — MUAC (P\u00e9rim\u00e8tre Brachial)",
          subtitle = sprintf("MAG (< 125mm): %.1f%% \u00b7 MAS (< 115mm): %.1f%% \u00b7 Moy: %.1f mm",
                             pct_mag, pct_mas, muac_mean),
          icon_name = "ruler", icon_bg = sev_icon_bg[[sev_muac]],
          severity = sev_muac,
          progress_pct = min(100, pct_mag * 3)
        )
      }
    }

    # ── Estimation Phase IPC (si FCS + rCSI disponibles) ──
    fcs_s_ipc <- if (!is.null(fcs_col) && fcs_col %in% names(d)) d[[fcs_col]] else NULL
    rcsi_s_ipc <- if (!is.null(rcsi_col) && rcsi_col %in% names(d)) d[[rcsi_col]] else NULL
    hhs_s_ipc  <- if (!is.null(hhs_col) && hhs_col %in% names(d)) d[[hhs_col]] else NULL
    muac_s_ipc <- if (length(muac_col) > 0) as.numeric(d[[muac_col[1]]]) else NULL

    if (!is.null(fcs_s_ipc) || !is.null(rcsi_s_ipc)) {
      phases_ipc <- tryCatch(
        calc_ipc_phase(fcs_s_ipc, rcsi_s_ipc, hhs_s_ipc, muac_s_ipc),
        error = function(e) NULL
      )
      if (!is.null(phases_ipc)) {
        pct_phase3plus <- round(mean(phases_ipc >= 3) * 100, 1)
        pct_phase4plus <- round(mean(phases_ipc >= 4) * 100, 1)
        sev_ipc <- if (pct_phase3plus >= 20) "critique" else if (pct_phase3plus >= 10) "serieux" else if (pct_phase3plus >= 5) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = paste0(pct_phase3plus, "%"),
          label = "Phase IPC 3+ (Crise \u00e0 Urgence)",
          subtitle = sprintf("Phase 4+: %.1f%% \u00b7 Estimation approximative FCS+rCSI+HHS%s",
                             pct_phase4plus,
                             if (!is.null(muac_s_ipc)) "+MUAC" else ""),
          icon_name = "globe",
          icon_bg = sev_icon_bg[[sev_ipc]],
          severity = sev_ipc
        )
      }
    }

    if (length(cards) == 0) return(NULL)
    tagList(
      tags$div(class = "nutri-section-title", icon("utensils"),
               " Sécurité alimentaire · MUAC · Phase IPC"),
      tags$div(class = "nutri-kpi-grid", tagList(cards))
    )
  })

  output$accueil_prev_chart <- renderPlotly({
    req(has_oms()); d <- filter_context()$data; m <- map_cols()
    grp_col <- if (!is.null(m$region) && m$region %in% names(d)) m$region else vars_cat()[1]
    req(!is.null(grp_col))
    inds <- intersect(c("GAM","SAM","Stunting","Underweight"), names(d))
    req(length(inds) > 0)
    res <- d |> group_by(.data[[grp_col]]) |>
      group_modify(~map_dfr(inds, function(v)
        calc_prev_wilson(.x, v, v))) |> ungroup()
    n_grp <- length(unique(res[[grp_col]]))
    res$..region_key <- as.character(res[[grp_col]])
    res[[grp_col]] <- truncate_label(res[[grp_col]], 20)
    use_horiz <- n_grp > 10
    p <- if (use_horiz) {
      ggplot(res, aes(y=.data[[grp_col]], x=`Prév. (%)`,
                       fill=Indicateur, key=.data[["..region_key"]],
                       text=paste0(.data[[grp_col]]," — ",Indicateur,
                                   "\n",`Prév. (%)`,"% [",`IC95 bas (%)`,"-",`IC95 haut (%)`,"%]"))) +
        geom_col(position="dodge", width=0.72, alpha=0.88) +
        geom_errorbarh(aes(xmin=`IC95 bas (%)`, xmax=`IC95 haut (%)`),
                       position=position_dodge(0.72), height=0.22, color="gray30", linewidth=0.7) +
        geom_vline(xintercept=15, linetype="dashed", color=C$rouge, linewidth=0.7) +
        scale_fill_manual(values=c(GAM=C$rouge,SAM=C$rouge_f,
                                    Stunting=C$bleu,Underweight=C$violet,
                                    MAM=C$orange)) +
        labs(y="", x="Prévalence (%)") + theme_nutri()
    } else {
      ggplot(res, aes(x=.data[[grp_col]], y=`Prév. (%)`,
                       fill=Indicateur, key=.data[["..region_key"]],
                       text=paste0(.data[[grp_col]]," — ",Indicateur,
                                   "\n",`Prév. (%)`,"% [",`IC95 bas (%)`,"-",`IC95 haut (%)`,"%]"))) +
        geom_col(position="dodge", width=0.72, alpha=0.88) +
        geom_errorbar(aes(ymin=`IC95 bas (%)`, ymax=`IC95 haut (%)`),
                      position=position_dodge(0.72), width=0.22, color="gray30", linewidth=0.7) +
        geom_hline(yintercept=15, linetype="dashed", color=C$rouge, linewidth=0.7) +
        scale_fill_manual(values=c(GAM=C$rouge,SAM=C$rouge_f,
                                    Stunting=C$bleu,Underweight=C$violet,
                                    MAM=C$orange)) +
        labs(x="", y="Prévalence (%)") + theme_nutri() + smart_axis_x(n_grp)
    }
    ggplotly(p, tooltip="text", source="accueil_prev") |>
      event_register("plotly_click") |>
      layout(legend=list(orientation="h", y=-0.25), hovermode="closest") |> nutri_config()
  })

  observeEvent(event_data("plotly_click", source = "accueil_prev"), {
    click <- event_data("plotly_click", source = "accueil_prev")
    key <- click$key
    req(is.character(key), length(key) == 1, nzchar(key))
    if (!is.null(input$f_region)) {
      updateSelectInput(session, "f_region", selected = key)
      showNotification(paste0("Filtre région : ", key), type = "message", duration = 2)
    }
  })

  output$accueil_gauge <- renderPlotly({
    req(has_oms()); d <- filter_context()$data
    gam_p <- round(mean(d$GAM, na.rm=TRUE)*100, 1)
    plot_ly(type="indicator", mode="gauge+number+delta",
            value=gam_p,
            title=list(text="GAM (%)", font=list(size=15, color=C$sidebar)),
            delta=list(reference=15, increasing=list(color=C$rouge),
                       decreasing=list(color=C$vert)),
            gauge=list(
              axis=list(range=list(0,50), tickwidth=1, tickfont=list(size=10)),
              bar=list(color=couleur_prevalence(gam_p)),
              steps=list(
                list(range=c(0,5),  color="#e8f5e9"),
                list(range=c(5,10), color="#fffde7"),
                list(range=c(10,15),color="#fff3e0"),
                list(range=c(15,30),color="#ffebee"),
                list(range=c(30,50),color="#ffcdd2")),
              threshold=list(
                line=list(color=C$rouge,width=3),
                thickness=0.75, value=15)
            )) |>
      layout(margin=list(l=20,r=20,b=10,t=40),
             paper_bgcolor="rgba(0,0,0,0)")
  })

  output$accueil_seuil_badge <- renderUI({
    req(has_oms()); d <- filter_context()$data
    p <- round(mean(d$GAM, na.rm=TRUE)*100, 1)
    cls <- if(p<5)"alerte-ok" else if(p<10)"alerte-alerte" else if(p<15)"alerte-serieux" else "alerte-crit"
    tags$div(class=paste("alerte-box",cls), style="margin:8px;",
             tags$strong(alerte_label(p)), " — GAM : ", p, "%")
  })

  output$accueil_zscore <- renderPlotly({
    d <- filter_context()$data; m <- map_cols()
    z_cols <- c(m$whz, m$haz, m$waz)
    z_cols <- z_cols[!sapply(z_cols,is.null) & z_cols %in% names(d)]
    req(length(z_cols) > 0)
    x_ref <- seq(-4.5, 4.5, length.out=300)
    ref_df <- tibble(x=x_ref, y=dnorm(x_ref), Courbe="Référence OMS N(0,1)")
    df_long <- d |> select(all_of(z_cols)) |>
      pivot_longer(everything(), names_to="Score", values_to="Val")
    noms <- setNames(c("WHZ","HAZ","WAZ","MUAC")[1:length(z_cols)], z_cols)
    df_long$Score <- noms[df_long$Score]
    p <- ggplot(df_long, aes(x=Val, fill=Score, color=Score)) +
      geom_density(alpha=0.3, linewidth=0.8) +
      geom_line(data=ref_df, aes(x=x,y=y), color="black",
                linewidth=1, linetype="dashed", inherit.aes=FALSE) +
      geom_vline(xintercept=c(-2,-3), linetype="dashed",
                 color=c(C$rouge,C$rouge_f), linewidth=0.7) +
      scale_fill_manual(values=gpal(length(unique(df_long$Score)),"cat")) +
      scale_color_manual(values=gpal(length(unique(df_long$Score)),"cat")) +
      labs(x="Score Z", y="Densité",
           caption="Tirets rouges = seuils OMS -2 et -3 DS | Tiret noir = référence OMS N(0,1)") +
      theme_minimal(base_size=11)
    ggplotly(p) |> layout(hovermode="x unified",
                          legend=list(orientation="h", y=-0.2)) |> nutri_config()
  })

  # ── SÉCURITÉ ALIMENTAIRE — rangée conditionnelle rCSI + HDDS ────────────────
  output$accueil_sa_charts_row <- renderUI({
    d <- filter_context()$data; m <- map_cols()
    # Diagnostic SA
    message("[SA_DIAG] nrow=", nrow(d),
            " | .fcs=", ".fcs" %in% names(d),
            " | .rcsi=", ".rcsi" %in% names(d),
            " | .hdds=", ".hdds" %in% names(d),
            " | fcs_cat=", "fcs_cat" %in% names(d),
            " | rcsi_cat=", "rcsi_cat" %in% names(d))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    has_hdds <- ".hdds" %in% names(d) && !all(is.na(d$.hdds))
    if (!has_rcsi && !has_hdds) {
      message("[SA_DIAG] Returning NULL — has_rcsi=", has_rcsi, " has_hdds=", has_hdds)
      return(NULL)
    }

    # ── Construire la liste des variables de regroupement disponibles ──────────
    grp_choices <- c()
    add_grp <- function(col, label) {
      if (!is.null(col) && col %in% names(d) &&
          length(unique(na.omit(d[[col]]))) >= 2 &&
          length(unique(na.omit(d[[col]]))) <= 50)
        grp_choices[label] <<- col
    }
    add_grp(m$region,  paste0("\U0001f5fa\ufe0f  R\u00e9gion  (", m$region, ")"))
    add_grp(m$dept,    paste0("\U0001f4cd  D\u00e9partement  (", m$dept, ")"))
    add_grp(m$survey,  paste0("\U0001f504  Round / Vague  (", m$survey, ")"))
    add_grp(m$sexe,    paste0("\U0001f464  Sexe  (", m$sexe, ")"))
    add_grp(m$depl,    paste0("\U0001f3d5\ufe0f  D\u00e9placement  (", m$depl, ")"))
    add_grp(m$strate,  paste0("\U0001f5c2\ufe0f  Strate  (", m$strate, ")"))
    add_grp(m$cluster, paste0("\U0001f3af  Cluster  (", m$cluster, ")"))
    # Ajouter d'autres catégorielles détectées non encore incluses (max 5 suppl.)
    extra_cats <- setdiff(vars_cat(), unname(grp_choices))
    extra_cats <- extra_cats[!startsWith(extra_cats, ".")]
    for (col in head(extra_cats, 5))
      grp_choices[col] <- col
    if (length(grp_choices) == 0) return(NULL)

    # S\u00e9lection par d\u00e9faut : r\u00e9gion en premier si disponible
    default_grp <- grp_choices[1]
    current_sel <- isolate(input$sa_grp_var)
    sel <- if (!is.null(current_sel) && current_sel %in% grp_choices) current_sel else default_grp

    charts <- list()
    width  <- if (has_rcsi && has_hdds) 6L else 12L
    if (has_rcsi) {
      charts <- c(charts, list(
        column(width,
          tags$div(class="nutri-chart-card",
            tags$div(
              style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:6px; margin-bottom:6px;",
              tags$div(class="chart-title", style="margin-bottom:0;",
                       icon("hand-holding-heart"),
                       " Distribution rCSI \u2014 Strat\u00e9gies d'Adaptation (PAM/WFP)"),
              downloadButton(
                "dl_rcsi_csv",
                label = " T\u00e9l\u00e9charger CSV",
                icon  = icon("download"),
                style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                class = "btn-sm btn-outline-secondary"
              )
            ),
            plotlyOutput("accueil_rcsi_chart", height="320px")
          )
        )
      ))
    }
    if (has_hdds) {
      charts <- c(charts, list(
        column(width,
          tags$div(class="nutri-chart-card",
            tags$div(
              style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:6px; margin-bottom:6px;",
              tags$div(class="chart-title", style="margin-bottom:0;",
                       icon("carrot"),
                       " Distribution HDDS \u2014 Diversit\u00e9 Alimentaire (FAO/FANTA)"),
              downloadButton(
                "dl_hdds_csv",
                label = " T\u00e9l\u00e9charger CSV",
                icon  = icon("download"),
                style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                class = "btn-sm btn-outline-secondary"
              )
            ),
            plotlyOutput("accueil_hdds_chart", height="320px")
          )
        )
      ))
    }

    tagList(
      # ── En-t\u00eate de section avec le s\u00e9lecteur inline ────────────────────────────
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("utensils"),
                  " S\u00e9curit\u00e9 alimentaire \u2014 Distributions par groupe"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; font-size:13px; font-weight:400;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("layer-group"), " Regrouper par"
          ),
          tags$div(
            style = "min-width:260px;",
            selectInput(
              inputId  = "sa_grp_var",
              label    = NULL,
              choices  = grp_choices,
              selected = sel,
              width    = "100%"
            )
          )
        )
      ),
      fluidRow(charts)
    )
  })

  # ── rCSI : distribution des catégories par groupe ───────────────────────────
  output$accueil_rcsi_chart <- renderPlotly({
    d <- df_f(); m <- map_cols()
    req(".rcsi" %in% names(d), "rcsi_cat" %in% names(d))
    req(!is.null(input$sa_grp_var), input$sa_grp_var %in% names(d))
    grp_col <- input$sa_grp_var

    cat_lvls <- c("Pas/Peu de coping", "Coping moyen", "Coping \u00e9lev\u00e9 (\u226519)")
    pal_rcsi  <- c("Pas/Peu de coping" = C$vert,
                   "Coping moyen"      = C$orange,
                   "Coping \u00e9lev\u00e9 (\u226519)" = C$rouge)

    res <- d |>
      filter(!is.na(rcsi_cat), !is.na(.data[[grp_col]])) |>
      mutate(rcsi_cat = factor(rcsi_cat, levels = cat_lvls)) |>
      count(.data[[grp_col]], rcsi_cat, name = "n") |>
      group_by(.data[[grp_col]]) |>
      mutate(pct = round(n / sum(n) * 100, 1),
             total = sum(n)) |>
      ungroup()

    n_grp <- length(unique(res[[grp_col]]))
    # Barres horizontales si trop de groupes
    use_horiz <- n_grp > 8
    res[[grp_col]] <- truncate_label(res[[grp_col]], if (use_horiz) 25 else 18)

    p <- if (use_horiz) {
      ggplot(res,
             aes(y = .data[[grp_col]], x = pct, fill = rcsi_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               rcsi_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        geom_vline(xintercept = 50, linetype = "dashed",
                   color = C$rouge, linewidth = 0.7, alpha = 0.7) +
        scale_fill_manual(values = pal_rcsi, name = "Niveau coping") +
        scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(y = "", x = "% m\u00e9nages",
             caption = "Tiret rouge = 50% \u2014 Score rCSI \u226519 = coping \u00e9lev\u00e9 (PAM)") +
        theme_nutri()
    } else {
      ggplot(res,
             aes(x = .data[[grp_col]], y = pct, fill = rcsi_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               rcsi_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        geom_hline(yintercept = 50, linetype = "dashed",
                   color = C$rouge, linewidth = 0.7, alpha = 0.7) +
        scale_fill_manual(values = pal_rcsi, name = "Niveau coping") +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(x = "", y = "% m\u00e9nages",
             caption = "Tiret rouge = 50% \u2014 Score rCSI \u226519 = coping \u00e9lev\u00e9 (PAM)") +
        theme_nutri() + smart_axis_x(n_grp)
    }

    ggplotly(p, tooltip = "text") |>
      layout(barmode  = "stack",
             legend   = list(orientation = "h", y = -0.28, x = 0.05),
             hovermode = "closest") |>
      nutri_config()
  })

  # ── HDDS : distribution des catégories par groupe ───────────────────────────
  output$accueil_hdds_chart <- renderPlotly({
    d <- df_f(); m <- map_cols()
    req(".hdds" %in% names(d), "hdds_cat" %in% names(d))
    req(!is.null(input$sa_grp_var), input$sa_grp_var %in% names(d))
    grp_col <- input$sa_grp_var

    cat_lvls <- c("Faible diversit\u00e9 (\u22643)",
                  "Diversit\u00e9 moyenne (4-5)",
                  "Bonne diversit\u00e9 (\u22656)")
    pal_hdds <- c("Faible diversit\u00e9 (\u22643)"   = C$rouge,
                  "Diversit\u00e9 moyenne (4-5)" = C$orange,
                  "Bonne diversit\u00e9 (\u22656)"    = C$vert)

    res <- d |>
      filter(!is.na(hdds_cat), !is.na(.data[[grp_col]])) |>
      mutate(hdds_cat = factor(hdds_cat, levels = cat_lvls)) |>
      count(.data[[grp_col]], hdds_cat, name = "n") |>
      group_by(.data[[grp_col]]) |>
      mutate(pct = round(n / sum(n) * 100, 1), total = sum(n)) |>
      ungroup()

    n_grp <- length(unique(res[[grp_col]]))
    use_horiz <- n_grp > 8
    res[[grp_col]] <- truncate_label(res[[grp_col]], if (use_horiz) 25 else 18)

    p <- if (use_horiz) {
      ggplot(res,
             aes(y = .data[[grp_col]], x = pct, fill = hdds_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               hdds_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        scale_fill_manual(values = pal_hdds, name = "Diversit\u00e9 alimentaire") +
        scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(y = "", x = "% m\u00e9nages",
             caption = "HDDS : 12 groupes \u2014 Seuils FAO/FANTA : \u22643 faible, 4-5 moyen, \u22656 bonne") +
        theme_nutri()
    } else {
      ggplot(res,
             aes(x = .data[[grp_col]], y = pct, fill = hdds_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               hdds_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        scale_fill_manual(values = pal_hdds, name = "Diversit\u00e9 alimentaire") +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(x = "", y = "% m\u00e9nages",
             caption = "HDDS : 12 groupes \u2014 Seuils FAO/FANTA : \u22643 faible, 4-5 moyen, \u22656 bonne") +
        theme_nutri() + smart_axis_x(n_grp)
    }

    ggplotly(p, tooltip = "text") |>
      layout(barmode  = "stack",
             legend   = list(orientation = "h", y = -0.28, x = 0.05),
             hovermode = "closest") |>
      nutri_config()
  })


  # ── Export CSV : rCSI distribution par groupe ───────────────────────────────
  output$dl_rcsi_csv <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$sa_grp_var)) input$sa_grp_var else "groupe"
      paste0("rCSI_distribution_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d <- df_f()
      req(".rcsi" %in% names(d), "rcsi_cat" %in% names(d))
      grp_col <- req(input$sa_grp_var)
      cat_lvls <- c("Pas/Peu de coping", "Coping moyen", "Coping \u00e9lev\u00e9 (\u226519)")
      out <- d |>
        filter(!is.na(rcsi_cat), !is.na(.data[[grp_col]])) |>
        mutate(rcsi_cat = factor(rcsi_cat, levels = cat_lvls)) |>
        count(.data[[grp_col]], rcsi_cat, name = "n") |>
        group_by(.data[[grp_col]]) |>
        mutate(
          pct    = round(n / sum(n) * 100, 1),
          total  = sum(n)
        ) |>
        ungroup() |>
        rename(groupe = all_of(grp_col), categorie_rcsi = rcsi_cat,
               effectif = n, pourcentage = pct, total_groupe = total) |>
        arrange(groupe, categorie_rcsi)
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )


  # ── Export CSV : HDDS distribution par groupe ────────────────────────────────
  output$dl_hdds_csv <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$sa_grp_var)) input$sa_grp_var else "groupe"
      paste0("HDDS_distribution_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d <- df_f()
      req(".hdds" %in% names(d), "hdds_cat" %in% names(d))
      grp_col <- req(input$sa_grp_var)
      cat_lvls <- c("Faible diversit\u00e9 (\u22643)",
                    "Diversit\u00e9 moyenne (4-5)",
                    "Bonne diversit\u00e9 (\u22656)")
      out <- d |>
        filter(!is.na(hdds_cat), !is.na(.data[[grp_col]])) |>
        mutate(hdds_cat = factor(hdds_cat, levels = cat_lvls)) |>
        count(.data[[grp_col]], hdds_cat, name = "n") |>
        group_by(.data[[grp_col]]) |>
        mutate(
          pct    = round(n / sum(n) * 100, 1),
          total  = sum(n)
        ) |>
        ungroup() |>
        rename(groupe = all_of(grp_col), categorie_hdds = hdds_cat,
               effectif = n, pourcentage = pct, total_groupe = total) |>
        arrange(groupe, categorie_hdds)
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── FCS × rCSI : UI section (conditionnel) ──────────────────────────────────
  output$accueil_fcs_rcsi_scatter <- renderUI({
    d <- df_f(); m <- map_cols()
    has_fcs  <- ".fcs"  %in% names(d) && !all(is.na(d$.fcs))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    if (!has_fcs || !has_rcsi) {
      message("[SCATTER_DIAG] Returning NULL — has_fcs=", has_fcs, " has_rcsi=", has_rcsi)
      return(NULL)
    }

    # Construire les choix de colorisation
    col_choices <- c("Niveau coping rCSI" = "rcsi_cat")
    if ("fcs_cat" %in% names(d))
      col_choices["Cat\u00e9gorie FCS"] <- "fcs_cat"
    for (nm in c("region", "dept", "sexe", "survey", "depl", "strate")) {
      col <- m[[nm]]
      if (!is.null(col) && col %in% names(d) &&
          length(unique(na.omit(d[[col]]))) >= 2 &&
          length(unique(na.omit(d[[col]]))) <= 30)
        col_choices[paste0("\U0001f5fa\ufe0f  ", col)] <- col
    }

    tagList(
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("project-diagram"),
                  " Coh\u00e9rence FCS \u00d7 rCSI \u2014 Nuage de points individuels"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; font-size:13px; font-weight:400;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("palette"), " Colorer par"
          ),
          tags$div(
            style = "min-width:220px;",
            selectInput("fcs_rcsi_color_var", label = NULL,
                        choices = col_choices, selected = col_choices[1], width = "100%")
          ),
          downloadButton(
            "dl_fcs_rcsi_csv",
            label = " T\u00e9l\u00e9charger CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      fluidRow(
        column(12,
          tags$div(class = "nutri-chart-card",
            plotlyOutput("accueil_fcs_rcsi_plot", height = "440px")
          )
        )
      )
    )
  })

  # ── FCS × rCSI : nuage de points ────────────────────────────────────────────
  output$accueil_fcs_rcsi_plot <- renderPlotly({
    d <- df_f()
    req(".fcs" %in% names(d), ".rcsi" %in% names(d))
    req(!is.null(input$fcs_rcsi_color_var),
        input$fcs_rcsi_color_var %in% names(d))

    color_var <- input$fcs_rcsi_color_var
    pal_rcsi  <- c("Pas/Peu de coping"        = C$vert,
                   "Coping moyen"             = C$orange,
                   "Coping \u00e9lev\u00e9 (\u226519)" = C$rouge)
    pal_fcs   <- c("Pauvre"        = C$rouge,
                   "Limite"        = C$orange,
                   "Acceptable"    = C$vert)

    # Sous-ensemble utilisable
    pd <- d |>
      filter(!is.na(.fcs), !is.na(.rcsi), !is.na(.data[[color_var]])) |>
      mutate(
        .color_lbl = as.character(.data[[color_var]]),
        .tooltip = paste0(
          "FCS : ", round(.fcs, 1), "\n",
          "rCSI : ", round(.rcsi, 1), "\n",
          color_var, " : ", .data[[color_var]]
        )
      )

    req(nrow(pd) >= 5)

    # Corrélation de Pearson
    cor_val <- cor(pd$.fcs, pd$.rcsi, use = "complete.obs")
    cor_lbl <- paste0("r = ", round(cor_val, 2),
                      " (", if (cor_val < 0) "relation attendue \u2212" else "relation inattendue +", ")")

    # Palette dynamique selon variable
    use_fixed_pal <- color_var %in% c("rcsi_cat", "fcs_cat")
    fixed_pal <- if (color_var == "rcsi_cat") pal_rcsi else pal_fcs

    p <- ggplot(pd, aes(x = .fcs, y = .rcsi, color = .color_lbl, text = .tooltip)) +
      # Zones FCS en arrière-plan
      annotate("rect", xmin = -Inf, xmax = 21,   ymin = -Inf, ymax = Inf,
               fill = C$rouge,  alpha = 0.04) +
      annotate("rect", xmin = 21,   xmax = 35,   ymin = -Inf, ymax = Inf,
               fill = C$orange, alpha = 0.04) +
      annotate("rect", xmin = 35,   xmax = Inf,  ymin = -Inf, ymax = Inf,
               fill = C$vert,   alpha = 0.04) +
      # Seuils de référence
      geom_vline(xintercept = 21, linetype = "dashed",
                 color = C$rouge,  linewidth = 0.65, alpha = 0.6) +
      geom_vline(xintercept = 35, linetype = "dashed",
                 color = C$orange, linewidth = 0.65, alpha = 0.6) +
      geom_hline(yintercept = 19, linetype = "dashed",
                 color = C$rouge,  linewidth = 0.65, alpha = 0.6) +
      # Points
      geom_point(size = 2, alpha = 0.6, shape = 16) +
      # Ligne de tendance (lm simple)
      geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                  color = "#334155", fill = "#94a3b8",
                  linewidth = 0.8, alpha = 0.15,
                  show.legend = FALSE) +
      # Annotations seuils
      annotate("text", x = 10,  y = max(pd$.rcsi, na.rm=TRUE) * 0.97,
               label = "FCS pauvre", color = C$rouge, size = 3, hjust = 0.5, fontface = "bold") +
      annotate("text", x = 28,  y = max(pd$.rcsi, na.rm=TRUE) * 0.97,
               label = "FCS limite", color = C$orange, size = 3, hjust = 0.5, fontface = "bold") +
      annotate("text", x = 60,  y = max(pd$.rcsi, na.rm=TRUE) * 0.97,
               label = "FCS acceptable", color = C$vert, size = 3, hjust = 0.5, fontface = "bold") +
      annotate("text", x = max(pd$.fcs, na.rm=TRUE) * 0.97, y = 20.5,
               label = "rCSI \u226519", color = C$rouge, size = 2.8, hjust = 1) +
      labs(
        x       = "Score FCS (Food Consumption Score)",
        y       = "Score rCSI (Reduced Coping Strategy Index)",
        color   = color_var,
        caption = paste0(cor_lbl,
                         "  \u2014  Tirets : seuils FCS (21/35) et rCSI (19)  \u2014  Bande grise : IC 95% r\u00e9gression")
      ) +
      theme_nutri() +
      theme(legend.position = "bottom",
            legend.title     = element_text(size = 10, face = "bold"),
            legend.text      = element_text(size = 9))

    # Palette
    if (use_fixed_pal) {
      p <- p + scale_color_manual(values = fixed_pal)
    } else {
      n_col <- length(unique(pd$.color_lbl))
      p <- p + scale_color_manual(
        values = setNames(
          colorRampPalette(c("#0077B6", "#00838F", "#D98B14",
                             "#BF3636", "#6B3FA0", "#0B3954"))(n_col),
          unique(pd$.color_lbl)
        )
      )
    }

    ggplotly(p, tooltip = "text") |>
      layout(
        legend     = list(orientation = "h", y = -0.22, x = 0),
        hovermode  = "closest",
        annotations = list(list(
          x = 0.01, y = 0.99, xref = "paper", yref = "paper",
          text = paste0("<b>", cor_lbl, "</b>"),
          showarrow = FALSE,
          font = list(size = 12, color = "#334155"),
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#cbd5e1",
          borderwidth = 1,
          borderpad   = 4
        ))
      ) |>
      nutri_config()
  })


  # ── Export CSV : FCS × rCSI données individuelles ────────────────────────────
  output$dl_fcs_rcsi_csv <- downloadHandler(
    filename = function() {
      paste0("FCS_rCSI_coherence_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d <- df_f(); m <- map_cols()
      req(".fcs" %in% names(d), ".rcsi" %in% names(d))
      # Colonnes à exporter : scores + catégories + variables de groupe
      keep_cols <- intersect(
        c(".fcs", ".rcsi", "fcs_cat", "rcsi_cat",
          m$region, m$dept, m$sexe, m$survey, m$cluster, m$strate),
        names(d)
      )
      out <- d[keep_cols] |>
        filter(!is.na(.fcs), !is.na(.rcsi)) |>
        rename(fcs_score = .fcs, rcsi_score = .rcsi)
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Analyse temporelle : helper tri intelligent des rounds ──────────────────
  sort_rounds <- function(rounds) {
    nums <- suppressWarnings(as.numeric(gsub("[^0-9\\.]+", "", rounds)))
    if (all(!is.na(nums))) rounds[order(nums)] else sort(unique(rounds))
  }

  # ── Analyse temporelle : UI section (conditionnel) ──────────────────────────
  output$accueil_temporal_row <- renderUI({
    d  <- mapped_data(); req(d)
    m  <- map_cols()
    sv_col <- m$survey
    req(!is.null(sv_col), sv_col %in% names(d))

    rounds <- unique(na.omit(as.character(d[[sv_col]])))
    if (length(rounds) < 2) return(NULL)

    has_fcs  <- ".fcs"  %in% names(d) && !all(is.na(d$.fcs))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    if (!has_fcs && !has_rcsi) return(NULL)

    # Choix de ventilation par groupe
    grp_choices <- c("Tous les m\u00e9nages" = "__global__")
    add_g <- function(col, lbl) {
      if (!is.null(col) && col %in% names(d) &&
          dplyr::n_distinct(na.omit(d[[col]])) >= 2 &&
          dplyr::n_distinct(na.omit(d[[col]])) <= 20)
        grp_choices[lbl] <<- col
    }
    add_g(m$region, paste0("\U0001f5fa\ufe0f  R\u00e9gion  (", m$region, ")"))
    add_g(m$dept,   paste0("\U0001f4cd  D\u00e9partement  (", m$dept, ")"))
    add_g(m$sexe,   paste0("\U0001f464  Sexe  (", m$sexe, ")"))
    add_g(m$depl,   paste0("\U0001f3d5\ufe0f  D\u00e9placement  (", m$depl, ")"))
    add_g(m$strate, paste0("\U0001f5c2\ufe0f  Strate  (", m$strate, ")"))

    current_sel <- isolate(input$temporal_grp_var)
    sel <- if (!is.null(current_sel) && current_sel %in% grp_choices) current_sel else "__global__"

    tagList(
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("chart-line"),
                  " \u00c9volution temporelle \u2014 FCS & rCSI par round de collecte"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; font-size:13px; font-weight:400; flex-wrap:wrap;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("layer-group"), " Ventiler par"
          ),
          tags$div(style = "min-width:240px;",
            selectInput("temporal_grp_var", label = NULL,
                        choices = grp_choices, selected = sel, width = "100%")
          ),
          downloadButton(
            "dl_temporal_csv",
            label = " T\u00e9l\u00e9charger CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      tags$p(
        style = "font-size:11px; color:#94a3b8; margin:-8px 0 12px 2px;",
        icon("info-circle"),
        " Donn\u00e9es non filtr\u00e9es par round \u2014 tous les rounds disponibles sont affich\u00e9s.",
        " Moyenne \u00b1 IC 95% (m\u00e9thode normale). ",
        tags$b(paste0(length(rounds), " rounds d\u00e9tect\u00e9s : ",
                      paste(sort_rounds(rounds), collapse = " \u2192 ")))
      ),
      fluidRow(
        if (has_fcs)
          column(if (has_rcsi) 6L else 12L,
            tags$div(class = "nutri-chart-card",
              tags$div(class = "chart-title",
                       icon("utensils"), " Tendance FCS \u2014 Score moyen par round"),
              plotlyOutput("accueil_fcs_temporal", height = "360px")
            )
          ),
        if (has_rcsi)
          column(if (has_fcs) 6L else 12L,
            tags$div(class = "nutri-chart-card",
              tags$div(class = "chart-title",
                       icon("hand-holding-heart"), " Tendance rCSI \u2014 Score moyen par round"),
              plotlyOutput("accueil_rcsi_temporal", height = "360px")
            )
          )
      )
    )
  })

  # ── Helper : agréger un score par round (et optionnel groupe) ────────────────
  temporal_agg <- function(d, sv_col, score_col, grp_col = NULL) {
    grp_vars <- c(sv_col, if (!is.null(grp_col)) grp_col)
    res <- d |>
      filter(!is.na(.data[[score_col]]), !is.na(.data[[sv_col]])) |>
      mutate(.round_chr = as.character(.data[[sv_col]])) |>
      group_by(across(all_of(c(".round_chr", if (!is.null(grp_col)) grp_col)))) |>
      summarise(
        mean_val = mean(.data[[score_col]], na.rm = TRUE),
        sd_val   = sd(.data[[score_col]],   na.rm = TRUE),
        n        = sum(!is.na(.data[[score_col]])),
        .groups  = "drop"
      ) |>
      mutate(
        ci_lo = mean_val - 1.96 * sd_val / sqrt(pmax(n, 1)),
        ci_hi = mean_val + 1.96 * sd_val / sqrt(pmax(n, 1))
      )
    # Tri intelligent des rounds
    ordered_rounds <- sort_rounds(unique(res$.round_chr))
    res$.round_chr <- factor(res$.round_chr, levels = ordered_rounds)
    res
  }

  # ── Palette pour lignes multiples ────────────────────────────────────────────
  temporal_pal <- function(n) {
    if (n == 1) return(c("#0077B6"))
    colorRampPalette(c("#0077B6", "#00838F", "#D98B14",
                       "#BF3636", "#6B3FA0", "#0B3954", "#D4A843"))(n)
  }

  # ── Analyse temporelle FCS ────────────────────────────────────────────────────
  output$accueil_fcs_temporal <- renderPlotly({
    d  <- mapped_data(); req(d); m <- map_cols()
    sv_col <- m$survey
    req(!is.null(sv_col), sv_col %in% names(d), ".fcs" %in% names(d))

    grp_col <- if (!is.null(input$temporal_grp_var) &&
                   input$temporal_grp_var != "__global__" &&
                   input$temporal_grp_var %in% names(d))
      input$temporal_grp_var else NULL

    res <- temporal_agg(d, sv_col, ".fcs", grp_col)
    req(nrow(res) >= 1)

    n_grp  <- if (!is.null(grp_col)) length(unique(res[[grp_col]])) else 1
    pal    <- setNames(temporal_pal(n_grp),
                       if (!is.null(grp_col)) unique(as.character(res[[grp_col]])) else "global")
    grp_aes <- if (!is.null(grp_col)) grp_col else NULL

    tooltip_txt <- if (!is.null(grp_col)) {
      paste0(res[[grp_col]], " — ", res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    } else {
      paste0(res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    }
    res$.tooltip <- tooltip_txt

    p <- ggplot(res, aes(x = .round_chr, y = mean_val,
                          group = if (!is.null(grp_col)) .data[[grp_col]] else 1,
                          color = if (!is.null(grp_col)) .data[[grp_col]] else "FCS moyen",
                          text  = .tooltip)) +
      # Zones de sévérité FCS
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 21,
               fill = C$rouge,  alpha = 0.06) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 21,  ymax = 35,
               fill = C$orange, alpha = 0.06) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 35,  ymax = Inf,
               fill = C$vert,   alpha = 0.04) +
      geom_hline(yintercept = 21, linetype = "dashed",
                 color = C$rouge,  linewidth = 0.6, alpha = 0.6) +
      geom_hline(yintercept = 35, linetype = "dashed",
                 color = C$orange, linewidth = 0.6, alpha = 0.6) +
      # IC 95% en ruban
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi,
                      fill = if (!is.null(grp_col)) .data[[grp_col]] else "FCS moyen"),
                  alpha = 0.12, color = NA, show.legend = FALSE) +
      # Ligne de tendance + points
      geom_line(linewidth = 1.1) +
      geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
      scale_color_manual(values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      scale_fill_manual( values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      annotate("text", x = Inf, y = 10,  hjust = 1.05, size = 2.8,
               label = "Pauvre (<21)",    color = C$rouge,  fontface = "italic") +
      annotate("text", x = Inf, y = 28,  hjust = 1.05, size = 2.8,
               label = "Limite (21-35)",  color = C$orange, fontface = "italic") +
      annotate("text", x = Inf, y = 38,  hjust = 1.05, size = 2.8,
               label = "Acceptable (>35)",color = C$vert,   fontface = "italic") +
      labs(x = "Round de collecte", y = "FCS moyen (\u00b1 IC 95%)",
           caption = "Tirets : seuils FCS OMS/WFP \u2014 Ruban : intervalle de confiance 95%") +
      theme_nutri() +
      theme(legend.position = if (!is.null(grp_col)) "bottom" else "none",
            axis.text.x = element_text(angle = if (length(levels(res$.round_chr)) > 5) 30 else 0,
                                       hjust = 1))

    ggplotly(p, tooltip = "text") |>
      layout(legend    = list(orientation = "h", y = -0.25, x = 0),
             hovermode = "closest") |>
      nutri_config()
  })

  # ── Analyse temporelle rCSI ───────────────────────────────────────────────────
  output$accueil_rcsi_temporal <- renderPlotly({
    d  <- mapped_data(); req(d); m <- map_cols()
    sv_col <- m$survey
    req(!is.null(sv_col), sv_col %in% names(d), ".rcsi" %in% names(d))

    grp_col <- if (!is.null(input$temporal_grp_var) &&
                   input$temporal_grp_var != "__global__" &&
                   input$temporal_grp_var %in% names(d))
      input$temporal_grp_var else NULL

    res <- temporal_agg(d, sv_col, ".rcsi", grp_col)
    req(nrow(res) >= 1)

    n_grp <- if (!is.null(grp_col)) length(unique(res[[grp_col]])) else 1
    pal   <- setNames(temporal_pal(n_grp),
                      if (!is.null(grp_col)) unique(as.character(res[[grp_col]])) else "global")

    tooltip_txt <- if (!is.null(grp_col)) {
      paste0(res[[grp_col]], " — ", res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    } else {
      paste0(res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    }
    res$.tooltip <- tooltip_txt

    p <- ggplot(res, aes(x = .round_chr, y = mean_val,
                          group = if (!is.null(grp_col)) .data[[grp_col]] else 1,
                          color = if (!is.null(grp_col)) .data[[grp_col]] else "rCSI moyen",
                          text  = .tooltip)) +
      # Zone coping élevé
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 19, ymax = Inf,
               fill = C$rouge, alpha = 0.06) +
      geom_hline(yintercept = 19, linetype = "dashed",
                 color = C$rouge, linewidth = 0.6, alpha = 0.6) +
      # IC 95%
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi,
                      fill = if (!is.null(grp_col)) .data[[grp_col]] else "rCSI moyen"),
                  alpha = 0.12, color = NA, show.legend = FALSE) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
      scale_color_manual(values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      scale_fill_manual( values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      annotate("text", x = Inf, y = 20.5, hjust = 1.05, size = 2.8,
               label = "Coping \u00e9lev\u00e9 (\u226519)", color = C$rouge, fontface = "italic") +
      labs(x = "Round de collecte", y = "rCSI moyen (\u00b1 IC 95%)",
           caption = "Tiret rouge : seuil PAM/WFP rCSI \u226519 \u2014 Ruban : intervalle de confiance 95%") +
      theme_nutri() +
      theme(legend.position = if (!is.null(grp_col)) "bottom" else "none",
            axis.text.x = element_text(angle = if (length(levels(res$.round_chr)) > 5) 30 else 0,
                                       hjust = 1))

    ggplotly(p, tooltip = "text") |>
      layout(legend    = list(orientation = "h", y = -0.25, x = 0),
             hovermode = "closest") |>
      nutri_config()
  })


  # ── Export CSV : tendances agrégées FCS & rCSI ───────────────────────────────
  output$dl_temporal_csv <- downloadHandler(
    filename = function() {
      paste0("tendance_FCS_rCSI_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d  <- mapped_data(); req(d); m <- map_cols()
      sv_col  <- req(m$survey)
      grp_col <- if (!is.null(input$temporal_grp_var) &&
                     input$temporal_grp_var != "__global__" &&
                     input$temporal_grp_var %in% names(d))
        input$temporal_grp_var else NULL

      rows <- list()
      if (".fcs" %in% names(d)) {
        tmp <- temporal_agg(d, sv_col, ".fcs", grp_col) |>
          mutate(indicateur = "FCS",
                 round = as.character(.round_chr)) |>
          select(indicateur, round,
                 any_of(grp_col),
                 moyenne = mean_val,
                 ic_bas  = ci_lo,
                 ic_haut = ci_hi,
                 n)
        rows <- c(rows, list(tmp))
      }
      if (".rcsi" %in% names(d)) {
        tmp <- temporal_agg(d, sv_col, ".rcsi", grp_col) |>
          mutate(indicateur = "rCSI",
                 round = as.character(.round_chr)) |>
          select(indicateur, round,
                 any_of(grp_col),
                 moyenne = mean_val,
                 ic_bas  = ci_lo,
                 ic_haut = ci_hi,
                 n)
        rows <- c(rows, list(tmp))
      }
      req(length(rows) > 0)
      out <- bind_rows(rows) |>
        mutate(across(where(is.numeric), ~round(.x, 2)))
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Tableau croisé rCSI × FCS — helper ────────────────────────────────────
  build_crosstab <- function(d, grp_col = NULL) {
    rcsi_lvls <- c("Pas/Peu de coping", "Coping moyen", "Coping \u00e9lev\u00e9 (\u226519)")
    fcs_lvls  <- c("Pauvre (<21)", "Limite (21-35)", "Acceptable (>35)")

    d2 <- d |>
      filter(!is.na(rcsi_cat), !is.na(fcs_cat)) |>
      mutate(
        rcsi_cat = factor(as.character(rcsi_cat), levels = rcsi_lvls),
        fcs_cat  = factor(as.character(fcs_cat),  levels = fcs_lvls),
        .grp     = if (!is.null(grp_col))
          as.character(.data[[grp_col]]) else "Ensemble"
      ) |>
      filter(!is.na(.grp))

    if (nrow(d2) == 0) return(NULL)

    grp_tot <- count(d2, .grp, name = "N_grp")

    # Comptage complet (toutes combinaisons)
    ct <- d2 |>
      count(.grp, rcsi_cat, fcs_cat, name = "n") |>
      tidyr::complete(.grp, rcsi_cat, fcs_cat, fill = list(n = 0)) |>
      left_join(grp_tot, by = ".grp")

    # Lignes de détail : pivot large par fcs_cat
    detail <- ct |>
      pivot_wider(id_cols = c(.grp, rcsi_cat, N_grp),
                  names_from = fcs_cat, values_from = n, values_fill = 0) |>
      rename(n_P = `Pauvre (<21)`, n_L = `Limite (21-35)`, n_A = `Acceptable (>35)`) |>
      mutate(
        Total_n     = n_P + n_L + n_A,
        is_subtotal = 0L,
        is_dblvuln  = if_else(as.character(rcsi_cat) == "Coping \u00e9lev\u00e9 (\u226519)", 1L, 0L),
        sort_order  = case_when(
          rcsi_cat == "Pas/Peu de coping"          ~ 1L,
          rcsi_cat == "Coping moyen"               ~ 2L,
          rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" ~ 3L,
          TRUE ~ 4L
        )
      )

    # Lignes de sous-total par groupe
    sub <- ct |>
      group_by(.grp, N_grp, fcs_cat) |>
      summarise(n = sum(n), .groups = "drop") |>
      pivot_wider(id_cols = c(.grp, N_grp),
                  names_from = fcs_cat, values_from = n, values_fill = 0) |>
      rename(n_P = `Pauvre (<21)`, n_L = `Limite (21-35)`, n_A = `Acceptable (>35)`) |>
      mutate(
        rcsi_cat    = "— Sous-total",
        Total_n     = N_grp,
        is_subtotal = 1L,
        is_dblvuln  = 0L,
        sort_order  = 5L
      )

    # Ligne grand total toutes régions
    grand <- tibble(
      .grp        = "\u25ba TOTAL G\u00c9N\u00c9RAL",
      rcsi_cat    = "Tous",
      N_grp       = nrow(d2),
      n_P         = as.integer(sum(d2$fcs_cat == "Pauvre (<21)",      na.rm = TRUE)),
      n_L         = as.integer(sum(d2$fcs_cat == "Limite (21-35)",    na.rm = TRUE)),
      n_A         = as.integer(sum(d2$fcs_cat == "Acceptable (>35)",  na.rm = TRUE)),
      Total_n     = nrow(d2),
      is_subtotal = 2L,
      is_dblvuln  = 0L,
      sort_order  = 6L
    )

    bind_rows(detail, sub, grand) |>
      arrange(.grp == "\u25ba TOTAL G\u00c9N\u00c9RAL", .grp, sort_order) |>
      mutate(
        pct_P = round(n_P      / N_grp * 100, 1),
        pct_L = round(n_L      / N_grp * 100, 1),
        pct_A = round(n_A      / N_grp * 100, 1),
        pct_T = round(Total_n  / N_grp * 100, 1)
      )
  }

  # Reactive
  crosstab_data <- reactive({
    d <- df_f()
    req("rcsi_cat" %in% names(d), "fcs_cat" %in% names(d))
    grp_col <- if (!is.null(input$crosstab_grp_var) &&
                   input$crosstab_grp_var != "__none__" &&
                   input$crosstab_grp_var %in% names(d))
      input$crosstab_grp_var else NULL
    build_crosstab(d, grp_col)
  })

  # ── Tableau croisé rCSI × FCS — UI section ────────────────────────────────
  output$accueil_crosstab_row <- renderUI({
    d <- df_f(); m <- map_cols()
    if (!("rcsi_cat" %in% names(d)) || !("fcs_cat" %in% names(d))) return(NULL)

    # Choix de ventilation
    grp_choices <- c("Aucun regroupement" = "__none__")
    add_g <- function(col, lbl) {
      if (!is.null(col) && col %in% names(d) &&
          dplyr::n_distinct(na.omit(d[[col]])) >= 2 &&
          dplyr::n_distinct(na.omit(d[[col]])) <= 30)
        grp_choices[lbl] <<- col
    }
    add_g(m$region, paste0("\U0001f5fa\ufe0f  R\u00e9gion  (", m$region, ")"))
    add_g(m$dept,   paste0("\U0001f4cd  D\u00e9partement  (", m$dept, ")"))
    add_g(m$sexe,   paste0("\U0001f464  Sexe  (", m$sexe, ")"))
    add_g(m$depl,   paste0("\U0001f3d5\ufe0f  D\u00e9placement  (", m$depl, ")"))
    add_g(m$strate, paste0("\U0001f5c2\ufe0f  Strate  (", m$strate, ")"))

    default_sel <- if (!is.null(m$region) && m$region %in% names(d)) m$region else "__none__"
    current_sel <- isolate(input$crosstab_grp_var)
    sel <- if (!is.null(current_sel) && current_sel %in% grp_choices) current_sel else default_sel

    # KPI double vulnérabilité (pré-calculé)
    n_dv  <- sum(!is.na(d$rcsi_cat) & !is.na(d$fcs_cat) &
                   d$rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" &
                   d$fcs_cat  == "Pauvre (<21)", na.rm = TRUE)
    n_tot <- sum(!is.na(d$rcsi_cat) & !is.na(d$fcs_cat))
    pct_dv <- if (n_tot > 0) round(n_dv / n_tot * 100, 1) else 0

    tagList(
      # ── En-tête ─────────────────────────────────────────────────────────────
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("th"), " Tableau crois\u00e9 rCSI \u00d7 FCS \u2014 Double vuln\u00e9rabilit\u00e9 alimentaire"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap; font-size:13px; font-weight:400;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("layer-group"), " Ventiler par"
          ),
          tags$div(style = "min-width:240px;",
            selectInput("crosstab_grp_var", label = NULL,
                        choices = grp_choices, selected = sel, width = "100%")
          ),
          downloadButton("dl_crosstab_csv",
            label = " CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          ),
          downloadButton("dl_crosstab_xlsx",
            label = " Excel",
            icon  = icon("file-excel"),
            style = paste0("font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                           "color:white !important; background:#00838F; border-color:#00838F;"),
            class = "btn-sm"
          )
        )
      ),
      # ── KPI double vulnérabilité ─────────────────────────────────────────────
      fluidRow(
        column(12,
          tags$div(
            style = paste0(
              "background:linear-gradient(135deg,#fff0f0 0%,#ffe4e4 100%);",
              "border-left:5px solid #BF3636; border-radius:12px;",
              "padding:16px 22px; margin-bottom:16px;",
              "display:flex; align-items:center; gap:20px; flex-wrap:wrap;"
            ),
            tags$div(
              style = paste0(
                "width:52px; height:52px; border-radius:12px; background:#BF3636;",
                "display:flex; align-items:center; justify-content:center;",
                "font-size:20px; color:white; flex-shrink:0;"
              ),
              icon("exclamation-triangle")
            ),
            tags$div(
              tags$div(
                style = "font-size:11px; font-weight:700; text-transform:uppercase; letter-spacing:1px; color:#991b1b; margin-bottom:4px;",
                "\u26a0\ufe0f  M\u00e9nages en double vuln\u00e9rabilit\u00e9 alimentaire"
              ),
              tags$div(
                tags$span(style="font-size:32px; font-weight:800; color:#BF3636; line-height:1;",
                          n_dv),
                tags$span(style="font-size:15px; font-weight:400; color:#666; margin-left:8px;",
                          paste0("m\u00e9nages \u2014 ", pct_dv, "% des observations valid\u00e9es"))
              ),
              tags$div(
                style = "font-size:12px; color:#555; margin-top:5px;",
                tags$b("Crit\u00e8re :"), " FCS < 21 (Pauvre)",
                tags$span(style="font-weight:700; color:#BF3636; margin:0 4px;", "ET"),
                "rCSI \u2265 19 (Coping \u00e9lev\u00e9) \u2014 Double crise alimentaire (WFP / FAO)"
              )
            )
          )
        )
      ),
      # ── Test du Chi² ────────────────────────────────────────────────────────
      {
        d_v    <- d[!is.na(d$rcsi_cat) & !is.na(d$fcs_cat), ]
        ct_mat <- table(d_v$rcsi_cat, d_v$fcs_cat)
        chi2 <- tryCatch(chisq.test(ct_mat, correct = FALSE), error = function(e) NULL)

        if (!is.null(chi2)) {
          n_obs    <- sum(ct_mat)
          k_min    <- min(nrow(ct_mat), ncol(ct_mat))
          cramer_v <- if (k_min > 1 && n_obs > 0)
            as.numeric(sqrt(chi2$statistic / (n_obs * (k_min - 1)))) else 0
          min_exp  <- tryCatch(round(min(chi2$expected), 2), error = function(e) NA_real_)
          low_exp  <- isTRUE(min_exp < 5)

          # ── Test exact de Fisher (Monte Carlo) si fréquences attendues < 5 ──
          fisher_res <- if (low_exp) {
            tryCatch(
              fisher.test(ct_mat, simulate.p.value = TRUE, B = 10000),
              error = function(e) NULL
            )
          } else NULL
          use_fisher <- !is.null(fisher_res)

          # p-valeur de référence : Fisher si disponible, Chi² sinon
          # Protégé contre NA (chi2 peut donner p.value = NA si table dégénérée)
          pval      <- if (use_fisher) fisher_res$p.value else chi2$p.value
          if (is.null(pval) || !is.finite(pval)) pval <- NA_real_

          pval_txt  <- if (isTRUE(pval < 0.001)) "< 0,001" else
                       if (isTRUE(pval < 0.01))  paste0("= ", round(pval, 4)) else
                       if (!is.na(pval)) paste0("= ", round(pval, 3)) else "N/A"
          chi2_p    <- chi2$p.value
          chi2_ptxt <- if (isTRUE(chi2_p < 0.001)) "< 0,001" else
                       if (!is.na(chi2_p)) paste0(round(chi2_p, 4)) else "N/A"

          sig_lbl <- if (isTRUE(pval < 0.001)) "Hautement significatif (***)" else
                     if (isTRUE(pval < 0.01))  "Tr\u00e8s significatif (**)" else
                     if (isTRUE(pval < 0.05))  "Significatif (*)" else
                     "Non significatif (p \u2265 0,05)"
          sig_col <- if (isTRUE(pval < 0.05)) C$rouge else "#64748b"
          sig_bg  <- if (isTRUE(pval < 0.001)) "#fff0f0" else
                     if (isTRUE(pval < 0.05))  "#fff8f0" else "#f1f5f9"

          if (is.na(cramer_v) || !is.finite(cramer_v)) cramer_v <- 0
          v_lbl <- if (cramer_v < 0.10) "N\u00e9gligeable (V < 0,10)" else
                   if (cramer_v < 0.20) "Faible (0,10 \u2264 V < 0,20)" else
                   if (cramer_v < 0.30) "Mod\u00e9r\u00e9e (0,20 \u2264 V < 0,30)" else
                   "Forte (V \u2265 0,30)"
          v_col <- if (cramer_v < 0.10) "#64748b" else
                   if (cramer_v < 0.20) C$orange else C$rouge

          concordant <- if (use_fisher)
            isTRUE(chi2_p < 0.05) == isTRUE(fisher_res$p.value < 0.05) else TRUE

          test_titre <- if (use_fisher)
            "Test exact de Fisher (Monte Carlo) \u2014 rCSI \u00d7 FCS"
          else
            "Test du \u03c7\u00b2 d'ind\u00e9pendance \u2014 rCSI \u00d7 FCS"
          test_icone <- if (use_fisher) "microscope" else "flask"
          pval_label <- if (use_fisher) "p-valeur (Fisher MC)" else "p-valeur (\u03c7\u00b2)"

          chi2_row <- fluidRow(
            column(12,
              tags$div(
                style = paste0(
                  "background:", sig_bg, "; border-left:5px solid ", sig_col, ";",
                  "border-radius:12px; padding:18px 22px; margin-bottom:16px;"
                ),
                # ── Titre + badge ───────────────────────────────────────────
                tags$div(
                  style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:8px; margin-bottom:14px;",
                  tags$div(
                    style = "font-size:14px; font-weight:700; color:#1e293b;",
                    icon(test_icone), paste0(" ", test_titre)
                  ),
                  tags$div(
                    style = paste0("background:", sig_col, "; color:white; padding:3px 12px;",
                                   "border-radius:20px; font-size:11px; font-weight:700;",
                                   "text-transform:uppercase; letter-spacing:.5px;"),
                    sig_lbl
                  )
                ),
                # ── 4 métriques ─────────────────────────────────────────────
                tags$div(
                  style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(160px,1fr)); gap:14px; margin-bottom:14px;",
                  # Chi² (grisé/pointillé si Fisher prend la relève)
                  tags$div(
                    style = paste0(
                      "background:white; border-radius:8px; padding:12px 14px; text-align:center;",
                      "border:1px ", if (use_fisher) "dashed #d97706" else "solid #e2e8f0", ";",
                      if (use_fisher) "opacity:0.78;" else ""
                    ),
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      if (use_fisher) "\u03c7\u00b2 (approximatif)" else "Statistique \u03c7\u00b2"
                    ),
                    tags$div(style = "font-size:26px;font-weight:800;color:#1e293b;line-height:1;",
                             round(chi2$statistic, 2)),
                    tags$div(style = "font-size:11px;color:#94a3b8;margin-top:2px;",
                             paste0("ddl = ", chi2$parameter)),
                    if (use_fisher)
                      tags$div(
                        style = "font-size:10px;color:#d97706;font-weight:600;margin-top:3px;",
                        "\u26a0\ufe0f Fr\u00e9q. < 5 \u2014 voir Fisher"
                      )
                  ),
                  # p-valeur de référence
                  tags$div(
                    style = paste0(
                      "background:white; border-radius:8px; padding:12px 14px;",
                      "border:1px solid #e2e8f0; text-align:center;",
                      if (isTRUE(pval < 0.05)) "border-color:#BF3636; box-shadow:0 0 0 2px rgba(201,22,29,.1);" else ""
                    ),
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      pval_label
                    ),
                    tags$div(
                      style = paste0("font-size:22px;font-weight:800;line-height:1;color:",
                                     if (isTRUE(pval < 0.05)) sig_col else "#1e293b", ";"),
                      pval_txt
                    ),
                    tags$div(style = "font-size:11px;color:#94a3b8;margin-top:2px;",
                             paste0("n\u00a0=\u00a0", format(n_obs, big.mark = "\u00a0")))
                  ),
                  # V de Cramér
                  tags$div(
                    style = "background:white; border-radius:8px; padding:12px 14px; border:1px solid #e2e8f0; text-align:center;",
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      "V de Cram\u00e9r"
                    ),
                    tags$div(style = paste0("font-size:26px;font-weight:800;line-height:1;color:", v_col, ";"),
                             round(cramer_v, 3)),
                    tags$div(style = paste0("font-size:11px;color:", v_col, ";font-weight:600;margin-top:2px;"),
                             v_lbl)
                  ),
                  # Fréquences attendues min.
                  tags$div(
                    style = paste0(
                      "background:white; border-radius:8px; padding:12px 14px; text-align:center;",
                      "border:1px solid ", if (low_exp) "#d97706" else "#e2e8f0", ";",
                      if (low_exp) "box-shadow:0 0 0 2px rgba(217,119,6,.12);" else ""
                    ),
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      "Fr\u00e9q. attendue min."
                    ),
                    tags$div(
                      style = paste0("font-size:26px;font-weight:800;line-height:1;color:",
                                     if (low_exp) C$orange else C$vert, ";"),
                      min_exp
                    ),
                    tags$div(
                      style = paste0("font-size:11px;font-weight:600;color:",
                                     if (low_exp) C$orange else "#64748b", ";margin-top:2px;"),
                      if (low_exp) "\u26a0\ufe0f Hypoth\u00e8se \u00e0 v\u00e9rifier"
                      else         "\u2713 Hypoth\u00e8se v\u00e9rifi\u00e9e"
                    )
                  )
                ),
                # ── Bande comparaison Chi² ↔ Fisher ─────────────────────────
                if (use_fisher)
                  tags$div(
                    style = paste0(
                      "background:linear-gradient(135deg,#fffbeb,#fff7ed);",
                      "border:1px solid #f59e0b; border-radius:8px;",
                      "padding:10px 16px; margin-bottom:14px;",
                      "display:flex; align-items:center; flex-wrap:wrap; gap:12px;"
                    ),
                    tags$div(
                      style = "font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:.6px;color:#92400e;flex-shrink:0;",
                      icon("balance-scale"), " Comparaison"
                    ),
                    # Chi² (approximatif)
                    tags$div(
                      style = paste0(
                        "background:white; border-radius:6px; padding:6px 12px;",
                        "border:1px dashed #d97706; font-size:12px; white-space:nowrap;"
                      ),
                      tags$span(style = "color:#64748b; font-weight:600;", "\u03c7\u00b2 : "),
                      tags$span(
                        style = paste0("font-weight:800; color:",
                                       if (isTRUE(chi2$p.value < 0.05)) C$rouge else "#475569", ";"),
                        paste0("p\u00a0=\u00a0", chi2_ptxt)
                      ),
                      tags$span(style = "font-size:10px;color:#94a3b8;margin-left:4px;",
                                "(approximatif)")
                    ),
                    tags$div(
                      style = "font-size:18px; color:#d97706; flex-shrink:0;",
                      if (isTRUE(concordant)) "\u21d4\u00a0\u2713" else "\u21d4\u00a0!"
                    ),
                    # Fisher (référence)
                    tags$div(
                      style = paste0(
                        "background:#0B3954; border-radius:6px; padding:6px 12px;",
                        "font-size:12px; white-space:nowrap;"
                      ),
                      tags$span(style = "color:#93c5fd; font-weight:600;", "Fisher MC : "),
                      tags$span(
                        style = paste0("font-weight:800; color:",
                                       if (isTRUE(fisher_res$p.value < 0.05)) "#fca5a5" else "white", ";"),
                        pval_txt
                      ),
                      tags$span(style = "font-size:10px;color:#93c5fd;margin-left:4px;",
                                "(r\u00e9f\u00e9rence)")
                    ),
                    tags$div(
                      style = paste0(
                        "font-size:11px; font-weight:700; color:",
                        if (isTRUE(concordant)) "#166534" else "#991b1b", ";"
                      ),
                      if (isTRUE(concordant)) "\u2713 Conclusions concordantes"
                      else            "! Conclusions divergentes \u2014 privil\u00e9gier Fisher"
                    )
                  ),
                # ── Interprétation textuelle ─────────────────────────────────
                tags$div(
                  style = "background:white; border-radius:8px; padding:12px 16px; border:1px solid #e2e8f0; font-size:12.5px; line-height:1.7; color:#334155;",
                  tags$b(icon("comment-alt"), " Interpr\u00e9tation : "),
                  if (!isTRUE(pval < 0.05)) {
                    paste0(
                      "Aucune association statistiquement significative n'est d\u00e9tect\u00e9e",
                      " entre les cat\u00e9gories rCSI et FCS dans cet \u00e9chantillon (p \u2265 0,05).",
                      " Les deux indicateurs semblent ind\u00e9pendants",
                      " \u2014 ce qui m\u00e9rite une investigation suppl\u00e9mentaire."
                    )
                  } else {
                    tags$span(
                      if (use_fisher)
                        paste0("Le test exact de Fisher (simulation Monte Carlo, B\u00a0=\u00a010\u00a0000) ",
                               "r\u00e9v\u00e8le une association ",
                               tolower(sub(" \\(.*", "", sig_lbl)),
                               " entre les cat\u00e9gories rCSI et FCS (p\u00a0", pval_txt, "). ")
                      else
                        paste0("Le test du \u03c7\u00b2 r\u00e9v\u00e8le une association ",
                               tolower(sub(" \\(.*", "", sig_lbl)),
                               " entre les cat\u00e9gories rCSI et FCS (\u03c7\u00b2\u00a0=\u00a0",
                               round(chi2$statistic, 2), ", ddl\u00a0=\u00a0", chi2$parameter,
                               ", p\u00a0", pval_txt, "). "),
                      paste0("La force de l'association est ",
                             tolower(sub(" \\(.*", "", v_lbl)),
                             " (V de Cram\u00e9r\u00a0=\u00a0", round(cramer_v, 3), "). "),
                      "La relation attendue est ", tags$b("n\u00e9gative"),
                      " : un FCS \u00e9lev\u00e9 devrait correspondre \u00e0 un rCSI faible, et inversement.",
                      if (use_fisher && concordant)
                        tags$span(
                          style = "display:block; margin-top:6px; color:#166534; font-weight:600;",
                          "\u2713 Le test exact de Fisher confirme la conclusion du \u03c7\u00b2",
                          " malgr\u00e9 des fr\u00e9quences attendues faibles (min.\u00a0=\u00a0", min_exp, ")."
                        ),
                      if (use_fisher && !concordant)
                        tags$span(
                          style = "display:block; margin-top:6px; color:#991b1b; font-weight:700;",
                          "\u26a0\ufe0f Les deux tests divergent. La p-valeur de Fisher (r\u00e9f\u00e9rence) ",
                          if (fisher_res$p.value < 0.05)
                            "contredit la non-significativit\u00e9 indiqu\u00e9e par le \u03c7\u00b2."
                          else
                            "temp\u00e8re la significativit\u00e9 indiqu\u00e9e par le \u03c7\u00b2.",
                          " Privil\u00e9gier Fisher en raison des fr\u00e9quences attendues insuffisantes."
                        )
                    )
                  }
                )
              )
            )
          )
        } else {
          chi2_row <- NULL
        }
        chi2_row
      },
      # ── Tableau ─────────────────────────────────────────────────────────────
      fluidRow(
        column(12,
          tags$div(class = "nutri-chart-card",
            tags$p(
              style = "font-size:11px; color:#94a3b8; margin:0 0 10px;",
              icon("info-circle"),
              " Pourcentages calcul\u00e9s sur le total du groupe de ventilation. \u2014 ",
              tags$span(
                style = "background:#fee2e2; color:#BF3636; padding:1px 7px; border-radius:4px; font-weight:700;",
                "Cellule rouge"
              ), " = rCSI \u00e9lev\u00e9 \u00d7 FCS Pauvre. \u2014 ",
              tags$span(
                style = "background:#f0f4f8; padding:1px 7px; border-radius:4px; font-weight:700;",
                "Ligne bleue"
              ), " = sous-total groupe."
            ),
            DT::DTOutput("accueil_crosstab_table")
          )
        )
      )
    )
  })

  # ── Tableau croisé rCSI × FCS — DT render ────────────────────────────────
  output$accueil_crosstab_table <- DT::renderDT({
    ct <- crosstab_data()
    req(!is.null(ct), nrow(ct) > 0)

    grp_label <- if (!is.null(input$crosstab_grp_var) &&
                     input$crosstab_grp_var != "__none__")
      input$crosstab_grp_var else "Groupe"

    # Construire la table d'affichage + colonnes cachées helpers
    disp <- ct |>
      transmute(
        Groupe_           = .grp,
        Cat_rCSI          = as.character(rcsi_cat),
        FCS_Pauvre        = paste0(n_P,     "\u00a0(", pct_P, "%)"),
        FCS_Limite        = paste0(n_L,     "\u00a0(", pct_L, "%)"),
        FCS_Acceptable    = paste0(n_A,     "\u00a0(", pct_A, "%)"),
        Total             = paste0(Total_n, "\u00a0(", pct_T, "%)"),
        .sub              = is_subtotal,
        .dv               = is_dblvuln
      ) |>
      rename(
        !!grp_label              := Groupe_,
        "Cat\u00e9gorie rCSI"    = Cat_rCSI,
        "FCS Pauvre (<21)"       = FCS_Pauvre,
        "FCS Limite (21-35)"     = FCS_Limite,
        "FCS Acceptable (>35)"   = FCS_Acceptable
      )

    n_tot_col <- ncol(disp)
    hidden_0idx <- c(n_tot_col - 2L, n_tot_col - 1L)  # 0-indexed

    DT::datatable(
      disp,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength = 100,
        dom        = "ftp",
        scrollX    = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = hidden_0idx),
          list(className = "dt-center", targets = 2:(n_tot_col - 3L))
        )
      )
    ) |>
      DT::formatStyle(
        columns     = 1:(n_tot_col - 2L),
        valueColumns = ".sub",
        backgroundColor = DT::styleEqual(c(1, 2), c("#e8f0fb", "#0B3954")),
        color           = DT::styleEqual(c(1, 2), c("#0B3954", "white")),
        fontWeight      = DT::styleEqual(c(1, 2), c("700", "800")),
        borderTop       = DT::styleEqual(c(1, 2), c("2px solid #0077B6", "3px solid #0077B6"))
      ) |>
      DT::formatStyle(
        columns      = 3,
        valueColumns = ".dv",
        backgroundColor = DT::styleEqual(1, "rgba(201,22,29,0.18)"),
        color           = DT::styleEqual(1, "#BF3636"),
        fontWeight      = DT::styleEqual(1, "800")
      ) |>
      DT::formatStyle(
        columns    = 1:(n_tot_col - 2L),
        lineHeight = "1.7"
      )
  })

  # ── Export CSV ────────────────────────────────────────────────────────────
  output$dl_crosstab_csv <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$crosstab_grp_var) &&
                  input$crosstab_grp_var != "__none__")
        input$crosstab_grp_var else "ensemble"
      paste0("crosstab_FCS_rCSI_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      ct <- crosstab_data(); req(!is.null(ct))
      out <- ct |>
        transmute(
          Groupe                 = .grp,
          Categorie_rCSI         = as.character(rcsi_cat),
          n_FCS_Pauvre           = n_P,
          pct_FCS_Pauvre         = pct_P,
          n_FCS_Limite           = n_L,
          pct_FCS_Limite         = pct_L,
          n_FCS_Acceptable       = n_A,
          pct_FCS_Acceptable     = pct_A,
          n_Total_groupe         = Total_n,
          pct_Total_groupe       = pct_T,
          N_groupe               = N_grp,
          Double_vulnerabilite   = if_else(is_dblvuln == 1L, "Oui", "Non"),
          Type_ligne             = case_when(
            is_subtotal == 1L ~ "Sous-total",
            is_subtotal == 2L ~ "Grand total",
            TRUE              ~ "Detail"
          )
        )
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Export Excel ─────────────────────────────────────────────────────────
  output$dl_crosstab_xlsx <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$crosstab_grp_var) &&
                  input$crosstab_grp_var != "__none__")
        input$crosstab_grp_var else "ensemble"
      paste0("crosstab_FCS_rCSI_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      ct <- crosstab_data(); req(!is.null(ct))

      # Feuille 1 : affichage formaté
      sheet1 <- ct |>
        transmute(
          Groupe                = .grp,
          `Catégorie rCSI`      = as.character(rcsi_cat),
          `FCS Pauvre (<21)`    = paste0(n_P, " (", pct_P, "%)"),
          `FCS Limite (21-35)`  = paste0(n_L, " (", pct_L, "%)"),
          `FCS Acceptable (>35)` = paste0(n_A, " (", pct_A, "%)"),
          Total                 = paste0(Total_n, " (", pct_T, "%)"),
          `Double vuln.`        = if_else(is_dblvuln == 1L, "OUI ⚠️", ""),
          `Type ligne`          = case_when(
            is_subtotal == 1L ~ "Sous-total",
            is_subtotal == 2L ~ "TOTAL",
            TRUE              ~ "Détail"
          )
        )

      # Feuille 2 : données brutes numériques
      sheet2 <- ct |>
        filter(is_subtotal != 2L) |>   # exclure grand total
        transmute(
          Groupe             = .grp,
          Categorie_rCSI     = as.character(rcsi_cat),
          n_FCS_Pauvre       = n_P,
          pct_FCS_Pauvre     = pct_P,
          n_FCS_Limite       = n_L,
          pct_FCS_Limite     = pct_L,
          n_FCS_Acceptable   = n_A,
          pct_FCS_Acceptable = pct_A,
          n_Total            = Total_n,
          N_groupe           = N_grp,
          Double_vuln        = if_else(is_dblvuln == 1L, 1L, 0L)
        )

      tryCatch(
        writexl::write_xlsx(
          list("Tableau croisé" = sheet1, "Données numériques" = sheet2),
          path = file
        ),
        error = function(e) {
          # Fallback CSV si writexl indisponible
          write.csv(sheet1, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      )
    }
  )

  # ── Carte double vulnérabilité — helpers de matching ────────────────────────
  normalize_region <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
    gsub("[^a-z0-9]", "", x)
  }

  # Pour chaque nom de région/département enquête → trouve le nom GADM le plus proche
  # name_col : "NAME_1" pour régions, "NAME_2" pour départements
  match_gadm_regions <- function(survey_names, sf_obj, name_col = "NAME_1") {
    gadm_names <- sf_obj[[name_col]]
    var_col    <- sub("^NAME_", "VARNAME_", name_col)
    varname    <- if (var_col %in% names(sf_obj)) sf_obj[[var_col]] else rep(NA_character_, nrow(sf_obj))
    norm_g     <- normalize_region(gadm_names)

    vapply(survey_names, function(s) {
      ns <- normalize_region(s)

      # 1. Correspondance exacte
      idx <- which(norm_g == ns)
      if (length(idx)) return(gadm_names[idx[1]])

      # 2. Variantes GADM (VARNAME_x, séparées par | ou ,)
      for (i in seq_along(varname)) {
        if (!is.na(varname[i]) && nzchar(varname[i])) {
          vn <- normalize_region(strsplit(varname[i], "[|,;]")[[1]])
          if (ns %in% vn) return(gadm_names[i])
        }
      }

      # 3. Inclusion réciproque (min 3 car.)
      if (nchar(ns) >= 3) {
        ci <- which(vapply(norm_g, function(g)
          nchar(g) >= 3 && (grepl(ns, g, fixed = TRUE) || grepl(g, ns, fixed = TRUE)),
          logical(1)))
        if (length(ci)) return(gadm_names[ci[1]])
      }

      # 4. Correspondance approximative (agrep)
      fi <- agrep(ns, norm_g, max.distance = 0.25, value = FALSE)
      if (length(fi)) return(gadm_names[fi[1]])

      NA_character_
    }, character(1), USE.NAMES = FALSE)
  }

  # ── Carte double vulnérabilité — reactive données (multi-niveaux) ────────────
  dv_map_data <- reactive({
    d <- df_f(); m <- map_cols()
    geo <- selected_geo()
    req(!is.null(geo), !is.null(geo$country_code), nzchar(geo$country_code))
    req("rcsi_cat" %in% names(d), "fcs_cat" %in% names(d))

    # Niveau admin demandé (défaut : 1)
    lvl_req <- if (is.null(input$dv_map_level)) 1L else as.integer(input$dv_map_level)

    # Colonne enquête et nom GADM selon le niveau
    grp_col  <- if (lvl_req == 2L && !is.null(m$dept) && m$dept %in% names(d))
                  m$dept else m$region
    lvl      <- if (identical(grp_col, m$dept) && lvl_req == 2L) 2L else 1L
    name_col <- paste0("NAME_", lvl)
    unit_lbl <- if (lvl == 2L) "d\u00e9partements" else "r\u00e9gions"

    req(!is.null(grp_col), grp_col %in% names(d))

    # Prévalence par unité admin
    prev <- d |>
      filter(!is.na(.data[[grp_col]]), !is.na(rcsi_cat), !is.na(fcs_cat)) |>
      group_by(unit_name = as.character(.data[[grp_col]])) |>
      summarise(
        n_total = n(),
        n_dv    = sum(
          rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" & fcs_cat == "Pauvre (<21)",
          na.rm = TRUE
        ),
        pct_dv  = round(n_dv / n_total * 100, 1),
        .groups = "drop"
      )

    if (nrow(prev) == 0) return(NULL)

    # Chargement GADM au niveau demandé
    sf_adm <- tryCatch(
      load_gadm_level(geo$country_code, level = lvl),
      error = function(e) { message("GADM L", lvl, " : ", e$message); NULL }
    )
    if (is.null(sf_adm))
      return(list(prev = prev, sf = NULL, level = lvl, unit_lbl = unit_lbl,
                  error = "gadm_unavailable"))

    # Frontières L1 pour overlay (si on est en L2)
    sf_l1_borders <- if (lvl == 2L) {
      tryCatch(load_gadm_level(geo$country_code, level = 1L),
               error = function(e) NULL)
    } else NULL

    # Jointure fuzzy enquête → GADM
    prev[[name_col]] <- match_gadm_regions(prev$unit_name, sf_adm, name_col = name_col)
    n_matched        <- sum(!is.na(prev[[name_col]]))

    # Jointure spatiale (all GADM polygons, left join)
    sf_joined <- sf_adm |>
      dplyr::left_join(
        dplyr::select(prev, all_of(name_col), n_total, n_dv, pct_dv, unit_name),
        by = name_col
      )

    list(
      sf            = sf_joined,
      sf_l1_borders = sf_l1_borders,
      prev          = prev,
      name_col      = name_col,
      n_gadm        = nrow(sf_adm),
      n_survey      = nrow(prev),
      n_matched     = n_matched,
      country       = geo$country_name,
      level         = lvl,
      unit_lbl      = unit_lbl,
      error         = NULL
    )
  })

  # ── Carte double vulnérabilité — UI section ──────────────────────────────────
  output$accueil_dv_map_row <- renderUI({
    d <- df_f(); m <- map_cols()
    if (is.null(m$region) || !(m$region %in% names(d))) return(NULL)
    if (!("rcsi_cat" %in% names(d)) || !("fcs_cat" %in% names(d))) return(NULL)

    geo           <- selected_geo()
    country_ok    <- !is.null(geo) && !is.null(geo$country_code) && nzchar(geo$country_code)
    country_label <- if (country_ok) geo$country_name else ""

    # Disponibilité GADM L2 + colonne département dans les données
    max_gadm_lvl <- if (country_ok) {
      tryCatch(get_available_levels(geo$country_code), error = function(e) 1L)
    } else 1L
    has_dept  <- !is.null(m$dept) && m$dept %in% names(d)
    show_l2   <- country_ok && (max_gadm_lvl >= 2L) && has_dept

    # Sélecteur niveau (uniquement si L2 dispo)
    lvl_selector <- if (show_l2) {
      current_lvl <- if (is.null(isolate(input$dv_map_level))) "1" else
        isolate(input$dv_map_level)
      tags$div(
        style = "display:flex; align-items:center; gap:6px; font-size:13px;",
        tags$span(
          style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px; white-space:nowrap;",
          icon("layer-group"), " Niveau"
        ),
        tags$div(
          style = "min-width:230px;",
          selectInput(
            "dv_map_level",
            label    = NULL,
            choices  = setNames(
              c("1", "2"),
              c(paste0("Niveau 1 \u2014 R\u00e9gion / Province  (", m$region, ")"),
                paste0("Niveau 2 \u2014 D\u00e9partement / District  (", m$dept, ")"))
            ),
            selected = current_lvl,
            width    = "100%"
          )
        )
      )
    } else NULL

    tagList(
      # En-tête section
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(
          icon("map-marked-alt"),
          " Carte de double vuln\u00e9rabilit\u00e9 \u2014 FCS Pauvre \u00d7 rCSI \u00e9lev\u00e9"
        ),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
          lvl_selector,
          if (country_ok)
            tags$span(
              style = "font-size:12px; color:#64748b; font-weight:600; white-space:nowrap;",
              icon("globe-africa"), "\u00a0", country_label
            ),
          downloadButton(
            "dl_dv_map_csv",
            label = " CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      # Corps
      if (!country_ok) {
        tags$div(
          style = paste0(
            "background:#f0f8ff; border-left:4px solid #0077B6;",
            "border-radius:8px; padding:16px 20px; margin-bottom:16px;",
            "font-size:13px; color:#1e293b; display:flex; align-items:center; gap:12px;"
          ),
          tags$div(
            style = paste0("width:40px; height:40px; border-radius:10px; background:#0077B6;",
                           "display:flex; align-items:center; justify-content:center;",
                           "color:white; font-size:18px; flex-shrink:0;"),
            icon("map")
          ),
          tags$div(
            tags$b("S\u00e9lectionnez un pays pour afficher la carte"),
            tags$br(),
            tags$span(
              style = "font-size:12px; color:#64748b;",
              "Ouvrez l'onglet ", tags$b("Cartographie"),
              " dans la barre lat\u00e9rale, s\u00e9lectionnez votre pays,",
              " puis revenez sur ce tableau de bord."
            )
          )
        )
      } else {
        fluidRow(
          column(12,
            tags$div(class = "nutri-chart-card",
              uiOutput("accueil_dv_map_note"),
              leafletOutput("accueil_dv_map", height = "500px")
            )
          )
        )
      }
    )
  })

  # ── Note de matching ─────────────────────────────────────────────────────────
  output$accueil_dv_map_note <- renderUI({
    data <- dv_map_data()
    if (is.null(data)) return(NULL)

    if (!is.null(data$error) && data$error == "gadm_unavailable") {
      return(tags$div(
        style = "background:#fff3e0; border-left:3px solid #D98B14; border-radius:6px; padding:10px 14px; margin-bottom:10px; font-size:12px; color:#7c2d12;",
        icon("exclamation-triangle"),
        paste0(" Impossible de charger les fronti\u00e8res GADM niveau ", data$level,
               " pour ce pays. V\u00e9rifiez la connexion internet ou le cache GADM.")
      ))
    }

    n_m      <- data$n_matched
    n_s      <- data$n_survey
    ul       <- data$unit_lbl
    nm_col   <- data$name_col
    unmatched <- data$prev$unit_name[is.na(data$prev[[nm_col]])]

    # Badge niveau admin
    lvl_badge <- tags$span(
      style = paste0("background:", if (data$level == 2L) "#6B3FA0" else "#0077B6",
                     "; color:white; padding:1px 8px; border-radius:12px;",
                     "font-size:10px; font-weight:700; margin-right:6px;"),
      paste0("L", data$level, " \u2014 ", toupper(substr(ul, 1, 1)), substr(ul, 2, nchar(ul)))
    )

    if (n_m == n_s) {
      tags$div(
        style = "background:#f0fdf4; border-left:3px solid #2E7D52; border-radius:6px; padding:8px 14px; margin-bottom:10px; font-size:11.5px; color:#166534; display:flex; align-items:center; gap:6px;",
        lvl_badge,
        icon("check-circle"),
        paste0(" ", n_m, "/", n_s, " ", ul, " appari\u00e9es avec GADM (", data$country, ").")
      )
    } else {
      tags$div(
        style = "background:#fffbeb; border-left:3px solid #f59e0b; border-radius:6px; padding:8px 14px; margin-bottom:10px; font-size:11.5px; color:#78350f;",
        lvl_badge,
        icon("exclamation-circle"),
        paste0(" ", n_m, "/", n_s, " ", ul, " appari\u00e9es. "),
        tags$b("Non appari\u00e9es : "),
        paste(unmatched, collapse = ", "),
        paste0(". V\u00e9rifiez l'orthographe par rapport aux noms GADM.")
      )
    }
  })

  # ── Rendu Leaflet choroplèthe (multi-niveaux) ─────────────────────────────────
  output$accueil_dv_map <- renderLeaflet({
    data <- dv_map_data()
    req(!is.null(data), !is.null(data$sf))

    sf_data  <- data$sf
    nm_col   <- data$name_col
    lvl      <- data$level

    # Palette rouge séquentielle
    all_pcts   <- sf_data$pct_dv[!is.na(sf_data$pct_dv)]
    pal_domain <- if (length(all_pcts) == 0 || max(all_pcts) == 0)
      c(0, 30) else c(0, max(all_pcts, 5))

    pal <- colorNumeric(
      palette  = c("#f8fafc", "#fee2e2", "#fca5a5", "#ef4444", "#b91c1c", "#450a0a"),
      domain   = pal_domain,
      na.color = "#e2e8f0"
    )

    # Étiquettes : nom + région parente (L2 uniquement)
    labels <- lapply(seq_len(nrow(sf_data)), function(i) {
      row  <- sf_data[i, ]
      name <- row[[nm_col]]
      pct  <- row$pct_dv
      ndv  <- row$n_dv
      ntot <- row$n_total

      parent <- if (lvl == 2L && "NAME_1" %in% names(sf_data))
        paste0("<span style='color:#64748b;font-size:11px;'>",
               row$NAME_1, "</span><br/>") else ""

      if (is.na(pct)) {
        htmltools::HTML(sprintf(
          "%s<b>%s</b><br/><i style='color:#94a3b8'>Non couvert</i>",
          parent, name
        ))
      } else {
        htmltools::HTML(sprintf(
          "%s<b>%s</b><br/>Double vuln.\u00a0: <b style='color:#BF3636'>%.1f%%</b><br/>%d / %d m\u00e9nages",
          parent, name, pct,
          if (is.na(ndv)) 0L else as.integer(ndv),
          if (is.na(ntot)) 0L else as.integer(ntot)
        ))
      }
    })

    # Contrôle info
    ctrl_html <- htmltools::HTML(paste0(
      "<div style='background:white;padding:6px 10px;border-radius:6px;",
      "border:1px solid #e2e8f0;font-size:11px;color:#64748b;line-height:1.5;'>",
      "<b>", data$country, "</b><br/>",
      "Niveau\u00a0", lvl, " \u2014 ",
      data$n_matched, "/", data$n_survey, "\u00a0", data$unit_lbl,
      "</div>"
    ))

    m <- leaflet(sf_data) |>
      addProviderTiles("Esri.WorldGrayCanvas",
                       options = providerTileOptions(minZoom = 3, maxZoom = 14)) |>
      addPolygons(
        fillColor    = ~pal(pct_dv),
        fillOpacity  = 0.78,
        color        = "white",
        weight       = if (lvl == 2L) 0.7 else 1.2,
        opacity      = 1,
        smoothFactor = 0.4,
        highlight    = highlightOptions(
          weight = 3, color = "#0077B6", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label        = labels,
        labelOptions = labelOptions(
          style  = list("font-size" = "13px", "padding" = "6px 10px",
                        "border-radius" = "6px"),
          sticky = FALSE
        )
      )

    # Overlay frontières L1 (contexte géographique en mode L2)
    if (lvl == 2L && !is.null(data$sf_l1_borders)) {
      m <- m |>
        addPolygons(
          data        = data$sf_l1_borders,
          fill        = FALSE,
          color       = "#1e3a5f",
          weight      = 2,
          opacity     = 0.55,
          smoothFactor = 0.4,
          label       = ~NAME_1,
          labelOptions = labelOptions(
            style    = list("font-size" = "11px", "color" = "#1e3a5f",
                            "font-weight" = "bold"),
            sticky   = FALSE,
            textOnly = TRUE
          ),
          options     = pathOptions(clickable = FALSE)
        )
    }

    m |>
      addLegend(
        pal      = pal,
        values   = ~pct_dv,
        opacity  = 0.85,
        title    = htmltools::HTML(
          "Double vuln\u00e9rabilit\u00e9<br/><small>FCS Pauvre + rCSI \u2265 19</small>"
        ),
        position  = "bottomright",
        labFormat = labelFormat(suffix = "%")
      ) |>
      addControl(html = ctrl_html, position = "topleft")
  })

  # ── Export CSV carte ─────────────────────────────────────────────────────────
  output$dl_dv_map_csv <- downloadHandler(
    filename = function() {
      geo  <- selected_geo()
      pays <- if (!is.null(geo) && nzchar(geo$country_code)) geo$country_code else "pays"
      lvl  <- if (is.null(input$dv_map_level)) 1L else as.integer(input$dv_map_level)
      paste0("dv_carte_L", lvl, "_", pays, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- dv_map_data(); req(!is.null(data))
      nm   <- data$name_col
      ul   <- sub("\u00e9", "e", data$unit_lbl)  # ASCII pour noms de colonnes CSV

      out <- data$prev |>
        rename(
          !!paste0(ul, "_enquete") := unit_name,
          !!paste0(ul, "_gadm")    := all_of(nm)
        ) |>
        mutate(
          apparie       = if_else(!is.na(.data[[paste0(ul, "_gadm")]]), "Oui", "Non"),
          niveau_admin  = paste0("L", data$level)
        ) |>
        rename(
          n_menages       = n_total,
          n_double_vuln   = n_dv,
          pct_double_vuln = pct_dv
        ) |>
        arrange(desc(pct_double_vuln))

      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Rapport automatique — helper : agréger par groupe pour graphiques ────────
  agg_cat_by_grp <- function(d, score_col, grp_col) {
    if (is.null(grp_col) || !(grp_col %in% names(d))) return(NULL)
    if (!(score_col %in% names(d))) return(NULL)
    d |>
      filter(!is.na(.data[[score_col]]), !is.na(.data[[grp_col]])) |>
      count(grp = as.character(.data[[grp_col]]),
            !!score_col := .data[[score_col]]) |>
      group_by(grp) |>
      mutate(pct = n / sum(n) * 100) |>
      ungroup() |>
      rename_with(~ score_col, all_of(score_col))
  }

  temporal_agg_rpt <- function(d, sv_col, score_col) {
    if (is.null(sv_col) || !(sv_col %in% names(d))) return(NULL)
    if (!(score_col %in% names(d))) return(NULL)
    rounds_ord <- sort_rounds(unique(na.omit(as.character(d[[sv_col]]))))
    d |>
      filter(!is.na(.data[[score_col]]), !is.na(.data[[sv_col]])) |>
      mutate(.round_chr = factor(as.character(.data[[sv_col]]), levels = rounds_ord)) |>
      group_by(.round_chr) |>
      summarise(
        mean_val = mean(.data[[score_col]], na.rm = TRUE),
        sd_val   = sd(.data[[score_col]],   na.rm = TRUE),
        n        = sum(!is.na(.data[[score_col]])),
        .groups  = "drop"
      ) |>
      mutate(
        ci_lo = mean_val - 1.96 * sd_val / sqrt(pmax(n, 1)),
        ci_hi = mean_val + 1.96 * sd_val / sqrt(pmax(n, 1))
      )
  }

  # ── Rapport automatique — reactive données ────────────────────────────────────
  rapport_data <- reactive({
    d <- df_f(); m <- map_cols()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    geo       <- selected_geo()
    pays      <- if (!is.null(geo) && nzchar(geo$country_code)) geo$country_name else ""
    grp_col   <- m$region
    grp_label <- if (!is.null(grp_col) && grp_col %in% names(d)) grp_col else "Groupe"
    sv_col    <- m$survey

    # Flags disponibilité
    has_fcs  <- ".fcs"  %in% names(d) && !all(is.na(d$.fcs))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    has_hdds <- ".hdds" %in% names(d) && !all(is.na(d$.hdds))
    has_hhs  <- ".hhs"  %in% names(d) && !all(is.na(d$.hhs))
    has_temporal <- !is.null(sv_col) && sv_col %in% names(d) &&
      (has_fcs || has_rcsi) &&
      length(unique(na.omit(as.character(d[[sv_col]])))) >= 2

    # ── Statistiques FCS ──────────────────────────────────────────────────────
    fcs_mean <- fcs_sd <- fcs_median <- fcs_min <- fcs_max <- NA
    pct_fcs_poor <- pct_fcs_limit <- pct_fcs_acceptable <- 0
    if (has_fcs) {
      fcs_v  <- d$.fcs[!is.na(d$.fcs)]
      fcs_mean   <- mean(fcs_v);   fcs_sd     <- sd(fcs_v)
      fcs_median <- median(fcs_v); fcs_min    <- min(fcs_v); fcs_max <- max(fcs_v)
      if ("fcs_cat" %in% names(d)) {
        fc  <- table(d$fcs_cat); tot <- sum(fc)
        spct <- function(k) { v <- as.numeric(fc[k]); if (is.na(v)) 0 else v / tot * 100 }
        pct_fcs_poor       <- spct("Pauvre (<21)")
        pct_fcs_limit      <- spct("Limite (21-35)")
        pct_fcs_acceptable <- spct("Acceptable (>35)")
      }
    }

    # ── Statistiques rCSI ─────────────────────────────────────────────────────
    rcsi_mean <- rcsi_sd <- rcsi_median <- rcsi_min <- rcsi_max <- NA
    pct_rcsi_low <- pct_rcsi_mid <- pct_rcsi_high <- 0
    if (has_rcsi) {
      rv <- d$.rcsi[!is.na(d$.rcsi)]
      rcsi_mean   <- mean(rv);   rcsi_sd     <- sd(rv)
      rcsi_median <- median(rv); rcsi_min    <- min(rv); rcsi_max <- max(rv)
      if ("rcsi_cat" %in% names(d)) {
        rc  <- table(d$rcsi_cat); tot <- sum(rc)
        spct <- function(k) { v <- as.numeric(rc[k]); if (is.na(v)) 0 else v / tot * 100 }
        pct_rcsi_low  <- spct("Pas/Peu de coping")
        pct_rcsi_mid  <- spct("Coping moyen")
        pct_rcsi_high <- spct("Coping \u00e9lev\u00e9 (\u226519)")
      }
    }

    # ── Statistiques HDDS ────────────────────────────────────────────────────
    hdds_mean <- hdds_sd <- hdds_median <- NA
    pct_hdds_low <- pct_hdds_mid <- pct_hdds_high <- 0
    if (has_hdds) {
      hv <- d$.hdds[!is.na(d$.hdds)]
      hdds_mean <- mean(hv); hdds_sd <- sd(hv); hdds_median <- median(hv)
      if ("hdds_cat" %in% names(d)) {
        hc  <- table(d$hdds_cat); tot <- sum(hc)
        spct <- function(k) { v <- as.numeric(hc[k]); if (is.na(v)) 0 else v / tot * 100 }
        pct_hdds_low  <- spct("Faible diversit\u00e9 (\u22643)")
        pct_hdds_mid  <- spct("Diversit\u00e9 moyenne (4-5)")
        pct_hdds_high <- spct("Bonne diversit\u00e9 (\u22656)")
      }
    }

    # ── Statistiques HHS ─────────────────────────────────────────────────────
    hhs_mean <- hhs_sd <- NA
    if (has_hhs) {
      hh <- d$.hhs[!is.na(d$.hhs)]
      hhs_mean <- mean(hh); hhs_sd <- sd(hh)
    }

    # ── Double vulnérabilité ──────────────────────────────────────────────────
    n_valid <- sum(!is.na(d$rcsi_cat) & !is.na(d$fcs_cat))
    n_dv    <- sum(
      !is.na(d$rcsi_cat) & !is.na(d$fcs_cat) &
        d$rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" &
        d$fcs_cat  == "Pauvre (<21)", na.rm = TRUE
    )
    pct_dv <- if (n_valid > 0) round(n_dv / n_valid * 100, 1) else 0

    # ── Données pour graphiques ───────────────────────────────────────────────
    fcs_cat_by_grp  <- if (has_fcs  && "fcs_cat"  %in% names(d))
      agg_cat_by_grp(d, "fcs_cat",  grp_col) else NULL
    rcsi_cat_by_grp <- if (has_rcsi && "rcsi_cat" %in% names(d))
      agg_cat_by_grp(d, "rcsi_cat", grp_col) else NULL
    hdds_cat_by_grp <- if (has_hdds && "hdds_cat" %in% names(d))
      agg_cat_by_grp(d, "hdds_cat", grp_col) else NULL

    # Scatter (échantillon max 2000 pts)
    scatter_data <- if (has_fcs && has_rcsi) {
      sc_cols <- intersect(c(".fcs", ".rcsi", "rcsi_cat", "fcs_cat"), names(d))
      sc <- d[complete.cases(d[sc_cols]), sc_cols]
      if (nrow(sc) > 2000) sc[sample(nrow(sc), 2000), ] else sc
    } else NULL

    # Données temporelles
    temporal_fcs  <- if (has_temporal && has_fcs)
      temporal_agg_rpt(d, sv_col, ".fcs") else NULL
    temporal_rcsi <- if (has_temporal && has_rcsi)
      temporal_agg_rpt(d, sv_col, ".rcsi") else NULL
    rounds <- if (has_temporal)
      sort_rounds(unique(na.omit(as.character(d[[sv_col]])))) else character(0)

    # Tableau croisé (version display sans colonnes cachées)
    crosstab_disp <- if ("rcsi_cat" %in% names(d) && "fcs_cat" %in% names(d)) {
      ct <- build_crosstab(d, grp_col)
      if (!is.null(ct)) {
        ct |>
          transmute(
            Groupe          = .grp,
            `Cat. rCSI`     = as.character(rcsi_cat),
            `FCS Pauvre (<21)`     = paste0(n_P, " (", pct_P, "%)"),
            `FCS Limite (21-35)`   = paste0(n_L, " (", pct_L, "%)"),
            `FCS Acceptable (>35)` = paste0(n_A, " (", pct_A, "%)"),
            Total                  = paste0(Total_n, " (", pct_T, "%)")
          )
      } else NULL
    } else NULL

    # Résultats Chi²/Fisher
    chi2_stat <- chi2_df <- chi2_pval <- cramer_v <- fisher_pval <- min_exp <- NULL
    use_fisher <- FALSE; concordant <- TRUE
    if ("rcsi_cat" %in% names(d) && "fcs_cat" %in% names(d)) {
      d_v    <- d[!is.na(d$rcsi_cat) & !is.na(d$fcs_cat), ]
      ct_mat <- table(d_v$rcsi_cat, d_v$fcs_cat)
      chi2r  <- tryCatch(chisq.test(ct_mat, correct = FALSE), error = function(e) NULL)
      if (!is.null(chi2r)) {
        chi2_stat  <- as.numeric(chi2r$statistic)
        chi2_df    <- as.integer(chi2r$parameter)
        chi2_pval  <- chi2r$p.value
        n_ct       <- sum(ct_mat)
        cramer_v   <- sqrt(chi2_stat / (n_ct * (min(nrow(ct_mat), ncol(ct_mat)) - 1)))
        min_exp    <- round(min(chi2r$expected), 2)
        if (min_exp < 5) {
          fr <- tryCatch(
            fisher.test(ct_mat, simulate.p.value = TRUE, B = 10000),
            error = function(e) NULL
          )
          if (!is.null(fr)) {
            use_fisher  <- TRUE
            fisher_pval <- fr$p.value
            concordant  <- (chi2_pval < 0.05) == (fisher_pval < 0.05)
          }
        }
      }
    }

    list(
      pays = pays, n_total = nrow(d), date_rapport = format(Sys.Date(), "%d/%m/%Y"),
      grp_label = grp_label,
      has_fcs = has_fcs, has_rcsi = has_rcsi, has_hdds = has_hdds,
      has_hhs = has_hhs, has_temporal = has_temporal,
      fcs_mean = fcs_mean, fcs_sd = fcs_sd, fcs_median = fcs_median,
      fcs_min = fcs_min, fcs_max = fcs_max,
      pct_fcs_poor = pct_fcs_poor, pct_fcs_limit = pct_fcs_limit,
      pct_fcs_acceptable = pct_fcs_acceptable,
      rcsi_mean = rcsi_mean, rcsi_sd = rcsi_sd, rcsi_median = rcsi_median,
      rcsi_min = rcsi_min, rcsi_max = rcsi_max,
      pct_rcsi_low = pct_rcsi_low, pct_rcsi_mid = pct_rcsi_mid,
      pct_rcsi_high = pct_rcsi_high,
      hdds_mean = hdds_mean, hdds_sd = hdds_sd, hdds_median = hdds_median,
      pct_hdds_low = pct_hdds_low, pct_hdds_mid = pct_hdds_mid,
      pct_hdds_high = pct_hdds_high,
      hhs_mean = hhs_mean, hhs_sd = hhs_sd,
      n_dv = n_dv, pct_dv = pct_dv,
      fcs_cat_by_grp = fcs_cat_by_grp, rcsi_cat_by_grp = rcsi_cat_by_grp,
      hdds_cat_by_grp = hdds_cat_by_grp, scatter_data = scatter_data,
      temporal_fcs = temporal_fcs, temporal_rcsi = temporal_rcsi, rounds = rounds,
      crosstab_disp = crosstab_disp,
      chi2_stat = chi2_stat, chi2_df = chi2_df, chi2_pval = chi2_pval,
      cramer_v = cramer_v, fisher_pval = fisher_pval, min_exp = min_exp,
      use_fisher = use_fisher, concordant = concordant
    )
  })

  # ── Rapport automatique — UI barre ────────────────────────────────────────────
  output$accueil_rapport_bar <- renderUI({
    d <- df_f()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    tags$div(
      style = paste0(
        "background:linear-gradient(135deg,#0B3954 0%,#0077B6 100%);",
        "border-radius:12px; padding:14px 22px; margin-bottom:18px;",
        "display:flex; align-items:center; justify-content:space-between;",
        "flex-wrap:wrap; gap:12px;"
      ),
      # Texte
      tags$div(
        tags$div(
          style = "font-size:14px; font-weight:700; color:white; margin-bottom:3px;",
          icon("file-alt"), " G\u00e9n\u00e9rer le rapport automatique"
        ),
        tags$div(
          style = "font-size:11.5px; color:rgba(255,255,255,0.80);",
          "Synth\u00e8se compl\u00e8te : KPI, graphiques, tableau crois\u00e9, tests statistiques. ",
          tags$b(style = "color:white;", paste0(nrow(d), " m\u00e9nages")), " inclus."
        )
      ),
      # Boutons
      tags$div(
        style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
        downloadButton(
          "dl_rapport_word",
          label = tagList(icon("file-word"),   " Word (.docx)"),
          style = paste0(
            "background:white; color:#0B3954; border:none; font-weight:700;",
            "font-size:12px; padding:7px 14px; border-radius:8px;"
          )
        ),
        downloadButton(
          "dl_rapport_html",
          label = tagList(icon("globe"), " HTML"),
          style = paste0(
            "background:rgba(255,255,255,0.2); color:white; border:1px solid rgba(255,255,255,0.5);",
            "font-weight:600; font-size:12px; padding:7px 14px; border-radius:8px;"
          )
        ),
        tags$span(
          style = "font-size:10px; color:rgba(255,255,255,0.65); max-width:160px; line-height:1.3;",
          icon("clock"), " G\u00e9n\u00e9ration 15-30 sec."
        )
      )
    )
  })

  # ── Helper : construire + sauvegarder le RDS, retourner chemin template ───────
  .build_rapport_rds <- function() {
    rpt  <- rapport_data()
    if (is.null(rpt)) stop("Données insuffisantes pour générer le rapport.")
    rds  <- tempfile(fileext = ".rds")
    saveRDS(rpt, rds)
    tmpl <- file.path(getwd(), "rapport_template_nutri.Rmd")
    if (!file.exists(tmpl)) stop(paste0("Template introuvable : ", tmpl))
    list(rds = rds, tmpl = tmpl,
         titre = paste0("Rapport s\u00e9curit\u00e9 alimentaire",
                        if (nzchar(rpt$pays)) paste0(" \u2014 ", rpt$pays) else ""),
         date_rapport = rpt$date_rapport)
  }

  # ── Download Word ─────────────────────────────────────────────────────────────
  output$dl_rapport_word <- downloadHandler(
    filename = function() {
      rpt <- rapport_data()
      pays <- if (!is.null(rpt) && nzchar(rpt$pays))
        gsub("[^A-Za-z0-9]", "_", rpt$pays) else "rapport"
      paste0("rapport_SA_", pays, "_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      info <- .build_rapport_rds()
      rmarkdown::render(
        input         = info$tmpl,
        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 2),
        output_file   = file,
        params        = list(data_file    = info$rds,
                             titre        = info$titre,
                             date_rapport = info$date_rapport),
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
    }
  )

  # ── Download HTML ─────────────────────────────────────────────────────────────
  output$dl_rapport_html <- downloadHandler(
    filename = function() {
      rpt <- rapport_data()
      pays <- if (!is.null(rpt) && nzchar(rpt$pays))
        gsub("[^A-Za-z0-9]", "_", rpt$pays) else "rapport"
      paste0("rapport_SA_", pays, "_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      info <- .build_rapport_rds()
      rmarkdown::render(
        input         = info$tmpl,
        output_format = rmarkdown::html_document(
          toc = TRUE, toc_float = TRUE, toc_depth = 2,
          self_contained = TRUE, theme = "flatly"
        ),
        output_file   = file,
        params        = list(data_file    = info$rds,
                             titre        = info$titre,
                             date_rapport = info$date_rapport),
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
    }
  )

  # ── MAPPEUR ─────────────────────────────────────────────────────────────────
  make_mapper_row <- function(key, lbl, side="left") {
    df  <- raw_data(); req(df)
    m   <- map_cols()
    nms <- c("(Non mappé)", names(df))
    val <- m[[key]]
    classe <- if (!is.null(val) && val %in% names(df)) "badge-ok"
              else if (!is.null(val)) "badge-warn" else "badge-err"
    badge <- if (!is.null(val) && val %in% names(df))
      tags$span(class=classe, paste0("✓ ", val))
    else tags$span(class="badge-err", "Non trouvé — sélectionnez")

    tags$div(class="wizard-step",
      style="flex-wrap:wrap;",
      tags$div(class="ws-num",
               style=if(!is.null(val) && val%in%names(df))"background:#2E7D52;color:white;" else "background:#BF3636;color:white;",
               if(!is.null(val) && val%in%names(df)) "✓" else "!"),
      tags$div(style="flex:1;min-width:120px;",
        tags$strong(lbl, style="font-size:13px;"),
        tags$br(), badge
      ),
      tags$div(style="min-width:200px;",
        selectInput(paste0("map_",key), NULL,
                    choices=nms,
                    selected=if(!is.null(val) && val%in%nms) val else "(Non mappé)",
                    width="100%")
      )
    )
  }

  mapper_defs <- list(
    whz="Score WHZ", haz="Score HAZ", waz="Score WAZ",
    muac="MUAC (mm)", oedema="Œdèmes", age_m="Âge (mois)",
    sexe="Sexe", region="Région", dept="Département",
    lat="Latitude GPS", lon="Longitude GPS",
    poids="Poids (kg)", taille="Taille (cm)",
    fcs="FCS (PAM)", hhs="HHS (PAM)", depl="Statut déplacement"
  )

  # ── Tableau de vérification dictionnaire OMS ────────────────────────────
  output$dict_verification_table <- renderDT({
    df <- raw_data(); req(df)
    m  <- map_cols()

    rows <- lapply(names(OMS_DICT), function(key) {
      detected_col <- m[[key]]
      aliases      <- paste(OMS_DICT[[key]], collapse = " · ")
      label        <- if (key %in% names(OMS_META)) OMS_META[[key]]$label else key
      cat_oms      <- if (key %in% names(OMS_META)) OMS_META[[key]]$cat   else ""

      sample_vals <- ""
      if (!is.null(detected_col) && detected_col %in% names(df)) {
        vals <- head(na.omit(df[[detected_col]]), 5)
        sample_vals <- paste(vals, collapse = ", ")
      }

      data.frame(
        Variable_OMS         = paste0(key, " \u2014 ", label),
        Categorie            = cat_oms,
        Colonne_detectee     = if (!is.null(detected_col)) detected_col else "\u2014",
        Statut               = if (!is.null(detected_col) && detected_col %in% names(df))
                                 "\u2705 D\u00e9tect\u00e9"
                               else "\u274c Non trouv\u00e9",
        Aliases_dictionnaire = aliases,
        Apercu               = sample_vals,
        stringsAsFactors     = FALSE
      )
    })

    verify_df <- do.call(rbind, rows)

    datatable(verify_df, rownames = FALSE,
      colnames = c("Variable OMS", "Cat\u00e9gorie", "Colonne d\u00e9tect\u00e9e",
                    "Statut", "Alias reconnus (dictionnaire)", "Aper\u00e7u valeurs"),
      options = list(
        pageLength = 15, scrollX = TRUE, dom = "frtip",
        language = list(search = "Rechercher :"),
        columnDefs = list(
          list(width = "170px", targets = 0),
          list(width = "120px", targets = 1),
          list(width = "130px", targets = 2),
          list(width = "100px", targets = 3),
          list(width = "320px", targets = 4),
          list(width = "140px", targets = 5)
        )
      ),
      class = "compact stripe hover"
    ) |>
      formatStyle("Statut",
        backgroundColor = styleEqual(
          c("\u2705 D\u00e9tect\u00e9", "\u274c Non trouv\u00e9"),
          c("#f0fff4", "#fff5f5")
        ),
        fontWeight = "600", fontSize = "12px"
      )
  })

  output$mapper_col1 <- renderUI({
    req(raw_data())
    keys1 <- names(mapper_defs)[1:8]
    tagList(
      tags$div(class="section-header","Variables nutritionnelles et démographiques"),
      lapply(keys1, function(k) make_mapper_row(k, mapper_defs[[k]]))
    )
  })

  output$mapper_col2 <- renderUI({
    req(raw_data())
    keys2 <- names(mapper_defs)[9:length(mapper_defs)]
    tagList(
      tags$div(class="section-header","Variables géographiques, PAM et déplacement"),
      lapply(keys2, function(k) make_mapper_row(k, mapper_defs[[k]]))
    )
  })

  output$import_preview <- renderDT({
    df <- raw_data(); req(df)
    df |> head(15) |>
      datatable(rownames=FALSE,
                options=list(dom="t",scrollX=TRUE,pageLength=15),
                class="compact stripe")
  })

  output$import_summary <- renderUI({
    df <- raw_data(); req(df); m <- map_cols()
    detected <- sum(!sapply(m,is.null))
    tags$div(
      tags$div(class="stat-card",
        tags$p(class="sc-val", format(nrow(df),big.mark=" ")),
        tags$p(class="sc-lbl","Observations")),
      tags$div(class="stat-card",
        tags$p(class="sc-val", ncol(df)),
        tags$p(class="sc-lbl","Variables")),
      tags$div(class="stat-card",
        tags$p(class="sc-val", paste0(detected,"/",length(OMS_DICT))),
        tags$p(class="sc-lbl","Colonnes OMS détectées")),
      tags$div(class="stat-card",
        tags$p(class="sc-val",
          paste0(round(mean(sapply(df,function(x) mean(is.na(x))))*100,1),"%")),
        tags$p(class="sc-lbl","Valeurs manquantes (moy.)")),
      tags$p(style="font-size:12px;color:#888;margin-top:8px;",
        tags$b("Num. : "), sum(sapply(df,is.numeric)), " | ",
        tags$b("Catég. : "), sum(sapply(df,function(x)is.factor(x)||is.character(x)))
      )
    )
  })

  # ── CONFORMITÉ OMS ─────────────────────────────────────────────────────────
  # Reactive : corrections manuelles (key = colonne, value = variable OMS)
  oms_manual_fixes <- reactiveVal(list())

  # Reactive : résultat du check de conformité
  oms_check_data <- reactive({
    df <- raw_data(); req(df)
    check <- oms_conformity_check(df)
    # Appliquer les corrections manuelles
    fixes <- oms_manual_fixes()
    for (col in names(fixes)) {
      idx <- which(check$colonne == col)
      if (length(idx) == 1) {
        key <- fixes[[col]]
        meta <- if (key %in% names(OMS_META)) OMS_META[[key]]
                else list(cat = "", label = key, unit = "", seuils = "", type = "")
        check$variable_oms[idx]  <- key
        check$label_oms[idx]     <- meta$label
        check$categorie_oms[idx] <- meta$cat
        check$confiance[idx]     <- "manual"
        check$type_attendu[idx]  <- meta$type
        check$unite[idx]         <- meta$unit
        check$seuils[idx]        <- meta$seuils
      }
    }
    check
  })

  # Score global de conformité
  output$oms_conform_score <- renderUI({
    check <- oms_check_data(); req(check)
    n_total  <- nrow(check)
    n_high   <- sum(check$confiance == "high", na.rm = TRUE)
    n_medium <- sum(check$confiance == "medium", na.rm = TRUE)
    n_manual <- sum(check$confiance == "manual", na.rm = TRUE)
    n_none   <- sum(check$confiance == "none", na.rm = TRUE)
    n_detected <- n_high + n_medium + n_manual
    pct <- round(n_detected / n_total * 100, 0)

    # Couleur selon le score
    score_col <- if (pct >= 70) "#2E7D52" else if (pct >= 40) "#D98B14" else "#BF3636"

    tags$div(style = "display:flex;gap:16px;flex-wrap:wrap;margin:12px 0;",
      # Score principal
      tags$div(
        style = sprintf("background:white;border-radius:12px;padding:16px 24px;
                         border-left:5px solid %s;box-shadow:0 2px 8px rgba(0,0,0,.06);
                         min-width:180px;text-align:center;", score_col),
        tags$div(style = sprintf("font-size:36px;font-weight:800;color:%s;", score_col),
                 paste0(n_detected, "/", n_total)),
        tags$div(style = "font-size:12px;color:#64748b;font-weight:600;",
                 paste0("Variables conformes OMS (", pct, "%)"))
      ),
      # Détail par confiance
      tags$div(style = "background:white;border-radius:12px;padding:16px 20px;
                         box-shadow:0 2px 8px rgba(0,0,0,.06);flex:1;min-width:300px;",
        tags$div(style="display:flex;gap:20px;flex-wrap:wrap;",
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#00838F;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_high, " détection certaine"))
          ),
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#f59e0b;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_medium, " détection probable"))
          ),
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#6366f1;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_manual, " assignation manuelle"))
          ),
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#dc2626;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_none, " non reconnu"))
          )
        )
      )
    )
  })

  # Tableau de conformité interactif
  output$oms_conform_table <- renderDT({
    check <- oms_check_data(); req(check)

    # Colonnes d'affichage
    conf_label <- function(c) switch(c,
      "high"   = paste0("\u2705 Certaine"),
      "medium" = paste0("\u26a0\ufe0f Probable"),
      "manual" = paste0("\u2714 Manuelle"),
      paste0("\u274c Non reconnu")
    )
    em_dash <- "\u2014"
    disp <- data.frame(
      Colonne       = check$colonne,
      Type          = check$type_r,
      Variable.OMS  = ifelse(is.na(check$variable_oms), em_dash,
                             paste0(check$variable_oms, " ", em_dash, " ", check$label_oms)),
      Categorie     = ifelse(check$categorie_oms == "", em_dash, check$categorie_oms),
      Confiance     = vapply(check$confiance, conf_label, character(1)),
      Seuils        = ifelse(check$seuils == "", em_dash, check$seuils),
      check = FALSE, stringsAsFactors = FALSE
    )

    datatable(disp, rownames = FALSE, selection = "single",
      options = list(
        pageLength = 20, scrollX = TRUE,
        dom = "frtip",
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ lignes",
          info = "_START_ \u00e0 _END_ sur _TOTAL_ colonnes",
          paginate = list(`previous` = "Pr\u00e9c\u00e9dent", `next` = "Suivant")
        ),
        columnDefs = list(
          list(width = "160px", targets = 0),
          list(width = "70px",  targets = 1),
          list(width = "280px", targets = 2),
          list(width = "130px", targets = 3),
          list(width = "110px", targets = 4)
        )
      ),
      class = "compact stripe hover"
    ) |>
      formatStyle("Confiance",
        backgroundColor = styleEqual(
          c("\u2705 Certaine", "\u26a0\ufe0f Probable", "\u2714 Manuelle", "\u274c Non reconnu"),
          c("#f0fff4", "#fffbeb", "#f0f0ff", "#fff5f5")
        ),
        fontWeight = "600", fontSize = "12px"
      )
  })

  # Mettre à jour les dropdown de correction
  observe({
    df <- raw_data(); req(df)
    check <- oms_check_data()
    # Colonnes non reconnues ou probables
    cols_to_fix <- check$colonne[check$confiance %in% c("none", "medium")]
    all_cols <- check$colonne
    updateSelectInput(session, "oms_fix_col",
                      choices = c("-- Choisir --" = "", all_cols),
                      selected = "")

    # Variables OMS disponibles (avec labels)
    oms_choices <- setNames(
      names(OMS_META),
      sapply(names(OMS_META), function(k) paste0(k, " \u2014 ", OMS_META[[k]]$label))
    )
    updateSelectInput(session, "oms_fix_var",
                      choices = c("-- Variable OMS --" = "", oms_choices),
                      selected = "")
  })

  # Appliquer une correction manuelle
  observeEvent(input$oms_fix_apply, {
    req(input$oms_fix_col != "", input$oms_fix_var != "")
    fixes <- oms_manual_fixes()
    fixes[[input$oms_fix_col]] <- input$oms_fix_var
    oms_manual_fixes(fixes)

    # Mettre aussi à jour le map_cols pour la cohérence
    m <- map_cols()
    if (!is.null(m)) {
      m[[input$oms_fix_var]] <- input$oms_fix_col
      map_cols(m)
    }

    showNotification(
      sprintf("\u2705 %s \u2192 %s (%s)",
              input$oms_fix_col, input$oms_fix_var,
              OMS_META[[input$oms_fix_var]]$label),
      type = "message", duration = 4
    )
  })

  # Réinitialiser les corrections
  observeEvent(input$oms_fix_reset, {
    oms_manual_fixes(list())
    showNotification("\u21A9 Corrections manuelles réinitialisées", type = "warning", duration = 3)
  })

  # Clic sur une ligne du tableau → pré-remplir le dropdown
  observeEvent(input$oms_conform_table_rows_selected, {
    check <- oms_check_data()
    sel <- input$oms_conform_table_rows_selected
    req(length(sel) == 1)
    updateSelectInput(session, "oms_fix_col", selected = check$colonne[sel])
  })

  # ── Panneau détail dictionnaire (clic sur ligne conformité) ──────────
  output$dict_selected_aliases <- renderUI({
    check <- oms_check_data()
    sel   <- input$oms_conform_table_rows_selected
    req(length(sel) == 1)

    col_name <- check$colonne[sel]
    var_oms  <- check$variable_oms[sel]
    df       <- raw_data(); req(df)

    # Aperçu des valeurs
    sample_vals <- if (col_name %in% names(df)) {
      vals <- head(na.omit(df[[col_name]]), 8)
      paste(vals, collapse = " \u2502 ")
    } else "\u2014"

    # Badges des alias
    aliases_html <- if (!is.na(var_oms) && var_oms %in% names(OMS_DICT)) {
      aliases <- OMS_DICT[[var_oms]]
      tags$div(style = "display:flex;flex-wrap:wrap;gap:4px;",
        lapply(aliases, function(a) {
          is_match <- tolower(a) == tolower(col_name)
          tags$span(
            style = sprintf(
              "display:inline-block;padding:3px 10px;border-radius:14px;font-size:11px;%s",
              if (is_match) "background:#00838F;color:white;font-weight:700;"
              else "background:#e2e8f0;color:#475569;"
            ), a
          )
        })
      )
    } else {
      # Pas de variable OMS : montrer toutes les variables possibles du dictionnaire
      tags$div(
        tags$p(style = "color:#888;font-size:12px;margin-bottom:6px;",
               "Aucune variable OMS d\u00e9tect\u00e9e. Voici les variables disponibles dans le dictionnaire :"),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:4px;max-height:200px;overflow-y:auto;",
          lapply(names(OMS_DICT), function(k) {
            lbl <- if (k %in% names(OMS_META)) OMS_META[[k]]$label else k
            tags$span(
              style = "display:inline-block;padding:3px 10px;border-radius:14px;
                       font-size:11px;background:#f0f4ff;color:#3b5998;cursor:default;",
              paste0(k, " (", lbl, ")")
            )
          })
        )
      )
    }

    tags$div(
      style = "background:white;border:1px solid #e2e8f0;border-radius:10px;
               padding:16px 20px;margin-top:14px;box-shadow:0 2px 8px rgba(0,0,0,.05);",
      tags$div(style = "display:flex;align-items:center;gap:10px;margin-bottom:12px;",
        tags$div(
          style = "width:36px;height:36px;border-radius:8px;
                   background:linear-gradient(135deg,#0077B6,#00838F);
                   display:flex;align-items:center;justify-content:center;",
          icon("search-plus", style = "color:white;font-size:16px;")
        ),
        tags$h5(style = "margin:0;color:#1e293b;font-weight:700;font-size:14px;",
                paste0("D\u00e9tail : ", col_name,
                       if (!is.na(var_oms)) paste0(" \u2192 ", var_oms) else ""))
      ),
      fluidRow(
        column(4,
          tags$div(style = "font-weight:600;color:#475569;font-size:12px;margin-bottom:6px;",
                   "Valeurs \u00e9chantillon de votre colonne :"),
          tags$div(style = "font-size:12px;color:#1e293b;background:#f8fafc;
                            padding:8px 12px;border-radius:6px;word-break:break-all;",
                   sample_vals)
        ),
        column(8,
          tags$div(style = "font-weight:600;color:#475569;font-size:12px;margin-bottom:6px;",
                   if (!is.na(var_oms))
                     paste0("Alias reconnus pour \u00ab ", var_oms, " \u00bb dans le dictionnaire :")
                   else "Variables du dictionnaire OMS :"),
          aliases_html
        )
      )
    )
  })

  # ── EXPLORATION ─────────────────────────────────────────────────────────────
  output$explor_cols_ui <- renderUI({
    df <- df_f(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("explor_cols","Colonnes à afficher :",
                   choices=vis, selected=head(vis,8),
                   multiple=TRUE, width="100%")
  })

  output$explor_table <- renderDT({
    req(input$explor_cols)
    df_f() |> select(any_of(input$explor_cols)) |>
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=input$explor_n, scrollX=TRUE, dom="frtip"),
                class="compact stripe hover")
  })

  observe({
    vn <- vars_num()
    updateSelectInput(session,"explor_var", choices=vn, selected=vn[1])
  })

  output$explor_stats <- renderTable({
    req(input$explor_var); x <- df_f()[[input$explor_var]]
    if (is.numeric(x)) {
      v <- na.omit(x)
      sw <- tryCatch(shapiro.test(v[1:min(5000,length(v))]), error=function(e) NULL)
      jb_p <- tryCatch({
        n <- length(v); m3 <- mean((v-mean(v))^3); m4 <- mean((v-mean(v))^4)
        s <- m3/(sd(v)^3); k <- m4/(sd(v)^4)-3
        jb_stat <- n*(s^2/6 + k^2/24)
        round(pchisq(jb_stat, df=2, lower.tail=FALSE), 4)
      }, error=function(e) NA)
      tibble(
        Mesure = c(
          "── TENDANCE CENTRALE ──","Moyenne (μ)","Médiane","Mode","Moyenne géométrique","Moyenne harmonique",
          "── DISPERSION ──","Écart-type (σ)","Variance (σ²)","CV (%)","IQR","Étendue","Écart absolu moyen",
          "── POSITION ──","Min","Q1 (25%)","Q2 (50%)","Q3 (75%)","Max","P5","P10","P90","P95",
          "── FORME ──","Asymétrie (Skewness)","Aplatissement (Kurtosis)","Kurtosis excès",
          "── NORMALITÉ ──","Shapiro-Wilk W","Shapiro-Wilk p","Jarque-Bera p","Distribution",
          "── DONNÉES ──","N valides","N manquants","% manquants"
        ),
        Valeur = c(
          "","round(mean(v),4)","round(median(v),4)",
          as.character(round(as.numeric(e1071::moment(v,order=1,center=FALSE)),4)),
          round(exp(mean(log(v[v>0]))),4),
          round(length(v)/sum(1/v[v!=0]),4),
          "",round(sd(v),4),round(var(v),4),round(sd(v)/mean(v)*100,2),
          round(IQR(v),4),round(diff(range(v)),4),round(mean(abs(v-mean(v))),4),
          "",round(min(v),4),round(quantile(v,.25),4),round(median(v),4),
          round(quantile(v,.75),4),round(max(v),4),
          round(quantile(v,.05),4),round(quantile(v,.10),4),
          round(quantile(v,.90),4),round(quantile(v,.95),4),
          "",round(e1071::skewness(v),4),round(e1071::kurtosis(v)+3,4),round(e1071::kurtosis(v),4),
          "",if(!is.null(sw))round(sw$statistic,4) else "N/A",
          if(!is.null(sw))round(sw$p.value,4) else "N/A",
          if(!is.na(jb_p))jb_p else "N/A",
          if(!is.null(sw)) if(sw$p.value<.05)"❌ Non normale (p<0.05)" else "✅ Normale (p≥0.05)" else "N/A",
          "",length(v),sum(is.na(x)),round(mean(is.na(x))*100,1)
        )
      ) |> filter(Mesure != "" | Valeur != "")
    } else {
      cnt <- df_f() |> count(.data[[input$explor_var]]) |>
        mutate(Pct=round(n/sum(n)*100,1), Cum=cumsum(Pct))
      tibble(Modalité=as.character(cnt[[input$explor_var]]),
             N=cnt$n, `%`=cnt$Pct, `% cumulé`=cnt$Cum)
    }
  }, striped=TRUE, hover=TRUE, bordered=TRUE, spacing="xs")

  output$explor_auto_plot <- renderPlotly({
    req(input$explor_var); d <- df_f(); v <- input$explor_var
    x <- d[[v]]
    tp <- var_type(x)
    if (tp %in% c("continue","discrete")) {
      m_v <- mean(x,na.rm=TRUE); s_v <- sd(x,na.rm=TRUE); md_v <- median(x,na.rm=TRUE)
      p <- ggplot(d, aes(x=.data[[v]])) +
        geom_histogram(aes(y=after_stat(density)),bins=28,fill=gprimary(),color="white",alpha=0.82)+
        stat_function(fun=dnorm,args=list(mean=m_v,sd=s_v),color="black",linewidth=1.1)+
        geom_vline(xintercept=m_v,color=gsecondary(),linewidth=1,linetype="dashed")+
        geom_vline(xintercept=md_v,color=C$orange,linewidth=1,linetype="dotted")+
        labs(x=v,y="Densité",
             subtitle=paste0("μ=",round(m_v,3)," | σ=",round(s_v,3)," | Méd=",round(md_v,3)),
             caption="Rouge=moyenne | Orange=médiane | Courbe=N(μ,σ)")+
        theme_nutri()
    } else {
      cnt <- d |> count(.data[[v]]) |> mutate(pct=round(n/sum(n)*100,1))
      cnt <- smart_top_n(cnt, v, "n", 25)
      niv <- nrow(cnt)
      cnt[[v]] <- truncate_label(cnt[[v]], 22)
      p <- ggplot(cnt,aes(y=fct_reorder(.data[[v]],n),x=n,fill=.data[[v]],
                          text=paste0(.data[[v]],"\nN=",n," (",pct,"%)"))) +
        geom_col(alpha=0.88,width=0.7)+
        scale_fill_manual(values=gpal(niv,"cat"))+
        labs(y="",x="Effectif")+
        theme_nutri()+theme(legend.position="none")
    }
    ggplotly(p, tooltip=if(tp %in% c("continue","discrete")) NULL else "text") |>
      layout(hovermode="x unified") |> nutri_config()
  })

  output$dl_csv <- downloadHandler(
    filename=function() paste0("data_",Sys.Date(),".csv"),
    content=function(f) write.csv(df_f()|>select(!starts_with(".")),f,row.names=FALSE)
  )
  output$dl_xlsx <- downloadHandler(
    filename=function() paste0("data_",Sys.Date(),".xlsx"),
    content=function(f) write_xlsx(df_f()|>select(!starts_with(".")),f)
  )

  # ── Statistiques complètes par sections ───────────────────────────────────
  observe({
    all_v <- c(vars_num(), vars_cat())
    updateSelectInput(session,"explor_var", choices=all_v,
                      selected=if(length(vars_num())>0) vars_num()[1] else all_v[1])
  })

# [DEAD CODE] explor_central — output serveur sans UI correspondante
#   output$explor_central <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     tibble(
#       Mesure = c("Moyenne arithmétique","Médiane (Q2)","Mode statistique",
#                  "Moyenne géométrique","Moyenne harmonique","Moyenne tronquée 5%"),
#       Valeur = c(round(mean(x),4), round(median(x),4),
#                  round(as.numeric(mode_stat(x)),4),
#                  round(exp(mean(log(abs(x[x!=0])))),4),
#                  round(length(x[x!=0])/sum(1/x[x!=0]),4),
#                  round(mean(x,trim=0.05),4))
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs", digits=4)

# [DEAD CODE] explor_dispersion — output serveur sans UI correspondante
#   output$explor_dispersion <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     tibble(
#       Mesure = c("Écart-type (s)","Variance (s²)","CV (%)","Étendue",
#                  "IQR (Q3-Q1)","MAD (écart absolu médian)",
#                  "Erreur standard (SE)","IC95 bas","IC95 haut"),
#       Valeur = c(round(sd(x),4), round(var(x),4),
#                  round(sd(x)/mean(x)*100,2),
#                  round(max(x)-min(x),4), round(IQR(x),4),
#                  round(mad(x),4),
#                  round(sd(x)/sqrt(length(x)),4),
#                  round(mean(x)-1.96*sd(x)/sqrt(length(x)),4),
#                  round(mean(x)+1.96*sd(x)/sqrt(length(x)),4))
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs", digits=4)

# [DEAD CODE] explor_forme — output serveur sans UI correspondante
#   output$explor_forme <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     sw   <- tryCatch(shapiro.test(x[1:min(5000,length(x))]), error=function(e) NULL)
#     asym <- e1071::skewness(x)
#     kurt <- e1071::kurtosis(x)
#     tibble(
#       Mesure = c("Asymétrie (Skewness)","Interprétation skewness",
#                  "Aplatissement (Kurtosis)","Interprétation kurtosis",
#                  "Shapiro-Wilk W","Shapiro-Wilk p-value","Distribution"),
#       Valeur = c(round(asym,4),
#                  if(asym>0.5)"Asymétrie droite (+)" else if(asym< -0.5)"Asymétrie gauche (-)" else "Symétrique (≈0)",
#                  round(kurt,4),
#                  if(kurt>0)"Leptokurtique (queues lourdes)" else if(kurt<0)"Platykurtique (queues légères)" else "Mésokurtique (≈normale)",
#                  if(!is.null(sw)) round(sw$statistic,4) else "N/A",
#                  if(!is.null(sw)) round(sw$p.value,4)   else "N/A",
#                  if(!is.null(sw)) if(sw$p.value<.05)"❌ Non normale → Tests non param." else "✅ Normale → Tests param. possibles" else "N/A")
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs")

# [DEAD CODE] explor_quantiles — output serveur sans UI correspondante
#   output$explor_quantiles <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     q <- quantile(x, probs=c(.01,.05,.10,.25,.50,.75,.90,.95,.99))
#     tibble(
#       Quantile = c("P1 (1%)","P5 (5%)","P10 (10%)","Q1 — 25%",
#                    "Q2 — Médiane (50%)","Q3 — 75%",
#                    "P90 (90%)","P95 (95%)","P99 (99%)"),
#       Valeur   = round(q, 4)
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs", digits=4)

# [DEAD CODE] explor_boxplot_dist — output serveur sans UI correspondante
#   output$explor_boxplot_dist <- renderPlotly({
#     req(input$explor_var); d <- df_f(); v <- input$explor_var
#     req(is.numeric(d[[v]]))
#     p <- ggplot(d, aes(x="", y=.data[[v]])) +
#       geom_violin(fill=gprimary(), alpha=.35, trim=FALSE, color=C$violet) +
#       geom_boxplot(fill="white", color="#2c3e50", width=.15,
#                    outlier.shape=21, outlier.color=C$rouge) +
#       stat_summary(fun=mean, geom="point", shape=18, size=4, color=C$rouge) +
#       coord_flip() +
#       labs(x="", y=v, title="Violin + Boxplot — Distribution complète") +
#       theme_minimal(base_size=11)
#     ggplotly(p) |> nutri_config()
#   })

# [DEAD CODE] manip_filter_ui — output serveur sans UI correspondante
#   # ── Manipulation dplyr ────────────────────────────────────────────────────
#   output$manip_filter_ui <- renderUI({
#     df <- df_f(); req(df)
#     vn <- vars_num(); vc <- vars_cat()
#     tagList(
#       selectInput("manip_f_var","Variable à filtrer :",
#                   choices=c("(aucun)"="", c(vn,vc)), width="100%"),
#       uiOutput("manip_f_cond_ui")
#     )
#   })
  output$manip_f_cond_ui <- renderUI({
    req(input$manip_f_var, input$manip_f_var != "")
    d <- df_f(); v <- input$manip_f_var
    if (is.numeric(d[[v]])) {
      r <- range(d[[v]], na.rm=TRUE)
      sliderInput("manip_f_range", paste0("Plage [",v,"] :"),
                  min=floor(r[1]), max=ceiling(r[2]),
                  value=c(floor(r[1]),ceiling(r[2])), width="100%")
    } else {
      vals <- sort(unique(na.omit(as.character(d[[v]]))))
      checkboxGroupInput("manip_f_cats", paste0("Valeurs [",v,"] :"),
                         choices=vals, selected=vals)
    }
  })
# [DEAD CODE] manip_select_ui — output serveur sans UI correspondante
#   output$manip_select_ui <- renderUI({
#     df <- df_f(); req(df)
#     vis <- names(df)[!startsWith(names(df),".")]
#     selectizeInput("manip_sel_cols","Colonnes à garder :",
#                    choices=vis, selected=head(vis,8), multiple=TRUE, width="100%")
#   })
# [DEAD CODE] manip_arrange_ui — output serveur sans UI correspondante
#   output$manip_arrange_ui <- renderUI({
#     vn <- c(vars_num(), vars_cat())
#     selectInput("manip_sort_var","Trier par :",
#                 choices=c("(aucun)"="", vn), width="100%")
#   })
# [DEAD CODE] manip_group_ui — output serveur sans UI correspondante
#   output$manip_group_ui <- renderUI({
#     vc <- vars_cat()
#     selectInput("manip_grp","Grouper par :",
#                 choices=c("(aucun)"="", vc), width="100%")
#   })
# [DEAD CODE] manip_summarise_ui — output serveur sans UI correspondante
#   output$manip_summarise_ui <- renderUI({
#     vn <- vars_num()
#     tagList(
#       selectInput("manip_sum_var","Variable à résumer :", choices=vn, width="100%"),
#       checkboxGroupInput("manip_sum_fns","Fonctions :",
#         choices=c("Moyenne"="mean","Médiane"="median","Écart-type"="sd",
#                   "Min"="min","Max"="max","N"="n","% manquants"="na_pct"),
#         selected=c("mean","median","sd","n"), inline=TRUE)
#     )
#   })

# [DEAD CODE] manip_code_preview — output serveur sans UI correspondante
#   output$manip_code_preview <- renderText({
#     lines <- c("df_manipule <- df")
#     if (!is.null(input$manip_f_var) && input$manip_f_var != "") {
#       v <- input$manip_f_var; d <- df_f()
#       if (is.numeric(d[[v]]) && !is.null(input$manip_f_range))
#         lines <- c(lines, paste0("  |> filter(",v," >= ",input$manip_f_range[1],
#                                   " & ",v," <= ",input$manip_f_range[2],")"))
#       else if (!is.null(input$manip_f_cats) && length(input$manip_f_cats)>0)
#         lines <- c(lines, paste0('  |> filter(',v,' %in% c(',
#                                   paste0('"',input$manip_f_cats,'"',collapse=","),'))'))
#     }
#     if (!is.null(input$manip_sel_cols) && length(input$manip_sel_cols)>0)
#       lines <- c(lines, paste0("  |> select(",paste(input$manip_sel_cols,collapse=", "),")"))
#     if (!is.null(input$manip_sort_var) && input$manip_sort_var != "")
#       lines <- c(lines, paste0("  |> arrange(",
#         if(isTRUE(input$manip_desc)) paste0("desc(",input$manip_sort_var,")")
#         else input$manip_sort_var,")"))
#     if (!is.null(input$manip_grp) && input$manip_grp != "" &&
#         !is.null(input$manip_sum_var)) {
#       fns <- input$manip_sum_fns; v2 <- input$manip_sum_var
#       parts <- c()
#       if("mean"  %in%fns) parts<-c(parts,paste0("moy=mean(",v2,",na.rm=T)"))
#       if("median"%in%fns) parts<-c(parts,paste0("med=median(",v2,",na.rm=T)"))
#       if("sd"    %in%fns) parts<-c(parts,paste0("sd=sd(",v2,",na.rm=T)"))
#       if("min"   %in%fns) parts<-c(parts,paste0("min=min(",v2,",na.rm=T)"))
#       if("max"   %in%fns) parts<-c(parts,paste0("max=max(",v2,",na.rm=T)"))
#       if("n"     %in%fns) parts<-c(parts,"n=n()")
#       if("na_pct"%in%fns) parts<-c(parts,paste0("pct_na=mean(is.na(",v2,"))*100"))
#       lines <- c(lines, paste0("  |> group_by(",input$manip_grp,")"),
#                  paste0("  |> summarise(",paste(parts,collapse=",
#               "),")"))
#     }
#     paste(lines, collapse="
# ")
#   })

  manip_result <- reactive({
    d <- df_f()
    tryCatch({
      if (!is.null(input$manip_f_var) && input$manip_f_var != "") {
        v <- input$manip_f_var
        if (is.numeric(d[[v]]) && !is.null(input$manip_f_range))
          d <- filter(d, .data[[v]]>=input$manip_f_range[1] & .data[[v]]<=input$manip_f_range[2])
        else if (!is.null(input$manip_f_cats))
          d <- filter(d, as.character(.data[[v]]) %in% input$manip_f_cats)
      }
      if (!is.null(input$manip_sel_cols) && length(input$manip_sel_cols)>0)
        d <- select(d, any_of(input$manip_sel_cols))
      if (!is.null(input$manip_sort_var) && input$manip_sort_var != "")
        d <- if(isTRUE(input$manip_desc)) arrange(d,desc(.data[[input$manip_sort_var]]))
             else arrange(d,.data[[input$manip_sort_var]])
      if (!is.null(input$manip_grp) && input$manip_grp != "" &&
          input$manip_grp %in% names(d) &&
          !is.null(input$manip_sum_var) && input$manip_sum_var %in% names(d)) {
        v2 <- input$manip_sum_var; fns <- input$manip_sum_fns
        d <- d |> group_by(.data[[input$manip_grp]]) |>
          summarise(
            N               = n(),
            Moyenne         = if("mean"  %in%fns) round(mean(.data[[v2]],na.rm=T),3) else NA,
            Médiane         = if("median"%in%fns) round(median(.data[[v2]],na.rm=T),3) else NA,
            `Écart-type`    = if("sd"    %in%fns) round(sd(.data[[v2]],na.rm=T),3) else NA,
            Min             = if("min"   %in%fns) round(min(.data[[v2]],na.rm=T),3) else NA,
            Max             = if("max"   %in%fns) round(max(.data[[v2]],na.rm=T),3) else NA,
            `% manquants`   = if("na_pct"%in%fns) round(mean(is.na(.data[[v2]]))*100,1) else NA,
            .groups="drop") |>
          select(where(~!all(is.na(.))))
      }
      d
    }, error=function(e) df_f())
  })

  observeEvent(input$manip_add_var, {
    req(nchar(input$manip_newvar_name)>0, nchar(input$manip_newvar_expr)>0)
    d <- df_f()
    tryCatch({
      d <- safe_mutate(d, input$manip_newvar_name, input$manip_newvar_expr)
      mapped_data(d)
      showNotification(paste0("✅ Variable «",input$manip_newvar_name,"» créée"),
                       type="message", duration=3)
    }, error=function(e)
      showNotification(paste("❌",e$message), type="error", duration=5))
  })

  observeEvent(input$manip_reset, {
    df_reset <- tryCatch(calc_indicateurs(raw_data(), map_cols()), error=function(e) raw_data())
    df_reset <- tryCatch(ensure_sa_columns(df_reset), error=function(e) df_reset)
    mapped_data(df_reset)
    showNotification("🔄 Données réinitialisées", type="message", duration=3)
  })

# [DEAD CODE] manip_table — output serveur sans UI correspondante
#   output$manip_table <- renderDT({
#     manip_result() |>
#       datatable(rownames=FALSE, filter="top",
#                 options=list(pageLength=12, scrollX=TRUE, dom="frtip"),
#                 class="compact stripe hover")
#   })

# [DEAD CODE] dl_manip_csv — output serveur sans UI correspondante
#   output$dl_manip_csv <- downloadHandler(
#     filename=function() paste0("data_dplyr_",Sys.Date(),".csv"),
#     content=function(f) write.csv(manip_result(), f, row.names=FALSE)
#   )
# [DEAD CODE] dl_manip_xlsx — output serveur sans UI correspondante
#   output$dl_manip_xlsx <- downloadHandler(
#     filename=function() paste0("data_dplyr_",Sys.Date(),".xlsx"),
#     content=function(f) writexl::write_xlsx(manip_result(), f)
#   )

# [DEAD CODE] adv_filter_num_ui — output serveur sans UI correspondante
#   # ── Filtres avancés ───────────────────────────────────────────────────────
#   output$adv_filter_num_ui <- renderUI({
#     vn <- head(vars_num(),4); d <- df_f()
#     lapply(vn, function(v) {
#       r <- range(d[[v]], na.rm=TRUE)
#       sliderInput(paste0("adv_num_",v), paste0(v," :"),
#                   min=floor(r[1]), max=ceiling(r[2]),
#                   value=c(floor(r[1]),ceiling(r[2])), width="100%")
#     })
#   })
# [DEAD CODE] adv_filter_cat_ui — output serveur sans UI correspondante
#   output$adv_filter_cat_ui <- renderUI({
#     vc <- head(vars_cat(),3); d <- df_f()
#     lapply(vc, function(v) {
#       vals <- sort(unique(na.omit(as.character(d[[v]]))))
#       if(length(vals)<=12)
#         checkboxGroupInput(paste0("adv_cat_",v), paste0(v," :"),
#                            choices=vals, selected=vals, inline=TRUE)
#       else
#         selectizeInput(paste0("adv_cat_",v), paste0(v," :"),
#                        choices=vals, selected=vals, multiple=TRUE, width="100%")
#     })
#   })
# [DEAD CODE] adv_filter_na_ui — output serveur sans UI correspondante
#   output$adv_filter_na_ui <- renderUI({
#     checkboxInput("adv_excl_na","Exclure les lignes avec valeurs manquantes",FALSE)
#   })

  adv_result <- eventReactive(input$adv_apply, {
    d <- df_f()
    tryCatch({
      for (v in head(vars_num(),4)) {
        id <- paste0("adv_num_",v)
        if (!is.null(input[[id]]))
          d <- filter(d, is.na(.data[[v]]) |
                        (.data[[v]]>=input[[id]][1] & .data[[v]]<=input[[id]][2]))
      }
      for (v in head(vars_cat(),3)) {
        id <- paste0("adv_cat_",v)
        if (!is.null(input[[id]]) && length(input[[id]])>0)
          d <- filter(d, as.character(.data[[v]]) %in% input[[id]])
      }
      if (isTRUE(input$adv_excl_na)) d <- drop_na(d)
      d
    }, error=function(e) df_f())
  }, ignoreNULL=FALSE)

  adv_data_r <- reactive({
    if (is.null(input$adv_apply) || input$adv_apply==0) df_f()
    else adv_result()
  })

# [DEAD CODE] adv_filter_summary_ui — output serveur sans UI correspondante
#   output$adv_filter_summary_ui <- renderUI({
#     d_orig <- df_f(); d_filt <- adv_data_r()
#     n_orig <- nrow(d_orig); n_filt <- nrow(d_filt)
#     pct <- round((1-n_filt/n_orig)*100,1)
#     cls <- if(pct>50)"alerte-crit" else if(pct>20)"alerte-serieux" else "alerte-ok"
#     tags$div(class=paste("alerte-box",cls),
#       tags$b(paste0(format(n_filt,big.mark=" ")," / ",
#                     format(n_orig,big.mark=" ")," observations")),
#       tags$br(),
#       tags$span(style="font-size:12px;",paste0(pct,"% filtrées"))
#     )
#   })
# [DEAD CODE] adv_table — output serveur sans UI correspondante
#   output$adv_table <- renderDT({
#     adv_data_r() |>
#       datatable(rownames=FALSE, filter="top",
#                 options=list(pageLength=12, scrollX=TRUE, dom="frtip"),
#                 class="compact stripe hover")
#   })
# [DEAD CODE] adv_overview — output serveur sans UI correspondante
#   output$adv_overview <- renderPlotly({
#     d <- adv_data_r(); vn <- vars_num(); req(length(vn)>0)
#     v <- vn[1]
#     p <- ggplot(d, aes(x=.data[[v]])) +
#       geom_histogram(fill=gprimary(),color="white",alpha=.82,bins=25) +
#       labs(x=v,y="Effectif",title=paste0("Distribution de ",v," (données filtrées)")) +
#       theme_minimal(base_size=11)
#     ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()
#   })

  # ── UNIVARIÉ ────────────────────────────────────────────────────────────────
  output$uni_var_ui <- renderUI({
    type <- input$uni_type
    if (type %in% c("barplot","barplot_pct","pie","mosaic")) {
      ch <- vars_cat_pie()
      if (type %in% c("barplot","barplot_pct")) ch <- vars_cat()
      req(length(ch)>0)
      selectInput("uni_var","Variable :",choices=ch,selected=ch[1],width="100%")
    } else {
      vn <- vars_num(); req(length(vn)>0)
      selectInput("uni_var","Variable :",choices=vn,selected=vn[1],width="100%")
    }
  })

  output$uni_stats_tbl <- renderTable({
    req(input$uni_var); x <- df_f()[[input$uni_var]]
    if(is.numeric(x)) stats_complete(x)
    else {
      cnt <- df_f()|>count(.data[[input$uni_var]])|>mutate(pct=round(n/sum(n)*100,1))
      setNames(cnt, c("Modalité","N","%"))
    }
  }, striped=TRUE, bordered=TRUE, spacing="xs")

  make_uni_plotly <- function() {
    req(input$uni_var); d <- df_f(); v <- input$uni_var; type <- input$uni_type
    pal <- function(n) {
      up <- input$uni_pal
      if (is.null(up) || up == "Auto (Global)") return(gpal(n, "cat"))
      pal_fn_uni(n, up)
    }

    tryCatch({
      if (type == "barplot") {
        cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
        cnt <- smart_top_n(cnt, v, "n", 25)
        cnt[[v]] <- truncate_label(cnt[[v]], 22)
        p <- ggplot(cnt,aes(y=fct_reorder(.data[[v]],n),x=n,fill=.data[[v]],
                            text=paste0(.data[[v]],"\nN=",n," (",pct,"%)"))) +
          geom_col(alpha=.88,width=.7)+
          {if(input$uni_lbl) geom_text(aes(label=paste0(n," (",pct,"%)")),
                                       hjust=-0.08,size=3)}+
          scale_fill_manual(values=pal(nrow(cnt)))+
          labs(y="",x="Effectif",title=paste("Distribution de",v))+
          theme_nutri()+theme(legend.position="none")
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "barplot_pct") {
        cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
        cnt <- smart_top_n(cnt, v, "n", 25)
        cnt[[v]] <- truncate_label(cnt[[v]], 22)
        p <- ggplot(cnt,aes(y=fct_reorder(.data[[v]],pct),x=pct,fill=.data[[v]],
                            text=paste0(.data[[v]],"\n",pct,"% (N=",n,")"))) +
          geom_col(alpha=.88,width=.7)+
          {if(input$uni_lbl) geom_text(aes(label=paste0(pct,"%")),hjust=-0.08,size=3)}+
          scale_fill_manual(values=pal(nrow(cnt)))+
          labs(y="",x="Proportion (%)",title=paste("Proportions —",v))+
          theme_nutri()+theme(legend.position="none")
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "pie") {
        cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
        plot_ly(cnt,labels=~.data[[v]],values=~n,type="pie",hole=0.45,
                textinfo="label+percent",
                marker=list(colors=pal(nrow(cnt)),line=list(color="white",width=2)),
                hovertemplate="%{label}<br>N=%{value}<br>%{percent}<extra></extra>")|>
          layout(showlegend=TRUE,legend=list(orientation="h",y=-0.15),
                 title=paste("Distribution de",v)) |> nutri_config()

      } else if (type == "histo") {
        m_v <- mean(d[[v]],na.rm=T);s_v <- sd(d[[v]],na.rm=T);md_v <- median(d[[v]],na.rm=T)
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_histogram(aes(y=after_stat(density)),bins=30,
                         fill=gprimary(),color="white",alpha=.82)+
          stat_function(fun=dnorm,args=list(mean=m_v,sd=s_v),color="black",linewidth=1.1)+
          geom_vline(xintercept=m_v,color=gsecondary(),linewidth=1,linetype="dashed")+
          geom_vline(xintercept=md_v,color=C$orange,linewidth=1,linetype="dotted")+
          labs(x=v,y="Densité",
               subtitle=paste0("μ=",round(m_v,3)," | σ=",round(s_v,3)),
               caption="Rouge=moyenne | Orange=médiane | Courbe=N(μ,σ)")+
          theme_nutri()
        ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()

      } else if (type == "boxplot") {
        q <- quantile(d[[v]],c(.25,.5,.75),na.rm=TRUE)
        p <- ggplot(d,aes(x="",y=.data[[v]]))+
          geom_boxplot(fill=gprimary(),color="#004f7c",alpha=.75,
                       outlier.shape=21,outlier.color=C$rouge,outlier.size=2,width=.4)+
          stat_summary(fun=mean,geom="point",shape=18,size=5,color=C$rouge)+
          annotate("text",x=1.35,y=q[1],label=paste0("Q1=",round(q[1],2)),size=3.5)+
          annotate("text",x=1.35,y=q[2],label=paste0("Méd=",round(q[2],2)),size=3.5)+
          annotate("text",x=1.35,y=q[3],label=paste0("Q3=",round(q[3],2)),size=3.5)+
          annotate("point",x=1,y=mean(d[[v]],na.rm=TRUE),color=C$rouge,size=4,shape=18)+
          labs(x="",y=v,subtitle=paste0("IQR=",round(q[3]-q[1],2),
                                         " | Étendue=",round(diff(range(d[[v]],na.rm=TRUE)),2),
                                         " | Moy=",round(mean(d[[v]],na.rm=TRUE),2)))+
          theme_nutri()
        ggplotly(p) |> nutri_config()

      } else if (type == "violin") {
        p <- ggplot(d,aes(x="",y=.data[[v]]))+
          geom_violin(fill=gprimary(),alpha=.45,trim=FALSE,color=C$violet,linewidth=.8)+
          geom_boxplot(fill="white",color="#2c3e50",width=.1,
                       outlier.shape=21,outlier.color=C$rouge)+
          stat_summary(fun=mean,geom="point",shape=18,size=4,color=C$rouge)+
          labs(x="",y=v)+theme_nutri()
        ggplotly(p) |> nutri_config()

      } else if (type == "density") {
        m_v <- mean(d[[v]],na.rm=TRUE)
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_density(fill=gprimary(),color=gprimary(),alpha=.38,linewidth=.9)+
          geom_area(stat="function",fun=dnorm,
                    args=list(mean=m_v,sd=sd(d[[v]],na.rm=TRUE)),
                    fill="transparent",color="black",linewidth=.8,linetype="dashed")+
          geom_vline(xintercept=m_v,color=gsecondary(),linewidth=1,linetype="dashed")+
          geom_vline(xintercept=median(d[[v]],na.rm=TRUE),
                     color=C$orange,linewidth=1,linetype="dotted")+
          labs(x=v,y="Densité",caption="Bleu=observé | Noir pointillé=N(μ,σ)")+
          theme_minimal(base_size=11)
        ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()

      } else if (type == "qqplot") {
        sw <- tryCatch(shapiro.test(na.omit(d[[v]])[1:min(5000,sum(!is.na(d[[v]])))]),
                       error=function(e) NULL)
        p <- ggplot(d,aes(sample=.data[[v]]))+
          stat_qq(color=gprimary(),alpha=.6,size=1.8)+
          stat_qq_line(color=C$rouge,linewidth=1)+
          labs(x="Quantiles théoriques",y="Quantiles observés",
               title=paste0("QQ-plot — ",v),
               subtitle=if(!is.null(sw))
                 paste0("Shapiro-Wilk: W=",round(sw$statistic,4),
                        "  p=",round(sw$p.value,4),
                        if(sw$p.value<.05)"  → NON NORMALE ❌" else "  → Normale ✅")
               else "")+
          theme_minimal(base_size=11)
        ggplotly(p) |> nutri_config()

      } else if (type == "oms_ref") {
        x_ref <- seq(min(d[[v]],na.rm=TRUE)-1, max(d[[v]],na.rm=TRUE)+1, length.out=300)
        ref_df <- tibble(x=x_ref, y=dnorm(x_ref,0,1))
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_density(aes(fill="Données observées",color="Données observées"),
                       alpha=.38,linewidth=.9)+
          geom_line(data=ref_df,aes(x=x,y=y,color="Référence OMS N(0,1)"),
                    linewidth=1,linetype="dashed",inherit.aes=FALSE)+
          scale_fill_manual(values=c("Données observées"=C$rouge,"Référence OMS N(0,1)"="transparent"))+
          scale_color_manual(values=c("Données observées"=C$rouge,"Référence OMS N(0,1)"="black"))+
          labs(x=v,y="Densité",
               subtitle="Décalage à gauche = prévalence élevée de malnutrition")+
          theme_minimal(base_size=11)+theme(legend.position="bottom")
        ggplotly(p) |> nutri_config()

      } else if (type == "histo_dens") {
        m_v <- mean(d[[v]],na.rm=TRUE); s_v <- sd(d[[v]],na.rm=TRUE)
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_histogram(aes(y=after_stat(density)),bins=28,
                         fill=gprimary(),color="white",alpha=.55)+
          geom_density(color=C$rouge,linewidth=1.2,fill=C$rouge,alpha=.2)+
          stat_function(fun=dnorm,args=list(mean=m_v,sd=s_v),
                        color="black",linewidth=.9,linetype="dashed")+
          labs(x=v,y="Densité")+theme_minimal(base_size=11)
        ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()

      } else if (type == "boxjitter") {
        p <- ggplot(d,aes(x="",y=.data[[v]]))+
          geom_boxplot(fill=gprimary(),color="#004f7c",alpha=.6,width=.35,
                       outlier.shape=NA)+
          geom_jitter(width=.12,height=0,alpha=.35,size=1.5,color=gprimary())+
          stat_summary(fun=mean,geom="point",shape=18,size=5,color=C$rouge)+
          labs(x="",y=v)+theme_minimal(base_size=11)
        ggplotly(p) |> nutri_config()

      } else {
        plotly_empty() |> layout(title=list(text="Sélectionnez un type de graphique",x=.5))
      }
    }, error=function(e)
      plotly_empty()|>layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  }

  output$uni_plot_ui <- renderUI({
    plotlyOutput("uni_plot_out", height="500px")
  })
  output$uni_plot_out <- renderPlotly({ make_uni_plotly() })

  output$uni_dl_plot <- downloadHandler(
    filename=function() paste0("graphique_",input$uni_var,"_",Sys.Date(),".png"),
    content=function(f) {
      req(input$uni_var); d <- df_f(); v <- input$uni_var
      ggsave(f, plot=last_plot(), width=12, height=8, dpi=300)
    }
  )

  # ── BIVARIÉ ─────────────────────────────────────────────────────────────────
  output$biv_x_ui <- renderUI({
    vc <- vars_cat(); req(length(vc)>0)
    selectInput("biv_x","Variable X / Groupe :",choices=vc,selected=vc[1],width="100%")
  })
  output$biv_y_ui <- renderUI({
    type <- input$biv_type
    if (type %in% c("dodge","stack","heatfreq","mosaic")) {
      vc <- vars_cat()
      selectInput("biv_y","Variable Y (catégorielle) :",
                  choices=vc, selected=if(length(vc)>1)vc[2] else vc[1], width="100%")
    } else if (type %in% c("scatter","hexbin","cormap")) {
      vn <- vars_num()
      tagList(
        selectInput("biv_y","Variable Y (continue) :",
                    choices=vn, selected=if(length(vn)>1)vn[2] else vn[1], width="100%"),
        selectInput("biv_z","Couleur par :",
                    choices=c("Aucune",vars_cat()), selected="Aucune", width="100%")
      )
    } else {
      vn <- vars_num()
      selectInput("biv_y","Variable continue :",choices=vn,selected=vn[1],width="100%")
    }
  })

  output$biv_test_result <- renderPrint({
    req(input$biv_x, input$biv_y); d <- df_f()
    tp_x <- var_type(d[[input$biv_x]])
    tp_y <- var_type(d[[input$biv_y]])
    res  <- auto_test(d, input$biv_x, input$biv_y, tp_x, tp_y)
    if (!is.null(res)) {
      cat("Test :", res$nom, "\n")
      if(!is.na(res$stat)) cat("Statistique :", res$stat, "\n")
      if(!is.na(res$df)) cat("ddl :", res$df, "\n")
      cat("p-valeur :", round(res$p,4), "\n\n")
      cat(res$interpretation)
    }
  })

  output$biv_interpretation <- renderUI({
    req(input$biv_x, input$biv_y); d <- df_f()
    tp_x <- var_type(d[[input$biv_x]])
    tp_y <- var_type(d[[input$biv_y]])
    res  <- auto_test(d, input$biv_x, input$biv_y, tp_x, tp_y)
    if (is.null(res)) return(NULL)
    cls  <- if(!is.na(res$p) && res$p < 0.05) "alerte-crit" else "alerte-ok"
    tags$div(class=paste("alerte-box",cls),
             style="margin-top:8px;font-size:12px;",
             res$interpretation)
  })

  output$biv_plot <- renderPlotly({
    req(input$biv_x, input$biv_y); d <- df_f()
    v1 <- input$biv_x; v2 <- input$biv_y; type <- input$biv_type
    niv1 <- length(unique(na.omit(d[[v1]])))

    gpalf <- function(n) {
      bp <- input$biv_pal
      if (is.null(bp) || bp == "Auto (Global)") return(gpal(n, "cat"))
      switch(bp,
        "Auto (OMS)" = gpal(n, "cat"),
        "Viridis"    = viridis::viridis(n),
        "Set2"       = RColorBrewer::brewer.pal(min(n, 8), "Set2"),
        "Spectral"   = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n),
        "RdYlGn"     = colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))(n),
        gpal(n, "cat")
      )
    }

    tryCatch({
      if (type == "dodge") {
        cnt <- d|>count(.data[[v1]],.data[[v2]])|>mutate(pct=round(n/sum(n)*100,1))
        niv2 <- length(unique(na.omit(d[[v2]])))
        cnt[[v1]] <- truncate_label(cnt[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(cnt,aes(y=.data[[v1]],x=n,fill=.data[[v2]],
            text=paste0(v1,"=",.data[[v1]],"\n",v2,"=",.data[[v2]],"\nN=",n," (",pct,")")))+
            geom_col(position="dodge",width=.72,alpha=.88)+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            labs(y=v1,x="Effectif")+theme_nutri()
        } else {
          p <- ggplot(cnt,aes(x=.data[[v1]],y=n,fill=.data[[v2]],
            text=paste0(v1,"=",.data[[v1]],"\n",v2,"=",.data[[v2]],"\nN=",n," (",pct,")")))+
            geom_col(position="dodge",width=.72,alpha=.88)+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            labs(x=v1,y="Effectif")+theme_nutri()+smart_axis_x(niv1)
        }
        ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.3)) |> nutri_config()

      } else if (type == "stack") {
        cnt <- d|>count(.data[[v1]],.data[[v2]])|>
          group_by(.data[[v1]])|>mutate(pct=round(n/sum(n)*100,1))|>ungroup()
        niv2 <- length(unique(na.omit(d[[v2]])))
        cnt[[v1]] <- truncate_label(cnt[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(cnt,aes(y=.data[[v1]],x=pct,fill=.data[[v2]],
            text=paste0(.data[[v1]]," | ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
            geom_col(position=position_fill(),width=.65,alpha=.88)+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            scale_x_continuous(labels=scales::percent)+
            labs(y=v1,x="Proportion")+theme_nutri()
        } else {
          p <- ggplot(cnt,aes(x=.data[[v1]],y=pct,fill=.data[[v2]],
            text=paste0(.data[[v1]]," | ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
            geom_col(position=position_fill(),width=.65,alpha=.88)+
            geom_text(aes(label=paste0(pct,"%")),
                      position=position_fill(vjust=.5),size=3,color="white",fontface="bold")+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            scale_y_continuous(labels=scales::percent)+
            labs(x=v1,y="Proportion")+theme_nutri()+smart_axis_x(niv1)
        }
        ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.3)) |> nutri_config()

      } else if (type == "heatfreq") {
        cnt <- d|>count(.data[[v1]],.data[[v2]])|>
          group_by(.data[[v1]])|>mutate(pct=round(n/sum(n)*100,1))|>ungroup()
        cnt[[v1]] <- truncate_label(cnt[[v1]], 18)
        cnt[[v2]] <- truncate_label(cnt[[v2]], 18)
        p <- ggplot(cnt,aes(x=.data[[v2]],y=.data[[v1]],fill=pct,
          text=paste0(.data[[v1]]," × ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
          geom_tile(color="white",linewidth=1.2)+
          geom_text(aes(label=paste0(pct,"%")),size=3.5,fontface="bold",color="white")+
          scale_fill_gradientn(colors=c("#f0f8ff",C$bleu,C$rouge),name="%")+
          labs(x=v2,y=v1)+theme_nutri()+
          smart_axis_x(length(unique(cnt[[v2]])))
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "boxgrp") {
        d[[v1]] <- truncate_label(d[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(d,aes(y=.data[[v1]],x=.data[[v2]],fill=.data[[v1]],
            text=paste0(.data[[v1]],"\n",v2,"=",round(.data[[v2]],2))))+
            geom_boxplot(alpha=.75,outlier.shape=21,outlier.color=C$rouge,width=.55)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color="white")+
            scale_fill_manual(values=gpalf(niv1))+
            labs(y=v1,x=v2)+theme_nutri()+theme(legend.position="none")
        } else {
          p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]],
            text=paste0(.data[[v1]],"\n",v2,"=",round(.data[[v2]],2))))+
            geom_boxplot(alpha=.75,outlier.shape=21,outlier.color=C$rouge,width=.55)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color="white")+
            scale_fill_manual(values=gpalf(niv1))+
            labs(x=v1,y=v2)+theme_nutri()+smart_axis_x(niv1)+
            theme(legend.position="none")
        }
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "violgrp") {
        d[[v1]] <- truncate_label(d[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(d,aes(y=.data[[v1]],x=.data[[v2]],fill=.data[[v1]]))+
            geom_violin(alpha=.5,trim=FALSE,linewidth=.7)+
            geom_boxplot(fill="white",width=.1,outlier.shape=21,outlier.color=C$rouge)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color=C$rouge)+
            scale_fill_manual(values=gpalf(niv1))+
            labs(y=v1,x=v2)+theme_nutri()+theme(legend.position="none")
        } else {
          p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]]))+
            geom_violin(alpha=.5,trim=FALSE,linewidth=.7)+
            geom_boxplot(fill="white",width=.1,outlier.shape=21,outlier.color=C$rouge)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color=C$rouge)+
            scale_fill_manual(values=gpalf(niv1))+
            labs(x=v1,y=v2)+theme_nutri()+smart_axis_x(niv1)+
            theme(legend.position="none")
        }
        ggplotly(p) |> nutri_config()

      } else if (type == "densgrp") {
        p <- ggplot(d,aes(x=.data[[v2]],fill=.data[[v1]],color=.data[[v1]]))+
          geom_density(alpha=.28,linewidth=.8)+
          scale_fill_manual(values=gpalf(niv1),name=v1)+
          scale_color_manual(values=gpalf(niv1),name=v1)+
          labs(x=v2,y="Densité")+theme_nutri()
        ggplotly(p)|>layout(legend=list(orientation="h",y=-0.2)) |> nutri_config()

      } else if (type == "facethist") {
        p <- ggplot(d,aes(x=.data[[v2]],fill=.data[[v1]]))+
          geom_histogram(aes(y=after_stat(density)),bins=25,color="white",alpha=.82)+
          scale_fill_manual(values=gpalf(niv1))+
          facet_wrap(as.formula(paste("~",v1)),ncol=min(3,niv1),scales="free_y")+
          labs(x=v2,y="Densité")+theme_nutri(10)+theme(legend.position="none")
        ggplotly(p) |> nutri_config()

      } else if (type == "halfeye") {
        d_h <- d; d_h[[v1]] <- truncate_label(d_h[[v1]], 20)
        p <- ggplot(d_h,aes(x=.data[[v2]],y=.data[[v1]],fill=.data[[v1]]))+
          ggdist::stat_halfeye(alpha=.75)+
          scale_fill_manual(values=gpalf(niv1))+
          labs(x=v2,y=v1)+theme_nutri()+theme(legend.position="none")
        ggplotly(p) |> nutri_config()

      } else if (type == "scatter") {
        v3  <- if(!is.null(input$biv_z)&&input$biv_z!="Aucune") input$biv_z else NULL
        rho <- cor.test(d[[v1]],d[[v2]],method="spearman",exact=FALSE)
        niv3 <- if(!is.null(v3)) length(unique(na.omit(d[[v3]]))) else 0
        p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],
          color=if(!is.null(v3)).data[[v3]] else NULL,
          text=paste0(v1,"=",round(.data[[v1]],2),"\n",v2,"=",round(.data[[v2]],2))))+
          geom_point(alpha=.45,size=1.8)+
          geom_smooth(method="loess",se=TRUE,color="black",linewidth=.9,aes(group=1))+
          {if(!is.null(v3)) scale_color_manual(values=gpalf(niv3))}+
          labs(x=v1,y=v2,
               subtitle=paste0("ρ Spearman=",round(rho$estimate,3),
                               "  p=",round(rho$p.value,4)))+
          theme_nutri()
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "hexbin") {
        p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]]))+
          geom_hex(bins=25)+
          scale_fill_gradientn(colors=c("#f0f8ff",C$bleu,C$rouge),name="Count")+
          labs(x=v1,y=v2)+theme_nutri()
        ggplotly(p) |> nutri_config()

      } else if (type == "cormap") {
        vn_sel <- vars_num()[vars_num() %in% c(v1,v2,
          vars_num()[1:min(10,length(vars_num()))])]
        mat <- df_f()|>select(all_of(vn_sel))|>drop_na()|>cor(method="spearman")
        mat_df <- as.data.frame(as.table(mat))
        mat_df$Var1 <- truncate_label(mat_df$Var1, 18)
        mat_df$Var2 <- truncate_label(mat_df$Var2, 18)
        p <- ggplot(mat_df,aes(x=Var1,y=Var2,fill=Freq,
          text=paste0(Var1," × ",Var2,"\nρ=",round(Freq,3))))+
          geom_tile(color="white",linewidth=.8)+
          geom_text(aes(label=round(Freq,2)),size=3,fontface="bold")+
          scale_fill_gradient2(low=C$rouge_f,mid="white",high=C$bleu,midpoint=0,name="ρ")+
          labs(x="",y="")+theme_nutri(10)+
          smart_axis_x(length(unique(mat_df$Var1)))
        ggplotly(p,tooltip="text") |> nutri_config()

      } else {
        plotly_empty()|>layout(title=list(text="Sélectionnez un type",x=.5))
      }
    },error=function(e)
      plotly_empty()|>layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  output$biv_dl_plot <- downloadHandler(
    filename=function() paste0("bivar_",Sys.Date(),".png"),
    content=function(f) ggsave(f, plot=last_plot(), width=12, height=8, dpi=300)
  )

  # ── DESCRIPTIVE OMS ─────────────────────────────────────────────────────────
  output$desc_by_ui <- renderUI({
    vc <- vars_cat()
    selectInput("desc_by","Stratifier par :",
                choices=c("Aucune"="none",setNames(vc,vc)),
                selected="none",width="100%")
  })
  output$desc_vars_ui <- renderUI({
    df <- df_f(); vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("desc_vars","Variables :",choices=vis,selected=head(vis,14),
                   multiple=TRUE,width="100%")
  })
  output$desc_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("desc_grp","Grouper par :",choices=vc,selected=vc[1],width="100%")
  })
  output$desc_corr_vars_ui <- renderUI({
    vn <- vars_num()
    selectizeInput("desc_corr_vars","Variables pour corrélation :",
                   choices=vn,selected=head(vn,10),multiple=TRUE,width="100%")
  })

  output$desc_gt_out <- renderUI({
    req(input$desc_vars); d <- df_f()
    tryCatch({
      tbl_data <- d |> select(any_of(
        if(input$desc_by!="none") c(input$desc_vars,input$desc_by) else input$desc_vars))
      gt_obj <- if (input$desc_by == "none") {
        tbl_data |>
          tbl_summary(
            statistic = list(all_continuous()  ~ "{mean} ± {sd} [{min}–{max}]",
                             all_categorical() ~ "{n} ({p}%)"),
            digits  = all_continuous() ~ 1,
            missing = "no") |>
          bold_labels() |>
          modify_caption("**Tableau descriptif — Style OMS/UNICEF**")
      } else {
        # Construire le tableau de base, puis ajouter les p-valeurs si demandé
        # (on évite le placeholder magrittr '.' incompatible avec le pipe natif |>)
        tbl_base <- tbl_data |>
          tbl_summary(
            by      = input$desc_by,
            statistic = list(all_continuous()  ~ "{mean} ± {sd}",
                             all_categorical() ~ "{n} ({p}%)"),
            digits  = all_continuous() ~ 1,
            missing = "no") |>
          bold_labels()
        if (isTRUE(input$desc_add_p))
          tbl_base |> add_p() |> bold_p(t = 0.05)
        else
          tbl_base
      }
      gt_obj |> as_gt() |> tab_options(table.font.size="12px") |> as_raw_html()
    }, error=function(e)
      tags$div(style="color:red;",paste("Erreur gtsummary:",e$message)))
  })

  output$desc_corr_plot <- renderPlot({
    req(input$desc_corr_vars); d <- df_f()
    vn <- intersect(input$desc_corr_vars, vars_num())
    req(length(vn) >= 2)
    mat <- d |> select(all_of(vn)) |> drop_na() |> cor(method="spearman")
    corrplot::corrplot(mat,method="color",type="upper",
                       addCoef.col="black",number.cex=.65,
                       tl.cex=.72,tl.col="#0B3954",tl.srt=45,
                       col=colorRampPalette(c(C$rouge,"white",C$bleu))(200),
                       mar=c(0,0,1,0))
  }, height=380)

  output$desc_zscore_plot <- renderPlotly({
    req(input$desc_grp); d <- df_f(); m <- map_cols()
    z_cols <- c(m$whz,m$haz,m$waz)
    z_cols <- z_cols[!sapply(z_cols,is.null) & z_cols %in% names(d)]
    req(length(z_cols)>0)
    df_long <- d |> select(all_of(c(input$desc_grp,z_cols))) |>
      pivot_longer(-all_of(input$desc_grp), names_to="Score", values_to="Val")
    niv <- length(unique(na.omit(d[[input$desc_grp]])))
    p <- ggplot(df_long,aes(x=.data[[input$desc_grp]],y=Val,fill=.data[[input$desc_grp]]))+
      geom_boxplot(alpha=.7,outlier.shape=21,outlier.color=C$rouge,width=.5)+
      geom_hline(yintercept=-2,linetype="dashed",color=C$rouge,linewidth=.7)+
      geom_hline(yintercept=-3,linetype="dashed",color=C$rouge_f,linewidth=.7)+
      geom_hline(yintercept=0,linetype="dotted",color="gray50",linewidth=.4)+
      scale_fill_manual(values=gpal(niv,"cat"))+
      facet_wrap(~Score,scales="free_y")+
      labs(x="",y="Score Z")+theme_bw(base_size=10)+
      theme(legend.position="none",axis.text.x=element_text(angle=25,hjust=1))
    ggplotly(p)|>layout(hovermode="closest") |> nutri_config()
  })

  output$dl_gt <- downloadHandler(
    filename=function() paste0("tableau_desc_",Sys.Date(),".docx"),
    content=function(f) {
      d <- df_f(); req(input$desc_vars)
      tbl_data <- d |> select(any_of(input$desc_vars))
      tbl <- tbl_data |>
        tbl_summary(statistic=list(all_continuous()~"{mean} ± {sd}",
                                    all_categorical()~"{n} ({p}%)"),
                    digits=all_continuous()~1, missing="no") |>
        bold_labels()
      tryCatch(
        tbl |> as_flex_table() |> flextable::save_as_docx(path=f),
        error=function(e) writeLines("Erreur export Word", f)
      )
    }
  )

  # ── INFÉRENTIEL — Module unifié (tous les tests à la fois) ──────────────────

  # Sélecteurs de variables
  output$infer_x_ui <- renderUI({
    all_vars <- c(vars_cat(), vars_num())
    selectInput("infer_x", "Variable X :", choices = all_vars,
                selected = all_vars[1], width = "100%")
  })
  output$infer_y_ui <- renderUI({
    all_vars <- c(vars_num(), vars_cat())
    sel <- if (length(all_vars) > 1) all_vars[2] else all_vars[1]
    selectInput("infer_y", "Variable Y :", choices = all_vars,
                selected = sel, width = "100%")
  })

  # Badge type de combinaison (réactif)
  output$infer_type_badge <- renderUI({
    req(input$infer_x, input$infer_y)
    d <- df_f()
    req(input$infer_x %in% names(d), input$infer_y %in% names(d))
    detect_combination_badge(input$infer_x, input$infer_y, d)
  })

  # Stockage des résultats
  infer_results <- reactiveVal(NULL)

  observeEvent(input$run_infer, {
    req(input$infer_x, input$infer_y)
    d <- df_f()
    req(input$infer_x %in% names(d), input$infer_y %in% names(d))
    alpha <- input$infer_alpha %||% 0.05

    withProgress(message = "Exécution de tous les tests...", value = 0.5, {
      res <- tryCatch(
        run_all_tests(d, input$infer_x, input$infer_y, alpha),
        error = function(e) list(error = e$message)
      )
      incProgress(0.5)
    })
    infer_results(res)
  })

  # Tableau récapitulatif
  output$infer_summary_table <- renderDT({
    res <- infer_results()
    req(!is.null(res), length(res) > 0)
    if (!is.null(res$error)) {
      return(datatable(data.frame(Erreur = res$error), rownames = FALSE))
    }
    df_summ <- render_summary_df(res)
    datatable(df_summ, rownames = FALSE,
              options = list(dom = "t", pageLength = 10, ordering = FALSE),
              class = "compact stripe hover",
              escape = FALSE) |>
      formatStyle("Valide",
                  backgroundColor = styleEqual(
                    c("\u2705 Oui", "\u274c Non"),
                    c("#f0fdf4", "#fef2f2")),
                  fontWeight = "bold") |>
      formatStyle("D\u00e9cision",
                  color = styleEqual(
                    c("\U0001f534 Significatif", "\U0001f7e2 Non significatif", "\u2b1c Non applicable"),
                    c("#BF3636", "#2E7D52", "#6B7B8D")),
                  fontWeight = "bold")
  })

  # Cartes détaillées
  output$infer_all_cards <- renderUI({
    res <- infer_results()
    req(!is.null(res), length(res) > 0)
    if (!is.null(res$error)) {
      return(tags$div(style="color:#BF3636;padding:20px;",
                      tags$h4("Erreur :"), tags$p(res$error)))
    }
    render_all_cards(res)
  })

  # Graphique
  output$infer_plot <- renderPlotly({
    req(infer_results(), length(infer_results()) > 0)
    req(is.null(infer_results()$error))
    d <- df_f()
    req(input$infer_x %in% names(d), input$infer_y %in% names(d))
    make_infer_plot(d, input$infer_x, input$infer_y, input$infer_alpha %||% 0.05)
  })

  # Multi-variables (régression logistique — conservée)
  output$multi_outcome_ui <- renderUI({
    opts <- intersect(c("GAM","SAM","Stunting","Underweight"),names(df_f()))
    if(length(opts)==0) opts <- vars_cat()
    selectInput("multi_out","Outcome :",choices=opts,width="100%")
  })
  output$multi_vars_ui <- renderUI({
    vn <- vars_num()
    checkboxGroupInput("multi_vars","Variables explicatives :",choices=vn,selected=head(vn,6))
  })
  output$multi_summary <- renderPrint({
    req(input$multi_vars,input$multi_out); d<-df_f()
    tryCatch({
      d_ml <- d|>select(all_of(c(input$multi_out,input$multi_vars)))|>drop_na()
      fm <- as.formula(paste(input$multi_out,"~",paste(input$multi_vars,collapse="+")))
      mod <- glm(fm,data=d_ml,family=binomial)
      cat("Régression logistique\n")
      cat("N=",nrow(d_ml),"\n\n")
      print(round(exp(cbind(OR=coef(mod),confint.default(mod))),3))
    },error=function(e) cat("Erreur:",e$message))
  })
  output$multi_forest <- renderPlotly({
    req(input$multi_vars,input$multi_out); d<-df_f()
    tryCatch({
      d_ml <- d|>select(all_of(c(input$multi_out,input$multi_vars)))|>drop_na()
      fm <- as.formula(paste(input$multi_out,"~",paste(input$multi_vars,collapse="+")))
      mod <- glm(fm,data=d_ml,family=binomial)
      coefs <- coef(mod)[-1]; ci <- confint.default(mod)[-1,]
      pvals <- summary(mod)$coefficients[-1,4]
      or_df <- tibble(
        Variable=names(coefs),
        OR=exp(coefs), CI_low=exp(ci[,1]), CI_high=exp(ci[,2]),
        p=pvals, Sig=p<.05
      ) |> filter(is.finite(OR),OR<50)
      p <- ggplot(or_df,aes(x=OR,y=fct_reorder(Variable,OR),color=Sig,
        text=paste0(Variable,"\nOR=",round(OR,3),"\nIC95=[",round(CI_low,3),"-",round(CI_high,3),"]","\np=",round(p,4))))+
        geom_vline(xintercept=1,linetype="dashed",color="gray50")+
        geom_errorbarh(aes(xmin=CI_low,xmax=CI_high),height=.3,linewidth=.8)+
        geom_point(size=3.5)+
        scale_color_manual(values=c("TRUE"=C$rouge,"FALSE"=C$gris),
                           labels=c("TRUE"="Significatif (p<.05)","FALSE"="NS"))+
        scale_x_log10()+
        labs(x="Odds Ratio (log scale)",y="",
             subtitle="OR>1 = facteur de risque | OR<1 = facteur protecteur")+
        theme_minimal(base_size=10)
      ggplotly(p,tooltip="text") |> nutri_config()
    },error=function(e)
      plotly_empty()|>layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  # ══════════════════════════════════════════════════════════════════
  # QUALITÉ SMART — Plausibility Check (style ENA)
  # ══════════════════════════════════════════════════════════════════

  # Helper : chercher les colonnes probables pour chaque variable
  guess_col <- function(d, patterns, type = "num") {
    nms <- names(d)
    for (pat in patterns) {
      matches <- grep(pat, nms, ignore.case = TRUE, value = TRUE)
      if (length(matches) == 0) next
      if (type == "num") matches <- matches[vapply(matches, function(v) is.numeric(d[[v]]), logical(1))]
      if (type == "cat") matches <- matches[vapply(matches, function(v) !is.numeric(d[[v]]) || length(unique(na.omit(d[[v]]))) <= 30, logical(1))]
      if (length(matches) > 0) return(matches[1])
    }
    NULL
  }

  # UI dynamiques pour les sélecteurs de variables
  output$sq_col_whz_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^whz$","^WHZ$","wfhz","zscore.*wh","z.*wh","_whz","whz_"))
    sel <- guess %||% (if (any(grepl("whz|WHZ", vn, ignore.case=TRUE))) grep("whz|WHZ", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_whz", "WHZ (poids-taille) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_haz_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^haz$","^HAZ$","hfaz","zscore.*ha","z.*ha","_haz","haz_"))
    sel <- guess %||% (if (any(grepl("haz|HAZ", vn, ignore.case=TRUE))) grep("haz|HAZ", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_haz", "HAZ (taille-âge) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_waz_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^waz$","^WAZ$","wfaz","zscore.*wa","z.*wa","_waz","waz_"))
    sel <- guess %||% (if (any(grepl("waz|WAZ", vn, ignore.case=TRUE))) grep("waz|WAZ", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_waz", "WAZ (poids-âge) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_muac_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^muac$","^MUAC$","pb","perim.*bra","mid.*arm"))
    sel <- guess %||% (if (any(grepl("muac|MUAC|pb|PB", vn, ignore.case=TRUE))) grep("muac|MUAC|pb|PB", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_muac", "MUAC (mm) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })

  output$sq_col_sex_ui <- renderUI({
    d <- df_f(); vc <- c(vars_cat(), vars_num())
    guess <- guess_col(d, c("^sex","^genre","^gender"), type = "cat")
    sel <- guess %||% ""
    selectInput("sq_col_sex", "Sexe :",
                choices = c("— Aucun —" = "", vc), selected = sel, width = "100%")
  })
  output$sq_col_age_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^age.*mois","^age.*month","^age_m","^agemois","^age$"))
    sel <- guess %||% (if (any(grepl("age", vn, ignore.case=TRUE))) grep("age", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_age", "Âge (mois) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_weight_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^poids$","^weight$","^wt$","^pds$"))
    sel <- guess %||% (if (any(grepl("poids|weight|wt", vn, ignore.case=TRUE))) grep("poids|weight|wt", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_weight", "Poids (kg) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_height_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^taille$","^height$","^ht$","^longueur$"))
    sel <- guess %||% (if (any(grepl("taille|height|ht", vn, ignore.case=TRUE))) grep("taille|height|ht", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_height", "Taille (cm) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_team_ui <- renderUI({
    d <- df_f(); vc <- c(vars_cat(), vars_num())
    guess <- guess_col(d, c("^equipe","^team","^enqueteur","^superviseur","^cluster"), type = "cat")
    sel <- guess %||% ""
    selectInput("sq_col_team", "Équipe / Enquêteur :",
                choices = c("— Aucun —" = "", vc), selected = sel, width = "100%")
  })

  # Reactive stores
  sq_results <- reactiveValues(
    plaus = NULL, flags = NULL, flags_summary = NULL,
    dp = NULL, age_heap = NULL, sd_team = NULL, completude = NULL
  )

  # Sélecteur digit preference
  output$sq_dp_var_ui <- renderUI({
    vn <- vars_num()
    selectInput("sq_dp_var", "Variable pour digit preference :",
                choices = vn, selected = vn[1], width = "100%")
  })

  # Lancer le contrôle qualité
  observeEvent(input$run_qualite, {
    d <- df_f(); req(nrow(d) > 0)

    # Récupérer les colonnes (chaîne vide → NULL)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_whz    <- nullify(input$sq_col_whz)
    col_haz    <- nullify(input$sq_col_haz)
    col_waz    <- nullify(input$sq_col_waz)
    col_muac   <- nullify(input$sq_col_muac)
    col_sex    <- nullify(input$sq_col_sex)
    col_age    <- nullify(input$sq_col_age)
    col_weight <- nullify(input$sq_col_weight)
    col_height <- nullify(input$sq_col_height)
    col_team   <- nullify(input$sq_col_team)

    withProgress(message = "Contrôle qualité SMART en cours...", value = 0, {

      # 1. Flags
      incProgress(0.15, detail = "SMART Flags...")
      sq_results$flags <- detect_smart_flags(d, col_whz, col_haz, col_waz, col_muac)
      sq_results$flags_summary <- summarise_flags(sq_results$flags, nrow(d))

      # 2. Score de plausibilité
      incProgress(0.25, detail = "Score de plausibilité...")
      sq_results$plaus <- calc_plausibility_score(
        d, col_whz, col_haz, col_waz, col_muac,
        col_sex, col_age, col_weight, col_height, col_team
      )

      # 3. Age heaping
      incProgress(0.15, detail = "Age heaping...")
      sq_results$age_heap <- if (!is.null(col_age)) calc_age_heaping(d[[col_age]]) else NULL

      # 4. SD par équipe
      incProgress(0.15, detail = "SD par équipe...")
      sq_results$sd_team <- calc_sd_by_team(d, col_whz %||% col_haz, col_team)

      # 5. Complétude
      incProgress(0.15, detail = "Complétude...")
      vars_cles <- c(col_whz, col_haz, col_waz, col_muac, col_sex, col_age,
                     col_weight, col_height, col_team)
      vars_cles <- vars_cles[!sapply(vars_cles, is.null)]
      sq_results$completude <- calc_completude(d, vars_cles)

      incProgress(0.15, detail = "Terminé ✅")
    })
  })

  # ── Rendu : Score de plausibilité ──
  output$sq_plausibility_html <- renderUI({
    req(sq_results$plaus)
    render_plausibility_html(sq_results$plaus)
  })

  # ── Rendu : SMART Flags table ──
  output$sq_flags_table <- renderDT({
    req(sq_results$flags_summary)
    datatable(sq_results$flags_summary, rownames = FALSE,
              options = list(dom = "t", pageLength = 10),
              class = "compact stripe hover") |>
      formatStyle("Qualité",
                  backgroundColor = styleEqual(
                    c("🟢 Excellent", "🟡 Acceptable", "🟠 Problématique", "🔴 Sérieux"),
                    c("#f0fdf4", "#fefce8", "#fff7ed", "#fef2f2")),
                  fontWeight = "bold")
  })

  # ── Rendu : Flags plot ──
  output$sq_flags_plot <- renderPlotly({
    req(sq_results$flags)
    d <- df_f()
    col_whz <- if (!is.null(input$sq_col_whz) && input$sq_col_whz != "") input$sq_col_whz else NULL
    if (!is.null(col_whz)) {
      plot_whz_flags(d, col_whz) |> nutri_config()
    } else {
      plotly_empty() |> layout(title = list(text = "Sélectionnez la colonne WHZ", x = .5))
    }
  })

  # ── Rendu : Digit Preference ──
  output$sq_dp_table <- renderDT({
    req(input$sq_dp_var)
    d <- df_f()
    dp <- calc_digit_preference(d[[input$sq_dp_var]], input$sq_dp_var)
    sq_results$dp <- dp
    req(dp$freq)
    datatable(dp$freq, rownames = FALSE,
              options = list(dom = "t", pageLength = 10),
              class = "compact stripe",
              caption = paste0("DPS = ", dp$dps, " — ", dp$quality))
  })

  output$sq_dp_plot <- renderPlotly({
    req(input$sq_dp_var)
    d <- df_f()
    dp <- calc_digit_preference(d[[input$sq_dp_var]], input$sq_dp_var)
    plot_digit_preference(dp) |> nutri_config()
  })

  # ── Rendu : Age Heaping ──
  output$sq_age_summary <- renderUI({
    ah <- sq_results$age_heap
    if (is.null(ah)) {
      return(tags$p(style = "color:#888;padding:20px;",
                    "Sélectionnez la colonne Âge (mois) et lancez le contrôle"))
    }
    tags$div(style = "padding:10px;",
      tags$div(style = "background:#f0f8ff;border-radius:8px;padding:14px;margin-bottom:12px;",
        tags$div(style = "font-size:11px;text-transform:uppercase;color:#888;", "RATIO AGE HEAPING"),
        tags$div(style = "font-size:32px;font-weight:800;color:#0B3954;", ah$index %||% "N/A"),
        tags$div(style = "font-size:12px;color:#555;", ah$quality %||% ""),
        tags$div(style = "font-size:11px;color:#888;margin-top:6px;",
                 paste0(ah$pct_round %||% "?", "% sur multiples de 6 (attendu : ",
                        ah$expected_pct %||% "?", "%)"))
      ),
      tags$div(style = "font-size:11px;color:#666;line-height:1.7;",
        tags$b("Interprétation :"), tags$br(),
        "Ratio ≤ 1.3 : Excellent", tags$br(),
        "Ratio 1.3-2.0 : Acceptable", tags$br(),
        "Ratio 2.0-3.0 : Problématique", tags$br(),
        "Ratio > 3.0 : Sérieux"
      )
    )
  })

  output$sq_age_plot <- renderPlotly({
    d <- df_f()
    col_age <- if (!is.null(input$sq_col_age) && input$sq_col_age != "") input$sq_col_age else NULL
    if (!is.null(col_age)) {
      plot_age_distribution(d, col_age) |> nutri_config()
    } else {
      plotly_empty() |> layout(title = list(text = "Sélectionnez la colonne Âge", x = .5))
    }
  })

  # ── Rendu : SD par équipe ──
  output$sq_team_table <- renderDT({
    req(sq_results$sd_team)
    datatable(sq_results$sd_team$table, rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe hover",
              caption = paste0("SD global = ", sq_results$sd_team$sd_global,
                               " — ", sq_results$sd_team$quality)) |>
      formatStyle("Qualité",
                  backgroundColor = styleEqual(
                    c("🟢 OK", "🟡 Attention", "🔴 Problème"),
                    c("#f0fdf4", "#fefce8", "#fef2f2")),
                  fontWeight = "bold")
  })

  output$sq_team_plot <- renderPlotly({
    req(sq_results$sd_team)
    plot_sd_teams(sq_results$sd_team,
                  if (!is.null(input$sq_col_whz) && input$sq_col_whz != "") "WHZ" else "Z-score") |>
      nutri_config()
  })

  # ── Rendu : Complétude ──
  output$sq_completude_table <- renderDT({
    req(sq_results$completude)
    datatable(sq_results$completude, rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe hover") |>
      formatStyle("% complet",
                  background = styleColorBar(c(0, 100), "#0077B6"),
                  backgroundSize = "100% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Qualité",
                  backgroundColor = styleEqual(
                    c("🟢 Excellent", "🟡 Acceptable", "🟠 Problématique", "🔴 Sérieux"),
                    c("#f0fdf4", "#fefce8", "#fff7ed", "#fef2f2")),
                  fontWeight = "bold")
  })

  # ══════════════════════════════════════════════════════════════════
  # PRÉVALENCES SMART — Pondérées + DEFF + IC 95%
  # ══════════════════════════════════════════════════════════════════

  # UI dynamiques pour le plan de sondage
  output$sp_col_cluster_ui <- renderUI({
    all_v <- c(vars_cat(), vars_num())
    d <- df_f()
    guess <- guess_col(d, c("^cluster","^grappe","^psu","^ea$","^zd$"), type = "cat")
    sel <- guess %||% ""
    selectInput("sp_col_cluster", "Cluster / Grappe (PSU) :",
                choices = c("— Aucun —" = "", all_v), selected = sel, width = "100%")
  })
  output$sp_col_strata_ui <- renderUI({
    all_v <- c(vars_cat(), vars_num())
    d <- df_f()
    guess <- guess_col(d, c("^strat","^region","^département","^zone"), type = "cat")
    sel <- guess %||% ""
    selectInput("sp_col_strata", "Strate :",
                choices = c("— Aucune —" = "", all_v), selected = sel, width = "100%")
  })
  output$sp_col_weights_ui <- renderUI({
    vn <- vars_num()
    d <- df_f()
    guess <- guess_col(d, c("^weight$","^poids_sond","^wt$","^sampling.*weight","^ponder"))
    sel <- guess %||% ""
    selectInput("sp_col_weights", "Poids d'échantillonnage :",
                choices = c("— Aucun (poids = 1) —" = "", vn), selected = sel, width = "100%")
  })

  # UI : Indicateurs à calculer (auto-détection des colonnes binaires OMS)
  output$sp_indicators_ui <- renderUI({
    d <- df_f()
    # Chercher les colonnes binaires (0/1) qui ressemblent à des indicateurs
    binary_cols <- names(d)[vapply(names(d), function(v) {
      vals <- na.omit(d[[v]])
      is.numeric(vals) && all(vals %in% c(0, 1)) && length(vals) > 10
    }, logical(1))]
    # Prioriser les noms connus
    known <- c("GAM", "SAM", "MAM", "Stunting", "Underweight", "MUAC_GAM",
               "gam", "sam", "mam", "stunting", "underweight")
    selected <- intersect(known, binary_cols)
    if (length(selected) == 0) selected <- head(binary_cols, 5)

    checkboxGroupInput("sp_indicators", "Indicateurs binaires (0/1) :",
                       choices = binary_cols, selected = selected, width = "100%")
  })

  # UI : Stratifications
  output$sp_stratifications_ui <- renderUI({
    d <- df_f()
    vc <- vars_cat()
    # Suggestions intelligentes
    known_strats <- c("Sexe", "sexe", "sex", "Sex", "Groupe_age", "age_group",
                      "Region", "region", "Strate", "strate", "Deplacement",
                      "deplacement", "displacement")
    suggested <- intersect(known_strats, vc)
    if (length(suggested) == 0) suggested <- head(vc, 3)

    checkboxGroupInput("sp_stratifications", "Stratifier par :",
                       choices = vc, selected = suggested, width = "100%")
  })

  # Stockage des résultats
  sp_results <- reactiveVal(NULL)

  # Lancer le calcul
  observeEvent(input$run_prevalence, {
    d <- df_f()
    req(nrow(d) > 0, input$sp_indicators)

    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    col_weights <- nullify(input$sp_col_weights)

    # Construire la liste d'indicateurs
    indicators <- setNames(
      as.list(input$sp_indicators),
      input$sp_indicators
    )

    # Construire la liste de stratifications
    stratifications <- if (!is.null(input$sp_stratifications) && length(input$sp_stratifications) > 0) {
      setNames(as.list(input$sp_stratifications), input$sp_stratifications)
    } else NULL

    withProgress(message = "Calcul des prévalences SMART...", value = 0, {
      incProgress(0.2, detail = "Création du design de sondage...")
      res <- tryCatch(
        run_smart_prevalences(d, indicators, col_cluster, col_strata,
                               col_weights, stratifications),
        error = function(e) list(error = e$message)
      )
      incProgress(0.8, detail = "Terminé ✅")
    })

    sp_results(res)
  })

  # ── Rendu : Info design ──
  output$sp_design_info <- renderUI({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    render_design_info_html(res$design_info, res$method)
  })

  # ── Rendu : Tableau global HTML ──
  output$sp_global_table_html <- renderUI({
    res <- sp_results(); req(!is.null(res), !is.null(res$global))
    if (!is.null(res$error)) return(tags$p(style="color:#e74c3c;", res$error))
    render_smart_table_html(res$global)
  })

  # ── Rendu : Forest plot global ──
  output$sp_forest_global <- renderPlotly({
    res <- sp_results(); req(!is.null(res), !is.null(res$global), nrow(res$global) > 0)
    plot_prevalence_forest(res$global, "Prévalences SMART globales — IC 95%")
  })

  # ── Rendu : Tableau par strate ──
  output$sp_strat_table <- renderDT({
    res <- sp_results(); req(!is.null(res), !is.null(res$strat), nrow(res$strat) > 0)
    datatable(res$strat, rownames = FALSE,
              options = list(dom = "ftip", pageLength = 20, scrollX = TRUE),
              class = "compact stripe hover") |>
      formatStyle("Prévalence (%)",
                  background = styleColorBar(c(0, max(res$strat$`Prévalence (%)`, 30)), "#e74c3c"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatRound(c("Prévalence (%)", "IC bas", "IC haut", "DEFF"), digits = 2)
  })

  # ── Rendu : Forest plot par strate ──
  output$sp_forest_strat <- renderPlotly({
    res <- sp_results(); req(!is.null(res), !is.null(res$strat), nrow(res$strat) > 0)
    plot_prevalence_forest(res$strat, "Prévalences par strate — IC 95%")
  })

  # ── Rendu : Tableau complet (toutes données combinées) ──
  output$sp_full_table <- renderDT({
    res <- sp_results(); req(!is.null(res))
    # Combiner global + strat
    all_data <- bind_rows(
      if (!is.null(res$global)) res$global |> mutate(Strate = "Global", Groupe = "Tous"),
      res$strat
    )
    req(nrow(all_data) > 0)
    datatable(all_data, rownames = FALSE,
              options = list(dom = "ftip", pageLength = 50, scrollX = TRUE),
              class = "compact stripe hover",
              filter = "top") |>
      formatRound(c("Prévalence (%)", "IC bas", "IC haut", "DEFF"), digits = 2)
  })

  # ── Download CSV ──
  output$dl_smart_table <- downloadHandler(
    filename = function() paste0("prevalences_smart_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- sp_results(); req(!is.null(res))
      all_data <- bind_rows(
        if (!is.null(res$global)) res$global |> mutate(Strate = "Global", Groupe = "Tous"),
        res$strat
      )
      write.csv(all_data, file, row.names = FALSE)
    }
  )

  # ── Export ENA/SMART ──
  output$dl_smart_ena <- downloadHandler(
    filename = function() paste0("SMART_export_", Sys.Date(), ".csv"),
    content  = function(file) {
      res <- sp_results(); req(!is.null(res))
      out <- tryCatch(
        format_smart_export(res$global, res$strat, res$design_info, res$method),
        error = function(e) res$global
      )
      write.csv(out, file, row.names = FALSE)
    }
  )

  # ══════════════════════════════════════════════════════════════════
  # PRÉVALENCES — NOUVEAUX OUTPUTS (Moyennes SA · Comparaison designs · ICC · Diagnostics)
  # ══════════════════════════════════════════════════════════════════

  # ── Réactive partagée : design actif ──
  .sp_design_r <- reactive({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    col_weights <- nullify(input$sp_col_weights)
    if (!HAS_SURVEY) return(NULL)
    options(survey.lonely.psu = "adjust")
    tryCatch(
      create_survey_design(d, col_cluster, col_strata, col_weights),
      error = function(e) NULL
    )
  })

  # ── UI : sélecteurs colonnes SA continues ──
  output$sp_means_sa_cols_ui <- renderUI({
    d <- df_f(); req(d)
    cont_cols <- names(d)[sapply(names(d), function(v) is.numeric(d[[v]]))]
    # Auto-détecter colonnes SA
    guess_sa <- function(pats) {
      m <- unname(sapply(pats, function(p) {
        found <- cont_cols[grepl(p, tolower(cont_cols))]
        if (length(found)) found[1] else NA_character_
      }))
      na.omit(m)[1]
    }
    defaults <- c(
      guess_sa(c("^fcs$","^fcs_score$","food.cons","^score_fcs")),
      guess_sa(c("^rcsi$","^rcsi_score$","^r_csi$")),
      guess_sa(c("^hdds$","^hdds_score$","dietary.div")),
      guess_sa(c("^hhs$","^hhs_score$","household.hunger"))
    )
    checkboxGroupInput("sp_means_sa_cols", "Indicateurs continus :",
      choices  = setNames(cont_cols, cont_cols),
      selected = na.omit(defaults),
      width    = "100%")
  })

  output$sp_means_group_ui <- renderUI({
    d <- df_f(); req(d)
    cat_cols <- names(d)[sapply(names(d), function(v) is.character(d[[v]]) || is.factor(d[[v]]))]
    choices <- c("Global" = "", setNames(cat_cols, cat_cols))
    selectInput("sp_means_group_var", "Grouper par :",
                choices = choices, selected = "", width = "100%")
  })

  output$sp_means_forest <- renderPlotly({
    des <- .sp_design_r(); req(des)
    req(input$sp_means_sa_cols, length(input$sp_means_sa_cols) > 0)
    d <- df_f(); req(d)

    cols <- setNames(input$sp_means_sa_cols, input$sp_means_sa_cols)
    grp  <- if (!is.null(input$sp_means_group_var) && input$sp_means_group_var != "")
              input$sp_means_group_var else NULL

    tbl <- tryCatch(calc_means_survey(des, cols, grp), error = function(e) NULL)
    req(tbl, nrow(tbl) > 0)

    has_group <- "Groupe" %in% names(tbl)
    tbl$y_lbl <- if (has_group) paste0(tbl$Indicateur, " — ", tbl$Groupe) else tbl$Indicateur
    tbl$hover <- paste0(
      "<b>", tbl$y_lbl, "</b><br>",
      "Moy : ", tbl$Moyenne, "<br>",
      "IC 95% : [", tbl$IC_lo, " – ", tbl$IC_hi, "]<br>",
      "SE : ", tbl$SE, "<br>",
      "DEFF : ", tbl$DEFF, "<br>",
      "N : ", tbl$N, " | N eff : ", tbl$n_eff
    )

    cols_ind <- scales::hue_pal()(length(unique(tbl$Indicateur)))
    col_map  <- setNames(cols_ind, unique(tbl$Indicateur))

    fig <- plot_ly()
    for (ind in unique(tbl$Indicateur)) {
      d_ind <- tbl |> filter(Indicateur == ind)
      fig <- fig |>
        add_segments(
          data = d_ind, x = ~IC_lo, xend = ~IC_hi,
          y = ~y_lbl, yend = ~y_lbl,
          line = list(color = col_map[ind], width = 3.5),
          name = ind, legendgroup = ind, showlegend = TRUE, hoverinfo = "none"
        ) |>
        add_trace(
          data = d_ind, x = ~Moyenne, y = ~y_lbl,
          type = "scatter", mode = "markers",
          marker = list(color = col_map[ind], size = 10, symbol = "circle",
                        line = list(color = "white", width = 1.5)),
          text = ~hover, hoverinfo = "text",
          name = ind, legendgroup = ind, showlegend = FALSE
        )
    }
    fig |> layout(
      title = list(text = "<b>Moyennes avec IC 95% design-based</b>",
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "Valeur moyenne (échelle de l'indicateur)", gridcolor = "#f0f0f0"),
      yaxis = list(title = "", tickfont = list(size = 10)),
      legend = list(orientation = "h", y = -0.2),
      plot_bgcolor = "white", paper_bgcolor = "white"
    )
  })

  output$sp_means_table <- renderDT({
    des <- .sp_design_r(); req(des)
    req(input$sp_means_sa_cols, length(input$sp_means_sa_cols) > 0)
    cols <- setNames(input$sp_means_sa_cols, input$sp_means_sa_cols)
    grp  <- if (!is.null(input$sp_means_group_var) && input$sp_means_group_var != "")
              input$sp_means_group_var else NULL
    tbl <- tryCatch(calc_means_survey(des, cols, grp), error = function(e) NULL)
    req(tbl, nrow(tbl) > 0)
    datatable(
      tbl |> rename(`IC bas` = IC_lo, `IC haut` = IC_hi,
                     `N effectif` = n_eff),
      rownames = FALSE,
      options  = list(dom = "ftip", pageLength = 20, scrollX = TRUE),
      class    = "compact stripe hover"
    ) |>
      formatStyle("DEFF",
        color = styleInterval(c(1.5, 2.5), c("#27AE60", "#E67E22", "#C0392B")),
        fontWeight = "bold"
      )
  })

  # ── Comparaison designs ──
  .sp_cmp_r <- reactive({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    col_weights <- nullify(input$sp_col_weights)
    inds_sel    <- if (!is.null(input$sp_cmp_inds)) input$sp_cmp_inds else c("GAM","MAG_muac")

    # Indicateurs disponibles dans les données
    inds_avail <- intersect(inds_sel, names(d))
    req(length(inds_avail) > 0)
    col_inds <- setNames(inds_avail, inds_avail)

    tryCatch(
      calc_design_comparison(d, col_inds, col_cluster, col_strata, col_weights),
      error = function(e) NULL
    )
  })

  output$sp_cmp_forest <- renderPlotly({
    df_cmp <- .sp_cmp_r(); req(df_cmp, nrow(df_cmp) > 0)
    tryCatch(
      plot_design_comparison(df_cmp),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x = .5))
    )
  })

  output$sp_cmp_table <- renderDT({
    df_cmp <- .sp_cmp_r(); req(df_cmp, nrow(df_cmp) > 0)
    datatable(
      df_cmp |>
        rename(`Prévalence (%)` = Prev_pct, `IC bas` = CI_lo, `IC haut` = CI_hi,
               `Largeur IC (pts)` = Largeur_IC, `SE (%)` = SE_pct, `N effectif` = n_eff) |>
        arrange(Indicateur, Design),
      rownames = FALSE,
      options  = list(dom = "ftip", pageLength = 25, scrollX = TRUE),
      class    = "compact stripe hover"
    ) |>
      formatStyle("Design",
        target = "row",
        backgroundColor = styleEqual(
          c("Stratifié + Grappes", "Grappes seules", "SRS (naïf)"),
          c("#e8f4fd", "#f0fff4", "#f9f9f9")
        )
      ) |>
      formatStyle("DEFF",
        color = styleInterval(c(1.5, 2.5), c("#27AE60", "#E67E22", "#C0392B")),
        fontWeight = "bold"
      )
  })

  # ── ICC et DEFF ──
  output$sp_icc_table <- renderDT({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    req(col_cluster)

    inds_all <- c(GAM = "GAM", SAM = "SAM",
                  `MAG MUAC` = "MAG_muac", `MAS MUAC` = "MAS_muac",
                  Stunting = "Stunting", Underweight = "Underweight")
    inds_avail <- inds_all[inds_all %in% names(d)]
    req(length(inds_avail) > 0)

    tbl <- tryCatch(
      calc_icc_table(d, inds_avail, col_cluster),
      error = function(e) NULL
    )
    req(tbl, nrow(tbl) > 0)

    datatable(
      tbl |> rename(
        `DEFF théorique` = DEFF_theorique, `m̄ (taille moy. grappe)` = m_bar,
        `K (grappes)` = n_clusters, `N total` = N_total
      ),
      rownames = FALSE,
      options  = list(dom = "t", pageLength = 15),
      class    = "compact stripe"
    ) |>
      formatStyle("ICC",
        background = styleColorBar(c(0, 0.4), "#E67E22"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      formatStyle("DEFF théorique",
        color = styleInterval(c(1.5, 2.5), c("#27AE60", "#E67E22", "#C0392B")),
        fontWeight = "bold"
      )
  })

  output$sp_deff_bar <- renderPlotly({
    res <- sp_results(); req(!is.null(res), !is.null(res$global))
    df_g <- res$global
    req("DEFF" %in% names(df_g))
    df_g <- df_g |> filter(!is.na(DEFF))
    req(nrow(df_g) > 0)

    df_g$couleur <- case_when(
      df_g$DEFF < 1.5  ~ "#27AE60",
      df_g$DEFF < 2.5  ~ "#E67E22",
      TRUE             ~ "#C0392B"
    )
    df_g$hover <- paste0(
      "<b>", df_g$Indicateur, "</b><br>",
      "DEFF = ", df_g$DEFF, "<br>",
      "N effectif = ", round(df_g$N / df_g$DEFF, 0), " / ", df_g$N
    )

    plot_ly(df_g, x = ~DEFF, y = ~reorder(Indicateur, DEFF),
            type = "bar", orientation = "h",
            marker = list(color = ~couleur, line = list(color = "white", width = 1)),
            text = ~paste0("DEFF = ", DEFF), textposition = "outside",
            hoverinfo = "text", hovertext = ~hover) |>
      add_vline(x = 1, line_color = "#888", line_dash = "dash",
                line_width = 1, annotation_text = "DEFF = 1 (SRS)", 
                annotation_font = list(size = 10, color = "#888")) |>
      layout(
        title = list(text = "<b>Effets de sondage (DEFF) par indicateur</b>",
                     font = list(size = 13, color = "#0B3954"), x = 0),
        xaxis = list(title = "DEFF", gridcolor = "#f0f0f0"),
        yaxis = list(title = ""),
        showlegend = FALSE,
        plot_bgcolor = "white", paper_bgcolor = "white"
      )
  })

  # ── Diagnostics plan de sondage ──
  .sp_diag_r <- reactive({
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    req(col_cluster, col_cluster %in% names(d))
    tryCatch(
      calc_design_diagnostics(d, col_cluster, col_strata),
      error = function(e) NULL
    )
  })

  output$sp_diag_alerts <- renderUI({
    diag <- .sp_diag_r()
    if (is.null(diag)) {
      return(tags$div(
        style = "background:#f5f5f5;border-radius:8px;padding:12px;font-size:11px;color:#888;",
        icon("info-circle"), " Sélectionnez une variable Cluster pour afficher les diagnostics."
      ))
    }
    tagList(
      tags$div(
        style = "display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
        lapply(list(
          list("Grappes (PSU)", diag$n_clusters, "#0B3954"),
          list("Taille min", diag$min_size, "#27AE60"),
          list("Taille max", diag$max_size, "#E67E22"),
          list("Taille moyenne", diag$mean_size, "#0077B6"),
          list("CV taille (%)", paste0(diag$cv_size, "%"),
               if (diag$cv_size > 30) "#C0392B" else "#27AE60"),
          list("PSU outliers", diag$outlier_clusters,
               if (diag$outlier_clusters > 0) "#E67E22" else "#27AE60")
        ), function(box) {
          tags$div(
            style = paste0("background:white;border-radius:8px;padding:10px 14px;",
                           "border-left:3px solid ", box[[3]], ";text-align:center;flex:1;min-width:100px;"),
            tags$div(style = paste0("font-size:20px;font-weight:800;color:", box[[3]], ";"), box[[2]]),
            tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", box[[1]])
          )
        })
      ),
      render_design_alerts(diag)
    )
  })

  output$sp_cluster_sizes_hist <- renderPlotly({
    diag <- .sp_diag_r(); req(diag)
    tryCatch(
      plot_cluster_sizes(diag),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x = .5))
    )
  })

  output$sp_cluster_sizes_table <- renderDT({
    diag <- .sp_diag_r(); req(diag)
    tbl  <- diag$cluster_sizes
    datatable(
      tbl,
      rownames = FALSE,
      options  = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class    = "compact stripe"
    ) |>
      formatStyle("N",
        background = styleColorBar(range(tbl$N), "#0077B6"),
        backgroundSize = "100% 75%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ══════════════════════════════════════════════════════════════════
  # SÉCURITÉ ALIMENTAIRE — FCS · rCSI · HDDS · HHS
  # ══════════════════════════════════════════════════════════════════

  # Helper : sélecteur rapide de colonne numérique avec guess
  sa_select <- function(id, label, patterns, d, vn) {
    guess <- guess_col(d, patterns)
    sel <- guess %||% ""
    selectInput(id, label, choices = c("— Aucun —" = "", vn),
                selected = sel, width = "100%")
  }

  # ── UI dynamiques FCS pré-calculé ──
  output$sa_fcs_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_precalc", "Colonne FCS :",
              c("^fcs$","^FCS$","^food.*cons.*score","^fcs_score","^score.*fcs",
                "^fcscore","FCSScore","food_consumption","sca$","^sca_score",
                "^score_consommation"), d, vn)
  })

  # ── UI dynamiques FCS groupes ──
  output$sa_fcs_cereales_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_cereales", "Céréales/Tubercules :",
              c("^cereale","^cereal","^staple","^fcs.*cereal","^fcs.*staple",
                "FCSStap","fcs_staple","tubercul","main_staple","grp.*cereal",
                "g1.*cereal","food.*cereal","alim.*cereal"), d, vn)
  })
  output$sa_fcs_legumineuses_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_legumineuses", "Légumineuses :",
              c("^legum","^pulse","^fcs.*pulse","^fcs.*legum","FCSPulse",
                "fcs_pulse","bean","haricot","grp.*legum","g2.*pulse",
                "food.*pulse","alim.*legum","noix_legum"), d, vn)
  })
  output$sa_fcs_legumes_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_legumes", "Légumes :",
              c("^legume","^veget","^fcs.*veg","^fcs.*legum","FCSVeg","fcs_veg",
                "grp.*legume","g3.*veg","food.*veg","alim.*legume"), d, vn)
  })
  output$sa_fcs_fruits_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_fruits", "Fruits :",
              c("^fruit","^fcs.*fruit","FCSFruit","fcs_fruit",
                "grp.*fruit","g4.*fruit","food.*fruit","alim.*fruit"), d, vn)
  })
  output$sa_fcs_viande_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_viande", "Viande/Poisson :",
              c("^viande","^meat","^protein","^fcs.*meat","^fcs.*prot",
                "FCSPr","fcs_meat","fcs_protein","poisson","fish",
                "grp.*viande","grp.*meat","g5.*meat","food.*meat",
                "alim.*viande","animal_protein"), d, vn)
  })
  output$sa_fcs_lait_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_lait", "Lait/Prod. laitiers :",
              c("^lait","^milk","^dairy","^fcs.*milk","^fcs.*dairy","^fcs.*lait",
                "FCSDairy","fcs_dairy","prod.*lait","grp.*lait",
                "g6.*dairy","g6.*milk","food.*dairy","alim.*lait"), d, vn)
  })
  output$sa_fcs_sucre_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_sucre", "Sucre :",
              c("^sucre","^sugar","^fcs.*sugar","^fcs.*sucre","FCSSugar","fcs_sugar",
                "grp.*sucre","g7.*sugar","food.*sugar","alim.*sucre",
                "sweet","sucrerie"), d, vn)
  })
  output$sa_fcs_huile_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_huile", "Huile/Graisse :",
              c("^huile","^oil","^fat","^fcs.*oil","^fcs.*fat","^fcs.*huile",
                "FCSFat","FCSOil","fcs_oil","fcs_fat","graisse",
                "grp.*huile","g8.*oil","g8.*fat","food.*oil","alim.*huile"), d, vn)
  })

  # ── UI dynamiques rCSI ──
  output$sa_rcsi_prefere_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_prefere", "Aliments moins préférés :",
              c("^rcsi.*pref","^less.*prefer","^moins.*pref","rcsi1",
                "rCSILessQlty","rcsi_less","coping.*pref","strat.*pref",
                "alim.*moins.*pref","less_preferred"), d, vn)
  })
  output$sa_rcsi_emprunter_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_emprunter", "Emprunter nourriture :",
              c("^rcsi.*borrow","^emprunt","rcsi2","rCSIBorrow",
                "rcsi_borrow","coping.*borrow","coping.*emprunt",
                "strat.*emprunt","borrow_food"), d, vn)
  })
  output$sa_rcsi_repas_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_repas", "Réduire nb de repas :",
              c("^rcsi.*meal","^redui.*repas","rcsi3","rCSIMealNb",
                "rcsi_meal","reduce.*meal","coping.*meal","coping.*repas",
                "strat.*repas","limit.*meal","fewer_meal"), d, vn)
  })
  output$sa_rcsi_portions_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_portions", "Réduire les portions :",
              c("^rcsi.*port","^redui.*port","rcsi4","rCSIMealSize","meal.*size","meal.*si",
                "rcsi_portion","reduce.*portion","coping.*portion",
                "strat.*portion","limit.*portion","smaller_meal"), d, vn)
  })
  output$sa_rcsi_adultes_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_adultes", "Réduire conso adultes :",
              c("^rcsi.*adult","^redui.*adult","rcsi5","rCSIMealAdult",
                "rcsi_adult","restrict.*adult","coping.*adult",
                "strat.*adult","adults_restrict"), d, vn)
  })

  # ── UI dynamiques HDDS ──
  output$sa_hdds_cols_ui <- renderUI({
    vn <- vars_num()
    hdds_guess <- setdiff(
      grep("^group|^grp.*alim|^hdds_|^dd_|^dietary|^diversit",
           vn, ignore.case = TRUE, value = TRUE),
      c("hdds", "hdds_score"))
    checkboxGroupInput("sa_hdds_cols", "Colonnes des groupes alimentaires (0/1) :",
                       choices = vn,
                       selected = if (length(hdds_guess) > 0) hdds_guess else NULL,
                       width = "100%")
  })

  # ── UI dynamiques HHS ──
  output$sa_hhs_nofood_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_nofood", "Pas de nourriture :",
              c("^hhs.*nofood","^hhs.*no.*food","^hhs1","pas.*nourr",
                "HHSNoFood","hhs_nofood","no_food_home","hunger.*nofood",
                "faim.*nourr","manque.*nourr"), d, vn)
  })
  output$sa_hhs_sleep_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_sleep", "Dormir affamé :",
              c("^hhs.*sleep","^hhs.*bed","^hhs2","dormir.*affam",
                "HHSBedHungry","hhs_sleep","hhs_bed","sleep.*hungry",
                "bed_hungry","coucher.*faim"), d, vn)
  })
  output$sa_hhs_wholeday_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_wholeday", "Journée sans manger :",
              c("^hhs.*whole","^hhs.*day","^hhs3","journee.*sans",
                "HHSNoMealDay","hhs_wholeday","hhs_allday","whole_day",
                "no_eat_day","jour.*sans.*mang"), d, vn)
  })

  # ── UI dynamiques : scores pré-calculés (HDDS · rCSI · HHS) ──
  output$sa_hdds_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hdds_precalc", "Score HDDS (0-12) :",
              c("^hdds$","^hdds_score$","^score.*hdds","^dietary.*diversity",
                "^dd_score","^dds$","diversit"), d, vn)
  })
  output$sa_rcsi_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_precalc", "Score rCSI (0-56) :",
              c("^coping.*strategy","^rcsi$","^rcsi_score$","^score.*rcsi",
                "casesi","^csi$","coping.*index"), d, vn)
  })
  output$sa_hhs_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_precalc", "Score HHS (0-6) :",
              c("^household.*hunger","^hhs$","^hhs_score$","^score.*hhs",
                "^hhs_total","faim.*score","hunger.*score"), d, vn)
  })

  # ── UI dynamiques : LCSI · FIES · MDD-W ──
  sa_select_any <- function(id, label, patterns) {
    d <- df_f()
    guess <- guess_col(d, patterns, type = "any")
    selectInput(id, label, choices = c("— Aucun —" = "", names(d)),
                selected = guess %||% "", width = "100%")
  }
  output$sa_lcsi_class_ui <- renderUI({
    sa_select_any("sa_lcsi_class", "Classe LCSI pré-calculée :",
                  c("^lcsi.*class","^lcsi$","live.*coping.*index","lcs.*class",
                    "coping.*subsistance","strategie.*subsistance"))
  })
  output$sa_lcsi_stress_ui <- renderUI({
    sa_select_any("sa_lcsi_stress", "Stratégies de stress (oui/non) :",
                  c("^lcsi.*stress","stress.*lcsi","^stress","lcsi.*s1"))
  })
  output$sa_lcsi_crisis_ui <- renderUI({
    sa_select_any("sa_lcsi_crisis", "Stratégies de crise (oui/non) :",
                  c("^lcsi.*crise","^lcsi.*crisis","crise.*lcsi","^crise","^crisis"))
  })
  output$sa_lcsi_emerg_ui <- renderUI({
    sa_select_any("sa_lcsi_emerg", "Stratégies d'urgence (oui/non) :",
                  c("^lcsi.*urg","urg.*lcsi","^urgence","^emerg","emergency.*lcsi"))
  })
  output$sa_fies_col_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fies_col", "Score FIES (0-8) :",
              c("^fies","food.*insecurity.*experience","^score.*fies","insecurite.*experience"),
              d, vn)
  })
  output$sa_mddw_col_ui <- renderUI({
    sa_select_any("sa_mddw_col", "MDD-W (≥5 groupes, oui/non) :",
                  c("^mdd.*w","^mddw","mdd_w","dietary.*diversity.*wom",
                    "women.*dietary","diversite.*femme"))
  })

  # ── UI stratification ──
  output$sa_strat_ui <- renderUI({
    vc <- vars_cat()
    selectInput("sa_strat_var", "Stratifier par :",
                choices = c("— Aucune —" = "", vc), selected = "", width = "100%")
  })

  # ── Stockage résultats ──
  sa_res <- reactiveValues(fcs = NULL, rcsi = NULL, hdds = NULL, hhs = NULL,
                           lcsi = NULL, fies = NULL, mddw = NULL)

  # ── Calcul ──
  observeEvent(input$run_sa, {
    d <- df_f(); req(nrow(d) > 0)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    mode <- input$sa_mode

    # Reset des resultats
    sa_res$fcs <- NULL; sa_res$rcsi <- NULL; sa_res$hdds <- NULL; sa_res$hhs <- NULL
    sa_res$lcsi <- NULL; sa_res$fies <- NULL; sa_res$mddw <- NULL
    computed <- character(0)
    skipped  <- character(0)

    # Compteur de colonnes mappées par indicateur (pour diagnostic)
    sa_diag <- list()

    withProgress(message = "Calcul des indicateurs de s\u00e9curit\u00e9 alimentaire...", value = 0, {

      # FCS pre-calcule
      if (mode %in% c("fcs_precalc", "all")) {
        incProgress(0.1, detail = "FCS (pr\u00e9-calcul\u00e9)...")
        col_fcs <- nullify(input$sa_fcs_precalc)
        if (!is.null(col_fcs) && col_fcs %in% names(d)) {
          sa_res$fcs <- classify_fcs_precalc(d[[col_fcs]])
          d$FCS_class <- sa_res$fcs$class
          computed <- c(computed, "FCS (pr\u00e9-calcul\u00e9)")
        } else {
          sa_diag[["FCS pr\u00e9-calcul\u00e9"]] <- "aucune colonne FCS s\u00e9lectionn\u00e9e"
          if (mode == "fcs_precalc") {
            skipped <- c(skipped, "FCS pr\u00e9-calcul\u00e9 : s\u00e9lectionnez une colonne contenant le score FCS d\u00e9j\u00e0 calcul\u00e9")
          }
        }
      }

      # FCS calcule depuis groupes
      if (mode %in% c("fcs_calc", "all")) {
        incProgress(0.15, detail = "FCS (groupes alimentaires)...")
        fcs_cols <- list(
          cereales = nullify(input$sa_fcs_cereales),
          legumineuses = nullify(input$sa_fcs_legumineuses),
          legumes = nullify(input$sa_fcs_legumes),
          fruits = nullify(input$sa_fcs_fruits),
          viande = nullify(input$sa_fcs_viande),
          lait = nullify(input$sa_fcs_lait),
          sucre = nullify(input$sa_fcs_sucre),
          huile = nullify(input$sa_fcs_huile)
        )
        n_fcs_mapped <- sum(!sapply(fcs_cols, is.null))
        fcs <- calc_fcs(d,
          col_cereales = fcs_cols$cereales,
          col_legumineuses = fcs_cols$legumineuses,
          col_legumes = fcs_cols$legumes,
          col_fruits = fcs_cols$fruits,
          col_viande = fcs_cols$viande,
          col_lait = fcs_cols$lait,
          col_sucre = fcs_cols$sucre,
          col_huile = fcs_cols$huile
        )
        if (!is.null(fcs)) {
          sa_res$fcs <- fcs
          d$FCS_class <- fcs$class
          computed <- c(computed, paste0("FCS (", fcs$n_groups, " groupes)"))
        } else {
          sa_diag[["FCS groupes"]] <- paste0(n_fcs_mapped, "/8 groupes mapp\u00e9s (min 4)")
          if (mode == "fcs_calc") {
            skipped <- c(skipped, paste0("FCS : ", n_fcs_mapped, "/8 groupes mapp\u00e9s (minimum 4 requis). ",
                         "S\u00e9lectionnez les colonnes de fr\u00e9quence alimentaire (0-7 jours/semaine)"))
          }
        }
      }

      # rCSI
      if (mode %in% c("rcsi", "all")) {
        incProgress(0.2, detail = "rCSI...")
        col_rcsi_pre <- nullify(input$sa_rcsi_precalc)
        rcsi_cols <- list(
          prefere = nullify(input$sa_rcsi_prefere),
          emprunter = nullify(input$sa_rcsi_emprunter),
          repas = nullify(input$sa_rcsi_repas),
          portions = nullify(input$sa_rcsi_portions),
          adultes = nullify(input$sa_rcsi_adultes)
        )
        n_rcsi_mapped <- sum(!sapply(rcsi_cols, is.null))
        rcsi <- if (!is.null(col_rcsi_pre) && col_rcsi_pre %in% names(d)) {
          calc_rcsi_precalc(d, col_rcsi_pre)
        } else {
          calc_rcsi(d,
            col_moins_prefere = rcsi_cols$prefere,
            col_emprunter = rcsi_cols$emprunter,
            col_reduire_repas = rcsi_cols$repas,
            col_reduire_portions = rcsi_cols$portions,
            col_reduire_adultes = rcsi_cols$adultes
          )
        }
        if (!is.null(rcsi)) {
          sa_res$rcsi <- rcsi
          d$rCSI_class <- rcsi$class
          computed <- c(computed, if (isTRUE(rcsi$precalc)) "rCSI (pr\u00e9-calcul\u00e9)" else "rCSI")
        } else {
          sa_diag[["rCSI"]] <- paste0(n_rcsi_mapped, "/5 strat\u00e9gies mapp\u00e9es (min 3)")
          if (mode == "rcsi") {
            skipped <- c(skipped, paste0("rCSI : ", n_rcsi_mapped, "/5 strat\u00e9gies mapp\u00e9es (minimum 3 requises). ",
                         "S\u00e9lectionnez les colonnes de strat\u00e9gies d'adaptation (0-7 jours)"))
          }
        }
      }

      # HDDS
      if (mode %in% c("hdds", "all")) {
        incProgress(0.2, detail = "HDDS...")
        col_hdds_pre <- nullify(input$sa_hdds_precalc)
        cols_hdds <- input$sa_hdds_cols
        n_hdds <- if (!is.null(cols_hdds)) length(cols_hdds) else 0
        if (!is.null(col_hdds_pre) && col_hdds_pre %in% names(d)) {
          hdds <- calc_hdds_precalc(d, col_hdds_pre)
          sa_res$hdds <- hdds
          d$HDDS_class <- hdds$class
          computed <- c(computed, "HDDS (pr\u00e9-calcul\u00e9)")
        } else if (n_hdds >= 4) {
          hdds <- calc_hdds(d, cols_hdds)
          if (!is.null(hdds)) {
            sa_res$hdds <- hdds
            d$HDDS_class <- hdds$class
            computed <- c(computed, paste0("HDDS (", hdds$n_groups_used, " groupes)"))
          }
        } else {
          sa_diag[["HDDS"]] <- paste0(n_hdds, " groupes coch\u00e9s (min 4)")
          if (mode == "hdds") {
            skipped <- c(skipped, paste0("HDDS : ", n_hdds, " groupes coch\u00e9s (minimum 4 requis). ",
                         "Cochez les colonnes binaires (0/1) des groupes alimentaires"))
          }
        }
      }

      # HHS
      if (mode %in% c("hhs", "all")) {
        incProgress(0.2, detail = "HHS...")
        col_hhs_pre <- nullify(input$sa_hhs_precalc)
        hhs_cols <- list(
          nofood = nullify(input$sa_hhs_nofood),
          sleep = nullify(input$sa_hhs_sleep),
          wholeday = nullify(input$sa_hhs_wholeday)
        )
        n_hhs_mapped <- sum(!sapply(hhs_cols, is.null))
        hhs <- if (!is.null(col_hhs_pre) && col_hhs_pre %in% names(d)) {
          calc_hhs_precalc(d, col_hhs_pre)
        } else {
          calc_hhs(d,
            col_no_food = hhs_cols$nofood,
            col_sleep_hungry = hhs_cols$sleep,
            col_whole_day = hhs_cols$wholeday
          )
        }
        if (!is.null(hhs)) {
          sa_res$hhs <- hhs
          d$HHS_class <- hhs$class
          computed <- c(computed, if (isTRUE(hhs$precalc)) "HHS (pr\u00e9-calcul\u00e9)" else "HHS")
        } else {
          sa_diag[["HHS"]] <- paste0(n_hhs_mapped, "/3 questions mapp\u00e9es (min 2)")
          if (mode == "hhs") {
            skipped <- c(skipped, paste0("HHS : ", n_hhs_mapped, "/3 questions mapp\u00e9es (minimum 2 requises). ",
                         "S\u00e9lectionnez les colonnes de fr\u00e9quence de faim (0-2)"))
          }
        }
      }

      # LCSI
      if (mode %in% c("lcsi", "all")) {
        incProgress(0.1, detail = "LCSI...")
        lcsi_cols <- list(
          stress = nullify(input$sa_lcsi_stress),
          crisis = nullify(input$sa_lcsi_crisis),
          emerg  = nullify(input$sa_lcsi_emerg),
          class  = nullify(input$sa_lcsi_class)
        )
        n_lcsi_mapped <- sum(!sapply(lcsi_cols, is.null))
        if (n_lcsi_mapped >= 1) {
          lcsi <- calc_lcsi(d,
            col_stress = lcsi_cols$stress,
            col_crisis = lcsi_cols$crisis,
            col_emergency = lcsi_cols$emerg,
            col_lcsi_class = lcsi_cols$class
          )
          if (!is.null(lcsi$summary) && nrow(lcsi$summary) > 0) {
            sa_res$lcsi <- lcsi
            computed <- c(computed, "LCSI")
          } else {
            sa_diag[["LCSI"]] <- "colonnes LCSI invalides"
          }
        } else {
          sa_diag[["LCSI"]] <- "aucune colonne LCSI s\u00e9lectionn\u00e9e"
          if (mode == "lcsi") {
            skipped <- c(skipped, paste0("LCSI : s\u00e9lectionnez la classe pr\u00e9-calcul\u00e9e ",
                         "ou au moins une famille de strat\u00e9gies (stress/crise/urgence)"))
          }
        }
      }

      # FIES
      if (mode %in% c("fies", "all")) {
        incProgress(0.1, detail = "FIES...")
        col_fies <- nullify(input$sa_fies_col)
        if (!is.null(col_fies) && col_fies %in% names(d)) {
          fies <- calc_fies(d, col_fies)
          if (!is.null(fies$summary) && nrow(fies$summary) > 0 &&
              sum(fies$summary$N, na.rm = TRUE) > 0) {
            sa_res$fies <- fies
            computed <- c(computed, "FIES")
          } else {
            sa_diag[["FIES"]] <- "score FIES non valide (0-8 attendu)"
          }
        } else {
          sa_diag[["FIES"]] <- "aucune colonne FIES s\u00e9lectionn\u00e9e"
          if (mode == "fies") {
            skipped <- c(skipped, "FIES : s\u00e9lectionnez une colonne de score FIES (0-8)")
          }
        }
      }

      # MDD-W
      if (mode %in% c("mddw", "all")) {
        incProgress(0.1, detail = "MDD-W...")
        col_mddw <- nullify(input$sa_mddw_col)
        if (!is.null(col_mddw) && col_mddw %in% names(d)) {
          mddw <- calc_mddw(d, col_mddw)
          if (!is.na(mddw$pct) && mddw$n_valid > 0) {
            sa_res$mddw <- mddw
            computed <- c(computed, "MDD-W")
          } else {
            sa_diag[["MDD-W"]] <- "colonne MDD-W non exploitable (oui/non attendu)"
          }
        } else {
          sa_diag[["MDD-W"]] <- "aucune colonne MDD-W s\u00e9lectionn\u00e9e"
          if (mode == "mddw") {
            skipped <- c(skipped, "MDD-W : s\u00e9lectionnez la colonne binaire (oui/non) du seuil \u2265 5 groupes")
          }
        }
      }

      incProgress(0.15, detail = "Termin\u00e9")
    })

    # Feedback utilisateur
    if (length(computed) > 0) {
      showNotification(
        paste0("\u2705 Indicateurs calcul\u00e9s : ", paste(computed, collapse = ", ")),
        type = "message", duration = 6)
    }
    if (length(computed) == 0) {
      # Message détaillé selon le mode
      if (mode == "all") {
        # En mode "all", donner un diagnostic précis
        diag_msg <- paste(names(sa_diag), sa_diag, sep = " \u2192 ", collapse = " | ")
        showNotification(
          HTML(paste0(
            "<b>\u26a0 Aucun indicateur calcul\u00e9</b><br/>",
            "En mode \u00ab Tous les indicateurs \u00bb, aucune colonne de s\u00e9curit\u00e9 alimentaire ",
            "n'a \u00e9t\u00e9 d\u00e9tect\u00e9e automatiquement.<br/><br/>",
            "<b>Actions recommand\u00e9es :</b><br/>",
            "\u2022 S\u00e9lectionnez un mode sp\u00e9cifique (FCS, rCSI, HDDS ou HHS)<br/>",
            "\u2022 Mappez manuellement vos colonnes dans le panneau de gauche<br/>",
            "\u2022 V\u00e9rifiez que vos donn\u00e9es contiennent des variables de s\u00e9curit\u00e9 alimentaire"
          )),
          type = "warning", duration = 12)
      } else {
        showNotification(
          HTML(paste0(
            "<b>\u26a0 Aucun indicateur calcul\u00e9</b><br/>",
            "V\u00e9rifiez que les colonnes sont correctement s\u00e9lectionn\u00e9es ",
            "dans le panneau de gauche.<br/><br/>",
            if (length(skipped) > 0) paste0("<b>D\u00e9tail :</b> ", paste(skipped, collapse = "<br/>"))
            else "S\u00e9lectionnez les colonnes correspondant aux variables demand\u00e9es."
          )),
          type = "warning", duration = 10)
      }
    }
    if (length(computed) > 0 && length(skipped) > 0) {
      showNotification(
        paste0("Indicateurs ignor\u00e9s : ", paste(skipped, collapse = " | ")),
        type = "warning", duration = 8)
    }
  })

  # ── Rendu : Synthèse ──
  sa_severity_r <- reactive({
    calc_sa_severity_index(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs)
  })
  sa_verdict_r <- reactive({
    calc_sa_verdict(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs,
                    sev_res = sa_severity_r(), ipc_summary = calc_ipc_summary(.ipc_phases_r()))
  })

  output$sa_verdict <- renderUI({
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_res) return(NULL)
    v <- sa_verdict_r(); req(v)
    tagList(
      tags$div(style = paste0("background:", v$bg, ";border-left:5px solid ", v$color,
        ";border-radius:10px;padding:14px 18px;margin-bottom:12px;"),
        tags$div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          tags$span(style = paste0("background:", v$color,
            ";color:white;font-size:11px;font-weight:700;padding:3px 10px;",
            "border-radius:20px;letter-spacing:.5px;text-transform:uppercase;"), v$badge),
          tags$span(style = "font-size:13px;color:#333;",
            tags$b(v$headline), " ", v$recommendation)
        ),
        if (length(v$bullets) > 0)
          tags$ul(style = "margin:8px 0 0 0;padding-left:18px;font-size:12px;color:#444;line-height:1.7;",
            lapply(v$bullets, function(b) tags$li(b)))
      )
    )
  })

  output$sa_summary_cards <- renderUI({
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_res) return(NULL)
    card <- function(icon_name, col, title, value, sub = NULL) {
      tags$div(class = "zs-kpi-card", style = "flex:1;min-width:150px;padding:14px 16px;",
        tags$div(class = "zs-header",
          tags$div(class = "zs-header-icon", style = paste0("background:", col, ";"),
                   icon(icon_name)),
          tags$div(class = "zs-title", title)),
        tags$div(class = "zs-value", style = paste0("color:", col, ";font-size:26px;"), value),
        if (!is.null(sub)) tags$div(class = "zs-subtitle", sub))
    }
    n_obs <- max(c(length(sa_res$fcs$score), length(sa_res$rcsi$score),
                   length(sa_res$hdds$score), length(sa_res$hhs$score)), 0)
    pct_worst <- function(r, level) {
      if (is.null(r)) return(NULL)
      round(sum(r$class == level, na.rm = TRUE) / sum(!is.na(r$class)) * 100, 1)
    }
    tags$div(style = "display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
      card("users", "#0B3954", "Ménages analysés", n_obs,
           paste0(sum(c(!is.null(sa_res$fcs), !is.null(sa_res$rcsi),
                        !is.null(sa_res$hdds), !is.null(sa_res$hhs))), " indicateur(s) core")),
      if (!is.null(sa_res$fcs))
        card("utensils", "#0077B6", "FCS moyen",
             round(mean(sa_res$fcs$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$fcs, "Pauvre"), " % pauvre")),
      if (!is.null(sa_res$rcsi))
        card("triangle-exclamation", "#E67E22", "rCSI moyen",
             round(mean(sa_res$rcsi$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$rcsi, "Coping \u00e9lev\u00e9"), " % coping \u00e9lev\u00e9")),
      if (!is.null(sa_res$hdds))
        card("seedling", "#27AE60", "HDDS moyen",
             round(mean(sa_res$hdds$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$hdds, "Faible diversit\u00e9"), " % faible diversit\u00e9")),
      if (!is.null(sa_res$hhs))
        card("heartbeat", "#C0392B", "HHS moyen",
             round(mean(sa_res$hhs$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$hhs, "Faim s\u00e9v\u00e8re"), " % faim s\u00e9v\u00e8re")),
      if (!is.null(sa_res$mddw))
        card("bowl-food", "#8E44AD", "MDD-W adéquat",
             paste0(sa_res$mddw$pct, " %"),
             paste0("IC 95 % [", sa_res$mddw$ci_lower, "\u2013", sa_res$mddw$ci_upper, "]")),
      if (!is.null(sa_res$fies))
        card("face-frown", "#b36b00", "FIES mod./sévère",
             paste0(sa_res$fies$pct_moderate_severe, " %"),
             paste0("moyenne ", sa_res$fies$mean_score, "/8"))
    )
  })

  output$sa_severity_plot <- renderPlotly({
    sev <- sa_severity_r()
    if (is.null(sev))
      return(plotly_empty() |> layout(title = list(
        text = "Calculez au moins un indicateur pour l'indice composite", x = .5)))
    plot_sa_donut(sev$summary |> rename(Catégorie = Classe),
                  "Indice composite de sévérité")
  })

  output$sa_synthesis_reading <- renderUI({
    txt <- sa_reading_synthesis(sa_verdict_r())
    txt_sev <- sa_reading_severity(sa_severity_r())
    if (is.null(txt) && is.null(txt_sev)) return(NULL)
    tagList(
      if (!is.null(txt)) pk_info(txt, "lightbulb", "#0077B6", "#e8f6fc"),
      if (!is.null(txt_sev)) pk_info(txt_sev, "chart-simple", "#8E44AD", "#f5eefa")
    )
  })

  output$sa_synthesis_details <- renderUI({
    any_result <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
                  !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_result) {
      sa_gs <- "padding:6px 8px;border:1px solid #e5e5e5;"
      sa_hs <- "padding:8px;border:1px solid #d0e8f5;"
      return(tags$div(style = "padding:15px;",
        tags$div(style = "background:#f0f8ff;border-left:4px solid #0077B6;padding:14px 16px;border-radius:0 8px 8px 0;margin-bottom:16px;",
          tags$div(style = "font-size:14px;font-weight:700;color:#0B3954;margin-bottom:8px;",
            icon("info-circle"), " Guide : Comment utiliser ce module"),
          tags$ol(style = "font-size:12px;color:#333;line-height:1.8;margin:0;padding-left:20px;",
            tags$li("Choisissez un ", tags$b("mode de calcul"), " dans le panneau de gauche."),
            tags$li("Pour chaque indicateur, ", tags$b("s\u00e9lectionnez manuellement"), " les colonnes correspondantes."),
            tags$li("Cliquez sur ", tags$b("\u00ab Calculer les indicateurs \u00bb"), ".")
          ),
          tags$div(style="color:#888;font-size:10px;margin-top:6px;",
            "Astuce : les colonnes aux noms standards (FCSStap, rCSILessQlty, HHSNoFood\u2026) sont pr\u00e9-s\u00e9lectionn\u00e9es automatiquement.")
        ),
        tags$table(style = "width:100%;border-collapse:collapse;font-size:11px;",
          tags$thead(tags$tr(style = "background:#e8f4fd;",
            tags$th(style = sa_hs, "Indicateur"),
            tags$th(style = sa_hs, "Description"),
            tags$th(style = sa_hs, "Format attendu"),
            tags$th(style = sa_hs, "Seuils")
          )),
          tags$tbody(
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "FCS"),
              tags$td(style = sa_gs, "Somme pond\u00e9r\u00e9e de 8 groupes alimentaires"),
              tags$td(style = sa_gs, "8 colonnes, 0-7 jours"),
              tags$td(style = sa_gs, "Pauvre <21 | Limite 21-35 | Acceptable >35")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "rCSI"),
              tags$td(style = sa_gs, "Somme pond\u00e9r\u00e9e de 5 strat\u00e9gies d'adaptation"),
              tags$td(style = sa_gs, "5 colonnes, 0-7 jours"),
              tags$td(style = sa_gs, "Faible 0-3 | Moyen 4-18 | \u00c9lev\u00e9 \u226519")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "HDDS"),
              tags$td(style = sa_gs, "Groupes alimentaires consomm\u00e9s en 24h"),
              tags$td(style = sa_gs, "Colonnes binaires 0/1"),
              tags$td(style = sa_gs, "Faible 0-3 | Moyen 4-5 | \u00c9lev\u00e9 \u22656")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "HHS"),
              tags$td(style = sa_gs, "3 questions d'exp\u00e9rience de faim"),
              tags$td(style = sa_gs, "3 colonnes, valeurs 0-2"),
              tags$td(style = sa_gs, "Aucune 0-1 | Mod\u00e9r\u00e9e 2-3 | S\u00e9v\u00e8re 4-6")
            )
          )
        )
      ))
    }
    # Tableau de synthèse des stats
    stats_all <- bind_rows(
      if (!is.null(sa_res$fcs))  sa_res$fcs$stats  |> mutate(Indicateur = "FCS"),
      if (!is.null(sa_res$rcsi)) sa_res$rcsi$stats |> mutate(Indicateur = "rCSI"),
      if (!is.null(sa_res$hdds)) sa_res$hdds$stats |> mutate(Indicateur = "HDDS"),
      if (!is.null(sa_res$hhs))  sa_res$hhs$stats  |> mutate(Indicateur = "HHS")
    )
    if (nrow(stats_all) == 0) return(NULL)
    tags$div(
      tags$div(class = "section-header", "Statistiques descriptives"),
      renderDT(
        datatable(stats_all |> pivot_wider(names_from = Indicateur, values_from = Valeur),
                  rownames = FALSE, options = list(dom = "t"), class = "compact stripe"),
        outputArgs = list()
      )
    )
  })

  # ── Rendu : FCS ──
  output$sa_fcs_donut <- renderPlotly({
    req(sa_res$fcs); plot_sa_donut(sa_res$fcs$summary, "FCS — Classification PAM")
  })
  output$sa_fcs_hist <- renderPlotly({
    req(sa_res$fcs)
    plot_sa_histogram(sa_res$fcs$score, "Distribution du FCS",
      breaks_lines = list(
        list(x = 21, color = "#e74c3c", label = "Pauvre"),
        list(x = 35, color = "#f39c12", label = "Limite")
      ))
  })
  output$sa_fcs_stats <- renderDT({
    req(sa_res$fcs)
    datatable(sa_add_ci(sa_res$fcs$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : rCSI ──
  output$sa_rcsi_donut <- renderPlotly({
    req(sa_res$rcsi); plot_sa_donut(sa_res$rcsi$summary, "rCSI — Stratégies d'adaptation")
  })
  output$sa_rcsi_hist <- renderPlotly({
    req(sa_res$rcsi)
    plot_sa_histogram(sa_res$rcsi$score, "Distribution du rCSI",
      breaks_lines = list(
        list(x = 4, color = "#f39c12", label = "Moyen"),
        list(x = 19, color = "#e74c3c", label = "Élevé")
      ))
  })
  output$sa_rcsi_stats <- renderDT({
    req(sa_res$rcsi)
    datatable(sa_add_ci(sa_res$rcsi$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : HDDS ──
  output$sa_hdds_donut <- renderPlotly({
    req(sa_res$hdds); plot_sa_donut(sa_res$hdds$summary, "HDDS — Diversité alimentaire")
  })
  output$sa_hdds_hist <- renderPlotly({
    req(sa_res$hdds)
    plot_sa_histogram(sa_res$hdds$score, "Distribution du HDDS",
      breaks_lines = list(
        list(x = 3.5, color = "#e74c3c", label = "Faible"),
        list(x = 5.5, color = "#f39c12", label = "Moyen")
      ))
  })
  output$sa_hdds_groups <- renderDT({
    req(sa_res$hdds)
    if (isTRUE(sa_res$hdds$precalc)) {
      datatable(sa_add_ci(sa_res$hdds$summary) |> select(-Couleur) |> rename(`N ménages` = N),
                rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    } else {
      datatable(sa_res$hdds$group_freq, rownames = FALSE,
                options = list(dom = "t", pageLength = 15), class = "compact stripe") |>
        formatStyle("% consommé",
                    background = styleColorBar(c(0, 100), "#0077B6"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    }
  })

  # ── Rendu : HHS ──
  output$sa_hhs_donut <- renderPlotly({
    req(sa_res$hhs); plot_sa_donut(sa_res$hhs$summary, "HHS — Échelle de la Faim")
  })
  output$sa_hhs_hist <- renderPlotly({
    req(sa_res$hhs)
    plot_sa_histogram(sa_res$hhs$score, "Distribution du HHS",
      breaks_lines = list(
        list(x = 1.5, color = "#f39c12", label = "Modérée"),
        list(x = 3.5, color = "#e74c3c", label = "Sévère")
      ))
  })
  output$sa_hhs_stats <- renderDT({
    req(sa_res$hhs)
    datatable(sa_add_ci(sa_res$hhs$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : Par strate ──
  output$sa_strat_plot <- renderPlotly({
    d <- df_f()
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    req(strat_var)

    # Utiliser le FCS comme indicateur principal pour la stratification
    if (!is.null(sa_res$fcs)) {
      d$FCS_class <- factor(sa_res$fcs$class,
                             levels = c("Pauvre", "Limite", "Acceptable"))
      plot_sa_by_group(d, "FCS_class", strat_var, "FCS par strate",
                        color_map = c("Pauvre" = "#e74c3c", "Limite" = "#f39c12",
                                      "Acceptable" = "#27ae60")) |> nutri_config()
    } else if (!is.null(sa_res$rcsi)) {
      d$rCSI_class <- factor(sa_res$rcsi$class,
                              levels = c("Pas/Peu de coping", "Coping moyen", "Coping élevé"))
      plot_sa_by_group(d, "rCSI_class", strat_var, "rCSI par strate",
                        color_map = c("Pas/Peu de coping" = "#27ae60",
                                      "Coping moyen" = "#f39c12",
                                      "Coping élevé" = "#e74c3c")) |> nutri_config()
    } else {
      plotly_empty() |> layout(title = list(text = "Calculez un indicateur d'abord", x = .5))
    }
  })

  output$sa_strat_table <- renderDT({
    d <- df_f()
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    req(strat_var)

    results <- list()
    if (!is.null(sa_res$fcs)) {
      d$FCS_class <- sa_res$fcs$class
      results$FCS <- d |>
        filter(!is.na(.data[[strat_var]])) |>
        group_by(Groupe = .data[[strat_var]]) |>
        summarise(
          `FCS moyen` = round(mean(sa_res$fcs$score[cur_group_rows()], na.rm = TRUE), 1),
          `% Pauvre` = round(sum(FCS_class == "Pauvre") / n() * 100, 1),
          `% Limite` = round(sum(FCS_class == "Limite") / n() * 100, 1),
          `% Acceptable` = round(sum(FCS_class == "Acceptable") / n() * 100, 1),
          N = n(), .groups = "drop"
        )
    }
    if (!is.null(sa_res$hhs)) {
      d$HHS_class <- sa_res$hhs$class
      results$HHS <- d |>
        filter(!is.na(.data[[strat_var]])) |>
        group_by(Groupe = .data[[strat_var]]) |>
        summarise(
          `HHS moyen` = round(mean(sa_res$hhs$score[cur_group_rows()], na.rm = TRUE), 1),
          `% Faim sévère` = round(sum(HHS_class == "Faim sévère") / n() * 100, 1),
          N = n(), .groups = "drop"
        )
    }

    if (length(results) > 0) {
      tbl <- results[[1]]
      if (length(results) > 1) {
        for (i in 2:length(results)) {
          tbl <- left_join(tbl, results[[i]] |> select(-N), by = "Groupe")
        }
      }
      datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 20),
                class = "compact stripe hover")
    } else {
      datatable(data.frame(Message = "Calculez les indicateurs d'abord"), rownames = FALSE)
    }
  })

  # ══════════════════════════════════════════════════════════════════
  # MODULE SA — NOUVEAUX OUTPUTS (FCS Composition · rCSI Détail · IPC · Rounds)
  # ══════════════════════════════════════════════════════════════════

  # ── FCS Composition ──
  output$sa_fcs_composition_bar <- renderPlotly({
    req(sa_res$fcs)
    tryCatch(
      plot_fcs_composition_bar(sa_res$fcs),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_radar <- renderPlotly({
    tryCatch(
      plot_sa_radar(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs),
      error = function(e) plotly_empty() |> layout(title = list(text = "Radar — calculez 2+ indicateurs", x=.5))
    )
  })

  output$sa_fcs_composition_table <- renderDT({
    req(sa_res$fcs)
    comp <- calc_fcs_composition(sa_res$fcs)
    req(comp)
    datatable(
      comp |> rename(
        `Groupe alimentaire` = Groupe, `Poids PAM` = Poids_PAM,
        `Consommation moy (j/sem)` = Consommation_moy,
        `Contribution moy (pts)` = Contribution_moy,
        `Contribution (%)` = Contribution_pct
      ),
      rownames = FALSE, options = list(dom = "t", pageLength = 12),
      class = "compact stripe"
    ) |>
      formatStyle(
        "Contribution (%)",
        background = styleColorBar(c(0, 100), "#0077B6"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ── rCSI Composantes ──
  output$sa_rcsi_components_plot <- renderPlotly({
    req(sa_res$rcsi)
    d_cur <- df_f()
    req(d_cur)
    cols_rcsi <- list(
      col_moins_prefere    = if (!is.null(input$sa_rcsi_prefere))    input$sa_rcsi_prefere    else NULL,
      col_emprunter        = if (!is.null(input$sa_rcsi_emprunter))   input$sa_rcsi_emprunter   else NULL,
      col_reduire_repas    = if (!is.null(input$sa_rcsi_repas))       input$sa_rcsi_repas       else NULL,
      col_reduire_portions = if (!is.null(input$sa_rcsi_portions))    input$sa_rcsi_portions    else NULL,
      col_reduire_adultes  = if (!is.null(input$sa_rcsi_adultes))     input$sa_rcsi_adultes     else NULL
    )
    tryCatch(
      do.call(plot_rcsi_components, c(list(d = d_cur), cols_rcsi)),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_rcsi_components_table <- renderDT({
    req(sa_res$rcsi)
    d_cur <- df_f(); req(d_cur)
    cols_rcsi <- list(
      col_moins_prefere    = if (!is.null(input$sa_rcsi_prefere))    input$sa_rcsi_prefere    else NULL,
      col_emprunter        = if (!is.null(input$sa_rcsi_emprunter))   input$sa_rcsi_emprunter   else NULL,
      col_reduire_repas    = if (!is.null(input$sa_rcsi_repas))       input$sa_rcsi_repas       else NULL,
      col_reduire_portions = if (!is.null(input$sa_rcsi_portions))    input$sa_rcsi_portions    else NULL,
      col_reduire_adultes  = if (!is.null(input$sa_rcsi_adultes))     input$sa_rcsi_adultes     else NULL
    )
    comp <- tryCatch(
      do.call(calc_rcsi_components, c(list(d = d_cur), cols_rcsi)),
      error = function(e) NULL
    )
    req(comp)
    datatable(
      comp |> rename(
        `Stratégie d'adaptation` = Stratégie, `Poids sévérité` = Poids,
        `Moy jours/sem` = Moy_jours, `% ménages recourant` = Pct_recours,
        `Contribution pondérée` = Contribution_moy
      ),
      rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
    ) |>
      formatStyle(
        "% ménages recourant",
        background = styleColorBar(c(0, 100), "#E67E22"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
      )
  })

  # ── Phases IPC/CH ──
  .ipc_phases_r <- reactive({
    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL
    d_cur  <- df_f()
    muac_s <- if (!is.null(d_cur) && ".muac" %in% names(d_cur)) d_cur$.muac else NULL
    if (is.null(fcs_s) && is.null(rcsi_s) && is.null(hhs_s)) return(NULL)
    calc_ipc_phase(fcs_s, rcsi_s, hhs_s, muac_s)
  })

  output$sa_ipc_kpi_cards <- renderUI({
    phases <- .ipc_phases_r(); req(phases)
    summ   <- calc_ipc_summary(phases); req(summ)
    phase3plus <- sum(summ$N[summ$Phase >= 3])
    pct3plus   <- round(phase3plus / sum(summ$N) * 100, 1)
    phase4plus <- sum(summ$N[summ$Phase >= 4])
    pct4plus   <- round(phase4plus / sum(summ$N) * 100, 1)

    col_main <- if (pct3plus >= 20) "#C80000" else if (pct3plus >= 10) "#E67800" else "#59A608"

    tags$div(
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;margin-bottom:10px;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Phase 3+ (Crise+)"),
        tags$div(style=paste0("font-size:32px;font-weight:800;color:", col_main, ";"), paste0(pct3plus, "%")),
        tags$div(style="font-size:11px;color:#555;", paste0(phase3plus, " / ", sum(summ$N), " ménages"))
      ),
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;margin-bottom:10px;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Phase 4-5 (Urgence+)"),
        tags$div(style="font-size:28px;font-weight:800;color:#C80000;", paste0(pct4plus, "%")),
        tags$div(style="font-size:11px;color:#555;", paste0(phase4plus, " ménages"))
      ),
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Répartition phases"),
        lapply(1:5, function(ph) {
          r <- summ[summ$Phase == ph, ]
          if (nrow(r) == 0 || r$N == 0) return(NULL)
          tags$div(style="display:flex;align-items:center;gap:6px;margin:3px 0;",
            tags$div(style=paste0("width:12px;height:12px;border-radius:2px;background:", r$Couleur, ";flex-shrink:0;")),
            tags$span(style="font-size:10px;color:#555;flex:1;", paste0("Ph", ph)),
            tags$span(style="font-size:11px;font-weight:700;", paste0(r$Pct, "%"))
          )
        })
      )
    )
  })

  output$sa_ipc_phases_plot <- renderPlotly({
    phases <- .ipc_phases_r(); req(phases)
    tryCatch(
      plot_ipc_phases(phases),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_ipc_by_group_plot <- renderPlotly({
    phases <- .ipc_phases_r(); req(phases)
    d_cur <- df_f(); req(d_cur)
    grp_var <- if (!is.null(input$sa_ipc_group_var) && input$sa_ipc_group_var != "")
      input$sa_ipc_group_var else NULL
    grp_vec <- if (!is.null(grp_var) && grp_var %in% names(d_cur)) d_cur[[grp_var]] else NULL
    tryCatch(
      plot_ipc_phases(phases, group_var = grp_vec),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_ipc_table <- renderDT({
    phases <- .ipc_phases_r(); req(phases)
    summ <- calc_ipc_summary(phases); req(summ)
    datatable(
      summ |> select(-Couleur) |> rename(`Phase IPC` = Phase, Libellé = Label,
                                          `N ménages` = N, `%` = Pct),
      rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
    ) |>
      formatStyle("Phase IPC",
        target = "row",
        backgroundColor = styleEqual(1:5, c("#2E7D52","#D4A843","#D98B14","#BF3636","#5C1A1A")),
        color = styleEqual(1:5, c("white","black","white","white","white"))
      )
  })

  # ── Comparaison inter-rounds ──
  output$sa_rounds_plot <- renderPlotly({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    req(length(round_col) > 0)
    round_col <- round_col[1]
    if (length(unique(d_cur[[round_col]])) < 2)
      return(plotly_empty() |> layout(title = list(
        text = "Au moins 2 rounds requis pour la comparaison", x = .5)))

    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hdds_s <- if (!is.null(sa_res$hdds)) sa_res$hdds$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL

    if (is.null(fcs_s) && is.null(rcsi_s) && is.null(hdds_s) && is.null(hhs_s))
      return(plotly_empty() |> layout(title = list(text = "Calculez les indicateurs SA d'abord", x = .5)))

    tryCatch(
      plot_sa_rounds(d_cur, round_col, fcs_s, rcsi_s, hdds_s, hhs_s),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_rounds_table <- renderDT({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    req(length(round_col) > 0)
    round_col <- round_col[1]
    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hdds_s <- if (!is.null(sa_res$hdds)) sa_res$hdds$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL

    tbl <- tryCatch(
      calc_sa_by_round(d_cur, round_col, fcs_s, rcsi_s, hdds_s, hhs_s),
      error = function(e) NULL
    )
    req(tbl)
    datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
              class = "compact stripe")
  })

  # ══════════════════════════════════════════════════════════════════
  # MODULE SA — COUCHE EXPERTE (lectures · OMS · ménages · export)
  # ══════════════════════════════════════════════════════════════════

  # ── Lectures expertes par indicateur ──
  output$sa_fcs_reading <- renderUI({
    txt <- sa_reading_fcs(sa_res$fcs); if (is.null(txt)) return(NULL)
    pk_info(txt, "utensils", "#0077B6", "#e8f6fc")
  })
  output$sa_rcsi_reading <- renderUI({
    txt <- sa_reading_rcsi(sa_res$rcsi); if (is.null(txt)) return(NULL)
    pk_info(txt, "triangle-exclamation", "#E67E22", "#fff3e0")
  })
  output$sa_hdds_reading <- renderUI({
    txt <- sa_reading_hdds(sa_res$hdds); if (is.null(txt)) return(NULL)
    pk_info(txt, "seedling", "#27AE60", "#eafaf1")
  })
  output$sa_hhs_reading <- renderUI({
    txt <- sa_reading_hhs(sa_res$hhs); if (is.null(txt)) return(NULL)
    pk_info(txt, "heartbeat", "#C0392B", "#fdf2f2")
  })

  # ── Lecture composition FCS ──
  output$sa_fcs_comp_reading <- renderUI({
    r <- sa_res$fcs; if (is.null(r)) return(NULL)
    comp <- calc_fcs_composition(r); if (is.null(comp) || nrow(comp) == 0) return(NULL)
    top <- comp[1, ]
    txt <- sprintf(paste0(
      "Lecture experte : le groupe %s contribue le plus au score moyen (%.1f pts, %.1f %% du FCS), ",
      "suivi de %s. Les c\u00e9r\u00e9ales et l\u00e9gumineuses p\u00e8sent structurellement lourd (poids PAM \u00d72 et \u00d73) : ",
      "un score acceptable peut masquer une alimentation peu diversifi\u00e9e si ces seuls groupes sont consomm\u00e9s."),
      top$Groupe, top$Contribution_moy, top$Contribution_pct,
      if (nrow(comp) > 1) comp$Groupe[2] else "\u2014")
    pk_info(txt, "chart-bar", "#0077B6", "#e8f6fc")
  })

  # ── Lecture rCSI détail ──
  output$sa_rcsi_comp_reading <- renderUI({
    if (is.null(sa_res$rcsi)) return(NULL)
    d_cur <- df_f(); req(d_cur)
    comp <- tryCatch(calc_rcsi_components(d_cur,
      col_moins_prefere    = if (is.null(input$sa_rcsi_prefere) || input$sa_rcsi_prefere == "") NULL else input$sa_rcsi_prefere,
      col_emprunter        = if (is.null(input$sa_rcsi_emprunter) || input$sa_rcsi_emprunter == "") NULL else input$sa_rcsi_emprunter,
      col_reduire_repas    = if (is.null(input$sa_rcsi_repas) || input$sa_rcsi_repas == "") NULL else input$sa_rcsi_repas,
      col_reduire_portions = if (is.null(input$sa_rcsi_portions) || input$sa_rcsi_portions == "") NULL else input$sa_rcsi_portions,
      col_reduire_adultes  = if (is.null(input$sa_rcsi_adultes) || input$sa_rcsi_adultes == "") NULL else input$sa_rcsi_adultes),
      error = function(e) NULL)
    if (is.null(comp)) return(NULL)
    top <- comp[1, ]
    txt <- sprintf(paste0(
      "Lecture experte : la strat\u00e9gie la plus fr\u00e9quente est \u00ab %s \u00bb (%.1f %% des m\u00e9nages, ",
      "%.2f j/sem en moyenne, contribution %.2f pts). ",
      "Les strat\u00e9gies de poids 3 (restriction des adultes) signalent les situations les plus s\u00e9v\u00e8res : ",
      "les suivre s\u00e9par\u00e9ment permet de distinguer stress \u00e9conomique et ins\u00e9curit\u00e9 alimentaire install\u00e9e."),
      top$Stratégie, top$Pct_recours, top$Moy_jours, top$Contribution_moy)
    pk_info(txt, "exclamation-circle", "#E67E22", "#fff3e0")
  })

  # ── Lecture stratification (χ²) ──
  output$sa_strat_reading <- renderUI({
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    if (is.null(strat_var)) return(NULL)
    d_cur <- df_f(); req(d_cur, strat_var %in% names(d_cur))
    class_vec <- if (!is.null(sa_res$fcs)) sa_res$fcs$class
                 else if (!is.null(sa_res$rcsi)) sa_res$rcsi$class
                 else NULL
    if (is.null(class_vec)) return(NULL)
    test <- calc_strat_test(class_vec, d_cur[[strat_var]])
    txt <- sa_reading_strat(test, strat_var)
    pk_info(txt, "chart-simple", "#0B3954", "#eef2f5")
  })

  # ── Lecture IPC ──
  output$sa_ipc_reading <- renderUI({
    phases <- .ipc_phases_r(); if (is.null(phases)) return(NULL)
    txt <- sa_reading_ipc(calc_ipc_summary(phases)); if (is.null(txt)) return(NULL)
    pk_info(txt, "globe", "#D4A843", "#fff8e1")
  })

  # ── Lecture rounds (tendance + test) ──
  output$sa_rounds_reading <- renderUI({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    if (length(round_col) == 0 || is.null(sa_res$fcs)) return(NULL)
    trend <- tryCatch(
      calc_rounds_trend(d_cur[[round_col[1]]], sa_res$fcs$class, "Pauvre",
                        "FCS pauvre"),
      error = function(e) NULL)
    txt <- sa_reading_rounds(trend); if (is.null(txt)) return(NULL)
    pk_info(txt, "calendar-alt", "#27AE60", "#eafaf1")
  })

  # ── Indicateurs OMS : LCSI · FIES · MDD-W ──
  sa_oms_any <- function() {
    !is.null(sa_res$lcsi) || !is.null(sa_res$fies) || !is.null(sa_res$mddw)
  }
  output$sa_lcsi_plot <- renderPlotly({
    if (is.null(sa_res$lcsi))
      return(plotly_empty() |> layout(title = list(
        text = "LCSI non calculé — mappez une classe ou les stratégies", x = .5)))
    tryCatch(plot_lcsi_plotly(sa_res$lcsi),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_lcsi_table <- renderDT({
    if (is.null(sa_res$lcsi) || is.null(sa_res$lcsi$summary))
      return(datatable(data.frame(Message = "LCSI non calculé"), rownames = FALSE, options = list(dom = "t")))
    datatable(sa_add_ci(sa_res$lcsi$summary) |> select(-Couleur) |>
                rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })
  output$sa_fies_plot <- renderPlotly({
    if (is.null(sa_res$fies))
      return(plotly_empty() |> layout(title = list(
        text = "FIES non calculé — sélectionnez une colonne de score (0-8)", x = .5)))
    tryCatch(plot_fies_plotly(sa_res$fies),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_fies_table <- renderDT({
    if (is.null(sa_res$fies) || is.null(sa_res$fies$summary))
      return(datatable(data.frame(Message = "FIES non calculé"), rownames = FALSE, options = list(dom = "t")))
    s <- sa_add_ci(sa_res$fies$summary) |> select(-Couleur)
    datatable(s, rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })
  output$sa_mddw_plot <- renderPlotly({
    if (is.null(sa_res$mddw))
      return(plotly_empty() |> layout(title = list(
        text = "MDD-W non calculé — sélectionnez la colonne binaire", x = .5)))
    tryCatch(plot_mddw_plotly(sa_res$mddw),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_oms_reading <- renderUI({
    if (!sa_oms_any()) return(NULL)
    tagList(
      if (!is.null(sa_res$lcsi)) pk_info(sa_reading_lcsi(sa_res$lcsi),
        "briefcase", "#0077B6", "#e8f6fc"),
      if (!is.null(sa_res$fies)) pk_info(sa_reading_fies(sa_res$fies),
        "face-frown", "#E67E22", "#fff3e0"),
      if (!is.null(sa_res$mddw)) pk_info(sa_reading_mddw(sa_res$mddw),
        "bowl-food", "#27AE60", "#eafaf1")
    )
  })

  # ── Table ménages ──
  sa_household_r <- reactive({
    d_cur <- df_f(); req(d_cur)
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs) ||
               !is.null(sa_res$lcsi) || !is.null(sa_res$fies) || !is.null(sa_res$mddw)
    if (!any_res) return(NULL)
    build_sa_household_table(d_cur,
      fcs_res = sa_res$fcs, rcsi_res = sa_res$rcsi,
      hdds_res = sa_res$hdds, hhs_res = sa_res$hhs,
      lcsi_res = sa_res$lcsi, fies_res = sa_res$fies,
      mddw_res = sa_res$mddw, sev_res = sa_severity_r(),
      ipc_phases = .ipc_phases_r())
  })
  output$sa_household_table <- renderDT({
    tbl <- sa_household_r(); req(tbl)
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
              class = "compact stripe hover", filter = "top")
  })

  # ── Exports ──
  sa_frames_r <- reactive({
    sa_export_frames(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs,
                     sa_res$lcsi, sa_res$fies, sa_res$mddw,
                     sa_severity_r(), calc_ipc_summary(.ipc_phases_r()),
                     sa_household_r(), sa_verdict_r())
  })
  output$sa_dl_xlsx <- downloadHandler(
    filename = function() paste0("securite_alimentaire_", Sys.Date(), ".xlsx"),
    content = function(file) {
      frames <- sa_frames_r()
      if (length(frames) == 0) { writeLines("Aucun résultat", file); return() }
      wb <- openxlsx::createWorkbook()
      for (nm in names(frames)) {
        openxlsx::addWorksheet(wb, substr(nm, 1, 31))
        openxlsx::writeData(wb, substr(nm, 1, 31), frames[[nm]])
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })
  sa_write_csv <- function(file) {
    hh <- sa_household_r()
    if (is.null(hh)) { writeLines("Aucun résultat", file); return(invisible()) }
    utils::write.csv2(hh, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
  output$sa_dl_csv_hh <- downloadHandler(
    filename = function() paste0("menages_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) sa_write_csv(file))
  output$sa_dl_csv_hh2 <- downloadHandler(
    filename = function() paste0("menages_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) sa_write_csv(file))
  output$sa_dl_csv_summary <- downloadHandler(
    filename = function() paste0("synthese_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) {
      frames <- sa_frames_r()
      frames$Menages <- NULL
      if (length(frames) == 0) { writeLines("Aucun résultat", file); return() }
      synth <- bind_rows(lapply(names(frames), function(nm) {
        s <- frames[[nm]]
        if (is.null(s) || nrow(s) == 0) return(NULL)
        s$Section <- nm
        s
      }))
      utils::write.csv2(synth, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$sa_export_info <- renderUI({
    frames <- sa_frames_r()
    tags$div(style = "font-size:11px;color:#666;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
      icon("circle-info"), " Contenu disponible : ",
      tags$b(paste(length(frames), "feuille(s)")), " — ",
      paste(setdiff(names(frames), "Menages"), collapse = ", "),
      if ("Menages" %in% names(frames))
        paste0(" + table ménages (", nrow(sa_household_r()), " lignes)")
    )
  })

  # ══════════════════════════════════════════════════════════════════
  # RÉDUCTION DE DIMENSION — Kaiser + FactoInvestigate + Couleurs
  # ══════════════════════════════════════════════════════════════════

  # ── Helper : critère bâton brisé (Broken Stick) ──────────────────
  broken_stick <- function(n_vars) {
    rev(cumsum(rev(1 / seq_len(n_vars)))) / n_vars * 100
  }

  # ── Helper robuste : extraction var/ind selon la méthode ─────────
  # Gère PCA, MCA, FAMD, CA sans "indice hors limites"
  get_var_info <- function(r, method) {
    tryCatch({
      if (method == "famd") {
        # FAMD : combiner quanti.var et quali.var AVEC marqueur de type
        qn <- r$quanti.var
        ql <- r$quali.var
        if (is.null(qn) && is.null(ql)) return(NULL)
        nc <- if (!is.null(qn)) min(ncol(qn$coord), 10)
              else min(ncol(ql$coord), 10)
        
        combine_typed <- function(a, b) {
          if (is.null(a) && is.null(b)) return(NULL)
          da <- if (!is.null(a)) {
            d <- as.data.frame(a[, 1:nc, drop=FALSE])
            d$.var_type <- "quanti"
            d
          }
          db <- if (!is.null(b)) {
            d <- as.data.frame(b[, 1:nc, drop=FALSE])
            d$.var_type <- "quali"
            d
          }
          if (is.null(da)) return(db)
          if (is.null(db)) return(da)
          rbind(da, db)
        }
        
        return(list(
          coord   = combine_typed(qn$coord,   ql$coord),
          cos2    = combine_typed(qn$cos2,    ql$cos2),
          contrib = combine_typed(qn$contrib, ql$contrib),
          # Accès séparé pour le cercle de corrélations
          quanti_coord  = if (!is.null(qn)) as.data.frame(qn$coord[, 1:nc, drop=FALSE]),
          quali_coord   = if (!is.null(ql)) as.data.frame(ql$coord[, 1:nc, drop=FALSE]),
          quanti_cos2   = if (!is.null(qn)) as.data.frame(qn$cos2[, 1:nc, drop=FALSE]),
          quali_cos2    = if (!is.null(ql)) as.data.frame(ql$cos2[, 1:nc, drop=FALSE]),
          quanti_contrib = if (!is.null(qn)) as.data.frame(qn$contrib[, 1:nc, drop=FALSE]),
          quali_contrib  = if (!is.null(ql)) as.data.frame(ql$contrib[, 1:nc, drop=FALSE])
        ))
      }
      obj <- switch(method,
        pca  = r$var,
        mca  = r$var,
        ca   = r$col,
        r$var
      )
      if (is.null(obj)) return(NULL)
      nc <- min(ncol(obj$coord), 10)
      list(
        coord   = as.data.frame(obj$coord[, 1:nc, drop=FALSE]),
        cos2    = as.data.frame(obj$cos2[,  1:nc, drop=FALSE]),
        contrib = as.data.frame(obj$contrib[,1:nc, drop=FALSE])
      )
    }, error = function(e) NULL)
  }

  get_ind_info <- function(r, method) {
    tryCatch({
      obj <- switch(method,
        pca  = r$ind,
        mca  = r$ind,
        famd = r$ind,
        ca   = r$row,   # lignes AFC
        r$ind
      )
      if (is.null(obj)) return(NULL)
      nc <- min(ncol(obj$coord), 10)
      list(
        coord = as.data.frame(obj$coord[, 1:nc, drop=FALSE]),
        cos2  = as.data.frame(obj$cos2[,  1:nc, drop=FALSE])
      )
    }, error = function(e) NULL)
  }

  # Pour AFC : lignes et colonnes ensemble
  get_ca_both <- function(r) {
    tryCatch({
      nc_r <- min(ncol(r$row$coord), 2)
      nc_c <- min(ncol(r$col$coord), 2)
      rows <- as.data.frame(r$row$coord[, 1:nc_r, drop=FALSE])
      cols <- as.data.frame(r$col$coord[, 1:nc_c, drop=FALSE])
      names(rows)[1:2] <- c("Dim.1","Dim.2")
      names(cols)[1:2] <- c("Dim.1","Dim.2")
      rows$Label <- rownames(rows); rows$Type <- "Ligne"
      cols$Label <- rownames(cols); cols$Type <- "Colonne"
      bind_rows(rows, cols)
    }, error = function(e) NULL)
  }

  # ── Stockage des ggplots pour téléchargement ─────────────────────
  rdim_gg <- reactiveValues(
    scree=NULL, circle=NULL, biplot=NULL,
    contrib=NULL, cos2=NULL, heatmap=NULL
  )

  # ── Helper : test de permutation (FactoInvestigate-style) ────────
  perm_test_pca <- function(data_scaled, n_perm = 200) {
    real_eig <- prcomp(data_scaled, center=FALSE, scale.=FALSE)$sdev^2
    n         <- min(length(real_eig), ncol(data_scaled))
    perm_mat  <- replicate(n_perm, {
      d_perm <- apply(data_scaled, 2, sample)
      prcomp(d_perm, center=FALSE, scale.=FALSE)$sdev^2[1:n]
    })
    if (!is.matrix(perm_mat)) perm_mat <- matrix(perm_mat, nrow=n)
    p_vals <- sapply(seq_len(n), function(i) mean(perm_mat[i,] >= real_eig[i]))
    list(real_eig=real_eig[1:n], p_vals=p_vals,
         sig_axes=which(p_vals < 0.05))
  }

  # ── UI dynamiques de la section rdim ─────────────────────────────
  output$rdim_vars_ui <- renderUI({
    vn <- vars_num(); vc <- vars_cat()
    method <- input$rdim_method
    
    if (method == "pca") {
      # ACP : variables NUMÉRIQUES uniquement
      selectizeInput("rdim_vars","Variables numériques :", choices=vn,
                     selected=head(vn, min(8, length(vn))), multiple=TRUE, width="100%")
    } else if (method == "famd") {
      # FAMD : mix obligatoire — séparer les groupes visuellement
      choices_grouped <- list(
        "Variables numériques" = vn,
        "Variables catégorielles" = vc
      )
      selected_famd <- c(head(vn, min(4, length(vn))), head(vc, min(3, length(vc))))
      selectizeInput("rdim_vars","Variables mixtes (num. + cat.) :", choices=choices_grouped,
                     selected=selected_famd, multiple=TRUE, width="100%")
    } else if (method == "auto") {
      # Auto : toutes variables, groupées
      choices_grouped <- list(
        "Variables numériques" = vn,
        "Variables catégorielles" = vc
      )
      selectizeInput("rdim_vars","Variables :", choices=choices_grouped,
                     selected=head(vn, min(8, length(vn))), multiple=TRUE, width="100%")
    } else if (method == "mca") {
      # ACM : variables CATÉGORIELLES uniquement
      selectizeInput("rdim_vars","Variables catégorielles :", choices=vc,
                     selected=head(vc, min(5, length(vc))), multiple=TRUE, width="100%")
    } else {
      # AFC : 2 variables catégorielles
      tagList(
        selectInput("rdim_var1","Variable lignes :", choices=vc,
                    selected=vc[1], width="100%"),
        selectInput("rdim_var2","Variable colonnes :", choices=vc,
                    selected=if(length(vc)>1)vc[2] else vc[1], width="100%")
      )
    }
  })

  output$rdim_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("rdim_grp","Colorier individus par :",
                choices=c("Aucun"="", vc), selected="", width="100%")
  })

  output$rdim_biplot_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("rdim_biplot_grp","Variable de groupage :",
                choices=c("Aucun"="", vc), selected=input$rdim_grp, width="100%")
  })

  output$rdim_contrib_dim_ui <- renderUI({
    req(rdim_res())
    eig <- tryCatch(get_eigenvalue(rdim_res()$res), error=function(e) NULL)
    n_dim <- if (!is.null(eig)) nrow(eig) else 5
    selectInput("rdim_contrib_dim","Dimension :",
                choices=setNames(1:n_dim, paste0("Dim.",1:n_dim)),
                selected=1, width="100%")
  })

  output$rdim_method_info <- renderPrint({
    m <- input$rdim_method
    info <- switch(m,
      "auto" = "Choix automatique :\n• Toutes numériques → ACP\n• Toutes catégorielles → ACM\n• Mixte → FAMD",
      "pca"  = "ACP — Analyse en Composantes Principales\n• Variables numériques uniquement\n• Centrer-réduire recommandé\n• Maximise la variance expliquée",
      "mca"  = "ACM — Analyse des Correspondances Multiples\n• Variables catégorielles uniquement\n• Tableau disjonctif complet",
      "famd" = "FAMD — Factor Analysis of Mixed Data\n• Variables mixtes (num. + cat.)\n• Combine ACP et ACM",
      "ca"   = "AFC — Analyse Factorielle des Correspondances\n• Tableau de contingence 2 variables\n• Similarité profils ligne/colonne"
    )
    cat(info)
  })

  # Badge Kaiser dans la sidebar (après analyse)
  output$rdim_kaiser_badge <- renderUI({
    req(rdim_res())
    r <- rdim_res()$res
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))
    n_kaiser <- sum(eig[,1] > 1)
    if (n_kaiser == 0) return(NULL)
    tags$div(style="margin-top:6px;",
      tags$div(style=paste0("background:linear-gradient(135deg,#0B3954,#00838F);",
                            "color:white;border-radius:8px;padding:8px 10px;font-size:12px;"),
        tags$b("📐 Critère de Kaiser"),
        tags$br(),
        paste0("→ ", n_kaiser, " axe", if(n_kaiser>1)"s", " retenu",
               if(n_kaiser>1)"s", " (λ > 1)"),
        tags$br(),
        tags$span(style="font-size:10px;opacity:.85;",
          paste0("Variance cumulée : ",
                 round(eig[min(n_kaiser, nrow(eig)), 3], 1), "%"))
      )
    )
  })

  observeEvent(input$run_rdim, {
    req(mapped_data()); d <- df_f()
    method <- input$rdim_method

    # ===== VALIDATION AVEC NOUVEAU SYSTEME =====
    validation <- if (method == "ca") {
      validate_ca(d, input$rdim_var1, input$rdim_var2)
    } else {
      validate_rdim(d, method, vars = input$rdim_vars)
    }
    
    # Si validation echoue → afficher erreur et arreter
    if (!validation$valid) {
      showNotification(
        HTML(sprintf(
          '<div style="color: #BF3636; background: #ffebee; padding: 12px; border-radius: 4px; border-left: 4px solid #BF3636;">
            <strong>⚠️ Erreur validation:</strong><br/>%s
          </div>',
          validation$error
        )),
        type = "error",
        duration = 8,
        closeButton = TRUE
      )
      return()
    }

    withProgress(message = "Calcul en cours...", {
      tryCatch({
        
        # ===== DETERMINER VRAIE METHODE si auto =====
        true_method <- method
        if (method == "auto") {
          vars_clean <- input$rdim_vars[input$rdim_vars %in% names(d)]
          all_num <- all(vapply(vars_clean, function(v) is.numeric(d[[v]]), logical(1)))
          all_cat <- all(vapply(vars_clean, function(v) is.factor(d[[v]]) || is.character(d[[v]]), logical(1)))
          true_method <- if (all_num) "pca" else if (all_cat) "mca" else "famd"
        }
        
        # ===== EXECUTER LA REDUCTION =====
        res <- switch(true_method,
          
          "pca" = {
            # Variables numeriques seulement
            vars_num <- validation$vars_clean
            d_pca <- d[, vars_num, drop = FALSE] |> drop_na()
            PCA(d_pca, 
                scale.unit = input$rdim_scale, 
                ncp = min(input$rdim_ndim, ncol(d_pca) - 1), 
                graph = FALSE)
          },
          
          "mca" = {
            # Variables categorielles seulement
            vars_cat <- validation$vars_clean
            d_mca <- d[, vars_cat, drop = FALSE] |>
              mutate(across(everything(), as.factor)) |>
              drop_na()
            MCA(d_mca, ncp = min(input$rdim_ndim, 10), graph = FALSE)
          },
          
          "famd" = {
            # Variables mixtes
            vars_famd <- validation$vars_clean
            d_famd <- d[, vars_famd, drop = FALSE] |>
              mutate(across(where(is.character), as.factor)) |>
              drop_na()
            ncp_famd <- max(2, min(input$rdim_ndim, ncol(d_famd) - 1))
            FAMD(d_famd, ncp = ncp_famd, graph = FALSE)
          },
          
          "ca" = {
            # Tableau contingence
            tbl <- table(d[[input$rdim_var1]], d[[input$rdim_var2]])
            CA(tbl, ncp = min(input$rdim_ndim, min(dim(tbl)) - 1), graph = FALSE)
          },
          
          stop(sprintf("Methode '%s' non supportee", true_method))
        )

        # ===== SAUVEGARDER RESULTATS =====
        rdim_res(list(
          res = res, 
          method = true_method, 
          method_display = method,
          vars = if (true_method != "ca") validation$vars_clean else c(input$rdim_var1, input$rdim_var2)
        ))
        
        # ===== NOTIFICATION SUCCES =====
        msg_detail <- switch(true_method,
          "pca" = sprintf("%d variables numeriques", validation$n_vars),
          "mca" = sprintf("%d variables categorielles", validation$n_vars),
          "famd" = sprintf("%d num. + %d cat.", validation$n_numeric, validation$n_categorical),
          "ca" = sprintf("Table %s", validation$table_dim),
          "?"
        )
        
        showNotification(
          HTML(sprintf(
            '<div style="color: #2e7d32; background: #e8f5e9; padding: 10px; border-radius: 4px;">
              <strong>✅ %s reussie</strong><br/>
              %d observations | %s
            </div>',
            toupper(true_method),
            validation$n_obs,
            msg_detail
          )),
          type = "message",
          duration = 5,
          closeButton = TRUE
        )
        
      }, error = function(e) {
        showNotification(
          HTML(sprintf(
            '<div style="color: #BF3636; background: #ffebee; padding: 10px; border-radius: 4px;">
              <strong>❌ Erreur execution:</strong><br/>%s
            </div>',
            e$message
          )),
          type = "error",
          duration = 8,
          closeButton = TRUE
        )
      })
    })
  })

  output$rdim_scree <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))
    df_eig <- tibble(
      Dim         = paste0("Dim.",1:nrow(eig)),
      Lambda      = round(eig[,1], 4),
      Variance_pct= round(eig[,2], 2),
      Variance_cum= round(eig[,3], 2),
      # Kaiser : vert si λ > 1, orange sinon
      Kaiser      = ifelse(eig[,1] > 1, "Retenu (λ > 1)", "Rejeté (λ ≤ 1)")
    )
    # Bâton brisé pour superposition
    bs <- broken_stick(nrow(df_eig))
    df_eig$BatonBrise <- round(bs, 2)

    p <- ggplot(df_eig, aes(
        x    = Dim,
        y    = Variance_pct,
        fill = Kaiser,
        text = paste0("<b>", Dim, "</b><br>",
                      "λ = ", Lambda, "<br>",
                      "Variance : ", Variance_pct, "%<br>",
                      "Cumulée : ", Variance_cum, "%<br>",
                      "Bâton brisé : ", BatonBrise, "%<br>",
                      "Kaiser : ", Kaiser)
      )) +
      geom_col(alpha=.88, width=.72) +
      # Ligne bâton brisé
      geom_line(aes(y=BatonBrise, group=1, text=NULL),
                color="#9B59B6", linewidth=1.1, linetype="dotted") +
      geom_point(aes(y=BatonBrise, text=NULL), color="#9B59B6", size=2.5) +
      # Ligne cumulée
      geom_line(aes(y=Variance_cum, group=1, text=NULL),
                color=C$rouge, linewidth=1.3) +
      geom_point(aes(y=Variance_cum, text=NULL), color=C$rouge, size=3) +
      # Seuils de variance
      geom_hline(yintercept=80, linetype="dashed", color="gray50", linewidth=.6) +
      geom_hline(yintercept=70, linetype="dotted", color="gray70", linewidth=.5) +
      annotate("text", x=nrow(df_eig)*0.9, y=82, label="80%", size=3, color="gray50") +
      annotate("text", x=nrow(df_eig)*0.9, y=72, label="70%", size=3, color="gray70") +
      scale_fill_manual(
        values = c("Retenu (λ > 1)"="#00838F", "Rejeté (λ ≤ 1)"="#BDC3C7"),
        name   = "Critère de Kaiser"
      ) +
      scale_y_continuous(
        name      = "Variance expliquée (%)",
        sec.axis  = sec_axis(~., name="Variance cumulée / Bâton brisé (%)")
      ) +
      labs(x="Dimension", title="Éboulis des valeurs propres — Critère de Kaiser",
           subtitle="Vert : λ > 1 (Kaiser) · Ligne rouge : cumul · Pointillés violets : bâton brisé") +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom",
            plot.subtitle=element_text(size=9, color="gray50"))
    ggplotly(p, tooltip="text") |>
      layout(legend=list(orientation="h", y=-0.18)) |> nutri_config()
  })

  # ── FactoShiny ────────────────────────────────────────────────────
  observeEvent(input$run_factoshiny, {
    req(rdim_res())
    if (requireNamespace("Factoshiny", quietly=TRUE)) {
      tryCatch(
        Factoshiny::Factoshiny(rdim_res()$res),
        error=function(e) showNotification(paste("❌ FactoShiny:", e$message), type="error")
      )
    } else {
      showModal(modalDialog(
        title = "📦 FactoShiny non installé",
        tags$p("Pour utiliser FactoShiny, installez le package :"),
        tags$code("install.packages('Factoshiny')"),
        tags$br(), tags$br(),
        tags$p("Puis relancez l'application."),
        footer = modalButton("Fermer")
      ))
    }
  })

  # ══════════════════════════════════════════════════════════════════
  # RDIM — SWITCH UI CONDITIONNEL (Plotly ↔ factoextra)
  # ══════════════════════════════════════════════════════════════════
  
  rdim_mode <- reactive(input$rdim_render_mode %||% "plotly")
  
  output$rdim_scree_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput("rdim_scree_fviz", height="390px")
    else plotlyOutput("rdim_scree", height="390px")
  })
  output$rdim_circle_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput("rdim_circle_fviz", height="450px")
    else plotlyOutput("rdim_circle", height="450px")
  })
  output$rdim_biplot_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput("rdim_biplot_fviz", height="450px")
    else plotlyOutput("rdim_biplot", height="450px")
  })
  output$rdim_contrib_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput("rdim_contrib_fviz", height="420px")
    else plotlyOutput("rdim_contrib", height="420px")
  })
  output$rdim_cos2_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput("rdim_cos2_fviz", height="410px")
    else plotlyOutput("rdim_cos2", height="410px")
  })
  output$rdim_heatmap_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput("rdim_heatmap_fviz", height="410px")
    else plotlyOutput("rdim_heatmap_contrib", height="410px")
  })
  
  # ══════════════════════════════════════════════════════════════════
  # RDIM — RENDERERS factoextra (mode publication)
  # ══════════════════════════════════════════════════════════════════
  
  # Palette cohérente pour factoextra
  fviz_gradient <- c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C")
  
  # ── 1. Scree plot ────────────────────────────────────────────────
  output$rdim_scree_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res
    p <- fviz_screeplot(r, addlabels = TRUE, barfill = "#3498DB", barcolor = "#2C3E50",
                        linecolor = "#E74C3C", ncp = min(10, length(get_eigenvalue(r)[[1]]))) +
      geom_hline(yintercept = 100 / ncol(get_eigenvalue(r)), linetype = "dashed",
                 color = "#00838F", linewidth = 0.7) +
      labs(title = "Éboulis des valeurs propres",
           subtitle = "Barres = % variance · Ligne rouge = cumul · Pointillé vert = seuil Kaiser moyen") +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(size = 10, color = "gray50"))
    rdim_gg$scree <- p
    p
  })
  
  # ── 2. Cercle des corrélations ───────────────────────────────────
  output$rdim_circle_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    
    ax_x <- as.integer(gsub("Dim\\.", "", input$rdim_axes_x %||% "Dim.1"))
    ax_y <- as.integer(gsub("Dim\\.", "", input$rdim_axes_y %||% "Dim.2"))
    color_by <- input$rdim_circle_color %||% "cos2"
    
    p <- if (method == "famd") {
      # FAMD : 2 sous-graphiques (quanti + quali) côte à côte
      p_qn <- fviz_famd_var(r, "quanti.var", axes = c(ax_x, ax_y),
                             repel = TRUE, col.var = color_by,
                             gradient.cols = fviz_gradient,
                             title = "Variables quantitatives (corrélations)")
      p_ql <- fviz_famd_var(r, "quali.var", axes = c(ax_x, ax_y),
                             repel = TRUE, col.var = color_by,
                             gradient.cols = c("#BDC3C7","#8E44AD","#E74C3C"),
                             title = "Variables qualitatives (centroïdes)")
      # Combiner avec patchwork si disponible
      if (requireNamespace("patchwork", quietly = TRUE)) {
        p_qn + p_ql + patchwork::plot_annotation(
          title = "FAMD — Plan factoriel",
          subtitle = paste0("Dim.", ax_x, " × Dim.", ax_y),
          theme = theme(plot.title = element_text(face = "bold", size = 14))
        )
      } else {
        p_qn  # Fallback sans patchwork
      }
    } else if (method == "pca") {
      fviz_pca_var(r, axes = c(ax_x, ax_y), repel = TRUE,
                   col.var = color_by, gradient.cols = fviz_gradient,
                   title = "ACP — Cercle des corrélations") +
        theme_minimal(base_size = 13)
    } else if (method == "mca") {
      fviz_mca_var(r, axes = c(ax_x, ax_y), repel = TRUE,
                   col.var = color_by, gradient.cols = fviz_gradient,
                   title = "ACM — Plan des modalités") +
        theme_minimal(base_size = 13)
    } else if (method == "ca") {
      fviz_ca_biplot(r, axes = c(ax_x, ax_y), repel = TRUE,
                     col.row = "#3498DB", col.col = "#E74C3C",
                     title = "AFC — Biplot lignes × colonnes") +
        theme_minimal(base_size = 13)
    }
    rdim_gg$circle <- p
    p
  })
  
  # ── 3. Carte des individus / Biplot ──────────────────────────────
  output$rdim_biplot_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    
    grp_var <- input$rdim_biplot_grp %||% input$rdim_grp %||% ""
    d <- df_f()
    use_repel <- nrow(d) <= 100  # repel seulement si ≤ 100 obs (sinon trop lent)
    
    p <- if (method == "pca") {
      if (nchar(grp_var) > 0 && grp_var %in% names(d)) {
        habillage_idx <- which(names(d) == grp_var)
        fviz_pca_ind(r, habillage = d[[grp_var]], addEllipses = TRUE,
                     ellipse.level = 0.95, repel = use_repel,
                     pointsize = 1.8, alpha.ind = 0.6,
                     title = paste0("ACP — Individus par ", grp_var)) +
          theme_minimal(base_size = 13)
      } else {
        fviz_pca_ind(r, col.ind = "cos2", repel = use_repel,
                     gradient.cols = fviz_gradient, pointsize = 1.8,
                     title = "ACP — Individus (Cos²)") +
          theme_minimal(base_size = 13)
      }
    } else if (method == "famd") {
      if (nchar(grp_var) > 0 && grp_var %in% names(d)) {
        fviz_famd_ind(r, habillage = d[[grp_var]], addEllipses = TRUE,
                      ellipse.level = 0.95, repel = use_repel,
                      pointsize = 1.8, alpha.ind = 0.6,
                      title = paste0("FAMD — Individus par ", grp_var)) +
          theme_minimal(base_size = 13)
      } else {
        fviz_famd_ind(r, col.ind = "cos2", repel = use_repel,
                      gradient.cols = fviz_gradient, pointsize = 1.8,
                      title = "FAMD — Individus (Cos²)") +
          theme_minimal(base_size = 13)
      }
    } else if (method == "mca") {
      if (nchar(grp_var) > 0 && grp_var %in% names(d)) {
        fviz_mca_ind(r, habillage = d[[grp_var]], addEllipses = TRUE,
                     repel = use_repel, pointsize = 1.8, alpha.ind = 0.6,
                     title = paste0("ACM — Individus par ", grp_var)) +
          theme_minimal(base_size = 13)
      } else {
        fviz_mca_ind(r, col.ind = "cos2", repel = use_repel,
                     gradient.cols = fviz_gradient, pointsize = 1.8,
                     title = "ACM — Individus (Cos²)") +
          theme_minimal(base_size = 13)
      }
    } else if (method == "ca") {
      fviz_ca_biplot(r, repel = TRUE,
                     col.row = "#3498DB", col.col = "#E74C3C",
                     title = "AFC — Biplot") +
        theme_minimal(base_size = 13)
    }
    rdim_gg$biplot <- p
    p
  })
  
  # ── 4. Contributions ─────────────────────────────────────────────
  output$rdim_contrib_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    dim_sel <- as.integer(input$rdim_contrib_dim %||% 1)
    
    choice <- if (method == "ca") "col" else "var"
    
    p <- fviz_contrib(r, choice = choice, axes = dim_sel,
                      fill = "#3498DB", color = "#2C3E50",
                      top = 25,  # limiter à 25 variables max pour lisibilité
                      title = paste0("Contributions — Dim.", dim_sel)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    
    rdim_gg$contrib <- p
    p
  })
  
  # ── 5. Cos² ──────────────────────────────────────────────────────
  output$rdim_cos2_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    
    choice <- if (method == "ca") "col" else "var"
    n_dims <- min(3, nrow(get_eigenvalue(r)))
    
    p <- fviz_cos2(r, choice = choice, axes = 1:n_dims,
                   fill = "#E67E22", color = "#2C3E50",
                   top = 25,
                   title = paste0("Cos² — Qualité de représentation (Dim.1:", n_dims, ")")) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    
    rdim_gg$cos2 <- p
    p
  })
  
  # ── 6. Heatmap (factoextra n'a pas d'équivalent → corrplot-style) ─
  output$rdim_heatmap_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    vi <- get_var_info(r, method)
    req(!is.null(vi))
    contrib <- vi$contrib
    if (".var_type" %in% names(contrib)) contrib <- contrib[, names(contrib) != ".var_type", drop = FALSE]
    n_dim <- min(ncol(contrib), 8)
    contrib <- contrib[, 1:n_dim, drop = FALSE]
    
    # Ordonner par contribution totale décroissante
    ordre <- order(rowSums(contrib), decreasing = TRUE)
    contrib <- contrib[ordre, , drop = FALSE]
    # Limiter à 25 variables
    if (nrow(contrib) > 25) contrib <- contrib[1:25, , drop = FALSE]
    
    mat <- as.matrix(contrib)
    df_heat <- expand.grid(
      Variable  = factor(rownames(mat), levels = rev(rownames(mat))),
      Dimension = factor(colnames(mat), levels = colnames(mat))
    )
    df_heat$Contrib <- as.vector(mat[rev(seq_len(nrow(mat))), ])
    
    ggplot(df_heat, aes(x = Dimension, y = Variable, fill = Contrib)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(Contrib, 1)), size = 3, color = "white", fontface = "bold") +
      scale_fill_gradientn(
        colors = c("#2C3E50", "#2980B9", "#27AE60", "#F39C12", "#E74C3C"),
        name = "Contribution (%)"
      ) +
      labs(x = "", y = "", title = "Heatmap — Contributions variables × dimensions",
           subtitle = "Top 25 variables par contribution totale") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_text(size = 10),
            plot.subtitle = element_text(size = 10, color = "gray50"),
            legend.position = "right")
  })
  
  # ── Download handlers pour chaque graphique ───────────────────────
  make_dl <- function(id, gg_name, fname) {
    output[[id]] <- downloadHandler(
      filename = function() paste0(fname, "_", Sys.Date(), ".png"),
      content  = function(f) {
        gg <- rdim_gg[[gg_name]]
        if (!is.null(gg)) {
          ggsave(f, gg, width=10, height=7, dpi=200, bg="white")
        } else {
          # fallback : graphique vide
          ggsave(f, ggplot() + labs(title="Lancez d'abord l'analyse"), width=8, height=6)
        }
      }
    )
  }
  make_dl("dl_rdim_scree",   "scree",   "eboulis_valeurs_propres")
  make_dl("dl_rdim_circle",  "circle",  "cercle_correlations")
  make_dl("dl_rdim_biplot",  "biplot",  "carte_individus")
  make_dl("dl_rdim_contrib", "contrib", "contributions_variables")
  make_dl("dl_rdim_cos2",    "cos2",    "cos2_qualite")
  make_dl("dl_rdim_heat",    "heatmap", "heatmap_contributions")

  output$rdim_circle <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      # Axes sélectionnés
      ax_x <- as.integer(gsub("Dim\\.","", input$rdim_axes_x %||% "Dim.1"))
      ax_y <- as.integer(gsub("Dim\\.","", input$rdim_axes_y %||% "Dim.2"))
      ax_x <- max(1, min(ax_x, 10)); ax_y <- max(1, min(ax_y, 10))

      # ── Pour l'AFC : afficher biplot lignes + colonnes ────────────
      if (method == "ca") {
        df_ca <- get_ca_both(r)
        req(!is.null(df_ca))
        eig <- get_eigenvalue(r)
        pct1 <- round(eig[1,2],1)
        pct2 <- if (nrow(eig) >= 2) round(eig[2,2],1) else 0
        p <- ggplot(df_ca, aes(x=Dim.1, y=Dim.2, color=Type, shape=Type,
              text=paste0("<b>",Label,"</b><br>Type: ",Type,"<br>",
                          "Dim1=",round(Dim.1,3)," | Dim2=",round(Dim.2,3)))) +
          geom_hline(yintercept=0, color="gray70", linewidth=.3) +
          geom_vline(xintercept=0, color="gray70", linewidth=.3) +
          geom_point(size=3.5, alpha=.85) +
          geom_text(aes(label=Label), size=3.2, vjust=-0.6, fontface="bold") +
          scale_color_manual(values=c(Ligne=C$bleu, Colonne=C$rouge)) +
          scale_shape_manual(values=c(Ligne=16, Colonne=17)) +
          labs(x=paste0("Dim.1 (",pct1,"%)"), y=paste0("Dim.2 (",pct2,"%)"),
               title="AFC — Biplot lignes et colonnes",
               subtitle="Bleu = profils lignes · Rouge = profils colonnes") +
          theme_minimal(base_size=12) +
          theme(plot.subtitle=element_text(size=9, color="gray50"))
        rdim_gg$circle <- p
        return(ggplotly(p, tooltip="text"))
      }

      # ── PCA / MCA / FAMD ──────────────────────────────────────────
      vi <- get_var_info(r, method)
      req(!is.null(vi))
      eig <- get_eigenvalue(r)
      
      # ── FAMD : traitement spécifique quanti + quali ──────────────
      if (method == "famd") {
        
        # --- Variables quantitatives : cercle de corrélations (coord [-1,1]) ---
        qn_coord   <- vi$quanti_coord
        qn_cos2    <- vi$quanti_cos2
        qn_contrib <- vi$quanti_contrib
        
        # --- Variables qualitatives : centroïdes (coord non bornées) ---
        ql_coord   <- vi$quali_coord
        ql_cos2    <- vi$quali_cos2
        ql_contrib <- vi$quali_contrib
        
        validate(need(
          (!is.null(qn_coord) && ncol(qn_coord) >= 2) ||
          (!is.null(ql_coord) && ncol(ql_coord) >= 2),
          "Au moins 2 dimensions nécessaires. Augmentez le nombre de dimensions."))
        
        ax_x_nm <- if (!is.null(qn_coord)) names(qn_coord)[min(ax_x, ncol(qn_coord))]
                   else names(ql_coord)[min(ax_x, ncol(ql_coord))]
        ax_y_nm <- if (!is.null(qn_coord)) names(qn_coord)[min(ax_y, ncol(qn_coord))]
                   else names(ql_coord)[min(ax_y, ncol(ql_coord))]
        pct_x <- round(eig[min(ax_x, nrow(eig)), 2], 1)
        pct_y <- round(eig[min(ax_y, nrow(eig)), 2], 1)
        
        color_choice <- input$rdim_circle_color %||% "cos2"
        
        # Construire df pour variables quantitatives (flèches + cercle)
        df_quanti <- NULL
        if (!is.null(qn_coord) && nrow(qn_coord) > 0) {
          qcx <- qn_coord[[ax_x_nm]]; qcy <- qn_coord[[ax_y_nm]]
          qcs <- (qn_cos2[[ax_x_nm]] + qn_cos2[[ax_y_nm]]) / 2
          qct <- (qn_contrib[[ax_x_nm]] + qn_contrib[[ax_y_nm]]) / 2
          qcol <- switch(color_choice, cos2=qcs, contrib=qct, rep(0.5, length(qcx)))
          df_quanti <- tibble(
            Variable = rownames(qn_coord), Dim_x = qcx, Dim_y = qcy,
            Qualite = round(qcol, 3), Cos2 = round(qcs, 3), Contrib = round(qct, 3),
            Type = "Numérique",
            text = paste0("<b>", rownames(qn_coord), "</b> [num]<br>",
                          ax_x_nm, " = ", round(qcx,3), "<br>",
                          ax_y_nm, " = ", round(qcy,3), "<br>",
                          "Cos² moy = ", round(qcs,3), "<br>",
                          "Contrib moy = ", round(qct,2), "%")
          )
        }
        
        # Construire df pour variables qualitatives (points, pas de flèches)
        df_quali <- NULL
        if (!is.null(ql_coord) && nrow(ql_coord) > 0) {
          lcx <- ql_coord[[ax_x_nm]]; lcy <- ql_coord[[ax_y_nm]]
          lcs <- (ql_cos2[[ax_x_nm]] + ql_cos2[[ax_y_nm]]) / 2
          lct <- (ql_contrib[[ax_x_nm]] + ql_contrib[[ax_y_nm]]) / 2
          lcol <- switch(color_choice, cos2=lcs, contrib=lct, rep(0.5, length(lcx)))
          df_quali <- tibble(
            Variable = rownames(ql_coord), Dim_x = lcx, Dim_y = lcy,
            Qualite = round(lcol, 3), Cos2 = round(lcs, 3), Contrib = round(lct, 3),
            Type = "Catégorielle",
            text = paste0("<b>", rownames(ql_coord), "</b> [cat]<br>",
                          ax_x_nm, " = ", round(lcx,3), "<br>",
                          ax_y_nm, " = ", round(lcy,3), "<br>",
                          "Cos² moy = ", round(lcs,3), "<br>",
                          "Contrib moy = ", round(lct,2), "%")
          )
        }
        
        # Cercle unitaire (uniquement si variables quantitatives présentes)
        theta     <- seq(0, 2*pi, length.out=200)
        circle_df <- tibble(x=cos(theta), y=sin(theta))
        
        # Déterminer les limites du graphique
        all_x <- c(if(!is.null(df_quanti)) df_quanti$Dim_x, if(!is.null(df_quali)) df_quali$Dim_x)
        all_y <- c(if(!is.null(df_quanti)) df_quanti$Dim_y, if(!is.null(df_quali)) df_quali$Dim_y)
        lim_max <- max(1.15, max(abs(c(all_x, all_y)), na.rm=TRUE) * 1.15)
        
        p <- ggplot()
        
        # Cercle unitaire (référence pour variables numériques)
        if (!is.null(df_quanti)) {
          p <- p +
            geom_path(data=circle_df, aes(x=x, y=y),
                      color="gray75", linewidth=.4, linetype="dashed") +
            geom_segment(data=df_quanti,
                         aes(x=0, y=0, xend=Dim_x, yend=Dim_y, color=Qualite, text=text),
                         linewidth=.9,
                         arrow=arrow(length=unit(.22,"cm"), type="closed")) +
            geom_text(data=df_quanti, aes(x=Dim_x, y=Dim_y, label=Variable, color=Qualite),
                      size=3, vjust=-0.6, fontface="bold")
        }
        
        # Variables catégorielles : points + étiquettes (pas de flèches, pas de cercle)
        if (!is.null(df_quali)) {
          p <- p +
            geom_point(data=df_quali,
                       aes(x=Dim_x, y=Dim_y, text=text),
                       shape=17, size=3.5, color="#8E44AD", alpha=.85) +
            geom_text(data=df_quali,
                      aes(x=Dim_x, y=Dim_y, label=Variable),
                      size=2.8, vjust=-0.7, color="#8E44AD", fontface="italic")
        }
        
        color_label <- switch(color_choice,
                              cos2="Cos² moyen", contrib="Contrib. (%)", "Qualité")
        
        p <- p +
          geom_hline(yintercept=0, color="gray70", linewidth=.3) +
          geom_vline(xintercept=0, color="gray70", linewidth=.3) +
          scale_color_gradientn(
            colors = c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C"),
            name   = color_label
          ) +
          coord_fixed() +
          xlim(-lim_max, lim_max) + ylim(-lim_max, lim_max) +
          labs(x=paste0(ax_x_nm," (",pct_x,"%)"),
               y=paste0(ax_y_nm," (",pct_y,"%)"),
               title="FAMD — Variables sur le plan factoriel",
               subtitle=paste0("Flèches = numériques (cercle unitaire) · ",
                               "Triangles violets = catégorielles (centroïdes)")) +
          theme_minimal(base_size=12) +
          theme(legend.position="right",
                plot.subtitle=element_text(size=9, color="gray50"))
        
        rdim_gg$circle <- p
        return(ggplotly(p, tooltip="text") |> nutri_config())
      }
      
      # ── PCA / MCA : cercle de corrélations classique ──────────────
      coord   <- vi$coord
      cos2_df <- vi$cos2
      contrib_df <- vi$contrib
      validate(need(ncol(coord) >= 2,
        "Au moins 2 dimensions nécessaires pour le cercle des corrélations. Augmentez le nombre de dimensions à garder."))

      ax_x_nm <- names(coord)[min(ax_x, ncol(coord))]
      ax_y_nm <- names(coord)[min(ax_y, ncol(coord))]
      cx <- coord[[ax_x_nm]]; cy <- coord[[ax_y_nm]]
      cs <- (cos2_df[[ax_x_nm]] + cos2_df[[ax_y_nm]]) / 2
      ct <- (contrib_df[[ax_x_nm]] + contrib_df[[ax_y_nm]]) / 2

      color_val <- switch(input$rdim_circle_color %||% "cos2",
        cos2   = cs,
        contrib= ct,
        none   = rep(0.5, length(cx))
      )

      df_v <- tibble(
        Variable = rownames(coord),
        Dim_x = cx, Dim_y = cy,
        Qualite = round(color_val, 3),
        Cos2    = round(cs, 3),
        Contrib = round(ct, 3),
        text = paste0("<b>", rownames(coord), "</b><br>",
                      ax_x_nm, " = ", round(cx,3), "<br>",
                      ax_y_nm, " = ", round(cy,3), "<br>",
                      "Cos² moy = ", round(cs,3), "<br>",
                      "Contrib moy = ", round(ct,2),"%")
      )

      theta      <- seq(0, 2*pi, length.out=200)
      circle_df  <- tibble(x=cos(theta), y=sin(theta))
      pct_x <- round(eig[min(ax_x,nrow(eig)), 2], 1)
      pct_y <- round(eig[min(ax_y,nrow(eig)), 2], 1)

      # Limites adaptatives : cercle unitaire pour PCA, flexible pour MCA
      lim_val <- if (method == "pca") 1.15
                 else max(1.15, max(abs(c(cx, cy)), na.rm=TRUE) * 1.15)

      p <- ggplot(df_v, aes(x=Dim_x, y=Dim_y, text=text)) +
        {if (method == "pca")
          geom_path(data=circle_df, aes(x=x,y=y),
                    color="gray80", linewidth=.5, inherit.aes=FALSE)
        } +
        geom_segment(aes(xend=0, yend=0, color=Qualite),
                     linewidth=.9,
                     arrow=arrow(length=unit(.22,"cm"), type="closed")) +
        geom_point(aes(color=Qualite), size=2.5) +
        geom_text(aes(label=Variable, color=Qualite),
                  size=3, vjust=-0.6, fontface="bold") +
        geom_hline(yintercept=0, color="gray70", linewidth=.3) +
        geom_vline(xintercept=0, color="gray70", linewidth=.3) +
        scale_color_gradientn(
          colors = c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C"),
          name   = switch(input$rdim_circle_color %||% "cos2",
                          cos2="Cos² moyen", contrib="Contrib. (%)", "Qualité")
        ) +
        coord_fixed() + xlim(-lim_val, lim_val) + ylim(-lim_val, lim_val) +
        labs(x=paste0(ax_x_nm," (",pct_x,"%)"),
             y=paste0(ax_y_nm," (",pct_y,"%)"),
             title=paste0(toupper(method), " — Cercle des corrélations"),
             subtitle="Couleur = qualité de représentation (Cos²)") +
        theme_minimal(base_size=12) +
        theme(legend.position="right",
              plot.subtitle=element_text(size=9,color="gray50"))
      rdim_gg$circle <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Cercle non disponible:",e$message),x=.5)))
  })

  output$rdim_biplot <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      ii  <- get_ind_info(r, method)
      req(!is.null(ii))
      n_ax <- min(2, ncol(ii$coord))
      validate(need(n_ax >= 2, "Au moins 2 dimensions nécessaires pour la carte des individus. Augmentez le nombre de dimensions à garder."))
      ind_coord <- ii$coord[, 1:n_ax, drop=FALSE]
      names(ind_coord) <- paste0("Dim", seq_len(n_ax))
      ind_coord$Index  <- 1:nrow(ind_coord)
      # Cos² individus
      cos2_ind <- tryCatch(rowSums(ii$cos2[, 1:n_ax, drop=FALSE]),
                           error=function(e) rep(0.5, nrow(ind_coord)))
      ind_coord$Cos2   <- round(cos2_ind, 3)

      eig   <- get_eigenvalue(r)
      pct1  <- round(eig[1,2], 1)
      pct2  <- if (nrow(eig) >= 2) round(eig[2,2], 1) else 0

      # Groupage (variable catégorielle)
      grp_var <- input$rdim_biplot_grp %||% input$rdim_grp %||% ""
      d <- df_f()

      color_mode <- input$rdim_ind_color %||% "grp"

      if (color_mode == "grp" && !is.null(grp_var) && nchar(grp_var) > 0
          && grp_var %in% names(d) && nrow(d) == nrow(ind_coord)) {
        ind_coord$Groupe <- as.character(d[[grp_var]])
        n_grp <- length(unique(ind_coord$Groupe))
        p <- ggplot(ind_coord, aes(x=Dim1, y=Dim2,
            color=Groupe,
            text=paste0("Obs. ", Index, "<br>",
                        grp_var," : ", Groupe, "<br>",
                        "Dim1=",round(Dim1,3)," | Dim2=",round(Dim2,3),"<br>",
                        "Cos²=",Cos2))) +
          geom_point(alpha=.65, size=2) +
          scale_color_manual(values=gpal(n_grp,"cat"), name=grp_var) +
          stat_ellipse(aes(fill=Groupe), geom="polygon",
                       alpha=.08, type="t", level=0.95, show.legend=FALSE) +
          scale_fill_manual(values=gpal(n_grp,"cat"))
      } else if (color_mode == "cos2") {
        p <- ggplot(ind_coord, aes(x=Dim1, y=Dim2, color=Cos2,
            text=paste0("Obs. ", Index, "<br>",
                        "Dim1=",round(Dim1,3)," | Dim2=",round(Dim2,3),"<br>",
                        "Cos²=",Cos2))) +
          geom_point(alpha=.7, size=2) +
          scale_color_gradientn(
            colors=c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C"),
            name="Cos²"
          )
      } else {
        # Densité 2D (coloration par densité)
        p <- ggplot(ind_coord, aes(x=Dim1, y=Dim2,
            text=paste0("Obs. ",Index))) +
          geom_point(alpha=.5, size=1.8, color=gprimary()) +
          geom_density_2d_filled(alpha=.35, show.legend=FALSE) +
          scale_fill_viridis_d(option="turbo")
      }

      p <- p +
        geom_hline(yintercept=0, color="gray70", linewidth=.3) +
        geom_vline(xintercept=0, color="gray70", linewidth=.3) +
        labs(x=paste0("Dim 1 (",pct1,"%)"),
             y=paste0("Dim 2 (",pct2,"%)"),
             title="Carte des individus",
             subtitle=if(color_mode=="grp" && nchar(grp_var)>0)
               paste0("Ellipses de confiance 95% par groupe (",grp_var,")")
             else if(color_mode=="cos2") "Couleur = qualité de représentation (Cos²)"
             else "Couleur = densité 2D") +
        theme_minimal(base_size=12) +
        theme(legend.position="right",
              plot.subtitle=element_text(size=9, color="gray50"))

      rdim_gg$biplot <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  output$rdim_contrib <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      dim_sel <- as.integer(input$rdim_contrib_dim %||% 1)
      vi      <- get_var_info(r, method)
      req(!is.null(vi))
      contrib  <- vi$contrib
      # Retirer la colonne .var_type si présente (FAMD)
      if (".var_type" %in% names(contrib)) contrib <- contrib[, names(contrib) != ".var_type", drop=FALSE]
      n_dim    <- min(ncol(contrib), 10)
      contrib  <- contrib[,1:n_dim, drop=FALSE]
      contrib$Variable <- rownames(contrib)

      # Seuil uniforme 1/p × 100
      seuil <- 100 / nrow(contrib)
      dim_nm <- names(contrib)[dim_sel]
      contrib$contrib_sel <- contrib[[dim_nm]]
      contrib$Depasse <- contrib$contrib_sel >= seuil

      p <- ggplot(contrib,
            aes(x=fct_reorder(Variable, contrib_sel),
                y=contrib_sel,
                fill=contrib_sel,
                text=paste0("<b>", Variable, "</b><br>",
                            "Contribution : ", round(contrib_sel,2), "%<br>",
                            if(isTRUE(input$rdim_contrib_thresh))
                              paste0("Seuil (1/p)×100 : ", round(seuil,2),
                                     "%<br>", ifelse(contrib_sel>=seuil,"✅ Au-dessus","❌ En-dessous"))
                            else ""))) +
        geom_col(alpha=.9, width=.75) +
        scale_fill_gradientn(
          colors=c("#BDC3C7","#5DADE2","#1A5276","#E67E22","#C0392B"),
          name="Contribution (%)"
        ) +
        {if(isTRUE(input$rdim_contrib_thresh))
          geom_hline(yintercept=seuil, linetype="dashed",
                     color="#E74C3C", linewidth=.8)
        } +
        {if(isTRUE(input$rdim_contrib_thresh))
          annotate("text", x=nrow(contrib)*0.85, y=seuil+0.3,
                   label=paste0("Seuil=",round(seuil,1),"%"),
                   size=3, color="#E74C3C", fontface="bold")
        } +
        labs(x="", y="Contribution (%)",
             title=paste0("Contributions — ", dim_nm),
             subtitle=paste0("Seuil d'équi-contribution : ",round(seuil,1),"% (1/",nrow(contrib)," vars)")) +
        theme_minimal(base_size=12) +
        theme(legend.position="right",
              plot.subtitle=element_text(size=9,color="gray50"))
      rdim_gg$contrib <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  output$rdim_cos2 <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      vi    <- get_var_info(r, method)
      req(!is.null(vi))
      cos2  <- vi$cos2
      # Retirer la colonne .var_type si présente (FAMD)
      if (".var_type" %in% names(cos2)) cos2 <- cos2[, names(cos2) != ".var_type", drop=FALSE]
      n_dim <- min(ncol(cos2), 6)
      cos2  <- cos2[,1:n_dim, drop=FALSE]
      cos2$Variable <- rownames(cos2)
      df_long <- pivot_longer(cos2, -Variable, names_to="Dimension", values_to="Cos2")

      # Palette séquentielle bleu clair → violet foncé par dimension
      n_d    <- n_dim
      dim_pal <- colorRampPalette(c("#AED6F1","#2980B9","#1A5276","#8E44AD","#6C3483"))(n_d)

      n_vars <- length(unique(df_long$Variable))
      df_long$Variable <- truncate_label(df_long$Variable, 18)
      use_horiz <- n_vars > 12
      p <- if (use_horiz) {
        ggplot(df_long,
              aes(y=fct_reorder(Variable, Cos2, .fun=max),
                  x=Cos2, fill=Dimension,
                  text=paste0("<b>",Variable,"</b><br>",
                              Dimension," : Cos²=",round(Cos2,3),"<br>",
                              "Qualité : ",
                              case_when(Cos2>=0.7~"Très bonne",
                                        Cos2>=0.5~"Bonne",
                                        Cos2>=0.3~"Acceptable",
                                        TRUE~"Faible")))) +
          geom_col(position="dodge", alpha=.9, width=.75) +
          scale_fill_manual(values=setNames(dim_pal, paste0("Dim.",1:n_d)),
                            name="Dimension") +
          geom_vline(xintercept=0.5, linetype="dashed", color="#E74C3C", linewidth=.6) +
          labs(y="", x="Cos²",
               title="Cos² — Qualité de représentation",
               subtitle="Cos² > 0.5 = bonne représentation") +
          theme_nutri(11) +
          theme(legend.position="right")
      } else {
        ggplot(df_long,
              aes(x=fct_reorder(Variable, Cos2, .fun=max),
                  y=Cos2, fill=Dimension,
                  text=paste0("<b>",Variable,"</b><br>",
                              Dimension," : Cos²=",round(Cos2,3),"<br>",
                              "Qualité : ",
                              case_when(Cos2>=0.7~"Très bonne",
                                        Cos2>=0.5~"Bonne",
                                        Cos2>=0.3~"Acceptable",
                                        TRUE~"Faible")))) +
          geom_col(position="dodge", alpha=.9, width=.75) +
          scale_fill_manual(values=setNames(dim_pal, paste0("Dim.",1:n_d)),
                            name="Dimension") +
          geom_hline(yintercept=0.5, linetype="dashed", color="#E74C3C", linewidth=.6) +
          labs(x="", y="Cos²",
               title="Cos² — Qualité de représentation",
               subtitle="Cos² > 0.5 = bonne représentation") +
          theme_nutri(11) + smart_axis_x(n_vars) +
          theme(legend.position="right")
      }
      rdim_gg$cos2 <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  # ── Heatmap contributions (toutes dimensions × toutes variables) ──
  output$rdim_heatmap_contrib <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      vi      <- get_var_info(r, method)
      req(!is.null(vi))
      contrib  <- vi$contrib
      # Retirer la colonne .var_type si présente (FAMD)
      if (".var_type" %in% names(contrib)) contrib <- contrib[, names(contrib) != ".var_type", drop=FALSE]
      n_dim    <- min(ncol(contrib), 8)
      contrib  <- contrib[,1:n_dim, drop=FALSE]
      contrib$Variable <- rownames(contrib)
      df_long  <- pivot_longer(contrib, -Variable,
                               names_to="Dimension", values_to="Contrib")

      # Ordonner les variables par contribution totale
      ordre <- contrib |>
        mutate(total=rowSums(across(-Variable))) |>
        arrange(desc(total)) |>
        pull(Variable)
      df_long$Variable <- factor(df_long$Variable, levels=ordre)

      p <- ggplot(df_long, aes(x=Dimension, y=Variable, fill=Contrib,
            text=paste0("<b>",Variable,"</b><br>",
                        Dimension," : ",round(Contrib,2),"%"))) +
        geom_tile(color="white", linewidth=.5) +
        geom_text(aes(label=round(Contrib,1)), size=2.8, color="white", fontface="bold") +
        scale_fill_gradientn(
          colors=c("#F0F3F4","#AED6F1","#2980B9","#1A5276","#6C3483","#4A235A"),
          name="Contribution (%)",
          guide=guide_colorbar(barwidth=.8, barheight=8)
        ) +
        scale_y_discrete(labels = function(x) truncate_label(x, 18)) +
        labs(x="Dimension", y="Variable",
             title="Heatmap des contributions variables × dimensions",
             subtitle="Plus la couleur est foncée, plus la variable contribue à l'axe") +
        theme_nutri() +
        theme(axis.text.x=element_text(face="bold"))
      rdim_gg$heatmap <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Heatmap non disponible:",e$message),x=.5)))
  })

  # ── Aussi enregistrer le scree dans rdim_gg ───────────────────────
  # (le renderPlotly du scree est défini plus haut — on ajoute l'enregistrement via observe)
  observeEvent(rdim_res(), {
    req(rdim_res()); r <- rdim_res()$res
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))
    df_eig <- tibble(
      Dim=paste0("Dim.",1:nrow(eig)),
      Lambda=round(eig[,1],4), Variance_pct=round(eig[,2],2),
      Variance_cum=round(eig[,3],2),
      Kaiser=ifelse(eig[,1]>1,"Retenu (λ > 1)","Rejeté (λ ≤ 1)")
    )
    bs <- broken_stick(nrow(df_eig)); df_eig$BatonBrise <- round(bs,2)
    rdim_gg$scree <- ggplot(df_eig,aes(x=Dim,y=Variance_pct,fill=Kaiser)) +
      geom_col(alpha=.88,width=.72) +
      geom_line(aes(y=BatonBrise,group=1),color="#9B59B6",linewidth=1.1,linetype="dotted") +
      geom_line(aes(y=Variance_cum,group=1),color=C$rouge,linewidth=1.3) +
      geom_hline(yintercept=80,linetype="dashed",color="gray50") +
      scale_fill_manual(values=c("Retenu (λ > 1)"="#00838F","Rejeté (λ ≤ 1)"="#BDC3C7")) +
      labs(x="Dimension",y="Variance (%)",title="Éboulis — Critère de Kaiser") +
      theme_minimal(base_size=12)
  })

  # ── Onglet Sélection automatique des axes ────────────────────────
  output$rdim_selec_auto <- renderUI({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))

    n_vars    <- nrow(eig)
    lambdas   <- eig[,1]
    var_pct   <- eig[,2]
    var_cum   <- eig[,3]

    # ─ Kaiser : λ > 1
    n_kaiser  <- max(1, sum(lambdas > 1))
    cum_kaiser<- round(var_cum[n_kaiser], 1)

    # ─ Bâton brisé
    bs        <- broken_stick(n_vars)
    n_bs      <- max(1, sum(var_pct > bs))
    cum_bs    <- round(var_cum[n_bs], 1)

    # ─ Variance 70% / 80%
    n_v70 <- max(1, which(var_cum >= 70)[1])
    n_v80 <- max(1, which(var_cum >= 80)[1])

    # ─ Coude (elbow) : 2ème dérivée du scree
    n_elbow <- if (n_vars >= 3) {
      d2 <- diff(diff(var_pct))
      max(1, which.max(abs(d2)) + 1)
    } else 1

    # ─ Recommandation synthèse (vote majoritaire)
    votes <- c(n_kaiser, n_bs, n_elbow)
    n_recommande <- as.integer(names(sort(table(votes), decreasing=TRUE))[1])

    # ─ Tableau synthèse
    tbl_synth <- tibble(
      Critère   = c("📐 Kaiser (λ > 1)",
                    "🥢 Bâton brisé",
                    "📏 Coude du scree",
                    "📊 Variance ≥ 70%",
                    "📊 Variance ≥ 80%"),
      `Axes retenus` = c(n_kaiser, n_bs, n_elbow, n_v70, n_v80),
      `Variance cumulée (%)` = c(cum_kaiser, cum_bs,
                                 round(var_cum[n_elbow],1),
                                 round(var_cum[n_v70],1),
                                 round(var_cum[n_v80],1)),
      Justification = c(
        paste0(sum(lambdas>1)," valeurs propres > 1"),
        paste0(n_bs," axes au-dessus du modèle nul"),
        paste0("Inflexion maximale à la dim. ",n_elbow),
        paste0("Seuil 70% atteint à la dim. ",n_v70),
        paste0("Seuil 80% atteint à la dim. ",n_v80)
      )
    )

    tagList(
      # Bannière recommandation
      tags$div(style=paste0(
        "background:linear-gradient(135deg,#0B3954 0%,#00838F 100%);",
        "color:white;padding:16px 20px;border-radius:10px;margin-bottom:16px;"
      ),
        fluidRow(
          column(8,
            tags$h4(style="margin:0;font-weight:700;",
                    "🎯 Recommandation FactoInvestigate"),
            tags$p(style="margin:4px 0 0;font-size:14px;opacity:.9;",
                   paste0("Retenir ", tags$b(n_recommande), " axe",
                          if(n_recommande>1)"s", " factoriel",
                          if(n_recommande>1)"s")  |> as.character()),
            tags$p(style="margin:2px 0 0;font-size:12px;opacity:.8;",
                   paste0("Variance expliquée : ",
                          round(var_cum[min(n_recommande, n_vars)],1),
                          "% — Vote majoritaire de ",length(votes)," critères"))
          ),
          column(4, style="text-align:center;",
            tags$div(style=paste0(
              "background:rgba(255,255,255,.2);border-radius:50%;",
              "width:80px;height:80px;display:flex;align-items:center;",
              "justify-content:center;margin:auto;font-size:36px;font-weight:900;"
            ), n_recommande)
          )
        )
      ),

      # Tableau des critères
      fluidRow(
        column(12,
          tags$div(class="section-header", "Comparaison des critères de sélection"),
          renderDT(
            tbl_synth |>
              datatable(rownames=FALSE,
                        options=list(dom="t", pageLength=10, scrollX=TRUE),
                        class="compact stripe hover") |>
              formatStyle("Axes retenus",
                background=styleEqual(
                  unique(tbl_synth$`Axes retenus`),
                  ifelse(unique(tbl_synth$`Axes retenus`) == n_recommande,
                         "#d5f5e3","white")
                )) |>
              formatStyle("Variance cumulée (%)",
                background=styleInterval(c(60,70,80),
                  c("#FDEDEC","#FDEBD0","#D5F5E3","#A9DFBF")))
          )
        )
      ),
      br(),

      # Tableau complet valeurs propres avec Kaiser highlight
      fluidRow(
        column(12,
          tags$div(class="section-header","Tableau des valeurs propres (Critère de Kaiser)"),
          renderDT({
            tibble(
              Dimension      = paste0("Dim.", 1:n_vars),
              `λ (val. propre)` = round(lambdas, 4),
              `Variance (%)`    = round(var_pct, 2),
              `Cumul (%)`       = round(var_cum, 2),
              `Bâton brisé (%)` = round(bs,       2),
              Kaiser            = ifelse(lambdas > 1, "✅ Retenu","❌ Rejeté"),
              `Brisée`          = ifelse(var_pct > bs,"✅ Retenu","❌ Rejeté")
            ) |>
              datatable(rownames=FALSE,
                        options=list(dom="t", pageLength=15),
                        class="compact stripe hover") |>
              formatStyle("Kaiser",
                color=styleEqual(c("✅ Retenu","❌ Rejeté"),
                                 c("#00838F","#C0392B")),
                fontWeight="bold") |>
              formatStyle(2,   # colonne λ (val. propre)
                background=styleInterval(1, c("#FDEDEC","#D5F5E3")))
          })
        )
      ),
      br(),

      # ── Description textuelle des axes (style FactoInvestigate) ───
      {
        vi <- get_var_info(rdim_res()$res, method)
        # Retirer .var_type si présente (FAMD)
        if (!is.null(vi)) {
          if (".var_type" %in% names(vi$contrib)) vi$contrib <- vi$contrib[, names(vi$contrib) != ".var_type", drop=FALSE]
          if (".var_type" %in% names(vi$cos2))    vi$cos2 <- vi$cos2[, names(vi$cos2) != ".var_type", drop=FALSE]
        }
        if (!is.null(vi) && ncol(vi$contrib) >= 1) {
          n_desc <- min(n_recommande, ncol(vi$contrib))
          seuil_contrib <- 100 / nrow(vi$contrib)

          cartes <- lapply(seq_len(n_desc), function(ax) {
            dim_nm  <- names(vi$contrib)[ax]
            ct_vec  <- vi$contrib[[dim_nm]]
            cs_vec  <- vi$cos2[[dim_nm]]
            nms     <- rownames(vi$contrib)

            # Variables au-dessus du seuil, triées par contribution décroissante
            top_idx <- order(ct_vec, decreasing=TRUE)
            top_idx <- top_idx[ct_vec[top_idx] >= seuil_contrib]
            if (length(top_idx) == 0) top_idx <- order(ct_vec, decreasing=TRUE)[1:3]
            top_vars <- nms[top_idx]
            top_ct   <- round(ct_vec[top_idx], 1)
            top_cs   <- round(cs_vec[top_idx], 3)

            # Qualité globale de l'axe
            qualite_axe <- case_when(
              var_pct[ax] >= 25 ~ "très élevée",
              var_pct[ax] >= 15 ~ "élevée",
              var_pct[ax] >= 8  ~ "modérée",
              TRUE              ~ "faible"
            )
            kaiser_txt <- if (lambdas[ax] > 1) "retenu (λ > 1)" else "rejeté (λ ≤ 1)"

            tags$div(
              style=paste0("border:1px solid #e0e8f0;border-radius:8px;padding:14px;",
                           "margin-bottom:10px;background:white;"),
              tags$div(style="display:flex;align-items:center;gap:10px;margin-bottom:8px;",
                tags$div(style=paste0(
                  "background:",if(lambdas[ax]>1)"#00838F" else "#BDC3C7",
                  ";color:white;border-radius:50%;width:32px;height:32px;",
                  "display:flex;align-items:center;justify-content:center;",
                  "font-weight:700;font-size:14px;flex-shrink:0;"), ax),
                tags$div(
                  tags$b(style="font-size:14px;color:#0B3954;",
                         paste0(dim_nm, " — ", round(var_pct[ax],1),
                                "% de variance (Kaiser : ", kaiser_txt, ")")),
                  tags$br(),
                  tags$span(style="font-size:11px;color:#888;",
                            paste0("Variance cumulée jusqu'à cet axe : ",
                                   round(var_cum[ax],1), "% | ",
                                   "Puissance de discrimination : ", qualite_axe))
                )
              ),
              tags$p(style="font-size:12px;color:#444;margin:0 0 6px;",
                tags$b("Variables structurantes (contribution ≥ seuil équi-contribution ",
                       round(seuil_contrib,1),"%) :"),
              ),
              tags$div(style="display:flex;flex-wrap:wrap;gap:6px;",
                lapply(seq_along(top_vars), function(j) {
                  cs_j <- top_cs[j]
                  badge_col <- if(cs_j >= 0.7)"#00838F" else if(cs_j>=0.5)"#E67E22" else "#BDC3C7"
                  tags$span(style=paste0(
                    "background:",badge_col,";color:white;border-radius:5px;",
                    "padding:3px 8px;font-size:11px;font-weight:600;"),
                    paste0(top_vars[j]," (", top_ct[j],"% | cos²=",top_cs[j],")"))
                })
              ),
              tags$p(style="font-size:11px;color:#888;margin:8px 0 0;font-style:italic;",
                paste0("Cet axe oppose ou résume principalement : ",
                       paste(head(top_vars, 3), collapse=", "), ".",
                       if (length(top_vars) > 3) paste0(" + ", length(top_vars)-3, " autre(s).") else ""))
            )
          })

          tagList(
            tags$div(class="section-header",
                     paste0("📝 Description des ", n_desc, " axe",
                            if(n_desc>1)"s", " retenus")),
            tagList(cartes)
          )
        } else NULL
      }
    )
  })

  output$rdim_table <- renderDT({
    req(rdim_res()); r <- rdim_res()$res
    tryCatch({
      eig <- get_eigenvalue(r)
      tibble(
        Dimension         = paste0("Dim.",1:nrow(eig)),
        `λ (val. propre)` = round(eig[,1],4),
        `Variance (%)`    = round(eig[,2],2),
        `Cumul (%)`       = round(eig[,3],2),
        Kaiser            = ifelse(eig[,1]>1,"✅ λ>1","❌ λ≤1")
      ) |>
        datatable(rownames=FALSE,
                  options=list(dom="t", pageLength=20, scrollX=TRUE),
                  class="compact stripe hover") |>
        formatStyle("Kaiser",
          color=styleEqual(c("✅ λ>1","❌ λ≤1"), c("#00838F","#C0392B")),
          fontWeight="bold") |>
        formatStyle(2,   # colonne λ
          background=styleInterval(1, c("#FDEDEC","#D5F5E3")))
    }, error=function(e) datatable(data.frame(Erreur=e$message),rownames=FALSE))
  })

  # ══════════════════════════════════════════════════════════════════
  # RDIM EXPERT — t-SNE / UMAP / Parallèle / HCPC / Bootstrap
  # ══════════════════════════════════════════════════════════════════

  rdim_tsne_res  <- reactiveVal(NULL)
  rdim_umap_res  <- reactiveVal(NULL)
  rdim_pa_res    <- reactiveVal(NULL)
  rdim_hcpc_res  <- reactiveVal(NULL)
  rdim_boot_res  <- reactiveVal(NULL)

  # ── UI dynamiques : selecteurs de variable groupe ──────────────
  output$tsne_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("tsne_grp","Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$umap_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("umap_grp","Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$cmp_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("cmp_grp","Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$boot_dim_ui <- renderUI({
    r <- rdim_boot_res()
    if (is.null(r)) dims <- paste0("Dim.",1:3)
    else dims <- unique(r$table$Dimension)
    selectInput("boot_dim","Dimension :", choices=dims, width="200px")
  })

  # ── 1. Analyse Parallèle (Horn) ───────────────────────────────
  observeEvent(input$run_parallel, {
    d <- df_f(); req(d)
    withProgress(message="Analyse parallèle en cours…", value=0.3, {
      tryCatch({
        pa <- calc_parallel_analysis(d, n_sim=input$pa_nsim)
        rdim_pa_res(pa)
        incProgress(0.7)
        showNotification(paste0("✅ Analyse parallèle : ",pa$n_retained," axe(s) retenu(s)"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$pa_plot <- renderPlotly({
    pa <- rdim_pa_res(); req(!is.null(pa))
    tryCatch(plot_parallel_analysis(pa), error=function(e) plotly_empty())
  })

  output$pa_table <- renderDT({
    pa <- rdim_pa_res(); req(!is.null(pa))
    pa$table |>
      mutate(Retenu=ifelse(Retenu,"✅ Significatif","❌ Non significatif")) |>
      datatable(rownames=FALSE, options=list(dom="t",pageLength=15),
                class="compact stripe hover") |>
      formatStyle("Retenu",
        color=styleEqual(c("✅ Significatif","❌ Non significatif"),c("#00838F","#C0392B")),
        fontWeight="bold")
  })

  # ── 2. HCPC ───────────────────────────────────────────────────
  observeEvent(input$run_hcpc, {
    r <- rdim_res(); req(!is.null(r), r$method %in% c("pca","mca","famd"))
    withProgress(message="HCPC en cours…", value=0.3, {
      tryCatch({
        hcpc <- run_hcpc(r$res, nb_clust=input$hcpc_nclust)
        rdim_hcpc_res(hcpc)
        incProgress(0.7)
        showNotification(paste0("✅ HCPC : ",hcpc$n_clust," clusters (inertie inter=",
                                hcpc$inertia_pct,"%)"), type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$hcpc_map <- renderPlotly({
    r <- rdim_res(); hcpc <- rdim_hcpc_res()
    req(!is.null(r), !is.null(hcpc))
    tryCatch(plot_hcpc_map(r$res, hcpc), error=function(e) plotly_empty())
  })

  output$hcpc_dendro <- renderPlotly({
    hcpc <- rdim_hcpc_res(); req(!is.null(hcpc))
    tryCatch(plot_hcpc_dendrogram(hcpc), error=function(e) plotly_empty())
  })

  output$hcpc_desc_table <- renderDT({
    hcpc <- rdim_hcpc_res(); req(!is.null(hcpc))
    desc <- tryCatch(hcpc_cluster_description(hcpc), error=function(e) NULL)
    if (is.null(desc) || nrow(desc)==0) return(datatable(data.frame(Info="Pas de description quantitative disponible")))
    datatable(desc, rownames=FALSE, options=list(dom="tip",pageLength=15,scrollX=TRUE),
              class="compact stripe hover")
  })

  # ── 3. t-SNE ──────────────────────────────────────────────────
  observeEvent(input$run_tsne, {
    d <- df_f(); req(d)
    withProgress(message="t-SNE en cours…", value=0.3, {
      tryCatch({
        ts <- run_tsne(d, perplexity=input$tsne_perp, max_iter=input$tsne_iter)
        rdim_tsne_res(ts)
        incProgress(0.7)
        showNotification(paste0("✅ t-SNE terminé (n=",ts$n_obs,", perp=",ts$perplexity,")"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$tsne_plot <- renderPlotly({
    ts <- rdim_tsne_res(); req(!is.null(ts))
    d <- df_f()
    grp <- if (!is.null(input$tsne_grp) && input$tsne_grp != "" && input$tsne_grp %in% names(d)) {
      d_clean <- d |> select(where(is.numeric)) |> tidyr::drop_na()
      # Aligner les indices
      idx <- which(complete.cases(d |> select(where(is.numeric))))
      d[[input$tsne_grp]][idx[seq_len(nrow(ts$coords))]]
    } else NULL
    tryCatch(plot_tsne(ts, grp), error=function(e) plotly_empty())
  })

  # ── 4. UMAP ───────────────────────────────────────────────────
  observeEvent(input$run_umap, {
    d <- df_f(); req(d)
    withProgress(message="UMAP en cours…", value=0.3, {
      tryCatch({
        um <- run_umap(d, n_neighbors=input$umap_nn, min_dist=input$umap_md)
        rdim_umap_res(um)
        incProgress(0.7)
        showNotification(paste0("✅ UMAP terminé (n=",um$n_obs,", voisins=",um$n_neighbors,")"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$umap_plot <- renderPlotly({
    um <- rdim_umap_res(); req(!is.null(um))
    d <- df_f()
    grp <- if (!is.null(input$umap_grp) && input$umap_grp != "" && input$umap_grp %in% names(d)) {
      idx <- which(complete.cases(d |> select(where(is.numeric))))
      d[[input$umap_grp]][idx[seq_len(nrow(um$coords))]]
    } else NULL
    tryCatch(plot_umap(um, grp), error=function(e) plotly_empty())
  })

  # ── 5. Bootstrap Stabilité ────────────────────────────────────
  observeEvent(input$run_boot, {
    d <- df_f(); req(d)
    withProgress(message="Bootstrap loadings en cours…", value=0.2, {
      tryCatch({
        bt <- calc_pca_bootstrap(d, n_boot=input$boot_nsim, n_dim=3)
        rdim_boot_res(bt)
        incProgress(0.8)
        showNotification(paste0("✅ Bootstrap terminé (",bt$n_boot," rééchantillonnages)"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$boot_plot <- renderPlotly({
    bt <- rdim_boot_res(); req(!is.null(bt), input$boot_dim)
    tryCatch(plot_pca_bootstrap(bt, input$boot_dim), error=function(e) plotly_empty())
  })

  output$boot_table <- renderDT({
    bt <- rdim_boot_res(); req(!is.null(bt))
    bt$table |>
      mutate(Stable=ifelse(Stable,"✅ Stable","⚠️ Instable")) |>
      datatable(rownames=FALSE, options=list(dom="tip",pageLength=20,scrollX=TRUE),
                class="compact stripe hover") |>
      formatStyle("Stable",
        color=styleEqual(c("✅ Stable","⚠️ Instable"),c("#2E7D52","#D98B14")),
        fontWeight="bold")
  })

  # ── 6. Comparaison méthodes (PCA vs t-SNE vs UMAP) ───────────
  observeEvent(input$run_cmp_methods, {
    d <- df_f(); req(d)
    grp <- if (!is.null(input$cmp_grp) && input$cmp_grp != "" && input$cmp_grp %in% names(d)) {
      idx <- which(complete.cases(d |> select(where(is.numeric))))
      d[[input$cmp_grp]][idx]
    } else NULL
    withProgress(message="Comparaison en cours…", value=0.2, {
      tryCatch({
        output$cmp_methods_plot <- renderPlotly({
          plot_methods_comparison(d, group_var=grp)
        })
        incProgress(0.8)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  # ── CLUSTERING ──────────────────────────────────────────────────────────────
  output$clust_vars_ui <- renderUI({
    vn <- vars_num()
    selectizeInput("clust_vars","Variables (numériques) :",choices=vn,
                   selected=head(vn,5),multiple=TRUE,width="100%")
  })
  output$clust_k_ui <- renderUI({
    if(!input$clust_auto_k && input$clust_method!="dbscan")
      sliderInput("clust_k","Nombre de clusters (k) :",2,10,3,1,width="100%")
  })
  output$clust_eps_ui <- renderUI({
    if(input$clust_method=="dbscan")
      numericInput("clust_eps","Epsilon (DBSCAN) :",0.5,0.01,5,.01,width="100%")
  })

  observeEvent(input$run_clust, {
    d <- df_f()
    vars <- input$clust_vars
    if (is.null(vars) || length(vars) < 2) {
      showNotification("⚠ Sélectionnez au moins 2 variables numériques", type="warning", duration=4)
      return()
    }
    req(d)

    withProgress(message="Clustering en cours...", {
      tryCatch({
        d_num <- d |> select(all_of(vars)) |> drop_na() |>
          mutate(across(everything(), as.numeric))

        if (nrow(d_num) < 10) stop("Trop peu de lignes complètes (< 10)")

        # Vérifier la variance : supprimer les colonnes constantes
        col_var <- sapply(d_num, var, na.rm=TRUE)
        if (any(col_var == 0)) {
          const_cols <- names(col_var[col_var == 0])
          showNotification(paste0("⚠ Colonnes constantes ignorées: ", paste(const_cols, collapse=", ")),
                          type="warning", duration=5)
          d_num <- d_num[, col_var > 0, drop=FALSE]
          if (ncol(d_num) < 2) stop("Pas assez de variables avec variance > 0")
        }

        d_scaled <- scale(d_num)
        method <- input$clust_method

        if (input$clust_auto_k && method != "dbscan") {
          max_k <- min(8, nrow(d_scaled)-1)
          if (max_k < 2) stop("Pas assez d'observations pour le clustering")
          sil_scores <- sapply(2:max_k, function(k) {
            km <- kmeans(d_scaled, k, nstart=10)
            mean(silhouette(km$cluster, dist(d_scaled))[,3])
          })
          k_opt <- which.max(sil_scores) + 1
        } else {
          k_opt <- isolate(input$clust_k)
          if (is.null(k_opt)) k_opt <- 3
        }

        res_clust <- if (method == "kmeans") {
          km <- kmeans(d_scaled, k_opt, nstart=25)
          list(clusters=km$cluster, method="K-means", k=k_opt,
               withinss=km$tot.withinss, obj=km)
        } else if (method == "hclust") {
          hc <- hclust(dist(d_scaled), method="ward.D2")
          cl <- cutree(hc, k_opt)
          list(clusters=cl, method="CAH (Ward.D2)", k=k_opt,
               obj=hc, d_num=d_scaled)
        } else {
          if (!requireNamespace("dbscan", quietly = TRUE))
            stop("Le package 'dbscan' est requis. Installez-le avec install.packages('dbscan').")
          eps_val <- isolate(input$clust_eps)
          if (is.null(eps_val)) eps_val <- 0.5
          db <- dbscan::dbscan(d_scaled, eps=eps_val, minPts=5)
          list(clusters=db$cluster, method="DBSCAN", k=length(unique(db$cluster)),
               obj=db)
        }

        pca_d <- PCA(d_scaled, ncp=2, graph=FALSE)
        clust_res(list(res=res_clust, pca=pca_d,
                       d_orig=d |> select(all_of(vars)) |> drop_na(),
                       d_scaled=d_scaled,
                       sil_scores=if(input$clust_auto_k && method!="dbscan")
                         tibble(k=2:min(8,nrow(d_scaled)-1), sil=sil_scores) else NULL))
        showNotification(
          paste0("✅ ", res_clust$method, " : ", res_clust$k, " clusters — ",
                 nrow(d_num), " observations"),
          type="message", duration=4)
      }, error=function(e) showNotification(paste("❌ Erreur:", e$message), type="error", duration=8))
    })
  })

  output$clust_plot2d <- renderPlotly({
    req(clust_res()); cr <- clust_res()
    ind <- as.data.frame(cr$pca$ind$coord[,1:2])
    names(ind) <- c("Dim1","Dim2")
    ind$Cluster <- factor(cr$res$clusters)
    nk <- length(unique(cr$res$clusters))
    p <- ggplot(ind,aes(x=Dim1,y=Dim2,color=Cluster,shape=Cluster,
      text=paste0("Cluster ",Cluster,"\nDim1=",round(Dim1,2),"\nDim2=",round(Dim2,2))))+
      geom_point(alpha=.7,size=2.5)+
      stat_ellipse(aes(group=Cluster),level=.95,linewidth=.8)+
      scale_color_manual(values=gpal(nk,"cat"))+
      labs(x=paste0("Dim1 (",round(get_eigenvalue(cr$pca)[1,2],1),"%)"),
           y=paste0("Dim2 (",round(get_eigenvalue(cr$pca)[2,2],1),"%)"),
           title=paste0(cr$res$method," — ",nk," clusters | Ellipses IC 95%"))+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") |> nutri_config()
  })

  output$clust_optimal <- renderPlotly({
    req(clust_res()); cr <- clust_res()
    if(is.null(cr$sil_scores)) {
      return(plotly_empty()|>layout(title=list(text="Nombre optimal non calculé",x=.5)))
    }
    p <- ggplot(cr$sil_scores,aes(x=k,y=sil,
      text=paste0("k=",k,"\nSilhouette=",round(sil,4))))+
      geom_line(color=gprimary(),linewidth=1.2)+
      geom_point(size=3.5,color=gprimary())+
      geom_point(data=filter(cr$sil_scores,sil==max(sil)),
                 size=5,color=C$rouge,shape=18)+
      labs(x="Nombre de clusters (k)",y="Score silhouette moyen",
           title="Nombre optimal de clusters (méthode silhouette)")+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") |> nutri_config()
  })

  output$clust_dendro <- renderPlot({
    req(clust_res()); cr <- clust_res()
    if(cr$res$method!="CAH (Ward.D2)") {
      plot(0,type="n",main="Dendrogramme disponible uniquement pour CAH"); return(NULL)
    }
    hc <- cr$res$obj; k <- cr$res$k
    plot(hc,main=paste0("Dendrogramme — CAH Ward.D2 (k=",k,")"),
         sub="",xlab="",cex=.6,hang=-1)
    rect.hclust(hc,k=k,border=RColorBrewer::brewer.pal(min(k,8),"Set2"))
  }, height=380)

  output$clust_profile <- renderPlotly({
    req(clust_res()); cr <- clust_res()
    d_orig <- cr$d_orig
    d_orig$Cluster <- factor(cr$res$clusters)
    nk <- length(unique(cr$res$clusters))
    profil <- d_orig |> group_by(Cluster) |>
      summarise(across(where(is.numeric),~round(mean(.,na.rm=TRUE),3)),.groups="drop") |>
      pivot_longer(-Cluster,names_to="Variable",values_to="Moyenne")
    n_vars <- length(unique(profil$Variable))
    profil$Variable <- truncate_label(profil$Variable, 18)
    # Horizontal si beaucoup de variables
    p <- if (n_vars > 10) {
      ggplot(profil,aes(y=Variable,x=Moyenne,fill=Cluster,
        text=paste0("Cluster ",Cluster,"\n",Variable,"=",round(Moyenne,3))))+
        geom_col(position="dodge",alpha=.88,width=.72)+
        scale_fill_manual(values=gpal(nk,"cat"))+
        labs(y="",x="Moyenne")+theme_nutri()
    } else {
      ggplot(profil,aes(x=Variable,y=Moyenne,fill=Cluster,
        text=paste0("Cluster ",Cluster,"\n",Variable,"=",round(Moyenne,3))))+
        geom_col(position="dodge",alpha=.88,width=.72)+
        scale_fill_manual(values=gpal(nk,"cat"))+
        labs(x="",y="Moyenne")+theme_nutri()+smart_axis_x(n_vars)
    }
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.3)) |> nutri_config()
  })

  output$clust_summary <- renderPrint({
    req(clust_res()); cr <- clust_res()
    cat("Méthode :", cr$res$method, "\n")
    cat("Clusters :", cr$res$k, "\n")
    cat("Tailles :", table(cr$res$clusters), "\n")
    if(!is.null(cr$res$withinss))
      cat("Within SS :", round(cr$res$withinss,2), "\n")
  })

  output$clust_table <- renderDT({
    req(clust_res()); cr <- clust_res()
    d_orig <- cr$d_orig
    d_orig$Cluster <- factor(cr$res$clusters)
    d_orig |>
      group_by(Cluster) |>
      summarise(N=n(),across(where(is.numeric),~round(mean(.,na.rm=TRUE),2)),.groups="drop") |>
      datatable(rownames=FALSE,options=list(dom="t",pageLength=15,scrollX=TRUE),
                class="compact stripe")
  })

  # ══════════════════════════════════════════════════════════════════════════════
  # Z-SCORES OMS 2006 — Server Logic
  # ══════════════════════════════════════════════════════════════════════════════

  zs_data <- reactiveVal(NULL)
  source("zscore_expert_integration.R", local = TRUE)

  # Auto-détection des colonnes
  observe({
    d <- df_f(); req(d)
    noms <- names(d)
    det <- detect_zscore_columns(noms)
    num_cols <- noms[vapply(d, is.numeric, logical(1))]
    all_cols <- noms

    output$zs_col_poids_ui <- renderUI({
      sel <- if (!is.null(det$poids)) det$poids else NULL
      selectInput("zs_col_poids", "Poids (kg) :", choices = c("—" = "", num_cols), selected = sel)
    })
    output$zs_col_taille_ui <- renderUI({
      sel <- if (!is.null(det$taille)) det$taille else NULL
      selectInput("zs_col_taille", "Taille/Longueur (cm) :", choices = c("—" = "", num_cols), selected = sel)
    })
    output$zs_col_age_ui <- renderUI({
      sel <- if (!is.null(det$age)) det$age else NULL
      selectInput("zs_col_age", "Âge (mois) :", choices = c("—" = "", num_cols), selected = sel)
    })
    output$zs_col_sexe_ui <- renderUI({
      sel <- if (!is.null(det$sexe)) det$sexe else NULL
      selectInput("zs_col_sexe", "Sexe :", choices = c("—" = "", all_cols), selected = sel)
    })
  })

  # ══════════════════════════════════════════════════════════════════════════════
  # SERVEUR — MODULES SENS (Mortalité, IYCF, Anémie, Paludisme, WASH, Ménage, WG)
  # ══════════════════════════════════════════════════════════════════════════════

  # Helper pour créer un selectInput dynamique basé sur auto-détection
  sens_select <- function(id, label, detect_key, d, allow_empty = TRUE) {
    all_cols <- names(d)
    det <- auto_detect(d)
    sel <- det[[detect_key]]
    choices <- if (allow_empty) c("—" = "", all_cols) else all_cols
    selectInput(id, label, choices = choices, selected = sel)
  }

  # ── MORTALITÉ ──────────────────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$mort_deaths_ui    <- renderUI(sens_select("mort_deaths", "Nb décès", "nb_deces", d))
    output$mort_pop_start_ui <- renderUI(sens_select("mort_pop_start", "Pop début rappel", "pop_debut", d))
    output$mort_pop_end_ui   <- renderUI(sens_select("mort_pop_end", "Pop fin rappel", "pop_fin", d))
    output$mort_u5_pop_ui    <- renderUI(sens_select("mort_u5_pop", "Enfants < 5 ans", "taille_menage_0_5", d))
    output$mort_age_dec_ui   <- renderUI(sens_select("mort_age_dec", "Âge décédé", "age_decede", d))
    output$mort_cause_ui     <- renderUI(sens_select("mort_cause", "Cause décès", "cause_deces", d))
    output$mort_sex_dec_ui   <- renderUI(sens_select("mort_sex_dec", "Sexe décédé", "sexe_decede", d))
    output$mort_recall_ui    <- renderUI(sens_select("mort_recall", "Période rappel (col)", "recall_period", d))
  }) |> bindEvent(df_f())

  mort_res <- reactiveValues(cmr = NULL, u5mr = NULL, cause = NULL)

  observeEvent(input$run_mort, {
    d <- df_f(); req(d)
    withProgress(message = "Calcul mortalité...", {
      col_d <- if (!is.null(input$mort_deaths) && input$mort_deaths != "") input$mort_deaths else NULL
      col_ps <- if (!is.null(input$mort_pop_start) && input$mort_pop_start != "") input$mort_pop_start else NULL
      col_pe <- if (!is.null(input$mort_pop_end) && input$mort_pop_end != "") input$mort_pop_end else NULL
      col_u5 <- if (!is.null(input$mort_u5_pop) && input$mort_u5_pop != "") input$mort_u5_pop else NULL
      col_ad <- if (!is.null(input$mort_age_dec) && input$mort_age_dec != "") input$mort_age_dec else NULL
      col_rc <- if (!is.null(input$mort_recall) && input$mort_recall != "") input$mort_recall else NULL
      col_ca <- if (!is.null(input$mort_cause) && input$mort_cause != "") input$mort_cause else NULL

      if (!is.null(col_d) && !is.null(col_ps) && !is.null(col_pe)) {
        mort_res$cmr <- calc_cmr(d, col_d, col_ps, col_pe, col_rc, input$mort_recall_days)
      }
      if (!is.null(col_d) && !is.null(col_u5) && !is.null(col_ad)) {
        mort_res$u5mr <- calc_u5mr(d, col_d, col_ad, col_u5, col_recall_days = col_rc, recall_days = input$mort_recall_days)
      }
      if (!is.null(col_ca)) mort_res$cause <- calc_cause_death(d, col_ca)
    })
    showNotification("Mortalité calculée ✓", type = "message")
  })

  output$mort_kpi <- renderUI({ req(mort_res$cmr); HTML(render_mortality_kpi(mort_res$cmr, mort_res$u5mr)) })
  output$mort_summary_table <- DT::renderDT({
    req(mort_res$cmr)
    tbl <- render_mortality_summary_table(mort_res$cmr, mort_res$u5mr)
    if (nrow(tbl) > 0) DT::datatable(tbl, options = list(dom = "t"))
  })
  output$mort_cause_plot <- renderPlot({ req(mort_res$cause); plot_cause_death(mort_res$cause) })
  output$mort_cause_table <- DT::renderDT({
    req(mort_res$cause); DT::datatable(mort_res$cause$table, options = list(pageLength = 15))
  })
  output$mort_pyramid_plot <- renderPlot({
    d <- df_f(); req(d)
    col_age <- if (!is.null(input$mort_age_dec) && input$mort_age_dec != "") input$mort_age_dec else NULL
    col_sex <- if (!is.null(input$mort_sex_dec) && input$mort_sex_dec != "") input$mort_sex_dec else NULL
    req(col_age, col_sex)
    plot_mortality_pyramid(d[!is.na(d[[col_age]]), ], col_age, col_sex)
  })

  # ── IYCF ───────────────────────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    iycf_det <- iycf_detect_columns(d, list(), use_design = TRUE)
    iycf_pick <- function(key, aux = FALSE) {
      v <- if (aux) iycf_det$aux[[key]] else iycf_det$columns[[key]]
      if (is.null(v) || !nzchar(v)) "" else v
    }
    iycf_select <- function(id, label, selected) {
      selectInput(id, label, choices = c("—" = "", names(d)), selected = selected)
    }
    output$iycf_age_ui <- renderUI(iycf_select("iycf_age", "Âge (mois)", iycf_pick("age", TRUE)))
    output$iycf_cluster_ui <- renderUI({
      cand <- intersect(c("cluster", "cluster_id", "grappe", "psu", "primary_sampling_unit"), names(d))
      iycf_select("iycf_cluster", "Cluster / grappe", if (length(cand) > 0) cand[1] else "")
    })
    output$iycf_strata_ui <- renderUI({
      cand <- intersect(c("strate", "strata", "stratum", "sampling_strata"), names(d))
      iycf_select("iycf_strata", "Strates", if (length(cand) > 0) cand[1] else "")
    })
    output$iycf_weights_ui <- renderUI({
      cand <- intersect(c("poids_echantillon", "sampling_weight", "survey_weight",
                          "design_weight", "poids_pondere", "weights"), names(d))
      iycf_select("iycf_weights", "Poids de sondage", if (length(cand) > 0) cand[1] else "")
    })
    output$iycf_bfstatus_ui <- renderUI({
      iycf_select("iycf_bfstatus", "Statut d'allaitement (dérivation)", iycf_pick("bf_status", TRUE))
    })
    output$iycf_ebf_ui    <- renderUI(iycf_select("iycf_ebf", "Allaitement exclusif", iycf_pick("ebf")))
    output$iycf_cbf_ui    <- renderUI(iycf_select("iycf_cbf", "Allaitement continu", iycf_pick("cbf")))
    output$iycf_eibf_ui   <- renderUI(iycf_select("iycf_eibf", "Mise au sein précoce", iycf_pick("eibf")))
    output$iycf_isf_ui    <- renderUI(iycf_select("iycf_isf", "Intro aliments solides", iycf_pick("isf")))
    output$iycf_mdd_ui    <- renderUI(iycf_select("iycf_mdd", "Diversité min (MDD)", iycf_pick("mdd")))
    output$iycf_mmf_ui    <- renderUI(iycf_select("iycf_mmf", "Fréquence min repas (MMF)", iycf_pick("mmf")))
    output$iycf_mad_ui    <- renderUI(iycf_select("iycf_mad", "Régime acceptable (MAD)", iycf_pick("mad")))
    output$iycf_iron_ui   <- renderUI(iycf_select("iycf_iron", "Aliments riches en fer", iycf_pick("iron")))
    output$iycf_bottle_ui <- renderUI(iycf_select("iycf_bottle", "Biberon", iycf_pick("bottle")))
    output$iycf_sweet_ui  <- renderUI(iycf_select("iycf_sweet", "Boissons sucrées", iycf_pick("sweet")))
    output$iycf_upt_ui    <- renderUI(iycf_select("iycf_upt", "Ultra-transformés", iycf_pick("upt")))
    output$iycf_zvf_ui    <- renderUI(iycf_select("iycf_zvf", "Zéro légumes/fruits", iycf_pick("zvf")))
    output$iycf_vita_ui   <- renderUI(iycf_select("iycf_vita", "Vitamine A (6-59 mois)", iycf_pick("vita")))
    output$iycf_anc_ui    <- renderUI(iycf_select("iycf_anc", "ANC 4+ (visites CPN)", iycf_pick("anc4")))
    output$iycf_stratvar_ui <- renderUI({
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id"))
      sel <- intersect(c("region", "province", "department", "survey_round", "child_sex"), cats)
      selectInput("iycf_stratvar", "Strate", choices = cats,
                  selected = if (length(sel) > 0) sel[1] else cats[1])
    })
    output$iycf_equity_var_ui <- renderUI({
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id", "id_menage"))
      sel <- intersect(c("region", "province", "departement", "dept", "sexe",
                         "child_sex", "milieu", "strate"), cats)
      selectInput("iycf_equity_var", "Variable d'équité", choices = cats,
                  selected = if (length(sel) > 0) sel[1] else cats[1])
    })
  }) |> bindEvent(df_f())

  iycf_res <- reactiveVal(NULL)

  observeEvent(input$run_iycf, {
    d <- df_f(); req(d)
    withProgress(message = "Calcul IYCF...", {
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      result <- calc_iycf_indicators(d, col_age = g("iycf_age"),
        col_ebf = g("iycf_ebf"), col_cbf = g("iycf_cbf"), col_eibf = g("iycf_eibf"),
        col_isf = g("iycf_isf"), col_mdd = g("iycf_mdd"), col_mmf = g("iycf_mmf"),
        col_mad = g("iycf_mad"), col_iron = g("iycf_iron"), col_bottle = g("iycf_bottle"),
        col_sweet = g("iycf_sweet"), col_upt = g("iycf_upt"), col_zvf = g("iycf_zvf"),
        col_bfstatus = g("iycf_bfstatus"), col_vita = g("iycf_vita"),
        col_anc = g("iycf_anc"),
        col_cluster = g("iycf_cluster"), col_strata = g("iycf_strata"),
        col_weights = g("iycf_weights"), use_design = isTRUE(input$iycf_use_design))
      if (!is.null(result$error)) {
        showNotification(paste("IYCF :", result$error), type = "error", duration = 8)
        return(invisible(NULL))
      }
      iycf_res(result)
    })
    showNotification(paste0("IYCF : ", iycf_res()$n_indicators, " indicateurs calculés ✓"), type = "message")
  })

  output$iycf_kpi <- renderUI({ req(iycf_res()); HTML(render_iycf_kpi(iycf_res())) })

  iycf_gap_r <- reactive({ req(iycf_res()); calc_iycf_gap(iycf_res()) })
  iycf_verdict_r <- reactive({ req(iycf_res()); calc_iycf_verdict(iycf_res()) })

  output$iycf_summary_table <- DT::renderDT({
    g <- iycf_gap_r(); req(nrow(g) > 0)
    tbl <- data.frame(
      Indicateur = g$Indicateur,
      N = g$N, Positifs = g$Positifs,
      `Prévalence (%)` = g$Prevalence,
      `IC 95% inf` = g$IC_inf, `IC 95% sup` = g$IC_sup,
      `Cible OMS (%)` = g$Cible_OMS, `Écart (pts)` = g$Gap,
      Statut = g$Statut, check.names = FALSE)
    DT::datatable(tbl, rownames = FALSE,
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE),
                  class = "compact stripe hover")
  })

  output$iycf_verdict <- renderUI({
    v <- iycf_verdict_r(); req(v)
    tagList(
      tags$div(style = paste0("background:", v$bg, ";border-left:5px solid ", v$color,
        ";border-radius:10px;padding:14px 18px;margin-bottom:12px;"),
        tags$div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          tags$span(style = paste0("background:", v$color,
            ";color:white;font-size:11px;font-weight:700;padding:3px 10px;",
            "border-radius:20px;letter-spacing:.5px;text-transform:uppercase;"), v$badge),
          tags$span(style = "font-size:13px;color:#333;", tags$b(v$headline))
        ),
        if (length(v$bullets) > 0)
          tags$ul(style = "margin:8px 0 0 0;padding-left:18px;font-size:12px;color:#444;line-height:1.7;",
            lapply(v$bullets, function(b) tags$li(b)))
      )
    )
  })

  output$iycf_gauge <- renderPlotly({ v <- iycf_verdict_r(); req(v); plot_iycf_gauge(v) })
  output$iycf_reading_synth <- renderUI({ HTML(iycf_reading_synthesis(iycf_verdict_r()) %||% "") })

  output$iycf_dashboard_plot <- renderPlot({ req(iycf_res()); plot_iycf_dashboard(iycf_res()) })
  output$iycf_reading_dash <- renderUI({ HTML(iycf_reading_dashboard(iycf_res()) %||% "") })

  output$iycf_bf_cascade_plot <- renderPlot({ req(iycf_res()); plot_breastfeeding_cascade(iycf_res()) })
  output$iycf_reading_bf <- renderUI({ HTML(iycf_reading_bf(iycf_res()) %||% "") })

  output$iycf_diet_plot <- renderPlotly({
    g <- iycf_gap_r(); req(nrow(g) > 0)
    gg <- g[g$key %in% c("isf", "mdd", "mmf", "mad", "iron", "vita"), , drop = FALSE]
    plot_iycf_gap(gg)
  })
  output$iycf_reading_diet <- renderUI({ HTML(iycf_reading_diet(iycf_res()) %||% "") })

  output$iycf_risk_plot <- renderPlotly({ g <- iycf_gap_r(); req(nrow(g) > 0); plot_iycf_risk(g) })
  output$iycf_reading_risks <- renderUI({ HTML(iycf_reading_risks(iycf_res()) %||% "") })

  output$iycf_gap_plot <- renderPlotly({ plot_iycf_gap(iycf_gap_r()) })
  output$iycf_gap_table <- DT::renderDT({
    g <- iycf_gap_r(); req(nrow(g) > 0)
    tbl <- data.frame(
      Indicateur = g$Indicateur,
      `Prévalence (%)` = g$Prevalence,
      `IC inf` = g$IC_inf, `IC sup` = g$IC_sup,
      `Cible (%)` = g$Cible_OMS, `Écart (pts)` = g$Gap,
      Statut = g$Statut, check.names = FALSE)
    DT::datatable(tbl, rownames = FALSE,
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_gaps <- renderUI({ HTML(iycf_reading_gaps(iycf_gap_r()) %||% "") })

  output$iycf_focus_ui <- renderUI({
    r <- iycf_res()
    if (is.null(r) || nrow(r$summary) == 0)
      return(tags$div(style = "font-size:11px;color:#777;",
                      icon("info-circle"), " Lancer le calcul pour choisir l'indicateur de suivi."))
    keys <- r$summary$key
    labs <- vapply(keys, function(k) IYCF_INDICATORS[[k]]$label, character(1))
    sel <- if ("eibf" %in% keys) "eibf" else keys[1]
    selectInput("iycf_focus", "Indicateur de suivi",
                choices = stats::setNames(keys, labs), selected = sel)
  })

  iycf_strat_r <- reactive({
    r <- iycf_res(); d <- df_f()
    if (is.null(r) || is.null(input$iycf_focus) || is.null(input$iycf_stratvar)) return(NULL)
    calc_iycf_strat(r, input$iycf_focus, input$iycf_stratvar, d)
  })
  output$iycf_strat_plot <- renderPlotly({
    s <- iycf_strat_r()
    if (is.null(s))
      return(plotly::plotly_empty() |> plotly::layout(title = list(text = "Test non calculable", x = 0.5)))
    plot_iycf_strat(s)
  })
  output$iycf_strat_table <- DT::renderDT({
    s <- iycf_strat_r(); req(s)
    tbl <- as.data.frame(s$freq)
    names(tbl) <- c("Strate", "N", "Atteints", "Prévalence (%)")
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "ftip", pageLength = 15))
  })
  output$iycf_strat_test_out <- renderUI({
    s <- iycf_strat_r()
    if (is.null(s))
      return(tags$div(style = "font-size:12px;color:#777;", "Association non calculable pour cet indicateur."))
    tags$div(style = "font-size:12px;color:#333;background:#f8f9fa;border-radius:6px;padding:8px 12px;",
      tags$b("Test d'association — "), s$test$note, " ; ", fmt_p(s$test$p),
      " ; V de Cramér = ", ifelse(is.na(s$test$V), "—", round(s$test$V, 3)),
      " ; n = ", s$test$n)
  })
  output$iycf_reading_strat <- renderUI({ HTML(iycf_reading_strat(iycf_strat_r()) %||% "") })

  iycf_round_var <- reactive({
    d <- df_f(); req(d)
    cand <- c(auto_detect(d)[["survey"]],
              intersect(c("survey_round", "round", "vague", "wave", "vague_enquete",
                          "survey", "round_enquete"), names(d)))
    cand <- unique(cand[!is.na(cand) & nzchar(cand) & cand %in% names(d)])
    cand <- cand[vapply(cand, function(v) {
      length(unique(stats::na.omit(d[[v]]))) >= 2
    }, logical(1))]
    if (length(cand) > 0) cand[1] else NULL
  })
  iycf_rounds_r <- reactive({
    r <- iycf_res(); rv <- iycf_round_var(); d <- df_f()
    if (is.null(r) || is.null(input$iycf_focus) || is.null(rv)) return(NULL)
    calc_iycf_rounds(r, input$iycf_focus, rv, d)
  })
  output$iycf_rounds_plot <- renderPlotly({
    r <- iycf_rounds_r()
    if (is.null(r))
      return(plotly::plotly_empty() |> plotly::layout(title = list(text = "Tendance non calculable", x = 0.5)))
    plot_iycf_rounds(r)
  })
  output$iycf_rounds_table <- DT::renderDT({
    r <- iycf_rounds_r()
    if (is.null(r)) return(DT::datatable(data.frame(Message = "Tendance non calculable")))
    tbl <- as.data.frame(r$summary)
    names(tbl) <- c("Round", "N", "Atteints", "Prévalence (%)", "IC inf", "IC sup")
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 15))
  })
  output$iycf_reading_rounds <- renderUI({ HTML(iycf_reading_rounds(iycf_rounds_r()) %||% "") })

  invisible(lapply(c("iycf_verdict", "iycf_kpi", "iycf_gauge", "iycf_summary_table",
                    "iycf_quality_summary", "iycf_quality_controls", "iycf_quality_indicators",
                    "iycf_mapping_table", "iycf_age_plot", "iycf_age_table",
                    "iycf_equity_plot", "iycf_equity_table", "iycf_export_info",
                    "iycf_reading_synth", "iycf_reading_quality", "iycf_reading_age",
                    "iycf_reading_equity"),
                  function(id) outputOptions(output, id, suspendWhenHidden = FALSE)))

  output$iycf_quality_summary <- renderUI({
    r <- iycf_res(); q <- r$quality
    if (is.null(q))
      return(tags$div(style = "font-size:12px;color:#777;", "Contrôle qualité non disponible."))
    tags$div(style = paste0("background:#f8f9fa;border-left:5px solid ",
                            if (q$score >= 85) "#27AE60" else if (q$score >= 70) "#E67E22" else "#C80000",
                            ";border-radius:8px;padding:10px 14px;font-size:12px;color:#333;"),
      tags$b(sprintf("Qualité des données : %s/100 — %s", q$score, q$label)), tags$br(),
      if (length(q$issues) > 0) paste0("Points de vigilance : ", paste(q$issues, collapse = " ; "))
      else "Aucun problème majeur détecté.")
  })
  output$iycf_quality_controls <- DT::renderDT({
    r <- iycf_res(); q <- r$quality; req(q, q$child)
    DT::datatable(q$child, rownames = FALSE, options = list(dom = "t", pageLength = 15))
  })
  output$iycf_quality_indicators <- DT::renderDT({
    r <- iycf_res(); q <- r$quality; req(q, q$indicators)
    DT::datatable(q$indicators, rownames = FALSE,
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_mapping_table <- DT::renderDT({
    r <- iycf_res(); req(r)
    tbl <- iycf_mapping_table(r)
    if (nrow(tbl) == 0) return(DT::datatable(data.frame(Message = "Aucun mapping disponible")))
    DT::datatable(tbl, rownames = FALSE, filter = "top",
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_quality <- renderUI({ HTML(iycf_reading_quality(iycf_res()) %||% "") })

  output$iycf_age_plot <- renderPlot({
    r <- iycf_res(); req(r)
    plot_iycf_age(r, input$iycf_focus)
  })
  output$iycf_age_table <- DT::renderDT({
    r <- iycf_res(); ap <- r$age_profile
    if (is.null(ap) || nrow(ap) == 0) return(DT::datatable(data.frame(Message = "Profil par âge non disponible")))
    if (!is.null(input$iycf_focus) && input$iycf_focus %in% ap$key)
      ap <- ap[ap$key == input$iycf_focus, , drop = FALSE]
    tbl <- ap[, intersect(c("Indicateur", "Groupe_age", "N", "Positifs", "Prevalence",
                            "IC_inf", "IC_sup", "N_pondere", "Methode"), names(ap)), drop = FALSE]
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_age <- renderUI({ HTML(iycf_reading_age(iycf_res()) %||% "") })

  iycf_equity_r <- reactive({
    r <- iycf_res(); d <- df_f(); gv <- input$iycf_equity_var
    if (is.null(r) || is.null(gv) || !gv %in% names(d)) return(data.frame())
    calc_iycf_equity(r, d, gv)
  })
  output$iycf_equity_plot <- renderPlotly({
    e <- iycf_equity_r()
    if (is.null(e) || nrow(e) == 0)
      return(plotly::plotly_empty() |> plotly::layout(title = list(text = "Équité non calculable", x = 0.5)))
    plot_iycf_equity(e, input$iycf_focus)
  })
  output$iycf_equity_table <- DT::renderDT({
    e <- iycf_equity_r()
    if (is.null(e) || nrow(e) == 0) return(DT::datatable(data.frame(Message = "Équité non calculable")))
    tbl <- e[, intersect(c("Indicateur", "group", "n", "k", "pct", "ci_lo", "ci_hi",
                           "Prevalence_globale", "Ecart_pts", "p_value", "Test",
                           "Methode_test"), names(e)), drop = FALSE]
    DT::datatable(tbl, rownames = FALSE, filter = "top",
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_equity <- renderUI({ HTML(iycf_reading_equity(iycf_equity_r()) %||% "") })

  iycf_child_r <- reactive({ req(iycf_res()); build_iycf_child_table(df_f(), iycf_res()) })
  output$iycf_child_table <- DT::renderDT({
    tbl <- iycf_child_r(); req(tbl)
    DT::datatable(tbl, rownames = FALSE, filter = "top",
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE),
                  class = "compact stripe hover")
  })

  iycf_frames_r <- reactive({
    if (is.null(iycf_res())) return(list())
    iycf_export_frames(iycf_res(), iycf_verdict_r(), iycf_strat_r(),
                       iycf_rounds_r(), iycf_child_r(), iycf_equity_r())
  })
  output$iycf_dl_xlsx <- downloadHandler(
    filename = function() paste0("iycf_anje_", Sys.Date(), ".xlsx"),
    content = function(file) {
      frames <- iycf_frames_r()
      if (length(frames) == 0) { writeLines("Aucun résultat IYCF", file); return() }
      wb <- openxlsx::createWorkbook()
      for (nm in names(frames)) {
        openxlsx::addWorksheet(wb, substr(nm, 1, 31))
        openxlsx::writeData(wb, substr(nm, 1, 31), frames[[nm]])
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })
  output$iycf_dl_csv_summary <- downloadHandler(
    filename = function() paste0("synthese_iycf_", Sys.Date(), ".csv"),
    content = function(file) {
      frames <- iycf_frames_r()
      frames$Enfants <- NULL
      if (length(frames) == 0) { writeLines("Aucun résultat IYCF", file); return() }
      synth <- dplyr::bind_rows(lapply(names(frames), function(nm) {
        x <- frames[[nm]]
        if (is.null(x) || nrow(x) == 0) return(NULL)
        x$Section <- nm
        x
      }))
      utils::write.csv2(synth, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$iycf_dl_csv_child <- downloadHandler(
    filename = function() paste0("enfants_iycf_", Sys.Date(), ".csv"),
    content = function(file) {
      tbl <- iycf_child_r()
      if (is.null(tbl)) { writeLines("Aucun résultat IYCF", file); return() }
      utils::write.csv2(tbl, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$iycf_export_info <- renderUI({
    frames <- iycf_frames_r()
    tags$div(style = "font-size:11px;color:#666;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
      icon("circle-info"), " Contenu disponible : ",
      tags$b(paste(length(frames), "feuille(s)")), " — ",
      paste(setdiff(names(frames), "Enfants"), collapse = ", "),
      if ("Enfants" %in% names(frames))
        paste0(" + table enfants (", nrow(iycf_child_r()), " lignes)")
    )
  })

  # ── ANÉMIE ─────────────────────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$anem_hb_ui    <- renderUI(sens_select("anem_hb", "Hémoglobine (g/dL)", "hemoglobine", d))
    output$anem_alt_ui   <- renderUI(sens_select("anem_alt", "Altitude (m)", "altitude", d))
    output$anem_smoke_ui <- renderUI(sens_select("anem_smoke", "Fumeur", "fumeur", d))
    output$anem_preg_ui  <- renderUI(sens_select("anem_preg", "Enceinte", "enceinte", d))
  }) |> bindEvent(df_f())

  anem_res <- reactiveVal(NULL)

  observeEvent(input$run_anem, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    result <- calc_anemia_prevalence(d, col_hb = g("anem_hb"), col_altitude = g("anem_alt"),
      col_smoker = g("anem_smoke"), col_pregnant = g("anem_preg"), population = input$anem_pop)
    anem_res(result)
    showNotification(paste0("Anémie totale: ", result$total_anemia, "% ✓"), type = "message")
  })

  output$anem_kpi <- renderUI({ req(anem_res()); HTML(render_anemia_kpi(anem_res())) })
  output$anem_summary_table <- DT::renderDT({
    req(anem_res()); DT::datatable(anem_res()$summary, options = list(dom = "t"))
  })
  output$anem_hb_dist <- renderPlot({ req(anem_res()); plot_hb_distribution(anem_res()) })
  output$anem_donut <- plotly::renderPlotly({ req(anem_res()); plot_anemia_donut(anem_res()) })

  # ── PALUDISME ──────────────────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$palu_poss_ui    <- renderUI(sens_select("palu_poss", "Possession LLIN", "llin_possession", d))
    output$palu_use_ui     <- renderUI(sens_select("palu_use", "Utilisation LLIN", "llin_utilisation", d))
    output$palu_access_ui  <- renderUI(sens_select("palu_access", "Accès LLIN", "llin_acces", d))
    output$palu_nb_nets_ui <- renderUI(sens_select("palu_nb_nets", "Nb moustiquaires", "nb_moustiquaires", d))
    output$palu_hh_size_ui <- renderUI(sens_select("palu_hh_size", "Taille ménage", "taille_menage", d))
    output$palu_tdr_ui     <- renderUI(sens_select("palu_tdr", "Résultat TDR", "tdr_malaria", d))
    output$palu_fever_ui   <- renderUI(sens_select("palu_fever", "Fièvre", "fievre", d))
    output$palu_strat_ui   <- renderUI(sens_select("palu_strat", "Stratification", "region", d))
  }) |> bindEvent(df_f())

  palu_llin <- reactiveVal(NULL); palu_malaria <- reactiveVal(NULL)

  observeEvent(input$run_palu, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    withProgress(message = "Calcul paludisme...", {
      palu_llin(calc_llin_indicators(d, g("palu_poss"), g("palu_use"), g("palu_access"), g("palu_nb_nets"), g("palu_hh_size")))
      if (!is.null(g("palu_tdr"))) palu_malaria(calc_malaria_prevalence(d, g("palu_tdr"), g("palu_fever"), g("palu_strat")))
    })
    showNotification("Paludisme calculé ✓", type = "message")
  })

  output$palu_kpi <- renderUI({ HTML(render_malaria_kpi(palu_malaria(), palu_llin())) })
  output$palu_cascade_plot <- renderPlot({ req(palu_llin()); plot_llin_cascade(palu_llin()) })
  output$palu_by_fever <- DT::renderDT({
    req(palu_malaria()); if (!is.null(palu_malaria()$by_fever)) DT::datatable(palu_malaria()$by_fever, options = list(dom = "t"))
  })
  output$palu_by_group <- DT::renderDT({
    req(palu_malaria()); if (!is.null(palu_malaria()$by_group)) DT::datatable(palu_malaria()$by_group, options = list(dom = "t"))
  })

  # ── WASH ───────────────────────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$wash_source_ui  <- renderUI(sens_select("wash_source", "Source d'eau", "source_eau", d))
    output$wash_time_ui    <- renderUI(sens_select("wash_time", "Temps collecte (min)", "temps_collecte_eau", d))
    output$wash_qty_ui     <- renderUI(sens_select("wash_qty", "Quantité eau (L/p/j)", "quantite_eau", d))
    output$wash_treat_ui   <- renderUI(sens_select("wash_treat", "Traitement eau", "traitement_eau", d))
    output$wash_latrine_ui <- renderUI(sens_select("wash_latrine", "Type latrine", "type_latrine", d))
    output$wash_shared_ui  <- renderUI(sens_select("wash_shared", "Latrine partagée", "latrine_partagee", d))
    output$wash_oda_ui     <- renderUI(sens_select("wash_oda", "DAL", "defecation_air_libre", d))
    output$wash_hw_ui      <- renderUI(sens_select("wash_hw", "Dispositif lavage", "lavage_mains", d))
    output$wash_soap_ui    <- renderUI(sens_select("wash_soap", "Savon", "savon_disponible", d))
    output$wash_hwsoap_ui  <- renderUI(sens_select("wash_hwsoap", "Lavage + savon", "eau_savon_lavage", d))
  }) |> bindEvent(df_f())

  wash_res <- reactiveVal(NULL)

  observeEvent(input$run_wash, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    result <- calc_wash_indicators(d, g("wash_source"), g("wash_time"), g("wash_qty"), g("wash_treat"),
      g("wash_latrine"), g("wash_shared"), g("wash_oda"), g("wash_hw"), g("wash_soap"), g("wash_hwsoap"))
    wash_res(result)
    showNotification("WASH calculé ✓", type = "message")
  })

  output$wash_kpi <- renderUI({ req(wash_res()); HTML(render_wash_kpi(wash_res())) })
  output$wash_ladder_plot <- renderPlot({ req(wash_res()); plot_wash_ladder(wash_res()) })
  output$wash_sources_table <- DT::renderDT({
    req(wash_res()); if (!is.null(wash_res()$results$water_sources)) DT::datatable(wash_res()$results$water_sources)
  })
  output$wash_treatment_table <- DT::renderDT({
    req(wash_res()); if (!is.null(wash_res()$results$treatment_methods)) DT::datatable(wash_res()$results$treatment_methods)
  })

  # ── MÉNAGE & PROTECTION ───────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$hh_size_ui       <- renderUI(sens_select("hh_size", "Taille ménage", "taille_menage", d))
    output$hh_u5_ui         <- renderUI(sens_select("hh_u5", "Enfants < 5", "taille_menage_0_5", d))
    output$hh_5_17_ui       <- renderUI(sens_select("hh_5_17", "Enfants 5-17", "taille_menage_5_17", d))
    output$hh_adults_ui     <- renderUI(sens_select("hh_adults", "Adultes", "taille_menage_adultes", d))
    output$hh_head_sex_ui   <- renderUI(sens_select("hh_head_sex", "Sexe chef ménage", "chef_menage_sexe", d))
    output$hh_head_age_ui   <- renderUI(sens_select("hh_head_age", "Âge chef ménage", "chef_menage_age", d))
    output$hh_education_ui  <- renderUI(sens_select("hh_education", "Éducation chef", "education_cm", d))
    output$hh_income_ui     <- renderUI(sens_select("hh_income", "Revenus", "revenus", d))
    output$hh_income_src_ui <- renderUI(sens_select("hh_income_src", "Source revenus", "source_revenus", d))
    output$hh_expenditure_ui<- renderUI(sens_select("hh_expenditure", "Dépenses totales", "depenses", d))
    output$hh_food_exp_ui   <- renderUI(sens_select("hh_food_exp", "Dépenses alim.", "depenses_alim", d))
    output$hh_livelihood_ui <- renderUI(sens_select("hh_livelihood", "Moyens subsistance", "moyens_subsistance", d))
    output$hh_shelter_ui    <- renderUI(sens_select("hh_shelter", "Type abri", "type_abri", d))
    output$hh_tenure_ui     <- renderUI(sens_select("hh_tenure", "Statut propriété", "statut_propriete", d))
    output$hh_registration_ui  <- renderUI(sens_select("hh_registration", "Enregistrement", "enregistrement", d))
    output$hh_documentation_ui <- renderUI(sens_select("hh_documentation", "Documentation", "documentation", d))
    output$hh_incident_ui      <- renderUI(sens_select("hh_incident", "Incident protection", "incident_protection", d))
    output$hh_child_labor_ui   <- renderUI(sens_select("hh_child_labor", "Travail enfant", "travail_enfant", d))
    output$hh_marriage_ui      <- renderUI(sens_select("hh_marriage", "Mariage précoce", "mariage_precoce", d))
  }) |> bindEvent(df_f())

  hh_profile_res <- reactiveVal(NULL); hh_econ_res <- reactiveVal(NULL)
  hh_shelter_res <- reactiveVal(NULL); hh_prot_res <- reactiveVal(NULL)

  observeEvent(input$run_menage, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    withProgress(message = "Analyse ménage...", {
      hh_profile_res(calc_household_profile(d, g("hh_size"), g("hh_u5"), g("hh_5_17"), g("hh_adults"),
        g("hh_head_sex"), g("hh_head_age"), g("hh_education")))
      hh_econ_res(calc_household_economy(d, g("hh_income"), g("hh_income_src"),
        g("hh_expenditure"), g("hh_food_exp"), g("hh_livelihood")))
      hh_shelter_res(calc_shelter(d, g("hh_shelter"), g("hh_tenure")))
      hh_prot_res(calc_protection(d, g("hh_registration"), g("hh_documentation"),
        g("hh_incident"), g("hh_child_labor"), g("hh_marriage")))
    })
    showNotification("Analyse ménage terminée ✓", type = "message")
  })

  output$hh_kpi <- renderUI({ req(hh_profile_res()); HTML(render_household_kpi(hh_profile_res(), hh_econ_res())) })
  output$hh_composition_table <- DT::renderDT({
    req(hh_profile_res()); DT::datatable(hh_profile_res()$composition, options = list(dom = "t"))
  })
  output$hh_income_plot <- renderPlot({ req(hh_econ_res()); plot_income_expenditure(hh_econ_res()) })
  output$hh_livelihood_table <- DT::renderDT({
    req(hh_econ_res()); r <- hh_econ_res()$results
    if (!is.null(r$livelihood)) DT::datatable(r$livelihood, options = list(pageLength = 10))
  })
  output$hh_shelter_table <- DT::renderDT({
    req(hh_shelter_res()); r <- hh_shelter_res()$results
    if (!is.null(r$shelter_types)) DT::datatable(r$shelter_types, options = list(dom = "t"))
  })
  output$hh_tenure_table <- DT::renderDT({
    req(hh_shelter_res()); r <- hh_shelter_res()$results
    if (!is.null(r$tenure_status)) DT::datatable(r$tenure_status, options = list(dom = "t"))
  })
  output$hh_protection_plot <- renderPlot({ req(hh_prot_res()); plot_protection_indicators(hh_prot_res()) })
  output$hh_protection_table <- DT::renderDT({
    req(hh_prot_res()); DT::datatable(hh_prot_res()$summary, options = list(dom = "t"))
  })

  # ── HANDICAP (WASHINGTON GROUP) ────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$wg_voir_ui     <- renderUI(sens_select("wg_voir", "Vision", "handicap_voir", d))
    output$wg_entendre_ui <- renderUI(sens_select("wg_entendre", "Audition", "handicap_entendre", d))
    output$wg_marcher_ui  <- renderUI(sens_select("wg_marcher", "Mobilité", "handicap_marcher", d))
    output$wg_memoire_ui  <- renderUI(sens_select("wg_memoire", "Cognition", "handicap_memoire", d))
    output$wg_soins_ui    <- renderUI(sens_select("wg_soins", "Soins personnels", "handicap_soins_personnels", d))
    output$wg_comm_ui     <- renderUI(sens_select("wg_comm", "Communication", "handicap_communication", d))
  }) |> bindEvent(df_f())

  wg_res <- reactiveVal(NULL)

  observeEvent(input$run_wg, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    result <- calc_wg_prevalence(d, g("wg_voir"), g("wg_entendre"), g("wg_marcher"),
      g("wg_memoire"), g("wg_soins"), g("wg_comm"), threshold = input$wg_threshold)
    wg_res(result)
    showNotification(paste0("Handicap: ", result$prevalence, "% ✓"), type = "message")
  })

  output$wg_kpi <- renderUI({ req(wg_res()); HTML(render_wg_kpi(wg_res())) })
  output$wg_domain_table <- DT::renderDT({
    req(wg_res()); DT::datatable(wg_res()$domain_summary, options = list(dom = "t"))
  })
  output$wg_domain_plot <- renderPlot({ req(wg_res()); plot_wg_radar(wg_res()) })
  output$wg_severity_plot <- renderPlot({ req(wg_res()); plot_wg_severity(wg_res()) })

  # ══════════════════════════════════════════════════════════════════════════
  # FIN MODULES SENS
  # ══════════════════════════════════════════════════════════════════════════

  # Calcul des Z-scores
  observeEvent(input$run_zscore, {
    d <- df_f(); req(d)

    col_p <- if (!is.null(input$zs_col_poids) && input$zs_col_poids != "") input$zs_col_poids else NULL
    col_t <- if (!is.null(input$zs_col_taille) && input$zs_col_taille != "") input$zs_col_taille else NULL
    col_a <- if (!is.null(input$zs_col_age) && input$zs_col_age != "") input$zs_col_age else NULL
    col_s <- if (!is.null(input$zs_col_sexe) && input$zs_col_sexe != "") input$zs_col_sexe else NULL

    req(col_p, col_t, col_s)

    withProgress(message = "Calcul des Z-scores OMS 2006...", value = 0.3, {
      res <- tryCatch(
        calcul_zscores_oms(d,
          col_poids  = col_p,
          col_taille = col_t,
          col_age    = col_a,
          col_sexe   = col_s,
          val_garcon = input$zs_val_garcon %||% "M",
          val_fille  = input$zs_val_fille %||% "F"
        ),
        error = function(e) { showNotification(paste("Erreur:", e$message), type="error"); NULL }
      )
      setProgress(0.8)
      if (!is.null(res)) {
        zs_data(res)

        # Injecter WHZ_calc, HAZ_calc, WAZ_calc dans les données principales
        # Le calcul utilise df_f() (filtré) qui peut avoir moins de lignes que raw_data().
        # On initialise à NA puis on remplit les lignes correspondantes.
        rd <- raw_data()
        if (!is.null(rd)) {
          n_rd <- nrow(rd)
          n_rs <- nrow(res)
          if (n_rd == n_rs) {
            # Pas de filtre actif : injection directe
            rd$WHZ_calc <- res$WHZ_calc
            rd$HAZ_calc <- res$HAZ_calc
            rd$WAZ_calc <- res$WAZ_calc
          } else {
            # Filtre actif : initialiser à NA, remplir les lignes filtrées
            # On retrouve les indices des lignes filtrées dans raw_data
            if (!"..row_id.." %in% names(rd)) {
              rd$WHZ_calc <- NA_real_
              rd$HAZ_calc <- NA_real_
              rd$WAZ_calc <- NA_real_
              # Copier les résultats dans les n_rs premières lignes (fallback)
              rd$WHZ_calc[seq_len(n_rs)] <- res$WHZ_calc
              rd$HAZ_calc[seq_len(n_rs)] <- res$HAZ_calc
              rd$WAZ_calc[seq_len(n_rs)] <- res$WAZ_calc
            }
          }
          raw_data(rd)
        }

        showNotification("✅ Z-scores calculés avec succès !", type="message", duration=5)
      }
      setProgress(1)
    })
  })

  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  # EXPERT KPI CARDS — Niveau publication OMS/UNICEF
  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  # Fonction helper : créer une card expert avec badge sévérité OMS
  make_kpi_card <- function(icon_char, icon_bg_color, title, value, unit = "",
                            severity_label = "", severity_class = "",
                            decomp_rows = NULL, stat_rows = NULL) {
    tags$div(class = "zs-kpi-card",
      tags$div(class = "zs-icon-bg", icon_char),
      tags$div(class = "zs-header",
        tags$div(class = "zs-header-icon", style = paste0("background:", icon_bg_color, ";"), 
          icon(icon_char, style = "color:white;")),
        tags$div(
          tags$div(class = "zs-title", title),
          if (!is.null(severity_label) && severity_label != "")
            tags$div(class = paste("zs-severity-badge", paste0("zs-severity-", severity_class)),
              severity_label)
        )
      ),
      tags$div(style = "margin:8px 0;",
        tags$span(class = "zs-value", value),
        if (unit != "") tags$span(class = "zs-unit", unit)
      ),
      if (!is.null(decomp_rows)) tags$div(class = "zs-decomp", decomp_rows),
      if (!is.null(stat_rows)) tags$div(class = "zs-stat-row", stat_rows)
    )
  }

  # Fonction helper : mini-barre de décomposition
  make_decomp_bar <- function(label, pct, color) {
    tags$div(class = "zs-decomp-row",
      tags$span(class = "zs-decomp-label", label),
      tags$div(class = "zs-mini-bar-track",
        tags$div(class = "zs-mini-bar-fill", 
          style = paste0("width:", min(pct, 100), "%;background:", color, ";"))
      ),
      tags$span(class = "zs-decomp-val", paste0(round(pct, 1), "%"))
    )
  }

# [DEAD CODE] zs_resume_cards — output serveur sans UI correspondante
#   # Rendu des KPI cards
#   output$zs_resume_cards <- renderUI({
#     res <- zs_data(); req(res)
#     
#     # Extraire les vecteurs Z-score
#     whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
#     haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
#     waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]
# 
#     # Compter les observations
#     n_total <- nrow(res)
#     n_garcons <- sum(toupper(trimws(res[[input$zs_col_sexe]])) %in% c("M", "MALE", "1"), na.rm = TRUE)
#     n_filles <- sum(toupper(trimws(res[[input$zs_col_sexe]])) %in% c("F", "FEMALE", "2"), na.rm = TRUE)
#     n_whz <- length(whz)
#     n_haz <- length(haz)
#     n_waz <- length(waz)
# 
#     # Prévalences OMS
#     pct_gam <- if (n_whz > 0) round(100 * mean(whz < -2), 1) else NA
#     pct_sam <- if (n_whz > 0) round(100 * mean(whz < -3), 1) else NA
#     pct_mam <- if (n_whz > 0) round(100 * mean(whz < -2 & whz >= -3), 1) else NA
#     pct_stunt <- if (n_haz > 0) round(100 * mean(haz < -2), 1) else NA
#     pct_stunt_sev <- if (n_haz > 0) round(100 * mean(haz < -3), 1) else NA
#     pct_under <- if (n_waz > 0) round(100 * mean(waz < -2), 1) else NA
#     pct_under_sev <- if (n_waz > 0) round(100 * mean(waz < -3), 1) else NA
# 
#     # Moyennes et écart-types
#     m_whz <- if (n_whz > 0) round(mean(whz), 3) else NA
#     sd_whz <- if (n_whz > 0) round(sd(whz), 3) else NA
#     m_haz <- if (n_haz > 0) round(mean(haz), 3) else NA
#     sd_haz <- if (n_haz > 0) round(sd(haz), 3) else NA
#     m_waz <- if (n_waz > 0) round(mean(waz), 3) else NA
#     sd_waz <- if (n_waz > 0) round(sd(waz), 3) else NA
# 
#     # Flags
#     n_flag_whz <- sum(res$flag_WHZ, na.rm = TRUE)
#     n_flag_haz <- sum(res$flag_HAZ, na.rm = TRUE)
#     n_flag_waz <- sum(res$flag_WAZ, na.rm = TRUE)
#     pct_flag_whz <- if (n_whz > 0) round(100 * n_flag_whz / n_whz, 1) else NA
#     pct_flag_haz <- if (n_haz > 0) round(100 * n_flag_haz / n_haz, 1) else NA
#     pct_flag_waz <- if (n_waz > 0) round(100 * n_flag_waz / n_waz, 1) else NA
# 
#     # Classification des badges OMS
#     class_gam <- if (is.na(pct_gam)) "critique"
#                  else if (pct_gam >= 15) "critique"
#                  else if (pct_gam >= 10) "serieux"
#                  else if (pct_gam >= 5) "pauvre"
#                  else "acceptable"
# 
#     class_stunt <- if (is.na(pct_stunt)) "critique"
#                    else if (pct_stunt >= 40) "critique"
#                    else if (pct_stunt >= 30) "serieux"
#                    else if (pct_stunt >= 20) "pauvre"
#                    else "acceptable"
# 
#     class_under <- if (is.na(pct_under)) "critique"
#                    else if (pct_under >= 30) "critique"
#                    else if (pct_under >= 20) "serieux"
#                    else if (pct_under >= 10) "pauvre"
#                    else "acceptable"
# 
#     label_gam <- if (class_gam == "critique") "CRITIQUE ≥15%"
#                  else if (class_gam == "serieux") "SÉRIEUX 10-14%"
#                  else if (class_gam == "pauvre") "PAUVRE 5-9%"
#                  else "ACCEPTABLE <5%"
# 
#     label_stunt <- if (class_stunt == "critique") "CRITIQUE ≥40%"
#                    else if (class_stunt == "serieux") "SÉRIEUX 30-39%"
#                    else if (class_stunt == "pauvre") "PAUVRE 20-29%"
#                    else "ACCEPTABLE <20%"
# 
#     label_under <- if (class_under == "critique") "CRITIQUE ≥30%"
#                    else if (class_under == "serieux") "SÉRIEUX 20-29%"
#                    else if (class_under == "pauvre") "PAUVRE 10-19%"
#                    else "ACCEPTABLE <10%"
# 
#     tags$div(class = "zs-kpi-grid",
#       # ── Card 1 : Échantillon ──
#       make_kpi_card(
#         icon_char = "child",
#         icon_bg_color = "#0077B6",
#         title = "Échantillon analysé",
#         value = format(n_total, big.mark = " "),
#         unit = "enfants",
#         decomp_rows = tagList(
#           make_decomp_bar(paste0("Garçons (", n_garcons, ")"), 
#             if (n_total > 0) 100 * n_garcons / n_total else 0, "#1976d2"),
#           make_decomp_bar(paste0("Filles (", n_filles, ")"), 
#             if (n_total > 0) 100 * n_filles / n_total else 0, "#d81b60")
#         ),
#         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;",
#           tags$strong(style = "font-weight:700;", "Z-scores valides :"), 
#           " WHZ ", tags$span(style = "font-weight:700;", n_whz), " | ",
#           "HAZ ", tags$span(style = "font-weight:700;", n_haz), " | ",
#           "WAZ ", tags$span(style = "font-weight:700;", n_waz))
#       ),
# 
#       # ── Card 2 : GAM (Malnutrition Aiguë) ──
#       make_kpi_card(
#         icon_char = "exclamation-triangle",
#         icon_bg_color = if (class_gam == "critique") "#d32f2f" else if (class_gam == "serieux") "#f57c00" else "#fbc02d",
#         title = "Malnutrition aiguë globale (WHZ < -2)",
#         value = paste0(pct_gam, "%"),
#         severity_label = label_gam,
#         severity_class = class_gam,
#         decomp_rows = tagList(
#           make_decomp_bar("SMA (WHZ < -3)", pct_sam, "#d32f2f"),
#           make_decomp_bar("MAM (WHZ -3 à -2)", pct_mam, "#f57c00")
#         ),
#         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;gap:16px;",
#           tags$span(tags$strong("Moy :"), " ", m_whz),
#           tags$span(tags$strong("SD :"), " ", sd_whz),
#           tags$span(style = "margin-left:auto;", 
#             span(class = "zs-quality-dot", style = if (pct_flag_whz <= 2) "background:#4caf50;" else "background:#d32f2f;"),
#             tags$strong(paste0(pct_flag_whz, "% flags")))
#         )
#       ),
# 
#       # ── Card 3 : Stunting (Retard de croissance) ──
#       make_kpi_card(
#         icon_char = "arrow-up",
#         icon_bg_color = if (class_stunt == "critique") "#d32f2f" else if (class_stunt == "serieux") "#f57c00" else "#fbc02d",
#         title = "Retard de croissance (HAZ < -2)",
#         value = paste0(pct_stunt, "%"),
#         severity_label = label_stunt,
#         severity_class = class_stunt,
#         decomp_rows = tagList(
#           make_decomp_bar("Sévère (HAZ < -3)", pct_stunt_sev, "#d32f2f"),
#           make_decomp_bar("Modéré (HAZ -3 à -2)", pct_stunt - pct_stunt_sev, "#f57c00")
#         ),
#         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;gap:16px;",
#           tags$span(tags$strong("Moy :"), " ", m_haz),
#           tags$span(tags$strong("SD :"), " ", sd_haz),
#           tags$span(style = "margin-left:auto;", 
#             span(class = "zs-quality-dot", style = if (pct_flag_haz <= 2) "background:#4caf50;" else "background:#d32f2f;"),
#             tags$strong(paste0(pct_flag_haz, "% flags")))
#         )
#       ),
# 
#       # ── Card 4 : Underweight (Insuffisance pondérale) ──
#       make_kpi_card(
#         icon_char = "weight",
#         icon_bg_color = if (class_under == "critique") "#d32f2f" else if (class_under == "serieux") "#f57c00" else "#fbc02d",
#         title = "Insuffisance pondérale (WAZ < -2)",
#         value = paste0(pct_under, "%"),
#         severity_label = label_under,
#         severity_class = class_under,
#         decomp_rows = tagList(
#           make_decomp_bar("Sévère (WAZ < -3)", pct_under_sev, "#d32f2f"),
#           make_decomp_bar("Modéré (WAZ -3 à -2)", pct_under - pct_under_sev, "#f57c00")
#         ),
#         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;gap:16px;",
#           tags$span(tags$strong("Moy :"), " ", m_waz),
#           tags$span(tags$strong("SD :"), " ", sd_waz),
#           tags$span(style = "margin-left:auto;", 
#             span(class = "zs-quality-dot", style = if (pct_flag_waz <= 2) "background:#4caf50;" else "background:#d32f2f;"),
#             tags$strong(paste0(pct_flag_waz, "% flags")))
#         )
#       )
#     )
#   })

  # Tableau résumé EXPERT
  output$zs_resume_table <- renderDT({
    res <- zs_data(); req(res)
    tab <- resume_zscores(res)
    req(nrow(tab) > 0)
    datatable(tab, rownames = FALSE,
      options = list(dom = "t", pageLength = 5, scrollX = TRUE, pageLength = 10),
      class = "compact stripe hover") |>
      formatStyle("N", backgroundColor = "#f5f5f5", fontWeight = "bold") |>
      formatStyle("Moyenne", color = styleInterval(c(-2, -1), c("#d32f2f", "#f57c00", "#333"))) |>
      formatStyle("% < -2 SD", backgroundColor = styleInterval(c(5, 10, 15, 20),
        c("#e8f5e9", "#fff9c4", "#ffe0b2", "#ffcdd2", "#ffcdd2")),
        color = styleInterval(c(5, 10, 15), c("#2e7d32", "#f57f17", "#e65100", "#c62828")),
        fontWeight = "bold") |>
      formatStyle("% Flags", backgroundColor = styleInterval(c(2, 5),
        c("#e8f5e9", "#fff9c4", "#ffcdd2")),
        fontWeight = "bold")
  })

  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  # DISTRIBUTIONS Z-SCORES — Histogrammes expert avec seuils OMS
  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  zs_plot_dist_expert <- function(z_vals, titre, couleur, strat_var = NULL, strat_data = NULL) {
    z <- z_vals[!is.na(z_vals)]
    if (length(z) < 5) return(plotly_empty() |> layout(title = list(text = paste(titre, "— données insuffisantes"))))

    bins <- seq(floor(min(z, -4)), ceiling(max(z, 4)), by = 0.25)
    x_ref <- seq(-5, 5, 0.01)
    y_ref <- dnorm(x_ref)
    
    # Statistiques
    z_mean <- mean(z)
    z_median <- median(z)
    z_sd <- sd(z)
    z_ci_low <- quantile(z, 0.025)
    z_ci_high <- quantile(z, 0.975)

    if (is.null(strat_var) || strat_var == "global") {
      hist_data <- tibble::tibble(z = z)
      p <- ggplot(hist_data, aes(x = z)) +
        geom_histogram(aes(y = after_stat(density)), breaks = bins,
                       fill = couleur, alpha = 0.62, color = "white", linewidth = 0.3) +
        geom_line(data = data.frame(x = x_ref, y = y_ref), aes(x = x, y = y),
                  color = "#333", linewidth = 1.1, linetype = "dashed") +
        geom_vline(aes(xintercept = z_mean), color = couleur, linewidth = 1.2, linetype = "solid") +
        geom_vline(aes(xintercept = z_median), color = "#666", linewidth = 0.8, linetype = "dotted") +
        annotate("rect", xmin = -5, xmax = -3, ymin = 0, ymax = Inf, fill = "#d32f2f", alpha = 0.06) +
        annotate("rect", xmin = -3, xmax = -2, ymin = 0, ymax = Inf, fill = "#f57c00", alpha = 0.06) +
        annotate("rect", xmin = 2, xmax = 3, ymin = 0, ymax = Inf, fill = "#1976d2", alpha = 0.04) +
        annotate("text", x = -4, y = Inf, label = "Critique", hjust = 0.5, vjust = 1.2,
                 size = 3, color = "#d32f2f", fontface = "bold") +
        labs(title = titre,
             subtitle = paste0("n=", length(z), " | µ=", round(z_mean, 3), " σ=", round(z_sd, 3),
                               " | IC95% [", round(z_ci_low, 2), " - ", round(z_ci_high, 2), "]"),
             x = "Z-score", y = "Densité") +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 13, color = "#0B3954"),
          plot.subtitle = element_text(size = 10, color = "#666", margin = margin(t = 4, b = 8)),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "#f0f0f0", linewidth = 0.3)
        )
    } else if (strat_var == "sexe" && !is.null(strat_data)) {
      hist_data <- strat_data |> dplyr::select(z = 1, sexe = 2) |> 
        mutate(sexe = ifelse(is.na(sexe), "Unknown", sexe))
      p <- ggplot(hist_data, aes(x = z, fill = sexe)) +
        geom_histogram(aes(y = after_stat(density)), breaks = bins,
                       alpha = 0.55, color = "white", linewidth = 0.3, position = "dodge") +
        geom_line(data = data.frame(x = x_ref, y = y_ref), aes(x = x, y = y),
                  color = "#333", linewidth = 1, linetype = "dashed", inherit.aes = FALSE) +
        annotate("rect", xmin = -5, xmax = -3, ymin = 0, ymax = Inf, fill = "#d32f2f", alpha = 0.04) +
        annotate("rect", xmin = -3, xmax = -2, ymin = 0, ymax = Inf, fill = "#f57c00", alpha = 0.04) +
        scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60", "Unknown" = "#999")) +
        labs(title = titre, subtitle = paste0("Stratifié par sexe | n=", length(z)),
             x = "Z-score", y = "Densité", fill = "Sexe") +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 13, color = "#0B3954"),
          plot.subtitle = element_text(size = 10, color = "#666", margin = margin(t = 4, b = 8)),
          panel.grid.minor = element_blank(),
          legend.position = "top"
        )
    }

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(
        showlegend = TRUE,
        hovermode = "x unified",
        margin = list(t = 80, r = 40, b = 50, l = 50)
      ) |> nutri_config()
  }

  output$zs_dist_whz <- renderPlotly({
    res <- zs_data(); req(res)
    strat <- isolate(input$zs_strat_var)
    if (strat == "sexe") {
      strat_data <- tibble(z = res$WHZ_calc, sexe = res[[isolate(input$zs_col_sexe)]])
      zs_plot_dist_expert(res$WHZ_calc, "WHZ — Poids-pour-Taille", "#1976d2", strat, strat_data)
    } else {
      zs_plot_dist_expert(res$WHZ_calc, "WHZ — Poids-pour-Taille", "#1976d2", "global")
    }
  })
  output$zs_dist_haz <- renderPlotly({
    res <- zs_data(); req(res)
    strat <- isolate(input$zs_strat_var)
    if (strat == "sexe") {
      strat_data <- tibble(z = res$HAZ_calc, sexe = res[[isolate(input$zs_col_sexe)]])
      zs_plot_dist_expert(res$HAZ_calc, "HAZ — Taille-pour-Âge", "#388e3c", strat, strat_data)
    } else {
      zs_plot_dist_expert(res$HAZ_calc, "HAZ — Taille-pour-Âge", "#388e3c", "global")
    }
  })
  output$zs_dist_waz <- renderPlotly({
    res <- zs_data(); req(res)
    strat <- isolate(input$zs_strat_var)
    if (strat == "sexe") {
      strat_data <- tibble(z = res$WAZ_calc, sexe = res[[isolate(input$zs_col_sexe)]])
      zs_plot_dist_expert(res$WAZ_calc, "WAZ — Poids-pour-Âge", "#7b1fa2", strat, strat_data)
    } else {
      zs_plot_dist_expert(res$WAZ_calc, "WAZ — Poids-pour-Âge", "#7b1fa2", "global")
    }
  })

  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  # DONUTS EXPERT — Classification nutritionnelle avec % central
  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  zs_plot_donut <- function(classes, titre, worst_label = NULL) {
    cls <- classes[!is.na(classes)]
    if (length(cls) == 0) return(plotly_empty())
    tab <- as.data.frame(table(cls), stringsAsFactors = FALSE)
    names(tab) <- c("Classe", "N")
    tab$Pct <- round(100 * tab$N / sum(tab$N), 1)

    colors <- c("Normal" = "#4caf50", "MAS (Malnutrition Aiguë Sévère)" = "#d32f2f",
                "MAM (Malnutrition Aiguë Modérée)" = "#f57c00",
                "Retard sévère" = "#d32f2f", "Retard modéré" = "#f57c00",
                "Insuffisance sévère" = "#d32f2f", "Insuffisance modérée" = "#f57c00")
    cols <- ifelse(tab$Classe %in% names(colors), colors[tab$Classe], "#90a4ae")

    # Calculer le pourcentage critique (worst_label)
    pct_worst <- if (!is.null(worst_label)) {
      worst_rows <- tab[grepl(worst_label, tab$Classe, ignore.case = TRUE), ]
      sum(worst_rows$Pct)
    } else 0

    plot_ly(tab, labels = ~Classe, values = ~N, type = "pie",
            hole = 0.55,
            marker = list(colors = cols, line = list(color = "white", width = 2)),
            textinfo = "label+percent", textposition = "auto",
            hoverinfo = "label+value+percent",
            hovertemplate = "<b>%{label}</b><br>N=%{value}<br>%{percent}<extra></extra>") |>
      layout(
        title = list(text = paste0(titre, "<br><sub style='font-size:12px;'>n=", sum(tab$N), "</sub>"),
                     font = list(size = 13, color = "#0B3954")),
        showlegend = TRUE,
        legend = list(x = 0, y = -0.15, orientation = "h", font = list(size = 10)),
        margin = list(t = 80, b = 60, l = 20, r = 20),
        annotations = list(
          list(text = paste0(round(pct_worst, 1), "%<br><span style='font-size:11px;'>Critique</span>"),
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 20, color = if (pct_worst > 10) "#d32f2f" else "#0077B6"),
               xanchor = "center", yanchor = "middle")
        )
      )
  }

  output$zs_donut_gam <- renderPlotly({
    res <- zs_data(); req(res)
    zs_plot_donut(res$class_GAM, "Malnutrition Aiguë<br><sub>WHZ</sub>", "MAS") |> nutri_config()
  })
  output$zs_donut_retard <- renderPlotly({
    res <- zs_data(); req(res)
    zs_plot_donut(res$class_retard, "Retard de Croissance<br><sub>HAZ</sub>", "sévère")
  })
  output$zs_donut_insuf <- renderPlotly({
    res <- zs_data(); req(res)
    zs_plot_donut(res$class_insuf, "Insuffisance Pondérale<br><sub>WAZ</sub>", "sévère")
  })

  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  # STACKED BAR COMPARATIF — WHZ vs HAZ vs WAZ
  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  output$zs_stacked_bar <- renderPlotly({
    res <- zs_data(); req(res)
    
    # Créer la structure pour stacked bar
    bar_data <- tibble::tibble(
      Indicateur = c("WHZ", "HAZ", "WAZ"),
      `Normal` = c(
        round(100 * mean(res$WHZ_calc >= -2, na.rm = TRUE), 1),
        round(100 * mean(res$HAZ_calc >= -2, na.rm = TRUE), 1),
        round(100 * mean(res$WAZ_calc >= -2, na.rm = TRUE), 1)
      ),
      `Modéré (≤ -2)` = c(
        round(100 * mean(res$WHZ_calc < -2 & res$WHZ_calc >= -3, na.rm = TRUE), 1),
        round(100 * mean(res$HAZ_calc < -2 & res$HAZ_calc >= -3, na.rm = TRUE), 1),
        round(100 * mean(res$WAZ_calc < -2 & res$WAZ_calc >= -3, na.rm = TRUE), 1)
      ),
      `Sévère (< -3)` = c(
        round(100 * mean(res$WHZ_calc < -3, na.rm = TRUE), 1),
        round(100 * mean(res$HAZ_calc < -3, na.rm = TRUE), 1),
        round(100 * mean(res$WAZ_calc < -3, na.rm = TRUE), 1)
      )
    )

    p <- ggplot(bar_data |> pivot_longer(-Indicateur, names_to = "Classification", values_to = "Pct"),
                aes(x = Indicateur, y = Pct, fill = Classification)) +
      geom_bar(stat = "identity", width = 0.6, color = "white", linewidth = 1) +
      geom_text(aes(label = paste0(Pct, "%")), position = position_stack(vjust = 0.5),
                size = 3.5, fontface = "bold", color = "white") +
      scale_fill_manual(values = c("Normal" = "#4caf50", "Modéré (≤ -2)" = "#f57c00", "Sévère (< -3)" = "#d32f2f")) +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
      labs(title = "Comparaison des trois indicateurs nutritionnels",
           subtitle = "Distribution en trois catégories de malnutrition",
           x = "", y = "Prévalence (%)", fill = "Classification") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 13, color = "#0B3954", margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10, color = "#666", margin = margin(b = 12)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.margin = margin(b = 8)
      )

    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      layout(
        hovermode = "x unified",
        margin = list(t = 90, b = 50)
      ) |> nutri_config()
  })

  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  # COMPARAISON PAR SEXE — Box plots + Tableau
  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  output$zs_sex_whz <- renderPlotly({
    res <- zs_data(); req(res)
    col_s <- isolate(input$zs_col_sexe)
    if (!col_s %in% names(res)) return(plotly_empty())
    
    plot_data <- tibble::tibble(
      Z = res$WHZ_calc,
      Sexe = res[[col_s]]
    ) |> filter(!is.na(Z) & !is.na(Sexe))

    p <- ggplot(plot_data, aes(x = Sexe, y = Z, fill = Sexe)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = 1, outlier.size = 2) +
      geom_jitter(alpha = 0.2, width = 0.15, size = 1.5) +
      scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60")) +
      geom_hline(yintercept = c(-3, -2), linetype = "dashed", color = "#999", alpha = 0.5) +
      annotate("rect", ymin = -5, ymax = -3, xmin = -Inf, xmax = Inf, fill = "#d32f2f", alpha = 0.02) +
      annotate("rect", ymin = -3, ymax = -2, xmin = -Inf, xmax = Inf, fill = "#f57c00", alpha = 0.02) +
      labs(title = "WHZ — Poids-pour-Taille", x = "", y = "Z-score") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", plot.title = element_text(face = "bold", color = "#0B3954"))

    ggplotly(p, tooltip = c("x", "y")) |> layout(margin = list(t = 50)) |> nutri_config()
  })

  output$zs_sex_haz <- renderPlotly({
    res <- zs_data(); req(res)
    col_s <- isolate(input$zs_col_sexe)
    if (!col_s %in% names(res)) return(plotly_empty())
    
    plot_data <- tibble::tibble(
      Z = res$HAZ_calc,
      Sexe = res[[col_s]]
    ) |> filter(!is.na(Z) & !is.na(Sexe))

    p <- ggplot(plot_data, aes(x = Sexe, y = Z, fill = Sexe)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = 1, outlier.size = 2) +
      geom_jitter(alpha = 0.2, width = 0.15, size = 1.5) +
      scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60")) +
      geom_hline(yintercept = c(-3, -2), linetype = "dashed", color = "#999", alpha = 0.5) +
      annotate("rect", ymin = -5, ymax = -3, xmin = -Inf, xmax = Inf, fill = "#d32f2f", alpha = 0.02) +
      annotate("rect", ymin = -3, ymax = -2, xmin = -Inf, xmax = Inf, fill = "#f57c00", alpha = 0.02) +
      labs(title = "HAZ — Taille-pour-Âge", x = "", y = "Z-score") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", plot.title = element_text(face = "bold", color = "#0B3954"))

    ggplotly(p, tooltip = c("x", "y")) |> layout(margin = list(t = 50)) |> nutri_config()
  })

  output$zs_sex_waz <- renderPlotly({
    res <- zs_data(); req(res)
    col_s <- isolate(input$zs_col_sexe)
    if (!col_s %in% names(res)) return(plotly_empty())
    
    plot_data <- tibble::tibble(
      Z = res$WAZ_calc,
      Sexe = res[[col_s]]
    ) |> filter(!is.na(Z) & !is.na(Sexe))

    p <- ggplot(plot_data, aes(x = Sexe, y = Z, fill = Sexe)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = 1, outlier.size = 2) +
      geom_jitter(alpha = 0.2, width = 0.15, size = 1.5) +
      scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60")) +
      geom_hline(yintercept = c(-3, -2), linetype = "dashed", color = "#999", alpha = 0.5) +
      annotate("rect", ymin = -5, ymax = -3, xmin = -Inf, xmax = Inf, fill = "#d32f2f", alpha = 0.02) +
      annotate("rect", ymin = -3, ymax = -2, xmin = -Inf, xmax = Inf, fill = "#f57c00", alpha = 0.02) +
      labs(title = "WAZ — Poids-pour-Âge", x = "", y = "Z-score") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", plot.title = element_text(face = "bold", color = "#0B3954"))

    ggplotly(p, tooltip = c("x", "y")) |> layout(margin = list(t = 50)) |> nutri_config()
  })

  output$zs_sex_table <- renderDT({
    res <- zs_data(); req(res)
    col_s <- isolate(input$zs_col_sexe)
    if (!col_s %in% names(res)) return(datatable(tibble::tibble()))
    
    sex_summ <- res |> 
      mutate(Sexe = !!rlang::sym(col_s)) |>
      filter(!is.na(Sexe)) |>
      group_by(Sexe) |>
      summarise(
        N = n(),
        `WHZ moyen` = round(mean(WHZ_calc, na.rm = TRUE), 3),
        `GAM %` = round(100 * mean(WHZ_calc < -2, na.rm = TRUE), 1),
        `HAZ moyen` = round(mean(HAZ_calc, na.rm = TRUE), 3),
        `Retard %` = round(100 * mean(HAZ_calc < -2, na.rm = TRUE), 1),
        `WAZ moyen` = round(mean(WAZ_calc, na.rm = TRUE), 3),
        `Insuff. %` = round(100 * mean(WAZ_calc < -2, na.rm = TRUE), 1),
        .groups = "drop"
      )
    
    datatable(sex_summ, rownames = FALSE,
      options = list(dom = "t", pageLength = 5, scrollX = TRUE),
      class = "compact stripe hover") |>
      formatStyle(c("GAM %", "Retard %", "Insuff. %"),
        backgroundColor = styleInterval(c(5, 10), c("#e8f5e9", "#fff9c4", "#ffcdd2")))
  })

  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  # DASHBOARD QUALITÉ — Jauges + Résumé
  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# [DEAD CODE] zs_gauge_whz — output serveur sans UI correspondante
#   output$zs_gauge_whz <- renderPlotly({
#     res <- zs_data(); req(res)
#     n_valid <- sum(!is.na(res$WHZ_calc))
#     n_flags <- sum(res$flag_WHZ, na.rm = TRUE)
#     pct_qual <- if (n_valid > 0) round(100 * (1 - n_flags / n_valid), 1) else 100
# 
#     plot_ly(
#       type = "indicator",
#       mode = "gauge+number+delta",
#       value = pct_qual,
#       domain = list(x = c(0, 1), y = c(0, 1)),
#       title = list(text = "WHZ Qualité"),
#       gauge = list(
#         axis = list(range = list(0, 100)),
#         bar = list(color = if (pct_qual >= 95) "#4caf50" else if (pct_qual >= 90) "#fbc02d" else "#d32f2f"),
#         steps = list(
#           list(range = c(0, 50), color = "rgba(211, 47, 47, 0.1)"),
#           list(range = c(50, 90), color = "rgba(245, 127, 0, 0.1)"),
#           list(range = c(90, 100), color = "rgba(76, 175, 80, 0.1)")
#         ),
#         threshold = list(
#           line = list(color = "red", width = 2),
#           thickness = 0.75,
#           value = 80
#         )
#       )
#     ) |>
#       layout(margin = list(t = 80, b = 20, l = 60, r = 60), font = list(size = 11))
#   })

# [DEAD CODE] zs_gauge_haz — output serveur sans UI correspondante
#   output$zs_gauge_haz <- renderPlotly({
#     res <- zs_data(); req(res)
#     n_valid <- sum(!is.na(res$HAZ_calc))
#     n_flags <- sum(res$flag_HAZ, na.rm = TRUE)
#     pct_qual <- if (n_valid > 0) round(100 * (1 - n_flags / n_valid), 1) else 100
# 
#     plot_ly(
#       type = "indicator",
#       mode = "gauge+number+delta",
#       value = pct_qual,
#       domain = list(x = c(0, 1), y = c(0, 1)),
#       title = list(text = "HAZ Qualité"),
#       gauge = list(
#         axis = list(range = list(0, 100)),
#         bar = list(color = if (pct_qual >= 95) "#4caf50" else if (pct_qual >= 90) "#fbc02d" else "#d32f2f"),
#         steps = list(
#           list(range = c(0, 50), color = "rgba(211, 47, 47, 0.1)"),
#           list(range = c(50, 90), color = "rgba(245, 127, 0, 0.1)"),
#           list(range = c(90, 100), color = "rgba(76, 175, 80, 0.1)")
#         ),
#         threshold = list(
#           line = list(color = "red", width = 2),
#           thickness = 0.75,
#           value = 80
#         )
#       )
#     ) |>
#       layout(margin = list(t = 80, b = 20, l = 60, r = 60), font = list(size = 11))
#   })

# [DEAD CODE] zs_gauge_waz — output serveur sans UI correspondante
#   output$zs_gauge_waz <- renderPlotly({
#     res <- zs_data(); req(res)
#     n_valid <- sum(!is.na(res$WAZ_calc))
#     n_flags <- sum(res$flag_WAZ, na.rm = TRUE)
#     pct_qual <- if (n_valid > 0) round(100 * (1 - n_flags / n_valid), 1) else 100
# 
#     plot_ly(
#       type = "indicator",
#       mode = "gauge+number+delta",
#       value = pct_qual,
#       domain = list(x = c(0, 1), y = c(0, 1)),
#       title = list(text = "WAZ Qualité"),
#       gauge = list(
#         axis = list(range = list(0, 100)),
#         bar = list(color = if (pct_qual >= 95) "#4caf50" else if (pct_qual >= 90) "#fbc02d" else "#d32f2f"),
#         steps = list(
#           list(range = c(0, 50), color = "rgba(211, 47, 47, 0.1)"),
#           list(range = c(50, 90), color = "rgba(245, 127, 0, 0.1)"),
#           list(range = c(90, 100), color = "rgba(76, 175, 80, 0.1)")
#         ),
#         threshold = list(
#           line = list(color = "red", width = 2),
#           thickness = 0.75,
#           value = 80
#         )
#       )
#     ) |>
#       layout(margin = list(t = 80, b = 20, l = 60, r = 60), font = list(size = 11))
#   })

# [DEAD CODE] zs_quality_dashboard — output serveur sans UI correspondante
#   output$zs_quality_dashboard <- renderUI({
#     res <- zs_data(); req(res)
#     whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
#     haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
#     waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]
# 
#     sd_whz <- sd(whz)
#     sd_haz <- sd(haz)
#     sd_waz <- sd(waz)
# 
#     qual_score <- round(mean(c(
#       if (sd_whz > 0.8 && sd_whz < 1.2) 5 else if (sd_whz > 0.7 && sd_whz < 1.3) 3 else 1,
#       if (sd_haz > 0.8 && sd_haz < 1.2) 5 else if (sd_haz > 0.7 && sd_haz < 1.3) 3 else 1,
#       if (sd_waz > 0.8 && sd_waz < 1.2) 5 else if (sd_waz > 0.7 && sd_waz < 1.3) 3 else 1
#     )), 1)
# 
#     qual_label <- if (qual_score >= 4.5) "Excellent"
#                   else if (qual_score >= 3.5) "Bon"
#                   else if (qual_score >= 2.5) "Acceptable"
#                   else "Problématique"
# 
#     qual_color <- if (qual_score >= 4.5) "#4caf50"
#                   else if (qual_score >= 3.5) "#fbc02d"
#                   else if (qual_score >= 2.5) "#f57c00"
#                   else "#d32f2f"
# 
#     tags$div(style = paste0("background:white;border-radius:10px;padding:18px;border:1px solid #e8edf2;",
#                              "margin-bottom:16px;box-shadow:0 2px 8px rgba(0,0,0,.06);"),
#       tags$div(style = "display:flex;align-items:center;gap:14px;",
#         tags$div(style = paste0("width:80px;height:80px;border-radius:50%;background:", qual_color,
#                                  ";display:flex;align-items:center;justify-content:center;",
#                                  "font-weight:700;font-size:28px;color:white;"),
#           paste0(qual_score, "/5")
#         ),
#         tags$div(
#           tags$div(style = "font-size:13px;font-weight:700;color:#0B3954;",
#             "Score de qualité des données"),
#           tags$div(style = paste0("font-size:20px;font-weight:800;color:", qual_color, ";margin:4px 0;"),
#             qual_label),
#           tags$div(style = "font-size:11px;color:#666;margin-top:4px;",
#             "Évaluation basée sur les écarts-types observés vs attendus OMS N(0,1).")
#         )
#       )
#     )
#   })

  # Tableau classification
  output$zs_class_table <- renderDT({
    res <- zs_data(); req(res)

    make_cls <- function(col, label) {
      v <- res[[col]]
      v <- v[!is.na(v)]
      if (length(v) == 0) return(NULL)
      tab <- as.data.frame(table(v), stringsAsFactors = FALSE)
      names(tab) <- c("Classification", "N")
      tab$`%` <- round(100 * tab$N / sum(tab$N), 1)
      tab$Indicateur <- label
      tab[, c("Indicateur", "Classification", "N", "%")]
    }

    rows <- dplyr::bind_rows(
      make_cls("class_GAM", "WHZ — Malnutrition Aiguë"),
      make_cls("class_retard", "HAZ — Retard de Croissance"),
      make_cls("class_insuf", "WAZ — Insuffisance Pondérale")
    )
    req(nrow(rows) > 0)
    datatable(rows, rownames = FALSE,
      options = list(dom = "t", pageLength = 15, scrollX = TRUE),
      class = "compact stripe hover")
  })

  # Tableau données individuelles
  output$zs_data_table <- renderDT({
    res <- zs_data(); req(res)
    cols_show <- intersect(c(input$zs_col_sexe, input$zs_col_age, input$zs_col_poids,
                             input$zs_col_taille, "WHZ_calc", "HAZ_calc", "WAZ_calc",
                             "class_GAM", "class_retard", "class_insuf",
                             "flag_WHZ", "flag_HAZ", "flag_WAZ"), names(res))
    d_show <- res[, cols_show, drop = FALSE]
    # Arrondir les Z-scores
    for (zc in c("WHZ_calc", "HAZ_calc", "WAZ_calc")) {
      if (zc %in% names(d_show)) d_show[[zc]] <- round(d_show[[zc]], 2)
    }
    datatable(d_show, rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE),
      class = "compact stripe hover") |>
      formatStyle("WHZ_calc", color = styleInterval(c(-3, -2), c("#d32f2f", "#f57c00", "#333"))) |>
      formatStyle("HAZ_calc", color = styleInterval(c(-3, -2), c("#d32f2f", "#f57c00", "#333"))) |>
      formatStyle("WAZ_calc", color = styleInterval(c(-3, -2), c("#d32f2f", "#f57c00", "#333")))
  })

  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  # TÉLÉCHARGEMENTS — CSV et Excel
  # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  output$zs_dl_data <- downloadHandler(
    filename = function() paste0("zscores_oms_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- zs_data(); req(res)
      readr::write_csv(res, file)
    }
  )

  output$zs_dl_xlsx <- downloadHandler(
    filename = function() paste0("zscores_oms_", Sys.Date(), ".xlsx"),
    content = function(file) {
      res <- zs_data(); req(res)
      writexl::write_xlsx(
        list(
          "Z-scores" = res,
          "Résumé" = resume_zscores(res)
        ),
        file
      )
    }
  )

  # Flags summary
  output$zs_flags_summary <- renderUI({
    res <- zs_data(); req(res)
    n_flag_whz <- sum(res$flag_WHZ, na.rm = TRUE)
    n_flag_haz <- sum(res$flag_HAZ, na.rm = TRUE)
    n_flag_waz <- sum(res$flag_WAZ, na.rm = TRUE)
    total_flags <- n_flag_whz + n_flag_haz + n_flag_waz

    flag_color <- if (total_flags == 0) "#4caf50" else if (total_flags <= 5) "#f57c00" else "#d32f2f"

    tags$div(style = paste0("background:white;border-left:5px solid ", flag_color,
                             ";border-radius:8px;padding:14px;margin-bottom:12px;box-shadow:0 2px 4px rgba(0,0,0,.08);"),
      tags$div(style = "font-size:14px;font-weight:600;", paste("Total flags SMART :", total_flags)),
      tags$div(style = "font-size:12px;color:#666;margin-top:4px;",
        paste0("WHZ flags (|Z|>5) : ", n_flag_whz, " | ",
               "HAZ flags (|Z|>6) : ", n_flag_haz, " | ",
               "WAZ flags (Z<-6 ou Z>5) : ", n_flag_waz)),
      if (total_flags > 0)
        tags$div(style = "font-size:11px;color:#d32f2f;margin-top:6px;",
          icon("exclamation-triangle"), " Les observations flaggées peuvent indiquer des erreurs de mesure.")
    )
  })

  # Tableau flags
  output$zs_flags_table <- renderDT({
    res <- zs_data(); req(res)
    flagged <- res[res$flag_WHZ | res$flag_HAZ | res$flag_WAZ, , drop = FALSE]
    if (nrow(flagged) == 0) {
      return(datatable(tibble::tibble(Message = "Aucun flag détecté — toutes les valeurs sont dans les limites SMART."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    cols_show <- intersect(c(input$zs_col_sexe, input$zs_col_age, input$zs_col_poids,
                             input$zs_col_taille, "WHZ_calc", "HAZ_calc", "WAZ_calc",
                             "flag_WHZ", "flag_HAZ", "flag_WAZ"), names(flagged))
    d_show <- flagged[, cols_show, drop = FALSE]
    for (zc in c("WHZ_calc", "HAZ_calc", "WAZ_calc")) {
      if (zc %in% names(d_show)) d_show[[zc]] <- round(d_show[[zc]], 2)
    }
    datatable(d_show, rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE),
      class = "compact stripe hover")
  })

  # ══════════════════════════════════════════════════════════════════
  # MODULE MUAC — Périmètre Brachial (onglet Z-scores)
  # ══════════════════════════════════════════════════════════════════

  # Helper : extraire le vecteur MUAC (colonne .muac ou auto-détection)
  .muac_vec_r <- reactive({
    d_cur <- df_f(); req(d_cur)
    mc <- intersect(c(".muac", "muac", "MUAC", "pb", "PB", "mid_upper_arm_circ",
                      "muac_mm", "pbm"), names(d_cur))
    if (length(mc) == 0) return(NULL)
    as.numeric(d_cur[[mc[1]]])
  })

  output$zs_muac_group_ui <- renderUI({
    d_cur <- df_f(); req(d_cur)
    choices_grp <- c("Global" = "", "Région" = "region",
                     "Département" = "departement",
                     "Round" = "survey_round", "Sexe" = "sexe",
                     "Cluster" = "cluster")
    valid_choices <- choices_grp[c(TRUE, sapply(choices_grp[-1], function(v) v %in% names(d_cur)))]
    selectInput("zs_muac_group_var", "Grouper par :",
                choices = valid_choices, selected = "", width = "100%")
  })

  output$zs_muac_kpi_row <- renderUI({
    muac_v <- .muac_vec_r(); req(muac_v)
    muac_v <- muac_v[!is.na(muac_v)]
    req(length(muac_v) > 0)
    n_tot    <- length(muac_v)
    n_mag    <- sum(muac_v < 125)
    n_mas    <- sum(muac_v < 115)
    n_mam    <- sum(muac_v >= 115 & muac_v < 125)
    pct_mag  <- round(n_mag / n_tot * 100, 1)
    pct_mas  <- round(n_mas / n_tot * 100, 1)
    pct_mam  <- round(n_mam / n_tot * 100, 1)
    ci_res   <- tryCatch(binom::binom.wilson(n_mag, n_tot), error = function(e) NULL)
    ci_lo    <- if (!is.null(ci_res)) round(ci_res$lower * 100, 1) else "?"
    ci_hi    <- if (!is.null(ci_res)) round(ci_res$upper * 100, 1) else "?"

    sev_col  <- if (pct_mag >= 15) "#C0392B" else if (pct_mag >= 10) "#E67E22" else if (pct_mag >= 5) "#F1C40F" else "#27AE60"
    sev_lbl  <- if (pct_mag >= 15) "Critique" else if (pct_mag >= 10) "Sérieux" else if (pct_mag >= 5) "Alerte" else "Acceptable"

    make_muac_box <- function(val, lbl, sub_lbl, color) {
      tags$div(style=paste0("background:white;border-radius:10px;padding:14px 16px;",
                             "border-left:4px solid ", color, ";text-align:center;flex:1;"),
        tags$div(style=paste0("font-size:26px;font-weight:800;color:", color, ";"), val),
        tags$div(style="font-size:12px;font-weight:600;color:#333;", lbl),
        tags$div(style="font-size:10px;color:#888;margin-top:2px;", sub_lbl)
      )
    }
    tags$div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
      make_muac_box(paste0(pct_mag, "%"), "MAG (MUAC < 125mm)",
        paste0("IC 95% : [", ci_lo, "–", ci_hi, "%] | n=", n_mag, "/", n_tot), sev_col),
      make_muac_box(paste0(pct_mas, "%"), "MAS (MUAC < 115mm)",
        paste0("Malnutrition aiguë sévère | n=", n_mas), "#C0392B"),
      make_muac_box(paste0(pct_mam, "%"), "MAM (115–124mm)",
        paste0("Malnutrition aiguë modérée | n=", n_mam), "#E67E22"),
      make_muac_box(paste0(round(mean(muac_v), 1), "mm"), "MUAC moyen",
        paste0("Médiane : ", median(muac_v), "mm | SD : ", round(sd(muac_v), 1), "mm"), "#0077B6"),
      tags$div(style=paste0("background:white;border-radius:10px;padding:14px 16px;",
                             "border-left:4px solid ", sev_col, ";text-align:center;flex:1;"),
        tags$div(style=paste0("font-size:14px;font-weight:800;color:", sev_col, ";"), sev_lbl),
        tags$div(style="font-size:11px;font-weight:600;color:#333;", "Niveau MAG"),
        tags$div(style="font-size:10px;color:#888;",
                 if (pct_mag >= 15) "≥15% — Urgence nutritionnelle" else
                 if (pct_mag >= 10) "10–14% — Situation sérieuse" else
                 if (pct_mag >= 5)  "5–9% — Seuil d'alerte" else "< 5% — Acceptable")
      )
    )
  })

  output$zs_muac_dist <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    d_cur  <- df_f(); req(d_cur)
    grp_var <- if (!is.null(input$zs_muac_by_sex) && isTRUE(input$zs_muac_by_sex)) {
      sex_col <- intersect(c("sexe", "sex", "genre"), names(d_cur))
      if (length(sex_col) > 0) d_cur[[sex_col[1]]] else NULL
    } else NULL
    tryCatch(
      plot_muac_distribution(muac_v, group_var = grp_var),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_muac_donut <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    muac_v_clean <- muac_v[!is.na(muac_v)]
    req(length(muac_v_clean) > 0)
    counts <- c(
      sum(muac_v_clean < 115),
      sum(muac_v_clean >= 115 & muac_v_clean < 125),
      sum(muac_v_clean >= 125 & muac_v_clean < 135),
      sum(muac_v_clean >= 135)
    )
    labels <- MUAC_CATEGORIES$cat
    pcts   <- round(counts / sum(counts) * 100, 1)
    plot_ly(labels = labels, values = counts, type = "pie",
            hole = 0.5, textinfo = "percent",
            marker = list(colors = MUAC_CATEGORIES$couleur,
                          line = list(color = "white", width = 1.5)),
            hovertemplate = "<b>%{label}</b><br>%{value} enfants (%{percent})<extra></extra>") |>
      layout(
        title = list(text = "<b>Classification MUAC</b>",
                     font = list(size = 13, color = "#0B3954"), x = 0.5),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2, font = list(size = 10)),
        paper_bgcolor = "white"
      )
  })

  output$zs_muac_by_group <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    d_cur  <- df_f(); req(d_cur)
    grp_var_name <- if (!is.null(input$zs_muac_group_var) && input$zs_muac_group_var != "")
      input$zs_muac_group_var else NULL
    req(grp_var_name, grp_var_name %in% names(d_cur))
    grp_vec <- d_cur[[grp_var_name]]
    tryCatch(
      plot_muac_by_group(muac_v, grp_vec, grp_var_name),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_muac_vs_whz_panel <- renderUI({
    res <- zs_data()
    if (is.null(res) || !"WHZ_calc" %in% names(res))
      return(tags$div(style="background:#f5f5f5;border-radius:8px;padding:12px;font-size:11px;color:#888;",
        icon("info-circle"), " Calculez les Z-scores pour afficher la comparaison MUAC vs WHZ."))
    tagList(
      tags$div(class="section-header", "Comparaison MUAC vs WHZ (méthodes de mesure MAG)"),
      plotlyOutput("zs_muac_vs_whz_plot", height="350px")
    )
  })

  output$zs_muac_vs_whz_plot <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    res    <- zs_data(); req(res, "WHZ_calc" %in% names(res))
    d_cur  <- df_f(); req(d_cur)
    sex_col <- intersect(c("sexe", "sex", "genre"), names(d_cur))
    sex_vec <- if (length(sex_col) > 0) d_cur[[sex_col[1]]] else NULL
    tryCatch(
      plot_muac_vs_whz(muac_v, res$WHZ_calc, sex_vec),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_muac_table <- renderDT({
    muac_v <- .muac_vec_r(); req(muac_v)
    d_cur  <- df_f(); req(d_cur)
    grp_var_name <- if (!is.null(input$zs_muac_group_var) && input$zs_muac_group_var != "")
      input$zs_muac_group_var else NULL
    grp_vec <- if (!is.null(grp_var_name) && grp_var_name %in% names(d_cur)) d_cur[[grp_var_name]] else NULL
    summ <- tryCatch(
      calc_muac_summary(muac_v, grp_vec),
      error = function(e) NULL
    )
    req(summ)
    datatable(
      summ |> rename(Groupe = groupe, N_total = N,
                     `Moy (mm)` = Moy_MUAC, `Méd (mm)` = Med_MUAC, `SD` = SD_MUAC,
                     `n MAS` = N_MAS, `n MAM` = N_MAM, `n MAG` = N_MAG,
                     `% MAS` = Pct_MAS, `% MAM` = Pct_MAM, `% MAG` = Pct_MAG,
                     `IC lo MAG` = MAG_CI_lo, `IC hi MAG` = MAG_CI_hi),
      rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15),
      class = "compact stripe hover"
    ) |>
      formatStyle("% MAG",
        background = styleColorBar(c(0, 50), "#E67E22"),
        backgroundSize = "100% 75%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
      )
  })

  # ══════════════════════════════════════════════════════════════════
  # MODULE Z-SCORES PAR TRANCHE D'ÂGE
  # ══════════════════════════════════════════════════════════════════

  output$zs_age_boxplot <- renderPlotly({
    res <- zs_data(); req(res)
    ind <- if (!is.null(input$zs_age_indicator)) input$zs_age_indicator else "WHZ_calc"
    req(ind %in% names(res))
    tryCatch(
      plot_zscore_by_age_group(res, ind),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_age_table <- renderDT({
    res <- zs_data(); req(res)
    tbl <- tryCatch(calc_zscore_by_age_group(res), error = function(e) NULL)
    req(tbl)
    datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
              class = "compact stripe")
  })

  # ── INDICATEURS OMS ─────────────────────────────────────────────────────────
  output$oms_table <- renderDT({
    d <- df_f(); req(d); m <- map_cols()
    inds_avail <- intersect(c("GAM","SAM","MAM","Stunting","Sev_Stunting",
                               "Underweight","MUAC_GAM","MUAC_SAM"), names(d))
    labels_oms <- c(
      GAM="GAM (WHZ<-2|œdème)", SAM="SAM (WHZ<-3|œdème)",
      MAM="MAM (-3≤WHZ<-2)", Stunting="Stunting (HAZ<-2)",
      Sev_Stunting="Stunting sévère (HAZ<-3)",
      Underweight="Insuff. pondérale (WAZ<-2)",
      MUAC_GAM="MUAC GAM (<125mm)", MUAC_SAM="MUAC SAM (<115mm)"
    )
    groupes <- list()
    if("Global"%in%input$oms_groupes) groupes[["Global"]]=list(var=NULL)
    if("Région"%in%input$oms_groupes&&!is.null(m$region)&&m$region%in%names(d))
      groupes[["Région"]]=list(var=m$region)
    if("Sexe"%in%input$oms_groupes&&!is.null(m$sexe)&&m$sexe%in%names(d))
      groupes[["Sexe"]]=list(var=m$sexe)
    if("Groupe d'âge"%in%input$oms_groupes&&"age_group5"%in%names(d))
      groupes[["Groupe d'âge"]]=list(var="age_group5")
    if("Déplacement"%in%input$oms_groupes&&!is.null(m$depl)&&m$depl%in%names(d))
      groupes[["Déplacement"]]=list(var=m$depl)
    if("FCS"%in%input$oms_groupes&&"fcs_cat"%in%names(d))
      groupes[["Sécurité alim."]]=list(var="fcs_cat")

    map_dfr(names(groupes),function(gn){
      g <- groupes[[gn]]
      if(is.null(g$var)){
        map_dfr(inds_avail,function(v) calc_prev_wilson(d,v,labels_oms[v])) |>
          mutate(Stratification="Global",Groupe="—",.before=1)
      } else {
        d |> group_by(Groupe=.data[[g$var]]) |>
          group_modify(~map_dfr(inds_avail,function(v) calc_prev_wilson(.x,v,labels_oms[v]))) |>
          ungroup() |> mutate(Stratification=gn,.before=1)
      }
    }) |>
      datatable(rownames=FALSE,filter="top",
                options=list(pageLength=15,dom="ftp",scrollX=TRUE),
                class="compact stripe hover") |>
      formatStyle("Prév. (%)",
                  background=styleColorBar(c(0,100),C$bleu),
                  backgroundSize="100% 70%") |>
      formatStyle("Prév. (%)",
                  color=styleInterval(c(5,10,15,30),
                    c(C$vert,C$orange,C$rouge,C$rouge_f,C$rouge_f)),
                  fontWeight="bold") |>
      formatStyle("Sévérité",fontWeight="bold")
  })

  output$dl_oms_csv <- downloadHandler(
    filename=function() paste0("indicateurs_oms_",Sys.Date(),".csv"),
    content=function(f) {
      d <- df_f(); m <- map_cols()
      inds <- intersect(c("GAM","SAM","Stunting","Underweight"),names(d))
      res  <- map_dfr(inds, function(v) calc_prev_wilson(d,v,v))
      write.csv(res, f, row.names=FALSE)
    }
  )

  output$oms_forest <- renderPlotly({
    d <- df_f(); m <- map_cols(); req(d)
    inds <- intersect(c("GAM","SAM","Stunting","Underweight","MUAC_GAM"),names(d))
    req(length(inds)>0)
    grp <- if(!is.null(m$region)&&m$region%in%names(d)) m$region else vars_cat()[1]
    req(!is.null(grp))
    res <- d |> group_by(.data[[grp]]) |>
      group_modify(~map_dfr(inds,function(v) calc_prev_wilson(.x,v,v))) |>
      ungroup()
    nk <- length(unique(na.omit(res$Indicateur)))
    p <- ggplot(res,aes(x=`Prév. (%)`,
                        y=fct_reorder(paste0(.data[[grp]]," — ",Indicateur),`Prév. (%)`),
                        color=Indicateur,
                        text=paste0(.data[[grp]]," — ",Indicateur,
                                    "\n",`Prév. (%)`,"% [",`IC95 bas (%)`,"-",`IC95 haut (%)`,"%]")))+
      geom_vline(xintercept=c(5,10,15),linetype="dashed",
                 color=c(C$jaune,C$orange,C$rouge),linewidth=.6,alpha=.7)+
      geom_errorbarh(aes(xmin=`IC95 bas (%)`,xmax=`IC95 haut (%)`),
                     height=.4,linewidth=.8)+
      geom_point(size=3.5)+
      scale_color_manual(values=gpal(nk,"cat"))+
      labs(x="Prévalence (%)",y="")+theme_minimal(base_size=10)
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.15)) |> nutri_config()
  })

  output$oms_gauge_gam <- renderPlotly({
    req(has_oms()); d <- filter_context()$data
    gam_p <- round(mean(d$GAM,na.rm=TRUE)*100,1)
    plot_ly(type="indicator",mode="gauge+number",value=gam_p,
            title=list(text="GAM (%)",font=list(size=13)),
            gauge=list(axis=list(range=list(0,50)),
                       bar=list(color=couleur_prevalence(gam_p)),
                       steps=list(list(range=c(0,5),color="#e8f5e9"),
                                  list(range=c(5,10),color="#fffde7"),
                                  list(range=c(10,15),color="#fff3e0"),
                                  list(range=c(15,50),color="#ffebee")),
                       threshold=list(line=list(color=C$rouge,width=3),
                                       thickness=.75,value=15)))|>
      layout(margin=list(l=15,r=15,b=5,t=35),paper_bgcolor="rgba(0,0,0,0)") |> nutri_config()
  })

  output$oms_gauge_stunt <- renderPlotly({
    req("Stunting"%in%names(df_f())); d <- df_f()
    p <- round(mean(d$Stunting,na.rm=TRUE)*100,1)
    plot_ly(type="indicator",mode="gauge+number",value=p,
            title=list(text="Stunting (%)",font=list(size=13)),
            gauge=list(axis=list(range=list(0,70)),
                       bar=list(color=couleur_prevalence(p)),
                       steps=list(list(range=c(0,20),color="#e8f5e9"),
                                  list(range=c(20,30),color="#fffde7"),
                                  list(range=c(30,40),color="#fff3e0"),
                                  list(range=c(40,70),color="#ffebee")),
                       threshold=list(line=list(color=C$rouge,width=3),
                                       thickness=.75,value=30)))|>
      layout(margin=list(l=15,r=15,b=5,t=35),paper_bgcolor="rgba(0,0,0,0)") |> nutri_config()
  })

  output$oms_zscore_dist <- renderPlotly({
    d <- df_f(); m <- map_cols()
    z_cols <- c(m$whz,m$haz,m$waz)
    z_cols <- z_cols[!sapply(z_cols,is.null)&z_cols%in%names(d)]
    req(length(z_cols)>0)
    x_ref <- seq(-4.5,4.5,length.out=300)
    ref_df <- tibble(x=x_ref,y=dnorm(x_ref))
    df_long <- d|>select(all_of(z_cols))|>
      pivot_longer(everything(),names_to="Score",values_to="Val")
    p <- ggplot(df_long,aes(x=Val,fill=Score,color=Score))+
      geom_density(alpha=.28,linewidth=.8)+
      geom_line(data=ref_df,aes(x=x,y=y),color="black",linewidth=1,
                linetype="dashed",inherit.aes=FALSE)+
      geom_vline(xintercept=c(-2,-3),linetype="dashed",
                 color=c(C$rouge,C$rouge_f),linewidth=.7)+
      scale_fill_manual(values=gpal(length(z_cols),"cat"))+
      scale_color_manual(values=gpal(length(z_cols),"cat"))+
      labs(x="Score Z",y="Densité",
           caption="Tiret noir = OMS N(0,1) | Tirets rouges = seuils -2 DS et -3 DS")+
      theme_minimal(base_size=11)
    ggplotly(p)|>layout(hovermode="x unified",legend=list(orientation="h",y=-0.2)) |> nutri_config()
  })

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

  # ════════════════════════════════════════════════════════════════════
  # SERVER — MODULE DPLYR
  # ════════════════════════════════════════════════════════════════════
  
  # Données de travail dplyr (séparée de df_f pour ne pas perturber les autres onglets)
  dplyr_data <- reactiveVal(NULL)
  
  observe({
    df <- df_f(); req(df)
    dplyr_data(df)
  })

  # ── UI filtres dplyr ───────────────────────────────────────────────
  output$dplyr_filter_ui <- renderUI({
    df <- df_f(); req(df)
    vc <- names(df)[vapply(names(df), function(n) {
      x <- df[[n]]; !startsWith(n,".") &&
      (is.factor(x)||is.character(x)) && length(unique(na.omit(x)))<=30
    }, logical(1))]
    vn <- vars_num()
    tagList(
      selectInput("dplyr_f_var","Variable :",choices=c(vn,vc),width="100%"),
      uiOutput("dplyr_f_val_ui")
    )
  })
  
  output$dplyr_f_val_ui <- renderUI({
    req(input$dplyr_f_var); df <- df_f()
    v <- input$dplyr_f_var; x <- df[[v]]
    if (is.numeric(x)) {
      r <- range(x, na.rm=TRUE)
      sliderInput("dplyr_f_range","Plage :",min=r[1],max=r[2],value=r,width="100%")
    } else {
      vals <- sort(unique(na.omit(as.character(x))))
      checkboxGroupInput("dplyr_f_vals","Valeurs :",choices=vals,selected=vals)
    }
  })
  
  output$dplyr_select_ui <- renderUI({
    df <- df_f(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_sel_cols","Colonnes à garder :",
                   choices=vis,selected=head(vis,8),multiple=TRUE,width="100%")
  })
  
  output$dplyr_arrange_ui <- renderUI({
    vn <- vars_num(); vc <- vars_cat()
    selectInput("dplyr_arr_col","Trier par :",choices=c(vn,vc),width="100%")
  })
  
  output$dplyr_group_ui <- renderUI({
    vc <- vars_cat()
    selectizeInput("dplyr_grp_cols","Grouper par :",
                   choices=vc,selected=vc[1],multiple=TRUE,width="100%")
  })
  
  output$dplyr_summarise_ui <- renderUI({
    vn <- vars_num()
    tagList(
      checkboxGroupInput("dplyr_sum_fns","Fonctions :",
        choices=c("n (effectif)"="n","mean (moyenne)"="mean","median (médiane)"="median",
                  "sd (écart-type)"="sd","min"="min","max"="max","sum (somme)"="sum"),
        selected=c("n","mean","median"),inline=FALSE),
      selectizeInput("dplyr_sum_vars","Sur les variables :",
                     choices=vn,selected=head(vn,3),multiple=TRUE,width="100%")
    )
  })
  
  # ── Applications des opérations ────────────────────────────────────
  observeEvent(input$dplyr_apply_filter, {
    df <- df_f(); req(df, input$dplyr_f_var)
    v  <- input$dplyr_f_var; x <- df[[v]]
    tryCatch({
      result <- if (is.numeric(x) && !is.null(input$dplyr_f_range)) {
        filter(df, .data[[v]] >= input$dplyr_f_range[1] &
                   .data[[v]] <= input$dplyr_f_range[2])
      } else if (!is.null(input$dplyr_f_vals)) {
        filter(df, as.character(.data[[v]]) %in% input$dplyr_f_vals)
      } else df
      dplyr_data(result)
      showNotification(paste0("✅ Filtre appliqué : ",nrow(result)," lignes"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_select, {
    req(input$dplyr_sel_cols)
    tryCatch({
      result <- df_f() |> select(any_of(input$dplyr_sel_cols))
      dplyr_data(result)
      showNotification(paste0("✅ ",ncol(result)," colonnes sélectionnées"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_mutate, {
    req(input$dplyr_new_col, input$dplyr_expr)
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- safe_mutate(df, input$dplyr_new_col, input$dplyr_expr)
      dplyr_data(result)
      showNotification(paste0("✅ Colonne '",input$dplyr_new_col,"' créée"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌ Erreur expression:",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_arrange, {
    req(input$dplyr_arr_col)
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- if (input$dplyr_order == "desc")
        arrange(df, desc(.data[[input$dplyr_arr_col]]))
      else
        arrange(df, .data[[input$dplyr_arr_col]])
      dplyr_data(result)
      showNotification(paste0("✅ Trié par ",input$dplyr_arr_col," (",input$dplyr_order,")"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_summarise, {
    req(input$dplyr_grp_cols, input$dplyr_sum_fns, input$dplyr_sum_vars)
    tryCatch({
      df <- dplyr_data(); req(df)
      fns_list <- list()
      if ("n" %in% input$dplyr_sum_fns)
        fns_list$N <- ~n()
      for (fn in setdiff(input$dplyr_sum_fns,"n")) {
        for (v in input$dplyr_sum_vars) {
          nm <- paste0(fn,"_",v)
          fns_list[[nm]] <- eval(parse(text=paste0("~",fn,"(.data[['",v,"']],na.rm=TRUE)")))
        }
      }
      result <- df |>
        group_by(across(all_of(input$dplyr_grp_cols))) |>
        summarise(!!!fns_list, .groups="drop") |>
        mutate(across(where(is.numeric), ~round(.,3)))
      dplyr_data(result)
      showNotification(paste0("✅ Résumé : ",nrow(result)," groupes"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_run_code, {
    req(input$dplyr_code)
    tryCatch({
      env <- new.env(parent=globalenv())
      env$df_f <- df_f
      result <- eval(parse(text=input$dplyr_code), envir=env)
      if (is.data.frame(result)) {
        dplyr_data(result)
        showNotification(paste0("✅ Code exécuté : ",nrow(result)," lignes"),
                         type="message",duration=3)
      }
    }, error=function(e) showNotification(paste("❌ Erreur R:",e$message),type="error"))
  })
  
  output$dplyr_code_result <- renderPrint({
    req(input$dplyr_code)
    tryCatch({
      env <- new.env(parent=globalenv())
      env$df_f <- df_f
      result <- eval(parse(text=input$dplyr_code), envir=env)
      if (is.data.frame(result)) {
        cat(paste0("Résultat : ",nrow(result)," lignes × ",ncol(result)," colonnes\n"))
        print(head(result, 5))
      } else print(result)
    }, error=function(e) cat("Erreur:", e$message))
  })
  
  # ══════════════════════════════════════════════════════════════════
  # NOUVEAUX VERBES DPLYR — UI dynamiques
  # ══════════════════════════════════════════════════════════════════

  # ── RENOMMER ──────────────────────────────────────────────────────
  output$dplyr_rename_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    tagList(
      selectizeInput("dplyr_ren_old","Colonne à renommer :",
                     choices=vis, selected=vis[1], width="100%"),
      textInput("dplyr_ren_new","Nouveau nom :", value="", width="100%")
    )
  })
  observeEvent(input$dplyr_apply_rename, {
    req(input$dplyr_ren_old, input$dplyr_ren_new)
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- rename(df, !!input$dplyr_ren_new := !!sym(input$dplyr_ren_old))
      dplyr_data(result)
      showNotification(paste0("✅ '",input$dplyr_ren_old,"' → '",input$dplyr_ren_new,"'"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── DÉDOUBLONNER ──────────────────────────────────────────────────
  output$dplyr_distinct_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_dist_cols","Colonnes clés (vide = toutes) :",
                   choices=vis, selected=NULL, multiple=TRUE, width="100%",
                   options=list(placeholder="Toutes les colonnes"))
  })
  observeEvent(input$dplyr_apply_distinct, {
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- if (length(input$dplyr_dist_cols) > 0)
        distinct(df, across(all_of(input$dplyr_dist_cols)), .keep_all=isTRUE(input$dplyr_keep_all))
      else
        distinct(df)
      n_supp <- nrow(df) - nrow(result)
      dplyr_data(result)
      showNotification(paste0("✅ distinct() : ",n_supp," doublon(s) supprimé(s) → ",
                              nrow(result)," lignes"), type="message", duration=4)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── DÉCOUPER (SLICE) ──────────────────────────────────────────────
  output$dplyr_slice_col_ui <- renderUI({
    req(input$dplyr_slice_type %in% c("min","max"))
    vn <- names(dplyr_data())[vapply(dplyr_data(), is.numeric, logical(1))]
    selectInput("dplyr_slice_col","Variable pour min/max :", choices=vn, width="100%")
  })
  observeEvent(input$dplyr_apply_slice, {
    req(input$dplyr_slice_n)
    tryCatch({
      df  <- dplyr_data(); req(df)
      n   <- as.integer(input$dplyr_slice_n)
      wt  <- isTRUE(input$dplyr_slice_with_ties)
      result <- switch(input$dplyr_slice_type,
        head   = slice_head(df, n=n),
        tail   = slice_tail(df, n=n),
        sample = slice_sample(df, n=min(n, nrow(df))),
        min    = { req(input$dplyr_slice_col)
                   slice_min(df, order_by=.data[[input$dplyr_slice_col]], n=n, with_ties=wt) },
        max    = { req(input$dplyr_slice_col)
                   slice_max(df, order_by=.data[[input$dplyr_slice_col]], n=n, with_ties=wt) }
      )
      dplyr_data(result)
      showNotification(paste0("✅ slice_",input$dplyr_slice_type,"(n=",n,") → ",
                              nrow(result)," lignes"), type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── COMPTER ───────────────────────────────────────────────────────
  output$dplyr_count_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vc <- names(df)[vapply(df, function(x) is.character(x)||is.factor(x)||is.logical(x), logical(1))]
    vc <- vc[!startsWith(vc,".")]
    selectizeInput("dplyr_cnt_cols","Grouper par :", choices=vc, multiple=TRUE,
                   selected=vc[1], width="100%")
  })
  output$dplyr_count_wt_ui <- renderUI({
    vn <- names(dplyr_data())[vapply(dplyr_data(), is.numeric, logical(1))]
    vn <- vn[!startsWith(vn,".")]
    selectInput("dplyr_cnt_wt","Pondération wt (optionnel) :",
                choices=c("Aucune"="",vn), selected="", width="100%")
  })
  observeEvent(input$dplyr_apply_count, {
    req(input$dplyr_cnt_cols)
    tryCatch({
      df <- dplyr_data(); req(df)
      wt_var <- if (!is.null(input$dplyr_cnt_wt) && input$dplyr_cnt_wt != "")
        sym(input$dplyr_cnt_wt) else NULL
      result <- if (input$dplyr_count_mode == "count") {
        if (!is.null(wt_var))
          count(df, across(all_of(input$dplyr_cnt_cols)), wt=!!wt_var,
                sort=isTRUE(input$dplyr_count_sort))
        else
          count(df, across(all_of(input$dplyr_cnt_cols)),
                sort=isTRUE(input$dplyr_count_sort))
      } else {
        if (!is.null(wt_var))
          add_count(df, across(all_of(input$dplyr_cnt_cols)), wt=!!wt_var)
        else
          add_count(df, across(all_of(input$dplyr_cnt_cols)))
      }
      dplyr_data(result)
      showNotification(paste0("✅ ",input$dplyr_count_mode,"() → ",nrow(result)," lignes"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── PIVOTER ───────────────────────────────────────────────────────
  output$dplyr_pivot_names_from_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vc <- names(df)[!startsWith(names(df),".")]
    selectInput("dplyr_piv_nf","names_from (catégories → colonnes) :",
                choices=vc, width="100%")
  })
  output$dplyr_pivot_values_from_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df, is.numeric, logical(1)) & !startsWith(names(df),".")]
    selectInput("dplyr_piv_vf","values_from (valeurs) :",
                choices=vn, selected=vn[1], width="100%")
  })
  output$dplyr_pivot_longer_cols_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_piv_long_cols","Colonnes à pivoter en long :",
                   choices=vis, multiple=TRUE, selected=head(vis,3), width="100%")
  })
  observeEvent(input$dplyr_apply_pivot, {
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- if (input$dplyr_pivot_dir == "wider") {
        req(input$dplyr_piv_nf, input$dplyr_piv_vf)
        pivot_wider(df, names_from=all_of(input$dplyr_piv_nf),
                    values_from=all_of(input$dplyr_piv_vf),
                    values_fn=mean)
      } else {
        req(input$dplyr_piv_long_cols)
        pivot_longer(df, cols=all_of(input$dplyr_piv_long_cols),
                     names_to=input$dplyr_pivot_names_to,
                     values_to=input$dplyr_pivot_values_to)
      }
      dplyr_data(result)
      showNotification(paste0("✅ pivot_",input$dplyr_pivot_dir,"() → ",
                              nrow(result),"×",ncol(result)), type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── TRANSMUTER ────────────────────────────────────────────────────
  transmute_rows <- reactiveVal(1L)
  observeEvent(input$dplyr_add_transmute_row, {
    transmute_rows(transmute_rows() + 1L)
  })
  output$dplyr_transmute_ui <- renderUI({
    n <- transmute_rows()
    lapply(seq_len(n), function(i) {
      fluidRow(
        column(5, textInput(paste0("dplyr_tr_col_",i),
               if(i==1)"Nom colonne :" else NULL, value="", width="100%")),
        column(7, textInput(paste0("dplyr_tr_expr_",i),
               if(i==1)"Expression R :" else NULL,
               value="", width="100%"))
      )
    })
  })
  observeEvent(input$dplyr_apply_transmute, {
    tryCatch({
      df <- dplyr_data(); req(df)
      n   <- transmute_rows()
      lst <- list()
      for (i in seq_len(n)) {
        col_nm  <- input[[paste0("dplyr_tr_col_",i)]]
        col_ex  <- input[[paste0("dplyr_tr_expr_",i)]]
        if (!is.null(col_nm) && nchar(trimws(col_nm)) > 0 &&
            !is.null(col_ex)  && nchar(trimws(col_ex))  > 0)
          lst[[col_nm]] <- eval(parse(text=col_ex), envir=as.list(df))
      }
      req(length(lst) > 0)
      result <- do.call(transmute, c(list(df), lst))
      dplyr_data(result)
      showNotification(paste0("✅ transmute() : ",ncol(result)," nouvelle(s) colonne(s)"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── GÉRER NA ──────────────────────────────────────────────────────
  output$dplyr_na_cols_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_na_cols","Colonnes concernées (vide = toutes) :",
                   choices=vis, multiple=TRUE, selected=NULL, width="100%",
                   options=list(placeholder="Toutes les colonnes"))
  })
  observeEvent(input$dplyr_apply_na, {
    tryCatch({
      df   <- dplyr_data(); req(df)
      cols <- if (length(input$dplyr_na_cols) > 0) input$dplyr_na_cols else names(df)
      result <- switch(input$dplyr_na_op,
        drop = {
          drop_na(df, any_of(cols))
        },
        replace = {
          val <- tryCatch(as.numeric(input$dplyr_na_val), warning=function(w) input$dplyr_na_val)
          rpl <- setNames(rep(list(val), length(cols)), cols)
          replace_na(df, rpl)
        },
        fill = {
          dir <- if (input$dplyr_fill_dir == "up") "up" else "down"
          fill(df, any_of(cols), .direction=dir)
        }
      )
      n_avant <- sum(is.na(select(df, any_of(cols))))
      n_apres <- sum(is.na(select(result, any_of(cols))))
      dplyr_data(result)
      showNotification(paste0("✅ ",input$dplyr_na_op,"() : ",
                              n_avant - n_apres," NA traité(s)"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── ACROSS ────────────────────────────────────────────────────────
  output$dplyr_across_cols_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df, is.numeric, logical(1)) & !startsWith(names(df),".")]
    selectizeInput("dplyr_ac_cols","Colonnes numériques :",
                   choices=vn, multiple=TRUE, selected=head(vn,3), width="100%")
  })
  output$dplyr_across_grp_ui <- renderUI({
    req(input$dplyr_across_verb == "summarise")
    df <- dplyr_data(); req(df)
    vc <- names(df)[vapply(df,function(x) is.character(x)||is.factor(x)||is.logical(x), logical(1))]
    vc <- vc[!startsWith(vc,".")]
    selectizeInput("dplyr_ac_grp","Grouper par (pour summarise) :",
                   choices=vc, multiple=TRUE, selected=vc[1], width="100%")
  })
  observeEvent(input$dplyr_apply_across, {
    req(input$dplyr_ac_cols, input$dplyr_across_fn)
    tryCatch({
      df   <- dplyr_data(); req(df)
      fn   <- input$dplyr_across_fn
      cols <- input$dplyr_ac_cols
      fn_expr <- if (startsWith(fn, "~")) eval(parse(text=fn)) else match.fun(fn)
      result <- if (input$dplyr_across_verb == "mutate") {
        mutate(df, across(all_of(cols), fn_expr,
                          .names="{.col}_{gsub('[^a-zA-Z0-9]','',fn)}"))
      } else {
        grps <- if (!is.null(input$dplyr_ac_grp) && length(input$dplyr_ac_grp)>0)
          input$dplyr_ac_grp else character(0)
        df |>
          group_by(across(all_of(grps))) |>
          summarise(across(all_of(cols), fn_expr, na.rm=TRUE,
                           .names="{.col}_{gsub('[^a-zA-Z0-9]','',fn)}"),
                    .groups="drop")
      }
      dplyr_data(result)
      showNotification(paste0("✅ across(",length(cols)," colonnes, ",fn,")"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── Affichage résultat dplyr ───────────────────────────────────────
  output$dplyr_result_tbl <- renderDT({
    df <- dplyr_data(); req(df)
    df |> select(!starts_with(".")) |>
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15,scrollX=TRUE,dom="frtip"),
                class="compact stripe hover")
  })
  
  output$dplyr_nrow_ui <- renderUI({
    df <- dplyr_data(); req(df)
    orig <- nrow(df_f())
    curr <- nrow(df)
    tags$div(style="font-size:13px;color:#555;padding:4px 0;",
      tags$b(style="color:#0077B6;font-size:18px;",format(curr,big.mark=" ")),
      " lignes × ",
      tags$b(ncol(df))," colonnes",
      if(curr < orig)
        tags$span(style="color:#D98B14;margin-left:8px;font-size:12px;",
                  paste0("(filtré depuis ",format(orig,big.mark=" ")," lignes)"))
    )
  })
  
  output$dplyr_stats_quick_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df,is.numeric,logical(1)) & !startsWith(names(df),".")]
    if(length(vn)==0) return(NULL)
    tags$div(
      tags$div(class="section-header","Statistiques rapides du résultat"),
      fluidRow(
        lapply(head(vn,4), function(v) {
          x <- na.omit(df[[v]])
          if(length(x)==0) return(NULL)
          column(3, tags$div(
            style="background:white;border:0.5px solid #e0e0e0;border-radius:8px;
                   padding:10px;text-align:center;margin:4px;",
            tags$div(style="font-size:11px;color:#888;text-transform:uppercase;",v),
            tags$div(style="font-size:18px;font-weight:700;color:#0077B6;",round(mean(x),2)),
            tags$div(style="font-size:11px;color:#555;",
                     paste0("σ=",round(sd(x),2)," | Méd=",round(median(x),2)))
          ))
        })
      )
    )
  })
  
  output$dplyr_dl <- downloadHandler(
    filename=function() paste0("manipulation_dplyr_",Sys.Date(),".csv"),
    content=function(f) {
      df <- dplyr_data(); req(df)
      write.csv(df |> select(!starts_with(".")), f, row.names=FALSE)
    }
  )
  
  # ── Visualisation rapide dplyr ─────────────────────────────────────
  output$dplyr_plot_x_ui <- renderUI({
    df <- dplyr_data(); req(df)
    all_v <- names(df)[!startsWith(names(df),".")]
    selectInput("dplyr_px","Variable X :",choices=all_v,selected=all_v[1],width="100%")
  })
  output$dplyr_plot_y_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df,is.numeric,logical(1)) & !startsWith(names(df),".")]
    if(length(vn)==0) return(NULL)
    selectInput("dplyr_py","Variable Y (numérique) :",choices=vn,
                selected=vn[1],width="100%")
  })
  
  output$dplyr_plot_out <- renderPlotly({
    req(input$dplyr_plot_btn, input$dplyr_px)
    df <- dplyr_data(); req(df)
    x <- input$dplyr_px; y <- input$dplyr_py
    type <- input$dplyr_plot_type
    tryCatch({
      p <- if(type=="bar") {
        cnt <- df |> count(.data[[x]]) |> mutate(pct=round(n/sum(n)*100,1))
        cnt <- smart_top_n(cnt, x, "n", 25)
        cnt[[x]] <- truncate_label(cnt[[x]], 22)
        ggplot(cnt, aes(y=fct_reorder(.data[[x]],n), x=n, fill=.data[[x]],
          text=paste0(.data[[x]],"\nN=",n," (",pct,"%)"))) +
          geom_col(alpha=.88) +
          scale_fill_manual(values=gpal(nrow(cnt),"cat")) +
          labs(y="",x="N") + theme_nutri() + theme(legend.position="none")
      } else if(type=="hist") {
        req(y)
        ggplot(df,aes(x=.data[[y]]))+
          geom_histogram(fill=gprimary(),color="white",bins=28,alpha=.85)+
          geom_vline(xintercept=mean(df[[y]],na.rm=TRUE),color="#BF3636",linewidth=1,linetype="dashed")+
          labs(x=y,y="N")+theme_nutri()
      } else if(type=="box") {
        req(y)
        n_grp <- length(unique(na.omit(df[[x]])))
        df[[x]] <- truncate_label(df[[x]], 20)
        if (n_grp > 10) {
          ggplot(df,aes(y=.data[[x]],x=.data[[y]],fill=.data[[x]]))+
            geom_boxplot(alpha=.75,outlier.shape=21)+
            scale_fill_manual(values=gpal(n_grp,"cat"))+
            labs(y=x,x=y)+theme_nutri()+theme(legend.position="none")
        } else {
          ggplot(df,aes(x=.data[[x]],y=.data[[y]],fill=.data[[x]]))+
            geom_boxplot(alpha=.75,outlier.shape=21)+
            scale_fill_manual(values=gpal(n_grp,"cat"))+
            labs(x=x,y=y)+theme_nutri()+smart_axis_x(n_grp)+
            theme(legend.position="none")
        }
      } else if(type=="scatter") {
        req(y)
        ggplot(df,aes(x=.data[[x]],y=.data[[y]]))+
          geom_point(alpha=.5,color=gprimary(),size=2)+
          geom_smooth(method="loess",se=TRUE,color="#BF3636",linewidth=.9)+
          labs(x=x,y=y)+theme_nutri()
      } else {
        req(y)
        ggplot(df,aes(x=.data[[y]]))+
          geom_density(fill=gprimary(),alpha=.4,color=gprimary())+
          geom_vline(xintercept=mean(df[[y]],na.rm=TRUE),color="#BF3636",linewidth=1,linetype="dashed")+
          labs(x=y,y="Densité")+theme_nutri()
      }
      ggplotly(p,tooltip="text") |> layout(hovermode="closest") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })



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

}

shinyApp(ui=ui, server=server)
