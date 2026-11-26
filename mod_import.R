# Module Shiny — Import & Mappeur intelligent (Kobo / ODK / conformité OMS)
# UI namespacée + serveur moduleServer.
# Le store racine (raw_data, mapped_data, map_cols, df_f, …) reste dans
# server_import.R : df_f dépend des 24 filtres globaux f_* de la sidebar.

# Extrait de app_parts/ui/ui_body.R — onglet import
mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "import",
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("magic"), " Import & Mappeur intelligent"),
              tabsetPanel(id = ns("import_tabs"),

                # ── Onglet 1 : Import fichier local (existant) ──
                tabPanel(tags$span(icon("file-upload"), " Fichier local"),
                  tags$p(style="font-size:13px;color:#555;margin:12px 0;",
                    tags$span(class="badge-ok","🟢 Détecté"),
                    " automatiquement |",
                    tags$span(class="badge-warn","🟡 À vérifier"),
                    " |",
                    tags$span(class="badge-err","🔴 Non trouvé"),
                    " — cliquez sur une ligne pour corriger"
                  ),
                  fluidRow(
                    column(6, uiOutput(ns("mapper_col1"))),
                    column(6, uiOutput(ns("mapper_col2")))
                  ),
                  # ── Vérification avec le dictionnaire OMS ──
                  tags$hr(style="margin:18px 0 10px;"),
                  tags$div(
                    style="display:flex;align-items:center;gap:10px;margin-bottom:10px;",
                    tags$div(
                      style="width:40px;height:40px;border-radius:10px;
                             background:linear-gradient(135deg,#0077B6,#00838F);
                             display:flex;align-items:center;justify-content:center;",
                      icon("book-open", style="color:white;font-size:18px;")
                    ),
                    tags$div(
                      tags$h5(style="margin:0;color:#1e293b;font-weight:700;font-size:14px;",
                              "Vérification avec le dictionnaire OMS"),
                      tags$p(style="margin:0;color:#64748b;font-size:12px;",
                             "Comparez vos colonnes avec les alias reconnus pour vérifier et corriger les équivalences.")
                    )
                  ),
                  DTOutput(ns("dict_verification_table"))
                ),

                # ── Onglet 2 : KoboToolbox API ──
                tabPanel(tags$span(icon("cloud-download-alt"), " KoboToolbox / ODK"),
                  tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin:12px 0;font-size:12px;",
                    tags$b("KoboToolbox / ODK Connect"), " — Connectez-vous directement à votre compte KoboToolbox ",
                    "pour importer les données d'enquête. Compatible avec les serveurs OCHA (humanitarianresponse.info) ",
                    "et KoboToolbox.org. ",
                    tags$a(href="https://support.kobotoolbox.org/api.html", target="_blank",
                           style="color:#0077B6;", "Comment obtenir votre token API ?")
                  ),
                  fluidRow(
                    column(4,
                      tags$div(style="padding:8px;",
                        tags$div(class="section-header", "Connexion"),
                        selectInput(ns("kobo_server"), "Serveur :",
                                    choices = KOBO_SERVERS, selected = KOBO_SERVERS[1], width = "100%"),
                        conditionalPanel(ns = ns, "input.kobo_server == 'custom'",
                          textInput(ns("kobo_custom_url"), "URL personnalisée :",
                                    placeholder = "https://votre-serveur.com", width = "100%")
                        ),
                        passwordInput(ns("kobo_token"), "Token API :",
                                      placeholder = "Collez votre token ici", width = "100%"),
                        fluidRow(
                          column(6,
                            actionButton(ns("kobo_test"), "🔌 Tester",
                                         class="btn btn-info btn-block", width="100%",
                                         style="font-size:12px;")
                          ),
                          column(6,
                            actionButton(ns("kobo_list_forms"), "📋 Lister les formulaires",
                                         class="btn btn-primary btn-block", width="100%",
                                         style="font-size:12px;")
                          )
                        ),
                        uiOutput(ns("kobo_connection_status")),
                        hr(),
                        tags$div(class="section-header", "Formulaire sélectionné"),
                        uiOutput(ns("kobo_form_select_ui")),
                        actionButton(ns("kobo_download"), "📥 Télécharger les données",
                                     class="btn btn-oms btn-block", width="100%",
                                     style="font-size:13px;padding:8px;margin-top:8px;"),
                        hr(),
                        uiOutput(ns("kobo_download_status"))
                      )
                    ),
                    column(8,
                      tags$div(style="padding:8px;",
                        tags$div(class="section-header", "Formulaires disponibles"),
                        uiOutput(ns("kobo_forms_list")),
                        hr(),
                        tags$div(class="section-header", "Mapping automatique ODK → SMART"),
                        uiOutput(ns("kobo_mapping_html")),
                        hr(),
                        tags$div(class="section-header", "Aperçu des données"),
                        DTOutput(ns("kobo_preview_table"))
                      )
                    )
                  )
                ),

                # ── Onglet 3 : Import fichier ODK/Kobo export ──
                tabPanel(tags$span(icon("file-csv"), " Fichier ODK Export"),
                  tags$div(style="background:#f0f8ff;border-left:4px solid #27ae60;padding:10px 14px;border-radius:0 8px 8px 0;margin:12px 0;font-size:12px;",
                    tags$b("Import d'un export KoboToolbox/ODK"), " — Importez un fichier CSV ou Excel ",
                    "exporté depuis KoboToolbox. Les colonnes système seront automatiquement nettoyées ",
                    "et les variables SMART détectées."
                  ),
                  fluidRow(
                    column(4,
                      fileInput(ns("kobo_file_upload"), "Fichier d'export Kobo (CSV/Excel) :",
                                accept = c(".csv", ".xlsx", ".xls"),
                                buttonLabel = "Choisir", placeholder = "Aucun fichier",
                                width = "100%"),
                      actionButton(ns("kobo_file_import"), "📥 Importer et nettoyer",
                                   class="btn btn-oms btn-block", width="100%",
                                   style="font-size:13px;padding:8px;"),
                      hr(),
                      uiOutput(ns("kobo_file_status")),
                      uiOutput(ns("kobo_file_mapping"))
                    ),
                    column(8,
                      DTOutput(ns("kobo_file_preview"))
                    )
                  )
                ),

                # ── Onglet 4 : Vérification conformité OMS ──
                tabPanel(tags$span(icon("check-double"), " Conformité OMS"),
                  value = "oms_conform",
                  tags$div(
                    style = "background:linear-gradient(135deg,#f0fff4 0%,#e8f8f0 100%);
                             border-left:4px solid #00838F;padding:14px 18px;
                             border-radius:0 8px 8px 0;margin:12px 0;",
                    tags$div(style="display:flex;align-items:center;gap:12px;",
                      tags$div(
                        style = "width:48px;height:48px;border-radius:12px;
                                 background:linear-gradient(135deg,#00838F,#0B3954);
                                 display:flex;align-items:center;justify-content:center;",
                        icon("stethoscope", style="color:white;font-size:22px;")
                      ),
                      tags$div(
                        tags$h4(style="margin:0;color:#1e293b;font-weight:700;font-size:16px;",
                                "Vérification de Conformité OMS / DHIS2"),
                        tags$p(style="margin:2px 0 0;color:#64748b;font-size:12px;",
                               "Analyse automatique de chaque colonne : détection des variables ",
                               "standards OMS, UNICEF, PAM, SMART/ENA et DHIS2. ",
                               "Corrigez les correspondances manuellement si nécessaire.")
                      )
                    )
                  ),
                  # Score global
                  uiOutput(ns("oms_conform_score")),
                  # Tableau de conformité
                  tags$div(style="margin-top:12px;",
                    DTOutput(ns("oms_conform_table"))
                  ),
                  # Corrections manuelles
                  fluidRow(style="margin-top:16px;",
                    column(4,
                      selectInput(ns("oms_fix_col"), "Colonne à corriger :",
                                  choices = NULL, width = "100%")
                    ),
                    column(4,
                      selectInput(ns("oms_fix_var"), "Assigner à variable OMS :",
                                  choices = NULL, width = "100%")
                    ),
                    column(4,
                      tags$div(style="padding-top:25px;",
                        actionButton(ns("oms_fix_apply"),
                                     tags$span(icon("link"), " Assigner"),
                                     class="btn btn-oms",
                                     style="padding:8px 20px;font-size:13px;"),
                        actionButton(ns("oms_fix_reset"),
                                     tags$span(icon("undo"), " Réinitialiser"),
                                     class="btn btn-default",
                                     style="padding:8px 16px;font-size:13px;margin-left:8px;")
                      )
                    )
                  ),
                  # ── Panneau détail dictionnaire (apparaît au clic sur une ligne) ──
                  uiOutput(ns("dict_selected_aliases"))
                )
              )
          )
        ),
        fluidRow(
          box(width=8, status="info", solidHeader=TRUE,
              title="Aperçu des données",
              DTOutput(ns("import_preview"))),
          box(width=4, status="success", solidHeader=TRUE,
              title="Résumé de l'import",
              uiOutput(ns("import_summary")),
              br(),
              actionButton(ns("apply_map"), "✅ Appliquer le mappeur",
                           class="btn btn-oms btn-block", width="100%")
          )
        )
      )
}

mod_import_server <- function(id, raw_data, mapped_data, map_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      selectInput(ns("kobo_form_uid"), "Formulaire :", choices = choices, width = "100%")
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
          selectInput(ns(paste0("map_", key)), NULL,
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
  })
}
