# =============================================================================
# mod_redcap.R — Module REDCap API
# NutriAnalyse Pro v2 | YIMGA WANDJI KEVIN | DSAMID
# REDCap REST API v13+ — Records, Instruments, Data Dictionary, Events
# =============================================================================

# ── Packages requis : httr2 (ou httr), jsonlite, DT ─────────────────────────

# Helper : appel REDCap API (POST)
.redcap_api <- function(url, token, content, extra=list(), timeout=60) {
  params <- c(list(token=token, content=content, format="json", returnFormat="json"), extra)
  if (!requireNamespace("httr2", quietly=TRUE)) {
    r <- httr::POST(url, body=params, encode="form", httr::timeout(timeout))
    httr::stop_for_status(r)
    txt <- httr::content(r, "text", encoding="UTF-8")
  } else {
    r <- httr2::request(url) |>
      httr2::req_body_form(!!!params) |>
      httr2::req_timeout(timeout) |>
      httr2::req_headers(Accept="application/json") |>
      httr2::req_perform()
    httr2::resp_check_status(r)
    txt <- httr2::resp_body_string(r)
  }
  # Détecter erreur REDCap dans le JSON
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector=TRUE), error=function(e) txt)
  if (is.data.frame(parsed) || is.list(parsed)) {
    if (!is.null(parsed$error)) stop(parsed$error)
  }
  parsed
}

# =============================================================================
# UI
# =============================================================================
redcapUI <- function(id, lang="fr") {
  ns <- NS(id)
  l  <- lang

  tagList(
    tags$head(
      tags$style(HTML("
        .rc-header  { background:linear-gradient(90deg,#c00000,#8b0000); color:#fff; padding:18px 22px; border-radius:10px; margin-bottom:20px; }
        .rc-card     { padding:12px; border-radius:6px; border-left:4px solid #c00000; background:#fff5f5; margin-bottom:8px; }
        .rc-connected{ border-left-color:#28a745; background:#f0fff4; }
        .rc-instr-item { padding:8px 10px; border-bottom:1px solid #f0f0f0; cursor:pointer; }
        .rc-instr-item:hover { background:#fff5f5; }
        .rc-instr-item.selected { background:#ffe8e8; font-weight:600; }
        .rc-badge { display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; font-weight:600; }
        .rc-badge-form   { background:#c0000022; color:#c00000; }
        .rc-badge-survey { background:#28a74522; color:#28a745; }
      "))
    ),

    tags$div(class="rc-header",
      tags$h3(style="margin:0; font-weight:700;",
        icon("hospital"), " ", tr("rc_title", l)),
      tags$p(style="margin:4px 0 0; opacity:.85; font-size:13px;", tr("rc_subtitle", l))
    ),

    tabsetPanel(id=ns("rc_tabs"),

      # ── Onglet 1 : Connexion & Projet ──────────────────────────────────────
      tabPanel(tr("rc_tab_conn", l), value="conn", icon=icon("plug"),
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("plug"), " ", tr("rc_conn_title", l)),
                status="danger", solidHeader=TRUE, width=12,

              textInput(ns("rc_url"), tr("rc_url_label", l),
                        placeholder="https://redcap.institution.org/api/"),
              passwordInput(ns("rc_token"), tr("rc_token_label", l),
                            placeholder="32-caractères token API"),
              tags$small(class="text-muted", tr("rc_token_hint", l)),
              br(), br(),
              fluidRow(
                column(6, actionButton(ns("btn_rc_connect"), tr("rc_connect", l),
                                       icon=icon("link"), class="btn-danger btn-block")),
                column(6, actionButton(ns("btn_rc_disc"), tr("odk_disconnect", l),
                                       icon=icon("unlink"), class="btn-secondary btn-block"))
              ),
              br(),
              uiOutput(ns("rc_status_ui"))
            ),

            box(title=tags$span(icon("info-circle"), " ", tr("rc_project_info", l)),
                status="info", solidHeader=TRUE, width=12, collapsible=TRUE,
              uiOutput(ns("rc_project_info_ui"))
            )
          ),

          column(8,
            box(title=tags$span(icon("list"), " ", tr("rc_instruments_title", l)),
                status="primary", solidHeader=TRUE, width=12,

              fluidRow(
                column(8, textInput(ns("rc_instr_filter"), tr("rc_filter_instruments", l), placeholder="Filtre...")),
                column(4, br(), actionButton(ns("btn_rc_load_instr"), icon("sync"), class="btn-sm btn-info"))
              ),
              br(),
              uiOutput(ns("rc_instruments_ui")),
              br(),
              uiOutput(ns("rc_events_ui"))
            )
          )
        )
      ),

      # ── Onglet 2 : Export des Données ──────────────────────────────────────
      tabPanel(tr("rc_tab_export", l), value="export", icon=icon("download"),
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("sliders-h"), " ", tr("rc_export_params", l)),
                status="danger", solidHeader=TRUE, width=12,

              # Sélection instruments
              uiOutput(ns("rc_instr_select_ui")),
              hr(),

              # Sélection events (longitudinal)
              uiOutput(ns("rc_event_select_ui")),
              hr(),

              # Champs
              radioButtons(ns("rc_fields_type"), tr("rc_fields_type", l),
                choices=setNames(c("all", "select"),
                                c(tr("rc_all_fields", l), tr("rc_sel_fields", l))),
                inline=TRUE),
              uiOutput(ns("rc_fields_select_ui")),
              hr(),

              # Filtres
              textInput(ns("rc_record_ids"), tr("rc_record_ids", l),
                        placeholder="1,2,3 ou vide pour tout"),
              fluidRow(
                column(6, textInput(ns("rc_date_begin"), tr("rc_date_begin", l),
                                    value=format(Sys.Date()-365, "%Y-%m-%d"))),
                column(6, textInput(ns("rc_date_end"),   tr("rc_date_end", l),
                                    value=format(Sys.Date(), "%Y-%m-%d")))
              ),
              selectInput(ns("rc_date_field"), tr("rc_date_field", l),
                choices=c("record_date_range"="record_date_range",
                          "created_after"="created_after",
                          "modified_after"="modified_after")),
              hr(),

              checkboxInput(ns("rc_exportsurvey"), tr("rc_survey_fields", l), FALSE),
              checkboxInput(ns("rc_labels"),        tr("rc_labels", l),        TRUE),
              checkboxInput(ns("rc_header_labels"), tr("rc_header_labels", l), FALSE),

              actionButton(ns("btn_rc_export"), tr("rc_export_btn", l),
                           icon=icon("download"), class="btn-danger btn-block btn-lg")
            )
          ),

          column(8,
            box(title=tags$span(icon("table"), " ", tr("rc_results", l)),
                status="success", solidHeader=TRUE, width=12,

              fluidRow(
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("rc_n_records"))), tags$small(tr("rc_records", l)))),
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("rc_n_fields"))),  tags$small(tr("rc_fields", l)))),
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("rc_n_complete"))), tags$small(tr("rc_complete", l)))),
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("rc_n_missing"))),  tags$small(tr("rc_missing", l))))
              ),
              br(),
              uiOutput(ns("rc_export_msg_ui")),
              br(),
              fluidRow(
                column(4, downloadButton(ns("rc_dl_csv"),   "CSV",   class="btn-sm btn-outline-success")),
                column(4, downloadButton(ns("rc_dl_excel"), "Excel", class="btn-sm btn-outline-primary")),
                column(4, actionButton(ns("btn_rc_import"), tr("db_import_app", l),
                                       icon=icon("arrow-right"), class="btn-sm btn-warning"))
              ),
              br(),
              DT::dataTableOutput(ns("rc_data_table"))
            )
          )
        )
      ),

      # ── Onglet 3 : Dictionnaire des données ────────────────────────────────
      tabPanel(tr("rc_tab_dict", l), value="dict", icon=icon("book"),
        br(),
        fluidRow(
          column(3,
            box(title=tr("rc_dict_options", l), status="info", solidHeader=TRUE, width=12,
              actionButton(ns("btn_rc_load_dict"), tr("rc_load_dict", l),
                           icon=icon("sync"), class="btn-info btn-block"),
              br(), br(),
              textInput(ns("dict_filter"), tr("rc_dict_filter", l), placeholder="Recherche champ..."),
              selectInput(ns("dict_type_filter"), tr("rc_dict_type", l),
                choices=setNames(c("", "text","number","date","dropdown","checkbox","radio","yesno","calc","file","notes"),
                                c(tr("rc_all", l), "text","number","date","dropdown","checkbox","radio","yesno","calc","file","notes"))),
              downloadButton(ns("rc_dl_dict"), "CSV", class="btn-sm btn-outline-success mt-2")
            )
          ),
          column(9,
            box(title=tags$span(icon("list"), " ", tr("rc_dict_title", l)),
                status="primary", solidHeader=TRUE, width=12,
              uiOutput(ns("rc_dict_summary_ui")),
              br(),
              DT::dataTableOutput(ns("rc_dict_table"))
            )
          )
        )
      ),

      # ── Onglet 4 : Import dans REDCap ──────────────────────────────────────
      tabPanel(tr("rc_tab_import", l), value="import_rc", icon=icon("upload"),
        br(),
        fluidRow(
          column(5,
            box(title=tags$span(icon("upload"), " ", tr("rc_import_title", l)),
                status="warning", solidHeader=TRUE, width=12,

              tags$div(class="alert alert-warning",
                icon("exclamation-triangle"), " ", tr("rc_import_warning", l)
              ),
              uiOutput(ns("rc_import_preview_ui")),
              textInput(ns("rc_import_form"), tr("rc_import_form_label", l), placeholder="instrument_name"),
              checkboxInput(ns("rc_import_overwrite"), tr("rc_import_overwrite", l), FALSE),
              actionButton(ns("btn_rc_write"), tr("rc_import_btn", l),
                           icon=icon("upload"), class="btn-warning btn-block"),
              br(),
              uiOutput(ns("rc_import_status_ui"))
            )
          ),
          column(7,
            box(title=tags$span(icon("info-circle"), " ", tr("rc_import_guide", l)),
                status="info", solidHeader=TRUE, width=12,
              tags$h5(tr("rc_import_requirements", l)),
              tags$ul(
                tags$li(tr("rc_req1", l)),
                tags$li(tr("rc_req2", l)),
                tags$li(tr("rc_req3", l)),
                tags$li(tr("rc_req4", l))
              ),
              tags$h5(tr("rc_import_fields_mapping", l)),
              tags$p(tr("rc_mapping_hint", l)),
              DT::dataTableOutput(ns("rc_import_preview_table"))
            )
          )
        )
      ),

      # ── Onglet 5 : Logs & Audit ─────────────────────────────────────────────
      tabPanel(tr("rc_tab_logs", l), value="logs", icon=icon("history"),
        br(),
        fluidRow(
          column(3,
            box(title=tr("rc_logs_params", l), status="info", solidHeader=TRUE, width=12,
              textInput(ns("log_user"), tr("rc_log_user", l), placeholder="username (optionnel)"),
              textInput(ns("log_date_begin"), tr("rc_date_begin", l), value=format(Sys.Date()-30, "%Y-%m-%d")),
              textInput(ns("log_date_end"),   tr("rc_date_end", l),   value=format(Sys.Date(), "%Y-%m-%d")),
              numericInput(ns("log_limit"), tr("rc_log_limit", l), value=1000, min=1, max=10000),
              actionButton(ns("btn_rc_load_logs"), tr("rc_load_logs", l),
                           icon=icon("sync"), class="btn-secondary btn-block"),
              br(),
              downloadButton(ns("rc_dl_logs"), "CSV", class="btn-sm btn-outline-success mt-1")
            )
          ),
          column(9,
            box(title=tags$span(icon("history"), " ", tr("rc_logs_title", l)),
                status="info", solidHeader=TRUE, width=12,
              DT::dataTableOutput(ns("rc_logs_table"))
            )
          )
        )
      )
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================
redcapServer <- function(id, mapped_data=reactive(NULL),
                          set_mapped_data=NULL, lang=reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    l  <- lang

    # ── State ────────────────────────────────────────────────────────────────
    rc_creds      <- reactiveVal(NULL)   # list(url, token)
    rc_project    <- reactiveVal(NULL)   # project info
    rc_instruments<- reactiveVal(NULL)   # list of instruments
    rc_events     <- reactiveVal(NULL)   # longitudinal events
    rc_fields     <- reactiveVal(NULL)   # data dictionary (fields)
    rc_data       <- reactiveVal(NULL)   # exported records
    rc_logs_data  <- reactiveVal(NULL)

    # ── Connexion ──────────────────────────────────────────────────────────
    observeEvent(input$btn_rc_connect, {
      url   <- trimws(input$rc_url)
      token <- trimws(input$rc_token)
      if (!nzchar(url) || !nzchar(token)) {
        showNotification(tr("rc_credentials_required", l()), type="warning"); return()
      }
      if (!grepl("^https?://", url)) url <- paste0("https://", url)
      if (!grepl("/api/?$", url)) url <- paste0(sub("/+$","",url), "/api/")

      withProgress(message=tr("rc_connecting", l()), value=0.5, {
        tryCatch({
          info <- .redcap_api(url, token, "project")
          rc_creds(list(url=url, token=token))
          rc_project(info)
          showNotification(paste0(tr("rc_connected_ok", l()), " — ", info$project_title %||% "REDCap"),
                           type="message", duration=4)
          # Charger instruments et events en arrière plan
          tryCatch({
            instr <- .redcap_api(url, token, "instrument")
            rc_instruments(instr)
            # Essayer les events (seulement longitudinal)
            tryCatch({
              evts <- .redcap_api(url, token, "event")
              rc_events(evts)
            }, error=function(e) rc_events(NULL))
          }, error=function(e) NULL)
        }, error=function(e) {
          rc_creds(NULL); rc_project(NULL)
          showNotification(paste0(tr("rc_conn_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    observeEvent(input$btn_rc_disc, {
      rc_creds(NULL); rc_project(NULL); rc_instruments(NULL)
      rc_events(NULL); rc_fields(NULL); rc_data(NULL)
      showNotification(tr("odk_disconnected", l()), type="warning", duration=3)
    })

    # ── Status UI ─────────────────────────────────────────────────────────
    output$rc_status_ui <- renderUI({
      cr <- rc_creds()
      if (!is.null(cr))
        tags$div(class="rc-card rc-connected", icon("check-circle", class="text-success"), " ",
                 tags$b(tr("rc_connected_ok", l())), tags$br(),
                 tags$small(class="text-muted", cr$url))
      else
        tags$div(class="rc-card", icon("times-circle", class="text-muted"), " ",
                 tags$span(class="text-muted", tr("rc_not_connected", l())))
    })

    output$rc_project_info_ui <- renderUI({
      info <- rc_project()
      if (is.null(info)) return(tags$p(class="text-muted", tr("rc_no_info", l())))
      tagList(
        tags$table(class="table table-sm",
          tags$tr(tags$td(tags$b(tr("rc_project_title", l()))),  tags$td(info$project_title %||% "—")),
          tags$tr(tags$td(tags$b(tr("rc_project_id", l()))),     tags$td(info$project_id %||% "—")),
          tags$tr(tags$td(tags$b(tr("rc_longitudinal", l()))),   tags$td(as.character(isTRUE(info$is_longitudinal==1)))),
          tags$tr(tags$td(tags$b(tr("rc_surveys", l()))),        tags$td(as.character(isTRUE(info$surveys_enabled==1)))),
          tags$tr(tags$td(tags$b(tr("rc_purpose", l()))),        tags$td(info$purpose_other %||% info$purpose %||% "—")),
          tags$tr(tags$td(tags$b(tr("rc_created", l()))),        tags$td(info$creation_time %||% "—"))
        )
      )
    })

    # ── Instruments ────────────────────────────────────────────────────────
    observeEvent(input$btn_rc_load_instr, {
      cr <- rc_creds(); req(!is.null(cr))
      tryCatch({
        instr <- .redcap_api(cr$url, cr$token, "instrument")
        rc_instruments(instr)
      }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
    })

    output$rc_instruments_ui <- renderUI({
      instr <- rc_instruments()
      flt   <- tolower(trimws(input$rc_instr_filter %||% ""))
      if (is.null(instr) || (is.data.frame(instr) && nrow(instr)==0) || length(instr)==0)
        return(tags$p(class="text-muted", tr("rc_no_instruments", l())))
      if (!is.data.frame(instr)) instr <- as.data.frame(do.call(rbind, instr), stringsAsFactors=FALSE)
      if (nzchar(flt)) instr <- instr[grepl(flt, tolower(instr$instrument_label %||% ""), fixed=TRUE), , drop=FALSE]
      tagList(
        tags$p(tags$b(tr("rc_instruments_title", l()), " (", nrow(instr), ")")),
        tags$div(style="max-height:250px; overflow-y:auto;",
          apply(instr, 1, function(r) {
            tags$div(class="rc-instr-item",
              icon("wpforms"), " ",
              tags$b(r["instrument_label"] %||% r["instrument_name"]),
              tags$small(class="text-muted ml-2", r["instrument_name"])
            )
          })
        )
      )
    })

    output$rc_events_ui <- renderUI({
      evts <- rc_events()
      if (is.null(evts)) return(NULL)
      if (!is.data.frame(evts)) evts <- as.data.frame(do.call(rbind, evts), stringsAsFactors=FALSE)
      tags$div(
        tags$p(tags$b(tr("rc_events_label", l()), " (", nrow(evts), ")")),
        tags$small(class="text-muted",
          paste(evts$event_name %||% evts$unique_event_name, collapse=", "))
      )
    })

    # ── Sélecteurs dynamiques pour l'export ────────────────────────────────
    output$rc_instr_select_ui <- renderUI({
      instr <- rc_instruments()
      if (is.null(instr) || (is.data.frame(instr) && nrow(instr)==0)) return(NULL)
      if (!is.data.frame(instr)) instr <- as.data.frame(do.call(rbind, instr), stringsAsFactors=FALSE)
      nms <- instr$instrument_label %||% instr$instrument_name
      ids <- instr$instrument_name
      selectInput(ns("rc_export_forms"), tr("rc_select_instruments", l),
                  choices=setNames(ids, nms), multiple=TRUE,
                  selected=NULL)
    })

    output$rc_event_select_ui <- renderUI({
      evts <- rc_events()
      if (is.null(evts)) return(NULL)
      if (!is.data.frame(evts)) evts <- as.data.frame(do.call(rbind, evts), stringsAsFactors=FALSE)
      nms <- evts$event_name %||% evts$unique_event_name
      ids <- evts$unique_event_name
      selectInput(ns("rc_export_events"), tr("rc_select_events", l),
                  choices=setNames(ids, nms), multiple=TRUE, selected=NULL)
    })

    output$rc_fields_select_ui <- renderUI({
      req(input$rc_fields_type == "select")
      flds <- rc_fields()
      if (is.null(flds)) return(tags$p(class="text-muted text-sm", tr("rc_load_dict_first", l())))
      if (!is.data.frame(flds)) flds <- as.data.frame(do.call(rbind, flds), stringsAsFactors=FALSE)
      nms <- flds$field_label %||% flds$field_name
      ids <- flds$field_name
      selectizeInput(ns("rc_selected_fields"), tr("rc_sel_fields_label", l),
                     choices=setNames(ids, nms), multiple=TRUE, selected=NULL,
                     options=list(maxItems=100, placeholder=tr("rc_fields_placeholder", l())))
    })

    # ── Export Records ─────────────────────────────────────────────────────
    observeEvent(input$btn_rc_export, {
      cr <- rc_creds(); req(!is.null(cr))
      extra <- list(
        type          = "flat",
        rawOrLabel    = if (isTRUE(input$rc_labels)) "label" else "raw",
        rawOrLabelHeaders = if (isTRUE(input$rc_header_labels)) "label" else "raw",
        exportSurveyFields = if (isTRUE(input$rc_exportsurvey)) "true" else "false",
        exportDataAccessGroups = "false"
      )
      # Record IDs
      ids <- trimws(input$rc_record_ids %||% "")
      if (nzchar(ids)) extra$records <- ids

      # Instruments
      forms <- input$rc_export_forms
      if (!is.null(forms) && length(forms)>0) extra$forms <- paste(forms, collapse=",")

      # Events
      events <- input$rc_export_events
      if (!is.null(events) && length(events)>0) extra$events <- paste(events, collapse=",")

      # Fields
      if (!is.null(input$rc_fields_type) && input$rc_fields_type=="select") {
        flds <- input$rc_selected_fields
        if (!is.null(flds) && length(flds)>0) extra$fields <- paste(flds, collapse=",")
      }

      # Dates
      if (nzchar(input$rc_date_begin %||% "") && nzchar(input$rc_date_end %||% "")) {
        extra$dateRangeBegin <- input$rc_date_begin
        extra$dateRangeEnd   <- input$rc_date_end
      }

      withProgress(message=tr("rc_exporting", l()), value=0.5, {
        tryCatch({
          res <- .redcap_api(cr$url, cr$token, "record", extra=extra)
          df  <- if (is.data.frame(res)) res else {
            jsonlite::fromJSON(jsonlite::toJSON(res, auto_unbox=TRUE), flatten=TRUE)
          }
          if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors=FALSE)
          rc_data(df)
          showNotification(sprintf(tr("rc_export_ok", l()), nrow(df), ncol(df)), type="message", duration=4)
        }, error=function(e) {
          rc_data(NULL)
          showNotification(paste0(tr("rc_export_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    # ── Statistiques résultat ─────────────────────────────────────────────
    output$rc_n_records  <- renderUI({ df <- rc_data(); if(is.null(df)) "—" else format(nrow(df), big.mark=" ") })
    output$rc_n_fields   <- renderUI({ df <- rc_data(); if(is.null(df)) "—" else ncol(df) })
    output$rc_n_complete <- renderUI({
      df <- rc_data()
      if (is.null(df)) return("—")
      comp_cols <- names(df)[grepl("_complete$", names(df))]
      if (length(comp_cols)==0) return("—")
      sum(sapply(comp_cols, function(c) sum(df[[c]] %in% c("2","Complete"), na.rm=TRUE)))
    })
    output$rc_n_missing <- renderUI({
      df <- rc_data()
      if (is.null(df)) return("—")
      sum(is.na(df) | df=="")
    })

    output$rc_export_msg_ui <- renderUI({
      df <- rc_data()
      if (!is.null(df))
        tags$div(class="alert alert-success", icon("check"), " ",
                 sprintf(tr("rc_export_ok", l()), nrow(df), ncol(df)))
      else NULL
    })

    output$rc_data_table <- DT::renderDataTable({
      df <- rc_data(); req(!is.null(df) && nrow(df)>0)
      DT::datatable(df, options=list(pageLength=15, scrollX=TRUE, scrollY="320px",
                                     dom='Bfrtip', buttons=c('copy','csv','excel')),
                    rownames=FALSE, class="table table-sm table-striped table-hover")
    })

    output$rc_dl_csv <- downloadHandler(
      filename=function() paste0("redcap_export_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
      content =function(file) { df <- rc_data(); req(!is.null(df)); write.csv(df, file, row.names=FALSE) }
    )
    output$rc_dl_excel <- downloadHandler(
      filename=function() paste0("redcap_export_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".xlsx"),
      content =function(file) {
        df <- rc_data(); req(!is.null(df))
        if (requireNamespace("openxlsx", quietly=TRUE)) openxlsx::write.xlsx(df, file)
        else write.csv(df, file, row.names=FALSE)
      }
    )

    observeEvent(input$btn_rc_import, {
      df <- rc_data()
      if (is.null(df) || nrow(df)==0) {
        showNotification(tr("db_no_data_import", l()), type="warning"); return()
      }
      if (!is.null(set_mapped_data)) {
        set_mapped_data(df)
        showNotification(sprintf(tr("db_imported_ok", l()), nrow(df), ncol(df)), type="message", duration=5)
      }
    })

    # ── Dictionnaire de données ────────────────────────────────────────────
    observeEvent(input$btn_rc_load_dict, {
      cr <- rc_creds(); req(!is.null(cr))
      withProgress(message=tr("rc_loading_dict", l()), value=0.5, {
        tryCatch({
          flds <- .redcap_api(cr$url, cr$token, "metadata")
          if (!is.data.frame(flds)) flds <- as.data.frame(do.call(rbind, flds), stringsAsFactors=FALSE)
          rc_fields(flds)
          showNotification(sprintf(tr("rc_dict_ok", l()), nrow(flds)), type="message", duration=3)
        }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
      })
    })

    dict_filtered <- reactive({
      df   <- rc_fields()
      flt  <- tolower(trimws(input$dict_filter %||% ""))
      type <- input$dict_type_filter %||% ""
      if (is.null(df) || (is.data.frame(df) && nrow(df)==0)) return(NULL)
      if (!is.data.frame(df)) df <- as.data.frame(do.call(rbind, df), stringsAsFactors=FALSE)
      if (nzchar(flt)) {
        idx <- apply(df[, intersect(c("field_name","field_label","section_header"), names(df)), drop=FALSE],
                     1, function(r) any(grepl(flt, tolower(r), fixed=TRUE)))
        df <- df[idx, , drop=FALSE]
      }
      if (nzchar(type) && "field_type" %in% names(df))
        df <- df[df$field_type == type, , drop=FALSE]
      df
    })

    output$rc_dict_summary_ui <- renderUI({
      df <- rc_fields()
      if (is.null(df)) return(NULL)
      if (!is.data.frame(df)) df <- as.data.frame(do.call(rbind, df), stringsAsFactors=FALSE)
      if ("field_type" %in% names(df)) {
        counts <- sort(table(df$field_type), decreasing=TRUE)
        tags$div(
          lapply(names(counts), function(t) {
            tags$span(class="rc-badge rc-badge-form mr-1",
                      t, " (", counts[t], ")")
          })
        )
      }
    })

    output$rc_dict_table <- DT::renderDataTable({
      df <- dict_filtered(); req(!is.null(df))
      show <- intersect(c("field_name","field_label","field_type","form_name",
                          "field_note","select_choices_or_calculations","required_field"),
                        names(df))
      DT::datatable(df[, show, drop=FALSE],
        options=list(pageLength=20, scrollX=TRUE, dom='Bfrtip', buttons=c('copy','csv')),
        rownames=FALSE, class="table table-sm table-striped table-hover")
    })

    output$rc_dl_dict <- downloadHandler(
      filename=function() paste0("redcap_dictionary_", format(Sys.time(),"%Y%m%d"), ".csv"),
      content =function(file) { df <- dict_filtered(); req(!is.null(df)); write.csv(df, file, row.names=FALSE) }
    )

    # ── Import vers REDCap ─────────────────────────────────────────────────
    output$rc_import_preview_ui <- renderUI({
      df <- mapped_data()
      if (is.null(df)) return(tags$p(class="text-muted", tr("db_no_source", l())))
      tags$div(class="alert alert-info",
        icon("info-circle"), " ",
        sprintf("%d %s × %d %s", nrow(df), tr("rc_records", l()), ncol(df), tr("rc_fields", l()))
      )
    })

    output$rc_import_preview_table <- DT::renderDataTable({
      df <- mapped_data(); req(!is.null(df))
      DT::datatable(head(df, 5), options=list(dom='t', scrollX=TRUE),
                    rownames=FALSE, class="table table-sm table-striped")
    })

    observeEvent(input$btn_rc_write, {
      cr  <- rc_creds(); req(!is.null(cr))
      df  <- mapped_data()
      req(!is.null(df) && nrow(df)>0)
      if (!"record_id" %in% names(df) && !"id" %in% names(df)) {
        showNotification(tr("rc_missing_record_id", l()), type="error"); return()
      }
      withProgress(message=tr("rc_importing", l()), value=0.5, {
        tryCatch({
          json_data <- jsonlite::toJSON(df, na="string")
          res <- .redcap_api(cr$url, cr$token, "record",
                             extra=list(
                               action     = "import",
                               data       = json_data,
                               overwriteBehavior = if(input$rc_import_overwrite) "overwrite" else "normal",
                               forceAutoNumber   = "false"
                             ))
          output$rc_import_status_ui <- renderUI({
            tags$div(class="alert alert-success", icon("check"), " ",
                     sprintf(tr("rc_import_ok", l()), res$count %||% nrow(df)))
          })
          showNotification(sprintf(tr("rc_import_ok", l()), res$count %||% nrow(df)), type="message", duration=5)
        }, error=function(e) {
          output$rc_import_status_ui <- renderUI({
            tags$div(class="alert alert-danger", icon("times"), " ", conditionMessage(e))
          })
          showNotification(conditionMessage(e), type="error", duration=8)
        })
      })
    })

    # ── Logs ──────────────────────────────────────────────────────────────
    observeEvent(input$btn_rc_load_logs, {
      cr <- rc_creds(); req(!is.null(cr))
      withProgress(message=tr("rc_loading_logs", l()), value=0.5, {
        tryCatch({
          extra <- list(logtype="")
          usr <- trimws(input$log_user %||% "")
          if (nzchar(usr)) extra$user <- usr
          if (nzchar(input$log_date_begin %||% "")) extra$beginTime <- input$log_date_begin
          if (nzchar(input$log_date_end   %||% "")) extra$endTime   <- input$log_date_end
          res <- .redcap_api(cr$url, cr$token, "log", extra=extra)
          df  <- if (is.data.frame(res)) res else as.data.frame(do.call(rbind, res), stringsAsFactors=FALSE)
          rc_logs_data(head(df, input$log_limit))
          showNotification(sprintf("%d logs chargés", nrow(df)), type="message", duration=3)
        }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
      })
    })

    output$rc_logs_table <- DT::renderDataTable({
      df <- rc_logs_data(); req(!is.null(df) && nrow(df)>0)
      DT::datatable(df, options=list(pageLength=20, scrollX=TRUE, dom='Bfrtip',
                                     buttons=c('copy','csv')),
                    rownames=FALSE, class="table table-sm table-striped table-hover")
    })

    output$rc_dl_logs <- downloadHandler(
      filename=function() paste0("redcap_logs_", format(Sys.time(),"%Y%m%d"), ".csv"),
      content =function(file) { df <- rc_logs_data(); req(!is.null(df)); write.csv(df, file, row.names=FALSE) }
    )
  })
}
