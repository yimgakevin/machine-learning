# =============================================================================
# mod_odk_central.R — Module ODK Central + KoboToolbox API v2
# NutriAnalyse Pro v2 | YIMGA WANDJI KEVIN | DSAMID
# ODK Central REST API v1 + KoboToolbox v2 API
# =============================================================================

# ── Packages requis : httr2 (ou httr), jsonlite, DT ─────────────────────────

# Helper requête ODK/Kobo
.odk_get <- function(base_url, endpoint, token=NULL, user=NULL, pass=NULL,
                     params=list(), timeout=30, type="json") {
  full_url <- paste0(sub("/+$","",base_url), endpoint)
  if (!requireNamespace("httr2", quietly=TRUE)) {
    req <- httr::GET(full_url,
      if (!is.null(token)) httr::add_headers(Authorization=paste("Token", token))
      else httr::authenticate(user %||% "", pass %||% ""),
      query=params, httr::timeout(timeout),
      httr::accept(if(type=="json") "application/json" else "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    )
    httr::stop_for_status(req)
    if (type=="json") jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"), simplifyVector=FALSE)
    else httr::content(req, "raw")
  } else {
    r <- httr2::request(full_url) |>
      httr2::req_timeout(timeout)
    if (!is.null(token)) {
      r <- r |> httr2::req_headers(Authorization=paste("Token", token))
    } else {
      r <- r |> httr2::req_auth_basic(user %||% "", pass %||% "")
    }
    r <- r |> httr2::req_url_query(!!!params) |>
      httr2::req_headers(Accept=if(type=="json") "application/json" else "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    resp <- httr2::req_perform(r)
    httr2::resp_check_status(resp)
    if (type=="json") jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector=FALSE)
    else httr2::resp_body_raw(resp)
  }
}

.odk_post <- function(base_url, endpoint, body, user=NULL, pass=NULL, token=NULL, timeout=30) {
  full_url <- paste0(sub("/+$","",base_url), endpoint)
  if (!requireNamespace("httr2", quietly=TRUE)) {
    req <- httr::POST(full_url,
      if (!is.null(token)) httr::add_headers(Authorization=paste("Token", token))
      else httr::authenticate(user %||% "", pass %||% ""),
      body=jsonlite::toJSON(body, auto_unbox=TRUE),
      httr::content_type_json(), httr::timeout(timeout)
    )
    httr::stop_for_status(req)
    jsonlite::fromJSON(httr::content(req,"text",encoding="UTF-8"), simplifyVector=FALSE)
  } else {
    r <- httr2::request(full_url) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(timeout)
    if (!is.null(token)) r <- r |> httr2::req_headers(Authorization=paste("Token", token))
    else r <- r |> httr2::req_auth_basic(user %||% "", pass %||% "")
    resp <- httr2::req_perform(r)
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector=FALSE)
  }
}

# =============================================================================
# UI
# =============================================================================
odkCentralUI <- function(id, lang="fr") {
  ns <- NS(id)
  l  <- lang

  tagList(
    tags$head(
      tags$style(HTML("
        .odk-header  { background:linear-gradient(90deg,#e8530a,#bf4200); color:#fff; padding:18px 22px; border-radius:10px; margin-bottom:20px; }
        .kobo-header { background:linear-gradient(90deg,#416bb7,#294a99); color:#fff; }
        .odk-form-card { padding:10px; border:1px solid #dee2e6; border-radius:6px; margin-bottom:8px; cursor:pointer; }
        .odk-form-card:hover { border-color:#e8530a; background:#fff5f0; }
        .odk-form-card.selected { border-color:#e8530a; background:#fff0e8; }
        .odk-badge { display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; font-weight:600; }
        .badge-open   { background:#28a74522; color:#28a745; }
        .badge-closed { background:#dc354522; color:#dc3545; }
        .badge-enc    { background:#ffc10722; color:#856404; }
        .submission-stat { text-align:center; padding:8px; background:#fff; border-radius:8px;
                           box-shadow:0 1px 4px rgba(0,0,0,.1); }
        .submission-stat .num { font-size:20px; font-weight:700; color:#e8530a; }
      "))
    ),

    fluidRow(
      column(12,
        tags$div(class="odk-header",
          tags$h3(style="margin:0; font-weight:700;",
            icon("wpforms"), " ", tr("odk_title", l)),
          tags$p(style="margin:4px 0 0; opacity:.85; font-size:13px;", tr("odk_subtitle", l))
        )
      )
    ),

    tabsetPanel(id=ns("odk_platform"),

      # ── ODK Central ────────────────────────────────────────────────────────
      tabPanel(tags$span(icon("server"), " ODK Central"), value="odk_central",
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("plug"), " ", tr("odk_conn_title", l)),
                status="warning", solidHeader=TRUE, width=12,

              textInput(ns("odk_url"), tr("odk_url_label", l),
                        placeholder="https://odk.mondomaine.org",
                        value="https://sandbox.central.getodk.org"),
              textInput(ns("odk_email"), tr("odk_email", l), placeholder="admin@exemple.org"),
              passwordInput(ns("odk_pass"), tr("odk_pass", l)),
              fluidRow(
                column(6, actionButton(ns("btn_odk_connect"), tr("odk_connect", l),
                                       icon=icon("link"), class="btn-warning btn-block")),
                column(6, actionButton(ns("btn_odk_disc"), tr("odk_disconnect", l),
                                       icon=icon("unlink"), class="btn-danger btn-block"))
              ),
              br(),
              uiOutput(ns("odk_status_ui"))
            ),

            box(title=tags$span(icon("project-diagram"), " ", tr("odk_projects", l)),
                status="info", solidHeader=TRUE, width=12,
              uiOutput(ns("odk_projects_ui")),
              br(),
              uiOutput(ns("odk_forms_ui"))
            )
          ),

          column(8,
            box(title=tags$span(icon("list"), " ", tr("odk_form_detail", l)),
                status="primary", solidHeader=TRUE, width=12,
              uiOutput(ns("odk_form_detail_ui"))
            ),

            box(title=tags$span(icon("download"), " ", tr("odk_download_subs", l)),
                status="success", solidHeader=TRUE, width=12,

              fluidRow(
                column(4,
                  selectInput(ns("odk_format"), tr("odk_dl_format", l),
                    choices=c("JSON"="json","CSV"="csv","CSV+ZIP"="zip")),
                  checkboxInput(ns("odk_inc_deleted"), tr("odk_inc_deleted", l), FALSE),
                  numericInput(ns("odk_max_subs"), tr("odk_max_subs", l), value=500, min=1, max=100000)
                ),
                column(4,
                  textInput(ns("odk_filter_date_start"), tr("odk_date_start", l),
                            value=format(Sys.Date()-365, "%Y-%m-%d")),
                  textInput(ns("odk_filter_date_end"), tr("odk_date_end", l),
                            value=format(Sys.Date(), "%Y-%m-%d")),
                  selectInput(ns("odk_review_status"), tr("odk_review_status", l),
                    choices=setNames(c("", "approved", "rejected", "hasIssues"),
                                    c(tr("odk_all", l), "approved", "rejected", "hasIssues")), multiple=FALSE)
                ),
                column(4,
                  br(),
                  actionButton(ns("btn_odk_download"), tr("odk_download_btn", l),
                               icon=icon("download"), class="btn-success btn-block btn-lg"),
                  br(),
                  downloadButton(ns("odk_dl_csv"), "Export CSV", class="btn-outline-success btn-block"),
                  br(),
                  actionButton(ns("btn_odk_import"), tr("db_import_app", l),
                               icon=icon("arrow-right"), class="btn-warning btn-block")
                )
              ),

              fluidRow(
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("odk_n_subs"))), tags$small(tr("odk_submissions", l)))),
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("odk_n_cols"))), tags$small(tr("db_cols", l)))),
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("odk_n_approved"))), tags$small("Approved"))),
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("odk_n_pending"))), tags$small("Pending")))
              ),
              br(),
              DT::dataTableOutput(ns("odk_subs_table"))
            )
          )
        )
      ),

      # ── KoboToolbox ────────────────────────────────────────────────────────
      tabPanel(tags$span(icon("wpforms"), " KoboToolbox"), value="kobo_api",
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("plug"), " ", tr("kobo_conn_title", l)),
                status="primary", solidHeader=TRUE, width=12,

              selectInput(ns("kobo_server"), tr("kobo_server_label", l),
                choices=setNames(
                  c("https://kf.kobotoolbox.org", "https://eu.kobotoolbox.org", "custom"),
                  c("KoboToolbox (humanitarian)", "KoboToolbox (non-humanit.)",
                    tr("kobo_custom_server", l))
                )),
              conditionalPanel(
                condition=sprintf("input['%s'] == 'custom'", ns("kobo_server")),
                textInput(ns("kobo_custom_url"), tr("kobo_custom_url", l),
                          placeholder="https://kobo.mondomaine.org")
              ),
              passwordInput(ns("kobo_token"), tr("kobo_token_label", l),
                            placeholder="Token API KoboToolbox"),
              tags$small(class="text-muted",
                tr("kobo_token_hint", l), " ",
                tags$a(href="https://kf.kobotoolbox.org/accounts/api-key",
                       target="_blank", tr("kobo_get_token", l))
              ),
              br(),br(),
              fluidRow(
                column(6, actionButton(ns("btn_kobo_connect"), tr("odk_connect", l),
                                       icon=icon("link"), class="btn-primary btn-block")),
                column(6, actionButton(ns("btn_kobo_disc"), tr("odk_disconnect", l),
                                       icon=icon("unlink"), class="btn-danger btn-block"))
              ),
              br(),
              uiOutput(ns("kobo_status_ui"))
            ),

            box(title=tags$span(icon("list-alt"), " ", tr("kobo_assets_title", l)),
                status="info", solidHeader=TRUE, width=12, collapsible=TRUE,

              fluidRow(
                column(8, textInput(ns("kobo_asset_filter"), tr("kobo_filter", l), placeholder="Recherche...")),
                column(4, br(), actionButton(ns("btn_kobo_refresh"), icon("sync"), class="btn-sm btn-info"))
              ),
              uiOutput(ns("kobo_assets_ui"))
            )
          ),

          column(8,
            box(title=tags$span(icon("info-circle"), " ", tr("kobo_form_detail", l)),
                status="warning", solidHeader=TRUE, width=12,
              uiOutput(ns("kobo_form_detail_ui"))
            ),

            box(title=tags$span(icon("database"), " ", tr("kobo_data_download", l)),
                status="success", solidHeader=TRUE, width=12,

              fluidRow(
                column(4,
                  selectInput(ns("kobo_dl_format"), tr("odk_dl_format", l),
                    choices=c("JSON"="json","CSV"="csv","XLS"="xls","SPSS"="spss","STATA"="stata")),
                  checkboxInput(ns("kobo_inc_media"), tr("kobo_inc_media", l), FALSE),
                  checkboxInput(ns("kobo_flat"), tr("kobo_flatten_groups", l), TRUE)
                ),
                column(4,
                  textInput(ns("kobo_start_date"), tr("odk_date_start", l),
                            value=format(Sys.Date()-365, "%Y-%m-%d")),
                  textInput(ns("kobo_end_date"), tr("odk_date_end", l),
                            value=format(Sys.Date(), "%Y-%m-%d")),
                  numericInput(ns("kobo_max_count"), tr("odk_max_subs", l), value=5000, min=1, max=500000)
                ),
                column(4,
                  br(),
                  actionButton(ns("btn_kobo_download"), tr("odk_download_btn", l),
                               icon=icon("download"), class="btn-success btn-block btn-lg"),
                  br(),
                  downloadButton(ns("kobo_dl_csv"), "Export CSV", class="btn-outline-success btn-block"),
                  br(),
                  actionButton(ns("btn_kobo_import"), tr("db_import_app", l),
                               icon=icon("arrow-right"), class="btn-warning btn-block")
                )
              ),
              br(),
              fluidRow(
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("kobo_n_subs"))), tags$small(tr("odk_submissions", l)))),
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("kobo_n_cols"))), tags$small(tr("db_cols", l)))),
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("kobo_n_q"))), tags$small(tr("kobo_questions", l)))),
                column(3, div(class="submission-stat", div(class="num", uiOutput(ns("kobo_deployed"))), tags$small(tr("kobo_deployed_label", l))))
              ),
              br(),
              DT::dataTableOutput(ns("kobo_data_table"))
            )
          )
        )
      ),

      # ── ODK Aggregate (legacy) — déprécié, lecture seule ─────────────────
      tabPanel(tags$span(icon("archive"), " ODK Aggregate"), value="odk_agg",
        br(),
        tags$div(class="alert alert-warning",
          icon("exclamation-triangle"), " ",
          tags$b(tr("odk_agg_info", l)), tags$br(),
          tr("odk_agg_deprecated", l), " ",
          tags$a(href="https://docs.getodk.org/central-intro/", target="_blank",
                 "ODK Central", icon("external-link-alt"))
        )
      )
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================
odkCentralServer <- function(id, mapped_data=reactive(NULL),
                               set_mapped_data=NULL, lang=reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    l  <- lang

    # ── State ────────────────────────────────────────────────────────────────
    odk_token      <- reactiveVal(NULL)
    odk_projects   <- reactiveVal(NULL)
    odk_forms_data <- reactiveVal(NULL)
    selected_form  <- reactiveVal(NULL)   # list(projectId, xmlFormId, name)
    odk_subs       <- reactiveVal(NULL)

    kobo_creds     <- reactiveVal(NULL)   # list(url, token)
    kobo_assets    <- reactiveVal(NULL)
    selected_asset <- reactiveVal(NULL)
    kobo_data      <- reactiveVal(NULL)

    # ── ODK Central : Connexion ───────────────────────────────────────────
    observeEvent(input$btn_odk_connect, {
      url  <- sub("/+$","",trimws(input$odk_url))
      mail <- input$odk_email
      pw   <- input$odk_pass
      withProgress(message=tr("odk_connecting", l()), value=0.5, {
        tryCatch({
          # Session token via /v1/sessions
          res <- .odk_post(url, "/v1/sessions",
                           list(email=mail, password=pw))
          token <- res$token
          if (is.null(token)) stop(tr("odk_no_token", l()))
          odk_token(list(url=url, token=token))
          # Charger les projets
          proj <- .odk_get(url, "/v1/projects", token=token)
          odk_projects(proj)
          showNotification(paste0(tr("odk_connected_ok", l()), " — ", length(proj), " ", tr("odk_projects", l())),
                           type="message", duration=4)
        }, error=function(e) {
          odk_token(NULL)
          showNotification(paste0(tr("odk_conn_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    observeEvent(input$btn_odk_disc, {
      cr <- odk_token()
      if (!is.null(cr)) {
        tryCatch(.odk_post(cr$url, "/v1/sessions/current", list(), token=cr$token), error=function(e) NULL)
      }
      odk_token(NULL); odk_projects(NULL); odk_forms_data(NULL); selected_form(NULL)
      showNotification(tr("odk_disconnected", l()), type="warning", duration=3)
    })

    output$odk_status_ui <- renderUI({
      cr <- odk_token()
      if (!is.null(cr))
        tags$div(class="dhis2-card dhis2-connected", icon("check-circle", class="text-success"), " ",
                 tags$b(tr("odk_connected_ok", l())), tags$br(), tags$small(class="text-muted", cr$url))
      else
        tags$div(class="dhis2-card dhis2-disconn", icon("times-circle", class="text-muted"), " ",
                 tags$span(class="text-muted", tr("odk_not_connected", l())))
    })

    # ── Projets ──────────────────────────────────────────────────────────
    output$odk_projects_ui <- renderUI({
      proj <- odk_projects()
      if (is.null(proj) || length(proj)==0)
        return(tags$p(class="text-muted", tr("odk_no_projects", l())))
      ids  <- sapply(proj, `[[`, "id")
      nms  <- sapply(proj, `[[`, "name")
      tagList(
        tags$p(tags$b(tr("odk_select_project", l()))),
        selectInput(ns("odk_project_id"), NULL, choices=setNames(ids, nms)),
        actionButton(ns("btn_load_forms"), tr("odk_load_forms", l),
                     icon=icon("sync"), class="btn-sm btn-info btn-block")
      )
    })

    observeEvent(input$btn_load_forms, {
      cr  <- odk_token()
      req(!is.null(cr))
      pid <- input$odk_project_id
      req(!is.null(pid))
      withProgress(message=tr("odk_loading_forms", l()), value=0.5, {
        tryCatch({
          forms <- .odk_get(cr$url, paste0("/v1/projects/", pid, "/forms"), token=cr$token)
          odk_forms_data(forms)
        }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
      })
    })

    output$odk_forms_ui <- renderUI({
      forms <- odk_forms_data()
      if (is.null(forms) || length(forms)==0) return(NULL)
      tagList(
        tags$p(tags$b(tr("odk_forms", l()), " (", length(forms), ")")),
        tags$div(style="max-height:250px; overflow-y:auto;",
          lapply(forms, function(f) {
            state_badge <- if (f$state %||% "open" == "open")
              tags$span(class="odk-badge badge-open", "open")
            else tags$span(class="odk-badge badge-closed", f$state %||% "?")
            tags$div(class="odk-form-card",
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("odk_form_click"), f$xmlFormId %||% ""),
              tags$b(f$name %||% f$xmlFormId), " ", state_badge, tags$br(),
              tags$small(class="text-muted", "v", f$version %||% "—", " | ", f$submissions %||% 0, " submissions")
            )
          })
        )
      )
    })

    observeEvent(input$odk_form_click, {
      cr  <- odk_token()
      pid <- input$odk_project_id
      fid <- input$odk_form_click
      req(!is.null(cr), !is.null(pid), nzchar(fid))
      tryCatch({
        detail <- .odk_get(cr$url, paste0("/v1/projects/", pid, "/forms/", fid), token=cr$token)
        selected_form(list(projectId=pid, xmlFormId=fid, detail=detail))
      }, error=function(e) NULL)
    })

    output$odk_form_detail_ui <- renderUI({
      sf <- selected_form()
      if (is.null(sf)) return(tags$p(class="text-muted text-center", tr("odk_select_form_hint", l())))
      d  <- sf$detail
      tagList(
        fluidRow(
          column(6, tags$table(class="table table-sm",
            tags$tr(tags$td(tags$b(tr("odk_form_name", l()))),    tags$td(d$name %||% "—")),
            tags$tr(tags$td(tags$b(tr("odk_form_id", l()))),      tags$td(d$xmlFormId %||% "—")),
            tags$tr(tags$td(tags$b(tr("odk_form_version", l()))), tags$td(d$version %||% "—")),
            tags$tr(tags$td(tags$b(tr("odk_submissions", l()))),  tags$td(tags$b(d$submissions %||% 0)))
          )),
          column(6, tags$table(class="table table-sm",
            tags$tr(tags$td(tags$b("State")),     tags$td(d$state %||% "—")),
            tags$tr(tags$td(tags$b("Created")),   tags$td(substr(d$createdAt %||% "—", 1, 10))),
            tags$tr(tags$td(tags$b("Updated")),   tags$td(substr(d$updatedAt %||% "—", 1, 10))),
            tags$tr(tags$td(tags$b("Published")), tags$td(substr(d$publishedAt %||% "—", 1, 10)))
          ))
        )
      )
    })

    # ── Download Submissions ODK ──────────────────────────────────────────
    observeEvent(input$btn_odk_download, {
      cr  <- odk_token()
      sf  <- selected_form()
      req(!is.null(cr), !is.null(sf))
      pid <- sf$projectId
      fid <- sf$xmlFormId
      fmt <- input$odk_format
      withProgress(message=tr("odk_downloading", l()), value=0.5, {
        tryCatch({
          endpoint <- paste0("/v1/projects/", pid, "/forms/", fid, "/submissions.csv")
          raw <- .odk_get(cr$url, endpoint, token=cr$token, type="csv")
          # Si CSV direct
          if (is.character(raw)) {
            df <- read.csv(textConnection(raw), stringsAsFactors=FALSE, check.names=FALSE)
          } else {
            # JSON submissions
            endpoint_j <- paste0("/v1/projects/", pid, "/forms/", fid, "/submissions")
            res <- .odk_get(cr$url, endpoint_j, token=cr$token,
                            params=list(`$top`=input$odk_max_subs))
            subs <- res$value %||% res
            if (length(subs)==0) { odk_subs(data.frame()); return() }
            df <- jsonlite::fromJSON(jsonlite::toJSON(subs, auto_unbox=TRUE), flatten=TRUE)
            if (!is.data.frame(df)) df <- as.data.frame(df)
          }
          # Filtrage dates
          if ("submissionDate" %in% names(df)) {
            df$submissionDate <- as.Date(substr(df$submissionDate, 1, 10))
            s <- input$odk_filter_date_start; e <- input$odk_filter_date_end
            if (nzchar(s)) df <- df[!is.na(df$submissionDate) & df$submissionDate >= as.Date(s), , drop=FALSE]
            if (nzchar(e)) df <- df[!is.na(df$submissionDate) & df$submissionDate <= as.Date(e), , drop=FALSE]
          }
          odk_subs(df)
          showNotification(sprintf(tr("odk_dl_ok", l()), nrow(df)), type="message", duration=4)
        }, error=function(e) {
          odk_subs(NULL)
          showNotification(paste0(tr("odk_dl_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    output$odk_n_subs    <- renderUI({ df <- odk_subs(); if(is.null(df)) "—" else format(nrow(df), big.mark=" ") })
    output$odk_n_cols    <- renderUI({ df <- odk_subs(); if(is.null(df)) "—" else ncol(df) })
    output$odk_n_approved <- renderUI({
      df <- odk_subs()
      if(is.null(df)||!"__system/reviewState"%in%names(df)) return("—")
      sum(df[["__system/reviewState"]]=="approved", na.rm=TRUE)
    })
    output$odk_n_pending <- renderUI({
      df <- odk_subs()
      if(is.null(df)||!"__system/reviewState"%in%names(df)) return("—")
      sum(is.na(df[["__system/reviewState"]]) | df[["__system/reviewState"]]=="", na.rm=TRUE)
    })

    output$odk_subs_table <- DT::renderDataTable({
      df <- odk_subs(); req(!is.null(df) && nrow(df)>0)
      # Cacher colonnes système
      show_cols <- names(df)[!grepl("^__",names(df))]
      DT::datatable(df[, show_cols, drop=FALSE],
        options=list(pageLength=15, scrollX=TRUE, scrollY="300px",
                     dom='Bfrtip', buttons=c('copy','csv','excel')),
        rownames=FALSE, class="table table-sm table-striped table-hover")
    })

    output$odk_dl_csv <- downloadHandler(
      filename=function() paste0("odk_submissions_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
      content =function(file) { df <- odk_subs(); req(!is.null(df)); write.csv(df, file, row.names=FALSE) }
    )

    observeEvent(input$btn_odk_import, {
      df <- odk_subs()
      if (is.null(df) || nrow(df)==0) {
        showNotification(tr("db_no_data_import", l()), type="warning"); return()
      }
      if (!is.null(set_mapped_data)) {
        set_mapped_data(df)
        showNotification(sprintf(tr("db_imported_ok", l()), nrow(df), ncol(df)), type="message", duration=5)
      }
    })

    # ── KoboToolbox : Connexion ───────────────────────────────────────────
    observeEvent(input$btn_kobo_connect, {
      url <- if (input$kobo_server == "custom") trimws(input$kobo_custom_url) else input$kobo_server
      token <- trimws(input$kobo_token)
      if (!nzchar(token)) { showNotification(tr("kobo_token_required", l()), type="warning"); return() }
      withProgress(message=tr("odk_connecting", l()), value=0.5, {
        tryCatch({
          info <- .odk_get(url, "/api/v2/me/", token=token)
          kobo_creds(list(url=url, token=token, username=info$username %||% "?"))
          # Charger les assets
          assets <- .odk_get(url, "/api/v2/assets/",
                             token=token, params=list(limit=200, format="json"))
          kobo_assets(assets$results %||% assets)
          showNotification(paste0(tr("kobo_connected_ok", l()), " — ", info$username %||% "?"),
                           type="message", duration=4)
        }, error=function(e) {
          kobo_creds(NULL)
          showNotification(paste0(tr("odk_conn_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    observeEvent(input$btn_kobo_disc, {
      kobo_creds(NULL); kobo_assets(NULL); selected_asset(NULL); kobo_data(NULL)
      showNotification(tr("odk_disconnected", l()), type="warning", duration=3)
    })

    observeEvent(input$btn_kobo_refresh, {
      cr <- kobo_creds(); req(!is.null(cr))
      tryCatch({
        assets <- .odk_get(cr$url, "/api/v2/assets/",
                           token=cr$token, params=list(limit=200, format="json"))
        kobo_assets(assets$results %||% assets)
      }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
    })

    output$kobo_status_ui <- renderUI({
      cr <- kobo_creds()
      if (!is.null(cr))
        tags$div(class="dhis2-card dhis2-connected", icon("check-circle", class="text-success"), " ",
                 tags$b(tr("kobo_connected_ok", l())), " (", cr$username, ")", tags$br(),
                 tags$small(class="text-muted", cr$url))
      else
        tags$div(class="dhis2-card dhis2-disconn", icon("times-circle", class="text-muted"), " ",
                 tags$span(class="text-muted", tr("odk_not_connected", l())))
    })

    output$kobo_assets_ui <- renderUI({
      assets <- kobo_assets()
      flt    <- tolower(trimws(input$kobo_asset_filter %||% ""))
      if (is.null(assets) || length(assets)==0) return(tags$p(class="text-muted", tr("kobo_no_assets", l())))
      items  <- assets
      if (nzchar(flt)) items <- Filter(function(a) grepl(flt, tolower(a$name %||% ""), fixed=TRUE), items)
      tagList(
        tags$div(style="max-height:300px; overflow-y:auto;",
          lapply(items, function(a) {
            dep_badge <- if (isTRUE(a$has_deployment) && a$deployment__active)
              tags$span(class="odk-badge badge-open", "deployed")
            else tags$span(class="odk-badge badge-closed", "draft")
            tags$div(class="odk-form-card",
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("kobo_asset_click"), a$uid %||% ""),
              tags$b(a$name %||% "Sans titre"), " ", dep_badge, tags$br(),
              tags$small(class="text-muted",
                a$deployment__submission_count %||% 0, " sub | ",
                a$asset_type %||% "survey", " | ", substr(a$date_modified %||% "", 1, 10))
            )
          })
        )
      )
    })

    observeEvent(input$kobo_asset_click, {
      cr  <- kobo_creds()
      uid <- input$kobo_asset_click
      req(!is.null(cr), nzchar(uid))
      tryCatch({
        detail <- .odk_get(cr$url, paste0("/api/v2/assets/", uid, "/"), token=cr$token)
        selected_asset(detail)
      }, error=function(e) NULL)
    })

    output$kobo_form_detail_ui <- renderUI({
      sa <- selected_asset()
      if (is.null(sa)) return(tags$p(class="text-muted text-center", tr("kobo_select_form_hint", l())))
      tagList(
        fluidRow(
          column(6, tags$table(class="table table-sm",
            tags$tr(tags$td(tags$b(tr("odk_form_name", l()))),    tags$td(sa$name %||% "—")),
            tags$tr(tags$td(tags$b("UID")),                       tags$td(sa$uid %||% "—")),
            tags$tr(tags$td(tags$b(tr("odk_submissions", l()))),  tags$td(tags$b(sa$deployment__submission_count %||% 0))),
            tags$tr(tags$td(tags$b("Type")),                      tags$td(sa$asset_type %||% "—"))
          )),
          column(6, tags$table(class="table table-sm",
            tags$tr(tags$td(tags$b("Owner")),   tags$td(basename(sa$owner__username %||% "—"))),
            tags$tr(tags$td(tags$b("Created")), tags$td(substr(sa$date_created %||% "—", 1, 10))),
            tags$tr(tags$td(tags$b("Updated")), tags$td(substr(sa$date_modified %||% "—", 1, 10))),
            tags$tr(tags$td(tags$b("Active")),  tags$td(as.character(sa$deployment__active %||% FALSE)))
          ))
        )
      )
    })

    # ── Download KoboToolbox Data ─────────────────────────────────────────
    observeEvent(input$btn_kobo_download, {
      cr <- kobo_creds()
      sa <- selected_asset()
      req(!is.null(cr), !is.null(sa))
      uid <- sa$uid
      withProgress(message=tr("odk_downloading", l()), value=0.5, {
        tryCatch({
          fmt <- input$kobo_dl_format
          endpoint <- paste0("/api/v2/assets/", uid, "/data/")
          params <- list(format=fmt, limit=input$kobo_max_count)
          if (nzchar(input$kobo_start_date)) params$start <- input$kobo_start_date
          if (nzchar(input$kobo_end_date))   params$end   <- input$kobo_end_date

          if (fmt %in% c("json")) {
            res  <- .odk_get(cr$url, endpoint, token=cr$token, params=params)
            subs <- res$results %||% res
            if (length(subs)==0) { kobo_data(data.frame()); return() }
            df <- jsonlite::fromJSON(jsonlite::toJSON(subs, auto_unbox=TRUE),
                                     flatten=input$kobo_flat)
            if (!is.data.frame(df)) df <- as.data.frame(df)
          } else {
            # CSV
            params$format <- "csv"
            raw_text <- .odk_get(cr$url, endpoint, token=cr$token, params=params, type="text")
            df <- read.csv(textConnection(raw_text), stringsAsFactors=FALSE, check.names=FALSE)
          }
          kobo_data(df)
          showNotification(sprintf(tr("odk_dl_ok", l()), nrow(df)), type="message", duration=4)
        }, error=function(e) {
          kobo_data(NULL)
          showNotification(paste0(tr("odk_dl_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    output$kobo_n_subs  <- renderUI({ df <- kobo_data(); if(is.null(df)) "—" else format(nrow(df), big.mark=" ") })
    output$kobo_n_cols  <- renderUI({ df <- kobo_data(); if(is.null(df)) "—" else ncol(df) })
    output$kobo_n_q     <- renderUI({
      sa <- selected_asset()
      if(is.null(sa)) return("—")
      q <- sa$summary$geo_question_name
      sa$summary$languages_count %||% "—"
    })
    output$kobo_deployed <- renderUI({
      sa <- selected_asset()
      if(is.null(sa)) return("—")
      if (isTRUE(sa$deployment__active)) icon("check-circle", class="text-success fa-lg")
      else icon("times-circle", class="text-muted fa-lg")
    })

    output$kobo_data_table <- DT::renderDataTable({
      df <- kobo_data(); req(!is.null(df) && nrow(df)>0)
      DT::datatable(df, options=list(pageLength=15, scrollX=TRUE, scrollY="300px",
                                     dom='Bfrtip', buttons=c('copy','csv','excel')),
                    rownames=FALSE, class="table table-sm table-striped table-hover")
    })

    output$kobo_dl_csv <- downloadHandler(
      filename=function() paste0("kobo_data_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
      content =function(file) { df <- kobo_data(); req(!is.null(df)); write.csv(df, file, row.names=FALSE) }
    )

    observeEvent(input$btn_kobo_import, {
      df <- kobo_data()
      if (is.null(df) || nrow(df)==0) {
        showNotification(tr("db_no_data_import", l()), type="warning"); return()
      }
      if (!is.null(set_mapped_data)) {
        set_mapped_data(df)
        showNotification(sprintf(tr("db_imported_ok", l()), nrow(df), ncol(df)), type="message", duration=5)
      }
    })

    # ── ODK Aggregate (legacy) : plus de connexion, protocole déprécié ─────
  })
}
