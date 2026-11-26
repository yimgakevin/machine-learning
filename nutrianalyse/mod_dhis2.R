# =============================================================================
# mod_dhis2.R — Module Intégration DHIS2
# NutriAnalyse Pro v2 | YIMGA WANDJI KEVIN | DSAMID
# DHIS2 REST API v2.x — Analytics, Data Values, Metadata, OrgUnits
# =============================================================================

# ── Packages requis : httr2 (ou httr), jsonlite, DT ──────────────────────────

# Helper : requête DHIS2 avec authentification
.dhis2_get <- function(base_url, endpoint, user, pass, params=list(), timeout=30) {
  if (!requireNamespace("httr2", quietly=TRUE)) {
    if (!requireNamespace("httr", quietly=TRUE))
      stop("Package httr2 ou httr requis.")
    # Fallback httr
    url <- paste0(base_url, endpoint)
    r   <- httr::GET(url, httr::authenticate(user, pass),
                     query=params, httr::timeout(timeout))
    httr::stop_for_status(r)
    jsonlite::fromJSON(httr::content(r, "text", encoding="UTF-8"), simplifyVector=FALSE)
  } else {
    req <- httr2::request(paste0(base_url, endpoint)) |>
      httr2::req_auth_basic(user, pass) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_timeout(timeout) |>
      httr2::req_headers(Accept="application/json")
    resp <- httr2::req_perform(req)
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector=FALSE)
  }
}

# =============================================================================
# UI
# =============================================================================
dhis2UI <- function(id, lang="fr") {
  ns <- NS(id)
  l  <- lang

  tagList(
    tags$head(
      tags$style(HTML("
        .dhis2-header    { background:linear-gradient(90deg,#1a73a7,#0d4e7a); color:#fff; padding:18px 22px; border-radius:10px; margin-bottom:20px; }
        .dhis2-connected { border-left:4px solid #28a745; background:#f0fff4; }
        .dhis2-disconn   { border-left:4px solid #6c757d; background:#f8f9fa; }
        .dhis2-card      { padding:12px; border-radius:6px; margin-bottom:10px; }
        .dhis2-meta-tag  { display:inline-block; background:#1a73a722; color:#1a73a7; border-radius:10px;
                           padding:2px 8px; font-size:11px; font-weight:600; margin:2px; }
        .dhis2-ou-item   { padding:5px 8px; cursor:pointer; border-bottom:1px solid #eee; font-size:13px; }
        .dhis2-ou-item:hover { background:#e3f2fd; }
      "))
    ),

    tags$div(class="dhis2-header",
      tags$h3(style="margin:0; font-weight:700;",
        icon("globe"), " ", tr("dhis2_title", l)),
      tags$p(style="margin:4px 0 0; opacity:.85; font-size:13px;", tr("dhis2_subtitle", l))
    ),

    tabsetPanel(id=ns("dhis2_tabs"),

      # ── Onglet 1 : Connexion & Métadonnées ────────────────────────────────
      tabPanel(tr("dhis2_tab_conn", l), value="conn", icon=icon("plug"),
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("server"), " ", tr("dhis2_server", l)),
                status="primary", solidHeader=TRUE, width=12,

              textInput(ns("dhis2_url"), tr("dhis2_url_label", l),
                value="https://play.dhis2.org/40/api",
                placeholder="https://instance.dhis2.org/api"),
              textInput(ns("dhis2_user"), tr("dhis2_user_label", l), value="admin"),
              passwordInput(ns("dhis2_pass"), tr("dhis2_pass_label", l), value="district"),
              selectInput(ns("dhis2_api_ver"), tr("dhis2_api_ver", l),
                choices=c("Auto-detect"="auto","40","39","38","37","36")),

              fluidRow(
                column(6, actionButton(ns("btn_dhis2_connect"), tr("dhis2_connect", l),
                                       icon=icon("plug"), class="btn-primary btn-block")),
                column(6, actionButton(ns("btn_dhis2_disc"), tr("dhis2_disconnect", l),
                                       icon=icon("power-off"), class="btn-danger btn-block"))
              ),
              br(),
              uiOutput(ns("dhis2_status_ui"))
            ),

            box(title=tags$span(icon("info-circle"), " ", tr("dhis2_server_info", l)),
                status="info", solidHeader=TRUE, width=12, collapsible=TRUE,
              uiOutput(ns("dhis2_server_info_ui"))
            )
          ),

          column(8,
            box(title=tags$span(icon("sitemap"), " ", tr("dhis2_orgunit_browser", l)),
                status="success", solidHeader=TRUE, width=12,

              fluidRow(
                column(4, selectInput(ns("ou_level"), tr("dhis2_ou_level", l),
                                      choices=c("1","2","3","4"), selected="1")),
                column(4, textInput(ns("ou_filter"), tr("dhis2_ou_filter", l), placeholder="Filtre...")),
                column(4, br(), actionButton(ns("btn_load_ou"), tr("dhis2_load_ou", l),
                                             icon=icon("sync"), class="btn-info btn-block"))
              ),
              uiOutput(ns("ou_list_ui")),
              br(),
              tags$p(tags$b(tr("dhis2_selected_ous", l))),
              uiOutput(ns("selected_ous_ui"))
            )
          )
        )
      ),

      # ── Onglet 2 : Téléchargement Analytics ──────────────────────────────
      tabPanel(tr("dhis2_tab_analytics", l), value="analytics", icon=icon("chart-bar"),
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("sliders-h"), " ", tr("dhis2_query_params", l)),
                status="primary", solidHeader=TRUE, width=12,

              radioButtons(ns("data_type"), tr("dhis2_data_type", l),
                choices=setNames(
                  c("indicators", "dataElements", "dataSets", "programs"),
                  c(tr("dhis2_type_indicator", l), tr("dhis2_type_element", l),
                    tr("dhis2_type_dataset", l), tr("dhis2_type_program", l))
                ), inline=FALSE),
              hr(),

              # Indicateurs/éléments
              fluidRow(
                column(9, textInput(ns("de_search"), tr("dhis2_search_de", l), placeholder="GAM, MUAC...")),
                column(3, br(), actionButton(ns("btn_search_de"), icon("search"), class="btn-sm btn-info"))
              ),
              uiOutput(ns("de_list_ui")),
              hr(),

              # Période
              selectInput(ns("period_type"), tr("dhis2_period_type", l),
                choices=setNames(
                  c("MONTHLY", "QUARTERLY", "YEARLY", "CUSTOM"),
                  c(tr("dhis2_pt_monthly", l), tr("dhis2_pt_quarterly", l),
                    tr("dhis2_pt_yearly", l), tr("dhis2_pt_custom", l))
                )),
              conditionalPanel(
                condition=sprintf("input['%s'] != 'CUSTOM'", ns("period_type")),
                fluidRow(
                  column(6, numericInput(ns("period_year_start"), tr("dhis2_year_start", l),
                                         value=as.integer(format(Sys.Date(),"%Y"))-1,
                                         min=2000, max=2030)),
                  column(6, numericInput(ns("period_year_end"), tr("dhis2_year_end", l),
                                         value=as.integer(format(Sys.Date(),"%Y")),
                                         min=2000, max=2030))
                )
              ),
              conditionalPanel(
                condition=sprintf("input['%s'] == 'CUSTOM'", ns("period_type")),
                textInput(ns("period_custom"), tr("dhis2_period_custom", l),
                          placeholder="202301;202302;202303")
              ),

              hr(),
              tags$p(tags$b(tr("dhis2_output_fmt", l))),
              selectInput(ns("output_format"), NULL,
                choices=setNames(
                  c("table", "pivot", "timeseries"),
                  c(tr("dhis2_fmt_table", l), tr("dhis2_fmt_pivot", l),
                    tr("dhis2_fmt_timeseries", l))
                )),

              actionButton(ns("btn_run_analytics"), tr("dhis2_run_query", l),
                           icon=icon("download"), class="btn-success btn-block btn-lg")
            )
          ),

          column(8,
            box(title=tags$span(icon("table"), " ", tr("dhis2_results", l)),
                status="success", solidHeader=TRUE, width=12,

              fluidRow(
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("an_stat_rows"))), tags$small(tr("db_rows", l)))),
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("an_stat_cols"))), tags$small(tr("db_cols", l)))),
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("an_stat_periods"))), tags$small(tr("dhis2_periods", l)))),
                column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("an_stat_ous"))), tags$small(tr("dhis2_ous_count", l))))
              ),
              br(),
              uiOutput(ns("analytics_msg_ui")),
              DT::dataTableOutput(ns("analytics_table")),
              br(),
              fluidRow(
                column(4, downloadButton(ns("dl_analytics_csv"),   "CSV",   class="btn-sm btn-outline-success")),
                column(4, downloadButton(ns("dl_analytics_excel"), "Excel", class="btn-sm btn-outline-primary")),
                column(4, actionButton(ns("btn_import_dhis2"),   tr("db_import_app", l),
                                       icon=icon("arrow-right"), class="btn-sm btn-warning"))
              )
            ),

            box(title=tags$span(icon("chart-line"), " ", tr("dhis2_chart_preview", l)),
                status="info", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=FALSE,
              plotOutput(ns("analytics_plot"), height="280px")
            )
          )
        )
      ),

      # ── Onglet 3 : Data Value Sets ────────────────────────────────────────
      tabPanel(tr("dhis2_tab_dvs", l), value="dvs", icon=icon("database"),
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("filter"), " ", tr("dhis2_dvs_params", l)),
                status="warning", solidHeader=TRUE, width=12,

              uiOutput(ns("dataset_selector_ui")),
              textInput(ns("dvs_period"), tr("dhis2_dvs_period", l),
                        placeholder="202301", value=format(Sys.Date(), "%Y%m")),
              uiOutput(ns("dvs_ou_selector_ui")),
              checkboxInput(ns("dvs_children"), tr("dhis2_dvs_children", l), value=TRUE),
              actionButton(ns("btn_load_dvs"), tr("dhis2_load_dvs", l),
                           icon=icon("download"), class="btn-warning btn-block")
            )
          ),
          column(8,
            box(title=tags$span(icon("table"), " ", tr("dhis2_dvs_results", l)),
                status="success", solidHeader=TRUE, width=12,
              uiOutput(ns("dvs_msg_ui")),
              DT::dataTableOutput(ns("dvs_table")),
              br(),
              actionButton(ns("btn_import_dvs"), tr("db_import_app", l),
                           icon=icon("arrow-right"), class="btn-warning")
            )
          )
        )
      ),

      # ── Onglet 4 : Tracker / Events ──────────────────────────────────────
      tabPanel(tr("dhis2_tab_tracker", l), value="tracker", icon=icon("user"),
        br(),
        fluidRow(
          column(4,
            box(title=tags$span(icon("user-tag"), " ", tr("dhis2_tracker_params", l)),
                status="info", solidHeader=TRUE, width=12,

              uiOutput(ns("program_selector_ui")),
              textInput(ns("tr_start_date"), tr("dhis2_start_date", l),
                        value=format(Sys.Date()-365, "%Y-%m-%d")),
              textInput(ns("tr_end_date"), tr("dhis2_end_date", l),
                        value=format(Sys.Date(), "%Y-%m-%d")),
              uiOutput(ns("tr_ou_selector_ui")),
              numericInput(ns("tr_page_size"), tr("dhis2_page_size", l),
                           value=500, min=1, max=10000),
              actionButton(ns("btn_load_tracker"), tr("dhis2_load_tracker", l),
                           icon=icon("download"), class="btn-info btn-block")
            )
          ),
          column(8,
            box(title=tags$span(icon("list"), " ", tr("dhis2_tracker_results", l)),
                status="success", solidHeader=TRUE, width=12,
              uiOutput(ns("tracker_msg_ui")),
              DT::dataTableOutput(ns("tracker_table")),
              br(),
              actionButton(ns("btn_import_tracker"), tr("db_import_app", l),
                           icon=icon("arrow-right"), class="btn-warning")
            )
          )
        )
      ),

      # ── Onglet 5 : Métadonnées & Dictionnaire ─────────────────────────────
      tabPanel(tr("dhis2_tab_meta", l), value="meta", icon=icon("book"),
        br(),
        fluidRow(
          column(3,
            box(title=tr("dhis2_meta_type", l), status="primary", solidHeader=TRUE, width=12,
              radioButtons(ns("meta_type"), NULL,
                choices=c(
                  "Organisation Units"   = "organisationUnits",
                  "Data Elements"        = "dataElements",
                  "Indicators"           = "indicators",
                  "Indicator Groups"     = "indicatorGroups",
                  "Data Sets"            = "dataSets",
                  "Programs"             = "programs",
                  "Tracked Entity Types" = "trackedEntityTypes",
                  "Category Combos"      = "categoryCombos",
                  "User Groups"          = "userGroups"
                )
              ),
              actionButton(ns("btn_load_meta"), tr("dhis2_load_meta", l),
                           icon=icon("sync"), class="btn-primary btn-block")
            )
          ),
          column(9,
            box(title=tags$span(icon("list"), " ", tr("dhis2_meta_results", l)),
                status="info", solidHeader=TRUE, width=12,
              fluidRow(
                column(6, textInput(ns("meta_filter"), tr("dhis2_meta_filter", l), placeholder="Recherche...")),
                column(6, br(), downloadButton(ns("dl_meta_csv"), "CSV", class="btn-sm btn-outline-success"))
              ),
              br(),
              DT::dataTableOutput(ns("meta_table"))
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
dhis2Server <- function(id, mapped_data=reactive(NULL),
                         set_mapped_data=NULL, lang=reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    l  <- lang

    # ── State ────────────────────────────────────────────────────────────────
    dhis2_creds   <- reactiveVal(NULL)   # list(url, user, pass)
    dhis2_info    <- reactiveVal(NULL)   # server info JSON
    ou_data       <- reactiveVal(NULL)   # org units loaded
    selected_ous  <- reactiveVal(c())    # UIDs org units sélectionnées
    de_found      <- reactiveVal(NULL)   # éléments/indicateurs trouvés
    selected_des  <- reactiveVal(c())    # UIDs éléments sélectionnés
    analytics_res <- reactiveVal(NULL)   # résultat analytics
    dvs_result    <- reactiveVal(NULL)
    tracker_res   <- reactiveVal(NULL)
    meta_res      <- reactiveVal(NULL)
    datasets_list <- reactiveVal(NULL)
    programs_list <- reactiveVal(NULL)

    # ── Normalisation URL ─────────────────────────────────────────────────
    clean_url <- function(u) {
      u <- trimws(u)
      if (!grepl("/api$", u)) u <- paste0(sub("/+$","",u), "/api")
      u
    }

    # ── Connexion ─────────────────────────────────────────────────────────
    observeEvent(input$btn_dhis2_connect, {
      url  <- clean_url(input$dhis2_url)
      user <- input$dhis2_user
      pass <- input$dhis2_pass
      withProgress(message=tr("dhis2_connecting", l()), value=0.5, {
        tryCatch({
          info <- .dhis2_get(url, "/system/info", user, pass)
          dhis2_creds(list(url=url, user=user, pass=pass))
          dhis2_info(info)
          showNotification(paste0(tr("dhis2_connected_ok", l()), " — ", info$instanceName %||% "DHIS2"),
                           type="message", duration=4)
          # Charger les datasets et programmes en arrière-plan
          tryCatch({
            ds <- .dhis2_get(url, "/dataSets", user, pass,
                             params=list(fields="id,displayName", paging="false"))
            datasets_list(ds$dataSets)
            pr <- .dhis2_get(url, "/programs", user, pass,
                             params=list(fields="id,displayName,programType", paging="false"))
            programs_list(pr$programs)
          }, error=function(e) NULL)
        }, error=function(e) {
          dhis2_creds(NULL)
          dhis2_info(NULL)
          showNotification(paste0(tr("dhis2_conn_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    observeEvent(input$btn_dhis2_disc, {
      dhis2_creds(NULL); dhis2_info(NULL)
      showNotification(tr("dhis2_disconnected", l()), type="warning", duration=3)
    })

    # ── Status UI ────────────────────────────────────────────────────────
    output$dhis2_status_ui <- renderUI({
      cr <- dhis2_creds()
      if (!is.null(cr)) {
        tags$div(class="dhis2-card dhis2-connected",
          icon("check-circle", class="text-success"), " ",
          tags$b(tr("dhis2_connected_ok", l())),
          tags$br(),
          tags$small(class="text-muted", cr$url)
        )
      } else {
        tags$div(class="dhis2-card dhis2-disconn",
          icon("times-circle", class="text-muted"), " ",
          tags$span(class="text-muted", tr("dhis2_not_connected", l()))
        )
      }
    })

    output$dhis2_server_info_ui <- renderUI({
      info <- dhis2_info()
      if (is.null(info)) return(tags$p(class="text-muted", tr("dhis2_no_info", l())))
      tagList(
        tags$p(tags$b(info$instanceName %||% "DHIS2")),
        tags$table(class="table table-sm",
          tags$tr(tags$td(tr("dhis2_version", l())), tags$td(tags$b(info$version %||% "—"))),
          tags$tr(tags$td("Build"),                  tags$td(info$buildTime %||% "—")),
          tags$tr(tags$td("Revision"),               tags$td(info$revision %||% "—")),
          tags$tr(tags$td("Database"),               tags$td(info$databaseInfo$name %||% "—")),
          tags$tr(tags$td("Java"),                   tags$td(info$javaVersion %||% "—"))
        )
      )
    })

    # ── Chargement Org Units ─────────────────────────────────────────────
    observeEvent(input$btn_load_ou, {
      cr  <- dhis2_creds()
      req(!is.null(cr))
      lvl <- input$ou_level
      withProgress(message=tr("dhis2_loading_ou", l()), value=0.5, {
        tryCatch({
          res <- .dhis2_get(cr$url, "/organisationUnits", cr$user, cr$pass,
                            params=list(level=lvl, fields="id,displayName,path,level",
                                        paging="false"))
          ous <- res$organisationUnits
          ou_data(ous)
          showNotification(sprintf(tr("dhis2_ou_loaded", l()), length(ous)), type="message", duration=3)
        }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
      })
    })

    output$ou_list_ui <- renderUI({
      ous <- ou_data()
      flt <- tolower(trimws(input$ou_filter %||% ""))
      if (is.null(ous) || length(ous)==0) return(tags$p(class="text-muted", tr("dhis2_no_ou", l())))
      items <- ous
      if (nzchar(flt))
        items <- Filter(function(o) grepl(flt, tolower(o$displayName), fixed=TRUE), ous)
      tagList(
        tags$div(style="max-height:250px; overflow-y:auto; border:1px solid #dee2e6; border-radius:4px;",
          lapply(head(items, 200), function(o) {
            sel  <- selected_ous()
            is_s <- o$id %in% sel
            tags$div(
              class=paste("dhis2-ou-item", if(is_s) "bg-primary text-white" else ""),
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("ou_click"), o$id),
              icon(if(is_s) "check-square" else "square"), " ", o$displayName,
              tags$small(class=if(is_s) "ml-2 text-white-50" else "ml-2 text-muted", o$id)
            )
          })
        ),
        tags$small(class="text-muted", length(items), " ", tr("dhis2_ous_found", l()))
      )
    })

    observeEvent(input$ou_click, {
      uid <- input$ou_click
      sel <- selected_ous()
      if (uid %in% sel) selected_ous(setdiff(sel, uid))
      else              selected_ous(c(sel, uid))
    })

    output$selected_ous_ui <- renderUI({
      sel <- selected_ous()
      ous <- ou_data()
      if (length(sel)==0) return(tags$p(class="text-muted", tr("dhis2_no_ou_selected", l())))
      names <- if(!is.null(ous)) {
        sapply(sel, function(uid) {
          match <- Filter(function(o) o$id==uid, ous)
          if(length(match)>0) match[[1]]$displayName else uid
        })
      } else sel
      tagList(
        lapply(seq_along(sel), function(i) {
          tags$span(class="dhis2-meta-tag",
            names[i], " ",
            actionLink(ns(paste0("rm_ou_",i)), icon("times"),
                       onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("remove_ou"), sel[i]))
          )
        }),
        br(),
        actionButton(ns("btn_clear_ous"), tr("dhis2_clear_ous", l),
                     class="btn-sm btn-outline-danger mt-1")
      )
    })

    observeEvent(input$remove_ou, { selected_ous(setdiff(selected_ous(), input$remove_ou)) })
    observeEvent(input$btn_clear_ous, { selected_ous(c()) })

    # ── Recherche Data Elements / Indicators ─────────────────────────────
    observeEvent(input$btn_search_de, {
      cr   <- dhis2_creds()
      req(!is.null(cr))
      kw   <- trimws(input$de_search)
      type <- input$data_type
      endpoint <- paste0("/", type)
      params <- list(fields="id,displayName,shortName", paging="false")
      if (nzchar(kw)) params$filter <- paste0("displayName:ilike:", kw)
      withProgress(message=tr("dhis2_searching", l()), value=0.5, {
        tryCatch({
          res  <- .dhis2_get(cr$url, endpoint, cr$user, cr$pass, params=params)
          items <- res[[type]]
          de_found(items)
          showNotification(sprintf(tr("dhis2_de_found", l()), length(items)), type="message", duration=3)
        }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
      })
    })

    output$de_list_ui <- renderUI({
      items <- de_found()
      if (is.null(items) || length(items)==0)
        return(tags$p(class="text-muted text-center mt-2", tr("dhis2_de_hint", l())))
      tagList(
        tags$div(style="max-height:200px; overflow-y:auto; border:1px solid #dee2e6; border-radius:4px; margin-top:6px;",
          lapply(head(items, 100), function(it) {
            sel  <- selected_des()
            is_s <- it$id %in% sel
            tags$div(
              class=paste("dhis2-ou-item", if(is_s) "bg-success text-white" else ""),
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("de_click"), it$id),
              icon(if(is_s) "check-square" else "square"), " ",
              tags$span(style="font-size:12px;", it$displayName),
              tags$small(class=if(is_s) "text-white-50 ml-1" else "text-muted ml-1", it$id)
            )
          })
        ),
        tags$small(class="text-muted", length(items), " ", tr("dhis2_de_found_label", l))
      )
    })

    observeEvent(input$de_click, {
      uid <- input$de_click
      sel <- selected_des()
      if (uid %in% sel) selected_des(setdiff(sel, uid))
      else              selected_des(c(sel, uid))
    })

    # ── Analytics Query ───────────────────────────────────────────────────
    observeEvent(input$btn_run_analytics, {
      cr   <- dhis2_creds()
      req(!is.null(cr))
      sel_des <- selected_des()
      sel_ous <- selected_ous()
      if (length(sel_des)==0) { showNotification(tr("dhis2_no_de_selected", l()), type="warning"); return() }
      if (length(sel_ous)==0) { showNotification(tr("dhis2_no_ou_selected", l()), type="warning"); return() }

      # Construction des périodes
      periods <- .build_dhis2_periods(
        input$period_type, input$period_year_start,
        input$period_year_end, input$period_custom
      )
      if (length(periods)==0) { showNotification(tr("dhis2_no_period", l()), type="warning"); return() }

      dx  <- paste(sel_des, collapse=";")
      ou  <- paste(sel_ous, collapse=";")
      pe  <- paste(periods,  collapse=";")

      withProgress(message=tr("dhis2_running_analytics", l()), value=0.5, {
        tryCatch({
          res <- .dhis2_get(cr$url, "/analytics", cr$user, cr$pass,
                            params=list(
                              dimension = paste0("dx:", dx),
                              dimension2= paste0("ou:", ou),
                              dimension3= paste0("pe:", pe),
                              displayProperty="SHORTNAME",
                              outputIdScheme="CODE",
                              skipMeta="false"
                            ))
          # Reshape en data.frame
          df <- .dhis2_analytics_to_df(res)
          analytics_res(df)
          showNotification(sprintf(tr("dhis2_analytics_ok", l()), nrow(df)), type="message", duration=4)
        }, error=function(e) {
          analytics_res(NULL)
          showNotification(paste0(tr("dhis2_analytics_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    output$an_stat_rows    <- renderUI({ df <- analytics_res(); if(is.null(df)) "—" else format(nrow(df), big.mark=" ") })
    output$an_stat_cols    <- renderUI({ df <- analytics_res(); if(is.null(df)) "—" else ncol(df) })
    output$an_stat_periods <- renderUI({ df <- analytics_res(); if(is.null(df)||!"period"%in%names(df)) "—" else length(unique(df$period)) })
    output$an_stat_ous     <- renderUI({ df <- analytics_res(); if(is.null(df)||!"orgUnit"%in%names(df)) "—" else length(unique(df$orgUnit)) })

    output$analytics_msg_ui <- renderUI({
      df <- analytics_res()
      if (!is.null(df)) tags$div(class="alert alert-success", icon("check"), " ",
                                  sprintf(tr("dhis2_analytics_ok", l()), nrow(df)))
      else NULL
    })

    output$analytics_table <- DT::renderDataTable({
      df <- analytics_res()
      req(!is.null(df))
      DT::datatable(df, options=list(pageLength=15, scrollX=TRUE, dom='Bfrtip',
                                     buttons=c('copy','csv','excel')), rownames=FALSE,
                    class="table table-sm table-striped")
    })

    output$analytics_plot <- renderPlot({
      df <- analytics_res()
      req(!is.null(df), "value" %in% names(df))
      library(ggplot2)
      if ("period" %in% names(df) && "dx" %in% names(df)) {
        ggplot(df, aes(x=period, y=as.numeric(value), color=dx, group=dx)) +
          geom_line(linewidth=1) + geom_point(size=2) +
          labs(x=tr("dhis2_period", l()), y=tr("dhis2_value", l()),
               color=tr("dhis2_indicator", l())) +
          theme_minimal() + theme(axis.text.x=element_text(angle=45, hjust=1))
      } else {
        ggplot(df, aes(x=seq_len(nrow(df)), y=as.numeric(value))) +
          geom_col() + theme_minimal() +
          labs(x="", y=tr("dhis2_value", l()))
      }
    })

    output$dl_analytics_csv <- downloadHandler(
      filename=function() paste0("dhis2_analytics_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
      content =function(file) { df <- analytics_res(); req(!is.null(df)); write.csv(df, file, row.names=FALSE) }
    )
    output$dl_analytics_excel <- downloadHandler(
      filename=function() paste0("dhis2_analytics_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".xlsx"),
      content =function(file) {
        df <- analytics_res(); req(!is.null(df))
        if (requireNamespace("openxlsx", quietly=TRUE)) openxlsx::write.xlsx(df, file)
        else { showNotification("openxlsx requis", type="warning"); write.csv(df, file, row.names=FALSE) }
      }
    )

    observeEvent(input$btn_import_dhis2, {
      df <- analytics_res()
      req(!is.null(df))
      if (!is.null(set_mapped_data)) {
        set_mapped_data(df)
        showNotification(sprintf(tr("db_imported_ok", l()), nrow(df), ncol(df)),
                         type="message", duration=5)
      }
    })

    # ── Data Value Sets ───────────────────────────────────────────────────
    output$dataset_selector_ui <- renderUI({
      ds <- datasets_list()
      if (is.null(ds)) return(textInput(ns("dvs_dataset_id"), tr("dhis2_dataset_id", l()), placeholder="dataSetUID"))
      nms <- sapply(ds, `[[`, "displayName")
      ids <- sapply(ds, `[[`, "id")
      selectInput(ns("dvs_dataset_id"), tr("dhis2_dataset_label", l()), choices=setNames(ids, nms))
    })

    output$dvs_ou_selector_ui <- renderUI({
      ous <- ou_data()
      if (is.null(ous) || length(ous)==0)
        return(textInput(ns("dvs_ou_id"), tr("dhis2_ou_id", l()), placeholder="orgUnitUID"))
      nms <- sapply(ous, `[[`, "displayName")
      ids <- sapply(ous, `[[`, "id")
      selectInput(ns("dvs_ou_id"), tr("dhis2_dvs_ou", l()), choices=setNames(ids, nms), multiple=TRUE)
    })

    observeEvent(input$btn_load_dvs, {
      cr <- dhis2_creds()
      req(!is.null(cr))
      ds_id  <- input$dvs_dataset_id
      period <- input$dvs_period
      ou_ids <- paste(input$dvs_ou_id, collapse=";")
      req(nzchar(ds_id), nzchar(period))
      withProgress(message=tr("dhis2_loading_dvs", l()), value=0.5, {
        tryCatch({
          params <- list(
            dataSet=ds_id, period=period, orgUnit=ou_ids,
            children=tolower(as.character(input$dvs_children))
          )
          res <- .dhis2_get(cr$url, "/dataValueSets", cr$user, cr$pass, params=params)
          dv  <- res$dataValues
          if (is.null(dv) || length(dv)==0) {
            dvs_result(data.frame()); showNotification(tr("dhis2_no_dvs", l()), type="warning"); return()
          }
          df <- do.call(rbind, lapply(dv, function(r) as.data.frame(r, stringsAsFactors=FALSE)))
          dvs_result(df)
          showNotification(sprintf(tr("dhis2_dvs_ok", l()), nrow(df)), type="message", duration=4)
        }, error=function(e) {
          dvs_result(NULL)
          showNotification(conditionMessage(e), type="error", duration=8)
        })
      })
    })

    output$dvs_msg_ui <- renderUI({
      df <- dvs_result()
      if (!is.null(df) && nrow(df) > 0) return(NULL)
      tags$p(class="text-muted", style="font-size:12px;margin:0 0 8px;",
             icon("info-circle"), " ",
             if (identical(l(), "en")) "Set the parameters, then click Load."
             else "Renseignez les paramètres puis cliquez sur Charger.")
    })

    output$dvs_table <- DT::renderDataTable({
      df <- dvs_result(); req(!is.null(df) && nrow(df)>0)
      DT::datatable(df, options=list(pageLength=15, scrollX=TRUE), rownames=FALSE,
                    class="table table-sm table-striped")
    })

    observeEvent(input$btn_import_dvs, {
      df <- dvs_result(); req(!is.null(df) && nrow(df)>0)
      if (!is.null(set_mapped_data)) {
        set_mapped_data(df)
        showNotification(sprintf(tr("db_imported_ok", l()), nrow(df), ncol(df)), type="message", duration=5)
      }
    })

    # ── Tracker / Events ──────────────────────────────────────────────────
    output$program_selector_ui <- renderUI({
      pr <- programs_list()
      if (is.null(pr)) return(textInput(ns("tr_program_id"), tr("dhis2_program_id", l()), placeholder="programUID"))
      nms <- sapply(pr, `[[`, "displayName")
      ids <- sapply(pr, `[[`, "id")
      selectInput(ns("tr_program_id"), tr("dhis2_program_label", l()), choices=setNames(ids, nms))
    })

    output$tr_ou_selector_ui <- renderUI({
      ous <- ou_data()
      if (is.null(ous) || length(ous)==0)
        return(textInput(ns("tr_ou_id"), tr("dhis2_ou_id", l()), placeholder="orgUnitUID"))
      nms <- sapply(ous, `[[`, "displayName")
      ids <- sapply(ous, `[[`, "id")
      selectInput(ns("tr_ou_id"), tr("dhis2_tracker_ou", l()), choices=setNames(ids, nms))
    })

    observeEvent(input$btn_load_tracker, {
      cr      <- dhis2_creds()
      req(!is.null(cr))
      prog_id <- input$tr_program_id
      ou_id   <- input$tr_ou_id
      req(nzchar(prog_id), nzchar(ou_id))
      withProgress(message=tr("dhis2_loading_tracker", l()), value=0.5, {
        tryCatch({
          # Events API
          params <- list(
            program    = prog_id,
            orgUnit    = ou_id,
            startDate  = input$tr_start_date,
            endDate    = input$tr_end_date,
            pageSize   = input$tr_page_size,
            paging     = "false"
          )
          res  <- .dhis2_get(cr$url, "/events", cr$user, cr$pass, params=params)
          evts <- res$events
          if (is.null(evts) || length(evts)==0) {
            tracker_res(data.frame()); showNotification(tr("dhis2_no_events", l()), type="warning"); return()
          }
          # Flatten des dataValues dans chaque événement
          df <- do.call(rbind, lapply(evts, function(e) {
            base <- data.frame(
              event      = e$event %||% NA,
              status     = e$status %||% NA,
              program    = e$program %||% NA,
              orgUnit    = e$orgUnit %||% NA,
              eventDate  = e$eventDate %||% NA,
              stringsAsFactors=FALSE
            )
            dv <- e$dataValues
            if (!is.null(dv) && length(dv)>0) {
              vals <- setNames(sapply(dv, `[[`, "value"), sapply(dv, `[[`, "dataElement"))
              base <- cbind(base, as.data.frame(t(vals), stringsAsFactors=FALSE))
            }
            base
          }))
          tracker_res(df)
          showNotification(sprintf(tr("dhis2_tracker_ok", l()), nrow(df)), type="message", duration=4)
        }, error=function(e) {
          tracker_res(NULL)
          showNotification(conditionMessage(e), type="error", duration=8)
        })
      })
    })

    output$tracker_msg_ui <- renderUI({
      df <- tracker_res()
      if (!is.null(df) && nrow(df) > 0) return(NULL)
      tags$p(class="text-muted", style="font-size:12px;margin:0 0 8px;",
             icon("info-circle"), " ",
             if (identical(l(), "en")) "Set the dates and organisation unit, then click Load."
             else "Renseignez les dates et l'unité d'organisation puis cliquez sur Charger.")
    })

    output$tracker_table <- DT::renderDataTable({
      df <- tracker_res(); req(!is.null(df) && nrow(df)>0)
      DT::datatable(df, options=list(pageLength=15, scrollX=TRUE), rownames=FALSE,
                    class="table table-sm table-striped")
    })

    observeEvent(input$btn_import_tracker, {
      df <- tracker_res(); req(!is.null(df) && nrow(df)>0)
      if (!is.null(set_mapped_data)) {
        set_mapped_data(df)
        showNotification(sprintf(tr("db_imported_ok", l()), nrow(df), ncol(df)), type="message", duration=5)
      }
    })

    # ── Métadonnées ────────────────────────────────────────────────────────
    observeEvent(input$btn_load_meta, {
      cr   <- dhis2_creds()
      req(!is.null(cr))
      type <- input$meta_type
      withProgress(message=tr("dhis2_loading_meta", l()), value=0.5, {
        tryCatch({
          res   <- .dhis2_get(cr$url, paste0("/", type), cr$user, cr$pass,
                              params=list(fields="id,displayName,shortName,code,description",
                                         paging="false"))
          items <- res[[type]]
          if (is.null(items) || length(items)==0) {
            meta_res(data.frame()); return()
          }
          df <- do.call(rbind, lapply(items, function(it) {
            data.frame(id=it$id%||%NA, displayName=it$displayName%||%NA,
                       shortName=it$shortName%||%NA, code=it$code%||%NA,
                       stringsAsFactors=FALSE)
          }))
          meta_res(df)
          showNotification(sprintf(tr("dhis2_meta_ok", l()), nrow(df)), type="message", duration=3)
        }, error=function(e) showNotification(conditionMessage(e), type="error", duration=6))
      })
    })

    meta_filtered <- reactive({
      df  <- meta_res()
      flt <- tolower(trimws(input$meta_filter %||% ""))
      if (is.null(df) || nrow(df)==0) return(NULL)
      if (!nzchar(flt)) return(df)
      idx <- apply(df, 1, function(r) any(grepl(flt, tolower(r), fixed=TRUE)))
      df[idx, , drop=FALSE]
    })

    output$meta_table <- DT::renderDataTable({
      df <- meta_filtered(); req(!is.null(df))
      DT::datatable(df, options=list(pageLength=20, scrollX=TRUE), rownames=FALSE,
                    class="table table-sm table-striped table-hover")
    })

    output$dl_meta_csv <- downloadHandler(
      filename=function() paste0("dhis2_meta_", input$meta_type, "_", format(Sys.time(),"%Y%m%d"), ".csv"),
      content =function(file) { df <- meta_filtered(); req(!is.null(df)); write.csv(df, file, row.names=FALSE) }
    )
  })
}

# ── Helpers ──────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a)>0) a else b

.build_dhis2_periods <- function(type, year_start, year_end, custom) {
  if (type == "CUSTOM") {
    strsplit(trimws(custom), ";")[[1]]
  } else {
    years <- seq(year_start, year_end)
    switch(type,
      "YEARLY"    = as.character(years),
      "MONTHLY"   = unlist(lapply(years, function(y) sprintf("%d%02d", y, 1:12))),
      "QUARTERLY" = unlist(lapply(years, function(y) sprintf("%dQ%d", y, 1:4))),
      as.character(years)
    )
  }
}

.dhis2_analytics_to_df <- function(res) {
  rows <- res$rows
  hdrs <- res$headers
  if (is.null(rows) || length(rows)==0) return(data.frame())
  col_names <- sapply(hdrs, `[[`, "name")
  df <- as.data.frame(
    do.call(rbind, lapply(rows, function(r) setNames(as.data.frame(r, stringsAsFactors=FALSE), col_names))),
    stringsAsFactors=FALSE
  )
  # Résoudre les noms via metaData si disponible
  meta <- res$metaData$items
  if (!is.null(meta)) {
    for (col in c("dx","pe","ou")) {
      if (col %in% names(df)) {
        df[[paste0(col,"_name")]] <- sapply(df[[col]], function(uid) {
          nm <- meta[[uid]]$name
          if (!is.null(nm)) nm else uid
        })
      }
    }
  }
  df
}
