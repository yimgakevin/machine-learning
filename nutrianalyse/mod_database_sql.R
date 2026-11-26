# =============================================================================
# mod_database_sql.R — Module Connexion Bases de Données SQL
# NutriAnalyse Pro v2 | YIMGA WANDJI KEVIN | DSAMID
# Supports: SQLite, PostgreSQL, MySQL/MariaDB, SQL Server (ODBC)
# =============================================================================

# ── Packages requis : DBI, RSQLite, RPostgres, RMariaDB, odbc, DT ─────────

databaseSQLUI <- function(id, lang = "fr") {
  ns <- NS(id)
  l  <- lang

  tagList(
    tags$head(
      tags$style(HTML("
        .db-connection-card { border-left: 4px solid #007bff; background:#f8f9ff; padding:12px; border-radius:6px; margin-bottom:10px; }
        .db-connected        { border-left-color: #28a745 !important; }
        .db-error            { border-left-color: #dc3545 !important; }
        .sql-editor          { font-family: 'Courier New', monospace; font-size: 13px; }
        .schema-badge        { display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; font-weight:600; }
        .badge-table         { background:#007bff22; color:#007bff; }
        .badge-view          { background:#6610f222; color:#6610f2; }
        .db-stat-box         { text-align:center; padding:10px; background:#fff; border-radius:8px;
                               box-shadow:0 1px 4px rgba(0,0,0,.1); }
        .db-stat-num         { font-size:22px; font-weight:700; color:#007bff; }
        .db-history-item     { padding:6px 10px; cursor:pointer; border-bottom:1px solid #eee; font-size:12px;
                               font-family:'Courier New',monospace; }
        .db-history-item:hover { background:#f0f4ff; }
      "))
    ),

    fluidRow(
      column(12,
        tags$div(
          style="background:linear-gradient(90deg,#007bff,#0056d2); color:#fff; padding:18px 22px; border-radius:10px; margin-bottom:20px;",
          tags$h3(style="margin:0; font-weight:700;",
            icon("database"), " ", tr("db_title", l)),
          tags$p(style="margin:4px 0 0; opacity:.85; font-size:13px;",
            tr("db_subtitle", l))
        )
      )
    ),

    fluidRow(
      # ── Panneau Connexion ────────────────────────────────────────────────
      column(4,
        box(title=tags$span(icon("plug"), " ", tr("db_connection", l)),
            status="primary", solidHeader=TRUE, width=12, collapsible=TRUE,

          selectInput(ns("db_type"), tr("db_type", l),
            choices = c(
              "SQLite"          = "sqlite",
              "PostgreSQL"      = "postgres",
              "MySQL/MariaDB"   = "mysql",
              "SQL Server (ODBC)"= "sqlserver",
              "Oracle (ODBC)"   = "oracle"
            )
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'sqlite'", ns("db_type")),
            fileInput(ns("sqlite_file"), tr("db_sqlite_file", l),
                      accept = c(".db", ".sqlite", ".sqlite3")),
            tags$small(class="text-muted", tr("db_sqlite_hint", l))
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] != 'sqlite'", ns("db_type")),
            textInput(ns("db_host"), tr("db_host", l), value = "localhost"),
            fluidRow(
              column(8, textInput(ns("db_name"), tr("db_dbname", l), value = "")),
              column(4, numericInput(ns("db_port"), tr("db_port", l), value = 5432, min=1, max=65535))
            ),
            textInput(ns("db_user"), tr("db_user", l), value = ""),
            passwordInput(ns("db_pass"), tr("db_pass", l), value = ""),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'sqlserver' || input['%s'] == 'oracle'", ns("db_type"), ns("db_type")),
              textInput(ns("db_dsn"), tr("db_dsn", l), value = "", placeholder = "DSN ODBC (optionnel)")
            )
          ),

          fluidRow(
            column(6, actionButton(ns("btn_connect"), tr("db_connect", l),
                                   icon=icon("link"), class="btn-success btn-block")),
            column(6, actionButton(ns("btn_disconnect"), tr("db_disconnect", l),
                                   icon=icon("unlink"), class="btn-danger btn-block"))
          ),
          br(),
          uiOutput(ns("conn_status_ui"))
        ),

        # ── Explorateur de schéma ─────────────────────────────────────────
        box(title=tags$span(icon("sitemap"), " ", tr("db_schema", l)),
            status="info", solidHeader=TRUE, width=12, collapsible=TRUE,

          uiOutput(ns("schema_tables_ui")),
          br(),
          uiOutput(ns("schema_cols_ui"))
        )
      ),

      # ── Editeur SQL + Résultats ──────────────────────────────────────────
      column(8,
        box(title=tags$span(icon("code"), " ", tr("db_query_editor", l)),
            status="primary", solidHeader=TRUE, width=12,

          fluidRow(
            column(9,
              textAreaInput(ns("sql_query"), NULL,
                value = "SELECT * FROM ma_table LIMIT 100;",
                rows = 8, width = "100%",
                placeholder = "Entrez votre requête SQL ici..."
              ) |> tagAppendAttributes(class = "sql-editor")
            ),
            column(3,
              tags$div(style="padding-top:5px;",
                actionButton(ns("btn_run_query"), tr("db_run_query", l),
                             icon=icon("play"), class="btn-primary btn-block"),
                br(),
                actionButton(ns("btn_clear_query"), tr("db_clear", l),
                             icon=icon("eraser"), class="btn-secondary btn-block"),
                br(),
                downloadButton(ns("btn_export_csv"), tr("db_export_csv", l),
                               class="btn-outline-success btn-block"),
                br(),
                actionButton(ns("btn_import_app"), tr("db_import_app", l),
                             icon=icon("arrow-right"), class="btn-warning btn-block")
              )
            )
          ),

          # Templates de requêtes rapides
          tags$div(
            tags$small(tags$b(tr("db_templates", l), ": ")),
            actionLink(ns("tpl_select"), "SELECT *", class="badge badge-primary mr-1"),
            actionLink(ns("tpl_count"),  "COUNT",    class="badge badge-info mr-1"),
            actionLink(ns("tpl_join"),   "JOIN",     class="badge badge-secondary mr-1"),
            actionLink(ns("tpl_group"),  "GROUP BY", class="badge badge-success mr-1"),
            actionLink(ns("tpl_create"), "CREATE TABLE", class="badge badge-warning mr-1")
          )
        ),

        # ── Résultats ────────────────────────────────────────────────────
        box(title=tags$span(icon("table"), " ", tr("db_results", l)),
            status="success", solidHeader=TRUE, width=12, collapsible=TRUE,

          fluidRow(
            column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("stat_rows"))), tags$small(tr("db_rows", l)))),
            column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("stat_cols"))), tags$small(tr("db_cols", l)))),
            column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("stat_time"))), tags$small(tr("db_exec_time", l)))),
            column(3, div(class="db-stat-box", div(class="db-stat-num", uiOutput(ns("stat_status"))), tags$small(tr("db_status", l))))
          ),
          br(),
          uiOutput(ns("query_message_ui")),
          DT::dataTableOutput(ns("query_results_table"))
        ),

        # ── Historique des requêtes ──────────────────────────────────────
        box(title=tags$span(icon("history"), " ", tr("db_history", l)),
            status="warning", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=TRUE,

          uiOutput(ns("query_history_ui")),
          actionButton(ns("btn_clear_history"), tr("db_clear_history", l),
                       icon=icon("trash"), class="btn-outline-danger btn-sm")
        )
      )
    ),

    # ── Onglet : Écriture / Import vers DB ──────────────────────────────────
    fluidRow(
      column(12,
        box(title=tags$span(icon("upload"), " ", tr("db_write_title", l)),
            status="warning", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=TRUE,

          fluidRow(
            column(4,
              textInput(ns("write_table_name"), tr("db_table_name", l), value=""),
              selectInput(ns("write_mode"), tr("db_write_mode", l),
                choices=c("overwrite","append","fail"), selected="overwrite"),
              checkboxInput(ns("write_rownames"), tr("db_rownames", l), value=FALSE)
            ),
            column(4,
              tags$p(tags$b(tr("db_source_data", l))),
              verbatimTextOutput(ns("write_preview_info")),
              actionButton(ns("btn_write_db"), tr("db_write_btn", l),
                           icon=icon("database"), class="btn-warning")
            ),
            column(4,
              tags$p(tags$b(tr("db_write_info", l))),
              tags$ul(
                tags$li(tr("db_write_info1", l)),
                tags$li(tr("db_write_info2", l)),
                tags$li(tr("db_write_info3", l))
              )
            )
          ),
          uiOutput(ns("write_status_ui"))
        )
      )
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================
databaseSQLServer <- function(id, mapped_data = reactive(NULL),
                               set_mapped_data = NULL, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    l  <- lang

    # ── State réactif ───────────────────────────────────────────────────────
    conn_state   <- reactiveVal(NULL)   # objet DBI connexion
    query_result <- reactiveVal(NULL)   # dernier résultat SELECT
    query_hist   <- reactiveVal(c())    # historique des requêtes
    exec_time_ms <- reactiveVal(NA)
    query_status <- reactiveVal("")

    # ── Connexion ───────────────────────────────────────────────────────────
    observeEvent(input$btn_connect, {
      req(input$db_type)
      withProgress(message = tr("db_connecting", l()), value=0.5, {
        tryCatch({
          con <- switch(input$db_type,
            "sqlite" = {
              req(input$sqlite_file)
              if (!requireNamespace("RSQLite", quietly=TRUE))
                stop("Package RSQLite requis. Installez-le avec install.packages('RSQLite')")
              DBI::dbConnect(RSQLite::SQLite(), dbname = input$sqlite_file$datapath)
            },
            "postgres" = {
              if (!requireNamespace("RPostgres", quietly=TRUE))
                stop("Package RPostgres requis. Installez-le avec install.packages('RPostgres')")
              DBI::dbConnect(RPostgres::Postgres(),
                host     = input$db_host,
                dbname   = input$db_name,
                user     = input$db_user,
                password = input$db_pass,
                port     = input$db_port)
            },
            "mysql" = {
              if (!requireNamespace("RMariaDB", quietly=TRUE))
                stop("Package RMariaDB requis. Installez-le avec install.packages('RMariaDB')")
              DBI::dbConnect(RMariaDB::MariaDB(),
                host     = input$db_host,
                dbname   = input$db_name,
                user     = input$db_user,
                password = input$db_pass,
                port     = input$db_port)
            },
            "sqlserver" = , "oracle" = {
              if (!requireNamespace("odbc", quietly=TRUE))
                stop("Package odbc requis. Installez-le avec install.packages('odbc')")
              if (nzchar(input$db_dsn)) {
                DBI::dbConnect(odbc::odbc(), dsn=input$db_dsn,
                               UID=input$db_user, PWD=input$db_pass)
              } else {
                DBI::dbConnect(odbc::odbc(),
                               Driver   = ifelse(input$db_type=="sqlserver","SQL Server","Oracle"),
                               Server   = input$db_host,
                               Database = input$db_name,
                               UID      = input$db_user,
                               PWD      = input$db_pass,
                               Port     = input$db_port)
              }
            }
          )
          conn_state(con)
          query_status(tr("db_connected_ok", l()))
          showNotification(tr("db_connected_ok", l()), type="message", duration=3)
        }, error = function(e) {
          conn_state(NULL)
          query_status(paste0(tr("db_conn_error", l()), ": ", conditionMessage(e)))
          showNotification(paste0(tr("db_conn_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    # ── Déconnexion ─────────────────────────────────────────────────────────
    observeEvent(input$btn_disconnect, {
      con <- conn_state()
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
        conn_state(NULL)
        query_status(tr("db_disconnected", l()))
        showNotification(tr("db_disconnected", l()), type="warning", duration=3)
      }
    })

    # ── Statut connexion UI ──────────────────────────────────────────────────
    output$conn_status_ui <- renderUI({
      con <- conn_state()
      if (!is.null(con) && DBI::dbIsValid(con)) {
        tags$div(class="db-connection-card db-connected",
          icon("check-circle", class="text-success"), " ",
          tags$b(tr("db_connected_ok", l())),
          tags$br(),
          tags$small(class="text-muted",
            tr("db_type", l()), ": ", toupper(input$db_type), " | ",
            format(Sys.time(), "%H:%M:%S"))
        )
      } else {
        tags$div(class="db-connection-card",
          icon("times-circle", class="text-muted"), " ",
          tags$span(class="text-muted", tr("db_not_connected", l()))
        )
      }
    })

    # ── Explorateur de schéma ───────────────────────────────────────────────
    tables_list <- reactive({
      con <- conn_state()
      req(!is.null(con), DBI::dbIsValid(con))
      tryCatch(DBI::dbListTables(con), error=function(e) character(0))
    })

    output$schema_tables_ui <- renderUI({
      tbls <- tables_list()
      if (length(tbls) == 0) {
        return(tags$p(class="text-muted", tr("db_no_tables", l())))
      }
      tagList(
        tags$p(tags$b(tr("db_tables", l()), " (", length(tbls), ")")),
        tags$div(style="max-height:200px; overflow-y:auto;",
          lapply(tbls, function(t) {
            actionLink(ns(paste0("tbl_", gsub("[^a-zA-Z0-9]","_",t))),
              tags$span(icon("table"), " ", t),
              class="db-history-item d-block",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                ns("selected_table"), t
              )
            )
          })
        )
      )
    })

    output$schema_cols_ui <- renderUI({
      con <- conn_state()
      tbl <- input$selected_table
      if (is.null(con) || !DBI::dbIsValid(con) || is.null(tbl) || !nzchar(tbl)) return(NULL)
      tryCatch({
        info <- DBI::dbGetQuery(con,
          paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(con, tbl), " LIMIT 0"))
        types <- sapply(info, class)
        tags$div(
          tags$p(tags$b(tr("db_columns", l()), ": ", tbl)),
          tags$div(style="max-height:150px; overflow-y:auto;",
            lapply(seq_along(types), function(i) {
              tags$div(style="display:flex; justify-content:space-between; padding:2px 4px;",
                tags$span(icon("columns"), " ", names(types)[i]),
                tags$span(class="schema-badge badge-table", types[i])
              )
            })
          ),
          actionButton(ns("btn_preview_table"), tr("db_preview", l()),
                       icon=icon("eye"), class="btn-sm btn-outline-primary mt-1")
        )
      }, error=function(e) NULL)
    })

    # Click preview table
    observeEvent(input$btn_preview_table, {
      tbl <- input$selected_table
      if (!is.null(tbl) && nzchar(tbl)) {
        updateTextAreaInput(session, "sql_query",
          value = paste0("SELECT * FROM ", tbl, " LIMIT 100;"))
      }
    })

    # ── Templates de requêtes ────────────────────────────────────────────────
    observeEvent(input$tpl_select, {
      tbl <- if (!is.null(input$selected_table) && nzchar(input$selected_table)) input$selected_table else "ma_table"
      updateTextAreaInput(session, "sql_query", value=paste0("SELECT *\nFROM ", tbl, "\nLIMIT 100;"))
    })
    observeEvent(input$tpl_count, {
      tbl <- if (!is.null(input$selected_table) && nzchar(input$selected_table)) input$selected_table else "ma_table"
      updateTextAreaInput(session, "sql_query", value=paste0("SELECT COUNT(*) AS n\nFROM ", tbl, ";"))
    })
    observeEvent(input$tpl_join, {
      updateTextAreaInput(session, "sql_query",
        value="SELECT a.*, b.col2\nFROM table_a a\nINNER JOIN table_b b ON a.id = b.id\nLIMIT 100;")
    })
    observeEvent(input$tpl_group, {
      tbl <- if (!is.null(input$selected_table) && nzchar(input$selected_table)) input$selected_table else "ma_table"
      updateTextAreaInput(session, "sql_query",
        value=paste0("SELECT col_groupe, COUNT(*) AS n, AVG(col_numerique) AS moyenne\nFROM ", tbl, "\nGROUP BY col_groupe\nORDER BY n DESC;"))
    })
    observeEvent(input$tpl_create, {
      updateTextAreaInput(session, "sql_query",
        value="CREATE TABLE nouvelle_table (\n  id INTEGER PRIMARY KEY,\n  nom TEXT NOT NULL,\n  valeur REAL,\n  date_mesure DATE\n);")
    })

    # ── Exécution requête ────────────────────────────────────────────────────
    observeEvent(input$btn_run_query, {
      con  <- conn_state()
      sql  <- trimws(input$sql_query)
      if (is.null(con) || !DBI::dbIsValid(con)) {
        showNotification(tr("db_not_connected", l()), type="error"); return()
      }
      if (!nzchar(sql)) {
        showNotification(tr("db_empty_query", l()), type="warning"); return()
      }

      # Ajouter à l'historique
      hist <- query_hist()
      query_hist(unique(c(sql, hist))[1:min(20, length(c(sql, hist)))])

      withProgress(message=tr("db_running", l()), value=0.5, {
        tryCatch({
          t0  <- proc.time()["elapsed"]
          # Détection SELECT vs DML
          is_select <- grepl("^\\s*(SELECT|WITH|SHOW|DESCRIBE|EXPLAIN|PRAGMA)", sql, ignore.case=TRUE)
          if (is_select) {
            res <- DBI::dbGetQuery(con, sql)
            query_result(res)
            exec_time_ms(round((proc.time()["elapsed"] - t0) * 1000, 1))
            query_status(tr("db_query_ok", l()))
            showNotification(sprintf(tr("db_rows_returned", l()), nrow(res)), type="message", duration=3)
          } else {
            rows <- DBI::dbExecute(con, sql)
            query_result(NULL)
            exec_time_ms(round((proc.time()["elapsed"] - t0) * 1000, 1))
            query_status(sprintf(tr("db_rows_affected", l()), rows))
            showNotification(sprintf(tr("db_rows_affected", l()), rows), type="message", duration=3)
          }
        }, error=function(e) {
          query_result(NULL)
          query_status(paste0(tr("db_query_error", l()), ": ", conditionMessage(e)))
          showNotification(paste0(tr("db_query_error", l()), ": ", conditionMessage(e)),
                           type="error", duration=8)
        })
      })
    })

    observeEvent(input$btn_clear_query, {
      updateTextAreaInput(session, "sql_query", value="")
    })

    # ── Statistiques résultat ────────────────────────────────────────────────
    output$stat_rows   <- renderUI({ df <- query_result(); if(is.null(df)) "—" else format(nrow(df), big.mark=" ") })
    output$stat_cols   <- renderUI({ df <- query_result(); if(is.null(df)) "—" else ncol(df) })
    output$stat_time   <- renderUI({ t <- exec_time_ms(); if(is.na(t)) "—" else paste0(t, " ms") })
    output$stat_status <- renderUI({
      s <- query_status()
      if (grepl("error|erreur", s, ignore.case=TRUE)) icon("times-circle", class="text-danger fa-lg")
      else if (nzchar(s)) icon("check-circle", class="text-success fa-lg")
      else "—"
    })

    output$query_message_ui <- renderUI({
      s <- query_status()
      if (!nzchar(s)) return(NULL)
      cl <- if (grepl("error|erreur", s, ignore.case=TRUE)) "alert alert-danger" else "alert alert-success"
      tags$div(class=cl, style="padding:8px; margin-bottom:8px;", s)
    })

    # ── Table résultats ──────────────────────────────────────────────────────
    output$query_results_table <- DT::renderDataTable({
      df <- query_result()
      if (is.null(df)) return(NULL)
      DT::datatable(df,
        options = list(
          pageLength=15, scrollX=TRUE, scrollY="350px",
          lengthMenu=c(10,15,25,50,100),
          dom='Bfrtip',
          buttons=c('copy','csv','excel')
        ),
        rownames=FALSE,
        class="table table-sm table-striped table-hover"
      )
    })

    # ── Export CSV ──────────────────────────────────────────────────────────
    output$btn_export_csv <- downloadHandler(
      filename = function() paste0("query_result_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        df <- query_result()
        req(!is.null(df))
        write.csv(df, file, row.names=FALSE, fileEncoding="UTF-8")
      }
    )

    # ── Importer dans l'app ──────────────────────────────────────────────────
    observeEvent(input$btn_import_app, {
      df <- query_result()
      if (is.null(df) || nrow(df)==0) {
        showNotification(tr("db_no_data_import", l()), type="warning"); return()
      }
      if (!is.null(set_mapped_data)) {
        set_mapped_data(df)
        showNotification(
          sprintf(tr("db_imported_ok", l()), nrow(df), ncol(df)),
          type="message", duration=5)
      }
    })

    # ── Historique ──────────────────────────────────────────────────────────
    output$query_history_ui <- renderUI({
      hist <- query_hist()
      if (length(hist)==0) return(tags$p(class="text-muted", tr("db_no_history", l())))
      tagList(
        lapply(seq_along(hist), function(i) {
          q <- hist[i]
          tags$div(class="db-history-item",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("hist_click"), i),
            tags$span(class="text-muted", paste0("#", i, " ")),
            substr(q, 1, 80), if(nchar(q)>80) "…"
          )
        })
      )
    })

    observeEvent(input$hist_click, {
      idx  <- input$hist_click
      hist <- query_hist()
      if (!is.null(idx) && idx <= length(hist))
        updateTextAreaInput(session, "sql_query", value=hist[idx])
    })

    observeEvent(input$btn_clear_history, {
      query_hist(c())
    })

    # ── Ecriture vers DB ────────────────────────────────────────────────────
    output$write_preview_info <- renderText({
      df <- mapped_data()
      if (is.null(df)) return(tr("db_no_source", l()))
      sprintf("%d lignes × %d colonnes", nrow(df), ncol(df))
    })

    observeEvent(input$btn_write_db, {
      con   <- conn_state()
      df    <- mapped_data()
      tname <- trimws(input$write_table_name)
      if (is.null(con) || !DBI::dbIsValid(con)) {
        showNotification(tr("db_not_connected", l()), type="error"); return()
      }
      if (is.null(df) || nrow(df)==0) {
        showNotification(tr("db_no_source", l()), type="warning"); return()
      }
      if (!nzchar(tname)) {
        showNotification(tr("db_table_name_required", l()), type="warning"); return()
      }
      withProgress(message=tr("db_writing", l()), value=0.5, {
        tryCatch({
          DBI::dbWriteTable(con, name=tname, value=df,
                            overwrite=(input$write_mode=="overwrite"),
                            append   =(input$write_mode=="append"),
                            row.names=input$write_rownames)
          output$write_status_ui <- renderUI({
            tags$div(class="alert alert-success",
              icon("check"), " ",
              sprintf(tr("db_write_ok", l()), nrow(df), tname))
          })
          showNotification(sprintf(tr("db_write_ok", l()), nrow(df), tname),
                           type="message", duration=5)
        }, error=function(e) {
          output$write_status_ui <- renderUI({
            tags$div(class="alert alert-danger",
              icon("times"), " ", conditionMessage(e))
          })
          showNotification(conditionMessage(e), type="error", duration=8)
        })
      })
    })

    # Déconnexion propre à la fermeture de session
    session$onSessionEnded(function() {
      con <- conn_state()
      if (!is.null(con) && tryCatch(DBI::dbIsValid(con), error=function(e) FALSE))
        DBI::dbDisconnect(con)
    })
  })
}
