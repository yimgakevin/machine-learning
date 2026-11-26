# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_audit_trail.R — Audit Trail & Journal d'Analyse Horodaté             ║
# ║  Traçabilité complète des actions utilisateur                              ║
# ║  Niveau Expert — NutriAnalyse Pro v2                                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── UI ─────────────────────────────────────────────────────────────────────────
auditTrailUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    # Bandeau
    tags$div(
      style = paste0(
        "background:linear-gradient(135deg,#00426A 0%,#2c3e50 50%,#34495e 100%);",
        "color:white;padding:24px 28px;border-radius:12px;margin-bottom:20px;",
        "box-shadow:0 4px 20px rgba(0,66,106,.3);"
      ),
      fluidRow(
        column(7,
          tags$h2(style="margin:0;font-weight:800;font-size:22px;",
                  icon("history"), " Journal d'Audit & Traçabilité"),
          tags$p(style="margin:6px 0 0;font-size:13px;opacity:.9;",
                 "Historique horodaté de toutes les actions — Reproductibilité institutionnelle"),
          tags$p(style="margin:4px 0 0;font-size:11px;opacity:.7;",
                 "Chaque import, calcul, filtre et export est enregistré automatiquement")
        ),
        column(5, style="text-align:right;",
          uiOutput(ns("audit_counter")),
          tags$div(style="margin-top:8px;",
            downloadButton(ns("audit_export_csv"), "Exporter CSV",
                           style="background:rgba(255,255,255,.15);border:1px solid rgba(255,255,255,.3);color:white;font-size:11px;padding:6px 14px;border-radius:16px;margin-right:6px;"),
            downloadButton(ns("audit_export_json"), "Exporter JSON",
                           style="background:rgba(255,255,255,.15);border:1px solid rgba(255,255,255,.3);color:white;font-size:11px;padding:6px 14px;border-radius:16px;margin-right:6px;"),
            actionButton(ns("audit_clear"), "Vider le journal",
                         icon=icon("trash"),
                         style="background:rgba(201,22,29,.3);border:1px solid rgba(255,255,255,.3);color:white;font-size:11px;padding:6px 14px;border-radius:16px;")
          )
        )
      )
    ),

    # Statistiques de session
    fluidRow(
      column(3,
        tags$div(style="background:white;border-radius:10px;padding:16px;text-align:center;box-shadow:0 2px 8px rgba(0,0,0,.06);",
          tags$div(style="font-size:28px;font-weight:800;color:#008DC9;", textOutput(ns("stat_total"), inline=TRUE)),
          tags$div(style="font-size:12px;color:#666;", "Actions totales")
        )
      ),
      column(3,
        tags$div(style="background:white;border-radius:10px;padding:16px;text-align:center;box-shadow:0 2px 8px rgba(0,0,0,.06);",
          tags$div(style="font-size:28px;font-weight:800;color:#4dac26;", textOutput(ns("stat_imports"), inline=TRUE)),
          tags$div(style="font-size:12px;color:#666;", "Imports")
        )
      ),
      column(3,
        tags$div(style="background:white;border-radius:10px;padding:16px;text-align:center;box-shadow:0 2px 8px rgba(0,0,0,.06);",
          tags$div(style="font-size:28px;font-weight:800;color:#e88400;", textOutput(ns("stat_analyses"), inline=TRUE)),
          tags$div(style="font-size:12px;color:#666;", "Analyses")
        )
      ),
      column(3,
        tags$div(style="background:white;border-radius:10px;padding:16px;text-align:center;box-shadow:0 2px 8px rgba(0,0,0,.06);",
          tags$div(style="font-size:28px;font-weight:800;color:#c9161d;", textOutput(ns("stat_duration"), inline=TRUE)),
          tags$div(style="font-size:12px;color:#666;", "Durée session")
        )
      )
    ),

    tags$br(),

    # Tabs: Journal détaillé / Timeline / Hash données
    tabsetPanel(id=ns("audit_tabs"), type="pills",
      tabPanel(title=tagList(icon("list"), " Journal Détaillé"),
        fluidRow(
          column(3,
            selectInput(ns("filter_module"), "Filtrer par module :",
                        choices=c("Tous"="all"), width="100%")
          ),
          column(3,
            selectInput(ns("filter_action"), "Filtrer par action :",
                        choices=c("Toutes"="all"), width="100%")
          ),
          column(3,
            dateRangeInput(ns("filter_date"), "Période :",
                           start=Sys.Date(), end=Sys.Date(), width="100%",
                           language="fr", separator="à")
          ),
          column(3, style="padding-top:25px;",
            actionButton(ns("filter_apply"), "Filtrer", icon=icon("filter"),
                         class="btn-primary btn-sm")
          )
        ),
        DTOutput(ns("audit_table")),
        tags$br()
      ),

      tabPanel(title=tagList(icon("stream"), " Timeline"),
        uiOutput(ns("audit_timeline"))
      ),

      tabPanel(title=tagList(icon("fingerprint"), " Intégrité Données"),
        fluidRow(
          box(width=12, status="info", solidHeader=TRUE,
              title=tags$span(icon("shield-alt"), " Hash & Intégrité des Données"),
              uiOutput(ns("data_integrity_panel")),
              tags$hr(),
              tags$h5(style="color:#00426A;font-weight:600;", "Historique des versions de données"),
              DTOutput(ns("data_hash_history"))
          )
        )
      ),

      tabPanel(title=tagList(icon("chart-pie"), " Résumé Session"),
        fluidRow(
          box(width=6, status="primary", solidHeader=TRUE,
              title="Actions par Module",
              plotlyOutput(ns("audit_by_module"), height="350px")
          ),
          box(width=6, status="warning", solidHeader=TRUE,
              title="Activité dans le Temps",
              plotlyOutput(ns("audit_timeline_chart"), height="350px")
          )
        )
      )
    )
  )
}

# ── SERVER ─────────────────────────────────────────────────────────────────────
auditTrailServer <- function(id, df_f, mapped_data, audit_log_rv, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session_start <- Sys.time()

    # ── Réactifs auxiliaires ──────────────────────────────────────────

    # Hashes de données
    data_hashes <- reactiveVal(list())

    # Observer les changements de données pour le hash
    observeEvent(mapped_data(), {
      d <- mapped_data()
      if (is.null(d)) return()

      hash_val <- digest::digest(d, algo="md5")
      current <- data_hashes()
      current[[length(current)+1]] <- list(
        timestamp = Sys.time(),
        hash = hash_val,
        nrow = nrow(d),
        ncol = ncol(d),
        size_kb = round(object.size(d) / 1024, 1)
      )
      data_hashes(current)
    })

    # ── Stats de session ──────────────────────────────────────────────
    output$stat_total <- renderText({
      log <- audit_log_rv()
      length(log)
    })

    output$stat_imports <- renderText({
      log <- audit_log_rv()
      sum(sapply(log, function(x) grepl("import|upload|kobo", x$action, ignore.case=TRUE)))
    })

    output$stat_analyses <- renderText({
      log <- audit_log_rv()
      sum(sapply(log, function(x) grepl("calc|analysis|test|model|audit|prevalence|zscore",
                                         x$action, ignore.case=TRUE)))
    })

    output$stat_duration <- renderText({
      invalidateLater(60000, session)
      diff <- difftime(Sys.time(), session_start, units="mins")
      if (diff < 60) paste0(round(diff), " min")
      else paste0(round(as.numeric(diff)/60, 1), " h")
    })

    output$audit_counter <- renderUI({
      log <- audit_log_rv()
      n <- length(log)
      tags$span(
        style="background:rgba(255,255,255,.2);padding:8px 16px;border-radius:20px;font-size:14px;font-weight:600;",
        icon("clipboard-list"), paste0(" ", n, " action(s) enregistrée(s)")
      )
    })

    # ── Mettre à jour les filtres dynamiquement ────────────────────────
    observe({
      log <- audit_log_rv()
      if (length(log) == 0) return()

      modules <- unique(sapply(log, function(x) x$module %||% "Inconnu"))
      actions <- unique(sapply(log, function(x) x$action %||% "Inconnu"))

      updateSelectInput(session, "filter_module",
                        choices=c("Tous"="all", setNames(modules, modules)))
      updateSelectInput(session, "filter_action",
                        choices=c("Toutes"="all", setNames(actions, actions)))
    })

    # ── Données filtrées du journal ────────────────────────────────────
    filtered_log <- reactive({
      log <- audit_log_rv()
      if (length(log) == 0) return(data.frame())

      df_log <- do.call(rbind, lapply(seq_along(log), function(i) {
        entry <- log[[i]]
        data.frame(
          N = i,
          Horodatage = format(entry$timestamp, "%Y-%m-%d %H:%M:%S"),
          Module = entry$module %||% "—",
          Action = entry$action %||% "—",
          Details = entry$details %||% "—",
          stringsAsFactors=FALSE
        )
      }))

      # Appliquer filtres
      if (!is.null(input$filter_module) && input$filter_module != "all") {
        df_log <- df_log[df_log$Module == input$filter_module,]
      }
      if (!is.null(input$filter_action) && input$filter_action != "all") {
        df_log <- df_log[df_log$Action == input$filter_action,]
      }

      df_log
    })

    # ── Table du journal ──────────────────────────────────────────────
    output$audit_table <- renderDT({
      df_log <- filtered_log()
      if (nrow(df_log) == 0) {
        return(datatable(
          data.frame(Message="Aucune action enregistrée"),
          rownames=FALSE, options=list(dom="t")
        ))
      }
      # Ordre décroissant (plus récent en haut)
      df_log <- df_log[order(-df_log$N),]

      datatable(df_log, rownames=FALSE, filter="top",
                options=list(pageLength=25, dom="frtip", scrollX=TRUE,
                             language=list(search="Rechercher :")),
                class="compact stripe hover") |>
        formatStyle("Module",
                    color=styleEqual(
                      unique(df_log$Module),
                      rep(c("#008DC9","#c9161d","#4dac26","#e88400","#640000","#1D9E75"),
                          length.out=length(unique(df_log$Module)))),
                    fontWeight="bold") |>
        formatStyle("Action",
                    backgroundColor=styleEqual(
                      c("import","export","error","quality_audit","dashboard_refresh"),
                      c("#e3f2fd","#e8f5e9","#ffebee","#ede7f6","#fff3e0")))
    })

    # ── Timeline visuelle ─────────────────────────────────────────────
    output$audit_timeline <- renderUI({
      log <- audit_log_rv()
      if (length(log) == 0) {
        return(tags$div(style="text-align:center;padding:40px;color:#888;",
          icon("clock"), " Aucune action enregistrée"))
      }

      # Grouper par heure
      timeline_items <- lapply(rev(log), function(entry) {
        icon_name <- switch(entry$action %||% "other",
          import = "upload",
          export = "download",
          filter = "filter",
          calculation = "calculator",
          navigation = "compass",
          quality_audit = "shield-alt",
          dashboard_refresh = "sync-alt",
          temporal_test = "chart-line",
          "circle"
        )
        color <- switch(entry$action %||% "other",
          import = "#008DC9",
          export = "#4dac26",
          error = "#c9161d",
          quality_audit = "#6c3483",
          temporal_test = "#e88400",
          "#00426A"
        )

        tags$div(
          style="display:flex;margin-bottom:12px;",
          # Dot + line
          tags$div(style="width:50px;text-align:center;flex-shrink:0;",
            tags$div(style=paste0("width:36px;height:36px;border-radius:50%;background:", color,
                                  ";display:inline-flex;align-items:center;justify-content:center;"),
              icon(icon_name, style="color:white;font-size:14px;")
            ),
            tags$div(style=paste0("width:2px;height:20px;background:", color,
                                  ";opacity:0.3;margin:0 auto;"))
          ),
          # Content
          tags$div(style="flex:1;background:white;border-radius:8px;padding:10px 14px;box-shadow:0 1px 4px rgba(0,0,0,.06);border-left:3px solid ",
            tags$div(
              style=paste0("border-left-color:", color, ";"),
              tags$div(style="display:flex;justify-content:space-between;",
                tags$strong(style=paste0("color:", color, ";font-size:13px;"),
                            entry$module %||% "Système"),
                tags$span(style="font-size:11px;color:#999;",
                          format(entry$timestamp, "%H:%M:%S"))
              ),
              tags$div(style="font-size:12px;color:#333;margin-top:4px;",
                        entry$details %||% entry$action),
              tags$span(style=paste0("display:inline-block;margin-top:4px;padding:2px 8px;",
                                     "border-radius:10px;font-size:10px;background:", color,
                                     ";color:white;opacity:0.8;"),
                        entry$action %||% "action")
            )
          )
        )
      })

      tags$div(style="max-height:600px;overflow-y:auto;padding:10px;",
               tagList(timeline_items))
    })

    # ── Intégrité des données ─────────────────────────────────────────
    output$data_integrity_panel <- renderUI({
      hashes <- data_hashes()
      d <- mapped_data()

      if (is.null(d)) {
        return(tags$div(style="text-align:center;padding:30px;color:#888;",
          "Aucune donnée chargée"))
      }

      current_hash <- digest::digest(d, algo="md5")
      last_hash <- if(length(hashes) > 0) hashes[[length(hashes)]]$hash else NULL
      changed <- !is.null(last_hash) && last_hash != current_hash

      tags$div(
        style=paste0("background:", if(changed) "#fff3e0" else "#e8f5e9",
                     ";border-radius:10px;padding:16px;"),
        fluidRow(
          column(1, style="text-align:center;padding-top:4px;",
            icon(if(changed) "exclamation-triangle" else "lock",
                 style=paste0("font-size:28px;color:", if(changed) "#e88400" else "#4dac26", ";"))
          ),
          column(7,
            tags$strong(style="font-size:14px;",
                        if(changed) "Données modifiées depuis le dernier hash"
                        else "Données intactes — Intégrité vérifiée"),
            tags$br(),
            tags$code(style="font-size:11px;color:#666;", paste0("MD5: ", current_hash))
          ),
          column(4, style="text-align:right;font-size:12px;color:#555;",
            tags$div(paste0(format(nrow(d), big.mark=" "), " lignes × ", ncol(d), " colonnes")),
            tags$div(paste0("Taille: ", round(object.size(d)/1024, 1), " Ko")),
            tags$div(paste0(length(hashes), " version(s) enregistrée(s)"))
          )
        )
      )
    })

    output$data_hash_history <- renderDT({
      hashes <- data_hashes()
      if (length(hashes) == 0) {
        return(datatable(data.frame(Message="Aucun historique"), rownames=FALSE, options=list(dom="t")))
      }

      df_hash <- do.call(rbind, lapply(seq_along(hashes), function(i) {
        h <- hashes[[i]]
        data.frame(
          Version = i,
          Horodatage = format(h$timestamp, "%Y-%m-%d %H:%M:%S"),
          Hash_MD5 = h$hash,
          Lignes = h$nrow,
          Colonnes = h$ncol,
          Taille_Ko = h$size_kb,
          stringsAsFactors=FALSE
        )
      }))

      datatable(df_hash, rownames=FALSE, options=list(pageLength=10, dom="t", scrollX=TRUE),
                class="compact stripe hover")
    })

    # ── Graphiques résumé ─────────────────────────────────────────────
    output$audit_by_module <- renderPlotly({
      log <- audit_log_rv()
      if (length(log) == 0) return(plotly_empty() |> layout(title="Aucune donnée"))

      modules <- sapply(log, function(x) x$module %||% "Autre")
      df_mod <- as.data.frame(table(Module=modules), stringsAsFactors=FALSE)
      names(df_mod)[2] <- "Count"

      p <- ggplot(df_mod, aes(y=reorder(Module, Count), x=Count)) +
        geom_col(fill="#008DC9", alpha=0.85, width=0.6) +
        labs(x="Nombre d'actions", y="") +
        theme_minimal(base_size=12)

      ggplotly(p)
    })

    output$audit_timeline_chart <- renderPlotly({
      log <- audit_log_rv()
      if (length(log) == 0) return(plotly_empty() |> layout(title="Aucune donnée"))

      times <- sapply(log, function(x) format(x$timestamp, "%H:%M"))
      df_time <- as.data.frame(table(Heure=times), stringsAsFactors=FALSE)
      names(df_time)[2] <- "Actions"

      p <- ggplot(df_time, aes(x=Heure, y=Actions, group=1)) +
        geom_line(color="#008DC9", linewidth=1) +
        geom_point(color="#00426A", size=2) +
        labs(x="Heure", y="Actions") +
        theme_minimal(base_size=11) +
        theme(axis.text.x=element_text(angle=45, hjust=1, size=8))

      ggplotly(p)
    })

    # ── Exports ───────────────────────────────────────────────────────
    output$audit_export_csv <- downloadHandler(
      filename = function() {
        paste0("Audit_Trail_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        log <- audit_log_rv()
        if (length(log) == 0) {
          write.csv(data.frame(Message="Journal vide"), file, row.names=FALSE)
          return()
        }
        df_log <- do.call(rbind, lapply(log, function(entry) {
          data.frame(
            Horodatage = format(entry$timestamp, "%Y-%m-%d %H:%M:%S"),
            Module = entry$module %||% "",
            Action = entry$action %||% "",
            Details = entry$details %||% "",
            stringsAsFactors=FALSE
          )
        }))
        write.csv(df_log, file, row.names=FALSE)

        # Auto-log this export
        if (is.function(audit_log_rv)) {
          # Note: can't easily self-log in downloadHandler
        }
      }
    )

    output$audit_export_json <- downloadHandler(
      filename = function() {
        paste0("Audit_Trail_", format(Sys.time(), "%Y%m%d_%H%M"), ".json")
      },
      content = function(file) {
        log <- audit_log_rv()
        json_data <- lapply(log, function(entry) {
          list(
            timestamp = format(entry$timestamp, "%Y-%m-%dT%H:%M:%S"),
            module = entry$module %||% "",
            action = entry$action %||% "",
            details = entry$details %||% ""
          )
        })
        writeLines(jsonlite::toJSON(json_data, pretty=TRUE, auto_unbox=TRUE), file)
      }
    )

    # ── Vider le journal ──────────────────────────────────────────────
    observeEvent(input$audit_clear, {
      showModal(modalDialog(
        title = "Confirmer la suppression",
        tags$p("Êtes-vous sûr de vouloir vider le journal d'audit ?"),
        tags$p(style="color:#c9161d;font-weight:600;", "Cette action est irréversible."),
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("confirm_clear"), "Supprimer", class="btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_clear, {
      audit_log_rv(list())
      data_hashes(list())
      removeModal()
      showNotification("Journal d'audit vidé", type="warning", duration=3)
    })

  })
}

cat("mod_audit_trail.R chargé — Audit Trail & Traçabilité Expert\n")
