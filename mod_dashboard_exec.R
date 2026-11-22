# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_dashboard_exec.R — Dashboard Exécutif Premium                        ║
# ║  Design Power BI+ — KPIs OMS & Alertes — NutriAnalyse Pro v2             ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

if (!exists("measure_indicator", mode = "function")) source("measures.R")

# ── UI ─────────────────────────────────────────────────────────────────────────
dashboardExecUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    # ═══ Bandeau hero premium ═══
    tags$div(
      style = paste0(
        "background:linear-gradient(135deg,#00426A 0%,#006fa3 35%,#008DC9 65%,#1D9E75 100%);",
        "color:white;padding:30px 36px;border-radius:16px;margin-bottom:22px;",
        "box-shadow:0 6px 30px rgba(0,66,106,.25);position:relative;overflow:hidden;"
      ),
      tags$div(style="position:absolute;top:-30px;right:-30px;width:220px;height:220px;border-radius:50%;background:rgba(255,255,255,.05);"),
      tags$div(style="position:absolute;bottom:-50px;right:100px;width:140px;height:140px;border-radius:50%;background:rgba(255,255,255,.03);"),
      fluidRow(
        column(7,
          tags$h2(style="margin:0;font-weight:800;font-size:26px;letter-spacing:.3px;",
                  icon("tachometer-alt"), " ", tr("exec_title", lang)),
          tags$p(style="margin:8px 0 0;font-size:15px;opacity:.92;font-weight:300;",
                 tr("exec_subtitle", lang)),
          tags$p(style="margin:4px 0 0;font-size:11px;opacity:.7;",
                 tr("exec_hint", lang))
        ),
        column(5, style="text-align:right;position:relative;z-index:1;",
          uiOutput(ns("exec_status_badge")),
          tags$div(style="margin-top:8px;",
            actionButton(ns("refresh_dashboard"), tr("exec_btn_refresh", lang),
                         icon=icon("sync-alt"),
                         style="background:rgba(255,255,255,.15);border:1px solid rgba(255,255,255,.3);color:white;font-size:12px;padding:7px 18px;border-radius:20px;backdrop-filter:blur(4px);transition:all .2s;")
          )
        )
      )
    ),

    # ═══ Score barre horizontale ═══
    uiOutput(ns("exec_score_global")),

    # ═══ Alertes automatiques OMS (toast-style) ═══
    uiOutput(ns("exec_alertes_panel")),

    # ═══ KPI Anthropométriques ═══
    tags$div(class = "nutri-section-title",
      icon("child"), " ", tr("exec_kpi_anthro", lang)
    ),
    uiOutput(ns("exec_kpi_anthro")),

    # ═══ KPI MUAC & Z-scores ═══
    tags$div(class = "nutri-section-title",
      icon("ruler"), " ", tr("exec_kpi_muac", lang)
    ),
    uiOutput(ns("exec_kpi_muac_z")),

    # ═══ KPI Sécurité alimentaire (si disponible) ═══
    uiOutput(ns("exec_kpi_secu_row")),

    # ═══ Graphiques de synthèse ═══
    tags$div(class = "nutri-section-title",
      icon("chart-bar"), " Synthèse graphique"
    ),
    fluidRow(
      column(7,
        tags$div(class = "nutri-chart-card",
          tags$div(class = "chart-title", icon("chart-bar"), " ", tr("exec_prev_chart", lang)),
          plotlyOutput(ns("exec_prevalence_bars"), height="420px")
        )
      ),
      column(5,
        tags$div(class = "nutri-chart-card",
          tags$div(class = "chart-title", icon("tachometer-alt"), " ", tr("exec_gauges_title", lang)),
          tags$div(class = "nutri-gauges-grid",
            tags$div(class = "nutri-gauge-card", uiOutput(ns("exec_gauge_gam_html"))),
            tags$div(class = "nutri-gauge-card", uiOutput(ns("exec_gauge_sam_html"))),
            tags$div(class = "nutri-gauge-card", uiOutput(ns("exec_gauge_stunting_html"))),
            tags$div(class = "nutri-gauge-card", uiOutput(ns("exec_gauge_muac_html")))
          )
        )
      )
    ),

    # ═══ Heatmap & Priorités ═══
    tags$div(class = "nutri-section-title",
      icon("project-diagram"), " Analyse par zone"
    ),
    fluidRow(
      column(7,
        tags$div(class = "nutri-chart-card",
          tags$div(class = "chart-title", icon("th"), " ", tr("exec_heatmap_title", lang)),
          plotlyOutput(ns("exec_heatmap_zones"), height="450px")
        )
      ),
      column(5,
        tags$div(class = "nutri-chart-card",
          tags$div(class = "chart-title", icon("sort-amount-down"), " ", tr("exec_priority_title", lang)),
          DTOutput(ns("exec_priority_table")),
          tags$hr(style="margin:12px 0;border-color:#e2e8f0;"),
          uiOutput(ns("exec_recommandations"))
        )
      )
    ),

    # ═══ Distribution Z-scores ═══
    tags$div(class = "nutri-section-title",
      icon("chart-area"), " Distribution Z-scores vs référence OMS"
    ),
    tags$div(class = "nutri-chart-card",
      plotlyOutput(ns("exec_zscore_dist"), height="380px")
    )
  )
}

# ── SERVER ─────────────────────────────────────────────────────────────────────
dashboardExecServer <- function(id, df_f, map_cols, mapped_data,
                                has_oms, audit_log = NULL,
                                lang = reactive("fr"),
                                filter_context = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_context <- reactive({
      if (!is.null(filter_context)) {
        ctx <- filter_context()
        if (!is.null(ctx)) return(ctx)
      }
      build_filter_context(df_f(), mapped_data())
    })
    current_data <- reactive({
      ctx <- current_context()
      req(ctx$data)
      ctx$data
    })

    # ── Helpers ──────────────────────────────────────────────
    sev_colors <- list(
      acceptable = list(bg="#dcfce7", border="#4dac26", text="#166534", icon_bg="#4dac26"),
      alerte     = list(bg="#fef9c3", border="#f9c734", text="#854d0e", icon_bg="#f9c734"),
      serieux    = list(bg="#ffedd5", border="#e88400", text="#9a3412", icon_bg="#e88400"),
      critique   = list(bg="#fee2e2", border="#c9161d", text="#991b1b", icon_bg="#c9161d"),
      urgence    = list(bg="#450a0a", border="#640000", text="#fef2f2", icon_bg="#640000")
    )

    make_kpi <- function(value, label, subtitle = "", icon_name = "chart-bar",
                         severity = "acceptable", progress_pct = NULL) {
      sc <- sev_colors[[severity]] %||% sev_colors$acceptable
      progress_html <- if (!is.null(progress_pct)) {
        pct <- min(100, max(0, progress_pct))
        tags$div(class = "kpi-progress",
          tags$div(class = "kpi-progress-fill",
                   style = sprintf("width:%.0f%%;background:%s;", pct, sc$icon_bg)))
      }
      tags$div(class = paste("nutri-kpi-card", paste0("sev-", severity)),
        tags$span(class = "kpi-bg-icon", icon(icon_name)),
        tags$div(class = "kpi-header",
          tags$div(class = "kpi-icon", style = sprintf("background:%s;", sc$icon_bg),
                   icon(icon_name)),
          tags$div(class = "kpi-label", label)
        ),
        tags$div(class = "kpi-value", style = sprintf("color:%s;", sc$text), value),
        if (nzchar(subtitle)) tags$p(class = "kpi-sub", subtitle),
        tags$span(class = paste("kpi-badge", paste0("kpi-badge-", severity)),
                  toupper(severity)),
        progress_html
      )
    }

    # Gauge HTML semicirculaire (rapide, pas de plotly)
    make_gauge_html <- function(value, label, max_val = 50, severity = "acceptable") {
      sc <- sev_colors[[severity]] %||% sev_colors$acceptable
      angle <- min(180, max(0, value / max_val * 180))
      tags$div(class = "nutri-gauge",
        tags$div(class = "nutri-gauge-bg",
          tags$div(class = "nutri-gauge-fill",
                   style = sprintf("background:%s;transform:rotate(%.0fdeg);", sc$icon_bg, angle - 180))
        ),
        tags$div(class = "nutri-gauge-value",
                 style = sprintf("color:%s;", sc$text),
                 paste0(round(value, 1), "%")),
        tags$div(class = "nutri-gauge-label", label)
      )
    }

    # ── Status badge ──
    output$exec_status_badge <- renderUI({
      if (!has_oms()) {
        tags$span(style="background:rgba(255,255,255,.15);padding:8px 16px;border-radius:20px;font-size:12px;",
                  icon("exclamation-circle"), " Données OMS non mappées")
      } else {
        ctx <- current_context(); req(ctx$data)
        tags$div(
          tags$span(style="background:rgba(77,172,38,.3);padding:6px 14px;border-radius:20px;font-size:12px;font-weight:600;",
                    icon("check-circle"), " ",
                    format(ctx$n, big.mark=" "), " / ",
                    format(ctx$n_total, big.mark=" "), " obs. \u00b7 OMS actif"),
          tags$div(style="margin-top:6px;font-size:10px;opacity:.7;",
                   format(Sys.time(), "%d/%m/%Y %H:%M"))
        )
      }
    })

    # ── Refresh ──
    observeEvent(input$refresh_dashboard, {
      if (!is.null(audit_log)) {
        tryCatch(audit_log(list(
          timestamp = Sys.time(), action = "dashboard_refresh",
          module = "Dashboard Exécutif", details = "Actualisation manuelle"
        )), error = function(e) NULL)
      }
      showNotification("\u2705 Dashboard actualisé", type = "message", duration = 2)
    })

    # ═══ Score Global (barre horizontale segmentée) ═══
    output$exec_score_global <- renderUI({
      if (!has_oms()) return(NULL)
      d <- current_data(); req(d)
      score <- measure_global_score(d)
      if (is.null(score)) return(NULL)
      score_global <- score$score
      sev_global <- score$severity
      details <- score$details
      scg <- sev_colors[[sev_global]]
      marker_pct <- 100 - score_global

      # Mini détails par indicateur
      detail_tags <- lapply(details, function(det) {
        dc <- sev_colors[[det$severity]]
        tags$div(style="display:inline-flex;align-items:center;gap:4px;margin-right:16px;",
          tags$span(style=sprintf("width:8px;height:8px;border-radius:50%%;background:%s;", dc$icon_bg)),
          tags$span(style="font-size:11px;font-weight:600;color:#475569;",
                    sprintf("%s : %.1f%%", det$indicator, det$value))
        )
      })

      tags$div(class = "nutri-score-bar-container",
        fluidRow(
          column(2,
            tags$div(style="text-align:center;",
              tags$div(style=sprintf("font-size:48px;font-weight:800;color:%s;line-height:1;", scg$icon_bg),
                       score_global),
              tags$div(style="font-size:11px;font-weight:600;color:#64748b;margin-top:4px;letter-spacing:.5px;",
                       "SCORE GLOBAL"),
              tags$span(class=paste0("kpi-badge kpi-badge-",sev_global),
                        style="margin-top:8px;", toupper(sev_global))
            )
          ),
          column(10,
            tags$div(style="padding-top:8px;",
              tags$div(class = "nutri-score-bar",
                tags$div(class = "nutri-score-marker",
                         style = sprintf("left:%.0f%%;", marker_pct))
              ),
              tags$div(class = "nutri-score-labels",
                tags$span("Urgence"), tags$span("Critique"),
                tags$span("Sérieux"), tags$span("Alerte"),
                tags$span("Acceptable")
              ),
              tags$div(style="margin-top:10px;", tagList(detail_tags))
            )
          )
        )
      )
    })

    # ═══ Alertes (toast-style) ═══
    output$exec_alertes_panel <- renderUI({
      if (!has_oms()) return(NULL)
      d <- current_data(); req(d)
      alert_defs <- list(
        list(col="GAM", label="GAM", seuil_crit=15,
             msg_crit="Intervention nutritionnelle d'urgence requise — programme alimentation ciblée",
             msg_ser="Programme d'alimentation supplémentaire recommandé",
             msg_alert="Surveillance nutritionnelle renforcée recommandée"),
        list(col="SAM", label="SAM", seuil_crit=5,
             msg_crit="Activer centres de stabilisation nutritionnelle",
             msg_ser="Prise en charge communautaire de la MAS requise",
             msg_alert="Dépistage actif MAS à renforcer"),
        list(col="Stunting", label="Retard de croissance", seuil_crit=40,
             msg_crit="Programmes nutrition préventive et WASH prioritaires",
             msg_ser="Interventions à long terme sur alimentation complémentaire",
             msg_alert="Surveillance des pratiques alimentaires du nourrisson")
      )

      alerts <- list()
      for (ad in alert_defs) {
        ms <- measure_indicator(d, ad$col)
        if (is.null(ms)) next
        p <- ms$pct; sev <- ms$severity
        if (is.na(p) || sev == "acceptable") next

        sc <- sev_colors[[sev]]
        msg <- switch(sev,
          urgence = ad$msg_crit, critique = ad$msg_crit,
          serieux = ad$msg_ser, alerte = ad$msg_alert,
          ""
        )
        alert_icon <- switch(sev,
          urgence = "skull-crossbones", critique = "times-circle",
          serieux = "exclamation-circle", alerte = "exclamation-triangle"
        )

        alerts[[length(alerts)+1]] <- tags$div(
          class = "nutri-alert",
          style = sprintf("background:%s;border-left-color:%s;%s",
                          sc$bg, sc$border,
                          if(sev=="urgence") "color:white;" else ""),
          tags$div(class = "nutri-alert-icon",
                   style = sprintf("background:%s;", sc$icon_bg),
                   icon(alert_icon)),
          tags$div(
            tags$div(class = "alert-title",
                     style = sprintf("color:%s;", sc$text),
                     sprintf("%s : %.1f%% — %s", ad$label, p, toupper(sev))),
            tags$div(class = "alert-text",
                     style = if(sev=="urgence") "color:rgba(255,255,255,.8);" else "",
                     msg)
          )
        )
      }

      if (length(alerts) == 0) return(NULL)
      tagList(alerts)
    })

    # ═══ KPI Anthropométriques ═══
    output$exec_kpi_anthro <- renderUI({
      if (!has_oms()) {
        return(tags$div(class="nutri-empty-state", style="padding:30px;",
          tags$div(class="empty-icon", icon("map-signs")),
          tags$h3("Mappez les colonnes OMS"),
          tags$p("Allez dans l'onglet Import et mappez vos colonnes pour activer les KPIs")
        ))
      }
      d <- current_data(); req(d)
      cards <- list()

      for (card in measure_card_specs(d, c("GAM", "SAM", "Stunting", "Underweight"))) {
        ms <- card$measure
        spec <- card$spec
        cards[[length(cards)+1]] <- make_kpi(
          value = paste0(ms$pct, "%"), label = spec$label,
          subtitle = measure_ci_text(ms, include_n = TRUE), icon_name = spec$icon,
          severity = ms$severity,
          progress_pct = min(100, ms$pct / spec$max_pct * 100)
        )
      }

      if (length(cards) == 0) return(tags$p(style="color:#94a3b8;", "Aucun indicateur anthropométrique disponible"))
      tags$div(class = "nutri-kpi-grid", tagList(cards))
    })

    # ═══ KPI MUAC & Z-scores ═══
    output$exec_kpi_muac_z <- renderUI({
      if (!has_oms()) return(NULL)
      d <- current_data(); req(d)
      cards <- list()

      for (card in measure_card_specs(d, c("MUAC_GAM", "MUAC_SAM", ".whz", ".haz"))) {
        ms <- card$measure
        spec <- card$spec
        if (identical(ms$kind, "mean")) {
          value <- ms$mean
          subtitle <- sprintf("± %.2f SD · Réf: 0 ± 1 · n=%d", ms$sd, ms$n)
          progress <- NULL
        } else {
          value <- paste0(ms$pct, "%")
          subtitle <- sprintf("n=%d", ms$n)
          progress <- if (!is.null(spec$max_pct)) min(100, ms$pct / spec$max_pct * 100) else NULL
        }
        cards[[length(cards)+1]] <- make_kpi(
          value = value, label = spec$label, subtitle = subtitle,
          icon_name = spec$icon, severity = ms$severity,
          progress_pct = progress
        )
      }

      if (length(cards) == 0) return(NULL)
      tags$div(class = "nutri-kpi-grid", tagList(cards))
    })

    # ═══ KPI Sécurité alimentaire ═══
    output$exec_kpi_secu_row <- renderUI({
      d <- current_data(); req(d)
      has_fcs <- "FCS" %in% names(d) || "fcs" %in% tolower(names(d))
      has_rcsi <- any(grepl("rcsi|rCSI", names(d), ignore.case=TRUE))
      if (!has_fcs && !has_rcsi) return(NULL)

      cards <- list()
      # FCS
      fcs_col <- names(d)[tolower(names(d)) == "fcs"]
      if (length(fcs_col) > 0) {
        fcs_col <- fcs_col[1]
        m <- round(mean(d[[fcs_col]], na.rm=TRUE), 1)
        poor_pct <- round(mean(d[[fcs_col]] <= 21, na.rm=TRUE) * 100, 1)
        sev <- if(poor_pct >= 40) "critique" else if(poor_pct >= 20) "serieux" else if(poor_pct >= 10) "alerte" else "acceptable"
        cards[[1]] <- make_kpi(
          value = m, label = "FCS moyen (Score Consommation Alimentaire)",
          subtitle = sprintf("Consommation pauvre (\u226421) : %.1f%%", poor_pct),
          icon_name = "utensils", severity = sev
        )
      }
      # rCSI
      rcsi_col <- names(d)[grepl("rcsi", names(d), ignore.case=TRUE)]
      if (length(rcsi_col) > 0) {
        rcsi_col <- rcsi_col[1]
        m <- round(mean(d[[rcsi_col]], na.rm=TRUE), 1)
        high_pct <- round(mean(d[[rcsi_col]] >= 19, na.rm=TRUE) * 100, 1)
        sev <- if(high_pct >= 50) "critique" else if(high_pct >= 30) "serieux" else if(high_pct >= 15) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_kpi(
          value = m, label = "rCSI moyen (Stratégies d'Adaptation)",
          subtitle = sprintf("Niveau élevé (\u226519) : %.1f%%", high_pct),
          icon_name = "hand-holding-heart", severity = sev
        )
      }

      tagList(
        tags$div(class = "nutri-section-title",
          icon("utensils"), " Sécurité alimentaire"),
        tags$div(class = "nutri-kpi-grid", tagList(cards))
      )
    })

    # ═══ Gauges HTML (semicirculaires — rapides) ═══
    for (g_info in list(
      list(out="exec_gauge_gam_html", col="GAM", label="GAM", max=50),
      list(out="exec_gauge_sam_html", col="SAM", label="SAM", max=20),
      list(out="exec_gauge_stunting_html", col="Stunting", label="Stunting", max=60),
      list(out="exec_gauge_muac_html", col="MUAC_GAM", label="MUAC GAM", max=30)
    )) {
      local({
        gi <- g_info
        output[[gi$out]] <- renderUI({
          if (!has_oms()) return(tags$p(style="color:#ccc;text-align:center;font-size:11px;", "N/A"))
          d <- current_data(); req(d)
          if (!gi$col %in% names(d)) return(tags$p(style="color:#ccc;text-align:center;font-size:11px;", "N/A"))
          ms <- measure_indicator(d, gi$col)
          make_gauge_html(ms$pct, gi$label, gi$max, ms$severity)
        })
      })
    }

    # ═══ Prévalence barplot ═══
    output$exec_prevalence_bars <- renderPlotly({
      req(has_oms()); d <- current_data(); m <- map_cols()
      grp_col <- if (!is.null(m$region) && m$region %in% names(d)) m$region else {
        cats <- names(d)[vapply(d, function(x) is.character(x)||is.factor(x), logical(1))]
        if (length(cats) > 0) cats[1] else NULL
      }
      req(!is.null(grp_col))
      inds <- intersect(c("GAM","SAM","Stunting","Underweight"), names(d))
      req(length(inds) > 0)

      res <- d |>
        dplyr::group_by(.data[[grp_col]]) |>
        dplyr::group_modify(~purrr::map_dfr(inds, function(v) calc_prev_wilson(.x, v, v))) |>
        dplyr::ungroup()

      pal <- c(GAM="#c9161d", SAM="#880000", Stunting="#008DC9", Underweight="#7c3aed",
               MAM="#e88400", MUAC_GAM="#e88400")

      p <- ggplot(res, aes(x = .data[[grp_col]], y = `Prév. (%)`,
                            fill = Indicateur,
                            text = paste0(.data[[grp_col]], " — ", Indicateur,
                                          "\n", `Prév. (%)`, "% [", `IC95 bas (%)`, "-", `IC95 haut (%)`, "%]"))) +
        geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
        geom_errorbar(aes(ymin = `IC95 bas (%)`, ymax = `IC95 haut (%)`),
                      position = position_dodge(0.7), width = 0.2, color = "gray40", linewidth = 0.6) +
        geom_hline(yintercept = 15, linetype = "dashed", color = "#c9161d", linewidth = 0.6) +
        annotate("text", x = 0.5, y = 16.5, label = "Seuil critique GAM",
                 hjust = 0, size = 3, color = "#c9161d", fontface = "italic") +
        scale_fill_manual(values = pal) +
        labs(x = NULL, y = "Prévalence (%)", fill = NULL) +
        theme_minimal(base_family = "Source Sans Pro", base_size = 12) +
        theme(legend.position = "bottom",
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.background = element_rect(fill = "white", color = NA))

      ggplotly(p, tooltip = "text") |>
        plotly::layout(legend = list(orientation = "h", y = -0.15))
    })

    # ═══ Heatmap zones ═══
    output$exec_heatmap_zones <- renderPlotly({
      req(has_oms()); d <- current_data(); m <- map_cols()
      grp_col <- if (!is.null(m$region) && m$region %in% names(d)) m$region else {
        cats <- names(d)[vapply(d, function(x) is.character(x)||is.factor(x), logical(1))]
        if (length(cats) > 0) cats[1] else NULL
      }
      req(!is.null(grp_col))
      inds <- intersect(c("GAM","SAM","Stunting","Underweight","MUAC_GAM"), names(d))
      req(length(inds) > 0)

      heat <- d |>
        dplyr::group_by(.data[[grp_col]]) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(inds),
          ~round(mean(.x, na.rm=TRUE)*100, 1)), .groups = "drop")

      mat <- as.matrix(heat[, -1])
      rownames(mat) <- heat[[1]]

      plot_ly(z = mat, x = colnames(mat), y = rownames(mat),
              type = "heatmap",
              colorscale = list(c(0,"#e8f5e9"), c(0.3,"#fffde7"),
                                c(0.5,"#fff3e0"), c(0.7,"#ffcdd2"),
                                c(1,"#880000")),
              hovertemplate = "%{y}<br>%{x}: %{z}%<extra></extra>") |>
        plotly::layout(
          xaxis = list(title = "", tickfont = list(size = 11)),
          yaxis = list(title = "", tickfont = list(size = 11)),
          margin = list(l = 120, b = 60, t = 10)
        )
    })

    # ═══ Priorité table ═══
    output$exec_priority_table <- renderDT({
      req(has_oms()); d <- current_data(); m <- map_cols()
      grp_col <- if (!is.null(m$region) && m$region %in% names(d)) m$region else {
        cats <- names(d)[vapply(d, function(x) is.character(x)||is.factor(x), logical(1))]
        if (length(cats) > 0) cats[1] else NULL
      }
      req(!is.null(grp_col))

      inds <- intersect(c("GAM","SAM","Stunting","Underweight"), names(d))
      req(length(inds) > 0)

      tbl <- d |>
        dplyr::group_by(Zone = .data[[grp_col]]) |>
        dplyr::summarise(
          N = dplyr::n(),
          dplyr::across(dplyr::all_of(inds), ~round(mean(.x, na.rm=TRUE)*100, 1)),
          .groups = "drop"
        )

      # Score de priorité
      tbl$Score <- rowSums(sapply(inds, function(ind) {
        vals <- tbl[[ind]]
        s <- OMS_SEUILS[[ind]]
        if (is.null(s)) return(rep(0, length(vals)))
        ifelse(vals >= s$critique, 4, ifelse(vals >= s$serieux, 3, ifelse(vals >= s$alerte, 2, 1)))
      }))

      tbl <- tbl |> dplyr::arrange(dplyr::desc(Score))
      tbl$Priorité <- ifelse(tbl$Score >= 12, "\U0001f534 URGENTE",
                       ifelse(tbl$Score >= 8, "\U0001f7e0 HAUTE",
                       ifelse(tbl$Score >= 5, "\U0001f7e1 MOYENNE", "\U0001f7e2 BASSE")))

      datatable(tbl, rownames = FALSE,
                options = list(dom = "t", pageLength = 20, scrollY = "250px",
                               columnDefs = list(list(className = "dt-center", targets = "_all"))),
                class = "compact stripe") |>
        formatStyle("Score", fontWeight = "bold",
                    backgroundColor = styleInterval(c(5,8,12), c("#dcfce7","#fef9c3","#fee2e2","#450a0a")),
                    color = styleInterval(c(5,8,12), c("#166534","#854d0e","#991b1b","white")))
    })

    # ═══ Recommandations ═══
    output$exec_recommandations <- renderUI({
      if (!has_oms()) return(NULL)
      d <- current_data(); req(d)
      recs <- list()

      if ("GAM" %in% names(d)) {
        gam <- mean(d$GAM, na.rm=TRUE)*100
        if (gam >= 15) recs[[length(recs)+1]] <- list(
          icon="exclamation-circle", color="#c9161d",
          text="Déclarer l'urgence nutritionnelle — intervention immédiate requise")
        else if (gam >= 10) recs[[length(recs)+1]] <- list(
          icon="exclamation-triangle", color="#e88400",
          text="Mettre en place un programme d'alimentation supplémentaire")
      }
      if ("SAM" %in% names(d) && mean(d$SAM, na.rm=TRUE)*100 >= 2) {
        recs[[length(recs)+1]] <- list(
          icon="heartbeat", color="#c9161d",
          text="Activer les centres de stabilisation nutritionnelle et la PCMA")
      }
      if (".whz" %in% names(d) && sd(d$.whz, na.rm=TRUE) > 1.2) {
        recs[[length(recs)+1]] <- list(
          icon="search", color="#e88400",
          text="Vérifier la qualité des mesures — écart-type WHZ élevé")
      }
      if (length(recs) == 0) {
        recs[[1]] <- list(icon="check-circle", color="#4dac26",
          text="Situation nutritionnelle globalement satisfaisante")
      }

      tagList(lapply(recs, function(r) {
        tags$div(style = sprintf(
          "display:flex;align-items:flex-start;gap:8px;margin-bottom:8px;padding:8px 10px;border-radius:8px;background:%s08;",
          r$color),
          icon(r$icon, style = sprintf("color:%s;margin-top:2px;", r$color)),
          tags$span(style = "font-size:12px;line-height:1.5;", r$text)
        )
      }))
    })

    # ═══ Z-Score distribution ═══
    output$exec_zscore_dist <- renderPlotly({
      d <- current_data(); req(d)
      zscore_cols <- intersect(c(".whz", ".haz", ".waz"), names(d))
      req(length(zscore_cols) > 0)

      labels <- c(.whz="WHZ", .haz="HAZ", .waz="WAZ")
      colors <- c(.whz="#c9161d", .haz="#008DC9", .waz="#4dac26")

      dd <- purrr::map_dfr(zscore_cols, function(col) {
        vals <- na.omit(d[[col]])
        if (length(vals) < 10) return(NULL)
        data.frame(zscore = vals, type = labels[col], stringsAsFactors = FALSE)
      })
      req(nrow(dd) > 0)

      x_ref <- seq(-4.5, 4.5, length.out = 300)
      ref_df <- data.frame(x = x_ref, y = dnorm(x_ref), type = "Réf. N(0,1)")

      p <- ggplot(dd, aes(x = zscore, fill = type, color = type)) +
        geom_density(alpha = 0.25, linewidth = 0.9) +
        geom_line(data = ref_df, aes(x = x, y = y), inherit.aes = FALSE,
                  linetype = "dashed", color = "black", linewidth = 0.7) +
        geom_vline(xintercept = c(-2, -3), linetype = "dotted", color = c("#c9161d", "#640000"), linewidth = 0.5) +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors) +
        labs(x = "Z-score", y = "Densité", fill = NULL, color = NULL) +
        theme_minimal(base_family = "Source Sans Pro", base_size = 12) +
        theme(legend.position = "bottom",
              panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = "white", color = NA))

      ggplotly(p) |>
        plotly::layout(legend = list(orientation = "h", y = -0.15))
    })

  })
}

cat("mod_dashboard_exec.R chargé — Dashboard Exécutif Premium\n")
