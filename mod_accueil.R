# Module Shiny — Accueil / tableau de bord (KPI, alertes, Sécurité alimentaire)
# Les filtres f_* restent globaux (df_f les lit au scope racine) : le renderUI
# du panneau de filtres ne leur applique donc PAS ns(). event_data('plotly_click',
# source='accueil_prev') et les update*() des filtres sont fournis par le root.

# Extrait de app_parts/ui/ui_body.R — onglet accueil
mod_accueil_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "accueil",
        # ═══ Bandeau hero premium ═══
        tags$div(
          style = paste0(
            "background:linear-gradient(135deg,#0B3954 0%,#0C4A6E 35%,#0077B6 65%,#00838F 100%);",
            "color:white;padding:28px 32px;border-radius:16px;margin-bottom:22px;",
            "box-shadow:0 6px 30px rgba(0,66,106,.25);position:relative;overflow:hidden;"
          ),
          # Motif décoratif
          tags$div(style="position:absolute;top:-20px;right:-20px;width:200px;height:200px;border-radius:50%;background:rgba(255,255,255,.05);"),
          tags$div(style="position:absolute;bottom:-40px;right:80px;width:120px;height:120px;border-radius:50%;background:rgba(255,255,255,.03);"),
          fluidRow(
            column(8,
              tags$h2(style="margin:0;font-weight:800;font-size:26px;letter-spacing:.3px;",
                      icon("heartbeat"), " NutriAnalyse Pro v2"),
              tags$p(style="margin:8px 0 0;font-size:15px;opacity:.92;font-weight:300;",
                     "Plateforme d'analyse nutritionnelle — DSAMID Data Science & Analysis"),
              tags$p(style="margin:4px 0 0;font-size:12px;opacity:.7;",
                     "Importez n'importe quel jeu de données \u2192 Analyse automatique complète")
            ),
            column(4, style="text-align:right;position:relative;z-index:1;",
              uiOutput(ns("accueil_badge"))
            )
          )
        ),

        # ═══ Panneau de filtres du tableau de bord ═══
        tags$div(
          class = "nutri-chart-card",
          style = "margin-bottom:16px;padding:0;overflow:hidden;",
          tags$div(
            style = "display:flex;align-items:center;gap:8px;padding:14px 18px;",
            icon("filter"),
            tags$span(style="font-weight:700;color:#0B3954;", "Filtres du tableau de bord"),
            tags$span(style="margin-left:auto;font-size:11px;font-weight:500;color:#64748b;text-align:right;",
                      uiOutput(ns("dash_filter_counter"), inline = TRUE)),
            actionButton(ns("btn_reset_filters"), "↺", title = "Réinitialiser tous les filtres",
              style = "background:#eef6fb;color:#0B3954;border:1px solid #bfd9e8;border-radius:5px;font-size:12px;padding:4px 10px;")
          ),
          tags$details(
            id = "dashboard_filter_details",
            tags$summary(
              style = "cursor:pointer;list-style:none;padding:0 18px 10px;font-size:11px;color:#64748b;",
              icon("chevron-down"), " Afficher / masquer les filtres"
            ),
            tags$div(
              style = "padding:0 18px 16px;border-top:1px solid #e2e8f0;",
              uiOutput(ns("dashboard_filters_label")),
              uiOutput(ns("dashboard_filters"))
            )
          )
        ),

        # ═══ Score de situation global (barre horizontale) ═══
        uiOutput(ns("accueil_score_bar")),

        # ═══ KPI universels (s'adaptent aux données) ═══
        uiOutput(ns("accueil_kpi_premium")),

        # ═══ KPI Qualité des données ═══
        uiOutput(ns("accueil_kpi_qualite")),

        # ═══ KPI Sécurité alimentaire (si disponible) ═══
        uiOutput(ns("accueil_kpi_secu")),

        # ═══ Alertes OMS (design toast) ═══
        uiOutput(ns("accueil_alertes")),

        # ═══ Graphiques de synthèse ═══
        fluidRow(
          column(8,
            tags$div(class="nutri-chart-card",
              tags$div(class="chart-title", icon("chart-bar"), " Prévalences par groupe (IC 95% Wilson) · cliquez une barre pour filtrer"),
              plotlyOutput(ns("accueil_prev_chart"), height="400px")
            )
          ),
          column(4,
            tags$div(class="nutri-chart-card",
              tags$div(class="chart-title", icon("tachometer-alt"), " Jauge GAM — Seuil OMS"),
              plotlyOutput(ns("accueil_gauge"), height="280px"),
              uiOutput(ns("accueil_seuil_badge"))
            )
          )
        ),

        # ═══ Z-Scores + Tableau de référence ═══
        fluidRow(
          column(7,
            tags$div(class="nutri-chart-card",
              tags$div(class="chart-title", icon("chart-area"), " Distribution Z-scores vs référence OMS N(0,1)"),
              plotlyOutput(ns("accueil_zscore"), height="340px")
            )
          ),
          column(5,
            tags$div(class="nutri-chart-card",
              tags$div(class="chart-title", icon("table"), " Seuils de référence OMS/SMART"),
              tags$table(
                style="width:100%;font-size:12px;border-collapse:separate;border-spacing:0 4px;",
                tags$thead(tags$tr(
                  lapply(c("","GAM","Sévérité","Action"),function(h)
                    tags$th(style="padding:8px 10px;background:#0B3954;color:white;font-weight:600;font-size:11px;text-transform:uppercase;letter-spacing:.5px;",
                            if(h=="") "" else h))
                )),
                tags$tbody(
                  tags$tr(style="background:#f0fdf4;border-radius:6px;",
                    tags$td(style="padding:8px 10px;border-radius:6px 0 0 6px;",
                      tags$span(style="display:inline-block;width:12px;height:12px;border-radius:50%;background:#2E7D52;")),
                    tags$td(style="padding:8px 10px;font-weight:700;color:#166534;","< 5 %"),
                    tags$td(style="padding:8px 10px;font-weight:600;","Acceptable"),
                    tags$td(style="padding:8px 10px;font-size:11px;border-radius:0 6px 6px 0;","Surveillance routinière")),
                  tags$tr(style="background:#fefce8;",
                    tags$td(style="padding:8px 10px;border-radius:6px 0 0 6px;",
                      tags$span(style="display:inline-block;width:12px;height:12px;border-radius:50%;background:#D4A843;")),
                    tags$td(style="padding:8px 10px;font-weight:700;color:#854d0e;","5 – 9,9 %"),
                    tags$td(style="padding:8px 10px;font-weight:600;","Alerte"),
                    tags$td(style="padding:8px 10px;font-size:11px;border-radius:0 6px 6px 0;","Surveillance renforcée")),
                  tags$tr(style="background:#fff7ed;",
                    tags$td(style="padding:8px 10px;border-radius:6px 0 0 6px;",
                      tags$span(style="display:inline-block;width:12px;height:12px;border-radius:50%;background:#D98B14;")),
                    tags$td(style="padding:8px 10px;font-weight:700;color:#9a3412;","10 – 14,9 %"),
                    tags$td(style="padding:8px 10px;font-weight:600;","Sérieux"),
                    tags$td(style="padding:8px 10px;font-size:11px;border-radius:0 6px 6px 0;","Intervention requise")),
                  tags$tr(style="background:#fef2f2;",
                    tags$td(style="padding:8px 10px;border-radius:6px 0 0 6px;",
                      tags$span(style="display:inline-block;width:12px;height:12px;border-radius:50%;background:#BF3636;")),
                    tags$td(style="padding:8px 10px;font-weight:700;color:#991b1b;","15 – 29,9 %"),
                    tags$td(style="padding:8px 10px;font-weight:600;","Critique"),
                    tags$td(style="padding:8px 10px;font-size:11px;border-radius:0 6px 6px 0;","Urgence humanitaire")),
                  tags$tr(style="background:#450a0a;color:white;",
                    tags$td(style="padding:8px 10px;border-radius:6px 0 0 6px;",
                      tags$span(style="display:inline-block;width:12px;height:12px;border-radius:50%;background:#fca5a5;")),
                    tags$td(style="padding:8px 10px;font-weight:700;","\u2265 30 %"),
                    tags$td(style="padding:8px 10px;font-weight:600;","Urgence max"),
                    tags$td(style="padding:8px 10px;font-size:11px;border-radius:0 6px 6px 0;","Crise grave"))
                )
              )
            )
          )
        ),

        # ═══ Barre de génération de rapport ═══
        uiOutput(ns("accueil_rapport_bar")),

        # ═══ Sécurité Alimentaire — rCSI & HDDS (conditionnel) ═══
        uiOutput(ns("accueil_sa_charts_row")),

        # ═══ Cohérence FCS × rCSI — Nuage de points ═══
        uiOutput(ns("accueil_fcs_rcsi_scatter")),

        # ═══ Analyse temporelle FCS & rCSI par round ═══
        uiOutput(ns("accueil_temporal_row")),

        # ═══ Tableau croisé rCSI × FCS — Double vulnérabilité ═══
        uiOutput(ns("accueil_crosstab_row")),

        # ═══ Carte choroplèthe — Double vulnérabilité par région ═══
        uiOutput(ns("accueil_dv_map_row"))

      )
}

mod_accueil_server <- function(id, df_f, mapped_data, map_cols, vars_num,
                               vars_cat, filter_context, has_oms, f_region,
                               accueil_click, parent_session, gpal, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

      observeEvent(accueil_click(), {
        click <- accueil_click()
        key <- click$key
        req(is.character(key), length(key) == 1, nzchar(key))
        if (!is.null(f_region())) {
          updateSelectInput(parent_session, "f_region", selected = key)
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
                  downloadButton(ns("dl_rcsi_csv"),
                    label = " T\u00e9l\u00e9charger CSV",
                    icon  = icon("download"),
                    style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                    class = "btn-sm btn-outline-secondary"
                  )
                ),
                plotlyOutput(ns("accueil_rcsi_chart"), height="320px")
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
                  downloadButton(ns("dl_hdds_csv"),
                    label = " T\u00e9l\u00e9charger CSV",
                    icon  = icon("download"),
                    style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                    class = "btn-sm btn-outline-secondary"
                  )
                ),
                plotlyOutput(ns("accueil_hdds_chart"), height="320px")
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
                selectInput(inputId = ns("sa_grp_var"),
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
                selectInput(ns("fcs_rcsi_color_var"), label = NULL,
                            choices = col_choices, selected = col_choices[1], width = "100%")
              ),
              downloadButton(ns("dl_fcs_rcsi_csv"),
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
                plotlyOutput(ns("accueil_fcs_rcsi_plot"), height = "440px")
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
                selectInput(ns("temporal_grp_var"), label = NULL,
                            choices = grp_choices, selected = sel, width = "100%")
              ),
              downloadButton(ns("dl_temporal_csv"),
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
                  plotlyOutput(ns("accueil_fcs_temporal"), height = "360px")
                )
              ),
            if (has_rcsi)
              column(if (has_fcs) 6L else 12L,
                tags$div(class = "nutri-chart-card",
                  tags$div(class = "chart-title",
                           icon("hand-holding-heart"), " Tendance rCSI \u2014 Score moyen par round"),
                  plotlyOutput(ns("accueil_rcsi_temporal"), height = "360px")
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
                selectInput(ns("crosstab_grp_var"), label = NULL,
                            choices = grp_choices, selected = sel, width = "100%")
              ),
              downloadButton(ns("dl_crosstab_csv"),
                label = " CSV",
                icon  = icon("download"),
                style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                class = "btn-sm btn-outline-secondary"
              ),
              downloadButton(ns("dl_crosstab_xlsx"),
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
                DT::DTOutput(ns("accueil_crosstab_table"))
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
              selectInput(ns("dv_map_level"),
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
              downloadButton(ns("dl_dv_map_csv"),
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
                  uiOutput(ns("accueil_dv_map_note")),
                  leafletOutput(ns("accueil_dv_map"), height = "500px")
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
            downloadButton(ns("dl_rapport_word"),
              label = tagList(icon("file-word"),   " Word (.docx)"),
              style = paste0(
                "background:white; color:#0B3954; border:none; font-weight:700;",
                "font-size:12px; padding:7px 14px; border-radius:8px;"
              )
            ),
            downloadButton(ns("dl_rapport_html"),
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
          uiOutput(ns("f_active_badge"))
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
          updateSelectInput(parent_session, "f_region", selected="Toutes")
        if (!is.null(m$dept) && m$dept %in% names(df))
          updateSelectInput(parent_session, "f_dept", selected="Tous")
        updateSelectizeInput(parent_session, "f_zone",    selected=character(0))
        updateSelectizeInput(parent_session, "f_cluster",       selected=character(0))
        updateSelectizeInput(parent_session, "f_strate",        selected=character(0))
        updateSelectizeInput(parent_session, "f_survey_round",  selected=character(0))

        # § Démographie
        if (!is.null(m$sexe) && m$sexe %in% names(df))
          updateSelectInput(parent_session, "f_sexe", selected="Tous")
        updateSelectizeInput(parent_session, "f_depl", selected=character(0))
        if (".age" %in% names(df)) {
          r <- range(df$.age, na.rm=TRUE)
          updateSliderInput(parent_session, "f_age", value=c(floor(r[1]), ceiling(r[2])))
        }
        age_grp_col <- detect_col(names(df), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
        if (!is.null(age_grp_col)) {
          all_vals <- sort(unique(na.omit(as.character(df[[age_grp_col]]))))
          updateCheckboxGroupInput(parent_session, "f_age_grp", selected=all_vals)
        }

        # § Indicateurs OMS
        if ("nutr_cat" %in% names(df))
          updateCheckboxGroupInput(parent_session, "f_nutr", selected=c("Normal","MAM","SAM"))
        if ("GAM"         %in% names(df)) updateSelectInput(parent_session, "f_gam",         selected="Tous")
        if ("SAM"         %in% names(df)) updateSelectInput(parent_session, "f_sam",         selected="Tous")
        if ("MAM"         %in% names(df)) updateSelectInput(parent_session, "f_mam",         selected="Tous")
        if ("Stunting"    %in% names(df)) updateSelectInput(parent_session, "f_stunting",    selected="Tous")
        if ("Underweight" %in% names(df)) updateSelectInput(parent_session, "f_underweight", selected="Tous")
        oedeme_col <- detect_col(names(df), c("oedeme","oedema","edema","oedèmes","oedème"))
        if (!is.null(oedeme_col))         updateSelectInput(parent_session, "f_oedeme",      selected="Tous")

        # § Anthropométrie
        for (info in list(list("f_muac",".muac",1), list("f_whz",".whz",.1),
                          list("f_haz",".haz",.1),  list("f_waz",".waz",.1))) {
          id <- info[[1]]; col <- info[[2]]
          if (col %in% names(df)) {
            r <- range(df[[col]], na.rm=TRUE)
            updateSliderInput(parent_session, id, value=r)
          }
        }

        # § Qualité
        updateCheckboxGroupInput(parent_session, "f_excl_na", selected=NULL)
        updateNumericInput(parent_session, "f_top_n", value=0)
        updateCheckboxInput(parent_session, "f_no_outlier", value=FALSE)

        showNotification("✅ Tous les filtres réinitialisés", type="message", duration=2)
      })
      # ── Filtres du dashboard (label réactif) ────────────────────────────────────
      output$dashboard_filters_label <- renderUI({
        tags$small(
          style = "color:#5a9ec4;text-transform:uppercase;font-size:10px;letter-spacing:1px;",
          tr("sidebar_filters", lang())
        )
      })
  })
}
