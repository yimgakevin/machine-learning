# Extrait de app_parts/ui/ui_body.R — onglet accueil
ui_tab_accueil <- function() {
      tabItem(tabName="accueil",
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
              uiOutput("accueil_badge")
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
                      uiOutput("dash_filter_counter", inline = TRUE)),
            actionButton("btn_reset_filters", "↺", title = "Réinitialiser tous les filtres",
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
              uiOutput("dashboard_filters_label"),
              uiOutput("dashboard_filters")
            )
          )
        ),

        # ═══ Score de situation global (barre horizontale) ═══
        uiOutput("accueil_score_bar"),

        # ═══ KPI universels (s'adaptent aux données) ═══
        uiOutput("accueil_kpi_premium"),

        # ═══ KPI Qualité des données ═══
        uiOutput("accueil_kpi_qualite"),

        # ═══ KPI Sécurité alimentaire (si disponible) ═══
        uiOutput("accueil_kpi_secu"),

        # ═══ Alertes OMS (design toast) ═══
        uiOutput("accueil_alertes"),

        # ═══ Graphiques de synthèse ═══
        fluidRow(
          column(8,
            tags$div(class="nutri-chart-card",
              tags$div(class="chart-title", icon("chart-bar"), " Prévalences par groupe (IC 95% Wilson) · cliquez une barre pour filtrer"),
              plotlyOutput("accueil_prev_chart", height="400px")
            )
          ),
          column(4,
            tags$div(class="nutri-chart-card",
              tags$div(class="chart-title", icon("tachometer-alt"), " Jauge GAM — Seuil OMS"),
              plotlyOutput("accueil_gauge", height="280px"),
              uiOutput("accueil_seuil_badge")
            )
          )
        ),

        # ═══ Z-Scores + Tableau de référence ═══
        fluidRow(
          column(7,
            tags$div(class="nutri-chart-card",
              tags$div(class="chart-title", icon("chart-area"), " Distribution Z-scores vs référence OMS N(0,1)"),
              plotlyOutput("accueil_zscore", height="340px")
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
        uiOutput("accueil_rapport_bar"),

        # ═══ Sécurité Alimentaire — rCSI & HDDS (conditionnel) ═══
        uiOutput("accueil_sa_charts_row"),

        # ═══ Cohérence FCS × rCSI — Nuage de points ═══
        uiOutput("accueil_fcs_rcsi_scatter"),

        # ═══ Analyse temporelle FCS & rCSI par round ═══
        uiOutput("accueil_temporal_row"),

        # ═══ Tableau croisé rCSI × FCS — Double vulnérabilité ═══
        uiOutput("accueil_crosstab_row"),

        # ═══ Carte choroplèthe — Double vulnérabilité par région ═══
        uiOutput("accueil_dv_map_row")

      )
}
