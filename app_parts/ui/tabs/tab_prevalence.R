# Extrait de app_parts/ui/ui_body.R — onglet prevalence
ui_tab_prevalence <- function() {
      tabItem(tabName="prevalence",
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("chart-line"),
                " Prévalences SMART — Pondérées · DEFF · IC 95% design-based"),
              tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin-bottom:12px;font-size:12px;",
                tags$b("Méthodologie SMART"), " — Calcul des prévalences de malnutrition ",
                "avec prise en compte du plan de sondage (clusters, strates, poids). ",
                "L'effet de grappe (DEFF) et les IC 95% sont calculés selon la méthodologie ",
                "recommandée par l'OMS, l'UNICEF et le PAM. ",
                tags$span(style="color:#e67e22;font-weight:600;",
                  if (!HAS_SURVEY) "⚠ Package 'survey' non installé — fallback avec DEFF approximatif. Installer avec install.packages('survey')" else "✅ Package 'survey' détecté — calculs design-based exacts")
              )
          )
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title="Plan de sondage",
              tags$div(class="section-header","Design"),
              uiOutput("sp_col_cluster_ui"),
              uiOutput("sp_col_strata_ui"),
              uiOutput("sp_col_weights_ui"),
              hr(),
              tags$div(class="section-header","Indicateurs à calculer"),
              uiOutput("sp_indicators_ui"),
              hr(),
              tags$div(class="section-header","Stratifications"),
              uiOutput("sp_stratifications_ui"),
              hr(),
              actionButton("run_prevalence", "📈 Calculer les prévalences",
                           class="btn btn-oms btn-block", width="100%",
                           style="font-size:14px;padding:10px;margin-top:8px;")
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Résultats — Prévalences SMART",
              tabsetPanel(
                tabPanel(tags$span(icon("globe"), " Prévalences globales"),
                  uiOutput("sp_design_info"),
                  br(),
                  uiOutput("sp_global_table_html"),
                  br(),
                  plotlyOutput("sp_forest_global", height="350px")
                ),
                tabPanel(tags$span(icon("layer-group"), " Par strate"),
                  DTOutput("sp_strat_table"),
                  br(),
                  plotlyOutput("sp_forest_strat", height="450px")
                ),
                tabPanel(tags$span(icon("table"), " Tableau SMART standard"),
                  tags$div(style="margin-bottom:10px;",
                    downloadButton("dl_smart_table", "📥 Télécharger (CSV)",
                      style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:6px 14px;"),
                    downloadButton("dl_smart_ena", "📥 Export ENA/SMART",
                      style="background:#27AE60;color:white;border:none;border-radius:6px;padding:6px 14px;margin-left:8px;")
                  ),
                  DTOutput("sp_full_table")
                ),

                # ── NOUVEAU : Moyennes SA design-based ──
                tabPanel(tags$span(icon("chart-line"), " Moyennes SA (IC design)"),
                  br(),
                  tags$div(
                    style = "background:#f0fff4;border-left:4px solid #27AE60;padding:10px 14px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:12px;",
                    icon("info-circle", style = "color:#27AE60;"),
                    " Moyennes de FCS, rCSI, HDDS et HHS avec IC 95% ajustés pour le plan de sondage complexe (grappes + strates). ",
                    "Ces estimations sont plus précises qu'un simple IC basé sur l'hypothèse SRS."
                  ),
                  fluidRow(
                    column(4,
                      uiOutput("sp_means_sa_cols_ui"),
                      hr(),
                      uiOutput("sp_means_group_ui")
                    ),
                    column(8, plotlyOutput("sp_means_forest", height = "360px"))
                  ),
                  br(),
                  DTOutput("sp_means_table")
                ),

                # ── NOUVEAU : Comparaison de designs ──
                tabPanel(tags$span(icon("balance-scale"), " Comparaison Designs"),
                  br(),
                  tags$div(
                    style = "background:#fff8e1;border-left:4px solid #FBC02D;padding:10px 14px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:12px;",
                    icon("exclamation-triangle", style = "color:#FBC02D;"),
                    " Compare les IC 95% et DEFF obtenus avec 3 designs : ",
                    tags$b("Stratifié + Grappes"), " (recommandé SMART), ",
                    tags$b("Grappes seules"), ", et ",
                    tags$b("SRS naïf"), " (ignore la structure de sondage). ",
                    "Un DEFF > 1 signifie que les IC naïfs sous-estiment l'incertitude réelle."
                  ),
                  fluidRow(
                    column(4,
                      checkboxGroupInput("sp_cmp_inds", "Indicateurs à comparer :",
                        choices = c("GAM" = "GAM", "SAM" = "SAM",
                                    "MAG MUAC" = "MAG_muac",
                                    "Stunting" = "Stunting",
                                    "Underweight" = "Underweight"),
                        selected = c("GAM", "MAG_muac"), width = "100%")
                    ),
                    column(8, plotlyOutput("sp_cmp_forest", height = "380px"))
                  ),
                  br(),
                  DTOutput("sp_cmp_table")
                ),

                # ── NOUVEAU : ICC et effets de sondage ──
                tabPanel(tags$span(icon("project-diagram"), " ICC & DEFF"),
                  br(),
                  tags$div(
                    style = "background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;font-size:11px;margin-bottom:12px;",
                    icon("info-circle"),
                    " L'ICC (Corrélation Intra-Classe) mesure la ressemblance entre enfants d'une même grappe. ",
                    "DEFF = 1 + (m̄ − 1) × ICC. Un ICC élevé implique un DEFF élevé et donc des IC plus larges.",
                    tags$b(" DEFF > 2 : IC doublé par rapport au SRS.")
                  ),
                  DTOutput("sp_icc_table"),
                  br(),
                  plotlyOutput("sp_deff_bar", height = "300px")
                ),

                # ── NOUVEAU : Diagnostics du plan ──
                tabPanel(tags$span(icon("stethoscope"), " Diagnostics"),
                  br(),
                  uiOutput("sp_diag_alerts"),
                  br(),
                  fluidRow(
                    column(6, plotlyOutput("sp_cluster_sizes_hist", height = "280px")),
                    column(6, DTOutput("sp_cluster_sizes_table"))
                  )
                )
              )
          )
        )
      )
}
