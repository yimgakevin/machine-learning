# Extrait de app_parts/ui/ui_body.R — onglet zscore
ui_tab_zscore <- function() {
      tabItem(tabName="zscore",
        # En-tête expert
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("calculator"), " Calcul automatique des Z-scores — Standards de Croissance OMS 2006 (0-60 mois)"),
              tags$div(style="background:linear-gradient(135deg,#F0F7FF 0%,#E8F0FE 100%);border-left:4px solid #0077B6;padding:14px 18px;border-radius:0 10px 10px 0;margin-bottom:8px;font-size:12px;",
                tags$div(style="display:flex;align-items:flex-start;gap:14px;",
                  tags$div(style="flex-shrink:0;width:44px;height:44px;background:linear-gradient(135deg,#0077B6,#0C4A6E);border-radius:12px;display:flex;align-items:center;justify-content:center;",
                    icon("child", style="color:white;font-size:20px;")
                  ),
                  tags$div(
                    tags$div(style="font-weight:700;font-size:13px;color:#0B3954;margin-bottom:4px;",
                      "Méthode LMS (Box-Cox) — WHO Child Growth Standards"
                    ),
                    tags$span("Calcul des Z-scores ", tags$b("WHZ"), " (Poids-pour-Taille), ",
                      tags$b("HAZ"), " (Taille-pour-Âge) et ", tags$b("WAZ"), " (Poids-pour-Âge) ",
                      "selon les tables de référence OMS 2006. Interpolation linéaire des paramètres L, M, S. ",
                      "Ajustement SD23 pour |Z| > 3. "
                    ),
                    tags$div(style="margin-top:6px;display:flex;gap:12px;flex-wrap:wrap;",
                      tags$span(style="background:#e8f5e9;color:#2e7d32;padding:2px 8px;border-radius:4px;font-size:10px;font-weight:600;", "Normal ≥ -2 SD"),
                      tags$span(style="background:#fff3e0;color:#e65100;padding:2px 8px;border-radius:4px;font-size:10px;font-weight:600;", "Modéré -3 à -2 SD"),
                      tags$span(style="background:#ffebee;color:#c62828;padding:2px 8px;border-radius:4px;font-size:10px;font-weight:600;", "Sévère < -3 SD")
                    )
                  )
                )
              )
          )
        ),
        fluidRow(
          # Panneau paramètres
          box(width=3, status="primary", solidHeader=TRUE,
              title=tags$span(icon("sliders-h"), " Paramètres"),
              tags$div(class="section-header", tags$span(icon("columns", style="font-size:11px;"), " Variables anthropométriques")),
              uiOutput("zs_col_poids_ui"),
              uiOutput("zs_col_taille_ui"),
              uiOutput("zs_col_age_ui"),
              uiOutput("zs_col_sexe_ui"),
              hr(),
              tags$div(class="section-header", tags$span(icon("venus-mars", style="font-size:11px;"), " Codage du sexe")),
              fluidRow(
                column(6, textInput("zs_val_garcon", "Garçon :", value = "M")),
                column(6, textInput("zs_val_fille", "Fille :", value = "F"))
              ),
              hr(),
              actionButton("run_zscore", "📐 Calculer les Z-scores",
                           class="btn btn-oms btn-block", width="100%",
                           style="font-weight:600;font-size:14px;padding:12px;margin-top:4px;"),
              br(), br(),
              tags$div(style="background:#f8f9fa;border-radius:8px;padding:10px 12px;font-size:11px;color:#5a6a7a;",
                icon("info-circle", style="color:#0077B6;"),
                " Les colonnes ", tags$code("WHZ_calc"), ", ", tags$code("HAZ_calc"), ", ", tags$code("WAZ_calc"),
                " seront injectées dans les données et disponibles dans ",
                tags$b("tous les modules"), " (Qualité SMART, Prévalences, etc.)."
              )
          ),
          # Panneau résultats — 6 onglets
          box(width=9, status="info", solidHeader=TRUE,
              title=tags$span(icon("chart-area"), " Résultats — Analyse nutritionnelle"),
              tabsetPanel(id="zs_tabs", type="pills",
                # ── Onglet 1 : Tableau de bord ──
                tabPanel(tags$span(icon("tachometer-alt"), " Tableau de bord"), value="zs_dashboard",
                  br(),
                  tags$div(style="background:linear-gradient(135deg,#F5F7FA 0%,#E8F0FE 100%);border-radius:12px;padding:14px;margin-bottom:16px;border-left:4px solid #0077B6;",
                    tags$div(style="font-size:12px;font-weight:600;color:#0B3954;text-transform:uppercase;letter-spacing:1px;",
                      icon("lightbulb", style="color:#fbc02d;"), " KPI Cards Expert OMS/UNICEF"
                    ),
                    tags$div(style="font-size:11px;color:#666;margin-top:6px;",
                      "Chaque card affiche : prévalence, décomposition, narratif clinique, et score de qualité"
                    )
                  ),
                  uiOutput("zs_expert_cards"),
                  br(),
                  tags$div(style="background:white;border-radius:10px;padding:16px;border:1px solid #e8edf2;",
                    tags$div(style="font-size:12px;font-weight:600;color:#0B3954;margin-bottom:8px;text-transform:uppercase;letter-spacing:1px;",
                      icon("table", style="color:#0077B6;"), " Synthèse statistique"
                    ),
                    DTOutput("zs_resume_table")
                  )
                ),
                # ── Onglet 2 : Distributions ──
                tabPanel(tags$span(icon("chart-area"), " Distributions"), value="zs_dist",
                  br(),
                  fluidRow(
                    column(4, selectInput("zs_strat_var", "Stratification :",
                      choices = c("Global (tous)" = "global", "Par sexe" = "sexe"),
                      selected = "global", width = "100%")),
                    column(8, tags$div(style="padding-top:28px;font-size:11px;color:#888;",
                      icon("info-circle"), " Courbe pointillée = distribution de référence N(0,1). Zones ombrées = seuils OMS."))
                  ),
                  plotlyOutput("zs_dist_whz", height="300px"),
                  plotlyOutput("zs_dist_haz", height="300px"),
                  plotlyOutput("zs_dist_waz", height="300px")
                ),
                # ── Onglet 3 : Classification ──
                tabPanel(tags$span(icon("th-large"), " Classification"), value="zs_class",
                  br(),
                  fluidRow(
                    column(4, plotlyOutput("zs_donut_gam", height="320px")),
                    column(4, plotlyOutput("zs_donut_retard", height="320px")),
                    column(4, plotlyOutput("zs_donut_insuf", height="320px"))
                  ),
                  br(),
                  plotlyOutput("zs_stacked_bar", height="280px"),
                  br(),
                  DTOutput("zs_class_table")
                ),
                # ── Onglet 4 : Comparaison par sexe ──
                tabPanel(tags$span(icon("venus-mars"), " Par sexe"), value="zs_bysex",
                  br(),
                  fluidRow(
                    column(6, plotlyOutput("zs_sex_whz", height="320px")),
                    column(6, plotlyOutput("zs_sex_haz", height="320px"))
                  ),
                  fluidRow(
                    column(6, plotlyOutput("zs_sex_waz", height="320px")),
                    column(6, DTOutput("zs_sex_table"))
                  )
                ),
                # ── Onglet 5 : Données individuelles ──
                tabPanel(tags$span(icon("database"), " Données"), value="zs_data",
                  br(),
                  fluidRow(
                    column(6, downloadButton("zs_dl_data", "📥 Télécharger CSV avec Z-scores",
                      style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:8px 16px;")),
                    column(6, downloadButton("zs_dl_xlsx", "📥 Télécharger Excel",
                      style="background:#217346;color:white;border:none;border-radius:6px;padding:8px 16px;"))
                  ),
                  br(),
                  DTOutput("zs_data_table")
                ),
                # ── Onglet 6 : Qualité Avancée ──
                tabPanel(tags$span(icon("shield-alt"), " Qualité"), value="zs_quality",
                  br(),
                  uiOutput("zs_quality_expert_dashboard"),
                  br(),
                  uiOutput("zs_flags_summary"),
                  br(),
                  DTOutput("zs_flags_table")
                ),

                # ── Onglet 7 : MUAC (Périmètre Brachial) ──
                tabPanel(tags$span(icon("ruler"), " MUAC (PB)"), value="zs_muac",
                  br(),
                  tags$div(style="background:linear-gradient(135deg,#fff8e1,#fffde7);border-left:4px solid #E67E22;padding:12px 16px;border-radius:0 8px 8px 0;margin-bottom:12px;",
                    tags$div(style="font-size:12px;font-weight:700;color:#E67E22;", icon("ruler"), " Périmètre Brachial (MUAC) — Standards UNICEF/OMS"),
                    tags$div(style="font-size:11px;color:#555;margin-top:4px;",
                      "MAS : < 115mm (rouge) | MAM : 115–124mm (orange) | Risque : 125–134mm (jaune) | Normal : ≥ 135mm (vert). ",
                      tags$b("MAG = MAS + MAM (MUAC < 125mm).")
                    )
                  ),
                  uiOutput("zs_muac_kpi_row"),
                  br(),
                  fluidRow(
                    column(6, uiOutput("zs_muac_group_ui")),
                    column(6, checkboxInput("zs_muac_by_sex", "Distinguer par sexe", value = FALSE))
                  ),
                  fluidRow(
                    column(7, plotlyOutput("zs_muac_dist", height="300px")),
                    column(5, plotlyOutput("zs_muac_donut", height="300px"))
                  ),
                  br(),
                  plotlyOutput("zs_muac_by_group", height="300px"),
                  br(),
                  uiOutput("zs_muac_vs_whz_panel"),
                  br(),
                  DTOutput("zs_muac_table")
                ),

                # ── Onglet 8 : Par tranche d'âge ──
                tabPanel(tags$span(icon("birthday-cake"), " Par âge"), value="zs_age_group",
                  br(),
                  tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin-bottom:12px;font-size:11px;",
                    icon("info-circle"), " Distribution des Z-scores par tranche d'âge standard (6-11m, 12-23m, 24-35m, 36-47m, 48-59m). ",
                    "Le retard de croissance (HAZ) est souvent plus prononcé entre 12 et 24 mois."
                  ),
                  fluidRow(
                    column(4, selectInput("zs_age_indicator", "Indicateur :",
                      choices = c("WHZ (Poids/Taille)" = "WHZ_calc",
                                  "HAZ (Taille/Âge)"   = "HAZ_calc",
                                  "WAZ (Poids/Âge)"    = "WAZ_calc"),
                      selected = "WHZ_calc", width = "100%"))
                  ),
                  plotlyOutput("zs_age_boxplot", height="380px"),
                  br(),
                  DTOutput("zs_age_table")
                ),

                # ── Onglet 9 : Rapport Expert ──
                tabPanel(tags$span(icon("file-text"), " Rapport Expert"), value="zs_expert_report",
                  br(),
                  fluidRow(
                    column(12,
                      downloadButton("zs_dl_rapport", "📥 Télécharger Rapport Markdown",
                        style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:8px 16px;margin-bottom:12px;"),
                      downloadButton("zs_dl_rapport_pdf", "📄 Exporter en PDF (bientôt)",
                        style="background:#666;color:white;border:none;border-radius:6px;padding:8px 16px;margin-bottom:12px;margin-left:8px;opacity:0.6;")
                    )
                  ),
                  br(),
                  uiOutput("zs_rapport_text")
                )
              )
          )
        )
      )
}
