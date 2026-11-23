# Module Shiny — Z-scores OMS 2006 (calcul, diagnostics, MUAC, âge, expert)
# zs_data reste un reactiveVal racine (défini dans server_zscore_setup.R) :
# il est partagé avec les autres onglets. L'injection WHZ_calc/HAZ_calc/
# WAZ_calc dans raw_data() est conservée telle quelle, avec le fallback
# ..row_id.. / seq_len(n_rs) lorsque le nombre de lignes diffère.

mod_zscore_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "zscore",
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
              uiOutput(ns("zs_col_poids_ui")),
              uiOutput(ns("zs_col_taille_ui")),
              uiOutput(ns("zs_col_age_ui")),
              uiOutput(ns("zs_col_sexe_ui")),
              hr(),
              tags$div(class="section-header", tags$span(icon("venus-mars", style="font-size:11px;"), " Codage du sexe")),
              fluidRow(
                column(6, textInput(ns("zs_val_garcon"), "Garçon :", value = "M")),
                column(6, textInput(ns("zs_val_fille"), "Fille :", value = "F"))
              ),
              hr(),
              actionButton(ns("run_zscore"), "📐 Calculer les Z-scores",
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
              tabsetPanel(id = ns("zs_tabs"), type="pills",
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
                  uiOutput(ns("zs_expert_cards")),
                  br(),
                  tags$div(style="background:white;border-radius:10px;padding:16px;border:1px solid #e8edf2;",
                    tags$div(style="font-size:12px;font-weight:600;color:#0B3954;margin-bottom:8px;text-transform:uppercase;letter-spacing:1px;",
                      icon("table", style="color:#0077B6;"), " Synthèse statistique"
                    ),
                    DTOutput(ns("zs_resume_table"))
                  )
                ),
                # ── Onglet 2 : Distributions ──
                tabPanel(tags$span(icon("chart-area"), " Distributions"), value="zs_dist",
                  br(),
                  fluidRow(
                    column(4, selectInput(ns("zs_strat_var"), "Stratification :",
                      choices = c("Global (tous)" = "global", "Par sexe" = "sexe"),
                      selected = "global", width = "100%")),
                    column(8, tags$div(style="padding-top:28px;font-size:11px;color:#888;",
                      icon("info-circle"), " Courbe pointillée = distribution de référence N(0,1). Zones ombrées = seuils OMS."))
                  ),
                  plotlyOutput(ns("zs_dist_whz"), height="300px"),
                  plotlyOutput(ns("zs_dist_haz"), height="300px"),
                  plotlyOutput(ns("zs_dist_waz"), height="300px")
                ),
                # ── Onglet 3 : Classification ──
                tabPanel(tags$span(icon("th-large"), " Classification"), value="zs_class",
                  br(),
                  fluidRow(
                    column(4, plotlyOutput(ns("zs_donut_gam"), height="320px")),
                    column(4, plotlyOutput(ns("zs_donut_retard"), height="320px")),
                    column(4, plotlyOutput(ns("zs_donut_insuf"), height="320px"))
                  ),
                  br(),
                  plotlyOutput(ns("zs_stacked_bar"), height="280px"),
                  br(),
                  DTOutput(ns("zs_class_table"))
                ),
                # ── Onglet 4 : Comparaison par sexe ──
                tabPanel(tags$span(icon("venus-mars"), " Par sexe"), value="zs_bysex",
                  br(),
                  fluidRow(
                    column(6, plotlyOutput(ns("zs_sex_whz"), height="320px")),
                    column(6, plotlyOutput(ns("zs_sex_haz"), height="320px"))
                  ),
                  fluidRow(
                    column(6, plotlyOutput(ns("zs_sex_waz"), height="320px")),
                    column(6, DTOutput(ns("zs_sex_table")))
                  )
                ),
                # ── Onglet 5 : Données individuelles ──
                tabPanel(tags$span(icon("database"), " Données"), value="zs_data",
                  br(),
                  fluidRow(
                    column(6, downloadButton(ns("zs_dl_data"), "📥 Télécharger CSV avec Z-scores",
                      style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:8px 16px;")),
                    column(6, downloadButton(ns("zs_dl_xlsx"), "📥 Télécharger Excel",
                      style="background:#217346;color:white;border:none;border-radius:6px;padding:8px 16px;"))
                  ),
                  br(),
                  DTOutput(ns("zs_data_table"))
                ),
                # ── Onglet 6 : Qualité Avancée ──
                tabPanel(tags$span(icon("shield-alt"), " Qualité"), value="zs_quality",
                  br(),
                  uiOutput(ns("zs_quality_expert_dashboard")),
                  br(),
                  uiOutput(ns("zs_flags_summary")),
                  br(),
                  DTOutput(ns("zs_flags_table"))
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
                  uiOutput(ns("zs_muac_kpi_row")),
                  br(),
                  fluidRow(
                    column(6, uiOutput(ns("zs_muac_group_ui"))),
                    column(6, checkboxInput(ns("zs_muac_by_sex"), "Distinguer par sexe", value = FALSE))
                  ),
                  fluidRow(
                    column(7, plotlyOutput(ns("zs_muac_dist"), height="300px")),
                    column(5, plotlyOutput(ns("zs_muac_donut"), height="300px"))
                  ),
                  br(),
                  plotlyOutput(ns("zs_muac_by_group"), height="300px"),
                  br(),
                  uiOutput(ns("zs_muac_vs_whz_panel")),
                  br(),
                  DTOutput(ns("zs_muac_table"))
                ),

                # ── Onglet 8 : Par tranche d'âge ──
                tabPanel(tags$span(icon("birthday-cake"), " Par âge"), value="zs_age_group",
                  br(),
                  tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin-bottom:12px;font-size:11px;",
                    icon("info-circle"), " Distribution des Z-scores par tranche d'âge standard (6-11m, 12-23m, 24-35m, 36-47m, 48-59m). ",
                    "Le retard de croissance (HAZ) est souvent plus prononcé entre 12 et 24 mois."
                  ),
                  fluidRow(
                    column(4, selectInput(ns("zs_age_indicator"), "Indicateur :",
                      choices = c("WHZ (Poids/Taille)" = "WHZ_calc",
                                  "HAZ (Taille/Âge)"   = "HAZ_calc",
                                  "WAZ (Poids/Âge)"    = "WAZ_calc"),
                      selected = "WHZ_calc", width = "100%"))
                  ),
                  plotlyOutput(ns("zs_age_boxplot"), height="380px"),
                  br(),
                  DTOutput(ns("zs_age_table"))
                ),

                # ── Onglet 9 : Rapport Expert ──
                tabPanel(tags$span(icon("file-text"), " Rapport Expert"), value="zs_expert_report",
                  br(),
                  fluidRow(
                    column(12,
                      downloadButton(ns("zs_dl_rapport"), "📥 Télécharger Rapport Markdown",
                        style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:8px 16px;margin-bottom:12px;"),
                      downloadButton(ns("zs_dl_rapport_pdf"), "📄 Exporter en PDF (bientôt)",
                        style="background:#666;color:white;border:none;border-radius:6px;padding:8px 16px;margin-bottom:12px;margin-left:8px;opacity:0.6;")
                    )
                  ),
                  br(),
                  uiOutput(ns("zs_rapport_text"))
                )
              )
          )
        )
      )
}

mod_zscore_server <- function(id, df_f, raw_data, zs_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

      # Auto-détection des colonnes
      observe({
        d <- df_f(); req(d)
        noms <- names(d)
        det <- detect_zscore_columns(noms)
        num_cols <- noms[vapply(d, is.numeric, logical(1))]
        all_cols <- noms

        output$zs_col_poids_ui <- renderUI({
          sel <- if (!is.null(det$poids)) det$poids else NULL
          selectInput(ns("zs_col_poids"), "Poids (kg) :", choices = c("—" = "", num_cols), selected = sel)
        })
        output$zs_col_taille_ui <- renderUI({
          sel <- if (!is.null(det$taille)) det$taille else NULL
          selectInput(ns("zs_col_taille"), "Taille/Longueur (cm) :", choices = c("—" = "", num_cols), selected = sel)
        })
        output$zs_col_age_ui <- renderUI({
          sel <- if (!is.null(det$age)) det$age else NULL
          selectInput(ns("zs_col_age"), "Âge (mois) :", choices = c("—" = "", num_cols), selected = sel)
        })
        output$zs_col_sexe_ui <- renderUI({
          sel <- if (!is.null(det$sexe)) det$sexe else NULL
          selectInput(ns("zs_col_sexe"), "Sexe :", choices = c("—" = "", all_cols), selected = sel)
        })
      })
    # REACTIVE VALUES
    # ══════════════════════════════════════════════════════════════════════════════

    zs_expert_quality <- reactiveVal(NULL)
    zs_expert_narratives <- reactiveVal(NULL)
    zs_expert_report <- reactiveVal(NULL)

    # ══════════════════════════════════════════════════════════════════════════════
    # AFTER Z-SCORE CALCULATION
    # ══════════════════════════════════════════════════════════════════════════════

    # Observer qui déclenche l'analyse expert après calcul des Z-scores
    # Protégé par tryCatch pour éviter de crasher l'app si les données sont invalides
    observe({
      res <- zs_data()
      req(res)

      tryCatch({
        # Vérifier qu'il y a au moins des Z-scores valides
        whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
        haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
        waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]

        if (length(whz) < 5 && length(haz) < 5 && length(waz) < 5) {
          showNotification("Trop peu de Z-scores valides pour l'analyse expert.", type = "warning")
          return()
        }

        # Calculer la qualité avec tous les critères
        quality <- calc_quality_score_expert(res,
          col_whz = "WHZ_calc",
          col_haz = "HAZ_calc",
          col_waz = "WAZ_calc",
          col_age = isolate(input$zs_col_age),
          col_sexe = isolate(input$zs_col_sexe),
          col_team = NULL
        )

        zs_expert_quality(quality)

        # ─────────────────────────────────────────────────────────────────
        # Générer les narratifs (avec protection NA)
        # ─────────────────────────────────────────────────────────────────

        safe_pct <- function(x, threshold) {
          if (length(x) == 0) return(0)
          round(100 * mean(x < threshold, na.rm = TRUE), 1)
        }

        pct_gam <- safe_pct(whz, -2)
        pct_sam <- safe_pct(whz, -3)
        pct_haz <- safe_pct(haz, -2)
        pct_haz_sev <- safe_pct(haz, -3)
        pct_waz <- safe_pct(waz, -2)
        pct_waz_sev <- safe_pct(waz, -3)

        narr_gam <- gen_narrative_kpi("GAM",
          pct_prevalence = pct_gam,
          pct_severe = pct_sam,
          pct_moderate = pct_gam - pct_sam,
          mean_z = mean(whz, na.rm = TRUE),
          sd_z = if (length(whz) > 1) sd(whz, na.rm = TRUE) else NA_real_,
          n_flags = sum(res$flag_WHZ, na.rm = TRUE),
          n_total = nrow(res),
          quality_score = quality$score_global
        )

        narr_haz <- gen_narrative_kpi("HAZ",
          pct_prevalence = pct_haz,
          pct_severe = pct_haz_sev,
          pct_moderate = pct_haz - pct_haz_sev,
          mean_z = mean(haz, na.rm = TRUE),
          sd_z = if (length(haz) > 1) sd(haz, na.rm = TRUE) else NA_real_,
          n_flags = sum(res$flag_HAZ, na.rm = TRUE),
          n_total = nrow(res),
          quality_score = quality$score_global
        )

        narr_waz <- gen_narrative_kpi("WAZ",
          pct_prevalence = pct_waz,
          pct_severe = pct_waz_sev,
          pct_moderate = pct_waz - pct_waz_sev,
          mean_z = mean(waz, na.rm = TRUE),
          sd_z = if (length(waz) > 1) sd(waz, na.rm = TRUE) else NA_real_,
          n_flags = sum(res$flag_WAZ, na.rm = TRUE),
          n_total = nrow(res),
          quality_score = quality$score_global
        )

        zs_expert_narratives(list(
          GAM = narr_gam,
          HAZ = narr_haz,
          WAZ = narr_waz,
          pct_gam = pct_gam,
          pct_haz = pct_haz,
          pct_waz = pct_waz,
          pct_sam = pct_sam,
          pct_haz_sev = pct_haz_sev,
          pct_waz_sev = pct_waz_sev
        ))

        # Générer le rapport complet
        rapport <- gen_rapport_expert(res, NULL, quality,
          col_sexe = isolate(input$zs_col_sexe),
          col_age = isolate(input$zs_col_age)
        )

        zs_expert_report(rapport)

      }, error = function(e) {
        showNotification(
          paste("Erreur analyse expert Z-scores :", e$message),
          type = "error", duration = 10
        )
        message("[zscore_expert] Erreur dans observe : ", e$message)
      })
    })

    # ══════════════════════════════════════════════════════════════════════════════
    # OUTPUT: KPI CARDS EXPERT AVEC NARRATIF
    # ══════════════════════════════════════════════════════════════════════════════

    output$zs_expert_cards <- renderUI({
      res <- zs_data()
      quality <- zs_expert_quality()
      narr <- zs_expert_narratives()
      req(res, quality, narr)

      whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
      haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
      waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]

      tags$div(class = "zs-kpi-grid",
        # ─ CARD 1: GAM ─
        tags$div(class = "zs-kpi-card",
          style = "border: 2px solid #d32f2f; position: relative; overflow: hidden;",
          tags$div(style = "position: absolute; top: -8px; right: -8px; font-size: 72px; opacity: 0.06; color: #d32f2f;",
            icon("exclamation-triangle")),
          tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px;",
            tags$div(style = "width:36px; height:36px; border-radius:10px; background:#d32f2f; display:flex; align-items:center; justify-content:center; color:white;",
              icon("exclamation-triangle"))
            ,
            tags$div(
              tags$div(class = "zs-title", "Malnutrition Aiguë (WHZ < -2)"),
              tags$div(class = paste("zs-severity-badge", if (narr$pct_gam >= 15) "zs-severity-critique" else "zs-severity-acceptable"),
                if (narr$pct_gam >= 15) "CRITIQUE ≥15%" else "ACCEPTABLE <5%")
            )
          ),
          tags$div(style = "margin: 8px 0;",
            tags$span(class = "zs-value", style = "color: #d32f2f;", paste0(narr$pct_gam, "%"))
          ),
          # Sparkline inline
          plotlyOutput(ns("zs_spark_gam"), height = "35px", width = "100%"),
          # Décomposition
          tags$div(class = "zs-decomp",
            tags$div(class = "zs-decomp-row",
              tags$span(class = "zs-decomp-label", paste0("SMA (< -3): ", narr$pct_sam, "%")),
              tags$div(class = "zs-mini-bar-track",
                tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min(narr$pct_sam * 4.5, 100), "%; background: #d32f2f;"))
              )
            ),
            tags$div(class = "zs-decomp-row",
              tags$span(class = "zs-decomp-label", paste0("MAM (-3 à -2): ", round(narr$pct_gam - narr$pct_sam, 1), "%")),
              tags$div(class = "zs-mini-bar-track",
                tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min((narr$pct_gam - narr$pct_sam) * 4.5, 100), "%; background: #f57c00;"))
              )
            )
          ),
          # Narratif
          tags$div(style = "margin-top: 12px; padding-top: 10px; border-top: 1px dashed #e8edf2; font-size: 12px; line-height: 1.5; color: #5a6a7a;",
            HTML(narr$GAM)
          )
        ),

        # ─ CARD 2: HAZ ─
        tags$div(class = "zs-kpi-card",
          style = "border: 2px solid #388e3c; position: relative; overflow: hidden;",
          tags$div(style = "position: absolute; top: -8px; right: -8px; font-size: 72px; opacity: 0.06; color: #388e3c;",
            icon("arrow-up")),
          tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px;",
            tags$div(style = "width:36px; height:36px; border-radius:10px; background:#388e3c; display:flex; align-items:center; justify-content:center; color:white;",
              icon("arrow-up")),
            tags$div(
              tags$div(class = "zs-title", "Retard de Croissance (HAZ < -2)"),
              tags$div(class = paste("zs-severity-badge", if (narr$pct_haz >= 40) "zs-severity-critique" else "zs-severity-acceptable"),
                if (narr$pct_haz >= 40) "CRITIQUE ≥40%" else "ACCEPTABLE <20%")
            )
          ),
          tags$div(style = "margin: 8px 0;",
            tags$span(class = "zs-value", style = "color: #388e3c;", paste0(narr$pct_haz, "%"))
          ),
          plotlyOutput(ns("zs_spark_haz"), height = "35px", width = "100%"),
          tags$div(class = "zs-decomp",
            tags$div(class = "zs-decomp-row",
              tags$span(class = "zs-decomp-label", paste0("Sévère (< -3): ", narr$pct_haz_sev, "%")),
              tags$div(class = "zs-mini-bar-track",
                tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min(narr$pct_haz_sev * 2, 100), "%; background: #d32f2f;"))
              )
            ),
            tags$div(class = "zs-decomp-row",
              tags$span(class = "zs-decomp-label", paste0("Modéré (-3 à -2): ", round(narr$pct_haz - narr$pct_haz_sev, 1), "%")),
              tags$div(class = "zs-mini-bar-track",
                tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min((narr$pct_haz - narr$pct_haz_sev) * 2, 100), "%; background: #f57c00;"))
              )
            )
          ),
          tags$div(style = "margin-top: 12px; padding-top: 10px; border-top: 1px dashed #e8edf2; font-size: 12px; line-height: 1.5; color: #5a6a7a;",
            HTML(narr$HAZ)
          )
        ),

        # ─ CARD 3: WAZ ─
        tags$div(class = "zs-kpi-card",
          style = "border: 2px solid #7b1fa2; position: relative; overflow: hidden;",
          tags$div(style = "position: absolute; top: -8px; right: -8px; font-size: 72px; opacity: 0.06; color: #7b1fa2;",
            icon("weight")),
          tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px;",
            tags$div(style = "width:36px; height:36px; border-radius:10px; background:#7b1fa2; display:flex; align-items:center; justify-content:center; color:white;",
              icon("weight")),
            tags$div(
              tags$div(class = "zs-title", "Insuffisance Pondérale (WAZ < -2)"),
              tags$div(class = paste("zs-severity-badge", if (narr$pct_waz >= 30) "zs-severity-critique" else "zs-severity-acceptable"),
                if (narr$pct_waz >= 30) "CRITIQUE ≥30%" else "ACCEPTABLE <10%")
            )
          ),
          tags$div(style = "margin: 8px 0;",
            tags$span(class = "zs-value", style = "color: #7b1fa2;", paste0(narr$pct_waz, "%"))
          ),
          plotlyOutput(ns("zs_spark_waz"), height = "35px", width = "100%"),
          tags$div(class = "zs-decomp",
            tags$div(class = "zs-decomp-row",
              tags$span(class = "zs-decomp-label", paste0("Sévère (< -3): ", narr$pct_waz_sev, "%")),
              tags$div(class = "zs-mini-bar-track",
                tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min(narr$pct_waz_sev * 2.2, 100), "%; background: #d32f2f;"))
              )
            ),
            tags$div(class = "zs-decomp-row",
              tags$span(class = "zs-decomp-label", paste0("Modéré (-3 à -2): ", round(narr$pct_waz - narr$pct_waz_sev, 1), "%")),
              tags$div(class = "zs-mini-bar-track",
                tags$div(class = "zs-mini-bar-fill", style = paste0("width: ", min((narr$pct_waz - narr$pct_waz_sev) * 2.2, 100), "%; background: #f57c00;"))
              )
            )
          ),
          tags$div(style = "margin-top: 12px; padding-top: 10px; border-top: 1px dashed #e8edf2; font-size: 12px; line-height: 1.5; color: #5a6a7a;",
            HTML(narr$WAZ)
          )
        )
      )
    })

    # Sparklines outputs
    output$zs_spark_gam <- renderPlotly({
      res <- zs_data(); req(res)
      p <- gen_sparkline_z(res$WHZ_calc, "#d32f2f")
      if (is.null(p)) return(plotly_empty())
      ggplotly(p, tooltip = FALSE) |> layout(margin = list(t = 0, b = 0, l = 0, r = 0))
    })

    output$zs_spark_haz <- renderPlotly({
      res <- zs_data(); req(res)
      p <- gen_sparkline_z(res$HAZ_calc, "#388e3c")
      if (is.null(p)) return(plotly_empty())
      ggplotly(p, tooltip = FALSE) |> layout(margin = list(t = 0, b = 0, l = 0, r = 0))
    })

    output$zs_spark_waz <- renderPlotly({
      res <- zs_data(); req(res)
      p <- gen_sparkline_z(res$WAZ_calc, "#7b1fa2")
      if (is.null(p)) return(plotly_empty())
      ggplotly(p, tooltip = FALSE) |> layout(margin = list(t = 0, b = 0, l = 0, r = 0))
    })

    # ══════════════════════════════════════════════════════════════════════════════
    # OUTPUT: QUALITY DASHBOARD EXPERT
    # ══════════════════════════════════════════════════════════════════════════════

    output$zs_quality_expert_dashboard <- renderUI({
      quality <- zs_expert_quality()
      req(quality)

      tags$div(
        # Score principal
        tags$div(style = paste0("background:white;border-radius:12px;padding:22px;border:1px solid #e8edf2;",
                                 "margin-bottom:16px;box-shadow:0 2px 8px rgba(0,0,0,.06);display:flex;align-items:center;gap:16px;"),
          tags$div(style = paste0("width:100px;height:100px;border-radius:50%;background:", quality$color,
                                   ";display:flex;align-items:center;justify-content:center;",
                                   "font-weight:800;font-size:32px;color:white;"),
            paste0(round(quality$score_global), "/100")
          ),
          tags$div(
            tags$div(style = "font-size:14px;font-weight:700;color:#00426A;margin-bottom:4px;",
              "Score de qualité global"),
            tags$div(style = paste0("font-size:20px;font-weight:800;color:", quality$color, ";margin:8px 0;"),
              quality$classification),
            tags$div(style = "font-size:11px;color:#666;", "Basé sur 10 critères OMS standardisés")
          )
        ),

        # Critères détaillés en tableau
        tags$div(style = "background:white;border-radius:12px;padding:16px;border:1px solid #e8edf2;",
          tags$div(style = "font-size:12px;font-weight:600;color:#00426A;margin-bottom:12px;text-transform:uppercase;letter-spacing:1px;",
            icon("list-check"), " Détail des 10 critères"),
          DTOutput(ns("zs_quality_criteria_dt"))
        )
      )
    })

    # ══════════════════════════════════════════════════════════════════════════════
    # OUTPUT: RAPPORT EXPERT TEXTE
    # ══════════════════════════════════════════════════════════════════════════════

    output$zs_rapport_text <- renderUI({
      rapport <- zs_expert_report()
      req(rapport)

      tags$div(style = "background:white;border-radius:12px;padding:20px;font-size:13px;line-height:1.8;color:#333;",
        HTML(markdown::markdownToHTML(text = rapport))
      )
    })

    # Download rapport Markdown
    output$zs_dl_rapport <- downloadHandler(
      filename = function() paste0("zscores_rapport_expert_", Sys.Date(), ".md"),
      content = function(file) {
        rapport <- zs_expert_report()
        writeLines(rapport, file)
      }
    )
    # Download rapport PDF
    output$zs_dl_rapport_pdf <- downloadHandler(
      filename = function() paste0("zscores_rapport_expert_", Sys.Date(), ".pdf"),
      content = function(file) {
        rapport <- zs_expert_report()
        req(rapport)
        # Écrire le markdown dans un fichier temporaire puis convertir en PDF via rmarkdown
        tmp_md <- tempfile(fileext = ".md")
        writeLines(rapport, tmp_md)
        tryCatch({
          rmarkdown::render(tmp_md, output_format = rmarkdown::pdf_document(), output_file = file, quiet = TRUE)
        }, error = function(e) {
          # Fallback : enregistrer en .md si la conversion PDF échoue (LaTeX manquant, etc.)
          file.copy(tmp_md, file)
          showNotification(
            paste("Export PDF impossible (LaTeX requis). Fichier Markdown exporté à la place.", e$message),
            type = "warning", duration = 8
          )
        })
      }
    )

    # Output: Quality criteria datatable (extracted from renderUI for stability)
    output$zs_quality_criteria_dt <- renderDT({
      quality <- zs_expert_quality()
      req(quality, quality$criteria_scores)
      datatable(quality$criteria_scores,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10, scrollX = TRUE),
        class = "compact stripe") |>
        formatStyle("Score",
          backgroundColor = styleInterval(c(60, 75, 90), c("#ffcdd2", "#fff9c4", "#c8e6c9", "#81c784")))
    })
      # Calcul des Z-scores
      observeEvent(input$run_zscore, {
        d <- df_f(); req(d)

        col_p <- if (!is.null(input$zs_col_poids) && input$zs_col_poids != "") input$zs_col_poids else NULL
        col_t <- if (!is.null(input$zs_col_taille) && input$zs_col_taille != "") input$zs_col_taille else NULL
        col_a <- if (!is.null(input$zs_col_age) && input$zs_col_age != "") input$zs_col_age else NULL
        col_s <- if (!is.null(input$zs_col_sexe) && input$zs_col_sexe != "") input$zs_col_sexe else NULL

        req(col_p, col_t, col_s)

        withProgress(message = "Calcul des Z-scores OMS 2006...", value = 0.3, {
          res <- tryCatch(
            calcul_zscores_oms(d,
              col_poids  = col_p,
              col_taille = col_t,
              col_age    = col_a,
              col_sexe   = col_s,
              val_garcon = input$zs_val_garcon %||% "M",
              val_fille  = input$zs_val_fille %||% "F"
            ),
            error = function(e) { showNotification(paste("Erreur:", e$message), type="error"); NULL }
          )
          setProgress(0.8)
          if (!is.null(res)) {
            zs_data(res)

            # Injecter WHZ_calc, HAZ_calc, WAZ_calc dans les données principales
            # Le calcul utilise df_f() (filtré) qui peut avoir moins de lignes que raw_data().
            # On initialise à NA puis on remplit les lignes correspondantes.
            rd <- raw_data()
            if (!is.null(rd)) {
              n_rd <- nrow(rd)
              n_rs <- nrow(res)
              if (n_rd == n_rs) {
                # Pas de filtre actif : injection directe
                rd$WHZ_calc <- res$WHZ_calc
                rd$HAZ_calc <- res$HAZ_calc
                rd$WAZ_calc <- res$WAZ_calc
              } else {
                # Filtre actif : initialiser à NA, remplir les lignes filtrées
                # On retrouve les indices des lignes filtrées dans raw_data
                if (!"..row_id.." %in% names(rd)) {
                  rd$WHZ_calc <- NA_real_
                  rd$HAZ_calc <- NA_real_
                  rd$WAZ_calc <- NA_real_
                  # Copier les résultats dans les n_rs premières lignes (fallback)
                  rd$WHZ_calc[seq_len(n_rs)] <- res$WHZ_calc
                  rd$HAZ_calc[seq_len(n_rs)] <- res$HAZ_calc
                  rd$WAZ_calc[seq_len(n_rs)] <- res$WAZ_calc
                }
              }
              raw_data(rd)
            }

            showNotification("✅ Z-scores calculés avec succès !", type="message", duration=5)
          }
          setProgress(1)
        })
      })

      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      # EXPERT KPI CARDS — Niveau publication OMS/UNICEF
      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      # Fonction helper : créer une card expert avec badge sévérité OMS
      make_kpi_card <- function(icon_char, icon_bg_color, title, value, unit = "",
                                severity_label = "", severity_class = "",
                                decomp_rows = NULL, stat_rows = NULL) {
        tags$div(class = "zs-kpi-card",
          tags$div(class = "zs-icon-bg", icon_char),
          tags$div(class = "zs-header",
            tags$div(class = "zs-header-icon", style = paste0("background:", icon_bg_color, ";"), 
              icon(icon_char, style = "color:white;")),
            tags$div(
              tags$div(class = "zs-title", title),
              if (!is.null(severity_label) && severity_label != "")
                tags$div(class = paste("zs-severity-badge", paste0("zs-severity-", severity_class)),
                  severity_label)
            )
          ),
          tags$div(style = "margin:8px 0;",
            tags$span(class = "zs-value", value),
            if (unit != "") tags$span(class = "zs-unit", unit)
          ),
          if (!is.null(decomp_rows)) tags$div(class = "zs-decomp", decomp_rows),
          if (!is.null(stat_rows)) tags$div(class = "zs-stat-row", stat_rows)
        )
      }

      # Fonction helper : mini-barre de décomposition
      make_decomp_bar <- function(label, pct, color) {
        tags$div(class = "zs-decomp-row",
          tags$span(class = "zs-decomp-label", label),
          tags$div(class = "zs-mini-bar-track",
            tags$div(class = "zs-mini-bar-fill", 
              style = paste0("width:", min(pct, 100), "%;background:", color, ";"))
          ),
          tags$span(class = "zs-decomp-val", paste0(round(pct, 1), "%"))
        )
      }

    # [DEAD CODE] zs_resume_cards — output serveur sans UI correspondante
    #   # Rendu des KPI cards
    #   output$zs_resume_cards <- renderUI({
    #     res <- zs_data(); req(res)
    #     
    #     # Extraire les vecteurs Z-score
    #     whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
    #     haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
    #     waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]
    # 
    #     # Compter les observations
    #     n_total <- nrow(res)
    #     n_garcons <- sum(toupper(trimws(res[[input$zs_col_sexe]])) %in% c("M", "MALE", "1"), na.rm = TRUE)
    #     n_filles <- sum(toupper(trimws(res[[input$zs_col_sexe]])) %in% c("F", "FEMALE", "2"), na.rm = TRUE)
    #     n_whz <- length(whz)
    #     n_haz <- length(haz)
    #     n_waz <- length(waz)
    # 
    #     # Prévalences OMS
    #     pct_gam <- if (n_whz > 0) round(100 * mean(whz < -2), 1) else NA
    #     pct_sam <- if (n_whz > 0) round(100 * mean(whz < -3), 1) else NA
    #     pct_mam <- if (n_whz > 0) round(100 * mean(whz < -2 & whz >= -3), 1) else NA
    #     pct_stunt <- if (n_haz > 0) round(100 * mean(haz < -2), 1) else NA
    #     pct_stunt_sev <- if (n_haz > 0) round(100 * mean(haz < -3), 1) else NA
    #     pct_under <- if (n_waz > 0) round(100 * mean(waz < -2), 1) else NA
    #     pct_under_sev <- if (n_waz > 0) round(100 * mean(waz < -3), 1) else NA
    # 
    #     # Moyennes et écart-types
    #     m_whz <- if (n_whz > 0) round(mean(whz), 3) else NA
    #     sd_whz <- if (n_whz > 0) round(sd(whz), 3) else NA
    #     m_haz <- if (n_haz > 0) round(mean(haz), 3) else NA
    #     sd_haz <- if (n_haz > 0) round(sd(haz), 3) else NA
    #     m_waz <- if (n_waz > 0) round(mean(waz), 3) else NA
    #     sd_waz <- if (n_waz > 0) round(sd(waz), 3) else NA
    # 
    #     # Flags
    #     n_flag_whz <- sum(res$flag_WHZ, na.rm = TRUE)
    #     n_flag_haz <- sum(res$flag_HAZ, na.rm = TRUE)
    #     n_flag_waz <- sum(res$flag_WAZ, na.rm = TRUE)
    #     pct_flag_whz <- if (n_whz > 0) round(100 * n_flag_whz / n_whz, 1) else NA
    #     pct_flag_haz <- if (n_haz > 0) round(100 * n_flag_haz / n_haz, 1) else NA
    #     pct_flag_waz <- if (n_waz > 0) round(100 * n_flag_waz / n_waz, 1) else NA
    # 
    #     # Classification des badges OMS
    #     class_gam <- if (is.na(pct_gam)) "critique"
    #                  else if (pct_gam >= 15) "critique"
    #                  else if (pct_gam >= 10) "serieux"
    #                  else if (pct_gam >= 5) "pauvre"
    #                  else "acceptable"
    # 
    #     class_stunt <- if (is.na(pct_stunt)) "critique"
    #                    else if (pct_stunt >= 40) "critique"
    #                    else if (pct_stunt >= 30) "serieux"
    #                    else if (pct_stunt >= 20) "pauvre"
    #                    else "acceptable"
    # 
    #     class_under <- if (is.na(pct_under)) "critique"
    #                    else if (pct_under >= 30) "critique"
    #                    else if (pct_under >= 20) "serieux"
    #                    else if (pct_under >= 10) "pauvre"
    #                    else "acceptable"
    # 
    #     label_gam <- if (class_gam == "critique") "CRITIQUE ≥15%"
    #                  else if (class_gam == "serieux") "SÉRIEUX 10-14%"
    #                  else if (class_gam == "pauvre") "PAUVRE 5-9%"
    #                  else "ACCEPTABLE <5%"
    # 
    #     label_stunt <- if (class_stunt == "critique") "CRITIQUE ≥40%"
    #                    else if (class_stunt == "serieux") "SÉRIEUX 30-39%"
    #                    else if (class_stunt == "pauvre") "PAUVRE 20-29%"
    #                    else "ACCEPTABLE <20%"
    # 
    #     label_under <- if (class_under == "critique") "CRITIQUE ≥30%"
    #                    else if (class_under == "serieux") "SÉRIEUX 20-29%"
    #                    else if (class_under == "pauvre") "PAUVRE 10-19%"
    #                    else "ACCEPTABLE <10%"
    # 
    #     tags$div(class = "zs-kpi-grid",
    #       # ── Card 1 : Échantillon ──
    #       make_kpi_card(
    #         icon_char = "child",
    #         icon_bg_color = "#0077B6",
    #         title = "Échantillon analysé",
    #         value = format(n_total, big.mark = " "),
    #         unit = "enfants",
    #         decomp_rows = tagList(
    #           make_decomp_bar(paste0("Garçons (", n_garcons, ")"), 
    #             if (n_total > 0) 100 * n_garcons / n_total else 0, "#1976d2"),
    #           make_decomp_bar(paste0("Filles (", n_filles, ")"), 
    #             if (n_total > 0) 100 * n_filles / n_total else 0, "#d81b60")
    #         ),
    #         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;",
    #           tags$strong(style = "font-weight:700;", "Z-scores valides :"), 
    #           " WHZ ", tags$span(style = "font-weight:700;", n_whz), " | ",
    #           "HAZ ", tags$span(style = "font-weight:700;", n_haz), " | ",
    #           "WAZ ", tags$span(style = "font-weight:700;", n_waz))
    #       ),
    # 
    #       # ── Card 2 : GAM (Malnutrition Aiguë) ──
    #       make_kpi_card(
    #         icon_char = "exclamation-triangle",
    #         icon_bg_color = if (class_gam == "critique") "#d32f2f" else if (class_gam == "serieux") "#f57c00" else "#fbc02d",
    #         title = "Malnutrition aiguë globale (WHZ < -2)",
    #         value = paste0(pct_gam, "%"),
    #         severity_label = label_gam,
    #         severity_class = class_gam,
    #         decomp_rows = tagList(
    #           make_decomp_bar("SMA (WHZ < -3)", pct_sam, "#d32f2f"),
    #           make_decomp_bar("MAM (WHZ -3 à -2)", pct_mam, "#f57c00")
    #         ),
    #         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;gap:16px;",
    #           tags$span(tags$strong("Moy :"), " ", m_whz),
    #           tags$span(tags$strong("SD :"), " ", sd_whz),
    #           tags$span(style = "margin-left:auto;", 
    #             span(class = "zs-quality-dot", style = if (pct_flag_whz <= 2) "background:#4caf50;" else "background:#d32f2f;"),
    #             tags$strong(paste0(pct_flag_whz, "% flags")))
    #         )
    #       ),
    # 
    #       # ── Card 3 : Stunting (Retard de croissance) ──
    #       make_kpi_card(
    #         icon_char = "arrow-up",
    #         icon_bg_color = if (class_stunt == "critique") "#d32f2f" else if (class_stunt == "serieux") "#f57c00" else "#fbc02d",
    #         title = "Retard de croissance (HAZ < -2)",
    #         value = paste0(pct_stunt, "%"),
    #         severity_label = label_stunt,
    #         severity_class = class_stunt,
    #         decomp_rows = tagList(
    #           make_decomp_bar("Sévère (HAZ < -3)", pct_stunt_sev, "#d32f2f"),
    #           make_decomp_bar("Modéré (HAZ -3 à -2)", pct_stunt - pct_stunt_sev, "#f57c00")
    #         ),
    #         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;gap:16px;",
    #           tags$span(tags$strong("Moy :"), " ", m_haz),
    #           tags$span(tags$strong("SD :"), " ", sd_haz),
    #           tags$span(style = "margin-left:auto;", 
    #             span(class = "zs-quality-dot", style = if (pct_flag_haz <= 2) "background:#4caf50;" else "background:#d32f2f;"),
    #             tags$strong(paste0(pct_flag_haz, "% flags")))
    #         )
    #       ),
    # 
    #       # ── Card 4 : Underweight (Insuffisance pondérale) ──
    #       make_kpi_card(
    #         icon_char = "weight",
    #         icon_bg_color = if (class_under == "critique") "#d32f2f" else if (class_under == "serieux") "#f57c00" else "#fbc02d",
    #         title = "Insuffisance pondérale (WAZ < -2)",
    #         value = paste0(pct_under, "%"),
    #         severity_label = label_under,
    #         severity_class = class_under,
    #         decomp_rows = tagList(
    #           make_decomp_bar("Sévère (WAZ < -3)", pct_under_sev, "#d32f2f"),
    #           make_decomp_bar("Modéré (WAZ -3 à -2)", pct_under - pct_under_sev, "#f57c00")
    #         ),
    #         stat_rows = tags$span(style = "font-size:10px;color:#5a6a7a;gap:16px;",
    #           tags$span(tags$strong("Moy :"), " ", m_waz),
    #           tags$span(tags$strong("SD :"), " ", sd_waz),
    #           tags$span(style = "margin-left:auto;", 
    #             span(class = "zs-quality-dot", style = if (pct_flag_waz <= 2) "background:#4caf50;" else "background:#d32f2f;"),
    #             tags$strong(paste0(pct_flag_waz, "% flags")))
    #         )
    #       )
    #     )
    #   })

      # Tableau résumé EXPERT
      output$zs_resume_table <- renderDT({
        res <- zs_data(); req(res)
        tab <- resume_zscores(res)
        req(nrow(tab) > 0)
        datatable(tab, rownames = FALSE,
          options = list(dom = "t", pageLength = 5, scrollX = TRUE, pageLength = 10),
          class = "compact stripe hover") |>
          formatStyle("N", backgroundColor = "#f5f5f5", fontWeight = "bold") |>
          formatStyle("Moyenne", color = styleInterval(c(-2, -1), c("#d32f2f", "#f57c00", "#333"))) |>
          formatStyle("% < -2 SD", backgroundColor = styleInterval(c(5, 10, 15, 20),
            c("#e8f5e9", "#fff9c4", "#ffe0b2", "#ffcdd2", "#ffcdd2")),
            color = styleInterval(c(5, 10, 15), c("#2e7d32", "#f57f17", "#e65100", "#c62828")),
            fontWeight = "bold") |>
          formatStyle("% Flags", backgroundColor = styleInterval(c(2, 5),
            c("#e8f5e9", "#fff9c4", "#ffcdd2")),
            fontWeight = "bold")
      })

      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      # DISTRIBUTIONS Z-SCORES — Histogrammes expert avec seuils OMS
      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      zs_plot_dist_expert <- function(z_vals, titre, couleur, strat_var = NULL, strat_data = NULL) {
        z <- z_vals[!is.na(z_vals)]
        if (length(z) < 5) return(plotly_empty() |> layout(title = list(text = paste(titre, "— données insuffisantes"))))

        bins <- seq(floor(min(z, -4)), ceiling(max(z, 4)), by = 0.25)
        x_ref <- seq(-5, 5, 0.01)
        y_ref <- dnorm(x_ref)
    
        # Statistiques
        z_mean <- mean(z)
        z_median <- median(z)
        z_sd <- sd(z)
        z_ci_low <- quantile(z, 0.025)
        z_ci_high <- quantile(z, 0.975)

        if (is.null(strat_var) || strat_var == "global") {
          hist_data <- tibble::tibble(z = z)
          p <- ggplot(hist_data, aes(x = z)) +
            geom_histogram(aes(y = after_stat(density)), breaks = bins,
                           fill = couleur, alpha = 0.62, color = "white", linewidth = 0.3) +
            geom_line(data = data.frame(x = x_ref, y = y_ref), aes(x = x, y = y),
                      color = "#333", linewidth = 1.1, linetype = "dashed") +
            geom_vline(aes(xintercept = z_mean), color = couleur, linewidth = 1.2, linetype = "solid") +
            geom_vline(aes(xintercept = z_median), color = "#666", linewidth = 0.8, linetype = "dotted") +
            annotate("rect", xmin = -5, xmax = -3, ymin = 0, ymax = Inf, fill = "#d32f2f", alpha = 0.06) +
            annotate("rect", xmin = -3, xmax = -2, ymin = 0, ymax = Inf, fill = "#f57c00", alpha = 0.06) +
            annotate("rect", xmin = 2, xmax = 3, ymin = 0, ymax = Inf, fill = "#1976d2", alpha = 0.04) +
            annotate("text", x = -4, y = Inf, label = "Critique", hjust = 0.5, vjust = 1.2,
                     size = 3, color = "#d32f2f", fontface = "bold") +
            labs(title = titre,
                 subtitle = paste0("n=", length(z), " | µ=", round(z_mean, 3), " σ=", round(z_sd, 3),
                                   " | IC95% [", round(z_ci_low, 2), " - ", round(z_ci_high, 2), "]"),
                 x = "Z-score", y = "Densité") +
            theme_minimal(base_size = 11) +
            theme(
              plot.title = element_text(face = "bold", size = 13, color = "#0B3954"),
              plot.subtitle = element_text(size = 10, color = "#666", margin = margin(t = 4, b = 8)),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line(color = "#f0f0f0", linewidth = 0.3)
            )
        } else if (strat_var == "sexe" && !is.null(strat_data)) {
          hist_data <- strat_data |> dplyr::select(z = 1, sexe = 2) |> 
            mutate(sexe = ifelse(is.na(sexe), "Unknown", sexe))
          p <- ggplot(hist_data, aes(x = z, fill = sexe)) +
            geom_histogram(aes(y = after_stat(density)), breaks = bins,
                           alpha = 0.55, color = "white", linewidth = 0.3, position = "dodge") +
            geom_line(data = data.frame(x = x_ref, y = y_ref), aes(x = x, y = y),
                      color = "#333", linewidth = 1, linetype = "dashed", inherit.aes = FALSE) +
            annotate("rect", xmin = -5, xmax = -3, ymin = 0, ymax = Inf, fill = "#d32f2f", alpha = 0.04) +
            annotate("rect", xmin = -3, xmax = -2, ymin = 0, ymax = Inf, fill = "#f57c00", alpha = 0.04) +
            scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60", "Unknown" = "#999")) +
            labs(title = titre, subtitle = paste0("Stratifié par sexe | n=", length(z)),
                 x = "Z-score", y = "Densité", fill = "Sexe") +
            theme_minimal(base_size = 11) +
            theme(
              plot.title = element_text(face = "bold", size = 13, color = "#0B3954"),
              plot.subtitle = element_text(size = 10, color = "#666", margin = margin(t = 4, b = 8)),
              panel.grid.minor = element_blank(),
              legend.position = "top"
            )
        }

        ggplotly(p, tooltip = c("x", "y")) |>
          layout(
            showlegend = TRUE,
            hovermode = "x unified",
            margin = list(t = 80, r = 40, b = 50, l = 50)
          ) |> nutri_config()
      }

      output$zs_dist_whz <- renderPlotly({
        res <- zs_data(); req(res)
        strat <- isolate(input$zs_strat_var)
        if (strat == "sexe") {
          strat_data <- tibble(z = res$WHZ_calc, sexe = res[[isolate(input$zs_col_sexe)]])
          zs_plot_dist_expert(res$WHZ_calc, "WHZ — Poids-pour-Taille", "#1976d2", strat, strat_data)
        } else {
          zs_plot_dist_expert(res$WHZ_calc, "WHZ — Poids-pour-Taille", "#1976d2", "global")
        }
      })
      output$zs_dist_haz <- renderPlotly({
        res <- zs_data(); req(res)
        strat <- isolate(input$zs_strat_var)
        if (strat == "sexe") {
          strat_data <- tibble(z = res$HAZ_calc, sexe = res[[isolate(input$zs_col_sexe)]])
          zs_plot_dist_expert(res$HAZ_calc, "HAZ — Taille-pour-Âge", "#388e3c", strat, strat_data)
        } else {
          zs_plot_dist_expert(res$HAZ_calc, "HAZ — Taille-pour-Âge", "#388e3c", "global")
        }
      })
      output$zs_dist_waz <- renderPlotly({
        res <- zs_data(); req(res)
        strat <- isolate(input$zs_strat_var)
        if (strat == "sexe") {
          strat_data <- tibble(z = res$WAZ_calc, sexe = res[[isolate(input$zs_col_sexe)]])
          zs_plot_dist_expert(res$WAZ_calc, "WAZ — Poids-pour-Âge", "#7b1fa2", strat, strat_data)
        } else {
          zs_plot_dist_expert(res$WAZ_calc, "WAZ — Poids-pour-Âge", "#7b1fa2", "global")
        }
      })

      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      # DONUTS EXPERT — Classification nutritionnelle avec % central
      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      zs_plot_donut <- function(classes, titre, worst_label = NULL) {
        cls <- classes[!is.na(classes)]
        if (length(cls) == 0) return(plotly_empty())
        tab <- as.data.frame(table(cls), stringsAsFactors = FALSE)
        names(tab) <- c("Classe", "N")
        tab$Pct <- round(100 * tab$N / sum(tab$N), 1)

        colors <- c("Normal" = "#4caf50", "MAS (Malnutrition Aiguë Sévère)" = "#d32f2f",
                    "MAM (Malnutrition Aiguë Modérée)" = "#f57c00",
                    "Retard sévère" = "#d32f2f", "Retard modéré" = "#f57c00",
                    "Insuffisance sévère" = "#d32f2f", "Insuffisance modérée" = "#f57c00")
        cols <- ifelse(tab$Classe %in% names(colors), colors[tab$Classe], "#90a4ae")

        # Calculer le pourcentage critique (worst_label)
        pct_worst <- if (!is.null(worst_label)) {
          worst_rows <- tab[grepl(worst_label, tab$Classe, ignore.case = TRUE), ]
          sum(worst_rows$Pct)
        } else 0

        plot_ly(tab, labels = ~Classe, values = ~N, type = "pie",
                hole = 0.55,
                marker = list(colors = cols, line = list(color = "white", width = 2)),
                textinfo = "label+percent", textposition = "auto",
                hoverinfo = "label+value+percent",
                hovertemplate = "<b>%{label}</b><br>N=%{value}<br>%{percent}<extra></extra>") |>
          layout(
            title = list(text = paste0(titre, "<br><sub style='font-size:12px;'>n=", sum(tab$N), "</sub>"),
                         font = list(size = 13, color = "#0B3954")),
            showlegend = TRUE,
            legend = list(x = 0, y = -0.15, orientation = "h", font = list(size = 10)),
            margin = list(t = 80, b = 60, l = 20, r = 20),
            annotations = list(
              list(text = paste0(round(pct_worst, 1), "%<br><span style='font-size:11px;'>Critique</span>"),
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 20, color = if (pct_worst > 10) "#d32f2f" else "#0077B6"),
                   xanchor = "center", yanchor = "middle")
            )
          )
      }

      output$zs_donut_gam <- renderPlotly({
        res <- zs_data(); req(res)
        zs_plot_donut(res$class_GAM, "Malnutrition Aiguë<br><sub>WHZ</sub>", "MAS") |> nutri_config()
      })
      output$zs_donut_retard <- renderPlotly({
        res <- zs_data(); req(res)
        zs_plot_donut(res$class_retard, "Retard de Croissance<br><sub>HAZ</sub>", "sévère")
      })
      output$zs_donut_insuf <- renderPlotly({
        res <- zs_data(); req(res)
        zs_plot_donut(res$class_insuf, "Insuffisance Pondérale<br><sub>WAZ</sub>", "sévère")
      })

      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      # STACKED BAR COMPARATIF — WHZ vs HAZ vs WAZ
      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      output$zs_stacked_bar <- renderPlotly({
        res <- zs_data(); req(res)
    
        # Créer la structure pour stacked bar
        bar_data <- tibble::tibble(
          Indicateur = c("WHZ", "HAZ", "WAZ"),
          `Normal` = c(
            round(100 * mean(res$WHZ_calc >= -2, na.rm = TRUE), 1),
            round(100 * mean(res$HAZ_calc >= -2, na.rm = TRUE), 1),
            round(100 * mean(res$WAZ_calc >= -2, na.rm = TRUE), 1)
          ),
          `Modéré (≤ -2)` = c(
            round(100 * mean(res$WHZ_calc < -2 & res$WHZ_calc >= -3, na.rm = TRUE), 1),
            round(100 * mean(res$HAZ_calc < -2 & res$HAZ_calc >= -3, na.rm = TRUE), 1),
            round(100 * mean(res$WAZ_calc < -2 & res$WAZ_calc >= -3, na.rm = TRUE), 1)
          ),
          `Sévère (< -3)` = c(
            round(100 * mean(res$WHZ_calc < -3, na.rm = TRUE), 1),
            round(100 * mean(res$HAZ_calc < -3, na.rm = TRUE), 1),
            round(100 * mean(res$WAZ_calc < -3, na.rm = TRUE), 1)
          )
        )

        p <- ggplot(bar_data |> pivot_longer(-Indicateur, names_to = "Classification", values_to = "Pct"),
                    aes(x = Indicateur, y = Pct, fill = Classification)) +
          geom_bar(stat = "identity", width = 0.6, color = "white", linewidth = 1) +
          geom_text(aes(label = paste0(Pct, "%")), position = position_stack(vjust = 0.5),
                    size = 3.5, fontface = "bold", color = "white") +
          scale_fill_manual(values = c("Normal" = "#4caf50", "Modéré (≤ -2)" = "#f57c00", "Sévère (< -3)" = "#d32f2f")) +
          scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
          labs(title = "Comparaison des trois indicateurs nutritionnels",
               subtitle = "Distribution en trois catégories de malnutrition",
               x = "", y = "Prévalence (%)", fill = "Classification") +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(face = "bold", size = 13, color = "#0B3954", margin = margin(b = 4)),
            plot.subtitle = element_text(size = 10, color = "#666", margin = margin(b = 12)),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "top",
            legend.margin = margin(b = 8)
          )

        ggplotly(p, tooltip = c("x", "y", "fill")) |>
          layout(
            hovermode = "x unified",
            margin = list(t = 90, b = 50)
          ) |> nutri_config()
      })

      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      # COMPARAISON PAR SEXE — Box plots + Tableau
      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      output$zs_sex_whz <- renderPlotly({
        res <- zs_data(); req(res)
        col_s <- isolate(input$zs_col_sexe)
        if (!col_s %in% names(res)) return(plotly_empty())
    
        plot_data <- tibble::tibble(
          Z = res$WHZ_calc,
          Sexe = res[[col_s]]
        ) |> filter(!is.na(Z) & !is.na(Sexe))

        p <- ggplot(plot_data, aes(x = Sexe, y = Z, fill = Sexe)) +
          geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = 1, outlier.size = 2) +
          geom_jitter(alpha = 0.2, width = 0.15, size = 1.5) +
          scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60")) +
          geom_hline(yintercept = c(-3, -2), linetype = "dashed", color = "#999", alpha = 0.5) +
          annotate("rect", ymin = -5, ymax = -3, xmin = -Inf, xmax = Inf, fill = "#d32f2f", alpha = 0.02) +
          annotate("rect", ymin = -3, ymax = -2, xmin = -Inf, xmax = Inf, fill = "#f57c00", alpha = 0.02) +
          labs(title = "WHZ — Poids-pour-Taille", x = "", y = "Z-score") +
          theme_minimal(base_size = 11) +
          theme(legend.position = "none", plot.title = element_text(face = "bold", color = "#0B3954"))

        ggplotly(p, tooltip = c("x", "y")) |> layout(margin = list(t = 50)) |> nutri_config()
      })

      output$zs_sex_haz <- renderPlotly({
        res <- zs_data(); req(res)
        col_s <- isolate(input$zs_col_sexe)
        if (!col_s %in% names(res)) return(plotly_empty())
    
        plot_data <- tibble::tibble(
          Z = res$HAZ_calc,
          Sexe = res[[col_s]]
        ) |> filter(!is.na(Z) & !is.na(Sexe))

        p <- ggplot(plot_data, aes(x = Sexe, y = Z, fill = Sexe)) +
          geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = 1, outlier.size = 2) +
          geom_jitter(alpha = 0.2, width = 0.15, size = 1.5) +
          scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60")) +
          geom_hline(yintercept = c(-3, -2), linetype = "dashed", color = "#999", alpha = 0.5) +
          annotate("rect", ymin = -5, ymax = -3, xmin = -Inf, xmax = Inf, fill = "#d32f2f", alpha = 0.02) +
          annotate("rect", ymin = -3, ymax = -2, xmin = -Inf, xmax = Inf, fill = "#f57c00", alpha = 0.02) +
          labs(title = "HAZ — Taille-pour-Âge", x = "", y = "Z-score") +
          theme_minimal(base_size = 11) +
          theme(legend.position = "none", plot.title = element_text(face = "bold", color = "#0B3954"))

        ggplotly(p, tooltip = c("x", "y")) |> layout(margin = list(t = 50)) |> nutri_config()
      })

      output$zs_sex_waz <- renderPlotly({
        res <- zs_data(); req(res)
        col_s <- isolate(input$zs_col_sexe)
        if (!col_s %in% names(res)) return(plotly_empty())
    
        plot_data <- tibble::tibble(
          Z = res$WAZ_calc,
          Sexe = res[[col_s]]
        ) |> filter(!is.na(Z) & !is.na(Sexe))

        p <- ggplot(plot_data, aes(x = Sexe, y = Z, fill = Sexe)) +
          geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = 1, outlier.size = 2) +
          geom_jitter(alpha = 0.2, width = 0.15, size = 1.5) +
          scale_fill_manual(values = c("M" = "#1976d2", "F" = "#d81b60")) +
          geom_hline(yintercept = c(-3, -2), linetype = "dashed", color = "#999", alpha = 0.5) +
          annotate("rect", ymin = -5, ymax = -3, xmin = -Inf, xmax = Inf, fill = "#d32f2f", alpha = 0.02) +
          annotate("rect", ymin = -3, ymax = -2, xmin = -Inf, xmax = Inf, fill = "#f57c00", alpha = 0.02) +
          labs(title = "WAZ — Poids-pour-Âge", x = "", y = "Z-score") +
          theme_minimal(base_size = 11) +
          theme(legend.position = "none", plot.title = element_text(face = "bold", color = "#0B3954"))

        ggplotly(p, tooltip = c("x", "y")) |> layout(margin = list(t = 50)) |> nutri_config()
      })

      output$zs_sex_table <- renderDT({
        res <- zs_data(); req(res)
        col_s <- isolate(input$zs_col_sexe)
        if (!col_s %in% names(res)) return(datatable(tibble::tibble()))
    
        sex_summ <- res |> 
          mutate(Sexe = !!rlang::sym(col_s)) |>
          filter(!is.na(Sexe)) |>
          group_by(Sexe) |>
          summarise(
            N = n(),
            `WHZ moyen` = round(mean(WHZ_calc, na.rm = TRUE), 3),
            `GAM %` = round(100 * mean(WHZ_calc < -2, na.rm = TRUE), 1),
            `HAZ moyen` = round(mean(HAZ_calc, na.rm = TRUE), 3),
            `Retard %` = round(100 * mean(HAZ_calc < -2, na.rm = TRUE), 1),
            `WAZ moyen` = round(mean(WAZ_calc, na.rm = TRUE), 3),
            `Insuff. %` = round(100 * mean(WAZ_calc < -2, na.rm = TRUE), 1),
            .groups = "drop"
          )
    
        datatable(sex_summ, rownames = FALSE,
          options = list(dom = "t", pageLength = 5, scrollX = TRUE),
          class = "compact stripe hover") |>
          formatStyle(c("GAM %", "Retard %", "Insuff. %"),
            backgroundColor = styleInterval(c(5, 10), c("#e8f5e9", "#fff9c4", "#ffcdd2")))
      })

      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      # DASHBOARD QUALITÉ — Jauges + Résumé
      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    # [DEAD CODE] zs_gauge_whz — output serveur sans UI correspondante
    #   output$zs_gauge_whz <- renderPlotly({
    #     res <- zs_data(); req(res)
    #     n_valid <- sum(!is.na(res$WHZ_calc))
    #     n_flags <- sum(res$flag_WHZ, na.rm = TRUE)
    #     pct_qual <- if (n_valid > 0) round(100 * (1 - n_flags / n_valid), 1) else 100
    # 
    #     plot_ly(
    #       type = "indicator",
    #       mode = "gauge+number+delta",
    #       value = pct_qual,
    #       domain = list(x = c(0, 1), y = c(0, 1)),
    #       title = list(text = "WHZ Qualité"),
    #       gauge = list(
    #         axis = list(range = list(0, 100)),
    #         bar = list(color = if (pct_qual >= 95) "#4caf50" else if (pct_qual >= 90) "#fbc02d" else "#d32f2f"),
    #         steps = list(
    #           list(range = c(0, 50), color = "rgba(211, 47, 47, 0.1)"),
    #           list(range = c(50, 90), color = "rgba(245, 127, 0, 0.1)"),
    #           list(range = c(90, 100), color = "rgba(76, 175, 80, 0.1)")
    #         ),
    #         threshold = list(
    #           line = list(color = "red", width = 2),
    #           thickness = 0.75,
    #           value = 80
    #         )
    #       )
    #     ) |>
    #       layout(margin = list(t = 80, b = 20, l = 60, r = 60), font = list(size = 11))
    #   })

    # [DEAD CODE] zs_gauge_haz — output serveur sans UI correspondante
    #   output$zs_gauge_haz <- renderPlotly({
    #     res <- zs_data(); req(res)
    #     n_valid <- sum(!is.na(res$HAZ_calc))
    #     n_flags <- sum(res$flag_HAZ, na.rm = TRUE)
    #     pct_qual <- if (n_valid > 0) round(100 * (1 - n_flags / n_valid), 1) else 100
    # 
    #     plot_ly(
    #       type = "indicator",
    #       mode = "gauge+number+delta",
    #       value = pct_qual,
    #       domain = list(x = c(0, 1), y = c(0, 1)),
    #       title = list(text = "HAZ Qualité"),
    #       gauge = list(
    #         axis = list(range = list(0, 100)),
    #         bar = list(color = if (pct_qual >= 95) "#4caf50" else if (pct_qual >= 90) "#fbc02d" else "#d32f2f"),
    #         steps = list(
    #           list(range = c(0, 50), color = "rgba(211, 47, 47, 0.1)"),
    #           list(range = c(50, 90), color = "rgba(245, 127, 0, 0.1)"),
    #           list(range = c(90, 100), color = "rgba(76, 175, 80, 0.1)")
    #         ),
    #         threshold = list(
    #           line = list(color = "red", width = 2),
    #           thickness = 0.75,
    #           value = 80
    #         )
    #       )
    #     ) |>
    #       layout(margin = list(t = 80, b = 20, l = 60, r = 60), font = list(size = 11))
    #   })

    # [DEAD CODE] zs_gauge_waz — output serveur sans UI correspondante
    #   output$zs_gauge_waz <- renderPlotly({
    #     res <- zs_data(); req(res)
    #     n_valid <- sum(!is.na(res$WAZ_calc))
    #     n_flags <- sum(res$flag_WAZ, na.rm = TRUE)
    #     pct_qual <- if (n_valid > 0) round(100 * (1 - n_flags / n_valid), 1) else 100
    # 
    #     plot_ly(
    #       type = "indicator",
    #       mode = "gauge+number+delta",
    #       value = pct_qual,
    #       domain = list(x = c(0, 1), y = c(0, 1)),
    #       title = list(text = "WAZ Qualité"),
    #       gauge = list(
    #         axis = list(range = list(0, 100)),
    #         bar = list(color = if (pct_qual >= 95) "#4caf50" else if (pct_qual >= 90) "#fbc02d" else "#d32f2f"),
    #         steps = list(
    #           list(range = c(0, 50), color = "rgba(211, 47, 47, 0.1)"),
    #           list(range = c(50, 90), color = "rgba(245, 127, 0, 0.1)"),
    #           list(range = c(90, 100), color = "rgba(76, 175, 80, 0.1)")
    #         ),
    #         threshold = list(
    #           line = list(color = "red", width = 2),
    #           thickness = 0.75,
    #           value = 80
    #         )
    #       )
    #     ) |>
    #       layout(margin = list(t = 80, b = 20, l = 60, r = 60), font = list(size = 11))
    #   })

    # [DEAD CODE] zs_quality_dashboard — output serveur sans UI correspondante
    #   output$zs_quality_dashboard <- renderUI({
    #     res <- zs_data(); req(res)
    #     whz <- res$WHZ_calc[!is.na(res$WHZ_calc)]
    #     haz <- res$HAZ_calc[!is.na(res$HAZ_calc)]
    #     waz <- res$WAZ_calc[!is.na(res$WAZ_calc)]
    # 
    #     sd_whz <- sd(whz)
    #     sd_haz <- sd(haz)
    #     sd_waz <- sd(waz)
    # 
    #     qual_score <- round(mean(c(
    #       if (sd_whz > 0.8 && sd_whz < 1.2) 5 else if (sd_whz > 0.7 && sd_whz < 1.3) 3 else 1,
    #       if (sd_haz > 0.8 && sd_haz < 1.2) 5 else if (sd_haz > 0.7 && sd_haz < 1.3) 3 else 1,
    #       if (sd_waz > 0.8 && sd_waz < 1.2) 5 else if (sd_waz > 0.7 && sd_waz < 1.3) 3 else 1
    #     )), 1)
    # 
    #     qual_label <- if (qual_score >= 4.5) "Excellent"
    #                   else if (qual_score >= 3.5) "Bon"
    #                   else if (qual_score >= 2.5) "Acceptable"
    #                   else "Problématique"
    # 
    #     qual_color <- if (qual_score >= 4.5) "#4caf50"
    #                   else if (qual_score >= 3.5) "#fbc02d"
    #                   else if (qual_score >= 2.5) "#f57c00"
    #                   else "#d32f2f"
    # 
    #     tags$div(style = paste0("background:white;border-radius:10px;padding:18px;border:1px solid #e8edf2;",
    #                              "margin-bottom:16px;box-shadow:0 2px 8px rgba(0,0,0,.06);"),
    #       tags$div(style = "display:flex;align-items:center;gap:14px;",
    #         tags$div(style = paste0("width:80px;height:80px;border-radius:50%;background:", qual_color,
    #                                  ";display:flex;align-items:center;justify-content:center;",
    #                                  "font-weight:700;font-size:28px;color:white;"),
    #           paste0(qual_score, "/5")
    #         ),
    #         tags$div(
    #           tags$div(style = "font-size:13px;font-weight:700;color:#0B3954;",
    #             "Score de qualité des données"),
    #           tags$div(style = paste0("font-size:20px;font-weight:800;color:", qual_color, ";margin:4px 0;"),
    #             qual_label),
    #           tags$div(style = "font-size:11px;color:#666;margin-top:4px;",
    #             "Évaluation basée sur les écarts-types observés vs attendus OMS N(0,1).")
    #         )
    #       )
    #     )
    #   })

      # Tableau classification
      output$zs_class_table <- renderDT({
        res <- zs_data(); req(res)

        make_cls <- function(col, label) {
          v <- res[[col]]
          v <- v[!is.na(v)]
          if (length(v) == 0) return(NULL)
          tab <- as.data.frame(table(v), stringsAsFactors = FALSE)
          names(tab) <- c("Classification", "N")
          tab$`%` <- round(100 * tab$N / sum(tab$N), 1)
          tab$Indicateur <- label
          tab[, c("Indicateur", "Classification", "N", "%")]
        }

        rows <- dplyr::bind_rows(
          make_cls("class_GAM", "WHZ — Malnutrition Aiguë"),
          make_cls("class_retard", "HAZ — Retard de Croissance"),
          make_cls("class_insuf", "WAZ — Insuffisance Pondérale")
        )
        req(nrow(rows) > 0)
        datatable(rows, rownames = FALSE,
          options = list(dom = "t", pageLength = 15, scrollX = TRUE),
          class = "compact stripe hover")
      })

      # Tableau données individuelles
      output$zs_data_table <- renderDT({
        res <- zs_data(); req(res)
        cols_show <- intersect(c(input$zs_col_sexe, input$zs_col_age, input$zs_col_poids,
                                 input$zs_col_taille, "WHZ_calc", "HAZ_calc", "WAZ_calc",
                                 "class_GAM", "class_retard", "class_insuf",
                                 "flag_WHZ", "flag_HAZ", "flag_WAZ"), names(res))
        d_show <- res[, cols_show, drop = FALSE]
        # Arrondir les Z-scores
        for (zc in c("WHZ_calc", "HAZ_calc", "WAZ_calc")) {
          if (zc %in% names(d_show)) d_show[[zc]] <- round(d_show[[zc]], 2)
        }
        datatable(d_show, rownames = FALSE,
          options = list(pageLength = 20, scrollX = TRUE),
          class = "compact stripe hover") |>
          formatStyle("WHZ_calc", color = styleInterval(c(-3, -2), c("#d32f2f", "#f57c00", "#333"))) |>
          formatStyle("HAZ_calc", color = styleInterval(c(-3, -2), c("#d32f2f", "#f57c00", "#333"))) |>
          formatStyle("WAZ_calc", color = styleInterval(c(-3, -2), c("#d32f2f", "#f57c00", "#333")))
      })

      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      # TÉLÉCHARGEMENTS — CSV et Excel
      # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      output$zs_dl_data <- downloadHandler(
        filename = function() paste0("zscores_oms_", Sys.Date(), ".csv"),
        content = function(file) {
          res <- zs_data(); req(res)
          readr::write_csv(res, file)
        }
      )

      output$zs_dl_xlsx <- downloadHandler(
        filename = function() paste0("zscores_oms_", Sys.Date(), ".xlsx"),
        content = function(file) {
          res <- zs_data(); req(res)
          writexl::write_xlsx(
            list(
              "Z-scores" = res,
              "Résumé" = resume_zscores(res)
            ),
            file
          )
        }
      )

      # Flags summary
      output$zs_flags_summary <- renderUI({
        res <- zs_data(); req(res)
        n_flag_whz <- sum(res$flag_WHZ, na.rm = TRUE)
        n_flag_haz <- sum(res$flag_HAZ, na.rm = TRUE)
        n_flag_waz <- sum(res$flag_WAZ, na.rm = TRUE)
        total_flags <- n_flag_whz + n_flag_haz + n_flag_waz

        flag_color <- if (total_flags == 0) "#4caf50" else if (total_flags <= 5) "#f57c00" else "#d32f2f"

        tags$div(style = paste0("background:white;border-left:5px solid ", flag_color,
                                 ";border-radius:8px;padding:14px;margin-bottom:12px;box-shadow:0 2px 4px rgba(0,0,0,.08);"),
          tags$div(style = "font-size:14px;font-weight:600;", paste("Total flags SMART :", total_flags)),
          tags$div(style = "font-size:12px;color:#666;margin-top:4px;",
            paste0("WHZ flags (|Z|>5) : ", n_flag_whz, " | ",
                   "HAZ flags (|Z|>6) : ", n_flag_haz, " | ",
                   "WAZ flags (Z<-6 ou Z>5) : ", n_flag_waz)),
          if (total_flags > 0)
            tags$div(style = "font-size:11px;color:#d32f2f;margin-top:6px;",
              icon("exclamation-triangle"), " Les observations flaggées peuvent indiquer des erreurs de mesure.")
        )
      })

      # Tableau flags
      output$zs_flags_table <- renderDT({
        res <- zs_data(); req(res)
        flagged <- res[res$flag_WHZ | res$flag_HAZ | res$flag_WAZ, , drop = FALSE]
        if (nrow(flagged) == 0) {
          return(datatable(tibble::tibble(Message = "Aucun flag détecté — toutes les valeurs sont dans les limites SMART."),
                           rownames = FALSE, options = list(dom = "t")))
        }
        cols_show <- intersect(c(input$zs_col_sexe, input$zs_col_age, input$zs_col_poids,
                                 input$zs_col_taille, "WHZ_calc", "HAZ_calc", "WAZ_calc",
                                 "flag_WHZ", "flag_HAZ", "flag_WAZ"), names(flagged))
        d_show <- flagged[, cols_show, drop = FALSE]
        for (zc in c("WHZ_calc", "HAZ_calc", "WAZ_calc")) {
          if (zc %in% names(d_show)) d_show[[zc]] <- round(d_show[[zc]], 2)
        }
        datatable(d_show, rownames = FALSE,
          options = list(pageLength = 20, scrollX = TRUE),
          class = "compact stripe hover")
      })
      # ══════════════════════════════════════════════════════════════════
      # MODULE MUAC — Périmètre Brachial (onglet Z-scores)
      # ══════════════════════════════════════════════════════════════════

      # Helper : extraire le vecteur MUAC (colonne .muac ou auto-détection)
      .muac_vec_r <- reactive({
        d_cur <- df_f(); req(d_cur)
        mc <- intersect(c(".muac", "muac", "MUAC", "pb", "PB", "mid_upper_arm_circ",
                          "muac_mm", "pbm"), names(d_cur))
        if (length(mc) == 0) return(NULL)
        as.numeric(d_cur[[mc[1]]])
      })

      output$zs_muac_group_ui <- renderUI({
        d_cur <- df_f(); req(d_cur)
        choices_grp <- c("Global" = "", "Région" = "region",
                         "Département" = "departement",
                         "Round" = "survey_round", "Sexe" = "sexe",
                         "Cluster" = "cluster")
        valid_choices <- choices_grp[c(TRUE, sapply(choices_grp[-1], function(v) v %in% names(d_cur)))]
        selectInput(ns("zs_muac_group_var"), "Grouper par :",
                    choices = valid_choices, selected = "", width = "100%")
      })

      output$zs_muac_kpi_row <- renderUI({
        muac_v <- .muac_vec_r(); req(muac_v)
        muac_v <- muac_v[!is.na(muac_v)]
        req(length(muac_v) > 0)
        n_tot    <- length(muac_v)
        n_mag    <- sum(muac_v < 125)
        n_mas    <- sum(muac_v < 115)
        n_mam    <- sum(muac_v >= 115 & muac_v < 125)
        pct_mag  <- round(n_mag / n_tot * 100, 1)
        pct_mas  <- round(n_mas / n_tot * 100, 1)
        pct_mam  <- round(n_mam / n_tot * 100, 1)
        ci_res   <- tryCatch(binom::binom.wilson(n_mag, n_tot), error = function(e) NULL)
        ci_lo    <- if (!is.null(ci_res)) round(ci_res$lower * 100, 1) else "?"
        ci_hi    <- if (!is.null(ci_res)) round(ci_res$upper * 100, 1) else "?"

        sev_col  <- if (pct_mag >= 15) "#C0392B" else if (pct_mag >= 10) "#E67E22" else if (pct_mag >= 5) "#F1C40F" else "#27AE60"
        sev_lbl  <- if (pct_mag >= 15) "Critique" else if (pct_mag >= 10) "Sérieux" else if (pct_mag >= 5) "Alerte" else "Acceptable"

        make_muac_box <- function(val, lbl, sub_lbl, color) {
          tags$div(style=paste0("background:white;border-radius:10px;padding:14px 16px;",
                                 "border-left:4px solid ", color, ";text-align:center;flex:1;"),
            tags$div(style=paste0("font-size:26px;font-weight:800;color:", color, ";"), val),
            tags$div(style="font-size:12px;font-weight:600;color:#333;", lbl),
            tags$div(style="font-size:10px;color:#888;margin-top:2px;", sub_lbl)
          )
        }
        tags$div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
          make_muac_box(paste0(pct_mag, "%"), "MAG (MUAC < 125mm)",
            paste0("IC 95% : [", ci_lo, "–", ci_hi, "%] | n=", n_mag, "/", n_tot), sev_col),
          make_muac_box(paste0(pct_mas, "%"), "MAS (MUAC < 115mm)",
            paste0("Malnutrition aiguë sévère | n=", n_mas), "#C0392B"),
          make_muac_box(paste0(pct_mam, "%"), "MAM (115–124mm)",
            paste0("Malnutrition aiguë modérée | n=", n_mam), "#E67E22"),
          make_muac_box(paste0(round(mean(muac_v), 1), "mm"), "MUAC moyen",
            paste0("Médiane : ", median(muac_v), "mm | SD : ", round(sd(muac_v), 1), "mm"), "#0077B6"),
          tags$div(style=paste0("background:white;border-radius:10px;padding:14px 16px;",
                                 "border-left:4px solid ", sev_col, ";text-align:center;flex:1;"),
            tags$div(style=paste0("font-size:14px;font-weight:800;color:", sev_col, ";"), sev_lbl),
            tags$div(style="font-size:11px;font-weight:600;color:#333;", "Niveau MAG"),
            tags$div(style="font-size:10px;color:#888;",
                     if (pct_mag >= 15) "≥15% — Urgence nutritionnelle" else
                     if (pct_mag >= 10) "10–14% — Situation sérieuse" else
                     if (pct_mag >= 5)  "5–9% — Seuil d'alerte" else "< 5% — Acceptable")
          )
        )
      })

      output$zs_muac_dist <- renderPlotly({
        muac_v <- .muac_vec_r(); req(muac_v)
        d_cur  <- df_f(); req(d_cur)
        grp_var <- if (!is.null(input$zs_muac_by_sex) && isTRUE(input$zs_muac_by_sex)) {
          sex_col <- intersect(c("sexe", "sex", "genre"), names(d_cur))
          if (length(sex_col) > 0) d_cur[[sex_col[1]]] else NULL
        } else NULL
        tryCatch(
          plot_muac_distribution(muac_v, group_var = grp_var),
          error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
        )
      })

      output$zs_muac_donut <- renderPlotly({
        muac_v <- .muac_vec_r(); req(muac_v)
        muac_v_clean <- muac_v[!is.na(muac_v)]
        req(length(muac_v_clean) > 0)
        counts <- c(
          sum(muac_v_clean < 115),
          sum(muac_v_clean >= 115 & muac_v_clean < 125),
          sum(muac_v_clean >= 125 & muac_v_clean < 135),
          sum(muac_v_clean >= 135)
        )
        labels <- MUAC_CATEGORIES$cat
        pcts   <- round(counts / sum(counts) * 100, 1)
        plot_ly(labels = labels, values = counts, type = "pie",
                hole = 0.5, textinfo = "percent",
                marker = list(colors = MUAC_CATEGORIES$couleur,
                              line = list(color = "white", width = 1.5)),
                hovertemplate = "<b>%{label}</b><br>%{value} enfants (%{percent})<extra></extra>") |>
          layout(
            title = list(text = "<b>Classification MUAC</b>",
                         font = list(size = 13, color = "#0B3954"), x = 0.5),
            showlegend = TRUE,
            legend = list(orientation = "h", y = -0.2, font = list(size = 10)),
            paper_bgcolor = "white"
          )
      })

      output$zs_muac_by_group <- renderPlotly({
        muac_v <- .muac_vec_r(); req(muac_v)
        d_cur  <- df_f(); req(d_cur)
        grp_var_name <- if (!is.null(input$zs_muac_group_var) && input$zs_muac_group_var != "")
          input$zs_muac_group_var else NULL
        req(grp_var_name, grp_var_name %in% names(d_cur))
        grp_vec <- d_cur[[grp_var_name]]
        tryCatch(
          plot_muac_by_group(muac_v, grp_vec, grp_var_name),
          error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
        )
      })

      output$zs_muac_vs_whz_panel <- renderUI({
        res <- zs_data()
        if (is.null(res) || !"WHZ_calc" %in% names(res))
          return(tags$div(style="background:#f5f5f5;border-radius:8px;padding:12px;font-size:11px;color:#888;",
            icon("info-circle"), " Calculez les Z-scores pour afficher la comparaison MUAC vs WHZ."))
        tagList(
          tags$div(class="section-header", "Comparaison MUAC vs WHZ (méthodes de mesure MAG)"),
          plotlyOutput(ns("zs_muac_vs_whz_plot"), height="350px")
        )
      })

      output$zs_muac_vs_whz_plot <- renderPlotly({
        muac_v <- .muac_vec_r(); req(muac_v)
        res    <- zs_data(); req(res, "WHZ_calc" %in% names(res))
        d_cur  <- df_f(); req(d_cur)
        sex_col <- intersect(c("sexe", "sex", "genre"), names(d_cur))
        sex_vec <- if (length(sex_col) > 0) d_cur[[sex_col[1]]] else NULL
        tryCatch(
          plot_muac_vs_whz(muac_v, res$WHZ_calc, sex_vec),
          error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
        )
      })

      output$zs_muac_table <- renderDT({
        muac_v <- .muac_vec_r(); req(muac_v)
        d_cur  <- df_f(); req(d_cur)
        grp_var_name <- if (!is.null(input$zs_muac_group_var) && input$zs_muac_group_var != "")
          input$zs_muac_group_var else NULL
        grp_vec <- if (!is.null(grp_var_name) && grp_var_name %in% names(d_cur)) d_cur[[grp_var_name]] else NULL
        summ <- tryCatch(
          calc_muac_summary(muac_v, grp_vec),
          error = function(e) NULL
        )
        req(summ)
        datatable(
          summ |> rename(Groupe = groupe, N_total = N,
                         `Moy (mm)` = Moy_MUAC, `Méd (mm)` = Med_MUAC, `SD` = SD_MUAC,
                         `n MAS` = N_MAS, `n MAM` = N_MAM, `n MAG` = N_MAG,
                         `% MAS` = Pct_MAS, `% MAM` = Pct_MAM, `% MAG` = Pct_MAG,
                         `IC lo MAG` = MAG_CI_lo, `IC hi MAG` = MAG_CI_hi),
          rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15),
          class = "compact stripe hover"
        ) |>
          formatStyle("% MAG",
            background = styleColorBar(c(0, 50), "#E67E22"),
            backgroundSize = "100% 75%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
          )
      })

      # ══════════════════════════════════════════════════════════════════
      # MODULE Z-SCORES PAR TRANCHE D'ÂGE
      # ══════════════════════════════════════════════════════════════════

      output$zs_age_boxplot <- renderPlotly({
        res <- zs_data(); req(res)
        ind <- if (!is.null(input$zs_age_indicator)) input$zs_age_indicator else "WHZ_calc"
        req(ind %in% names(res))
        tryCatch(
          plot_zscore_by_age_group(res, ind),
          error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
        )
      })

      output$zs_age_table <- renderDT({
        res <- zs_data(); req(res)
        tbl <- tryCatch(calc_zscore_by_age_group(res), error = function(e) NULL)
        req(tbl)
        datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
                  class = "compact stripe")
      })
  })
}
