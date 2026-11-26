# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  ui_temporel_expert.R — Interface Expert Analyse Temporelle Avancée       ║
# ║  7 Onglets Expert pour NutriAnalyse Pro v2                                 ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

temporal_expert_tabs <- function(ns) {
  tagList(

    # ── TAB 1: Comparaison Moyennes Continues ──────────────────────────────
    tabPanel(
      title = tags$span(icon("balance-scale"), " Moyennes Continues"),
      value = "tab_temp_continu",
      tags$br(),
      fluidRow(
        box(width = 3, status = "primary", solidHeader = TRUE,
            title = tags$span(icon("cogs"), " Paramètres"),
            tags$p(style = "font-size:11px;color:#666;",
                   "Comparaison de moyennes continues entre 2 vagues ",
                   "(t-test Welch + Wilcoxon + Cohen d bootstrap)"),
            uiOutput(ns("temp_cont_vars_ui")),
            sliderInput(ns("temp_cont_conf"), "Niveau de confiance :",
                        min = 0.90, max = 0.99, value = 0.95, step = 0.01, width = "100%"),
            actionButton(ns("run_temp_continu"), "Lancer l'analyse",
                         icon = icon("play"), class = "btn-primary",
                         style = "width:100%;padding:10px;font-weight:700;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("table"), " Résultats"),
            DTOutput(ns("temp_cont_table")),
            tags$br(),
            plotlyOutput(ns("temp_cont_forest"), height = "380px"),
            uiOutput(ns("temp_cont_summary"))
        )
      )
    ),

    # ── TAB 2: Difference-in-Differences ───────────────────────────────────
    tabPanel(
      title = tags$span(icon("exchange-alt"), " Diff-in-Diff"),
      value = "tab_temp_did",
      tags$br(),
      fluidRow(
        box(width = 3, status = "warning", solidHeader = TRUE,
            title = tags$span(icon("cogs"), " Paramètres DID"),
            tags$p(style = "font-size:11px;color:#666;",
                   "Évaluation d'impact par double différence : ",
                   "(traitement vs contrôle) × (avant vs après)"),
            uiOutput(ns("did_outcome_ui")),
            uiOutput(ns("did_group_ui")),
            uiOutput(ns("did_treat_ui")),
            uiOutput(ns("did_control_ui")),
            actionButton(ns("run_did"), "Estimer le DID",
                         icon = icon("calculator"), class = "btn-warning",
                         style = "width:100%;padding:10px;font-weight:700;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("chart-line"), " Résultats DID"),
            uiOutput(ns("did_estimate_box")),
            fluidRow(
              column(6, DTOutput(ns("did_coef_table"))),
              column(6, plotlyOutput(ns("did_plot"), height = "350px"))
            ),
            DTOutput(ns("did_cells_table")),
            uiOutput(ns("did_summary"))
        )
      )
    ),

    # ── TAB 3: Analyse de Tendance Multi-Rounds ───────────────────────────
    tabPanel(
      title = tags$span(icon("chart-line"), " Tendances"),
      value = "tab_temp_trend",
      tags$br(),
      fluidRow(
        box(width = 3, status = "success", solidHeader = TRUE,
            title = tags$span(icon("cogs"), " Paramètres"),
            tags$p(style = "font-size:11px;color:#666;",
                   "Analyse de tendance : Cochran-Armitage (proportions), ",
                   "Jonckheere-Terpstra (continu), régression linéaire"),
            uiOutput(ns("trend_vars_ui")),
            uiOutput(ns("trend_order_ui")),
            actionButton(ns("run_trend"), "Analyser les tendances",
                         icon = icon("chart-line"), class = "btn-success",
                         style = "width:100%;padding:10px;font-weight:700;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("signal"), " Résultats Tendances"),
            DTOutput(ns("trend_summary_table")),
            tags$br(),
            uiOutput(ns("trend_var_selector_ui")),
            plotlyOutput(ns("trend_plot"), height = "380px"),
            uiOutput(ns("trend_interpretation"))
        )
      )
    ),

    # ── TAB 4: CUSUM — Alerte Précoce ─────────────────────────────────────
    tabPanel(
      title = tags$span(icon("exclamation-triangle"), " CUSUM Alerte"),
      value = "tab_temp_cusum",
      tags$br(),
      fluidRow(
        box(width = 3, status = "danger", solidHeader = TRUE,
            title = tags$span(icon("cogs"), " Paramètres CUSUM"),
            tags$p(style = "font-size:11px;color:#666;",
                   "Cumulative Sum Control Chart — détection de décalages ",
                   "par rapport à une valeur de référence"),
            uiOutput(ns("cusum_var_ui")),
            uiOutput(ns("cusum_zone_ui")),
            numericInput(ns("cusum_k"), "Paramètre k (allowance) :",
                         value = 0.5, min = 0.1, max = 2, step = 0.1, width = "100%"),
            numericInput(ns("cusum_h"), "Paramètre h (seuil décision) :",
                         value = 4, min = 1, max = 10, step = 0.5, width = "100%"),
            numericInput(ns("cusum_target"), "Moyenne cible (optionnel) :",
                         value = NA, width = "100%"),
            tags$p(style = "font-size:10px;color:#999;",
                   "Laisser NA pour utiliser la moyenne observée"),
            actionButton(ns("run_cusum"), "Lancer le CUSUM",
                         icon = icon("exclamation-triangle"), class = "btn-danger",
                         style = "width:100%;padding:10px;font-weight:700;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("chart-area"), " Carte de Contrôle CUSUM"),
            uiOutput(ns("cusum_alert_box")),
            plotlyOutput(ns("cusum_plot"), height = "380px"),
            DTOutput(ns("cusum_table")),
            uiOutput(ns("cusum_summary"))
        )
      )
    ),

    # ── TAB 5: Bootstrap Temporal ──────────────────────────────────────────
    tabPanel(
      title = tags$span(icon("random"), " Bootstrap IC"),
      value = "tab_temp_bootstrap",
      tags$br(),
      fluidRow(
        box(width = 3, status = "primary", solidHeader = TRUE,
            title = tags$span(icon("cogs"), " Paramètres Bootstrap"),
            tags$p(style = "font-size:11px;color:#666;",
                   "IC Bootstrap (percentile + BCa) pour les différences ",
                   "inter-vagues — sans hypothèse de normalité"),
            uiOutput(ns("boot_temp_vars_ui")),
            numericInput(ns("boot_temp_n"), "Nombre de rééchantillonnages :",
                         value = 2000, min = 500, max = 10000, step = 500, width = "100%"),
            sliderInput(ns("boot_temp_conf"), "Niveau de confiance :",
                        min = 0.90, max = 0.99, value = 0.95, step = 0.01, width = "100%"),
            actionButton(ns("run_boot_temp"), "Lancer le Bootstrap",
                         icon = icon("random"), class = "btn-primary",
                         style = "width:100%;padding:10px;font-weight:700;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("table"), " Résultats Bootstrap"),
            DTOutput(ns("boot_temp_table")),
            tags$br(),
            plotlyOutput(ns("boot_temp_forest"), height = "380px"),
            uiOutput(ns("boot_temp_summary"))
        )
      )
    ),

    # ── TAB 6: Heatmap Temporelle Multivariée ─────────────────────────────
    tabPanel(
      title = tags$span(icon("th"), " Heatmap Multi"),
      value = "tab_temp_heatmap",
      tags$br(),
      fluidRow(
        box(width = 3, status = "info", solidHeader = TRUE,
            title = tags$span(icon("cogs"), " Paramètres Heatmap"),
            tags$p(style = "font-size:11px;color:#666;",
                   "Matrice multi-indicateur × multi-vague avec tests de ",
                   "significativité entre vagues consécutives"),
            uiOutput(ns("hm_vars_ui")),
            radioButtons(ns("hm_show"), "Afficher :",
                         choices = c("Valeurs par vague" = "values",
                                     "Variations (Δ)" = "changes"),
                         selected = "values", width = "100%"),
            actionButton(ns("run_heatmap"), "Générer la Heatmap",
                         icon = icon("th"), class = "btn-info",
                         style = "width:100%;padding:10px;font-weight:700;color:white;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("fire"), " Heatmap Temporelle"),
            plotlyOutput(ns("hm_plot"), height = "450px"),
            uiOutput(ns("hm_summary"))
        )
      )
    ),

    # ── TAB 7: Profil Évolutif par Zone ───────────────────────────────────
    tabPanel(
      title = tags$span(icon("project-diagram"), " Trajectoires Zones"),
      value = "tab_temp_trajectory",
      tags$br(),
      fluidRow(
        box(width = 3, status = "success", solidHeader = TRUE,
            title = tags$span(icon("cogs"), " Paramètres"),
            tags$p(style = "font-size:11px;color:#666;",
                   "Trajectoires zone-level avec identification des ",
                   "zones les plus divergentes de la tendance nationale"),
            uiOutput(ns("traj_var_ui")),
            uiOutput(ns("traj_zone_ui")),
            actionButton(ns("run_trajectory"), "Tracer les trajectoires",
                         icon = icon("project-diagram"), class = "btn-success",
                         style = "width:100%;padding:10px;font-weight:700;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("route"), " Trajectoires par Zone"),
            plotlyOutput(ns("traj_plot"), height = "420px"),
            tags$br(),
            tags$h5(style = "color:#00426A;font-weight:700;",
                    icon("search"), " Zones les plus divergentes"),
            DTOutput(ns("traj_anomaly_table")),
            uiOutput(ns("traj_summary"))
        )
      )
    )
  )
}

cat("✅ ui_temporel_expert.R chargé — 7 onglets expert\n")
