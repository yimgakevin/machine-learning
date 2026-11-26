# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  ui_epidemio_expert.R — Tabs expert UI pour épidémiologie analytique       ║
# ║  Logistique uni/multi, Poisson, Dose-réponse, Interaction, I², PAF, ROC    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

epid_expert_tabs <- function(ns) {
  tagList(

    # ── Tab 1 : Régression logistique univariée (screening) ─────────────
    tabPanel(tags$span(icon("filter"), " Screening logistique"),
      br(),
      fluidRow(
        box(width = 3, status = "primary", solidHeader = TRUE,
            title = tags$span(icon("filter"), " Screening univarié"),
            uiOutput(ns("epid_screen_outcome_ui")),
            uiOutput(ns("epid_screen_vars_ui")),
            sliderInput(ns("epid_screen_conf"), "Niveau de confiance :",
                        min = 0.80, max = 0.99, value = 0.95, step = 0.01, width = "100%"),
            hr(),
            actionButton(ns("run_screen"), "Lancer screening",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "font-size:13px;padding:10px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Screening univarié :"), tags$br(),
              "Régression logistique pour chaque exposition séparément. ",
              "Identifie les candidats (p<0.20) pour le modèle multivarié."
            )
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = tags$span(icon("table"), " Résultats du screening"),
            DTOutput(ns("epid_screen_table")),
            br(),
            plotlyOutput(ns("epid_screen_forest"), height = "450px")
        )
      )
    ),

    # ── Tab 2 : Régression logistique multivariée ───────────────────────
    tabPanel(tags$span(icon("project-diagram"), " Logistique multivariée"),
      br(),
      fluidRow(
        box(width = 3, status = "warning", solidHeader = TRUE,
            title = tags$span(icon("cog"), " Paramètres"),
            uiOutput(ns("epid_multi_outcome_ui")),
            uiOutput(ns("epid_multi_pred_ui")),
            checkboxInput(ns("epid_multi_stepwise"), "Sélection stepwise (AIC)", FALSE),
            sliderInput(ns("epid_multi_conf"), "IC :",
                        min = 0.80, max = 0.99, value = 0.95, step = 0.01, width = "100%"),
            hr(),
            actionButton(ns("run_multi_logistic"), "Ajuster le modèle",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "font-size:13px;padding:10px;"),
            hr(),
            uiOutput(ns("epid_multi_summary"))
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = "Résultats du modèle multivarié",
            tabsetPanel(
              tabPanel("OR ajustés",
                DTOutput(ns("epid_multi_coef_table")),
                br(),
                plotlyOutput(ns("epid_multi_forest"), height = "400px")
              ),
              tabPanel("Diagnostics",
                DTOutput(ns("epid_multi_diag_table")),
                br(),
                uiOutput(ns("epid_multi_diag_interp"))
              ),
              tabPanel("ROC",
                plotlyOutput(ns("epid_multi_roc"), height = "400px")
              ),
              tabPanel("VIF",
                DTOutput(ns("epid_multi_vif_table"))
              )
            )
        )
      )
    ),

    # ── Tab 3 : Régression de Poisson ───────────────────────────────────
    tabPanel(tags$span(icon("calculator"), " Poisson / BN"),
      br(),
      fluidRow(
        box(width = 3, status = "success", solidHeader = TRUE,
            title = tags$span(icon("calculator"), " Paramètres"),
            uiOutput(ns("epid_pois_outcome_ui")),
            uiOutput(ns("epid_pois_pred_ui")),
            uiOutput(ns("epid_pois_offset_ui")),
            radioButtons(ns("epid_pois_type"), "Type de modèle :",
                         choices = c("Poisson" = "poisson",
                                     "Binomiale négative" = "negbin"),
                         selected = "poisson", inline = TRUE),
            hr(),
            actionButton(ns("run_poisson"), "Ajuster",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "font-size:13px;padding:10px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Poisson :"), " Pour données de comptage. ",
              "IRR = Incidence Rate Ratio.", tags$br(),
              tags$b("Binomiale neg. :"), " Si surdispersion détectée."
            )
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = "Résultats — IRR (Incidence Rate Ratio)",
            DTOutput(ns("epid_pois_coef_table")),
            br(),
            uiOutput(ns("epid_pois_diag"))
        )
      )
    ),

    # ── Tab 4 : Dose-réponse avancée ────────────────────────────────────
    tabPanel(tags$span(icon("chart-line"), " Dose-réponse"),
      br(),
      fluidRow(
        box(width = 3, status = "danger", solidHeader = TRUE,
            title = tags$span(icon("sort-amount-up"), " Paramètres"),
            uiOutput(ns("epid_dr_dose_ui")),
            uiOutput(ns("epid_dr_outcome_ui")),
            sliderInput(ns("epid_dr_ndoses"), "Nombre de catégories (quintiles) :",
                        min = 3, max = 10, value = 5, step = 1, width = "100%"),
            hr(),
            actionButton(ns("run_dose_response"), "Analyser dose-réponse",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "font-size:13px;padding:10px;")
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = "Analyse dose-réponse",
            plotlyOutput(ns("epid_dr_plot"), height = "380px"),
            br(),
            DTOutput(ns("epid_dr_table")),
            br(),
            uiOutput(ns("epid_dr_summary"))
        )
      )
    ),

    # ── Tab 5 : Interaction / Modification d'effet ──────────────────────
    tabPanel(tags$span(icon("random"), " Interaction"),
      br(),
      fluidRow(
        box(width = 3, status = "primary", solidHeader = TRUE,
            title = tags$span(icon("random"), " Paramètres"),
            uiOutput(ns("epid_inter_outcome_ui")),
            uiOutput(ns("epid_inter_expo1_ui")),
            uiOutput(ns("epid_inter_expo2_ui")),
            hr(),
            actionButton(ns("run_interaction"), "Tester l'interaction",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "font-size:13px;padding:10px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Interaction :"), tags$br(),
              "Teste si l'effet d'une exposition dépend du niveau ",
              "d'une autre variable (modification d'effet). ",
              "LRT compare modèle avec vs sans terme d'interaction."
            )
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = "Résultats — Test d'interaction",
            uiOutput(ns("epid_inter_result")),
            br(),
            plotlyOutput(ns("epid_inter_plot"), height = "380px"),
            br(),
            DTOutput(ns("epid_inter_coef_table"))
        )
      )
    ),

    # ── Tab 6 : Hétérogénéité I² ───────────────────────────────────────
    tabPanel(tags$span(icon("chart-bar"), " Hétérogénéité I²"),
      br(),
      fluidRow(
        box(width = 3, status = "warning", solidHeader = TRUE,
            title = tags$span(icon("chart-bar"), " Paramètres"),
            uiOutput(ns("epid_het_expo_ui")),
            uiOutput(ns("epid_het_outcome_ui")),
            uiOutput(ns("epid_het_strata_ui")),
            hr(),
            actionButton(ns("run_heterogeneity"), "Calculer I²",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "font-size:13px;padding:10px;"),
            hr(),
            uiOutput(ns("epid_het_summary"))
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = "Forest plot méta-analytique + I²",
            plotlyOutput(ns("epid_het_forest"), height = "450px"),
            br(),
            DTOutput(ns("epid_het_table"))
        )
      )
    ),

    # ── Tab 7 : PAF ajusté ─────────────────────────────────────────────
    tabPanel(tags$span(icon("bullseye"), " PAF ajusté"),
      br(),
      fluidRow(
        box(width = 3, status = "danger", solidHeader = TRUE,
            title = tags$span(icon("bullseye"), " Paramètres"),
            uiOutput(ns("epid_paf_expo_ui")),
            uiOutput(ns("epid_paf_outcome_ui")),
            uiOutput(ns("epid_paf_conf_ui")),
            hr(),
            actionButton(ns("run_paf_adj"), "Calculer PAF ajusté",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "font-size:13px;padding:10px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("PAF ajusté (Bruzzi) :"), tags$br(),
              "Fraction attribuable en population, ajustée pour les ",
              "facteurs de confusion via régression logistique. ",
              "IC par bootstrap (1000 rééchantillonnages)."
            )
        ),
        box(width = 9, status = "info", solidHeader = TRUE,
            title = "Fraction Attribuable en Population (ajustée)",
            uiOutput(ns("epid_paf_result")),
            br(),
            uiOutput(ns("epid_paf_details"))
        )
      )
    )
  )
}

cat("ui_epidemio_expert.R chargé — 7 tabs épidémiologie expert\n")
