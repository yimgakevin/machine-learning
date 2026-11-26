# mod_predictif.R -- Module Shiny Expert : Modelisation predictive (v2)
# ═══════════════════════════════════════════════════════════════════════════════
# 5 modeles : GLM | Random Forest | XGBoost | SVM (RBF) | k-NN
# 12 onglets : Metriques | ROC & DeLong | PR & Lift | Confusion | Calibration
#              Importance | Permutation | Apprentissage | Seuil | Stabilite
#              Export | Scoring
# ═══════════════════════════════════════════════════════════════════════════════
source("module_predictif.R")

pk_info <- function(text, icon_name = "lightbulb", col = "#0077B6", bg = "#e8f6fc") {
  tags$div(
    style = paste0("margin-top:8px;padding:10px 12px;border-radius:8px;background:",
                   bg, ";border-left:4px solid ", col,
                   ";font-size:12px;color:#333;line-height:1.55;"),
    icon(icon_name), " ", tags$span(text)
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
predictifUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "background:linear-gradient(135deg,#f0f8ff 0%,#e8f4f8 100%);
               border-left:4px solid #008DC9;border-radius:6px;padding:15px 20px;margin-bottom:15px;",
      tags$h4(style = "margin:0 0 8px 0;color:#008DC9;",
              icon("brain"), "Mod\u00e9lisation Pr\u00e9dictive Expert"),
      tags$p(style = "margin:0;color:#555;font-size:13px;line-height:1.5;",
        "Pipeline complet de Machine Learning : ",
        tags$b("5 mod\u00e8les"), " (GLM, Random Forest, XGBoost, SVM, k-NN), ",
        tags$b("15+ m\u00e9triques"), " (AUC, PR-AUC, MCC, Brier, LogLoss\u2026), ",
        "courbes ROC/PR/Gain/Lift, importance par permutation, courbes d'apprentissage, ",
        "stabilité multi-seeds, scoring de nouvelles données, exports 300 dpi/PDF. ",
        "Classification binaire ou multiclasse."
      )
    ),
    fluidRow(
      # ── Panneau gauche : Configuration ──────────────────────────
      box(width = 3, status = "primary", solidHeader = TRUE,
          title = tags$span(icon("cog"), " Configuration"),

          tags$div(class = "section-header", "1. Variable cible"),
          tags$div(style = "font-size:10px;color:#666;margin-bottom:4px;line-height:1.4;",
            "S\u00e9lectionnez la variable \u00e0 pr\u00e9dire. D\u00e9tection ",
            "automatique binaire vs multiclasse."
          ),
          uiOutput(ns("sel_outcome")),
          uiOutput(ns("outcome_info")),
          hr(),

          tags$div(class = "section-header", "2. Pr\u00e9dicteurs"),
          tags$div(style = "font-size:10px;color:#666;margin-bottom:4px;",
            "Variables explicatives \u00e0 inclure dans le mod\u00e8le."),
          uiOutput(ns("sel_predictors")),
          hr(),

          tags$div(class = "section-header", "3. Mod\u00e8les"),
          uiOutput(ns("models_ui")),
          sliderInput(ns("k_folds"), "Nombre de folds (CV) :",
                      min = 3, max = 10, value = 5, step = 1),
          sliderInput(ns("train_pct"), "% Entra\u00eenement :",
                      min = 60, max = 85, value = 70, step = 5),
          hr(),

          tags$div(class = "section-header", "4. Options"),
          uiOutput(ns("options_ui")),
          hr(),

          actionButton(ns("run_pred"), "\u25b6 Lancer le pipeline",
                       class = "btn btn-oms btn-block", width = "100%",
                       style = "font-size:13px;padding:10px;font-weight:600;"),
          br(),
          uiOutput(ns("pipeline_info"))
      ),

      # ── Panneau droit : Resultats (10 onglets) ─────────────────
      box(width = 9, status = "info", solidHeader = TRUE,
          title = tags$span(icon("chart-line"), " R\u00e9sultats"),
          uiOutput(ns("kpi_panel")),
          tabsetPanel(id = ns("pred_tabs"),

            # --- Onglet 1 : Metriques (15+) ---
            tabPanel(tags$span(icon("table"), " M\u00e9triques"), br(),
              tags$div(style = "margin-bottom:10px;",
                tags$span(style = "font-size:12px;color:#666;",
                  icon("info-circle"),
                  " 15 m\u00e9triques pour la classification binaire, ",
                  "9 pour la multiclasse. Les meilleures valeurs sont ",
                  "surlign\u00e9es automatiquement.")),
              DTOutput(ns("metrics_dt")),
              tags$div(style = "font-size:10px;color:#666;margin-top:4px;",
                "Échelle : ",
                tags$span(style = "background:#fef2f2;padding:1px 6px;border-radius:3px;", "< 0.50"),
                " ", tags$span(style = "background:#fff7ed;padding:1px 6px;border-radius:3px;", "0.50–0.70"),
                " ", tags$span(style = "background:#f0fdf4;padding:1px 6px;border-radius:3px;", "0.70–0.85"),
                " ", tags$span(style = "background:#dcfce7;padding:1px 6px;border-radius:3px;", "> 0.85")),
              br(),
              uiOutput(ns("best_model_msg")), br(),
              plotlyOutput(ns("metrics_bar"), height = "420px")
            ),

            # --- Onglet 2 : ROC & DeLong ---
            tabPanel(tags$span(icon("chart-area"), " ROC & DeLong"), br(),
              uiOutput(ns("roc_info")),
              fluidRow(
                column(7, plotlyOutput(ns("roc_plot"), height = "500px")),
                column(5,
                  tags$div(style = "margin-top:20px;",
                    uiOutput(ns("sel_model_roc_ci")),
                    plotlyOutput(ns("roc_ci_plot"), height = "400px")
                  )
                )
              ),
              br(), uiOutput(ns("auc_comparison")),
              br(), plotlyOutput(ns("cv_fold_heatmap"), height = "300px")
            ),

            # --- Onglet 3 : PR & Lift (NOUVEAU) ---
            tabPanel(tags$span(icon("bullseye"), " PR & Lift"), br(),
              tags$div(style = "margin-bottom:10px;font-size:12px;color:#666;",
                icon("info-circle"),
                " La courbe PR est plus informative que la ROC en cas de ",
                "d\u00e9s\u00e9quilibre de classes. Le Gain/Lift montre ",
                "l'efficacit\u00e9 du ciblage."
              ),
              plotlyOutput(ns("pr_plot"), height = "450px"), br(),
              uiOutput(ns("sel_model_lift")),
              plotlyOutput(ns("gain_lift_plot"), height = "380px"),
              uiOutput(ns("pr_reading"))
            ),

            # --- Onglet 4 : Matrice de confusion ---
            tabPanel(tags$span(icon("th"), " Confusion"), br(),
              uiOutput(ns("sel_model_cm")),
              fluidRow(
                column(6, plotlyOutput(ns("confusion_heatmap"), height = "400px")),
                column(6, uiOutput(ns("confusion_metrics")))
              ),
              br(),
              uiOutput(ns("sel_model_dist")),
              plotlyOutput(ns("pred_distribution"), height = "350px"),
              uiOutput(ns("confusion_reading"))
            ),

            # --- Onglet 5 : Calibration ---
            tabPanel(tags$span(icon("balance-scale"), " Calibration"), br(),
              tags$div(style = "margin-bottom:10px;font-size:12px;color:#666;",
                icon("info-circle"),
                " Un mod\u00e8le bien calibr\u00e9 a ses points proches de ",
                "la diagonale. Le test de Hosmer-Lemeshow (p > 0.05) confirme ",
                "une bonne calibration."
              ),
              uiOutput(ns("sel_model_calib")),
              plotlyOutput(ns("calib_plot"), height = "500px"), br(),
              uiOutput(ns("calib_interpretation"))
            ),

            # --- Onglet 6 : Importance (varImp) + PDP ---
            tabPanel(tags$span(icon("sort-amount-down"), " Importance"), br(),
              uiOutput(ns("sel_model_imp")),
              plotlyOutput(ns("import_plot"), height = "500px"), br(),
              DTOutput(ns("import_table")),
              hr(),
              tags$div(style = "margin-bottom:8px;font-size:12px;color:#666;",
                icon("info-circle"),
                " Effet marginal (PDP) : probabilit\u00e9 moyenne pr\u00e9dite quand ",
                "une seule variable varie, les autres fixes \u00e0 leur valeur de ",
                "r\u00e9f\u00e9rence. Binaire uniquement."
              ),
              fluidRow(
                column(6, uiOutput(ns("pdp_var_ui"))),
                column(6, uiOutput(ns("pdp_model_ui")))
              ),
              plotlyOutput(ns("pdp_plot"), height = "340px")
            ),

            # --- Onglet 7 : Importance Permutation (NOUVEAU) ---
            tabPanel(tags$span(icon("random"), " Permutation"), br(),
              tags$div(style = "margin-bottom:10px;font-size:12px;color:#666;",
                icon("info-circle"),
                " L'importance par permutation est agnostique au mod\u00e8le : ",
                "on permute chaque variable et on mesure la baisse de performance. ",
                "Plus robuste que varImp pour les mod\u00e8les non-lin\u00e9aires."
              ),
              fluidRow(
                column(4, uiOutput(ns("sel_model_perm"))),
                column(4, sliderInput(ns("n_perm"), "Permutations :",
                                      min = 10, max = 50, value = 10, step = 5)),
                column(4, br(),
                  actionButton(ns("run_perm"), "\u25b6 Calculer",
                               class = "btn btn-warning",
                               style = "margin-top:5px;font-weight:600;"))
              ),
              plotlyOutput(ns("perm_plot"), height = "500px"),
              uiOutput(ns("perm_reading")), br(),
              DTOutput(ns("perm_table"))
            ),

            # --- Onglet 8 : Courbes d'apprentissage (NOUVEAU) ---
            tabPanel(tags$span(icon("graduation-cap"), " Apprentissage"), br(),
              tags$div(style = "margin-bottom:10px;font-size:12px;color:#666;",
                icon("info-circle"),
                " Les courbes d'apprentissage diagnostiquent le biais et la variance. ",
                "Si les courbes convergent : faible variance. Si elles restent ",
                "\u00e9loign\u00e9es : surapprentissage."
              ),
              fluidRow(
                column(4, uiOutput(ns("sel_model_lc"))),
                column(4, sliderInput(ns("lc_folds"), "Folds CV :",
                                      min = 2, max = 5, value = 3, step = 1)),
                column(4, br(),
                  actionButton(ns("run_lc"), "\u25b6 Calculer courbes",
                               class = "btn btn-info",
                               style = "margin-top:5px;font-weight:600;color:white;"))
              ),
              plotlyOutput(ns("lc_plot"), height = "480px"),
              uiOutput(ns("lc_reading"))
            ),

            # --- Onglet 9 : Seuil optimal ---
            tabPanel(tags$span(icon("crosshairs"), " Seuil optimal"), br(),
              uiOutput(ns("threshold_info")),
              uiOutput(ns("sel_model_thresh")),
              plotlyOutput(ns("threshold_plot"), height = "450px"), br(),
              uiOutput(ns("threshold_result"))
            ),

            # --- Onglet 10 : Stabilite (NOUVEAU) ---
            tabPanel(tags$span(icon("wave-square"), " Stabilit\u00e9"), br(),
              tags$div(style = "margin-bottom:10px;font-size:12px;color:#666;",
                icon("info-circle"),
                " Un split al\u00e9atoire donne un AUC unique \u2014 peut-\u00eatre par ",
                "chance. Ici le pipeline est relanc\u00e9 sur plusieurs seeds : ",
                "un mod\u00e8le expert garde un score \u00e9lev\u00e9 avec un \u00e9cart-type faible."
              ),
              fluidRow(
                column(3, sliderInput(ns("n_seeds"), "Nombre de seeds :",
                                      min = 3, max = 10, value = 3, step = 1)),
                column(3, br(),
                  actionButton(ns("run_seeds"), "\u25b6 Analyser la stabilit\u00e9",
                               class = "btn btn-warning",
                               style = "font-weight:600;"))
              ),
              uiOutput(ns("stability_msg")),
              plotlyOutput(ns("stability_plot"), height = "440px"), br(),
              DTOutput(ns("stability_dt"))
            ),

            # --- Onglet 11 : Export ---
            tabPanel(tags$span(icon("download"), " Export"), br(),
              fluidRow(box(width = 12, status = "success", solidHeader = TRUE,
                title = tags$span(icon("download"), " Exporter les r\u00e9sultats"),
                fluidRow(
                  column(3, downloadButton(ns("dl_metrics"),
                    "\U0001f4ca M\u00e9triques (CSV)",
                    style = "background:#008DC9;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                  column(3, downloadButton(ns("dl_roc_png"),
                    "\U0001f4c8 ROC (PNG)",
                    style = "background:#4dac26;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                  column(3, downloadButton(ns("dl_predictions"),
                    "\U0001f4cb Pr\u00e9dictions (CSV)",
                    style = "background:#e88400;color:white;border:none;width:100%;border-radius:6px;padding:8px;")),
                  column(3, downloadButton(ns("dl_excel"),
                    "\U0001f4d1 Rapport (Excel)",
                    style = "background:#7b2d8b;color:white;border:none;width:100%;border-radius:6px;padding:8px;"))
                ),
                hr(),
                tags$div(style = "font-size:11px;color:#666;margin-bottom:8px;",
                  icon("file-export"),
                  " Exports qualit\u00e9 publication (comme la cartographie) :"),
                fluidRow(
                  column(4, downloadButton(ns("dl_roc_300"),
                    "\U0001f5bc ROC 300 dpi (PNG)",
                    style = "background:#00426A;color:white;border:none;width:100%;border-radius:6px;padding:8px;font-weight:600;")),
                  column(4, downloadButton(ns("dl_rapport_pdf"),
                    "\U0001f4d5 Rapport complet (PDF)",
                    style = "background:#c9161d;color:white;border:none;width:100%;border-radius:6px;padding:8px;font-weight:600;")),
                  column(4, downloadButton(ns("dl_model_rds"),
                    "\U0001f4dd Mod\u00e8le (RDS)",
                    style = "background:#4dac26;color:white;border:none;width:100%;border-radius:6px;padding:8px;"))
                )
              ))
            ),

            # --- Onglet 12 : Scoring & Deploiement (NOUVEAU) ---
            tabPanel(tags$span(icon("rocket"), " Scoring"), br(),
              fluidRow(
                box(width = 4, status = "primary", solidHeader = TRUE,
                  title = tags$span(icon("bullseye"), " Appliquer le mod\u00e8le"),
                  tags$div(style = "font-size:11px;color:#666;margin-bottom:8px;",
                    "Entra\u00eenez d'abord le pipeline, puis appliquez le meilleur ",
                    "mod\u00e8le \u00e0 un fichier CSV de nouvelles observations ",
                    "(m\u00eames colonnes pr\u00e9dictrices)."),
                  uiOutput(ns("score_model_ui")),
                  fileInput(ns("score_file"), "Nouvelles donn\u00e9es (CSV) :",
                            accept = ".csv", width = "100%"),
                  actionButton(ns("run_score"),
                               "\u25b6 Pr\u00e9dire sur le nouveau fichier",
                               class = "btn btn-oms btn-block", width = "100%",
                               style = "font-size:12px;padding:8px;font-weight:600;"),
                  br(), uiOutput(ns("score_info"))
                ),
                box(width = 8, status = "info", solidHeader = TRUE,
                  title = tags$span(icon("table"), " Pr\u00e9dictions"),
                  DTOutput(ns("score_preview")), br(),
                  downloadButton(ns("dl_scored"),
                    "\U0001f4e5 T\u00e9l\u00e9charger les pr\u00e9dictions (CSV)",
                    style = "background:#008DC9;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;")
                )
              )
            )
          )
      )
    )
  )
}

# ── SERVER ────────────────────────────────────────────────────────────────────
predictifServer <- function(id, df_f, map_cols, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Variables reactives ──────────────────────────────────────
    vars_target <- reactive({
      d <- df_f(); req(d)
      names(d)[vapply(d, function(x) {
        vals <- unique(na.omit(x))
        n <- length(vals)
        n >= 2 && n <= 10 &&
          (is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x))
      }, logical(1))]
    })

    vars_all <- reactive({
      d <- df_f(); req(d)
      names(d)[vapply(d, function(x)
        is.numeric(x) || is.factor(x) || is.character(x), logical(1))]
    })

    n_classes <- reactive({
      d <- df_f(); req(d, input$outcome)
      length(unique(na.omit(d[[input$outcome]])))
    })

    is_binary <- reactive({ n_classes() == 2 })

    pipeline_res <- reactiveVal(NULL)
    perm_imp_res <- reactiveVal(NULL)
    lc_res       <- reactiveVal(NULL)
    seeds_res    <- reactiveVal(NULL)
    scored_res   <- reactiveVal(NULL)

    model_names <- reactive({
      r <- pipeline_res(); req(!is.null(r))
      names(r$results)
    })

    # ── UI dynamiques : configuration ────────────────────────────
    output$sel_outcome <- renderUI({
      selectInput(ns("outcome"), "Variable cible :",
                  choices = vars_target(), width = "100%")
    })

    output$outcome_info <- renderUI({
      req(input$outcome)
      nc  <- n_classes()
      d   <- df_f()
      lvls <- unique(na.omit(d[[input$outcome]]))
      if (nc == 2) {
        tags$div(style = "background:#e8f5e9;border-radius:6px;padding:8px 10px;font-size:11px;margin-top:4px;",
          tags$b(style = "color:#2e7d32;", icon("check-circle"),
                 " Classification binaire"),
          tags$br(),
          paste0("2 classes : ", paste(lvls, collapse = " vs ")),
          tags$br(),
          tags$span(style = "color:#555;",
            "5 mod\u00e8les \u00b7 15 m\u00e9triques \u00b7 ROC/PR/Lift/Calibration")
        )
      } else {
        tags$div(style = "background:#e3f2fd;border-radius:6px;padding:8px 10px;font-size:11px;margin-top:4px;",
          tags$b(style = "color:#1565c0;", icon("layer-group"),
                 paste0(" Classification multiclasse (", nc, " classes)")),
          tags$br(),
          paste0("Classes : ", paste(head(lvls, 6), collapse = ", "),
                 if (length(lvls) > 6) "\u2026" else ""),
          tags$br(),
          tags$span(style = "color:#555;",
            "Multinomiale/RF/XGBoost \u00b7 9 m\u00e9triques \u00b7 Macro-moyenn\u00e9es")
        )
      }
    })

    output$sel_predictors <- renderUI({
      req(input$outcome)
      preds <- setdiff(vars_all(), input$outcome)
      checkboxGroupInput(ns("predictors"), NULL,
                         choices = preds,
                         selected = head(preds, min(8, length(preds))))
    })

    output$models_ui <- renderUI({
      req(input$outcome)
      avail <- c(
        glm = TRUE,
        rf  = requireNamespace("randomForest", quietly = TRUE),
        xgb = requireNamespace("xgboost", quietly = TRUE),
        svm = requireNamespace("e1071", quietly = TRUE),
        knn = requireNamespace("class", quietly = TRUE)
      )
      if (is_binary()) {
        ch <- c(
          "R\u00e9gression logistique" = "glm",
          "Random Forest"              = "rf",
          "XGBoost"                    = "xgb",
          "SVM (noyau RBF)"            = "svm",
          "k-NN"                       = "knn"
        )
        ch <- ch[avail[ch] %in% TRUE]
        sel <- intersect(c("glm", "rf", "xgb"), ch)
        checkboxGroupInput(ns("models"), NULL, choices = ch, selected = sel)
      } else {
        ch <- c(
          "R\u00e9gression multinomiale" = "glm",
          "Random Forest"                = "rf",
          "XGBoost"                      = "xgb"
        )
        ch <- ch[avail[ch] %in% TRUE]
        sel <- intersect(c("glm", "rf"), ch)
        checkboxGroupInput(ns("models"), NULL, choices = ch, selected = sel)
      }
    })

    output$options_ui <- renderUI({
      if (is_binary()) {
        tagList(
          checkboxInput(ns("auto_balance"),
                        "R\u00e9\u00e9quilibrer classes (ROSE)", TRUE),
          checkboxInput(ns("optimal_threshold"),
                        "Seuil optimal (Youden)", TRUE)
        )
      } else {
        tags$div(style = "font-size:11px;color:#888;", icon("info-circle"),
          " ROSE et seuil optimal disponibles uniquement en binaire.")
      }
    })

    # ── Selecteurs de modeles (dans les onglets) ─────────────────
    .model_select <- function(input_id, label = "Mod\u00e8le :") {
      renderUI(selectInput(ns(input_id), label,
                           choices = model_names(), width = "300px"))
    }
    output$sel_model_cm     <- .model_select("model_cm")
    output$sel_model_calib  <- .model_select("model_calib")
    output$sel_model_imp    <- .model_select("model_imp")
    output$sel_model_thresh <- .model_select("model_thresh")
    output$sel_model_lift   <- .model_select("model_lift")
    output$sel_model_perm   <- .model_select("model_perm")
    output$sel_model_roc_ci <- .model_select("model_roc_ci")
    output$sel_model_dist   <- .model_select("model_dist")
    output$sel_model_lc     <- renderUI({
      req(pipeline_res())
      # Map model display names back to pipeline codes
      model_map <- c("Logistique" = "glm", "Multinomiale" = "glm",
                      "Random Forest" = "rf", "XGBoost" = "xgb",
                      "SVM (RBF)" = "svm", "k-NN" = "knn")
      nms  <- model_names()
      codes <- model_map[nms]
      codes <- codes[!is.na(codes)]
      selectInput(ns("model_lc"), "Mod\u00e8le :",
                  choices = setNames(codes, names(codes)), width = "300px")
    })

    # ══════════════════════════════════════════════════════════════
    # PIPELINE PRINCIPAL
    # ══════════════════════════════════════════════════════════════
    observeEvent(input$run_pred, {
      d <- df_f(); req(d, input$outcome, input$predictors, input$models)
      req(length(input$predictors) >= 1, length(input$models) >= 1)

      withProgress(message = "Pipeline pr\u00e9dictif en cours\u2026", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Pr\u00e9paration des donn\u00e9es")
          res <- run_predictive_pipeline(
            d          = d,
            outcome    = input$outcome,
            predictors = input$predictors,
            k_folds    = input$k_folds,
            models     = input$models,
            train_pct  = input$train_pct / 100
          )
          incProgress(0.8, detail = paste0(length(res$results), " mod\u00e8les entra\u00een\u00e9s"))
          pipeline_res(res)
          perm_imp_res(NULL)  # Reset
          lc_res(NULL)
          manquants <- setdiff(input$models, res$model_codes %||% input$models)
          if (length(manquants) > 0)
            showNotification(
              paste0("⚠ Modèle(s) non entraîné(s) : ",
                     paste(manquants, collapse = ", "),
                     " (package manquant ou erreur)"),
              type = "warning", duration = 8)
          showNotification(
            paste0("\u2705 Pipeline termin\u00e9 \u2014 ",
                   length(res$results), " mod\u00e8les (",
                   if (res$is_binary) "binaire" else paste0(res$n_classes, " classes"),
                   ")"),
            type = "message", duration = 5)
        }, error = function(e) {
          showNotification(paste("Erreur :", e$message), type = "error", duration = 8)
        })
      })
    })

    output$pipeline_info <- renderUI({
      r <- pipeline_res(); if (is.null(r)) return(NULL)
      type_lbl <- if (r$is_binary) "Binaire" else paste0("Multiclasse (", r$n_classes, " cl.)")
      tags$div(style = "font-size:11px;color:#666;margin-top:8px;background:#f8f9fa;padding:8px;border-radius:4px;",
        tags$b(type_lbl), tags$br(),
        paste0(r$n_train, " train | ", r$n_test, " test | ", r$k_folds, " folds"),
        tags$br(),
        paste0(length(r$results), " mod\u00e8le(s) : ", paste(names(r$results), collapse = ", ")),
        if (r$is_binary && r$imbalance_ratio > 2)
          tags$span(style = "color:#e88400;", tags$br(), "\u26a0 ROSE appliqu\u00e9")
      )
    })

    # ── Panneau KPI + verdict expert (style tableau de bord) ────
    pk_card <- function(icon_name, col, title, value, sub = NULL) {
      tags$div(class = "zs-kpi-card", style = "flex:1;min-width:150px;padding:14px 16px;",
        tags$div(class = "zs-header",
          tags$div(class = "zs-header-icon", style = paste0("background:", col, ";"),
                   icon(icon_name)),
          tags$div(class = "zs-title", title)),
        tags$div(class = "zs-value", style = paste0("color:", col, ";font-size:26px;"), value),
        if (!is.null(sub)) tags$div(class = "zs-subtitle", sub))
    }
    output$kpi_panel <- renderUI({
      r <- pipeline_res(); if (is.null(r)) return(NULL)
      mt <- tryCatch(metrics_table(r), error = function(e) NULL)
      req(!is.null(mt), nrow(mt) > 0)
      metric_col <- if (r$is_binary && "AUC" %in% names(mt)) "AUC"
                    else if ("Accuracy" %in% names(mt)) "Accuracy" else NULL
      req(!is.null(metric_col))
      vals <- suppressWarnings(as.numeric(mt[[metric_col]]))
      ib   <- which.max(vals); best <- mt[ib, ]; bv <- vals[ib]
      q <- if (is.na(bv)) list(lbl = "N/A", col = "#6c757d", bg = "#f1f3f5")
           else if (bv >= 0.9) list(lbl = "Excellent", col = "#1b5e3b", bg = "#e8f5e9")
           else if (bv >= 0.8) list(lbl = "Tr\u00e8s bon", col = "#2e7d32", bg = "#e8f5e9")
           else if (bv >= 0.7) list(lbl = "Bon", col = "#0077B6", bg = "#e8f6fc")
           else if (bv >= 0.6) list(lbl = "Faible", col = "#b36b00", bg = "#fdf6ec")
           else list(lbl = "Proche du hasard", col = "#c9161d", bg = "#fef2f2")

      bullets <- list()
      if (r$is_binary) {
        oc  <- r$outcome_col %||% ".y"
        obs <- suppressWarnings(as.numeric(r$test_data[[oc]]) - 1)
        base <- mean(obs == 1, na.rm = TRUE)
        pr <- if ("PR_AUC" %in% names(mt)) suppressWarnings(as.numeric(best$PR_AUC)) else NA_real_
        if (!is.na(pr))
          bullets <- c(bullets, list(tags$li(paste0(
            "PR-AUC = ", round(pr, 3), " vs hasard = ", round(base, 3),
            " (pr\u00e9valence) \u2014 ",
            if (pr > base) "meilleur ciblage des positifs qu'un tirage al\u00e9atoire."
            else "pas mieux qu'un tirage al\u00e9atoire sur la precision-rappel."))))
        if (!is.null(r$imbalance_ratio) && r$imbalance_ratio > 2)
          bullets <- c(bullets, list(tags$li(paste0(
            "D\u00e9s\u00e9quilibre \u00d7", r$imbalance_ratio,
            " \u2014 privil\u00e9gier PR-AUC et balanced accuracy plut\u00f4t que l'accuracy."))))
      } else {
        chance <- 1 / max(2, r$n_classes)
        bullets <- c(bullets, list(tags$li(paste0(
          "Niveau du hasard = ", round(chance, 3), " (1 / ", r$n_classes,
          " classes) \u2014 l'accuracy doit \u00eatre lue par rapport \u00e0 ce rep\u00e8re."))))
      }
      if (nrow(mt) >= 2) {
        ord2 <- order(-suppressWarnings(as.numeric(mt[[metric_col]])))
        second <- mt[ord2[2], ]
        sv <- suppressWarnings(as.numeric(second[[metric_col]]))
        bullets <- c(bullets, list(tags$li(paste0(
          "Avance sur le 2e mod\u00e8le (", second$Modele, ") : +",
          round(bv - sv, 3), " de ", metric_col, "."))))
      }
      sr <- seeds_res()
      if (!is.null(sr) && nrow(sr$summary) > 0) {
        sb <- sr$summary[1, ]
        bullets <- c(bullets, list(tags$li(paste0(
          "Stabilit\u00e9 : ", sb$Modele, " = ", round(sb$Score_moyen, 3),
          " \u00b1 ", round(sb$Score_sd, 3), " sur ", sb$N_seeds, " seeds."))))
      }
      rec <- if (!is.na(bv) && bv >= 0.7)
               "Pr\u00eat pour validation m\u00e9tier : v\u00e9rifiez calibration et stabilit\u00e9 avant d\u00e9ploiement."
             else if (!is.na(bv) && bv >= 0.6)
               "Discrimination faible \u00e0 mod\u00e9r\u00e9e : enrichir les variables ou ajuster le seuil d\u00e9cisionnel."
             else
               "Performance proche du hasard : reconsid\u00e9rer les variables, l'\u00e9quilibrage ou la cible."

      tagList(
        tags$div(style = "display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
          pk_card("trophy", "#0B3954", "Meilleur mod\u00e8le", best$Modele),
          pk_card("bullseye", "#0077B6", paste0(metric_col, " (test)"), round(bv, 3)),
          pk_card("database", "#2e7d32", "Observations", paste0(r$n_train, " / ", r$n_test), "train / test"),
          pk_card("layer-group", "#b36b00", "Validation crois\u00e9e", paste0(r$k_folds, " folds \u00b7 ", length(r$results), " mod.")),
          if (r$is_binary)
            pk_card("balance-scale", "#7b2d8b", "D\u00e9s\u00e9quilibre", paste0("\u00d7", r$imbalance_ratio), "max / min")
        ),
        tags$div(style = paste0("background:", q$bg, ";border-left:5px solid ", q$col,
          ";border-radius:10px;padding:14px 18px;margin-bottom:4px;"),
          tags$div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
            tags$span(style = paste0("background:", q$col,
              ";color:white;font-size:11px;font-weight:700;padding:3px 10px;",
              "border-radius:20px;letter-spacing:.5px;text-transform:uppercase;"), q$lbl),
            tags$span(style = "font-size:13px;color:#333;",
              tags$b(paste0("Verdict : ", best$Modele, " \u2014 ", metric_col, " = ", round(bv, 3), ".")),
              paste0(" ", rec))
          ),
          if (length(bullets) > 0)
            tags$ul(style = "margin:8px 0 0 0;padding-left:18px;font-size:12px;color:#444;line-height:1.7;",
                    bullets)
        )
      )
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 1 : METRIQUES (15+)
    # ══════════════════════════════════════════════════════════════
    output$metrics_dt <- renderDT({
      res <- pipeline_res(); req(!is.null(res))
      mt <- tryCatch(metrics_table(res), error = function(e) NULL); req(!is.null(mt))
      mt |>
        mutate(across(where(is.numeric), ~round(., 4))) |>
        datatable(
          rownames = FALSE,
          options = list(dom = "t", pageLength = 10, scrollX = TRUE,
                         columnDefs = list(list(className = "dt-center", targets = "_all"))),
          class = "compact stripe hover"
        ) |>
        formatStyle(
          columns = names(mt)[-1],
          backgroundColor = styleInterval(
            c(0.5, 0.7, 0.85),
            c("#fef2f2", "#fff7ed", "#f0fdf4", "#dcfce7")
          )
        )
    })

    output$best_model_msg <- renderUI({
      res <- pipeline_res(); req(!is.null(res))
      mt <- tryCatch(metrics_table(res), error = function(e) NULL); req(!is.null(mt))
      metric_col <- if (res$is_binary && "AUC" %in% names(mt)) "AUC"
                    else if ("Accuracy" %in% names(mt)) "Accuracy" else NULL
      req(!is.null(metric_col))
      best <- mt[which.max(mt[[metric_col]]), ]
      tags$div(style = "background:#e8f5e9;padding:14px;border-radius:8px;border-left:4px solid #4dac26;",
        tags$b("\U0001f3c6 Meilleur mod\u00e8le : "), best$Modele,
        tags$span(style = "color:#4dac26;font-weight:700;margin-left:8px;",
                  paste0(metric_col, " = ", round(best[[metric_col]], 4))),
        if (res$is_binary && "MCC" %in% names(mt))
          tags$span(style = "color:#555;margin-left:12px;",
                    paste0("| MCC = ", round(best$MCC, 4),
                           " | Brier = ", round(best$Brier, 4)))
      )
    })

    output$metrics_bar <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res))
      mt <- tryCatch(metrics_table(res), error = function(e) NULL); req(!is.null(mt))
      # Selectionner les metriques les plus pertinentes pour le graphique
      if (res$is_binary) {
        cols_sel <- intersect(c("AUC", "PR_AUC", "F1", "MCC", "Kappa",
                                "Balanced_Acc", "Sensibilite", "Specificite"),
                              names(mt))
      } else {
        cols_sel <- intersect(c("Accuracy", "Balanced_Acc", "F1_macro",
                                "Sensibilite_macro", "Kappa"), names(mt))
      }
      ml <- mt |>
        select(Modele, all_of(cols_sel)) |>
        tidyr::pivot_longer(-Modele, names_to = "Metrique", values_to = "Valeur")
      pal <- c("#008DC9", "#c9161d", "#4dac26", "#7b2d8b", "#e88400")
      p <- ggplot(ml, aes(x = Metrique, y = Valeur, fill = Modele)) +
        geom_col(position = "dodge", alpha = 0.85) +
        scale_fill_manual(values = pal[seq_len(length(unique(ml$Modele)))]) +
        labs(x = "", y = "Score", fill = "Mod\u00e8le") +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
      ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.35))
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 2 : ROC & DeLong
    # ══════════════════════════════════════════════════════════════
    output$roc_info <- renderUI({
      r <- pipeline_res()
      if (!is.null(r) && !r$is_binary)
        tags$div(style = "background:#fff3cd;padding:10px;border-radius:6px;margin-bottom:10px;font-size:12px;",
          icon("info-circle"),
          " En multiclasse, les courbes ROC individuelles ne sont pas affich\u00e9es. ",
          "AUC macro disponible dans M\u00e9triques.")
    })

    output$roc_plot <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), res$is_binary)
      tryCatch(plot_roc_compare(res), error = function(e) plotly_empty())
    })

    output$roc_ci_plot <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), res$is_binary, input$model_roc_ci)
      tryCatch(plot_roc_ci(res, input$model_roc_ci, n_boot = 200),
               error = function(e) plotly_empty())
    })

    output$auc_comparison <- renderUI({
      res <- pipeline_res()
      req(!is.null(res), res$is_binary, length(res$results) >= 2)
      tryCatch({
        nms <- names(res$results)
        # Test DeLong pour chaque paire
        pairs <- combn(nms, 2, simplify = FALSE)
        tags$div(
          tags$h6(style = "font-weight:700;color:#00426A;",
                  "\U0001f9ea Tests de DeLong (comparaison des AUC)"),
          lapply(pairs, function(pr) {
            dl <- tryCatch(
              pROC::roc.test(res$results[[pr[1]]]$roc,
                             res$results[[pr[2]]]$roc, method = "delong"),
              error = function(e) NULL)
            if (is.null(dl)) return(NULL)
            sig <- dl$p.value < 0.05
            tags$div(
              style = paste0("padding:10px;border-radius:6px;margin-bottom:6px;",
                             "border-left:4px solid ",
                             if (sig) "#c9161d" else "#4dac26", ";",
                             "background:", if (sig) "#fef2f2" else "#f0fdf4", ";"),
              tags$b(paste0(pr[1], " vs ", pr[2])),
              tags$span(style = "margin-left:10px;",
                paste0("p = ", format.pval(dl$p.value, digits = 4))),
              tags$span(style = paste0("margin-left:10px;font-weight:600;color:",
                                       if (sig) "#c9161d" else "#4dac26", ";"),
                if (sig) "Diff\u00e9rence significative"
                else "Pas de diff\u00e9rence significative")
            )
          })
        )
      }, error = function(e) NULL)
    })

    output$cv_fold_heatmap <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res))
      tryCatch(plot_cv_fold_heatmap(res), error = function(e) plotly_empty())
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 3 : PR & Lift
    # ══════════════════════════════════════════════════════════════
    output$pr_plot <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), res$is_binary)
      tryCatch(plot_pr_curve(res), error = function(e) plotly_empty())
    })

    output$gain_lift_plot <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), res$is_binary, input$model_lift)
      tryCatch(plot_gain_lift(res, input$model_lift),
               error = function(e) plotly_empty())
    })

    output$pr_reading <- renderUI({
      res <- pipeline_res(); req(!is.null(res))
      if (!res$is_binary)
        return(pk_info("Les courbes PR, Gain et Lift sont disponibles en classification binaire uniquement.",
                       icon_name = "info-circle", col = "#b36b00", bg = "#fdf6ec"))
      mt <- tryCatch(metrics_table(res), error = function(e) NULL)
      if (is.null(mt) || !"PR_AUC" %in% names(mt)) return(NULL)
      oc  <- res$outcome_col %||% ".y"
      obs <- suppressWarnings(as.numeric(res$test_data[[oc]]) - 1)
      base <- mean(obs == 1, na.rm = TRUE)
      bst <- mt[which.max(mt$PR_AUC), ]
      pk_info(paste0("Lecture : PR-AUC de ", bst$Modele, " = ", round(bst$PR_AUC, 3),
        " \u2014 un classifieur al\u00e9atoire obtiendrait \u2248 la pr\u00e9valence (",
        round(base, 3), "). ",
        if (!is.na(bst$PR_AUC) && bst$PR_AUC > base)
          "Le mod\u00e8le concentre donc les positifs en t\u00eate de liste (utile pour le ciblage)."
        else "Aucun gain de ciblage par rapport au hasard."))
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 4 : Matrice de confusion
    # ══════════════════════════════════════════════════════════════
    output$confusion_heatmap <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), input$model_cm)
      r <- res$results[[input$model_cm]]; req(!is.null(r))
      df_cm <- as.data.frame(r$cm$table)
      names(df_cm) <- c("Predit", "Reel", "N")
      p <- ggplot(df_cm, aes(x = Predit, y = Reel, fill = N)) +
        geom_tile(color = "white", linewidth = 2) +
        geom_text(aes(label = N), size = 6, fontface = "bold", color = "white") +
        scale_fill_gradient(low = "#90CAF9", high = "#00426A") +
        labs(title = paste0("Matrice de confusion \u2014 ", input$model_cm),
             x = "Pr\u00e9dit", y = "R\u00e9el") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none",
              axis.text.x = element_text(
                angle = if (res$n_classes > 4) 45 else 0, hjust = 1))
      ggplotly(p)
    })

    output$confusion_metrics <- renderUI({
      res <- pipeline_res(); req(!is.null(res), input$model_cm)
      r <- res$results[[input$model_cm]]; req(!is.null(r)); cm <- r$cm
      if (res$is_binary) {
        metrics <- list(
          c("Accuracy",     round(cm$overall["Accuracy"], 4)),
          c("Sensibilit\u00e9", round(cm$byClass["Sensitivity"], 4)),
          c("Sp\u00e9cificit\u00e9", round(cm$byClass["Specificity"], 4)),
          c("VPP",          round(cm$byClass["Pos Pred Value"], 4)),
          c("VPN",          round(cm$byClass["Neg Pred Value"], 4)),
          c("F1",           round(cm$byClass["F1"], 4)),
          c("Kappa",        round(cm$overall["Kappa"], 4)),
          c("AUC",          round(r$auc, 4))
        )
      } else {
        by_cl <- if (is.matrix(cm$byClass)) colMeans(cm$byClass, na.rm = TRUE) else cm$byClass
        metrics <- list(
          c("Accuracy",     round(cm$overall["Accuracy"], 4)),
          c("Sens.(macro)", round(by_cl["Sensitivity"], 4)),
          c("Spec.(macro)", round(by_cl["Specificity"], 4)),
          c("F1(macro)",    round(by_cl["F1"], 4)),
          c("Kappa",        round(cm$overall["Kappa"], 4)),
          c("AUC macro",    round(r$auc %||% NA_real_, 4))
        )
      }
      tags$div(style = "padding:10px;",
        tags$h6(style = "font-weight:700;color:#00426A;", "M\u00e9triques d\u00e9taill\u00e9es"),
        lapply(metrics, function(m) {
          val <- as.numeric(m[2])
          col <- if (!is.na(val) && val > 0.8) "#4dac26"
                 else if (!is.na(val) && val > 0.6) "#e88400"
                 else "#c9161d"
          tags$div(style = "margin-bottom:4px;padding:5px 8px;background:#f8f9fa;border-radius:4px;",
            fluidRow(
              column(7, tags$span(style = "font-size:11px;", m[1])),
              column(5, tags$span(style = paste0(
                "font-size:14px;font-weight:700;color:", col, ";float:right;"), m[2]))
            ))
        })
      )
    })

    output$pred_distribution <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), res$is_binary, input$model_dist)
      tryCatch(plot_pred_distribution(res, input$model_dist),
               error = function(e) plotly_empty())
    })

    output$confusion_reading <- renderUI({
      res <- pipeline_res(); req(!is.null(res), input$model_cm)
      r <- res$results[[input$model_cm]]; req(!is.null(r), !is.null(r$cm))
      tb <- r$cm$table
      if (res$is_binary) {
        TP <- unname(tb["Positif", "Positif"]); FN <- unname(tb["Negatif", "Positif"])
        FP <- unname(tb["Positif", "Negatif"]); TN <- unname(tb["Negatif", "Negatif"])
        pk_info(paste0("Lecture : ", FN, " positifs manqu\u00e9s sur ", TP + FN,
          " (", round(100 * FN / max(1, TP + FN), 1), " %) ; ", FP,
          " fausses alertes sur ", FP + TN, " n\u00e9gatifs (",
          round(100 * FP / max(1, FP + TN), 1), " %). ",
          "L'onglet Seuil optimal permet d'arbitrer entre ces deux types d'erreur."),
          icon_name = "stethoscope")
      } else {
        acc <- as.numeric(r$cm$overall["Accuracy"])
        pk_info(paste0("Lecture : erreur globale = ", round(100 * (1 - acc), 1),
          " % des observations. V\u00e9rifiez qu'aucune classe minoritaire n'est ",
          "syst\u00e9matiquement confondue (lignes hors diagonale)."),
          icon_name = "stethoscope")
      }
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 5 : Calibration
    # ══════════════════════════════════════════════════════════════
    output$calib_plot <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), res$is_binary, input$model_calib)
      tryCatch(plot_calibration(res, input$model_calib),
               error = function(e) plotly_empty())
    })

    output$calib_interpretation <- renderUI({
      res <- pipeline_res(); req(!is.null(res))
      if (!res$is_binary)
        return(pk_info("La courbe de calibration n'est disponible qu'en classification binaire.",
                       icon_name = "info-circle", col = "#b36b00", bg = "#fdf6ec"))
      req(input$model_calib)
      r <- res$results[[input$model_calib]]; req(!is.null(r))
      oc  <- res$outcome_col %||% ".y"
      obs <- as.numeric(res$test_data[[oc]]) - 1
      hl <- tryCatch(hoslem.test(obs, r$pred_prob), error = function(e) NULL)
      if (is.null(hl)) hl <- tryCatch(hoslem.test(obs, r$pred_prob, g = 5), error = function(e) NULL)
      if (is.null(hl)) hl <- tryCatch(hoslem.test(obs, r$pred_prob, g = 3), error = function(e) NULL)
      brier <- tryCatch({
        pp <- r$pred_prob
        if (is.null(pp)) NA_real_ else mean((pp - obs)^2, na.rm = TRUE)
      }, error = function(e) NA_real_)
      ok <- if (!is.null(hl)) hl$p.value > 0.05 else NA
      brier_txt <- if (!is.na(brier))
        tags$span(style = "color:#555;font-size:11px;",
          tags$br(), paste0("Brier = ", round(brier, 3),
            " (0.25 = hasard pour un \u00e9v\u00e9nement binaire \u00e9quilibr\u00e9 ; plus bas = mieux)."))
      if (is.null(hl))
        return(tags$div(
          style = "padding:12px;border-radius:6px;border-left:4px solid #6c757d;background:#f1f3f5;",
          tags$b("Test de Hosmer-Lemeshow non applicable"),
          tags$br(),
          "Trop peu de valeurs pr\u00e9dites distinctes (souvent avec des probabilit\u00e9s tr\u00e8s extr\u00eames). ",
          "Lisez le graphique de calibration et le Brier score.", brier_txt))
      tags$div(
        style = paste0("padding:12px;border-radius:6px;border-left:4px solid ",
                       if (ok) "#4dac26" else "#c9161d", ";background:",
                       if (ok) "#f0fdf4" else "#fef2f2", ";"),
        tags$b(paste0("Hosmer-Lemeshow p = ", round(hl$p.value, 4))),
        tags$br(),
        if (ok) "Bonne calibration (p > 0.05) \u2014 les probabilit\u00e9s sont fiables."
        else "Calibration insuffisante (p \u2264 0.05) \u2014 les probabilit\u00e9s peuvent \u00eatre biais\u00e9es.",
        brier_txt
      )
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 6 : Importance (varImp)
    # ══════════════════════════════════════════════════════════════
    output$import_plot <- renderPlotly({
      res <- pipeline_res(); req(!is.null(res), input$model_imp)
      tryCatch(plot_importance(res, input$model_imp),
               error = function(e) plotly_empty())
    })

    output$import_table <- renderDT({
      res <- pipeline_res(); req(!is.null(res), input$model_imp)
      imp <- tryCatch(importance_table(res, input$model_imp),
                      error = function(e) NULL)
      req(!is.null(imp))
      datatable(imp, rownames = FALSE,
                options = list(dom = "t", pageLength = 25),
                class = "compact stripe hover")
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 7 : Importance Permutation
    # ══════════════════════════════════════════════════════════════
    observeEvent(input$run_perm, {
      res <- pipeline_res(); req(!is.null(res), input$model_perm)
      withProgress(message = "Calcul importance par permutation\u2026", value = 0.3, {
        tryCatch({
          pi <- calc_permutation_importance(res, input$model_perm,
                                            n_perm = input$n_perm)
          perm_imp_res(pi)
          incProgress(0.7)
          showNotification("\u2705 Importance par permutation calcul\u00e9e",
                           type = "message", duration = 3)
        }, error = function(e) {
          showNotification(paste("Erreur :", e$message), type = "error")
        })
      })
    })

    output$perm_plot <- renderPlotly({
      pi <- perm_imp_res(); req(!is.null(pi))
      model_nm <- input$model_perm %||% ""
      tryCatch(plot_permutation_importance(pi, model_nm),
               error = function(e) plotly_empty())
    })

    output$perm_table <- renderDT({
      pi <- perm_imp_res(); req(!is.null(pi))
      pi |>
        mutate(across(where(is.numeric), ~round(., 4))) |>
        datatable(rownames = FALSE,
                  options = list(dom = "t", pageLength = 25),
                  class = "compact stripe hover")
    })

    output$perm_reading <- renderUI({
      pi <- perm_imp_res(); if (is.null(pi) || nrow(pi) == 0) return(NULL)
      top <- head(pi[order(-pi$Importance), ], 3)
      pk_info(paste0("Lecture : variables les plus influentes \u2014 ",
        paste(paste0(top$Variable, " (", round(top$Importance, 3), ")"), collapse = ", "),
        ". Une importance \u2248 0 signifie que permuter la variable ne d\u00e9grade ",
        "pas la performance : elle n'apporte pas d'information au mod\u00e8le."))
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 8 : Courbes d'apprentissage
    # ══════════════════════════════════════════════════════════════
    observeEvent(input$run_lc, {
      res <- pipeline_res(); req(!is.null(res), input$model_lc)
      withProgress(message = "Calcul des courbes d'apprentissage\u2026", value = 0.1, {
        tryCatch({
          lc <- calc_learning_curves(
            pipeline_res = res,
            model_type   = input$model_lc,
            sizes         = c(0.2, 0.4, 0.6, 0.8, 1.0),
            k_folds       = input$lc_folds,
            seed          = 2847
          )
          lc_res(lc)
          incProgress(0.9)
          showNotification("\u2705 Courbes d'apprentissage calcul\u00e9es",
                           type = "message", duration = 3)
        }, error = function(e) {
          showNotification(paste("Erreur :", e$message), type = "error")
        })
      })
    })

    output$lc_plot <- renderPlotly({
      lc <- lc_res(); req(!is.null(lc))
      res <- pipeline_res()
      metric_nm <- if (!is.null(res) && res$is_binary) "AUC" else "Accuracy"
      model_nm  <- input$model_lc %||% ""
      # Map code back to display name
      nm_map <- c(glm = "Logistique", rf = "Random Forest",
                  xgb = "XGBoost", svm = "SVM (RBF)", knn = "k-NN")
      display_nm <- nm_map[model_nm] %||% model_nm
      tryCatch(plot_learning_curves(lc, display_nm, metric_nm),
               error = function(e) plotly_empty())
    })

    output$lc_reading <- renderUI({
      lc <- lc_res(); if (is.null(lc) || nrow(lc) == 0) return(NULL)
      last <- lc[nrow(lc), ]
      gap  <- last$Train - last$CV
      if (is.na(gap)) return(NULL)
      lab <- if (gap > 0.10) "Le mod\u00e8le sur-apprend (il m\u00e9morise le train)."
             else if (gap > 0.05) "L\u00e9ger sur-apprentissage, g\u00e9n\u00e9ralisation correcte."
             else "Bonne g\u00e9n\u00e9ralisation (train \u2248 CV)."
      extra <- if (gap > 0.10) " Ajouter des observations peut r\u00e9duire cet \u00e9cart." else ""
      pk_info(paste0("Lecture : \u00e9cart train \u2212 CV = ", round(gap, 3),
                     " au point 100 %. ", lab, extra),
              icon_name = "graduation-cap")
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 9 : Seuil optimal (Youden)
    # ══════════════════════════════════════════════════════════════
    output$threshold_info <- renderUI({
      r <- pipeline_res()
      if (!is.null(r) && !r$is_binary)
        tags$div(style = "background:#fff3cd;padding:10px;border-radius:6px;margin-bottom:10px;font-size:12px;",
          icon("info-circle"),
          " Le seuil optimal (Youden) n'est disponible qu'en classification binaire.")
    })

    output$threshold_plot <- renderPlotly({
      res <- pipeline_res()
      req(!is.null(res), res$is_binary, input$model_thresh)
      r <- res$results[[input$model_thresh]]; req(!is.null(r))
      tryCatch({
        roc_obj <- r$roc
        df_t <- tibble(
          Seuil  = roc_obj$thresholds,
          Sens   = roc_obj$sensitivities,
          Spec   = roc_obj$specificities,
          Youden = roc_obj$sensitivities + roc_obj$specificities - 1,
          F1_approx = 2 * roc_obj$sensitivities *
            (roc_obj$sensitivities / (roc_obj$sensitivities + (1 - roc_obj$specificities) + 1e-10)) /
            (roc_obj$sensitivities + roc_obj$sensitivities /
              (roc_obj$sensitivities + (1 - roc_obj$specificities) + 1e-10) + 1e-10)
        ) |> filter(is.finite(Seuil))
        bi <- which.max(df_t$Youden)
        dl <- df_t |>
          tidyr::pivot_longer(c(Sens, Spec, Youden), names_to = "M")
        p <- ggplot(dl, aes(x = Seuil, y = value, color = M)) +
          geom_line(linewidth = 1) +
          geom_vline(xintercept = df_t$Seuil[bi], linetype = "dashed",
                     color = "#7b2d8b", linewidth = 0.8) +
          annotate("text", x = df_t$Seuil[bi], y = 0.05,
                   label = paste0("Optimal = ", round(df_t$Seuil[bi], 3)),
                   color = "#7b2d8b", size = 3.5, hjust = -0.1) +
          scale_color_manual(values = c("Sens" = "#c9161d", "Spec" = "#008DC9",
                                        "Youden" = "#4dac26")) +
          labs(x = "Seuil de d\u00e9cision", y = "Score",
               title = paste0("Optimisation du seuil \u2014 ", input$model_thresh),
               color = "M\u00e9trique") +
          theme_minimal(base_size = 11) + theme(legend.position = "bottom")
        ggplotly(p)
      }, error = function(e) plotly_empty())
    })

    output$threshold_result <- renderUI({
      res <- pipeline_res()
      req(!is.null(res), res$is_binary, input$model_thresh)
      r <- res$results[[input$model_thresh]]; req(!is.null(r))
      tryCatch({
        cb_all <- pROC::coords(r$roc, x = "all",
                               ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"),
                               transpose = FALSE)
        cb_all <- cb_all[is.finite(cb_all$threshold), , drop = FALSE]
        if (nrow(cb_all) == 0)
          cb_all <- data.frame(threshold = 0.5, sensitivity = NA_real_,
                               specificity = NA_real_, ppv = NA_real_, npv = NA_real_)
        cb <- cb_all[which.max(cb_all$sensitivity + cb_all$specificity - 1), , drop = FALSE]
        youden <- cb$sensitivity + cb$specificity - 1
        c05 <- tryCatch(
          pROC::coords(r$roc, 0.5, ret = c("sensitivity", "specificity"),
                       transpose = FALSE),
          error = function(e) NULL)
        cmp_txt <- if (!is.null(c05) && nrow(c05) >= 1) {
          ds <- cb$sensitivity - c05$sensitivity[1]
          de <- cb$specificity - c05$specificity[1]
          paste0("au seuil 0.5 : Sens = ", round(c05$sensitivity[1], 3),
                 " / Spec = ", round(c05$specificity[1], 3),
                 " ; le seuil optimal change la sensibilit\u00e9 de ",
                 ifelse(ds >= 0, "+", ""), round(ds, 3),
                 " et la sp\u00e9cificit\u00e9 de ", ifelse(de >= 0, "+", ""), round(de, 3), ".")
        } else NULL
        tags$div(
          style = "background:#f0f8ff;padding:14px;border-radius:8px;border-left:4px solid #7b2d8b;",
          fluidRow(
            column(2, tags$div(class = "stat-card",
              tags$p(class = "sc-lbl", "Seuil"),
              tags$p(class = "sc-val", style = "color:#7b2d8b;", round(cb$threshold, 3)))),
            column(2, tags$div(class = "stat-card",
              tags$p(class = "sc-lbl", "Sens."),
              tags$p(class = "sc-val", round(cb$sensitivity, 3)))),
            column(2, tags$div(class = "stat-card",
              tags$p(class = "sc-lbl", "Spec."),
              tags$p(class = "sc-val", round(cb$specificity, 3)))),
            column(2, tags$div(class = "stat-card",
              tags$p(class = "sc-lbl", "VPP"),
              tags$p(class = "sc-val", round(cb$ppv, 3)))),
            column(2, tags$div(class = "stat-card",
              tags$p(class = "sc-lbl", "VPN"),
              tags$p(class = "sc-val", round(cb$npv, 3)))),
            column(2, tags$div(class = "stat-card",
              tags$p(class = "sc-lbl", "Youden"),
              tags$p(class = "sc-val", style = "color:#4dac26;", round(youden, 3))))
          ),
          if (!is.null(cmp_txt))
            tags$div(style = "margin-top:10px;font-size:12px;color:#444;",
              tags$b("Comparaison "), cmp_txt)
        )
      }, error = function(e) NULL)
    })

    # ══════════════════════════════════════════════════════════════
    # ONGLET 10 : Export
    # ══════════════════════════════════════════════════════════════
    output$dl_metrics <- downloadHandler(
      filename = function() paste0("metriques_", Sys.Date(), ".csv"),
      content = function(file) {
        res <- pipeline_res(); req(!is.null(res))
        write.csv(metrics_table(res), file, row.names = FALSE)
      })

    output$dl_roc_png <- downloadHandler(
      filename = function() paste0("roc_", Sys.Date(), ".png"),
      content = function(file) {
        res <- pipeline_res(); req(!is.null(res), res$is_binary)
        tryCatch({
          p <- plot_roc_compare(res)
          if (inherits(p, "gg")) ggsave(file, p, width = 10, height = 7, dpi = 200)
        }, error = function(e) NULL)
      })

    output$dl_predictions <- downloadHandler(
      filename = function() paste0("predictions_", Sys.Date(), ".csv"),
      content = function(file) {
        res <- pipeline_res(); req(!is.null(res))
        tryCatch({
          r <- res$results[[1]]
          oc <- res$outcome_col %||% ".y"
          df_p <- res$test_data |>
            mutate(Prediction = as.character(r$pred_class))
          if (!is.null(r$pred_prob) && is.numeric(r$pred_prob))
            df_p$Probabilite_Positif <- round(r$pred_prob, 4)
          write.csv(df_p, file, row.names = FALSE)
        }, error = function(e) writeLines(paste("Erreur:", e$message), file))
      })

    output$dl_excel <- downloadHandler(
      filename = function() paste0("predictif_rapport_", Sys.Date(), ".xlsx"),
      content = function(file) {
        res <- pipeline_res(); req(!is.null(res))
        sheets <- list()
        mt <- tryCatch(metrics_table(res), error = function(e) NULL)
        if (!is.null(mt)) sheets[["Metriques"]] <- mt

        for (nm in names(res$results)) {
          sheets[[paste0("Confusion_", nm)]] <- as.data.frame(res$results[[nm]]$cm$table)
          imp <- tryCatch(importance_table(res, nm), error = function(e) NULL)
          if (!is.null(imp)) sheets[[paste0("Importance_", nm)]] <- imp
        }

        pi <- perm_imp_res()
        if (!is.null(pi)) sheets[["Permutation"]] <- pi

        sheets[["Info"]] <- tibble(
          Param  = c("Type", "N_classes", "N_train", "N_test", "K_folds", "Modeles"),
          Valeur = c(
            if (res$is_binary) "Binaire" else "Multiclasse",
            res$n_classes, res$n_train, res$n_test, res$k_folds,
            paste(names(res$results), collapse = " | ")
          )
        )
        writexl::write_xlsx(sheets, file)
      })

    # ══════════════════════════════════════════════════════════════
    # STABILITE MULTI-SEEDS
    # ══════════════════════════════════════════════════════════════
    observeEvent(input$run_seeds, {
      d <- df_f(); req(d, input$outcome, input$predictors, input$models)
      n_s <- req(input$n_seeds)
      withProgress(message = paste0("Stabilit\u00e9 : ", n_s, " seeds en cours\u2026"), value = 0, {
        tryCatch({
          sr <- run_pipeline_seeds(d, input$outcome, input$predictors,
            k_folds = input$k_folds, models = input$models,
            train_pct = input$train_pct / 100, n_seeds = n_s)
          req(!is.null(sr))
          seeds_res(sr)
          incProgress(0.9)
        }, error = function(e)
          showNotification(paste("Erreur stabilit\u00e9 :", e$message),
                           type = "error", duration = 8))
      })
    })

    output$stability_msg <- renderUI({
      sr <- seeds_res(); if (is.null(sr)) return(NULL)
      sm <- sr$summary; req(nrow(sm) > 0)
      best <- sm[1, ]
      tags$div(style = "background:#e8f5e9;padding:10px 14px;border-radius:8px;border-left:4px solid #4dac26;margin-bottom:10px;",
        tags$b("\U0001f3c6 ", best$Modele),
        paste0(" : ", sr$metric, " moyen = ", round(best$Score_moyen, 3),
               " \u00b1 ", round(best$Score_sd, 3),
               "  (min ", round(best$Score_min, 3), " / max ", round(best$Score_max, 3), ")"),
        tags$span(style = "color:#666;margin-left:12px;font-size:11px;",
          ifelse(best$Score_sd < 0.02,
            "Variabilit\u00e9 faible \u2014 r\u00e9sultat robuste",
            "Variabilit\u00e9 notable \u2014 interpr\u00e9ter l'AUC moyen, pas un split unique"))
      )
    })

    output$stability_plot <- renderPlotly({
      sr <- seeds_res(); req(!is.null(sr))
      ggplotly(plot_stability_seeds(sr))
    })

    output$stability_dt <- renderDT({
      sr <- seeds_res(); req(!is.null(sr))
      sm <- sr$summary
      sm |>
        mutate(across(where(is.numeric), ~round(., 4))) |>
        datatable(rownames = FALSE,
          options = list(dom = "t", pageLength = 10, scrollX = TRUE,
            columnDefs = list(list(className = "dt-center", targets = "_all"))),
          class = "compact stripe hover") |>
        formatStyle(columns = "Score_moyen",
          backgroundColor = styleInterval(c(0.6, 0.75, 0.85),
            c("#fef2f2", "#fff7ed", "#f0fdf4", "#dcfce7")))
    })

    # ══════════════════════════════════════════════════════════════
    # EXPORTS PUBLICATION (300 dpi / PDF / RDS)
    # ══════════════════════════════════════════════════════════════
    output$dl_roc_300 <- downloadHandler(
      filename = function() paste0("roc_", Sys.Date(), "_300dpi.png"),
      content = function(file) {
        res <- pipeline_res(); req(!is.null(res))
        p <- gg_roc_simple(res)
        req(!is.null(p))
        ggsave(file, p, width = 11, height = 8.5, dpi = 300)
      })

    output$dl_rapport_pdf <- downloadHandler(
      filename = function() paste0("rapport_predictif_", Sys.Date(), ".pdf"),
      content = function(file) {
        res <- pipeline_res(); req(!is.null(res))
        ok <- rapport_predictions_pdf(res, file,
          model_name = input$sel_model_cm %||% NULL)
        if (!ok) showNotification("Erreur lors de la g\u00e9n\u00e9ration du PDF",
                                  type = "error", duration = 8)
      })

    output$dl_model_rds <- downloadHandler(
      filename = function() paste0("modele_", Sys.Date(), ".rds"),
      content = function(file) {
        res <- pipeline_res(); req(!is.null(res))
        mt <- tryCatch(metrics_table(res), error = function(e) NULL)
        metric_col <- if (res$is_binary && !is.null(mt) && "AUC" %in% names(mt)) "AUC"
                      else if (!is.null(mt) && "Accuracy" %in% names(mt)) "Accuracy" else NULL
        best_nm <- if (!is.null(metric_col)) mt[which.max(mt[[metric_col]]), ]$Modele
                   else names(res$results)[1]
        saveRDS(list(
          fit        = res$results[[best_nm]]$fit,
          outcome    = res$outcome_col,
          predictors = res$predictors,
          class_levels = res$original_levels,
          best_model = best_nm,
          date       = Sys.Date()), file)
      })

    # ══════════════════════════════════════════════════════════════
    # SCORING BATCH
    # ══════════════════════════════════════════════════════════════
    output$score_model_ui <- renderUI({
      mns <- model_names()
      if (is.null(mns)) return(
        tags$div(style = "font-size:11px;color:#e88400;", icon("info-circle"),
                 " Lancez d'abord le pipeline (onglet Configuration)."))
      r <- pipeline_res()
      mt <- tryCatch(metrics_table(r), error = function(e) NULL)
      metric_col <- if (r$is_binary && !is.null(mt) && "AUC" %in% names(mt)) "AUC"
                    else if (!is.null(mt) && "Accuracy" %in% names(mt)) "Accuracy" else NULL
      sel <- if (!is.null(metric_col)) mt[which.max(mt[[metric_col]]), ]$Modele else mns[1]
      selectInput(ns("score_model"), "Mod\u00e8le \u00e0 appliquer :",
                  choices = setNames(mns, mns), selected = sel, width = "100%")
    })

    observeEvent(input$run_score, {
      res <- pipeline_res(); req(!is.null(res), input$score_file,
                                 input$score_model, input$predictors)
      withProgress(message = "Scoring en cours\u2026", value = 0.3, {
        tryCatch({
          newd <- read.csv(input$score_file$datapath, stringsAsFactors = TRUE,
                           check.names = FALSE)
          # Recoder la cible si presente (inutile pour la prediction mais garde en sortie)
          sc <- score_new_data(res$results[[input$score_model]]$fit, newd,
                               input$predictors)
          incProgress(0.7)
          scored_res(list(data = cbind(newd, sc),
                          file = input$score_file$name,
                          model = input$score_model, n = nrow(newd)))
          showNotification(paste0("\u2705 ", nrow(newd),
            " observations scor\u00e9es avec ", input$score_model),
            type = "message", duration = 5)
        }, error = function(e)
          showNotification(paste("Erreur scoring :", e$message),
                           type = "error", duration = 8))
      })
    })

    output$score_info <- renderUI({
      s <- scored_res(); if (is.null(s)) return(NULL)
      tags$div(style = "background:#e8f5e9;border-radius:6px;padding:8px 10px;font-size:11px;margin-top:6px;",
        tags$b(icon("check-circle"), " ", s$n, " observations scor\u00e9es"),
        tags$br(), paste0("Fichier : ", s$file, " \u00b7 Mod\u00e8le : ", s$model))
    })

    output$score_preview <- renderDT({
      s <- scored_res(); req(!is.null(s))
      d <- as.data.frame(head(s$data, 200))
      datatable(d, rownames = FALSE,
        options = list(dom = "t", pageLength = 10, scrollX = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))),
        class = "compact stripe hover")
    })

    output$dl_scored <- downloadHandler(
      filename = function() {
        s <- scored_res(); req(!is.null(s))
        paste0("predictions_", tools::file_path_sans_ext(s$file), "_",
               Sys.Date(), ".csv")
      },
      content = function(file) {
        s <- scored_res(); req(!is.null(s))
        write.csv(s$data, file, row.names = FALSE, fileEncoding = "UTF-8")
      })

    # ══════════════════════════════════════════════════════════════
    # PDP (effet marginal)
    # ══════════════════════════════════════════════════════════════
    pdp_vars_order <- reactive({
      r <- pipeline_res(); req(!is.null(r))
      it <- tryCatch(importance_table(r, names(r$results)[1]), error = function(e) NULL)
      base <- input$predictors
      if (!is.null(it) && nrow(it) > 0) {
        ord <- as.character(it$Variable)
        ord <- ord[!is.na(ord) & nzchar(ord)]
        res <- intersect(ord, base)
        return(if (length(res) > 0) res else base)
      }
      base
    })

    output$pdp_var_ui <- renderUI({
      pv <- tryCatch(pdp_vars_order(), error = function(e) paste0("ERREUR:", conditionMessage(e)))
      if (is.character(pv) && length(pv) == 1 && startsWith(pv, "ERREUR:"))
        return(tags$div(style = "color:#c9161d;font-size:11px;", pv))
      if (is.null(pv) || length(pv) == 0)
        return(tags$div(style = "color:#e88400;font-size:11px;",
          "Aucune variable pr\u00e9dictive disponible (n_pred=",
          length(input$predictors %||% character(0)), ")"))
      selectInput(ns("pdp_var"), "Variable \u00e0 explorer :",
                  choices = pv, width = "100%")
    })

    output$pdp_model_ui <- renderUI({
      mns <- model_names()
      if (is.null(mns)) return(NULL)
      selectInput(ns("pdp_model"), "Mod\u00e8le :",
                  choices = setNames(mns, mns), width = "100%")
    })

    output$pdp_plot <- renderPlotly({
      r <- pipeline_res()
      if (is.null(r)) return(
        plotly_empty() |>
        layout(title = "Lancez le pipeline pour activer le PDP"))
      if (!r$is_binary) return(plotly_empty())
      req(input$pdp_var, input$pdp_model)
      pd <- tryCatch(
        calc_pdp(r$results[[input$pdp_model]]$fit, r$train_data,
                 input$pdp_var),
        error = function(e) NULL)
      lbl <- map_cols()[[input$pdp_var]] %||% NULL
      plot_pdp(pd, input$pdp_var, lbl)
    })

  })
}
