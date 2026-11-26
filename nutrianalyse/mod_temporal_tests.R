# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_temporal_tests.R — Tests Statistiques Temporels entre Vagues          ║
# ║  Chi-carré, Z-test proportions, Taille d'effet, Significativité           ║
# ║  Niveau Expert — NutriAnalyse Pro v2                                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── MOTEUR STATISTIQUE TEMPOREL (fonctions pures) ───────────────────────────

#' Test de comparaison de proportions entre deux vagues d'enquête
#' @param d1 données de la vague 1
#' @param d2 données de la vague 2
#' @param indicator nom de la colonne binaire (0/1) à comparer
#' @param method "chi2", "z_test", ou "fisher"
#' @return liste avec résultats du test, IC, taille d'effet
test_temporal_proportion <- function(d1, d2, indicator, method = "auto") {
  x1 <- d1[[indicator]]; x2 <- d2[[indicator]]
  x1 <- x1[!is.na(x1)]; x2 <- x2[!is.na(x2)]

  n1 <- length(x1); n2 <- length(x2)
  if (n1 < 5 || n2 < 5) {
    return(list(error="Effectifs insuffisants (n < 5 dans au moins une vague)"))
  }

  k1 <- sum(x1 == 1); k2 <- sum(x2 == 1)
  p1 <- k1/n1; p2 <- k2/n2

  # Construction du tableau 2×2
  mat <- matrix(c(k1, n1-k1, k2, n2-k2), nrow=2, byrow=TRUE,
                dimnames=list(c("Vague1","Vague2"), c("Cas","Non-cas")))

  # Auto-sélection de méthode
  if (method == "auto") {
    min_expected <- min(c(k1, n1-k1, k2, n2-k2) * c(n1,n1,n2,n2) / (n1+n2))
    method <- if(min(mat) < 5) "fisher" else "chi2"
  }

  # Test
  test_result <- tryCatch({
    if (method == "fisher") {
      ft <- fisher.test(mat)
      list(statistic=NA, p_value=ft$p.value, method="Fisher exact",
           or=if(!is.null(ft$estimate)) round(ft$estimate, 3) else NA,
           or_ci=if(!is.null(ft$conf.int)) round(ft$conf.int, 3) else c(NA,NA))
    } else if (method == "z_test") {
      pt <- prop.test(c(k1, k2), c(n1, n2), correct=TRUE)
      list(statistic=round(pt$statistic, 3), p_value=pt$p.value,
           method="Z-test (prop.test avec correction Yates)",
           or=NA, or_ci=c(NA,NA))
    } else {
      ct <- chisq.test(mat, correct=TRUE)
      list(statistic=round(ct$statistic, 3), p_value=ct$p.value,
           method="Chi-carré (Yates)", or=NA, or_ci=c(NA,NA))
    }
  }, error=function(e) {
    list(statistic=NA, p_value=NA, method=method, error=e$message,
         or=NA, or_ci=c(NA,NA))
  })

  # Différence de proportions avec IC 95%
  diff_prop <- p2 - p1
  se_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  ci_lo <- diff_prop - 1.96 * se_diff
  ci_hi <- diff_prop + 1.96 * se_diff

  # Taille d'effet (Cohen's h)
  h <- 2 * asin(sqrt(p2)) - 2 * asin(sqrt(p1))
  h_interpretation <- if(abs(h) < 0.2) "Négligeable"
                      else if(abs(h) < 0.5) "Petit"
                      else if(abs(h) < 0.8) "Moyen"
                      else "Grand"

  # Significativité
  sig <- if(is.na(test_result$p_value)) "Indéterminé"
         else if(test_result$p_value < 0.001) "Très significatif (***)"
         else if(test_result$p_value < 0.01) "Significatif (**)"
         else if(test_result$p_value < 0.05) "Significatif (*)"
         else if(test_result$p_value < 0.10) "Tendance (†)"
         else "Non significatif (ns)"

  stars <- if(is.na(test_result$p_value)) ""
           else if(test_result$p_value < 0.001) "***"
           else if(test_result$p_value < 0.01) "**"
           else if(test_result$p_value < 0.05) "*"
           else if(test_result$p_value < 0.10) "†"
           else "ns"

  # Direction
  direction <- if(diff_prop > 0) "↗ Augmentation"
               else if(diff_prop < 0) "↘ Diminution"
               else "→ Stable"

  list(
    indicator = indicator,
    n1 = n1, n2 = n2, k1 = k1, k2 = k2,
    p1 = round(p1*100, 2), p2 = round(p2*100, 2),
    diff_pp = round(diff_prop*100, 2),
    diff_ci_lo = round(ci_lo*100, 2), diff_ci_hi = round(ci_hi*100, 2),
    se = round(se_diff*100, 3),
    method = test_result$method,
    statistic = test_result$statistic,
    p_value = test_result$p_value,
    p_formatted = if(!is.na(test_result$p_value)) format.pval(test_result$p_value, digits=4) else "NA",
    effect_size_h = round(h, 3),
    effect_interpretation = h_interpretation,
    significance = sig,
    stars = stars,
    direction = direction,
    or = test_result$or,
    or_ci = test_result$or_ci
  )
}

#' Test de comparaison de proportions stratifié par zone géographique
test_temporal_by_zone <- function(d1, d2, indicator, zone_col) {
  zones <- sort(intersect(unique(d1[[zone_col]]), unique(d2[[zone_col]])))
  zones <- zones[!is.na(zones)]

  results <- lapply(zones, function(z) {
    d1z <- d1[d1[[zone_col]] == z & !is.na(d1[[zone_col]]),]
    d2z <- d2[d2[[zone_col]] == z & !is.na(d2[[zone_col]]),]
    if (nrow(d1z) < 5 || nrow(d2z) < 5) return(NULL)
    res <- test_temporal_proportion(d1z, d2z, indicator)
    res$zone <- as.character(z)
    res
  })
  results <- results[!sapply(results, is.null)]

  # Correction de Bonferroni si multiple zones
  if (length(results) > 1) {
    p_values <- sapply(results, function(r) r$p_value)
    p_adjusted <- p.adjust(p_values, method="holm")
    for (i in seq_along(results)) {
      results[[i]]$p_adjusted <- p_adjusted[i]
      results[[i]]$p_adj_formatted <- format.pval(p_adjusted[i], digits=4)
    }
  }

  results
}

# ── UI ─────────────────────────────────────────────────────────────────────────
temporalTestsUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    # Bandeau
    tags$div(
      style = paste0(
        "background:linear-gradient(135deg,#00426A 0%,#008DC9 40%,#e88400 100%);",
        "color:white;padding:24px 28px;border-radius:12px;margin-bottom:20px;",
        "box-shadow:0 4px 20px rgba(0,66,106,.3);"
      ),
      fluidRow(
        column(8,
          tags$h2(style="margin:0;font-weight:800;font-size:22px;",
                  icon("chart-line"), " Tests Statistiques Temporels"),
          tags$p(style="margin:6px 0 0;font-size:13px;opacity:.9;",
                 "Comparaison de prévalences entre vagues d'enquête — Chi², Z-test, Fisher exact"),
          tags$p(style="margin:4px 0 0;font-size:11px;opacity:.7;",
                 "Taille d'effet (Cohen's h) · IC 95% · Correction multi-tests · Analyse par zone")
        ),
        column(4, style="text-align:right;",
          uiOutput(ns("temporal_status"))
        )
      )
    ),

    # Panneau de configuration
    fluidRow(
      box(width=12, status="primary", solidHeader=TRUE,
          title=tags$span(icon("cogs"), " Configuration de l'Analyse"),
          fluidRow(
            column(3,
              uiOutput(ns("temporal_vague_col_ui")),
              uiOutput(ns("temporal_r1_ui")),
              uiOutput(ns("temporal_r2_ui"))
            ),
            column(3,
              uiOutput(ns("temporal_indicators_ui")),
              selectInput(ns("temporal_method"), "Méthode de test :",
                          choices=c("Auto-sélection"="auto", "Chi-carré"="chi2",
                                    "Z-test proportions"="z_test", "Fisher exact"="fisher"),
                          selected="auto", width="100%")
            ),
            column(3,
              uiOutput(ns("temporal_zone_col_ui")),
              numericInput(ns("temporal_alpha"), "Seuil α :", value=0.05, min=0.001, max=0.10, step=0.01, width="100%")
            ),
            column(3, style="padding-top:20px;",
              actionButton(ns("run_temporal_test"), "Lancer les Tests",
                           icon=icon("flask"),
                           style="width:100%;padding:12px;font-size:14px;font-weight:700;",
                           class="btn-danger"),
              tags$br(), tags$br(),
              checkboxInput(ns("temporal_by_zone"), "Analyse stratifiée par zone", value=FALSE, width="100%")
            )
          )
      )
    ),

    # Résultats
    uiOutput(ns("temporal_results_panel")),

    # Détails
    do.call(tabsetPanel, c(
      list(id=ns("temporal_tabs"), type="pills"),
      list(
      tabPanel(title=tagList(icon("table"), " Résultats Proportions"),
        DTOutput(ns("temporal_results_table")),
        tags$br(),
        uiOutput(ns("temporal_interpretation"))
      ),
      tabPanel(title=tagList(icon("chart-bar"), " Visualisation"),
        fluidRow(
          box(width=6, status="info", solidHeader=TRUE,
              title="Comparaison des Prévalences (V1 vs V2)",
              plotlyOutput(ns("temporal_comparison_plot"), height="400px")
          ),
          box(width=6, status="warning", solidHeader=TRUE,
              title="Forest Plot — Différences (pp)",
              plotlyOutput(ns("temporal_forest_plot"), height="400px")
          )
        ),
        fluidRow(
          box(width=12, status="success", solidHeader=TRUE,
              title="Taille d'Effet (Cohen's h) par Indicateur",
              plotlyOutput(ns("temporal_effect_plot"), height="300px")
          )
        )
      ),
      tabPanel(title=tagList(icon("map-marker-alt"), " Par Zone"),
        uiOutput(ns("temporal_zone_results")),
        DTOutput(ns("temporal_zone_table")),
        plotlyOutput(ns("temporal_zone_plot"), height="450px")
      )
      ),

      # ── Onglets Expert (7 techniques avancées) ──
      temporal_expert_tabs(ns)
    ))
  )
}

# ── SERVER ─────────────────────────────────────────────────────────────────────
temporalTestsServer <- function(id, df_f, map_cols, audit_log = NULL, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Stockage des résultats
    test_results <- reactiveVal(NULL)
    zone_results <- reactiveVal(NULL)

    # ── UI dynamiques ─────────────────────────────────────────────────

    # Détection automatique de la colonne vague
    survey_col <- reactive({
      d <- df_f(); m <- map_cols(); req(d)
      if (!is.null(m$survey) && m$survey %in% names(d)) return(m$survey)
      # [M2] Patterns élargis pour auto-détection de la colonne vague
      candidates <- grep("survey_round|round|vague|enquete_round|wave|phase|period|passage|collecte|cycle|saison|trimestre",
                         names(d), ignore.case=TRUE, value=TRUE)
      if (length(candidates) > 0) candidates[1] else NULL
    })

    output$temporal_vague_col_ui <- renderUI({
      d <- df_f(); req(d)
      sc <- survey_col()
      all_cats <- names(d)[sapply(d, function(x) is.character(x) || is.factor(x) || dplyr::n_distinct(x) <= 20)]
      selectInput(ns("temporal_vague_col"), "Colonne Vague/Round :",
                  choices=all_cats, selected=sc, width="100%")
    })

    output$temporal_r1_ui <- renderUI({
      d <- df_f(); req(d, input$temporal_vague_col)
      col <- input$temporal_vague_col
      req(col %in% names(d))
      vagues <- sort(unique(na.omit(d[[col]])))
      selectInput(ns("temporal_r1"), "Vague 1 (référence) :",
                  choices=vagues, selected=vagues[1], width="100%")
    })

    output$temporal_r2_ui <- renderUI({
      d <- df_f(); req(d, input$temporal_vague_col)
      col <- input$temporal_vague_col
      req(col %in% names(d))
      vagues <- sort(unique(na.omit(d[[col]])))
      selectInput(ns("temporal_r2"), "Vague 2 (comparaison) :",
                  choices=vagues, selected=vagues[length(vagues)], width="100%")
    })

    output$temporal_indicators_ui <- renderUI({
      d <- df_f(); req(d)
      # Indicateurs binaires (0/1)
      bin_cols <- names(d)[sapply(d, function(x) {
        vals <- unique(na.omit(x))
        length(vals) <= 2 && all(vals %in% c(0, 1))
      })]
      # [M1] Prioriser les indicateurs OMS — noms exacts + patterns regex
      oms_exact <- intersect(c("GAM","SAM","MAM","MAG","MAS","Stunting","Underweight",
                                "MUAC_GAM","MUAC_SAM","Retard_croissance","Emaciation",
                                "Insuffisance_ponderale"), bin_cols)
      oms_regex <- grep("gam|sam|mam|mag|mas|stunt|wast|underw|emaci|malnut",
                         bin_cols, ignore.case = TRUE, value = TRUE)
      oms_first <- unique(c(oms_exact, oms_regex))
      others <- setdiff(bin_cols, oms_first)
      all_choices <- c(oms_first, others)

      checkboxGroupInput(ns("temporal_indicators"), "Indicateurs à tester :",
                         choices=all_choices,
                         selected=oms_first[1:min(3, length(oms_first))],
                         width="100%")
    })

    output$temporal_zone_col_ui <- renderUI({
      d <- df_f(); m <- map_cols(); req(d)
      zone_candidates <- c(m$region, m$dept)
      zone_candidates <- zone_candidates[!sapply(zone_candidates, is.null) & zone_candidates %in% names(d)]
      all_cats <- names(d)[sapply(d, function(x) (is.character(x) || is.factor(x)) && dplyr::n_distinct(x) >= 2 && dplyr::n_distinct(x) <= 50)]
      selectInput(ns("temporal_zone_col"), "Colonne zone (stratification) :",
                  choices=all_cats, selected=if(length(zone_candidates)>0) zone_candidates[1] else all_cats[1],
                  width="100%")
    })

    output$temporal_status <- renderUI({
      d <- df_f()
      sc <- survey_col()
      if (is.null(d)) {
        return(tags$span(style="font-size:13px;opacity:.7;", "Aucune donnée chargée"))
      }
      if (is.null(sc)) {
        return(tags$span(
          style="background:rgba(201,22,29,.4);padding:6px 14px;border-radius:16px;font-size:12px;",
          icon("exclamation-triangle"), " Colonne vague non détectée"
        ))
      }
      nv <- length(unique(na.omit(d[[sc]])))
      tags$span(
        style="background:rgba(77,172,38,.4);padding:6px 14px;border-radius:16px;font-size:13px;font-weight:600;",
        icon("check-circle"), paste0(" ", nv, " vagues dans '", sc, "'")
      )
    })

    # ── Lancer les tests ──────────────────────────────────────────────
    observeEvent(input$run_temporal_test, {
      d <- df_f()
      req(d, input$temporal_vague_col, input$temporal_r1, input$temporal_r2, input$temporal_indicators)
      req(input$temporal_r1 != input$temporal_r2)

      col_vague <- input$temporal_vague_col
      r1 <- input$temporal_r1; r2 <- input$temporal_r2
      indicators <- input$temporal_indicators
      method <- input$temporal_method

      d1 <- d[d[[col_vague]] == r1 & !is.na(d[[col_vague]]),]
      d2 <- d[d[[col_vague]] == r2 & !is.na(d[[col_vague]]),]

      req(nrow(d1) > 0, nrow(d2) > 0)

      withProgress(message="Tests statistiques en cours...", value=0, {

        # Tests globaux
        results <- lapply(seq_along(indicators), function(i) {
          incProgress(1/length(indicators) * 0.6, detail=indicators[i])
          res <- test_temporal_proportion(d1, d2, indicators[i], method)
          res$vague1 <- as.character(r1)
          res$vague2 <- as.character(r2)
          res
        })

        # Correction multi-tests (Holm-Bonferroni)
        if (length(results) > 1) {
          p_vals <- sapply(results, function(r) r$p_value)
          p_adj <- p.adjust(p_vals, method="holm")
          for (i in seq_along(results)) {
            results[[i]]$p_adjusted <- p_adj[i]
            results[[i]]$p_adj_formatted <- format.pval(p_adj[i], digits=4)
            results[[i]]$sig_adjusted <- if(is.na(p_adj[i])) "Indéterminé"
                                          else if(p_adj[i] < 0.001) "Très significatif (***)"
                                          else if(p_adj[i] < 0.01) "Significatif (**)"
                                          else if(p_adj[i] < 0.05) "Significatif (*)"
                                          else "Non significatif (ns)"
          }
        }

        test_results(results)

        # Tests par zone si demandé
        if (input$temporal_by_zone && !is.null(input$temporal_zone_col)) {
          incProgress(0.3, detail="Analyse par zone...")
          zone_col <- input$temporal_zone_col
          zr <- lapply(indicators, function(ind) {
            test_temporal_by_zone(d1, d2, ind, zone_col)
          })
          names(zr) <- indicators
          zone_results(zr)
        } else {
          zone_results(NULL)
        }
      })

      # Log
      if (is.function(audit_log)) {
        n_sig <- sum(sapply(results, function(r) !is.na(r$p_value) && r$p_value < 0.05))
        audit_log(list(
          timestamp = Sys.time(),
          action = "temporal_test",
          module = "Tests Temporels",
          details = paste0(length(indicators), " indicateurs testés (",
                          r1, " vs ", r2, ") — ", n_sig, " significatifs")
        ))
      }

      showNotification(
        paste0("Tests terminés — ", length(indicators), " indicateurs analysés"),
        type="message", duration=4
      )
    })

    # ── Panneau de résultats global ────────────────────────────────────
    output$temporal_results_panel <- renderUI({
      results <- test_results()
      if (is.null(results)) return(NULL)

      n_sig <- sum(sapply(results, function(r) !is.na(r$p_value) && r$p_value < 0.05))
      n_total <- length(results)

      cards <- lapply(results, function(r) {
        if (!is.null(r$error)) {
          return(column(width=4,
            tags$div(style="background:#fff3e0;border-radius:10px;padding:14px;margin-bottom:12px;",
              tags$strong(style="color:#e88400;", r$indicator), tags$br(),
              tags$span(style="color:#e88400;font-size:12px;", r$error))
          ))
        }

        sev_style <- if(!is.na(r$p_value) && r$p_value < 0.05) {
          list(bg="#ffebee", border="#c9161d", text="#b71c1c")
        } else {
          list(bg="#e8f5e9", border="#4dac26", text="#2e7d32")
        }

        dir_color <- if(r$diff_pp > 0) "#c9161d" else if(r$diff_pp < 0) "#4dac26" else "#666"

        column(width=4,
          tags$div(
            style=paste0("background:", sev_style$bg, ";border:2px solid ", sev_style$border,
                         ";border-radius:12px;padding:16px;margin-bottom:12px;min-height:200px;"),
            # Header
            tags$div(style="display:flex;justify-content:space-between;margin-bottom:10px;",
              tags$strong(style="font-size:15px;color:#00426A;", r$indicator),
              tags$span(style=paste0("padding:3px 10px;border-radius:12px;font-size:11px;font-weight:700;",
                                     "background:", sev_style$border, ";color:white;"),
                        r$stars)
            ),
            # Prevalences
            tags$div(style="display:flex;justify-content:space-around;text-align:center;margin-bottom:8px;",
              tags$div(
                tags$div(style="font-size:22px;font-weight:800;color:#008DC9;", paste0(r$p1, "%")),
                tags$div(style="font-size:11px;color:#666;", paste0("V1 (n=", r$n1, ")"))
              ),
              tags$div(style="font-size:20px;color:#888;padding-top:4px;", "→"),
              tags$div(
                tags$div(style="font-size:22px;font-weight:800;color:#e88400;", paste0(r$p2, "%")),
                tags$div(style="font-size:11px;color:#666;", paste0("V2 (n=", r$n2, ")"))
              )
            ),
            # Différence
            tags$div(style=paste0("text-align:center;padding:8px;background:white;border-radius:8px;margin-bottom:6px;"),
              tags$div(style=paste0("font-size:18px;font-weight:700;color:", dir_color, ";"),
                       paste0(r$direction, " ", ifelse(r$diff_pp > 0, "+", ""), r$diff_pp, " pp")),
              tags$div(style="font-size:11px;color:#666;",
                       paste0("IC95: [", r$diff_ci_lo, " ; ", r$diff_ci_hi, "] pp"))
            ),
            # Test
            tags$div(style="font-size:11px;color:#555;",
              tags$span(paste0("p = ", r$p_formatted, " (", r$method, ")")),
              tags$br(),
              tags$span(paste0("Cohen's h = ", r$effect_size_h, " (", r$effect_interpretation, ")"))
            )
          )
        )
      })

      tagList(
        tags$div(
          style=paste0("background:white;border-radius:12px;padding:16px;margin-bottom:16px;",
                       "box-shadow:0 2px 8px rgba(0,0,0,.06);"),
          fluidRow(
            column(6,
              tags$h4(style="margin:0;color:#00426A;font-weight:700;",
                      icon("flask"), paste0(" Résultats: ", n_sig, "/", n_total, " tests significatifs"))
            ),
            column(6, style="text-align:right;font-size:12px;color:#666;",
              tags$span(paste0("Seuil α = ", input$temporal_alpha)),
              if(n_total > 1) tags$span(style="margin-left:12px;",
                                        tags$em("Correction Holm-Bonferroni appliquée"))
            )
          )
        ),
        fluidRow(cards)
      )
    })

    # ── Table détaillée ────────────────────────────────────────────────
    output$temporal_results_table <- renderDT({
      results <- test_results(); req(results)

      df_res <- do.call(rbind, lapply(results, function(r) {
        if (!is.null(r$error)) {
          return(data.frame(Indicateur=r$indicator, Erreur=r$error, stringsAsFactors=FALSE))
        }
        data.frame(
          Indicateur = r$indicator,
          `Prev V1 (%)` = r$p1,
          `n V1` = r$n1,
          `Prev V2 (%)` = r$p2,
          `n V2` = r$n2,
          `Diff (pp)` = r$diff_pp,
          `IC95 bas` = r$diff_ci_lo,
          `IC95 haut` = r$diff_ci_hi,
          Methode = r$method,
          `p-value` = r$p_formatted,
          `p ajuste` = if(!is.null(r$p_adj_formatted)) r$p_adj_formatted else r$p_formatted,
          `Cohen h` = r$effect_size_h,
          Effet = r$effect_interpretation,
          Significativite = r$significance,
          Direction = r$direction,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }))

      datatable(df_res, rownames=FALSE, filter="top",
                options=list(pageLength=20, dom="ftp", scrollX=TRUE,
                             columnDefs=list(list(className="dt-center", targets="_all"))),
                class="compact stripe hover") |>
        formatStyle("Significativite",
                    backgroundColor=styleEqual(
                      c("Très significatif (***)","Significatif (**)","Significatif (*)",
                        "Tendance (†)","Non significatif (ns)"),
                      c("#ffcdd2","#ffcdd2","#ffe0b2","#fff9c4","#e8f5e9")),
                    fontWeight="bold") |>
        formatStyle("Direction",
                    color=styleEqual(
                      c("↗ Augmentation","↘ Diminution","→ Stable"),
                      c("#c9161d","#4dac26","#666")),
                    fontWeight="bold")
    })

    # ── Interprétation ─────────────────────────────────────────────────
    output$temporal_interpretation <- renderUI({
      results <- test_results(); req(results)

      interps <- lapply(results, function(r) {
        if (!is.null(r$error)) return(NULL)
        vc <- if(!is.na(r$p_value) && r$p_value < 0.05) "#c9161d" else "#4dac26"

        tags$div(style=paste0("border-left:4px solid ", vc,
                               ";padding:10px 14px;background:#f8f9fa;border-radius:6px;margin-bottom:8px;"),
          tags$strong(style=paste0("color:", vc, ";"), r$indicator, " : "),
          tags$span(
            paste0(
              "La prévalence est passée de ", r$p1, "% (V1, n=", r$n1, ") à ",
              r$p2, "% (V2, n=", r$n2, "), soit une variation de ",
              ifelse(r$diff_pp > 0, "+", ""), r$diff_pp, " points de pourcentage ",
              "(IC95: [", r$diff_ci_lo, " ; ", r$diff_ci_hi, "]). "
            )
          ),
          tags$span(
            if(!is.na(r$p_value) && r$p_value < 0.05) {
              paste0("Cette différence est statistiquement significative (", r$method,
                     ", p=", r$p_formatted, "). L'ampleur de l'effet est ",
                     tolower(r$effect_interpretation), " (h=", r$effect_size_h, ").")
            } else {
              paste0("Cette différence n'est pas statistiquement significative (", r$method,
                     ", p=", r$p_formatted, "). ",
                     "L'absence de significativité peut être due à un effectif insuffisant ou à une réelle stabilité de la situation.")
            }
          )
        )
      })

      tags$div(
        tags$h5(style="color:#00426A;font-weight:700;margin:16px 0 10px;",
                icon("comment-medical"), " Interprétation des Résultats"),
        tagList(interps[!sapply(interps, is.null)]),
        if(length(results) > 1) {
          tags$div(style="background:#ede7f6;border-radius:8px;padding:12px;margin-top:12px;font-size:12px;",
            icon("info-circle", style="color:#6c3483;"),
            tags$strong(style="color:#6c3483;", " Note méthodologique : "),
            "Une correction de Holm-Bonferroni a été appliquée pour tenir compte des tests multiples. ",
            "Les p-values ajustées contrôlent le taux d'erreur global (FWER)."
          )
        }
      )
    })

    # ── Visualisation: Comparaison ─────────────────────────────────────
    output$temporal_comparison_plot <- renderPlotly({
      results <- test_results(); req(results)
      valid <- results[sapply(results, function(r) is.null(r$error))]
      req(length(valid) > 0)

      df_plot <- do.call(rbind, lapply(valid, function(r) {
        rbind(
          data.frame(Indicateur=r$indicator, Vague="V1", Prevalence=r$p1, n=r$n1, stringsAsFactors=FALSE),
          data.frame(Indicateur=r$indicator, Vague="V2", Prevalence=r$p2, n=r$n2, stringsAsFactors=FALSE)
        )
      }))

      p <- ggplot(df_plot, aes(x=Indicateur, y=Prevalence, fill=Vague,
                                text=paste0(Indicateur, " — ", Vague, "\n",
                                           Prevalence, "% (n=", n, ")"))) +
        geom_col(position="dodge", width=0.65, alpha=0.85) +
        scale_fill_manual(values=c("V1"="#008DC9", "V2"="#e88400")) +
        labs(x="", y="Prévalence (%)") +
        theme_minimal(base_size=12) +
        theme(legend.position="bottom")

      ggplotly(p, tooltip="text") |> layout(hovermode="closest",
                                             legend=list(orientation="h", y=-0.15))
    })

    # ── Visualisation: Forest plot ─────────────────────────────────────
    output$temporal_forest_plot <- renderPlotly({
      results <- test_results(); req(results)
      valid <- results[sapply(results, function(r) is.null(r$error))]
      req(length(valid) > 0)

      df_fp <- do.call(rbind, lapply(valid, function(r) {
        data.frame(
          Indicateur=r$indicator,
          Diff=r$diff_pp,
          CI_lo=r$diff_ci_lo,
          CI_hi=r$diff_ci_hi,
          Sig=if(!is.na(r$p_value) && r$p_value < 0.05) "Significatif" else "Non significatif",
          stringsAsFactors=FALSE
        )
      }))

      p <- ggplot(df_fp, aes(x=Diff, y=reorder(Indicateur, abs(Diff)), color=Sig,
                              text=paste0(Indicateur, "\nDiff: ", ifelse(Diff>0,"+",""), Diff,
                                         " pp\nIC95: [", CI_lo, " ; ", CI_hi, "]"))) +
        geom_vline(xintercept=0, linetype="dashed", color="gray50") +
        geom_errorbarh(aes(xmin=CI_lo, xmax=CI_hi), height=0.25, linewidth=0.8) +
        geom_point(size=4) +
        scale_color_manual(values=c("Significatif"="#c9161d", "Non significatif"="#008DC9")) +
        labs(x="Différence (points de pourcentage)", y="", color="") +
        theme_minimal(base_size=12) +
        theme(legend.position="bottom")

      ggplotly(p, tooltip="text") |> layout(hovermode="closest")
    })

    # ── Taille d'effet ─────────────────────────────────────────────────
    output$temporal_effect_plot <- renderPlotly({
      results <- test_results(); req(results)
      valid <- results[sapply(results, function(r) is.null(r$error))]
      req(length(valid) > 0)

      df_eff <- do.call(rbind, lapply(valid, function(r) {
        data.frame(Indicateur=r$indicator, h=r$effect_size_h,
                   Interp=r$effect_interpretation, stringsAsFactors=FALSE)
      }))

      cols <- c("Négligeable"="#4dac26", "Petit"="#f9c734", "Moyen"="#e88400", "Grand"="#c9161d")

      p <- ggplot(df_eff, aes(y=reorder(Indicateur, abs(h)), x=abs(h), fill=Interp,
                               text=paste0(Indicateur, "\nh=", h, " (", Interp, ")"))) +
        geom_col(alpha=0.85, width=0.5) +
        geom_vline(xintercept=c(0.2, 0.5, 0.8), linetype="dotted", color="gray60") +
        scale_fill_manual(values=cols) +
        labs(x="Cohen's h (valeur absolue)", y="", fill="Effet") +
        theme_minimal(base_size=12) +
        theme(legend.position="bottom")

      ggplotly(p, tooltip="text")
    })

    # ── Résultats par zone ─────────────────────────────────────────────
    output$temporal_zone_results <- renderUI({
      zr <- zone_results()
      if (is.null(zr)) {
        return(tags$div(style="padding:30px;text-align:center;color:#888;background:#f5f5f5;border-radius:10px;",
          icon("info-circle"), " Activez 'Analyse stratifiée par zone' et relancez les tests"))
      }
      n_tests <- sum(sapply(zr, length))
      n_sig <- sum(sapply(zr, function(ind_res) {
        sum(sapply(ind_res, function(r) !is.na(r$p_value) && r$p_value < 0.05))
      }))
      tags$div(style="background:#e3f2fd;border-radius:10px;padding:12px 16px;margin-bottom:12px;",
        icon("map-marker-alt", style="color:#008DC9;"),
        tags$strong(style="color:#00426A;", paste0(" ", n_tests, " tests par zone — ", n_sig, " significatifs")),
        tags$span(style="font-size:12px;color:#666;margin-left:12px;",
                  "(Correction Holm appliquée par indicateur)")
      )
    })

    output$temporal_zone_table <- renderDT({
      zr <- zone_results(); req(zr)

      df_zone <- do.call(rbind, lapply(names(zr), function(ind) {
        do.call(rbind, lapply(zr[[ind]], function(r) {
          if (!is.null(r$error)) return(NULL)
          data.frame(
            Indicateur = ind,
            Zone = r$zone,
            `V1 (%)` = r$p1,
            `V2 (%)` = r$p2,
            `Diff (pp)` = r$diff_pp,
            `p-value` = r$p_formatted,
            `p ajusté` = if(!is.null(r$p_adj_formatted)) r$p_adj_formatted else r$p_formatted,
            Direction = r$direction,
            `Cohen h` = r$effect_size_h,
            check.names=FALSE, stringsAsFactors=FALSE
          )
        }))
      }))

      req(nrow(df_zone) > 0)

      datatable(df_zone, rownames=FALSE, filter="top",
                options=list(pageLength=20, dom="ftp", scrollX=TRUE),
                class="compact stripe hover") |>
        formatStyle("Direction",
                    color=styleEqual(c("↗ Augmentation","↘ Diminution","→ Stable"),
                                     c("#c9161d","#4dac26","#666")),
                    fontWeight="bold")
    })

    output$temporal_zone_plot <- renderPlotly({
      zr <- zone_results(); req(zr)

      df_zp <- do.call(rbind, lapply(names(zr), function(ind) {
        do.call(rbind, lapply(zr[[ind]], function(r) {
          if (!is.null(r$error)) return(NULL)
          data.frame(Indicateur=ind, Zone=r$zone, Diff=r$diff_pp,
                     Sig=if(!is.na(r$p_value) && r$p_value < 0.05) "Sig" else "NS",
                     stringsAsFactors=FALSE)
        }))
      }))
      req(nrow(df_zp) > 0)

      p <- ggplot(df_zp, aes(x=Zone, y=Diff, fill=Indicateur, alpha=Sig,
                              text=paste0(Zone, " — ", Indicateur, "\nDiff: ",
                                         ifelse(Diff>0,"+",""), Diff, " pp"))) +
        geom_col(position="dodge", width=0.7) +
        geom_hline(yintercept=0, color="gray30") +
        scale_alpha_manual(values=c("Sig"=0.9, "NS"=0.4)) +
        labs(x="", y="Différence (pp)", fill="Indicateur") +
        theme_minimal(base_size=12) +
        theme(axis.text.x=element_text(angle=30, hjust=1), legend.position="bottom")

      ggplotly(p, tooltip="text") |> layout(hovermode="closest")
    })

    # ── Expert Tabs Server (7 techniques avancées) ──────────────────────
    source("server_temporel_expert.R", local = TRUE)

  })
}

cat("mod_temporal_tests.R chargé — Tests Statistiques Temporels Expert\n")
