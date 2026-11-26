# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_qualite_auto.R — Contrôle Qualité Automatisé Expert                  ║
# ║  Digit Preference, Score Global, Cohérence, Anomalies par cluster         ║
# ║  Niveau Expert — NutriAnalyse Pro v2                                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── UI ─────────────────────────────────────────────────────────────────────────
qualiteAutoUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    # Bandeau
    tags$div(
      style = paste0(
        "background:linear-gradient(135deg,#00426A 0%,#008DC9 50%,#c9161d 100%);",
        "color:white;padding:24px 28px;border-radius:12px;margin-bottom:20px;",
        "box-shadow:0 4px 20px rgba(0,66,106,.3);"
      ),
      fluidRow(
        column(8,
          tags$h2(style="margin:0;font-weight:800;font-size:22px;",
                  icon("shield-alt"), " Contrôle Qualité Automatisé Expert"),
          tags$p(style="margin:6px 0 0;font-size:13px;opacity:.9;",
                 "Digit preference · Cohérence âge/taille · Score qualité global · Anomalies par cluster")
        ),
        column(4, style="text-align:right;",
          actionButton(ns("run_qc"), "Lancer l'Audit Qualité",
                       icon=icon("play-circle"),
                       style="background:rgba(255,255,255,.2);border:1px solid rgba(255,255,255,.5);color:white;font-size:13px;padding:10px 24px;border-radius:24px;font-weight:600;"),
          tags$br(), tags$br(),
          uiOutput(ns("qc_status_badge"))
        )
      )
    ),

    # Score global
    uiOutput(ns("qc_score_global_panel")),

    # Détails par critère
    tabsetPanel(id=ns("qc_tabs"), type="pills",
      tabPanel(title=tagList(icon("fingerprint"), " Digit Preference"),
        fluidRow(
          box(width=6, status="warning", solidHeader=TRUE,
              title="Digit Preference — Poids",
              plotlyOutput(ns("qc_dp_weight"), height="320px"),
              uiOutput(ns("qc_dp_weight_result"))
          ),
          box(width=6, status="warning", solidHeader=TRUE,
              title="Digit Preference — Taille",
              plotlyOutput(ns("qc_dp_height"), height="320px"),
              uiOutput(ns("qc_dp_height_result"))
          )
        ),
        fluidRow(
          box(width=6, status="info", solidHeader=TRUE,
              title="Digit Preference — MUAC",
              plotlyOutput(ns("qc_dp_muac"), height="320px"),
              uiOutput(ns("qc_dp_muac_result"))
          ),
          box(width=6, status="info", solidHeader=TRUE,
              title="Digit Preference — Âge (mois)",
              plotlyOutput(ns("qc_dp_age"), height="320px"),
              uiOutput(ns("qc_dp_age_result"))
          )
        )
      ),

      tabPanel(title=tagList(icon("check-double"), " Cohérence"),
        fluidRow(
          box(width=6, status="danger", solidHeader=TRUE,
              title="Cohérence Âge/Taille",
              plotlyOutput(ns("qc_age_height"), height="380px"),
              uiOutput(ns("qc_age_height_result"))
          ),
          box(width=6, status="danger", solidHeader=TRUE,
              title="Cohérence Poids/Taille",
              plotlyOutput(ns("qc_weight_height"), height="380px"),
              uiOutput(ns("qc_weight_height_result"))
          )
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title="Flags de plausibilité SMART",
              DTOutput(ns("qc_flags_table")),
              uiOutput(ns("qc_flags_summary"))
          )
        )
      ),

      tabPanel(title=tagList(icon("map-marker-alt"), " Anomalies Cluster"),
        fluidRow(
          box(width=12, status="danger", solidHeader=TRUE,
              title="Détection d'anomalies par cluster/grappe",
              uiOutput(ns("qc_cluster_alert")),
              DTOutput(ns("qc_cluster_table")),
              plotlyOutput(ns("qc_cluster_plot"), height="400px")
          )
        )
      ),

      tabPanel(title=tagList(icon("clipboard-check"), " Complétude"),
        fluidRow(
          box(width=12, status="success", solidHeader=TRUE,
              title="Taux de complétude par variable clé",
              plotlyOutput(ns("qc_completeness"), height="350px"),
              DTOutput(ns("qc_completeness_table"))
          )
        )
      ),

      tabPanel(title=tagList(icon("file-medical-alt"), " Rapport Qualité"),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title="Rapport de synthèse qualité",
              uiOutput(ns("qc_full_report")),
              downloadButton(ns("qc_download_report"), "Télécharger le rapport qualité",
                             style="margin-top:12px;")
          )
        )
      )
    )
  )
}

# ── SERVER ─────────────────────────────────────────────────────────────────────
qualiteAutoServer <- function(id, df_f, map_cols, audit_log = NULL, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Réactif pour stocker les résultats QC
    qc_results <- reactiveVal(NULL)

    # ── Fonctions utilitaires ──────────────────────────────────────────

    # Test de Digit Preference (DPS = Digit Preference Score)
    # Méthode SMART: DPS basé sur la déviation de l'uniformité
    calc_digit_preference <- function(values, n_digits=10) {
      values <- values[!is.na(values)]
      if (length(values) < 30) return(list(dps=NA, classification="Insuffisant", freq=NULL, p_value=NA))

      # Extraire le dernier chiffre
      if (all(values == round(values))) {
        last_digits <- values %% n_digits
      } else {
        # Pour les décimales, prendre le dernier chiffre significatif
        last_digits <- round(values * 10) %% 10
      }

      freq <- table(factor(last_digits, levels=0:(n_digits-1)))
      n <- length(last_digits)
      expected <- n / n_digits

      # DPS (SMART method): sum of absolute deviations from expected
      dps <- round(sum(abs(as.numeric(freq) - expected)) / n * 100, 1)

      # Chi-squared test
      chi_test <- tryCatch(
        chisq.test(as.numeric(freq)),
        error = function(e) list(p.value=NA)
      )

      # Classification SMART
      classification <- if(dps <= 7) "Bon (0–7)"
                        else if(dps <= 12) "Acceptable (8–12)"
                        else if(dps <= 20) "Problématique (13–20)"
                        else "Sérieux (>20)"

      quality <- if(dps <= 7) "excellent"
                 else if(dps <= 12) "bon"
                 else if(dps <= 20) "problematique"
                 else "serieux"

      list(dps=dps, classification=classification, quality=quality,
           freq=as.data.frame(freq), p_value=chi_test$p.value, n=n)
    }

    # Age heaping (index de Whipple simplifié)
    calc_age_heaping <- function(ages) {
      ages <- ages[!is.na(ages)]
      if (length(ages) < 50) return(list(index=NA, classification="Insuffisant"))

      # Fréquence des âges multiples de 6 et 12 mois
      n_total <- length(ages)
      n_mult6 <- sum(ages %% 6 == 0)
      n_mult12 <- sum(ages %% 12 == 0)

      idx_6 <- round(n_mult6 / n_total * 100, 1)
      idx_12 <- round(n_mult12 / n_total * 100, 1)

      # Attendu si uniforme: 16.7% pour mult6, 8.3% pour mult12
      classification <- if(idx_12 > 25) "Sérieux — forte préférence âges ronds"
                        else if(idx_12 > 15) "Problématique — préférence modérée"
                        else "Acceptable"

      list(index_6=idx_6, index_12=idx_12, classification=classification, n=n_total)
    }

    # Score de qualité global (0-100)
    calc_quality_score <- function(d, m) {
      scores <- list()
      details <- list()

      # 1. Complétude des variables clés (max 25 pts)
      key_vars <- c(m$weight, m$height, m$age, m$muac, m$sex)
      key_vars <- key_vars[!sapply(key_vars, is.null) & key_vars %in% names(d)]
      if (length(key_vars) > 0) {
        completeness <- mean(sapply(key_vars, function(v) sum(!is.na(d[[v]])) / nrow(d)))
        sc <- round(completeness * 25, 0)
        scores$completude <- sc
        details$completude <- paste0(round(completeness*100,1), "% complet")
      }

      # 2. Digit preference poids (max 20 pts)
      if (!is.null(m$weight) && m$weight %in% names(d)) {
        dp_w <- calc_digit_preference(d[[m$weight]])
        sc <- if(is.na(dp_w$dps)) 10
              else if(dp_w$dps <= 7) 20
              else if(dp_w$dps <= 12) 15
              else if(dp_w$dps <= 20) 8
              else 0
        scores$dp_poids <- sc
        details$dp_poids <- paste0("DPS=", dp_w$dps)
      }

      # 3. Digit preference taille (max 20 pts)
      if (!is.null(m$height) && m$height %in% names(d)) {
        dp_h <- calc_digit_preference(d[[m$height]])
        sc <- if(is.na(dp_h$dps)) 10
              else if(dp_h$dps <= 7) 20
              else if(dp_h$dps <= 12) 15
              else if(dp_h$dps <= 20) 8
              else 0
        scores$dp_taille <- sc
        details$dp_taille <- paste0("DPS=", dp_h$dps)
      }

      # 4. Flags SMART (max 20 pts)
      z_cols <- c(".whz",".haz",".waz")
      z_present <- z_cols[z_cols %in% names(d)]
      if (length(z_present) > 0) {
        n_flags <- sum(sapply(z_present, function(z) {
          v <- d[[z]]
          sum(v < -5 | v > 5, na.rm=TRUE)
        }))
        pct_flags <- n_flags / (nrow(d) * length(z_present)) * 100
        sc <- if(pct_flags <= 2.5) 20
              else if(pct_flags <= 5) 15
              else if(pct_flags <= 7.5) 8
              else 0
        scores$flags <- sc
        details$flags <- paste0(round(pct_flags,1), "% flags")
      }

      # 5. Écart-type WHZ (max 15 pts)
      if (".whz" %in% names(d)) {
        sd_whz <- sd(d$.whz, na.rm=TRUE)
        sc <- if(is.na(sd_whz)) 7
              else if(sd_whz >= 0.8 && sd_whz <= 1.2) 15
              else if(sd_whz >= 0.7 && sd_whz <= 1.3) 10
              else if(sd_whz >= 0.6 && sd_whz <= 1.5) 5
              else 0
        scores$sd_whz <- sc
        details$sd_whz <- paste0("SD=", round(sd_whz,2))
      }

      total_possible <- sum(c(25, 20, 20, 20, 15)[seq_along(scores)])
      total_score <- sum(unlist(scores))
      pct <- round(total_score / total_possible * 100, 0)

      classification <- if(pct >= 80) list(label="EXCELLENT", color="#4dac26", icon="check-circle")
                        else if(pct >= 60) list(label="BON", color="#008DC9", icon="thumbs-up")
                        else if(pct >= 40) list(label="PROBLÉMATIQUE", color="#e88400", icon="exclamation-triangle")
                        else list(label="SÉRIEUX", color="#c9161d", icon="times-circle")

      list(score=pct, scores=scores, details=details,
           total=total_score, max=total_possible,
           classification=classification)
    }

    # ── Action: Lancer l'audit ─────────────────────────────────────────
    observeEvent(input$run_qc, {
      d <- df_f(); m <- map_cols(); req(d, m)

      withProgress(message="Audit qualité en cours...", value=0, {
        results <- list()

        # Digit preference
        incProgress(0.15, detail="Digit preference...")
        if (!is.null(m$weight) && m$weight %in% names(d))
          results$dp_weight <- calc_digit_preference(d[[m$weight]])
        if (!is.null(m$height) && m$height %in% names(d))
          results$dp_height <- calc_digit_preference(d[[m$height]])
        if (!is.null(m$muac) && m$muac %in% names(d))
          results$dp_muac <- calc_digit_preference(d[[m$muac]])
        if (!is.null(m$age) && m$age %in% names(d))
          results$dp_age <- calc_digit_preference(d[[m$age]])

        # Age heaping
        incProgress(0.15, detail="Age heaping...")
        if (!is.null(m$age) && m$age %in% names(d))
          results$age_heaping <- calc_age_heaping(d[[m$age]])

        # Score global
        incProgress(0.2, detail="Score qualité global...")
        results$quality_score <- calc_quality_score(d, m)

        # Anomalies par cluster
        incProgress(0.2, detail="Anomalies par cluster...")
        cluster_col <- m$cluster
        if (is.null(cluster_col)) {
          # Chercher automatiquement
          candidates <- grep("cluster|grappe|equipe|team|enum", names(d), ignore.case=TRUE, value=TRUE)
          if (length(candidates) > 0) cluster_col <- candidates[1]
        }
        if (!is.null(cluster_col) && cluster_col %in% names(d)) {
          results$cluster_analysis <- calc_cluster_anomalies(d, m, cluster_col)
          results$cluster_col <- cluster_col
        }

        # Complétude
        incProgress(0.15, detail="Complétude...")
        results$completeness <- calc_completeness(d, m)

        # Flags SMART
        incProgress(0.15, detail="Flags plausibilité...")
        results$flags <- calc_smart_flags(d, m)

        qc_results(results)
      })

      # Log audit
      if (is.function(audit_log)) {
        audit_log(list(
          timestamp = Sys.time(),
          action = "quality_audit",
          module = "Contrôle Qualité Auto",
          details = paste0("Score: ", results$quality_score$score, "/100 — ",
                          results$quality_score$classification$label)
        ))
      }

      showNotification(
        paste0("Audit qualité terminé — Score: ",
               results$quality_score$score, "/100"),
        type="message", duration=5
      )
    })

    # ── Fonctions cluster et complétude ─────────────────────────────────

    calc_cluster_anomalies <- function(d, m, cluster_col) {
      clusters <- unique(na.omit(d[[cluster_col]]))
      if (length(clusters) < 2) return(NULL)

      z_cols <- intersect(c(".whz",".haz",".waz"), names(d))
      if (length(z_cols) == 0) return(NULL)

      cluster_stats <- do.call(rbind, lapply(clusters, function(cl) {
        dc <- d[d[[cluster_col]] == cl & !is.na(d[[cluster_col]]),]
        if (nrow(dc) < 3) return(NULL)

        row <- data.frame(Cluster=as.character(cl), N=nrow(dc), stringsAsFactors=FALSE)

        for (z in z_cols) {
          if (z %in% names(dc)) {
            vals <- dc[[z]][!is.na(dc[[z]])]
            if (length(vals) >= 3) {
              z_label <- gsub("^\\.", "", z)
              row[[paste0(z_label, "_mean")]] <- round(mean(vals), 2)
              row[[paste0(z_label, "_sd")]] <- round(sd(vals), 2)
              row[[paste0(z_label, "_flags")]] <- sum(vals < -5 | vals > 5)
            }
          }
        }

        # Score anomalie
        anomaly_score <- 0
        if ("whz_sd" %in% names(row) && !is.na(row$whz_sd)) {
          if (row$whz_sd > 1.5 || row$whz_sd < 0.6) anomaly_score <- anomaly_score + 2
          else if (row$whz_sd > 1.3 || row$whz_sd < 0.7) anomaly_score <- anomaly_score + 1
        }
        if ("whz_mean" %in% names(row) && !is.na(row$whz_mean)) {
          if (abs(row$whz_mean) > 1.5) anomaly_score <- anomaly_score + 2
        }

        # Digit preference dans le cluster
        if (!is.null(m$weight) && m$weight %in% names(dc)) {
          dp <- calc_digit_preference(dc[[m$weight]])
          if (!is.na(dp$dps) && dp$dps > 20) anomaly_score <- anomaly_score + 2
          else if (!is.na(dp$dps) && dp$dps > 12) anomaly_score <- anomaly_score + 1
          row$DP_poids <- dp$dps
        }

        row$Anomalie <- anomaly_score
        row$Statut <- if(anomaly_score >= 4) "🔴 CRITIQUE"
                      else if(anomaly_score >= 2) "🟠 SUSPECT"
                      else "🟢 OK"
        row
      }))

      cluster_stats
    }

    calc_completeness <- function(d, m) {
      key_vars <- list(
        Poids = m$weight, Taille = m$height, Age = m$age,
        Sexe = m$sex, MUAC = m$muac, Cluster = m$cluster,
        WHZ = ".whz", HAZ = ".haz", WAZ = ".waz"
      )

      comp <- do.call(rbind, lapply(names(key_vars), function(lbl) {
        col <- key_vars[[lbl]]
        if (is.null(col) || !col %in% names(d)) {
          return(data.frame(Variable=lbl, Colonne="Non mappé", N_total=nrow(d),
                           N_valide=0, N_manquant=nrow(d), Pct_complet=0, stringsAsFactors=FALSE))
        }
        n_valid <- sum(!is.na(d[[col]]))
        data.frame(Variable=lbl, Colonne=col, N_total=nrow(d),
                   N_valide=n_valid, N_manquant=nrow(d)-n_valid,
                   Pct_complet=round(n_valid/nrow(d)*100, 1), stringsAsFactors=FALSE)
      }))
      comp
    }

    calc_smart_flags <- function(d, m) {
      flags_list <- list()

      # WHZ flags
      if (".whz" %in% names(d)) {
        whz <- d$.whz
        flags_list$WHZ <- data.frame(
          Indicateur = "WHZ",
          N = sum(!is.na(whz)),
          Flags_abs = sum(whz < -5 | whz > 5, na.rm=TRUE),
          Pct_flags = round(sum(whz < -5 | whz > 5, na.rm=TRUE) / sum(!is.na(whz)) * 100, 2),
          Mean = round(mean(whz, na.rm=TRUE), 3),
          SD = round(sd(whz, na.rm=TRUE), 3),
          stringsAsFactors=FALSE
        )
      }
      if (".haz" %in% names(d)) {
        haz <- d$.haz
        flags_list$HAZ <- data.frame(
          Indicateur = "HAZ",
          N = sum(!is.na(haz)),
          Flags_abs = sum(haz < -6 | haz > 6, na.rm=TRUE),
          Pct_flags = round(sum(haz < -6 | haz > 6, na.rm=TRUE) / sum(!is.na(haz)) * 100, 2),
          Mean = round(mean(haz, na.rm=TRUE), 3),
          SD = round(sd(haz, na.rm=TRUE), 3),
          stringsAsFactors=FALSE
        )
      }
      if (".waz" %in% names(d)) {
        waz <- d$.waz
        flags_list$WAZ <- data.frame(
          Indicateur = "WAZ",
          N = sum(!is.na(waz)),
          Flags_abs = sum(waz < -6 | waz > 5, na.rm=TRUE),
          Pct_flags = round(sum(waz < -6 | waz > 5, na.rm=TRUE) / sum(!is.na(waz)) * 100, 2),
          Mean = round(mean(waz, na.rm=TRUE), 3),
          SD = round(sd(waz, na.rm=TRUE), 3),
          stringsAsFactors=FALSE
        )
      }
      if (length(flags_list) == 0) return(NULL)
      do.call(rbind, flags_list)
    }

    # ── Outputs ─────────────────────────────────────────────────────────

    output$qc_status_badge <- renderUI({
      res <- qc_results()
      if (is.null(res)) {
        return(tags$span(style="font-size:12px;opacity:.7;", "Cliquez pour lancer l'audit"))
      }
      qs <- res$quality_score
      tags$span(
        style=paste0("background:", qs$classification$color,
                     ";padding:6px 14px;border-radius:16px;font-size:13px;font-weight:700;"),
        icon(qs$classification$icon), " ", qs$score, "/100"
      )
    })

    # Score global panel
    output$qc_score_global_panel <- renderUI({
      res <- qc_results()
      if (is.null(res)) {
        return(tags$div(
          style="text-align:center;padding:40px;background:#f5f5f5;border-radius:12px;margin-bottom:20px;color:#888;",
          tags$h4(icon("play-circle"), " Cliquez sur 'Lancer l'Audit Qualité' pour commencer"),
          tags$p("L'audit analyse automatiquement la digit preference, la cohérence des mesures,",
                 "les flags de plausibilité et les anomalies par cluster.")
        ))
      }

      qs <- res$quality_score
      s <- qs$score
      cl <- qs$classification

      tags$div(
        style=paste0("background:white;border:3px solid ", cl$color,
                     ";border-radius:14px;padding:24px;margin-bottom:20px;",
                     "box-shadow:0 4px 16px rgba(0,0,0,.08);"),
        fluidRow(
          column(3, style="text-align:center;",
            tags$div(style=paste0(
              "width:110px;height:110px;border-radius:50%;border:8px solid ", cl$color,
              ";display:inline-flex;align-items:center;justify-content:center;",
              "background:white;box-shadow:0 4px 16px rgba(0,0,0,.1);"),
              tags$div(
                tags$span(style=paste0("font-size:34px;font-weight:900;color:", cl$color, ";"), s),
                tags$br(),
                tags$span(style="font-size:11px;color:#666;", "/100")
              )
            ),
            tags$div(style=paste0("margin-top:10px;padding:4px 14px;border-radius:16px;",
                                  "background:", cl$color, ";color:white;font-weight:700;font-size:12px;",
                                  "display:inline-block;"),
                     icon(cl$icon), " ", cl$label)
          ),
          column(9,
            tags$h4(style=paste0("margin:0 0 12px;font-weight:700;color:", cl$color, ";"),
                    "Score Qualité SMART — Résultat de l'Audit"),
            # Barres de détail
            lapply(names(qs$scores), function(crit) {
              val <- qs$scores[[crit]]
              max_val <- switch(crit, completude=25, dp_poids=20, dp_taille=20, flags=20, sd_whz=15, 20)
              pct <- round(val/max_val*100, 0)
              vc <- if(pct >= 80) "#4dac26" else if(pct >= 60) "#008DC9" else if(pct >= 40) "#e88400" else "#c9161d"
              lbl <- switch(crit, completude="Complétude", dp_poids="DP Poids",
                           dp_taille="DP Taille", flags="Flags SMART", sd_whz="SD WHZ", crit)
              detail <- qs$details[[crit]]

              tags$div(style="margin:6px 0;",
                tags$div(style="display:flex;justify-content:space-between;font-size:12px;",
                  tags$span(style="font-weight:600;", lbl),
                  tags$span(style="color:#666;", paste0(val, "/", max_val, " — ", detail))
                ),
                tags$div(style="background:#e0e0e0;border-radius:6px;height:10px;overflow:hidden;",
                  tags$div(style=paste0("width:", pct, "%;height:100%;background:", vc,
                                       ";border-radius:6px;transition:width 0.8s;"))
                )
              )
            })
          )
        )
      )
    })

    # ── Digit Preference Plots ──────────────────────────────────────────

    make_dp_plot <- function(dp_result, title) {
      if (is.null(dp_result) || is.na(dp_result$dps)) {
        return(plotly_empty() |> layout(title=list(text="Données insuffisantes")))
      }

      freq_df <- dp_result$freq
      names(freq_df) <- c("Digit", "Freq")
      expected <- dp_result$n / 10
      freq_df$Expected <- expected
      freq_df$Deviation <- freq_df$Freq - expected
      freq_df$Color <- ifelse(abs(freq_df$Deviation) > expected * 0.3, "#c9161d", "#008DC9")

      p <- ggplot(freq_df, aes(x=Digit, y=Freq)) +
        geom_col(aes(fill=Color), alpha=0.85, width=0.7) +
        geom_hline(yintercept=expected, linetype="dashed", color="#c9161d", linewidth=0.8) +
        scale_fill_identity() +
        annotate("text", x=9, y=expected, label=paste0("Attendu: ", round(expected)),
                 vjust=-0.5, hjust=1, size=3.5, color="#c9161d") +
        labs(x="Dernier chiffre", y="Fréquence", title=title) +
        theme_minimal(base_size=12) +
        theme(plot.title=element_text(face="bold", color="#00426A"))

      ggplotly(p, tooltip=c("x","y")) |> layout(showlegend=FALSE)
    }

    make_dp_result_ui <- function(dp_result, label) {
      if (is.null(dp_result) || is.na(dp_result$dps)) {
        return(tags$div(style="padding:8px;color:#888;font-size:12px;",
                        "Données insuffisantes pour ce test"))
      }
      vc <- switch(dp_result$quality,
                   excellent="#4dac26", bon="#008DC9",
                   problematique="#e88400", serieux="#c9161d", "#666")
      tags$div(
        style=paste0("background:#f8f9fa;border-left:4px solid ", vc,
                     ";padding:10px 14px;border-radius:6px;margin-top:10px;"),
        tags$strong(style=paste0("color:", vc, ";"), label, " — DPS = ", dp_result$dps),
        tags$br(),
        tags$span(style="font-size:12px;",
                  "Classification: ", dp_result$classification),
        if (!is.na(dp_result$p_value))
          tags$span(style="font-size:11px;color:#666;margin-left:12px;",
                    paste0("Chi² p=", format.pval(dp_result$p_value, digits=3)))
      )
    }

    output$qc_dp_weight <- renderPlotly({
      res <- qc_results(); req(res, res$dp_weight)
      make_dp_plot(res$dp_weight, "Distribution des derniers chiffres — Poids")
    })
    output$qc_dp_weight_result <- renderUI({
      res <- qc_results(); req(res, res$dp_weight)
      make_dp_result_ui(res$dp_weight, "Poids")
    })

    output$qc_dp_height <- renderPlotly({
      res <- qc_results(); req(res, res$dp_height)
      make_dp_plot(res$dp_height, "Distribution des derniers chiffres — Taille")
    })
    output$qc_dp_height_result <- renderUI({
      res <- qc_results(); req(res, res$dp_height)
      make_dp_result_ui(res$dp_height, "Taille")
    })

    output$qc_dp_muac <- renderPlotly({
      res <- qc_results(); req(res, res$dp_muac)
      make_dp_plot(res$dp_muac, "Distribution des derniers chiffres — MUAC")
    })
    output$qc_dp_muac_result <- renderUI({
      res <- qc_results(); req(res, res$dp_muac)
      make_dp_result_ui(res$dp_muac, "MUAC")
    })

    output$qc_dp_age <- renderPlotly({
      res <- qc_results(); req(res, res$dp_age)
      make_dp_plot(res$dp_age, "Distribution des derniers chiffres — Âge")
    })
    output$qc_dp_age_result <- renderUI({
      res <- qc_results(); req(res, res$dp_age)
      make_dp_result_ui(res$dp_age, "Âge")
    })

    # ── Cohérence plots ─────────────────────────────────────────────────
    output$qc_age_height <- renderPlotly({
      d <- df_f(); m <- map_cols()
      req(d, m, m$age, m$height, m$age %in% names(d), m$height %in% names(d))

      p <- ggplot(d, aes(x=.data[[m$age]], y=.data[[m$height]])) +
        geom_point(alpha=0.3, color="#008DC9", size=1.5) +
        geom_smooth(method="loess", se=TRUE, color="#c9161d", linewidth=0.8) +
        labs(x="Âge (mois)", y="Taille (cm)", title="Cohérence Âge/Taille") +
        theme_minimal(base_size=12) +
        theme(plot.title=element_text(face="bold", color="#00426A"))

      ggplotly(p) |> layout(hovermode="closest")
    })

    output$qc_age_height_result <- renderUI({
      d <- df_f(); m <- map_cols()
      req(d, m, m$age, m$height, m$age %in% names(d), m$height %in% names(d))

      cor_val <- cor(d[[m$age]], d[[m$height]], use="pairwise.complete.obs")
      vc <- if(cor_val > 0.7) "#4dac26" else if(cor_val > 0.4) "#e88400" else "#c9161d"
      tags$div(style=paste0("border-left:4px solid ", vc, ";padding:8px 12px;background:#f8f9fa;border-radius:6px;margin-top:8px;"),
        tags$strong(paste0("Corrélation: r = ", round(cor_val, 3))),
        tags$span(style="margin-left:12px;font-size:12px;color:#666;",
                  if(cor_val > 0.7) "Cohérence satisfaisante"
                  else if(cor_val > 0.4) "Cohérence modérée — vérifier les outliers"
                  else "Faible cohérence — problème de mesure possible")
      )
    })

    output$qc_weight_height <- renderPlotly({
      d <- df_f(); m <- map_cols()
      req(d, m, m$weight, m$height, m$weight %in% names(d), m$height %in% names(d))

      p <- ggplot(d, aes(x=.data[[m$height]], y=.data[[m$weight]])) +
        geom_point(alpha=0.3, color="#e88400", size=1.5) +
        geom_smooth(method="loess", se=TRUE, color="#c9161d", linewidth=0.8) +
        labs(x="Taille (cm)", y="Poids (kg)", title="Cohérence Poids/Taille") +
        theme_minimal(base_size=12) +
        theme(plot.title=element_text(face="bold", color="#00426A"))

      ggplotly(p) |> layout(hovermode="closest")
    })

    output$qc_weight_height_result <- renderUI({
      d <- df_f(); m <- map_cols()
      req(d, m, m$weight, m$height, m$weight %in% names(d), m$height %in% names(d))
      cor_val <- cor(d[[m$weight]], d[[m$height]], use="pairwise.complete.obs")
      vc <- if(cor_val > 0.7) "#4dac26" else if(cor_val > 0.4) "#e88400" else "#c9161d"
      tags$div(style=paste0("border-left:4px solid ", vc, ";padding:8px 12px;background:#f8f9fa;border-radius:6px;margin-top:8px;"),
        tags$strong(paste0("Corrélation: r = ", round(cor_val, 3))),
        tags$span(style="margin-left:12px;font-size:12px;color:#666;",
                  if(cor_val > 0.7) "Cohérence satisfaisante"
                  else "Vérifier les mesures aberrantes")
      )
    })

    # ── Flags table ─────────────────────────────────────────────────────
    output$qc_flags_table <- renderDT({
      res <- qc_results(); req(res, res$flags)
      datatable(res$flags, rownames=FALSE,
                options=list(pageLength=10, dom="t"),
                class="compact stripe hover") |>
        formatStyle("Pct_flags",
                    backgroundColor=styleInterval(c(2.5, 5, 7.5),
                                                   c("#e8f5e9","#fffde7","#fff3e0","#ffebee")),
                    fontWeight="bold") |>
        formatStyle("SD",
                    backgroundColor=styleInterval(c(0.8, 1.2, 1.5),
                                                   c("#ffebee","#e8f5e9","#fffde7","#ffebee")))
    })

    output$qc_flags_summary <- renderUI({
      res <- qc_results(); req(res, res$flags)
      total_flags <- sum(res$flags$Flags_abs)
      total_n <- sum(res$flags$N)
      pct <- round(total_flags / total_n * 100, 2)
      vc <- if(pct <= 2.5) "#4dac26" else if(pct <= 5) "#e88400" else "#c9161d"
      tags$div(style=paste0("border-left:4px solid ", vc, ";padding:10px 14px;background:#f8f9fa;border-radius:6px;margin-top:12px;"),
        tags$strong(style=paste0("color:", vc, ";"),
                    paste0(total_flags, " flags sur ", total_n, " mesures (", pct, "%)")),
        tags$span(style="font-size:12px;color:#666;margin-left:12px;",
                  "Seuil SMART: ≤2.5% = Excellent, ≤5% = Acceptable, >7.5% = Sérieux")
      )
    })

    # ── Cluster anomalies ──────────────────────────────────────────────
    output$qc_cluster_alert <- renderUI({
      res <- qc_results()
      if (is.null(res) || is.null(res$cluster_analysis)) {
        return(tags$div(style="padding:20px;background:#fff3cd;border-radius:8px;text-align:center;color:#856404;",
          icon("info-circle"), " Pas de colonne cluster/grappe détectée. ",
          "Nommez une colonne 'cluster', 'grappe' ou 'equipe'."))
      }
      ca <- res$cluster_analysis
      n_crit <- sum(grepl("CRITIQUE", ca$Statut))
      n_susp <- sum(grepl("SUSPECT", ca$Statut))

      if (n_crit > 0) {
        tags$div(style="background:#ffebee;border-left:5px solid #c9161d;padding:12px;border-radius:8px;margin-bottom:12px;",
          tags$strong(style="color:#c9161d;",
                      icon("exclamation-circle"), paste0(" ", n_crit, " cluster(s) CRITIQUE(S) et ",
                                                          n_susp, " suspect(s) détectés")),
          tags$p(style="font-size:12px;color:#666;margin:4px 0 0;",
                 "Les clusters critiques présentent des anomalies dans la distribution des mesures, ",
                 "la digit preference ou l'écart-type. Vérifier la qualité de la collecte."))
      } else if (n_susp > 0) {
        tags$div(style="background:#fff3e0;border-left:5px solid #e88400;padding:12px;border-radius:8px;margin-bottom:12px;",
          tags$strong(style="color:#e88400;",
                      icon("exclamation-triangle"), paste0(" ", n_susp, " cluster(s) suspect(s)")))
      } else {
        tags$div(style="background:#e8f5e9;border-left:5px solid #4dac26;padding:12px;border-radius:8px;margin-bottom:12px;",
          icon("check-circle", style="color:#4dac26;"), tags$strong(style="color:#2e7d32;", " Tous les clusters sont conformes"))
      }
    })

    output$qc_cluster_table <- renderDT({
      res <- qc_results(); req(res, res$cluster_analysis)
      datatable(res$cluster_analysis, rownames=FALSE,
                options=list(pageLength=20, dom="ftp", scrollX=TRUE),
                class="compact stripe hover") |>
        formatStyle("Statut",
                    backgroundColor=styleEqual(
                      c("🔴 CRITIQUE","🟠 SUSPECT","🟢 OK"),
                      c("#ffebee","#fff3e0","#e8f5e9")),
                    fontWeight="bold") |>
        formatRound(grep("_mean|_sd|DP", names(res$cluster_analysis), value=TRUE), digits=2)
    })

    output$qc_cluster_plot <- renderPlotly({
      res <- qc_results(); req(res, res$cluster_analysis)
      ca <- res$cluster_analysis
      req("whz_sd" %in% names(ca), "whz_mean" %in% names(ca))

      p <- ggplot(ca, aes(x=whz_mean, y=whz_sd, size=N, color=Statut,
                           text=paste0("Cluster: ", Cluster, "\nN=", N,
                                       "\nWHZ mean=", whz_mean, "\nWHZ SD=", whz_sd))) +
        geom_point(alpha=0.7) +
        geom_hline(yintercept=c(0.8, 1.2), linetype="dashed", color="#4dac26", linewidth=0.5) +
        geom_hline(yintercept=c(0.6, 1.5), linetype="dotted", color="#c9161d", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="solid", color="gray50", linewidth=0.3) +
        scale_color_manual(values=c("🔴 CRITIQUE"="#c9161d", "🟠 SUSPECT"="#e88400", "🟢 OK"="#4dac26")) +
        scale_size_continuous(range=c(3, 12)) +
        labs(x="WHZ Moyen", y="WHZ Écart-type", title="Clusters: Moyenne vs SD du WHZ") +
        theme_minimal(base_size=12) +
        theme(plot.title=element_text(face="bold", color="#00426A"))

      ggplotly(p, tooltip="text") |> layout(hovermode="closest")
    })

    # ── Complétude ──────────────────────────────────────────────────────
    output$qc_completeness <- renderPlotly({
      res <- qc_results(); req(res, res$completeness)
      comp <- res$completeness
      comp$Color <- ifelse(comp$Pct_complet >= 95, "#4dac26",
                          ifelse(comp$Pct_complet >= 80, "#e88400", "#c9161d"))

      p <- ggplot(comp, aes(y=reorder(Variable, Pct_complet), x=Pct_complet,
                             fill=Color,
                             text=paste0(Variable, ": ", Pct_complet, "%\n",
                                         N_valide, "/", N_total, " valeurs"))) +
        geom_col(width=0.6, alpha=0.85) +
        geom_vline(xintercept=95, linetype="dashed", color="#4dac26", linewidth=0.7) +
        geom_vline(xintercept=80, linetype="dotted", color="#e88400", linewidth=0.5) +
        scale_fill_identity() +
        scale_x_continuous(limits=c(0,105)) +
        labs(x="Complétude (%)", y="") +
        theme_minimal(base_size=12)

      ggplotly(p, tooltip="text") |> layout(showlegend=FALSE)
    })

    output$qc_completeness_table <- renderDT({
      res <- qc_results(); req(res, res$completeness)
      datatable(res$completeness, rownames=FALSE,
                options=list(pageLength=10, dom="t"),
                class="compact stripe hover") |>
        formatStyle("Pct_complet",
                    background=styleColorBar(c(0,100), "#008DC9"),
                    backgroundSize="98% 80%", backgroundRepeat="no-repeat",
                    backgroundPosition="left")
    })

    # ── Rapport complet ─────────────────────────────────────────────────
    output$qc_full_report <- renderUI({
      res <- qc_results()
      if (is.null(res)) return(tags$p("Lancez l'audit pour générer le rapport."))

      qs <- res$quality_score
      report_sections <- list()

      # En-tête
      report_sections[[1]] <- tags$div(
        style="background:#f8f9fa;padding:20px;border-radius:10px;margin-bottom:16px;",
        tags$h4(style="color:#00426A;margin:0;", "Rapport d'Audit Qualité SMART"),
        tags$p(style="font-size:12px;color:#666;",
               paste0("Généré le ", format(Sys.time(), "%d/%m/%Y à %H:%M"), " | ",
                      "Score global: ", qs$score, "/100 — ", qs$classification$label)),
        tags$hr()
      )

      # Score détaillé
      report_sections[[2]] <- tags$div(
        style="margin-bottom:16px;",
        tags$h5(style="color:#00426A;", "1. Score de Qualité Global"),
        tags$table(
          style="width:100%;border-collapse:collapse;font-size:12px;",
          tags$thead(tags$tr(
            lapply(c("Critère","Score","Détail"), function(h)
              tags$th(style="padding:8px;background:#008DC9;color:white;text-align:left;", h))
          )),
          tags$tbody(
            lapply(names(qs$scores), function(crit) {
              tags$tr(
                tags$td(style="padding:6px;border-bottom:1px solid #eee;",
                        switch(crit, completude="Complétude variables clés", dp_poids="Digit Preference Poids",
                               dp_taille="Digit Preference Taille", flags="Flags plausibilité", sd_whz="SD WHZ", crit)),
                tags$td(style="padding:6px;border-bottom:1px solid #eee;font-weight:600;",
                        paste0(qs$scores[[crit]], "/",
                               switch(crit, completude=25, dp_poids=20, dp_taille=20, flags=20, sd_whz=15, 20))),
                tags$td(style="padding:6px;border-bottom:1px solid #eee;color:#666;",
                        qs$details[[crit]])
              )
            })
          )
        )
      )

      # Digit preference summary
      if (!is.null(res$dp_weight) || !is.null(res$dp_height)) {
        report_sections[[3]] <- tags$div(
          style="margin-bottom:16px;",
          tags$h5(style="color:#00426A;", "2. Digit Preference"),
          tags$ul(style="font-size:12px;",
            if(!is.null(res$dp_weight) && !is.na(res$dp_weight$dps))
              tags$li(paste0("Poids: DPS=", res$dp_weight$dps, " — ", res$dp_weight$classification)),
            if(!is.null(res$dp_height) && !is.na(res$dp_height$dps))
              tags$li(paste0("Taille: DPS=", res$dp_height$dps, " — ", res$dp_height$classification)),
            if(!is.null(res$dp_muac) && !is.na(res$dp_muac$dps))
              tags$li(paste0("MUAC: DPS=", res$dp_muac$dps, " — ", res$dp_muac$classification)),
            if(!is.null(res$dp_age) && !is.na(res$dp_age$dps))
              tags$li(paste0("Âge: DPS=", res$dp_age$dps, " — ", res$dp_age$classification))
          )
        )
      }

      tagList(report_sections)
    })

    # Download rapport
    output$qc_download_report <- downloadHandler(
      filename = function() {
        paste0("Rapport_Qualite_SMART_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        res <- qc_results(); req(res)
        qs <- res$quality_score

        # Construire un CSV de synthèse
        report_df <- data.frame(
          Critere = names(qs$scores),
          Score = unlist(qs$scores),
          Detail = unlist(qs$details),
          stringsAsFactors=FALSE
        )
        report_df <- rbind(report_df,
          data.frame(Critere="TOTAL", Score=qs$score,
                     Detail=qs$classification$label))

        write.csv(report_df, file, row.names=FALSE)
      }
    )

  })
}

cat("mod_qualite_auto.R chargé — Contrôle Qualité Automatisé Expert\n")
