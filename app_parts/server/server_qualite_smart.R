# Extrait de app_v2_final.R — contrôle qualité SMART / plausibility check
  # ══════════════════════════════════════════════════════════════════
  # QUALITÉ SMART — Plausibility Check (style ENA)
  # ══════════════════════════════════════════════════════════════════

  # Helper : chercher les colonnes probables pour chaque variable
  guess_col <- function(d, patterns, type = "num") {
    nms <- names(d)
    for (pat in patterns) {
      matches <- grep(pat, nms, ignore.case = TRUE, value = TRUE)
      if (length(matches) == 0) next
      if (type == "num") matches <- matches[vapply(matches, function(v) is.numeric(d[[v]]), logical(1))]
      if (type == "cat") matches <- matches[vapply(matches, function(v) !is.numeric(d[[v]]) || length(unique(na.omit(d[[v]]))) <= 30, logical(1))]
      if (length(matches) > 0) return(matches[1])
    }
    NULL
  }

  # UI dynamiques pour les sélecteurs de variables
  output$sq_col_whz_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^whz$","^WHZ$","wfhz","zscore.*wh","z.*wh","_whz","whz_"))
    sel <- guess %||% (if (any(grepl("whz|WHZ", vn, ignore.case=TRUE))) grep("whz|WHZ", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_whz", "WHZ (poids-taille) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_haz_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^haz$","^HAZ$","hfaz","zscore.*ha","z.*ha","_haz","haz_"))
    sel <- guess %||% (if (any(grepl("haz|HAZ", vn, ignore.case=TRUE))) grep("haz|HAZ", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_haz", "HAZ (taille-âge) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_waz_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^waz$","^WAZ$","wfaz","zscore.*wa","z.*wa","_waz","waz_"))
    sel <- guess %||% (if (any(grepl("waz|WAZ", vn, ignore.case=TRUE))) grep("waz|WAZ", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_waz", "WAZ (poids-âge) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_muac_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^muac$","^MUAC$","pb","perim.*bra","mid.*arm"))
    sel <- guess %||% (if (any(grepl("muac|MUAC|pb|PB", vn, ignore.case=TRUE))) grep("muac|MUAC|pb|PB", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_muac", "MUAC (mm) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })

  output$sq_col_sex_ui <- renderUI({
    d <- df_f(); vc <- c(vars_cat(), vars_num())
    guess <- guess_col(d, c("^sex","^genre","^gender"), type = "cat")
    sel <- guess %||% ""
    selectInput("sq_col_sex", "Sexe :",
                choices = c("— Aucun —" = "", vc), selected = sel, width = "100%")
  })
  output$sq_col_age_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^age.*mois","^age.*month","^age_m","^agemois","^age$"))
    sel <- guess %||% (if (any(grepl("age", vn, ignore.case=TRUE))) grep("age", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_age", "Âge (mois) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_weight_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^poids$","^weight$","^wt$","^pds$"))
    sel <- guess %||% (if (any(grepl("poids|weight|wt", vn, ignore.case=TRUE))) grep("poids|weight|wt", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_weight", "Poids (kg) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_height_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    guess <- guess_col(d, c("^taille$","^height$","^ht$","^longueur$"))
    sel <- guess %||% (if (any(grepl("taille|height|ht", vn, ignore.case=TRUE))) grep("taille|height|ht", vn, ignore.case=TRUE, value=TRUE)[1] else "")
    selectInput("sq_col_height", "Taille (cm) :",
                choices = c("— Aucun —" = "", vn), selected = sel, width = "100%")
  })
  output$sq_col_team_ui <- renderUI({
    d <- df_f(); vc <- c(vars_cat(), vars_num())
    guess <- guess_col(d, c("^equipe","^team","^enqueteur","^superviseur","^cluster"), type = "cat")
    sel <- guess %||% ""
    selectInput("sq_col_team", "Équipe / Enquêteur :",
                choices = c("— Aucun —" = "", vc), selected = sel, width = "100%")
  })

  # Reactive stores
  sq_results <- reactiveValues(
    plaus = NULL, flags = NULL, flags_summary = NULL,
    dp = NULL, age_heap = NULL, sd_team = NULL, completude = NULL
  )

  # Sélecteur digit preference
  output$sq_dp_var_ui <- renderUI({
    vn <- vars_num()
    selectInput("sq_dp_var", "Variable pour digit preference :",
                choices = vn, selected = vn[1], width = "100%")
  })

  # Lancer le contrôle qualité
  observeEvent(input$run_qualite, {
    d <- df_f(); req(nrow(d) > 0)

    # Récupérer les colonnes (chaîne vide → NULL)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_whz    <- nullify(input$sq_col_whz)
    col_haz    <- nullify(input$sq_col_haz)
    col_waz    <- nullify(input$sq_col_waz)
    col_muac   <- nullify(input$sq_col_muac)
    col_sex    <- nullify(input$sq_col_sex)
    col_age    <- nullify(input$sq_col_age)
    col_weight <- nullify(input$sq_col_weight)
    col_height <- nullify(input$sq_col_height)
    col_team   <- nullify(input$sq_col_team)

    withProgress(message = "Contrôle qualité SMART en cours...", value = 0, {

      # 1. Flags
      incProgress(0.15, detail = "SMART Flags...")
      sq_results$flags <- detect_smart_flags(d, col_whz, col_haz, col_waz, col_muac)
      sq_results$flags_summary <- summarise_flags(sq_results$flags, nrow(d))

      # 2. Score de plausibilité
      incProgress(0.25, detail = "Score de plausibilité...")
      sq_results$plaus <- calc_plausibility_score(
        d, col_whz, col_haz, col_waz, col_muac,
        col_sex, col_age, col_weight, col_height, col_team
      )

      # 3. Age heaping
      incProgress(0.15, detail = "Age heaping...")
      sq_results$age_heap <- if (!is.null(col_age)) calc_age_heaping(d[[col_age]]) else NULL

      # 4. SD par équipe
      incProgress(0.15, detail = "SD par équipe...")
      sq_results$sd_team <- calc_sd_by_team(d, col_whz %||% col_haz, col_team)

      # 5. Complétude
      incProgress(0.15, detail = "Complétude...")
      vars_cles <- c(col_whz, col_haz, col_waz, col_muac, col_sex, col_age,
                     col_weight, col_height, col_team)
      vars_cles <- vars_cles[!sapply(vars_cles, is.null)]
      sq_results$completude <- calc_completude(d, vars_cles)

      incProgress(0.15, detail = "Terminé ✅")
    })
  })

  # ── Rendu : Score de plausibilité ──
  output$sq_plausibility_html <- renderUI({
    req(sq_results$plaus)
    render_plausibility_html(sq_results$plaus)
  })

  # ── Rendu : SMART Flags table ──
  output$sq_flags_table <- renderDT({
    req(sq_results$flags_summary)
    datatable(sq_results$flags_summary, rownames = FALSE,
              options = list(dom = "t", pageLength = 10),
              class = "compact stripe hover") |>
      formatStyle("Qualité",
                  backgroundColor = styleEqual(
                    c("🟢 Excellent", "🟡 Acceptable", "🟠 Problématique", "🔴 Sérieux"),
                    c("#f0fdf4", "#fefce8", "#fff7ed", "#fef2f2")),
                  fontWeight = "bold")
  })

  # ── Rendu : Flags plot ──
  output$sq_flags_plot <- renderPlotly({
    req(sq_results$flags)
    d <- df_f()
    col_whz <- if (!is.null(input$sq_col_whz) && input$sq_col_whz != "") input$sq_col_whz else NULL
    if (!is.null(col_whz)) {
      plot_whz_flags(d, col_whz) |> nutri_config()
    } else {
      plotly_empty() |> layout(title = list(text = "Sélectionnez la colonne WHZ", x = .5))
    }
  })

  # ── Rendu : Digit Preference ──
  output$sq_dp_table <- renderDT({
    req(input$sq_dp_var)
    d <- df_f()
    dp <- calc_digit_preference(d[[input$sq_dp_var]], input$sq_dp_var)
    sq_results$dp <- dp
    req(dp$freq)
    datatable(dp$freq, rownames = FALSE,
              options = list(dom = "t", pageLength = 10),
              class = "compact stripe",
              caption = paste0("DPS = ", dp$dps, " — ", dp$quality))
  })

  output$sq_dp_plot <- renderPlotly({
    req(input$sq_dp_var)
    d <- df_f()
    dp <- calc_digit_preference(d[[input$sq_dp_var]], input$sq_dp_var)
    plot_digit_preference(dp) |> nutri_config()
  })

  # ── Rendu : Age Heaping ──
  output$sq_age_summary <- renderUI({
    ah <- sq_results$age_heap
    if (is.null(ah)) {
      return(tags$p(style = "color:#888;padding:20px;",
                    "Sélectionnez la colonne Âge (mois) et lancez le contrôle"))
    }
    tags$div(style = "padding:10px;",
      tags$div(style = "background:#f0f8ff;border-radius:8px;padding:14px;margin-bottom:12px;",
        tags$div(style = "font-size:11px;text-transform:uppercase;color:#888;", "RATIO AGE HEAPING"),
        tags$div(style = "font-size:32px;font-weight:800;color:#0B3954;", ah$index %||% "N/A"),
        tags$div(style = "font-size:12px;color:#555;", ah$quality %||% ""),
        tags$div(style = "font-size:11px;color:#888;margin-top:6px;",
                 paste0(ah$pct_round %||% "?", "% sur multiples de 6 (attendu : ",
                        ah$expected_pct %||% "?", "%)"))
      ),
      tags$div(style = "font-size:11px;color:#666;line-height:1.7;",
        tags$b("Interprétation :"), tags$br(),
        "Ratio ≤ 1.3 : Excellent", tags$br(),
        "Ratio 1.3-2.0 : Acceptable", tags$br(),
        "Ratio 2.0-3.0 : Problématique", tags$br(),
        "Ratio > 3.0 : Sérieux"
      )
    )
  })

  output$sq_age_plot <- renderPlotly({
    d <- df_f()
    col_age <- if (!is.null(input$sq_col_age) && input$sq_col_age != "") input$sq_col_age else NULL
    if (!is.null(col_age)) {
      plot_age_distribution(d, col_age) |> nutri_config()
    } else {
      plotly_empty() |> layout(title = list(text = "Sélectionnez la colonne Âge", x = .5))
    }
  })

  # ── Rendu : SD par équipe ──
  output$sq_team_table <- renderDT({
    req(sq_results$sd_team)
    datatable(sq_results$sd_team$table, rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe hover",
              caption = paste0("SD global = ", sq_results$sd_team$sd_global,
                               " — ", sq_results$sd_team$quality)) |>
      formatStyle("Qualité",
                  backgroundColor = styleEqual(
                    c("🟢 OK", "🟡 Attention", "🔴 Problème"),
                    c("#f0fdf4", "#fefce8", "#fef2f2")),
                  fontWeight = "bold")
  })

  output$sq_team_plot <- renderPlotly({
    req(sq_results$sd_team)
    plot_sd_teams(sq_results$sd_team,
                  if (!is.null(input$sq_col_whz) && input$sq_col_whz != "") "WHZ" else "Z-score") |>
      nutri_config()
  })

  # ── Rendu : Complétude ──
  output$sq_completude_table <- renderDT({
    req(sq_results$completude)
    datatable(sq_results$completude, rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe hover") |>
      formatStyle("% complet",
                  background = styleColorBar(c(0, 100), "#0077B6"),
                  backgroundSize = "100% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Qualité",
                  backgroundColor = styleEqual(
                    c("🟢 Excellent", "🟡 Acceptable", "🟠 Problématique", "🔴 Sérieux"),
                    c("#f0fdf4", "#fefce8", "#fff7ed", "#fef2f2")),
                  fontWeight = "bold")
  })

