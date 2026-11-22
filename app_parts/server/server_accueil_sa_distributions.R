# Extrait de app_v2_final.R — dashboard accueil — distributions rCSI / HDDS / FCS×rCSI
  # ── SÉCURITÉ ALIMENTAIRE — rangée conditionnelle rCSI + HDDS ────────────────
  output$accueil_sa_charts_row <- renderUI({
    d <- filter_context()$data; m <- map_cols()
    # Diagnostic SA
    message("[SA_DIAG] nrow=", nrow(d),
            " | .fcs=", ".fcs" %in% names(d),
            " | .rcsi=", ".rcsi" %in% names(d),
            " | .hdds=", ".hdds" %in% names(d),
            " | fcs_cat=", "fcs_cat" %in% names(d),
            " | rcsi_cat=", "rcsi_cat" %in% names(d))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    has_hdds <- ".hdds" %in% names(d) && !all(is.na(d$.hdds))
    if (!has_rcsi && !has_hdds) {
      message("[SA_DIAG] Returning NULL — has_rcsi=", has_rcsi, " has_hdds=", has_hdds)
      return(NULL)
    }

    # ── Construire la liste des variables de regroupement disponibles ──────────
    grp_choices <- c()
    add_grp <- function(col, label) {
      if (!is.null(col) && col %in% names(d) &&
          length(unique(na.omit(d[[col]]))) >= 2 &&
          length(unique(na.omit(d[[col]]))) <= 50)
        grp_choices[label] <<- col
    }
    add_grp(m$region,  paste0("\U0001f5fa\ufe0f  R\u00e9gion  (", m$region, ")"))
    add_grp(m$dept,    paste0("\U0001f4cd  D\u00e9partement  (", m$dept, ")"))
    add_grp(m$survey,  paste0("\U0001f504  Round / Vague  (", m$survey, ")"))
    add_grp(m$sexe,    paste0("\U0001f464  Sexe  (", m$sexe, ")"))
    add_grp(m$depl,    paste0("\U0001f3d5\ufe0f  D\u00e9placement  (", m$depl, ")"))
    add_grp(m$strate,  paste0("\U0001f5c2\ufe0f  Strate  (", m$strate, ")"))
    add_grp(m$cluster, paste0("\U0001f3af  Cluster  (", m$cluster, ")"))
    # Ajouter d'autres catégorielles détectées non encore incluses (max 5 suppl.)
    extra_cats <- setdiff(vars_cat(), unname(grp_choices))
    extra_cats <- extra_cats[!startsWith(extra_cats, ".")]
    for (col in head(extra_cats, 5))
      grp_choices[col] <- col
    if (length(grp_choices) == 0) return(NULL)

    # S\u00e9lection par d\u00e9faut : r\u00e9gion en premier si disponible
    default_grp <- grp_choices[1]
    current_sel <- isolate(input$sa_grp_var)
    sel <- if (!is.null(current_sel) && current_sel %in% grp_choices) current_sel else default_grp

    charts <- list()
    width  <- if (has_rcsi && has_hdds) 6L else 12L
    if (has_rcsi) {
      charts <- c(charts, list(
        column(width,
          tags$div(class="nutri-chart-card",
            tags$div(
              style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:6px; margin-bottom:6px;",
              tags$div(class="chart-title", style="margin-bottom:0;",
                       icon("hand-holding-heart"),
                       " Distribution rCSI \u2014 Strat\u00e9gies d'Adaptation (PAM/WFP)"),
              downloadButton(
                "dl_rcsi_csv",
                label = " T\u00e9l\u00e9charger CSV",
                icon  = icon("download"),
                style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                class = "btn-sm btn-outline-secondary"
              )
            ),
            plotlyOutput("accueil_rcsi_chart", height="320px")
          )
        )
      ))
    }
    if (has_hdds) {
      charts <- c(charts, list(
        column(width,
          tags$div(class="nutri-chart-card",
            tags$div(
              style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:6px; margin-bottom:6px;",
              tags$div(class="chart-title", style="margin-bottom:0;",
                       icon("carrot"),
                       " Distribution HDDS \u2014 Diversit\u00e9 Alimentaire (FAO/FANTA)"),
              downloadButton(
                "dl_hdds_csv",
                label = " T\u00e9l\u00e9charger CSV",
                icon  = icon("download"),
                style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                class = "btn-sm btn-outline-secondary"
              )
            ),
            plotlyOutput("accueil_hdds_chart", height="320px")
          )
        )
      ))
    }

    tagList(
      # ── En-t\u00eate de section avec le s\u00e9lecteur inline ────────────────────────────
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("utensils"),
                  " S\u00e9curit\u00e9 alimentaire \u2014 Distributions par groupe"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; font-size:13px; font-weight:400;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("layer-group"), " Regrouper par"
          ),
          tags$div(
            style = "min-width:260px;",
            selectInput(
              inputId  = "sa_grp_var",
              label    = NULL,
              choices  = grp_choices,
              selected = sel,
              width    = "100%"
            )
          )
        )
      ),
      fluidRow(charts)
    )
  })

  # ── rCSI : distribution des catégories par groupe ───────────────────────────
  output$accueil_rcsi_chart <- renderPlotly({
    d <- df_f(); m <- map_cols()
    req(".rcsi" %in% names(d), "rcsi_cat" %in% names(d))
    req(!is.null(input$sa_grp_var), input$sa_grp_var %in% names(d))
    grp_col <- input$sa_grp_var

    cat_lvls <- c("Pas/Peu de coping", "Coping moyen", "Coping \u00e9lev\u00e9 (\u226519)")
    pal_rcsi  <- c("Pas/Peu de coping" = C$vert,
                   "Coping moyen"      = C$orange,
                   "Coping \u00e9lev\u00e9 (\u226519)" = C$rouge)

    res <- d |>
      filter(!is.na(rcsi_cat), !is.na(.data[[grp_col]])) |>
      mutate(rcsi_cat = factor(rcsi_cat, levels = cat_lvls)) |>
      count(.data[[grp_col]], rcsi_cat, name = "n") |>
      group_by(.data[[grp_col]]) |>
      mutate(pct = round(n / sum(n) * 100, 1),
             total = sum(n)) |>
      ungroup()

    n_grp <- length(unique(res[[grp_col]]))
    # Barres horizontales si trop de groupes
    use_horiz <- n_grp > 8
    res[[grp_col]] <- truncate_label(res[[grp_col]], if (use_horiz) 25 else 18)

    p <- if (use_horiz) {
      ggplot(res,
             aes(y = .data[[grp_col]], x = pct, fill = rcsi_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               rcsi_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        geom_vline(xintercept = 50, linetype = "dashed",
                   color = C$rouge, linewidth = 0.7, alpha = 0.7) +
        scale_fill_manual(values = pal_rcsi, name = "Niveau coping") +
        scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(y = "", x = "% m\u00e9nages",
             caption = "Tiret rouge = 50% \u2014 Score rCSI \u226519 = coping \u00e9lev\u00e9 (PAM)") +
        theme_nutri()
    } else {
      ggplot(res,
             aes(x = .data[[grp_col]], y = pct, fill = rcsi_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               rcsi_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        geom_hline(yintercept = 50, linetype = "dashed",
                   color = C$rouge, linewidth = 0.7, alpha = 0.7) +
        scale_fill_manual(values = pal_rcsi, name = "Niveau coping") +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(x = "", y = "% m\u00e9nages",
             caption = "Tiret rouge = 50% \u2014 Score rCSI \u226519 = coping \u00e9lev\u00e9 (PAM)") +
        theme_nutri() + smart_axis_x(n_grp)
    }

    ggplotly(p, tooltip = "text") |>
      layout(barmode  = "stack",
             legend   = list(orientation = "h", y = -0.28, x = 0.05),
             hovermode = "closest") |>
      nutri_config()
  })

  # ── HDDS : distribution des catégories par groupe ───────────────────────────
  output$accueil_hdds_chart <- renderPlotly({
    d <- df_f(); m <- map_cols()
    req(".hdds" %in% names(d), "hdds_cat" %in% names(d))
    req(!is.null(input$sa_grp_var), input$sa_grp_var %in% names(d))
    grp_col <- input$sa_grp_var

    cat_lvls <- c("Faible diversit\u00e9 (\u22643)",
                  "Diversit\u00e9 moyenne (4-5)",
                  "Bonne diversit\u00e9 (\u22656)")
    pal_hdds <- c("Faible diversit\u00e9 (\u22643)"   = C$rouge,
                  "Diversit\u00e9 moyenne (4-5)" = C$orange,
                  "Bonne diversit\u00e9 (\u22656)"    = C$vert)

    res <- d |>
      filter(!is.na(hdds_cat), !is.na(.data[[grp_col]])) |>
      mutate(hdds_cat = factor(hdds_cat, levels = cat_lvls)) |>
      count(.data[[grp_col]], hdds_cat, name = "n") |>
      group_by(.data[[grp_col]]) |>
      mutate(pct = round(n / sum(n) * 100, 1), total = sum(n)) |>
      ungroup()

    n_grp <- length(unique(res[[grp_col]]))
    use_horiz <- n_grp > 8
    res[[grp_col]] <- truncate_label(res[[grp_col]], if (use_horiz) 25 else 18)

    p <- if (use_horiz) {
      ggplot(res,
             aes(y = .data[[grp_col]], x = pct, fill = hdds_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               hdds_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        scale_fill_manual(values = pal_hdds, name = "Diversit\u00e9 alimentaire") +
        scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(y = "", x = "% m\u00e9nages",
             caption = "HDDS : 12 groupes \u2014 Seuils FAO/FANTA : \u22643 faible, 4-5 moyen, \u22656 bonne") +
        theme_nutri()
    } else {
      ggplot(res,
             aes(x = .data[[grp_col]], y = pct, fill = hdds_cat,
                 text = paste0(.data[[grp_col]], "\n",
                               hdds_cat, " : ", pct, "%  (n=", n, "/", total, ")"))) +
        geom_col(position = "stack", width = 0.72, alpha = 0.9) +
        scale_fill_manual(values = pal_hdds, name = "Diversit\u00e9 alimentaire") +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100.5)) +
        labs(x = "", y = "% m\u00e9nages",
             caption = "HDDS : 12 groupes \u2014 Seuils FAO/FANTA : \u22643 faible, 4-5 moyen, \u22656 bonne") +
        theme_nutri() + smart_axis_x(n_grp)
    }

    ggplotly(p, tooltip = "text") |>
      layout(barmode  = "stack",
             legend   = list(orientation = "h", y = -0.28, x = 0.05),
             hovermode = "closest") |>
      nutri_config()
  })


  # ── Export CSV : rCSI distribution par groupe ───────────────────────────────
  output$dl_rcsi_csv <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$sa_grp_var)) input$sa_grp_var else "groupe"
      paste0("rCSI_distribution_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d <- df_f()
      req(".rcsi" %in% names(d), "rcsi_cat" %in% names(d))
      grp_col <- req(input$sa_grp_var)
      cat_lvls <- c("Pas/Peu de coping", "Coping moyen", "Coping \u00e9lev\u00e9 (\u226519)")
      out <- d |>
        filter(!is.na(rcsi_cat), !is.na(.data[[grp_col]])) |>
        mutate(rcsi_cat = factor(rcsi_cat, levels = cat_lvls)) |>
        count(.data[[grp_col]], rcsi_cat, name = "n") |>
        group_by(.data[[grp_col]]) |>
        mutate(
          pct    = round(n / sum(n) * 100, 1),
          total  = sum(n)
        ) |>
        ungroup() |>
        rename(groupe = all_of(grp_col), categorie_rcsi = rcsi_cat,
               effectif = n, pourcentage = pct, total_groupe = total) |>
        arrange(groupe, categorie_rcsi)
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )


  # ── Export CSV : HDDS distribution par groupe ────────────────────────────────
  output$dl_hdds_csv <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$sa_grp_var)) input$sa_grp_var else "groupe"
      paste0("HDDS_distribution_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d <- df_f()
      req(".hdds" %in% names(d), "hdds_cat" %in% names(d))
      grp_col <- req(input$sa_grp_var)
      cat_lvls <- c("Faible diversit\u00e9 (\u22643)",
                    "Diversit\u00e9 moyenne (4-5)",
                    "Bonne diversit\u00e9 (\u22656)")
      out <- d |>
        filter(!is.na(hdds_cat), !is.na(.data[[grp_col]])) |>
        mutate(hdds_cat = factor(hdds_cat, levels = cat_lvls)) |>
        count(.data[[grp_col]], hdds_cat, name = "n") |>
        group_by(.data[[grp_col]]) |>
        mutate(
          pct    = round(n / sum(n) * 100, 1),
          total  = sum(n)
        ) |>
        ungroup() |>
        rename(groupe = all_of(grp_col), categorie_hdds = hdds_cat,
               effectif = n, pourcentage = pct, total_groupe = total) |>
        arrange(groupe, categorie_hdds)
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── FCS × rCSI : UI section (conditionnel) ──────────────────────────────────
  output$accueil_fcs_rcsi_scatter <- renderUI({
    d <- df_f(); m <- map_cols()
    has_fcs  <- ".fcs"  %in% names(d) && !all(is.na(d$.fcs))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    if (!has_fcs || !has_rcsi) {
      message("[SCATTER_DIAG] Returning NULL — has_fcs=", has_fcs, " has_rcsi=", has_rcsi)
      return(NULL)
    }

    # Construire les choix de colorisation
    col_choices <- c("Niveau coping rCSI" = "rcsi_cat")
    if ("fcs_cat" %in% names(d))
      col_choices["Cat\u00e9gorie FCS"] <- "fcs_cat"
    for (nm in c("region", "dept", "sexe", "survey", "depl", "strate")) {
      col <- m[[nm]]
      if (!is.null(col) && col %in% names(d) &&
          length(unique(na.omit(d[[col]]))) >= 2 &&
          length(unique(na.omit(d[[col]]))) <= 30)
        col_choices[paste0("\U0001f5fa\ufe0f  ", col)] <- col
    }

    tagList(
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("project-diagram"),
                  " Coh\u00e9rence FCS \u00d7 rCSI \u2014 Nuage de points individuels"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; font-size:13px; font-weight:400;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("palette"), " Colorer par"
          ),
          tags$div(
            style = "min-width:220px;",
            selectInput("fcs_rcsi_color_var", label = NULL,
                        choices = col_choices, selected = col_choices[1], width = "100%")
          ),
          downloadButton(
            "dl_fcs_rcsi_csv",
            label = " T\u00e9l\u00e9charger CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      fluidRow(
        column(12,
          tags$div(class = "nutri-chart-card",
            plotlyOutput("accueil_fcs_rcsi_plot", height = "440px")
          )
        )
      )
    )
  })

  # ── FCS × rCSI : nuage de points ────────────────────────────────────────────
  output$accueil_fcs_rcsi_plot <- renderPlotly({
    d <- df_f()
    req(".fcs" %in% names(d), ".rcsi" %in% names(d))
    req(!is.null(input$fcs_rcsi_color_var),
        input$fcs_rcsi_color_var %in% names(d))

    color_var <- input$fcs_rcsi_color_var
    pal_rcsi  <- c("Pas/Peu de coping"        = C$vert,
                   "Coping moyen"             = C$orange,
                   "Coping \u00e9lev\u00e9 (\u226519)" = C$rouge)
    pal_fcs   <- c("Pauvre"        = C$rouge,
                   "Limite"        = C$orange,
                   "Acceptable"    = C$vert)

    # Sous-ensemble utilisable
    pd <- d |>
      filter(!is.na(.fcs), !is.na(.rcsi), !is.na(.data[[color_var]])) |>
      mutate(
        .color_lbl = as.character(.data[[color_var]]),
        .tooltip = paste0(
          "FCS : ", round(.fcs, 1), "\n",
          "rCSI : ", round(.rcsi, 1), "\n",
          color_var, " : ", .data[[color_var]]
        )
      )

    req(nrow(pd) >= 5)

    # Corrélation de Pearson
    cor_val <- cor(pd$.fcs, pd$.rcsi, use = "complete.obs")
    cor_lbl <- paste0("r = ", round(cor_val, 2),
                      " (", if (cor_val < 0) "relation attendue \u2212" else "relation inattendue +", ")")

    # Palette dynamique selon variable
    use_fixed_pal <- color_var %in% c("rcsi_cat", "fcs_cat")
    fixed_pal <- if (color_var == "rcsi_cat") pal_rcsi else pal_fcs

    p <- ggplot(pd, aes(x = .fcs, y = .rcsi, color = .color_lbl, text = .tooltip)) +
      # Zones FCS en arrière-plan
      annotate("rect", xmin = -Inf, xmax = 21,   ymin = -Inf, ymax = Inf,
               fill = C$rouge,  alpha = 0.04) +
      annotate("rect", xmin = 21,   xmax = 35,   ymin = -Inf, ymax = Inf,
               fill = C$orange, alpha = 0.04) +
      annotate("rect", xmin = 35,   xmax = Inf,  ymin = -Inf, ymax = Inf,
               fill = C$vert,   alpha = 0.04) +
      # Seuils de référence
      geom_vline(xintercept = 21, linetype = "dashed",
                 color = C$rouge,  linewidth = 0.65, alpha = 0.6) +
      geom_vline(xintercept = 35, linetype = "dashed",
                 color = C$orange, linewidth = 0.65, alpha = 0.6) +
      geom_hline(yintercept = 19, linetype = "dashed",
                 color = C$rouge,  linewidth = 0.65, alpha = 0.6) +
      # Points
      geom_point(size = 2, alpha = 0.6, shape = 16) +
      # Ligne de tendance (lm simple)
      geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                  color = "#334155", fill = "#94a3b8",
                  linewidth = 0.8, alpha = 0.15,
                  show.legend = FALSE) +
      # Annotations seuils
      annotate("text", x = 10,  y = max(pd$.rcsi, na.rm=TRUE) * 0.97,
               label = "FCS pauvre", color = C$rouge, size = 3, hjust = 0.5, fontface = "bold") +
      annotate("text", x = 28,  y = max(pd$.rcsi, na.rm=TRUE) * 0.97,
               label = "FCS limite", color = C$orange, size = 3, hjust = 0.5, fontface = "bold") +
      annotate("text", x = 60,  y = max(pd$.rcsi, na.rm=TRUE) * 0.97,
               label = "FCS acceptable", color = C$vert, size = 3, hjust = 0.5, fontface = "bold") +
      annotate("text", x = max(pd$.fcs, na.rm=TRUE) * 0.97, y = 20.5,
               label = "rCSI \u226519", color = C$rouge, size = 2.8, hjust = 1) +
      labs(
        x       = "Score FCS (Food Consumption Score)",
        y       = "Score rCSI (Reduced Coping Strategy Index)",
        color   = color_var,
        caption = paste0(cor_lbl,
                         "  \u2014  Tirets : seuils FCS (21/35) et rCSI (19)  \u2014  Bande grise : IC 95% r\u00e9gression")
      ) +
      theme_nutri() +
      theme(legend.position = "bottom",
            legend.title     = element_text(size = 10, face = "bold"),
            legend.text      = element_text(size = 9))

    # Palette
    if (use_fixed_pal) {
      p <- p + scale_color_manual(values = fixed_pal)
    } else {
      n_col <- length(unique(pd$.color_lbl))
      p <- p + scale_color_manual(
        values = setNames(
          colorRampPalette(c("#0077B6", "#00838F", "#D98B14",
                             "#BF3636", "#6B3FA0", "#0B3954"))(n_col),
          unique(pd$.color_lbl)
        )
      )
    }

    ggplotly(p, tooltip = "text") |>
      layout(
        legend     = list(orientation = "h", y = -0.22, x = 0),
        hovermode  = "closest",
        annotations = list(list(
          x = 0.01, y = 0.99, xref = "paper", yref = "paper",
          text = paste0("<b>", cor_lbl, "</b>"),
          showarrow = FALSE,
          font = list(size = 12, color = "#334155"),
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#cbd5e1",
          borderwidth = 1,
          borderpad   = 4
        ))
      ) |>
      nutri_config()
  })


  # ── Export CSV : FCS × rCSI données individuelles ────────────────────────────
  output$dl_fcs_rcsi_csv <- downloadHandler(
    filename = function() {
      paste0("FCS_rCSI_coherence_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d <- df_f(); m <- map_cols()
      req(".fcs" %in% names(d), ".rcsi" %in% names(d))
      # Colonnes à exporter : scores + catégories + variables de groupe
      keep_cols <- intersect(
        c(".fcs", ".rcsi", "fcs_cat", "rcsi_cat",
          m$region, m$dept, m$sexe, m$survey, m$cluster, m$strate),
        names(d)
      )
      out <- d[keep_cols] |>
        filter(!is.na(.fcs), !is.na(.rcsi)) |>
        rename(fcs_score = .fcs, rcsi_score = .rcsi)
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Analyse temporelle : helper tri intelligent des rounds ──────────────────
  sort_rounds <- function(rounds) {
    nums <- suppressWarnings(as.numeric(gsub("[^0-9\\.]+", "", rounds)))
    if (all(!is.na(nums))) rounds[order(nums)] else sort(unique(rounds))
  }

  # ── Analyse temporelle : UI section (conditionnel) ──────────────────────────
  output$accueil_temporal_row <- renderUI({
    d  <- mapped_data(); req(d)
    m  <- map_cols()
    sv_col <- m$survey
    req(!is.null(sv_col), sv_col %in% names(d))

    rounds <- unique(na.omit(as.character(d[[sv_col]])))
    if (length(rounds) < 2) return(NULL)

    has_fcs  <- ".fcs"  %in% names(d) && !all(is.na(d$.fcs))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    if (!has_fcs && !has_rcsi) return(NULL)

    # Choix de ventilation par groupe
    grp_choices <- c("Tous les m\u00e9nages" = "__global__")
    add_g <- function(col, lbl) {
      if (!is.null(col) && col %in% names(d) &&
          dplyr::n_distinct(na.omit(d[[col]])) >= 2 &&
          dplyr::n_distinct(na.omit(d[[col]])) <= 20)
        grp_choices[lbl] <<- col
    }
    add_g(m$region, paste0("\U0001f5fa\ufe0f  R\u00e9gion  (", m$region, ")"))
    add_g(m$dept,   paste0("\U0001f4cd  D\u00e9partement  (", m$dept, ")"))
    add_g(m$sexe,   paste0("\U0001f464  Sexe  (", m$sexe, ")"))
    add_g(m$depl,   paste0("\U0001f3d5\ufe0f  D\u00e9placement  (", m$depl, ")"))
    add_g(m$strate, paste0("\U0001f5c2\ufe0f  Strate  (", m$strate, ")"))

    current_sel <- isolate(input$temporal_grp_var)
    sel <- if (!is.null(current_sel) && current_sel %in% grp_choices) current_sel else "__global__"

    tagList(
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("chart-line"),
                  " \u00c9volution temporelle \u2014 FCS & rCSI par round de collecte"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; font-size:13px; font-weight:400; flex-wrap:wrap;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("layer-group"), " Ventiler par"
          ),
          tags$div(style = "min-width:240px;",
            selectInput("temporal_grp_var", label = NULL,
                        choices = grp_choices, selected = sel, width = "100%")
          ),
          downloadButton(
            "dl_temporal_csv",
            label = " T\u00e9l\u00e9charger CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      tags$p(
        style = "font-size:11px; color:#94a3b8; margin:-8px 0 12px 2px;",
        icon("info-circle"),
        " Donn\u00e9es non filtr\u00e9es par round \u2014 tous les rounds disponibles sont affich\u00e9s.",
        " Moyenne \u00b1 IC 95% (m\u00e9thode normale). ",
        tags$b(paste0(length(rounds), " rounds d\u00e9tect\u00e9s : ",
                      paste(sort_rounds(rounds), collapse = " \u2192 ")))
      ),
      fluidRow(
        if (has_fcs)
          column(if (has_rcsi) 6L else 12L,
            tags$div(class = "nutri-chart-card",
              tags$div(class = "chart-title",
                       icon("utensils"), " Tendance FCS \u2014 Score moyen par round"),
              plotlyOutput("accueil_fcs_temporal", height = "360px")
            )
          ),
        if (has_rcsi)
          column(if (has_fcs) 6L else 12L,
            tags$div(class = "nutri-chart-card",
              tags$div(class = "chart-title",
                       icon("hand-holding-heart"), " Tendance rCSI \u2014 Score moyen par round"),
              plotlyOutput("accueil_rcsi_temporal", height = "360px")
            )
          )
      )
    )
  })

  # ── Helper : agréger un score par round (et optionnel groupe) ────────────────
  temporal_agg <- function(d, sv_col, score_col, grp_col = NULL) {
    grp_vars <- c(sv_col, if (!is.null(grp_col)) grp_col)
    res <- d |>
      filter(!is.na(.data[[score_col]]), !is.na(.data[[sv_col]])) |>
      mutate(.round_chr = as.character(.data[[sv_col]])) |>
      group_by(across(all_of(c(".round_chr", if (!is.null(grp_col)) grp_col)))) |>
      summarise(
        mean_val = mean(.data[[score_col]], na.rm = TRUE),
        sd_val   = sd(.data[[score_col]],   na.rm = TRUE),
        n        = sum(!is.na(.data[[score_col]])),
        .groups  = "drop"
      ) |>
      mutate(
        ci_lo = mean_val - 1.96 * sd_val / sqrt(pmax(n, 1)),
        ci_hi = mean_val + 1.96 * sd_val / sqrt(pmax(n, 1))
      )
    # Tri intelligent des rounds
    ordered_rounds <- sort_rounds(unique(res$.round_chr))
    res$.round_chr <- factor(res$.round_chr, levels = ordered_rounds)
    res
  }

  # ── Palette pour lignes multiples ────────────────────────────────────────────
  temporal_pal <- function(n) {
    if (n == 1) return(c("#0077B6"))
    colorRampPalette(c("#0077B6", "#00838F", "#D98B14",
                       "#BF3636", "#6B3FA0", "#0B3954", "#D4A843"))(n)
  }

  # ── Analyse temporelle FCS ────────────────────────────────────────────────────
  output$accueil_fcs_temporal <- renderPlotly({
    d  <- mapped_data(); req(d); m <- map_cols()
    sv_col <- m$survey
    req(!is.null(sv_col), sv_col %in% names(d), ".fcs" %in% names(d))

    grp_col <- if (!is.null(input$temporal_grp_var) &&
                   input$temporal_grp_var != "__global__" &&
                   input$temporal_grp_var %in% names(d))
      input$temporal_grp_var else NULL

    res <- temporal_agg(d, sv_col, ".fcs", grp_col)
    req(nrow(res) >= 1)

    n_grp  <- if (!is.null(grp_col)) length(unique(res[[grp_col]])) else 1
    pal    <- setNames(temporal_pal(n_grp),
                       if (!is.null(grp_col)) unique(as.character(res[[grp_col]])) else "global")
    grp_aes <- if (!is.null(grp_col)) grp_col else NULL

    tooltip_txt <- if (!is.null(grp_col)) {
      paste0(res[[grp_col]], " — ", res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    } else {
      paste0(res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    }
    res$.tooltip <- tooltip_txt

    p <- ggplot(res, aes(x = .round_chr, y = mean_val,
                          group = if (!is.null(grp_col)) .data[[grp_col]] else 1,
                          color = if (!is.null(grp_col)) .data[[grp_col]] else "FCS moyen",
                          text  = .tooltip)) +
      # Zones de sévérité FCS
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 21,
               fill = C$rouge,  alpha = 0.06) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 21,  ymax = 35,
               fill = C$orange, alpha = 0.06) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 35,  ymax = Inf,
               fill = C$vert,   alpha = 0.04) +
      geom_hline(yintercept = 21, linetype = "dashed",
                 color = C$rouge,  linewidth = 0.6, alpha = 0.6) +
      geom_hline(yintercept = 35, linetype = "dashed",
                 color = C$orange, linewidth = 0.6, alpha = 0.6) +
      # IC 95% en ruban
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi,
                      fill = if (!is.null(grp_col)) .data[[grp_col]] else "FCS moyen"),
                  alpha = 0.12, color = NA, show.legend = FALSE) +
      # Ligne de tendance + points
      geom_line(linewidth = 1.1) +
      geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
      scale_color_manual(values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      scale_fill_manual( values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      annotate("text", x = Inf, y = 10,  hjust = 1.05, size = 2.8,
               label = "Pauvre (<21)",    color = C$rouge,  fontface = "italic") +
      annotate("text", x = Inf, y = 28,  hjust = 1.05, size = 2.8,
               label = "Limite (21-35)",  color = C$orange, fontface = "italic") +
      annotate("text", x = Inf, y = 38,  hjust = 1.05, size = 2.8,
               label = "Acceptable (>35)",color = C$vert,   fontface = "italic") +
      labs(x = "Round de collecte", y = "FCS moyen (\u00b1 IC 95%)",
           caption = "Tirets : seuils FCS OMS/WFP \u2014 Ruban : intervalle de confiance 95%") +
      theme_nutri() +
      theme(legend.position = if (!is.null(grp_col)) "bottom" else "none",
            axis.text.x = element_text(angle = if (length(levels(res$.round_chr)) > 5) 30 else 0,
                                       hjust = 1))

    ggplotly(p, tooltip = "text") |>
      layout(legend    = list(orientation = "h", y = -0.25, x = 0),
             hovermode = "closest") |>
      nutri_config()
  })

  # ── Analyse temporelle rCSI ───────────────────────────────────────────────────
  output$accueil_rcsi_temporal <- renderPlotly({
    d  <- mapped_data(); req(d); m <- map_cols()
    sv_col <- m$survey
    req(!is.null(sv_col), sv_col %in% names(d), ".rcsi" %in% names(d))

    grp_col <- if (!is.null(input$temporal_grp_var) &&
                   input$temporal_grp_var != "__global__" &&
                   input$temporal_grp_var %in% names(d))
      input$temporal_grp_var else NULL

    res <- temporal_agg(d, sv_col, ".rcsi", grp_col)
    req(nrow(res) >= 1)

    n_grp <- if (!is.null(grp_col)) length(unique(res[[grp_col]])) else 1
    pal   <- setNames(temporal_pal(n_grp),
                      if (!is.null(grp_col)) unique(as.character(res[[grp_col]])) else "global")

    tooltip_txt <- if (!is.null(grp_col)) {
      paste0(res[[grp_col]], " — ", res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    } else {
      paste0(res$.round_chr, "\n",
             "Moyenne : ", round(res$mean_val, 1), "\n",
             "IC 95% : [", round(res$ci_lo, 1), " ; ", round(res$ci_hi, 1), "]\n",
             "n = ", res$n)
    }
    res$.tooltip <- tooltip_txt

    p <- ggplot(res, aes(x = .round_chr, y = mean_val,
                          group = if (!is.null(grp_col)) .data[[grp_col]] else 1,
                          color = if (!is.null(grp_col)) .data[[grp_col]] else "rCSI moyen",
                          text  = .tooltip)) +
      # Zone coping élevé
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 19, ymax = Inf,
               fill = C$rouge, alpha = 0.06) +
      geom_hline(yintercept = 19, linetype = "dashed",
                 color = C$rouge, linewidth = 0.6, alpha = 0.6) +
      # IC 95%
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi,
                      fill = if (!is.null(grp_col)) .data[[grp_col]] else "rCSI moyen"),
                  alpha = 0.12, color = NA, show.legend = FALSE) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
      scale_color_manual(values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      scale_fill_manual( values = pal, name = if (!is.null(grp_col)) grp_col else "") +
      annotate("text", x = Inf, y = 20.5, hjust = 1.05, size = 2.8,
               label = "Coping \u00e9lev\u00e9 (\u226519)", color = C$rouge, fontface = "italic") +
      labs(x = "Round de collecte", y = "rCSI moyen (\u00b1 IC 95%)",
           caption = "Tiret rouge : seuil PAM/WFP rCSI \u226519 \u2014 Ruban : intervalle de confiance 95%") +
      theme_nutri() +
      theme(legend.position = if (!is.null(grp_col)) "bottom" else "none",
            axis.text.x = element_text(angle = if (length(levels(res$.round_chr)) > 5) 30 else 0,
                                       hjust = 1))

    ggplotly(p, tooltip = "text") |>
      layout(legend    = list(orientation = "h", y = -0.25, x = 0),
             hovermode = "closest") |>
      nutri_config()
  })


  # ── Export CSV : tendances agrégées FCS & rCSI ───────────────────────────────
  output$dl_temporal_csv <- downloadHandler(
    filename = function() {
      paste0("tendance_FCS_rCSI_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d  <- mapped_data(); req(d); m <- map_cols()
      sv_col  <- req(m$survey)
      grp_col <- if (!is.null(input$temporal_grp_var) &&
                     input$temporal_grp_var != "__global__" &&
                     input$temporal_grp_var %in% names(d))
        input$temporal_grp_var else NULL

      rows <- list()
      if (".fcs" %in% names(d)) {
        tmp <- temporal_agg(d, sv_col, ".fcs", grp_col) |>
          mutate(indicateur = "FCS",
                 round = as.character(.round_chr)) |>
          select(indicateur, round,
                 any_of(grp_col),
                 moyenne = mean_val,
                 ic_bas  = ci_lo,
                 ic_haut = ci_hi,
                 n)
        rows <- c(rows, list(tmp))
      }
      if (".rcsi" %in% names(d)) {
        tmp <- temporal_agg(d, sv_col, ".rcsi", grp_col) |>
          mutate(indicateur = "rCSI",
                 round = as.character(.round_chr)) |>
          select(indicateur, round,
                 any_of(grp_col),
                 moyenne = mean_val,
                 ic_bas  = ci_lo,
                 ic_haut = ci_hi,
                 n)
        rows <- c(rows, list(tmp))
      }
      req(length(rows) > 0)
      out <- bind_rows(rows) |>
        mutate(across(where(is.numeric), ~round(.x, 2)))
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

