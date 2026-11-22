# Extrait de app_v2_final.R — dashboard accueil — tableau croisé rCSI × FCS
  # ── Tableau croisé rCSI × FCS — helper ────────────────────────────────────
  build_crosstab <- function(d, grp_col = NULL) {
    rcsi_lvls <- c("Pas/Peu de coping", "Coping moyen", "Coping \u00e9lev\u00e9 (\u226519)")
    fcs_lvls  <- c("Pauvre (<21)", "Limite (21-35)", "Acceptable (>35)")

    d2 <- d |>
      filter(!is.na(rcsi_cat), !is.na(fcs_cat)) |>
      mutate(
        rcsi_cat = factor(as.character(rcsi_cat), levels = rcsi_lvls),
        fcs_cat  = factor(as.character(fcs_cat),  levels = fcs_lvls),
        .grp     = if (!is.null(grp_col))
          as.character(.data[[grp_col]]) else "Ensemble"
      ) |>
      filter(!is.na(.grp))

    if (nrow(d2) == 0) return(NULL)

    grp_tot <- count(d2, .grp, name = "N_grp")

    # Comptage complet (toutes combinaisons)
    ct <- d2 |>
      count(.grp, rcsi_cat, fcs_cat, name = "n") |>
      tidyr::complete(.grp, rcsi_cat, fcs_cat, fill = list(n = 0)) |>
      left_join(grp_tot, by = ".grp")

    # Lignes de détail : pivot large par fcs_cat
    detail <- ct |>
      pivot_wider(id_cols = c(.grp, rcsi_cat, N_grp),
                  names_from = fcs_cat, values_from = n, values_fill = 0) |>
      rename(n_P = `Pauvre (<21)`, n_L = `Limite (21-35)`, n_A = `Acceptable (>35)`) |>
      mutate(
        Total_n     = n_P + n_L + n_A,
        is_subtotal = 0L,
        is_dblvuln  = if_else(as.character(rcsi_cat) == "Coping \u00e9lev\u00e9 (\u226519)", 1L, 0L),
        sort_order  = case_when(
          rcsi_cat == "Pas/Peu de coping"          ~ 1L,
          rcsi_cat == "Coping moyen"               ~ 2L,
          rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" ~ 3L,
          TRUE ~ 4L
        )
      )

    # Lignes de sous-total par groupe
    sub <- ct |>
      group_by(.grp, N_grp, fcs_cat) |>
      summarise(n = sum(n), .groups = "drop") |>
      pivot_wider(id_cols = c(.grp, N_grp),
                  names_from = fcs_cat, values_from = n, values_fill = 0) |>
      rename(n_P = `Pauvre (<21)`, n_L = `Limite (21-35)`, n_A = `Acceptable (>35)`) |>
      mutate(
        rcsi_cat    = "— Sous-total",
        Total_n     = N_grp,
        is_subtotal = 1L,
        is_dblvuln  = 0L,
        sort_order  = 5L
      )

    # Ligne grand total toutes régions
    grand <- tibble(
      .grp        = "\u25ba TOTAL G\u00c9N\u00c9RAL",
      rcsi_cat    = "Tous",
      N_grp       = nrow(d2),
      n_P         = as.integer(sum(d2$fcs_cat == "Pauvre (<21)",      na.rm = TRUE)),
      n_L         = as.integer(sum(d2$fcs_cat == "Limite (21-35)",    na.rm = TRUE)),
      n_A         = as.integer(sum(d2$fcs_cat == "Acceptable (>35)",  na.rm = TRUE)),
      Total_n     = nrow(d2),
      is_subtotal = 2L,
      is_dblvuln  = 0L,
      sort_order  = 6L
    )

    bind_rows(detail, sub, grand) |>
      arrange(.grp == "\u25ba TOTAL G\u00c9N\u00c9RAL", .grp, sort_order) |>
      mutate(
        pct_P = round(n_P      / N_grp * 100, 1),
        pct_L = round(n_L      / N_grp * 100, 1),
        pct_A = round(n_A      / N_grp * 100, 1),
        pct_T = round(Total_n  / N_grp * 100, 1)
      )
  }

  # Reactive
  crosstab_data <- reactive({
    d <- df_f()
    req("rcsi_cat" %in% names(d), "fcs_cat" %in% names(d))
    grp_col <- if (!is.null(input$crosstab_grp_var) &&
                   input$crosstab_grp_var != "__none__" &&
                   input$crosstab_grp_var %in% names(d))
      input$crosstab_grp_var else NULL
    build_crosstab(d, grp_col)
  })

  # ── Tableau croisé rCSI × FCS — UI section ────────────────────────────────
  output$accueil_crosstab_row <- renderUI({
    d <- df_f(); m <- map_cols()
    if (!("rcsi_cat" %in% names(d)) || !("fcs_cat" %in% names(d))) return(NULL)

    # Choix de ventilation
    grp_choices <- c("Aucun regroupement" = "__none__")
    add_g <- function(col, lbl) {
      if (!is.null(col) && col %in% names(d) &&
          dplyr::n_distinct(na.omit(d[[col]])) >= 2 &&
          dplyr::n_distinct(na.omit(d[[col]])) <= 30)
        grp_choices[lbl] <<- col
    }
    add_g(m$region, paste0("\U0001f5fa\ufe0f  R\u00e9gion  (", m$region, ")"))
    add_g(m$dept,   paste0("\U0001f4cd  D\u00e9partement  (", m$dept, ")"))
    add_g(m$sexe,   paste0("\U0001f464  Sexe  (", m$sexe, ")"))
    add_g(m$depl,   paste0("\U0001f3d5\ufe0f  D\u00e9placement  (", m$depl, ")"))
    add_g(m$strate, paste0("\U0001f5c2\ufe0f  Strate  (", m$strate, ")"))

    default_sel <- if (!is.null(m$region) && m$region %in% names(d)) m$region else "__none__"
    current_sel <- isolate(input$crosstab_grp_var)
    sel <- if (!is.null(current_sel) && current_sel %in% grp_choices) current_sel else default_sel

    # KPI double vulnérabilité (pré-calculé)
    n_dv  <- sum(!is.na(d$rcsi_cat) & !is.na(d$fcs_cat) &
                   d$rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" &
                   d$fcs_cat  == "Pauvre (<21)", na.rm = TRUE)
    n_tot <- sum(!is.na(d$rcsi_cat) & !is.na(d$fcs_cat))
    pct_dv <- if (n_tot > 0) round(n_dv / n_tot * 100, 1) else 0

    tagList(
      # ── En-tête ─────────────────────────────────────────────────────────────
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(icon("th"), " Tableau crois\u00e9 rCSI \u00d7 FCS \u2014 Double vuln\u00e9rabilit\u00e9 alimentaire"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap; font-size:13px; font-weight:400;",
          tags$span(
            style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;",
            icon("layer-group"), " Ventiler par"
          ),
          tags$div(style = "min-width:240px;",
            selectInput("crosstab_grp_var", label = NULL,
                        choices = grp_choices, selected = sel, width = "100%")
          ),
          downloadButton("dl_crosstab_csv",
            label = " CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          ),
          downloadButton("dl_crosstab_xlsx",
            label = " Excel",
            icon  = icon("file-excel"),
            style = paste0("font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
                           "color:white !important; background:#00838F; border-color:#00838F;"),
            class = "btn-sm"
          )
        )
      ),
      # ── KPI double vulnérabilité ─────────────────────────────────────────────
      fluidRow(
        column(12,
          tags$div(
            style = paste0(
              "background:linear-gradient(135deg,#fff0f0 0%,#ffe4e4 100%);",
              "border-left:5px solid #BF3636; border-radius:12px;",
              "padding:16px 22px; margin-bottom:16px;",
              "display:flex; align-items:center; gap:20px; flex-wrap:wrap;"
            ),
            tags$div(
              style = paste0(
                "width:52px; height:52px; border-radius:12px; background:#BF3636;",
                "display:flex; align-items:center; justify-content:center;",
                "font-size:20px; color:white; flex-shrink:0;"
              ),
              icon("exclamation-triangle")
            ),
            tags$div(
              tags$div(
                style = "font-size:11px; font-weight:700; text-transform:uppercase; letter-spacing:1px; color:#991b1b; margin-bottom:4px;",
                "\u26a0\ufe0f  M\u00e9nages en double vuln\u00e9rabilit\u00e9 alimentaire"
              ),
              tags$div(
                tags$span(style="font-size:32px; font-weight:800; color:#BF3636; line-height:1;",
                          n_dv),
                tags$span(style="font-size:15px; font-weight:400; color:#666; margin-left:8px;",
                          paste0("m\u00e9nages \u2014 ", pct_dv, "% des observations valid\u00e9es"))
              ),
              tags$div(
                style = "font-size:12px; color:#555; margin-top:5px;",
                tags$b("Crit\u00e8re :"), " FCS < 21 (Pauvre)",
                tags$span(style="font-weight:700; color:#BF3636; margin:0 4px;", "ET"),
                "rCSI \u2265 19 (Coping \u00e9lev\u00e9) \u2014 Double crise alimentaire (WFP / FAO)"
              )
            )
          )
        )
      ),
      # ── Test du Chi² ────────────────────────────────────────────────────────
      {
        d_v    <- d[!is.na(d$rcsi_cat) & !is.na(d$fcs_cat), ]
        ct_mat <- table(d_v$rcsi_cat, d_v$fcs_cat)
        chi2 <- tryCatch(chisq.test(ct_mat, correct = FALSE), error = function(e) NULL)

        if (!is.null(chi2)) {
          n_obs    <- sum(ct_mat)
          k_min    <- min(nrow(ct_mat), ncol(ct_mat))
          cramer_v <- if (k_min > 1 && n_obs > 0)
            as.numeric(sqrt(chi2$statistic / (n_obs * (k_min - 1)))) else 0
          min_exp  <- tryCatch(round(min(chi2$expected), 2), error = function(e) NA_real_)
          low_exp  <- isTRUE(min_exp < 5)

          # ── Test exact de Fisher (Monte Carlo) si fréquences attendues < 5 ──
          fisher_res <- if (low_exp) {
            tryCatch(
              fisher.test(ct_mat, simulate.p.value = TRUE, B = 10000),
              error = function(e) NULL
            )
          } else NULL
          use_fisher <- !is.null(fisher_res)

          # p-valeur de référence : Fisher si disponible, Chi² sinon
          # Protégé contre NA (chi2 peut donner p.value = NA si table dégénérée)
          pval      <- if (use_fisher) fisher_res$p.value else chi2$p.value
          if (is.null(pval) || !is.finite(pval)) pval <- NA_real_

          pval_txt  <- if (isTRUE(pval < 0.001)) "< 0,001" else
                       if (isTRUE(pval < 0.01))  paste0("= ", round(pval, 4)) else
                       if (!is.na(pval)) paste0("= ", round(pval, 3)) else "N/A"
          chi2_p    <- chi2$p.value
          chi2_ptxt <- if (isTRUE(chi2_p < 0.001)) "< 0,001" else
                       if (!is.na(chi2_p)) paste0(round(chi2_p, 4)) else "N/A"

          sig_lbl <- if (isTRUE(pval < 0.001)) "Hautement significatif (***)" else
                     if (isTRUE(pval < 0.01))  "Tr\u00e8s significatif (**)" else
                     if (isTRUE(pval < 0.05))  "Significatif (*)" else
                     "Non significatif (p \u2265 0,05)"
          sig_col <- if (isTRUE(pval < 0.05)) C$rouge else "#64748b"
          sig_bg  <- if (isTRUE(pval < 0.001)) "#fff0f0" else
                     if (isTRUE(pval < 0.05))  "#fff8f0" else "#f1f5f9"

          if (is.na(cramer_v) || !is.finite(cramer_v)) cramer_v <- 0
          v_lbl <- if (cramer_v < 0.10) "N\u00e9gligeable (V < 0,10)" else
                   if (cramer_v < 0.20) "Faible (0,10 \u2264 V < 0,20)" else
                   if (cramer_v < 0.30) "Mod\u00e9r\u00e9e (0,20 \u2264 V < 0,30)" else
                   "Forte (V \u2265 0,30)"
          v_col <- if (cramer_v < 0.10) "#64748b" else
                   if (cramer_v < 0.20) C$orange else C$rouge

          concordant <- if (use_fisher)
            isTRUE(chi2_p < 0.05) == isTRUE(fisher_res$p.value < 0.05) else TRUE

          test_titre <- if (use_fisher)
            "Test exact de Fisher (Monte Carlo) \u2014 rCSI \u00d7 FCS"
          else
            "Test du \u03c7\u00b2 d'ind\u00e9pendance \u2014 rCSI \u00d7 FCS"
          test_icone <- if (use_fisher) "microscope" else "flask"
          pval_label <- if (use_fisher) "p-valeur (Fisher MC)" else "p-valeur (\u03c7\u00b2)"

          chi2_row <- fluidRow(
            column(12,
              tags$div(
                style = paste0(
                  "background:", sig_bg, "; border-left:5px solid ", sig_col, ";",
                  "border-radius:12px; padding:18px 22px; margin-bottom:16px;"
                ),
                # ── Titre + badge ───────────────────────────────────────────
                tags$div(
                  style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:8px; margin-bottom:14px;",
                  tags$div(
                    style = "font-size:14px; font-weight:700; color:#1e293b;",
                    icon(test_icone), paste0(" ", test_titre)
                  ),
                  tags$div(
                    style = paste0("background:", sig_col, "; color:white; padding:3px 12px;",
                                   "border-radius:20px; font-size:11px; font-weight:700;",
                                   "text-transform:uppercase; letter-spacing:.5px;"),
                    sig_lbl
                  )
                ),
                # ── 4 métriques ─────────────────────────────────────────────
                tags$div(
                  style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(160px,1fr)); gap:14px; margin-bottom:14px;",
                  # Chi² (grisé/pointillé si Fisher prend la relève)
                  tags$div(
                    style = paste0(
                      "background:white; border-radius:8px; padding:12px 14px; text-align:center;",
                      "border:1px ", if (use_fisher) "dashed #d97706" else "solid #e2e8f0", ";",
                      if (use_fisher) "opacity:0.78;" else ""
                    ),
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      if (use_fisher) "\u03c7\u00b2 (approximatif)" else "Statistique \u03c7\u00b2"
                    ),
                    tags$div(style = "font-size:26px;font-weight:800;color:#1e293b;line-height:1;",
                             round(chi2$statistic, 2)),
                    tags$div(style = "font-size:11px;color:#94a3b8;margin-top:2px;",
                             paste0("ddl = ", chi2$parameter)),
                    if (use_fisher)
                      tags$div(
                        style = "font-size:10px;color:#d97706;font-weight:600;margin-top:3px;",
                        "\u26a0\ufe0f Fr\u00e9q. < 5 \u2014 voir Fisher"
                      )
                  ),
                  # p-valeur de référence
                  tags$div(
                    style = paste0(
                      "background:white; border-radius:8px; padding:12px 14px;",
                      "border:1px solid #e2e8f0; text-align:center;",
                      if (isTRUE(pval < 0.05)) "border-color:#BF3636; box-shadow:0 0 0 2px rgba(201,22,29,.1);" else ""
                    ),
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      pval_label
                    ),
                    tags$div(
                      style = paste0("font-size:22px;font-weight:800;line-height:1;color:",
                                     if (isTRUE(pval < 0.05)) sig_col else "#1e293b", ";"),
                      pval_txt
                    ),
                    tags$div(style = "font-size:11px;color:#94a3b8;margin-top:2px;",
                             paste0("n\u00a0=\u00a0", format(n_obs, big.mark = "\u00a0")))
                  ),
                  # V de Cramér
                  tags$div(
                    style = "background:white; border-radius:8px; padding:12px 14px; border:1px solid #e2e8f0; text-align:center;",
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      "V de Cram\u00e9r"
                    ),
                    tags$div(style = paste0("font-size:26px;font-weight:800;line-height:1;color:", v_col, ";"),
                             round(cramer_v, 3)),
                    tags$div(style = paste0("font-size:11px;color:", v_col, ";font-weight:600;margin-top:2px;"),
                             v_lbl)
                  ),
                  # Fréquences attendues min.
                  tags$div(
                    style = paste0(
                      "background:white; border-radius:8px; padding:12px 14px; text-align:center;",
                      "border:1px solid ", if (low_exp) "#d97706" else "#e2e8f0", ";",
                      if (low_exp) "box-shadow:0 0 0 2px rgba(217,119,6,.12);" else ""
                    ),
                    tags$div(
                      style = "font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#64748b;margin-bottom:4px;",
                      "Fr\u00e9q. attendue min."
                    ),
                    tags$div(
                      style = paste0("font-size:26px;font-weight:800;line-height:1;color:",
                                     if (low_exp) C$orange else C$vert, ";"),
                      min_exp
                    ),
                    tags$div(
                      style = paste0("font-size:11px;font-weight:600;color:",
                                     if (low_exp) C$orange else "#64748b", ";margin-top:2px;"),
                      if (low_exp) "\u26a0\ufe0f Hypoth\u00e8se \u00e0 v\u00e9rifier"
                      else         "\u2713 Hypoth\u00e8se v\u00e9rifi\u00e9e"
                    )
                  )
                ),
                # ── Bande comparaison Chi² ↔ Fisher ─────────────────────────
                if (use_fisher)
                  tags$div(
                    style = paste0(
                      "background:linear-gradient(135deg,#fffbeb,#fff7ed);",
                      "border:1px solid #f59e0b; border-radius:8px;",
                      "padding:10px 16px; margin-bottom:14px;",
                      "display:flex; align-items:center; flex-wrap:wrap; gap:12px;"
                    ),
                    tags$div(
                      style = "font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:.6px;color:#92400e;flex-shrink:0;",
                      icon("balance-scale"), " Comparaison"
                    ),
                    # Chi² (approximatif)
                    tags$div(
                      style = paste0(
                        "background:white; border-radius:6px; padding:6px 12px;",
                        "border:1px dashed #d97706; font-size:12px; white-space:nowrap;"
                      ),
                      tags$span(style = "color:#64748b; font-weight:600;", "\u03c7\u00b2 : "),
                      tags$span(
                        style = paste0("font-weight:800; color:",
                                       if (isTRUE(chi2$p.value < 0.05)) C$rouge else "#475569", ";"),
                        paste0("p\u00a0=\u00a0", chi2_ptxt)
                      ),
                      tags$span(style = "font-size:10px;color:#94a3b8;margin-left:4px;",
                                "(approximatif)")
                    ),
                    tags$div(
                      style = "font-size:18px; color:#d97706; flex-shrink:0;",
                      if (isTRUE(concordant)) "\u21d4\u00a0\u2713" else "\u21d4\u00a0!"
                    ),
                    # Fisher (référence)
                    tags$div(
                      style = paste0(
                        "background:#0B3954; border-radius:6px; padding:6px 12px;",
                        "font-size:12px; white-space:nowrap;"
                      ),
                      tags$span(style = "color:#93c5fd; font-weight:600;", "Fisher MC : "),
                      tags$span(
                        style = paste0("font-weight:800; color:",
                                       if (isTRUE(fisher_res$p.value < 0.05)) "#fca5a5" else "white", ";"),
                        pval_txt
                      ),
                      tags$span(style = "font-size:10px;color:#93c5fd;margin-left:4px;",
                                "(r\u00e9f\u00e9rence)")
                    ),
                    tags$div(
                      style = paste0(
                        "font-size:11px; font-weight:700; color:",
                        if (isTRUE(concordant)) "#166534" else "#991b1b", ";"
                      ),
                      if (isTRUE(concordant)) "\u2713 Conclusions concordantes"
                      else            "! Conclusions divergentes \u2014 privil\u00e9gier Fisher"
                    )
                  ),
                # ── Interprétation textuelle ─────────────────────────────────
                tags$div(
                  style = "background:white; border-radius:8px; padding:12px 16px; border:1px solid #e2e8f0; font-size:12.5px; line-height:1.7; color:#334155;",
                  tags$b(icon("comment-alt"), " Interpr\u00e9tation : "),
                  if (!isTRUE(pval < 0.05)) {
                    paste0(
                      "Aucune association statistiquement significative n'est d\u00e9tect\u00e9e",
                      " entre les cat\u00e9gories rCSI et FCS dans cet \u00e9chantillon (p \u2265 0,05).",
                      " Les deux indicateurs semblent ind\u00e9pendants",
                      " \u2014 ce qui m\u00e9rite une investigation suppl\u00e9mentaire."
                    )
                  } else {
                    tags$span(
                      if (use_fisher)
                        paste0("Le test exact de Fisher (simulation Monte Carlo, B\u00a0=\u00a010\u00a0000) ",
                               "r\u00e9v\u00e8le une association ",
                               tolower(sub(" \\(.*", "", sig_lbl)),
                               " entre les cat\u00e9gories rCSI et FCS (p\u00a0", pval_txt, "). ")
                      else
                        paste0("Le test du \u03c7\u00b2 r\u00e9v\u00e8le une association ",
                               tolower(sub(" \\(.*", "", sig_lbl)),
                               " entre les cat\u00e9gories rCSI et FCS (\u03c7\u00b2\u00a0=\u00a0",
                               round(chi2$statistic, 2), ", ddl\u00a0=\u00a0", chi2$parameter,
                               ", p\u00a0", pval_txt, "). "),
                      paste0("La force de l'association est ",
                             tolower(sub(" \\(.*", "", v_lbl)),
                             " (V de Cram\u00e9r\u00a0=\u00a0", round(cramer_v, 3), "). "),
                      "La relation attendue est ", tags$b("n\u00e9gative"),
                      " : un FCS \u00e9lev\u00e9 devrait correspondre \u00e0 un rCSI faible, et inversement.",
                      if (use_fisher && concordant)
                        tags$span(
                          style = "display:block; margin-top:6px; color:#166534; font-weight:600;",
                          "\u2713 Le test exact de Fisher confirme la conclusion du \u03c7\u00b2",
                          " malgr\u00e9 des fr\u00e9quences attendues faibles (min.\u00a0=\u00a0", min_exp, ")."
                        ),
                      if (use_fisher && !concordant)
                        tags$span(
                          style = "display:block; margin-top:6px; color:#991b1b; font-weight:700;",
                          "\u26a0\ufe0f Les deux tests divergent. La p-valeur de Fisher (r\u00e9f\u00e9rence) ",
                          if (fisher_res$p.value < 0.05)
                            "contredit la non-significativit\u00e9 indiqu\u00e9e par le \u03c7\u00b2."
                          else
                            "temp\u00e8re la significativit\u00e9 indiqu\u00e9e par le \u03c7\u00b2.",
                          " Privil\u00e9gier Fisher en raison des fr\u00e9quences attendues insuffisantes."
                        )
                    )
                  }
                )
              )
            )
          )
        } else {
          chi2_row <- NULL
        }
        chi2_row
      },
      # ── Tableau ─────────────────────────────────────────────────────────────
      fluidRow(
        column(12,
          tags$div(class = "nutri-chart-card",
            tags$p(
              style = "font-size:11px; color:#94a3b8; margin:0 0 10px;",
              icon("info-circle"),
              " Pourcentages calcul\u00e9s sur le total du groupe de ventilation. \u2014 ",
              tags$span(
                style = "background:#fee2e2; color:#BF3636; padding:1px 7px; border-radius:4px; font-weight:700;",
                "Cellule rouge"
              ), " = rCSI \u00e9lev\u00e9 \u00d7 FCS Pauvre. \u2014 ",
              tags$span(
                style = "background:#f0f4f8; padding:1px 7px; border-radius:4px; font-weight:700;",
                "Ligne bleue"
              ), " = sous-total groupe."
            ),
            DT::DTOutput("accueil_crosstab_table")
          )
        )
      )
    )
  })

  # ── Tableau croisé rCSI × FCS — DT render ────────────────────────────────
  output$accueil_crosstab_table <- DT::renderDT({
    ct <- crosstab_data()
    req(!is.null(ct), nrow(ct) > 0)

    grp_label <- if (!is.null(input$crosstab_grp_var) &&
                     input$crosstab_grp_var != "__none__")
      input$crosstab_grp_var else "Groupe"

    # Construire la table d'affichage + colonnes cachées helpers
    disp <- ct |>
      transmute(
        Groupe_           = .grp,
        Cat_rCSI          = as.character(rcsi_cat),
        FCS_Pauvre        = paste0(n_P,     "\u00a0(", pct_P, "%)"),
        FCS_Limite        = paste0(n_L,     "\u00a0(", pct_L, "%)"),
        FCS_Acceptable    = paste0(n_A,     "\u00a0(", pct_A, "%)"),
        Total             = paste0(Total_n, "\u00a0(", pct_T, "%)"),
        .sub              = is_subtotal,
        .dv               = is_dblvuln
      ) |>
      rename(
        !!grp_label              := Groupe_,
        "Cat\u00e9gorie rCSI"    = Cat_rCSI,
        "FCS Pauvre (<21)"       = FCS_Pauvre,
        "FCS Limite (21-35)"     = FCS_Limite,
        "FCS Acceptable (>35)"   = FCS_Acceptable
      )

    n_tot_col <- ncol(disp)
    hidden_0idx <- c(n_tot_col - 2L, n_tot_col - 1L)  # 0-indexed

    DT::datatable(
      disp,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength = 100,
        dom        = "ftp",
        scrollX    = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = hidden_0idx),
          list(className = "dt-center", targets = 2:(n_tot_col - 3L))
        )
      )
    ) |>
      DT::formatStyle(
        columns     = 1:(n_tot_col - 2L),
        valueColumns = ".sub",
        backgroundColor = DT::styleEqual(c(1, 2), c("#e8f0fb", "#0B3954")),
        color           = DT::styleEqual(c(1, 2), c("#0B3954", "white")),
        fontWeight      = DT::styleEqual(c(1, 2), c("700", "800")),
        borderTop       = DT::styleEqual(c(1, 2), c("2px solid #0077B6", "3px solid #0077B6"))
      ) |>
      DT::formatStyle(
        columns      = 3,
        valueColumns = ".dv",
        backgroundColor = DT::styleEqual(1, "rgba(201,22,29,0.18)"),
        color           = DT::styleEqual(1, "#BF3636"),
        fontWeight      = DT::styleEqual(1, "800")
      ) |>
      DT::formatStyle(
        columns    = 1:(n_tot_col - 2L),
        lineHeight = "1.7"
      )
  })

  # ── Export CSV ────────────────────────────────────────────────────────────
  output$dl_crosstab_csv <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$crosstab_grp_var) &&
                  input$crosstab_grp_var != "__none__")
        input$crosstab_grp_var else "ensemble"
      paste0("crosstab_FCS_rCSI_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      ct <- crosstab_data(); req(!is.null(ct))
      out <- ct |>
        transmute(
          Groupe                 = .grp,
          Categorie_rCSI         = as.character(rcsi_cat),
          n_FCS_Pauvre           = n_P,
          pct_FCS_Pauvre         = pct_P,
          n_FCS_Limite           = n_L,
          pct_FCS_Limite         = pct_L,
          n_FCS_Acceptable       = n_A,
          pct_FCS_Acceptable     = pct_A,
          n_Total_groupe         = Total_n,
          pct_Total_groupe       = pct_T,
          N_groupe               = N_grp,
          Double_vulnerabilite   = if_else(is_dblvuln == 1L, "Oui", "Non"),
          Type_ligne             = case_when(
            is_subtotal == 1L ~ "Sous-total",
            is_subtotal == 2L ~ "Grand total",
            TRUE              ~ "Detail"
          )
        )
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Export Excel ─────────────────────────────────────────────────────────
  output$dl_crosstab_xlsx <- downloadHandler(
    filename = function() {
      grp <- if (!is.null(input$crosstab_grp_var) &&
                  input$crosstab_grp_var != "__none__")
        input$crosstab_grp_var else "ensemble"
      paste0("crosstab_FCS_rCSI_", grp, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      ct <- crosstab_data(); req(!is.null(ct))

      # Feuille 1 : affichage formaté
      sheet1 <- ct |>
        transmute(
          Groupe                = .grp,
          `Catégorie rCSI`      = as.character(rcsi_cat),
          `FCS Pauvre (<21)`    = paste0(n_P, " (", pct_P, "%)"),
          `FCS Limite (21-35)`  = paste0(n_L, " (", pct_L, "%)"),
          `FCS Acceptable (>35)` = paste0(n_A, " (", pct_A, "%)"),
          Total                 = paste0(Total_n, " (", pct_T, "%)"),
          `Double vuln.`        = if_else(is_dblvuln == 1L, "OUI ⚠️", ""),
          `Type ligne`          = case_when(
            is_subtotal == 1L ~ "Sous-total",
            is_subtotal == 2L ~ "TOTAL",
            TRUE              ~ "Détail"
          )
        )

      # Feuille 2 : données brutes numériques
      sheet2 <- ct |>
        filter(is_subtotal != 2L) |>   # exclure grand total
        transmute(
          Groupe             = .grp,
          Categorie_rCSI     = as.character(rcsi_cat),
          n_FCS_Pauvre       = n_P,
          pct_FCS_Pauvre     = pct_P,
          n_FCS_Limite       = n_L,
          pct_FCS_Limite     = pct_L,
          n_FCS_Acceptable   = n_A,
          pct_FCS_Acceptable = pct_A,
          n_Total            = Total_n,
          N_groupe           = N_grp,
          Double_vuln        = if_else(is_dblvuln == 1L, 1L, 0L)
        )

      tryCatch(
        writexl::write_xlsx(
          list("Tableau croisé" = sheet1, "Données numériques" = sheet2),
          path = file
        ),
        error = function(e) {
          # Fallback CSV si writexl indisponible
          write.csv(sheet1, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      )
    }
  )

