# Module Shiny — Tableau descriptif OMS + analyses descriptives expert
# Les onglets expert sont namespacés via desc_expert_tabs(ns).

mod_descoms_ui <- function(id) {
  ns <- shiny::NS(id)

      tabItem(tabName="descoms",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("stethoscope"), "Tableau descriptif — Standards OMS"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Générez un tableau récapitulatif de vos indicateurs selon les standards OMS : ",
                 "moyennes, écarts-types, médianes et intervalles de confiance, ",
                 "avec possibilité de stratifier par groupe (via gtsummary).")
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=icon("stethoscope"), " Tableau descriptif complet — Style OMS (gtsummary)",
              fluidRow(
                column(3, uiOutput(ns("desc_by_ui"))),
                column(5, uiOutput(ns("desc_vars_ui"))),
                column(2, br(),
                       checkboxInput(ns("desc_add_p"),"Ajouter p-values",TRUE)),
                column(2, br(),
                       downloadButton(ns("dl_gt"),"📥 Word",
                         style="background:#2E7D52;color:white;border:none;border-radius:6px;padding:6px 12px;width:100%;"))
              ),
              uiOutput(ns("desc_gt_out"))
          )
        ),
        fluidRow(
          box(width=6, status="info", solidHeader=TRUE,
              title="Matrice corrélation Spearman",
              uiOutput(ns("desc_corr_vars_ui")),
              plotOutput(ns("desc_corr_plot"), height="400px")),
          box(width=6, status="warning", solidHeader=TRUE,
              title="Scores Z par groupe vs seuils OMS",
              uiOutput(ns("desc_grp_ui")),
              plotlyOutput(ns("desc_zscore_plot"), height="380px"))
        ),

        # ── TABS EXPERT DESCRIPTIF ──────────────────────────────────────────────
        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE,
              title = tags$span(icon("flask"), " Analyses descriptives expert"),
              do.call(tabsetPanel, c(list(id = ns("desc_expert_tabs"), type = "pills"), desc_expert_tabs(ns)))
          )
        )
      )
}

mod_descoms_server <- function(id, df_f, vars_cat, vars_num, map_cols, gpal) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # ── DESCRIPTIVE OMS ─────────────────────────────────────────────────────────
  output$desc_by_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("desc_by"),"Stratifier par :",
                choices=c("Aucune"="none",setNames(vc,vc)),
                selected="none",width="100%")
  })
  output$desc_vars_ui <- renderUI({
    df <- df_f(); vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput(ns("desc_vars"),"Variables :",choices=vis,selected=head(vis,14),
                   multiple=TRUE,width="100%")
  })
  output$desc_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("desc_grp"),"Grouper par :",choices=vc,selected=vc[1],width="100%")
  })
  output$desc_corr_vars_ui <- renderUI({
    vn <- vars_num()
    selectizeInput(ns("desc_corr_vars"),"Variables pour corrélation :",
                   choices=vn,selected=head(vn,10),multiple=TRUE,width="100%")
  })

  output$desc_gt_out <- renderUI({
    req(input$desc_vars); d <- df_f()
    tryCatch({
      tbl_data <- d |> select(any_of(
        if(input$desc_by!="none") c(input$desc_vars,input$desc_by) else input$desc_vars))
      gt_obj <- if (input$desc_by == "none") {
        tbl_data |>
          tbl_summary(
            statistic = list(all_continuous()  ~ "{mean} ± {sd} [{min}–{max}]",
                             all_categorical() ~ "{n} ({p}%)"),
            digits  = all_continuous() ~ 1,
            missing = "no") |>
          bold_labels() |>
          modify_caption("**Tableau descriptif — Style OMS/UNICEF**")
      } else {
        # Construire le tableau de base, puis ajouter les p-valeurs si demandé
        # (on évite le placeholder magrittr '.' incompatible avec le pipe natif |>)
        tbl_base <- tbl_data |>
          tbl_summary(
            by      = input$desc_by,
            statistic = list(all_continuous()  ~ "{mean} ± {sd}",
                             all_categorical() ~ "{n} ({p}%)"),
            digits  = all_continuous() ~ 1,
            missing = "no") |>
          bold_labels()
        if (isTRUE(input$desc_add_p))
          tbl_base |> add_p() |> bold_p(t = 0.05)
        else
          tbl_base
      }
      gt_obj |> as_gt() |> tab_options(table.font.size="12px") |> as_raw_html()
    }, error=function(e)
      tags$div(style="color:red;",paste("Erreur gtsummary:",e$message)))
  })

  output$desc_corr_plot <- renderPlot({
    req(input$desc_corr_vars); d <- df_f()
    vn <- intersect(input$desc_corr_vars, vars_num())
    req(length(vn) >= 2)
    mat <- d |> select(all_of(vn)) |> drop_na() |> cor(method="spearman")
    corrplot::corrplot(mat,method="color",type="upper",
                       addCoef.col="black",number.cex=.65,
                       tl.cex=.72,tl.col="#0B3954",tl.srt=45,
                       col=colorRampPalette(c(C$rouge,"white",C$bleu))(200),
                       mar=c(0,0,1,0))
  }, height=380)

  output$desc_zscore_plot <- renderPlotly({
    req(input$desc_grp); d <- df_f(); m <- map_cols()
    z_cols <- c(m$whz,m$haz,m$waz)
    z_cols <- z_cols[!sapply(z_cols,is.null) & z_cols %in% names(d)]
    req(length(z_cols)>0)
    df_long <- d |> select(all_of(c(input$desc_grp,z_cols))) |>
      pivot_longer(-all_of(input$desc_grp), names_to="Score", values_to="Val")
    niv <- length(unique(na.omit(d[[input$desc_grp]])))
    p <- ggplot(df_long,aes(x=.data[[input$desc_grp]],y=Val,fill=.data[[input$desc_grp]]))+
      geom_boxplot(alpha=.7,outlier.shape=21,outlier.color=C$rouge,width=.5)+
      geom_hline(yintercept=-2,linetype="dashed",color=C$rouge,linewidth=.7)+
      geom_hline(yintercept=-3,linetype="dashed",color=C$rouge_f,linewidth=.7)+
      geom_hline(yintercept=0,linetype="dotted",color="gray50",linewidth=.4)+
      scale_fill_manual(values=gpal(niv,"cat"))+
      facet_wrap(~Score,scales="free_y")+
      labs(x="",y="Score Z")+theme_bw(base_size=10)+
      theme(legend.position="none",axis.text.x=element_text(angle=25,hjust=1))
    ggplotly(p)|>layout(hovermode="closest") |> nutri_config()
  })

  output$dl_gt <- downloadHandler(
    filename=function() paste0("tableau_desc_",Sys.Date(),".docx"),
    content=function(f) {
      d <- df_f(); req(input$desc_vars)
      tbl_data <- d |> select(any_of(input$desc_vars))
      tbl <- tbl_data |>
        tbl_summary(statistic=list(all_continuous()~"{mean} ± {sd}",
                                    all_categorical()~"{n} ({p}%)"),
                    digits=all_continuous()~1, missing="no") |>
        bold_labels()
      tryCatch(
        tbl |> as_flex_table() |> flextable::save_as_docx(path=f),
        error=function(e) writeLines("Erreur export Word", f)
      )
    }
  )


# ═══════════════════════════════════════════════════════════════════════════════

# ── A1 : Bootstrap IC ─────────────────────────────────────────────────────────
output$desc_boot_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput(ns("desc_boot_var"), "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_boot_res <- reactiveVal(NULL)

observeEvent(input$run_desc_boot, {
  req(input$desc_boot_var)
  d <- df_f()
  req(input$desc_boot_var %in% names(d))
  withProgress(message = "Bootstrap en cours...", value = 0.5, {
    res <- tryCatch(
      calc_stats_bootstrap(d[[input$desc_boot_var]],
                           B = input$desc_boot_B,
                           conf = input$desc_boot_conf),
      error = function(e) NULL)
    incProgress(0.5)
  })
  desc_boot_res(res)
})

output$desc_boot_table <- renderDT({
  req(desc_boot_res())
  datatable(desc_boot_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 15, ordering = FALSE),
            class = "compact stripe hover") |>
    formatRound(c("Estimation", "IC inf", "IC sup", "Err. std. bootstrap"), 4)
})

output$desc_boot_comparison <- renderUI({
  req(desc_boot_res())
  res <- desc_boot_res()
  moy <- res$Estimation[res$Statistique == "Moyenne"]
  med <- res$Estimation[res$Statistique == "Mediane"]
  trim <- res$Estimation[res$Statistique == "Moyenne tronquee 10%"]
  tags$div(style = "display:flex;gap:12px;flex-wrap:wrap;",
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:12px;",
      tags$div(style = "font-size:22px;font-weight:700;color:#2980b9;", moy),
      tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "Moyenne")),
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:12px;",
      tags$div(style = "font-size:22px;font-weight:700;color:#27ae60;", med),
      tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "Médiane")),
    tags$div(style = "flex:1;min-width:120px;text-align:center;background:#f8f9fa;border-radius:8px;padding:12px;",
      tags$div(style = "font-size:22px;font-weight:700;color:#8e44ad;", trim),
      tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", "Tronquée 10%"))
  )
})

# ── A2 : Statistiques robustes ────────────────────────────────────────────────
output$desc_robust_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput(ns("desc_robust_var"), "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_robust_res <- reactiveVal(NULL)

observeEvent(input$run_desc_robust, {
  req(input$desc_robust_var)
  d <- df_f()
  req(input$desc_robust_var %in% names(d))
  res <- tryCatch(calc_robust_stats(d[[input$desc_robust_var]]), error = function(e) NULL)
  desc_robust_res(res)
})

output$desc_robust_table <- renderDT({
  req(desc_robust_res())
  datatable(desc_robust_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 20, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("Type",
      backgroundColor = styleEqual(c("Classique", "Robuste", "Diagnostic"),
                                    c("#e8f4fd", "#f0fdf4", "#fef9e7")),
      fontWeight = "bold")
})

# ── A3 : Batterie de normalité ────────────────────────────────────────────────
output$desc_norm_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput(ns("desc_norm_var"), "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_norm_res <- reactiveVal(NULL)

observeEvent(input$run_desc_norm, {
  req(input$desc_norm_var)
  d <- df_f()
  req(input$desc_norm_var %in% names(d))
  withProgress(message = "Tests de normalité...", value = 0.5, {
    res <- tryCatch(
      calc_normality_battery(d[[input$desc_norm_var]], alpha = input$desc_norm_alpha),
      error = function(e) NULL)
    incProgress(0.5)
  })
  desc_norm_res(res)
})

output$desc_norm_table <- renderDT({
  req(desc_norm_res())
  datatable(desc_norm_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 10, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("Conclusion",
      color = styleEqual(c("Normale", "Non normale"), c("#27ae60", "#e74c3c")),
      fontWeight = "bold")
})

output$desc_norm_verdict <- renderUI({
  req(desc_norm_res())
  verdict <- attr(desc_norm_res(), "verdict")
  n_obs <- attr(desc_norm_res(), "n_obs")
  is_normal <- grepl("^NORMALE", verdict)
  col <- if (is_normal) "#27ae60" else "#e74c3c"
  ico <- if (is_normal) "check-circle" else "times-circle"
  tags$div(
    style = paste0("background:", col, "15;border:2px solid ", col,
                   ";border-radius:8px;padding:12px;text-align:center;"),
    tags$div(style = paste0("color:", col, ";font-size:14px;font-weight:700;"),
             icon(ico), " ", verdict),
    tags$div(style = "font-size:11px;color:#555;margin-top:4px;",
             paste0("N = ", n_obs, " observations"))
  )
})

output$desc_norm_qqplot <- renderPlotly({
  req(input$desc_norm_var)
  d <- df_f()
  req(input$desc_norm_var %in% names(d))
  x <- na.omit(as.numeric(d[[input$desc_norm_var]]))
  df_qq <- tibble::tibble(val = x)
  sw <- tryCatch(shapiro.test(x[1:min(5000, length(x))]), error = function(e) NULL)

  p <- ggplot(df_qq, aes(sample = val)) +
    stat_qq(color = "#3498db", alpha = 0.5, size = 1.5) +
    stat_qq_line(color = "#e74c3c", linewidth = 0.8) +
    labs(x = "Quantiles théoriques", y = "Quantiles observés",
         title = paste0("QQ-plot — ", input$desc_norm_var),
         subtitle = if (!is.null(sw))
           paste0("Shapiro-Wilk W=", round(sw$statistic, 4), " p=", round(sw$p.value, 4))
         else "") +
    theme_minimal(base_size = 11)
  ggplotly(p)
})

# ── A4 : Détection outliers ──────────────────────────────────────────────────
output$desc_outlier_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput(ns("desc_outlier_var"), "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_outlier_res <- reactiveVal(NULL)

observeEvent(input$run_desc_outlier, {
  req(input$desc_outlier_var)
  d <- df_f()
  req(input$desc_outlier_var %in% names(d))
  res <- tryCatch(calc_outlier_detection(d[[input$desc_outlier_var]], input$desc_outlier_var),
                  error = function(e) NULL)
  desc_outlier_res(res)
})

output$desc_outlier_table <- renderDT({
  req(desc_outlier_res())
  datatable(desc_outlier_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 10, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("% outliers",
      color = styleInterval(c(5, 10), c("#27ae60", "#f39c12", "#e74c3c")),
      fontWeight = "bold")
})

output$desc_outlier_plot <- renderPlotly({
  req(input$desc_outlier_var)
  d <- df_f()
  req(input$desc_outlier_var %in% names(d))
  plot_outlier_diagnostic(d[[input$desc_outlier_var]], input$desc_outlier_var)
})

# ── A5 : Ajustement de distributions ─────────────────────────────────────────
output$desc_distfit_var_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectInput(ns("desc_distfit_var"), "Variable :", choices = vn, selected = vn[1], width = "100%")
})

desc_distfit_res <- reactiveVal(NULL)

observeEvent(input$run_desc_distfit, {
  req(input$desc_distfit_var)
  d <- df_f()
  req(input$desc_distfit_var %in% names(d))
  withProgress(message = "Ajustement des distributions...", value = 0.5, {
    res <- tryCatch(calc_distribution_fit(d[[input$desc_distfit_var]]),
                    error = function(e) NULL)
    incProgress(0.5)
  })
  desc_distfit_res(res)
})

output$desc_distfit_table <- renderDT({
  req(desc_distfit_res())
  datatable(desc_distfit_res(), rownames = FALSE,
            options = list(dom = "t", pageLength = 10, ordering = FALSE),
            class = "compact stripe hover") |>
    formatStyle("Adequation",
      color = styleEqual(c("Adéquate", "Inadéquate"), c("#27ae60", "#e74c3c")),
      fontWeight = "bold") |>
    formatRound(c("AIC", "BIC", "LogLik"), 2)
})

output$desc_distfit_best <- renderUI({
  req(desc_distfit_res())
  best <- attr(desc_distfit_res(), "best")
  tags$div(
    style = "background:#27ae60;color:white;border-radius:8px;padding:10px;text-align:center;",
    tags$div(style = "font-size:11px;text-transform:uppercase;", "Meilleur ajustement"),
    tags$div(style = "font-size:16px;font-weight:700;", best)
  )
})

output$desc_distfit_plot <- renderPlotly({
  req(input$desc_distfit_var)
  d <- df_f()
  req(input$desc_distfit_var %in% names(d))
  plot_distribution_fit(d[[input$desc_distfit_var]], input$desc_distfit_var)
})

# ── A6 : Profil par groupe ───────────────────────────────────────────────────
output$desc_profile_grp_ui <- renderUI({
  vc <- vars_cat(); req(length(vc) > 0)
  selectInput(ns("desc_profile_grp"), "Grouper par :", choices = vc, selected = vc[1], width = "100%")
})

output$desc_profile_vars_ui <- renderUI({
  vn <- vars_num(); req(length(vn) > 0)
  selectizeInput(ns("desc_profile_vars"), "Variables numériques :",
                 choices = vn, selected = head(vn, 8), multiple = TRUE, width = "100%")
})

desc_profile_res <- reactiveVal(NULL)

observeEvent(input$run_desc_profile, {
  req(input$desc_profile_grp, input$desc_profile_vars)
  d <- df_f()
  res <- tryCatch(calc_group_profile(d, input$desc_profile_vars, input$desc_profile_grp),
                  error = function(e) NULL)
  desc_profile_res(res)
})

output$desc_profile_table <- renderDT({
  req(desc_profile_res())
  datatable(desc_profile_res(), rownames = FALSE,
            options = list(dom = "tip", pageLength = 20, scrollX = TRUE),
            class = "compact stripe hover") |>
    formatRound(c("Moyenne", "Mediane", "Ec-type", "CV (%)", "Min", "Q1", "Q3", "Max", "Skew", "Kurt"), 2)
})

output$desc_profile_heatmap <- renderPlotly({
  req(input$desc_profile_grp, input$desc_profile_vars)
  d <- df_f()
  plot_group_heatmap(d, input$desc_profile_vars, input$desc_profile_grp)
})

# ── A7 : Corrélation avancée ─────────────────────────────────────────────────
output$desc_cor_adv_vars_ui <- renderUI({
  vn <- vars_num(); req(length(vn) >= 2)
  selectizeInput(ns("desc_cor_adv_vars"), "Variables :",
                 choices = vn, selected = head(vn, 8), multiple = TRUE, width = "100%")
})

desc_cor_adv_res <- reactiveVal(NULL)

observeEvent(input$run_desc_cor_adv, {
  req(input$desc_cor_adv_vars, length(input$desc_cor_adv_vars) >= 2)
  d <- df_f()
  withProgress(message = "Calcul des corrélations...", value = 0.5, {
    res <- tryCatch(
      calc_correlation_matrix_advanced(d, input$desc_cor_adv_vars,
                                       method = input$desc_cor_method,
                                       p_adjust = input$desc_cor_padj),
      error = function(e) NULL)
    incProgress(0.5)
  })
  desc_cor_adv_res(res)
})

output$desc_cor_adv_table <- renderDT({
  req(desc_cor_adv_res())
  tbl <- format_cor_table(desc_cor_adv_res())
  req(tbl)
  datatable(tbl, rownames = FALSE,
            options = list(dom = "tip", pageLength = 20, scrollX = TRUE),
            class = "compact stripe hover") |>
    formatRound("r", 4) |>
    formatStyle("Significative",
      color = styleEqual(c("Oui", "Non"), c("#27ae60", "#95a5a6")),
      fontWeight = "bold") |>
    formatStyle("Force",
      backgroundColor = styleEqual(
        c("Très forte", "Forte", "Modérée", "Faible", "Négligeable"),
        c("#e74c3c30", "#f39c1230", "#f1c40f30", "#3498db20", "#ecf0f120")))
})

output$desc_cor_adv_summary <- renderUI({
  req(desc_cor_adv_res())
  res <- desc_cor_adv_res()
  tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
    tags$b("Résumé :"), tags$br(),
    paste0("N = ", res$n, " observations"), tags$br(),
    paste0(res$n_tests, " paires testées"), tags$br(),
    paste0(res$n_sig_raw, " significatives (brut)"), tags$br(),
    paste0(res$n_sig_adj, " significatives (après ", res$p_adjust, ")")
  )
})

output$desc_cor_adv_plot <- renderPlot({
  req(desc_cor_adv_res())
  res <- desc_cor_adv_res()
  # Masquer les non-significatives
  sig_mask <- res$pval_adj < 0.05
  cor_display <- res$cor
  cor_display[!sig_mask & !diag(nrow(sig_mask))] <- NA

  corrplot::corrplot(res$cor, method = "color", type = "upper",
                     addCoef.col = "black", number.cex = 0.65,
                     tl.cex = 0.72, tl.col = "#00426A", tl.srt = 45,
                     col = colorRampPalette(c("#e74c3c", "white", "#2980b9"))(200),
                     p.mat = res$pval_adj, sig.level = 0.05,
                     insig = "cross", pch.cex = 1.5,
                     mar = c(0, 0, 2, 0),
                     title = paste0("Corrélation ", res$method, " (× = non sig. après ", res$p_adjust, ")"))
}, height = 450)

  })
}
