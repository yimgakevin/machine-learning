# Module Shiny — Exploration interactive et statistiques descriptives
# UI et serveur namespacés (ns).
#
# Les blocs historiques manip_* et adv_* (manip_result, manip_add_var,
# manip_reset, adv_result, adv_data_r) ont été retirés : aucun widget
# correspondant n'existe dans l'interface, ils étaient donc inatteignables.

# ── Statistiques numériques complètes ────────────────────────────────────────
# Fonction pure, testable : retourne un tibble Mesure / Valeur.
explor_numeric_stats <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  v <- x[!is.na(x)]
  n <- length(v)
  n_total <- length(x)

  fmt <- function(z, digits = 4) {
    if (length(z) == 0 || is.null(z) || !is.finite(z)) return("N/A")
    formatC(z, format = "f", digits = digits)
  }
  fmt_p <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) return("N/A")
    format.pval(p, digits = 4, eps = 0.0001)
  }

  if (n == 0) {
    return(tibble::tibble(
      Mesure = c("── DONNÉES ──", "N valides", "N manquants", "% manquants"),
      Valeur = c("", "0", as.character(n_total),
                 if (n_total > 0) "100" else "N/A")
    ))
  }

  mean_v   <- mean(v)
  median_v <- median(v)
  mode_v   <- NA_real_
  if (length(v) > 1) {
    u <- unique(v)
    if (length(u) < length(v)) {
      idx <- tabulate(match(v, u))
      if (max(idx) > 1) mode_v <- u[which.max(idx)]
    }
  }
  geo_v    <- if (all(v > 0)) exp(mean(log(v))) else NA_real_
  harm_v   <- if (all(v != 0)) length(v) / sum(1 / v) else NA_real_
  sd_v     <- if (n > 1) stats::sd(v) else NA_real_
  var_v    <- if (n > 1) stats::var(v) else NA_real_
  cv_v     <- if (!is.na(sd_v) && is.finite(mean_v) && mean_v != 0) {
    sd_v / abs(mean_v) * 100
  } else {
    NA_real_
  }
  iqr_v    <- if (n > 1) stats::IQR(v) else NA_real_
  range_v  <- if (n > 1) diff(range(v)) else NA_real_
  mad_v    <- if (n > 1) mean(abs(v - mean_v)) else NA_real_

  qs <- if (n > 1) {
    stats::quantile(v, probs = c(.05, .10, .25, .50, .75, .90, .95),
                    na.rm = TRUE, names = FALSE)
  } else {
    rep(NA_real_, 7)
  }

  skew_v <- if (n > 2 && requireNamespace("e1071", quietly = TRUE)) {
    e1071::skewness(v)
  } else {
    NA_real_
  }
  kurt_v <- if (n > 3 && requireNamespace("e1071", quietly = TRUE)) {
    e1071::kurtosis(v)
  } else {
    NA_real_
  }

  sw <- if (n >= 3 && n <= 5000) {
    tryCatch(stats::shapiro.test(v), error = function(e) NULL)
  } else {
    NULL
  }
  jb_p <- if (n > 3) {
    m3 <- mean((v - mean_v)^3)
    m4 <- mean((v - mean_v)^4)
    s  <- m3 / (sd_v^3)
    k  <- m4 / (sd_v^4) - 3
    stats::pchisq(n * (s^2 / 6 + k^2 / 24), df = 2, lower.tail = FALSE)
  } else {
    NA_real_
  }

  ci_lo <- ci_hi <- NA_real_
  if (n > 1) {
    se <- sd_v / sqrt(n)
    tcrit <- stats::qt(0.975, df = n - 1)
    ci_lo <- mean_v - tcrit * se
    ci_hi <- mean_v + tcrit * se
  }

  tibble::tibble(
    Mesure = c(
      "── TENDANCE CENTRALE ──", "Moyenne (μ)", "Médiane", "Mode",
      "Moyenne géométrique", "Moyenne harmonique",
      "── DISPERSION ──", "Écart-type (σ)", "Variance (σ²)", "CV (%)",
      "IQR", "Étendue", "Écart absolu moyen",
      "── POSITION ──", "Min", "Q1 (25%)", "Q2 (50%)", "Q3 (75%)", "Max",
      "P5", "P10", "P90", "P95",
      "── FORME ──", "Asymétrie (Skewness)", "Kurtosis", "Kurtosis excès",
      "── NORMALITÉ ──", "Shapiro-Wilk W", "Shapiro-Wilk p",
      "Jarque-Bera p", "Distribution",
      "── INTERVALLE DE CONFIANCE ──", "IC 95% moyenne inf",
      "IC 95% moyenne sup",
      "── DONNÉES ──", "N valides", "N manquants", "% manquants"
    ),
    Valeur = c(
      "", fmt(mean_v), fmt(median_v), fmt(mode_v),
      fmt(geo_v), fmt(harm_v),
      "", fmt(sd_v), fmt(var_v), fmt(cv_v),
      fmt(iqr_v), fmt(range_v), fmt(mad_v),
      "", fmt(min(v)), fmt(qs[3]), fmt(qs[4]), fmt(qs[5]), fmt(max(v)),
      fmt(qs[1]), fmt(qs[2]), fmt(qs[6]), fmt(qs[7]),
      "", fmt(skew_v), fmt(kurt_v + 3), fmt(kurt_v),
      "", if (!is.null(sw)) fmt(unname(sw$statistic)) else "N/A",
      if (!is.null(sw)) fmt_p(sw$p.value) else "N/A",
      fmt_p(jb_p),
      if (!is.null(sw)) {
        if (sw$p.value < .05) "❌ Non normale (p<0.05)" else "✅ Normale (p≥0.05)"
      } else {
        "N/A"
      },
      "", fmt(ci_lo), fmt(ci_hi),
      "", as.character(n), as.character(n_total - n),
      formatC((n_total - n) / n_total * 100, format = "f", digits = 1)
    )
  )
}

# ── Statistiques catégorielles avec IC Wilson ───────────────────────────────
explor_categorical_stats <- function(x) {
  x_chr <- as.character(x)
  valid <- !is.na(x_chr) & nzchar(trimws(x_chr))
  n <- sum(valid)
  if (n == 0) {
    return(tibble::tibble(
      Modalité = character(), N = integer(), `%` = numeric(),
      `% cumulé` = numeric(), `IC 95% inf` = numeric(),
      `IC 95% sup` = numeric()
    ))
  }

  tab <- table(x_chr[valid], useNA = "no")
  cnt <- tibble::tibble(
    Modalité = names(tab),
    N = as.integer(tab)
  ) |>
    dplyr::mutate(
      `%` = round(N / sum(N) * 100, 1),
      `% cumulé` = round(cumsum(N) / sum(N) * 100, 1)
    )

  ic <- binom::binom.confint(cnt$N, sum(cnt$N), methods = "wilson")
  cnt$`IC 95% inf` <- round(ic$lower * 100, 1)
  cnt$`IC 95% sup` <- round(ic$upper * 100, 1)
  cnt
}

mod_explor_ui <- function(id) {
  ns <- shiny::NS(id)

      tabItem(tabName="explor",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("search"), "Exploration interactive des données"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Découvrez la structure de vos données : aperçu des premières lignes, types de variables, ",
                 "valeurs manquantes et résumés statistiques. ",
                 tags$b("Point de départ recommandé"), " avant toute analyse.")
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=icon("search"), " Exploration interactive des données",
              fluidRow(
                column(5, uiOutput(ns("explor_cols_ui"))),
                column(2, numericInput(ns("explor_n"),"Lignes/page:",15,5,200,5)),
                column(2, br(),
                       downloadButton(ns("dl_csv"),"📥 CSV",
                                      style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:6px 12px;width:100%;")),
                column(2, br(),
                       downloadButton(ns("dl_xlsx"),"📥 Excel",
                                      style="background:#2E7D52;color:white;border:none;border-radius:6px;padding:6px 12px;width:100%;"))
              ),
              br(),
              DTOutput(ns("explor_table"))
          )
        ),
        fluidRow(
          box(width=4, status="info", solidHeader=TRUE,
              title=icon("calculator"), " Statistiques complètes · IC 95%",
              selectInput(ns("explor_var"),"Variable :",choices=NULL,width="100%"),
              div(style="max-height:400px;overflow-y:auto;",
                  tableOutput(ns("explor_stats")))
          ),
          box(width=8, status="info", solidHeader=TRUE,
              title=icon("chart-bar"), " Graphique automatique selon le type de variable",
              plotlyOutput(ns("explor_auto_plot"), height="360px")
          )
        )
      )
}

mod_explor_server <- function(id, df_f, vars_cat, vars_num,
                              gpal, gprimary, gsecondary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  output$explor_cols_ui <- renderUI({
    df <- df_f(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput(ns("explor_cols"),"Colonnes à afficher :",
                   choices=vis, selected=head(vis,8),
                   multiple=TRUE, width="100%")
  })

  output$explor_table <- renderDT({
    req(input$explor_cols)
    df_f() |> select(any_of(input$explor_cols)) |>
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=input$explor_n, scrollX=TRUE, dom="frtip"),
                class="compact stripe hover")
  })

  observe({
    vn <- vars_num()
    updateSelectInput(session,"explor_var", choices=vn, selected=vn[1])
  })

  output$explor_stats <- renderTable({
    req(input$explor_var)
    x <- df_f()[[input$explor_var]]
    if (is.numeric(x)) {
      explor_numeric_stats(x)
    } else {
      explor_categorical_stats(x)
    }
  }, striped=TRUE, hover=TRUE, bordered=TRUE, spacing="xs")

  output$explor_auto_plot <- renderPlotly({
    req(input$explor_var); d <- df_f(); v <- input$explor_var
    x <- d[[v]]
    tp <- var_type(x)
    if (tp %in% c("continue","discrete")) {
      m_v <- mean(x,na.rm=TRUE); s_v <- sd(x,na.rm=TRUE); md_v <- median(x,na.rm=TRUE)
      p <- ggplot(d, aes(x=.data[[v]])) +
        geom_histogram(aes(y=after_stat(density)),bins=28,fill=gprimary(),color="white",alpha=0.82)+
        stat_function(fun=dnorm,args=list(mean=m_v,sd=s_v),color="black",linewidth=1.1)+
        geom_vline(xintercept=m_v,color=gsecondary(),linewidth=1,linetype="dashed")+
        geom_vline(xintercept=md_v,color=C$orange,linewidth=1,linetype="dotted")+
        labs(x=v,y="Densité",
             subtitle=paste0("μ=",round(m_v,3)," | σ=",round(s_v,3)," | Méd=",round(md_v,3)),
             caption="Rouge=moyenne | Orange=médiane | Courbe=N(μ,σ)")+
        theme_nutri()
    } else {
      cnt <- d |> count(.data[[v]]) |> mutate(pct=round(n/sum(n)*100,1))
      cnt <- smart_top_n(cnt, v, "n", 25)
      niv <- nrow(cnt)
      cnt[[v]] <- truncate_label(cnt[[v]], 22)
      p <- ggplot(cnt,aes(y=fct_reorder(.data[[v]],n),x=n,fill=.data[[v]],
                          text=paste0(.data[[v]],"\nN=",n," (",pct,"%)"))) +
        geom_col(alpha=0.88,width=0.7)+
        scale_fill_manual(values=gpal(niv,"cat"))+
        labs(y="",x="Effectif")+
        theme_nutri()+theme(legend.position="none")
    }
    ggplotly(p, tooltip=if(tp %in% c("continue","discrete")) NULL else "text") |>
      layout(hovermode="x unified") |> nutri_config()
  })

  output$dl_csv <- downloadHandler(
    filename=function() paste0("data_",Sys.Date(),".csv"),
    content=function(f) write.csv(df_f()|>select(!starts_with(".")),f,row.names=FALSE)
  )
  output$dl_xlsx <- downloadHandler(
    filename=function() paste0("data_",Sys.Date(),".xlsx"),
    content=function(f) write_xlsx(df_f()|>select(!starts_with(".")),f)
  )

  # ── Statistiques complètes par sections ───────────────────────────────────
  observe({
    all_v <- c(vars_num(), vars_cat())
    updateSelectInput(session,"explor_var", choices=all_v,
                      selected=if(length(vars_num())>0) vars_num()[1] else all_v[1])
  })
  })
}
