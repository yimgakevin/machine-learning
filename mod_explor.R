# Module Shiny — Exploration interactive et statistiques descriptives
# UI et serveur namespacés (ns).
#
# Les blocs historiques manip_* et adv_* (manip_result, manip_add_var,
# manip_reset, adv_result, adv_data_r) ont été retirés : aucun widget
# correspondant n'existe dans l'interface, ils étaient donc inatteignables.

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
              title=icon("calculator"), " Statistiques complètes",
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
    req(input$explor_var); x <- df_f()[[input$explor_var]]
    if (is.numeric(x)) {
      v <- na.omit(x)
      sw <- tryCatch(shapiro.test(v[1:min(5000,length(v))]), error=function(e) NULL)
      jb_p <- tryCatch({
        n <- length(v); m3 <- mean((v-mean(v))^3); m4 <- mean((v-mean(v))^4)
        s <- m3/(sd(v)^3); k <- m4/(sd(v)^4)-3
        jb_stat <- n*(s^2/6 + k^2/24)
        round(pchisq(jb_stat, df=2, lower.tail=FALSE), 4)
      }, error=function(e) NA)
      tibble(
        Mesure = c(
          "── TENDANCE CENTRALE ──","Moyenne (μ)","Médiane","Mode","Moyenne géométrique","Moyenne harmonique",
          "── DISPERSION ──","Écart-type (σ)","Variance (σ²)","CV (%)","IQR","Étendue","Écart absolu moyen",
          "── POSITION ──","Min","Q1 (25%)","Q2 (50%)","Q3 (75%)","Max","P5","P10","P90","P95",
          "── FORME ──","Asymétrie (Skewness)","Aplatissement (Kurtosis)","Kurtosis excès",
          "── NORMALITÉ ──","Shapiro-Wilk W","Shapiro-Wilk p","Jarque-Bera p","Distribution",
          "── DONNÉES ──","N valides","N manquants","% manquants"
        ),
        Valeur = c(
          "","round(mean(v),4)","round(median(v),4)",
          as.character(round(as.numeric(e1071::moment(v,order=1,center=FALSE)),4)),
          round(exp(mean(log(v[v>0]))),4),
          round(length(v)/sum(1/v[v!=0]),4),
          "",round(sd(v),4),round(var(v),4),round(sd(v)/mean(v)*100,2),
          round(IQR(v),4),round(diff(range(v)),4),round(mean(abs(v-mean(v))),4),
          "",round(min(v),4),round(quantile(v,.25),4),round(median(v),4),
          round(quantile(v,.75),4),round(max(v),4),
          round(quantile(v,.05),4),round(quantile(v,.10),4),
          round(quantile(v,.90),4),round(quantile(v,.95),4),
          "",round(e1071::skewness(v),4),round(e1071::kurtosis(v)+3,4),round(e1071::kurtosis(v),4),
          "",if(!is.null(sw))round(sw$statistic,4) else "N/A",
          if(!is.null(sw))round(sw$p.value,4) else "N/A",
          if(!is.na(jb_p))jb_p else "N/A",
          if(!is.null(sw)) if(sw$p.value<.05)"❌ Non normale (p<0.05)" else "✅ Normale (p≥0.05)" else "N/A",
          "",length(v),sum(is.na(x)),round(mean(is.na(x))*100,1)
        )
      ) |> filter(Mesure != "" | Valeur != "")
    } else {
      cnt <- df_f() |> count(.data[[input$explor_var]]) |>
        mutate(Pct=round(n/sum(n)*100,1), Cum=cumsum(Pct))
      tibble(Modalité=as.character(cnt[[input$explor_var]]),
             N=cnt$n, `%`=cnt$Pct, `% cumulé`=cnt$Cum)
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
