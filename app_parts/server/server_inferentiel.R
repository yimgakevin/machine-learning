# Extrait de app_v2_final.R — inférentiel unifié
  # ── INFÉRENTIEL — Module unifié (tous les tests à la fois) ──────────────────

  # Sélecteurs de variables
  output$infer_x_ui <- renderUI({
    all_vars <- c(vars_cat(), vars_num())
    selectInput("infer_x", "Variable X :", choices = all_vars,
                selected = all_vars[1], width = "100%")
  })
  output$infer_y_ui <- renderUI({
    all_vars <- c(vars_num(), vars_cat())
    sel <- if (length(all_vars) > 1) all_vars[2] else all_vars[1]
    selectInput("infer_y", "Variable Y :", choices = all_vars,
                selected = sel, width = "100%")
  })

  # Badge type de combinaison (réactif)
  output$infer_type_badge <- renderUI({
    req(input$infer_x, input$infer_y)
    d <- df_f()
    req(input$infer_x %in% names(d), input$infer_y %in% names(d))
    detect_combination_badge(input$infer_x, input$infer_y, d)
  })

  # Stockage des résultats
  infer_results <- reactiveVal(NULL)

  observeEvent(input$run_infer, {
    req(input$infer_x, input$infer_y)
    d <- df_f()
    req(input$infer_x %in% names(d), input$infer_y %in% names(d))
    alpha <- input$infer_alpha %||% 0.05

    withProgress(message = "Exécution de tous les tests...", value = 0.5, {
      res <- tryCatch(
        run_all_tests(d, input$infer_x, input$infer_y, alpha),
        error = function(e) list(error = e$message)
      )
      incProgress(0.5)
    })
    infer_results(res)
  })

  # Tableau récapitulatif
  output$infer_summary_table <- renderDT({
    res <- infer_results()
    req(!is.null(res), length(res) > 0)
    if (!is.null(res$error)) {
      return(datatable(data.frame(Erreur = res$error), rownames = FALSE))
    }
    df_summ <- render_summary_df(res)
    datatable(df_summ, rownames = FALSE,
              options = list(dom = "t", pageLength = 10, ordering = FALSE),
              class = "compact stripe hover",
              escape = FALSE) |>
      formatStyle("Valide",
                  backgroundColor = styleEqual(
                    c("\u2705 Oui", "\u274c Non"),
                    c("#f0fdf4", "#fef2f2")),
                  fontWeight = "bold") |>
      formatStyle("D\u00e9cision",
                  color = styleEqual(
                    c("\U0001f534 Significatif", "\U0001f7e2 Non significatif", "\u2b1c Non applicable"),
                    c("#BF3636", "#2E7D52", "#6B7B8D")),
                  fontWeight = "bold")
  })

  # Cartes détaillées
  output$infer_all_cards <- renderUI({
    res <- infer_results()
    req(!is.null(res), length(res) > 0)
    if (!is.null(res$error)) {
      return(tags$div(style="color:#BF3636;padding:20px;",
                      tags$h4("Erreur :"), tags$p(res$error)))
    }
    render_all_cards(res)
  })

  # Graphique
  output$infer_plot <- renderPlotly({
    req(infer_results(), length(infer_results()) > 0)
    req(is.null(infer_results()$error))
    d <- df_f()
    req(input$infer_x %in% names(d), input$infer_y %in% names(d))
    make_infer_plot(d, input$infer_x, input$infer_y, input$infer_alpha %||% 0.05)
  })

  # Multi-variables (régression logistique — conservée)
  output$multi_outcome_ui <- renderUI({
    opts <- intersect(c("GAM","SAM","Stunting","Underweight"),names(df_f()))
    if(length(opts)==0) opts <- vars_cat()
    selectInput("multi_out","Outcome :",choices=opts,width="100%")
  })
  output$multi_vars_ui <- renderUI({
    vn <- vars_num()
    checkboxGroupInput("multi_vars","Variables explicatives :",choices=vn,selected=head(vn,6))
  })
  output$multi_summary <- renderPrint({
    req(input$multi_vars,input$multi_out); d<-df_f()
    tryCatch({
      d_ml <- d|>select(all_of(c(input$multi_out,input$multi_vars)))|>drop_na()
      fm <- as.formula(paste(input$multi_out,"~",paste(input$multi_vars,collapse="+")))
      mod <- glm(fm,data=d_ml,family=binomial)
      cat("Régression logistique\n")
      cat("N=",nrow(d_ml),"\n\n")
      print(round(exp(cbind(OR=coef(mod),confint.default(mod))),3))
    },error=function(e) cat("Erreur:",e$message))
  })
  output$multi_forest <- renderPlotly({
    req(input$multi_vars,input$multi_out); d<-df_f()
    tryCatch({
      d_ml <- d|>select(all_of(c(input$multi_out,input$multi_vars)))|>drop_na()
      fm <- as.formula(paste(input$multi_out,"~",paste(input$multi_vars,collapse="+")))
      mod <- glm(fm,data=d_ml,family=binomial)
      coefs <- coef(mod)[-1]; ci <- confint.default(mod)[-1,]
      pvals <- summary(mod)$coefficients[-1,4]
      or_df <- tibble(
        Variable=names(coefs),
        OR=exp(coefs), CI_low=exp(ci[,1]), CI_high=exp(ci[,2]),
        p=pvals, Sig=p<.05
      ) |> filter(is.finite(OR),OR<50)
      p <- ggplot(or_df,aes(x=OR,y=fct_reorder(Variable,OR),color=Sig,
        text=paste0(Variable,"\nOR=",round(OR,3),"\nIC95=[",round(CI_low,3),"-",round(CI_high,3),"]","\np=",round(p,4))))+
        geom_vline(xintercept=1,linetype="dashed",color="gray50")+
        geom_errorbarh(aes(xmin=CI_low,xmax=CI_high),height=.3,linewidth=.8)+
        geom_point(size=3.5)+
        scale_color_manual(values=c("TRUE"=C$rouge,"FALSE"=C$gris),
                           labels=c("TRUE"="Significatif (p<.05)","FALSE"="NS"))+
        scale_x_log10()+
        labs(x="Odds Ratio (log scale)",y="",
             subtitle="OR>1 = facteur de risque | OR<1 = facteur protecteur")+
        theme_minimal(base_size=10)
      ggplotly(p,tooltip="text") |> nutri_config()
    },error=function(e)
      plotly_empty()|>layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

