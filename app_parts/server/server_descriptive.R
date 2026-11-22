# Extrait de app_v2_final.R — descriptive OMS
  # ── DESCRIPTIVE OMS ─────────────────────────────────────────────────────────
  output$desc_by_ui <- renderUI({
    vc <- vars_cat()
    selectInput("desc_by","Stratifier par :",
                choices=c("Aucune"="none",setNames(vc,vc)),
                selected="none",width="100%")
  })
  output$desc_vars_ui <- renderUI({
    df <- df_f(); vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("desc_vars","Variables :",choices=vis,selected=head(vis,14),
                   multiple=TRUE,width="100%")
  })
  output$desc_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("desc_grp","Grouper par :",choices=vc,selected=vc[1],width="100%")
  })
  output$desc_corr_vars_ui <- renderUI({
    vn <- vars_num()
    selectizeInput("desc_corr_vars","Variables pour corrélation :",
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

