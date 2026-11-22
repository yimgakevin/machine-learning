# Extrait de app_v2_final.R — analyses univariées
  # ── UNIVARIÉ ────────────────────────────────────────────────────────────────
  output$uni_var_ui <- renderUI({
    type <- input$uni_type
    if (type %in% c("barplot","barplot_pct","pie","mosaic")) {
      ch <- vars_cat_pie()
      if (type %in% c("barplot","barplot_pct")) ch <- vars_cat()
      req(length(ch)>0)
      selectInput("uni_var","Variable :",choices=ch,selected=ch[1],width="100%")
    } else {
      vn <- vars_num(); req(length(vn)>0)
      selectInput("uni_var","Variable :",choices=vn,selected=vn[1],width="100%")
    }
  })

  output$uni_stats_tbl <- renderTable({
    req(input$uni_var); x <- df_f()[[input$uni_var]]
    if(is.numeric(x)) stats_complete(x)
    else {
      cnt <- df_f()|>count(.data[[input$uni_var]])|>mutate(pct=round(n/sum(n)*100,1))
      setNames(cnt, c("Modalité","N","%"))
    }
  }, striped=TRUE, bordered=TRUE, spacing="xs")

  make_uni_plotly <- function() {
    req(input$uni_var); d <- df_f(); v <- input$uni_var; type <- input$uni_type
    pal <- function(n) {
      up <- input$uni_pal
      if (is.null(up) || up == "Auto (Global)") return(gpal(n, "cat"))
      pal_fn_uni(n, up)
    }

    tryCatch({
      if (type == "barplot") {
        cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
        cnt <- smart_top_n(cnt, v, "n", 25)
        cnt[[v]] <- truncate_label(cnt[[v]], 22)
        p <- ggplot(cnt,aes(y=fct_reorder(.data[[v]],n),x=n,fill=.data[[v]],
                            text=paste0(.data[[v]],"\nN=",n," (",pct,"%)"))) +
          geom_col(alpha=.88,width=.7)+
          {if(input$uni_lbl) geom_text(aes(label=paste0(n," (",pct,"%)")),
                                       hjust=-0.08,size=3)}+
          scale_fill_manual(values=pal(nrow(cnt)))+
          labs(y="",x="Effectif",title=paste("Distribution de",v))+
          theme_nutri()+theme(legend.position="none")
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "barplot_pct") {
        cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
        cnt <- smart_top_n(cnt, v, "n", 25)
        cnt[[v]] <- truncate_label(cnt[[v]], 22)
        p <- ggplot(cnt,aes(y=fct_reorder(.data[[v]],pct),x=pct,fill=.data[[v]],
                            text=paste0(.data[[v]],"\n",pct,"% (N=",n,")"))) +
          geom_col(alpha=.88,width=.7)+
          {if(input$uni_lbl) geom_text(aes(label=paste0(pct,"%")),hjust=-0.08,size=3)}+
          scale_fill_manual(values=pal(nrow(cnt)))+
          labs(y="",x="Proportion (%)",title=paste("Proportions —",v))+
          theme_nutri()+theme(legend.position="none")
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "pie") {
        cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
        plot_ly(cnt,labels=~.data[[v]],values=~n,type="pie",hole=0.45,
                textinfo="label+percent",
                marker=list(colors=pal(nrow(cnt)),line=list(color="white",width=2)),
                hovertemplate="%{label}<br>N=%{value}<br>%{percent}<extra></extra>")|>
          layout(showlegend=TRUE,legend=list(orientation="h",y=-0.15),
                 title=paste("Distribution de",v)) |> nutri_config()

      } else if (type == "histo") {
        m_v <- mean(d[[v]],na.rm=T);s_v <- sd(d[[v]],na.rm=T);md_v <- median(d[[v]],na.rm=T)
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_histogram(aes(y=after_stat(density)),bins=30,
                         fill=gprimary(),color="white",alpha=.82)+
          stat_function(fun=dnorm,args=list(mean=m_v,sd=s_v),color="black",linewidth=1.1)+
          geom_vline(xintercept=m_v,color=gsecondary(),linewidth=1,linetype="dashed")+
          geom_vline(xintercept=md_v,color=C$orange,linewidth=1,linetype="dotted")+
          labs(x=v,y="Densité",
               subtitle=paste0("μ=",round(m_v,3)," | σ=",round(s_v,3)),
               caption="Rouge=moyenne | Orange=médiane | Courbe=N(μ,σ)")+
          theme_nutri()
        ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()

      } else if (type == "boxplot") {
        q <- quantile(d[[v]],c(.25,.5,.75),na.rm=TRUE)
        p <- ggplot(d,aes(x="",y=.data[[v]]))+
          geom_boxplot(fill=gprimary(),color="#004f7c",alpha=.75,
                       outlier.shape=21,outlier.color=C$rouge,outlier.size=2,width=.4)+
          stat_summary(fun=mean,geom="point",shape=18,size=5,color=C$rouge)+
          annotate("text",x=1.35,y=q[1],label=paste0("Q1=",round(q[1],2)),size=3.5)+
          annotate("text",x=1.35,y=q[2],label=paste0("Méd=",round(q[2],2)),size=3.5)+
          annotate("text",x=1.35,y=q[3],label=paste0("Q3=",round(q[3],2)),size=3.5)+
          annotate("point",x=1,y=mean(d[[v]],na.rm=TRUE),color=C$rouge,size=4,shape=18)+
          labs(x="",y=v,subtitle=paste0("IQR=",round(q[3]-q[1],2),
                                         " | Étendue=",round(diff(range(d[[v]],na.rm=TRUE)),2),
                                         " | Moy=",round(mean(d[[v]],na.rm=TRUE),2)))+
          theme_nutri()
        ggplotly(p) |> nutri_config()

      } else if (type == "violin") {
        p <- ggplot(d,aes(x="",y=.data[[v]]))+
          geom_violin(fill=gprimary(),alpha=.45,trim=FALSE,color=C$violet,linewidth=.8)+
          geom_boxplot(fill="white",color="#2c3e50",width=.1,
                       outlier.shape=21,outlier.color=C$rouge)+
          stat_summary(fun=mean,geom="point",shape=18,size=4,color=C$rouge)+
          labs(x="",y=v)+theme_nutri()
        ggplotly(p) |> nutri_config()

      } else if (type == "density") {
        m_v <- mean(d[[v]],na.rm=TRUE)
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_density(fill=gprimary(),color=gprimary(),alpha=.38,linewidth=.9)+
          geom_area(stat="function",fun=dnorm,
                    args=list(mean=m_v,sd=sd(d[[v]],na.rm=TRUE)),
                    fill="transparent",color="black",linewidth=.8,linetype="dashed")+
          geom_vline(xintercept=m_v,color=gsecondary(),linewidth=1,linetype="dashed")+
          geom_vline(xintercept=median(d[[v]],na.rm=TRUE),
                     color=C$orange,linewidth=1,linetype="dotted")+
          labs(x=v,y="Densité",caption="Bleu=observé | Noir pointillé=N(μ,σ)")+
          theme_minimal(base_size=11)
        ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()

      } else if (type == "qqplot") {
        sw <- tryCatch(shapiro.test(na.omit(d[[v]])[1:min(5000,sum(!is.na(d[[v]])))]),
                       error=function(e) NULL)
        p <- ggplot(d,aes(sample=.data[[v]]))+
          stat_qq(color=gprimary(),alpha=.6,size=1.8)+
          stat_qq_line(color=C$rouge,linewidth=1)+
          labs(x="Quantiles théoriques",y="Quantiles observés",
               title=paste0("QQ-plot — ",v),
               subtitle=if(!is.null(sw))
                 paste0("Shapiro-Wilk: W=",round(sw$statistic,4),
                        "  p=",round(sw$p.value,4),
                        if(sw$p.value<.05)"  → NON NORMALE ❌" else "  → Normale ✅")
               else "")+
          theme_minimal(base_size=11)
        ggplotly(p) |> nutri_config()

      } else if (type == "oms_ref") {
        x_ref <- seq(min(d[[v]],na.rm=TRUE)-1, max(d[[v]],na.rm=TRUE)+1, length.out=300)
        ref_df <- tibble(x=x_ref, y=dnorm(x_ref,0,1))
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_density(aes(fill="Données observées",color="Données observées"),
                       alpha=.38,linewidth=.9)+
          geom_line(data=ref_df,aes(x=x,y=y,color="Référence OMS N(0,1)"),
                    linewidth=1,linetype="dashed",inherit.aes=FALSE)+
          scale_fill_manual(values=c("Données observées"=C$rouge,"Référence OMS N(0,1)"="transparent"))+
          scale_color_manual(values=c("Données observées"=C$rouge,"Référence OMS N(0,1)"="black"))+
          labs(x=v,y="Densité",
               subtitle="Décalage à gauche = prévalence élevée de malnutrition")+
          theme_minimal(base_size=11)+theme(legend.position="bottom")
        ggplotly(p) |> nutri_config()

      } else if (type == "histo_dens") {
        m_v <- mean(d[[v]],na.rm=TRUE); s_v <- sd(d[[v]],na.rm=TRUE)
        p <- ggplot(d,aes(x=.data[[v]]))+
          geom_histogram(aes(y=after_stat(density)),bins=28,
                         fill=gprimary(),color="white",alpha=.55)+
          geom_density(color=C$rouge,linewidth=1.2,fill=C$rouge,alpha=.2)+
          stat_function(fun=dnorm,args=list(mean=m_v,sd=s_v),
                        color="black",linewidth=.9,linetype="dashed")+
          labs(x=v,y="Densité")+theme_minimal(base_size=11)
        ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()

      } else if (type == "boxjitter") {
        p <- ggplot(d,aes(x="",y=.data[[v]]))+
          geom_boxplot(fill=gprimary(),color="#004f7c",alpha=.6,width=.35,
                       outlier.shape=NA)+
          geom_jitter(width=.12,height=0,alpha=.35,size=1.5,color=gprimary())+
          stat_summary(fun=mean,geom="point",shape=18,size=5,color=C$rouge)+
          labs(x="",y=v)+theme_minimal(base_size=11)
        ggplotly(p) |> nutri_config()

      } else {
        plotly_empty() |> layout(title=list(text="Sélectionnez un type de graphique",x=.5))
      }
    }, error=function(e)
      plotly_empty()|>layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  }

  output$uni_plot_ui <- renderUI({
    plotlyOutput("uni_plot_out", height="500px")
  })
  output$uni_plot_out <- renderPlotly({ make_uni_plotly() })

  output$uni_dl_plot <- downloadHandler(
    filename=function() paste0("graphique_",input$uni_var,"_",Sys.Date(),".png"),
    content=function(f) {
      req(input$uni_var); d <- df_f(); v <- input$uni_var
      ggsave(f, plot=last_plot(), width=12, height=8, dpi=300)
    }
  )

