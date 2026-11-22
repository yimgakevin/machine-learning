# Extrait de app_v2_final.R — analyses bivariées
  # ── BIVARIÉ ─────────────────────────────────────────────────────────────────
  output$biv_x_ui <- renderUI({
    vc <- vars_cat(); req(length(vc)>0)
    selectInput("biv_x","Variable X / Groupe :",choices=vc,selected=vc[1],width="100%")
  })
  output$biv_y_ui <- renderUI({
    type <- input$biv_type
    if (type %in% c("dodge","stack","heatfreq","mosaic")) {
      vc <- vars_cat()
      selectInput("biv_y","Variable Y (catégorielle) :",
                  choices=vc, selected=if(length(vc)>1)vc[2] else vc[1], width="100%")
    } else if (type %in% c("scatter","hexbin","cormap")) {
      vn <- vars_num()
      tagList(
        selectInput("biv_y","Variable Y (continue) :",
                    choices=vn, selected=if(length(vn)>1)vn[2] else vn[1], width="100%"),
        selectInput("biv_z","Couleur par :",
                    choices=c("Aucune",vars_cat()), selected="Aucune", width="100%")
      )
    } else {
      vn <- vars_num()
      selectInput("biv_y","Variable continue :",choices=vn,selected=vn[1],width="100%")
    }
  })

  output$biv_test_result <- renderPrint({
    req(input$biv_x, input$biv_y); d <- df_f()
    tp_x <- var_type(d[[input$biv_x]])
    tp_y <- var_type(d[[input$biv_y]])
    res  <- auto_test(d, input$biv_x, input$biv_y, tp_x, tp_y)
    if (!is.null(res)) {
      cat("Test :", res$nom, "\n")
      if(!is.na(res$stat)) cat("Statistique :", res$stat, "\n")
      if(!is.na(res$df)) cat("ddl :", res$df, "\n")
      cat("p-valeur :", round(res$p,4), "\n\n")
      cat(res$interpretation)
    }
  })

  output$biv_interpretation <- renderUI({
    req(input$biv_x, input$biv_y); d <- df_f()
    tp_x <- var_type(d[[input$biv_x]])
    tp_y <- var_type(d[[input$biv_y]])
    res  <- auto_test(d, input$biv_x, input$biv_y, tp_x, tp_y)
    if (is.null(res)) return(NULL)
    cls  <- if(!is.na(res$p) && res$p < 0.05) "alerte-crit" else "alerte-ok"
    tags$div(class=paste("alerte-box",cls),
             style="margin-top:8px;font-size:12px;",
             res$interpretation)
  })

  output$biv_plot <- renderPlotly({
    req(input$biv_x, input$biv_y); d <- df_f()
    v1 <- input$biv_x; v2 <- input$biv_y; type <- input$biv_type
    niv1 <- length(unique(na.omit(d[[v1]])))

    gpalf <- function(n) {
      bp <- input$biv_pal
      if (is.null(bp) || bp == "Auto (Global)") return(gpal(n, "cat"))
      switch(bp,
        "Auto (OMS)" = gpal(n, "cat"),
        "Viridis"    = viridis::viridis(n),
        "Set2"       = RColorBrewer::brewer.pal(min(n, 8), "Set2"),
        "Spectral"   = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n),
        "RdYlGn"     = colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))(n),
        gpal(n, "cat")
      )
    }

    tryCatch({
      if (type == "dodge") {
        cnt <- d|>count(.data[[v1]],.data[[v2]])|>mutate(pct=round(n/sum(n)*100,1))
        niv2 <- length(unique(na.omit(d[[v2]])))
        cnt[[v1]] <- truncate_label(cnt[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(cnt,aes(y=.data[[v1]],x=n,fill=.data[[v2]],
            text=paste0(v1,"=",.data[[v1]],"\n",v2,"=",.data[[v2]],"\nN=",n," (",pct,")")))+
            geom_col(position="dodge",width=.72,alpha=.88)+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            labs(y=v1,x="Effectif")+theme_nutri()
        } else {
          p <- ggplot(cnt,aes(x=.data[[v1]],y=n,fill=.data[[v2]],
            text=paste0(v1,"=",.data[[v1]],"\n",v2,"=",.data[[v2]],"\nN=",n," (",pct,")")))+
            geom_col(position="dodge",width=.72,alpha=.88)+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            labs(x=v1,y="Effectif")+theme_nutri()+smart_axis_x(niv1)
        }
        ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.3)) |> nutri_config()

      } else if (type == "stack") {
        cnt <- d|>count(.data[[v1]],.data[[v2]])|>
          group_by(.data[[v1]])|>mutate(pct=round(n/sum(n)*100,1))|>ungroup()
        niv2 <- length(unique(na.omit(d[[v2]])))
        cnt[[v1]] <- truncate_label(cnt[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(cnt,aes(y=.data[[v1]],x=pct,fill=.data[[v2]],
            text=paste0(.data[[v1]]," | ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
            geom_col(position=position_fill(),width=.65,alpha=.88)+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            scale_x_continuous(labels=scales::percent)+
            labs(y=v1,x="Proportion")+theme_nutri()
        } else {
          p <- ggplot(cnt,aes(x=.data[[v1]],y=pct,fill=.data[[v2]],
            text=paste0(.data[[v1]]," | ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
            geom_col(position=position_fill(),width=.65,alpha=.88)+
            geom_text(aes(label=paste0(pct,"%")),
                      position=position_fill(vjust=.5),size=3,color="white",fontface="bold")+
            scale_fill_manual(values=gpalf(niv2),name=v2)+
            scale_y_continuous(labels=scales::percent)+
            labs(x=v1,y="Proportion")+theme_nutri()+smart_axis_x(niv1)
        }
        ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.3)) |> nutri_config()

      } else if (type == "heatfreq") {
        cnt <- d|>count(.data[[v1]],.data[[v2]])|>
          group_by(.data[[v1]])|>mutate(pct=round(n/sum(n)*100,1))|>ungroup()
        cnt[[v1]] <- truncate_label(cnt[[v1]], 18)
        cnt[[v2]] <- truncate_label(cnt[[v2]], 18)
        p <- ggplot(cnt,aes(x=.data[[v2]],y=.data[[v1]],fill=pct,
          text=paste0(.data[[v1]]," × ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
          geom_tile(color="white",linewidth=1.2)+
          geom_text(aes(label=paste0(pct,"%")),size=3.5,fontface="bold",color="white")+
          scale_fill_gradientn(colors=c("#f0f8ff",C$bleu,C$rouge),name="%")+
          labs(x=v2,y=v1)+theme_nutri()+
          smart_axis_x(length(unique(cnt[[v2]])))
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "boxgrp") {
        d[[v1]] <- truncate_label(d[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(d,aes(y=.data[[v1]],x=.data[[v2]],fill=.data[[v1]],
            text=paste0(.data[[v1]],"\n",v2,"=",round(.data[[v2]],2))))+
            geom_boxplot(alpha=.75,outlier.shape=21,outlier.color=C$rouge,width=.55)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color="white")+
            scale_fill_manual(values=gpalf(niv1))+
            labs(y=v1,x=v2)+theme_nutri()+theme(legend.position="none")
        } else {
          p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]],
            text=paste0(.data[[v1]],"\n",v2,"=",round(.data[[v2]],2))))+
            geom_boxplot(alpha=.75,outlier.shape=21,outlier.color=C$rouge,width=.55)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color="white")+
            scale_fill_manual(values=gpalf(niv1))+
            labs(x=v1,y=v2)+theme_nutri()+smart_axis_x(niv1)+
            theme(legend.position="none")
        }
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "violgrp") {
        d[[v1]] <- truncate_label(d[[v1]], 20)
        if (niv1 > 10) {
          p <- ggplot(d,aes(y=.data[[v1]],x=.data[[v2]],fill=.data[[v1]]))+
            geom_violin(alpha=.5,trim=FALSE,linewidth=.7)+
            geom_boxplot(fill="white",width=.1,outlier.shape=21,outlier.color=C$rouge)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color=C$rouge)+
            scale_fill_manual(values=gpalf(niv1))+
            labs(y=v1,x=v2)+theme_nutri()+theme(legend.position="none")
        } else {
          p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]]))+
            geom_violin(alpha=.5,trim=FALSE,linewidth=.7)+
            geom_boxplot(fill="white",width=.1,outlier.shape=21,outlier.color=C$rouge)+
            stat_summary(fun=mean,geom="point",shape=18,size=3.5,color=C$rouge)+
            scale_fill_manual(values=gpalf(niv1))+
            labs(x=v1,y=v2)+theme_nutri()+smart_axis_x(niv1)+
            theme(legend.position="none")
        }
        ggplotly(p) |> nutri_config()

      } else if (type == "densgrp") {
        p <- ggplot(d,aes(x=.data[[v2]],fill=.data[[v1]],color=.data[[v1]]))+
          geom_density(alpha=.28,linewidth=.8)+
          scale_fill_manual(values=gpalf(niv1),name=v1)+
          scale_color_manual(values=gpalf(niv1),name=v1)+
          labs(x=v2,y="Densité")+theme_nutri()
        ggplotly(p)|>layout(legend=list(orientation="h",y=-0.2)) |> nutri_config()

      } else if (type == "facethist") {
        p <- ggplot(d,aes(x=.data[[v2]],fill=.data[[v1]]))+
          geom_histogram(aes(y=after_stat(density)),bins=25,color="white",alpha=.82)+
          scale_fill_manual(values=gpalf(niv1))+
          facet_wrap(as.formula(paste("~",v1)),ncol=min(3,niv1),scales="free_y")+
          labs(x=v2,y="Densité")+theme_nutri(10)+theme(legend.position="none")
        ggplotly(p) |> nutri_config()

      } else if (type == "halfeye") {
        d_h <- d; d_h[[v1]] <- truncate_label(d_h[[v1]], 20)
        p <- ggplot(d_h,aes(x=.data[[v2]],y=.data[[v1]],fill=.data[[v1]]))+
          ggdist::stat_halfeye(alpha=.75)+
          scale_fill_manual(values=gpalf(niv1))+
          labs(x=v2,y=v1)+theme_nutri()+theme(legend.position="none")
        ggplotly(p) |> nutri_config()

      } else if (type == "scatter") {
        v3  <- if(!is.null(input$biv_z)&&input$biv_z!="Aucune") input$biv_z else NULL
        rho <- cor.test(d[[v1]],d[[v2]],method="spearman",exact=FALSE)
        niv3 <- if(!is.null(v3)) length(unique(na.omit(d[[v3]]))) else 0
        p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],
          color=if(!is.null(v3)).data[[v3]] else NULL,
          text=paste0(v1,"=",round(.data[[v1]],2),"\n",v2,"=",round(.data[[v2]],2))))+
          geom_point(alpha=.45,size=1.8)+
          geom_smooth(method="loess",se=TRUE,color="black",linewidth=.9,aes(group=1))+
          {if(!is.null(v3)) scale_color_manual(values=gpalf(niv3))}+
          labs(x=v1,y=v2,
               subtitle=paste0("ρ Spearman=",round(rho$estimate,3),
                               "  p=",round(rho$p.value,4)))+
          theme_nutri()
        ggplotly(p,tooltip="text") |> nutri_config()

      } else if (type == "hexbin") {
        p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]]))+
          geom_hex(bins=25)+
          scale_fill_gradientn(colors=c("#f0f8ff",C$bleu,C$rouge),name="Count")+
          labs(x=v1,y=v2)+theme_nutri()
        ggplotly(p) |> nutri_config()

      } else if (type == "cormap") {
        vn_sel <- vars_num()[vars_num() %in% c(v1,v2,
          vars_num()[1:min(10,length(vars_num()))])]
        mat <- df_f()|>select(all_of(vn_sel))|>drop_na()|>cor(method="spearman")
        mat_df <- as.data.frame(as.table(mat))
        mat_df$Var1 <- truncate_label(mat_df$Var1, 18)
        mat_df$Var2 <- truncate_label(mat_df$Var2, 18)
        p <- ggplot(mat_df,aes(x=Var1,y=Var2,fill=Freq,
          text=paste0(Var1," × ",Var2,"\nρ=",round(Freq,3))))+
          geom_tile(color="white",linewidth=.8)+
          geom_text(aes(label=round(Freq,2)),size=3,fontface="bold")+
          scale_fill_gradient2(low=C$rouge_f,mid="white",high=C$bleu,midpoint=0,name="ρ")+
          labs(x="",y="")+theme_nutri(10)+
          smart_axis_x(length(unique(mat_df$Var1)))
        ggplotly(p,tooltip="text") |> nutri_config()

      } else {
        plotly_empty()|>layout(title=list(text="Sélectionnez un type",x=.5))
      }
    },error=function(e)
      plotly_empty()|>layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  output$biv_dl_plot <- downloadHandler(
    filename=function() paste0("bivar_",Sys.Date(),".png"),
    content=function(f) ggsave(f, plot=last_plot(), width=12, height=8, dpi=300)
  )

