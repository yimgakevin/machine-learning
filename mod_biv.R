# Module Shiny — Analyse bivariée
# UI et serveur namespacés (ns). Les réactifs partagés sont passés en arguments.

mod_biv_ui <- function(id) {
  ns <- shiny::NS(id)

  tabItem(
    tabName = "biv",
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #0077B6; border-radius: 8px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
              icon("chart-line"), "Analyse bivariée"),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             "Étudiez la relation entre deux variables : tableaux croisés, nuages de points, ",
             "corrélations et tests d'association. Permet de détecter des liens ",
             "entre facteurs (quali × quali, quali × quanti, quanti × quanti).")
    ),
    fluidRow(
      box(width=3, status="primary", solidHeader=TRUE,
          title=icon("sliders"), " Paramètres bivariés",
          selectInput(ns("biv_type"),"Type :",
            choices=c(
              "— Quali × Quali —"="",
              "Barplot groupé (dodge)"="dodge",
              "Barplot empilé (%)"="stack",
              "Heatmap fréquences"="heatfreq",
              "Mosaïque (proportions)"="mosaic",
              "— Quali × Quanti —"="",
              "Boxplot par groupe"="boxgrp",
              "Violin par groupe"="violgrp",
              "Densités superposées"="densgrp",
              "Histogrammes facettes"="facethist",
              "ggdist (halfeye)"="halfeye",
              "— Quanti × Quanti —"="",
              "Nuage de points + LOESS"="scatter",
              "Hexbin (densité 2D)"="hexbin",
              "Heatmap corrélations"="cormap"
            ), width="100%"),
          uiOutput(ns("biv_x_ui")),
          uiOutput(ns("biv_y_ui")),
          selectInput(ns("biv_pal"),"Palette :",
            choices=c("Auto (Global)" = "Auto (Global)",
                      PALETTE_CHOICES,
                      "Set2","RdYlGn"),
            width="100%"),
          hr(),
          tags$div(class="section-header","🧪 Test automatique"),
          verbatimTextOutput(ns("biv_test_result")),
          uiOutput(ns("biv_interpretation"))
      ),
      box(width=9, status="primary", solidHeader=TRUE,
          title="Graphique bivarié",
          div(style="text-align:right;margin-bottom:8px;",
            downloadButton(ns("biv_dl_plot"),"📥 Télécharger HD",
              style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:5px 12px;font-size:12px;")),
          plotlyOutput(ns("biv_plot"), height="500px")
      )
    ),
    fluidRow(
      box(width=12, status="info", solidHeader=TRUE, collapsible=TRUE,
          title=tags$span(icon("flask"), " Statistiques expertes — tailles d'effet & tests multiples"),
          tabsetPanel(
            tabPanel("Tailles d'effet",
              uiOutput(ns("biv_effect_note")), br(),
              DT::DTOutput(ns("biv_effect_tbl"))),
            tabPanel("Tests multiples (BH)",
              uiOutput(ns("biv_multi_note")), br(),
              DT::DTOutput(ns("biv_multi_tbl"))),
            tabPanel("Export", expert_exports_ui(ns, "biv"))
          )
      )
    )
  )
}

mod_biv_server <- function(id, df_f, vars_cat, vars_num,
                           gpal, global_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sync palette globale → sélecteur local (comportement d'origine)
    observeEvent(global_palette(), {
      pn <- global_palette()
      if (!is.null(pn) && pn %in% PALETTE_CHOICES) {
        updateSelectInput(session, "biv_pal", selected = pn)
      }
    }, ignoreInit = TRUE)

    output$biv_x_ui <- renderUI({
      vc <- vars_cat(); req(length(vc)>0)
      selectInput(ns("biv_x"),"Variable X / Groupe :",choices=vc,selected=vc[1],width="100%")
    })
    output$biv_y_ui <- renderUI({
      type <- input$biv_type
      if (type %in% c("dodge","stack","heatfreq","mosaic")) {
        vc <- vars_cat()
        selectInput(ns("biv_y"),"Variable Y (catégorielle) :",
                    choices=vc, selected=if(length(vc)>1)vc[2] else vc[1], width="100%")
      } else if (type %in% c("scatter","hexbin","cormap")) {
        vn <- vars_num()
        tagList(
          selectInput(ns("biv_y"),"Variable Y (continue) :",
                      choices=vn, selected=if(length(vn)>1)vn[2] else vn[1], width="100%"),
          selectInput(ns("biv_z"),"Couleur par :",
                      choices=c("Aucune",vars_cat()), selected="Aucune", width="100%")
        )
      } else {
        vn <- vars_num()
        selectInput(ns("biv_y"),"Variable continue :",choices=vn,selected=vn[1],width="100%")
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

    # ── Statistiques expertes : tailles d'effet & tests multiples ────────
    biv_effects <- reactive({
      req(input$biv_x, input$biv_y)
      expert_effect_sizes(df_f(), input$biv_x, input$biv_y)
    })
    biv_multi <- reactive({
      req(input$biv_y)
      expert_multi_tests(df_f(), input$biv_y, unique(c(vars_num(), vars_cat())))
    })

    output$biv_effect_note <- renderUI({
      tags$div(style="font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
        icon("circle-info"), " ", tags$b(input$biv_x), " × ", tags$b(input$biv_y),
        " — V de Cramér (quali×quali), d de Cohen / Wilcoxon (quali×quanti), ",
        "r de Pearson / ρ de Spearman (quanti×quanti).")
    })

    output$biv_effect_tbl <- DT::renderDT({
      e <- biv_effects()
      if (is.null(e) || nrow(e) == 0) {
        return(DT::datatable(data.frame(Message = "Aucune taille d'effet calculable pour ce couple."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      expert_dt(e)
    })

    output$biv_multi_note <- renderUI({
      req(input$biv_y)
      n <- tryCatch(nrow(biv_multi()), error = function(e) 0)
      tags$div(style="font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
        icon("layer-group"), " Tests de ", tags$b(input$biv_y), " contre toutes les autres variables (",
        n, " tests), p ajustées par Benjamini-Hochberg (contrôle du FDR à 5 %).")
    })

    output$biv_multi_tbl <- DT::renderDT({
      m <- biv_multi()
      if (is.null(m) || nrow(m) == 0) {
        return(DT::datatable(data.frame(Message = "Aucun test exploitable."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      expert_dt(m)
    })

    expert_export_server(input, output, session, "biv", reactive({
      list(`Tailles d'effet` = biv_effects(), `Tests multiples BH` = biv_multi())
    }))

    output$biv_dl_plot <- downloadHandler(
      filename=function() paste0("bivar_",Sys.Date(),".png"),
      content=function(f) ggsave(f, plot=last_plot(), width=12, height=8, dpi=300)
    )
  })
}
