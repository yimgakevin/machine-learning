# Module Shiny — Indicateurs OMS / PAM / UNICEF
# UI et serveur namespacés (ns). Les réactifs partagés sont passés en arguments.

mod_oms_ui <- function(id) {
  ns <- shiny::NS(id)

  tabItem(
    tabName = "oms",
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #0077B6; border-radius: 8px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
              icon("heartbeat"), "Indicateurs OMS / PAM / UNICEF"),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             "Calculez les prévalences nutritionnelles standardisées avec intervalles de confiance à 95% (Wilson) : ",
             "GAM, SAM, retard de croissance, insuffisance pondérale. ",
             "Stratification possible par groupe selon les normes OMS 2006, PAM et UNICEF.")
    ),
    fluidRow(
      box(width=12, status="primary", solidHeader=TRUE,
          title=icon("heartbeat"), " Prévalences avec IC 95% (Wilson) — Normes OMS 2006 · PAM · UNICEF",
          fluidRow(
            column(3,
              checkboxGroupInput(ns("oms_groupes"),"Stratifications :",
                choices=c("Global","Région","Sexe","Groupe d'âge","Déplacement","FCS"),
                selected=c("Global","Région"))
            ),
            column(9,
              DTOutput(ns("oms_table")),
              br(),
              downloadButton(ns("dl_oms_csv"),"📥 Télécharger",
                style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:6px 14px;")
            )
          )
      )
    ),
    fluidRow(
      box(width=8, status="info", solidHeader=TRUE,
          title="Forest plot — Prévalences IC 95% toutes stratifications",
          plotlyOutput(ns("oms_forest"), height="450px")),
      box(width=4, status="warning", solidHeader=TRUE,
          title="Jauges OMS",
          plotlyOutput(ns("oms_gauge_gam"), height="200px"),
          plotlyOutput(ns("oms_gauge_stunt"), height="200px"))
    ),
    fluidRow(
      box(width=12, status="primary", solidHeader=TRUE,
          title="Scores Z — Comparaison à la distribution de référence OMS N(0,1)",
          plotlyOutput(ns("oms_zscore_dist"), height="300px"))
    )
  )
}

mod_oms_server <- function(id, df_f, map_cols, vars_cat, has_oms, filter_context, gpal) {
  moduleServer(id, function(input, output, session) {

    output$oms_table <- renderDT({
      d <- df_f(); req(d); m <- map_cols()
      inds_avail <- intersect(c("GAM","SAM","MAM","Stunting","Sev_Stunting",
                                 "Underweight","MUAC_GAM","MUAC_SAM"), names(d))
      labels_oms <- c(
        GAM="GAM (WHZ<-2|œdème)", SAM="SAM (WHZ<-3|œdème)",
        MAM="MAM (-3≤WHZ<-2)", Stunting="Stunting (HAZ<-2)",
        Sev_Stunting="Stunting sévère (HAZ<-3)",
        Underweight="Insuff. pondérale (WAZ<-2)",
        MUAC_GAM="MUAC GAM (<125mm)", MUAC_SAM="MUAC SAM (<115mm)"
      )
      groupes <- list()
      if("Global"%in%input$oms_groupes) groupes[["Global"]]=list(var=NULL)
      if("Région"%in%input$oms_groupes&&!is.null(m$region)&&m$region%in%names(d))
        groupes[["Région"]]=list(var=m$region)
      if("Sexe"%in%input$oms_groupes&&!is.null(m$sexe)&&m$sexe%in%names(d))
        groupes[["Sexe"]]=list(var=m$sexe)
      if("Groupe d'âge"%in%input$oms_groupes&&"age_group5"%in%names(d))
        groupes[["Groupe d'âge"]]=list(var="age_group5")
      if("Déplacement"%in%input$oms_groupes&&!is.null(m$depl)&&m$depl%in%names(d))
        groupes[["Déplacement"]]=list(var=m$depl)
      if("FCS"%in%input$oms_groupes&&"fcs_cat"%in%names(d))
        groupes[["Sécurité alim."]]=list(var="fcs_cat")

      map_dfr(names(groupes),function(gn){
        g <- groupes[[gn]]
        if(is.null(g$var)){
          map_dfr(inds_avail,function(v) calc_prev_wilson(d,v,labels_oms[v])) |>
            mutate(Stratification="Global",Groupe="—",.before=1)
        } else {
          d |> group_by(Groupe=.data[[g$var]]) |>
            group_modify(~map_dfr(inds_avail,function(v) calc_prev_wilson(.x,v,labels_oms[v]))) |>
            ungroup() |> mutate(Stratification=gn,.before=1)
        }
      }) |>
        datatable(rownames=FALSE,filter="top",
                  options=list(pageLength=15,dom="ftp",scrollX=TRUE),
                  class="compact stripe hover") |>
        formatStyle("Prév. (%)",
                    background=styleColorBar(c(0,100),C$bleu),
                    backgroundSize="100% 70%") |>
        formatStyle("Prév. (%)",
                    color=styleInterval(c(5,10,15,30),
                      c(C$vert,C$orange,C$rouge,C$rouge_f,C$rouge_f)),
                    fontWeight="bold") |>
        formatStyle("Sévérité",fontWeight="bold")
    })

    output$dl_oms_csv <- downloadHandler(
      filename=function() paste0("indicateurs_oms_",Sys.Date(),".csv"),
      content=function(f) {
        d <- df_f(); m <- map_cols()
        inds <- intersect(c("GAM","SAM","Stunting","Underweight"),names(d))
        res  <- map_dfr(inds, function(v) calc_prev_wilson(d,v,v))
        write.csv(res, f, row.names=FALSE)
      }
    )

    output$oms_forest <- renderPlotly({
      d <- df_f(); m <- map_cols(); req(d)
      inds <- intersect(c("GAM","SAM","Stunting","Underweight","MUAC_GAM"),names(d))
      req(length(inds)>0)
      grp <- if(!is.null(m$region)&&m$region%in%names(d)) m$region else vars_cat()[1]
      req(!is.null(grp))
      res <- d |> group_by(.data[[grp]]) |>
        group_modify(~map_dfr(inds,function(v) calc_prev_wilson(.x,v,v))) |>
        ungroup()
      nk <- length(unique(na.omit(res$Indicateur)))
      p <- ggplot(res,aes(x=`Prév. (%)`,
                          y=fct_reorder(paste0(.data[[grp]]," — ",Indicateur),`Prév. (%)`),
                          color=Indicateur,
                          text=paste0(.data[[grp]]," — ",Indicateur,
                                      "\n",`Prév. (%)`,"% [",`IC95 bas (%)`,"-",`IC95 haut (%)`,"%]")))+
        geom_vline(xintercept=c(5,10,15),linetype="dashed",
                   color=c(C$jaune,C$orange,C$rouge),linewidth=.6,alpha=.7)+
        geom_errorbarh(aes(xmin=`IC95 bas (%)`,xmax=`IC95 haut (%)`),
                       height=.4,linewidth=.8)+
        geom_point(size=3.5)+
        scale_color_manual(values=gpal(nk,"cat"))+
        labs(x="Prévalence (%)",y="")+theme_minimal(base_size=10)
      ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.15)) |> nutri_config()
    })

    output$oms_gauge_gam <- renderPlotly({
      req(has_oms()); d <- filter_context()$data
      gam_p <- round(mean(d$GAM,na.rm=TRUE)*100,1)
      plot_ly(type="indicator",mode="gauge+number",value=gam_p,
              title=list(text="GAM (%)",font=list(size=13)),
              gauge=list(axis=list(range=list(0,50)),
                         bar=list(color=couleur_prevalence(gam_p)),
                         steps=list(list(range=c(0,5),color="#e8f5e9"),
                                    list(range=c(5,10),color="#fffde7"),
                                    list(range=c(10,15),color="#fff3e0"),
                                    list(range=c(15,50),color="#ffebee")),
                         threshold=list(line=list(color=C$rouge,width=3),
                                         thickness=.75,value=15)))|>
        layout(margin=list(l=15,r=15,b=5,t=35),paper_bgcolor="rgba(0,0,0,0)") |> nutri_config()
    })

    output$oms_gauge_stunt <- renderPlotly({
      req("Stunting"%in%names(df_f())); d <- df_f()
      p <- round(mean(d$Stunting,na.rm=TRUE)*100,1)
      plot_ly(type="indicator",mode="gauge+number",value=p,
              title=list(text="Stunting (%)",font=list(size=13)),
              gauge=list(axis=list(range=list(0,70)),
                         bar=list(color=couleur_prevalence(p)),
                         steps=list(list(range=c(0,20),color="#e8f5e9"),
                                    list(range=c(20,30),color="#fffde7"),
                                    list(range=c(30,40),color="#fff3e0"),
                                    list(range=c(40,70),color="#ffebee")),
                         threshold=list(line=list(color=C$rouge,width=3),
                                         thickness=.75,value=30)))|>
        layout(margin=list(l=15,r=15,b=5,t=35),paper_bgcolor="rgba(0,0,0,0)") |> nutri_config()
    })

    output$oms_zscore_dist <- renderPlotly({
      d <- df_f(); m <- map_cols()
      z_cols <- c(m$whz,m$haz,m$waz)
      z_cols <- z_cols[!sapply(z_cols,is.null)&z_cols%in%names(d)]
      req(length(z_cols)>0)
      x_ref <- seq(-4.5,4.5,length.out=300)
      ref_df <- tibble(x=x_ref,y=dnorm(x_ref))
      df_long <- d|>select(all_of(z_cols))|>
        pivot_longer(everything(),names_to="Score",values_to="Val")
      p <- ggplot(df_long,aes(x=Val,fill=Score,color=Score))+
        geom_density(alpha=.28,linewidth=.8)+
        geom_line(data=ref_df,aes(x=x,y=y),color="black",linewidth=1,
                  linetype="dashed",inherit.aes=FALSE)+
        geom_vline(xintercept=c(-2,-3),linetype="dashed",
                   color=c(C$rouge,C$rouge_f),linewidth=.7)+
        scale_fill_manual(values=gpal(length(z_cols),"cat"))+
        scale_color_manual(values=gpal(length(z_cols),"cat"))+
        labs(x="Score Z",y="Densité",
             caption="Tiret noir = OMS N(0,1) | Tirets rouges = seuils -2 DS et -3 DS")+
        theme_minimal(base_size=11)
      ggplotly(p)|>layout(hovermode="x unified",legend=list(orientation="h",y=-0.2)) |> nutri_config()
    })

  })
}
