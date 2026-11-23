# Module Shiny — Clustering (K-means, CAH, DBSCAN)
# UI et serveur namespacés (ns). Les réactifs partagés sont passés en arguments.

mod_clust_ui <- function(id) {
  ns <- shiny::NS(id)

  tabItem(
    tabName = "clust",
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #0077B6; border-radius: 8px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
              icon("object-group"), "Clustering — Classification non supervisée"),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             "Regroupez automatiquement vos observations en profils similaires. ",
             "Trois méthodes disponibles : K-means, CAH (Classification Ascendante Hiérarchique) et DBSCAN. ",
             "Utile pour identifier des sous-populations ou des typologies dans vos données.")
    ),
    fluidRow(
      box(width=3, status="info", solidHeader=TRUE,
          title=icon("object-group"), " Paramètres",
          radioButtons(ns("clust_method"),"Méthode :",
            choices=c("K-means"="kmeans","CAH (Ward.D2)"="hclust","DBSCAN"="dbscan"),
            selected="kmeans"),
          uiOutput(ns("clust_vars_ui")),
          uiOutput(ns("clust_k_ui")),
          checkboxInput(ns("clust_auto_k"),"Nombre optimal automatique (silhouette)",TRUE),
          uiOutput(ns("clust_eps_ui")),
          actionButton(ns("run_clust"),"🔗 Lancer le clustering",
                       class="btn btn-oms btn-block", width="100%"),
          hr(),
          verbatimTextOutput(ns("clust_summary"))
      ),
      box(width=9, status="info", solidHeader=TRUE,
          title="Résultats — Clustering",
          tabsetPanel(
            tabPanel("Visualisation 2D (PCA)",
                     plotlyOutput(ns("clust_plot2d"), height="420px")),
            tabPanel("Nombre optimal de clusters",
                     plotlyOutput(ns("clust_optimal"), height="350px")),
            tabPanel("Dendrogramme (CAH)",
                     plotOutput(ns("clust_dendro"), height="420px")),
            tabPanel("Profil des clusters",
                     plotlyOutput(ns("clust_profile"), height="380px")),
            tabPanel("Tableau des clusters",
                     DTOutput(ns("clust_table")))
          )
      )
    )
  )
}

mod_clust_server <- function(id, df_f, vars_num, gpal, gprimary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    clust_res <- reactiveVal(NULL)

    output$clust_vars_ui <- renderUI({
      vn <- vars_num()
      selectizeInput(ns("clust_vars"),"Variables (numériques) :",choices=vn,
                     selected=head(vn,5),multiple=TRUE,width="100%")
    })
    output$clust_k_ui <- renderUI({
      if(!input$clust_auto_k && input$clust_method!="dbscan")
        sliderInput(ns("clust_k"),"Nombre de clusters (k) :",2,10,3,1,width="100%")
    })
    output$clust_eps_ui <- renderUI({
      if(input$clust_method=="dbscan")
        numericInput(ns("clust_eps"),"Epsilon (DBSCAN) :",0.5,0.01,5,.01,width="100%")
    })

    observeEvent(input$run_clust, {
      d <- df_f()
      vars <- input$clust_vars
      if (is.null(vars) || length(vars) < 2) {
        showNotification("⚠ Sélectionnez au moins 2 variables numériques", type="warning", duration=4)
        return()
      }
      req(d)

      withProgress(message="Clustering en cours...", {
        tryCatch({
          d_num <- d |> select(all_of(vars)) |> drop_na() |>
            mutate(across(everything(), as.numeric))

          if (nrow(d_num) < 10) stop("Trop peu de lignes complètes (< 10)")

          # Vérifier la variance : supprimer les colonnes constantes
          col_var <- sapply(d_num, var, na.rm=TRUE)
          if (any(col_var == 0)) {
            const_cols <- names(col_var[col_var == 0])
            showNotification(paste0("⚠ Colonnes constantes ignorées: ", paste(const_cols, collapse=", ")),
                            type="warning", duration=5)
            d_num <- d_num[, col_var > 0, drop=FALSE]
            if (ncol(d_num) < 2) stop("Pas assez de variables avec variance > 0")
          }

          d_scaled <- scale(d_num)
          method <- input$clust_method

          if (input$clust_auto_k && method != "dbscan") {
            max_k <- min(8, nrow(d_scaled)-1)
            if (max_k < 2) stop("Pas assez d'observations pour le clustering")
            sil_scores <- sapply(2:max_k, function(k) {
              km <- kmeans(d_scaled, k, nstart=10)
              mean(silhouette(km$cluster, dist(d_scaled))[,3])
            })
            k_opt <- which.max(sil_scores) + 1
          } else {
            k_opt <- isolate(input$clust_k)
            if (is.null(k_opt)) k_opt <- 3
          }

          res_clust <- if (method == "kmeans") {
            km <- kmeans(d_scaled, k_opt, nstart=25)
            list(clusters=km$cluster, method="K-means", k=k_opt,
                 withinss=km$tot.withinss, obj=km)
          } else if (method == "hclust") {
            hc <- hclust(dist(d_scaled), method="ward.D2")
            cl <- cutree(hc, k_opt)
            list(clusters=cl, method="CAH (Ward.D2)", k=k_opt,
                 obj=hc, d_num=d_scaled)
          } else {
            if (!requireNamespace("dbscan", quietly = TRUE))
              stop("Le package 'dbscan' est requis. Installez-le avec install.packages('dbscan').")
            eps_val <- isolate(input$clust_eps)
            if (is.null(eps_val)) eps_val <- 0.5
            db <- dbscan::dbscan(d_scaled, eps=eps_val, minPts=5)
            list(clusters=db$cluster, method="DBSCAN", k=length(unique(db$cluster)),
                 obj=db)
          }

          pca_d <- PCA(d_scaled, ncp=2, graph=FALSE)
          clust_res(list(res=res_clust, pca=pca_d,
                         d_orig=d |> select(all_of(vars)) |> drop_na(),
                         d_scaled=d_scaled,
                         sil_scores=if(input$clust_auto_k && method!="dbscan")
                           tibble(k=2:min(8,nrow(d_scaled)-1), sil=sil_scores) else NULL))
          showNotification(
            paste0("✅ ", res_clust$method, " : ", res_clust$k, " clusters — ",
                   nrow(d_num), " observations"),
            type="message", duration=4)
        }, error=function(e) showNotification(paste("❌ Erreur:", e$message), type="error", duration=8))
      })
    })

    output$clust_plot2d <- renderPlotly({
      req(clust_res()); cr <- clust_res()
      ind <- as.data.frame(cr$pca$ind$coord[,1:2])
      names(ind) <- c("Dim1","Dim2")
      ind$Cluster <- factor(cr$res$clusters)
      nk <- length(unique(cr$res$clusters))
      p <- ggplot(ind,aes(x=Dim1,y=Dim2,color=Cluster,shape=Cluster,
        text=paste0("Cluster ",Cluster,"\nDim1=",round(Dim1,2),"\nDim2=",round(Dim2,2))))+
        geom_point(alpha=.7,size=2.5)+
        stat_ellipse(aes(group=Cluster),level=.95,linewidth=.8)+
        scale_color_manual(values=gpal(nk,"cat"))+
        labs(x=paste0("Dim1 (",round(get_eigenvalue(cr$pca)[1,2],1),"%)"),
             y=paste0("Dim2 (",round(get_eigenvalue(cr$pca)[2,2],1),"%)"),
             title=paste0(cr$res$method," — ",nk," clusters | Ellipses IC 95%"))+
        theme_minimal(base_size=11)
      ggplotly(p,tooltip="text") |> nutri_config()
    })

    output$clust_optimal <- renderPlotly({
      req(clust_res()); cr <- clust_res()
      if(is.null(cr$sil_scores)) {
        return(plotly_empty()|>layout(title=list(text="Nombre optimal non calculé",x=.5)))
      }
      p <- ggplot(cr$sil_scores,aes(x=k,y=sil,
        text=paste0("k=",k,"\nSilhouette=",round(sil,4))))+
        geom_line(color=gprimary(),linewidth=1.2)+
        geom_point(size=3.5,color=gprimary())+
        geom_point(data=filter(cr$sil_scores,sil==max(sil)),
                   size=5,color=C$rouge,shape=18)+
        labs(x="Nombre de clusters (k)",y="Score silhouette moyen",
             title="Nombre optimal de clusters (méthode silhouette)")+
        theme_minimal(base_size=11)
      ggplotly(p,tooltip="text") |> nutri_config()
    })

    output$clust_dendro <- renderPlot({
      req(clust_res()); cr <- clust_res()
      if(cr$res$method!="CAH (Ward.D2)") {
        plot(0,type="n",main="Dendrogramme disponible uniquement pour CAH"); return(NULL)
      }
      hc <- cr$res$obj; k <- cr$res$k
      plot(hc,main=paste0("Dendrogramme — CAH Ward.D2 (k=",k,")"),
           sub="",xlab="",cex=.6,hang=-1)
      rect.hclust(hc,k=k,border=RColorBrewer::brewer.pal(min(k,8),"Set2"))
    }, height=380)

    output$clust_profile <- renderPlotly({
      req(clust_res()); cr <- clust_res()
      d_orig <- cr$d_orig
      d_orig$Cluster <- factor(cr$res$clusters)
      nk <- length(unique(cr$res$clusters))
      profil <- d_orig |> group_by(Cluster) |>
        summarise(across(where(is.numeric),~round(mean(.,na.rm=TRUE),3)),.groups="drop") |>
        pivot_longer(-Cluster,names_to="Variable",values_to="Moyenne")
      n_vars <- length(unique(profil$Variable))
      profil$Variable <- truncate_label(profil$Variable, 18)
      # Horizontal si beaucoup de variables
      p <- if (n_vars > 10) {
        ggplot(profil,aes(y=Variable,x=Moyenne,fill=Cluster,
          text=paste0("Cluster ",Cluster,"\n",Variable,"=",round(Moyenne,3))))+
          geom_col(position="dodge",alpha=.88,width=.72)+
          scale_fill_manual(values=gpal(nk,"cat"))+
          labs(y="",x="Moyenne")+theme_nutri()
      } else {
        ggplot(profil,aes(x=Variable,y=Moyenne,fill=Cluster,
          text=paste0("Cluster ",Cluster,"\n",Variable,"=",round(Moyenne,3))))+
          geom_col(position="dodge",alpha=.88,width=.72)+
          scale_fill_manual(values=gpal(nk,"cat"))+
          labs(x="",y="Moyenne")+theme_nutri()+smart_axis_x(n_vars)
      }
      ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.3)) |> nutri_config()
    })

    output$clust_summary <- renderPrint({
      req(clust_res()); cr <- clust_res()
      cat("Méthode :", cr$res$method, "\n")
      cat("Clusters :", cr$res$k, "\n")
      cat("Tailles :", table(cr$res$clusters), "\n")
      if(!is.null(cr$res$withinss))
        cat("Within SS :", round(cr$res$withinss,2), "\n")
    })

    output$clust_table <- renderDT({
      req(clust_res()); cr <- clust_res()
      d_orig <- cr$d_orig
      d_orig$Cluster <- factor(cr$res$clusters)
      d_orig |>
        group_by(Cluster) |>
        summarise(N=n(),across(where(is.numeric),~round(mean(.,na.rm=TRUE),2)),.groups="drop") |>
        datatable(rownames=FALSE,options=list(dom="t",pageLength=15,scrollX=TRUE),
                  class="compact stripe")
    })
  })
}
