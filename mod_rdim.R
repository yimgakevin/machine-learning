# Module Shiny — Réduction de dimension (ACP, MCA, FAMD, t-SNE, UMAP, HCPC, bootstrap)
# UI et serveur namespacés (ns).
# `rdim_res` est local au module : aucun autre onglet ne lit ce résultat.

mod_rdim_ui <- function(id) {
  ns <- shiny::NS(id)

      tabItem(tabName="rdim",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("sitemap"), "Réduction de dimension"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Simplifiez des données multivariées complexes avec l'ACP (Analyse en Composantes Principales) ",
                 "ou l'AFC (Analyse Factorielle des Correspondances). ",
                 "Identifiez les axes principaux de variation et visualisez les individus et variables dans un espace réduit.")
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title=icon("sitemap"), " Paramètres",
              tags$div(class="section-header","Méthode"),
              radioButtons(ns("rdim_method"),"Choisir automatiquement ou manuellement :",
                choices=c(
                  "Auto (selon le type de données)"="auto",
                  "ACP — Variables numériques"="pca",
                  "ACM — Variables catégorielles"="mca",
                  "FAMD — Données mixtes"="famd",
                  "AFC — Tableau de contingence"="ca"
                ), selected="auto"),
              uiOutput(ns("rdim_vars_ui")),
              numericInput(ns("rdim_ndim"),"Dimensions à garder :",5,2,15,1),
              checkboxInput(ns("rdim_scale"),"Centrer-réduire (ACP)",TRUE),
              hr(),
              tags$div(class="section-header","Variable de groupage (biplot)"),
              uiOutput(ns("rdim_grp_ui")),
              hr(),
              tags$div(class="section-header","Mode d'affichage"),
              radioButtons(ns("rdim_render_mode"),"",
                choices=c(
                  "🖱️ Interactif (Plotly)"="plotly",
                  "📊 factoextra (Publication)"="factoextra"
                ), selected="plotly", inline=FALSE),
              hr(),
              tags$div(class="section-header","Résumé méthode"),
              verbatimTextOutput(ns("rdim_method_info")),
              actionButton(ns("run_rdim"),"🔬 Lancer l'analyse",
                           class="btn btn-oms btn-block", width="100%"),
              br(),
              actionButton(ns("run_factoshiny"),"🌐 Ouvrir FactoShiny",
                style="width:100%;background:linear-gradient(135deg,#8E44AD,#6C3483);
                       color:white;border:none;border-radius:5px;padding:7px;
                       font-size:12px;font-weight:600;margin-top:4px;"),
              br(),
              uiOutput(ns("rdim_kaiser_badge"))
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Résultats — Réduction de dimension",
              tabsetPanel(
                tabPanel("📊 Éboulis des valeurs propres",
                  downloadButton(ns("dl_rdim_scree"),"⬇ PNG", class="btn btn-xs",
                    style="float:right;margin:4px;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:6px;padding:3px 8px;font-size:11px;"),
                  uiOutput(ns("rdim_scree_ui"))),
                tabPanel("🎯 Sélection automatique des axes",
                         uiOutput(ns("rdim_selec_auto"))),
                tabPanel("🔵 Cercle des corrélations",
                         fluidRow(
                           column(3,
                             selectInput(ns("rdim_axes_x"),"Axe X :",
                               choices=paste0("Dim.",1:10), selected="Dim.1", width="100%"),
                             selectInput(ns("rdim_axes_y"),"Axe Y :",
                               choices=paste0("Dim.",1:10), selected="Dim.2", width="100%"),
                             radioButtons(ns("rdim_circle_color"),"Colorer par :",
                               choices=c("Cos² (qualité)"="cos2","Contribution"="contrib","Uniforme"="none"),
                               selected="cos2"),
                             downloadButton(ns("dl_rdim_circle"),"⬇ PNG",
                               style="width:100%;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;
                                      border-radius:4px;padding:4px;font-size:11px;margin-top:6px;")
                           ),
                           column(9, uiOutput(ns("rdim_circle_ui")))
                         )),
                tabPanel("👥 Carte des individus",
                         fluidRow(
                           column(3,
                             uiOutput(ns("rdim_biplot_grp_ui")),
                             radioButtons(ns("rdim_ind_color"),"Colorer par :",
                               choices=c("Variable groupe"="grp","Cos²"="cos2","Densité"="density"),
                               selected="grp"),
                             downloadButton(ns("dl_rdim_biplot"),"⬇ PNG",
                               style="width:100%;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;
                                      border-radius:4px;padding:4px;font-size:11px;margin-top:6px;")
                           ),
                           column(9, uiOutput(ns("rdim_biplot_ui")))
                         )),
                tabPanel("📈 Contributions variables",
                         fluidRow(
                           column(3,
                             uiOutput(ns("rdim_contrib_dim_ui")),
                             checkboxInput(ns("rdim_contrib_thresh"),"Afficher seuil (1/p)×100",TRUE),
                             downloadButton(ns("dl_rdim_contrib"),"⬇ PNG",
                               style="width:100%;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;
                                      border-radius:4px;padding:4px;font-size:11px;margin-top:6px;")
                           ),
                           column(9, uiOutput(ns("rdim_contrib_ui")))
                         )),
                tabPanel("🎨 Cos² (qualité)",
                  downloadButton(ns("dl_rdim_cos2"),"⬇ PNG", class="btn btn-xs",
                    style="float:right;margin:4px;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:6px;padding:3px 8px;font-size:11px;"),
                  uiOutput(ns("rdim_cos2_ui"))),
                tabPanel("🔥 Heatmap contributions",
                  downloadButton(ns("dl_rdim_heat"),"⬇ PNG", class="btn btn-xs",
                    style="float:right;margin:4px;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:6px;padding:3px 8px;font-size:11px;"),
                  uiOutput(ns("rdim_heatmap_ui"))),
                tabPanel("📋 Tableau des résultats",
                         DTOutput(ns("rdim_table"))),

                # ── ONGLETS EXPERT (module_rdim_expert.R) ────────────
                tabPanel("\U0001f9ec Analyse Parallèle (Horn)",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " L'analyse parallèle de Horn compare les valeurs propres réelles ",
                    "à celles obtenues sur données aléatoires. Plus rigoureux que Kaiser."),
                  fluidRow(
                    column(4, sliderInput(ns("pa_nsim"),"Simulations :",
                      min=100, max=1000, value=500, step=100)),
                    column(4, br(), actionButton(ns("run_parallel"),"▶ Lancer analyse parallèle",
                      class="btn btn-warning", style="margin-top:5px;font-weight:600;"))
                  ),
                  plotlyOutput(ns("pa_plot"), height="450px"), br(),
                  DTOutput(ns("pa_table"))
                ),

                tabPanel("\U0001f333 HCPC (Classification sur CP)",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " Classification Hiérarchique sur Composantes Principales. ",
                    "Combine la réduction de dimension avec le clustering."),
                  fluidRow(
                    column(4, numericInput(ns("hcpc_nclust"),"Nombre de clusters (0=auto) :",
                      value=0, min=0, max=10, step=1)),
                    column(4, br(), actionButton(ns("run_hcpc"),"▶ Lancer HCPC",
                      class="btn btn-success", style="margin-top:5px;font-weight:600;color:white;"))
                  ),
                  fluidRow(
                    column(6, plotlyOutput(ns("hcpc_map"), height="420px")),
                    column(6, plotlyOutput(ns("hcpc_dendro"), height="420px"))
                  ), br(),
                  DTOutput(ns("hcpc_desc_table"))
                ),

                tabPanel("\U0001f30c t-SNE",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " t-SNE projette les données de haute dimension en 2D en préservant ",
                    "les voisinages locaux. Idéal pour détecter des clusters non-linéaires."),
                  fluidRow(
                    column(3, sliderInput(ns("tsne_perp"),"Perplexité :",
                      min=5, max=100, value=30, step=5)),
                    column(3, sliderInput(ns("tsne_iter"),"Itérations :",
                      min=300, max=2000, value=1000, step=100)),
                    column(3, uiOutput(ns("tsne_grp_ui"))),
                    column(3, br(), actionButton(ns("run_tsne"),"▶ Lancer t-SNE",
                      class="btn btn-info", style="margin-top:5px;font-weight:600;color:white;"))
                  ),
                  plotlyOutput(ns("tsne_plot"), height="480px")
                ),

                tabPanel("\U0001f52d UMAP",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " UMAP préserve mieux la structure globale que t-SNE et est plus rapide. ",
                    "Paramètres : n_neighbors (structure locale) et min_dist (compacité)."),
                  fluidRow(
                    column(3, sliderInput(ns("umap_nn"),"Voisins (n_neighbors) :",
                      min=3, max=50, value=15, step=2)),
                    column(3, sliderInput(ns("umap_md"),"Distance min (min_dist) :",
                      min=0.01, max=0.99, value=0.1, step=0.05)),
                    column(3, uiOutput(ns("umap_grp_ui"))),
                    column(3, br(), actionButton(ns("run_umap"),"▶ Lancer UMAP",
                      class="btn btn-info", style="margin-top:5px;font-weight:600;color:white;"))
                  ),
                  plotlyOutput(ns("umap_plot"), height="480px")
                ),

                tabPanel("\U0001f504 Stabilité Bootstrap",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " Évalue la stabilité des loadings ACP par rééchantillonnage bootstrap. ",
                    "Un IC 95% qui inclut 0 indique un loading instable."),
                  fluidRow(
                    column(4, sliderInput(ns("boot_nsim"),"Rééchantillonnages :",
                      min=100, max=1000, value=500, step=100)),
                    column(4, uiOutput(ns("boot_dim_ui"))),
                    column(4, br(), actionButton(ns("run_boot"),"▶ Lancer bootstrap",
                      class="btn btn-warning", style="margin-top:5px;font-weight:600;"))
                  ),
                  plotlyOutput(ns("boot_plot"), height="480px"), br(),
                  DTOutput(ns("boot_table"))
                ),

                tabPanel("\U0001f504 Comparaison méthodes",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " Compare ACP (linéaire) vs t-SNE vs UMAP (non-linéaires) ",
                    "sur les mêmes données."),
                  fluidRow(
                    column(4, uiOutput(ns("cmp_grp_ui"))),
                    column(4, br(), actionButton(ns("run_cmp_methods"),"▶ Comparer",
                      class="btn btn-primary", style="margin-top:5px;font-weight:600;"))
                  ),
                  plotlyOutput(ns("cmp_methods_plot"), height="450px")
                )
              )
          )
        )
      )
}

mod_rdim_server <- function(id, df_f, vars_cat, vars_num, mapped_data,
                            gpal, gprimary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rdim_res <- reactiveVal(NULL)

  # ══════════════════════════════════════════════════════════════════
  # RÉDUCTION DE DIMENSION — Kaiser + FactoInvestigate + Couleurs
  # ══════════════════════════════════════════════════════════════════

  # ── Helper : critère bâton brisé (Broken Stick) ──────────────────
  broken_stick <- function(n_vars) {
    rev(cumsum(rev(1 / seq_len(n_vars)))) / n_vars * 100
  }

  # ── Helper robuste : extraction var/ind selon la méthode ─────────
  # Gère PCA, MCA, FAMD, CA sans "indice hors limites"
  get_var_info <- function(r, method) {
    tryCatch({
      if (method == "famd") {
        # FAMD : combiner quanti.var et quali.var AVEC marqueur de type
        qn <- r$quanti.var
        ql <- r$quali.var
        if (is.null(qn) && is.null(ql)) return(NULL)
        nc <- if (!is.null(qn)) min(ncol(qn$coord), 10)
              else min(ncol(ql$coord), 10)
        
        combine_typed <- function(a, b) {
          if (is.null(a) && is.null(b)) return(NULL)
          da <- if (!is.null(a)) {
            d <- as.data.frame(a[, 1:nc, drop=FALSE])
            d$.var_type <- "quanti"
            d
          }
          db <- if (!is.null(b)) {
            d <- as.data.frame(b[, 1:nc, drop=FALSE])
            d$.var_type <- "quali"
            d
          }
          if (is.null(da)) return(db)
          if (is.null(db)) return(da)
          rbind(da, db)
        }
        
        return(list(
          coord   = combine_typed(qn$coord,   ql$coord),
          cos2    = combine_typed(qn$cos2,    ql$cos2),
          contrib = combine_typed(qn$contrib, ql$contrib),
          # Accès séparé pour le cercle de corrélations
          quanti_coord  = if (!is.null(qn)) as.data.frame(qn$coord[, 1:nc, drop=FALSE]),
          quali_coord   = if (!is.null(ql)) as.data.frame(ql$coord[, 1:nc, drop=FALSE]),
          quanti_cos2   = if (!is.null(qn)) as.data.frame(qn$cos2[, 1:nc, drop=FALSE]),
          quali_cos2    = if (!is.null(ql)) as.data.frame(ql$cos2[, 1:nc, drop=FALSE]),
          quanti_contrib = if (!is.null(qn)) as.data.frame(qn$contrib[, 1:nc, drop=FALSE]),
          quali_contrib  = if (!is.null(ql)) as.data.frame(ql$contrib[, 1:nc, drop=FALSE])
        ))
      }
      obj <- switch(method,
        pca  = r$var,
        mca  = r$var,
        ca   = r$col,
        r$var
      )
      if (is.null(obj)) return(NULL)
      nc <- min(ncol(obj$coord), 10)
      list(
        coord   = as.data.frame(obj$coord[, 1:nc, drop=FALSE]),
        cos2    = as.data.frame(obj$cos2[,  1:nc, drop=FALSE]),
        contrib = as.data.frame(obj$contrib[,1:nc, drop=FALSE])
      )
    }, error = function(e) NULL)
  }

  get_ind_info <- function(r, method) {
    tryCatch({
      obj <- switch(method,
        pca  = r$ind,
        mca  = r$ind,
        famd = r$ind,
        ca   = r$row,   # lignes AFC
        r$ind
      )
      if (is.null(obj)) return(NULL)
      nc <- min(ncol(obj$coord), 10)
      list(
        coord = as.data.frame(obj$coord[, 1:nc, drop=FALSE]),
        cos2  = as.data.frame(obj$cos2[,  1:nc, drop=FALSE])
      )
    }, error = function(e) NULL)
  }

  # Pour AFC : lignes et colonnes ensemble
  get_ca_both <- function(r) {
    tryCatch({
      nc_r <- min(ncol(r$row$coord), 2)
      nc_c <- min(ncol(r$col$coord), 2)
      rows <- as.data.frame(r$row$coord[, 1:nc_r, drop=FALSE])
      cols <- as.data.frame(r$col$coord[, 1:nc_c, drop=FALSE])
      names(rows)[1:2] <- c("Dim.1","Dim.2")
      names(cols)[1:2] <- c("Dim.1","Dim.2")
      rows$Label <- rownames(rows); rows$Type <- "Ligne"
      cols$Label <- rownames(cols); cols$Type <- "Colonne"
      bind_rows(rows, cols)
    }, error = function(e) NULL)
  }

  # ── Stockage des ggplots pour téléchargement ─────────────────────
  rdim_gg <- reactiveValues(
    scree=NULL, circle=NULL, biplot=NULL,
    contrib=NULL, cos2=NULL, heatmap=NULL
  )

  # ── Helper : test de permutation (FactoInvestigate-style) ────────
  perm_test_pca <- function(data_scaled, n_perm = 200) {
    real_eig <- prcomp(data_scaled, center=FALSE, scale.=FALSE)$sdev^2
    n         <- min(length(real_eig), ncol(data_scaled))
    perm_mat  <- replicate(n_perm, {
      d_perm <- apply(data_scaled, 2, sample)
      prcomp(d_perm, center=FALSE, scale.=FALSE)$sdev^2[1:n]
    })
    if (!is.matrix(perm_mat)) perm_mat <- matrix(perm_mat, nrow=n)
    p_vals <- sapply(seq_len(n), function(i) mean(perm_mat[i,] >= real_eig[i]))
    list(real_eig=real_eig[1:n], p_vals=p_vals,
         sig_axes=which(p_vals < 0.05))
  }

  # ── UI dynamiques de la section rdim ─────────────────────────────
  output$rdim_vars_ui <- renderUI({
    vn <- vars_num(); vc <- vars_cat()
    method <- input$rdim_method
    
    if (method == "pca") {
      # ACP : variables NUMÉRIQUES uniquement
      selectizeInput(ns("rdim_vars"),"Variables numériques :", choices=vn,
                     selected=head(vn, min(8, length(vn))), multiple=TRUE, width="100%")
    } else if (method == "famd") {
      # FAMD : mix obligatoire — séparer les groupes visuellement
      choices_grouped <- list(
        "Variables numériques" = vn,
        "Variables catégorielles" = vc
      )
      selected_famd <- c(head(vn, min(4, length(vn))), head(vc, min(3, length(vc))))
      selectizeInput(ns("rdim_vars"),"Variables mixtes (num. + cat.) :", choices=choices_grouped,
                     selected=selected_famd, multiple=TRUE, width="100%")
    } else if (method == "auto") {
      # Auto : toutes variables, groupées
      choices_grouped <- list(
        "Variables numériques" = vn,
        "Variables catégorielles" = vc
      )
      selectizeInput(ns("rdim_vars"),"Variables :", choices=choices_grouped,
                     selected=head(vn, min(8, length(vn))), multiple=TRUE, width="100%")
    } else if (method == "mca") {
      # ACM : variables CATÉGORIELLES uniquement
      selectizeInput(ns("rdim_vars"),"Variables catégorielles :", choices=vc,
                     selected=head(vc, min(5, length(vc))), multiple=TRUE, width="100%")
    } else {
      # AFC : 2 variables catégorielles
      tagList(
        selectInput(ns("rdim_var1"),"Variable lignes :", choices=vc,
                    selected=vc[1], width="100%"),
        selectInput(ns("rdim_var2"),"Variable colonnes :", choices=vc,
                    selected=if(length(vc)>1)vc[2] else vc[1], width="100%")
      )
    }
  })

  output$rdim_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("rdim_grp"),"Colorier individus par :",
                choices=c("Aucun"="", vc), selected="", width="100%")
  })

  output$rdim_biplot_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("rdim_biplot_grp"),"Variable de groupage :",
                choices=c("Aucun"="", vc), selected=input$rdim_grp, width="100%")
  })

  output$rdim_contrib_dim_ui <- renderUI({
    req(rdim_res())
    eig <- tryCatch(get_eigenvalue(rdim_res()$res), error=function(e) NULL)
    n_dim <- if (!is.null(eig)) nrow(eig) else 5
    selectInput(ns("rdim_contrib_dim"),"Dimension :",
                choices=setNames(1:n_dim, paste0("Dim.",1:n_dim)),
                selected=1, width="100%")
  })

  output$rdim_method_info <- renderPrint({
    m <- input$rdim_method
    info <- switch(m,
      "auto" = "Choix automatique :\n• Toutes numériques → ACP\n• Toutes catégorielles → ACM\n• Mixte → FAMD",
      "pca"  = "ACP — Analyse en Composantes Principales\n• Variables numériques uniquement\n• Centrer-réduire recommandé\n• Maximise la variance expliquée",
      "mca"  = "ACM — Analyse des Correspondances Multiples\n• Variables catégorielles uniquement\n• Tableau disjonctif complet",
      "famd" = "FAMD — Factor Analysis of Mixed Data\n• Variables mixtes (num. + cat.)\n• Combine ACP et ACM",
      "ca"   = "AFC — Analyse Factorielle des Correspondances\n• Tableau de contingence 2 variables\n• Similarité profils ligne/colonne"
    )
    cat(info)
  })

  # Badge Kaiser dans la sidebar (après analyse)
  output$rdim_kaiser_badge <- renderUI({
    req(rdim_res())
    r <- rdim_res()$res
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))
    n_kaiser <- sum(eig[,1] > 1)
    if (n_kaiser == 0) return(NULL)
    tags$div(style="margin-top:6px;",
      tags$div(style=paste0("background:linear-gradient(135deg,#0B3954,#00838F);",
                            "color:white;border-radius:8px;padding:8px 10px;font-size:12px;"),
        tags$b("📐 Critère de Kaiser"),
        tags$br(),
        paste0("→ ", n_kaiser, " axe", if(n_kaiser>1)"s", " retenu",
               if(n_kaiser>1)"s", " (λ > 1)"),
        tags$br(),
        tags$span(style="font-size:10px;opacity:.85;",
          paste0("Variance cumulée : ",
                 round(eig[min(n_kaiser, nrow(eig)), 3], 1), "%"))
      )
    )
  })

  observeEvent(input$run_rdim, {
    req(mapped_data()); d <- df_f()
    method <- input$rdim_method

    # ===== VALIDATION AVEC NOUVEAU SYSTEME =====
    validation <- if (method == "ca") {
      validate_ca(d, input$rdim_var1, input$rdim_var2)
    } else {
      validate_rdim(d, method, vars = input$rdim_vars)
    }
    
    # Si validation echoue → afficher erreur et arreter
    if (!validation$valid) {
      showNotification(
        HTML(sprintf(
          '<div style="color: #BF3636; background: #ffebee; padding: 12px; border-radius: 4px; border-left: 4px solid #BF3636;">
            <strong>⚠️ Erreur validation:</strong><br/>%s
          </div>',
          validation$error
        )),
        type = "error",
        duration = 8,
        closeButton = TRUE
      )
      return()
    }

    withProgress(message = "Calcul en cours...", {
      tryCatch({
        
        # ===== DETERMINER VRAIE METHODE si auto =====
        true_method <- method
        if (method == "auto") {
          vars_clean <- input$rdim_vars[input$rdim_vars %in% names(d)]
          all_num <- all(vapply(vars_clean, function(v) is.numeric(d[[v]]), logical(1)))
          all_cat <- all(vapply(vars_clean, function(v) is.factor(d[[v]]) || is.character(d[[v]]), logical(1)))
          true_method <- if (all_num) "pca" else if (all_cat) "mca" else "famd"
        }
        
        # ===== EXECUTER LA REDUCTION =====
        res <- switch(true_method,
          
          "pca" = {
            # Variables numeriques seulement
            vars_num <- validation$vars_clean
            d_pca <- d[, vars_num, drop = FALSE] |> drop_na()
            PCA(d_pca, 
                scale.unit = input$rdim_scale, 
                ncp = min(input$rdim_ndim, ncol(d_pca) - 1), 
                graph = FALSE)
          },
          
          "mca" = {
            # Variables categorielles seulement
            vars_cat <- validation$vars_clean
            d_mca <- d[, vars_cat, drop = FALSE] |>
              mutate(across(everything(), as.factor)) |>
              drop_na()
            MCA(d_mca, ncp = min(input$rdim_ndim, 10), graph = FALSE)
          },
          
          "famd" = {
            # Variables mixtes
            vars_famd <- validation$vars_clean
            d_famd <- d[, vars_famd, drop = FALSE] |>
              mutate(across(where(is.character), as.factor)) |>
              drop_na()
            ncp_famd <- max(2, min(input$rdim_ndim, ncol(d_famd) - 1))
            FAMD(d_famd, ncp = ncp_famd, graph = FALSE)
          },
          
          "ca" = {
            # Tableau contingence
            tbl <- table(d[[input$rdim_var1]], d[[input$rdim_var2]])
            CA(tbl, ncp = min(input$rdim_ndim, min(dim(tbl)) - 1), graph = FALSE)
          },
          
          stop(sprintf("Methode '%s' non supportee", true_method))
        )

        # ===== SAUVEGARDER RESULTATS =====
        rdim_res(list(
          res = res, 
          method = true_method, 
          method_display = method,
          vars = if (true_method != "ca") validation$vars_clean else c(input$rdim_var1, input$rdim_var2)
        ))
        
        # ===== NOTIFICATION SUCCES =====
        msg_detail <- switch(true_method,
          "pca" = sprintf("%d variables numeriques", validation$n_vars),
          "mca" = sprintf("%d variables categorielles", validation$n_vars),
          "famd" = sprintf("%d num. + %d cat.", validation$n_numeric, validation$n_categorical),
          "ca" = sprintf("Table %s", validation$table_dim),
          "?"
        )
        
        showNotification(
          HTML(sprintf(
            '<div style="color: #2e7d32; background: #e8f5e9; padding: 10px; border-radius: 4px;">
              <strong>✅ %s reussie</strong><br/>
              %d observations | %s
            </div>',
            toupper(true_method),
            validation$n_obs,
            msg_detail
          )),
          type = "message",
          duration = 5,
          closeButton = TRUE
        )
        
      }, error = function(e) {
        showNotification(
          HTML(sprintf(
            '<div style="color: #BF3636; background: #ffebee; padding: 10px; border-radius: 4px;">
              <strong>❌ Erreur execution:</strong><br/>%s
            </div>',
            e$message
          )),
          type = "error",
          duration = 8,
          closeButton = TRUE
        )
      })
    })
  })

  output$rdim_scree <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))
    df_eig <- tibble(
      Dim         = paste0("Dim.",1:nrow(eig)),
      Lambda      = round(eig[,1], 4),
      Variance_pct= round(eig[,2], 2),
      Variance_cum= round(eig[,3], 2),
      # Kaiser : vert si λ > 1, orange sinon
      Kaiser      = ifelse(eig[,1] > 1, "Retenu (λ > 1)", "Rejeté (λ ≤ 1)")
    )
    # Bâton brisé pour superposition
    bs <- broken_stick(nrow(df_eig))
    df_eig$BatonBrise <- round(bs, 2)

    p <- ggplot(df_eig, aes(
        x    = Dim,
        y    = Variance_pct,
        fill = Kaiser,
        text = paste0("<b>", Dim, "</b><br>",
                      "λ = ", Lambda, "<br>",
                      "Variance : ", Variance_pct, "%<br>",
                      "Cumulée : ", Variance_cum, "%<br>",
                      "Bâton brisé : ", BatonBrise, "%<br>",
                      "Kaiser : ", Kaiser)
      )) +
      geom_col(alpha=.88, width=.72) +
      # Ligne bâton brisé
      geom_line(aes(y=BatonBrise, group=1, text=NULL),
                color="#9B59B6", linewidth=1.1, linetype="dotted") +
      geom_point(aes(y=BatonBrise, text=NULL), color="#9B59B6", size=2.5) +
      # Ligne cumulée
      geom_line(aes(y=Variance_cum, group=1, text=NULL),
                color=C$rouge, linewidth=1.3) +
      geom_point(aes(y=Variance_cum, text=NULL), color=C$rouge, size=3) +
      # Seuils de variance
      geom_hline(yintercept=80, linetype="dashed", color="gray50", linewidth=.6) +
      geom_hline(yintercept=70, linetype="dotted", color="gray70", linewidth=.5) +
      annotate("text", x=nrow(df_eig)*0.9, y=82, label="80%", size=3, color="gray50") +
      annotate("text", x=nrow(df_eig)*0.9, y=72, label="70%", size=3, color="gray70") +
      scale_fill_manual(
        values = c("Retenu (λ > 1)"="#00838F", "Rejeté (λ ≤ 1)"="#BDC3C7"),
        name   = "Critère de Kaiser"
      ) +
      scale_y_continuous(
        name      = "Variance expliquée (%)",
        sec.axis  = sec_axis(~., name="Variance cumulée / Bâton brisé (%)")
      ) +
      labs(x="Dimension", title="Éboulis des valeurs propres — Critère de Kaiser",
           subtitle="Vert : λ > 1 (Kaiser) · Ligne rouge : cumul · Pointillés violets : bâton brisé") +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom",
            plot.subtitle=element_text(size=9, color="gray50"))
    ggplotly(p, tooltip="text") |>
      layout(legend=list(orientation="h", y=-0.18)) |> nutri_config()
  })

  # ── FactoShiny ────────────────────────────────────────────────────
  observeEvent(input$run_factoshiny, {
    req(rdim_res())
    if (requireNamespace("Factoshiny", quietly=TRUE)) {
      tryCatch(
        Factoshiny::Factoshiny(rdim_res()$res),
        error=function(e) showNotification(paste("❌ FactoShiny:", e$message), type="error")
      )
    } else {
      showModal(modalDialog(
        title = "📦 FactoShiny non installé",
        tags$p("Pour utiliser FactoShiny, installez le package :"),
        tags$code("install.packages('Factoshiny')"),
        tags$br(), tags$br(),
        tags$p("Puis relancez l'application."),
        footer = modalButton("Fermer")
      ))
    }
  })


  # ══════════════════════════════════════════════════════════════════
  # RDIM — SWITCH UI CONDITIONNEL (Plotly ↔ factoextra)
  # ══════════════════════════════════════════════════════════════════
  
  rdim_mode <- reactive(input$rdim_render_mode %||% "plotly")
  
  output$rdim_scree_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput(ns("rdim_scree_fviz"), height="390px")
    else plotlyOutput(ns("rdim_scree"), height="390px")
  })
  output$rdim_circle_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput(ns("rdim_circle_fviz"), height="450px")
    else plotlyOutput(ns("rdim_circle"), height="450px")
  })
  output$rdim_biplot_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput(ns("rdim_biplot_fviz"), height="450px")
    else plotlyOutput(ns("rdim_biplot"), height="450px")
  })
  output$rdim_contrib_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput(ns("rdim_contrib_fviz"), height="420px")
    else plotlyOutput(ns("rdim_contrib"), height="420px")
  })
  output$rdim_cos2_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput(ns("rdim_cos2_fviz"), height="410px")
    else plotlyOutput(ns("rdim_cos2"), height="410px")
  })
  output$rdim_heatmap_ui <- renderUI({
    if (rdim_mode() == "factoextra") plotOutput(ns("rdim_heatmap_fviz"), height="410px")
    else plotlyOutput(ns("rdim_heatmap_contrib"), height="410px")
  })
  
  # ══════════════════════════════════════════════════════════════════
  # RDIM — RENDERERS factoextra (mode publication)
  # ══════════════════════════════════════════════════════════════════
  
  # Palette cohérente pour factoextra
  fviz_gradient <- c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C")
  
  # ── 1. Scree plot ────────────────────────────────────────────────
  output$rdim_scree_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res
    p <- fviz_screeplot(r, addlabels = TRUE, barfill = "#3498DB", barcolor = "#2C3E50",
                        linecolor = "#E74C3C", ncp = min(10, length(get_eigenvalue(r)[[1]]))) +
      geom_hline(yintercept = 100 / ncol(get_eigenvalue(r)), linetype = "dashed",
                 color = "#00838F", linewidth = 0.7) +
      labs(title = "Éboulis des valeurs propres",
           subtitle = "Barres = % variance · Ligne rouge = cumul · Pointillé vert = seuil Kaiser moyen") +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(size = 10, color = "gray50"))
    rdim_gg$scree <- p
    p
  })
  
  # ── 2. Cercle des corrélations ───────────────────────────────────
  output$rdim_circle_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    
    ax_x <- as.integer(gsub("Dim\\.", "", input$rdim_axes_x %||% "Dim.1"))
    ax_y <- as.integer(gsub("Dim\\.", "", input$rdim_axes_y %||% "Dim.2"))
    color_by <- input$rdim_circle_color %||% "cos2"
    
    p <- if (method == "famd") {
      # FAMD : 2 sous-graphiques (quanti + quali) côte à côte
      p_qn <- fviz_famd_var(r, "quanti.var", axes = c(ax_x, ax_y),
                             repel = TRUE, col.var = color_by,
                             gradient.cols = fviz_gradient,
                             title = "Variables quantitatives (corrélations)")
      p_ql <- fviz_famd_var(r, "quali.var", axes = c(ax_x, ax_y),
                             repel = TRUE, col.var = color_by,
                             gradient.cols = c("#BDC3C7","#8E44AD","#E74C3C"),
                             title = "Variables qualitatives (centroïdes)")
      # Combiner avec patchwork si disponible
      if (requireNamespace("patchwork", quietly = TRUE)) {
        p_qn + p_ql + patchwork::plot_annotation(
          title = "FAMD — Plan factoriel",
          subtitle = paste0("Dim.", ax_x, " × Dim.", ax_y),
          theme = theme(plot.title = element_text(face = "bold", size = 14))
        )
      } else {
        p_qn  # Fallback sans patchwork
      }
    } else if (method == "pca") {
      fviz_pca_var(r, axes = c(ax_x, ax_y), repel = TRUE,
                   col.var = color_by, gradient.cols = fviz_gradient,
                   title = "ACP — Cercle des corrélations") +
        theme_minimal(base_size = 13)
    } else if (method == "mca") {
      fviz_mca_var(r, axes = c(ax_x, ax_y), repel = TRUE,
                   col.var = color_by, gradient.cols = fviz_gradient,
                   title = "ACM — Plan des modalités") +
        theme_minimal(base_size = 13)
    } else if (method == "ca") {
      fviz_ca_biplot(r, axes = c(ax_x, ax_y), repel = TRUE,
                     col.row = "#3498DB", col.col = "#E74C3C",
                     title = "AFC — Biplot lignes × colonnes") +
        theme_minimal(base_size = 13)
    }
    rdim_gg$circle <- p
    p
  })
  
  # ── 3. Carte des individus / Biplot ──────────────────────────────
  output$rdim_biplot_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    
    grp_var <- input$rdim_biplot_grp %||% input$rdim_grp %||% ""
    d <- df_f()
    use_repel <- nrow(d) <= 100  # repel seulement si ≤ 100 obs (sinon trop lent)
    
    p <- if (method == "pca") {
      if (nchar(grp_var) > 0 && grp_var %in% names(d)) {
        habillage_idx <- which(names(d) == grp_var)
        fviz_pca_ind(r, habillage = d[[grp_var]], addEllipses = TRUE,
                     ellipse.level = 0.95, repel = use_repel,
                     pointsize = 1.8, alpha.ind = 0.6,
                     title = paste0("ACP — Individus par ", grp_var)) +
          theme_minimal(base_size = 13)
      } else {
        fviz_pca_ind(r, col.ind = "cos2", repel = use_repel,
                     gradient.cols = fviz_gradient, pointsize = 1.8,
                     title = "ACP — Individus (Cos²)") +
          theme_minimal(base_size = 13)
      }
    } else if (method == "famd") {
      if (nchar(grp_var) > 0 && grp_var %in% names(d)) {
        fviz_famd_ind(r, habillage = d[[grp_var]], addEllipses = TRUE,
                      ellipse.level = 0.95, repel = use_repel,
                      pointsize = 1.8, alpha.ind = 0.6,
                      title = paste0("FAMD — Individus par ", grp_var)) +
          theme_minimal(base_size = 13)
      } else {
        fviz_famd_ind(r, col.ind = "cos2", repel = use_repel,
                      gradient.cols = fviz_gradient, pointsize = 1.8,
                      title = "FAMD — Individus (Cos²)") +
          theme_minimal(base_size = 13)
      }
    } else if (method == "mca") {
      if (nchar(grp_var) > 0 && grp_var %in% names(d)) {
        fviz_mca_ind(r, habillage = d[[grp_var]], addEllipses = TRUE,
                     repel = use_repel, pointsize = 1.8, alpha.ind = 0.6,
                     title = paste0("ACM — Individus par ", grp_var)) +
          theme_minimal(base_size = 13)
      } else {
        fviz_mca_ind(r, col.ind = "cos2", repel = use_repel,
                     gradient.cols = fviz_gradient, pointsize = 1.8,
                     title = "ACM — Individus (Cos²)") +
          theme_minimal(base_size = 13)
      }
    } else if (method == "ca") {
      fviz_ca_biplot(r, repel = TRUE,
                     col.row = "#3498DB", col.col = "#E74C3C",
                     title = "AFC — Biplot") +
        theme_minimal(base_size = 13)
    }
    rdim_gg$biplot <- p
    p
  })
  
  # ── 4. Contributions ─────────────────────────────────────────────
  output$rdim_contrib_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    dim_sel <- as.integer(input$rdim_contrib_dim %||% 1)
    
    choice <- if (method == "ca") "col" else "var"
    
    p <- fviz_contrib(r, choice = choice, axes = dim_sel,
                      fill = "#3498DB", color = "#2C3E50",
                      top = 25,  # limiter à 25 variables max pour lisibilité
                      title = paste0("Contributions — Dim.", dim_sel)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    
    rdim_gg$contrib <- p
    p
  })
  
  # ── 5. Cos² ──────────────────────────────────────────────────────
  output$rdim_cos2_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    
    choice <- if (method == "ca") "col" else "var"
    n_dims <- min(3, nrow(get_eigenvalue(r)))
    
    p <- fviz_cos2(r, choice = choice, axes = 1:n_dims,
                   fill = "#E67E22", color = "#2C3E50",
                   top = 25,
                   title = paste0("Cos² — Qualité de représentation (Dim.1:", n_dims, ")")) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    
    rdim_gg$cos2 <- p
    p
  })
  
  # ── 6. Heatmap (factoextra n'a pas d'équivalent → corrplot-style) ─
  output$rdim_heatmap_fviz <- renderPlot({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    vi <- get_var_info(r, method)
    req(!is.null(vi))
    contrib <- vi$contrib
    if (".var_type" %in% names(contrib)) contrib <- contrib[, names(contrib) != ".var_type", drop = FALSE]
    n_dim <- min(ncol(contrib), 8)
    contrib <- contrib[, 1:n_dim, drop = FALSE]
    
    # Ordonner par contribution totale décroissante
    ordre <- order(rowSums(contrib), decreasing = TRUE)
    contrib <- contrib[ordre, , drop = FALSE]
    # Limiter à 25 variables
    if (nrow(contrib) > 25) contrib <- contrib[1:25, , drop = FALSE]
    
    mat <- as.matrix(contrib)
    df_heat <- expand.grid(
      Variable  = factor(rownames(mat), levels = rev(rownames(mat))),
      Dimension = factor(colnames(mat), levels = colnames(mat))
    )
    df_heat$Contrib <- as.vector(mat[rev(seq_len(nrow(mat))), ])
    
    ggplot(df_heat, aes(x = Dimension, y = Variable, fill = Contrib)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(Contrib, 1)), size = 3, color = "white", fontface = "bold") +
      scale_fill_gradientn(
        colors = c("#2C3E50", "#2980B9", "#27AE60", "#F39C12", "#E74C3C"),
        name = "Contribution (%)"
      ) +
      labs(x = "", y = "", title = "Heatmap — Contributions variables × dimensions",
           subtitle = "Top 25 variables par contribution totale") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_text(size = 10),
            plot.subtitle = element_text(size = 10, color = "gray50"),
            legend.position = "right")
  })
  
  # ── Download handlers pour chaque graphique ───────────────────────
  make_dl <- function(id, gg_name, fname) {
    output[[id]] <- downloadHandler(
      filename = function() paste0(fname, "_", Sys.Date(), ".png"),
      content  = function(f) {
        gg <- rdim_gg[[gg_name]]
        if (!is.null(gg)) {
          ggsave(f, gg, width=10, height=7, dpi=200, bg="white")
        } else {
          # fallback : graphique vide
          ggsave(f, ggplot() + labs(title="Lancez d'abord l'analyse"), width=8, height=6)
        }
      }
    )
  }
  make_dl("dl_rdim_scree",   "scree",   "eboulis_valeurs_propres")
  make_dl("dl_rdim_circle",  "circle",  "cercle_correlations")
  make_dl("dl_rdim_biplot",  "biplot",  "carte_individus")
  make_dl("dl_rdim_contrib", "contrib", "contributions_variables")
  make_dl("dl_rdim_cos2",    "cos2",    "cos2_qualite")
  make_dl("dl_rdim_heat",    "heatmap", "heatmap_contributions")

  output$rdim_circle <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      # Axes sélectionnés
      ax_x <- as.integer(gsub("Dim\\.","", input$rdim_axes_x %||% "Dim.1"))
      ax_y <- as.integer(gsub("Dim\\.","", input$rdim_axes_y %||% "Dim.2"))
      ax_x <- max(1, min(ax_x, 10)); ax_y <- max(1, min(ax_y, 10))

      # ── Pour l'AFC : afficher biplot lignes + colonnes ────────────
      if (method == "ca") {
        df_ca <- get_ca_both(r)
        req(!is.null(df_ca))
        eig <- get_eigenvalue(r)
        pct1 <- round(eig[1,2],1)
        pct2 <- if (nrow(eig) >= 2) round(eig[2,2],1) else 0
        p <- ggplot(df_ca, aes(x=Dim.1, y=Dim.2, color=Type, shape=Type,
              text=paste0("<b>",Label,"</b><br>Type: ",Type,"<br>",
                          "Dim1=",round(Dim.1,3)," | Dim2=",round(Dim.2,3)))) +
          geom_hline(yintercept=0, color="gray70", linewidth=.3) +
          geom_vline(xintercept=0, color="gray70", linewidth=.3) +
          geom_point(size=3.5, alpha=.85) +
          geom_text(aes(label=Label), size=3.2, vjust=-0.6, fontface="bold") +
          scale_color_manual(values=c(Ligne=C$bleu, Colonne=C$rouge)) +
          scale_shape_manual(values=c(Ligne=16, Colonne=17)) +
          labs(x=paste0("Dim.1 (",pct1,"%)"), y=paste0("Dim.2 (",pct2,"%)"),
               title="AFC — Biplot lignes et colonnes",
               subtitle="Bleu = profils lignes · Rouge = profils colonnes") +
          theme_minimal(base_size=12) +
          theme(plot.subtitle=element_text(size=9, color="gray50"))
        rdim_gg$circle <- p
        return(ggplotly(p, tooltip="text"))
      }

      # ── PCA / MCA / FAMD ──────────────────────────────────────────
      vi <- get_var_info(r, method)
      req(!is.null(vi))
      eig <- get_eigenvalue(r)
      
      # ── FAMD : traitement spécifique quanti + quali ──────────────
      if (method == "famd") {
        
        # --- Variables quantitatives : cercle de corrélations (coord [-1,1]) ---
        qn_coord   <- vi$quanti_coord
        qn_cos2    <- vi$quanti_cos2
        qn_contrib <- vi$quanti_contrib
        
        # --- Variables qualitatives : centroïdes (coord non bornées) ---
        ql_coord   <- vi$quali_coord
        ql_cos2    <- vi$quali_cos2
        ql_contrib <- vi$quali_contrib
        
        validate(need(
          (!is.null(qn_coord) && ncol(qn_coord) >= 2) ||
          (!is.null(ql_coord) && ncol(ql_coord) >= 2),
          "Au moins 2 dimensions nécessaires. Augmentez le nombre de dimensions."))
        
        ax_x_nm <- if (!is.null(qn_coord)) names(qn_coord)[min(ax_x, ncol(qn_coord))]
                   else names(ql_coord)[min(ax_x, ncol(ql_coord))]
        ax_y_nm <- if (!is.null(qn_coord)) names(qn_coord)[min(ax_y, ncol(qn_coord))]
                   else names(ql_coord)[min(ax_y, ncol(ql_coord))]
        pct_x <- round(eig[min(ax_x, nrow(eig)), 2], 1)
        pct_y <- round(eig[min(ax_y, nrow(eig)), 2], 1)
        
        color_choice <- input$rdim_circle_color %||% "cos2"
        
        # Construire df pour variables quantitatives (flèches + cercle)
        df_quanti <- NULL
        if (!is.null(qn_coord) && nrow(qn_coord) > 0) {
          qcx <- qn_coord[[ax_x_nm]]; qcy <- qn_coord[[ax_y_nm]]
          qcs <- (qn_cos2[[ax_x_nm]] + qn_cos2[[ax_y_nm]]) / 2
          qct <- (qn_contrib[[ax_x_nm]] + qn_contrib[[ax_y_nm]]) / 2
          qcol <- switch(color_choice, cos2=qcs, contrib=qct, rep(0.5, length(qcx)))
          df_quanti <- tibble(
            Variable = rownames(qn_coord), Dim_x = qcx, Dim_y = qcy,
            Qualite = round(qcol, 3), Cos2 = round(qcs, 3), Contrib = round(qct, 3),
            Type = "Numérique",
            text = paste0("<b>", rownames(qn_coord), "</b> [num]<br>",
                          ax_x_nm, " = ", round(qcx,3), "<br>",
                          ax_y_nm, " = ", round(qcy,3), "<br>",
                          "Cos² moy = ", round(qcs,3), "<br>",
                          "Contrib moy = ", round(qct,2), "%")
          )
        }
        
        # Construire df pour variables qualitatives (points, pas de flèches)
        df_quali <- NULL
        if (!is.null(ql_coord) && nrow(ql_coord) > 0) {
          lcx <- ql_coord[[ax_x_nm]]; lcy <- ql_coord[[ax_y_nm]]
          lcs <- (ql_cos2[[ax_x_nm]] + ql_cos2[[ax_y_nm]]) / 2
          lct <- (ql_contrib[[ax_x_nm]] + ql_contrib[[ax_y_nm]]) / 2
          lcol <- switch(color_choice, cos2=lcs, contrib=lct, rep(0.5, length(lcx)))
          df_quali <- tibble(
            Variable = rownames(ql_coord), Dim_x = lcx, Dim_y = lcy,
            Qualite = round(lcol, 3), Cos2 = round(lcs, 3), Contrib = round(lct, 3),
            Type = "Catégorielle",
            text = paste0("<b>", rownames(ql_coord), "</b> [cat]<br>",
                          ax_x_nm, " = ", round(lcx,3), "<br>",
                          ax_y_nm, " = ", round(lcy,3), "<br>",
                          "Cos² moy = ", round(lcs,3), "<br>",
                          "Contrib moy = ", round(lct,2), "%")
          )
        }
        
        # Cercle unitaire (uniquement si variables quantitatives présentes)
        theta     <- seq(0, 2*pi, length.out=200)
        circle_df <- tibble(x=cos(theta), y=sin(theta))
        
        # Déterminer les limites du graphique
        all_x <- c(if(!is.null(df_quanti)) df_quanti$Dim_x, if(!is.null(df_quali)) df_quali$Dim_x)
        all_y <- c(if(!is.null(df_quanti)) df_quanti$Dim_y, if(!is.null(df_quali)) df_quali$Dim_y)
        lim_max <- max(1.15, max(abs(c(all_x, all_y)), na.rm=TRUE) * 1.15)
        
        p <- ggplot()
        
        # Cercle unitaire (référence pour variables numériques)
        if (!is.null(df_quanti)) {
          p <- p +
            geom_path(data=circle_df, aes(x=x, y=y),
                      color="gray75", linewidth=.4, linetype="dashed") +
            geom_segment(data=df_quanti,
                         aes(x=0, y=0, xend=Dim_x, yend=Dim_y, color=Qualite, text=text),
                         linewidth=.9,
                         arrow=arrow(length=unit(.22,"cm"), type="closed")) +
            geom_text(data=df_quanti, aes(x=Dim_x, y=Dim_y, label=Variable, color=Qualite),
                      size=3, vjust=-0.6, fontface="bold")
        }
        
        # Variables catégorielles : points + étiquettes (pas de flèches, pas de cercle)
        if (!is.null(df_quali)) {
          p <- p +
            geom_point(data=df_quali,
                       aes(x=Dim_x, y=Dim_y, text=text),
                       shape=17, size=3.5, color="#8E44AD", alpha=.85) +
            geom_text(data=df_quali,
                      aes(x=Dim_x, y=Dim_y, label=Variable),
                      size=2.8, vjust=-0.7, color="#8E44AD", fontface="italic")
        }
        
        color_label <- switch(color_choice,
                              cos2="Cos² moyen", contrib="Contrib. (%)", "Qualité")
        
        p <- p +
          geom_hline(yintercept=0, color="gray70", linewidth=.3) +
          geom_vline(xintercept=0, color="gray70", linewidth=.3) +
          scale_color_gradientn(
            colors = c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C"),
            name   = color_label
          ) +
          coord_fixed() +
          xlim(-lim_max, lim_max) + ylim(-lim_max, lim_max) +
          labs(x=paste0(ax_x_nm," (",pct_x,"%)"),
               y=paste0(ax_y_nm," (",pct_y,"%)"),
               title="FAMD — Variables sur le plan factoriel",
               subtitle=paste0("Flèches = numériques (cercle unitaire) · ",
                               "Triangles violets = catégorielles (centroïdes)")) +
          theme_minimal(base_size=12) +
          theme(legend.position="right",
                plot.subtitle=element_text(size=9, color="gray50"))
        
        rdim_gg$circle <- p
        return(ggplotly(p, tooltip="text") |> nutri_config())
      }
      
      # ── PCA / MCA : cercle de corrélations classique ──────────────
      coord   <- vi$coord
      cos2_df <- vi$cos2
      contrib_df <- vi$contrib
      validate(need(ncol(coord) >= 2,
        "Au moins 2 dimensions nécessaires pour le cercle des corrélations. Augmentez le nombre de dimensions à garder."))

      ax_x_nm <- names(coord)[min(ax_x, ncol(coord))]
      ax_y_nm <- names(coord)[min(ax_y, ncol(coord))]
      cx <- coord[[ax_x_nm]]; cy <- coord[[ax_y_nm]]
      cs <- (cos2_df[[ax_x_nm]] + cos2_df[[ax_y_nm]]) / 2
      ct <- (contrib_df[[ax_x_nm]] + contrib_df[[ax_y_nm]]) / 2

      color_val <- switch(input$rdim_circle_color %||% "cos2",
        cos2   = cs,
        contrib= ct,
        none   = rep(0.5, length(cx))
      )

      df_v <- tibble(
        Variable = rownames(coord),
        Dim_x = cx, Dim_y = cy,
        Qualite = round(color_val, 3),
        Cos2    = round(cs, 3),
        Contrib = round(ct, 3),
        text = paste0("<b>", rownames(coord), "</b><br>",
                      ax_x_nm, " = ", round(cx,3), "<br>",
                      ax_y_nm, " = ", round(cy,3), "<br>",
                      "Cos² moy = ", round(cs,3), "<br>",
                      "Contrib moy = ", round(ct,2),"%")
      )

      theta      <- seq(0, 2*pi, length.out=200)
      circle_df  <- tibble(x=cos(theta), y=sin(theta))
      pct_x <- round(eig[min(ax_x,nrow(eig)), 2], 1)
      pct_y <- round(eig[min(ax_y,nrow(eig)), 2], 1)

      # Limites adaptatives : cercle unitaire pour PCA, flexible pour MCA
      lim_val <- if (method == "pca") 1.15
                 else max(1.15, max(abs(c(cx, cy)), na.rm=TRUE) * 1.15)

      p <- ggplot(df_v, aes(x=Dim_x, y=Dim_y, text=text)) +
        {if (method == "pca")
          geom_path(data=circle_df, aes(x=x,y=y),
                    color="gray80", linewidth=.5, inherit.aes=FALSE)
        } +
        geom_segment(aes(xend=0, yend=0, color=Qualite),
                     linewidth=.9,
                     arrow=arrow(length=unit(.22,"cm"), type="closed")) +
        geom_point(aes(color=Qualite), size=2.5) +
        geom_text(aes(label=Variable, color=Qualite),
                  size=3, vjust=-0.6, fontface="bold") +
        geom_hline(yintercept=0, color="gray70", linewidth=.3) +
        geom_vline(xintercept=0, color="gray70", linewidth=.3) +
        scale_color_gradientn(
          colors = c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C"),
          name   = switch(input$rdim_circle_color %||% "cos2",
                          cos2="Cos² moyen", contrib="Contrib. (%)", "Qualité")
        ) +
        coord_fixed() + xlim(-lim_val, lim_val) + ylim(-lim_val, lim_val) +
        labs(x=paste0(ax_x_nm," (",pct_x,"%)"),
             y=paste0(ax_y_nm," (",pct_y,"%)"),
             title=paste0(toupper(method), " — Cercle des corrélations"),
             subtitle="Couleur = qualité de représentation (Cos²)") +
        theme_minimal(base_size=12) +
        theme(legend.position="right",
              plot.subtitle=element_text(size=9,color="gray50"))
      rdim_gg$circle <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Cercle non disponible:",e$message),x=.5)))
  })

  output$rdim_biplot <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      ii  <- get_ind_info(r, method)
      req(!is.null(ii))
      n_ax <- min(2, ncol(ii$coord))
      validate(need(n_ax >= 2, "Au moins 2 dimensions nécessaires pour la carte des individus. Augmentez le nombre de dimensions à garder."))
      ind_coord <- ii$coord[, 1:n_ax, drop=FALSE]
      names(ind_coord) <- paste0("Dim", seq_len(n_ax))
      ind_coord$Index  <- 1:nrow(ind_coord)
      # Cos² individus
      cos2_ind <- tryCatch(rowSums(ii$cos2[, 1:n_ax, drop=FALSE]),
                           error=function(e) rep(0.5, nrow(ind_coord)))
      ind_coord$Cos2   <- round(cos2_ind, 3)

      eig   <- get_eigenvalue(r)
      pct1  <- round(eig[1,2], 1)
      pct2  <- if (nrow(eig) >= 2) round(eig[2,2], 1) else 0

      # Groupage (variable catégorielle)
      grp_var <- input$rdim_biplot_grp %||% input$rdim_grp %||% ""
      d <- df_f()

      color_mode <- input$rdim_ind_color %||% "grp"

      if (color_mode == "grp" && !is.null(grp_var) && nchar(grp_var) > 0
          && grp_var %in% names(d) && nrow(d) == nrow(ind_coord)) {
        ind_coord$Groupe <- as.character(d[[grp_var]])
        n_grp <- length(unique(ind_coord$Groupe))
        p <- ggplot(ind_coord, aes(x=Dim1, y=Dim2,
            color=Groupe,
            text=paste0("Obs. ", Index, "<br>",
                        grp_var," : ", Groupe, "<br>",
                        "Dim1=",round(Dim1,3)," | Dim2=",round(Dim2,3),"<br>",
                        "Cos²=",Cos2))) +
          geom_point(alpha=.65, size=2) +
          scale_color_manual(values=gpal(n_grp,"cat"), name=grp_var) +
          stat_ellipse(aes(fill=Groupe), geom="polygon",
                       alpha=.08, type="t", level=0.95, show.legend=FALSE) +
          scale_fill_manual(values=gpal(n_grp,"cat"))
      } else if (color_mode == "cos2") {
        p <- ggplot(ind_coord, aes(x=Dim1, y=Dim2, color=Cos2,
            text=paste0("Obs. ", Index, "<br>",
                        "Dim1=",round(Dim1,3)," | Dim2=",round(Dim2,3),"<br>",
                        "Cos²=",Cos2))) +
          geom_point(alpha=.7, size=2) +
          scale_color_gradientn(
            colors=c("#BDC3C7","#3498DB","#00838F","#E67E22","#E74C3C"),
            name="Cos²"
          )
      } else {
        # Densité 2D (coloration par densité)
        p <- ggplot(ind_coord, aes(x=Dim1, y=Dim2,
            text=paste0("Obs. ",Index))) +
          geom_point(alpha=.5, size=1.8, color=gprimary()) +
          geom_density_2d_filled(alpha=.35, show.legend=FALSE) +
          scale_fill_viridis_d(option="turbo")
      }

      p <- p +
        geom_hline(yintercept=0, color="gray70", linewidth=.3) +
        geom_vline(xintercept=0, color="gray70", linewidth=.3) +
        labs(x=paste0("Dim 1 (",pct1,"%)"),
             y=paste0("Dim 2 (",pct2,"%)"),
             title="Carte des individus",
             subtitle=if(color_mode=="grp" && nchar(grp_var)>0)
               paste0("Ellipses de confiance 95% par groupe (",grp_var,")")
             else if(color_mode=="cos2") "Couleur = qualité de représentation (Cos²)"
             else "Couleur = densité 2D") +
        theme_minimal(base_size=12) +
        theme(legend.position="right",
              plot.subtitle=element_text(size=9, color="gray50"))

      rdim_gg$biplot <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  output$rdim_contrib <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      dim_sel <- as.integer(input$rdim_contrib_dim %||% 1)
      vi      <- get_var_info(r, method)
      req(!is.null(vi))
      contrib  <- vi$contrib
      # Retirer la colonne .var_type si présente (FAMD)
      if (".var_type" %in% names(contrib)) contrib <- contrib[, names(contrib) != ".var_type", drop=FALSE]
      n_dim    <- min(ncol(contrib), 10)
      contrib  <- contrib[,1:n_dim, drop=FALSE]
      contrib$Variable <- rownames(contrib)

      # Seuil uniforme 1/p × 100
      seuil <- 100 / nrow(contrib)
      dim_nm <- names(contrib)[dim_sel]
      contrib$contrib_sel <- contrib[[dim_nm]]
      contrib$Depasse <- contrib$contrib_sel >= seuil

      p <- ggplot(contrib,
            aes(x=fct_reorder(Variable, contrib_sel),
                y=contrib_sel,
                fill=contrib_sel,
                text=paste0("<b>", Variable, "</b><br>",
                            "Contribution : ", round(contrib_sel,2), "%<br>",
                            if(isTRUE(input$rdim_contrib_thresh))
                              paste0("Seuil (1/p)×100 : ", round(seuil,2),
                                     "%<br>", ifelse(contrib_sel>=seuil,"✅ Au-dessus","❌ En-dessous"))
                            else ""))) +
        geom_col(alpha=.9, width=.75) +
        scale_fill_gradientn(
          colors=c("#BDC3C7","#5DADE2","#1A5276","#E67E22","#C0392B"),
          name="Contribution (%)"
        ) +
        {if(isTRUE(input$rdim_contrib_thresh))
          geom_hline(yintercept=seuil, linetype="dashed",
                     color="#E74C3C", linewidth=.8)
        } +
        {if(isTRUE(input$rdim_contrib_thresh))
          annotate("text", x=nrow(contrib)*0.85, y=seuil+0.3,
                   label=paste0("Seuil=",round(seuil,1),"%"),
                   size=3, color="#E74C3C", fontface="bold")
        } +
        labs(x="", y="Contribution (%)",
             title=paste0("Contributions — ", dim_nm),
             subtitle=paste0("Seuil d'équi-contribution : ",round(seuil,1),"% (1/",nrow(contrib)," vars)")) +
        theme_minimal(base_size=12) +
        theme(legend.position="right",
              plot.subtitle=element_text(size=9,color="gray50"))
      rdim_gg$contrib <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  output$rdim_cos2 <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      vi    <- get_var_info(r, method)
      req(!is.null(vi))
      cos2  <- vi$cos2
      # Retirer la colonne .var_type si présente (FAMD)
      if (".var_type" %in% names(cos2)) cos2 <- cos2[, names(cos2) != ".var_type", drop=FALSE]
      n_dim <- min(ncol(cos2), 6)
      cos2  <- cos2[,1:n_dim, drop=FALSE]
      cos2$Variable <- rownames(cos2)
      df_long <- pivot_longer(cos2, -Variable, names_to="Dimension", values_to="Cos2")

      # Palette séquentielle bleu clair → violet foncé par dimension
      n_d    <- n_dim
      dim_pal <- colorRampPalette(c("#AED6F1","#2980B9","#1A5276","#8E44AD","#6C3483"))(n_d)

      n_vars <- length(unique(df_long$Variable))
      df_long$Variable <- truncate_label(df_long$Variable, 18)
      use_horiz <- n_vars > 12
      p <- if (use_horiz) {
        ggplot(df_long,
              aes(y=fct_reorder(Variable, Cos2, .fun=max),
                  x=Cos2, fill=Dimension,
                  text=paste0("<b>",Variable,"</b><br>",
                              Dimension," : Cos²=",round(Cos2,3),"<br>",
                              "Qualité : ",
                              case_when(Cos2>=0.7~"Très bonne",
                                        Cos2>=0.5~"Bonne",
                                        Cos2>=0.3~"Acceptable",
                                        TRUE~"Faible")))) +
          geom_col(position="dodge", alpha=.9, width=.75) +
          scale_fill_manual(values=setNames(dim_pal, paste0("Dim.",1:n_d)),
                            name="Dimension") +
          geom_vline(xintercept=0.5, linetype="dashed", color="#E74C3C", linewidth=.6) +
          labs(y="", x="Cos²",
               title="Cos² — Qualité de représentation",
               subtitle="Cos² > 0.5 = bonne représentation") +
          theme_nutri(11) +
          theme(legend.position="right")
      } else {
        ggplot(df_long,
              aes(x=fct_reorder(Variable, Cos2, .fun=max),
                  y=Cos2, fill=Dimension,
                  text=paste0("<b>",Variable,"</b><br>",
                              Dimension," : Cos²=",round(Cos2,3),"<br>",
                              "Qualité : ",
                              case_when(Cos2>=0.7~"Très bonne",
                                        Cos2>=0.5~"Bonne",
                                        Cos2>=0.3~"Acceptable",
                                        TRUE~"Faible")))) +
          geom_col(position="dodge", alpha=.9, width=.75) +
          scale_fill_manual(values=setNames(dim_pal, paste0("Dim.",1:n_d)),
                            name="Dimension") +
          geom_hline(yintercept=0.5, linetype="dashed", color="#E74C3C", linewidth=.6) +
          labs(x="", y="Cos²",
               title="Cos² — Qualité de représentation",
               subtitle="Cos² > 0.5 = bonne représentation") +
          theme_nutri(11) + smart_axis_x(n_vars) +
          theme(legend.position="right")
      }
      rdim_gg$cos2 <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })

  # ── Heatmap contributions (toutes dimensions × toutes variables) ──
  output$rdim_heatmap_contrib <- renderPlotly({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    tryCatch({
      vi      <- get_var_info(r, method)
      req(!is.null(vi))
      contrib  <- vi$contrib
      # Retirer la colonne .var_type si présente (FAMD)
      if (".var_type" %in% names(contrib)) contrib <- contrib[, names(contrib) != ".var_type", drop=FALSE]
      n_dim    <- min(ncol(contrib), 8)
      contrib  <- contrib[,1:n_dim, drop=FALSE]
      contrib$Variable <- rownames(contrib)
      df_long  <- pivot_longer(contrib, -Variable,
                               names_to="Dimension", values_to="Contrib")

      # Ordonner les variables par contribution totale
      ordre <- contrib |>
        mutate(total=rowSums(across(-Variable))) |>
        arrange(desc(total)) |>
        pull(Variable)
      df_long$Variable <- factor(df_long$Variable, levels=ordre)

      p <- ggplot(df_long, aes(x=Dimension, y=Variable, fill=Contrib,
            text=paste0("<b>",Variable,"</b><br>",
                        Dimension," : ",round(Contrib,2),"%"))) +
        geom_tile(color="white", linewidth=.5) +
        geom_text(aes(label=round(Contrib,1)), size=2.8, color="white", fontface="bold") +
        scale_fill_gradientn(
          colors=c("#F0F3F4","#AED6F1","#2980B9","#1A5276","#6C3483","#4A235A"),
          name="Contribution (%)",
          guide=guide_colorbar(barwidth=.8, barheight=8)
        ) +
        scale_y_discrete(labels = function(x) truncate_label(x, 18)) +
        labs(x="Dimension", y="Variable",
             title="Heatmap des contributions variables × dimensions",
             subtitle="Plus la couleur est foncée, plus la variable contribue à l'axe") +
        theme_nutri() +
        theme(axis.text.x=element_text(face="bold"))
      rdim_gg$heatmap <- p
      ggplotly(p, tooltip="text") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Heatmap non disponible:",e$message),x=.5)))
  })

  # ── Aussi enregistrer le scree dans rdim_gg ───────────────────────
  # (le renderPlotly du scree est défini plus haut — on ajoute l'enregistrement via observe)
  observeEvent(rdim_res(), {
    req(rdim_res()); r <- rdim_res()$res
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))
    df_eig <- tibble(
      Dim=paste0("Dim.",1:nrow(eig)),
      Lambda=round(eig[,1],4), Variance_pct=round(eig[,2],2),
      Variance_cum=round(eig[,3],2),
      Kaiser=ifelse(eig[,1]>1,"Retenu (λ > 1)","Rejeté (λ ≤ 1)")
    )
    bs <- broken_stick(nrow(df_eig)); df_eig$BatonBrise <- round(bs,2)
    rdim_gg$scree <- ggplot(df_eig,aes(x=Dim,y=Variance_pct,fill=Kaiser)) +
      geom_col(alpha=.88,width=.72) +
      geom_line(aes(y=BatonBrise,group=1),color="#9B59B6",linewidth=1.1,linetype="dotted") +
      geom_line(aes(y=Variance_cum,group=1),color=C$rouge,linewidth=1.3) +
      geom_hline(yintercept=80,linetype="dashed",color="gray50") +
      scale_fill_manual(values=c("Retenu (λ > 1)"="#00838F","Rejeté (λ ≤ 1)"="#BDC3C7")) +
      labs(x="Dimension",y="Variance (%)",title="Éboulis — Critère de Kaiser") +
      theme_minimal(base_size=12)
  })

  # ── Onglet Sélection automatique des axes ────────────────────────
  output$rdim_selec_auto <- renderUI({
    req(rdim_res()); r <- rdim_res()$res; method <- rdim_res()$method
    eig <- tryCatch(get_eigenvalue(r), error=function(e) NULL)
    req(!is.null(eig))

    n_vars    <- nrow(eig)
    lambdas   <- eig[,1]
    var_pct   <- eig[,2]
    var_cum   <- eig[,3]

    # ─ Kaiser : λ > 1
    n_kaiser  <- max(1, sum(lambdas > 1))
    cum_kaiser<- round(var_cum[n_kaiser], 1)

    # ─ Bâton brisé
    bs        <- broken_stick(n_vars)
    n_bs      <- max(1, sum(var_pct > bs))
    cum_bs    <- round(var_cum[n_bs], 1)

    # ─ Variance 70% / 80%
    n_v70 <- max(1, which(var_cum >= 70)[1])
    n_v80 <- max(1, which(var_cum >= 80)[1])

    # ─ Coude (elbow) : 2ème dérivée du scree
    n_elbow <- if (n_vars >= 3) {
      d2 <- diff(diff(var_pct))
      max(1, which.max(abs(d2)) + 1)
    } else 1

    # ─ Recommandation synthèse (vote majoritaire)
    votes <- c(n_kaiser, n_bs, n_elbow)
    n_recommande <- as.integer(names(sort(table(votes), decreasing=TRUE))[1])

    # ─ Tableau synthèse
    tbl_synth <- tibble(
      Critère   = c("📐 Kaiser (λ > 1)",
                    "🥢 Bâton brisé",
                    "📏 Coude du scree",
                    "📊 Variance ≥ 70%",
                    "📊 Variance ≥ 80%"),
      `Axes retenus` = c(n_kaiser, n_bs, n_elbow, n_v70, n_v80),
      `Variance cumulée (%)` = c(cum_kaiser, cum_bs,
                                 round(var_cum[n_elbow],1),
                                 round(var_cum[n_v70],1),
                                 round(var_cum[n_v80],1)),
      Justification = c(
        paste0(sum(lambdas>1)," valeurs propres > 1"),
        paste0(n_bs," axes au-dessus du modèle nul"),
        paste0("Inflexion maximale à la dim. ",n_elbow),
        paste0("Seuil 70% atteint à la dim. ",n_v70),
        paste0("Seuil 80% atteint à la dim. ",n_v80)
      )
    )

    tagList(
      # Bannière recommandation
      tags$div(style=paste0(
        "background:linear-gradient(135deg,#0B3954 0%,#00838F 100%);",
        "color:white;padding:16px 20px;border-radius:10px;margin-bottom:16px;"
      ),
        fluidRow(
          column(8,
            tags$h4(style="margin:0;font-weight:700;",
                    "🎯 Recommandation FactoInvestigate"),
            tags$p(style="margin:4px 0 0;font-size:14px;opacity:.9;",
                   paste0("Retenir ", tags$b(n_recommande), " axe",
                          if(n_recommande>1)"s", " factoriel",
                          if(n_recommande>1)"s")  |> as.character()),
            tags$p(style="margin:2px 0 0;font-size:12px;opacity:.8;",
                   paste0("Variance expliquée : ",
                          round(var_cum[min(n_recommande, n_vars)],1),
                          "% — Vote majoritaire de ",length(votes)," critères"))
          ),
          column(4, style="text-align:center;",
            tags$div(style=paste0(
              "background:rgba(255,255,255,.2);border-radius:50%;",
              "width:80px;height:80px;display:flex;align-items:center;",
              "justify-content:center;margin:auto;font-size:36px;font-weight:900;"
            ), n_recommande)
          )
        )
      ),

      # Tableau des critères
      fluidRow(
        column(12,
          tags$div(class="section-header", "Comparaison des critères de sélection"),
          renderDT(
            tbl_synth |>
              datatable(rownames=FALSE,
                        options=list(dom="t", pageLength=10, scrollX=TRUE),
                        class="compact stripe hover") |>
              formatStyle("Axes retenus",
                background=styleEqual(
                  unique(tbl_synth$`Axes retenus`),
                  ifelse(unique(tbl_synth$`Axes retenus`) == n_recommande,
                         "#d5f5e3","white")
                )) |>
              formatStyle("Variance cumulée (%)",
                background=styleInterval(c(60,70,80),
                  c("#FDEDEC","#FDEBD0","#D5F5E3","#A9DFBF")))
          )
        )
      ),
      br(),

      # Tableau complet valeurs propres avec Kaiser highlight
      fluidRow(
        column(12,
          tags$div(class="section-header","Tableau des valeurs propres (Critère de Kaiser)"),
          renderDT({
            tibble(
              Dimension      = paste0("Dim.", 1:n_vars),
              `λ (val. propre)` = round(lambdas, 4),
              `Variance (%)`    = round(var_pct, 2),
              `Cumul (%)`       = round(var_cum, 2),
              `Bâton brisé (%)` = round(bs,       2),
              Kaiser            = ifelse(lambdas > 1, "✅ Retenu","❌ Rejeté"),
              `Brisée`          = ifelse(var_pct > bs,"✅ Retenu","❌ Rejeté")
            ) |>
              datatable(rownames=FALSE,
                        options=list(dom="t", pageLength=15),
                        class="compact stripe hover") |>
              formatStyle("Kaiser",
                color=styleEqual(c("✅ Retenu","❌ Rejeté"),
                                 c("#00838F","#C0392B")),
                fontWeight="bold") |>
              formatStyle(2,   # colonne λ (val. propre)
                background=styleInterval(1, c("#FDEDEC","#D5F5E3")))
          })
        )
      ),
      br(),

      # ── Description textuelle des axes (style FactoInvestigate) ───
      {
        vi <- get_var_info(rdim_res()$res, method)
        # Retirer .var_type si présente (FAMD)
        if (!is.null(vi)) {
          if (".var_type" %in% names(vi$contrib)) vi$contrib <- vi$contrib[, names(vi$contrib) != ".var_type", drop=FALSE]
          if (".var_type" %in% names(vi$cos2))    vi$cos2 <- vi$cos2[, names(vi$cos2) != ".var_type", drop=FALSE]
        }
        if (!is.null(vi) && ncol(vi$contrib) >= 1) {
          n_desc <- min(n_recommande, ncol(vi$contrib))
          seuil_contrib <- 100 / nrow(vi$contrib)

          cartes <- lapply(seq_len(n_desc), function(ax) {
            dim_nm  <- names(vi$contrib)[ax]
            ct_vec  <- vi$contrib[[dim_nm]]
            cs_vec  <- vi$cos2[[dim_nm]]
            nms     <- rownames(vi$contrib)

            # Variables au-dessus du seuil, triées par contribution décroissante
            top_idx <- order(ct_vec, decreasing=TRUE)
            top_idx <- top_idx[ct_vec[top_idx] >= seuil_contrib]
            if (length(top_idx) == 0) top_idx <- order(ct_vec, decreasing=TRUE)[1:3]
            top_vars <- nms[top_idx]
            top_ct   <- round(ct_vec[top_idx], 1)
            top_cs   <- round(cs_vec[top_idx], 3)

            # Qualité globale de l'axe
            qualite_axe <- case_when(
              var_pct[ax] >= 25 ~ "très élevée",
              var_pct[ax] >= 15 ~ "élevée",
              var_pct[ax] >= 8  ~ "modérée",
              TRUE              ~ "faible"
            )
            kaiser_txt <- if (lambdas[ax] > 1) "retenu (λ > 1)" else "rejeté (λ ≤ 1)"

            tags$div(
              style=paste0("border:1px solid #e0e8f0;border-radius:8px;padding:14px;",
                           "margin-bottom:10px;background:white;"),
              tags$div(style="display:flex;align-items:center;gap:10px;margin-bottom:8px;",
                tags$div(style=paste0(
                  "background:",if(lambdas[ax]>1)"#00838F" else "#BDC3C7",
                  ";color:white;border-radius:50%;width:32px;height:32px;",
                  "display:flex;align-items:center;justify-content:center;",
                  "font-weight:700;font-size:14px;flex-shrink:0;"), ax),
                tags$div(
                  tags$b(style="font-size:14px;color:#0B3954;",
                         paste0(dim_nm, " — ", round(var_pct[ax],1),
                                "% de variance (Kaiser : ", kaiser_txt, ")")),
                  tags$br(),
                  tags$span(style="font-size:11px;color:#888;",
                            paste0("Variance cumulée jusqu'à cet axe : ",
                                   round(var_cum[ax],1), "% | ",
                                   "Puissance de discrimination : ", qualite_axe))
                )
              ),
              tags$p(style="font-size:12px;color:#444;margin:0 0 6px;",
                tags$b("Variables structurantes (contribution ≥ seuil équi-contribution ",
                       round(seuil_contrib,1),"%) :"),
              ),
              tags$div(style="display:flex;flex-wrap:wrap;gap:6px;",
                lapply(seq_along(top_vars), function(j) {
                  cs_j <- top_cs[j]
                  badge_col <- if(cs_j >= 0.7)"#00838F" else if(cs_j>=0.5)"#E67E22" else "#BDC3C7"
                  tags$span(style=paste0(
                    "background:",badge_col,";color:white;border-radius:5px;",
                    "padding:3px 8px;font-size:11px;font-weight:600;"),
                    paste0(top_vars[j]," (", top_ct[j],"% | cos²=",top_cs[j],")"))
                })
              ),
              tags$p(style="font-size:11px;color:#888;margin:8px 0 0;font-style:italic;",
                paste0("Cet axe oppose ou résume principalement : ",
                       paste(head(top_vars, 3), collapse=", "), ".",
                       if (length(top_vars) > 3) paste0(" + ", length(top_vars)-3, " autre(s).") else ""))
            )
          })

          tagList(
            tags$div(class="section-header",
                     paste0("📝 Description des ", n_desc, " axe",
                            if(n_desc>1)"s", " retenus")),
            tagList(cartes)
          )
        } else NULL
      }
    )
  })

  output$rdim_table <- renderDT({
    req(rdim_res()); r <- rdim_res()$res
    tryCatch({
      eig <- get_eigenvalue(r)
      tibble(
        Dimension         = paste0("Dim.",1:nrow(eig)),
        `λ (val. propre)` = round(eig[,1],4),
        `Variance (%)`    = round(eig[,2],2),
        `Cumul (%)`       = round(eig[,3],2),
        Kaiser            = ifelse(eig[,1]>1,"✅ λ>1","❌ λ≤1")
      ) |>
        datatable(rownames=FALSE,
                  options=list(dom="t", pageLength=20, scrollX=TRUE),
                  class="compact stripe hover") |>
        formatStyle("Kaiser",
          color=styleEqual(c("✅ λ>1","❌ λ≤1"), c("#00838F","#C0392B")),
          fontWeight="bold") |>
        formatStyle(2,   # colonne λ
          background=styleInterval(1, c("#FDEDEC","#D5F5E3")))
    }, error=function(e) datatable(data.frame(Erreur=e$message),rownames=FALSE))
  })


  # ══════════════════════════════════════════════════════════════════
  # RDIM EXPERT — t-SNE / UMAP / Parallèle / HCPC / Bootstrap
  # ══════════════════════════════════════════════════════════════════

  rdim_tsne_res  <- reactiveVal(NULL)
  rdim_umap_res  <- reactiveVal(NULL)
  rdim_pa_res    <- reactiveVal(NULL)
  rdim_hcpc_res  <- reactiveVal(NULL)
  rdim_boot_res  <- reactiveVal(NULL)

  # ── UI dynamiques : selecteurs de variable groupe ──────────────
  output$tsne_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("tsne_grp"),"Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$umap_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("umap_grp"),"Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$cmp_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("cmp_grp"),"Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$boot_dim_ui <- renderUI({
    r <- rdim_boot_res()
    if (is.null(r)) dims <- paste0("Dim.",1:3)
    else dims <- unique(r$table$Dimension)
    selectInput(ns("boot_dim"),"Dimension :", choices=dims, width="200px")
  })

  # ── 1. Analyse Parallèle (Horn) ───────────────────────────────
  observeEvent(input$run_parallel, {
    d <- df_f(); req(d)
    withProgress(message="Analyse parallèle en cours…", value=0.3, {
      tryCatch({
        pa <- calc_parallel_analysis(d, n_sim=input$pa_nsim)
        rdim_pa_res(pa)
        incProgress(0.7)
        showNotification(paste0("✅ Analyse parallèle : ",pa$n_retained," axe(s) retenu(s)"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$pa_plot <- renderPlotly({
    pa <- rdim_pa_res(); req(!is.null(pa))
    tryCatch(plot_parallel_analysis(pa), error=function(e) plotly_empty())
  })

  output$pa_table <- renderDT({
    pa <- rdim_pa_res(); req(!is.null(pa))
    pa$table |>
      mutate(Retenu=ifelse(Retenu,"✅ Significatif","❌ Non significatif")) |>
      datatable(rownames=FALSE, options=list(dom="t",pageLength=15),
                class="compact stripe hover") |>
      formatStyle("Retenu",
        color=styleEqual(c("✅ Significatif","❌ Non significatif"),c("#00838F","#C0392B")),
        fontWeight="bold")
  })

  # ── 2. HCPC ───────────────────────────────────────────────────
  observeEvent(input$run_hcpc, {
    r <- rdim_res(); req(!is.null(r), r$method %in% c("pca","mca","famd"))
    withProgress(message="HCPC en cours…", value=0.3, {
      tryCatch({
        hcpc <- run_hcpc(r$res, nb_clust=input$hcpc_nclust)
        rdim_hcpc_res(hcpc)
        incProgress(0.7)
        showNotification(paste0("✅ HCPC : ",hcpc$n_clust," clusters (inertie inter=",
                                hcpc$inertia_pct,"%)"), type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$hcpc_map <- renderPlotly({
    r <- rdim_res(); hcpc <- rdim_hcpc_res()
    req(!is.null(r), !is.null(hcpc))
    tryCatch(plot_hcpc_map(r$res, hcpc), error=function(e) plotly_empty())
  })

  output$hcpc_dendro <- renderPlotly({
    hcpc <- rdim_hcpc_res(); req(!is.null(hcpc))
    tryCatch(plot_hcpc_dendrogram(hcpc), error=function(e) plotly_empty())
  })

  output$hcpc_desc_table <- renderDT({
    hcpc <- rdim_hcpc_res(); req(!is.null(hcpc))
    desc <- tryCatch(hcpc_cluster_description(hcpc), error=function(e) NULL)
    if (is.null(desc) || nrow(desc)==0) return(datatable(data.frame(Info="Pas de description quantitative disponible")))
    datatable(desc, rownames=FALSE, options=list(dom="tip",pageLength=15,scrollX=TRUE),
              class="compact stripe hover")
  })

  # ── 3. t-SNE ──────────────────────────────────────────────────
  observeEvent(input$run_tsne, {
    d <- df_f(); req(d)
    withProgress(message="t-SNE en cours…", value=0.3, {
      tryCatch({
        ts <- run_tsne(d, perplexity=input$tsne_perp, max_iter=input$tsne_iter)
        rdim_tsne_res(ts)
        incProgress(0.7)
        showNotification(paste0("✅ t-SNE terminé (n=",ts$n_obs,", perp=",ts$perplexity,")"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$tsne_plot <- renderPlotly({
    ts <- rdim_tsne_res(); req(!is.null(ts))
    d <- df_f()
    grp <- if (!is.null(input$tsne_grp) && input$tsne_grp != "" && input$tsne_grp %in% names(d)) {
      d_clean <- d |> select(where(is.numeric)) |> tidyr::drop_na()
      # Aligner les indices
      idx <- which(complete.cases(d |> select(where(is.numeric))))
      d[[input$tsne_grp]][idx[seq_len(nrow(ts$coords))]]
    } else NULL
    tryCatch(plot_tsne(ts, grp), error=function(e) plotly_empty())
  })

  # ── 4. UMAP ───────────────────────────────────────────────────
  observeEvent(input$run_umap, {
    d <- df_f(); req(d)
    withProgress(message="UMAP en cours…", value=0.3, {
      tryCatch({
        um <- run_umap(d, n_neighbors=input$umap_nn, min_dist=input$umap_md)
        rdim_umap_res(um)
        incProgress(0.7)
        showNotification(paste0("✅ UMAP terminé (n=",um$n_obs,", voisins=",um$n_neighbors,")"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$umap_plot <- renderPlotly({
    um <- rdim_umap_res(); req(!is.null(um))
    d <- df_f()
    grp <- if (!is.null(input$umap_grp) && input$umap_grp != "" && input$umap_grp %in% names(d)) {
      idx <- which(complete.cases(d |> select(where(is.numeric))))
      d[[input$umap_grp]][idx[seq_len(nrow(um$coords))]]
    } else NULL
    tryCatch(plot_umap(um, grp), error=function(e) plotly_empty())
  })

  # ── 5. Bootstrap Stabilité ────────────────────────────────────
  observeEvent(input$run_boot, {
    d <- df_f(); req(d)
    withProgress(message="Bootstrap loadings en cours…", value=0.2, {
      tryCatch({
        bt <- calc_pca_bootstrap(d, n_boot=input$boot_nsim, n_dim=3)
        rdim_boot_res(bt)
        incProgress(0.8)
        showNotification(paste0("✅ Bootstrap terminé (",bt$n_boot," rééchantillonnages)"),
                         type="message", duration=4)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  output$boot_plot <- renderPlotly({
    bt <- rdim_boot_res(); req(!is.null(bt), input$boot_dim)
    tryCatch(plot_pca_bootstrap(bt, input$boot_dim), error=function(e) plotly_empty())
  })

  output$boot_table <- renderDT({
    bt <- rdim_boot_res(); req(!is.null(bt))
    bt$table |>
      mutate(Stable=ifelse(Stable,"✅ Stable","⚠️ Instable")) |>
      datatable(rownames=FALSE, options=list(dom="tip",pageLength=20,scrollX=TRUE),
                class="compact stripe hover") |>
      formatStyle("Stable",
        color=styleEqual(c("✅ Stable","⚠️ Instable"),c("#2E7D52","#D98B14")),
        fontWeight="bold")
  })

  # ── 6. Comparaison méthodes (PCA vs t-SNE vs UMAP) ───────────
  observeEvent(input$run_cmp_methods, {
    d <- df_f(); req(d)
    grp <- if (!is.null(input$cmp_grp) && input$cmp_grp != "" && input$cmp_grp %in% names(d)) {
      idx <- which(complete.cases(d |> select(where(is.numeric))))
      d[[input$cmp_grp]][idx]
    } else NULL
    withProgress(message="Comparaison en cours…", value=0.2, {
      tryCatch({
        output$cmp_methods_plot <- renderPlotly({
          plot_methods_comparison(d, group_var=grp)
        })
        incProgress(0.8)
      }, error=function(e) showNotification(paste("Erreur:",e$message), type="error"))
    })
  })

  })
}
