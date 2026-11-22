# Extrait de app_v2_final.R — réduction de dimension — t-SNE, UMAP, HCPC, bootstrap
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
    selectInput("tsne_grp","Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$umap_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("umap_grp","Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$cmp_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("cmp_grp","Colorier par :", choices=c("Aucun"="",vc), width="100%")
  })
  output$boot_dim_ui <- renderUI({
    r <- rdim_boot_res()
    if (is.null(r)) dims <- paste0("Dim.",1:3)
    else dims <- unique(r$table$Dimension)
    selectInput("boot_dim","Dimension :", choices=dims, width="200px")
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

