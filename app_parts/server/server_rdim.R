# Extrait de app_v2_final.R — réduction de dimension — helpers et UI
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
      selectizeInput("rdim_vars","Variables numériques :", choices=vn,
                     selected=head(vn, min(8, length(vn))), multiple=TRUE, width="100%")
    } else if (method == "famd") {
      # FAMD : mix obligatoire — séparer les groupes visuellement
      choices_grouped <- list(
        "Variables numériques" = vn,
        "Variables catégorielles" = vc
      )
      selected_famd <- c(head(vn, min(4, length(vn))), head(vc, min(3, length(vc))))
      selectizeInput("rdim_vars","Variables mixtes (num. + cat.) :", choices=choices_grouped,
                     selected=selected_famd, multiple=TRUE, width="100%")
    } else if (method == "auto") {
      # Auto : toutes variables, groupées
      choices_grouped <- list(
        "Variables numériques" = vn,
        "Variables catégorielles" = vc
      )
      selectizeInput("rdim_vars","Variables :", choices=choices_grouped,
                     selected=head(vn, min(8, length(vn))), multiple=TRUE, width="100%")
    } else if (method == "mca") {
      # ACM : variables CATÉGORIELLES uniquement
      selectizeInput("rdim_vars","Variables catégorielles :", choices=vc,
                     selected=head(vc, min(5, length(vc))), multiple=TRUE, width="100%")
    } else {
      # AFC : 2 variables catégorielles
      tagList(
        selectInput("rdim_var1","Variable lignes :", choices=vc,
                    selected=vc[1], width="100%"),
        selectInput("rdim_var2","Variable colonnes :", choices=vc,
                    selected=if(length(vc)>1)vc[2] else vc[1], width="100%")
      )
    }
  })

  output$rdim_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("rdim_grp","Colorier individus par :",
                choices=c("Aucun"="", vc), selected="", width="100%")
  })

  output$rdim_biplot_grp_ui <- renderUI({
    vc <- vars_cat()
    selectInput("rdim_biplot_grp","Variable de groupage :",
                choices=c("Aucun"="", vc), selected=input$rdim_grp, width="100%")
  })

  output$rdim_contrib_dim_ui <- renderUI({
    req(rdim_res())
    eig <- tryCatch(get_eigenvalue(rdim_res()$res), error=function(e) NULL)
    n_dim <- if (!is.null(eig)) nrow(eig) else 5
    selectInput("rdim_contrib_dim","Dimension :",
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

