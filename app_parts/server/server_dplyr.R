# Extrait de app_v2_final.R — manipulation dplyr
  # ════════════════════════════════════════════════════════════════════
  # SERVER — MODULE DPLYR
  # ════════════════════════════════════════════════════════════════════
  
  # Données de travail dplyr (séparée de df_f pour ne pas perturber les autres onglets)
  dplyr_data <- reactiveVal(NULL)
  
  observe({
    df <- df_f(); req(df)
    dplyr_data(df)
  })

  # ── UI filtres dplyr ───────────────────────────────────────────────
  output$dplyr_filter_ui <- renderUI({
    df <- df_f(); req(df)
    vc <- names(df)[vapply(names(df), function(n) {
      x <- df[[n]]; !startsWith(n,".") &&
      (is.factor(x)||is.character(x)) && length(unique(na.omit(x)))<=30
    }, logical(1))]
    vn <- vars_num()
    tagList(
      selectInput("dplyr_f_var","Variable :",choices=c(vn,vc),width="100%"),
      uiOutput("dplyr_f_val_ui")
    )
  })
  
  output$dplyr_f_val_ui <- renderUI({
    req(input$dplyr_f_var); df <- df_f()
    v <- input$dplyr_f_var; x <- df[[v]]
    if (is.numeric(x)) {
      r <- range(x, na.rm=TRUE)
      sliderInput("dplyr_f_range","Plage :",min=r[1],max=r[2],value=r,width="100%")
    } else {
      vals <- sort(unique(na.omit(as.character(x))))
      checkboxGroupInput("dplyr_f_vals","Valeurs :",choices=vals,selected=vals)
    }
  })
  
  output$dplyr_select_ui <- renderUI({
    df <- df_f(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_sel_cols","Colonnes à garder :",
                   choices=vis,selected=head(vis,8),multiple=TRUE,width="100%")
  })
  
  output$dplyr_arrange_ui <- renderUI({
    vn <- vars_num(); vc <- vars_cat()
    selectInput("dplyr_arr_col","Trier par :",choices=c(vn,vc),width="100%")
  })
  
  output$dplyr_group_ui <- renderUI({
    vc <- vars_cat()
    selectizeInput("dplyr_grp_cols","Grouper par :",
                   choices=vc,selected=vc[1],multiple=TRUE,width="100%")
  })
  
  output$dplyr_summarise_ui <- renderUI({
    vn <- vars_num()
    tagList(
      checkboxGroupInput("dplyr_sum_fns","Fonctions :",
        choices=c("n (effectif)"="n","mean (moyenne)"="mean","median (médiane)"="median",
                  "sd (écart-type)"="sd","min"="min","max"="max","sum (somme)"="sum"),
        selected=c("n","mean","median"),inline=FALSE),
      selectizeInput("dplyr_sum_vars","Sur les variables :",
                     choices=vn,selected=head(vn,3),multiple=TRUE,width="100%")
    )
  })
  
  # ── Applications des opérations ────────────────────────────────────
  observeEvent(input$dplyr_apply_filter, {
    df <- df_f(); req(df, input$dplyr_f_var)
    v  <- input$dplyr_f_var; x <- df[[v]]
    tryCatch({
      result <- if (is.numeric(x) && !is.null(input$dplyr_f_range)) {
        filter(df, .data[[v]] >= input$dplyr_f_range[1] &
                   .data[[v]] <= input$dplyr_f_range[2])
      } else if (!is.null(input$dplyr_f_vals)) {
        filter(df, as.character(.data[[v]]) %in% input$dplyr_f_vals)
      } else df
      dplyr_data(result)
      showNotification(paste0("✅ Filtre appliqué : ",nrow(result)," lignes"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_select, {
    req(input$dplyr_sel_cols)
    tryCatch({
      result <- df_f() |> select(any_of(input$dplyr_sel_cols))
      dplyr_data(result)
      showNotification(paste0("✅ ",ncol(result)," colonnes sélectionnées"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_mutate, {
    req(input$dplyr_new_col, input$dplyr_expr)
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- safe_mutate(df, input$dplyr_new_col, input$dplyr_expr)
      dplyr_data(result)
      showNotification(paste0("✅ Colonne '",input$dplyr_new_col,"' créée"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌ Erreur expression:",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_arrange, {
    req(input$dplyr_arr_col)
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- if (input$dplyr_order == "desc")
        arrange(df, desc(.data[[input$dplyr_arr_col]]))
      else
        arrange(df, .data[[input$dplyr_arr_col]])
      dplyr_data(result)
      showNotification(paste0("✅ Trié par ",input$dplyr_arr_col," (",input$dplyr_order,")"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_apply_summarise, {
    req(input$dplyr_grp_cols, input$dplyr_sum_fns, input$dplyr_sum_vars)
    tryCatch({
      df <- dplyr_data(); req(df)
      fns_list <- list()
      if ("n" %in% input$dplyr_sum_fns)
        fns_list$N <- ~n()
      for (fn in setdiff(input$dplyr_sum_fns,"n")) {
        for (v in input$dplyr_sum_vars) {
          nm <- paste0(fn,"_",v)
          fns_list[[nm]] <- eval(parse(text=paste0("~",fn,"(.data[['",v,"']],na.rm=TRUE)")))
        }
      }
      result <- df |>
        group_by(across(all_of(input$dplyr_grp_cols))) |>
        summarise(!!!fns_list, .groups="drop") |>
        mutate(across(where(is.numeric), ~round(.,3)))
      dplyr_data(result)
      showNotification(paste0("✅ Résumé : ",nrow(result)," groupes"),
                       type="message",duration=3)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  observeEvent(input$dplyr_run_code, {
    req(input$dplyr_code)
    tryCatch({
      env <- new.env(parent=globalenv())
      env$df_f <- df_f
      result <- eval(parse(text=input$dplyr_code), envir=env)
      if (is.data.frame(result)) {
        dplyr_data(result)
        showNotification(paste0("✅ Code exécuté : ",nrow(result)," lignes"),
                         type="message",duration=3)
      }
    }, error=function(e) showNotification(paste("❌ Erreur R:",e$message),type="error"))
  })
  
  output$dplyr_code_result <- renderPrint({
    req(input$dplyr_code)
    tryCatch({
      env <- new.env(parent=globalenv())
      env$df_f <- df_f
      result <- eval(parse(text=input$dplyr_code), envir=env)
      if (is.data.frame(result)) {
        cat(paste0("Résultat : ",nrow(result)," lignes × ",ncol(result)," colonnes\n"))
        print(head(result, 5))
      } else print(result)
    }, error=function(e) cat("Erreur:", e$message))
  })
  
  # ══════════════════════════════════════════════════════════════════
  # NOUVEAUX VERBES DPLYR — UI dynamiques
  # ══════════════════════════════════════════════════════════════════

  # ── RENOMMER ──────────────────────────────────────────────────────
  output$dplyr_rename_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    tagList(
      selectizeInput("dplyr_ren_old","Colonne à renommer :",
                     choices=vis, selected=vis[1], width="100%"),
      textInput("dplyr_ren_new","Nouveau nom :", value="", width="100%")
    )
  })
  observeEvent(input$dplyr_apply_rename, {
    req(input$dplyr_ren_old, input$dplyr_ren_new)
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- rename(df, !!input$dplyr_ren_new := !!sym(input$dplyr_ren_old))
      dplyr_data(result)
      showNotification(paste0("✅ '",input$dplyr_ren_old,"' → '",input$dplyr_ren_new,"'"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── DÉDOUBLONNER ──────────────────────────────────────────────────
  output$dplyr_distinct_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_dist_cols","Colonnes clés (vide = toutes) :",
                   choices=vis, selected=NULL, multiple=TRUE, width="100%",
                   options=list(placeholder="Toutes les colonnes"))
  })
  observeEvent(input$dplyr_apply_distinct, {
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- if (length(input$dplyr_dist_cols) > 0)
        distinct(df, across(all_of(input$dplyr_dist_cols)), .keep_all=isTRUE(input$dplyr_keep_all))
      else
        distinct(df)
      n_supp <- nrow(df) - nrow(result)
      dplyr_data(result)
      showNotification(paste0("✅ distinct() : ",n_supp," doublon(s) supprimé(s) → ",
                              nrow(result)," lignes"), type="message", duration=4)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── DÉCOUPER (SLICE) ──────────────────────────────────────────────
  output$dplyr_slice_col_ui <- renderUI({
    req(input$dplyr_slice_type %in% c("min","max"))
    vn <- names(dplyr_data())[vapply(dplyr_data(), is.numeric, logical(1))]
    selectInput("dplyr_slice_col","Variable pour min/max :", choices=vn, width="100%")
  })
  observeEvent(input$dplyr_apply_slice, {
    req(input$dplyr_slice_n)
    tryCatch({
      df  <- dplyr_data(); req(df)
      n   <- as.integer(input$dplyr_slice_n)
      wt  <- isTRUE(input$dplyr_slice_with_ties)
      result <- switch(input$dplyr_slice_type,
        head   = slice_head(df, n=n),
        tail   = slice_tail(df, n=n),
        sample = slice_sample(df, n=min(n, nrow(df))),
        min    = { req(input$dplyr_slice_col)
                   slice_min(df, order_by=.data[[input$dplyr_slice_col]], n=n, with_ties=wt) },
        max    = { req(input$dplyr_slice_col)
                   slice_max(df, order_by=.data[[input$dplyr_slice_col]], n=n, with_ties=wt) }
      )
      dplyr_data(result)
      showNotification(paste0("✅ slice_",input$dplyr_slice_type,"(n=",n,") → ",
                              nrow(result)," lignes"), type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── COMPTER ───────────────────────────────────────────────────────
  output$dplyr_count_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vc <- names(df)[vapply(df, function(x) is.character(x)||is.factor(x)||is.logical(x), logical(1))]
    vc <- vc[!startsWith(vc,".")]
    selectizeInput("dplyr_cnt_cols","Grouper par :", choices=vc, multiple=TRUE,
                   selected=vc[1], width="100%")
  })
  output$dplyr_count_wt_ui <- renderUI({
    vn <- names(dplyr_data())[vapply(dplyr_data(), is.numeric, logical(1))]
    vn <- vn[!startsWith(vn,".")]
    selectInput("dplyr_cnt_wt","Pondération wt (optionnel) :",
                choices=c("Aucune"="",vn), selected="", width="100%")
  })
  observeEvent(input$dplyr_apply_count, {
    req(input$dplyr_cnt_cols)
    tryCatch({
      df <- dplyr_data(); req(df)
      wt_var <- if (!is.null(input$dplyr_cnt_wt) && input$dplyr_cnt_wt != "")
        sym(input$dplyr_cnt_wt) else NULL
      result <- if (input$dplyr_count_mode == "count") {
        if (!is.null(wt_var))
          count(df, across(all_of(input$dplyr_cnt_cols)), wt=!!wt_var,
                sort=isTRUE(input$dplyr_count_sort))
        else
          count(df, across(all_of(input$dplyr_cnt_cols)),
                sort=isTRUE(input$dplyr_count_sort))
      } else {
        if (!is.null(wt_var))
          add_count(df, across(all_of(input$dplyr_cnt_cols)), wt=!!wt_var)
        else
          add_count(df, across(all_of(input$dplyr_cnt_cols)))
      }
      dplyr_data(result)
      showNotification(paste0("✅ ",input$dplyr_count_mode,"() → ",nrow(result)," lignes"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── PIVOTER ───────────────────────────────────────────────────────
  output$dplyr_pivot_names_from_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vc <- names(df)[!startsWith(names(df),".")]
    selectInput("dplyr_piv_nf","names_from (catégories → colonnes) :",
                choices=vc, width="100%")
  })
  output$dplyr_pivot_values_from_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df, is.numeric, logical(1)) & !startsWith(names(df),".")]
    selectInput("dplyr_piv_vf","values_from (valeurs) :",
                choices=vn, selected=vn[1], width="100%")
  })
  output$dplyr_pivot_longer_cols_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_piv_long_cols","Colonnes à pivoter en long :",
                   choices=vis, multiple=TRUE, selected=head(vis,3), width="100%")
  })
  observeEvent(input$dplyr_apply_pivot, {
    tryCatch({
      df <- dplyr_data(); req(df)
      result <- if (input$dplyr_pivot_dir == "wider") {
        req(input$dplyr_piv_nf, input$dplyr_piv_vf)
        pivot_wider(df, names_from=all_of(input$dplyr_piv_nf),
                    values_from=all_of(input$dplyr_piv_vf),
                    values_fn=mean)
      } else {
        req(input$dplyr_piv_long_cols)
        pivot_longer(df, cols=all_of(input$dplyr_piv_long_cols),
                     names_to=input$dplyr_pivot_names_to,
                     values_to=input$dplyr_pivot_values_to)
      }
      dplyr_data(result)
      showNotification(paste0("✅ pivot_",input$dplyr_pivot_dir,"() → ",
                              nrow(result),"×",ncol(result)), type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── TRANSMUTER ────────────────────────────────────────────────────
  transmute_rows <- reactiveVal(1L)
  observeEvent(input$dplyr_add_transmute_row, {
    transmute_rows(transmute_rows() + 1L)
  })
  output$dplyr_transmute_ui <- renderUI({
    n <- transmute_rows()
    lapply(seq_len(n), function(i) {
      fluidRow(
        column(5, textInput(paste0("dplyr_tr_col_",i),
               if(i==1)"Nom colonne :" else NULL, value="", width="100%")),
        column(7, textInput(paste0("dplyr_tr_expr_",i),
               if(i==1)"Expression R :" else NULL,
               value="", width="100%"))
      )
    })
  })
  observeEvent(input$dplyr_apply_transmute, {
    tryCatch({
      df <- dplyr_data(); req(df)
      n   <- transmute_rows()
      lst <- list()
      for (i in seq_len(n)) {
        col_nm  <- input[[paste0("dplyr_tr_col_",i)]]
        col_ex  <- input[[paste0("dplyr_tr_expr_",i)]]
        if (!is.null(col_nm) && nchar(trimws(col_nm)) > 0 &&
            !is.null(col_ex)  && nchar(trimws(col_ex))  > 0)
          lst[[col_nm]] <- eval(parse(text=col_ex), envir=as.list(df))
      }
      req(length(lst) > 0)
      result <- do.call(transmute, c(list(df), lst))
      dplyr_data(result)
      showNotification(paste0("✅ transmute() : ",ncol(result)," nouvelle(s) colonne(s)"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── GÉRER NA ──────────────────────────────────────────────────────
  output$dplyr_na_cols_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("dplyr_na_cols","Colonnes concernées (vide = toutes) :",
                   choices=vis, multiple=TRUE, selected=NULL, width="100%",
                   options=list(placeholder="Toutes les colonnes"))
  })
  observeEvent(input$dplyr_apply_na, {
    tryCatch({
      df   <- dplyr_data(); req(df)
      cols <- if (length(input$dplyr_na_cols) > 0) input$dplyr_na_cols else names(df)
      result <- switch(input$dplyr_na_op,
        drop = {
          drop_na(df, any_of(cols))
        },
        replace = {
          val <- tryCatch(as.numeric(input$dplyr_na_val), warning=function(w) input$dplyr_na_val)
          rpl <- setNames(rep(list(val), length(cols)), cols)
          replace_na(df, rpl)
        },
        fill = {
          dir <- if (input$dplyr_fill_dir == "up") "up" else "down"
          fill(df, any_of(cols), .direction=dir)
        }
      )
      n_avant <- sum(is.na(select(df, any_of(cols))))
      n_apres <- sum(is.na(select(result, any_of(cols))))
      dplyr_data(result)
      showNotification(paste0("✅ ",input$dplyr_na_op,"() : ",
                              n_avant - n_apres," NA traité(s)"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── ACROSS ────────────────────────────────────────────────────────
  output$dplyr_across_cols_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df, is.numeric, logical(1)) & !startsWith(names(df),".")]
    selectizeInput("dplyr_ac_cols","Colonnes numériques :",
                   choices=vn, multiple=TRUE, selected=head(vn,3), width="100%")
  })
  output$dplyr_across_grp_ui <- renderUI({
    req(input$dplyr_across_verb == "summarise")
    df <- dplyr_data(); req(df)
    vc <- names(df)[vapply(df,function(x) is.character(x)||is.factor(x)||is.logical(x), logical(1))]
    vc <- vc[!startsWith(vc,".")]
    selectizeInput("dplyr_ac_grp","Grouper par (pour summarise) :",
                   choices=vc, multiple=TRUE, selected=vc[1], width="100%")
  })
  observeEvent(input$dplyr_apply_across, {
    req(input$dplyr_ac_cols, input$dplyr_across_fn)
    tryCatch({
      df   <- dplyr_data(); req(df)
      fn   <- input$dplyr_across_fn
      cols <- input$dplyr_ac_cols
      fn_expr <- if (startsWith(fn, "~")) eval(parse(text=fn)) else match.fun(fn)
      result <- if (input$dplyr_across_verb == "mutate") {
        mutate(df, across(all_of(cols), fn_expr,
                          .names="{.col}_{gsub('[^a-zA-Z0-9]','',fn)}"))
      } else {
        grps <- if (!is.null(input$dplyr_ac_grp) && length(input$dplyr_ac_grp)>0)
          input$dplyr_ac_grp else character(0)
        df |>
          group_by(across(all_of(grps))) |>
          summarise(across(all_of(cols), fn_expr, na.rm=TRUE,
                           .names="{.col}_{gsub('[^a-zA-Z0-9]','',fn)}"),
                    .groups="drop")
      }
      dplyr_data(result)
      showNotification(paste0("✅ across(",length(cols)," colonnes, ",fn,")"),
                       type="message", duration=3)
    }, error=function(e) showNotification(paste("❌",e$message), type="error"))
  })

  # ── Affichage résultat dplyr ───────────────────────────────────────
  output$dplyr_result_tbl <- renderDT({
    df <- dplyr_data(); req(df)
    df |> select(!starts_with(".")) |>
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15,scrollX=TRUE,dom="frtip"),
                class="compact stripe hover")
  })
  
  output$dplyr_nrow_ui <- renderUI({
    df <- dplyr_data(); req(df)
    orig <- nrow(df_f())
    curr <- nrow(df)
    tags$div(style="font-size:13px;color:#555;padding:4px 0;",
      tags$b(style="color:#0077B6;font-size:18px;",format(curr,big.mark=" ")),
      " lignes × ",
      tags$b(ncol(df))," colonnes",
      if(curr < orig)
        tags$span(style="color:#D98B14;margin-left:8px;font-size:12px;",
                  paste0("(filtré depuis ",format(orig,big.mark=" ")," lignes)"))
    )
  })
  
  output$dplyr_stats_quick_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df,is.numeric,logical(1)) & !startsWith(names(df),".")]
    if(length(vn)==0) return(NULL)
    tags$div(
      tags$div(class="section-header","Statistiques rapides du résultat"),
      fluidRow(
        lapply(head(vn,4), function(v) {
          x <- na.omit(df[[v]])
          if(length(x)==0) return(NULL)
          column(3, tags$div(
            style="background:white;border:0.5px solid #e0e0e0;border-radius:8px;
                   padding:10px;text-align:center;margin:4px;",
            tags$div(style="font-size:11px;color:#888;text-transform:uppercase;",v),
            tags$div(style="font-size:18px;font-weight:700;color:#0077B6;",round(mean(x),2)),
            tags$div(style="font-size:11px;color:#555;",
                     paste0("σ=",round(sd(x),2)," | Méd=",round(median(x),2)))
          ))
        })
      )
    )
  })
  
  output$dplyr_dl <- downloadHandler(
    filename=function() paste0("manipulation_dplyr_",Sys.Date(),".csv"),
    content=function(f) {
      df <- dplyr_data(); req(df)
      write.csv(df |> select(!starts_with(".")), f, row.names=FALSE)
    }
  )
  
  # ── Visualisation rapide dplyr ─────────────────────────────────────
  output$dplyr_plot_x_ui <- renderUI({
    df <- dplyr_data(); req(df)
    all_v <- names(df)[!startsWith(names(df),".")]
    selectInput("dplyr_px","Variable X :",choices=all_v,selected=all_v[1],width="100%")
  })
  output$dplyr_plot_y_ui <- renderUI({
    df <- dplyr_data(); req(df)
    vn <- names(df)[vapply(df,is.numeric,logical(1)) & !startsWith(names(df),".")]
    if(length(vn)==0) return(NULL)
    selectInput("dplyr_py","Variable Y (numérique) :",choices=vn,
                selected=vn[1],width="100%")
  })
  
  output$dplyr_plot_out <- renderPlotly({
    req(input$dplyr_plot_btn, input$dplyr_px)
    df <- dplyr_data(); req(df)
    x <- input$dplyr_px; y <- input$dplyr_py
    type <- input$dplyr_plot_type
    tryCatch({
      p <- if(type=="bar") {
        cnt <- df |> count(.data[[x]]) |> mutate(pct=round(n/sum(n)*100,1))
        cnt <- smart_top_n(cnt, x, "n", 25)
        cnt[[x]] <- truncate_label(cnt[[x]], 22)
        ggplot(cnt, aes(y=fct_reorder(.data[[x]],n), x=n, fill=.data[[x]],
          text=paste0(.data[[x]],"\nN=",n," (",pct,"%)"))) +
          geom_col(alpha=.88) +
          scale_fill_manual(values=gpal(nrow(cnt),"cat")) +
          labs(y="",x="N") + theme_nutri() + theme(legend.position="none")
      } else if(type=="hist") {
        req(y)
        ggplot(df,aes(x=.data[[y]]))+
          geom_histogram(fill=gprimary(),color="white",bins=28,alpha=.85)+
          geom_vline(xintercept=mean(df[[y]],na.rm=TRUE),color="#BF3636",linewidth=1,linetype="dashed")+
          labs(x=y,y="N")+theme_nutri()
      } else if(type=="box") {
        req(y)
        n_grp <- length(unique(na.omit(df[[x]])))
        df[[x]] <- truncate_label(df[[x]], 20)
        if (n_grp > 10) {
          ggplot(df,aes(y=.data[[x]],x=.data[[y]],fill=.data[[x]]))+
            geom_boxplot(alpha=.75,outlier.shape=21)+
            scale_fill_manual(values=gpal(n_grp,"cat"))+
            labs(y=x,x=y)+theme_nutri()+theme(legend.position="none")
        } else {
          ggplot(df,aes(x=.data[[x]],y=.data[[y]],fill=.data[[x]]))+
            geom_boxplot(alpha=.75,outlier.shape=21)+
            scale_fill_manual(values=gpal(n_grp,"cat"))+
            labs(x=x,y=y)+theme_nutri()+smart_axis_x(n_grp)+
            theme(legend.position="none")
        }
      } else if(type=="scatter") {
        req(y)
        ggplot(df,aes(x=.data[[x]],y=.data[[y]]))+
          geom_point(alpha=.5,color=gprimary(),size=2)+
          geom_smooth(method="loess",se=TRUE,color="#BF3636",linewidth=.9)+
          labs(x=x,y=y)+theme_nutri()
      } else {
        req(y)
        ggplot(df,aes(x=.data[[y]]))+
          geom_density(fill=gprimary(),alpha=.4,color=gprimary())+
          geom_vline(xintercept=mean(df[[y]],na.rm=TRUE),color="#BF3636",linewidth=1,linetype="dashed")+
          labs(x=y,y="Densité")+theme_nutri()
      }
      ggplotly(p,tooltip="text") |> layout(hovermode="closest") |> nutri_config()
    }, error=function(e)
      plotly_empty() |> layout(title=list(text=paste("Erreur:",e$message),x=.5)))
  })



