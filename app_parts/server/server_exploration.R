# Extrait de app_v2_final.R — exploration et statistiques complètes
  # ── EXPLORATION ─────────────────────────────────────────────────────────────
  output$explor_cols_ui <- renderUI({
    df <- df_f(); req(df)
    vis <- names(df)[!startsWith(names(df),".")]
    selectizeInput("explor_cols","Colonnes à afficher :",
                   choices=vis, selected=head(vis,8),
                   multiple=TRUE, width="100%")
  })

  output$explor_table <- renderDT({
    req(input$explor_cols)
    df_f() |> select(any_of(input$explor_cols)) |>
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=input$explor_n, scrollX=TRUE, dom="frtip"),
                class="compact stripe hover")
  })

  observe({
    vn <- vars_num()
    updateSelectInput(session,"explor_var", choices=vn, selected=vn[1])
  })

  output$explor_stats <- renderTable({
    req(input$explor_var); x <- df_f()[[input$explor_var]]
    if (is.numeric(x)) {
      v <- na.omit(x)
      sw <- tryCatch(shapiro.test(v[1:min(5000,length(v))]), error=function(e) NULL)
      jb_p <- tryCatch({
        n <- length(v); m3 <- mean((v-mean(v))^3); m4 <- mean((v-mean(v))^4)
        s <- m3/(sd(v)^3); k <- m4/(sd(v)^4)-3
        jb_stat <- n*(s^2/6 + k^2/24)
        round(pchisq(jb_stat, df=2, lower.tail=FALSE), 4)
      }, error=function(e) NA)
      tibble(
        Mesure = c(
          "── TENDANCE CENTRALE ──","Moyenne (μ)","Médiane","Mode","Moyenne géométrique","Moyenne harmonique",
          "── DISPERSION ──","Écart-type (σ)","Variance (σ²)","CV (%)","IQR","Étendue","Écart absolu moyen",
          "── POSITION ──","Min","Q1 (25%)","Q2 (50%)","Q3 (75%)","Max","P5","P10","P90","P95",
          "── FORME ──","Asymétrie (Skewness)","Aplatissement (Kurtosis)","Kurtosis excès",
          "── NORMALITÉ ──","Shapiro-Wilk W","Shapiro-Wilk p","Jarque-Bera p","Distribution",
          "── DONNÉES ──","N valides","N manquants","% manquants"
        ),
        Valeur = c(
          "","round(mean(v),4)","round(median(v),4)",
          as.character(round(as.numeric(e1071::moment(v,order=1,center=FALSE)),4)),
          round(exp(mean(log(v[v>0]))),4),
          round(length(v)/sum(1/v[v!=0]),4),
          "",round(sd(v),4),round(var(v),4),round(sd(v)/mean(v)*100,2),
          round(IQR(v),4),round(diff(range(v)),4),round(mean(abs(v-mean(v))),4),
          "",round(min(v),4),round(quantile(v,.25),4),round(median(v),4),
          round(quantile(v,.75),4),round(max(v),4),
          round(quantile(v,.05),4),round(quantile(v,.10),4),
          round(quantile(v,.90),4),round(quantile(v,.95),4),
          "",round(e1071::skewness(v),4),round(e1071::kurtosis(v)+3,4),round(e1071::kurtosis(v),4),
          "",if(!is.null(sw))round(sw$statistic,4) else "N/A",
          if(!is.null(sw))round(sw$p.value,4) else "N/A",
          if(!is.na(jb_p))jb_p else "N/A",
          if(!is.null(sw)) if(sw$p.value<.05)"❌ Non normale (p<0.05)" else "✅ Normale (p≥0.05)" else "N/A",
          "",length(v),sum(is.na(x)),round(mean(is.na(x))*100,1)
        )
      ) |> filter(Mesure != "" | Valeur != "")
    } else {
      cnt <- df_f() |> count(.data[[input$explor_var]]) |>
        mutate(Pct=round(n/sum(n)*100,1), Cum=cumsum(Pct))
      tibble(Modalité=as.character(cnt[[input$explor_var]]),
             N=cnt$n, `%`=cnt$Pct, `% cumulé`=cnt$Cum)
    }
  }, striped=TRUE, hover=TRUE, bordered=TRUE, spacing="xs")

  output$explor_auto_plot <- renderPlotly({
    req(input$explor_var); d <- df_f(); v <- input$explor_var
    x <- d[[v]]
    tp <- var_type(x)
    if (tp %in% c("continue","discrete")) {
      m_v <- mean(x,na.rm=TRUE); s_v <- sd(x,na.rm=TRUE); md_v <- median(x,na.rm=TRUE)
      p <- ggplot(d, aes(x=.data[[v]])) +
        geom_histogram(aes(y=after_stat(density)),bins=28,fill=gprimary(),color="white",alpha=0.82)+
        stat_function(fun=dnorm,args=list(mean=m_v,sd=s_v),color="black",linewidth=1.1)+
        geom_vline(xintercept=m_v,color=gsecondary(),linewidth=1,linetype="dashed")+
        geom_vline(xintercept=md_v,color=C$orange,linewidth=1,linetype="dotted")+
        labs(x=v,y="Densité",
             subtitle=paste0("μ=",round(m_v,3)," | σ=",round(s_v,3)," | Méd=",round(md_v,3)),
             caption="Rouge=moyenne | Orange=médiane | Courbe=N(μ,σ)")+
        theme_nutri()
    } else {
      cnt <- d |> count(.data[[v]]) |> mutate(pct=round(n/sum(n)*100,1))
      cnt <- smart_top_n(cnt, v, "n", 25)
      niv <- nrow(cnt)
      cnt[[v]] <- truncate_label(cnt[[v]], 22)
      p <- ggplot(cnt,aes(y=fct_reorder(.data[[v]],n),x=n,fill=.data[[v]],
                          text=paste0(.data[[v]],"\nN=",n," (",pct,"%)"))) +
        geom_col(alpha=0.88,width=0.7)+
        scale_fill_manual(values=gpal(niv,"cat"))+
        labs(y="",x="Effectif")+
        theme_nutri()+theme(legend.position="none")
    }
    ggplotly(p, tooltip=if(tp %in% c("continue","discrete")) NULL else "text") |>
      layout(hovermode="x unified") |> nutri_config()
  })

  output$dl_csv <- downloadHandler(
    filename=function() paste0("data_",Sys.Date(),".csv"),
    content=function(f) write.csv(df_f()|>select(!starts_with(".")),f,row.names=FALSE)
  )
  output$dl_xlsx <- downloadHandler(
    filename=function() paste0("data_",Sys.Date(),".xlsx"),
    content=function(f) write_xlsx(df_f()|>select(!starts_with(".")),f)
  )

  # ── Statistiques complètes par sections ───────────────────────────────────
  observe({
    all_v <- c(vars_num(), vars_cat())
    updateSelectInput(session,"explor_var", choices=all_v,
                      selected=if(length(vars_num())>0) vars_num()[1] else all_v[1])
  })

# [DEAD CODE] explor_central — output serveur sans UI correspondante
#   output$explor_central <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     tibble(
#       Mesure = c("Moyenne arithmétique","Médiane (Q2)","Mode statistique",
#                  "Moyenne géométrique","Moyenne harmonique","Moyenne tronquée 5%"),
#       Valeur = c(round(mean(x),4), round(median(x),4),
#                  round(as.numeric(mode_stat(x)),4),
#                  round(exp(mean(log(abs(x[x!=0])))),4),
#                  round(length(x[x!=0])/sum(1/x[x!=0]),4),
#                  round(mean(x,trim=0.05),4))
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs", digits=4)

# [DEAD CODE] explor_dispersion — output serveur sans UI correspondante
#   output$explor_dispersion <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     tibble(
#       Mesure = c("Écart-type (s)","Variance (s²)","CV (%)","Étendue",
#                  "IQR (Q3-Q1)","MAD (écart absolu médian)",
#                  "Erreur standard (SE)","IC95 bas","IC95 haut"),
#       Valeur = c(round(sd(x),4), round(var(x),4),
#                  round(sd(x)/mean(x)*100,2),
#                  round(max(x)-min(x),4), round(IQR(x),4),
#                  round(mad(x),4),
#                  round(sd(x)/sqrt(length(x)),4),
#                  round(mean(x)-1.96*sd(x)/sqrt(length(x)),4),
#                  round(mean(x)+1.96*sd(x)/sqrt(length(x)),4))
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs", digits=4)

# [DEAD CODE] explor_forme — output serveur sans UI correspondante
#   output$explor_forme <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     sw   <- tryCatch(shapiro.test(x[1:min(5000,length(x))]), error=function(e) NULL)
#     asym <- e1071::skewness(x)
#     kurt <- e1071::kurtosis(x)
#     tibble(
#       Mesure = c("Asymétrie (Skewness)","Interprétation skewness",
#                  "Aplatissement (Kurtosis)","Interprétation kurtosis",
#                  "Shapiro-Wilk W","Shapiro-Wilk p-value","Distribution"),
#       Valeur = c(round(asym,4),
#                  if(asym>0.5)"Asymétrie droite (+)" else if(asym< -0.5)"Asymétrie gauche (-)" else "Symétrique (≈0)",
#                  round(kurt,4),
#                  if(kurt>0)"Leptokurtique (queues lourdes)" else if(kurt<0)"Platykurtique (queues légères)" else "Mésokurtique (≈normale)",
#                  if(!is.null(sw)) round(sw$statistic,4) else "N/A",
#                  if(!is.null(sw)) round(sw$p.value,4)   else "N/A",
#                  if(!is.null(sw)) if(sw$p.value<.05)"❌ Non normale → Tests non param." else "✅ Normale → Tests param. possibles" else "N/A")
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs")

# [DEAD CODE] explor_quantiles — output serveur sans UI correspondante
#   output$explor_quantiles <- renderTable({
#     req(input$explor_var); x <- na.omit(df_f()[[input$explor_var]])
#     req(is.numeric(x), length(x)>1)
#     q <- quantile(x, probs=c(.01,.05,.10,.25,.50,.75,.90,.95,.99))
#     tibble(
#       Quantile = c("P1 (1%)","P5 (5%)","P10 (10%)","Q1 — 25%",
#                    "Q2 — Médiane (50%)","Q3 — 75%",
#                    "P90 (90%)","P95 (95%)","P99 (99%)"),
#       Valeur   = round(q, 4)
#     )
#   }, striped=TRUE, bordered=TRUE, spacing="xs", digits=4)

# [DEAD CODE] explor_boxplot_dist — output serveur sans UI correspondante
#   output$explor_boxplot_dist <- renderPlotly({
#     req(input$explor_var); d <- df_f(); v <- input$explor_var
#     req(is.numeric(d[[v]]))
#     p <- ggplot(d, aes(x="", y=.data[[v]])) +
#       geom_violin(fill=gprimary(), alpha=.35, trim=FALSE, color=C$violet) +
#       geom_boxplot(fill="white", color="#2c3e50", width=.15,
#                    outlier.shape=21, outlier.color=C$rouge) +
#       stat_summary(fun=mean, geom="point", shape=18, size=4, color=C$rouge) +
#       coord_flip() +
#       labs(x="", y=v, title="Violin + Boxplot — Distribution complète") +
#       theme_minimal(base_size=11)
#     ggplotly(p) |> nutri_config()
#   })

# [DEAD CODE] manip_filter_ui — output serveur sans UI correspondante
#   # ── Manipulation dplyr ────────────────────────────────────────────────────
#   output$manip_filter_ui <- renderUI({
#     df <- df_f(); req(df)
#     vn <- vars_num(); vc <- vars_cat()
#     tagList(
#       selectInput("manip_f_var","Variable à filtrer :",
#                   choices=c("(aucun)"="", c(vn,vc)), width="100%"),
#       uiOutput("manip_f_cond_ui")
#     )
#   })
  output$manip_f_cond_ui <- renderUI({
    req(input$manip_f_var, input$manip_f_var != "")
    d <- df_f(); v <- input$manip_f_var
    if (is.numeric(d[[v]])) {
      r <- range(d[[v]], na.rm=TRUE)
      sliderInput("manip_f_range", paste0("Plage [",v,"] :"),
                  min=floor(r[1]), max=ceiling(r[2]),
                  value=c(floor(r[1]),ceiling(r[2])), width="100%")
    } else {
      vals <- sort(unique(na.omit(as.character(d[[v]]))))
      checkboxGroupInput("manip_f_cats", paste0("Valeurs [",v,"] :"),
                         choices=vals, selected=vals)
    }
  })
# [DEAD CODE] manip_select_ui — output serveur sans UI correspondante
#   output$manip_select_ui <- renderUI({
#     df <- df_f(); req(df)
#     vis <- names(df)[!startsWith(names(df),".")]
#     selectizeInput("manip_sel_cols","Colonnes à garder :",
#                    choices=vis, selected=head(vis,8), multiple=TRUE, width="100%")
#   })
# [DEAD CODE] manip_arrange_ui — output serveur sans UI correspondante
#   output$manip_arrange_ui <- renderUI({
#     vn <- c(vars_num(), vars_cat())
#     selectInput("manip_sort_var","Trier par :",
#                 choices=c("(aucun)"="", vn), width="100%")
#   })
# [DEAD CODE] manip_group_ui — output serveur sans UI correspondante
#   output$manip_group_ui <- renderUI({
#     vc <- vars_cat()
#     selectInput("manip_grp","Grouper par :",
#                 choices=c("(aucun)"="", vc), width="100%")
#   })
# [DEAD CODE] manip_summarise_ui — output serveur sans UI correspondante
#   output$manip_summarise_ui <- renderUI({
#     vn <- vars_num()
#     tagList(
#       selectInput("manip_sum_var","Variable à résumer :", choices=vn, width="100%"),
#       checkboxGroupInput("manip_sum_fns","Fonctions :",
#         choices=c("Moyenne"="mean","Médiane"="median","Écart-type"="sd",
#                   "Min"="min","Max"="max","N"="n","% manquants"="na_pct"),
#         selected=c("mean","median","sd","n"), inline=TRUE)
#     )
#   })

# [DEAD CODE] manip_code_preview — output serveur sans UI correspondante
#   output$manip_code_preview <- renderText({
#     lines <- c("df_manipule <- df")
#     if (!is.null(input$manip_f_var) && input$manip_f_var != "") {
#       v <- input$manip_f_var; d <- df_f()
#       if (is.numeric(d[[v]]) && !is.null(input$manip_f_range))
#         lines <- c(lines, paste0("  |> filter(",v," >= ",input$manip_f_range[1],
#                                   " & ",v," <= ",input$manip_f_range[2],")"))
#       else if (!is.null(input$manip_f_cats) && length(input$manip_f_cats)>0)
#         lines <- c(lines, paste0('  |> filter(',v,' %in% c(',
#                                   paste0('"',input$manip_f_cats,'"',collapse=","),'))'))
#     }
#     if (!is.null(input$manip_sel_cols) && length(input$manip_sel_cols)>0)
#       lines <- c(lines, paste0("  |> select(",paste(input$manip_sel_cols,collapse=", "),")"))
#     if (!is.null(input$manip_sort_var) && input$manip_sort_var != "")
#       lines <- c(lines, paste0("  |> arrange(",
#         if(isTRUE(input$manip_desc)) paste0("desc(",input$manip_sort_var,")")
#         else input$manip_sort_var,")"))
#     if (!is.null(input$manip_grp) && input$manip_grp != "" &&
#         !is.null(input$manip_sum_var)) {
#       fns <- input$manip_sum_fns; v2 <- input$manip_sum_var
#       parts <- c()
#       if("mean"  %in%fns) parts<-c(parts,paste0("moy=mean(",v2,",na.rm=T)"))
#       if("median"%in%fns) parts<-c(parts,paste0("med=median(",v2,",na.rm=T)"))
#       if("sd"    %in%fns) parts<-c(parts,paste0("sd=sd(",v2,",na.rm=T)"))
#       if("min"   %in%fns) parts<-c(parts,paste0("min=min(",v2,",na.rm=T)"))
#       if("max"   %in%fns) parts<-c(parts,paste0("max=max(",v2,",na.rm=T)"))
#       if("n"     %in%fns) parts<-c(parts,"n=n()")
#       if("na_pct"%in%fns) parts<-c(parts,paste0("pct_na=mean(is.na(",v2,"))*100"))
#       lines <- c(lines, paste0("  |> group_by(",input$manip_grp,")"),
#                  paste0("  |> summarise(",paste(parts,collapse=",
#               "),")"))
#     }
#     paste(lines, collapse="
# ")
#   })

  manip_result <- reactive({
    d <- df_f()
    tryCatch({
      if (!is.null(input$manip_f_var) && input$manip_f_var != "") {
        v <- input$manip_f_var
        if (is.numeric(d[[v]]) && !is.null(input$manip_f_range))
          d <- filter(d, .data[[v]]>=input$manip_f_range[1] & .data[[v]]<=input$manip_f_range[2])
        else if (!is.null(input$manip_f_cats))
          d <- filter(d, as.character(.data[[v]]) %in% input$manip_f_cats)
      }
      if (!is.null(input$manip_sel_cols) && length(input$manip_sel_cols)>0)
        d <- select(d, any_of(input$manip_sel_cols))
      if (!is.null(input$manip_sort_var) && input$manip_sort_var != "")
        d <- if(isTRUE(input$manip_desc)) arrange(d,desc(.data[[input$manip_sort_var]]))
             else arrange(d,.data[[input$manip_sort_var]])
      if (!is.null(input$manip_grp) && input$manip_grp != "" &&
          input$manip_grp %in% names(d) &&
          !is.null(input$manip_sum_var) && input$manip_sum_var %in% names(d)) {
        v2 <- input$manip_sum_var; fns <- input$manip_sum_fns
        d <- d |> group_by(.data[[input$manip_grp]]) |>
          summarise(
            N               = n(),
            Moyenne         = if("mean"  %in%fns) round(mean(.data[[v2]],na.rm=T),3) else NA,
            Médiane         = if("median"%in%fns) round(median(.data[[v2]],na.rm=T),3) else NA,
            `Écart-type`    = if("sd"    %in%fns) round(sd(.data[[v2]],na.rm=T),3) else NA,
            Min             = if("min"   %in%fns) round(min(.data[[v2]],na.rm=T),3) else NA,
            Max             = if("max"   %in%fns) round(max(.data[[v2]],na.rm=T),3) else NA,
            `% manquants`   = if("na_pct"%in%fns) round(mean(is.na(.data[[v2]]))*100,1) else NA,
            .groups="drop") |>
          select(where(~!all(is.na(.))))
      }
      d
    }, error=function(e) df_f())
  })

  observeEvent(input$manip_add_var, {
    req(nchar(input$manip_newvar_name)>0, nchar(input$manip_newvar_expr)>0)
    d <- df_f()
    tryCatch({
      d <- safe_mutate(d, input$manip_newvar_name, input$manip_newvar_expr)
      mapped_data(d)
      showNotification(paste0("✅ Variable «",input$manip_newvar_name,"» créée"),
                       type="message", duration=3)
    }, error=function(e)
      showNotification(paste("❌",e$message), type="error", duration=5))
  })

  observeEvent(input$manip_reset, {
    df_reset <- tryCatch(calc_indicateurs(raw_data(), map_cols()), error=function(e) raw_data())
    df_reset <- tryCatch(ensure_sa_columns(df_reset), error=function(e) df_reset)
    mapped_data(df_reset)
    showNotification("🔄 Données réinitialisées", type="message", duration=3)
  })

# [DEAD CODE] manip_table — output serveur sans UI correspondante
#   output$manip_table <- renderDT({
#     manip_result() |>
#       datatable(rownames=FALSE, filter="top",
#                 options=list(pageLength=12, scrollX=TRUE, dom="frtip"),
#                 class="compact stripe hover")
#   })

# [DEAD CODE] dl_manip_csv — output serveur sans UI correspondante
#   output$dl_manip_csv <- downloadHandler(
#     filename=function() paste0("data_dplyr_",Sys.Date(),".csv"),
#     content=function(f) write.csv(manip_result(), f, row.names=FALSE)
#   )
# [DEAD CODE] dl_manip_xlsx — output serveur sans UI correspondante
#   output$dl_manip_xlsx <- downloadHandler(
#     filename=function() paste0("data_dplyr_",Sys.Date(),".xlsx"),
#     content=function(f) writexl::write_xlsx(manip_result(), f)
#   )

# [DEAD CODE] adv_filter_num_ui — output serveur sans UI correspondante
#   # ── Filtres avancés ───────────────────────────────────────────────────────
#   output$adv_filter_num_ui <- renderUI({
#     vn <- head(vars_num(),4); d <- df_f()
#     lapply(vn, function(v) {
#       r <- range(d[[v]], na.rm=TRUE)
#       sliderInput(paste0("adv_num_",v), paste0(v," :"),
#                   min=floor(r[1]), max=ceiling(r[2]),
#                   value=c(floor(r[1]),ceiling(r[2])), width="100%")
#     })
#   })
# [DEAD CODE] adv_filter_cat_ui — output serveur sans UI correspondante
#   output$adv_filter_cat_ui <- renderUI({
#     vc <- head(vars_cat(),3); d <- df_f()
#     lapply(vc, function(v) {
#       vals <- sort(unique(na.omit(as.character(d[[v]]))))
#       if(length(vals)<=12)
#         checkboxGroupInput(paste0("adv_cat_",v), paste0(v," :"),
#                            choices=vals, selected=vals, inline=TRUE)
#       else
#         selectizeInput(paste0("adv_cat_",v), paste0(v," :"),
#                        choices=vals, selected=vals, multiple=TRUE, width="100%")
#     })
#   })
# [DEAD CODE] adv_filter_na_ui — output serveur sans UI correspondante
#   output$adv_filter_na_ui <- renderUI({
#     checkboxInput("adv_excl_na","Exclure les lignes avec valeurs manquantes",FALSE)
#   })

  adv_result <- eventReactive(input$adv_apply, {
    d <- df_f()
    tryCatch({
      for (v in head(vars_num(),4)) {
        id <- paste0("adv_num_",v)
        if (!is.null(input[[id]]))
          d <- filter(d, is.na(.data[[v]]) |
                        (.data[[v]]>=input[[id]][1] & .data[[v]]<=input[[id]][2]))
      }
      for (v in head(vars_cat(),3)) {
        id <- paste0("adv_cat_",v)
        if (!is.null(input[[id]]) && length(input[[id]])>0)
          d <- filter(d, as.character(.data[[v]]) %in% input[[id]])
      }
      if (isTRUE(input$adv_excl_na)) d <- drop_na(d)
      d
    }, error=function(e) df_f())
  }, ignoreNULL=FALSE)

  adv_data_r <- reactive({
    if (is.null(input$adv_apply) || input$adv_apply==0) df_f()
    else adv_result()
  })

# [DEAD CODE] adv_filter_summary_ui — output serveur sans UI correspondante
#   output$adv_filter_summary_ui <- renderUI({
#     d_orig <- df_f(); d_filt <- adv_data_r()
#     n_orig <- nrow(d_orig); n_filt <- nrow(d_filt)
#     pct <- round((1-n_filt/n_orig)*100,1)
#     cls <- if(pct>50)"alerte-crit" else if(pct>20)"alerte-serieux" else "alerte-ok"
#     tags$div(class=paste("alerte-box",cls),
#       tags$b(paste0(format(n_filt,big.mark=" ")," / ",
#                     format(n_orig,big.mark=" ")," observations")),
#       tags$br(),
#       tags$span(style="font-size:12px;",paste0(pct,"% filtrées"))
#     )
#   })
# [DEAD CODE] adv_table — output serveur sans UI correspondante
#   output$adv_table <- renderDT({
#     adv_data_r() |>
#       datatable(rownames=FALSE, filter="top",
#                 options=list(pageLength=12, scrollX=TRUE, dom="frtip"),
#                 class="compact stripe hover")
#   })
# [DEAD CODE] adv_overview — output serveur sans UI correspondante
#   output$adv_overview <- renderPlotly({
#     d <- adv_data_r(); vn <- vars_num(); req(length(vn)>0)
#     v <- vn[1]
#     p <- ggplot(d, aes(x=.data[[v]])) +
#       geom_histogram(fill=gprimary(),color="white",alpha=.82,bins=25) +
#       labs(x=v,y="Effectif",title=paste0("Distribution de ",v," (données filtrées)")) +
#       theme_minimal(base_size=11)
#     ggplotly(p)|>layout(hovermode="x unified") |> nutri_config()
#   })

