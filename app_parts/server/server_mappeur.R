# Extrait de app_v2_final.R — mappeur de colonnes et conformité OMS
  # ── MAPPEUR ─────────────────────────────────────────────────────────────────
  make_mapper_row <- function(key, lbl, side="left") {
    df  <- raw_data(); req(df)
    m   <- map_cols()
    nms <- c("(Non mappé)", names(df))
    val <- m[[key]]
    classe <- if (!is.null(val) && val %in% names(df)) "badge-ok"
              else if (!is.null(val)) "badge-warn" else "badge-err"
    badge <- if (!is.null(val) && val %in% names(df))
      tags$span(class=classe, paste0("✓ ", val))
    else tags$span(class="badge-err", "Non trouvé — sélectionnez")

    tags$div(class="wizard-step",
      style="flex-wrap:wrap;",
      tags$div(class="ws-num",
               style=if(!is.null(val) && val%in%names(df))"background:#2E7D52;color:white;" else "background:#BF3636;color:white;",
               if(!is.null(val) && val%in%names(df)) "✓" else "!"),
      tags$div(style="flex:1;min-width:120px;",
        tags$strong(lbl, style="font-size:13px;"),
        tags$br(), badge
      ),
      tags$div(style="min-width:200px;",
        selectInput(paste0("map_",key), NULL,
                    choices=nms,
                    selected=if(!is.null(val) && val%in%nms) val else "(Non mappé)",
                    width="100%")
      )
    )
  }

  mapper_defs <- list(
    whz="Score WHZ", haz="Score HAZ", waz="Score WAZ",
    muac="MUAC (mm)", oedema="Œdèmes", age_m="Âge (mois)",
    sexe="Sexe", region="Région", dept="Département",
    lat="Latitude GPS", lon="Longitude GPS",
    poids="Poids (kg)", taille="Taille (cm)",
    fcs="FCS (PAM)", hhs="HHS (PAM)", depl="Statut déplacement"
  )

  # ── Tableau de vérification dictionnaire OMS ────────────────────────────
  output$dict_verification_table <- renderDT({
    df <- raw_data(); req(df)
    m  <- map_cols()

    rows <- lapply(names(OMS_DICT), function(key) {
      detected_col <- m[[key]]
      aliases      <- paste(OMS_DICT[[key]], collapse = " · ")
      label        <- if (key %in% names(OMS_META)) OMS_META[[key]]$label else key
      cat_oms      <- if (key %in% names(OMS_META)) OMS_META[[key]]$cat   else ""

      sample_vals <- ""
      if (!is.null(detected_col) && detected_col %in% names(df)) {
        vals <- head(na.omit(df[[detected_col]]), 5)
        sample_vals <- paste(vals, collapse = ", ")
      }

      data.frame(
        Variable_OMS         = paste0(key, " \u2014 ", label),
        Categorie            = cat_oms,
        Colonne_detectee     = if (!is.null(detected_col)) detected_col else "\u2014",
        Statut               = if (!is.null(detected_col) && detected_col %in% names(df))
                                 "\u2705 D\u00e9tect\u00e9"
                               else "\u274c Non trouv\u00e9",
        Aliases_dictionnaire = aliases,
        Apercu               = sample_vals,
        stringsAsFactors     = FALSE
      )
    })

    verify_df <- do.call(rbind, rows)

    datatable(verify_df, rownames = FALSE,
      colnames = c("Variable OMS", "Cat\u00e9gorie", "Colonne d\u00e9tect\u00e9e",
                    "Statut", "Alias reconnus (dictionnaire)", "Aper\u00e7u valeurs"),
      options = list(
        pageLength = 15, scrollX = TRUE, dom = "frtip",
        language = list(search = "Rechercher :"),
        columnDefs = list(
          list(width = "170px", targets = 0),
          list(width = "120px", targets = 1),
          list(width = "130px", targets = 2),
          list(width = "100px", targets = 3),
          list(width = "320px", targets = 4),
          list(width = "140px", targets = 5)
        )
      ),
      class = "compact stripe hover"
    ) |>
      formatStyle("Statut",
        backgroundColor = styleEqual(
          c("\u2705 D\u00e9tect\u00e9", "\u274c Non trouv\u00e9"),
          c("#f0fff4", "#fff5f5")
        ),
        fontWeight = "600", fontSize = "12px"
      )
  })

  output$mapper_col1 <- renderUI({
    req(raw_data())
    keys1 <- names(mapper_defs)[1:8]
    tagList(
      tags$div(class="section-header","Variables nutritionnelles et démographiques"),
      lapply(keys1, function(k) make_mapper_row(k, mapper_defs[[k]]))
    )
  })

  output$mapper_col2 <- renderUI({
    req(raw_data())
    keys2 <- names(mapper_defs)[9:length(mapper_defs)]
    tagList(
      tags$div(class="section-header","Variables géographiques, PAM et déplacement"),
      lapply(keys2, function(k) make_mapper_row(k, mapper_defs[[k]]))
    )
  })

  output$import_preview <- renderDT({
    df <- raw_data(); req(df)
    df |> head(15) |>
      datatable(rownames=FALSE,
                options=list(dom="t",scrollX=TRUE,pageLength=15),
                class="compact stripe")
  })

  output$import_summary <- renderUI({
    df <- raw_data(); req(df); m <- map_cols()
    detected <- sum(!sapply(m,is.null))
    tags$div(
      tags$div(class="stat-card",
        tags$p(class="sc-val", format(nrow(df),big.mark=" ")),
        tags$p(class="sc-lbl","Observations")),
      tags$div(class="stat-card",
        tags$p(class="sc-val", ncol(df)),
        tags$p(class="sc-lbl","Variables")),
      tags$div(class="stat-card",
        tags$p(class="sc-val", paste0(detected,"/",length(OMS_DICT))),
        tags$p(class="sc-lbl","Colonnes OMS détectées")),
      tags$div(class="stat-card",
        tags$p(class="sc-val",
          paste0(round(mean(sapply(df,function(x) mean(is.na(x))))*100,1),"%")),
        tags$p(class="sc-lbl","Valeurs manquantes (moy.)")),
      tags$p(style="font-size:12px;color:#888;margin-top:8px;",
        tags$b("Num. : "), sum(sapply(df,is.numeric)), " | ",
        tags$b("Catég. : "), sum(sapply(df,function(x)is.factor(x)||is.character(x)))
      )
    )
  })

  # ── CONFORMITÉ OMS ─────────────────────────────────────────────────────────
  # Reactive : corrections manuelles (key = colonne, value = variable OMS)
  oms_manual_fixes <- reactiveVal(list())

  # Reactive : résultat du check de conformité
  oms_check_data <- reactive({
    df <- raw_data(); req(df)
    check <- oms_conformity_check(df)
    # Appliquer les corrections manuelles
    fixes <- oms_manual_fixes()
    for (col in names(fixes)) {
      idx <- which(check$colonne == col)
      if (length(idx) == 1) {
        key <- fixes[[col]]
        meta <- if (key %in% names(OMS_META)) OMS_META[[key]]
                else list(cat = "", label = key, unit = "", seuils = "", type = "")
        check$variable_oms[idx]  <- key
        check$label_oms[idx]     <- meta$label
        check$categorie_oms[idx] <- meta$cat
        check$confiance[idx]     <- "manual"
        check$type_attendu[idx]  <- meta$type
        check$unite[idx]         <- meta$unit
        check$seuils[idx]        <- meta$seuils
      }
    }
    check
  })

  # Score global de conformité
  output$oms_conform_score <- renderUI({
    check <- oms_check_data(); req(check)
    n_total  <- nrow(check)
    n_high   <- sum(check$confiance == "high", na.rm = TRUE)
    n_medium <- sum(check$confiance == "medium", na.rm = TRUE)
    n_manual <- sum(check$confiance == "manual", na.rm = TRUE)
    n_none   <- sum(check$confiance == "none", na.rm = TRUE)
    n_detected <- n_high + n_medium + n_manual
    pct <- round(n_detected / n_total * 100, 0)

    # Couleur selon le score
    score_col <- if (pct >= 70) "#2E7D52" else if (pct >= 40) "#D98B14" else "#BF3636"

    tags$div(style = "display:flex;gap:16px;flex-wrap:wrap;margin:12px 0;",
      # Score principal
      tags$div(
        style = sprintf("background:white;border-radius:12px;padding:16px 24px;
                         border-left:5px solid %s;box-shadow:0 2px 8px rgba(0,0,0,.06);
                         min-width:180px;text-align:center;", score_col),
        tags$div(style = sprintf("font-size:36px;font-weight:800;color:%s;", score_col),
                 paste0(n_detected, "/", n_total)),
        tags$div(style = "font-size:12px;color:#64748b;font-weight:600;",
                 paste0("Variables conformes OMS (", pct, "%)"))
      ),
      # Détail par confiance
      tags$div(style = "background:white;border-radius:12px;padding:16px 20px;
                         box-shadow:0 2px 8px rgba(0,0,0,.06);flex:1;min-width:300px;",
        tags$div(style="display:flex;gap:20px;flex-wrap:wrap;",
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#00838F;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_high, " détection certaine"))
          ),
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#f59e0b;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_medium, " détection probable"))
          ),
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#6366f1;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_manual, " assignation manuelle"))
          ),
          tags$div(
            tags$span(style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#dc2626;margin-right:6px;"),
            tags$span(style="font-size:13px;color:#334155;", paste0(n_none, " non reconnu"))
          )
        )
      )
    )
  })

  # Tableau de conformité interactif
  output$oms_conform_table <- renderDT({
    check <- oms_check_data(); req(check)

    # Colonnes d'affichage
    conf_label <- function(c) switch(c,
      "high"   = paste0("\u2705 Certaine"),
      "medium" = paste0("\u26a0\ufe0f Probable"),
      "manual" = paste0("\u2714 Manuelle"),
      paste0("\u274c Non reconnu")
    )
    em_dash <- "\u2014"
    disp <- data.frame(
      Colonne       = check$colonne,
      Type          = check$type_r,
      Variable.OMS  = ifelse(is.na(check$variable_oms), em_dash,
                             paste0(check$variable_oms, " ", em_dash, " ", check$label_oms)),
      Categorie     = ifelse(check$categorie_oms == "", em_dash, check$categorie_oms),
      Confiance     = vapply(check$confiance, conf_label, character(1)),
      Seuils        = ifelse(check$seuils == "", em_dash, check$seuils),
      check = FALSE, stringsAsFactors = FALSE
    )

    datatable(disp, rownames = FALSE, selection = "single",
      options = list(
        pageLength = 20, scrollX = TRUE,
        dom = "frtip",
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ lignes",
          info = "_START_ \u00e0 _END_ sur _TOTAL_ colonnes",
          paginate = list(`previous` = "Pr\u00e9c\u00e9dent", `next` = "Suivant")
        ),
        columnDefs = list(
          list(width = "160px", targets = 0),
          list(width = "70px",  targets = 1),
          list(width = "280px", targets = 2),
          list(width = "130px", targets = 3),
          list(width = "110px", targets = 4)
        )
      ),
      class = "compact stripe hover"
    ) |>
      formatStyle("Confiance",
        backgroundColor = styleEqual(
          c("\u2705 Certaine", "\u26a0\ufe0f Probable", "\u2714 Manuelle", "\u274c Non reconnu"),
          c("#f0fff4", "#fffbeb", "#f0f0ff", "#fff5f5")
        ),
        fontWeight = "600", fontSize = "12px"
      )
  })

  # Mettre à jour les dropdown de correction
  observe({
    df <- raw_data(); req(df)
    check <- oms_check_data()
    # Colonnes non reconnues ou probables
    cols_to_fix <- check$colonne[check$confiance %in% c("none", "medium")]
    all_cols <- check$colonne
    updateSelectInput(session, "oms_fix_col",
                      choices = c("-- Choisir --" = "", all_cols),
                      selected = "")

    # Variables OMS disponibles (avec labels)
    oms_choices <- setNames(
      names(OMS_META),
      sapply(names(OMS_META), function(k) paste0(k, " \u2014 ", OMS_META[[k]]$label))
    )
    updateSelectInput(session, "oms_fix_var",
                      choices = c("-- Variable OMS --" = "", oms_choices),
                      selected = "")
  })

  # Appliquer une correction manuelle
  observeEvent(input$oms_fix_apply, {
    req(input$oms_fix_col != "", input$oms_fix_var != "")
    fixes <- oms_manual_fixes()
    fixes[[input$oms_fix_col]] <- input$oms_fix_var
    oms_manual_fixes(fixes)

    # Mettre aussi à jour le map_cols pour la cohérence
    m <- map_cols()
    if (!is.null(m)) {
      m[[input$oms_fix_var]] <- input$oms_fix_col
      map_cols(m)
    }

    showNotification(
      sprintf("\u2705 %s \u2192 %s (%s)",
              input$oms_fix_col, input$oms_fix_var,
              OMS_META[[input$oms_fix_var]]$label),
      type = "message", duration = 4
    )
  })

  # Réinitialiser les corrections
  observeEvent(input$oms_fix_reset, {
    oms_manual_fixes(list())
    showNotification("\u21A9 Corrections manuelles réinitialisées", type = "warning", duration = 3)
  })

  # Clic sur une ligne du tableau → pré-remplir le dropdown
  observeEvent(input$oms_conform_table_rows_selected, {
    check <- oms_check_data()
    sel <- input$oms_conform_table_rows_selected
    req(length(sel) == 1)
    updateSelectInput(session, "oms_fix_col", selected = check$colonne[sel])
  })

  # ── Panneau détail dictionnaire (clic sur ligne conformité) ──────────
  output$dict_selected_aliases <- renderUI({
    check <- oms_check_data()
    sel   <- input$oms_conform_table_rows_selected
    req(length(sel) == 1)

    col_name <- check$colonne[sel]
    var_oms  <- check$variable_oms[sel]
    df       <- raw_data(); req(df)

    # Aperçu des valeurs
    sample_vals <- if (col_name %in% names(df)) {
      vals <- head(na.omit(df[[col_name]]), 8)
      paste(vals, collapse = " \u2502 ")
    } else "\u2014"

    # Badges des alias
    aliases_html <- if (!is.na(var_oms) && var_oms %in% names(OMS_DICT)) {
      aliases <- OMS_DICT[[var_oms]]
      tags$div(style = "display:flex;flex-wrap:wrap;gap:4px;",
        lapply(aliases, function(a) {
          is_match <- tolower(a) == tolower(col_name)
          tags$span(
            style = sprintf(
              "display:inline-block;padding:3px 10px;border-radius:14px;font-size:11px;%s",
              if (is_match) "background:#00838F;color:white;font-weight:700;"
              else "background:#e2e8f0;color:#475569;"
            ), a
          )
        })
      )
    } else {
      # Pas de variable OMS : montrer toutes les variables possibles du dictionnaire
      tags$div(
        tags$p(style = "color:#888;font-size:12px;margin-bottom:6px;",
               "Aucune variable OMS d\u00e9tect\u00e9e. Voici les variables disponibles dans le dictionnaire :"),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:4px;max-height:200px;overflow-y:auto;",
          lapply(names(OMS_DICT), function(k) {
            lbl <- if (k %in% names(OMS_META)) OMS_META[[k]]$label else k
            tags$span(
              style = "display:inline-block;padding:3px 10px;border-radius:14px;
                       font-size:11px;background:#f0f4ff;color:#3b5998;cursor:default;",
              paste0(k, " (", lbl, ")")
            )
          })
        )
      )
    }

    tags$div(
      style = "background:white;border:1px solid #e2e8f0;border-radius:10px;
               padding:16px 20px;margin-top:14px;box-shadow:0 2px 8px rgba(0,0,0,.05);",
      tags$div(style = "display:flex;align-items:center;gap:10px;margin-bottom:12px;",
        tags$div(
          style = "width:36px;height:36px;border-radius:8px;
                   background:linear-gradient(135deg,#0077B6,#00838F);
                   display:flex;align-items:center;justify-content:center;",
          icon("search-plus", style = "color:white;font-size:16px;")
        ),
        tags$h5(style = "margin:0;color:#1e293b;font-weight:700;font-size:14px;",
                paste0("D\u00e9tail : ", col_name,
                       if (!is.na(var_oms)) paste0(" \u2192 ", var_oms) else ""))
      ),
      fluidRow(
        column(4,
          tags$div(style = "font-weight:600;color:#475569;font-size:12px;margin-bottom:6px;",
                   "Valeurs \u00e9chantillon de votre colonne :"),
          tags$div(style = "font-size:12px;color:#1e293b;background:#f8fafc;
                            padding:8px 12px;border-radius:6px;word-break:break-all;",
                   sample_vals)
        ),
        column(8,
          tags$div(style = "font-weight:600;color:#475569;font-size:12px;margin-bottom:6px;",
                   if (!is.na(var_oms))
                     paste0("Alias reconnus pour \u00ab ", var_oms, " \u00bb dans le dictionnaire :")
                   else "Variables du dictionnaire OMS :"),
          aliases_html
        )
      )
    )
  })

