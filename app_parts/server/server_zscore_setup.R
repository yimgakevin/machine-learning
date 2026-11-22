# Extrait de app_v2_final.R — z-scores — réactif et intégration expert
  # ══════════════════════════════════════════════════════════════════════════════
  # Z-SCORES OMS 2006 — Server Logic
  # ══════════════════════════════════════════════════════════════════════════════

  zs_data <- reactiveVal(NULL)
  source("zscore_expert_integration.R", local = TRUE)

  # Auto-détection des colonnes
  observe({
    d <- df_f(); req(d)
    noms <- names(d)
    det <- detect_zscore_columns(noms)
    num_cols <- noms[vapply(d, is.numeric, logical(1))]
    all_cols <- noms

    output$zs_col_poids_ui <- renderUI({
      sel <- if (!is.null(det$poids)) det$poids else NULL
      selectInput("zs_col_poids", "Poids (kg) :", choices = c("—" = "", num_cols), selected = sel)
    })
    output$zs_col_taille_ui <- renderUI({
      sel <- if (!is.null(det$taille)) det$taille else NULL
      selectInput("zs_col_taille", "Taille/Longueur (cm) :", choices = c("—" = "", num_cols), selected = sel)
    })
    output$zs_col_age_ui <- renderUI({
      sel <- if (!is.null(det$age)) det$age else NULL
      selectInput("zs_col_age", "Âge (mois) :", choices = c("—" = "", num_cols), selected = sel)
    })
    output$zs_col_sexe_ui <- renderUI({
      sel <- if (!is.null(det$sexe)) det$sexe else NULL
      selectInput("zs_col_sexe", "Sexe :", choices = c("—" = "", all_cols), selected = sel)
    })
  })

