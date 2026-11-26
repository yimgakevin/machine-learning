# Extrait de app_v2_final.R — Accueil : pont racine vers mod_accueil.R
  # ── ACCUEIL — module namespacé (mod_accueil.R) ─────────────────────────────
  # event_data et les update*() des filtres doivent rester au scope racine :
  # la source plotly 'accueil_prev' et les IDs f_* sont globaux.
  accueil_prev_click <- event_data("plotly_click", source = "accueil_prev")
  accueil_f_region <- reactive(input$f_region)

  mod_accueil_server("accueil",
    df_f = df_f, mapped_data = mapped_data, map_cols = map_cols,
    vars_num = vars_num, vars_cat = vars_cat,
    filter_context = filter_context, has_oms = has_oms,
    f_region = accueil_f_region, accueil_click = accueil_prev_click,
    parent_session = session, gpal = gpal, lang = lang)
