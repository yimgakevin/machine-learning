# Extrait de app_v2_final.R — z-scores — réactif partagé et pont vers mod_zscore.R
  # ══════════════════════════════════════════════════════════════════════════════
  # Z-SCORES OMS 2006 — zs_data reste au scope RACINE (partagé entre onglets)
  # ══════════════════════════════════════════════════════════════════════════════

  zs_data <- reactiveVal(NULL)

  mod_zscore_server("zscore", df_f = df_f, raw_data = raw_data, zs_data = zs_data)
