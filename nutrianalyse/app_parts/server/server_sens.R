# Extrait de app_v2_final.R — modules SENS — mortalité, IYCF, anémie, paludisme, WASH, ménage, handicap
  # ══════════════════════════════════════════════════════════════════════════════
  # SERVEUR — MODULES SENS (Mortalité, IYCF, Anémie, Paludisme, WASH, Ménage, WG)
  # ══════════════════════════════════════════════════════════════════════════════

  # Helper remplacé par sens_select_ns() dans mod_sens_helpers.R (namespacé).

  # ── MORTALITÉ — module namespacé (mod_mortalite.R) ─────────────────────────
  mod_mortalite_server("mortalite", df_f = df_f)

  # ── IYCF — module namespacé (mod_iycf.R) ──────────────────────────────────────
  mod_iycf_server("iycf", df_f = df_f)

  # ── ANÉMIE — module namespacé (mod_anemie.R) ───────────────────────────────
  mod_anemie_server("anemie", df_f = df_f)

  # ── PALUDISME — module namespacé (mod_paludisme.R) ─────────────────────────
  mod_paludisme_server("paludisme", df_f = df_f)

  # ── WASH — module namespacé (mod_wash.R) ───────────────────────────────────
  mod_wash_server("wash", df_f = df_f)

  # ── MÉNAGE & PROTECTION — module namespacé (mod_menage.R) ──────────────────
  mod_menage_server("menage", df_f = df_f)

  # ── HANDICAP — module namespacé (mod_handicap.R) ───────────────────────────
  mod_handicap_server("handicap", df_f = df_f)

