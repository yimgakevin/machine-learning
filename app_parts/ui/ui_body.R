# Extrait de app_v2_final.R — ui_body
ui_body <- function() {
  dashboardBody(
    tags$style(CSS),

    # ── CSS login ────────────────────────────────────────────────────────────
    tags$style(HTML("
      .login-overlay {
        position: fixed; inset: 0; z-index: 99999;
        background: linear-gradient(135deg, #0B3954 0%, #0C4A6E 40%, #00838F 100%);
        display: flex; align-items: center; justify-content: center;
      }
      .login-card {
        background: white; border-radius: 18px; padding: 42px 46px;
        width: 390px; box-shadow: 0 24px 72px rgba(0,0,0,.35);
        text-align: center;
        border-top: 4px solid #D4A843;
      }
      .login-logo {
        width: 68px; height: 68px; border-radius: 18px; margin: 0 auto 16px;
        background: linear-gradient(135deg, #0B3954, #0C4A6E);
        display: flex; align-items: center; justify-content: center;
        font-size: 28px; color: white;
        box-shadow: 0 4px 16px rgba(11,57,84,.3);
      }
      .login-card h2 { color: #0B3954; font-weight: 800; margin: 0 0 4px; font-size: 20px; }
      .login-card .subtitle { color: #6B7B8D; font-size: 12px; margin-bottom: 28px; }
      .login-card .form-control {
        border: 1.5px solid #E2E8F0 !important; border-radius: 10px !important;
        padding: 11px 14px !important; font-size: 13px !important;
        transition: border-color .2s, box-shadow .2s;
        background: #FAFBFC;
      }
      .login-card .form-control:focus { border-color: #0077B6 !important; box-shadow: 0 0 0 3px rgba(0,119,182,.1); background: white; }
      .login-card label { font-size: 12px; font-weight: 600; color: #4A5568; text-align: left; display: block; }
      .btn-login {
        background: linear-gradient(135deg, #0B3954, #0C4A6E) !important;
        color: white !important; width: 100%; border: none !important;
        border-radius: 10px !important; padding: 12px !important;
        font-weight: 700 !important; font-size: 14px !important;
        margin-top: 8px; transition: all .25s;
        box-shadow: 0 4px 16px rgba(11,57,84,.25);
      }
      .btn-login:hover { box-shadow: 0 6px 24px rgba(11,57,84,.35); transform: translateY(-1px); }
      .login-error { color: #BF3636; font-size: 12px; margin-top: 8px; }
      .login-footer { font-size: 10px; color: #94A3B8; margin-top: 20px; }
    ")),

    # ── Écran de login (masqué après connexion) ───────────────────────────────
    shinyauthr::loginUI(
      id            = "login",
      title         = tags$div(
        tags$div(class = "login-logo", "🌿"),
        tags$h2("NutriAnalyse Pro"),
        tags$p(class = "subtitle",
               tags$span(style="font-weight:600;", "DSAMID"),
               " · ", uiOutput("login_subtitle_out", inline = TRUE))
      ),
      user_title    = uiOutput("login_user_out",  inline = TRUE),
      pass_title    = uiOutput("login_pass_out",  inline = TRUE),
      login_title   = uiOutput("login_btn_out",   inline = TRUE),
      error_message = uiOutput("login_error_out", inline = TRUE),
      additional_ui = tags$div(
        tags$p(class = "login-footer",
               uiOutput("login_footer_out", inline = TRUE)),
        tags$div(
          style = "margin-top:12px;display:flex;justify-content:center;gap:16px;",
          tags$a(
            href = "https://www.linkedin.com/in/kevin-l-agneau-yimga-wandji-b6959b2a7/",
            target = "_blank",
            style = "color:#0C4A6E;font-size:11px;text-decoration:none;",
            icon("linkedin"), " LinkedIn"
          ),
          tags$a(
            href = "https://github.com/yimgakevin",
            target = "_blank",
            style = "color:#333;font-size:11px;text-decoration:none;",
            icon("github"), " GitHub"
          )
,
          tags$a(
            href = "https://orcid.org/0009-0006-1000-7339",
            target = "_blank",
            style = "color:#A6CE39;font-size:11px;text-decoration:none;font-weight:600;",
            tags$span(
              style = "display:inline-flex;align-items:center;justify-content:center;
                       width:14px;height:14px;border-radius:50%;background:#A6CE39;
                       color:white;font-size:7px;font-weight:900;margin-right:3px;",
              "iD"
            ),
            "ORCID"
          )
        )
      )
    ),

    # ── Application principale (visible après login) ──────────────────────────
    shinyjs::hidden(
      tags$div(
        id = "main_app_content",

    # ── CSS du chatbot flottant ───────────────────────────────────────────────
    tags$style(HTML("
      #ai_float_btn {
        position: fixed; bottom: 28px; right: 28px; z-index: 9999;
        width: 56px; height: 56px; border-radius: 50%;
        background: linear-gradient(135deg, #D4A843, #B8922F);
        color: #0B3954; font-size: 22px; border: none; cursor: pointer;
        box-shadow: 0 4px 16px rgba(212,168,67,.45);
        transition: transform .25s, box-shadow .25s;
        display: flex; align-items: center; justify-content: center;
      }
      #ai_float_btn:hover {
        transform: translateY(-3px) scale(1.06);
        box-shadow: 0 8px 28px rgba(212,168,67,.55);
      }
      #ai_float_panel {
        position: fixed; bottom: 95px; right: 28px; z-index: 9998;
        width: 380px; max-height: 580px;
        background: white; border-radius: 16px;
        box-shadow: 0 10px 40px rgba(11,57,84,.22);
        border: 1px solid rgba(11,57,84,.1);
        display: none; flex-direction: column; overflow: hidden;
      }
      #ai_float_panel.open { display: flex; }
      .ai-panel-header {
        background: linear-gradient(135deg, #0B3954 0%, #0C4A6E 100%);
        color: white; padding: 14px 18px;
        display: flex; align-items: center; justify-content: space-between;
        font-weight: 600; font-size: 14px; flex-shrink: 0;
        border-bottom: 2px solid #D4A843;
      }
      .ai-panel-header .ai-close {
        cursor: pointer; opacity: .75; font-size: 18px; line-height: 1;
        background: none; border: none; color: white; padding: 0;
      }
      .ai-panel-header .ai-close:hover { opacity: 1; }
      .ai-panel-body { flex: 1; overflow: hidden; }
    ")),

    # ── Bouton flottant 🤖 ─────────────────────────────────────────────────────
    tags$button(
      id = "ai_float_btn",
      title = "IA Assistant NutriAnalyse",
      tags$span("🤖")
    ),

    # ── Panel chatbot flottant ─────────────────────────────────────────────────
    tags$div(
      id = "ai_float_panel",
      tags$div(
        class = "ai-panel-header",
        tags$span(tags$span("🤖"), "  NutriAnalyse IA"),
        tags$button(class = "ai-close", onclick = "toggleAIPanel()", "✕")
      ),
      tags$div(
        class = "ai-panel-body",
        chatbot_ui("ai_chat", width = "100%", height = "460px")
      )
    ),

    # ── Script toggle du panel ─────────────────────────────────────────────────
    tags$script(HTML("
      function toggleAIPanel() {
        var panel = document.getElementById('ai_float_panel');
        panel.classList.toggle('open');
      }
      document.addEventListener('DOMContentLoaded', function() {
        var btn = document.getElementById('ai_float_btn');
        if (btn) btn.addEventListener('click', toggleAIPanel);
      });
    ")),

    tabItems(

      # ════════════════════════════════════════════
      # 0 — TABLEAU DE BORD EXÉCUTIF
      # ════════════════════════════════════════════
      ui_tab_accueil(),

      # ════════════════════════════════════════════
      # 1 — IMPORT & MAPPEUR
      # ════════════════════════════════════════════
      ui_tab_import(),

      # ════════════════════════════════════════════
      # 2 — EXPLORATION
      # ════════════════════════════════════════════
      mod_explor_ui("explor"),

      # ════════════════════════════════════════════
      # DPLYR — Manipulation des données
      # ════════════════════════════════════════════
      mod_dplyr_ui("dplyr"),

      # ════════════════════════════════════════════
      # 3a — UNIVARIÉ
      # ════════════════════════════════════════════
      mod_uni_ui("uni"),

      # ════════════════════════════════════════════
      # 3b — BIVARIÉ
      # ════════════════════════════════════════════
      mod_biv_ui("biv"),

      # ════════════════════════════════════════════
      # 3c — DESCRIPTIVE OMS
      # ════════════════════════════════════════════
      mod_descoms_ui("descoms"),

      # ════════════════════════════════════════════
      # 4 — INFÉRENTIELLE
      # ════════════════════════════════════════════
      ui_tab_infer(),

      # ════════════════════════════════════════════
      # 5 — RÉDUCTION DE DIMENSION (FactoMineR)
      # ════════════════════════════════════════════
      mod_rdim_ui("rdim"),

      # ════════════════════════════════════════════
      # 6 — CLUSTERING
      # ════════════════════════════════════════════
      mod_clust_ui("clust"),

      # ════════════════════════════════════════════
      # ════════════════════════════════════════════
      # 6b — QUALITÉ SMART (Plausibility Check)
      # ════════════════════════════════════════════
      mod_qualite_ui("qualite"),

      # ════════════════════════════════════════════
      # 6c — PRÉVALENCES SMART (pondérées + DEFF)
      # ════════════════════════════════════════════
      mod_prevalence_ui("prevalence"),

      # ════════════════════════════════════════════
      # 6d — SÉCURITÉ ALIMENTAIRE (FCS/rCSI/HDDS/HHS)
      # ════════════════════════════════════════════
      mod_secu_alim_ui("secu_alim"),

      # ════════════════════════════════════════════
      # SENS-1 — MORTALITÉ SMART
      # ════════════════════════════════════════════
      ui_tab_mortalite(),

      # ════════════════════════════════════════════
      # SENS-2 — IYCF / ANJE
      # ════════════════════════════════════════════
      ui_tab_iycf(),

      # ════════════════════════════════════════════
      # SENS-3 — ANÉMIE
      # ════════════════════════════════════════════
      ui_tab_anemie(),

      # ════════════════════════════════════════════
      # SENS-4 — PALUDISME / MOUSTIQUAIRES
      # ════════════════════════════════════════════
      ui_tab_paludisme(),

      # ════════════════════════════════════════════
      # SENS-5 — WASH
      # ════════════════════════════════════════════
      ui_tab_wash(),

      # ════════════════════════════════════════════
      # SENS-6 — MÉNAGE & PROTECTION
      # ════════════════════════════════════════════
      ui_tab_menage(),

      # ════════════════════════════════════════════
      # SENS-7 — HANDICAP (WASHINGTON GROUP)
      # ════════════════════════════════════════════
      ui_tab_handicap(),

      # ════════════════════════════════════════════
      # 7b — Z-SCORES OMS 2006
      # ════════════════════════════════════════════
      ui_tab_zscore(),

      # ════════════════════════════════════════════
      # 7 — INDICATEURS OMS/PAM
      # ════════════════════════════════════════════
      mod_oms_ui("oms"),

      # ════════════════════════════════════════════
      # 8 — CARTOGRAPHIE
      # ════════════════════════════════════════════
      ui_carto_pro,
      # ════════════════════════════════════
      # 9a — ÉPIDÉMIOLOGIE
      # ════════════════════════════════════
      tabItem(tabName="epidemio", uiOutput("epidemio_ui_dyn")),

      # ════════════════════════════════════
      # 9b — ANALYSE DE SURVIE
      # ════════════════════════════════════
      tabItem(tabName="survie", uiOutput("survie_ui_dyn")),

      # ════════════════════════════════════
      # 9c — MODÈLE PRÉDICTIF
      # ════════════════════════════════════
      tabItem(tabName="predictif", uiOutput("predictif_ui_dyn")),

      # ════════════════════════════════════════════
      # 10 — RAPPORT EXPORTABLE (moduleServer)
      # ════════════════════════════════════════════
      tabItem(tabName="rapport", uiOutput("rapport_ui_dyn")),

      # ════════════════════════════════════════════
      # ⭐ MODULES EXPERT
      # ════════════════════════════════════════════
      tabItem(tabName="exec_dash",      uiOutput("exec_dash_ui_dyn")),
      tabItem(tabName="qualite_auto",   uiOutput("qualite_auto_ui_dyn")),
      tabItem(tabName="audit_trail",    uiOutput("audit_trail_ui_dyn")),
      tabItem(tabName="temporal_tests", uiOutput("temporal_tests_ui_dyn")),

      # ── À PROPOS ──────────────────────────────────────────────────────────
      mod_about_ui("about"),

      # ════════════════════════════════════════════
      # 🔬 RECHERCHE AVANCÉE
      # ════════════════════════════════════════════
      tabItem(tabName="advanced_forecasting", uiOutput("forecasting_ui_dyn")),
      tabItem(tabName="advanced_geolocation", uiOutput("geolocation_ui_dyn")),
      tabItem(tabName="advanced_nlp", uiOutput("nlp_ui_dyn")),
      tabItem(tabName="advanced_benchmarking", uiOutput("benchmarking_ui_dyn")),

      # ── BASES DE DONNÉES & SOURCES EXTERNES ─────────────────────────────
      tabItem(tabName="db_sql",     uiOutput("db_sql_ui_dyn")),
      tabItem(tabName="db_dhis2",   uiOutput("db_dhis2_ui_dyn")),
      tabItem(tabName="db_odk",     uiOutput("db_odk_ui_dyn")),
      tabItem(tabName="db_redcap",  uiOutput("db_redcap_ui_dyn"))
    )

      ) # ferme tags$div(id="main_app_content")
  ) # ferme shinyjs::hidden
  ) # ferme dashboardBody
}
