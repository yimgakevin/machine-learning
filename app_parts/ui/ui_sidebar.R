# Extrait de app_v2_final.R — ui_sidebar
ui_sidebar <- function() {
  dashboardSidebar(
    width = 265,
    useShinyjs(),

    # Zone upload — version robuste
    tags$div(
      style = "padding:10px 12px 6px;",
      tags$div(
        style = paste0(
          "border:2px dashed rgba(255,255,255,.4);border-radius:8px;",
          "padding:10px;background:rgba(255,255,255,.06);"
        ),
        tags$label(
          style = "display:block;color:rgba(212,168,67,.8);font-size:11px;margin-bottom:6px;text-align:center;font-weight:500;",
          uiOutput("sidebar_upload_label", inline = TRUE)
        ),
        fileInput("upload_file", NULL,
          accept = c(".csv",".tsv",".xlsx",".xls",".sav",".dta",".parquet"),
          buttonLabel = uiOutput("sidebar_btn_label"),
          placeholder = "",
          width = "100%"
        ),
        tags$small(
          style = "display:block;text-align:center;color:rgba(180,210,235,.6);font-size:10px;margin-top:3px;",
          uiOutput("sidebar_formats_label", inline = TRUE)
        )
      ),
      uiOutput("sidebar_status")
    ),
    tags$hr(style = "border-color:rgba(255,255,255,.1); margin:4px 0;"),

    # Sélecteur géographique — visible uniquement sur l'onglet Cartographie
    conditionalPanel(
      condition = "input.menu == 'carto'",
      tags$div(
        style = "background:linear-gradient(180deg,#0E4468 0%,#082740 100%);
                 padding:10px 12px; border-bottom:2px solid #D4A843;
                 margin-bottom:10px; border-radius:6px;",
        tags$h4(style = "margin:0 0 8px; color:white; font-size:13px; font-weight:600;",
                uiOutput("sidebar_geo_label", inline = TRUE)),
        carto_country_selector_UI("country_selector")
      ),
      tags$hr(style="border-color:rgba(255,255,255,.15);margin:4px 8px;")
    ),

    # ── Menu dynamique i18n ───────────────────────────────────────────────
    sidebarMenuOutput("dynamic_menu"),

    # ── Sélecteur de palette global ─────────────────────────────────────
    tags$hr(style="border-color:rgba(255,255,255,.1);margin:4px 0;"),
    tags$div(
      style = "padding:6px 12px 10px;",
      tags$div(style="color:#D4A843;font-size:11px;font-weight:600;margin-bottom:4px;",
               icon("palette"), " Th\u00e8me couleurs"),
      selectInput("global_palette", NULL,
                  choices = PALETTE_CHOICES,
                  selected = "OMS officiel",
                  width = "100%"),
      tags$div(style="color:#D4A843;font-size:11px;font-weight:600;margin:8px 0 4px;",
               icon("paint-roller"), " Th\u00e8me d'interface"),
      selectInput("ui_theme", NULL,
                  choices = UI_THEME_CHOICES,
                  selected = "OMS Classique",
                  width = "100%")
    ),

    tags$div(
      style="position:absolute;bottom:10px;left:0;right:0;text-align:center;
             font-size:10px;color:rgba(255,255,255,.35);padding:0 12px;",
      "NutriAnalyse Pro v2 \u00b7 DSAMID Data Science & Analysis"
    )
  )
}
