# Extrait de app_v2_final.R — ui_header
ui_header <- function() {
  dashboardHeader(
    title = tags$span(
      tags$span(
        style = "display:inline-flex;align-items:center;justify-content:center;
                 width:30px;height:30px;border-radius:8px;margin-right:8px;vertical-align:middle;
                 background:linear-gradient(135deg,#D4A843,#B8922F);font-size:14px;font-weight:800;
                 color:#0B3954;letter-spacing:-0.5px;box-shadow:0 2px 6px rgba(212,168,67,.4);",
        "DS"
      ),
      tags$span(style = "font-weight:700;letter-spacing:.3px;", "NutriAnalyse Pro"),
      tags$span(style = "font-size:10px;opacity:.7;margin-left:4px;font-weight:400;", "by DSAMID")
    ),
    titleWidth = 270,
    # ── Sélecteur de langue ────────────────────────────────────────────────
    tags$li(
      class = "dropdown",
      style = "padding:8px 10px;",
      tags$div(
        style = "display:flex;gap:4px;align-items:center;",
        actionButton("lang_fr", "🇫🇷 FR",
          class = "btn btn-xs btn-lang btn-lang-active",
          style = "padding:2px 8px;font-size:11px;font-weight:600;border-radius:4px;
                   background:#D4A843;color:#0B3954;border:none;cursor:pointer;"
        ),
        actionButton("lang_en", "🇬🇧 EN",
          class = "btn btn-xs btn-lang",
          style = "padding:2px 8px;font-size:11px;font-weight:600;border-radius:4px;
                   background:rgba(255,255,255,.15);color:white;border:none;cursor:pointer;"
        )
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(href="#", style="color:white;font-size:12px;opacity:.8;",
             uiOutput("header_status", inline=TRUE))
    )
  )
}
