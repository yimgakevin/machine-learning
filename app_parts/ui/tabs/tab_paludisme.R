# Extrait de app_parts/ui/ui_body.R — onglet paludisme
ui_tab_paludisme <- function() {
      tabItem(tabName="paludisme",
        fluidRow(box(width=12, status="danger", solidHeader=TRUE,
          title=tags$span(icon("bug"), " Paludisme & Moustiquaires SENS (Roll Back Malaria / OMS)"),
          tags$div(style="background:#fdf2f2;border-left:4px solid #c0392b;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
            tags$b("Indicateurs :"), " Possession / Accès / Utilisation LLIN, Prévalence TDR+, Traitement ACT")
        )),
        fluidRow(
          column(3,
            box(width=12, title="Configuration", status="danger", solidHeader=TRUE, collapsible=TRUE,
              uiOutput("palu_poss_ui"), uiOutput("palu_use_ui"), uiOutput("palu_access_ui"),
              uiOutput("palu_nb_nets_ui"), uiOutput("palu_hh_size_ui"),
              uiOutput("palu_tdr_ui"), uiOutput("palu_fever_ui"), uiOutput("palu_strat_ui"),
              actionButton("run_palu", "🦟  Calculer", class="btn-danger btn-block", style="margin-top:10px;")
            )
          ),
          column(9,
            tabsetPanel(id="palu_tabs",
              tabPanel("Synthèse", uiOutput("palu_kpi")),
              tabPanel("Cascade LLIN", plotOutput("palu_cascade_plot", height="400px")),
              tabPanel("TDR par groupe", DT::DTOutput("palu_by_fever"), br(), DT::DTOutput("palu_by_group"))
            )
          )
        )
      )
}
