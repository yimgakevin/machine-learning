# Assemblage de l'interface — NutriAnalyse Pro v2
source("app_parts/ui/ui_header.R", local = FALSE)
source("app_parts/ui/ui_sidebar.R", local = FALSE)
source("app_parts/ui/ui_body.R", local = FALSE)

ui <- dashboardPage(
  skin = "blue",
  ui_header(),
  ui_sidebar(),
  ui_body()
)
