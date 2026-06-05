# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  APPLICATION SHINY — MALNUTRITION AU CAMEROUN                              ║
# ║  Style OMS/UNICEF | shinydashboard | 7 onglets                             ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── 1. PACKAGES ───────────────────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(forcats)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(forcats)
library(scales)
library(DT)
library(plotly)
library(ggiraph)
library(leaflet)
library(leaflet.extras)
library(sf)
library(binom)
library(gtsummary)
library(gt)
library(corrplot)
library(RColorBrewer)
library(viridis)
library(e1071)
library(nortest)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinyjs)
library(writexl)

# ── 2. PALETTE OMS / UNICEF ───────────────────────────────────────────────────
oms_bleu        <- "#008DC9"
oms_vert        <- "#4dac26"
oms_jaune       <- "#f9c734"
oms_orange      <- "#e88400"
oms_rouge       <- "#c9161d"
oms_rouge_f     <- "#640000"
oms_gris        <- "#6c757d"
oms_violet      <- "#7b2d8b"
unicef_bleu     <- "#009fda"
unicef_jaune    <- "#ffc20e"

pal_gam <- c(oms_vert, oms_jaune, oms_orange, oms_rouge, oms_rouge_f)

couleurs_region <- c(
  "Extrême-Nord" = "#E41A1C",
  "Adamaoua"     = "#377EB8",
  "Nord"         = "#4DAF4A"
)

# ── 3. CHARGEMENT & PRÉPARATION DES DONNÉES ───────────────────────────────────
df <- read_csv(
  "data/nutrition_cameroon_synthetic_hierarchy (1).csv",
  col_types = "fffffddDfddfdffffffdddddfffdfdddfdddffffffddffddfffffffff",
  show_col_types = FALSE
) |>
  rename_with(tolower) |>
  mutate(
    # Anthropométrie
    IMC = weight_kg / (height_cm / 100)^2,
    statut_imc = case_when(
      IMC < 18.5 ~ "Sous-poids",
      IMC < 25   ~ "Normal",
      IMC < 30   ~ "Surpoids",
      TRUE       ~ "Obèse"
    ) |> factor(levels = c("Sous-poids","Normal","Surpoids","Obèse")),

    oedema_bin = (oedema == "Yes"),

    # ── Indicateurs OMS 2006 ──────────────────────────────────────────────────
    GAM          = (whz_score < -2) | oedema_bin,
    SAM          = (whz_score < -3) | oedema_bin,
    MAM          = (whz_score >= -3) & (whz_score < -2) & !oedema_bin,
    Stunting     = haz_score < -2,
    Sev_Stunting = haz_score < -3,
    Underweight  = waz_score < -2,
    MUAC_GAM     = muac_mm < 125,
    MUAC_SAM     = muac_mm < 115,
    MUAC_MAM     = muac_mm >= 115 & muac_mm < 125,

    nutr_cat = case_when(
      SAM ~ "SAM", MAM ~ "MAM", TRUE ~ "Normal"
    ) |> factor(levels = c("Normal","MAM","SAM")),

    muac_cat = case_when(
      muac_mm < 115  ~ "SAM (<115mm)",
      muac_mm < 125  ~ "MAM (115-124mm)",
      TRUE           ~ "Normal (≥125mm)"
    ) |> factor(levels = c("Normal (≥125mm)","MAM (115-124mm)","SAM (<115mm)")),

    # Groupes d'âge OMS
    age_group = cut(child_age_months,
                    breaks = c(0,5,11,17,23,35,47,59,Inf),
                    labels = c("0-5m","6-11m","12-17m","18-23m",
                               "24-35m","36-47m","48-59m","60+m"),
                    right = TRUE),

    # Sécurité alimentaire (PAM)
    fcs_cat = case_when(
      food_consumption_score <= 21 ~ "Pauvre",
      food_consumption_score <= 35 ~ "Limite",
      TRUE                         ~ "Acceptable"
    ) |> factor(levels = c("Pauvre","Limite","Acceptable")),

    hhs_cat = case_when(
      household_hunger_scale == 0 ~ "Aucune faim",
      household_hunger_scale <= 1 ~ "Faim légère",
      household_hunger_scale <= 3 ~ "Faim modérée",
      TRUE                        ~ "Faim sévère"
    ) |> factor(levels = c("Aucune faim","Faim légère","Faim modérée","Faim sévère")),

    # ICV — Indice composite de vulnérabilité
    ICV = rescale(food_consumption_score, to=c(0,20),
                  from=range(food_consumption_score, na.rm=TRUE)) +
          rescale(household_hunger_scale,  to=c(20,0),
                  from=range(household_hunger_scale,  na.rm=TRUE)) +
          rescale(coping_strategy_index,   to=c(20,0),
                  from=range(coping_strategy_index,   na.rm=TRUE)) +
          rescale(muac_mm,                 to=c(0,20),
                  from=range(muac_mm,                 na.rm=TRUE)) +
          rescale(anc_visits,              to=c(0,20),
                  from=range(anc_visits,              na.rm=TRUE)),

    cat_vulner = cut(ICV, breaks=c(0,40,60,80,100),
                     labels=c("Très vulnérable","Vulnérable","Modéré","Résilient"),
                     include.lowest=TRUE)
  )

# ── Variables par type ────────────────────────────────────────────────────────
vars_cat <- df |> select(where(is.factor)) |> names()
vars_num <- df |> select(where(is.numeric)) |>
  select(-starts_with("score_")) |> names()
vars_cat_pie <- vars_cat[sapply(vars_cat, function(v)
  nlevels(df[[v]]) >= 2 & nlevels(df[[v]]) <= 6)]

# ── Fonctions utilitaires ─────────────────────────────────────────────────────
mode_stat <- function(x) {
  ux <- unique(na.omit(x)); ux[which.max(tabulate(match(x, ux)))]
}
cv_stat <- function(x) round(sd(x,na.rm=T) / mean(x,na.rm=T) * 100, 2)

calc_prev <- function(data, var, label = NULL) {
  x  <- sum(data[[var]], na.rm=TRUE)
  n  <- sum(!is.na(data[[var]]))
  ic <- binom.confint(x, n, methods = "wilson")
  tibble(
    Indicateur      = if(is.null(label)) var else label,
    N               = n, Cas = x,
    `Prév. (%)` = round(100 * ic$mean, 1),
    `IC95 bas`  = round(100 * ic$lower, 1),
    `IC95 haut` = round(100 * ic$upper, 1)
  )
}

# ── Agrégations géographiques ─────────────────────────────────────────────────
prev_region <- df |>
  group_by(region) |>
  summarise(
    n            = n(),
    GAM_pct      = mean(GAM,        na.rm=T)*100,
    SAM_pct      = mean(SAM,        na.rm=T)*100,
    MAM_pct      = mean(MAM,        na.rm=T)*100,
    Stunting_pct = mean(Stunting,   na.rm=T)*100,
    Under_pct    = mean(Underweight,na.rm=T)*100,
    MUAC_GAM_pct = mean(MUAC_GAM,  na.rm=T)*100,
    whz_moy      = mean(whz_score,  na.rm=T),
    haz_moy      = mean(haz_score,  na.rm=T),
    muac_moy     = mean(muac_mm,    na.rm=T),
    icv_moy      = mean(ICV,        na.rm=T),
    lat          = mean(gps_latitude, na.rm=T),
    lon          = mean(gps_longitude,na.rm=T),
    .groups = "drop"
  )

prev_dept <- df |>
  group_by(region, department) |>
  summarise(
    n            = n(),
    GAM_pct      = mean(GAM,        na.rm=T)*100,
    SAM_pct      = mean(SAM,        na.rm=T)*100,
    Stunting_pct = mean(Stunting,   na.rm=T)*100,
    muac_moy     = mean(muac_mm,    na.rm=T),
    icv_moy      = mean(ICV,        na.rm=T),
    lat          = mean(gps_latitude, na.rm=T),
    lon          = mean(gps_longitude,na.rm=T),
    .groups = "drop"
  )

# ── Fond de carte Cameroun (GADM) ─────────────────────────────────────────────
cameroun_sf <- tryCatch(
  st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_1.json",
          quiet = TRUE) |> rename(region = NAME_1),
  error = function(e) NULL
)

# ── Carte du monde ────────────────────────────────────────────────────────────
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# ══════════════════════════════════════════════════════════════════════════════
# 4. CSS PERSONNALISÉ
# ══════════════════════════════════════════════════════════════════════════════
css_oms <- "
/* ── Sidebar ── */
.main-sidebar, .left-side { background-color: #00426A !important; }
.sidebar-menu > li > a { color: #cce8f4 !important; font-size: 13px; }
.sidebar-menu > li.active > a,
.sidebar-menu > li > a:hover {
  background-color: #008DC9 !important;
  color: white !important;
  border-left: 4px solid #4dac26 !important;
}
.sidebar-menu .treeview-menu > li > a { color: #a8d4eb !important; font-size: 12px; }
.logo { background-color: #00426A !important; }
.logo:hover { background-color: #008DC9 !important; }

/* ── Header ── */
.main-header .navbar { background-color: #008DC9 !important; }
.main-header .logo { background-color: #00426A !important; color: white !important; font-weight: 700; font-size: 14px; }

/* ── Value boxes ── */
.small-box { border-radius: 8px !important; }
.small-box:hover { transform: scale(1.02); transition: 0.2s; }
.small-box .icon { font-size: 60px !important; }

/* ── Cards ── */
.box { border-radius: 8px !important; box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important; }
.box-header { border-radius: 8px 8px 0 0 !important; }

/* ── Buttons ── */
.btn-oms { background-color: #008DC9; color: white; border: none; border-radius: 4px; }
.btn-oms:hover { background-color: #006fa3; color: white; }

/* ── Tables DT ── */
table.dataTable thead th {
  background-color: #008DC9 !important;
  color: white !important;
  font-weight: 600;
}
table.dataTable tbody tr:hover { background-color: #e8f4fb !important; }

/* ── Selectize ── */
.selectize-input { border: 1px solid #008DC9 !important; border-radius: 4px !important; }

/* ── Body ── */
.content-wrapper, .right-side { background-color: #f4f6f9 !important; }

/* ── Seuils OMS ── */
.seuil-acceptable { color: #4dac26; font-weight: bold; }
.seuil-alerte     { color: #f9c734; font-weight: bold; }
.seuil-serieux    { color: #e88400; font-weight: bold; }
.seuil-critique   { color: #c9161d; font-weight: bold; }

/* ── Boutons DT Export ── */
.dt-buttons { margin-bottom: 6px !important; }
.dt-button {
  background: #008DC9 !important;
  color: white !important;
  border: none !important;
  border-radius: 4px !important;
  padding: 4px 10px !important;
  font-size: 11px !important;
  font-weight: 600 !important;
  margin-right: 3px !important;
  cursor: pointer !important;
  box-shadow: 0 1px 3px rgba(0,0,0,0.15) !important;
}
.dt-button:hover {
  background: #006fa3 !important;
  color: white !important;
}
.dt-button.buttons-excel  { background: #1d7344 !important; }
.dt-button.buttons-excel:hover  { background: #155733 !important; }
.dt-button.buttons-pdf    { background: #c9161d !important; }
.dt-button.buttons-pdf:hover    { background: #a01015 !important; }
.dt-button.buttons-print  { background: #6c757d !important; }
.dt-button.buttons-print:hover  { background: #545b62 !important; }
.dt-button.buttons-copy   { background: #7b2d8b !important; }
.dt-button.buttons-copy:hover   { background: #5e2269 !important; }

/* ── Sidebar N counter ── */
#sidebar_n { color: #cce8f4 !important; font-size: 11px !important; font-weight: 600; }

/* ── Download buttons OMS ── */
.btn-dl-oms {
  background: #008DC9; color: white; border: none; border-radius: 4px;
  padding: 5px 12px; font-size: 11px; font-weight: 600; cursor: pointer;
  margin-right: 4px; margin-bottom: 4px; display: inline-block;
}
.btn-dl-oms:hover { background: #006fa3; color: white; text-decoration: none; }
.btn-dl-oms.excel { background: #1d7344; }
.btn-dl-oms.excel:hover { background: #155733; }
.btn-dl-oms.word  { background: #1e5ba8; }
.btn-dl-oms.word:hover  { background: #174890; }
"

# ══════════════════════════════════════════════════════════════════════════════
# 4b. HELPER UI — BOUTONS DE TÉLÉCHARGEMENT GRAPHIQUES (Plotly.downloadImage)
# ══════════════════════════════════════════════════════════════════════════════
dl_plotly <- function(plot_id, filename, format = "png",
                      label = NULL, color = "#008DC9") {
  lbl <- if (!is.null(label)) label else
    switch(format, "png" = "📷 PNG", "svg" = "🖼️ SVG", "jpeg" = "📷 JPEG", "PNG")
  tags$button(
    style = paste0(
      "background:", color, "; color:white; border:none; border-radius:4px; ",
      "padding:5px 11px; font-size:11px; font-weight:600; cursor:pointer; ",
      "margin-right:4px; margin-bottom:4px; box-shadow:0 1px 3px rgba(0,0,0,.15);"
    ),
    onclick = paste0(
      "Plotly.downloadImage(document.getElementById('", plot_id, "'), ",
      "{format:'", format, "', filename:'", filename,
      "', height:700, width:1200, scale:2});"
    ),
    lbl
  )
}

# Barre de boutons standard PNG + SVG pour un graphique Plotly
dl_bar <- function(plot_id, filename) {
  tags$div(
    style = "margin-bottom:8px; padding:4px 0;",
    dl_plotly(plot_id, filename, "png", "📷 PNG haute-résolution"),
    dl_plotly(plot_id, filename, "svg", "🖼️ SVG vectoriel", "#6c757d")
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "blue",

  # ── Header ────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/World_Health_Organization_Logo.svg/100px-World_Health_Organization_Logo.svg.png",
               height="28px", style="margin-right:8px;"),
      "Malnutrition — Cameroun"
    ),
    titleWidth = 280
  ),

  # ── Sidebar ───────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 260,
    tags$style(css_oms),

    # Filtres globaux
    tags$div(
      style = "padding: 12px 15px 5px; color: #cce8f4; font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 1px;",
      "Filtres globaux"
    ),
    tags$div(
      style = "padding: 0 12px 10px;",

      # ── Géographie ──────────────────────────────────────────────────────────
      tags$p(style="color:#7ab8d4;font-size:10px;font-weight:700;margin:6px 0 2px;letter-spacing:.5px;",
             "▸ GÉOGRAPHIE"),
      selectInput("f_region", "Région :",
                  choices = c("Toutes", levels(df$region)),
                  selected = "Toutes", width = "100%"),
      selectInput("f_dept", "Département :",
                  choices = c("Tous", levels(df$department)),
                  selected = "Tous", width = "100%"),

      # ── Population ──────────────────────────────────────────────────────────
      tags$p(style="color:#7ab8d4;font-size:10px;font-weight:700;margin:6px 0 2px;letter-spacing:.5px;",
             "▸ POPULATION"),
      selectInput("f_sexe", "Sexe :",
                  choices = c("Tous", "Male", "Female"),
                  selected = "Tous", width = "100%"),
      sliderInput("f_age", "Âge (mois) :",
                  min = 0, max = 60, value = c(0, 60), step = 1,
                  width = "100%"),
      selectInput("f_age_group", "Groupe d'âge OMS :",
                  choices = c("Tous", "0-5m","6-11m","12-17m","18-23m",
                              "24-35m","36-47m","48-59m","60+m"),
                  selected = "Tous", width = "100%"),
      selectInput("f_depl", "Statut déplacement :",
                  choices = c("Tous", levels(df$displacement_status)),
                  selected = "Tous", width = "100%"),

      # ── Enquête & Clinique ───────────────────────────────────────────────────
      tags$p(style="color:#7ab8d4;font-size:10px;font-weight:700;margin:6px 0 2px;letter-spacing:.5px;",
             "▸ ENQUÊTE & CLINIQUE"),
      selectInput("f_round", "Survey round :",
                  choices = c("Tous", sort(unique(as.character(df$survey_round)))),
                  selected = "Tous", width = "100%"),
      selectInput("f_nutr", "Statut nutritionnel :",
                  choices = c("Tous", levels(df$nutr_cat)),
                  selected = "Tous", width = "100%"),
      selectInput("f_oedema", "Œdèmes :",
                  choices = c("Tous", "Yes", "No"),
                  selected = "Tous", width = "100%"),

      # ── Sécurité alimentaire ─────────────────────────────────────────────────
      tags$p(style="color:#7ab8d4;font-size:10px;font-weight:700;margin:6px 0 2px;letter-spacing:.5px;",
             "▸ SÉCURITÉ ALIMENTAIRE"),
      selectInput("f_fcs", "Catégorie FCS :",
                  choices = c("Tous", levels(df$fcs_cat)),
                  selected = "Tous", width = "100%"),
      selectInput("f_hhs", "Faim ménage (HHS) :",
                  choices = c("Tous", levels(df$hhs_cat)),
                  selected = "Tous", width = "100%"),
      selectInput("f_vulner", "Vulnérabilité (ICV) :",
                  choices = c("Tous", levels(df$cat_vulner)),
                  selected = "Tous", width = "100%"),

      # ── Chocs & contexte ─────────────────────────────────────────────────────
      tags$p(style="color:#7ab8d4;font-size:10px;font-weight:700;margin:6px 0 2px;letter-spacing:.5px;",
             "▸ CHOCS CONTEXTUELS"),
      selectInput("f_conflict", "Exposition conflit :",
                  choices = c("Tous", levels(df$conflict_exposure)),
                  selected = "Tous", width = "100%"),
      selectInput("f_climate", "Choc climatique :",
                  choices = c("Tous", levels(df$climate_shock)),
                  selected = "Tous", width = "100%"),

      # ── Actions ──────────────────────────────────────────────────────────────
      tags$div(
        style = "margin-top:10px;",
        actionButton("reset_filters", "🔄 Réinitialiser tous les filtres",
                     style = "width:100%; background:#1a6a99; color:white; border:none;
                              border-radius:4px; padding:7px; font-size:11px; cursor:pointer;
                              font-weight:600;")
      ),
      tags$div(
        id = "sidebar_n_box",
        style = "margin-top:8px; padding:7px 8px; background:rgba(0,141,201,0.18);
                 border-radius:5px; text-align:center; border:1px solid #1a6a99;",
        textOutput("sidebar_n")
      )
    ),
    tags$hr(style = "border-color: #1a6a99; margin: 0;"),

    # Menu
    sidebarMenu(
      id = "menu",
      menuItem("🏠  Accueil & KPI",       tabName = "accueil",   icon = icon("home")),
      menuItem("🔍  Exploration",          tabName = "explor",    icon = icon("table")),
      menuItem("📊  Statistiques",         tabName = "stats",     icon = icon("chart-bar"),
        menuSubItem("Univariée",    tabName = "uni",   icon = icon("chart-simple")),
        menuSubItem("Bivariée",     tabName = "biv",   icon = icon("chart-line")),
        menuSubItem("Descriptive OMS", tabName = "descoms", icon = icon("stethoscope"))
      ),
      menuItem("🧪  Analyse inférentielle",tabName = "infer",     icon = icon("flask")),
      menuItem("🌍  Cartographie",         tabName = "carto",     icon = icon("map")),
      menuItem("📋  Indicateurs OMS",      tabName = "oms",       icon = icon("heart-pulse"))
    ),

    tags$div(
      style = "position: absolute; bottom: 15px; left: 0; right: 0; padding: 0 15px; font-size: 10px; color: #5a9ec4; text-align: center;",
      tags$hr(style="border-color:#1a6a99;"),
      "Données : Enquête nutritionnelle", tags$br(),
      "Cameroun 2024 — Style OMS/UNICEF"
    )
  ),

  # ── Body ─────────────────────────────────────────────────────────────────
  dashboardBody(
    tags$style(css_oms),
    useShinyjs(),

    tabItems(

      # ════════════════════════════════════════════════════
      # ONGLET 1 — ACCUEIL & KPI
      # ════════════════════════════════════════════════════
      tabItem(tabName = "accueil",
        # Bandeau titre
        tags$div(
          style = paste0("background: linear-gradient(135deg, #00426A, #008DC9);",
                         "color: white; padding: 20px 25px; border-radius: 10px;",
                         "margin-bottom: 20px;"),
          tags$h2(style="margin:0; font-weight:700;",
                  "Tableau de bord nutritionnel — Cameroun"),
          tags$p(style="margin:6px 0 0; font-size:14px; opacity:0.9;",
                 "Enquête nutritionnelle | Régions : Adamaoua · Nord · Extrême-Nord"),
          tags$p(style="margin:4px 0 0; font-size:12px; opacity:0.75;",
                 "Source : Enquête SMART 2024 | Normes OMS 2006")
        ),

        # KPI row 1
        fluidRow(
          valueBoxOutput("kpi_n",       width = 2),
          valueBoxOutput("kpi_gam",     width = 2),
          valueBoxOutput("kpi_sam",     width = 2),
          valueBoxOutput("kpi_mam",     width = 2),
          valueBoxOutput("kpi_stunt",   width = 2),
          valueBoxOutput("kpi_under",   width = 2)
        ),
        # KPI row 2
        fluidRow(
          valueBoxOutput("kpi_muac",    width = 2),
          valueBoxOutput("kpi_muac_sam",width = 2),
          valueBoxOutput("kpi_fcs",     width = 2),
          valueBoxOutput("kpi_icv",     width = 2),
          valueBoxOutput("kpi_whz",     width = 2),
          valueBoxOutput("kpi_haz",     width = 2)
        ),

        fluidRow(
          # Graphique prévalences par région
          box(title = "Prévalences GAM/SAM/Stunting par région (IC 95%)",
              status = "primary", solidHeader = TRUE, width = 7,
              plotlyOutput("accueil_prev", height = "320px")),
          # Tableau seuils OMS
          box(title = "Référence seuils OMS",
              status = "success", solidHeader = TRUE, width = 5,
              tags$table(
                style="width:100%; font-size:13px; border-collapse:collapse;",
                tags$thead(tags$tr(
                  tags$th(style="padding:6px; background:#008DC9; color:white;","Niveau"),
                  tags$th(style="padding:6px; background:#008DC9; color:white;","GAM (%)"),
                  tags$th(style="padding:6px; background:#008DC9; color:white;","Sévérité")
                )),
                tags$tbody(
                  tags$tr(style="background:#e8f5e9;",
                    tags$td(style="padding:6px;","🟢 Acceptable"),
                    tags$td(style="padding:6px; font-weight:bold; color:#4dac26;","< 5 %"),
                    tags$td(style="padding:6px;","Situation normale")),
                  tags$tr(style="background:#fffde7;",
                    tags$td(style="padding:6px;","🟡 Alerte"),
                    tags$td(style="padding:6px; font-weight:bold; color:#f9c734;","5 – 9,9 %"),
                    tags$td(style="padding:6px;","Surveillance renforcée")),
                  tags$tr(style="background:#fff3e0;",
                    tags$td(style="padding:6px;","🟠 Sérieux"),
                    tags$td(style="padding:6px; font-weight:bold; color:#e88400;","10 – 14,9 %"),
                    tags$td(style="padding:6px;","Intervention requise")),
                  tags$tr(style="background:#ffebee;",
                    tags$td(style="padding:6px;","🔴 Critique"),
                    tags$td(style="padding:6px; font-weight:bold; color:#c9161d;","≥ 15 %"),
                    tags$td(style="padding:6px;","Urgence humanitaire")),
                  tags$tr(style="background:#880000; color:white;",
                    tags$td(style="padding:6px;","🟥 Urgence max"),
                    tags$td(style="padding:6px; font-weight:bold;","≥ 30 %"),
                    tags$td(style="padding:6px;","Crise alimentaire grave"))
                )
              )
          )
        ),

        fluidRow(
          box(title = "Distribution des scores Z (WHZ · HAZ · WAZ) vs référence OMS N(0,1)",
              status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("accueil_zscore", height = "280px")),
          box(title = "Statut nutritionnel MUAC",
              status = "warning", solidHeader = TRUE, width = 4,
              plotlyOutput("accueil_muac_pie", height = "280px"))
        )
      ),

      # ════════════════════════════════════════════════════
      # ONGLET 2 — EXPLORATION
      # ════════════════════════════════════════════════════
      tabItem(tabName = "explor",
        fluidRow(
          box(title = "Aperçu du jeu de données",
              status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(3, selectInput("explor_cols", "Colonnes à afficher :",
                  choices = names(df), selected = c("region","department","child_sex",
                    "child_age_months","whz_score","haz_score","muac_mm","GAM","nutr_cat"),
                  multiple = TRUE, width = "100%")),
                column(2, numericInput("explor_n", "Lignes par page :", 15, 5, 100, 5)),
                column(4,
                  tags$div(style = "margin-top:22px;",
                    downloadButton("dl_data",  "📥 CSV",
                      style = "background:#008DC9; color:white; border:none; border-radius:4px;
                               font-size:11px; font-weight:600; padding:6px 10px; margin-right:4px;"),
                    downloadButton("dl_excel", "📊 Excel",
                      style = "background:#1d7344; color:white; border:none; border-radius:4px;
                               font-size:11px; font-weight:600; padding:6px 10px; margin-right:4px;"),
                    downloadButton("dl_excel_multi", "📋 Excel multi-feuilles",
                      style = "background:#1e5ba8; color:white; border:none; border-radius:4px;
                               font-size:11px; font-weight:600; padding:6px 10px;")
                  )
                )
              ),
              DTOutput("explor_table")
          )
        ),
        fluidRow(
          box(title = "Résumé rapide de la variable sélectionnée",
              status = "info", solidHeader = TRUE, width = 6,
              selectInput("explor_var", "Variable :", choices = vars_num,
                          selected = "whz_score", width = "100%"),
              tableOutput("explor_summary")),
          box(title = "Distribution rapide",
              status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("explor_dist", height = "250px"))
        )
      ),

      # ════════════════════════════════════════════════════
      # ONGLET 3a — STATISTIQUES UNIVARIÉES
      # ════════════════════════════════════════════════════
      tabItem(tabName = "uni",
        fluidRow(
          box(width = 3, status = "primary", solidHeader = TRUE,
              title = "Paramètres",
              selectInput("uni_type", "Type de graphique :",
                choices = c(
                  "Barplot (qual.)",
                  "Camembert / Donut",
                  "Histogramme + Gauss",
                  "Boxplot annoté",
                  "Violin + Boxplot",
                  "Courbe de densité",
                  "QQ-plot (normalité)",
                  "Densité vs OMS N(0,1)"
                ), width = "100%"),
              uiOutput("uni_var_ui"),
              selectInput("uni_palette", "Palette :",
                choices = c("OMS vert/rouge","UNICEF bleu","Viridis","Spectral"),
                width = "100%"),
              checkboxInput("uni_labels", "Afficher les effectifs", TRUE),
              checkboxInput("uni_ggiraph", "Version ggiraph (interactif+)", FALSE),
              hr(),
              h5("📐 Statistiques", style="color:#008DC9; font-weight:700;"),
              tableOutput("uni_stats")
          ),
          box(width = 9, status = "primary", solidHeader = TRUE,
              title = "Graphique",
              dl_bar("uni_plot", "statistique_univariee"),
              plotlyOutput("uni_plot", height = "480px")
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # ONGLET 3b — STATISTIQUES BIVARIÉES
      # ════════════════════════════════════════════════════
      tabItem(tabName = "biv",
        fluidRow(
          box(width = 3, status = "primary", solidHeader = TRUE,
              title = "Paramètres bivariés",
              selectInput("biv_type", "Type :",
                choices = c(
                  "Barplot groupé (dodge)",
                  "Barplot empilé (%)",
                  "Boxplot par groupe",
                  "Violin par groupe",
                  "Nuage de points + LOESS",
                  "Densités superposées",
                  "Histogrammes facettes",
                  "Heatmap de fréquences"
                ), width = "100%"),
              selectInput("biv_x", "Variable X / Groupe :",
                          choices = vars_cat, selected = "region", width = "100%"),
              uiOutput("biv_y_ui"),
              selectInput("biv_pal", "Palette :",
                choices = c("OMS régions","UNICEF bleu","Viridis","Set2"),
                width = "100%"),
              hr(),
              h5("📐 Test statistique", style="color:#008DC9; font-weight:700;"),
              verbatimTextOutput("biv_test")
          ),
          box(width = 9, status = "primary", solidHeader = TRUE,
              title = "Graphique bivarié",
              dl_bar("biv_plot", "statistique_bivariee"),
              plotlyOutput("biv_plot", height = "480px")
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # ONGLET 3c — DESCRIPTIVE OMS
      # ════════════════════════════════════════════════════
      tabItem(tabName = "descoms",
        fluidRow(
          box(title = "Tableau descriptif complet — Style OMS (gtsummary)",
              status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4,
                  selectInput("desc_by", "Stratifier par :",
                    choices = c("Aucune stratification" = "none",
                                "Région" = "region",
                                "Sexe" = "child_sex",
                                "Statut déplacement" = "displacement_status"),
                    width = "100%")
                ),
                column(8,
                  tags$div(style = "margin-top:25px;",
                    downloadButton("dl_gt_html",  "🌐 Télécharger HTML",
                      style = "background:#008DC9; color:white; border:none; border-radius:4px;
                               font-size:11px; font-weight:600; padding:6px 12px; margin-right:4px;"),
                    downloadButton("dl_gt_csv",   "📥 Télécharger CSV",
                      style = "background:#1d7344; color:white; border:none; border-radius:4px;
                               font-size:11px; font-weight:600; padding:6px 12px; margin-right:4px;"),
                    downloadButton("dl_gt_excel", "📊 Télécharger Excel",
                      style = "background:#1e5ba8; color:white; border:none; border-radius:4px;
                               font-size:11px; font-weight:600; padding:6px 12px;"),
                    tags$span(style="font-size:10px;color:#888;margin-left:8px;",
                              "💡 Pour Word : installer le package flextable")
                  )
                )
              ),
              uiOutput("desc_gt_out")
          )
        ),
        fluidRow(
          box(title = "Matrice de corrélation Spearman",
              status = "info", solidHeader = TRUE, width = 6,
              downloadButton("dl_corrplot", "⬇ Télécharger PNG",
                style = "margin-bottom:8px; background:#008DC9; color:white;
                         border:none; border-radius:4px; font-size:11px; padding:5px 10px;"),
              plotOutput("desc_corr", height = "420px")),
          box(title = "Scores Z vs référence OMS par région",
              status = "warning", solidHeader = TRUE, width = 6,
              dl_bar("desc_zscore", "scores_z_region"),
              plotlyOutput("desc_zscore", height = "420px"))
        )
      ),

      # ════════════════════════════════════════════════════
      # ONGLET 4 — ANALYSE INFÉRENTIELLE
      # ════════════════════════════════════════════════════
      tabItem(tabName = "infer",
        fluidRow(
          tabBox(width = 12, title = "Tests statistiques",
            tabPanel("Chi² / Fisher",
              fluidRow(
                column(4,
                  selectInput("chi2_outcome", "Variable outcome :",
                    choices = c("GAM","SAM","Stunting","Underweight","MUAC_GAM"),
                    selected = "GAM"),
                  checkboxGroupInput("chi2_vars", "Variables à tester :",
                    choices = c("child_sex","displacement_status","head_gender",
                      "head_education","water_source","sanitation_type","shelter_type",
                      "diarrhea_last_2w","fever_last_2w","malaria_test",
                      "vaccination_status","food_aid_received","climate_shock",
                      "conflict_exposure","crop_failure","livestock_loss","price_shock"),
                    selected = c("child_sex","displacement_status","vaccination_status",
                                 "diarrhea_last_2w","water_source"))
                ),
                column(8, DTOutput("chi2_table"))
              )
            ),

            tabPanel("Mann-Whitney",
              fluidRow(
                column(4,
                  selectInput("mw_outcome", "Groupe (outcome binaire) :",
                    choices = c("GAM","SAM","Stunting"),
                    selected = "GAM"),
                  checkboxGroupInput("mw_vars", "Variables continues :",
                    choices = c("child_age_months","food_consumption_score",
                      "household_hunger_scale","muac_mm","mother_muac",
                      "anc_visits","health_facility_distance_km",
                      "household_size","coping_strategy_index","ICV"),
                    selected = c("muac_mm","food_consumption_score",
                                 "household_hunger_scale","ICV"))
                ),
                column(8,
                  DTOutput("mw_table"),
                  br(),
                  dl_bar("mw_plot", "mann_whitney_boxplot"),
                  plotlyOutput("mw_plot", height = "280px")
                )
              )
            ),

            tabPanel("Kruskal-Wallis",
              fluidRow(
                column(4,
                  selectInput("kw_var", "Variable continue :",
                    choices = c("whz_score","haz_score","waz_score",
                                "muac_mm","food_consumption_score","ICV"),
                    selected = "whz_score"),
                  selectInput("kw_group", "Variable de groupe :",
                    choices = c("region","child_sex","displacement_status",
                                "fcs_cat","hhs_cat","age_group"),
                    selected = "region"),
                  verbatimTextOutput("kw_result"),
                  h5("Post-hoc (BH) :", style="color:#008DC9;"),
                  verbatimTextOutput("kw_posthoc"),
                  hr(),
                  downloadButton("dl_kw_results", "📄 Résultats TXT",
                    style = "background:#6c757d; color:white; border:none; border-radius:4px;
                             padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                             width:100%;")
                ),
                column(8,
                  dl_bar("kw_plot", "kruskal_wallis_boxplot"),
                  plotlyOutput("kw_plot", height = "400px")
                )
              )
            ),

            tabPanel("Corrélation Spearman",
              fluidRow(
                column(4,
                  selectInput("cor_x", "Variable X :",
                    choices = vars_num, selected = "whz_score"),
                  selectInput("cor_y", "Variable Y :",
                    choices = vars_num, selected = "muac_mm"),
                  selectInput("cor_color", "Couleur par :",
                    choices = c("Aucune", vars_cat), selected = "region"),
                  verbatimTextOutput("cor_result"),
                  hr(),
                  downloadButton("dl_cor_data", "📥 Données CSV",
                    style = "background:#1d7344; color:white; border:none; border-radius:4px;
                             padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                             width:100%; margin-bottom:4px;")
                ),
                column(8,
                  dl_bar("cor_plot", "correlation_spearman"),
                  plotlyOutput("cor_plot", height = "400px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # ONGLET 5 — CARTOGRAPHIE
      # ════════════════════════════════════════════════════
      tabItem(tabName = "carto",
        fluidRow(
          tabBox(width = 12, title = "Cartographie nutritionnelle",

            tabPanel("🗺️ Carte du Cameroun (Leaflet)",
              fluidRow(
                column(3,
                  selectInput("leaf_var", "Indicateur :",
                    choices = c("GAM (%)"="GAM_pct","SAM (%)"="SAM_pct",
                                "Stunting (%)"="Stunting_pct",
                                "MUAC moyen (mm)"="muac_moy",
                                "ICV moyen"="icv_moy"),
                    selected = "GAM_pct"),
                  selectInput("leaf_niv", "Niveau :",
                    choices = c("Région","Département"),
                    selected = "Région"),
                  selectInput("leaf_fond", "Fond de carte :",
                    choices = c("CartoDB.Positron","OpenStreetMap",
                                "Esri.WorldImagery","CartoDB.DarkMatter"),
                    selected = "CartoDB.Positron"),
                  checkboxInput("leaf_pts", "Afficher les points GPS", TRUE),
                  checkboxInput("leaf_gadm", "Polygones GADM", FALSE),
                  hr(),
                  tags$strong(style="color:#333;font-size:11px;","⬇ Télécharger les données :"),
                  br(), br(),
                  downloadButton("dl_leaf_excel", "📊 Excel indicateurs",
                    style="background:#1d7344; color:white; border:none; border-radius:4px;
                           padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                           width:100%; margin-bottom:4px;"),
                  downloadButton("dl_leaf_csv", "📥 CSV coordonnées",
                    style="background:#008DC9; color:white; border:none; border-radius:4px;
                           padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                           width:100%;")
                ),
                column(9, leafletOutput("leaf_map", height = "520px"))
              )
            ),

            tabPanel("📊 Carte statique (ggplot2)",
              fluidRow(
                column(3,
                  selectInput("gg_var", "Indicateur :",
                    choices = c("GAM (%)"="GAM_pct","SAM (%)"="SAM_pct",
                                "Stunting (%)"="Stunting_pct"),
                    selected = "GAM_pct"),
                  selectInput("gg_pal", "Palette :",
                    choices = c("OMS standard","YlOrRd","Blues","Viridis"),
                    selected = "OMS standard"),
                  radioButtons("gg_type", "Type de carte :",
                    choices = c("Choroplèthe","Points proportionnels"),
                    selected = "Points proportionnels")
                ),
                column(9,
                  dl_bar("gg_map", "carte_cameroun_statique"),
                  plotlyOutput("gg_map", height = "520px")
                )
              )
            ),

            tabPanel("🌍 Carte du monde (contexte régional)",
              fluidRow(
                column(3,
                  tags$p(style="font-size:13px; color:#555;",
                    "Position du Cameroun dans le contexte régional africain.
                     Les couleurs représentent la situation nutritionnelle relative."),
                  selectInput("world_highlight", "Mettre en évidence :",
                    choices = c("Cameroun seul","Afrique centrale","Afrique subsaharienne"),
                    selected = "Afrique centrale")
                ),
                column(9,
                  dl_bar("world_map", "carte_monde_contexte"),
                  plotlyOutput("world_map", height = "520px")
                )
              )
            ),

            tabPanel("📍 Points GPS détaillés",
              fluidRow(
                column(3,
                  selectInput("gps_color", "Couleur par :",
                    choices = c("Statut nutritionnel"="nutr_cat",
                                "Région"="region","Sexe"="child_sex"),
                    selected = "nutr_cat"),
                  checkboxInput("gps_cluster", "Regrouper (cluster)", TRUE),
                  sliderInput("gps_size", "Taille des points :", 3, 10, 5, 1),
                  hr(),
                  tags$strong(style="color:#333;font-size:11px;","⬇ Télécharger les données GPS :"),
                  br(), br(),
                  downloadButton("dl_gps_excel", "📊 Excel individus",
                    style="background:#1d7344; color:white; border:none; border-radius:4px;
                           padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                           width:100%; margin-bottom:4px;"),
                  downloadButton("dl_gps_csv", "📥 CSV GPS",
                    style="background:#008DC9; color:white; border:none; border-radius:4px;
                           padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                           width:100%;")
                ),
                column(9, leafletOutput("gps_map", height = "520px"))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # ONGLET 6 — INDICATEURS OMS
      # ════════════════════════════════════════════════════
      tabItem(tabName = "oms",
        fluidRow(
          box(title = "Prévalences avec IC 95% (Méthode Wilson) — Normes OMS 2006",
              status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(3,
                  checkboxGroupInput("oms_groupes", "Stratifier par :",
                    choices = c("Global","Région","Sexe","Groupe d'âge",
                                "Statut déplacement","Sécurité alim."),
                    selected = c("Global","Région")),
                  hr(),
                  tags$strong(style="color:#333;font-size:11px;","⬇ Export complet :"),
                  br(), br(),
                  downloadButton("dl_oms_excel_full", "📊 Excel multi-strates",
                    style="background:#1d7344; color:white; border:none; border-radius:4px;
                           padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                           width:100%; margin-bottom:4px;"),
                  downloadButton("dl_oms_csv_full", "📥 CSV complet",
                    style="background:#008DC9; color:white; border:none; border-radius:4px;
                           padding:5px 10px; font-size:11px; font-weight:600; cursor:pointer;
                           width:100%;")
                ),
                column(9, DTOutput("oms_table"))
              )
          )
        ),
        fluidRow(
          box(title = "Forest plot — Prévalences et IC 95% par région",
              status = "info", solidHeader = TRUE, width = 8,
              dl_bar("oms_forest", "oms_forest_plot_ic95"),
              plotlyOutput("oms_forest", height = "420px")),
          box(title = "Comparaison aux seuils OMS",
              status = "warning", solidHeader = TRUE, width = 4,
              dl_plotly("oms_gauge", "oms_jauge_gam", "png",
                        "📷 PNG jauge", "#008DC9"),
              plotlyOutput("oms_gauge", height = "420px"))
        ),
        fluidRow(
          box(title = "Évolution des indicateurs par survey round",
              status = "primary", solidHeader = TRUE, width = 12,
              dl_bar("oms_trend", "oms_evolution_survey_rounds"),
              plotlyOutput("oms_trend", height = "300px"))
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# 6. SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Données filtrées ────────────────────────────────────────────────────────
  df_f <- reactive({
    d <- df
    if (input$f_region   != "Toutes") d <- filter(d, region              == input$f_region)
    if (input$f_dept     != "Tous")   d <- filter(d, department          == input$f_dept)
    if (input$f_sexe     != "Tous")   d <- filter(d, child_sex           == input$f_sexe)
    if (input$f_depl     != "Tous")   d <- filter(d, displacement_status == input$f_depl)
    if (input$f_round    != "Tous")   d <- filter(d, as.character(survey_round) == input$f_round)
    if (input$f_nutr     != "Tous")   d <- filter(d, nutr_cat            == input$f_nutr)
    if (input$f_oedema   != "Tous")   d <- filter(d, oedema              == input$f_oedema)
    if (input$f_fcs      != "Tous")   d <- filter(d, fcs_cat             == input$f_fcs)
    if (input$f_hhs      != "Tous")   d <- filter(d, hhs_cat             == input$f_hhs)
    if (input$f_vulner   != "Tous")   d <- filter(d, cat_vulner          == input$f_vulner)
    if (input$f_conflict != "Tous")   d <- filter(d, conflict_exposure   == input$f_conflict)
    if (input$f_climate  != "Tous")   d <- filter(d, climate_shock       == input$f_climate)
    if (input$f_age_group != "Tous")  d <- filter(d, as.character(age_group) == input$f_age_group)
    filter(d, child_age_months >= input$f_age[1],
              child_age_months <= input$f_age[2])
  })

  # ── Département réactif au choix de région ──────────────────────────────────
  observe({
    dept_choices <- if (input$f_region == "Toutes") {
      c("Tous", levels(df$department))
    } else {
      depts <- df |> filter(region == input$f_region) |>
        pull(department) |> droplevels() |> levels()
      c("Tous", depts)
    }
    updateSelectInput(session, "f_dept", choices = dept_choices, selected = "Tous")
  })

  # ── Réinitialisation de tous les filtres ─────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "f_region",    selected = "Toutes")
    updateSelectInput(session, "f_dept",      choices = c("Tous", levels(df$department)), selected = "Tous")
    updateSelectInput(session, "f_sexe",      selected = "Tous")
    updateSelectInput(session, "f_depl",      selected = "Tous")
    updateSelectInput(session, "f_round",     selected = "Tous")
    updateSelectInput(session, "f_nutr",      selected = "Tous")
    updateSelectInput(session, "f_oedema",    selected = "Tous")
    updateSelectInput(session, "f_fcs",       selected = "Tous")
    updateSelectInput(session, "f_hhs",       selected = "Tous")
    updateSelectInput(session, "f_vulner",    selected = "Tous")
    updateSelectInput(session, "f_conflict",  selected = "Tous")
    updateSelectInput(session, "f_climate",   selected = "Tous")
    updateSelectInput(session, "f_age_group", selected = "Tous")
    updateSliderInput(session, "f_age",       value = c(0, 60))
  })

  # ── Compteur N dans la sidebar ───────────────────────────────────────────────
  output$sidebar_n <- renderText({
    n_sel <- nrow(df_f())
    n_tot <- nrow(df)
    pct   <- round(100 * n_sel / n_tot, 1)
    paste0("📊 ", format(n_sel, big.mark = "\u202f"), " / ",
           format(n_tot, big.mark = "\u202f"), " enfants (", pct, "%)")
  })

  # ── Palettes helper ─────────────────────────────────────────────────────────
  get_pal_uni <- function(n, nom) {
    switch(nom,
      "OMS vert/rouge"  = colorRampPalette(c(oms_vert, oms_bleu, oms_rouge))(n),
      "UNICEF bleu"     = colorRampPalette(c("#e0f4ff", unicef_bleu, "#003d6b"))(n),
      "Viridis"         = viridis(n),
      "Spectral"        = brewer.pal(min(n,11), "Spectral")
    )
  }

  get_pal_biv <- function(n, nom) {
    switch(nom,
      "OMS régions"    = unname(couleurs_region)[1:min(n,3)],
      "UNICEF bleu"    = colorRampPalette(c("#e0f4ff", unicef_bleu, "#003d6b"))(n),
      "Viridis"        = viridis(n),
      "Set2"           = brewer.pal(min(n,8), "Set2")
    )
  }

  # ── Téléchargement PNG/SVG haute résolution pour tous les graphiques ─────────
  cfg_dl <- function(p, nom = "graphique_oms") {
    p |> plotly::config(
      toImageButtonOptions = list(
        format   = "svg",
        filename = nom,
        height   = 700,
        width    = 1200,
        scale    = 2
      ),
      displaylogo        = FALSE,
      locale             = "fr",
      modeBarButtonsToRemove = list("lasso2d", "select2d")
    )
  }

  # ════════════════════════════════════════════════════
  # ONGLET 1 — KPI & ACCUEIL
  # ════════════════════════════════════════════════════
  kpi_color_gam <- reactive({
    p <- mean(df_f()$GAM, na.rm=T)*100
    if(p >= 30) "black" else if(p >= 15) "red" else if(p >= 10) "orange" else if(p >= 5) "yellow" else "green"
  })

  output$kpi_n <- renderValueBox(
    valueBox(format(nrow(df_f()), big.mark=" "), "Enfants enquêtés",
             icon=icon("users"), color="blue")
  )
  output$kpi_gam <- renderValueBox({
    p <- round(mean(df_f()$GAM, na.rm=T)*100, 1)
    valueBox(paste0(p, "%"), "GAM (WHZ<-2 ou œdème)",
             icon=icon("triangle-exclamation"),
             color=if(p>=15)"red" else if(p>=10)"orange" else "green")
  })
  output$kpi_sam <- renderValueBox({
    p <- round(mean(df_f()$SAM, na.rm=T)*100, 1)
    valueBox(paste0(p, "%"), "SAM (WHZ<-3 ou œdème)",
             icon=icon("heart-pulse"), color="red")
  })
  output$kpi_mam <- renderValueBox({
    p <- round(mean(df_f()$MAM, na.rm=T)*100, 1)
    valueBox(paste0(p, "%"), "MAM (WHZ -3 à -2)",
             icon=icon("heart"), color="orange")
  })
  output$kpi_stunt <- renderValueBox({
    p <- round(mean(df_f()$Stunting, na.rm=T)*100, 1)
    valueBox(paste0(p, "%"), "Stunting (HAZ<-2)",
             icon=icon("child"), color=if(p>=30)"red" else "orange")
  })
  output$kpi_under <- renderValueBox({
    p <- round(mean(df_f()$Underweight, na.rm=T)*100, 1)
    valueBox(paste0(p, "%"), "Insuff. pondérale (WAZ<-2)",
             icon=icon("weight-hanging"), color="yellow")
  })
  output$kpi_muac <- renderValueBox({
    m <- round(mean(df_f()$muac_mm, na.rm=T), 1)
    valueBox(paste0(m, " mm"), "MUAC moyen enfant",
             icon=icon("ruler-horizontal"), color="blue")
  })
  output$kpi_muac_sam <- renderValueBox({
    p <- round(mean(df_f()$MUAC_SAM, na.rm=T)*100, 1)
    valueBox(paste0(p, "%"), "MUAC SAM (<115mm)",
             icon=icon("circle-exclamation"), color="red")
  })
  output$kpi_fcs <- renderValueBox({
    m <- round(mean(df_f()$food_consumption_score, na.rm=T), 1)
    valueBox(m, "FCS moyen",
             icon=icon("utensils"), color="green")
  })
  output$kpi_icv <- renderValueBox({
    m <- round(mean(df_f()$ICV, na.rm=T), 1)
    valueBox(paste0(m, "/100"), "ICV (vulnérabilité)",
             icon=icon("shield-halved"),
             color=if(m<40)"red" else if(m<60)"orange" else "green")
  })
  output$kpi_whz <- renderValueBox({
    m <- round(mean(df_f()$whz_score, na.rm=T), 2)
    valueBox(m, "WHZ moyen",
             icon=icon("chart-line"),
             color=if(m < -2)"red" else if(m < -1)"orange" else "green")
  })
  output$kpi_haz <- renderValueBox({
    m <- round(mean(df_f()$haz_score, na.rm=T), 2)
    valueBox(m, "HAZ moyen",
             icon=icon("chart-line"),
             color=if(m < -2)"red" else if(m < -1)"orange" else "green")
  })

  # Graphique prévalences accueil
  output$accueil_prev <- renderPlotly({
    d <- df_f()
    inds <- c("GAM","SAM","Stunting")
    res <- d |> group_by(region) |>
      group_modify(~map_dfr(inds, function(v) calc_prev(.x, v, v))) |>
      ungroup()
    p <- ggplot(res, aes(x=region, y=`Prév. (%)`, fill=Indicateur,
      text=paste0(region," — ",Indicateur,
                  "\n",`Prév. (%)`,"% [",`IC95 bas`,"-",`IC95 haut`,"%]",
                  "\nN=",N))) +
      geom_col(position="dodge", width=0.7, alpha=0.88) +
      geom_errorbar(aes(ymin=`IC95 bas`, ymax=`IC95 haut`),
                    position=position_dodge(0.7), width=0.25, color="gray30") +
      geom_hline(yintercept=15, linetype="dashed", color=oms_rouge, linewidth=0.7) +
      scale_fill_manual(values=c(GAM=oms_rouge, SAM=oms_rouge_f, Stunting=oms_bleu)) +
      labs(x="", y="Prévalence (%)") + theme_minimal(base_size=11)
    ggplotly(p, tooltip="text") |>
      layout(legend=list(orientation="h", y=-0.2)) |>
      cfg_dl("accueil_prevalences_region")
  })

  # Scores Z accueil
  output$accueil_zscore <- renderPlotly({
    d <- df_f()
    x_ref <- seq(-4, 4, length.out=200)
    ref_df <- tibble(x=x_ref, y=dnorm(x_ref), Courbe="Référence OMS N(0,1)")
    df_long <- d |>
      select(region, whz_score, haz_score, waz_score) |>
      pivot_longer(-region, names_to="Score", values_to="Val") |>
      mutate(Score=recode(Score, whz_score="WHZ", haz_score="HAZ", waz_score="WAZ"))
    p <- ggplot(df_long, aes(x=Val, fill=Score, color=Score)) +
      geom_density(alpha=0.3, linewidth=0.8) +
      geom_line(data=ref_df, aes(x=x, y=y), color="black",
                linewidth=1, linetype="dashed", inherit.aes=FALSE) +
      geom_vline(xintercept=c(-2,-3), linetype="dashed",
                 color=c(oms_rouge, oms_rouge_f), linewidth=0.7) +
      scale_fill_manual(values=c(WHZ=oms_rouge, HAZ=oms_bleu, WAZ=oms_vert)) +
      scale_color_manual(values=c(WHZ=oms_rouge, HAZ=oms_bleu, WAZ=oms_vert)) +
      labs(x="Score Z", y="Densité",
           caption="Tirets noirs = distribution OMS de référence N(0,1)") +
      theme_minimal(base_size=11)
    ggplotly(p) |> layout(hovermode="x unified",
                          legend=list(orientation="h", y=-0.2)) |>
      cfg_dl("accueil_scores_z")
  })

  # Camembert MUAC accueil
  output$accueil_muac_pie <- renderPlotly({
    d <- df_f()
    cnt <- d |> count(muac_cat) |> mutate(pct=round(n/sum(n)*100,1))
    plot_ly(cnt, labels=~muac_cat, values=~n, type="pie", hole=0.5,
            textinfo="label+percent",
            marker=list(
              colors=c(oms_vert, oms_orange, oms_rouge),
              line=list(color="white", width=2)
            ),
            hovertemplate="%{label}<br>N=%{value}<br>%{percent}<extra></extra>") |>
      layout(showlegend=TRUE, legend=list(orientation="h", y=-0.1)) |>
      cfg_dl("accueil_muac_statut")
  })

  # ════════════════════════════════════════════════════
  # ONGLET 2 — EXPLORATION
  # ════════════════════════════════════════════════════
  output$explor_table <- renderDT({
    req(input$explor_cols)
    df_f() |>
      select(any_of(input$explor_cols)) |>
      datatable(
        rownames  = FALSE,
        filter    = "top",
        extensions = "Buttons",
        options   = list(
          pageLength = input$explor_n,
          scrollX    = TRUE,
          dom        = "Bfrtip",
          buttons    = list(
            list(extend = "csv",   text = "📥 CSV",
                 filename = paste0("donnees_nutrition_", Sys.Date()),
                 exportOptions = list(modifier = list(page = "all"))),
            list(extend = "excel", text = "📊 Excel",
                 filename = paste0("donnees_nutrition_", Sys.Date()),
                 title    = "Données nutritionnelles — Cameroun",
                 exportOptions = list(modifier = list(page = "all"))),
            list(extend = "pdf",   text = "📄 PDF",
                 filename    = paste0("donnees_nutrition_", Sys.Date()),
                 orientation = "landscape", pageSize = "A3",
                 title       = "Données nutritionnelles — Cameroun"),
            list(extend = "copy",  text = "📋 Copier"),
            list(extend = "print", text = "🖨️ Imprimer",
                 title  = "Données nutritionnelles — Enquête SMART Cameroun 2024")
          )
        ),
        class = "compact stripe hover"
      ) |>
      formatStyle(columns = character(0), fontSize = "12px")
  })

  output$explor_summary <- renderTable({
    v <- na.omit(df_f()[[input$explor_var]])
    tibble(
      Statistique=c("N","Moyenne","Médiane","Mode","Écart-type",
                    "CV (%)","Min","Max","Q1","Q3","Étendue","IQR",
                    "Asymétrie","Aplatissement"),
      Valeur=c(length(v), round(mean(v),3), round(median(v),3),
               round(mode_stat(v),3), round(sd(v),3), cv_stat(v),
               round(min(v),3), round(max(v),3),
               round(quantile(v,.25),3), round(quantile(v,.75),3),
               round(max(v)-min(v),3), round(IQR(v),3),
               round(skewness(v,na.rm=T),3), round(kurtosis(v,na.rm=T),3))
    )
  }, striped=TRUE, hover=TRUE, bordered=TRUE, digits=3)

  output$explor_dist <- renderPlotly({
    d <- df_f(); v <- input$explor_var
    m <- mean(d[[v]],na.rm=T); s <- sd(d[[v]],na.rm=T)
    p <- ggplot(d, aes(x=.data[[v]])) +
      geom_histogram(aes(y=after_stat(density)), bins=25,
                     fill=oms_bleu, color="white", alpha=0.8) +
      stat_function(fun=dnorm, args=list(mean=m,sd=s),
                    color="black", linewidth=1.1) +
      geom_vline(xintercept=m, color=oms_rouge, linewidth=1, linetype="dashed") +
      labs(x=v, y="Densité") + theme_minimal(base_size=11)
    ggplotly(p) |> cfg_dl("exploration_distribution")
  })

  output$dl_data <- downloadHandler(
    filename = function() paste0("donnees_nutrition_cameroun_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(df_f(), file)
  )

  output$dl_excel <- downloadHandler(
    filename = function() paste0("donnees_nutrition_cameroun_", Sys.Date(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(df_f() |> mutate(across(where(is.logical), as.integer)), file)
  )

  # ── Kruskal-Wallis : export résultats TXT ────────────────────────────────────
  output$dl_kw_results <- downloadHandler(
    filename = function() paste0("kruskal_wallis_", input$kw_var, "_", Sys.Date(), ".txt"),
    content  = function(file) {
      d  <- df_f()
      kw <- kruskal.test(d[[input$kw_var]] ~ d[[input$kw_group]])
      ph <- tryCatch(
        pairwise.wilcox.test(d[[input$kw_var]], d[[input$kw_group]],
                             p.adjust.method = "BH", exact = FALSE),
        error = function(e) NULL
      )
      sink(file)
      cat("═══ KRUSKAL-WALLIS — Cameroun ═══\n")
      cat("Variable :", input$kw_var, "\nGroupe   :", input$kw_group, "\n")
      cat("N =", nrow(d), "\n\n")
      cat("── Test Kruskal-Wallis ──\n")
      cat("H =", round(kw$statistic, 4), "\nddl =", kw$parameter,
          "\np-valeur =", round(kw$p.value, 6), "\n\n")
      if (!is.null(ph)) {
        cat("── Post-hoc pairwise (correction BH) ──\n")
        print(round(ph$p.value, 4))
      }
      cat("\n── Résumé par groupe ──\n")
      print(tapply(d[[input$kw_var]], d[[input$kw_group]],
                   function(x) round(c(n=length(x), med=median(x,na.rm=T),
                                       q1=quantile(x,.25,na.rm=T),
                                       q3=quantile(x,.75,na.rm=T)), 3)))
      sink()
    }
  )

  # ── Corrélation Spearman : export données CSV ─────────────────────────────
  output$dl_cor_data <- downloadHandler(
    filename = function() paste0("correlation_", input$cor_x, "_", input$cor_y,
                                 "_", Sys.Date(), ".csv"),
    content  = function(file) {
      df_f() |>
        select(region, child_sex, child_age_months,
               all_of(c(input$cor_x, input$cor_y))) |>
        write_csv(file)
    }
  )

  # ── Leaflet Cameroun : export données géographiques ──────────────────────
  output$dl_leaf_excel <- downloadHandler(
    filename = function() paste0("indicateurs_geo_cameroun_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      writexl::write_xlsx(
        list(
          "Région"     = prev_region |> mutate(across(where(is.numeric), ~round(., 2))),
          "Département" = prev_dept  |> mutate(across(where(is.numeric), ~round(., 2)))
        ),
        file
      )
    }
  )

  output$dl_leaf_csv <- downloadHandler(
    filename = function() paste0("coordonnees_gps_", Sys.Date(), ".csv"),
    content  = function(file) {
      df_f() |>
        filter(!is.na(gps_latitude), !is.na(gps_longitude)) |>
        select(household_id, region, department, village, district,
               gps_latitude, gps_longitude, nutr_cat, whz_score,
               haz_score, muac_mm, GAM, SAM, Stunting) |>
        write_csv(file)
    }
  )

  # ── GPS points : export données individuelles ─────────────────────────────
  output$dl_gps_excel <- downloadHandler(
    filename = function() paste0("donnees_gps_individus_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      df_f() |>
        filter(!is.na(gps_latitude), !is.na(gps_longitude)) |>
        mutate(across(where(is.logical), as.integer)) |>
        writexl::write_xlsx(file)
    }
  )

  output$dl_gps_csv <- downloadHandler(
    filename = function() paste0("gps_individus_", Sys.Date(), ".csv"),
    content  = function(file) {
      df_f() |>
        filter(!is.na(gps_latitude), !is.na(gps_longitude)) |>
        select(household_id, region, department, village,
               gps_latitude, gps_longitude, child_sex, child_age_months,
               nutr_cat, whz_score, haz_score, waz_score, muac_mm,
               GAM, SAM, MAM, Stunting, Underweight, ICV) |>
        write_csv(file)
    }
  )

  # ── Indicateurs OMS : export Excel multi-strates complet ─────────────────
  make_oms_table <- function(d) {
    inds <- list(
      GAM="GAM (WHZ<-2 ou œdème)", SAM="SAM (WHZ<-3 ou œdème)",
      MAM="MAM (WHZ -3 à -2)",     Stunting="Stunting (HAZ<-2)",
      Sev_Stunting="Stunting sévère (HAZ<-3)",
      Underweight="Insuff. pondérale (WAZ<-2)",
      MUAC_GAM="MUAC GAM (<125mm)", MUAC_SAM="MUAC SAM (<115mm)"
    )
    calc_strate <- function(data, strate_lbl, group_var = NULL) {
      if (is.null(group_var)) {
        map2_dfr(names(inds), inds, function(v, l) calc_prev(data, v, l)) |>
          mutate(Stratification = strate_lbl, Groupe = "Global", .before = 1)
      } else {
        data |> group_by(Groupe = .data[[group_var]]) |>
          group_modify(~map2_dfr(names(inds), inds, function(v, l) calc_prev(.x, v, l))) |>
          ungroup() |>
          mutate(Stratification = strate_lbl, Groupe = as.character(Groupe), .before = 1)
      }
    }
    bind_rows(
      calc_strate(d, "Global"),
      calc_strate(d, "Région",           "region"),
      calc_strate(d, "Sexe",             "child_sex"),
      calc_strate(d, "Groupe d'âge",     "age_group"),
      calc_strate(d, "Déplacement",      "displacement_status"),
      calc_strate(d, "Sécurité alim.",   "fcs_cat")
    )
  }

  output$dl_oms_excel_full <- downloadHandler(
    filename = function() paste0("indicateurs_OMS_complet_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      d   <- df_f()
      tbl <- make_oms_table(d)
      writexl::write_xlsx(
        list(
          "Toutes strates"      = tbl,
          "Global"              = tbl |> filter(Stratification == "Global"),
          "Par région"          = tbl |> filter(Stratification == "Région"),
          "Par sexe"            = tbl |> filter(Stratification == "Sexe"),
          "Par groupe d'âge"    = tbl |> filter(Stratification == "Groupe d'âge"),
          "Par déplacement"     = tbl |> filter(Stratification == "Déplacement"),
          "Par sécurité alim."  = tbl |> filter(Stratification == "Sécurité alim.")
        ),
        file
      )
    }
  )

  output$dl_oms_csv_full <- downloadHandler(
    filename = function() paste0("indicateurs_OMS_complet_", Sys.Date(), ".csv"),
    content  = function(file) make_oms_table(df_f()) |> write_csv(file)
  )

  output$dl_excel_multi <- downloadHandler(
    filename = function() paste0("rapport_nutrition_cameroun_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      d <- df_f()
      writexl::write_xlsx(
        list(
          "Données brutes"          = d |> mutate(across(where(is.logical), as.integer)),
          "Prévalences par région"  = prev_region,
          "Prévalences par département" = prev_dept,
          "Indicateurs globaux"     = bind_rows(
            calc_prev(d, "GAM",         "GAM (WHZ<-2 ou œdème)"),
            calc_prev(d, "SAM",         "SAM (WHZ<-3 ou œdème)"),
            calc_prev(d, "MAM",         "MAM (WHZ -3 à -2)"),
            calc_prev(d, "Stunting",    "Stunting (HAZ<-2)"),
            calc_prev(d, "Sev_Stunting","Stunting sévère (HAZ<-3)"),
            calc_prev(d, "Underweight", "Insuffisance pondérale (WAZ<-2)"),
            calc_prev(d, "MUAC_GAM",    "GAM MUAC (<125mm)"),
            calc_prev(d, "MUAC_SAM",    "SAM MUAC (<115mm)")
          )
        ),
        col_names = TRUE
      )
    }
  )

  # ════════════════════════════════════════════════════
  # ONGLET 3a — UNIVARIÉ
  # ════════════════════════════════════════════════════
  output$uni_var_ui <- renderUI({
    type <- input$uni_type
    if (grepl("Barplot|Camembert|Donut", type)) {
      ch <- if (grepl("Camembert|Donut", type)) vars_cat_pie else vars_cat
      selectInput("uni_var", "Variable :", choices=ch,
                  selected=ch[1], width="100%")
    } else {
      selectInput("uni_var", "Variable :", choices=vars_num,
                  selected="whz_score", width="100%")
    }
  })

  output$uni_stats <- renderTable({
    req(input$uni_var)
    d <- df_f(); v <- input$uni_var
    if (is.numeric(d[[v]])) {
      vals <- na.omit(d[[v]])
      sw <- tryCatch(shapiro.test(vals[1:min(5000,length(vals))]),error=function(e) NULL)
      tibble(
        Stat=c("N","Moy","Méd","Éc.-t.","CV%","Min","Max","Q1","Q3","Asymét.","Plat.","Shapiro p"),
        Val=c(length(vals), round(mean(vals),2), round(median(vals),2),
              round(sd(vals),2), cv_stat(vals), round(min(vals),2), round(max(vals),2),
              round(quantile(vals,.25),2), round(quantile(vals,.75),2),
              round(skewness(vals,na.rm=T),3), round(kurtosis(vals,na.rm=T),3),
              if(!is.null(sw)) round(sw$p.value,4) else NA)
      )
    } else {
      cnt <- d |> count(.data[[v]]) |> mutate(pct=round(n/sum(n)*100,1))
      tibble(Modalité=as.character(cnt[[v]]),
             N=cnt$n, `%`=cnt$pct)
    }
  }, striped=TRUE, bordered=TRUE, digits=3, spacing="xs")

  output$uni_plot <- renderPlotly({
    req(input$uni_var)
    d <- df_f(); v <- input$uni_var; type <- input$uni_type

    pal_fn <- function(n) get_pal_uni(n, input$uni_palette)

    # BARPLOT
    if (grepl("Barplot", type)) {
      cnt <- d |> count(.data[[v]]) |> mutate(pct=round(n/sum(n)*100,1))
      niv <- nrow(cnt)
      p <- ggplot(cnt, aes(x=fct_reorder(.data[[v]],n), y=n, fill=.data[[v]],
                           text=paste0(.data[[v]],"\nN=",n," (",pct,"%)"))) +
        geom_col(alpha=0.88, width=0.7) +
        {if(input$uni_labels) geom_text(aes(label=paste0(n,"\n(",pct,"%)")),
                                        hjust=-0.1, size=3)} +
        scale_fill_manual(values=pal_fn(niv)) +
        coord_flip() + theme_minimal(base_size=11) +
        labs(x="", y="Effectif", title=paste("Distribution de",v)) +
        theme(legend.position="none")
      ggplotly(p, tooltip="text") |> cfg_dl("uni_barplot")

    # CAMEMBERT
    } else if (grepl("Camembert|Donut", type)) {
      cnt <- d |> count(.data[[v]]) |> mutate(pct=round(n/sum(n)*100,1))
      plot_ly(cnt, labels=~.data[[v]], values=~n, type="pie", hole=0.42,
              textinfo="label+percent",
              marker=list(colors=pal_fn(nrow(cnt)),
                          line=list(color="white",width=2)),
              hovertemplate="%{label}<br>N=%{value}<br>%{percent}<extra></extra>") |>
        layout(title=paste("Distribution de",v), showlegend=TRUE,
               legend=list(orientation="h",y=-0.15)) |>
        cfg_dl("uni_camembert")

    # HISTOGRAMME
    } else if (grepl("Histogramme", type)) {
      m <- mean(d[[v]],na.rm=T); s <- sd(d[[v]],na.rm=T); md <- median(d[[v]],na.rm=T)
      p <- ggplot(d, aes(x=.data[[v]])) +
        geom_histogram(aes(y=after_stat(density)), bins=30,
                       fill=oms_bleu, color="white", alpha=0.82) +
        stat_function(fun=dnorm, args=list(mean=m,sd=s),
                      color="black", linewidth=1.1) +
        geom_vline(xintercept=m, color=oms_rouge, linewidth=1, linetype="dashed") +
        geom_vline(xintercept=md, color=oms_orange, linewidth=1, linetype="dotted") +
        labs(x=v, y="Densité",
             subtitle=paste0("μ=",round(m,2)," | σ=",round(s,2)),
             caption="Rouge=moyenne | Orange=médiane | Courbe=N(μ,σ)") +
        theme_minimal(base_size=11)
      ggplotly(p) |> layout(hovermode="x unified") |> cfg_dl("uni_histogramme")

    # BOXPLOT ANNOTÉ
    } else if (grepl("Boxplot", type)) {
      q <- quantile(d[[v]], c(.25,.5,.75), na.rm=T)
      p <- ggplot(d, aes(x="", y=.data[[v]])) +
        geom_boxplot(fill=oms_bleu, color="#004f7c", alpha=0.75,
                     outlier.shape=21, outlier.color=oms_rouge, width=0.4) +
        stat_summary(fun=mean, geom="point", shape=18, size=4, color=oms_rouge) +
        annotate("text", x=1.35, y=q[1], label=paste0("Q1=",round(q[1],2)), size=3) +
        annotate("text", x=1.35, y=q[2], label=paste0("Méd=",round(q[2],2)), size=3) +
        annotate("text", x=1.35, y=q[3], label=paste0("Q3=",round(q[3],2)), size=3) +
        labs(x="", y=v,
             subtitle=paste0("IQR=",round(q[3]-q[1],2),
                             " | Étendue=",round(max(d[[v]],na.rm=T)-min(d[[v]],na.rm=T),2))) +
        theme_minimal(base_size=11)
      ggplotly(p) |> cfg_dl("uni_boxplot")

    # VIOLIN
    } else if (grepl("Violin", type)) {
      p <- ggplot(d, aes(x="", y=.data[[v]])) +
        geom_violin(fill=oms_violet, alpha=0.5, trim=FALSE, color=oms_violet) +
        geom_boxplot(fill="white", color="#2c3e50", width=0.12,
                     outlier.shape=21, outlier.color=oms_rouge) +
        stat_summary(fun=mean, geom="point", shape=18, size=4, color=oms_rouge) +
        labs(x="", y=v) + theme_minimal(base_size=11)
      ggplotly(p) |> cfg_dl("uni_violin")

    # DENSITÉ
    } else if (grepl("densité", type)) {
      m <- mean(d[[v]], na.rm=T)
      p <- ggplot(d, aes(x=.data[[v]])) +
        geom_density(fill=oms_bleu, color=oms_bleu, alpha=0.4, linewidth=0.9) +
        geom_vline(xintercept=m, color=oms_rouge, linewidth=1, linetype="dashed") +
        geom_vline(xintercept=median(d[[v]],na.rm=T),
                   color=oms_orange, linewidth=1, linetype="dotted") +
        labs(x=v, y="Densité") + theme_minimal(base_size=11)
      ggplotly(p) |> layout(hovermode="x unified") |> cfg_dl("uni_densite")

    # QQ-PLOT
    } else if (grepl("QQ-plot", type)) {
      sw <- tryCatch(
        shapiro.test(na.omit(d[[v]])[1:min(5000,sum(!is.na(d[[v]])))]),
        error=function(e) NULL
      )
      p <- ggplot(d, aes(sample=.data[[v]])) +
        stat_qq(color=oms_bleu, alpha=0.6, size=1.5) +
        stat_qq_line(color=oms_rouge, linewidth=1) +
        labs(title=paste0("QQ-plot — ",v),
             subtitle=if(!is.null(sw))
               paste0("Shapiro-Wilk: W=",round(sw$statistic,4),
                      "  p=",round(sw$p.value,4),
                      if(sw$p.value<.05)"  → NON NORMALE" else "  → Normale")
             else "",
             x="Quantiles théoriques", y="Quantiles observés") +
        theme_minimal(base_size=11)
      ggplotly(p) |> cfg_dl("uni_qqplot")

    # DENSITÉ vs OMS
    } else {
      x_ref <- seq(min(d[[v]],na.rm=T)-1, max(d[[v]],na.rm=T)+1, length.out=300)
      ref_df <- tibble(x=x_ref, y=dnorm(x_ref,0,1))
      p <- ggplot(d, aes(x=.data[[v]])) +
        geom_density(aes(fill="Observé",color="Observé"),alpha=0.4,linewidth=0.9) +
        geom_line(data=ref_df, aes(x=x,y=y,color="OMS N(0,1)",fill="OMS N(0,1)"),
                  linewidth=1, linetype="dashed", inherit.aes=FALSE) +
        scale_fill_manual(values=c("Observé"=oms_rouge,"OMS N(0,1)"="transparent")) +
        scale_color_manual(values=c("Observé"=oms_rouge,"OMS N(0,1)"="black")) +
        labs(x=v, y="Densité",
             subtitle="Décalage gauche = prévalence élevée de malnutrition") +
        theme_minimal(base_size=11) + theme(legend.position="bottom")
      ggplotly(p) |> cfg_dl("uni_densite_vs_oms")
    }
  })

  # ════════════════════════════════════════════════════
  # ONGLET 3b — BIVARIÉ
  # ════════════════════════════════════════════════════
  output$biv_y_ui <- renderUI({
    type <- input$biv_type
    if (grepl("Barplot|Heatmap", type)) {
      selectInput("biv_y","Variable Y (catégorielle):",
                  choices=vars_cat, selected="nutr_cat", width="100%")
    } else if (grepl("Nuage", type)) {
      tagList(
        selectInput("biv_y","Variable Y (continue):",
                    choices=vars_num, selected="muac_mm", width="100%"),
        selectInput("biv_z","Couleur par:",
                    choices=c("Aucune",vars_cat), selected="region", width="100%")
      )
    } else {
      selectInput("biv_y","Variable continue:",
                  choices=vars_num, selected="whz_score", width="100%")
    }
  })

  output$biv_test <- renderPrint({
    req(input$biv_x, input$biv_y)
    d <- df_f()
    type <- input$biv_type
    tryCatch({
      if (grepl("Barplot|Heatmap", type)) {
        tbl <- table(d[[input$biv_x]], d[[input$biv_y]])
        res <- chisq.test(tbl, correct=FALSE)
        cat("Chi²:", round(res$statistic,3),"\np-value:", round(res$p.value,4),
            if(res$p.value<.05)"\n→ Association significative" else "\n→ NS")
      } else if (grepl("Nuage", type)) {
        r <- cor.test(d[[input$biv_x]], d[[input$biv_y]],
                      method="spearman", exact=FALSE)
        cat("Spearman ρ:", round(r$estimate,3),"\np-value:", round(r$p.value,4))
      } else {
        kw <- kruskal.test(d[[input$biv_y]] ~ d[[input$biv_x]])
        cat("Kruskal-Wallis H:", round(kw$statistic,3),
            "\np-value:", round(kw$p.value,4),
            if(kw$p.value<.05)"\n→ Différence significative" else "\n→ NS")
      }
    }, error=function(e) cat("Test non applicable"))
  })

  output$biv_plot <- renderPlotly({
    req(input$biv_x, input$biv_y)
    d <- df_f(); v1 <- input$biv_x; v2 <- input$biv_y
    type <- input$biv_type

    gpalf <- function(n) get_pal_biv(n, input$biv_pal)
    niv1 <- nlevels(d[[v1]])

    if (grepl("groupé", type)) {
      cnt <- d |> count(.data[[v1]],.data[[v2]]) |>
        mutate(pct=round(n/sum(n)*100,1))
      niv2 <- nlevels(d[[v2]])
      p <- ggplot(cnt, aes(x=.data[[v1]],y=n,fill=.data[[v2]],
                           text=paste0(v1,"=",.data[[v1]],"\n",
                                       v2,"=",.data[[v2]],"\nN=",n," (",pct,"%)"))) +
        geom_col(position="dodge",width=0.75,alpha=0.88) +
        scale_fill_manual(values=gpalf(niv2),name=v2) +
        labs(x=v1,y="Effectif") + theme_minimal(base_size=11) +
        theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip="text") |>
        layout(legend=list(orientation="h",y=-0.25)) |>
        cfg_dl("biv_barplot_groupe")

    } else if (grepl("empilé", type)) {
      cnt <- d |> count(.data[[v1]],.data[[v2]]) |>
        group_by(.data[[v1]]) |>
        mutate(pct=round(n/sum(n)*100,1)) |> ungroup()
      niv2 <- nlevels(d[[v2]])
      p <- ggplot(cnt, aes(x=.data[[v1]],y=pct,fill=.data[[v2]],
                           text=paste0(.data[[v1]]," | ",.data[[v2]],"\n",pct,"% (N=",n,")"))) +
        geom_col(position=position_fill(),width=0.65,alpha=0.88) +
        geom_text(aes(label=paste0(pct,"%")),
                  position=position_fill(vjust=0.5),size=3,color="white",fontface="bold") +
        scale_fill_manual(values=gpalf(niv2),name=v2) +
        scale_y_continuous(labels=percent) +
        labs(x=v1,y="Proportion") + theme_minimal(base_size=11) +
        theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip="text") |>
        layout(legend=list(orientation="h",y=-0.25)) |>
        cfg_dl("biv_barplot_empile")

    } else if (grepl("Boxplot par", type)) {
      p <- ggplot(d, aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]],
                         text=paste0(.data[[v1]],"\n",v2,"=",round(.data[[v2]],2)))) +
        geom_boxplot(alpha=0.75,outlier.shape=21,outlier.color=oms_rouge,width=0.55) +
        stat_summary(fun=mean,geom="point",shape=18,size=3,color="white") +
        scale_fill_manual(values=gpalf(niv1)) +
        labs(x=v1,y=v2) + theme_minimal(base_size=11) +
        theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip="text") |> cfg_dl("biv_boxplot")

    } else if (grepl("Violin par", type)) {
      p <- ggplot(d, aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]])) +
        geom_violin(alpha=0.6,trim=FALSE) +
        geom_boxplot(fill="white",width=0.1,outlier.shape=21,outlier.color=oms_rouge) +
        stat_summary(fun=mean,geom="point",shape=18,size=3,color=oms_rouge) +
        scale_fill_manual(values=gpalf(niv1)) +
        labs(x=v1,y=v2) + theme_minimal(base_size=11) +
        theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p) |> cfg_dl("biv_violin")

    } else if (grepl("Nuage", type)) {
      v3 <- if(!is.null(input$biv_z) && input$biv_z!="Aucune") input$biv_z else NULL
      rho <- cor.test(d[[v1]],d[[v2]],method="spearman",exact=FALSE)
      p <- ggplot(d, aes(x=.data[[v1]],y=.data[[v2]],
                         color=if(!is.null(v3)) .data[[v3]] else NULL,
                         text=paste0(v1,"=",round(.data[[v1]],2),
                                     "\n",v2,"=",round(.data[[v2]],2)))) +
        geom_point(alpha=0.5,size=1.5) +
        geom_smooth(method="loess",se=TRUE,color="black",
                    linewidth=0.9,aes(group=1)) +
        {if(!is.null(v3)) scale_color_manual(values=gpalf(nlevels(d[[v3]])))} +
        labs(x=v1,y=v2,
             subtitle=paste0("ρ=",round(rho$estimate,3),"  p=",round(rho$p.value,4))) +
        theme_minimal(base_size=11)
      ggplotly(p,tooltip="text") |> cfg_dl("biv_nuage_points")

    } else if (grepl("Densités", type)) {
      p <- ggplot(d, aes(x=.data[[v2]],fill=.data[[v1]],color=.data[[v1]])) +
        geom_density(alpha=0.3,linewidth=0.8) +
        scale_fill_manual(values=gpalf(niv1),name=v1) +
        scale_color_manual(values=gpalf(niv1),name=v1) +
        labs(x=v2,y="Densité") + theme_minimal(base_size=11)
      ggplotly(p) |> layout(legend=list(orientation="h",y=-0.2)) |>
        cfg_dl("biv_densites")

    } else if (grepl("facettes", type)) {
      p <- ggplot(d, aes(x=.data[[v2]],fill=.data[[v1]])) +
        geom_histogram(aes(y=after_stat(density)),bins=25,color="white",alpha=0.82) +
        scale_fill_manual(values=gpalf(niv1)) +
        facet_wrap(as.formula(paste("~",v1)),ncol=2) +
        labs(x=v2,y="Densité") + theme_bw(base_size=10) +
        theme(legend.position="none")
      ggplotly(p) |> cfg_dl("biv_histogrammes_facettes")

    } else { # Heatmap
      cnt <- d |> count(.data[[v1]],.data[[v2]]) |>
        group_by(.data[[v1]]) |>
        mutate(pct=round(n/sum(n)*100,1)) |> ungroup()
      p <- ggplot(cnt, aes(x=.data[[v2]],y=.data[[v1]],fill=pct,
                           text=paste0(.data[[v1]]," × ",.data[[v2]],"\n",pct,"% (N=",n,")"))) +
        geom_tile(color="white",linewidth=1) +
        geom_text(aes(label=paste0(pct,"%")),size=3.5,fontface="bold") +
        scale_fill_gradientn(colors=c("white",oms_bleu,oms_rouge),name="%") +
        labs(x=v2,y=v1) + theme_minimal(base_size=11) +
        theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip="text") |> cfg_dl("biv_heatmap")
    }
  })

  # ════════════════════════════════════════════════════
  # ONGLET 3c — DESCRIPTIVE OMS
  # ════════════════════════════════════════════════════
  # Reactive partagé pour le tableau gtsummary (UI + téléchargements)
  gt_summary_reactive <- reactive({
    d <- df_f()
    vars_sel <- c("region","child_age_months","child_sex","displacement_status",
                  "food_consumption_score","household_hunger_scale",
                  "whz_score","haz_score","waz_score","muac_mm","IMC",
                  "oedema","diarrhea_last_2w","fever_last_2w",
                  "vaccination_status","mother_muac","anc_visits",
                  "GAM","SAM","Stunting","Underweight","MUAC_GAM","ICV","cat_vulner")
    tbl_data <- d |> select(any_of(c(vars_sel,
      if(input$desc_by != "none") input$desc_by else NULL)))
    lbl <- list(child_age_months~"Âge (mois)", child_sex~"Sexe",
                displacement_status~"Statut déplacement",
                food_consumption_score~"FCS", household_hunger_scale~"HHS",
                whz_score~"WHZ", haz_score~"HAZ", waz_score~"WAZ",
                muac_mm~"MUAC (mm)", oedema~"Œdèmes",
                diarrhea_last_2w~"Diarrhée", fever_last_2w~"Fièvre",
                vaccination_status~"Statut vaccinal", mother_muac~"MUAC mère",
                anc_visits~"Visites CPN", GAM~"GAM", SAM~"SAM",
                Stunting~"Stunting", Underweight~"Insuff. pondérale",
                MUAC_GAM~"GAM MUAC", ICV~"ICV", cat_vulner~"Vulnérabilité")
    if (input$desc_by == "none") {
      tbl_data |>
        tbl_summary(
          statistic = list(all_continuous()~"{mean} ± {sd} [{min}–{max}]",
                           all_categorical()~"{n} ({p}%)"),
          digits = all_continuous()~1, missing = "no", label = lbl
        ) |> bold_labels()
    } else {
      tbl_data |>
        tbl_summary(
          by        = input$desc_by,
          statistic = list(all_continuous()~"{mean} ± {sd}",
                           all_categorical()~"{n} ({p}%)"),
          digits    = all_continuous()~1, missing = "no", label = lbl
        ) |> bold_labels() |> add_p() |> bold_p(t = 0.05)
    }
  })

  output$desc_gt_out <- renderUI({
    gt_summary_reactive() |>
      as_gt() |>
      tab_options(table.font.size = "12px") |>
      as_raw_html()
  })

  # ── Téléchargement HTML du tableau descriptif ──────────────────────────────
  output$dl_gt_html <- downloadHandler(
    filename = function() paste0("tableau_descriptif_OMS_", Sys.Date(), ".html"),
    content  = function(file) {
      gt_summary_reactive() |>
        as_gt() |>
        tab_header(
          title    = md("**Tableau descriptif — Enquête SMART Cameroun 2024**"),
          subtitle = md("*Normes OMS 2006 | Méthode SMART*")
        ) |>
        tab_options(table.font.size = "13px") |>
        gt::gtsave(file)
    }
  )

  # ── Téléchargement CSV du tableau descriptif ───────────────────────────────
  output$dl_gt_csv <- downloadHandler(
    filename = function() paste0("tableau_descriptif_OMS_", Sys.Date(), ".csv"),
    content  = function(file) {
      gt_summary_reactive() |>
        as_tibble() |>
        write_csv(file)
    }
  )

  # ── Téléchargement Excel du tableau descriptif ─────────────────────────────
  output$dl_gt_excel <- downloadHandler(
    filename = function() paste0("tableau_descriptif_OMS_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      gt_summary_reactive() |>
        as_tibble() |>
        writexl::write_xlsx(file)
    }
  )

  make_corrplot <- function(data) {
    vars_c <- c("waz_score","haz_score","whz_score","muac_mm","IMC",
                "food_consumption_score","household_hunger_scale",
                "coping_strategy_index","child_age_months",
                "mother_muac","anc_visits","ICV")
    mat <- data |> select(all_of(vars_c)) |> drop_na() |>
      cor(method="spearman")
    corrplot(mat, method="color", type="upper",
             addCoef.col="black", number.cex=0.65,
             tl.cex=0.72, tl.col="black", tl.srt=45,
             col=colorRampPalette(c(oms_rouge,"white",oms_bleu))(200),
             mar=c(0,0,1,0), title="Corrélations Spearman")
  }

  output$desc_corr <- renderPlot({
    make_corrplot(df_f())
  }, height=400)

  output$dl_corrplot <- downloadHandler(
    filename = function() paste0("correlation_spearman_", Sys.Date(), ".png"),
    content  = function(file) {
      png(file, width=1400, height=1200, res=150)
      make_corrplot(df_f())
      dev.off()
    }
  )

  output$desc_zscore <- renderPlotly({
    d <- df_f()
    df_long <- d |>
      select(region, whz_score, haz_score, waz_score) |>
      pivot_longer(-region, names_to="Score", values_to="Val") |>
      mutate(Score=recode(Score, whz_score="WHZ",haz_score="HAZ",waz_score="WAZ"))
    p <- ggplot(df_long, aes(x=region,y=Val,fill=region)) +
      geom_boxplot(alpha=0.7,outlier.shape=21,outlier.color=oms_rouge,width=0.5) +
      geom_hline(yintercept=-2,linetype="dashed",color=oms_rouge,linewidth=0.7) +
      geom_hline(yintercept=-3,linetype="dashed",color=oms_rouge_f,linewidth=0.7) +
      geom_hline(yintercept=0, linetype="dotted",color="gray50",linewidth=0.5) +
      scale_fill_manual(values=couleurs_region) +
      facet_wrap(~Score,scales="free_y") +
      labs(x="",y="Score Z") +
      theme_bw(base_size=10) +
      theme(legend.position="none",
            axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p) |> layout(hovermode="closest") |> cfg_dl("desc_scores_z_region")
  })

  # ════════════════════════════════════════════════════
  # ONGLET 4 — INFÉRENTIEL
  # ════════════════════════════════════════════════════
  output$chi2_table <- renderDT({
    req(input$chi2_vars, input$chi2_outcome)
    d <- df_f()
    map_dfr(input$chi2_vars, function(var) {
      tryCatch({
        tbl     <- table(d[[var]], d[[input$chi2_outcome]])
        res_chi <- chisq.test(tbl, correct=FALSE)
        min_exp <- min(res_chi$expected)
        if (min_exp < 5) {
          res  <- fisher.test(tbl, simulate.p.value=TRUE)
          test <- "Fisher"; stat <- NA_real_
        } else {
          res <- res_chi; test <- "Chi²"
          stat <- round(res$statistic,3)
        }
        tibble(Variable=var, Test=test,
               `Min. attendu`=round(min_exp,1), Stat=stat,
               `p-valeur`=round(res$p.value,4),
               Sig=case_when(res$p.value<.001~"***",res$p.value<.01~"**",
                             res$p.value<.05~"*",res$p.value<.10~".",TRUE~"NS"))
      }, error=function(e) NULL)
    }) |>
      datatable(
        rownames   = FALSE,
        extensions = "Buttons",
        options    = list(
          dom        = "Bftp",
          pageLength = 10,
          buttons    = list(
            list(extend = "csv",   text = "📥 CSV",   filename = paste0("chi2_fisher_", Sys.Date())),
            list(extend = "excel", text = "📊 Excel", filename = paste0("chi2_fisher_", Sys.Date()),
                 title  = "Tests Chi² / Fisher — Cameroun"),
            list(extend = "copy",  text = "📋 Copier"),
            list(extend = "print", text = "🖨️ Imprimer")
          )
        ),
        class = "compact stripe hover"
      ) |>
      formatStyle("Sig",target="row",
                  backgroundColor=styleEqual(
                    c("***","**","*",".","NS"),
                    c("#ffe0e0","#ffe0e0","#fff3cd","#f8f9fa","white")))
  })

  output$mw_table <- renderDT({
    req(input$mw_vars, input$mw_outcome)
    d <- df_f()
    map_dfr(input$mw_vars, function(var) {
      g0 <- d[[var]][!d[[input$mw_outcome]]]
      g1 <- d[[var]][ d[[input$mw_outcome]]]
      test <- wilcox.test(g0, g1, exact=FALSE)
      tibble(Variable=var,
             `Méd. Négatif`=round(median(g0,na.rm=T),2),
             `Méd. Positif`=round(median(g1,na.rm=T),2),
             W=round(test$statistic,1),
             `p-valeur`=round(test$p.value,4),
             Sig=case_when(test$p.value<.001~"***",test$p.value<.01~"**",
                           test$p.value<.05~"*",test$p.value<.10~".",TRUE~"NS"))
    }) |>
      datatable(
        rownames   = FALSE,
        extensions = "Buttons",
        options    = list(
          dom        = "Bftp",
          pageLength = 10,
          buttons    = list(
            list(extend = "csv",   text = "📥 CSV",   filename = paste0("mann_whitney_", Sys.Date())),
            list(extend = "excel", text = "📊 Excel", filename = paste0("mann_whitney_", Sys.Date()),
                 title  = "Tests Mann-Whitney — Cameroun"),
            list(extend = "copy",  text = "📋 Copier"),
            list(extend = "print", text = "🖨️ Imprimer")
          )
        ),
        class = "compact stripe hover"
      ) |>
      formatStyle("Sig",target="row",
                  backgroundColor=styleEqual(
                    c("***","**","*",".","NS"),
                    c("#ffe0e0","#ffe0e0","#fff3cd","#f8f9fa","white")))
  })

  output$mw_plot <- renderPlotly({
    req(input$mw_vars, input$mw_outcome)
    d <- df_f()
    var <- input$mw_vars[1]
    p <- ggplot(d, aes(x=.data[[input$mw_outcome]],y=.data[[var]],
                       fill=.data[[input$mw_outcome]])) +
      geom_boxplot(alpha=0.75,outlier.shape=21,outlier.color=oms_rouge,width=0.4) +
      scale_fill_manual(values=c("TRUE"=oms_rouge,"FALSE"=oms_bleu),
                        labels=c("TRUE"=input$mw_outcome,"FALSE"=paste0("Non-",input$mw_outcome))) +
      labs(x=input$mw_outcome,y=var) + theme_minimal(base_size=11) +
      theme(legend.position="none")
    ggplotly(p) |> cfg_dl("infer_mann_whitney")
  })

  output$kw_result <- renderPrint({
    d <- df_f()
    kw <- kruskal.test(d[[input$kw_var]] ~ d[[input$kw_group]])
    cat("H =", round(kw$statistic,3),"\nddl =",kw$parameter,
        "\np =", round(kw$p.value,4),
        if(kw$p.value<.05)"\n→ Significatif" else "\n→ Non significatif")
  })

  output$kw_posthoc <- renderPrint({
    d <- df_f()
    tryCatch({
      ph <- pairwise.wilcox.test(d[[input$kw_var]], d[[input$kw_group]],
                                 p.adjust.method="BH", exact=FALSE)
      print(round(ph$p.value,4))
    }, error=function(e) cat("Post-hoc non disponible"))
  })

  output$kw_plot <- renderPlotly({
    d <- df_f()
    niv <- nlevels(d[[input$kw_group]])
    p <- ggplot(d, aes(x=.data[[input$kw_group]],y=.data[[input$kw_var]],
                       fill=.data[[input$kw_group]])) +
      geom_boxplot(alpha=0.75,outlier.shape=21,outlier.color=oms_rouge,width=0.5) +
      geom_hline(yintercept=-2,linetype="dashed",color=oms_rouge,linewidth=0.7) +
      stat_summary(fun=mean,geom="point",shape=18,size=3,color="white") +
      scale_fill_manual(values=get_pal_biv(niv,"Viridis")) +
      labs(x=input$kw_group,y=input$kw_var) +
      theme_minimal(base_size=11) +
      theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p) |> cfg_dl("infer_kruskal_wallis")
  })

  output$cor_result <- renderPrint({
    d <- df_f()
    tryCatch({
      r <- cor.test(d[[input$cor_x]],d[[input$cor_y]],
                    method="spearman",exact=FALSE)
      cat("Spearman ρ =",round(r$estimate,4),
          "\np-valeur  =",round(r$p.value,4),
          if(r$p.value<.05)"\n→ Corrélation significative"
          else "\n→ Pas de corrélation significative",
          "\n\nInterprétation :\n",
          if(abs(r$estimate)>=.7)"Forte corrélation"
          else if(abs(r$estimate)>=.4)"Corrélation modérée"
          else "Corrélation faible")
    }, error=function(e) cat("Test non disponible"))
  })

  output$cor_plot <- renderPlotly({
    d <- df_f()
    rho <- tryCatch(
      cor.test(d[[input$cor_x]],d[[input$cor_y]],method="spearman",exact=FALSE),
      error=function(e) NULL
    )
    v3 <- if(!is.null(input$cor_color) && input$cor_color!="Aucune") input$cor_color else NULL
    p <- ggplot(d, aes(x=.data[[input$cor_x]], y=.data[[input$cor_y]],
                       color=if(!is.null(v3)) .data[[v3]] else NULL,
                       text=paste0(input$cor_x,"=",round(.data[[input$cor_x]],2),
                                   "\n",input$cor_y,"=",round(.data[[input$cor_y]],2)))) +
      geom_point(alpha=0.5,size=1.5) +
      geom_smooth(method="loess",se=TRUE,color="black",
                  linewidth=0.9,aes(group=1)) +
      {if(!is.null(v3)) scale_color_manual(values=get_pal_biv(nlevels(d[[v3]]),"OMS régions"))} +
      labs(x=input$cor_x,y=input$cor_y,
           subtitle=if(!is.null(rho))
             paste0("ρ=",round(rho$estimate,3),"  p=",round(rho$p.value,4))
           else "") +
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") |> cfg_dl("infer_correlation_spearman")
  })

  # ════════════════════════════════════════════════════
  # ONGLET 5 — CARTOGRAPHIE
  # ════════════════════════════════════════════════════
  output$leaf_map <- renderLeaflet({
    var  <- input$leaf_var
    fond <- input$leaf_fond

    df_geo <- if (input$leaf_niv == "Région") {
      prev_region |> mutate(nom=as.character(region))
    } else {
      prev_dept |> mutate(nom=paste0(department," (",region,")"))
    }
    vals <- df_geo[[var]]
    pal_l <- colorNumeric(pal_gam, domain=vals, na.color="#aaa")

    popup_html <- paste0(
      "<div style='font-family:Arial;font-size:13px;min-width:220px;line-height:1.8'>",
      "<b style='color:#008DC9;font-size:14px'>",df_geo$nom,"</b><hr>",
      "🔴 GAM : <b>",round(df_geo$GAM_pct,1)," %</b><br>",
      "🟠 SAM : <b>",round(df_geo$SAM_pct,1)," %</b><br>",
      "📏 Stunting : <b>",round(df_geo$Stunting_pct,1)," %</b><br>",
      "📊 MUAC moy : <b>",round(df_geo$muac_moy,1)," mm</b><br>",
      "🛡️ ICV : <b>",round(df_geo$icv_moy,1),"/100</b><br>",
      "👶 N : <b>",df_geo$n,"</b></div>"
    )

    m <- leaflet(df_geo) |>
      addProviderTiles(fond) |>
      addCircleMarkers(
        lat=~lat, lng=~lon,
        radius=~rescale(n, to=c(10,28)),
        fillColor=~pal_l(vals), fillOpacity=0.88,
        color="white", weight=2,
        popup=popup_html,
        label=~paste0(nom,": ",round(vals,1),"%")
      ) |>
      addLegend(pal=pal_l, values=vals, title=var,
                position="bottomright",
                labFormat=labelFormat(suffix=" %")) |>
      addScaleBar(position="bottomleft") |>
      addMiniMap(toggleDisplay=TRUE, minimized=TRUE)

    # Polygones GADM si disponibles
    if (input$leaf_gadm && !is.null(cameroun_sf)) {
      pal_poly <- colorNumeric(pal_gam,
        domain=cameroun_sf |> left_join(prev_region,by="region") |> pull(GAM_pct),
        na.color="#ddd")
      m <- m |>
        addPolygons(data=cameroun_sf |> left_join(prev_region,by="region"),
                    fillColor=~pal_poly(GAM_pct), fillOpacity=0.4,
                    color=oms_bleu, weight=1.5,
                    popup=~paste0("<b>",region,"</b><br>GAM: ",round(GAM_pct,1),"%"))
    }

    if (input$leaf_pts) {
      d_pts <- df |> filter(!is.na(gps_latitude), !is.na(gps_longitude))
      pal_pt <- colorFactor(c(oms_vert,oms_orange,oms_rouge),
                            domain=c("Normal","MAM","SAM"))
      m <- m |>
        addCircleMarkers(
          data=d_pts, lat=~gps_latitude, lng=~gps_longitude,
          radius=4, fillColor=~pal_pt(nutr_cat),
          fillOpacity=0.85, color="white", weight=0.5,
          clusterOptions=markerClusterOptions(disableClusteringAtZoom=11),
          popup=~paste0(
            "<div style='font-family:Arial;font-size:12px'>",
            "<b>",household_id,"</b><br>",
            "Village : ",village," | Zone : ",district,"<br>",
            "Statut : <b style='color:",
            ifelse(nutr_cat=="SAM",oms_rouge,
                   ifelse(nutr_cat=="MAM",oms_orange,oms_vert)),"'>",
            nutr_cat,"</b><br>",
            "WHZ=",round(whz_score,2)," | MUAC=",muac_mm,"mm</div>"
          )
        ) |>
        addLegend(pal=pal_pt, values=d_pts$nutr_cat,
                  title="Statut individuel", position="topleft")
    }
    m
  })

  output$gg_map <- renderPlotly({
    var <- input$gg_var
    pal_ch <- switch(input$gg_pal,
      "OMS standard" = pal_gam,
      "YlOrRd"       = c("#ffffb2","#fed976","#feb24c","#fd8d3c","#e31a1c"),
      "Blues"        = c("#eff3ff","#bdd7e7","#6baed6","#2171b5","#084594"),
      "Viridis"      = viridis(5)
    )

    if (input$gg_type == "Points proportionnels") {
      df_geo <- prev_region |> mutate(nom=as.character(region))
      p <- ggplot(df_geo) +
        {if(!is.null(cameroun_sf))
          geom_sf(data=cameroun_sf,fill="grey92",color="white",linewidth=0.4)
         else geom_blank()} +
        geom_point(aes(x=lon, y=lat, size=.data[[var]], color=.data[[var]],
                       text=paste0(nom,"\n",var,"=",round(.data[[var]],1),"%")),
                   alpha=0.85) +
        scale_size_continuous(range=c(5,20),guide="none") +
        scale_color_gradientn(colors=pal_ch,name=paste0(var," (%)")) +
        labs(title=paste("Cameroun —",var),
             caption="Source : Enquête nutritionnelle 2024 | GADM") +
        theme_void() +
        theme(plot.title=element_text(hjust=.5,face="bold",size=13),
              legend.position="right")
    } else {
      if (is.null(cameroun_sf)) {
        p <- ggplot() + annotate("text",x=0,y=0,label="GADM non disponible") + theme_void()
      } else {
        sf_data <- cameroun_sf |> left_join(prev_region, by="region")
        centroides <- sf_data |>
          filter(!is.na(.data[[var]])) |> st_centroid() |>
          mutate(lon=st_coordinates(geometry)[,1],
                 lat=st_coordinates(geometry)[,2]) |>
          st_drop_geometry()
        p <- ggplot(sf_data) +
          geom_sf(fill="grey92",color="white",linewidth=0.4) +
          geom_sf(data=filter(sf_data,!is.na(.data[[var]])),
                  aes(fill=.data[[var]]),color="white",linewidth=0.5) +
          geom_text(data=centroides,
                    aes(x=lon,y=lat,label=paste0(region,"\n",round(.data[[var]],1),"%")),
                    size=3,fontface="bold",color="white") +
          scale_fill_gradientn(colors=pal_ch,na.value="grey92",
                               name=paste0(var," (%)")) +
          labs(title=paste("Cameroun —",var),caption="Source : GADM + Enquête 2024") +
          theme_void() +
          theme(plot.title=element_text(hjust=.5,face="bold",size=13))
      }
    }
    ggplotly(p, tooltip="text") |> cfg_dl("carto_cameroun_ggplot")
  })

  output$world_map <- renderPlotly({
    highlight_countries <- switch(input$world_highlight,
      "Cameroun seul"          = c("Cameroon"),
      "Afrique centrale"       = c("Cameroon","Chad","Central African Republic",
                                    "Nigeria","Niger","Gabon","Congo",
                                    "Democratic Republic of the Congo"),
      "Afrique subsaharienne"  = world_sf$name[world_sf$continent=="Africa"]
    )
    world_data <- world_sf |>
      mutate(
        highlight = name %in% highlight_countries,
        fill_val  = case_when(
          name == "Cameroon" ~ 3,
          name %in% highlight_countries ~ 2,
          TRUE ~ 1
        )
      )
    p <- ggplot(world_data) +
      geom_sf(aes(fill=factor(fill_val),
                  text=paste0(name,
                              if_else(name=="Cameroon",
                                      paste0("\nGAM enquêtée : ",
                                             round(mean(df$GAM,na.rm=T)*100,1),"%"),
                                      ""))),
              color="white",linewidth=0.3) +
      scale_fill_manual(
        values=c("1"="grey85","2"="#b3d9ef","3"=oms_rouge),
        labels=c("1"="Reste du monde","2"="Afrique centrale","3"="Cameroun"),
        name=""
      ) +
      coord_sf(xlim=c(-20,50), ylim=c(-35,37)) +
      labs(title="Cameroun en contexte régional africain",
           caption="Source : Natural Earth | Enquête SMART 2024") +
      theme_void() +
      theme(plot.title=element_text(hjust=.5,face="bold",size=13),
            legend.position="bottom")
    ggplotly(p, tooltip="text") |>
      layout(legend=list(orientation="h",y=-0.1)) |>
      cfg_dl("carto_monde_contexte")
  })

  output$gps_map <- renderLeaflet({
    d_pts <- df |> filter(!is.na(gps_latitude), !is.na(gps_longitude))
    col_var <- input$gps_color
    col_vals <- d_pts[[col_var]]
    niv <- length(unique(col_vals))
    pal_pt <- colorFactor(
      palette=get_pal_uni(niv,"OMS vert/rouge"),
      domain=col_vals
    )
    m <- leaflet(d_pts) |>
      addProviderTiles("CartoDB.Positron") |>
      addScaleBar(position="bottomleft") |>
      addMiniMap(toggleDisplay=TRUE, minimized=TRUE)
    if (input$gps_cluster) {
      m <- m |>
        addCircleMarkers(
          lat=~gps_latitude, lng=~gps_longitude,
          radius=input$gps_size, fillColor=~pal_pt(col_vals),
          fillOpacity=0.85, color="white", weight=0.8,
          clusterOptions=markerClusterOptions(disableClusteringAtZoom=11),
          popup=~paste0("<b>",household_id,"</b><br>",
                        "Village: ",village,"<br>",
                        col_var,": ",col_vals,"<br>",
                        "WHZ=",round(whz_score,2)," MUAC=",muac_mm,"mm")
        )
    } else {
      m <- m |>
        addCircleMarkers(
          lat=~gps_latitude, lng=~gps_longitude,
          radius=input$gps_size, fillColor=~pal_pt(col_vals),
          fillOpacity=0.85, color="white", weight=0.8,
          popup=~paste0("<b>",household_id,"</b><br>",
                        "Village: ",village,"<br>",
                        col_var,": ",col_vals,"<br>",
                        "WHZ=",round(whz_score,2)," MUAC=",muac_mm,"mm")
        )
    }
    m |> addLegend(pal=pal_pt, values=col_vals, title=col_var,
                   position="bottomright")
  })

  # ════════════════════════════════════════════════════
  # ONGLET 6 — INDICATEURS OMS
  # ════════════════════════════════════════════════════
  output$oms_table <- renderDT({
    d <- df_f()
    inds <- list(
      GAM="GAM (WHZ<-2 ou œdème)",SAM="SAM (WHZ<-3 ou œdème)",
      MAM="MAM (WHZ -3 à -2)", Stunting="Stunting (HAZ<-2)",
      Sev_Stunting="Stunting sévère (HAZ<-3)",
      Underweight="Insuff. pondérale (WAZ<-2)",
      MUAC_GAM="MUAC GAM (<125mm)",MUAC_SAM="MUAC SAM (<115mm)"
    )
    groupes <- list()
    if ("Global" %in% input$oms_groupes)
      groupes[["Global"]] <- list(var=NULL, label="Global")
    if ("Région" %in% input$oms_groupes)
      groupes[["Région"]] <- list(var="region", label="Région")
    if ("Sexe" %in% input$oms_groupes)
      groupes[["Sexe"]] <- list(var="child_sex", label="Sexe")
    if ("Groupe d'âge" %in% input$oms_groupes)
      groupes[["Groupe d'âge"]] <- list(var="age_group", label="Groupe d'âge")
    if ("Statut déplacement" %in% input$oms_groupes)
      groupes[["Déplacement"]] <- list(var="displacement_status", label="Déplacement")
    if ("Sécurité alim." %in% input$oms_groupes)
      groupes[["Sécu. alim."]] <- list(var="fcs_cat", label="Sécu. alim.")

    map_dfr(groupes, function(g) {
      if (is.null(g$var)) {
        map2_dfr(names(inds),inds,function(v,l) calc_prev(d,v,l)) |>
          mutate(Stratification="Global", Groupe="—", .before=1)
      } else {
        d |> group_by(Groupe=.data[[g$var]]) |>
          group_modify(~map2_dfr(names(inds),inds,function(v,l) calc_prev(.x,v,l))) |>
          ungroup() |>
          mutate(Stratification=g$label, .before=1)
      }
    }) |>
      datatable(
        rownames   = FALSE,
        filter     = "top",
        extensions = "Buttons",
        options    = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "Bftp",
          buttons    = list(
            list(extend = "csv",   text = "📥 CSV",
                 filename    = paste0("indicateurs_OMS_", Sys.Date()),
                 exportOptions = list(modifier = list(page = "all"))),
            list(extend = "excel", text = "📊 Excel",
                 filename    = paste0("indicateurs_OMS_", Sys.Date()),
                 title       = "Indicateurs OMS 2006 — Enquête SMART Cameroun 2024",
                 exportOptions = list(modifier = list(page = "all"))),
            list(extend = "pdf",   text = "📄 PDF",
                 filename    = paste0("indicateurs_OMS_", Sys.Date()),
                 title       = "Indicateurs OMS 2006 — Cameroun",
                 orientation = "landscape", pageSize = "A3"),
            list(extend = "copy",  text = "📋 Copier"),
            list(extend = "print", text = "🖨️ Imprimer",
                 title  = "Indicateurs OMS 2006 — Enquête SMART Cameroun 2024")
          )
        ),
        class = "compact stripe hover"
      ) |>
      formatStyle("Prév. (%)",
                  background=styleColorBar(c(0,100), oms_bleu),
                  backgroundSize="100% 70%") |>
      formatStyle("Indicateur", fontWeight="bold") |>
      formatStyle("Prév. (%)",
                  color=styleInterval(c(5,10,15,30),
                                      c(oms_vert,oms_jaune,oms_orange,oms_rouge,oms_rouge_f)),
                  fontWeight="bold")
  })

  output$oms_forest <- renderPlotly({
    d <- df_f()
    inds <- c("GAM","SAM","Stunting","Underweight","MUAC_GAM")
    res <- d |> group_by(region) |>
      group_modify(~map_dfr(inds, function(v) calc_prev(.x,v,v))) |>
      ungroup()
    p <- ggplot(res, aes(x=`Prév. (%)`,
                         y=fct_reorder(paste0(region," — ",Indicateur),`Prév. (%)`),
                         color=Indicateur,
                         text=paste0(region," — ",Indicateur,
                                     "\n",`Prév. (%)`,"% [",`IC95 bas`,"-",`IC95 haut`,"%]",
                                     "\nN=",N))) +
      geom_vline(xintercept=c(5,10,15),linetype="dashed",
                 color=c(oms_jaune,oms_orange,oms_rouge),
                 linewidth=0.6, alpha=0.7) +
      geom_errorbarh(aes(xmin=`IC95 bas`,xmax=`IC95 haut`),
                     height=0.4,linewidth=0.8) +
      geom_point(size=3) +
      scale_color_manual(values=c(GAM=oms_rouge,SAM=oms_rouge_f,
                                   Stunting=oms_bleu,Underweight=oms_violet,
                                   MUAC_GAM=oms_orange)) +
      labs(x="Prévalence (%)",y="") +
      theme_minimal(base_size=10)
    ggplotly(p,tooltip="text") |>
      layout(legend=list(orientation="h",y=-0.15)) |>
      cfg_dl("oms_forest_plot_prevalences")
  })

  output$oms_gauge <- renderPlotly({
    d <- df_f()
    gam_val <- round(mean(d$GAM,na.rm=T)*100,1)
    plot_ly(
      type="indicator", mode="gauge+number+delta",
      value=gam_val,
      title=list(text="GAM globale (%)", font=list(size=16)),
      delta=list(reference=15, decreasing=list(color=oms_vert),
                 increasing=list(color=oms_rouge)),
      gauge=list(
        axis=list(range=list(0,50), tickwidth=1, tickcolor="gray"),
        bar=list(color=if(gam_val>=15) oms_rouge else if(gam_val>=10) oms_orange else oms_vert),
        steps=list(
          list(range=c(0,5),   color="#e8f5e9"),
          list(range=c(5,10),  color="#fffde7"),
          list(range=c(10,15), color="#fff3e0"),
          list(range=c(15,30), color="#ffebee"),
          list(range=c(30,50), color="#b71c1c")
        ),
        threshold=list(line=list(color=oms_rouge,width=4),
                       thickness=0.75, value=15)
      )
    ) |>
      layout(margin=list(l=20,r=30,b=20,t=60)) |>
      cfg_dl("oms_jauge_gam")
  })

  output$oms_trend <- renderPlotly({
    d <- df_f()
    inds <- c("GAM","SAM","Stunting")
    res <- d |>
      group_by(survey_round) |>
      summarise(across(all_of(inds),~round(mean(.,na.rm=T)*100,1),.names="{.col}"),
                .groups="drop") |>
      pivot_longer(-survey_round,names_to="Indicateur",values_to="Prév.")
    p <- ggplot(res,aes(x=survey_round,y=`Prév.`,
                        color=Indicateur,group=Indicateur,
                        text=paste0("Round: ",survey_round,
                                    "\n",Indicateur,": ",`Prév.`,"%"))) +
      geom_line(linewidth=1.2) + geom_point(size=3) +
      geom_hline(yintercept=15,linetype="dashed",color=oms_rouge,linewidth=0.7) +
      scale_color_manual(values=c(GAM=oms_rouge,SAM=oms_rouge_f,Stunting=oms_bleu)) +
      labs(x="Survey round",y="Prévalence (%)") +
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") |>
      layout(legend=list(orientation="h",y=-0.2),
             hovermode="x unified") |>
      cfg_dl("oms_tendance_survey_rounds")
  })
}

# ── 7. LANCEMENT ──────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
