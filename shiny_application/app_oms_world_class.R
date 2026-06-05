# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  APPLICATION SHINY — MALNUTRITION AU CAMEROUN  ║  VERSION MONDIALE OMS     ║
# ║  Standard OMS/UNICEF/PAM | 12 modules analytiques | Niveau financement     ║
# ╚══════════════════════════════════════════════════════════════════════════════╝
# Auteur   : Yimga kevin
# Date     : 2024
# Standard : OMS 2006 | SMART methodology | UNICEF Conceptual Framework

# ══════════════════════════════════════════════════════════════════════════════
# 1. PACKAGES
# ══════════════════════════════════════════════════════════════════════════════
pkgs <- c(
  "shiny","shinydashboard","shinydashboardPlus","shinyjs","shinyWidgets",
  "tidyverse","forcats","scales","lubridate",
  "DT","plotly","ggiraph","leaflet","leaflet.extras","sf",
  "binom","gtsummary","gt","corrplot","RColorBrewer","viridis",
  "e1071","nortest","rnaturalearth","rnaturalearthdata","writexl",
  "survival","survminer","ggalluvial","ggridges",
  "cluster","factoextra","nnet","pROC"
)
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    tryCatch(install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE),
             error = function(e) NULL)
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

# REPLACEMENT FUNCTION FOR WAFFLE CHARTS
# ══════════════════════════════════════════════════════════════════════════════
create_waffle_plot <- function(data, category, value) {
  df <- data %>%
    dplyr::select({{category}}, {{value}}) %>%
    tidyr::uncount({{value}}) %>%
    dplyr::mutate(
      x = rep(1:10, length.out = n()),
      y = rep(1:10, each = 10)[1:n()]
    )
  
  ggplot(df, aes(x, y, fill = {{category}})) +
    geom_tile(color = "white") +
    coord_equal() +
    theme_void() +
    scale_y_reverse()
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. PALETTE OMS / UNICEF
# ══════════════════════════════════════════════════════════════════════════════
oms_bleu    <- "#008DC9"; oms_vert    <- "#4dac26"; oms_jaune   <- "#f9c734"
oms_orange  <- "#e88400"; oms_rouge   <- "#c9161d"; oms_rouge_f <- "#640000"
oms_gris    <- "#6c757d"; oms_violet  <- "#7b2d8b"; unicef_bleu <- "#009fda"
oms_dark    <- "#00426A"; oms_teal    <- "#00897B"

pal_gam <- c(oms_vert, oms_jaune, oms_orange, oms_rouge, oms_rouge_f)
couleurs_region <- c("Extrême-Nord"="#E41A1C","Adamaoua"="#377EB8","Nord"="#4DAF4A")

# ══════════════════════════════════════════════════════════════════════════════
# 3. CHARGEMENT & PRÉPARATION DES DONNÉES
# ══════════════════════════════════════════════════════════════════════════════
df_raw <- read_csv(
  "data/nutrition_cameroon_synthetic_hierarchy (1).csv",
  show_col_types = FALSE
) |> rename_with(tolower)

df <- df_raw |>
  mutate(
    across(where(is.character), as.factor),
    oedema = oedema %in% c("Yes","TRUE","yes","true","1"),
    date_survey = as.Date(date_survey),

    # ── Anthropométrie ──────────────────────────────────────────────────────
    IMC = weight_kg / (height_cm / 100)^2,
    statut_imc = cut(IMC, breaks=c(0,18.5,25,30,Inf),
                     labels=c("Sous-poids","Normal","Surpoids","Obèse"),
                     right=FALSE),

    # ── Indicateurs OMS 2006 ──────────────────────────────────────────────
    GAM          = (whz_score < -2) | oedema,
    SAM          = (whz_score < -3) | oedema,
    MAM          = (whz_score >= -3) & (whz_score < -2) & !oedema,
    Stunting     = haz_score < -2,
    Sev_Stunting = haz_score < -3,
    Mod_Stunting = haz_score >= -3 & haz_score < -2,
    Underweight  = waz_score < -2,
    Sev_Underweight = waz_score < -3,
    MUAC_GAM     = muac_mm < 125,
    MUAC_SAM     = muac_mm < 115,
    MUAC_MAM     = muac_mm >= 115 & muac_mm < 125,

    nutr_cat = factor(case_when(SAM~"SAM",MAM~"MAM",TRUE~"Normal"),
                      levels=c("Normal","MAM","SAM")),
    muac_cat = factor(case_when(
      muac_mm<115~"SAM (<115mm)",muac_mm<125~"MAM (115-124mm)",TRUE~"Normal (≥125mm)"),
      levels=c("Normal (≥125mm)","MAM (115-124mm)","SAM (<115mm)")),

    # ── Groupes d'âge OMS ──────────────────────────────────────────────────
    age_group = cut(child_age_months,
                    breaks=c(0,5,11,17,23,35,47,59,Inf),
                    labels=c("0-5m","6-11m","12-17m","18-23m","24-35m","36-47m","48-59m","60+m"),
                    right=TRUE),

    # ── Sécurité alimentaire ────────────────────────────────────────────────
    fcs_cat = factor(case_when(
      food_consumption_score<=21~"Pauvre",
      food_consumption_score<=35~"Limite",TRUE~"Acceptable"),
      levels=c("Pauvre","Limite","Acceptable")),
    hhs_cat = factor(case_when(
      household_hunger_scale==0~"Aucune faim",household_hunger_scale<=1~"Faim légère",
      household_hunger_scale<=3~"Faim modérée",TRUE~"Faim sévère"),
      levels=c("Aucune faim","Faim légère","Faim modérée","Faim sévère")),

    # ── ICV — Indice composite de vulnérabilité ─────────────────────────────
    ICV = rescale(food_consumption_score, to=c(0,20),from=range(food_consumption_score,na.rm=T)) +
          rescale(household_hunger_scale,  to=c(20,0),from=range(household_hunger_scale,na.rm=T)) +
          rescale(coping_strategy_index,   to=c(20,0),from=range(coping_strategy_index,na.rm=T)) +
          rescale(muac_mm,                 to=c(0,20),from=range(muac_mm,na.rm=T)) +
          rescale(anc_visits,              to=c(0,20),from=range(anc_visits,na.rm=T)),

    cat_vulner = cut(ICV,breaks=c(0,40,60,80,100),
                     labels=c("Très vulnérable","Vulnérable","Modéré","Résilient"),
                     include.lowest=TRUE),

    # ── Score de risque composite (0-100) ─────────────────────────────────
    risk_score = rescale(
      -whz_score + (-haz_score)*0.5 + (-waz_score)*0.3 +
      (household_hunger_scale/4)*20 + (125-muac_mm)/10,
      to=c(0,100), from=c(-10,50)
    ),
    risk_cat = cut(risk_score,breaks=c(0,25,50,75,100),
                   labels=c("Faible","Modéré","Élevé","Critique"),
                   include.lowest=TRUE),

    # ── Accès aux services ─────────────────────────────────────────────────
    access_score = rescale(
      as.integer(nutrition_program_access=="Yes")*30 +
      as.integer(food_aid_received=="Yes")*20 +
      rescale(-health_facility_distance_km, to=c(0,30), from=range(health_facility_distance_km,na.rm=T)) +
      anc_visits*5,
      to=c(0,100), from=c(0,95)
    ),

    # ── Mois enquête ──────────────────────────────────────────────────────
    month_survey = floor_date(date_survey, "month"),
    trimestre = paste0("T",ceiling(month(date_survey)/3)),
    annee = year(date_survey)
  )

# ── Variables par type ──────────────────────────────────────────────────────
vars_cat <- df |> select(where(is.factor)) |> names()
vars_num <- df |> select(where(is.numeric)) |>
  select(-starts_with("score_")) |> names()
vars_cat_pie <- vars_cat[sapply(vars_cat, function(v)
  nlevels(df[[v]]) >= 2 & nlevels(df[[v]]) <= 7)]

# ── Fonctions utilitaires ───────────────────────────────────────────────────
mode_stat <- function(x) { ux <- unique(na.omit(x)); ux[which.max(tabulate(match(x,ux)))] }
cv_stat   <- function(x) round(sd(x,na.rm=T)/mean(x,na.rm=T)*100,2)

calc_prev <- function(data, var, label=NULL) {
  x  <- sum(data[[var]],na.rm=TRUE)
  n  <- sum(!is.na(data[[var]]))
  ic <- binom.confint(x,n,methods="wilson")
  tibble(
    Indicateur  = if(is.null(label)) var else label,
    N=n, Cas=x,
    `Prév. (%)` = round(100*ic$mean,1),
    `IC95 bas`  = round(100*ic$lower,1),
    `IC95 haut` = round(100*ic$upper,1)
  )
}

severity_color <- function(p) {
  if(p>=30) oms_rouge_f else if(p>=15) oms_rouge else if(p>=10) oms_orange else if(p>=5) oms_jaune else oms_vert
}

# ── Agrégations géographiques ──────────────────────────────────────────────
prev_region <- df |> group_by(region) |>
  summarise(n=n(), GAM_pct=mean(GAM,na.rm=T)*100, SAM_pct=mean(SAM,na.rm=T)*100,
            MAM_pct=mean(MAM,na.rm=T)*100, Stunting_pct=mean(Stunting,na.rm=T)*100,
            Under_pct=mean(Underweight,na.rm=T)*100, MUAC_GAM_pct=mean(MUAC_GAM,na.rm=T)*100,
            whz_moy=mean(whz_score,na.rm=T), haz_moy=mean(haz_score,na.rm=T),
            muac_moy=mean(muac_mm,na.rm=T), icv_moy=mean(ICV,na.rm=T),
            risk_moy=mean(risk_score,na.rm=T),
            lat=mean(gps_latitude,na.rm=T), lon=mean(gps_longitude,na.rm=T), .groups="drop")

prev_dept <- df |> group_by(region,department) |>
  summarise(n=n(), GAM_pct=mean(GAM,na.rm=T)*100, SAM_pct=mean(SAM,na.rm=T)*100,
            Stunting_pct=mean(Stunting,na.rm=T)*100, muac_moy=mean(muac_mm,na.rm=T),
            icv_moy=mean(ICV,na.rm=T), risk_moy=mean(risk_score,na.rm=T),
            lat=mean(gps_latitude,na.rm=T), lon=mean(gps_longitude,na.rm=T), .groups="drop")

# ── Fond de carte GADM ────────────────────────────────────────────────────
cameroun_sf <- tryCatch(
  st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_1.json",quiet=TRUE) |>
    rename(region=NAME_1), error=function(e) NULL)

world_sf <- ne_countries(scale="medium", returnclass="sf")

# ══════════════════════════════════════════════════════════════════════════════
# 4. CSS PERSONNALISÉ (niveau OMS)
# ══════════════════════════════════════════════════════════════════════════════
css_oms <- "
/* ── Sidebar ── */
.main-sidebar,.left-side { background:linear-gradient(180deg,#00264d,#00426A) !important; }
.sidebar-menu>li>a { color:#cce8f4 !important; font-size:12.5px; padding:10px 15px; }
.sidebar-menu>li.active>a,.sidebar-menu>li>a:hover {
  background:rgba(0,141,201,0.35) !important; color:white !important;
  border-left:4px solid #4dac26 !important; }
.sidebar-menu .treeview-menu>li>a { color:#a8d4eb !important; font-size:11.5px; }
.logo { background:#001f3f !important; }
.logo:hover { background:#008DC9 !important; }
/* ── Header ── */
.main-header .navbar { background:#008DC9 !important; }
.main-header .logo { background:#001f3f !important; color:white !important;
  font-weight:700; font-size:13px; letter-spacing:0.5px; }
/* ── KPI boxes ── */
.small-box { border-radius:10px !important; box-shadow:0 4px 12px rgba(0,0,0,0.15) !important; }
.small-box:hover { transform:translateY(-3px); transition:0.25s; box-shadow:0 8px 20px rgba(0,0,0,0.2) !important; }
/* ── Cards ── */
.box { border-radius:10px !important; box-shadow:0 3px 10px rgba(0,0,0,0.08) !important; }
.box-header { border-radius:10px 10px 0 0 !important; }
/* ── Tables DT ── */
table.dataTable thead th { background:#008DC9 !important; color:white !important; font-weight:600; }
table.dataTable tbody tr:hover { background:#e8f4fb !important; }
/* ── Body ── */
.content-wrapper,.right-side { background:#f0f3f7 !important; }
/* ── DT Export buttons ── */
.dt-button { background:#008DC9 !important; color:white !important; border:none !important;
  border-radius:4px !important; padding:4px 10px !important; font-size:11px !important;
  font-weight:600 !important; margin-right:3px !important; cursor:pointer !important; }
.dt-button:hover { background:#006fa3 !important; }
.dt-button.buttons-excel { background:#1d7344 !important; }
.dt-button.buttons-pdf   { background:#c9161d !important; }
/* ── Alert badges ── */
.badge-critique { background:#640000;color:white;padding:3px 7px;border-radius:3px;font-size:11px; }
.badge-alerte   { background:#c9161d;color:white;padding:3px 7px;border-radius:3px;font-size:11px; }
.badge-serieux  { background:#e88400;color:white;padding:3px 7px;border-radius:3px;font-size:11px; }
.badge-ok       { background:#4dac26;color:white;padding:3px 7px;border-radius:3px;font-size:11px; }
/* ── Tabs ── */
.nav-tabs-custom>.nav-tabs>li.active { border-top-color:#008DC9; }
/* ── Progress bar ── */
.progress-bar { background:#008DC9; }
/* ── Notification ring ── */
#alert_ring { animation: pulse 2s infinite; }
@keyframes pulse { 0%,100%{opacity:1} 50%{opacity:0.5} }
"

# ── Helper: barre boutons Plotly ───────────────────────────────────────────
dl_bar <- function(plot_id, filename) {
  tags$div(style="margin-bottom:6px;",
    tags$button(
      style="background:#008DC9;color:white;border:none;border-radius:4px;
             padding:5px 11px;font-size:11px;font-weight:600;cursor:pointer;margin-right:4px;",
      onclick=paste0("Plotly.downloadImage(document.getElementById('",plot_id,"'),
                     {format:'png',filename:'",filename,"',height:700,width:1200,scale:2});"),
      "📷 PNG"),
    tags$button(
      style="background:#6c757d;color:white;border:none;border-radius:4px;
             padding:5px 11px;font-size:11px;font-weight:600;cursor:pointer;",
      onclick=paste0("Plotly.downloadImage(document.getElementById('",plot_id,"'),
                     {format:'svg',filename:'",filename,"',height:700,width:1200,scale:2});"),
      "🖼️ SVG")
  )
}

cfg_dl <- function(p, nom="graphique_oms") {
  p |> plotly::config(
    toImageButtonOptions=list(format="svg",filename=nom,height=700,width=1200,scale=2),
    displaylogo=FALSE, locale="fr",
    modeBarButtonsToRemove=list("lasso2d","select2d"))
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin="blue",

  # ── Header ─────────────────────────────────────────────────────────────────
  dashboardHeader(
    title=tags$span(
      tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/World_Health_Organization_Logo.svg/100px-World_Health_Organization_Logo.svg.png",
               height="26px",style="margin-right:8px;"),
      "NutriCam — OMS/UNICEF"
    ),
    titleWidth=280,
    dropdownMenu(type="notifications", badgeStatus="danger",
      notificationItem("⚠️ Vérifier seuils critiques", status="danger"),
      notificationItem("📊 Rapport mensuel disponible", status="warning"),
      notificationItem("🗺️ Données GPS complètes", status="success")
    )
  ),

  # ── Sidebar ────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width=255,
    tags$style(css_oms),

    tags$div(style="padding:12px 14px 5px;color:#7ab8d4;font-size:10px;
             font-weight:700;text-transform:uppercase;letter-spacing:1px;",
             "🔧 FILTRES GLOBAUX"),
    tags$div(style="padding:0 12px 10px;",
      tags$p(style="color:#7ab8d4;font-size:9px;font-weight:700;margin:5px 0 2px;","▸ GÉOGRAPHIE"),
      selectInput("f_region","Région :",choices=c("Toutes",levels(df$region)),selected="Toutes",width="100%"),
      selectInput("f_dept","Département :",choices=c("Tous",levels(df$department)),selected="Tous",width="100%"),

      tags$p(style="color:#7ab8d4;font-size:9px;font-weight:700;margin:5px 0 2px;","▸ POPULATION"),
      selectInput("f_sexe","Sexe :",choices=c("Tous","Male","Female"),selected="Tous",width="100%"),
      sliderInput("f_age","Âge (mois):",min=0,max=60,value=c(0,60),step=1,width="100%"),
      selectInput("f_age_group","Groupe d'âge:",
                  choices=c("Tous","0-5m","6-11m","12-17m","18-23m","24-35m","36-47m","48-59m","60+m"),
                  selected="Tous",width="100%"),
      selectInput("f_depl","Statut déplacement:",choices=c("Tous",levels(df$displacement_status)),selected="Tous",width="100%"),

      tags$p(style="color:#7ab8d4;font-size:9px;font-weight:700;margin:5px 0 2px;","▸ ENQUÊTE"),
      selectInput("f_round","Survey round:",choices=c("Tous",sort(unique(as.character(df$survey_round)))),selected="Tous",width="100%"),
      selectInput("f_nutr","Statut nutritionnel:",choices=c("Tous",levels(df$nutr_cat)),selected="Tous",width="100%"),

      tags$p(style="color:#7ab8d4;font-size:9px;font-weight:700;margin:5px 0 2px;","▸ CONTEXTE"),
      selectInput("f_conflict","Exposition conflit:",choices=c("Tous",levels(df$conflict_exposure)),selected="Tous",width="100%"),
      selectInput("f_fcs","Catégorie FCS:",choices=c("Tous",levels(df$fcs_cat)),selected="Tous",width="100%"),
      selectInput("f_vulner","Vulnérabilité:",choices=c("Tous",levels(df$cat_vulner)),selected="Tous",width="100%"),

      tags$div(style="margin-top:10px;",
        actionButton("reset_filters","🔄 Réinitialiser",
                     style="width:100%;background:#1a6a99;color:white;border:none;
                            border-radius:5px;padding:7px;font-size:11px;font-weight:600;")),
      tags$div(style="margin-top:8px;padding:7px;background:rgba(0,141,201,0.18);
               border-radius:5px;text-align:center;border:1px solid #1a6a99;",
        textOutput("sidebar_n"))
    ),
    tags$hr(style="border-color:#1a6a99;margin:0;"),

    sidebarMenu(id="menu",
      menuItem("🏠  Tableau de bord",      tabName="accueil",  icon=icon("home")),
      menuItem("🔍  Exploration données",  tabName="explor",   icon=icon("table")),
      menuItem("📊  Statistiques",         tabName="stats",    icon=icon("chart-bar"),
        menuSubItem("Univariée",           tabName="uni",      icon=icon("chart-simple")),
        menuSubItem("Bivariée",            tabName="biv",      icon=icon("chart-line")),
        menuSubItem("Descriptive OMS",     tabName="descoms",  icon=icon("stethoscope"))
      ),
      menuItem("🧪  Inférentiel",          tabName="infer",    icon=icon("flask")),
      menuItem("🎯  Facteurs de risque",   tabName="logistic", icon=icon("magnifying-glass-chart")),
      menuItem("🤖  Machine Learning",     tabName="ml",       icon=icon("robot")),
      menuItem("📈  Tendances & Projections",tabName="trends", icon=icon("chart-line")),
      menuItem("🗺️  Cartographie",         tabName="carto",    icon=icon("map")),
      menuItem("📋  Indicateurs OMS",      tabName="oms",      icon=icon("heart-pulse")),
      menuItem("🏥  Ciblage humanitaire",  tabName="target",   icon=icon("bullseye")),
      menuItem("🌐  Contexte régional",    tabName="context",  icon=icon("globe")),
      menuItem("📝  Rapport automatique",  tabName="report",   icon=icon("file-pdf"))
    ),

    tags$div(style="position:absolute;bottom:12px;left:0;right:0;padding:0 12px;
             font-size:10px;color:#5a9ec4;text-align:center;",
      tags$hr(style="border-color:#1a6a99;"),
      "📡 Enquête SMART Cameroun 2024",tags$br(),
      "Normes OMS 2006 | UNICEF Framework")
  ),

  # ── Body ───────────────────────────────────────────────────────────────────
  dashboardBody(
    tags$style(css_oms),
    useShinyjs(),

    tabItems(

      # ══════════════════════════════════════════════════
      # 0 — TABLEAU DE BORD (Accueil)
      # ══════════════════════════════════════════════════
      tabItem(tabName="accueil",
        tags$div(
          style="background:linear-gradient(135deg,#001f3f,#008DC9);color:white;
                 padding:22px 28px;border-radius:12px;margin-bottom:20px;
                 box-shadow:0 4px 15px rgba(0,0,0,0.3);",
          tags$div(style="display:flex;justify-content:space-between;align-items:center;",
            tags$div(
              tags$h2(style="margin:0;font-weight:700;font-size:1.5rem;",
                      "🌍 Tableau de bord nutritionnel — Cameroun"),
              tags$p(style="margin:6px 0 0;font-size:14px;opacity:0.9;",
                     "Enquête SMART | Régions : Adamaoua · Nord · Extrême-Nord | Normes OMS 2006")
            ),
            tags$div(style="text-align:right;",
              tags$p(style="font-size:20px;font-weight:700;margin:0;","NUTRICAM"),
              tags$p(style="font-size:11px;opacity:0.8;margin:0;","Système de surveillance nutritionnelle")
            )
          )
        ),

        # KPI Row 1
        fluidRow(
          valueBoxOutput("kpi_n",       width=2),
          valueBoxOutput("kpi_gam",     width=2),
          valueBoxOutput("kpi_sam",     width=2),
          valueBoxOutput("kpi_mam",     width=2),
          valueBoxOutput("kpi_stunt",   width=2),
          valueBoxOutput("kpi_under",   width=2)
        ),
        # KPI Row 2
        fluidRow(
          valueBoxOutput("kpi_muac",    width=2),
          valueBoxOutput("kpi_muac_sam",width=2),
          valueBoxOutput("kpi_fcs",     width=2),
          valueBoxOutput("kpi_icv",     width=2),
          valueBoxOutput("kpi_whz",     width=2),
          valueBoxOutput("kpi_risk",    width=2)
        ),

        fluidRow(
          box(title="📊 Prévalences GAM/SAM/Stunting par région (IC 95%)",
              status="primary",solidHeader=TRUE,width=7,
              plotlyOutput("accueil_prev",height="300px")),
          box(title="🚦 Tableau de seuils OMS",
              status="success",solidHeader=TRUE,width=5,
              tableOutput("seuils_table"))
        ),
        fluidRow(
          box(title="📉 Distribution des scores Z vs référence OMS N(0,1)",
              status="primary",solidHeader=TRUE,width=8,
              plotlyOutput("accueil_zscore",height="270px")),
          box(title="🥧 Statut MUAC",
              status="warning",solidHeader=TRUE,width=4,
              plotlyOutput("accueil_muac_pie",height="270px"))
        ),
        fluidRow(
          box(title="🔥 Carte thermique des risques par département",
              status="danger",solidHeader=TRUE,width=6,
              plotlyOutput("accueil_heatmap",height="280px")),
          box(title="📅 Évolution temporelle des indicateurs",
              status="info",solidHeader=TRUE,width=6,
              plotlyOutput("accueil_trend_mini",height="280px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # 1 — EXPLORATION
      # ══════════════════════════════════════════════════
      tabItem(tabName="explor",
        fluidRow(
          box(title="🔎 Explorateur de données",status="primary",solidHeader=TRUE,width=12,
            fluidRow(
              column(3,selectInput("explor_cols","Colonnes à afficher:",choices=names(df),
                selected=c("region","department","child_sex","child_age_months",
                           "whz_score","haz_score","muac_mm","GAM","nutr_cat"),
                multiple=TRUE,width="100%")),
              column(2,numericInput("explor_n","Lignes/page:",15,5,100,5)),
              column(4,tags$div(style="margin-top:22px;",
                downloadButton("dl_data","📥 CSV",
                  style="background:#008DC9;color:white;border:none;border-radius:4px;
                         font-size:11px;font-weight:600;padding:6px 10px;margin-right:4px;"),
                downloadButton("dl_excel","📊 Excel",
                  style="background:#1d7344;color:white;border:none;border-radius:4px;
                         font-size:11px;font-weight:600;padding:6px 10px;margin-right:4px;"),
                downloadButton("dl_excel_multi","📋 Excel multi-feuilles",
                  style="background:#1e5ba8;color:white;border:none;border-radius:4px;
                         font-size:11px;font-weight:600;padding:6px 10px;")))
            ),
            DTOutput("explor_table")
          )
        ),
        fluidRow(
          box(title="📐 Statistiques descriptives",status="info",solidHeader=TRUE,width=5,
            selectInput("explor_var","Variable:",choices=vars_num,selected="whz_score",width="100%"),
            tableOutput("explor_summary")),
          box(title="📊 Distribution",status="info",solidHeader=TRUE,width=7,
            plotlyOutput("explor_dist",height="260px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # 2a — UNIVARIÉ
      # ══════════════════════════════════════════════════
      tabItem(tabName="uni",
        fluidRow(
          box(width=3,status="primary",solidHeader=TRUE,title="⚙️ Paramètres",
            selectInput("uni_type","Type de graphique:",choices=c(
              "Barplot (qual.)","Camembert / Donut","Histogramme + Gauss",
              "Boxplot annoté","Violin + Boxplot","Courbe de densité",
              "QQ-plot (normalité)","Densité vs OMS N(0,1)","Ridgeline par région"),
              width="100%"),
            uiOutput("uni_var_ui"),
            selectInput("uni_palette","Palette:",
              choices=c("OMS vert/rouge","UNICEF bleu","Viridis","Spectral"),width="100%"),
            checkboxInput("uni_labels","Afficher les effectifs",TRUE),
            hr(),
            h5("📐 Statistiques",style="color:#008DC9;font-weight:700;"),
            tableOutput("uni_stats")
          ),
          box(width=9,status="primary",solidHeader=TRUE,title="📈 Graphique",
            dl_bar("uni_plot","statistique_univariee"),
            plotlyOutput("uni_plot",height="480px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # 2b — BIVARIÉ
      # ══════════════════════════════════════════════════
      tabItem(tabName="biv",
        fluidRow(
          box(width=3,status="primary",solidHeader=TRUE,title="⚙️ Paramètres bivariés",
            selectInput("biv_type","Type:",choices=c(
              "Barplot groupé (dodge)","Barplot empilé (%)","Boxplot par groupe",
              "Violin par groupe","Nuage de points + LOESS","Densités superposées",
              "Histogrammes facettes","Heatmap de fréquences","Alluvial (flux)"),
              width="100%"),
            selectInput("biv_x","Variable X / Groupe:",choices=vars_cat,selected="region",width="100%"),
            uiOutput("biv_y_ui"),
            selectInput("biv_pal","Palette:",
              choices=c("OMS régions","UNICEF bleu","Viridis","Set2"),width="100%"),
            hr(),
            h5("📐 Test statistique",style="color:#008DC9;font-weight:700;"),
            verbatimTextOutput("biv_test")
          ),
          box(width=9,status="primary",solidHeader=TRUE,title="📈 Graphique bivarié",
            dl_bar("biv_plot","statistique_bivariee"),
            plotlyOutput("biv_plot",height="480px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # 2c — DESCRIPTIVE OMS
      # ══════════════════════════════════════════════════
      tabItem(tabName="descoms",
        fluidRow(
          box(title="📋 Tableau descriptif complet — Style OMS (gtsummary)",
              status="primary",solidHeader=TRUE,width=12,
            fluidRow(
              column(4,selectInput("desc_by","Stratifier par:",
                choices=c("Aucune stratification"="none","Région"="region",
                          "Sexe"="child_sex","Déplacement"="displacement_status"),
                width="100%")),
              column(8,tags$div(style="margin-top:25px;",
                downloadButton("dl_gt_html","🌐 HTML",
                  style="background:#008DC9;color:white;border:none;border-radius:4px;font-size:11px;font-weight:600;padding:6px 12px;margin-right:4px;"),
                downloadButton("dl_gt_csv","📥 CSV",
                  style="background:#1d7344;color:white;border:none;border-radius:4px;font-size:11px;font-weight:600;padding:6px 12px;margin-right:4px;"),
                downloadButton("dl_gt_excel","📊 Excel",
                  style="background:#1e5ba8;color:white;border:none;border-radius:4px;font-size:11px;font-weight:600;padding:6px 12px;")))
            ),
            uiOutput("desc_gt_out")
          )
        ),
        fluidRow(
          box(title="🔗 Matrice de corrélation Spearman",status="info",solidHeader=TRUE,width=6,
            downloadButton("dl_corrplot","⬇ Télécharger PNG",
              style="margin-bottom:8px;background:#008DC9;color:white;border:none;border-radius:4px;font-size:11px;padding:5px 10px;"),
            plotOutput("desc_corr",height="420px")),
          box(title="📦 Scores Z vs référence OMS par région",status="warning",solidHeader=TRUE,width=6,
            dl_bar("desc_zscore","scores_z_region"),
            plotlyOutput("desc_zscore",height="420px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # 3 — INFÉRENTIEL
      # ══════════════════════════════════════════════════
      tabItem(tabName="infer",
        fluidRow(
          tabBox(width=12,title="🧪 Tests statistiques",
            tabPanel("Chi² / Fisher",
              fluidRow(
                column(4,
                  selectInput("chi2_outcome","Outcome:",
                    choices=c("GAM","SAM","Stunting","Underweight","MUAC_GAM"),selected="GAM"),
                  checkboxGroupInput("chi2_vars","Variables à tester:",
                    choices=c("child_sex","displacement_status","head_gender","head_education",
                              "water_source","sanitation_type","shelter_type","diarrhea_last_2w",
                              "fever_last_2w","malaria_test","vaccination_status","food_aid_received",
                              "climate_shock","conflict_exposure","crop_failure","livestock_loss","price_shock"),
                    selected=c("child_sex","displacement_status","vaccination_status","diarrhea_last_2w","water_source"))),
                column(8,DTOutput("chi2_table"))
              )),
            tabPanel("Mann-Whitney",
              fluidRow(
                column(4,
                  selectInput("mw_outcome","Groupe:",choices=c("GAM","SAM","Stunting"),selected="GAM"),
                  checkboxGroupInput("mw_vars","Variables continues:",
                    choices=c("child_age_months","food_consumption_score","household_hunger_scale",
                              "muac_mm","mother_muac","anc_visits","health_facility_distance_km",
                              "household_size","coping_strategy_index","ICV"),
                    selected=c("muac_mm","food_consumption_score","household_hunger_scale","ICV"))),
                column(8,DTOutput("mw_table"),br(),dl_bar("mw_plot","mann_whitney"),plotlyOutput("mw_plot",height="280px"))
              )),
            tabPanel("Kruskal-Wallis",
              fluidRow(
                column(4,
                  selectInput("kw_var","Variable continue:",
                    choices=c("whz_score","haz_score","waz_score","muac_mm","food_consumption_score","ICV"),
                    selected="whz_score"),
                  selectInput("kw_group","Groupe:",
                    choices=c("region","child_sex","displacement_status","fcs_cat","age_group"),
                    selected="region"),
                  verbatimTextOutput("kw_result"),
                  h5("Post-hoc (BH):",style="color:#008DC9;"),
                  verbatimTextOutput("kw_posthoc"),
                  downloadButton("dl_kw_results","📄 Résultats TXT",
                    style="background:#6c757d;color:white;border:none;border-radius:4px;
                           padding:5px 10px;font-size:11px;font-weight:600;width:100%;")),
                column(8,dl_bar("kw_plot","kruskal_wallis"),plotlyOutput("kw_plot",height="400px"))
              )),
            tabPanel("Corrélation Spearman",
              fluidRow(
                column(4,
                  selectInput("cor_x","Variable X:",choices=vars_num,selected="whz_score"),
                  selectInput("cor_y","Variable Y:",choices=vars_num,selected="muac_mm"),
                  selectInput("cor_color","Couleur par:",choices=c("Aucune",vars_cat),selected="region"),
                  verbatimTextOutput("cor_result")),
                column(8,dl_bar("cor_plot","correlation"),plotlyOutput("cor_plot",height="400px"))
              ))
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # 4 — RÉGRESSION LOGISTIQUE (NOUVEAU)
      # ══════════════════════════════════════════════════
      tabItem(tabName="logistic",
        fluidRow(
          box(title="🎯 Analyse des facteurs de risque — Régression logistique multivariée",
              status="danger",solidHeader=TRUE,width=12,
            tags$div(style="background:#fff3cd;border-left:4px solid #ffc107;padding:10px 14px;
                     margin-bottom:14px;border-radius:4px;font-size:13px;",
              "📌 La régression logistique permet d'identifier les facteurs de risque indépendants
               de malnutrition, en contrôlant pour les facteurs confondants (standard OMS/SMART)."),
            fluidRow(
              column(3,
                selectInput("log_outcome","Variable outcome:",
                  choices=c("GAM","SAM","Stunting","Underweight","MUAC_GAM"),selected="GAM"),
                checkboxGroupInput("log_vars","Variables prédictives:",
                  choices=c("child_age_months","child_sex","household_size","head_education",
                            "displacement_status","water_source","sanitation_type","shelter_type",
                            "food_consumption_score","household_hunger_scale","coping_strategy_index",
                            "diarrhea_last_2w","fever_last_2w","malaria_test","vaccination_status",
                            "vitamin_a","breastfeeding_status","mother_muac","anc_visits",
                            "health_facility_distance_km","nutrition_program_access",
                            "conflict_exposure","climate_shock","food_aid_received"),
                  selected=c("child_age_months","child_sex","household_size","water_source",
                             "food_consumption_score","household_hunger_scale","vaccination_status",
                             "diarrhea_last_2w","displacement_status","conflict_exposure")),
                hr(),
                checkboxInput("log_interact","Inclure interactions région*sexe",FALSE),
                actionButton("run_logistic","▶ Lancer la régression",
                  style="width:100%;background:#c9161d;color:white;border:none;
                         border-radius:5px;padding:8px;font-weight:700;margin-top:8px;"),
                hr(),
                downloadButton("dl_logistic","📊 Résultats Excel",
                  style="background:#1d7344;color:white;border:none;border-radius:4px;
                         padding:5px 10px;font-size:11px;font-weight:600;width:100%;")
              ),
              column(9,
                tabBox(width=12,
                  tabPanel("🎯 Forest plot OR (IC 95%)",
                    dl_bar("logistic_forest","odds_ratio_forest_plot"),
                    plotlyOutput("logistic_forest",height="500px")),
                  tabPanel("📋 Tableau des OR",DTOutput("logistic_table")),
                  tabPanel("📈 Courbe ROC",
                    plotlyOutput("logistic_roc",height="400px"),
                    verbatimTextOutput("logistic_auc")),
                  tabPanel("📊 Résumé modèle",verbatimTextOutput("logistic_summary"))
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # 5 — MACHINE LEARNING (NOUVEAU)
      # ══════════════════════════════════════════════════
      tabItem(tabName="ml",
        fluidRow(
          box(title="🤖 Segmentation & Classification — Clustering K-moyennes",
              status="purple",solidHeader=TRUE,width=12,
            tags$div(style="background:#ede7f6;border-left:4px solid #7b2d8b;padding:10px 14px;
                     margin-bottom:14px;border-radius:4px;font-size:13px;",
              "📌 Le clustering identifie des profils nutritionnels distincts pour cibler les interventions."),
            fluidRow(
              column(3,
                checkboxGroupInput("ml_vars","Variables pour le clustering:",
                  choices=c("whz_score","haz_score","waz_score","muac_mm","food_consumption_score",
                            "household_hunger_scale","coping_strategy_index","ICV",
                            "health_facility_distance_km","anc_visits","risk_score"),
                  selected=c("whz_score","haz_score","muac_mm","food_consumption_score",
                             "household_hunger_scale","ICV")),
                numericInput("ml_k","Nombre de clusters (k):",value=4,min=2,max=8),
                actionButton("run_cluster","🔬 Lancer le clustering",
                  style="width:100%;background:#7b2d8b;color:white;border:none;
                         border-radius:5px;padding:8px;font-weight:700;margin-top:8px;")
              ),
              column(9,
                tabBox(width=12,
                  tabPanel("🗺️ Graphique des clusters (PCA)",
                    dl_bar("ml_cluster_plot","clustering_pca"),
                    plotlyOutput("ml_cluster_plot",height="450px")),
                  tabPanel("📊 Profil des clusters",
                    plotlyOutput("ml_cluster_profile",height="450px")),
                  tabPanel("📋 Tableau des centres",DTOutput("ml_cluster_table")),
                  tabPanel("📐 Méthode du coude",
                    plotlyOutput("ml_elbow",height="350px"))
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # 6 — TENDANCES & PROJECTIONS (NOUVEAU)
      # ══════════════════════════════════════════════════
      tabItem(tabName="trends",
        fluidRow(
          box(title="📈 Analyse des tendances temporelles & Projections",
              status="info",solidHeader=TRUE,width=12,
            tags$div(style="background:#e3f2fd;border-left:4px solid #008DC9;padding:10px 14px;
                     margin-bottom:14px;border-radius:4px;font-size:13px;",
              "📌 Analyse des tendances par survey round et projections pour les prochains cycles.
               Indispensable pour le suivi programmatique OMS/UNICEF."),
            fluidRow(
              column(3,
                checkboxGroupInput("trend_inds","Indicateurs à tracer:",
                  choices=c("GAM","SAM","MAM","Stunting","Sev_Stunting","Underweight","MUAC_GAM"),
                  selected=c("GAM","SAM","Stunting")),
                selectInput("trend_group","Stratifier par:",
                  choices=c("Global","Région"="region","Sexe"="child_sex",
                            "Déplacement"="displacement_status"),
                  selected="Global"),
                checkboxInput("trend_projection","Afficher projections (LOESS)",TRUE),
                checkboxInput("trend_threshold","Afficher seuils OMS",TRUE),
                hr(),
                downloadButton("dl_trends","📊 Données tendances Excel",
                  style="background:#1d7344;color:white;border:none;border-radius:4px;
                         padding:5px 10px;font-size:11px;font-weight:600;width:100%;")
              ),
              column(9,
                tabBox(width=12,
                  tabPanel("📈 Tendances prévalences",
                    dl_bar("trend_main","tendances_indicateurs"),
                    plotlyOutput("trend_main",height="420px")),
                  tabPanel("📊 Comparaison baseline vs endline",
                    plotlyOutput("trend_compare",height="420px")),
                  tabPanel("📉 Scores Z dans le temps",
                    plotlyOutput("trend_zscore",height="420px")),
                  tabPanel("🗂️ Tableau des variations",DTOutput("trend_table"))
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # 7 — CARTOGRAPHIE
      # ══════════════════════════════════════════════════
      tabItem(tabName="carto",
        fluidRow(
          tabBox(width=12,title="🗺️ Cartographie nutritionnelle",
            tabPanel("🗺️ Carte Leaflet interactive",
              fluidRow(
                column(3,
                  selectInput("leaf_var","Indicateur:",
                    choices=c("GAM (%)"="GAM_pct","SAM (%)"="SAM_pct",
                              "Stunting (%)"="Stunting_pct","MUAC moyen (mm)"="muac_moy",
                              "ICV moyen"="icv_moy","Risque moyen"="risk_moy"),
                    selected="GAM_pct"),
                  selectInput("leaf_niv","Niveau:",choices=c("Région","Département"),selected="Région"),
                  selectInput("leaf_fond","Fond de carte:",
                    choices=c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","CartoDB.DarkMatter"),
                    selected="CartoDB.Positron"),
                  checkboxInput("leaf_pts","Points GPS individus",TRUE),
                  checkboxInput("leaf_gadm","Polygones GADM",FALSE),
                  hr(),
                  downloadButton("dl_leaf_excel","📊 Excel indicateurs",
                    style="background:#1d7344;color:white;border:none;border-radius:4px;padding:5px 10px;font-size:11px;font-weight:600;width:100%;margin-bottom:4px;"),
                  downloadButton("dl_leaf_csv","📥 CSV coordonnées",
                    style="background:#008DC9;color:white;border:none;border-radius:4px;padding:5px 10px;font-size:11px;font-weight:600;width:100%;")
                ),
                column(9,leafletOutput("leaf_map",height="520px"))
              )),
            tabPanel("📊 Carte statique (ggplot2)",
              fluidRow(
                column(3,
                  selectInput("gg_var","Indicateur:",
                    choices=c("GAM (%)"="GAM_pct","SAM (%)"="SAM_pct","Stunting (%)"="Stunting_pct"),
                    selected="GAM_pct"),
                  selectInput("gg_pal","Palette:",choices=c("OMS standard","YlOrRd","Blues","Viridis"),selected="OMS standard"),
                  radioButtons("gg_type","Type:",choices=c("Choroplèthe","Points proportionnels"),selected="Points proportionnels")
                ),
                column(9,dl_bar("gg_map","carte_cameroun"),plotlyOutput("gg_map",height="520px"))
              )),
            tabPanel("🌍 Contexte régional africain",
              fluidRow(
                column(3,
                  selectInput("world_highlight","Mise en évidence:",
                    choices=c("Cameroun seul","Afrique centrale","Afrique subsaharienne"),
                    selected="Afrique centrale")
                ),
                column(9,dl_bar("world_map","carte_monde"),plotlyOutput("world_map",height="520px"))
              )),
            tabPanel("📍 Points GPS détaillés",
              fluidRow(
                column(3,
                  selectInput("gps_color","Couleur par:",
                    choices=c("Statut nutritionnel"="nutr_cat","Région"="region","Sexe"="child_sex","Risque"="risk_cat"),
                    selected="nutr_cat"),
                  checkboxInput("gps_cluster","Regrouper (cluster)",TRUE),
                  sliderInput("gps_size","Taille:",3,10,5,1)
                ),
                column(9,leafletOutput("gps_map",height="520px"))
              ))
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # 8 — INDICATEURS OMS
      # ══════════════════════════════════════════════════
      tabItem(tabName="oms",
        fluidRow(
          box(title="📋 Prévalences avec IC 95% (Wilson) — Normes OMS 2006",
              status="primary",solidHeader=TRUE,width=12,
            fluidRow(
              column(3,
                checkboxGroupInput("oms_groupes","Stratifier par:",
                  choices=c("Global","Région","Sexe","Groupe d'âge","Statut déplacement","Sécurité alim."),
                  selected=c("Global","Région")),
                hr(),
                downloadButton("dl_oms_excel_full","📊 Excel multi-strates",
                  style="background:#1d7344;color:white;border:none;border-radius:4px;padding:5px 10px;font-size:11px;font-weight:600;width:100%;margin-bottom:4px;"),
                downloadButton("dl_oms_csv_full","📥 CSV complet",
                  style="background:#008DC9;color:white;border:none;border-radius:4px;padding:5px 10px;font-size:11px;font-weight:600;width:100%;")
              ),
              column(9,DTOutput("oms_table"))
            )
          )
        ),
        fluidRow(
          box(title="🌳 Forest plot — IC 95%",status="info",solidHeader=TRUE,width=8,
            dl_bar("oms_forest","oms_forest_plot"),plotlyOutput("oms_forest",height="420px")),
          box(title="🎯 Jauge GAM globale",status="warning",solidHeader=TRUE,width=4,
            plotlyOutput("oms_gauge",height="420px"))
        ),
        fluidRow(
          box(title="📅 Évolution par survey round",status="primary",solidHeader=TRUE,width=12,
            dl_bar("oms_trend","oms_evolution"),plotlyOutput("oms_trend",height="280px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # 9 — CIBLAGE HUMANITAIRE (NOUVEAU)
      # ══════════════════════════════════════════════════
      tabItem(tabName="target",
        fluidRow(
          box(title="🏥 Outil de ciblage humanitaire — Priorisation des zones d'intervention",
              status="danger",solidHeader=TRUE,width=12,
            tags$div(style="background:#fce4ec;border-left:4px solid #c9161d;padding:10px 14px;
                     margin-bottom:14px;border-radius:4px;font-size:13px;",
              "📌 Ce module calcule un Score de Priorité d'Intervention (SPI) composite selon les
               standards OMS/PAM/UNICEF pour chaque zone géographique."),
            fluidRow(
              column(3,
                tags$strong("Pondérations du Score de Priorité (%)"),
                numericInput("w_gam",     "Poids GAM prévalence:",     30,0,100,5),
                numericInput("w_sam",     "Poids SAM prévalence:",     25,0,100,5),
                numericInput("w_stunting","Poids Stunting:",            20,0,100,5),
                numericInput("w_fcs",     "Poids sécurité alimentaire:",15,0,100,5),
                numericInput("w_access",  "Poids accès aux services:",  10,0,100,5),
                actionButton("calc_spi","🎯 Calculer le SPI",
                  style="width:100%;background:#c9161d;color:white;border:none;
                         border-radius:5px;padding:8px;font-weight:700;margin-top:8px;"),
                hr(),
                downloadButton("dl_spi","📊 Export SPI Excel",
                  style="background:#1d7344;color:white;border:none;border-radius:4px;
                         padding:5px 10px;font-size:11px;font-weight:600;width:100%;")
              ),
              column(9,
                tabBox(width=12,
                  tabPanel("🏆 Classement priorité",DTOutput("target_ranking")),
                  tabPanel("📊 Graphique SPI",
                    dl_bar("target_chart","spi_priorite"),
                    plotlyOutput("target_chart",height="450px")),
                  tabPanel("🗺️ Carte de priorité",leafletOutput("target_map",height="450px")),
                  tabPanel("📋 Plan d'action suggéré",uiOutput("target_action_plan"))
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # 10 — CONTEXTE RÉGIONAL (NOUVEAU)
      # ══════════════════════════════════════════════════
      tabItem(tabName="context",
        fluidRow(
          box(title="🌐 Analyse des déterminants sociaux — Cadre conceptuel UNICEF",
              status="success",solidHeader=TRUE,width=12,
            tags$div(style="background:#e8f5e9;border-left:4px solid #4dac26;padding:10px 14px;
                     margin-bottom:14px;border-radius:4px;font-size:13px;",
              "📌 Le cadre conceptuel UNICEF distingue causes immédiates, sous-jacentes et basiques
               de la malnutrition. Ce module les analyse systématiquement."),
            tabBox(width=12,
              tabPanel("⚡ Causes immédiates",
                tags$div(style="padding:10px;",
                  tags$h4("Apport alimentaire inadéquat & Maladies infectieuses",
                          style="color:#008DC9;font-weight:700;"),
                  fluidRow(
                    column(6,dl_bar("ctx_immediate_diet","causes_imm_alimentation"),
                           plotlyOutput("ctx_immediate_diet",height="350px")),
                    column(6,dl_bar("ctx_immediate_disease","causes_imm_maladies"),
                           plotlyOutput("ctx_immediate_disease",height="350px"))
                  )
                )),
              tabPanel("🔗 Causes sous-jacentes",
                fluidRow(
                  column(6,dl_bar("ctx_underlying_food","securite_alimentaire"),
                         plotlyOutput("ctx_underlying_food",height="350px")),
                  column(6,dl_bar("ctx_underlying_care","soins"),
                         plotlyOutput("ctx_underlying_care",height="350px"))
                )),
              tabPanel("🏛️ Causes basiques",
                fluidRow(
                  column(6,dl_bar("ctx_basic_capital","capital_ressources"),
                         plotlyOutput("ctx_basic_capital",height="350px")),
                  column(6,dl_bar("ctx_basic_context","contexte_chocs"),
                         plotlyOutput("ctx_basic_context",height="350px"))
                )),
              tabPanel("📊 Analyse multivariée",
                fluidRow(
                  column(5,
                    selectInput("ctx_y","Variable nutritionnelle (Y):",
                      choices=c("whz_score","haz_score","waz_score","muac_mm","ICV"),
                      selected="whz_score"),
                    selectInput("ctx_type","Type d'analyse:",
                      choices=c("Corrélation Spearman","Graphe en radar","Waffle plots"),
                      selected="Graphe en radar")),
                  column(7,plotlyOutput("ctx_multi",height="400px"))
                ))
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # 11 — RAPPORT AUTOMATIQUE (NOUVEAU)
      # ══════════════════════════════════════════════════
      tabItem(tabName="report",
        fluidRow(
          box(title="📝 Générateur de rapport automatique — Standard OMS/SMART",
              status="primary",solidHeader=TRUE,width=12,
            tags$div(style="background:#e3f2fd;border-left:4px solid #008DC9;padding:10px 14px;
                     margin-bottom:14px;border-radius:4px;font-size:13px;",
              "📌 Ce module génère automatiquement un rapport complet au format OMS avec tous
               les indicateurs clés, graphiques et recommandations."),
            fluidRow(
              column(4,
                textInput("rep_titre","Titre du rapport:","Rapport nutritionnel — Cameroun 2024"),
                textInput("rep_auteur","Auteur(s):","[Insérer auteurs]"),
                textInput("rep_org","Organisation:","Ministère de la Santé / OMS Cameroun"),
                textAreaInput("rep_contexte","Contexte (optionnel):","",rows=4),
                checkboxGroupInput("rep_sections","Sections à inclure:",
                  choices=c("Résumé exécutif","Méthodologie","KPI globaux",
                            "Analyse par région","Facteurs de risque",
                            "Cartographie","Tendances","Recommandations"),
                  selected=c("Résumé exécutif","KPI globaux","Analyse par région",
                             "Facteurs de risque","Recommandations")),
                selectInput("rep_langue","Langue:",choices=c("Français","English"),selected="Français"),
                hr(),
                downloadButton("dl_report_html","🌐 Rapport HTML",
                  style="background:#008DC9;color:white;border:none;border-radius:4px;
                         padding:8px 15px;font-size:12px;font-weight:700;width:100%;margin-bottom:6px;"),
                downloadButton("dl_report_data","📊 Données complètes Excel",
                  style="background:#1d7344;color:white;border:none;border-radius:4px;
                         padding:8px 15px;font-size:12px;font-weight:700;width:100%;")
              ),
              column(8,
                h4("📋 Aperçu du rapport",style="color:#008DC9;font-weight:700;margin-bottom:15px;"),
                uiOutput("report_preview")
              )
            )
          )
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
    if (input$f_conflict != "Tous")   d <- filter(d, conflict_exposure   == input$f_conflict)
    if (input$f_fcs      != "Tous")   d <- filter(d, fcs_cat             == input$f_fcs)
    if (input$f_vulner   != "Tous")   d <- filter(d, cat_vulner          == input$f_vulner)
    if (input$f_age_group != "Tous")  d <- filter(d, as.character(age_group) == input$f_age_group)
    filter(d, child_age_months >= input$f_age[1], child_age_months <= input$f_age[2])
  })

  # ── Département réactif ─────────────────────────────────────────────────────
  observe({
    dept_choices <- if (input$f_region == "Toutes") c("Tous", levels(df$department)) else {
      depts <- df |> filter(region==input$f_region) |> pull(department) |> droplevels() |> levels()
      c("Tous", depts)
    }
    updateSelectInput(session,"f_dept",choices=dept_choices,selected="Tous")
  })

  # ── Reset filtres ───────────────────────────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateSelectInput(session,"f_region",  selected="Toutes")
    updateSelectInput(session,"f_dept",    choices=c("Tous",levels(df$department)),selected="Tous")
    updateSelectInput(session,"f_sexe",    selected="Tous")
    updateSelectInput(session,"f_depl",    selected="Tous")
    updateSelectInput(session,"f_round",   selected="Tous")
    updateSelectInput(session,"f_nutr",    selected="Tous")
    updateSelectInput(session,"f_conflict",selected="Tous")
    updateSelectInput(session,"f_fcs",     selected="Tous")
    updateSelectInput(session,"f_vulner",  selected="Tous")
    updateSelectInput(session,"f_age_group",selected="Tous")
    updateSliderInput(session,"f_age",value=c(0,60))
  })

  # ── Compteur N ──────────────────────────────────────────────────────────────
  output$sidebar_n <- renderText({
    n <- nrow(df_f()); tot <- nrow(df); pct <- round(100*n/tot,1)
    paste0("📊 ",format(n,big.mark="\u202f")," / ",format(tot,big.mark="\u202f")," (",pct,"%)")
  })

  # ── Palettes ────────────────────────────────────────────────────────────────
  get_pal_uni <- function(n,nom) switch(nom,
    "OMS vert/rouge" =colorRampPalette(c(oms_vert,oms_bleu,oms_rouge))(n),
    "UNICEF bleu"    =colorRampPalette(c("#e0f4ff",unicef_bleu,"#003d6b"))(n),
    "Viridis"        =viridis(n),
    "Spectral"       =brewer.pal(min(n,11),"Spectral"))

  get_pal_biv <- function(n,nom) switch(nom,
    "OMS régions"  =unname(couleurs_region)[1:min(n,3)],
    "UNICEF bleu"  =colorRampPalette(c("#e0f4ff",unicef_bleu,"#003d6b"))(n),
    "Viridis"      =viridis(n),
    "Set2"         =brewer.pal(min(n,8),"Set2"))

  # ══════════════════════════════════════════════════
  # ACCUEIL — KPI & GRAPHIQUES
  # ══════════════════════════════════════════════════
  output$kpi_n <- renderValueBox(
    valueBox(format(nrow(df_f()),big.mark=" "),"Enfants enquêtés",icon=icon("users"),color="blue"))

  output$kpi_gam <- renderValueBox({
    p <- round(mean(df_f()$GAM,na.rm=T)*100,1)
    valueBox(paste0(p,"%"),"GAM (WHZ<-2 ou œdème)",icon=icon("triangle-exclamation"),
             color=if(p>=15)"red" else if(p>=10)"orange" else "green")})

  output$kpi_sam <- renderValueBox({
    p <- round(mean(df_f()$SAM,na.rm=T)*100,1)
    valueBox(paste0(p,"%"),"SAM (WHZ<-3 ou œdème)",icon=icon("heart-pulse"),color="red")})

  output$kpi_mam <- renderValueBox({
    p <- round(mean(df_f()$MAM,na.rm=T)*100,1)
    valueBox(paste0(p,"%"),"MAM (WHZ -3 à -2)",icon=icon("heart"),color="orange")})

  output$kpi_stunt <- renderValueBox({
    p <- round(mean(df_f()$Stunting,na.rm=T)*100,1)
    valueBox(paste0(p,"%"),"Stunting (HAZ<-2)",icon=icon("child"),
             color=if(p>=30)"red" else "orange")})

  output$kpi_under <- renderValueBox({
    p <- round(mean(df_f()$Underweight,na.rm=T)*100,1)
    valueBox(paste0(p,"%"),"Insuff. pondérale (WAZ<-2)",icon=icon("weight-hanging"),color="yellow")})

  output$kpi_muac <- renderValueBox({
    m <- round(mean(df_f()$muac_mm,na.rm=T),1)
    valueBox(paste0(m," mm"),"MUAC moyen",icon=icon("ruler-horizontal"),color="blue")})

  output$kpi_muac_sam <- renderValueBox({
    p <- round(mean(df_f()$MUAC_SAM,na.rm=T)*100,1)
    valueBox(paste0(p,"%"),"MUAC SAM (<115mm)",icon=icon("circle-exclamation"),color="red")})

  output$kpi_fcs <- renderValueBox({
    m <- round(mean(df_f()$food_consumption_score,na.rm=T),1)
    valueBox(m,"FCS moyen",icon=icon("utensils"),color="green")})

  output$kpi_icv <- renderValueBox({
    m <- round(mean(df_f()$ICV,na.rm=T),1)
    valueBox(paste0(m,"/100"),"ICV (vulnérabilité)",icon=icon("shield-halved"),
             color=if(m<40)"red" else if(m<60)"orange" else "green")})

  output$kpi_whz <- renderValueBox({
    m <- round(mean(df_f()$whz_score,na.rm=T),2)
    valueBox(m,"WHZ moyen",icon=icon("chart-line"),
             color=if(m< -2)"red" else if(m< -1)"orange" else "green")})

  output$kpi_risk <- renderValueBox({
    m <- round(mean(df_f()$risk_score,na.rm=T),1)
    valueBox(paste0(m,"/100"),"Score de risque",icon=icon("exclamation-triangle"),
             color=if(m>75)"red" else if(m>50)"orange" else "green")})

  output$seuils_table <- renderTable({
    tibble(
      `Niveau`=c("🟢 Acceptable","🟡 Alerte","🟠 Sérieux","🔴 Critique","🟥 Urgence max"),
      `GAM (%)`=c("< 5","5 – 9,9","10 – 14,9","15 – 29,9","≥ 30"),
      `Sévérité`=c("Situation normale","Surveillance renforcée","Intervention requise","Urgence humanitaire","Crise grave")
    )
  }, striped=TRUE, bordered=TRUE)

  output$accueil_prev <- renderPlotly({
    d <- df_f(); inds <- c("GAM","SAM","Stunting")
    res <- d |> group_by(region) |>
      group_modify(~map_dfr(inds,function(v) calc_prev(.x,v,v))) |> ungroup()
    p <- ggplot(res,aes(x=region,y=`Prév. (%)`,fill=Indicateur,
                        text=paste0(region," — ",Indicateur,"\n",`Prév. (%)`,"% [",`IC95 bas`,"-",`IC95 haut`,"%]\nN=",N))) +
      geom_col(position="dodge",width=0.7,alpha=0.88) +
      geom_errorbar(aes(ymin=`IC95 bas`,ymax=`IC95 haut`),position=position_dodge(0.7),width=0.25,color="gray30") +
      geom_hline(yintercept=15,linetype="dashed",color=oms_rouge,linewidth=0.7) +
      scale_fill_manual(values=c(GAM=oms_rouge,SAM=oms_rouge_f,Stunting=oms_bleu)) +
      labs(x="",y="Prévalence (%)") + theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") |> layout(legend=list(orientation="h",y=-0.2)) |>
      cfg_dl("accueil_prevalences")
  })

  output$accueil_zscore <- renderPlotly({
    d <- df_f(); x_ref <- seq(-4,4,length.out=200)
    ref_df <- tibble(x=x_ref,y=dnorm(x_ref))
    df_long <- d |> select(region,whz_score,haz_score,waz_score) |>
      pivot_longer(-region,names_to="Score",values_to="Val") |>
      mutate(Score=recode(Score,whz_score="WHZ",haz_score="HAZ",waz_score="WAZ"))
    p <- ggplot(df_long,aes(x=Val,fill=Score,color=Score)) +
      geom_density(alpha=0.3,linewidth=0.8) +
      geom_line(data=ref_df,aes(x=x,y=y),color="black",linewidth=1,linetype="dashed",inherit.aes=FALSE) +
      geom_vline(xintercept=c(-2,-3),linetype="dashed",color=c(oms_rouge,oms_rouge_f),linewidth=0.7) +
      scale_fill_manual(values=c(WHZ=oms_rouge,HAZ=oms_bleu,WAZ=oms_vert)) +
      scale_color_manual(values=c(WHZ=oms_rouge,HAZ=oms_bleu,WAZ=oms_vert)) +
      labs(x="Score Z",y="Densité") + theme_minimal(base_size=11)
    ggplotly(p) |> layout(hovermode="x unified",legend=list(orientation="h",y=-0.2)) |>
      cfg_dl("accueil_scores_z")
  })

  output$accueil_muac_pie <- renderPlotly({
    cnt <- df_f() |> count(muac_cat)
    plot_ly(cnt,labels=~muac_cat,values=~n,type="pie",hole=0.5,textinfo="label+percent",
            marker=list(colors=c(oms_vert,oms_orange,oms_rouge),line=list(color="white",width=2)),
            hovertemplate="%{label}<br>N=%{value}<br>%{percent}<extra></extra>") |>
      layout(showlegend=TRUE,legend=list(orientation="h",y=-0.1)) |>
      cfg_dl("muac_statut")
  })

  output$accueil_heatmap <- renderPlotly({
    d <- df_f()
    heat_data <- d |> group_by(region,department) |>
      summarise(GAM_pct=round(mean(GAM,na.rm=T)*100,1),n=n(),.groups="drop")
    p <- ggplot(heat_data,aes(x=department,y=region,fill=GAM_pct,
                              text=paste0(department," (",region,")\nGAM=",GAM_pct,"%\nN=",n))) +
      geom_tile(color="white",linewidth=0.8) +
      geom_text(aes(label=paste0(GAM_pct,"%")),size=3,fontface="bold",
                color=ifelse(heat_data$GAM_pct>20,"white","black")) +
      scale_fill_gradientn(colors=c("white",oms_jaune,oms_orange,oms_rouge,oms_rouge_f),
                           name="GAM (%)",limits=c(0,50),na.value="grey90") +
      labs(x="",y="") + theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=35,hjust=1,size=9))
    ggplotly(p,tooltip="text") |> cfg_dl("heatmap_risques")
  })

  output$accueil_trend_mini <- renderPlotly({
    d <- df_f()
    res <- d |> group_by(survey_round) |>
      summarise(GAM=round(mean(GAM,na.rm=T)*100,1),
                SAM=round(mean(SAM,na.rm=T)*100,1),
                Stunting=round(mean(Stunting,na.rm=T)*100,1),.groups="drop") |>
      pivot_longer(-survey_round,names_to="Indicateur",values_to="Prévalence")
    p <- ggplot(res,aes(x=survey_round,y=Prévalence,color=Indicateur,group=Indicateur,
                        text=paste0("Round: ",survey_round,"\n",Indicateur,": ",Prévalence,"%"))) +
      geom_line(linewidth=1.2) + geom_point(size=3) +
      geom_hline(yintercept=15,linetype="dashed",color=oms_rouge,alpha=0.7) +
      scale_color_manual(values=c(GAM=oms_rouge,SAM=oms_rouge_f,Stunting=oms_bleu)) +
      labs(x="Survey round",y="Prévalence (%)") + theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") |>
      layout(hovermode="x unified",legend=list(orientation="h",y=-0.2)) |>
      cfg_dl("tendances_mini")
  })

  # ══════════════════════════════════════════════════
  # EXPLORATION
  # ══════════════════════════════════════════════════
  output$explor_table <- renderDT({
    req(input$explor_cols)
    df_f() |> select(any_of(input$explor_cols)) |>
      datatable(rownames=FALSE,filter="top",extensions="Buttons",
        options=list(pageLength=input$explor_n,scrollX=TRUE,dom="Bfrtip",
          buttons=list(
            list(extend="csv",text="📥 CSV",filename=paste0("donnees_",Sys.Date()),
                 exportOptions=list(modifier=list(page="all"))),
            list(extend="excel",text="📊 Excel",filename=paste0("donnees_",Sys.Date()),
                 title="Données nutritionnelles — Cameroun",
                 exportOptions=list(modifier=list(page="all"))),
            list(extend="pdf",text="📄 PDF",filename=paste0("donnees_",Sys.Date()),
                 orientation="landscape",pageSize="A3"),
            list(extend="copy",text="📋 Copier"),
            list(extend="print",text="🖨️ Imprimer"))),
        class="compact stripe hover") |>
      formatStyle(columns=character(0),fontSize="12px")
  })

  output$explor_summary <- renderTable({
    v <- na.omit(df_f()[[input$explor_var]])
    tibble(
      Statistique=c("N","Moyenne","Médiane","Mode","Écart-type","CV (%)","Min","Max","Q1","Q3","IQR","Asymétrie","Aplatissement"),
      Valeur=c(length(v),round(mean(v),3),round(median(v),3),round(mode_stat(v),3),round(sd(v),3),cv_stat(v),
               round(min(v),3),round(max(v),3),round(quantile(v,.25),3),round(quantile(v,.75),3),
               round(IQR(v),3),round(skewness(v,na.rm=T),3),round(kurtosis(v,na.rm=T),3))
    )
  },striped=TRUE,hover=TRUE,bordered=TRUE,digits=3)

  output$explor_dist <- renderPlotly({
    d <- df_f(); v <- input$explor_var
    m <- mean(d[[v]],na.rm=T); s <- sd(d[[v]],na.rm=T)
    p <- ggplot(d,aes(x=.data[[v]])) +
      geom_histogram(aes(y=after_stat(density)),bins=25,fill=oms_bleu,color="white",alpha=0.8) +
      stat_function(fun=dnorm,args=list(mean=m,sd=s),color="black",linewidth=1.1) +
      geom_vline(xintercept=m,color=oms_rouge,linewidth=1,linetype="dashed") +
      labs(x=v,y="Densité") + theme_minimal(base_size=11)
    ggplotly(p) |> cfg_dl("exploration_distribution")
  })

  output$dl_data <- downloadHandler(
    filename=function() paste0("donnees_nutrition_cameroun_",Sys.Date(),".csv"),
    content=function(file) write_csv(df_f(),file))

  output$dl_excel <- downloadHandler(
    filename=function() paste0("donnees_nutrition_cameroun_",Sys.Date(),".xlsx"),
    content=function(file) writexl::write_xlsx(df_f()|>mutate(across(where(is.logical),as.integer)),file))

  output$dl_excel_multi <- downloadHandler(
    filename=function() paste0("rapport_nutrition_cameroun_",Sys.Date(),".xlsx"),
    content=function(file) {
      d <- df_f()
      writexl::write_xlsx(list(
        "Données brutes"=d|>mutate(across(where(is.logical),as.integer)),
        "Prévalences par région"=prev_region,"Prévalences par département"=prev_dept,
        "Indicateurs globaux"=bind_rows(
          calc_prev(d,"GAM","GAM (WHZ<-2 ou œdème)"),calc_prev(d,"SAM","SAM"),
          calc_prev(d,"MAM","MAM"),calc_prev(d,"Stunting","Stunting (HAZ<-2)"),
          calc_prev(d,"Sev_Stunting","Stunting sévère"),calc_prev(d,"Underweight","Insuff. pondérale"),
          calc_prev(d,"MUAC_GAM","GAM MUAC"),calc_prev(d,"MUAC_SAM","SAM MUAC"))),file)
    })

  # ══════════════════════════════════════════════════
  # UNIVARIÉ
  # ══════════════════════════════════════════════════
  output$uni_var_ui <- renderUI({
    type <- input$uni_type
    if (grepl("Barplot|Camembert|Donut",type)) {
      ch <- if (grepl("Camembert|Donut",type)) vars_cat_pie else vars_cat
      selectInput("uni_var","Variable:",choices=ch,selected=ch[1],width="100%")
    } else {
      selectInput("uni_var","Variable:",choices=vars_num,selected="whz_score",width="100%")
    }
  })

  output$uni_stats <- renderTable({
    req(input$uni_var); d <- df_f(); v <- input$uni_var
    if (is.numeric(d[[v]])) {
      vals <- na.omit(d[[v]])
      sw <- tryCatch(shapiro.test(vals[1:min(5000,length(vals))]),error=function(e) NULL)
      tibble(
        Stat=c("N","Moy","Méd","Éc.-t.","CV%","Min","Max","Q1","Q3","Asymét.","Plat.","Shapiro p"),
        Val=c(length(vals),round(mean(vals),2),round(median(vals),2),round(sd(vals),2),
              cv_stat(vals),round(min(vals),2),round(max(vals),2),round(quantile(vals,.25),2),
              round(quantile(vals,.75),2),round(skewness(vals,na.rm=T),3),round(kurtosis(vals,na.rm=T),3),
              if(!is.null(sw)) round(sw$p.value,4) else NA))
    } else {
      cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
      tibble(Modalité=as.character(cnt[[v]]),N=cnt$n,`%`=cnt$pct)
    }
  },striped=TRUE,bordered=TRUE,digits=3,spacing="xs")

  output$uni_plot <- renderPlotly({
    req(input$uni_var); d <- df_f(); v <- input$uni_var; type <- input$uni_type
    pal_fn <- function(n) get_pal_uni(n,input$uni_palette)

    if (grepl("Barplot",type)) {
      cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
      niv <- nrow(cnt)
      p <- ggplot(cnt,aes(x=fct_reorder(.data[[v]],n),y=n,fill=.data[[v]],
                          text=paste0(.data[[v]],"\nN=",n," (",pct,"%)"))) +
        geom_col(alpha=0.88,width=0.7) +
        {if(input$uni_labels) geom_text(aes(label=paste0(n,"\n(",pct,"%)")),hjust=-0.1,size=3)} +
        scale_fill_manual(values=pal_fn(niv)) +
        coord_flip()+theme_minimal(base_size=11)+labs(x="",y="Effectif")+theme(legend.position="none")
      ggplotly(p,tooltip="text")|>cfg_dl("uni_barplot")

    } else if (grepl("Camembert|Donut",type)) {
      cnt <- d|>count(.data[[v]])|>mutate(pct=round(n/sum(n)*100,1))
      plot_ly(cnt,labels=~.data[[v]],values=~n,type="pie",hole=0.42,textinfo="label+percent",
              marker=list(colors=pal_fn(nrow(cnt)),line=list(color="white",width=2)),
              hovertemplate="%{label}<br>N=%{value}<br>%{percent}<extra></extra>")|>
        layout(title=paste("Distribution de",v),showlegend=TRUE,legend=list(orientation="h",y=-0.15))|>
        cfg_dl("uni_camembert")

    } else if (grepl("Histogramme",type)) {
      m <- mean(d[[v]],na.rm=T); s <- sd(d[[v]],na.rm=T); md <- median(d[[v]],na.rm=T)
      p <- ggplot(d,aes(x=.data[[v]]))+
        geom_histogram(aes(y=after_stat(density)),bins=30,fill=oms_bleu,color="white",alpha=0.82)+
        stat_function(fun=dnorm,args=list(mean=m,sd=s),color="black",linewidth=1.1)+
        geom_vline(xintercept=m,color=oms_rouge,linewidth=1,linetype="dashed")+
        geom_vline(xintercept=md,color=oms_orange,linewidth=1,linetype="dotted")+
        labs(x=v,y="Densité",subtitle=paste0("μ=",round(m,2)," | σ=",round(s,2)))+theme_minimal(base_size=11)
      ggplotly(p)|>layout(hovermode="x unified")|>cfg_dl("uni_histogramme")

    } else if (grepl("Boxplot",type)) {
      q <- quantile(d[[v]],c(.25,.5,.75),na.rm=T)
      p <- ggplot(d,aes(x="",y=.data[[v]]))+
        geom_boxplot(fill=oms_bleu,color="#004f7c",alpha=0.75,outlier.shape=21,outlier.color=oms_rouge,width=0.4)+
        stat_summary(fun=mean,geom="point",shape=18,size=4,color=oms_rouge)+
        annotate("text",x=1.35,y=q[1],label=paste0("Q1=",round(q[1],2)),size=3)+
        annotate("text",x=1.35,y=q[2],label=paste0("Méd=",round(q[2],2)),size=3)+
        annotate("text",x=1.35,y=q[3],label=paste0("Q3=",round(q[3],2)),size=3)+
        labs(x="",y=v)+theme_minimal(base_size=11)
      ggplotly(p)|>cfg_dl("uni_boxplot")

    } else if (grepl("Violin",type)) {
      p <- ggplot(d,aes(x="",y=.data[[v]]))+
        geom_violin(fill=oms_violet,alpha=0.5,trim=FALSE,color=oms_violet)+
        geom_boxplot(fill="white",color="#2c3e50",width=0.12,outlier.shape=21,outlier.color=oms_rouge)+
        stat_summary(fun=mean,geom="point",shape=18,size=4,color=oms_rouge)+
        labs(x="",y=v)+theme_minimal(base_size=11)
      ggplotly(p)|>cfg_dl("uni_violin")

    } else if (grepl("densité",type)) {
      p <- ggplot(d,aes(x=.data[[v]]))+
        geom_density(fill=oms_bleu,color=oms_bleu,alpha=0.4,linewidth=0.9)+
        geom_vline(xintercept=mean(d[[v]],na.rm=T),color=oms_rouge,linewidth=1,linetype="dashed")+
        labs(x=v,y="Densité")+theme_minimal(base_size=11)
      ggplotly(p)|>layout(hovermode="x unified")|>cfg_dl("uni_densite")

    } else if (grepl("QQ",type)) {
      sw <- tryCatch(shapiro.test(na.omit(d[[v]])[1:min(5000,sum(!is.na(d[[v]])))]),error=function(e) NULL)
      p <- ggplot(d,aes(sample=.data[[v]]))+
        stat_qq(color=oms_bleu,alpha=0.6,size=1.5)+
        stat_qq_line(color=oms_rouge,linewidth=1)+
        labs(title=paste0("QQ-plot — ",v),
             subtitle=if(!is.null(sw)) paste0("Shapiro-Wilk: W=",round(sw$statistic,4),
               "  p=",round(sw$p.value,4),if(sw$p.value<.05)"  → NON NORMALE" else "  → Normale") else "",
             x="Quantiles théoriques",y="Quantiles observés")+theme_minimal(base_size=11)
      ggplotly(p)|>cfg_dl("uni_qqplot")

    } else if (grepl("Ridgeline",type)) {
      p <- ggplot(d,aes(x=.data[[v]],y=region,fill=region))+
        geom_density_ridges(alpha=0.7,scale=1.2)+
        scale_fill_manual(values=couleurs_region)+
        geom_vline(xintercept=c(-2,-3),linetype="dashed",color=oms_rouge,alpha=0.7)+
        labs(x=v,y="")+theme_minimal(base_size=11)+theme(legend.position="none")
      ggplotly(p)|>cfg_dl("uni_ridgeline")

    } else {
      x_ref <- seq(min(d[[v]],na.rm=T)-1,max(d[[v]],na.rm=T)+1,length.out=300)
      ref_df <- tibble(x=x_ref,y=dnorm(x_ref,0,1))
      p <- ggplot(d,aes(x=.data[[v]]))+
        geom_density(aes(fill="Observé",color="Observé"),alpha=0.4,linewidth=0.9)+
        geom_line(data=ref_df,aes(x=x,y=y,color="OMS N(0,1)",fill="OMS N(0,1)"),
                  linewidth=1,linetype="dashed",inherit.aes=FALSE)+
        scale_fill_manual(values=c("Observé"=oms_rouge,"OMS N(0,1)"="transparent"))+
        scale_color_manual(values=c("Observé"=oms_rouge,"OMS N(0,1)"="black"))+
        labs(x=v,y="Densité",subtitle="Décalage gauche = prévalence élevée de malnutrition")+
        theme_minimal(base_size=11)+theme(legend.position="bottom")
      ggplotly(p)|>cfg_dl("uni_vs_oms")
    }
  })

  # ══════════════════════════════════════════════════
  # BIVARIÉ
  # ══════════════════════════════════════════════════
  output$biv_y_ui <- renderUI({
    type <- input$biv_type
    if (grepl("Barplot|Heatmap|Alluvial",type)) {
      selectInput("biv_y","Variable Y (catégorielle):",choices=vars_cat,selected="nutr_cat",width="100%")
    } else if (grepl("Nuage",type)) {
      tagList(
        selectInput("biv_y","Variable Y (continue):",choices=vars_num,selected="muac_mm",width="100%"),
        selectInput("biv_z","Couleur par:",choices=c("Aucune",vars_cat),selected="region",width="100%"))
    } else {
      selectInput("biv_y","Variable continue:",choices=vars_num,selected="whz_score",width="100%")
    }
  })

  output$biv_test <- renderPrint({
    req(input$biv_x,input$biv_y); d <- df_f(); type <- input$biv_type
    tryCatch({
      if (grepl("Barplot|Heatmap|Alluvial",type)) {
        tbl <- table(d[[input$biv_x]],d[[input$biv_y]]); res <- chisq.test(tbl,correct=FALSE)
        cat("Chi²:",round(res$statistic,3),"\np-value:",round(res$p.value,4),
            if(res$p.value<.05)"\n→ Association significative" else "\n→ NS")
      } else if (grepl("Nuage",type)) {
        r <- cor.test(d[[input$biv_x]],d[[input$biv_y]],method="spearman",exact=FALSE)
        cat("Spearman ρ:",round(r$estimate,3),"\np-value:",round(r$p.value,4))
      } else {
        kw <- kruskal.test(d[[input$biv_y]]~d[[input$biv_x]])
        cat("Kruskal-Wallis H:",round(kw$statistic,3),"\np-value:",round(kw$p.value,4),
            if(kw$p.value<.05)"\n→ Différence significative" else "\n→ NS")
      }
    },error=function(e) cat("Test non applicable"))
  })

  output$biv_plot <- renderPlotly({
    req(input$biv_x,input$biv_y); d <- df_f(); v1 <- input$biv_x; v2 <- input$biv_y
    type <- input$biv_type; gpalf <- function(n) get_pal_biv(n,input$biv_pal)
    niv1 <- nlevels(d[[v1]])

    if (grepl("groupé",type)) {
      cnt <- d|>count(.data[[v1]],.data[[v2]])|>mutate(pct=round(n/sum(n)*100,1))
      niv2 <- nlevels(d[[v2]])
      p <- ggplot(cnt,aes(x=.data[[v1]],y=n,fill=.data[[v2]],
                          text=paste0(v1,"=",.data[[v1]],"\n",v2,"=",.data[[v2]],"\nN=",n," (",pct,"%)"))) +
        geom_col(position="dodge",width=0.75,alpha=0.88)+scale_fill_manual(values=gpalf(niv2),name=v2)+
        labs(x=v1,y="Effectif")+theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.25))|>cfg_dl("biv_barplot_groupe")

    } else if (grepl("empilé",type)) {
      cnt <- d|>count(.data[[v1]],.data[[v2]])|>group_by(.data[[v1]])|>
        mutate(pct=round(n/sum(n)*100,1))|>ungroup()
      niv2 <- nlevels(d[[v2]])
      p <- ggplot(cnt,aes(x=.data[[v1]],y=pct,fill=.data[[v2]],
                          text=paste0(.data[[v1]]," | ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
        geom_col(position=position_fill(),width=0.65,alpha=0.88)+
        geom_text(aes(label=paste0(pct,"%")),position=position_fill(vjust=0.5),size=3,color="white",fontface="bold")+
        scale_fill_manual(values=gpalf(niv2),name=v2)+scale_y_continuous(labels=percent)+
        labs(x=v1,y="Proportion")+theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.25))|>cfg_dl("biv_empile")

    } else if (grepl("Boxplot par",type)) {
      p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]]))+
        geom_boxplot(alpha=0.75,outlier.shape=21,outlier.color=oms_rouge,width=0.55)+
        stat_summary(fun=mean,geom="point",shape=18,size=3,color="white")+
        scale_fill_manual(values=gpalf(niv1))+labs(x=v1,y=v2)+theme_minimal(base_size=11)+
        theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p)|>cfg_dl("biv_boxplot")

    } else if (grepl("Violin par",type)) {
      p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],fill=.data[[v1]]))+
        geom_violin(alpha=0.6,trim=FALSE)+
        geom_boxplot(fill="white",width=0.1,outlier.shape=21,outlier.color=oms_rouge)+
        stat_summary(fun=mean,geom="point",shape=18,size=3,color=oms_rouge)+
        scale_fill_manual(values=gpalf(niv1))+labs(x=v1,y=v2)+theme_minimal(base_size=11)+
        theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p)|>cfg_dl("biv_violin")

    } else if (grepl("Nuage",type)) {
      v3 <- if(!is.null(input$biv_z)&&input$biv_z!="Aucune") input$biv_z else NULL
      rho <- cor.test(d[[v1]],d[[v2]],method="spearman",exact=FALSE)
      p <- ggplot(d,aes(x=.data[[v1]],y=.data[[v2]],
                        color=if(!is.null(v3)).data[[v3]] else NULL,
                        text=paste0(v1,"=",round(.data[[v1]],2),"\n",v2,"=",round(.data[[v2]],2))))+
        geom_point(alpha=0.5,size=1.5)+
        geom_smooth(method="loess",se=TRUE,color="black",linewidth=0.9,aes(group=1))+
        {if(!is.null(v3)) scale_color_manual(values=gpalf(nlevels(d[[v3]])))}+
        labs(x=v1,y=v2,subtitle=paste0("ρ=",round(rho$estimate,3),"  p=",round(rho$p.value,4)))+
        theme_minimal(base_size=11)
      ggplotly(p,tooltip="text")|>cfg_dl("biv_nuage")

    } else if (grepl("Densités",type)) {
      p <- ggplot(d,aes(x=.data[[v2]],fill=.data[[v1]],color=.data[[v1]]))+
        geom_density(alpha=0.3,linewidth=0.8)+scale_fill_manual(values=gpalf(niv1),name=v1)+
        scale_color_manual(values=gpalf(niv1),name=v1)+labs(x=v2,y="Densité")+theme_minimal(base_size=11)
      ggplotly(p)|>layout(legend=list(orientation="h",y=-0.2))|>cfg_dl("biv_densites")

    } else if (grepl("facettes",type)) {
      p <- ggplot(d,aes(x=.data[[v2]],fill=.data[[v1]]))+
        geom_histogram(aes(y=after_stat(density)),bins=25,color="white",alpha=0.82)+
        scale_fill_manual(values=gpalf(niv1))+
        facet_wrap(as.formula(paste("~",v1)),ncol=2)+
        labs(x=v2,y="Densité")+theme_bw(base_size=10)+theme(legend.position="none")
      ggplotly(p)|>cfg_dl("biv_facettes")

    } else if (grepl("Heatmap",type)) {
      cnt <- d|>count(.data[[v1]],.data[[v2]])|>group_by(.data[[v1]])|>
        mutate(pct=round(n/sum(n)*100,1))|>ungroup()
      p <- ggplot(cnt,aes(x=.data[[v2]],y=.data[[v1]],fill=pct,
                          text=paste0(.data[[v1]]," × ",.data[[v2]],"\n",pct,"% (N=",n,")")))+
        geom_tile(color="white",linewidth=1)+geom_text(aes(label=paste0(pct,"%")),size=3.5,fontface="bold")+
        scale_fill_gradientn(colors=c("white",oms_bleu,oms_rouge),name="%")+
        labs(x=v2,y=v1)+theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip="text")|>cfg_dl("biv_heatmap")

    } else { # Alluvial
      cnt <- d|>count(.data[[v1]],.data[[v2]])
      p <- ggplot(cnt,aes(axis1=.data[[v1]],axis2=.data[[v2]],y=n))+
        geom_alluvium(aes(fill=.data[[v1]]),alpha=0.7,width=1/3)+
        geom_stratum(width=1/3,fill="white",color="grey50")+
        geom_text(stat="stratum",aes(label=after_stat(stratum)),size=3)+
        scale_fill_manual(values=gpalf(niv1))+
        labs(x="",y="Effectif",title=paste("Flux",v1,"→",v2))+theme_minimal(base_size=11)
      ggplotly(p)|>cfg_dl("biv_alluvial")
    }
  })

  # ══════════════════════════════════════════════════
  # DESCRIPTIVE OMS
  # ══════════════════════════════════════════════════
  gt_summary_reactive <- reactive({
    d <- df_f()
    vars_sel <- c("region","child_age_months","child_sex","displacement_status",
                  "food_consumption_score","household_hunger_scale","whz_score","haz_score",
                  "waz_score","muac_mm","oedema","diarrhea_last_2w","fever_last_2w",
                  "vaccination_status","mother_muac","anc_visits",
                  "GAM","SAM","Stunting","Underweight","MUAC_GAM","ICV","cat_vulner")
    tbl_data <- d|>select(any_of(c(vars_sel,if(input$desc_by!="none") input$desc_by else NULL)))
    lbl <- list(child_age_months~"Âge (mois)",child_sex~"Sexe",
                displacement_status~"Statut déplacement",food_consumption_score~"FCS",
                household_hunger_scale~"HHS",whz_score~"WHZ",haz_score~"HAZ",waz_score~"WAZ",
                muac_mm~"MUAC (mm)",oedema~"Œdèmes",diarrhea_last_2w~"Diarrhée",
                fever_last_2w~"Fièvre",vaccination_status~"Statut vaccinal",
                mother_muac~"MUAC mère",anc_visits~"Visites CPN",
                GAM~"GAM",SAM~"SAM",Stunting~"Stunting",Underweight~"Insuff. pondérale",
                MUAC_GAM~"GAM MUAC",ICV~"ICV",cat_vulner~"Vulnérabilité")
    if (input$desc_by=="none") {
      tbl_data|>tbl_summary(
        statistic=list(all_continuous()~"{mean} ± {sd} [{min}–{max}]",all_categorical()~"{n} ({p}%)"),
        digits=all_continuous()~1,missing="no",label=lbl)|>bold_labels()
    } else {
      tbl_data|>tbl_summary(
        by=input$desc_by,
        statistic=list(all_continuous()~"{mean} ± {sd}",all_categorical()~"{n} ({p}%)"),
        digits=all_continuous()~1,missing="no",label=lbl)|>bold_labels()|>add_p()|>bold_p(t=0.05)
    }
  })

  output$desc_gt_out <- renderUI({
    gt_summary_reactive()|>as_gt()|>tab_options(table.font.size="12px")|>as_raw_html()
  })

  output$dl_gt_html <- downloadHandler(
    filename=function() paste0("tableau_descriptif_OMS_",Sys.Date(),".html"),
    content=function(file) {
      gt_summary_reactive()|>as_gt()|>
        tab_header(title=md("**Tableau descriptif — Enquête SMART Cameroun 2024**"),
                   subtitle=md("*Normes OMS 2006 | Méthode SMART*"))|>
        tab_options(table.font.size="13px")|>gt::gtsave(file)
    })

  output$dl_gt_csv <- downloadHandler(
    filename=function() paste0("tableau_descriptif_OMS_",Sys.Date(),".csv"),
    content=function(file) gt_summary_reactive()|>as_tibble()|>write_csv(file))

  output$dl_gt_excel <- downloadHandler(
    filename=function() paste0("tableau_descriptif_OMS_",Sys.Date(),".xlsx"),
    content=function(file) gt_summary_reactive()|>as_tibble()|>writexl::write_xlsx(file))

  make_corrplot <- function(data) {
    vars_c <- c("waz_score","haz_score","whz_score","muac_mm","food_consumption_score",
                "household_hunger_scale","coping_strategy_index","child_age_months",
                "mother_muac","anc_visits","ICV","risk_score")
    mat <- data|>select(all_of(vars_c))|>drop_na()|>cor(method="spearman")
    corrplot(mat,method="color",type="upper",addCoef.col="black",number.cex=0.65,
             tl.cex=0.72,tl.col="black",tl.srt=45,
             col=colorRampPalette(c(oms_rouge,"white",oms_bleu))(200),
             mar=c(0,0,1,0),title="Corrélations Spearman")
  }

  output$desc_corr <- renderPlot({ make_corrplot(df_f()) },height=400)

  output$dl_corrplot <- downloadHandler(
    filename=function() paste0("correlation_spearman_",Sys.Date(),".png"),
    content=function(file) { png(file,width=1400,height=1200,res=150); make_corrplot(df_f()); dev.off() })

  output$desc_zscore <- renderPlotly({
    d <- df_f()
    df_long <- d|>select(region,whz_score,haz_score,waz_score)|>
      pivot_longer(-region,names_to="Score",values_to="Val")|>
      mutate(Score=recode(Score,whz_score="WHZ",haz_score="HAZ",waz_score="WAZ"))
    p <- ggplot(df_long,aes(x=region,y=Val,fill=region))+
      geom_boxplot(alpha=0.7,outlier.shape=21,outlier.color=oms_rouge,width=0.5)+
      geom_hline(yintercept=-2,linetype="dashed",color=oms_rouge,linewidth=0.7)+
      geom_hline(yintercept=-3,linetype="dashed",color=oms_rouge_f,linewidth=0.7)+
      scale_fill_manual(values=couleurs_region)+
      facet_wrap(~Score,scales="free_y")+labs(x="",y="Score Z")+
      theme_bw(base_size=10)+theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p)|>layout(hovermode="closest")|>cfg_dl("desc_scores_z")
  })

  # ══════════════════════════════════════════════════
  # INFÉRENTIEL
  # ══════════════════════════════════════════════════
  output$chi2_table <- renderDT({
    req(input$chi2_vars,input$chi2_outcome); d <- df_f()
    map_dfr(input$chi2_vars,function(var) {
      tryCatch({
        tbl <- table(d[[var]],d[[input$chi2_outcome]])
        res_chi <- chisq.test(tbl,correct=FALSE)
        min_exp <- min(res_chi$expected)
        if (min_exp<5) { res <- fisher.test(tbl,simulate.p.value=TRUE); test <- "Fisher"; stat <- NA_real_ }
        else { res <- res_chi; test <- "Chi²"; stat <- round(res$statistic,3) }
        tibble(Variable=var,Test=test,`Min. attendu`=round(min_exp,1),Stat=stat,
               `p-valeur`=round(res$p.value,4),
               Sig=case_when(res$p.value<.001~"***",res$p.value<.01~"**",
                             res$p.value<.05~"*",res$p.value<.10~".",TRUE~"NS"))
      },error=function(e) NULL)
    })|>datatable(rownames=FALSE,extensions="Buttons",
      options=list(dom="Bftp",pageLength=10,
        buttons=list(list(extend="csv",text="📥 CSV",filename=paste0("chi2_",Sys.Date())),
                     list(extend="excel",text="📊 Excel",filename=paste0("chi2_",Sys.Date())),
                     list(extend="copy",text="📋 Copier"))),
      class="compact stripe hover")|>
      formatStyle("Sig",target="row",backgroundColor=styleEqual(
        c("***","**","*",".","NS"),c("#ffe0e0","#ffe0e0","#fff3cd","#f8f9fa","white")))
  })

  output$mw_table <- renderDT({
    req(input$mw_vars,input$mw_outcome); d <- df_f()
    map_dfr(input$mw_vars,function(var) {
      g0 <- d[[var]][!d[[input$mw_outcome]]]; g1 <- d[[var]][d[[input$mw_outcome]]]
      test <- wilcox.test(g0,g1,exact=FALSE)
      tibble(Variable=var,`Méd. Négatif`=round(median(g0,na.rm=T),2),
             `Méd. Positif`=round(median(g1,na.rm=T),2),W=round(test$statistic,1),
             `p-valeur`=round(test$p.value,4),
             Sig=case_when(test$p.value<.001~"***",test$p.value<.01~"**",
                           test$p.value<.05~"*",test$p.value<.10~".",TRUE~"NS"))
    })|>datatable(rownames=FALSE,extensions="Buttons",
      options=list(dom="Bftp",pageLength=10,
        buttons=list(list(extend="csv",text="📥 CSV",filename=paste0("mw_",Sys.Date())),
                     list(extend="excel",text="📊 Excel",filename=paste0("mw_",Sys.Date())))),
      class="compact stripe hover")
  })

  output$mw_plot <- renderPlotly({
    req(input$mw_vars,input$mw_outcome); d <- df_f(); var <- input$mw_vars[1]
    p <- ggplot(d,aes(x=.data[[input$mw_outcome]],y=.data[[var]],fill=.data[[input$mw_outcome]]))+
      geom_boxplot(alpha=0.75,outlier.shape=21,outlier.color=oms_rouge,width=0.4)+
      scale_fill_manual(values=c("TRUE"=oms_rouge,"FALSE"=oms_bleu),
                        labels=c("TRUE"=input$mw_outcome,"FALSE"=paste0("Non-",input$mw_outcome)))+
      labs(x=input$mw_outcome,y=var)+theme_minimal(base_size=11)+theme(legend.position="none")
    ggplotly(p)|>cfg_dl("infer_mw")
  })

  output$kw_result <- renderPrint({
    d <- df_f(); kw <- kruskal.test(d[[input$kw_var]]~d[[input$kw_group]])
    cat("H =",round(kw$statistic,3),"\nddl =",kw$parameter,"\np =",round(kw$p.value,4),
        if(kw$p.value<.05)"\n→ Significatif" else "\n→ Non significatif")
  })

  output$kw_posthoc <- renderPrint({
    d <- df_f()
    tryCatch({
      ph <- pairwise.wilcox.test(d[[input$kw_var]],d[[input$kw_group]],p.adjust.method="BH",exact=FALSE)
      print(round(ph$p.value,4))
    },error=function(e) cat("Post-hoc non disponible"))
  })

  output$dl_kw_results <- downloadHandler(
    filename=function() paste0("kruskal_wallis_",input$kw_var,"_",Sys.Date(),".txt"),
    content=function(file) {
      d <- df_f(); kw <- kruskal.test(d[[input$kw_var]]~d[[input$kw_group]])
      ph <- tryCatch(pairwise.wilcox.test(d[[input$kw_var]],d[[input$kw_group]],p.adjust.method="BH",exact=FALSE),error=function(e) NULL)
      sink(file)
      cat("═══ KRUSKAL-WALLIS ═══\nVariable:",input$kw_var,"\nGroupe:",input$kw_group,"\nN=",nrow(d),"\n\n")
      cat("H =",round(kw$statistic,4),"\nddl =",kw$parameter,"\np-valeur =",round(kw$p.value,6),"\n\n")
      if (!is.null(ph)) { cat("Post-hoc (BH):\n"); print(round(ph$p.value,4)) }
      sink()
    })

  output$kw_plot <- renderPlotly({
    d <- df_f(); niv <- nlevels(d[[input$kw_group]])
    p <- ggplot(d,aes(x=.data[[input$kw_group]],y=.data[[input$kw_var]],fill=.data[[input$kw_group]]))+
      geom_boxplot(alpha=0.75,outlier.shape=21,outlier.color=oms_rouge,width=0.5)+
      geom_hline(yintercept=-2,linetype="dashed",color=oms_rouge,linewidth=0.7)+
      stat_summary(fun=mean,geom="point",shape=18,size=3,color="white")+
      scale_fill_manual(values=get_pal_biv(niv,"Viridis"))+
      labs(x=input$kw_group,y=input$kw_var)+theme_minimal(base_size=11)+
      theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p)|>cfg_dl("infer_kw")
  })

  output$cor_result <- renderPrint({
    d <- df_f()
    tryCatch({
      r <- cor.test(d[[input$cor_x]],d[[input$cor_y]],method="spearman",exact=FALSE)
      cat("Spearman ρ =",round(r$estimate,4),"\np-valeur  =",round(r$p.value,4),
          if(r$p.value<.05)"\n→ Corrélation significative" else "\n→ NS",
          "\n\nInterprétation:\n",if(abs(r$estimate)>=.7)"Forte" else if(abs(r$estimate)>=.4)"Modérée" else "Faible")
    },error=function(e) cat("Test non disponible"))
  })

  output$cor_plot <- renderPlotly({
    d <- df_f()
    rho <- tryCatch(cor.test(d[[input$cor_x]],d[[input$cor_y]],method="spearman",exact=FALSE),error=function(e) NULL)
    v3 <- if(!is.null(input$cor_color)&&input$cor_color!="Aucune") input$cor_color else NULL
    p <- ggplot(d,aes(x=.data[[input$cor_x]],y=.data[[input$cor_y]],
                      color=if(!is.null(v3)).data[[v3]] else NULL,
                      text=paste0(input$cor_x,"=",round(.data[[input$cor_x]],2),"\n",input$cor_y,"=",round(.data[[input$cor_y]],2))))+
      geom_point(alpha=0.5,size=1.5)+geom_smooth(method="loess",se=TRUE,color="black",linewidth=0.9,aes(group=1))+
      {if(!is.null(v3)) scale_color_manual(values=get_pal_biv(nlevels(d[[v3]]),"OMS régions"))}+
      labs(x=input$cor_x,y=input$cor_y,subtitle=if(!is.null(rho)) paste0("ρ=",round(rho$estimate,3),"  p=",round(rho$p.value,4)) else "")+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text")|>cfg_dl("infer_cor")
  })

  # ══════════════════════════════════════════════════
  # RÉGRESSION LOGISTIQUE
  # ══════════════════════════════════════════════════
  log_model <- eventReactive(input$run_logistic, {
    d <- df_f(); outcome <- input$log_outcome; pred_vars <- input$log_vars
    req(length(pred_vars)>=2)
    d_model <- d|>select(all_of(c(outcome,pred_vars)))|>drop_na()
    formula_str <- paste(outcome,"~",paste(pred_vars,collapse=" + "))
    glm(as.formula(formula_str),data=d_model,family=binomial(link="logit"))
  })

  output$logistic_summary <- renderPrint({
    req(log_model()); print(summary(log_model()))
  })

  output$logistic_table <- renderDT({
    req(log_model()); m <- log_model()
    coefs <- broom::tidy(m,exponentiate=TRUE,conf.int=TRUE) |>
      filter(term!="(Intercept)") |>
      mutate(
        OR=round(estimate,3), `IC95 bas`=round(conf.low,3), `IC95 haut`=round(conf.high,3),
        `p-valeur`=round(p.value,4),
        Sig=case_when(p.value<.001~"***",p.value<.01~"**",p.value<.05~"*",p.value<.10~".",TRUE~"NS"),
        Interprétation=case_when(
          p.value>.05~"Non significatif",OR>1&p.value<=.05~"Facteur de risque",
          OR<1&p.value<=.05~"Facteur protecteur",TRUE~"")
      ) |> select(term,OR,`IC95 bas`,`IC95 haut`,`p-valeur`,Sig,Interprétation)
    datatable(coefs,rownames=FALSE,extensions="Buttons",
      options=list(dom="Bftp",pageLength=15,
        buttons=list(list(extend="csv",text="📥 CSV",filename=paste0("logistic_OR_",Sys.Date())),
                     list(extend="excel",text="📊 Excel",filename=paste0("logistic_OR_",Sys.Date())))),
      class="compact stripe hover") |>
      formatStyle("Sig",target="row",backgroundColor=styleEqual(
        c("***","**","*",".","NS"),c("#ffe0e0","#ffe0e0","#fff3cd","#f8f9fa","white"))) |>
      formatStyle("OR",background=styleColorBar(c(0,max(coefs$OR,na.rm=T)),oms_bleu),backgroundSize="100% 70%")
  })

  output$logistic_forest <- renderPlotly({
    req(log_model()); m <- log_model()
    coefs <- broom::tidy(m,exponentiate=TRUE,conf.int=TRUE)|>filter(term!="(Intercept)")|>
      mutate(OR=estimate, color=case_when(p.value<.05&OR>1~oms_rouge,p.value<.05&OR<1~oms_vert,TRUE~oms_gris),
             sig=p.value<.05)
    p <- ggplot(coefs,aes(x=OR,y=fct_reorder(term,OR),color=p.value<.05,
                          text=paste0(term,"\nOR=",round(OR,3)," [",round(conf.low,3),"-",round(conf.high,3),"]\np=",round(p.value,4))))+
      geom_vline(xintercept=1,linetype="dashed",color="black",linewidth=0.8)+
      geom_errorbarh(aes(xmin=conf.low,xmax=conf.high),height=0.4,linewidth=0.8)+
      geom_point(size=4,aes(shape=sig))+
      scale_color_manual(values=c("TRUE"=oms_rouge,"FALSE"=oms_gris),name="p<0.05")+
      scale_shape_manual(values=c("TRUE"=18,"FALSE"=16),guide="none")+
      labs(x="Odds Ratio (IC 95%)",y="",title=paste("Forest plot —",input$log_outcome))+
      theme_minimal(base_size=11)+theme(legend.position="bottom")
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.15))|>cfg_dl("logistic_forest_OR")
  })

  output$logistic_roc <- renderPlotly({
    req(log_model()); m <- log_model()
    d_pred <- broom::augment(m,type.predict="response")
    outcome_vals <- d_pred[[input$log_outcome]]
    roc_obj <- tryCatch(roc(outcome_vals,d_pred$.fitted,quiet=TRUE),error=function(e) NULL)
    if (is.null(roc_obj)) { plot_ly()|>layout(title="ROC non disponible"); return() }
    auc_val <- round(as.numeric(auc(roc_obj)),3)
    roc_df <- tibble(fpr=1-roc_obj$specificities,tpr=roc_obj$sensitivities)
    p <- ggplot(roc_df,aes(x=fpr,y=tpr))+
      geom_line(color=oms_rouge,linewidth=1.5)+
      geom_abline(linetype="dashed",color="gray50")+
      geom_ribbon(aes(ymin=0,ymax=tpr),fill=oms_rouge,alpha=0.15)+
      annotate("text",x=0.6,y=0.2,label=paste0("AUC = ",auc_val),size=5,fontface="bold",color=oms_rouge)+
      labs(x="1 - Spécificité (Faux positifs)",y="Sensibilité (Vrais positifs)",title="Courbe ROC")+
      theme_minimal(base_size=12)+coord_equal()
    ggplotly(p)|>cfg_dl("roc_courbe")
  })

  output$logistic_auc <- renderPrint({
    req(log_model()); m <- log_model()
    d_pred <- broom::augment(m,type.predict="response")
    outcome_vals <- d_pred[[input$log_outcome]]
    roc_obj <- tryCatch(roc(outcome_vals,d_pred$.fitted,quiet=TRUE),error=function(e) NULL)
    if (!is.null(roc_obj)) {
      auc_val <- as.numeric(auc(roc_obj))
      cat("AUC =",round(auc_val,4),"\n")
      cat("Interprétation:",
          if(auc_val>=.9)"Excellent (≥0.90)" else if(auc_val>=.8)"Bon (0.80-0.89)"
          else if(auc_val>=.7)"Acceptable (0.70-0.79)" else "Faible (<0.70)","\n")
      cat("\nN observations =",nrow(d_pred))
    } else cat("AUC non calculable")
  })

  output$dl_logistic <- downloadHandler(
    filename=function() paste0("regression_logistique_",input$log_outcome,"_",Sys.Date(),".xlsx"),
    content=function(file) {
      req(log_model()); m <- log_model()
      coefs <- broom::tidy(m,exponentiate=TRUE,conf.int=TRUE)|>
        filter(term!="(Intercept)")|>
        mutate(OR=round(estimate,3),`IC95 bas`=round(conf.low,3),`IC95 haut`=round(conf.high,3),`p-valeur`=round(p.value,4))
      writexl::write_xlsx(list("Odds Ratios"=coefs,"Résumé"=broom::glance(m)|>mutate(across(where(is.numeric),~round(.,4)))),file)
    })

  # ══════════════════════════════════════════════════
  # MACHINE LEARNING — CLUSTERING
  # ══════════════════════════════════════════════════
  cluster_result <- eventReactive(input$run_cluster, {
    req(input$ml_vars); d <- df_f()
    d_scaled <- d|>select(all_of(input$ml_vars))|>drop_na()|>scale()
    set.seed(42)
    km <- kmeans(d_scaled,centers=input$ml_k,nstart=25,iter.max=100)
    list(km=km,data_scaled=d_scaled,data_orig=d|>select(all_of(input$ml_vars))|>drop_na())
  })

  output$ml_cluster_plot <- renderPlotly({
    req(cluster_result()); res <- cluster_result()
    pca <- prcomp(res$data_scaled,scale.=FALSE)
    pca_df <- as_tibble(pca$x[,1:2])|>mutate(Cluster=factor(res$km$cluster))
    var_exp <- round(100*summary(pca)$importance[2,1:2],1)
    p <- ggplot(pca_df,aes(x=PC1,y=PC2,color=Cluster,
                           text=paste0("Cluster ",Cluster,"\nPC1=",round(PC1,2),"\nPC2=",round(PC2,2))))+
      geom_point(alpha=0.6,size=2)+
      stat_ellipse(aes(group=Cluster),type="norm",linetype="dashed",linewidth=0.8)+
      scale_color_brewer(palette="Set1")+
      labs(x=paste0("PC1 (",var_exp[1],"% variance)"),y=paste0("PC2 (",var_exp[2],"% variance)"),
           title=paste0("Clustering K-moyennes (k=",input$ml_k,") — Projection PCA"))+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text")|>cfg_dl("ml_clustering_pca")
  })

  output$ml_cluster_profile <- renderPlotly({
    req(cluster_result()); res <- cluster_result()
    centers_df <- as_tibble(res$km$centers)|>
      mutate(Cluster=factor(1:input$ml_k))|>
      pivot_longer(-Cluster,names_to="Variable",values_to="Valeur_standardisée")
    p <- ggplot(centers_df,aes(x=Variable,y=Valeur_standardisée,fill=Cluster,
                               text=paste0("Cluster ",Cluster,"\n",Variable,": ",round(Valeur_standardisée,2))))+
      geom_col(position="dodge",alpha=0.85)+
      scale_fill_brewer(palette="Set1")+
      geom_hline(yintercept=0,linetype="dashed",color="gray50")+
      labs(x="",y="Valeur standardisée (z-score)",title="Profil des clusters")+
      theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=35,hjust=1))
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.25))|>cfg_dl("ml_profil_clusters")
  })

  output$ml_cluster_table <- renderDT({
    req(cluster_result()); res <- cluster_result()
    centers <- as_tibble(res$km$centers)|>mutate(Cluster=factor(1:input$ml_k),.before=1)
    sizes <- tibble(Cluster=factor(1:input$ml_k),`N (enfants)`=as.integer(table(res$km$cluster)))
    left_join(centers,sizes,by="Cluster")|>
      mutate(across(where(is.numeric),~round(.,3)))|>
      datatable(rownames=FALSE,class="compact stripe hover",options=list(dom="t"))
  })

  output$ml_elbow <- renderPlotly({
    req(input$ml_vars); d <- df_f()
    d_scaled <- d|>select(all_of(input$ml_vars))|>drop_na()|>scale()
    wss <- map_dbl(1:8,function(k) {
      set.seed(42); km <- kmeans(d_scaled,centers=k,nstart=10,iter.max=50); km$tot.withinss
    })
    elbow_df <- tibble(k=1:8,WSS=wss)
    p <- ggplot(elbow_df,aes(x=k,y=WSS))+
      geom_line(color=oms_bleu,linewidth=1.2)+geom_point(size=4,color=oms_rouge)+
      geom_vline(xintercept=input$ml_k,linetype="dashed",color=oms_orange)+
      labs(x="Nombre de clusters (k)",y="Inertie intra-cluster (WSS)",title="Méthode du coude")+
      theme_minimal(base_size=12)
    ggplotly(p)|>cfg_dl("ml_methode_coude")
  })

  # ══════════════════════════════════════════════════
  # TENDANCES & PROJECTIONS
  # ══════════════════════════════════════════════════
  output$trend_main <- renderPlotly({
    d <- df_f(); inds <- input$trend_inds
    if (input$trend_group=="Global") {
      res <- d|>group_by(survey_round)|>
        summarise(across(all_of(inds),~round(mean(.,na.rm=T)*100,1),.names="{.col}"),.groups="drop")|>
        pivot_longer(-survey_round,names_to="Indicateur",values_to="Prévalence")
      p <- ggplot(res,aes(x=survey_round,y=Prévalence,color=Indicateur,group=Indicateur,
                          text=paste0("Round: ",survey_round,"\n",Indicateur,": ",Prévalence,"%")))+
        geom_line(linewidth=1.3)+geom_point(size=3.5)
    } else {
      res <- d|>group_by(survey_round,.data[[input$trend_group]])|>
        summarise(across(all_of(inds[1]),~round(mean(.,na.rm=T)*100,1),.names="{.col}"),.groups="drop")|>
        pivot_longer(-c(survey_round,.data[[input$trend_group]]),names_to="Indicateur",values_to="Prévalence")
      p <- ggplot(res,aes(x=survey_round,y=Prévalence,color=.data[[input$trend_group]],group=.data[[input$trend_group]],
                          text=paste0("Round: ",survey_round,"\n",input$trend_group,": ",.data[[input$trend_group]],"\n",Prévalence,"%")))+
        geom_line(linewidth=1.2)+geom_point(size=3)
    }
    p <- p+{if(input$trend_threshold) geom_hline(yintercept=15,linetype="dashed",color=oms_rouge,linewidth=0.8)}+
      {if(input$trend_projection) geom_smooth(method="loess",se=TRUE,alpha=0.15,linewidth=0.5,linetype="dotted",aes(group=1))}+
      scale_color_brewer(palette="Set1")+
      labs(x="Survey round",y="Prévalence (%)",title="Évolution des indicateurs nutritionnels")+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text")|>layout(hovermode="x unified",legend=list(orientation="h",y=-0.2))|>cfg_dl("tendances")
  })

  output$trend_compare <- renderPlotly({
    d <- df_f(); rounds <- sort(unique(as.character(d$survey_round)))
    if (length(rounds)<2) { plot_ly()|>layout(title="Minimum 2 rounds requis"); return() }
    r1 <- rounds[1]; r_last <- rounds[length(rounds)]
    inds <- c("GAM","SAM","Stunting","Underweight","MUAC_GAM")
    comp <- bind_rows(
      d|>filter(as.character(survey_round)==r1)|>
        summarise(across(all_of(inds),~round(mean(.,na.rm=T)*100,1)))|>mutate(Round=r1),
      d|>filter(as.character(survey_round)==r_last)|>
        summarise(across(all_of(inds),~round(mean(.,na.rm=T)*100,1)))|>mutate(Round=r_last)
    )|>pivot_longer(-Round,names_to="Indicateur",values_to="Prévalence")
    p <- ggplot(comp,aes(x=Indicateur,y=Prévalence,fill=Round,
                         text=paste0(Indicateur,"\n",Round,": ",Prévalence,"%")))+
      geom_col(position="dodge",width=0.65,alpha=0.88)+
      scale_fill_manual(values=c(oms_bleu,oms_rouge))+
      labs(x="",y="Prévalence (%)",title=paste("Comparaison",r1,"vs",r_last))+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.2))|>cfg_dl("comparison_rounds")
  })

  output$trend_zscore <- renderPlotly({
    d <- df_f()
    res <- d|>group_by(survey_round)|>
      summarise(WHZ=round(mean(whz_score,na.rm=T),3),HAZ=round(mean(haz_score,na.rm=T),3),
                WAZ=round(mean(waz_score,na.rm=T),3),.groups="drop")|>
      pivot_longer(-survey_round,names_to="Score",values_to="Moyenne")
    p <- ggplot(res,aes(x=survey_round,y=Moyenne,color=Score,group=Score,
                        text=paste0("Round: ",survey_round,"\n",Score,": ",Moyenne)))+
      geom_line(linewidth=1.2)+geom_point(size=3)+
      geom_hline(yintercept=-2,linetype="dashed",color=oms_rouge,linewidth=0.7)+
      geom_hline(yintercept=0,linetype="dotted",color="gray50")+
      scale_color_manual(values=c(WHZ=oms_rouge,HAZ=oms_bleu,WAZ=oms_vert))+
      labs(x="Survey round",y="Score Z moyen",title="Évolution des scores Z")+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text")|>layout(hovermode="x unified",legend=list(orientation="h",y=-0.2))|>cfg_dl("tendances_z")
  })

  output$trend_table <- renderDT({
    d <- df_f(); inds <- input$trend_inds
    res <- d|>group_by(survey_round)|>
      summarise(N=n(),across(all_of(inds),~round(mean(.,na.rm=T)*100,1),.names="{.col}_pct"),.groups="drop")
    datatable(res,rownames=FALSE,class="compact stripe hover",options=list(dom="ftp"))
  })

  output$dl_trends <- downloadHandler(
    filename=function() paste0("tendances_indicateurs_",Sys.Date(),".xlsx"),
    content=function(file) {
      d <- df_f(); inds <- c("GAM","SAM","MAM","Stunting","Underweight","MUAC_GAM")
      res <- d|>group_by(survey_round)|>
        summarise(N=n(),across(all_of(inds),~round(mean(.,na.rm=T)*100,1),.names="{.col}_pct"),.groups="drop")
      writexl::write_xlsx(res,file)
    })

  # ══════════════════════════════════════════════════
  # CARTOGRAPHIE
  # ══════════════════════════════════════════════════
  output$leaf_map <- renderLeaflet({
    var <- input$leaf_var; fond <- input$leaf_fond
    df_geo <- if (input$leaf_niv=="Région") prev_region|>mutate(nom=as.character(region))
              else prev_dept|>mutate(nom=paste0(department," (",region,")"))
    vals <- df_geo[[var]]; pal_l <- colorNumeric(pal_gam,domain=vals,na.color="#aaa")
    popup_html <- paste0(
      "<div style='font-family:Arial;font-size:13px;min-width:220px;line-height:1.8'>",
      "<b style='color:#008DC9;font-size:14px'>",df_geo$nom,"</b><hr>",
      "🔴 GAM : <b>",round(df_geo$GAM_pct,1)," %</b><br>",
      "🟠 SAM : <b>",round(df_geo$SAM_pct,1)," %</b><br>",
      "📏 Stunting : <b>",round(df_geo$Stunting_pct,1)," %</b><br>",
      "📊 MUAC moy : <b>",round(df_geo$muac_moy,1)," mm</b><br>",
      "🛡️ ICV : <b>",round(df_geo$icv_moy,1),"/100</b><br>",
      "👶 N : <b>",df_geo$n,"</b></div>")
    m <- leaflet(df_geo)|>addProviderTiles(fond)|>
      addCircleMarkers(lat=~lat,lng=~lon,radius=~rescale(n,to=c(10,28)),
                       fillColor=~pal_l(vals),fillOpacity=0.88,color="white",weight=2,
                       popup=popup_html,label=~paste0(nom,": ",round(vals,1),"%"))|>
      addLegend(pal=pal_l,values=vals,title=var,position="bottomright",labFormat=labelFormat(suffix="%"))|>
      addScaleBar(position="bottomleft")|>addMiniMap(toggleDisplay=TRUE,minimized=TRUE)
    if (input$leaf_gadm&&!is.null(cameroun_sf)) {
      pal_poly <- colorNumeric(pal_gam,domain=cameroun_sf|>left_join(prev_region,by="region")|>pull(GAM_pct),na.color="#ddd")
      m <- m|>addPolygons(data=cameroun_sf|>left_join(prev_region,by="region"),
                          fillColor=~pal_poly(GAM_pct),fillOpacity=0.4,color=oms_bleu,weight=1.5,
                          popup=~paste0("<b>",region,"</b><br>GAM: ",round(GAM_pct,1),"%"))
    }
    if (input$leaf_pts) {
      d_pts <- df|>filter(!is.na(gps_latitude),!is.na(gps_longitude))
      pal_pt <- colorFactor(c(oms_vert,oms_orange,oms_rouge),domain=c("Normal","MAM","SAM"))
      m <- m|>addCircleMarkers(data=d_pts,lat=~gps_latitude,lng=~gps_longitude,radius=4,
                                fillColor=~pal_pt(nutr_cat),fillOpacity=0.85,color="white",weight=0.5,
                                clusterOptions=markerClusterOptions(disableClusteringAtZoom=11),
                                popup=~paste0("<b>",household_id,"</b><br>Statut: <b>",nutr_cat,"</b><br>WHZ=",round(whz_score,2)," MUAC=",muac_mm,"mm"))|>
        addLegend(pal=pal_pt,values=d_pts$nutr_cat,title="Statut",position="topleft")
    }
    m
  })

  output$gg_map <- renderPlotly({
    var <- input$gg_var
    pal_ch <- switch(input$gg_pal,"OMS standard"=pal_gam,
      "YlOrRd"=c("#ffffb2","#fed976","#feb24c","#fd8d3c","#e31a1c"),
      "Blues"=c("#eff3ff","#bdd7e7","#6baed6","#2171b5","#084594"),"Viridis"=viridis(5))
    if (input$gg_type=="Points proportionnels") {
      df_geo <- prev_region|>mutate(nom=as.character(region))
      p <- ggplot(df_geo)+{if(!is.null(cameroun_sf)) geom_sf(data=cameroun_sf,fill="grey92",color="white",linewidth=0.4) else geom_blank()}+
        geom_point(aes(x=lon,y=lat,size=.data[[var]],color=.data[[var]],text=paste0(nom,"\n",var,"=",round(.data[[var]],1),"%")),alpha=0.85)+
        scale_size_continuous(range=c(5,20),guide="none")+
        scale_color_gradientn(colors=pal_ch,name=paste0(var," (%)"))+
        labs(title=paste("Cameroun —",var))+theme_void()+
        theme(plot.title=element_text(hjust=.5,face="bold",size=13))
    } else {
      if (is.null(cameroun_sf)) { p <- ggplot()+annotate("text",x=0,y=0,label="GADM non disponible")+theme_void()
      } else {
        sf_data <- cameroun_sf|>left_join(prev_region,by="region")
        p <- ggplot(sf_data)+geom_sf(fill="grey92",color="white",linewidth=0.4)+
          geom_sf(data=filter(sf_data,!is.na(.data[[var]])),aes(fill=.data[[var]]),color="white",linewidth=0.5)+
          scale_fill_gradientn(colors=pal_ch,na.value="grey92",name=paste0(var," (%)"))+
          labs(title=paste("Cameroun —",var))+theme_void()+
          theme(plot.title=element_text(hjust=.5,face="bold",size=13))
      }
    }
    ggplotly(p,tooltip="text")|>cfg_dl("carto_cameroun")
  })

  output$world_map <- renderPlotly({
    highlight_countries <- switch(input$world_highlight,
      "Cameroun seul"=c("Cameroon"),
      "Afrique centrale"=c("Cameroon","Chad","Central African Republic","Nigeria","Niger","Gabon","Congo"),
      "Afrique subsaharienne"=world_sf$name[world_sf$continent=="Africa"])
    world_data <- world_sf|>mutate(fill_val=case_when(name=="Cameroon"~3,name%in%highlight_countries~2,TRUE~1))
    p <- ggplot(world_data)+
      geom_sf(aes(fill=factor(fill_val),text=name),color="white",linewidth=0.3)+
      scale_fill_manual(values=c("1"="grey85","2"="#b3d9ef","3"=oms_rouge),
                        labels=c("1"="Reste","2"="Afrique centrale","3"="Cameroun"),name="")+
      coord_sf(xlim=c(-20,50),ylim=c(-35,37))+
      labs(title="Cameroun en contexte régional africain")+theme_void()+
      theme(plot.title=element_text(hjust=.5,face="bold",size=13),legend.position="bottom")
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.1))|>cfg_dl("carte_monde")
  })

  output$gps_map <- renderLeaflet({
    d_pts <- df|>filter(!is.na(gps_latitude),!is.na(gps_longitude))
    col_var <- input$gps_color; col_vals <- d_pts[[col_var]]; niv <- length(unique(col_vals))
    pal_pt <- colorFactor(palette=get_pal_uni(niv,"OMS vert/rouge"),domain=col_vals)
    m <- leaflet(d_pts)|>addProviderTiles("CartoDB.Positron")|>
      addScaleBar(position="bottomleft")|>addMiniMap(toggleDisplay=TRUE,minimized=TRUE)
    cluster_opt <- if (input$gps_cluster) markerClusterOptions(disableClusteringAtZoom=11) else NULL
    m|>addCircleMarkers(lat=~gps_latitude,lng=~gps_longitude,
                        radius=input$gps_size,fillColor=~pal_pt(col_vals),
                        fillOpacity=0.85,color="white",weight=0.8,
                        clusterOptions=cluster_opt,
                        popup=~paste0("<b>",household_id,"</b><br>Village: ",village,"<br>",col_var,": ",col_vals,"<br>WHZ=",round(whz_score,2)," MUAC=",muac_mm,"mm"))|>
      addLegend(pal=pal_pt,values=col_vals,title=col_var,position="bottomright")
  })

  output$dl_leaf_excel <- downloadHandler(
    filename=function() paste0("indicateurs_geo_",Sys.Date(),".xlsx"),
    content=function(file) writexl::write_xlsx(list("Région"=prev_region,"Département"=prev_dept),file))

  output$dl_leaf_csv <- downloadHandler(
    filename=function() paste0("coordonnees_gps_",Sys.Date(),".csv"),
    content=function(file) df_f()|>filter(!is.na(gps_latitude))|>
      select(household_id,region,department,village,gps_latitude,gps_longitude,nutr_cat,whz_score,muac_mm,GAM,SAM)|>
      write_csv(file))

  # ══════════════════════════════════════════════════
  # INDICATEURS OMS
  # ══════════════════════════════════════════════════
  make_oms_table <- function(d) {
    inds <- list(GAM="GAM (WHZ<-2 ou œdème)",SAM="SAM (WHZ<-3 ou œdème)",
                 MAM="MAM (WHZ -3 à -2)",Stunting="Stunting (HAZ<-2)",
                 Sev_Stunting="Stunting sévère (HAZ<-3)",Underweight="Insuff. pondérale (WAZ<-2)",
                 MUAC_GAM="MUAC GAM (<125mm)",MUAC_SAM="MUAC SAM (<115mm)")
    calc_strate <- function(data,strate_lbl,group_var=NULL) {
      if (is.null(group_var))
        map2_dfr(names(inds),inds,function(v,l) calc_prev(data,v,l))|>mutate(Stratification=strate_lbl,Groupe="Global",.before=1)
      else
        data|>group_by(Groupe=.data[[group_var]])|>
          group_modify(~map2_dfr(names(inds),inds,function(v,l) calc_prev(.x,v,l)))|>ungroup()|>
          mutate(Stratification=strate_lbl,Groupe=as.character(Groupe),.before=1)
    }
    bind_rows(calc_strate(d,"Global"),calc_strate(d,"Région","region"),
              calc_strate(d,"Sexe","child_sex"),calc_strate(d,"Groupe d'âge","age_group"),
              calc_strate(d,"Déplacement","displacement_status"),calc_strate(d,"Sécu. alim.","fcs_cat"))
  }

  output$oms_table <- renderDT({
    d <- df_f()
    inds <- list(GAM="GAM (WHZ<-2 ou œdème)",SAM="SAM",MAM="MAM",Stunting="Stunting (HAZ<-2)",
                 Sev_Stunting="Stunting sévère",Underweight="Insuff. pondérale",
                 MUAC_GAM="MUAC GAM",MUAC_SAM="MUAC SAM")
    groupes <- list()
    if ("Global"%in%input$oms_groupes) groupes[["Global"]] <- list(var=NULL,label="Global")
    if ("Région"%in%input$oms_groupes) groupes[["Région"]] <- list(var="region",label="Région")
    if ("Sexe"%in%input$oms_groupes) groupes[["Sexe"]] <- list(var="child_sex",label="Sexe")
    if ("Groupe d'âge"%in%input$oms_groupes) groupes[["Âge"]] <- list(var="age_group",label="Groupe d'âge")
    if ("Statut déplacement"%in%input$oms_groupes) groupes[["Dépl."]] <- list(var="displacement_status",label="Déplacement")
    if ("Sécurité alim."%in%input$oms_groupes) groupes[["FCS"]] <- list(var="fcs_cat",label="Sécu. alim.")
    map_dfr(groupes,function(g) {
      if (is.null(g$var)) map2_dfr(names(inds),inds,function(v,l) calc_prev(d,v,l))|>mutate(Stratification="Global",Groupe="—",.before=1)
      else d|>group_by(Groupe=.data[[g$var]])|>
        group_modify(~map2_dfr(names(inds),inds,function(v,l) calc_prev(.x,v,l)))|>ungroup()|>
        mutate(Stratification=g$label,.before=1)
    })|>datatable(rownames=FALSE,filter="top",extensions="Buttons",
      options=list(pageLength=15,scrollX=TRUE,dom="Bftp",
        buttons=list(list(extend="csv",text="📥 CSV",filename=paste0("oms_",Sys.Date()),exportOptions=list(modifier=list(page="all"))),
                     list(extend="excel",text="📊 Excel",filename=paste0("oms_",Sys.Date()),title="Indicateurs OMS — Cameroun",exportOptions=list(modifier=list(page="all"))),
                     list(extend="pdf",text="📄 PDF",orientation="landscape",pageSize="A3"),
                     list(extend="copy",text="📋 Copier"))),
      class="compact stripe hover")|>
      formatStyle("Prév. (%)",background=styleColorBar(c(0,100),oms_bleu),backgroundSize="100% 70%")|>
      formatStyle("Prév. (%)",color=styleInterval(c(5,10,15,30),c(oms_vert,oms_jaune,oms_orange,oms_rouge,oms_rouge_f)),fontWeight="bold")
  })

  output$dl_oms_excel_full <- downloadHandler(
    filename=function() paste0("indicateurs_OMS_complet_",Sys.Date(),".xlsx"),
    content=function(file) {
      tbl <- make_oms_table(df_f())
      writexl::write_xlsx(list("Toutes strates"=tbl,"Global"=filter(tbl,Stratification=="Global"),
        "Par région"=filter(tbl,Stratification=="Région"),"Par sexe"=filter(tbl,Stratification=="Sexe"),
        "Par âge"=filter(tbl,Stratification=="Groupe d'âge"),"Par déplacement"=filter(tbl,Stratification=="Déplacement")),file)
    })

  output$dl_oms_csv_full <- downloadHandler(
    filename=function() paste0("indicateurs_OMS_complet_",Sys.Date(),".csv"),
    content=function(file) make_oms_table(df_f())|>write_csv(file))

  output$oms_forest <- renderPlotly({
    d <- df_f(); inds <- c("GAM","SAM","Stunting","Underweight","MUAC_GAM")
    res <- d|>group_by(region)|>group_modify(~map_dfr(inds,function(v) calc_prev(.x,v,v)))|>ungroup()
    p <- ggplot(res,aes(x=`Prév. (%)`,y=fct_reorder(paste0(region," — ",Indicateur),`Prév. (%)`),
                        color=Indicateur,text=paste0(region," — ",Indicateur,"\n",`Prév. (%)`,"% [",`IC95 bas`,"-",`IC95 haut`,"%]\nN=",N)))+
      geom_vline(xintercept=c(5,10,15),linetype="dashed",color=c(oms_jaune,oms_orange,oms_rouge),linewidth=0.6,alpha=0.7)+
      geom_errorbarh(aes(xmin=`IC95 bas`,xmax=`IC95 haut`),height=0.4,linewidth=0.8)+geom_point(size=3)+
      scale_color_manual(values=c(GAM=oms_rouge,SAM=oms_rouge_f,Stunting=oms_bleu,Underweight=oms_violet,MUAC_GAM=oms_orange))+
      labs(x="Prévalence (%)",y="")+theme_minimal(base_size=10)
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.15))|>cfg_dl("oms_forest_plot")
  })

  output$oms_gauge <- renderPlotly({
    gam_val <- round(mean(df_f()$GAM,na.rm=T)*100,1)
    plot_ly(type="indicator",mode="gauge+number+delta",value=gam_val,
      title=list(text="GAM globale (%)",font=list(size=16)),
      delta=list(reference=15,decreasing=list(color=oms_vert),increasing=list(color=oms_rouge)),
      gauge=list(axis=list(range=list(0,50)),
        bar=list(color=if(gam_val>=15) oms_rouge else if(gam_val>=10) oms_orange else oms_vert),
        steps=list(list(range=c(0,5),color="#e8f5e9"),list(range=c(5,10),color="#fffde7"),
                   list(range=c(10,15),color="#fff3e0"),list(range=c(15,30),color="#ffebee"),
                   list(range=c(30,50),color="#b71c1c")),
        threshold=list(line=list(color=oms_rouge,width=4),thickness=0.75,value=15)))|>
      layout(margin=list(l=20,r=30,b=20,t=60))|>cfg_dl("oms_jauge")
  })

  output$oms_trend <- renderPlotly({
    d <- df_f()
    res <- d|>group_by(survey_round)|>
      summarise(GAM=round(mean(GAM,na.rm=T)*100,1),SAM=round(mean(SAM,na.rm=T)*100,1),
                Stunting=round(mean(Stunting,na.rm=T)*100,1),.groups="drop")|>
      pivot_longer(-survey_round,names_to="Indicateur",values_to="Prév.")
    p <- ggplot(res,aes(x=survey_round,y=`Prév.`,color=Indicateur,group=Indicateur,
                        text=paste0("Round: ",survey_round,"\n",Indicateur,": ",`Prév.`,"%")))+
      geom_line(linewidth=1.2)+geom_point(size=3)+
      geom_hline(yintercept=15,linetype="dashed",color=oms_rouge,linewidth=0.7)+
      scale_color_manual(values=c(GAM=oms_rouge,SAM=oms_rouge_f,Stunting=oms_bleu))+
      labs(x="Survey round",y="Prévalence (%)")+theme_minimal(base_size=11)
    ggplotly(p,tooltip="text")|>layout(hovermode="x unified",legend=list(orientation="h",y=-0.2))|>cfg_dl("oms_trend")
  })

  # ══════════════════════════════════════════════════
  # CIBLAGE HUMANITAIRE
  # ══════════════════════════════════════════════════
  spi_data <- eventReactive(input$calc_spi, {
    d <- df_f()
    # Normaliser les poids
    w_total <- input$w_gam + input$w_sam + input$w_stunting + input$w_fcs + input$w_access
    if (w_total==0) return(NULL)
    w_gam <- input$w_gam/w_total; w_sam <- input$w_sam/w_total
    w_stun <- input$w_stunting/w_total; w_fcs <- input$w_fcs/w_total; w_acc <- input$w_access/w_total

    dept_data <- d|>group_by(region,department)|>
      summarise(n=n(), GAM_pct=mean(GAM,na.rm=T)*100, SAM_pct=mean(SAM,na.rm=T)*100,
                Stunting_pct=mean(Stunting,na.rm=T)*100,
                FCS_pauvre_pct=mean(fcs_cat=="Pauvre",na.rm=T)*100,
                Access_pct=mean(access_score,na.rm=T),
                muac_moy=mean(muac_mm,na.rm=T), icv_moy=mean(ICV,na.rm=T),
                lat=mean(gps_latitude,na.rm=T), lon=mean(gps_longitude,na.rm=T), .groups="drop")|>
      mutate(
        SPI = round(
          rescale(GAM_pct,to=c(0,100))*w_gam +
          rescale(SAM_pct,to=c(0,100))*w_sam +
          rescale(Stunting_pct,to=c(0,100))*w_stun +
          rescale(FCS_pauvre_pct,to=c(0,100))*w_fcs +
          rescale(100-Access_pct,to=c(0,100))*w_acc, 2),
        Priorité=cut(SPI,breaks=c(0,25,50,75,100),
                     labels=c("🟢 Faible","🟡 Modérée","🟠 Haute","🔴 Critique"),
                     include.lowest=TRUE)
      )|>arrange(desc(SPI))
    dept_data
  })

  output$target_ranking <- renderDT({
    req(spi_data())
    spi_data()|>select(Priorité,region,department,SPI,n,GAM_pct,SAM_pct,Stunting_pct,FCS_pauvre_pct)|>
      mutate(across(where(is.double),~round(.,1)))|>
      datatable(rownames=FALSE,extensions="Buttons",
        options=list(dom="Bftp",pageLength=15,
          buttons=list(list(extend="csv",text="📥 CSV",filename=paste0("spi_",Sys.Date())),
                       list(extend="excel",text="📊 Excel",filename=paste0("spi_",Sys.Date())))),
        class="compact stripe hover")|>
      formatStyle("SPI",background=styleColorBar(c(0,100),oms_rouge),backgroundSize="100% 70%")|>
      formatStyle("SPI",color=styleInterval(c(25,50,75),c(oms_vert,oms_jaune,oms_orange,oms_rouge)),fontWeight="bold")
  })

  output$target_chart <- renderPlotly({
    req(spi_data()); d <- spi_data()
    p <- ggplot(d,aes(x=SPI,y=fct_reorder(paste0(department," (",region,")"),SPI),
                      fill=Priorité,text=paste0(department,"\nSPI=",SPI,"\nN=",n)))+
      geom_col(alpha=0.88,width=0.7)+
      scale_fill_manual(values=c("🟢 Faible"=oms_vert,"🟡 Modérée"=oms_jaune,
                                  "🟠 Haute"=oms_orange,"🔴 Critique"=oms_rouge))+
      geom_vline(xintercept=50,linetype="dashed",color="gray50",linewidth=0.7)+
      labs(x="Score de priorité d'intervention (SPI)",y="",title="Classement des zones d'intervention")+
      theme_minimal(base_size=11)
    ggplotly(p,tooltip="text")|>layout(legend=list(orientation="h",y=-0.15))|>cfg_dl("spi_ciblage")
  })

  output$target_map <- renderLeaflet({
    req(spi_data()); d <- spi_data()
    pal_spi <- colorNumeric(c(oms_vert,oms_jaune,oms_orange,oms_rouge),domain=c(0,100))
    leaflet(d)|>addProviderTiles("CartoDB.Positron")|>
      addCircleMarkers(lat=~lat,lng=~lon,radius=~rescale(SPI,to=c(8,25)),
                       fillColor=~pal_spi(SPI),fillOpacity=0.88,color="white",weight=2,
                       popup=~paste0("<b>",department," (",region,")</b><br>SPI: <b>",SPI,"</b><br>Priorité: ",Priorité,"<br>N=",n),
                       label=~paste0(department,": SPI=",SPI))|>
      addLegend(pal=pal_spi,values=c(0,100),title="SPI",position="bottomright")|>
      addScaleBar()
  })

  output$target_action_plan <- renderUI({
    req(spi_data()); d <- spi_data()
    top3 <- head(d,3); critiques <- filter(d,SPI>=75)
    gam_glob <- round(mean(df_f()$GAM,na.rm=T)*100,1)
    tags$div(style="padding:15px;",
      tags$h4(style="color:#c9161d;font-weight:700;","🏥 Plan d'action prioritaire — Standard OMS/PAM"),
      tags$hr(),
      tags$div(style="background:#fce4ec;border-left:4px solid #c9161d;padding:12px;border-radius:4px;margin-bottom:12px;",
        tags$strong(paste0("⚠️ Situation nutritionnelle globale : GAM = ",gam_glob,"% — ",
                           if(gam_glob>=15)"URGENCE HUMANITAIRE" else if(gam_glob>=10)"SÉRIEUX" else "ALERTE"))),
      tags$h5(style="color:#c9161d;font-weight:700;","🔴 Zones critiques prioritaires :"),
      tags$ul(map(seq_len(min(nrow(critiques),5)),function(i) {
        z <- critiques[i,]
        tags$li(tags$strong(paste0(z$department," (",z$region,")")),
                paste0(" — SPI=",z$SPI," | GAM=",round(z$GAM_pct,1),"% | N=",z$n," enfants"))
      })),
      tags$h5(style="color:#e88400;font-weight:700;margin-top:12px;","📋 Recommandations standard OMS :"),
      tags$ol(
        tags$li("Déploiement immédiat des ATPE (aliments thérapeutiques prêts à l'emploi) dans les zones critiques"),
        tags$li("Mise à l'échelle du dépistage MUAC communautaire dans les 3 premières zones"),
        tags$li(paste0(nrow(filter(d,SPI>=50))," zones nécessitent un programme MAM — Suppléments alimentaires")),
        tags$li("Renforcement de l'accès à l'eau potable et à l'assainissement (WASH)"),
        tags$li("Coordination avec PAM pour distribution alimentaire ciblée"),
        tags$li("Mise en place de PCIMA (Prise en charge intégrée de la malnutrition aiguë)")
      ),
      tags$hr(),
      tags$p(style="font-size:11px;color:#888;",
             paste0("Généré le ",Sys.Date()," | Basé sur ",nrow(df_f())," enfants enquêtés | Méthode SPI composite OMS/PAM"))
    )
  })

  output$dl_spi <- downloadHandler(
    filename=function() paste0("spi_ciblage_humanitaire_",Sys.Date(),".xlsx"),
    content=function(file) {
      req(spi_data())
      writexl::write_xlsx(list("Score de priorité (SPI)"=spi_data()|>mutate(across(where(is.numeric),~round(.,2)))),file)
    })

  # ══════════════════════════════════════════════════
  # CONTEXTE — DÉTERMINANTS UNICEF
  # ══════════════════════════════════════════════════
  output$ctx_immediate_diet <- renderPlotly({
    d <- df_f()
    vars_diet <- c("breastfeeding_status","vitamin_a","meals_per_day","food_aid_received")
    vars_diet <- intersect(vars_diet,names(d))
    cnt <- map_dfr(vars_diet,function(v) {
      d|>count(.data[[v]])|>mutate(Variable=v,Modalité=as.character(.data[[v]]))|>
        select(Variable,Modalité,n)|>mutate(pct=round(n/sum(n)*100,1))
    })
    p <- ggplot(cnt,aes(x=Modalité,y=pct,fill=Variable,
                        text=paste0(Variable,"\n",Modalité,": ",pct,"% (N=",n,")")))+
      geom_col(alpha=0.85,width=0.7)+facet_wrap(~Variable,scales="free_x",ncol=2)+
      scale_fill_brewer(palette="Set2")+labs(x="",y="%",title="Alimentation & Pratiques")+
      theme_bw(base_size=10)+theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p,tooltip="text")|>cfg_dl("ctx_alimentation")
  })

  output$ctx_immediate_disease <- renderPlotly({
    d <- df_f()
    vars_dis <- c("diarrhea_last_2w","fever_last_2w","malaria_test","vaccination_status")
    vars_dis <- intersect(vars_dis,names(d))
    cnt <- map_dfr(vars_dis,function(v) {
      d|>count(.data[[v]])|>mutate(Variable=v,Modalité=as.character(.data[[v]]))|>
        select(Variable,Modalité,n)|>mutate(pct=round(n/sum(n)*100,1))
    })
    p <- ggplot(cnt,aes(x=Modalité,y=pct,fill=Variable,text=paste0(Variable,"\n",Modalité,": ",pct,"%")))+
      geom_col(alpha=0.85,width=0.7)+facet_wrap(~Variable,scales="free_x",ncol=2)+
      scale_fill_brewer(palette="Set1")+labs(x="",y="%",title="Maladies infectieuses")+
      theme_bw(base_size=10)+theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p,tooltip="text")|>cfg_dl("ctx_maladies")
  })

  output$ctx_underlying_food <- renderPlotly({
    d <- df_f()
    p <- ggplot(d,aes(x=fcs_cat,fill=fcs_cat))+
      geom_bar(alpha=0.85)+
      scale_fill_manual(values=c("Pauvre"=oms_rouge,"Limite"=oms_orange,"Acceptable"=oms_vert))+
      labs(x="",y="Effectif",title="Score de consommation alimentaire (FCS)")+
      theme_minimal(base_size=11)+theme(legend.position="none")
    ggplotly(p)|>cfg_dl("ctx_fcs")
  })

  output$ctx_underlying_care <- renderPlotly({
    d <- df_f()
    care_vars <- c("anc_visits","health_facility_distance_km","nutrition_program_access","mother_muac")
    care_vars <- intersect(care_vars,names(d))
    care_df <- d|>select(all_of(care_vars))|>
      summarise(across(where(is.numeric),~round(mean(.,na.rm=T),1)))|>
      pivot_longer(everything(),names_to="Indicateur",values_to="Valeur")
    p <- ggplot(care_df,aes(x=Indicateur,y=Valeur,fill=Indicateur,text=paste0(Indicateur,": ",Valeur)))+
      geom_col(alpha=0.85,width=0.6)+scale_fill_brewer(palette="Blues")+
      labs(x="",y="Valeur moyenne",title="Soins & Accès aux services")+
      theme_minimal(base_size=11)+theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p,tooltip="text")|>cfg_dl("ctx_soins")
  })

  output$ctx_basic_capital <- renderPlotly({
    d <- df_f()
    p <- ggplot(d,aes(x=head_education,fill=region))+
      geom_bar(position="dodge",alpha=0.85)+
      scale_fill_manual(values=couleurs_region)+
      labs(x="Éducation chef de ménage",y="Effectif",title="Capital humain & Ressources")+
      theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=30,hjust=1))
    ggplotly(p)|>layout(legend=list(orientation="h",y=-0.3))|>cfg_dl("ctx_capital")
  })

  output$ctx_basic_context <- renderPlotly({
    d <- df_f()
    chocs <- c("conflict_exposure","climate_shock","crop_failure","livestock_loss","price_shock")
    chocs <- intersect(chocs,names(d))
    cnt <- map_dfr(chocs,function(v) {
      n_oui <- sum(d[[v]]=="Yes",na.rm=T); n_tot <- sum(!is.na(d[[v]]))
      tibble(Choc=v,Prévalence=round(100*n_oui/n_tot,1),N=n_oui)
    })
    p <- ggplot(cnt,aes(x=fct_reorder(Choc,Prévalence),y=Prévalence,fill=Choc,
                        text=paste0(Choc,": ",Prévalence,"% (N=",N,")")))+
      geom_col(alpha=0.88,width=0.7)+coord_flip()+
      scale_fill_manual(values=brewer.pal(length(chocs),"YlOrRd"))+
      labs(x="",y="% ménages affectés",title="Chocs contextuels")+
      theme_minimal(base_size=11)+theme(legend.position="none")
    ggplotly(p,tooltip="text")|>cfg_dl("ctx_chocs")
  })

  output$ctx_multi <- renderPlotly({
    d <- df_f()
    if (input$ctx_type=="Corrélation Spearman") {
      vars_c <- c(input$ctx_y,"food_consumption_score","household_hunger_scale",
                  "coping_strategy_index","muac_mm","anc_visits","mother_muac","ICV")
      vars_c <- intersect(vars_c,names(d))
      mat <- d|>select(all_of(vars_c))|>drop_na()|>cor(method="spearman")
      corr_df <- mat[input$ctx_y,] |> enframe(name="Variable",value="rho") |>
        filter(Variable!=input$ctx_y) |> arrange(desc(abs(rho)))
      p <- ggplot(corr_df,aes(x=fct_reorder(Variable,rho),y=rho,fill=rho>0,
                              text=paste0(Variable,": ρ=",round(rho,3))))+
        geom_col(alpha=0.85,width=0.7)+coord_flip()+geom_hline(yintercept=0)+
        scale_fill_manual(values=c("TRUE"=oms_bleu,"FALSE"=oms_rouge),guide="none")+
        labs(x="",y=paste("Corrélation Spearman avec",input$ctx_y),title="Déterminants — Force des associations")+
        theme_minimal(base_size=11)
      ggplotly(p,tooltip="text")|>cfg_dl("ctx_correlations")
    } else {
      # Graphe en radar simplifié
      vars_radar <- c("food_consumption_score","ICV","muac_mm","anc_visits","access_score")
      vars_radar <- intersect(vars_radar,names(d))
      radar_data <- d|>group_by(region)|>
        summarise(across(all_of(vars_radar),~rescale(mean(.,na.rm=T),to=c(0,100)),
                         .names="{.col}"),.groups="drop")|>
        pivot_longer(-region,names_to="Variable",values_to="Score")
      p <- ggplot(radar_data,aes(x=Variable,y=Score,color=region,group=region,fill=region,
                                 text=paste0(region,"\n",Variable,": ",round(Score,1))))+
        geom_line(linewidth=1.2)+geom_point(size=3)+
        scale_color_manual(values=couleurs_region)+scale_fill_manual(values=couleurs_region)+
        coord_polar()+ylim(0,100)+
        labs(title="Profil multi-dimensionnel par région")+theme_minimal(base_size=11)+
        theme(axis.text.x=element_text(size=9),legend.position="bottom")
      ggplotly(p,tooltip="text")|>cfg_dl("ctx_radar")
    }
  })

  # ══════════════════════════════════════════════════
  # RAPPORT AUTOMATIQUE
  # ══════════════════════════════════════════════════
  output$report_preview <- renderUI({
    d <- df_f()
    gam_val <- round(mean(d$GAM,na.rm=T)*100,1)
    sam_val <- round(mean(d$SAM,na.rm=T)*100,1)
    stun_val <- round(mean(d$Stunting,na.rm=T)*100,1)
    n_total <- nrow(d)
    sev_level <- if(gam_val>=30)"URGENCE MAXIMALE" else if(gam_val>=15)"URGENCE HUMANITAIRE" else if(gam_val>=10)"SÉRIEUX" else if(gam_val>=5)"ALERTE" else "ACCEPTABLE"
    sev_color <- if(gam_val>=15)"#c9161d" else if(gam_val>=10)"#e88400" else "#4dac26"

    tags$div(style="background:white;border-radius:8px;padding:20px;box-shadow:0 2px 8px rgba(0,0,0,0.1);",
      # En-tête
      tags$div(style="background:linear-gradient(135deg,#001f3f,#008DC9);color:white;padding:20px;border-radius:6px;margin-bottom:15px;",
        tags$h3(style="margin:0;font-weight:700;",input$rep_titre),
        tags$p(style="margin:5px 0 0;opacity:0.9;",paste0(input$rep_auteur," | ",input$rep_org)),
        tags$p(style="margin:3px 0 0;font-size:12px;opacity:0.7;",paste0("Généré le ",Sys.Date()," | N = ",format(n_total,big.mark=" ")," enfants"))
      ),
      # Alerte
      tags$div(style=paste0("background:",sev_color,";color:white;padding:12px 15px;border-radius:6px;margin-bottom:15px;font-weight:700;font-size:14px;"),
        paste0("⚠️ Niveau de sévérité : ",sev_level," — GAM = ",gam_val,"% (N=",format(n_total,big.mark=" "),")")),
      # KPI
      tags$h5(style="color:#008DC9;font-weight:700;","📊 Indicateurs clés"),
      fluidRow(
        column(4,tags$div(style="background:#fff5f5;border:1px solid #ffcdd2;border-radius:6px;padding:12px;text-align:center;",
          tags$div(style="font-size:24px;font-weight:700;color:#c9161d;",paste0(gam_val,"%")),
          tags$div(style="font-size:12px;color:#555;","GAM (Malnutrition aiguë globale)"))),
        column(4,tags$div(style="background:#fff5f5;border:1px solid #ffcdd2;border-radius:6px;padding:12px;text-align:center;",
          tags$div(style="font-size:24px;font-weight:700;color:#640000;",paste0(sam_val,"%")),
          tags$div(style="font-size:12px;color:#555;","SAM (Malnutrition aiguë sévère)"))),
        column(4,tags$div(style="background:#fff9e6;border:1px solid #ffe082;border-radius:6px;padding:12px;text-align:center;",
          tags$div(style="font-size:24px;font-weight:700;color:#e88400;",paste0(stun_val,"%")),
          tags$div(style="font-size:12px;color:#555;","Stunting (Retard de croissance)")))
      ),
      tags$br(),
      # Résumé par région
      if ("Analyse par région"%in%input$rep_sections) {
        prev_rep <- d|>group_by(region)|>
          summarise(GAM_pct=round(mean(GAM,na.rm=T)*100,1),SAM_pct=round(mean(SAM,na.rm=T)*100,1),N=n(),.groups="drop")
        tags$div(
          tags$h5(style="color:#008DC9;font-weight:700;","🗺️ Analyse par région"),
          tags$table(style="width:100%;border-collapse:collapse;font-size:13px;",
            tags$thead(tags$tr(lapply(c("Région","GAM (%)","SAM (%)","N"),function(h)
              tags$th(style="background:#008DC9;color:white;padding:6px;",h)))),
            tags$tbody(apply(prev_rep,1,function(r) tags$tr(
              lapply(r,function(v) tags$td(style="padding:6px;border-bottom:1px solid #eee;",v)))))
          )
        )
      },
      tags$br(),
      # Recommandations
      if ("Recommandations"%in%input$rep_sections) {
        tags$div(
          tags$h5(style="color:#008DC9;font-weight:700;","🏥 Recommandations OMS/UNICEF"),
          tags$ol(
            tags$li("Déploiement immédiat des programmes PCIMA dans les zones à GAM ≥ 15%"),
            tags$li("Renforcement du dépistage MUAC au niveau communautaire"),
            tags$li("Expansion des programmes de supplémentation alimentaire (ATPE, ASPE)"),
            tags$li("Amélioration de l'accès à l'eau potable et à l'assainissement"),
            tags$li("Renforcement des CPN et vaccinations pour réduire la morbi-mortalité"),
            tags$li("Coordination intersectorielle PAM-UNICEF-OMS pour réponse intégrée"),
            tags$li("Mise en place d'un système de surveillance nutritionnelle continue (SUN)")
          )
        )
      },
      tags$hr(),
      tags$p(style="font-size:10px;color:#888;text-align:center;",
             paste0("Rapport généré avec NUTRICAM | Méthode SMART | Normes OMS 2006 | ",Sys.Date()))
    )
  })

  output$dl_report_html <- downloadHandler(
    filename=function() paste0("rapport_nutritionnel_",gsub(" ","_",input$rep_titre),"_",Sys.Date(),".html"),
    content=function(file) {
      d <- df_f()
      gam_val <- round(mean(d$GAM,na.rm=T)*100,1)
      sam_val <- round(mean(d$SAM,na.rm=T)*100,1)
      stun_val <- round(mean(d$Stunting,na.rm=T)*100,1)
      n_total <- nrow(d)
      prev_reg <- d|>group_by(region)|>
        summarise(GAM_pct=round(mean(GAM,na.rm=T)*100,1),SAM_pct=round(mean(SAM,na.rm=T)*100,1),
                  Stunting_pct=round(mean(Stunting,na.rm=T)*100,1),N=n(),.groups="drop")
      sev_level <- if(gam_val>=30)"URGENCE MAXIMALE" else if(gam_val>=15)"URGENCE HUMANITAIRE" else if(gam_val>=10)"SÉRIEUX" else "ALERTE"
      html_content <- paste0(
        "<!DOCTYPE html><html lang='fr'><head><meta charset='UTF-8'>",
        "<title>",input$rep_titre,"</title>",
        "<style>body{font-family:Arial,sans-serif;max-width:1000px;margin:0 auto;padding:20px;color:#333;}",
        ".header{background:linear-gradient(135deg,#001f3f,#008DC9);color:white;padding:30px;border-radius:10px;margin-bottom:20px;}",
        ".kpi-box{display:inline-block;padding:15px 25px;border-radius:8px;text-align:center;margin:8px;min-width:150px;}",
        ".critical{background:#fce4ec;border:2px solid #c9161d;}",
        ".alert{background:#fff3e0;border:2px solid #e88400;}",
        ".table{width:100%;border-collapse:collapse;margin:15px 0;}",
        ".table th{background:#008DC9;color:white;padding:8px;}",
        ".table td{padding:7px;border-bottom:1px solid #eee;}",
        "h2{color:#008DC9;}h3{color:#00426A;}",
        ".footer{text-align:center;font-size:11px;color:#888;margin-top:30px;border-top:1px solid #eee;padding-top:15px;}",
        ".sev-box{padding:15px;border-radius:6px;font-weight:700;font-size:15px;margin:15px 0;",
        "background:",if(gam_val>=15)"#c9161d" else "#e88400",";color:white;}",
        "</style></head><body>",
        "<div class='header'>",
        "<h1 style='margin:0;'>",input$rep_titre,"</h1>",
        "<p>",input$rep_auteur," | ",input$rep_org,"</p>",
        "<p style='opacity:0.8;font-size:13px;'>Date : ",format(Sys.Date(),"%d %B %Y")," | N = ",format(n_total,big.mark=" ")," enfants enquêtés</p>",
        "</div>",
        if (!is.null(input$rep_contexte)&&nchar(input$rep_contexte)>0)
          paste0("<div style='background:#f5f5f5;padding:15px;border-radius:6px;margin-bottom:15px;'><h3>Contexte</h3><p>",input$rep_contexte,"</p></div>")
        else "",
        "<div class='sev-box'>⚠️ Niveau de sévérité : ",sev_level," — GAM = ",gam_val,"% (N=",format(n_total,big.mark=" "),")</div>",
        "<h2>📊 Résumé exécutif</h2>",
        "<p>L'enquête nutritionnelle menée auprès de <strong>",format(n_total,big.mark=" "),"</strong> enfants de 0-59 mois révèle une situation nutritionnelle ",
        if(gam_val>=15)"<strong>d'urgence humanitaire</strong>" else if(gam_val>=10)"<strong>sérieuse</strong>" else "<strong>préoccupante</strong>",
        " dans les régions enquêtées du Cameroun.</p>",
        "<div style='display:flex;flex-wrap:wrap;gap:10px;margin:20px 0;'>",
        "<div class='kpi-box critical'><div style='font-size:28px;font-weight:700;color:#c9161d;'>",gam_val,"%</div><div style='font-size:12px;'>GAM globale</div></div>",
        "<div class='kpi-box critical'><div style='font-size:28px;font-weight:700;color:#640000;'>",sam_val,"%</div><div style='font-size:12px;'>SAM globale</div></div>",
        "<div class='kpi-box alert'><div style='font-size:28px;font-weight:700;color:#e88400;'>",stun_val,"%</div><div style='font-size:12px;'>Stunting global</div></div>",
        "</div>",
        "<h2>🗺️ Résultats par région</h2>",
        "<table class='table'><tr><th>Région</th><th>GAM (%)</th><th>SAM (%)</th><th>Stunting (%)</th><th>N</th></tr>",
        paste(apply(prev_reg,1,function(r) paste0("<tr><td><b>",r[1],"</b></td><td>",r[2],"</td><td>",r[3],"</td><td>",r[4],"</td><td>",r[5],"</td></tr>")),collapse=""),
        "</table>",
        "<h2>🏥 Recommandations</h2><ol>",
        "<li>Déploiement immédiat des programmes PCIMA dans les zones à GAM ≥ 15%</li>",
        "<li>Renforcement du dépistage MUAC au niveau communautaire</li>",
        "<li>Expansion des programmes ATPE/ASPE pour les cas SAM/MAM</li>",
        "<li>Amélioration de l'accès à l'eau potable (WASH)</li>",
        "<li>Coordination intersectorielle PAM-UNICEF-OMS-Ministère de la Santé</li>",
        "<li>Mise en place d'un système de surveillance nutritionnelle continue</li>",
        "</ol>",
        "<h2>📐 Méthodologie</h2>",
        "<p>L'enquête a été réalisée selon la <strong>méthodologie SMART</strong> (Standardized Monitoring and Assessment of Relief and Transitions). ",
        "Les indicateurs anthropométriques sont calculés selon les <strong>normes de croissance OMS 2006</strong>. ",
        "Les intervalles de confiance à 95% sont calculés par la méthode de Wilson. ",
        "Les seuils de classification utilisent la grille OMS (GAM : &lt;5% acceptable, 5-9.9% alerte, 10-14.9% sérieux, ≥15% critique, ≥30% urgence maximale).</p>",
        "<div class='footer'>",
        "<p>Rapport généré avec <strong>NUTRICAM</strong> — Système de surveillance nutritionnelle</p>",
        "<p>Méthode SMART | Normes OMS 2006 | ",format(Sys.Date(),"%d %B %Y"),"</p>",
        "</div></body></html>"
      )
      writeLines(html_content,file)
    })

  output$dl_report_data <- downloadHandler(
    filename=function() paste0("donnees_completes_rapport_",Sys.Date(),".xlsx"),
    content=function(file) {
      d <- df_f()
      writexl::write_xlsx(list(
        "Données individuelles"=d|>mutate(across(where(is.logical),as.integer)),
        "Prévalences globales"=bind_rows(
          calc_prev(d,"GAM","GAM (WHZ<-2 ou œdème)"),calc_prev(d,"SAM","SAM"),
          calc_prev(d,"MAM","MAM"),calc_prev(d,"Stunting","Stunting"),
          calc_prev(d,"Sev_Stunting","Stunting sévère"),calc_prev(d,"Underweight","Insuff. pondérale"),
          calc_prev(d,"MUAC_GAM","GAM MUAC (<125mm)"),calc_prev(d,"MUAC_SAM","SAM MUAC (<115mm)")),
        "Par région"=prev_region,"Par département"=prev_dept
      ),file)
    })

}

# ══════════════════════════════════════════════════════════════════════════════
# 7. LANCEMENT
# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui=ui, server=server)
