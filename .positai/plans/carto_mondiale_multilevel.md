# PLAN : Architecture de Cartographie Mondiale Multi-Pays & Multi-Niveaux

**Auteur :** Kevin Yimga (DSAMID)  
**Date :** 25 mars 2026  
**Contexte :** NutriAnalyse Pro v2 actuellement limité au Cameroun. Besoin de supporter enquêtes dans **n'importe quel pays du monde** avec hiérarchie administrative complète (Continent → Pays → Région → Département → Arrondissement).

---

## 1. PROBLÈME ACTUEL

### État présent
- Fonds de carte **hardcodés pour le Cameroun uniquement** (GADM L0/L1/L2/L3)
- `GADM_URLS` en `config_oms.R` : URLs fixes Cameroun
- Module cartographie (`module_carto_v2.R`) charge Cameroun au démarrage
- Pas de sélecteur de pays → entrée appliquée non flexible

### Limitation majeure
Si enquête au Tchad, RCA, Mali, Sénégal ou ailleurs → **pas de cartes disponibles** → cartographie inutilisable.

---

## 2. SOLUTION PROPOSÉE : Hiérarchie Cartographique Mondiale

### Architecture en 5 niveaux (Top-down)

```
NIVEAU 0 : Continents (7)
├─ Afrique
├─ Amérique du Nord/Centrale
├─ Amérique du Sud
├─ Asie
├─ Europe
├─ Océanie
└─ Arctique/Antarctique

NIVEAU 1 : Pays (~195)
├─ Cameroun
├─ Tchad
├─ RCA (République Centrafricaine)
├─ Mali
├─ Sénégal
├─ ... (tous les pays du monde)

NIVEAU 2 : Régions/Provinces (~4000+ à l'échelle mondiale)
└─ Pour Cameroun : 10 régions (Centre, Sud, Est, Littoral, etc.)
└─ Pour Tchad : 22 régions
└─ Pour Mali : 8 régions

NIVEAU 3 : Départements (~20000 à l'échelle mondiale)
└─ Pour Cameroun : ~58 départements

NIVEAU 4 : Arrondissements (~60000 à l'échelle mondiale)
└─ Pour Cameroun : ~360+ arrondissements
```

### 2.1 Nouveaux fichiers à créer

| Fichier | Rôle | Contenu |
|---------|------|---------|
| **config_gadm_worldwide.R** | Configuration mondiale centralisée | URL GADM par pays/niveau, métadonnées |
| **gadm_loader.R** | Chargeur intelligent GADM | Détecte pays → charge L0/L1/L2/L3/L4 auto |
| **carto_country_selector.R** | UI sélecteur de pays | Dropdown continents + pays, cascade |
| **gadm_cache_manager.R** | Gestionnaire cache disque | Stocke GADM localement pour offline |

### 2.2 Modifications fichiers existants

| Fichier | Changement |
|---------|-----------|
| **config_oms.R** | Enlever `GADM_URLS` hardcodés, remplacer par fonction dynamique |
| **module_carto_v2.R** | Accepter paramètre `country_code` (ISO 3166-1 alpha-3) |
| **ui_carto_v2.R** | Ajouter sélecteur pays/continent avant cartes |
| **app_v2_final.R** | Sidebar : nouveau "Sélectionner le pays" en haut (avant filtres géo) |
| **inferentiel_v4.R** | Pas de changement (statistiques indépendantes de géographie) |

---

## 3. IMPLÉMENTATION DÉTAILLÉE

### 3.1 config_gadm_worldwide.R (nouveau)

**Structure :**

```r
# Métadonnées GADM mondiale
GADM_WORLDWIDE <- list(
  # AFRIQUE
  "CMR" = list(
    name = "Cameroun",
    continent = "Afrique",
    zoom_bbox = c(ymin=1.65, ymax=13, xmin=8.5, xmax=16.2),
    levels = 4,  # L0, L1, L2, L3, L4 disponibles
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1_levels/gadm41_CMR_[LEVEL].json"
  ),
  "TCD" = list(
    name = "Tchad",
    continent = "Afrique",
    zoom_bbox = c(ymin=7, ymax=24, xmin=13, xmax=24),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1_levels/gadm41_TCD_[LEVEL].json"
  ),
  "CAF" = list(
    name = "République Centrafricaine",
    continent = "Afrique",
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1_levels/gadm41_CAF_[LEVEL].json"
  ),
  # ... (tous pays du monde)
)

# Fonction helper
get_gadm_url <- function(country_code, level) {
  if (!country_code %in% names(GADM_WORLDWIDE)) {
    stop(sprintf("Pays '%s' non supporté. Voir GADM_WORLDWIDE.", country_code))
  }
  pattern <- GADM_WORLDWIDE[[country_code]]$url_pattern
  gsub("[LEVEL]", level, pattern)
}

get_country_name <- function(country_code) {
  GADM_WORLDWIDE[[country_code]]$name
}

list_countries_by_continent <- function(continent) {
  names(Filter(function(x) x$continent == continent, GADM_WORLDWIDE))
}

list_continents <- function() {
  unique(sapply(GADM_WORLDWIDE, function(x) x$continent))
}
```

### 3.2 gadm_loader.R (nouveau)

**Fonctions principales :**

```r
# Charger shapefile GADM avec cache local
load_gadm_level <- function(country_code, level, cache_dir = "~/.gadm_cache") {
  
  # 1. Créer dossier cache
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # 2. Chemin cache local
  cache_file <- file.path(cache_dir, paste0(country_code, "_L", level, ".rds"))
  
  # 3. Si existe en cache → charger
  if (file.exists(cache_file)) {
    message(sprintf("✅ Chargé depuis cache: %s niveau %d", country_code, level))
    return(readRDS(cache_file))
  }
  
  # 4. Sinon télécharger depuis GADM
  url <- get_gadm_url(country_code, level)
  message(sprintf("⬇️ Téléchargement GADM: %s niveau %d...", country_code, level))
  
  # 5. Convertir JSON GeoJSON → sf
  gadm_sf <- sf::st_read(url, quiet = TRUE)
  
  # 6. Cacher localement
  saveRDS(gadm_sf, cache_file)
  message(sprintf("💾 Mis en cache: %s", cache_file))
  
  return(gadm_sf)
}

# Précharger plusieurs niveaux en parallèle
preload_gadm_cascade <- function(country_code, levels = 0:3) {
  sapply(levels, function(l) {
    tryCatch(
      load_gadm_level(country_code, l),
      error = function(e) {
        warning(sprintf("⚠️ Impossible charger niveau %d: %s", l, e$message))
        NULL
      }
    )
  })
}

# Nettoyer cache si trop gros (>1 GB)
cleanup_gadm_cache <- function(cache_dir = "~/.gadm_cache", max_size_gb = 2) {
  total_size <- sum(file.size(list.files(cache_dir, full.names = TRUE)))
  if (total_size > max_size_gb * 1e9) {
    message(sprintf("🗑️ Cache GADM > %d GB. Suppression fichiers anciens...", max_size_gb))
    # Supprimer fichiers < 7 jours
    files <- list.files(cache_dir, full.names = TRUE)
    old_files <- files[file.info(files)$mtime < Sys.time() - 7*24*3600]
    file.remove(old_files)
  }
}
```

### 3.3 carto_country_selector.R (nouveau)

**UI & Server :**

```r
# UI
carto_country_selector_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Sélecteur continent
    selectInput(
      ns("continent"),
      "🌍 Continent",
      choices = c("-- Choisir un continent --" = "", list_continents()),
      width = "100%"
    ),
    
    # Sélecteur pays (dynamique)
    uiOutput(ns("country_ui")),
    
    # Affichage sélection courante + boutons
    uiOutput(ns("selection_display")),
    
    # Boutons
    fluidRow(
      column(6, actionButton(ns("load_maps"), "📥 Charger cartes", 
                             class = "btn btn-oms", width = "100%")),
      column(6, actionButton(ns("clear_selection"), "❌ Réinitialiser", 
                            class = "btn btn-default", width = "100%"))
    )
  )
}

# Server
carto_country_selector_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Réactif : pays disponibles par continent
    available_countries <- reactive({
      if (input$continent == "" || is.null(input$continent)) return(NULL)
      country_codes <- list_countries_by_continent(input$continent)
      setNames(country_codes, sapply(country_codes, get_country_name))
    })
    
    # Render sélecteur pays
    output$country_ui <- renderUI({
      if (is.null(available_countries())) {
        selectInput(session$ns("country"), "🏴 Pays", choices = c())
      } else {
        selectInput(session$ns("country"), "🏴 Pays", 
                   choices = c("-- Choisir un pays --" = "", available_countries()),
                   width = "100%")
      }
    })
    
    # Affichage sélection courante
    output$selection_display <- renderUI({
      if (input$country == "" || is.null(input$country)) {
        tags$div(style = "color: #999; margin-top: 10px;",
                 "Aucun pays sélectionné")
      } else {
        country_name <- get_country_name(input$country)
        tags$div(
          style = "background: #e8f5e9; padding: 10px; border-radius: 6px; margin-top: 10px;",
          tags$strong(sprintf("✅ Sélection : %s (%s)", country_name, input$country)),
          tags$br(),
          tags$small(sprintf("Hiérarchie : Continent → Pays → Région → Département → Arrondissement"))
        )
      }
    })
    
    # Réactif : sélection (pour usage global)
    selected_country <- reactive(input$country)
    
    return(selected_country)
  })
}
```

### 3.4 Modifications app_v2_final.R (sidebar)

**Avant (actuel) :**
```r
sidebarMenu(
  menuItem("🏠 Tableau de bord", tabName = "accueil", ...),
  # ... 22 filtres spécifiques à Cameroun
)
```

**Après (nouveau) :**
```r
sidebarMenu(
  # ★ NOUVEAU : Sélecteur pays AVANT tous les filtres
  tags$div(
    style = "padding: 12px; background: #f0f8ff; border-bottom: 2px solid #008DC9; margin-bottom: 12px;",
    tags$h4(style = "margin: 0 0 8px; color: #00426A;", "⚙️ Configuration Géographique"),
    carto_country_selector_UI("country_selector")
  ),
  
  # Filtres existants (mais adaptatifs par pays)
  menuItem("🏠 Tableau de bord", tabName = "accueil", ...),
  # ...
)
```

### 3.5 module_carto_v2.R (modifications)

**Signature fonction changée :**
```r
# AVANT
run_cartography_analysis <- function(df, map_cols) { ... }

# APRÈS
run_cartography_analysis <- function(df, map_cols, country_code = "CMR") {
  # 1. Charger GADM du pays sélectionné (avec cache)
  gadm_l0 <- load_gadm_level(country_code, 0)  # Pays entier
  gadm_l1 <- load_gadm_level(country_code, 1)  # Régions
  gadm_l2 <- load_gadm_level(country_code, 2)  # Départements
  
  # 2. Spatial join : rassembler données GPS → zone administrative
  data_with_geom <- sf::st_join(
    sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326),
    gadm_l2,
    join = st_within
  )
  
  # 3. Agréger par zone administrative (automatique)
  agg_by_zone <- data_with_geom |>
    st_drop_geometry() |>
    group_by(NAME_2) |>  # Nom département (GADM)
    summarise(
      n = n(),
      GAM_pct = mean(GAM, na.rm = TRUE) * 100,
      SAM_pct = mean(SAM, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  
  # 4. Joindre résultats à géométrie GADM L2
  map_data <- gadm_l2 |>
    left_join(agg_by_zone, by = c("NAME_2" = "NAME_2"))
  
  # 5. Rendu cartographique (identique)
  render_choropleth_map(map_data, indicator = "GAM_pct")
}
```

---

## 4. WORKFLOW UTILISATEUR (Nouveau)

### Avant lancement analyse
1. **Démarrer app** → Sidebar affiche "⚙️ Configuration Géographique"
2. **Sélectionner continent** (Afrique, Asie, etc.)
3. **Sélectionner pays** (Cameroun, Tchad, Mali, etc.)
4. **Cliquer "📥 Charger cartes"** → Télécharge GADM L0/L1/L2/L3/L4 du pays
5. **Les filtres géographiques s'adaptent automatiquement**
   - Région : liste régions du pays choisi
   - Département : liste départements du pays choisi
   - Arrondissement : liste arrondissements du pays choisi
6. **Importer données** → XYZ avec `lat/lon` ou colonne administrative
7. **Lancer analyse cartographique** → Cartes choroplèthes au niveau département ou arrondissement

---

## 5. AVANTAGES DE CETTE ARCHITECTURE

| Avantage | Détail |
|----------|--------|
| **Universalité** | Fonctionne avec n'importe quel pays du monde |
| **Granularité flexible** | Support L0 (pays entier) à L4 (arrondissements) |
| **Cache intelligent** | GADM téléchargé une fois, réutilisé offline |
| **Adaptatif** | Filtres sidebar s'ajustent au pays choisi |
| **Non-invasif** | Pas de modification cœur statistiques |
| **Scalable** | Ajouter pays: juste ligne dans `GADM_WORLDWIDE` |

---

## 6. IMPLÉMENTATION PAR ÉTAPES

### **Étape 1 : Fondation (3-4h)**
- [ ] Créer `config_gadm_worldwide.R` avec liste 50+ pays clés (Afrique centrale+Ouest)
- [ ] Créer `gadm_loader.R` avec cache local
- [ ] Créer `carto_country_selector.R` (UI/Server)

### **Étape 2 : Intégration (2-3h)**
- [ ] Modifier `config_oms.R` : enlever URLs hardcodées
- [ ] Modifier `module_carto_v2.R` : accepter `country_code` paramètre
- [ ] Modifier `ui_carto_v2.R` : ajouter sélecteur
- [ ] Modifier `app_v2_final.R` : intégrer sélecteur sidebar

### **Étape 3 : Test & Docs (2h)**
- [ ] Tester avec Cameroun (baseline)
- [ ] Tester avec Tchad, RCA
- [ ] Documenter dans `DOCUMENTATION_COMPLETE_NUTRIANALYSE_PRO_V2.md`
- [ ] Ajouter section "Configuration Pays" en début doc

### **Étape 4 : Expansion (1h)**
- [ ] Ajouter pays supplémentaires à `GADM_WORLDWIDE` selon besoin
- [ ] Optimiser performance cache pour gros pays (Nigeria, Kenya, etc.)

---

## 7. QUESTIONS À CLARIFIER AVANT IMPLÉMENTATION

1. **Quels pays prioritaires ?**
   - Afrique de l'Ouest/Centre : Mali, Sénégal, Côte d'Ivoire, Burkina, Niger, Mauritanie, Guinée, etc.?
   - Autres régions: Asie (Yemen?), Amérique Latine?
   
2. **Niveaux administratifs à supporter ?**
   - Tous (L0-L4) ou seulement jusqu'à L2 (départements) pour performance?

3. **Stockage cache GADM ?**
   - Local `~/.gadm_cache` ou à choisir par utilisateur?
   - Limite taille (2 GB par défaut)?

4. **Rétrocompatibilité ?**
   - Garder Cameroun comme défaut à lancement?
   - Migration données existantes Cameroun?

---

## 8. FICHIERS À CRÉER / MODIFIER

### Créer (nouveaux)
- `config_gadm_worldwide.R` (~200 lignes)
- `gadm_loader.R` (~150 lignes)
- `carto_country_selector.R` (~200 lignes)

### Modifier (existants)
- `config_oms.R` : enlever `GADM_URLS` hardcodés
- `module_carto_v2.R` : ajouter paramètre `country_code`
- `ui_carto_v2.R` : adapter à pays variable
- `app_v2_final.R` : sidebar + sélecteur
- `DOCUMENTATION_COMPLETE_NUTRIANALYSE_PRO_V2.md` : section cartographie mondiale

---

## ✅ PLAN PRÊT POUR APPROBATION

Ce plan offre une **solution complète, modulaire et scalable** pour supporter cartographie mondiale dans NutriAnalyse Pro v2, passant de l'approche actuelle (Cameroun dur-codé) à une architecture flexible (n'importe quel pays du monde).

**Durée estimée:** 8-10 heures de développement.
