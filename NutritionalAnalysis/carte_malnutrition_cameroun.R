# =============================================================================
# CARTOGRAPHIE DE LA MALNUTRITION AU CAMEROUN
# Auteur : [Votre nom]
# Date   : 2024
# Description :
#   Ce script produit deux types de cartes du Cameroun :
#     1. Une carte statique   avec ggplot2 (choroplèthe + points GPS)
#     2. Une carte interactive avec plotly  (infobulles au survol)
#   Les données proviennent de l'enquête nutritionnelle (données/donnees_malnutrition.csv)
# =============================================================================
#     1. Une carte statique   avec ggplot2 (choroplèthe + points GPS)
#     2. Une carte interactive avec plotly  (infobulles au survol)
#   Les données proviennent de l'enquête nutritionnelle (données/donnees_malnutrition.csv)
# =============================================================================


# ── 0. Packages nécessaires ──────────────────────────────────────────────────
# Installez-les si besoin avec : install.packages(c("sf", "ggplot2", "plotly", "dplyr"))

library(sf)       # Lecture et manipulation des shapefiles
library(dplyr)    # Manipulation des données (group_by, summarise, etc.)
library(ggplot2)  # Graphiques et cartes statiques
library(plotly)   # Cartes interactives


# ── 1. Charger les données de l'enquête ──────────────────────────────────────
df_map <- read.csv("data/donnees_malnutrition.csv", encoding = "UTF-8")

# Aperçu rapide : colonnes géographiques et nutritionnelles utilisées
# - region, district, village  → localisation administrative
# - gps_latitude, gps_longitude → coordonnées GPS du ménage
# - GAM, SAM, MAM              → indicateurs binaires (1 = cas, 0 = non)
# - nutr_cat                   → catégorie nutritionnelle ("SAM", "MAM", "Normal")
# - whz_score, muac_mm         → scores de malnutrition individuels


# ── 2. Calculer les prévalences par région ───────────────────────────────────
# On agrège les données au niveau région pour la carte choroplèthe
# GAM (Global Acute Malnutrition) = SAM + MAM

donnees_region <- df_map |>
  group_by(region) |>
  summarise(
    n_enfants    = n(),
    GAM_pct      = mean(GAM,      na.rm = TRUE) * 100,   # % malnutrition aiguë globale
    SAM_pct      = mean(SAM,      na.rm = TRUE) * 100,   # % malnutrition sévère
    MAM_pct      = mean(MAM,      na.rm = TRUE) * 100,   # % malnutrition modérée
    Stunting_pct = mean(Stunting, na.rm = TRUE) * 100,   # % retard de croissance
    .groups = "drop"
  )

donnees_region  # Afficher le tableau de résumé


# ── 3. Télécharger le fond de carte du Cameroun (GADM) ───────────────────────
# GADM fournit des shapefiles officiels des pays.
# Niveau 1 = régions administratives (10 régions du Cameroun)
# Source : https://gadm.org

url_gadm <- "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_1.json"
cameroun_sf <- st_read(url_gadm, quiet = TRUE) |>
  rename(region = NAME_1)  # Renommer pour correspondre à nos données


# ── 4. Joindre le shapefile avec les données de malnutrition ─────────────────
# left_join : toutes les régions du Cameroun sont conservées.
# Les régions sans données (non enquêtées) auront NA → elles resteront en gris.

carte_sf <- cameroun_sf |>
  left_join(donnees_region, by = "region")


# ── 5. CARTE STATIQUE avec ggplot2 ───────────────────────────────────────────
#
# Principe :
#   - geom_sf() dessine des polygones (régions) depuis un objet sf
#   - aes(fill = ...) colorie les polygones selon une variable numérique
#   - scale_fill_gradient() définit les couleurs (jaune = faible, rouge = élevé)
#   - theme_void() supprime les axes et le quadrillage (adapté aux cartes)

carte_ggplot <- ggplot(carte_sf) +
  # Couche 1 : fond gris pour TOUTES les régions du Cameroun
  geom_sf(fill = "grey90", color = "white", linewidth = 0.4) +
  # Couche 2 : régions enquêtées, colorées par taux de GAM
  geom_sf(
    data = filter(carte_sf, !is.na(GAM_pct)),  # seulement les régions avec données
    aes(fill = GAM_pct),
    color = "white", linewidth = 0.5
  ) +
  # Étiquettes : nom de la région + valeur du GAM
  geom_text(
    data = filter(carte_sf, !is.na(GAM_pct)) |>
      st_centroid() |>
      mutate(
        lon = st_coordinates(geometry)[, 1],
        lat = st_coordinates(geometry)[, 2]
      ) |>
      st_drop_geometry(),
    aes(x = lon, y = lat,
        label = paste0(region, "\n", round(GAM_pct, 1), "%")),
    size = 3, fontface = "bold", color = "white"
  ) +
  # Échelle de couleur : jaune clair → rouge foncé
  scale_fill_gradient(
    low      = "#f7dc6f",
    high     = "#c0392b",
    na.value = "grey90",   # régions sans données = gris
    name     = "GAM (%)",
    limits   = c(60, 80)   # ajuster selon vos données
  ) +
  labs(
    title    = "Prévalence de la Malnutrition Aiguë Globale (GAM) au Cameroun",
    subtitle = "Régions enquêtées : Adamaoua, Nord, Extrême-Nord",
    caption  = "Source : Enquête nutritionnelle 2024"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle   = element_text(hjust = 0.5, color = "grey40", size = 10),
    plot.caption    = element_text(hjust = 1,   color = "grey50", size = 8),
    legend.position = "right"
  )

# Afficher la carte
carte_ggplot


# ── 6. CARTE AVEC POINTS GPS (ggplot2) ───────────────────────────────────────
#
# On superpose les coordonnées GPS de chaque ménage sur la carte choroplèthe.
# Chaque point est coloré selon le statut nutritionnel de l'enfant.

# Convertir les données GPS en objet spatial sf (système de coordonnées WGS84)
points_enquete <- df_map |>
  filter(!is.na(gps_latitude), !is.na(gps_longitude)) |>
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326)

carte_avec_points <- ggplot() +
  # Fond : toutes les régions du Cameroun
  geom_sf(data = carte_sf, fill = "grey92", color = "white", linewidth = 0.4) +
  # Régions enquêtées colorées (semi-transparentes pour voir les points dessous)
  geom_sf(
    data  = filter(carte_sf, !is.na(GAM_pct)),
    aes(fill = GAM_pct),
    color = "white", linewidth = 0.5, alpha = 0.55
  ) +
  # Points GPS des ménages, colorés par statut nutritionnel
  geom_sf(
    data  = points_enquete,
    aes(color = nutr_cat),
    size  = 1.2, alpha = 0.75
  ) +
  scale_fill_gradient(
    low = "#f7dc6f", high = "#c0392b",
    na.value = "grey92", name = "GAM région (%)",
    limits = c(60, 80)
  ) +
  scale_color_manual(
    values = c("SAM" = "#e74c3c", "MAM" = "#e67e22", "Normal" = "#27ae60"),
    name   = "Statut individuel"
  ) +
  labs(
    title    = "Malnutrition au Cameroun : Prévalence régionale et cas individuels",
    subtitle = "Fond de carte = taux de GAM régional | Points = statut nutritionnel de l'enfant",
    caption  = "Source : Enquête nutritionnelle 2024"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle   = element_text(hjust = 0.5, color = "grey40", size = 9),
    plot.caption    = element_text(hjust = 1,   color = "grey50", size = 8),
    legend.position = "right"
  )

carte_avec_points


# ── 7. CARTE INTERACTIVE avec plotly ─────────────────────────────────────────
#
# Option A : Conversion rapide de la carte ggplot2 → plotly avec ggplotly()
#   Avantage : très simple, réutilise le code ggplot2
#   Limite   : les infobulles sont moins personnalisables

# On ajoute l'aesthetic "text" pour personnaliser les infobulles
carte_pour_plotly <- ggplot(carte_sf) +
  geom_sf(fill = "grey90", color = "white", linewidth = 0.4) +
  geom_sf(
    data = filter(carte_sf, !is.na(GAM_pct)),
    aes(
      fill = GAM_pct,
      # "text" est lu par plotly pour construire les infobulles
      text = paste0(
        "Région : ", region,      "\n",
        "GAM    : ", round(GAM_pct,      1), "%\n",
        "SAM    : ", round(SAM_pct,      1), "%\n",
        "MAM    : ", round(MAM_pct,      1), "%\n",
        "Retard croissance : ", round(Stunting_pct, 1), "%\n",
        "Enfants enquêtés : ", n_enfants
      )
    ),
    color = "white", linewidth = 0.5
  ) +
  scale_fill_gradient(
    low = "#f7dc6f", high = "#c0392b",
    na.value = "grey90", name = "GAM (%)", limits = c(60, 80)
  ) +
  labs(title = "Prévalence GAM au Cameroun — Carte interactive") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

# Conversion en carte plotly (tooltip = "text" active notre infobulle personnalisée)
carte_plotly_simple <- ggplotly(carte_pour_plotly, tooltip = "text")
carte_plotly_simple


# ── 8. CARTE INTERACTIVE COMPLÈTE avec plotly natif ──────────────────────────
#
# Option B : Carte plotly native avec add_sf() et add_trace()
#   Avantage : plus de contrôle sur l'apparence et les interactions
#   On superpose les polygones (régions) ET les points GPS

# Préparer les données des points avec le texte des infobulles
points_df_plotly <- df_map |>
  filter(!is.na(gps_latitude), !is.na(gps_longitude)) |>
  mutate(
    info = paste0(
      "Village : ",  village,           "\n",
      "District : ", district,          "\n",
      "Région : ",   region,            "\n",
      "Statut : ",   nutr_cat,          "\n",
      "WHZ : ",      round(whz_score, 2), "\n",
      "MUAC : ",     muac_mm,           " mm"
    )
  )

carte_plotly_complete <- plot_ly() |>
  # Couche 1 : polygones du Cameroun (fond gris)
  add_sf(
    data      = cameroun_sf,
    color     = I("grey85"),
    line      = list(color = "white", width = 1),
    name      = "Cameroun",
    hoverinfo = "none"
  ) |>
  # Couche 2 : régions enquêtées colorées selon le taux de GAM
  add_sf(
    data      = filter(carte_sf, !is.na(GAM_pct)),
    color     = ~GAM_pct,
    colors    = c("#f7dc6f", "#c0392b"),
    text      = ~paste0(
      "<b>", region, "</b><br>",
      "GAM : ", round(GAM_pct,      1), "%<br>",
      "SAM : ", round(SAM_pct,      1), "%<br>",
      "MAM : ", round(MAM_pct,      1), "%<br>",
      "Retard croissance : ", round(Stunting_pct, 1), "%<br>",
      "Enfants enquêtés : ", n_enfants
    ),
    hovertemplate = "%{text}<extra></extra>",  # <extra></extra> = retire le nom de la trace
    showlegend    = FALSE
  ) |>
  # Couche 3 : points GPS des ménages, colorés par statut nutritionnel
  add_trace(
    data          = points_df_plotly,
    type          = "scatter",
    mode          = "markers",
    x             = ~gps_longitude,
    y             = ~gps_latitude,
    color         = ~nutr_cat,
    colors        = c("SAM" = "#e74c3c", "MAM" = "#e67e22", "Normal" = "#27ae60"),
    marker        = list(size = 5, opacity = 0.75),
    text          = ~info,
    hovertemplate = "%{text}<extra></extra>",
    name          = ~nutr_cat  # nom de chaque catégorie dans la légende
  ) |>
  layout(
    title  = list(text = "<b>Malnutrition au Cameroun — Carte interactive</b>", x = 0.5),
    xaxis  = list(title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis  = list(title = "", showgrid = FALSE, zeroline = FALSE),
    legend = list(title = list(text = "<b>Statut nutritionnel</b>")),
    margin = list(l = 0, r = 0, t = 50, b = 0)
  )

carte_plotly_complete


# =============================================================================
# RÉSUMÉ DES FONCTIONS CLÉS
# =============================================================================
#
#  st_read(url)          → lire un shapefile ou GeoJSON (local ou en ligne)
#  left_join()           → joindre les données au shapefile par une colonne commune
#  geom_sf()             → dessiner des polygones/points sf dans ggplot2
#  aes(fill = variable)  → colorier les polygones selon une variable
#  scale_fill_gradient() → définir les couleurs de la légende continue
#  st_as_sf(coords = ...) → convertir un tableau GPS en objet sf (points)
#  ggplotly(carte, tooltip = "text") → convertir ggplot2 en plotly interactif
#  add_sf()              → ajouter des polygones sf à une figure plotly
#  add_trace()           → ajouter des points (scatter) à une figure plotly
#
# =============================================================================
