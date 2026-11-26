# Intégration du module cartographique v2

## Fichiers à ajouter dans votre dossier projet
- `module_carto_v2.R` — fonctions cartographiques (GADM, Leaflet, ggplot2)
- `ui_carto_v2.R`     — UI et server de l'onglet cartographie

## Étape 1 — Ajouter les source() en tête de app_v2.R

```r
source("utils.R")
source("inferentiel_v3.R")
source("module_epidemio.R")
source("module_predictif.R")
source("module_survie.R")
source("module_rapport.R")
source("module_carto_v2.R")   # ← NOUVEAU
source("ui_carto_v2.R")       # ← NOUVEAU
```

## Étape 2 — Remplacer l'onglet carto dans l'UI de app_v2.R

Trouvez dans app_v2.R le bloc :
```r
# ════════════════════════════════════════════════════
# 8 — CARTOGRAPHIE
# ════════════════════════════════════════════════════
tabItem(tabName="carto",
  ...
)
```

Remplacez-le entièrement par :
```r
ui_carto_v2,
```

## Étape 3 — Remplacer les outputs carto dans le server() de app_v2.R

Trouvez tous les `output$leaf_map`, `output$gg_map`, `output$world_map`,
`output$gps_map`, `output$leaf_var_ui`, `output$leaf_grp_ui`, etc.
et supprimez-les.

Ajoutez à la place, à la fin du server() et avant le `}` final :

```r
# ── Module cartographie v2 ──────────────────────────
server_carto_v2(input, output, session, df_f, map_cols)
```

## Ce que le nouveau module apporte

### Niveaux géographiques (GADM automatique)
- **Région** → GADM L1 (10 régions du Cameroun)
- **Département** → GADM L2 (~58 départements)
- **Arrondissement** → GADM L3 (~374 arrondissements)

### 5 types de cartes
1. **Choroplèthe** — polygones colorés selon l'indicateur OMS
2. **Cercles proportionnels** — taille = effectif, couleur = indicateur
3. **Points GPS** — chaque individu, clusterisés intelligemment
4. **Heatmap** — densité spatiale des enquêtés
5. **Étiquettes** — nom + valeur sur chaque zone

### Tous les indicateurs OMS/PAM
- GAM, SAM, MAM, Stunting, Stunting sévère, Insuffisance pondérale
- MUAC GAM, MUAC SAM
- WHZ moyen, HAZ moyen, WAZ moyen, MUAC moyen
- ICV, FCS, N enquêtés

### Filtres dynamiques
- Filtre par Région → met à jour automatiquement le filtre Département
- Filtre par Département → zoom sur les arrondissements de ce département

### 4 onglets dans la carte
1. **Leaflet interactif** — zoom, clic, popup riche OMS
2. **ggplot2 statique** — exportable PNG/PDF haute résolution
3. **Afrique centrale** — contexte régional avec pays voisins
4. **Tableau** — prévalences par zone + barplot + forest plot IC95%
5. **GPS détaillé** — points individuels avec clustering personnalisé

### Popups style OMS
Chaque zone affiche :
- N enquêtés
- GAM, SAM, MAM, Stunting, Underweight avec code couleur OMS
- Alerte automatique si seuil dépassé (⚠)
- WHZ, HAZ, MUAC, ICV moyens

### Export
- PNG 300 DPI (20×16 cm) — prêt pour publication
- PDF vectoriel — pour insertion dans thèse Word/LaTeX

## Packages supplémentaires (si pas encore installés)

```r
install.packages(c(
  "sf", "leaflet", "leaflet.extras",
  "rnaturalearth", "rnaturalearthdata",
  "plotly", "ggplot2", "scales"
))
```

## Note sur GADM

Les shapefiles GADM sont téléchargés automatiquement depuis internet
la première fois, puis mis en cache (pas de re-téléchargement).

Si vous n'avez pas internet, téléchargez manuellement :
- https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_1.json
- https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_2.json
- https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_3.json

Placez-les dans votre dossier projet et modifiez GADM_URLS dans module_carto_v2.R.
