# NutriAnalyse Pro v2 — Application Shiny Universelle
## Niveau publication scientifique | Style OMS/UNICEF officiel

---

## Structure du projet

```
mon_projet/
├── app_v2.R      ← Application principale
├── utils.R       ← Fonctions utilitaires (OBLIGATOIRE — dans le même dossier)
└── data/         ← Optionnel : vos fichiers de données
```

## Installation des packages (une seule fois)

```r
install.packages(c(
  # Interface
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",

  # Manipulation
  "tidyverse", "scales", "forcats",

  # Tableaux
  "DT", "gtsummary", "gt", "flextable",

  # Graphiques
  "plotly", "ggiraph", "ggdist", "corrplot", "viridis", "RColorBrewer", "e1071",

  # Cartes
  "leaflet", "leaflet.extras", "sf", "rnaturalearth", "rnaturalearthdata",

  # Statistiques
  "binom", "nortest",

  # Multivariée & clustering
  "FactoMineR", "factoextra", "cluster", "NbClust", "dbscan",

  # Import multi-format
  "readxl", "haven", "arrow",

  # Export
  "rmarkdown", "officer", "writexl", "knitr", "kableExtra"
))
```

## Lancement

```r
# Dans RStudio : ouvrir app_v2.R → Run App
shiny::runApp("app_v2.R")
```

---

## Les 10 onglets

| # | Onglet | Ce que vous y faites |
|---|--------|----------------------|
| 🏠 | **Tableau de bord** | Vue d'ensemble exécutive · 12 KPI · Alertes OMS · Jauges · Scores Z |
| ⚙️ | **Import & Mappeur** | Upload CSV/Excel/SPSS/Stata/Parquet · Détection auto + correction |
| 🔍 | **Exploration** | Tableau filtrable · 17 statistiques par variable · Graphique automatique |
| 📊 | **Univarié** | 11 types de graphiques · Palette OMS · Stats complètes · Export HD |
| 📊 | **Bivarié** | 10 types de graphiques · Test statistique automatique · Interprétation |
| 📊 | **Descriptive OMS** | gtsummary · p-values · corrplot Spearman · Scores Z par groupe |
| 🧪 | **Inférentielle** | Chi²/Fisher · Mann-Whitney/t-test auto · KW + BH · Corrélation · Shapiro · Régression logistique |
| 🔬 | **Réduction de dimension** | ACP · ACM · FAMD · AFC (FactoMineR) · Éboulis · Cercle · Biplot · Contributions · Cos² |
| 🔗 | **Clustering** | K-means · CAH Ward · DBSCAN · Silhouette auto · Dendrogramme · Profils |
| 📋 | **Indicateurs OMS/PAM** | GAM · SAM · MAM · Stunting · MUAC · FCS/HHS · IC95% Wilson · Forest plot · Jauges |
| 🌍 | **Cartographie** | Leaflet HD · Heatmap · GADM · ggplot2 · Carte monde · GPS clusterisés |
| 📄 | **Rapport exportable** | PDF · Word · Excel complet auto-généré |

---

## Indicateurs calculés automatiquement

### OMS 2006
- GAM (WHZ < -2 DS ou œdèmes bilatéraux)
- SAM (WHZ < -3 DS ou œdèmes)
- MAM (-3 ≤ WHZ < -2 DS, sans œdèmes)
- Stunting (HAZ < -2 DS)
- Stunting sévère (HAZ < -3 DS)
- Insuffisance pondérale (WAZ < -2 DS)
- MUAC GAM (< 125 mm), MAM (115-124 mm), SAM (< 115 mm)
- Prévalences + IC 95% (méthode Wilson)

### PAM
- FCS catégorisé : Pauvre / Limite / Acceptable
- HHS catégorisé : Aucune / Légère / Modérée / Sévère

### Indice composite
- ICV (Indice Composite de Vulnérabilité) 0-100

---

## Détection automatique des colonnes

L'application reconnaît automatiquement les noms standards et leurs variantes dans 5 langues.
Si une colonne n'est pas reconnue, utilisez le **Mappeur** (onglet Import & Mappeur).

| Variable | Exemples de noms reconnus |
|----------|--------------------------|
| WHZ | whz_score, whz, wfh, weight_for_height_z |
| HAZ | haz_score, haz, hfa, height_for_age_z |
| MUAC | muac_mm, muac, pb, pbm, brachial |
| Âge | child_age_months, age_months, agemos |
| Région | region, zone, admin1, wilaya, province |
| GPS | gps_latitude, latitude, lat, coord_lat |
