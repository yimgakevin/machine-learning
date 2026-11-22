# Plan: Refonte EXPERT des Cartes — Niveau Publication WHO

## 🎯 Objectif
Transformer les cartes de "bonnes mais basiques" à **"niveau publication scientifique WHO/UNICEF"** avec analyse spatiale avancée, intelligence géographique, et prédiction.

## 📦 Architecture Proposée

### **Tier 1: Cartes Intelligentes** (2-3h)

#### `carto_expert_engine.R` — Moteur cartographique expert
```
Fonctions :
├── jenks_breaks() → Ruptures naturelles optimales
├── spatial_aggregation() → Agrégation intelligente par niveau
├── pal_contextual() → Palettes adaptées à chaque indicateur + seuils OMS
├── annotate_regions() → Labels avec population enfants, n_obs
└── smart_zoom() → Zoom automatique par région
```

#### Améliorations cartes actuelles
- ✅ Jenks breaks (au lieu de linéaire)
- ✅ Annotations contextuelles (n enfants, population)
- ✅ Légende dynamique avec vraies prévallences
- ✅ Tooltip enrichie (valeur + IC + comparaison nationale)
- ✅ Points GPS avec sparklines au survol

### **Tier 2: Analyse Spatiale Avancée** (2-3h)

#### `spatial_analysis_engine.R` — Cluster & corrélation spatiale
```
Fonctions :
├── calc_lisas() → Local Indicators of Spatial Association
│   ├─ Hotspots (High-High) ← À INTERVENIR EN PRIORITÉ
│   ├─ Cold-spots (Low-Low)
│   ├─ Outliers (High-Low / Low-High)
│   └─ Cartes avec annotations
│
├── morans_i() → Auto-corrélation spatiale globale
├── spatial_autocorr_viz() → Heatmap corrélations par région
├── regional_comparison() → Radar charts + variance analysis
└── identify_hotspots() → Classement par urgence d'intervention
```

#### New outputs
- ✅ Carte LISA (4 couleurs : hotspots/coldspots/outliers/non-significatif)
- ✅ Heatmap corrélations inter-régionales
- ✅ Radar charts comparaison profils nutritionnels
- ✅ Tableau "Où intervenir d'abord" (algorithme prioritisation)

### **Tier 3: Intelligence Géographique** (2-3h)

#### `carto_intelligence_engine.R` — Contexte + recommandations
```
Fonctions :
├── calc_confidence_intervals() → IC 95% par région (Clopper-Pearson)
├── visualize_uncertainty() → IC affiché sur la carte (losanges/barres)
├── temporal_sparklines() → Tendances si données historiques
├── priority_algorithm() → Score urgence intervention
│   ├─ Facteurs : prévalence, urgence, population affectée, accès
│   └─ Recommandation : "CRITIQUE" / "SÉRIEUX" / "MONTRER"
│
├── geo_report() → Rapport géographique complet
└── export_geo() → Shape files + CSV + Markdown
```

#### New outputs
- ✅ Cartes avec IC affiché visuellement
- ✅ Sparklines mini (tendance 3-5 ans)
- ✅ Algorithme de priorisation (où intervenir)
- ✅ Rapport géographique exportable

### **Tier 4: Prédictif & Comparatif** (1-2h)

#### `carto_predictive_engine.R` — ML spatial
```
Fonctions :
├── predict_hotspots() → Où la malnutrition va augmenter
├── comparative_benchmark() → Vs moyenne nationale + région similaire
├── mobile_reach_analysis() → Accessibilité géographique
└── resource_allocation() → Où placer les ressources (algorithme)
```

#### New outputs
- ✅ Carte prédictive (probabilité augmentation)
- ✅ Benchmarking régional
- ✅ Analyse d'accessibilité
- ✅ Recommandations allocation ressources

---

## 🗂️ Fichiers à créer/modifier

### CRÉER (5 fichiers)
1. **`carto_expert_engine.R`** (300 lignes)
   - Jenks breaks, annotations, palettes, zoom smart
   
2. **`spatial_analysis_engine.R`** (400 lignes)
   - LISA, Moran's I, hotspots, radar charts
   
3. **`carto_intelligence_engine.R`** (350 lignes)
   - IC, sparklines, algorithme priorisation, rapport
   
4. **`carto_predictive_engine.R`** (250 lignes)
   - ML, benchmarking, allocation ressources
   
5. **`carto_expert_integration.R`** (200 lignes)
   - Intégration Shiny (reactive observers, outputs)

### MODIFIER (3 fichiers)
1. **`module_carto_v2.R`**
   - Source les 4 engines
   - Ajoute fonctions nouvelles

2. **`ui_carto_v2.R`**
   - Ajoute 4 nouveaux onglets
   - Ajoute paramètres pour Tier 2-4

3. **`app_v2_final.R`**
   - Source `carto_expert_integration.R`

### DOCUMENTATION (3 guides)
1. **`CARTO_EXPERT_GUIDE.md`** → Pour développeurs
2. **`CARTO_USER_MANUAL.md`** → Pour utilisateurs
3. **`CARTO_IMPLEMENTATION.md`** → Checklist

---

## 📊 Onglets finaux (10 onglets vs 1 actuel)

1. **Carte Choroplèthe Expert** (Tier 1)
   - Jenks breaks, annotations, zoom smart
   
2. **Analyse Hotspots LISA** (Tier 2)
   - Où la situation est critique (hotspots)
   - Où elle est bonne (coldspots)
   
3. **Auto-corrélation Spatiale** (Tier 2)
   - Heatmap corrélations régionales
   - Test Moran's I
   
4. **Comparaison Régionale** (Tier 2)
   - Radar charts (profils nutritionnels)
   - Variance analysis
   
5. **Incertitude & IC** (Tier 3)
   - Carte avec IC 95% visualisés
   - Tableau IC par région
   
6. **Sparklines Temporelles** (Tier 3)
   - Tendances 3-5 ans si données
   - Prédictions futures
   
7. **Priorisation Intervention** (Tier 3)
   - Algorithme : où intervenir d'abord
   - Tableau ranking par urgence
   
8. **Prédictions Spatiales** (Tier 4)
   - Où la malnutrition va augmenter
   - Probabilités par région
   
9. **Benchmarking Régional** (Tier 4)
   - Vs moyenne nationale
   - Vs régions similaires (ML)
   
10. **Rapport Géographique** (All Tiers)
    - Synthèse complète
    - Export shape files + CSV + MD

---

## 🧠 Concepts clés

### Jenks Natural Breaks
```
Au lieu de : 0%, 10%, 20%, 30%
Utilise : Groupements naturels dans les données
Résultat : Couleurs reflètent vraiment les variations
```

### LISA (Local Spatial Autocorrelation)
```
Identifie :
- 🔴 Hotspots : régions à malnutrition HAUTE entourées de hautes
- 🔵 Coldspots : régions à malnutrition BASSE entourées de basses
- ⚪ Outliers : isolées (haut entouré de bas, vice-versa)
- ⚫ Non-significatif : pas de pattern spatial
```

### Prioritisation Intelligente
```
Score = w1 × prévalence + w2 × urgence + w3 × population + w4 × accès
Résultat : Ranking objectif où intervenir
```

---

## ⏱️ Timeline

- **Tier 1** : 2-3h → Cartes intelligentes
- **Tier 2** : 2-3h → Analyse spatiale
- **Tier 3** : 2-3h → Intelligence géographique
- **Tier 4** : 1-2h → Prédictif
- **Intégration Shiny** : 1-2h
- **Documentation** : 1h

**Total : ~10-14h** (peut être fait en 2-3 sessions)

---

## ✅ Livérables finaux

- ✅ 5 moteurs R expertement codés
- ✅ 10 onglets cartographiques avancés
- ✅ Analyses spatiales au niveau scientifique
- ✅ Algorithme de priorisation d'intervention
- ✅ Prédictions spatiales (ML)
- ✅ 3 guides (dev + user + implementation)
- ✅ Reports exportables (GeoJSON + CSV + Markdown)

---

## 🎓 Ce que tu apprendras

1. **Analyse spatiale** : LISA, Moran's I, clusters
2. **Jenks algorithm** : Ruptures naturelles
3. **Confidence intervals** : Visualisation incertitude
4. **Spatial prediction** : ML géographique
5. **UX cartographique** : Interactivité avancée
6. **Priorisation algorithme** : Objectif vs subjectif

---

**C'est le niveau des maps que VAM/ENA/NutriDash utilisent (ou devraient).**
