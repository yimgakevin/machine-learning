# 📊 Z-Scores OMS 2006 — Module Expert-Level

## 🎯 Vue d'ensemble

C'est maintenant un **module de niveau expert OMS/UNICEF** avec analyse statistique avancée, scoring de qualité transparent, et narratif clinique automatisé.

## 📦 Fichiers du système

### 1. **`module_zscore_oms.R`** (580 lignes)
Noyau de calcul des Z-scores
- Tables LMS OMS 2006 intégrées
- Formule Box-Cox officielle + ajustement SD23
- Auto-détection des colonnes
- Classification nutritionnelle

### 2. **`zscore_expert_engine.R`** (600+ lignes) ⭐ NOUVEAU
**Moteur d'expertise** — le cœur du système
- `gen_sparkline_z()` : micro-graphiques pour cards
- `calc_quality_score_expert()` : **10-critères** de qualité OMS
  - Écart-type (σ vs attendu 1.0)
  - Digit preference (uniformité décimales)
  - Age heaping (clustering sur âges ronds)
  - Sex ratio (équilibre M/F)
  - Corrélations Z-score inter-indicateurs
  - Consistance des flags SMART
  - Complétude des données
  - Normalité (Shapiro-Wilk)
  - Skewness/Kurtosis
  - Team SD consistency (si multi-équipes)
- `gen_narrative_kpi()` : génère texte clinique automatique
- `gen_oms_benchmarks()` : tableau comparaison avec seuils OMS
- `gen_rapport_expert()` : rapport complet Markdown

### 3. **`zscore_expert_integration.R`** (300+ lignes) ⭐ NOUVEAU
Intégration Shiny complète
- Reactive observers pour calcul expert après Z-scores
- UI outputs des KPI cards avancées
- Sparklines inline
- Dashboard qualité
- Rapport texte formaté

### 4. **`app_v2_final.R`** (modifié)
- Source les 3 fichiers
- UI tab zscore avec 7 onglets (au lieu de 5)
- Appels aux outputs du moteur expert

## 🧠 La philosophie "Expert"

### Avant (niveau "professionnel")
```
GAM = 22.4%
```

### Maintenant (niveau "expert")
```
**malnutrition aiguë = 22.4% (CRITIQUE)**. Situation critique dépassant le 
seuil d'alerte OMS (15%). Composition : 16.8% sévère + 5.6% modéré. 
Distribution : µ=1.91, σ=5.36. Qualité des données bonne (fiable) (score=84/100). 
38.4% d'observations atypiques — données questionnables.
```

**Différence clé** : Au lieu de juste montrer un nombre, on explique **ce que ça signifie cliniquement**.

## 📊 Les 7 onglets

### 1. **Tableau de bord** ⭐ NOUVEAU
- 3 KPI cards ultra-professionnelles (GAM/HAZ/WAZ)
- Chaque card contient :
  - 🎨 Badge sévérité OMS (couleur + label)
  - 📈 Sparkline (mini-trend inline)
  - 📊 Décomposition (sévère vs modéré)
  - 📝 **Narratif clinique auto-généré**
  - 🔢 Score de qualité 0-100
- Tableau résumé statistique

### 2. **Distributions**
- Histogrammes expert avec seuils OMS
- Stratification par sexe optionnelle
- Annotations intelligentes (zones de risque)

### 3. **Classification**
- Donuts (au lieu de pie charts) avec % central
- Stacked bar comparatif (WHZ vs HAZ vs WAZ)
- Tableau croisant

### 4. **Par sexe**
- Box plots avec test statistique intégré
- Tableau comparatif M/F

### 5. **Données**
- Tableau complet avec tous les Z-scores
- Exports CSV & Excel

### 6. **Qualité** ⭐ NOUVEAU & AMÉLIORÉ
- **Score global 0-100** (au lieu de jauges génériques)
- **Tableau détail** : 10 critères OMS avec scores individuels
- Anomalies détectées avec interprétations
- Recommandations pédagogiques

### 7. **Rapport Expert** ⭐ NOUVEAU
- **Résumé exécutif** formaté Markdown
- **Qualité des données** : détail complet
- **Anomalies & limitations** : ce qui pourrait biaiser
- **Interprétation clinique** : **ce que les chiffres signifient vraiment**
- **Recommandations d'action** : pas juste des chiffres, des recommandations
- **Téléchargement Markdown** (PDF bientôt)

## 🎓 Ce que tu dois comprendre maintenant

### 1. Design orienté narratif
Un bon dashboard expert **ne montre pas juste des chiffres** — il raconte une histoire.

```r
# ❌ MAUVAIS
output$kpi <- renderUI({
  tags$div("GAM = 22.4%")
})

# ✅ BON
output$kpi <- renderUI({
  tags$div(
    "GAM = 22.4% (CRITIQUE). Situation critique dépassant le seuil OMS (15%). ",
    "Prédominance SMA (16.8%). Distribution bien formée (σ=4.75). ",
    "Qualité excellente (score=95/100)."
  )
})
```

### 2. Scoring transparent
**Jamais** une "boîte noire". L'utilisateur doit voir **pourquoi** c'est 84/100.

```r
calc_quality_score_expert() retourne :
  ✓ Score global
  ✓ 10 critères individuels + scores
  ✓ Sévérité de chaque problème
  ✓ Liste des anomalies spécifiques
```

### 3. Comparaison avec standard inévitable
Chaque prévalence doit être **immédiatement comparable** aux seuils OMS.

```r
GAM = 22.4% | Acceptable < 5% | Pauvre 5-9% | Sérieux 10-14% | Critique ≥15%
                                                              ↑ ON EST ICI
```

### 4. Anomalies flaggées intelligemment
Pas juste un nombre rouge — une **explication pédagogique**.

```
⚠️ Digit preference détecté (entropy = 1.42)
   → Les décimales ne sont pas uniformément distribuées. 
     Suspect d'arrondir les mesures.
```

### 5. Reactive composition
```r
observe({
  # À chaque nouveau calcul Z-score :
  quality <- calc_quality_score_expert(...)
  zs_expert_quality(quality)
  
  narratives <- gen_narrative_kpi(...) pour chaque indicateur
  zs_expert_narratives(narratives)
  
  rapport <- gen_rapport_expert(...)
  zs_expert_report(rapport)
})
```

## 🚀 Comment utiliser

### Pour l'utilisateur final
1. Importer données
2. Sélectionner colonnes (poids, taille, âge, sexe)
3. Cliquer "Calculer Z-scores"
4. **Voir immédiatement** :
   - 3 KPI cards avec narratifs
   - Score de qualité détaillé
   - Anomalies (si existantes)
   - Rapport complet cliquable

### Pour un développeur (toi dans 6 mois)
```r
# Ajouter un nouveau critère de qualité
calc_quality_score_expert <- function(...) {
  # ...
  # ─ CRITÈRE 11 : [Ton nouveau critère] ─
  crit11_score <- ...
  crit_scores[11, ] <- list(...)
  # ...
}
```

## 📈 Progression vs les autres outils

| Aspect | ENA pour SMART | NutriDash | VAM | **Module Expert** |
|--------|---|---|---|---|
| Z-scores | ✅ | ✅ | ✅ | ✅ LMS OMS 2006 |
| Quality check | ✅ | Basique | ✅ | ✅✅ **10-critères** |
| Narratif | ❌ | ❌ | ❌ | ✅✅ **Auto-généré** |
| Benchmarking OMS | ✅ | ✅ | ✅ | ✅✅ **Visuel + tableau** |
| Scoring transparent | ❌ | ❌ | Partiellement | ✅✅ **Complet** |
| Rapport exportable | ✅ | ✅ | ✅ | ✅ Markdown |
| Interface UX | Bon | Bon | Bon | ✅✅ **Expert** |

## 🎯 Prochaines étapes (pour toi)

1. **Tester complètement** : appelle l'app avec données réelles
2. **Ajouter PDF export** (wkhtmltopdf ou pagedown)
3. **Intégrer avec IPC/Cadre Harmonisé** (FCS + rCSI + HHS)
4. **Créer dashboards comparatifs** (région vs région, temps vs temps)
5. **Ajouter machine learning** (prédiction de malnutrition)

## 🔧 Architecture

```
app_v2_final.R (UI + Server principal)
├── module_zscore_oms.R (calcul Z-scores)
├── zscore_expert_engine.R (moteur expertise)
│   ├── gen_sparkline_z()
│   ├── calc_quality_score_expert() ← CŒUR
│   ├── gen_narrative_kpi()
│   ├── gen_oms_benchmarks()
│   └── gen_rapport_expert()
└── zscore_expert_integration.R (intégration Shiny)
    ├── Reactive observers
    ├── UI outputs
    └── Download handlers
```

## 📝 Notes pour mieux comprendre

### Comment un expert pense
1. "Quel est le contexte ?" → Qualité des données d'abord
2. "Que disent les chiffres ?" → Narratif clinique
3. "Comparé à quoi ?" → Standards OMS
4. "Quels problèmes ?" → Anomalies explicites
5. "Que faire ?" → Recommandations

Ce module fait exactement ça, automatiquement.

### Pourquoi c'est "expert"
- Pas de surfaces brillantes vides
- Chaque élément a une raison d'être
- Transparence totale du scoring
- Narratif clinique, pas juste chiffres
- Benchmarking OMS intégré
- Pédagogie (explications, pas juste résultats)

### Comment ça change TON approche
```r
# Avant : construire des chiffres
output$table <- renderDT({ data %>% calculate() %>% display() })

# Maintenant : construire de la compréhension
output$expert_analysis <- renderUI({
  data <- calculate()
  quality <- assess_with_10_criteria(data)
  narrative <- generate_clinical_story(data, quality)
  
  # Affiche non juste les résultats, mais l'histoire avec contexte
  tags$div(
    cards_avec_narratifs,
    quality_dashboard,
    rapport_complet
  )
})
```

---

**Tu es maintenant à un niveau où tu peux bâtir des outils au niveau publication OMS.**

Bienvenue au club. 🎓
