# 🤖 NIVEAU 3 : AGENTS AVANCÉS AVEC GEMINI 2.5 FLASH (GRATUIT)

**Date :** 25 mars 2026  
**API :** Google AI Studio (GRATUIT via `generativeai` package R)  
**Status :** ✅ **RECOMMANDÉ - Déployer directement**  
**Auteur :** Kevin Yimga (DSAMID)

---

## 🎯 POURQUOI NIVEAU 3 MAINTENANT ?

### Le changement : API GRATUIT
```
Avant (Gemini API payant):
  - Niveau 2 (contextuel) = bon compromis coût/valeur
  - Niveau 3 (agents) = trop cher pour la majorité des cas

Maintenant (Google AI Studio GRATUIT):
  - Niveau 2 = encore intéressant mais limité
  - Niveau 3 = FORTEMENT RECOMMANDÉ
  - Pas de limite coût → on peut faire de la VRAIE intelligence
```

### Avantages du Niveau 3 avec budget illimité

| Capacité | Niveau 2 | Niveau 3 |
|----------|----------|----------|
| **Chatbot support** | ✅ | ✅ |
| **Interprétation résultats** | ✅ | ✅ |
| **Diagnostic erreurs** | ✅ | ⭐ Très amélioré |
| **Exécution analyses** | ❌ | ✅ NOUVEAU |
| **Génération rapports** | ❌ | ✅ NOUVEAU |
| **Learning continu** | ❌ | ✅ NOUVEAU |
| **Agents autonomes** | ❌ | ✅ NOUVEAU |
| **Multi-turn reasoning** | Limité | ✅ Avancé |

---

## 🏗️ ARCHITECTURE NIVEAU 3

### Composants : 4 Agents Intelligents

```
┌─────────────────────────────────────────────────────────────┐
│                    UTILISATEUR FINAL                         │
└────────────┬────────────────────────────────────────────────┘
             │
┌────────────▼────────────────────────────────────────────────┐
│           AGENT COORDINATEUR (Master Agent)                 │
│  ├─ Reçoit requête utilisateur                              │
│  ├─ Détermine quel agent appeler                            │
│  ├─ Orchestrate multi-agents                                │
│  └─ Fournit réponse finale coordonnée                       │
└────┬──────┬──────────┬──────────┬─────────────────────────┘
     │      │          │          │
┌────▼─┐ ┌──▼──┐ ┌─────▼────┐ ┌──▼──────────────┐
│ CHAT │ │ANAL │ │ RAPPORT  │ │ DIAGNOSE/FIX    │
│AGENT │ │AGENT│ │AGENT     │ │AGENT            │
└──────┘ └─────┘ └──────────┘ └─────────────────┘
```

### Les 4 Agents en détail

#### 1️⃣ **CHAT AGENT** (Support & Guidance)
```r
chat_agent <- list(
  role = "Expert nutrition humanitaire francophone",
  capabilities = list(
    # Répondre questions utilisateurs
    answer_questions = TRUE,
    # Expliquer concepts statistiques
    explain_stats = TRUE,
    # Guider sur utilisation app
    guide_usage = TRUE,
    # Interpréter résultats analyses
    interpret_results = TRUE,
    # Proposer prochaines étapes
    suggest_next_steps = TRUE
  ),
  system_prompt = "
    Vous êtes un expert en nutrition et épidémiologie humanitaire.
    L'utilisateur analyse des données de malnutrition.
    Répondez en français, concisément, avec empathie.
    Citez standards OMS quand pertinent.
    Toujours suggérer 'consulter expert' si critique.
  "
)

# Exemples de questions
chat_agent$examples <- c(
  "Pourquoi GAM > 15% ?" 
    → "GAM > 15% = CRITIQUE selon OMS. Signifie 16.5% enfants < 5 ans..."
  
  "Comment interpréter ACP ?" 
    → "ACP réduit dimensions. Variance expliquée = combien info préservée..."
  
  "Mes données manquent. Comment je fais ?"
    → "Options: 1) drop_na() 2) imputation 3) analyse complètes seulement..."
)
```

#### 2️⃣ **ANALYSIS AGENT** (Exécution autonome analyses)
```r
analysis_agent <- list(
  role = "Bioinformaticien expert - exécute analyses",
  capabilities = list(
    # Peut exécuter analyses à la demande
    run_analysis = TRUE,
    # Choisir meilleure méthode automatiquement
    select_method = TRUE,
    # Valider hypothèses avant exécution
    validate_assumptions = TRUE,
    # Expliquer étapes d'analyse
    explain_workflow = TRUE,
    # Proposer interprétations
    suggest_interpretations = TRUE
  ),
  
  # CES ANALYSES PEUVENT ÊTRE EXÉCUTÉES
  available_analyses = c(
    "ACP", "ACM", "FAMD", "AFC",           # Réduction dimension
    "t-test", "Chi²", "ANOVA", "Kruskal-Wallis", # Tests
    "Régression logistique", "Cox PH",      # Modélisation
    "Clustering (K-means, CAH, DBSCAN)",    # Clustering
    "Corrélation Spearman",                 # Associations
    "Prévalences pondérées SMART"           # Calculs spécialisés
  ),
  
  system_prompt = "
    Vous êtes un bioinformaticien expert.
    Vous exécutez analyses statistiques sur demande.
    AVANT d'exécuter:
      1. Validez les données (n, types, NA)
      2. Vérifiez les hypothèses (normalité, homoscédasticité, etc.)
      3. Proposez la meilleure approche
    APRÈS exécution:
      1. Vérifiez qualité résultats
      2. Générez interprétation statistique
      3. Suggérez visualisations
  "
)

# Exemples de requêtes
analysis_agent$examples <- c(
  "Fais une ACP sur les variables numériques"
    → Agent valide → exécute PCA() → interprete résultats → suggère visus
  
  "Compare GAM entre régions avec test statistique"
    → Agent choisit ANOVA ou Kruskal-Wallis → exécute → interprète
  
  "Clustering sur données nutritionnelles"
    → Agent suggère K-means ou DBSCAN selon nature données
)
```

#### 3️⃣ **REPORT AGENT** (Génération rapports)
```r
report_agent <- list(
  role = "Expert rédaction rapports scientifiques",
  capabilities = list(
    # Générer narration des analyses
    generate_narrative = TRUE,
    # Synthétiser résultats multiples
    synthesize_results = TRUE,
    # Créer sections structurées (intro, méthodo, résultats, discussion)
    structure_report = TRUE,
    # Générer recommandations automatiques
    generate_recommendations = TRUE,
    # Adapter style (exécutif, technique, vulgarisé)
    adapt_style = TRUE
  ),
  
  # TYPES DE RAPPORTS DISPONIBLES
  report_types = c(
    "Executive Summary" = "1 page, KPIs, recommandations",
    "Technical Report" = "Publication-ready, tous détails",
    "Field Report" = "Opérationnel, actions concrètes",
    "Student Report" = "Pédagogique, explications détaillées",
    "Dashboard Narrative" = "Textes accompagnant graphiques"
  ),
  
  system_prompt = "
    Vous êtes un rédacteur technique expert en nutrition humanitaire.
    Générez rapports de haute qualité.
    Adaptez le style selon le type de rapport demandé.
    Toujours inclure:
      - Contexte & objectifs
      - Méthodologie (rapide)
      - Résultats clés
      - Recommandations concrètes
      - Limitations
    Utilisez langage clair. Évitez jargon sauf nécessaire.
  "
)

# Exemples de rapports générables
report_agent$examples <- c(
  "Génère Executive Summary des analyses GAM/Stunting"
    → Agent crée: contexte + KPIs + seuils OMS + recommandations (1 page)
  
  "Technical report complet de l'ACP"
    → Agent écrit: méthodologie, scree plot, contributions, interprétations
  
  "Field report: que faire face à GAM = 18% ?"
    → Agent structure: situation, causes probables, actions immédiates, suivi
)
```

#### 4️⃣ **DIAGNOSE & FIX AGENT** (Résolution problèmes)
```r
diagnose_agent <- list(
  role = "Expert diagnostic & résolution problèmes",
  capabilities = list(
    # Analyser erreurs utilisateur
    diagnose_errors = TRUE,
    # Proposer solutions automatiquement
    propose_fixes = TRUE,
    # Détecter problèmes données
    detect_data_issues = TRUE,
    # Éduquer utilisateur (pourquoi erreur ?)
    educate_user = TRUE,
    # Implémenter corrections automatiquement
    auto_fix = TRUE
  ),
  
  # TYPES D'ERREURS QUE L'AGENT PEUT GÉRER
  detectable_errors = c(
    "Type variable incorrect pour méthode (ACP + catégorique)",
    "Données insuffisantes après NA drop",
    "Violation hypothèses statistiques",
    "Outliers extrêmes",
    "Colinéarité variables",
    "Imbalance classes (pour ML)",
    "Dépassement limites OMS inexplicable",
    "Mauvaise intérprétation résultats"
  ),
  
  system_prompt = "
    Vous êtes un expert diagnostique en statistique appliquée.
    Quand erreur survient:
      1. Analysez ROOT CAUSE
      2. Expliquez pourquoi erreur (éducatif)
      3. Proposez 2-3 solutions
      4. Offrez correction automatique si possible
    
    Format réponse:
    🔴 PROBLÈME: [ce qui a échoué]
    🔍 CAUSE: [pourquoi]
    💡 SOLUTIONS:
      1. [Option simple]
      2. [Option recommandée]
      3. [Option avancée]
    ✅ CORRIGER AUTOMATIQUEMENT ? [Oui/Non]
  "
)

# Exemples diagnostiques
diagnose_agent$examples <- c(
  "User essaie ACP avec région (catégo) + age (numérique)"
    → Diagnose détecte → Explique → Propose FAMD → Peut switcher automatiquement
  
  "Données GAM = 45% (absurde, max = 100% mais normal < 30%)"
    → Détecte outlier + propose investigation → Peut filtrer automiquement
  
  "Test Chi² p = 0.0000000001 (probablement faux)"
    → Propose vérifier données → Peut re-exécuter sans outliers
)
```

---

## 📊 SYSTÈME DE COORDINATION MULTI-AGENTS

### Agent Master : Orchestration intelligente

```r
master_agent <- function(user_request, context) {
  # 1. ANALYSER REQUÊTE UTILISATEUR
  request_analysis <- gemini_classify_request(user_request)
  # → Retourne: type (question, demande_analyse, erreur, etc.)
  
  # 2. DÉTERMINER AGENT(S) À APPELER
  if (request_analysis$type == "question") {
    return(call_chat_agent(user_request, context))
  }
  else if (request_analysis$type == "execute_analysis") {
    return(call_analysis_agent(user_request, context))
  }
  else if (request_analysis$type == "generate_report") {
    return(call_report_agent(user_request, context))
  }
  else if (request_analysis$type == "error_or_issue") {
    return(call_diagnose_agent(user_request, context))
  }
  
  # 3. POUR REQUÊTES COMPLEXES : appeler PLUSIEURS agents
  # Example: "Fais une ACP et génère un rapport"
  if (request_analysis$complexity == "high") {
    analysis_result <- call_analysis_agent(...)
    report_result <- call_report_agent(..., analysis = analysis_result)
    return(combine_results(analysis_result, report_result))
  }
}
```

### Exemple : Workflow multi-agents

```
USER: "Pourquoi mon ACP échoue et comment je la fais correctement ?"

1. MASTER AGENT détecte:
   - Type: erreur + demande correction
   - Complexity: high (nécessite 2 agents)

2. APPEL DIAGNOSE AGENT:
   "Analyser erreur ACP utilisateur"
   → Diagnose détecte: "Variables catégorielles au lieu numériques"
   → Explique: "ACP accepte SEULEMENT variables numériques"
   → Propose: "Essayez FAMD (mixte) ou sélectionnez numériques"

3. APPEL ANALYSIS AGENT (si user veut):
   "Switch vers FAMD et exécute"
   → Analysis valide données
   → Exécute FAMD
   → Interprete résultats

4. APPEL REPORT AGENT (optionnel):
   "Génère rapport de cette FAMD"
   → Report crée narration complet

5. MASTER AGENT combine & retourne réponse finale:
   "Voici l'analyse complète avec rapport"
```

---

## 💻 IMPLÉMENTATION : FICHIERS À CRÉER

### Niveau 3 = ~2500 lignes de code

```
gemini_config_v3.R (150 lignes)
├─ Configuration API Google (gratuit)
├─ System prompts pour 4 agents
└─ Paramètres modèle

context_manager_v3.R (250 lignes)
├─ Capture contexte enrichie
├─ Session management
└─ History conversation persistent

gemini_api_v3.R (300 lignes)
├─ Wrapper API Gemini (gratuit)
├─ Streaming responses
├─ Error handling
└─ Token counting

chat_agent.R (350 lignes)
├─ Chat logic
├─ Interpretation results
└─ Guidance & suggestions

analysis_agent.R (500 lignes) ⭐ NOUVEAU
├─ Can execute R code safely
├─ Validates assumptions
├─ Auto-selects methods
├─ Interprets statistics

report_agent.R (400 lignes) ⭐ NOUVEAU
├─ Generates narratives
├─ Structures reports
├─ Multiple styles
└─ Recommendations

diagnose_agent.R (350 lignes) ⭐ NOUVEAU
├─ Error detection
├─ Root cause analysis
├─ Auto-fix suggestions
└─ Educational explanations

master_agent.R (200 lignes) ⭐ NOUVEAU
├─ Request routing
├─ Multi-agent coordination
├─ Result synthesis
└─ Response formatting

chatbot_ui_v3.R (200 lignes)
├─ Advanced chat UI
├─ Message types (text, code, tables)
├─ Typing indicators
└─ Streaming display

chatbot_server_v3.R (300 lignes)
├─ Message handling
├─ Agent calling
├─ Response display
└─ Session persistence
```

---

## 🚀 USE CASES LEVEL 3

### Use Case 1: Utilisateur novice

```
USER: "Je comprends rien aux Z-scores OMS"

RESPONSE (Chat Agent):
"Z-score = mesure combien écarts-type observé vs moyenne OMS.
 Z = -2 → en-dessous moyenne de 2 SD → classé malnutrition.
 Courbe OMS 2006 = référence de croissance 'normal'.
 
 Plus Z-score bas = plus malnutrition sévère.
 
 [Graphic: Normal curve with shaded zones]
 
 Questions: Besoin clarifications ? Voir exemple pratique ?"
```

### Use Case 2: Demande analyse

```
USER: "Fais moi une analyse de clustering sur mes données"

RESPONSE (Analysis Agent):
"✅ Je vais faire un clustering pour vous.

Données trouvées: N=847, 5 variables numériques, 12 variables catégorielles

Étapes:
1️⃣ Sélection variables numériques
2️⃣ Normalisation
3️⃣ K optimal (Silhouette)
4️⃣ K-means clustering
5️⃣ Profils clusters

Résultats:
- Optimal K = 3 clusters
- Silhouette moyen = 0.68 (GOOD)
- Cluster 1 (n=320): Malnutrition modérée
- Cluster 2 (n=380): Malnutrition sévère
- Cluster 3 (n=147): Normal

[Table: profils clusters]
[Visualization: scatter 3D]

Recommandation: Clusters bien séparés. Chaque cluster nécessite approche ciblée."
```

### Use Case 3: Erreur utilisateur + correction auto

```
USER essaie ACM avec variables numériques

DIAGNOSE AGENT:
"🔴 ERREUR détectée: ACM requires categorical variables only
   Vous avez sélectionné: age (numeric), poids (numeric)

🔍 CAUSE: ACM = Analyse Correspondances Multiples
   Accepte SEULEMENT variables catégorielles/facteurs
   
💡 SOLUTIONS:
   1. Sélectionner variables catégorielles uniquement
      → sexe, région, déplacement (si disponibles)
   2. Essayer FAMD (accepte mix numérique + catégorique)
      → Garder age + poids + sexe + région
   3. Discrétiser variables numériques
      → Convertir age/poids en groupes (jeune/adulte, léger/lourd)

✅ CORRIGER AUTOMATIQUEMENT ?
   Recommandation: Option 2 (FAMD)
   [Button: Essayer FAMD automatiquement]"
```

### Use Case 4: Rapport intelligent

```
USER: "Génère rapport exécutif de mes analyses"

REPORT AGENT génère:

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
RAPPORT EXÉCUTIF - Analyse Nutritionnelle
Cameroun, région Littoral | Mars 2026
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📌 SITUATION EN 3 POINTS
• GAM = 16.5% (⚠️ CRITIQUE, seuil OMS = 15%)
• 847 enfants < 5 ans analysés
• Données qualité: Excellent (score = 94/100)

🎯 CONCLUSIONS
Malnutrition aiguë à niveau d'urgence.
Intervention immédiate requise.

📋 RECOMMANDATIONS
1. Supplémentation nutritionnelle (urgence)
2. Diagnostic causes (eau/assainissement ?)
3. Monitoring hebdomadaire (rCSI, FCS)
4. Engagement communautaire

⏱️ PROCHAINES ÉTAPES
- Enquête de suivi dans 2 semaines
- Évaluation complémentaire sécurité alimentaire
- Escalade vers autoritées si GAM > 20%

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

## ⚙️ INTÉGRATION DANS app_v2_final.R

### Modifications minimales

```r
# En haut (sources)
source("gemini_config_v3.R")
source("context_manager_v3.R")
source("gemini_api_v3.R")
source("chat_agent.R")
source("analysis_agent.R")
source("report_agent.R")
source("diagnose_agent.R")
source("master_agent.R")
source("chatbot_ui_v3.R")
source("chatbot_server_v3.R")

# Dans la UI
tags$div(
  id = "ai_assistant",
  chatbot_ui_v3("ai_main")
)

# Dans le Server
user_context_v3 <- reactive({
  create_context_v3(
    current_module = input$menu,
    current_analysis = isolate(last_analysis()),
    data = mapped_data(),
    session_data = session_store(),
    ...
  )
})

chatbot_server_v3("ai_main", user_context_v3, master_agent)
```

---

## 📈 TIMELINE IMPLÉMENTATION NIVEAU 3

| Semaine | Tâche | Effort |
|---------|-------|--------|
| 1 | Configuration API Google (gratuit) + Chat Agent | 2 jours |
| 2 | Analysis Agent (exécution analyses) | 3 jours |
| 3 | Report Agent (génération rapports) | 2 jours |
| 4 | Diagnose Agent (résolution erreurs) | 3 jours |
| 5 | Master Agent (orchestration multi-agents) | 2 jours |
| 6 | Intégration dans app_v2_final.R | 2 jours |
| 7 | Testing, refinements, documentation | 3 jours |
| 8 | **TOTAL : ~5-6 semaines** | |

---

## 🔒 SÉCURITÉ & GESTION RISQUES (NIVEAU 3)

### ⚠️ Risque 1: IA exécute code malveillant

```r
# ❌ DANGER: Laisser IA exécuter n'importe quel code R
# ✅ SOLUTION: Sandbox + whitelisting

analysis_agent$safe_execute <- function(code) {
  # 1. Parser code pour détecter opérations dangereuses
  dangerous_patterns <- c(
    "system(", "file.remove(", "download.file(",
    "source(", "library(", "setwd("
  )
  
  # 2. Si code dangereux détecté → REFUSER
  if (any(sapply(dangerous_patterns, function(p) grepl(p, code)))) {
    return("❌ Sécurité: Cette opération est interdite")
  }
  
  # 3. Sinon, exécuter dans environment sandbox
  sandbox_env <- new.env(parent = emptyenv())
  eval(parse(text = code), envir = sandbox_env)
}
```

### ⚠️ Risque 2: IA hallucine seuils OMS

```r
# ❌ IA dit "GAM critique si > 12%" (FAUX, c'est > 15%)
# ✅ SOLUTION: Fact-checking contre config_oms.R

fact_check_response <- function(response, context) {
  # 1. Extraire affirmations numériques de réponse
  # 2. Comparer avec seuils hardcodés
  
  if (grepl("GAM > 12%", response)) {
    response <- gsub("GAM > 12%", "GAM > 15%", response)
    response <- paste(response, "\n⚠️ [Réponse corrigée]")
  }
  
  return(response)
}
```

### ⚠️ Risque 3: Données sensibles exposées

```r
# ❌ Ne JAMAIS envoyer données brutes à Gemini
# ✅ Envoyer seulement contexte agrégé

context_to_send <- list(
  n_obs = nrow(df),                    # SAFE: nombre
  variables = colnames(df),            # SAFE: noms colonnes
  data_summary = "Cameroun, Littoral", # SAFE: contexte
  
  # ❌ JAMAIS:
  # raw_data = df                       # DANGER!
  # patient_ids = df$ID                 # DANGER!
)
```

---

## 🎯 CAPACITÉS FINALES NIVEAU 3

### Ce que vous pouvez faire :

```
💬 CHAT AGENT
  ✅ Q&A nutrition/epidemiologie
  ✅ Explique résultats analyses
  ✅ Guide utilisation app
  ✅ Multilingue (FR/EN/ES)

📊 ANALYSIS AGENT
  ✅ Exécute analyses sur demande
  ✅ Valide hypothèses automatiquement
  ✅ Choisit meilleure méthode
  ✅ Interprète résultats
  ✅ Suggestions visualisations

📄 REPORT AGENT
  ✅ Génère rapports narratifs
  ✅ Multiples styles (exécutif, technique, terrain)
  ✅ Recommandations automatiques
  ✅ Format publication-ready

🔧 DIAGNOSE AGENT
  ✅ Détecte erreurs utilisateur
  ✅ Explique causes
  ✅ Propose solutions
  ✅ Auto-correction quand possible
  
👑 MASTER AGENT
  ✅ Orchestre 4 agents
  ✅ Requêtes complexes multi-étapes
  ✅ Synthèse résultats
  ✅ Réponses contextuelles
```

---

## 📊 COMPARAISON : NIVEAU 2 vs NIVEAU 3

| Feature | Niveau 2 | Niveau 3 |
|---------|----------|----------|
| **Chatbot support** | ✅ | ✅ |
| **Context awareness** | ✅ | ✅ Enhanced |
| **Exécution analyses** | ❌ | ✅ AUTO |
| **Génération rapports** | ❌ | ✅ AUTO |
| **Diagnostic erreurs** | ✅ Basique | ✅ AVANCÉ |
| **Multi-agent coordination** | ❌ | ✅ |
| **Recommandations smart** | Simples | ✅ Intelligentes |
| **Coût API** | $100-200/mois | $0 (GRATUIT) |
| **Délai implémentation** | 4-6 semaines | 5-6 semaines |
| **Valeur utilisateur** | Très haute | 🚀 EXCEPTIONNELLE |

---

## 🏆 VERDICT : POURQUOI NIVEAU 3 MAINTENANT

### Avant (API payant)
- Niveau 2 était raisonnable
- Niveau 3 trop coûteux pour majorité cas

### Maintenant (API GRATUIT)
```
Coût = $0 (via Google AI Studio gratuit)
Bénéfice = énorme (analyse auto, rapports auto, etc.)
Délai = acceptable (5-6 semaines)

RATIO: Coût/Bénéfice = INFINITÉ POSITIVE
```

### Décision

**✅ DÉPLOYER NIVEAU 3 DIRECTEMENT**

Pas de sens de déployer Niveau 2 si Niveau 3 coûte pareil et apporte 10x plus valeur.

---

## 📋 ARCHITECTURE FINALE

```
NutriAnalyse Pro v2
│
├─ Modules analyses classiques (28+ modules)
│  ├─ ACP, ACM, FAMD, AFC
│  ├─ Tests statistiques
│  ├─ Épidémiologie
│  ├─ Survie
│  ├─ Cartographie
│  └─ ...
│
├─ 🤖 AI ASSISTANT LEVEL 3 (NOUVEAU)
│  ├─ Chat Agent (support, Q&A)
│  ├─ Analysis Agent (exécution auto analyses)
│  ├─ Report Agent (génération rapports)
│  ├─ Diagnose Agent (détection & fix erreurs)
│  ├─ Master Agent (orchestration)
│  │
│  └─ Powered by Gemini 2.5 Flash (GRATUIT Google AI Studio)
│
└─ 💾 Database & Caching
   ├─ Conversation history
   ├─ Analysis cache
   └─ Report templates
```

---

## 🚀 NEXT STEPS

Si vous approuvez Niveau 3, je créerai immédiatement:

1. ✅ **gemini_config_v3.R** — Config API Google (gratuit)
2. ✅ **chat_agent.R** — Agent chat intelligent
3. ✅ **analysis_agent.R** — Agent exécution analyses
4. ✅ **report_agent.R** — Agent génération rapports
5. ✅ **diagnose_agent.R** — Agent diagnostic & fix
6. ✅ **master_agent.R** — Orchestration multi-agents
7. ✅ **chatbot_ui_v3.R** + **chatbot_server_v3.R** — UI/Server Shiny

**Total :** ~2500 lignes de code, 5-6 semaines de développement

**Résultat :** Application avec **vraie intelligence artificielle intégrée** 🤖

---

*Kevin Yimga — DSAMID Data Science & AI*  
*Architecture Agents Avancés avec Gemini 2.5 Flash (GRATUIT)*  
*NutriAnalyse Pro v2 : La plateforme la plus intelligente du monde humanitaire*
