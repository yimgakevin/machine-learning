# 📋 Plan Implémentation: Niveau 3 (Advanced AI Agents) - NutriAnalyse Pro v2

**Date:** March 25, 2026  
**Status:** 🔄 À Valider  
**Estimé:** ~8 heures (9 fichiers, ~3,500 lignes R)  

---

## 🎯 Objectif Global

Implémenter une couche d'**intelligence artificielle autonome** avec 4 agents spécialisés orchestrés par un Master Agent, utilisant l'API **Google Gemini 2.5 Flash** (gratuite).

### Valeur Ajoutée
- ✅ Analyses autonomes (ACP, ACM, FAMD, tests stats, modèles)
- ✅ Rapports auto-générés (narratifs publication-ready)
- ✅ Diagnostic intelligent d'erreurs utilisateur
- ✅ Chat contextuel bilingue (FR/EN/ES)
- ✅ Coût: $0 (API Google gratuite)

---

## 📊 Architecture Niveau 3

```
┌─────────────────────────────────────┐
│   app_v2_final.R (UI + Server)      │
│   └─ Ajouter module Chat UI/Server  │
└──────────────┬──────────────────────┘
               │
        ┌──────▼──────┐
        │ MASTER AGENT│ (Orchestrateur)
        └──────┬──────┘
         │  │  │  │
    ┌────┘  │  │  └────┐
    ▼       ▼  ▼       ▼
┌─────┐ ┌──────┐ ┌────────┐ ┌────────┐
│Chat │ │Analyse│ │Report │ │Diagnose│
│Agent│ │ Agent │ │ Agent │ │ Agent  │
└─────┘ └──────┘ └────────┘ └────────┘
    │       │       │          │
    └───────┴───────┴──────────┘
            │
    ┌───────▼────────┐
    │ Gemini API     │
    │ (Google)       │
    └────────────────┘
```

---

## 📁 9 Fichiers à Créer

### **1. `context_manager_v3.R`** (300 lignes) ⭐
**Rôle:** Capture contexte enrichi utilisateur pour agents intelligents

**Fonctions principales:**
- `create_user_context()` - Extraire infos utilisateur (langue, pays, expertise)
- `capture_data_context()` - Profil données (type, taille, variables, qualité)
- `capture_analysis_context()` - Historique analyses (derniers choix)
- `build_agent_prompt()` - Construire prompt contextuel intelligent
- `track_conversation()` - Maintenir état conversation multi-tour

**Intégration:** Utilisé par tous les agents avant chaque appel

---

### **2. `gemini_api_v3.R`** (400 lignes) ⭐⭐
**Rôle:** Wrapper API Gemini robuste avec gestion erreurs et streaming

**Fonctions principales:**
- `init_gemini_session()` - Initialiser session avec caching
- `call_gemini_streaming()` - Appels avec streaming (afficher réponse en direct)
- `call_gemini_batch()` - Appels batch pour multi-analyses
- `cache_gemini_response()` - Cacher réponses pour réduire latence
- `gemini_error_handler()` - Gestion intelligente erreurs
- `sanitize_code()` - Sécuriser code générés avant exécution
- `rate_limiter()` - Respecter limites API (~60 req/min)

**Sécurité:**
- ✅ Code sandbox (ne jamais exécuter code non-vérified)
- ✅ Masquage clé en erreurs (utiliser config_api_secure_v2.R)
- ✅ Validation réponses avant utilisation

---

### **3. `chat_agent.R`** (500 lignes) ⭐⭐⭐
**Rôle:** Agent Support & Q&A Expert en Nutrition/Épidémiologie

**Fonctions principales:**
- `chat_agent_initialize()` - Initialiser agent avec system prompt OMS
- `process_user_question()` - Traiter question utilisateur
- `contextualize_answer()` - Enrichir réponse avec contexte données
- `handle_multilingual()` - Support FR/EN/ES auto-détecté
- `explain_concepts()` - Explications pédagogiques concepts stats
- `reference_oms_standards()` - Citer normes OMS dans réponses

**Specialisations:**
- Épidémiologie (SMART, OMS, PAM standards)
- Nutrition (malnutrition, micronutriments, FCM)
- Sécurité alimentaire (FCS, RCSI, DiCES)
- Méthodologie stats (tests, modèles, EDA)

---

### **4. `analysis_agent.R`** (600 lignes) ⭐⭐⭐
**Rôle:** Agent Analyses Autonomes (ACP, ACM, FAMD, Stats, ML)

**Fonctions principales:**
- `analyze_request()` - Interpréter demande d'analyse
- `auto_select_method()` - Choisir meilleure méthode basée sur données
- `execute_analysis()` - Exécuter analyse en R
- `validate_assumptions()` - Vérifier hypothèses stat avant exécution
- `generate_interpretation()` - Interpréter résultats en langage naturel
- `suggest_visualizations()` - Proposer visualisations pertinentes

**Analyses supportées:**
- Réduction dimension: ACP, ACM, FAMD, AFC
- Tests stats: t-test, χ², ANOVA, Kruskal-Wallis
- Modèles: Régression, GLM, Random Forest, K-means
- Diagnostiques: Résidus, normalité, hétéroscédasticité

---

### **5. `report_agent.R`** (600 lignes) ⭐⭐⭐
**Rôle:** Agent Rapports Auto-Générés (Executive → Student)

**Fonctions principales:**
- `generate_report()` - Créer rapport structuré
- `select_report_style()` - 4 styles: Executive (1p), Technical (5-10p), Field (2-3p), Student (3-5p)
- `generate_sections()` - Auto-générer sections (Context, Methods, Results, Discussion, Limitations)
- `format_output()` - Exporter Word/PDF/HTML publication-ready
- `cite_sources()` - Inclure références méthodologie
- `localize_report()` - Adapter langue (FR/EN/ES)

**Structure rapport intelligent:**
1. Executive Summary (automatique)
2. Contexte (basé sur données + objectif)
3. Méthodologie (analyses exécutées)
4. Résultats (tableaux + graphiques)
5. Discussion (interprétation contextualisée)
6. Limitations (détection automatique)
7. Recommandations (basées sur résultats)

---

### **6. `diagnose_agent.R`** (500 lignes) ⭐⭐⭐
**Rôle:** Agent Diagnostic & Auto-Correction Erreurs Utilisateur

**Fonctions principales:**
- `detect_user_error()` - Identifier erreurs commises
- `explain_root_cause()` - Expliquer cause racine pédagogiquement
- `suggest_solutions()` - Proposer 2-3 solutions (simple → expert)
- `auto_correct()` - Corriger certaines erreurs avec approbation user
- `learn_from_error()` - Tracker erreurs pour améliorer prompts futurs
- `provide_education()` - Ressources apprentissage sur concept erroné

**Types d'erreurs détectées:**
- Type données incompatibles (ACP sur categorials)
- Variables manquantes/insuffisantes
- Valeurs aberrantes/impossibles
- Hypothèses stat violées
- Paramètres analytiques illogiques
- Incohérences résultats

---

### **7. `master_agent.R`** (400 lignes) ⭐⭐
**Rôle:** Orchestrateur Multi-Agent pour Requêtes Complexes

**Fonctions principales:**
- `route_request()` - Diriger demande au bon agent(s)
- `coordinate_agents()` - Coordonner plusieurs agents si besoin
- `synthesize_results()` - Combiner résultats agents
- `handle_context_switching()` - Gérer transitions entre agents
- `validate_workflow()` - Vérifier enchaînement logique requêtes
- `fallback_strategy()` - Stratégie si agent échoue

**Exemples routage:**
- "Explique ACP" → Chat Agent
- "Fais une ACP sur [données]" → Analysis Agent
- "Génère rapport" → Report Agent (+ Analysis si besoin)
- "Pourquoi erreur?" → Diagnose Agent

---

### **8. `chatbot_ui_v3.R`** (300 lignes) 🎨
**Rôle:** Interface utilisateur moderne pour Chat IA

**Composants:**
- `chatbot_ui()` - Module Shiny pour sidebar chat
- `message_list_ui()` - Afficher historique messages
- `input_area_ui()` - Zone saisie intelligente
- `response_formatting()` - Stylage réponses IA (code blocks, formules, etc.)
- `loading_states()` - Indicateurs chargement/thinking

**Features:**
- ✅ Auto-expand si réponse longue
- ✅ Syntax highlighting code R/Python
- ✅ Affichage formules LaTeX
- ✅ Boutons actions rapides (Regenerate, Copy, Export)
- ✅ Historique navigable

---

### **9. `chatbot_server_v3.R`** (400 lignes) ⚙️
**Rôle:** Logique serveur Chat (traitement messages, routage agents)

**Fonctions principales:**
- `chatbot_server()` - Module Shiny serveur
- `process_message()` - Traiter message utilisateur
- `route_to_agent()` - Déterminer agent approprié
- `stream_response()` - Afficher réponse en temps réel
- `save_conversation()` - Sauvegarder historique
- `export_chat()` - Exporter conversation en Markdown/PDF

**Intégrations:**
- Détection langue automatique (FR/EN/ES)
- Extraction contexte données courantes
- Linking avec analyses courantes app
- Historique session persistent

---

## 🔄 Flux Intégration dans `app_v2_final.R`

**Ajouts requis (50 lignes):**

```r
# Ligne ~40: Sources
source("config_api_secure_v2.R")
source("context_manager_v3.R")
source("gemini_api_v3.R")
source("chat_agent.R")
source("analysis_agent.R")
source("report_agent.R")
source("diagnose_agent.R")
source("master_agent.R")
source("chatbot_ui_v3.R")
source("chatbot_server_v3.R")

# Dans UI (après shinydashboard box definitions): Ajouter sidebar chat
#   chatbot_ui("ai_chat")

# Dans Server (après autres modules): Ajouter serveur chat
#   chatbot_server("ai_chat", df_f, map_cols, context_manager)
```

---

## 📦 Dépendances Requises

```r
# Déjà installées:
httr, jsonlite, tidyverse, FactoMineR, e1071

# À installer (si absent):
install.packages("stringi")      # Detection langue
install.packages("rmarkdown")    # Export rapports
install.packages("tinytex")      # LaTeX pour formules
```

---

## ✅ Checklist Implémentation

### Phase 1: Infrastructure (Files 1-2)
- [ ] `context_manager_v3.R` (capture contexte)
- [ ] `gemini_api_v3.R` (wrapper API robuste)
- [ ] Tests unitaires basiques

### Phase 2: Agents Autonomes (Files 3-6)
- [ ] `chat_agent.R` (support & Q&A)
- [ ] `analysis_agent.R` (analyses auto)
- [ ] `report_agent.R` (rapports auto)
- [ ] `diagnose_agent.R` (diagnostic erreurs)

### Phase 3: Orchestration & UI (Files 7-9)
- [ ] `master_agent.R` (routage multi-agent)
- [ ] `chatbot_ui_v3.R` (interface)
- [ ] `chatbot_server_v3.R` (logique serveur)

### Phase 4: Intégration Finale
- [ ] Ajouter sources dans `app_v2_final.R`
- [ ] Ajouter UI chatbot dans tableau de bord
- [ ] Ajouter serveur chatbot
- [ ] Tests d'intégration
- [ ] Documentation utilisateur

---

## 🧪 Tests Critiques

| Test | Fichier | Cas | Expected |
|------|---------|-----|----------|
| Config API | `gemini_api_v3.R` | Charger config + appel | ✅ Réponse sans clé exposée |
| Context capture | `context_manager_v3.R` | User + data + analysis | ✅ Contexte complet structure |
| Chat multilangue | `chat_agent.R` | Q en FR/EN/ES | ✅ Réponse même langue |
| Auto-analyse | `analysis_agent.R` | Demande ACP | ✅ ACP exécutée + interprétée |
| Rapport | `report_agent.R` | "Generate report" | ✅ Word/PDF prêt |
| Diagnostic | `diagnose_agent.R` | Erreur utilisateur | ✅ Solution proposée |
| Routage | `master_agent.R` | Multi-demandes | ✅ Agents corrects activés |

---

## ⏱️ Estimations Temps

| Fichier | Complexité | Temps | Notes |
|---------|-----------|-------|-------|
| 1. context_manager_v3.R | ⭐ | 45 min | Infrastructure légère |
| 2. gemini_api_v3.R | ⭐⭐⭐ | 90 min | Streaming, caching, erreurs |
| 3. chat_agent.R | ⭐⭐⭐ | 120 min | Prompt nuancés, multilingue |
| 4. analysis_agent.R | ⭐⭐⭐ | 150 min | Sélection auto-méthode |
| 5. report_agent.R | ⭐⭐⭐ | 150 min | Génération sections complexe |
| 6. diagnose_agent.R | ⭐⭐ | 90 min | Détection + suggestions |
| 7. master_agent.R | ⭐⭐ | 75 min | Orchestration relativement simple |
| 8. chatbot_ui_v3.R | ⭐ | 60 min | Interface Shiny standard |
| 9. chatbot_server_v3.R | ⭐⭐ | 90 min | Logique routage + streaming |
| **Intégration finale** | ⭐ | 30 min | Ajouts `app_v2_final.R` |
| **TOTAL** | | **8h 20min** | |

---

## 🚀 Ordre de Création Recommandé

1. **`context_manager_v3.R`** - Fondation (utilisé par tous)
2. **`gemini_api_v3.R`** - API wrapper (infrastructure)
3. **`chat_agent.R`** - Agent simple (valider architecture)
4. **`analysis_agent.R`** - Agent complexe (plus de logique)
5. **`report_agent.R`** - Agent spécialisé
6. **`diagnose_agent.R`** - Agent diagnostic
7. **`master_agent.R`** - Orchestrateur
8. **`chatbot_ui_v3.R`** + **`chatbot_server_v3.R`** - UI/Server ensemble
9. **Intégration finale** - Ajouter dans app_v2_final.R

---

## 🔐 Sécurité & Best Practices

- ✅ Clé API chargée via `config_api_secure_v2.R` (masquée)
- ✅ Code généré Gemini jamais exécuté directement (validation)
- ✅ Data jamais envoyée brute à Gemini (contexte + métadonnées seulement)
- ✅ Caching réponses pour réduire appels API
- ✅ Rate limiting respecté (~60 req/min)
- ✅ Erreurs masquent clé automatiquement
- ✅ Conversation locale (pas stockée Google)

---

## 📝 Livrables

Après implémentation complète:

1. ✅ 9 fichiers R (~3,500 lignes)
2. ✅ Chat IA intégré dans app_v2_final.R
3. ✅ 4 agents autonomes fonctionnels
4. ✅ Master Agent orchestrant requêtes
5. ✅ Documentation technique (READMEs)
6. ✅ Exemples utilisation
7. ✅ Tests validés

---

## ⚠️ Limitations & Futures Améliorations

**Limitations actuelles:**
- Analyses exécutées localement (pas multi-session)
- Rapports générés texte (pas graphiques intégrés)
- Pas de fine-tuning modèle Gemini
- Conversation stockée mémoire (pas persistence)

**Futures (Niveau 4):**
- Persistence conversation en BD
- Fine-tuning Gemini sur données OMS
- Analyses multi-session avec queue
- Export rapports avec graphiques intégrés
- Webhooks intégrations externes

---

## 📋 Approbation Plan

**À Valider:**
- [ ] Architecture agent OK?
- [ ] Ordre création accepté?
- [ ] Estimations temps réalistes?
- [ ] Tous les fichiers nécessaires?

**Approuvé par:** [À Valider]  
**Date approbation:** [À Compléter]  
**Démarrage implémentation:** [À Compléter]

---

**Plan Version:** 1.0  
**Status:** 🔄 En Attente Approbation  
**Prêt pour implémentation:** À Confirmer
