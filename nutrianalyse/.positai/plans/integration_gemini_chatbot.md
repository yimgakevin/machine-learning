# PLAN : Intégration Gemini 2.5 Flash + Chatbot Intelligent dans NutriAnalyse Pro v2

**Date :** 25 mars 2026  
**Demande :** Intégrer une IA (Gemini 2.5 Flash) + chatbot intelligent dans l'application Shiny  
**Auteur :** Kevin Yimga (DSAMID)

---

## 1. CONTEXTE & OPPORTUNITÉS

### État actuel de l'application
- ✅ 28+ modules d'analyse nutritionnelle & épidémiologique
- ✅ Documentation complète (1136 lignes)
- ✅ Interfaces UX professionnelles
- ❌ **Pas d'assistance intelligente pour l'utilisateur**
- ❌ **Pas d'aide contextuelle lors des analyses**

### Opportunités d'une IA intégrée

| Cas d'usage | Bénéfice | Priorité |
|-------------|----------|----------|
| 🤖 **Chatbot support** | Répondre questions, guider utilisateur | ⭐⭐⭐⭐⭐ |
| 📊 **Interprétation automatique** | Expliquer résultats ACP, tests stats | ⭐⭐⭐⭐⭐ |
| 🎓 **Tutoriel interactif** | Apprendre à utiliser les modules | ⭐⭐⭐⭐ |
| 💡 **Recommandations intelligentes** | "Avez-vous considéré..." | ⭐⭐⭐⭐ |
| 🐛 **Diagnostic d'erreurs** | Expliquer pourquoi une analyse a échoué | ⭐⭐⭐⭐ |
| 📝 **Génération de rapports narratifs** | Résumés textuels automatiques | ⭐⭐⭐⭐⭐ |
| 🌍 **Multilingue** | Support FR/EN/ES sans effort | ⭐⭐⭐ |

---

## 2. ARCHITECTURE PROPOSÉE

### 2.1 Stack Technologique

```
┌─────────────────────────────────────────────────────────┐
│  FRONTEND (Shiny R)                                     │
│  ├─ Chat UI (nouvelle onglet ou sidebar collapsible)   │
│  ├─ Messages interactifs (user ↔ AI)                   │
│  ├─ Context awareness (quelle analyse en cours ?)       │
│  └─ Historique conversation                             │
├─────────────────────────────────────────────────────────┤
│  BACKEND MIDDLEWARE (Node.js / Python FastAPI)          │
│  ├─ API Gateway                                         │
│  ├─ Prompt Engineering                                  │
│  ├─ Context Management                                  │
│  ├─ Rate Limiting & Caching                             │
│  └─ Error Handling                                      │
├─────────────────────────────────────────────────────────┤
│  AI SERVICE (Gemini 2.5 Flash)                          │
│  ├─ Text generation                                     │
│  ├─ Data analysis interpretation                        │
│  ├─ Question answering                                  │
│  └─ Multilingue                                         │
└─────────────────────────────────────────────────────────┘
```

### 2.2 Flux d'interaction

```
User écrit "Pourquoi GAM > 15%" 
    ↓
Shiny client → capture contexte
    • Module actuel: "Indicateurs OMS"
    • Données affichées: GAM = 16.5%
    • Région sélectionnée: "Littoral"
    ↓
API Middleware enrich message avec contexte
    ↓
Gemini API reçoit: "L'utilisateur demande pourquoi GAM > 15%.
    Contexte: Module Indicateurs OMS, GAM = 16.5% (Littoral, Cameroun)"
    ↓
Gemini répond: "Un GAM > 15% indique une situation CRITIQUE.
    Cela signifie que 16.5% des enfants < 5 ans souffrent de malnutrition aiguë.
    Seuil d'urgence OMS: 15%. Recommandation: intervention immédiate."
    ↓
Shiny reçoit réponse → affiche dans chat avec formatting
```

---

## 3. IMPLÉMENTATION : 3 NIVEAUX

### NIVEAU 1 : Chatbot Simple (Baseline)

**Fichiers à créer :**
- `gemini_config.R` — Configuration API Gemini
- `chatbot_ui.R` — UI chatbot Shiny (tab ou sidebar)
- `chatbot_server.R` — Server logic

**Caractéristiques :**
- ✅ Chat box simple (user input → AI response)
- ✅ Gemini API call via `httr::POST()`
- ✅ Historique conversation en sessionStorage
- ✅ Support multilingue (FR/EN)
- ✅ Caching des questions fréquentes

**Avantages :**
- Déploiement rapide (0-2 semaines)
- Pas de backend externe
- Coût minimal (pay-as-you-go API Gemini)
- Facile à maintenir

**Limitation :**
- Pas de context awareness (le chatbot ne sait pas quelle analyse est en cours)
- Latency API Gemini (~2-3s par response)

### NIVEAU 2 : Chatbot Contextuel (Recommandé)

**Fichiers à créer :**
- Tous du Niveau 1 +
- `context_manager.R` — Capture contexte utilisateur
- `prompt_engineer.R` — Crée prompts intelligents selon contexte
- `gemini_api_wrapper.R` — Wrapper API avec caching

**Contexte capturé :**
```r
context <- list(
  current_module = "Indicateurs OMS",      # Quel onglet ?
  current_analysis = "GAM stratifié",      # Quelle analyse ?
  selected_country = "Cameroun",           # Quel pays ?
  selected_region = "Littoral",            # Quelle région ?
  data_summary = "N=847, 52 départements", # Résumé données
  last_error = NULL,                       # Dernière erreur ?
  user_language = "fr"                     # Langue préf ?
)
```

**Prompt enrichi :**
```
"Vous êtes un expert en nutrition humanitaire parlant français.
 L'utilisateur analyse des données de malnutrition du Cameroun (région Littoral).
 L'analyse en cours: Prévalences GAM stratifiées par département.
 
 Question de l'utilisateur: [user input]
 
 Répondez en français, concision, avec empathie. 
 Citez les standards OMS si pertinent.
 Suggérez actions concrètes si applicable."
```

**Avantages :**
- ✅ Réponses super pertinentes (IA connaît le contexte)
- ✅ Détection automatique du problème ("Vous avez 3 variables catégorique pour une ACP ?")
- ✅ Recommandations intelligentes ("Pourquoi pas une FAMD ?")
- ✅ Adaptation langue + domaine automatique

**Déploiement :** 2-4 semaines

### NIVEAU 3 : Assistant Complet + Agents (Avancé)

**Additions :**
- `data_analysis_agent.R` — Agent qui exécute analyses et explique
- `report_generator_agent.R` — Agent qui génère rapports narratifs
- `error_diagnostic_agent.R` — Agent qui diagnostique erreurs

**Exemple : Error Diagnostic Agent**
```
User essaie ACP, sélectionne "région" (catégorique)
    ↓
Erreur: "Pas assez de variables numériques"
    ↓
Error Diagnostic Agent:
  1. Détecte: "User a sélectionné 1 numérique + 1 catégorique pour ACP"
  2. Détermine: "Confusion entre ACP (numérique) et FAMD (mixte)"
  3. Génère message: "Vous avez mélangé ACP et FAMD.
     - ACP = variables NUMÉRIQUES seulement
     - FAMD = MIX numérique + catégorique
     Suggestion: Essayez FAMD avec vos données."
    ↓
User clique "👍 Essayer FAMD"
    ↓
Analyse relancée automatiquement avec FAMD
```

**Déploiement :** 4-8 semaines

---

## 4. CONSIDÉRATIONS TECHNIQUES

### 4.1 API Gemini 2.5 Flash : Pros vs Cons

| Aspect | ✅ Avantage | ⚠️ Limitation |
|--------|-----------|-------------|
| **Coût** | Pay-as-you-go, très abordable (~$0.0005 par 1000 tokens) | Cumule vite avec 1000 users |
| **Latency** | ~2-3s par request | Pas acceptable pour RT chat (need < 1s) |
| **Quality** | Excellente (GPT-4 level) | Français légèrement moins bon qu'anglais |
| **Multilingue** | Excellent (FR/EN/ES/etc) | Dépend de la qualité du prompt |
| **Rate limits** | 60 req/min (tier gratuit), ∞ (payant) | À monitorer |
| **Context window** | 1M tokens | Énorme (peut inclure tout historique) |
| **Hallucinations** | Rares avec bons prompts | Risque avec données médicales (danger!) |

### 4.2 Risques & Mitigations

| Risque | Probabilité | Gravité | Mitigation |
|--------|------------|---------|-----------|
| ❌ **IA invente données médicales** | Moyenne | 🔴 CRITIQUE | ✅ Fact-checking layer + disclaimers |
| ❌ **IA répond n'importe quoi** | Faible | Moyenne | ✅ Prompt engineering strict + examples |
| ❌ **Coût API explosif** | Moyenne | Moyenne | ✅ Rate limiting + caching réponses |
| ❌ **Données sensibles exposées à Google** | Faible | 🔴 CRITIQUE | ✅ Chiffrement en transit, pas d'UID patient |
| ❌ **Dépendance à API (offline ?)** | Faible | Faible | ✅ Fallback à FAQ locale |
| ❌ **Latency utilisateur mauvaise** | Moyenne | Faible | ✅ Streaming responses + async |

---

## 5. PLAN D'IMPLÉMENTATION RECOMMANDÉ

### Phase 1 : MVP Chatbot Simple (Semaines 1-2)

**Scope minimal mais fonctionnel :**

```
app_v2_final.R modifications:
├─ Ajouter onglet "💬 Assistant IA"
├─ Chat UI (input + output)
└─ Server: appel API Gemini simple

Nouveaux fichiers:
├─ gemini_config.R (API key, modèle, params)
├─ chatbot_ui.R (UI chat)
└─ chatbot_server.R (appels API, historique)
```

**Capacités :**
- Répondre questions libres sur nutrition/épidémiologie
- Expliquer concepts statistiques
- Guider sur utilisation OMS standards
- Support FR/EN

**Délai :** 1-2 semaines  
**Coût API :** ~$50-100/mois (1000 users actifs)

### Phase 2 : Chatbot Contextuel (Semaines 3-6)

**Ajouter intelligence :**
```
Nouveaux fichiers:
├─ context_manager.R
├─ prompt_engineer.R
└─ gemini_api_wrapper.R (avec caching)

Modifications app_v2_final.R:
└─ Capturer & passer contexte au chatbot
```

**Capacités supplémentaires :**
- Réponses contextuelles ("Vous analysez GAM de Cameroun")
- Diagnostic d'erreurs automatique
- Recommandations d'analyse
- Interprétation résultats (ACP, tests stats, etc.)

**Délai :** 3-4 semaines  
**Coût :** Inchangé (~$50-100/mois)

### Phase 3 : Agents Avancés (Semaines 7-12, OPTIONNEL)

**Automatisation complète :**
```
Agents spécialisés:
├─ data_analysis_agent.R      (exécute analyses, explique)
├─ report_generator_agent.R   (génère narratifs)
└─ error_diagnostic_agent.R   (diagnostique problèmes)
```

**Capacités :**
- Générer rapports textuels complets
- Exécuter analyses sur demande
- Autodiagnostic d'erreurs
- Self-learning (amélioration continue)

**Délai :** 4-6 semaines (optionnel)  
**Coût :** +$200-300/mois (volume API plus haut)

---

## 6. ARCHITECTURE DÉTAILLÉE : NIVEAU 2 (Recommandé)

### 6.1 Fichiers à créer

```
gemini_config.R (50 lignes)
├─ GEMINI_API_KEY = Sys.getenv("GEMINI_API_KEY")
├─ GEMINI_MODEL = "gemini-2.5-flash"
├─ SYSTEM_PROMPTS = list(
│   nutrition = "Vous êtes expert nutrition OMS...",
│   epidemio = "Vous êtes épidémiologiste...",
│   stats = "Vous êtes biostatisticien..."
│ )
└─ CONFIG = list(temp=0.7, max_tokens=1000, ...)

context_manager.R (200 lignes)
├─ create_user_context() — Capture contexte utilisateur
├─ update_context() — Mise à jour temps réel
└─ serialize_context() — Format pour API

prompt_engineer.R (150 lignes)
├─ build_system_prompt(context) — Crée prompt système dynamique
├─ build_user_message(context, question) — Enrichit question utilisateur
└─ select_persona(module) — Choisit expert (nutrition, stats, etc.)

gemini_api_wrapper.R (250 lignes)
├─ call_gemini(message, context, cache=TRUE)
├─ stream_response(message) — Streaming pour latency
├─ cache_common_responses() — Cache FAQ
└─ error_handler() — Gère timeouts, rate limits, etc.

chatbot_ui.R (150 lignes)
├─ chatbot_ui() — UI Shiny du chat
├─ message_bubble() — Stylise messages user/AI
└─ typing_indicator() — Animation "IA en train de taper..."

chatbot_server.R (300 lignes)
├─ observeEvent(input$send_message) — Capture message user
├─ handle_user_message() — Traite input
├─ call_gemini_and_display() — Appel API + affiche réponse
└─ save_conversation() — Historique persistent
```

**Total :** ~1100 lignes (modular, bien organisé)

### 6.2 Intégration dans app_v2_final.R

```r
# En haut (imports)
source("gemini_config.R")
source("context_manager.R")
source("prompt_engineer.R")
source("gemini_api_wrapper.R")
source("chatbot_ui.R")
source("chatbot_server.R")

# Dans UI (sidebar ou nouvel onglet)
tags$div(
  id = "chatbot_container",
  chatbot_ui("chatbot_main")
)

# Dans Server (après réactifs de base)
user_context <- reactive({
  create_user_context(
    module = input$menu,
    data = mapped_data(),
    analysis_status = isolate(last_analysis()),
    ...
  )
})

chatbot_server("chatbot_main", user_context)
```

---

## 7. CONSIDÉRATIONS DE DÉPLOIEMENT

### 7.1 Sécurité & Données sensibles

```r
# ❌ À ÉVITER
call_gemini(user_data = df)  # Envoyer données complètes à Google !

# ✅ À FAIRE
context <- list(
  n_obs = nrow(df),
  variables = colnames(df),
  preview = "Données de malnutrition Cameroun"
  # PAS: les données réelles
)
call_gemini(context = context)
```

### 7.2 Authentification API

```r
# Option 1 : Variable d'environnement (RECOMMANDÉ)
GEMINI_API_KEY <- Sys.getenv("GEMINI_API_KEY")

# Option 2 : Fichier .Renviron
# ~/.Renviron contient: GEMINI_API_KEY=sk-...

# Option 3 : Cloud secret manager (Google Cloud)
# Pour déploiement production Shiny Server
```

### 7.3 Coûts estimés

| Scénario | Users/mois | Messages/user | Tokens/msg | Coût/mois |
|----------|-----------|---------------|-----------|-----------|
| **MVP** | 100 | 50 | 500 | $12 |
| **Petite déploiement** | 500 | 50 | 500 | $60 |
| **Moyen** | 1000 | 50 | 500 | $120 |
| **Large** | 5000 | 50 | 500 | $600 |

*(Basé sur tarif Gemini : $0.075/1M input tokens, $0.3/1M output tokens)*

---

## 8. RISQUES & MITIGATIONS DÉTAILLÉES

### 8.1 Hallucinations médicales

**Risque :** IA invente des "seuils OMS" ou recommandations faux

**Mitigation :**
```r
# Fact-checking layer
fact_check <- function(response, context) {
  # Vérifier que réponse respecte:
  # ✓ Seuils OMS sont corrects (hardcodés)
  # ✓ Pas de recommandations médicales > trop spécifiques
  # ✓ Toujours suggérer "consulter expert" si doute
  
  # Ajouter disclaimer:
  paste(response,
        "\n\n⚠️ DISCLAIMER: Cette réponse est générée par IA. ",
        "Consultez un expert pour les décisions critiques.")
}
```

### 8.2 Rate limiting & coûts

**Risque :** Une boucle infinie d'appels API → facture Google énorme

**Mitigation :**
```r
rate_limiter <- list(
  max_calls_per_hour = 100,
  max_calls_per_user_per_hour = 10,
  queue = list()
)

cache_responses <- list(
  "Comment utiliser ACP ?" = "...",  # Répondre sans API
  "Qu'est-ce que GAM ?" = "..."
)
```

---

## 9. QUESTIONNAIRE POUR CLARIFIER BESOIN

**Avant d'approuver, répondre :**

1. **Scope chatbot :**
   - Niveau 1 (chatbot simple) - 2 semaines
   - Niveau 2 (contextuel) - 6 semaines ← RECOMMANDÉ
   - Niveau 3 (agents) - 12 semaines

2. **Multi-langue :**
   - FR seulement ?
   - FR + EN + ES ?

3. **Déploiement :**
   - Local (serveur personnel) ?
   - Cloud (Shiny Server, Posit Connect) ?
   - Nombre d'utilisateurs prévus ?

4. **Budget :**
   - API Gemini monthly cost acceptable ?
   - Infrastructure backend ok ?

5. **Données :**
   - Quelles données peuvent être partagées avec IA ?
   - Besoin chiffrement données ?

---

## 10. RECOMMANDATION FINALE

### ✅ OUI, intégrez Gemini + Chatbot

**Pourquoi :**
- 📈 Énorme valeur pour utilisateurs (support 24/7)
- 💰 Coût très abordable (~$100-200/mois)
- ⚡ Implémentation rapide & non-invasive
- 🔒 Sécurité manageable avec bons design

### 🎯 Démarrer par Niveau 2 (Chatbot Contextuel)

**Raison :** Meilleur équilibre coût/bénéfice/délai
- Apporte 80% de valeur
- Déploiement en 4-6 semaines
- Coût minimal
- Facile à upgrader vers Niveau 3 plus tard

### 📋 Timeline recommandée

```
Semaine 1-2  : Phase 1 MVP (chatbot simple)
Semaine 3-6  : Phase 2 Contextuel (+ intelligence)
Semaine 7+   : Phase 3 Agents (optionnel, future)
```

---

## ✅ CHECKLIST APPROBATION

- [ ] Approuvez scope (Niveau 1/2/3 ?)
- [ ] Confirmez multi-langue (FR/EN/ES ?)
- [ ] Validez API key strategy
- [ ] OK budget API Gemini (~$100/mois)
- [ ] Acceptez disclaimer IA (hallucinations possibles)

---

*Kevin Yimga — DSAMID Data Science & AI Integration*  
*Architecture pour intégration Gemini 2.5 Flash + Chatbot intelligent*
