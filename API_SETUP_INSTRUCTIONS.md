# 🔐 Instructions Configuration API Gemini - Sécurisée

## ✅ Configuration Complétée

Votre clé API Google a été configurée de manière **SÉCURISÉE** avec 3 couches de protection:

### 1. Fichiers Créés

| Fichier | Rôle | Traiter Comme |
|---------|------|---------------|
| `.env` | Stocke clé API en clair | 🔴 **SECRET** - Ne jamais partager |
| `.gitignore` | Exclut `.env` de Git | 🟢 Public (peut être shared) |
| `config_api_secure_v2.R` | Charge et protège config | 🟢 Public |
| `SECURITY_GUIDE.md` | Guide complet sécurité | 🟢 Public |

---

## 🚀 Utilisation Rapide

### Dans `app_v2_final.R` (ou tout autre script)

```r
# Au démarrage de l'app
source("config_api_secure_v2.R")

# Vérifier configuration
print_config_safe()

# Appel API simple
response <- gemini("Explique l'analyse de variance en 100 mots")

# Appel API avancé avec paramètres
response <- call_gemini_api(
  prompt = "Ton prompt ici",
  max_tokens = 2048,
  temperature = 0.7
)
```

---

## 🔒 Protections Activées

### A. Fichier `.env` (Non Versionné)

✅ Votre clé API est dans `.env`  
✅ `.env` est dans `.gitignore`  
✅ Impossible de commiter `.env` accidentellement  

```bash
# Vérifier (exécuter dans terminal Git Bash)
git check-ignore -v .env
# Output: .env  .gitignore  ✅
```

### B. Masquage Automatique

✅ Clé JAMAIS affichée en console  
✅ Clé JAMAIS loggée  
✅ Clé JAMAIS visible dans les erreurs  

```r
# ✅ Sécurisé - Affiche: AIzaSy****[MASKED]
print_config_safe()

# ✅ Sécurisé - Clé masquée automatiquement
response <- gemini("votre prompt")

# ❌ JAMAIS FAIRE - Afficherait la clé!
# print(GEMINI_CONFIG$api_key)
```

### C. Fonction Wrapper `gemini()`

Utilisation **extrêmement simple** et sécurisée:

```r
# Une ligne!
answer <- gemini("Explique l'épidémiologie en 50 mots")
```

---

## 📋 Checklist: Avant de Coder

- [ ] `.env` créé avec votre clé ✅
- [ ] `.gitignore` créé ✅
- [ ] `config_api_secure_v2.R` chargé ✅
- [ ] Test de connexion réussi ✅

**Vérifier test de connexion:**
```r
source("config_api_secure_v2.R")
test_gemini_connection()
# Output: ✅ Connexion API fonctionnelle!
```

---

## 🛑 IMPORTANT: Choses Interdites

### ❌ NE JAMAIS faire ceci:

```r
# ❌ Afficher la clé
print(GEMINI_CONFIG$api_key)
cat("Mon API key:", GEMINI_CONFIG$api_key)

# ❌ Mettre clé en dur dans le code
api_key <- "REDACTED_GEMINI_KEY"
call_api(api_key)

# ❌ Commiter .env
git add .env
git commit -m "Add API key"

# ❌ Partager .env
# Email: .env → ❌
# Slack: .env → ❌
# GitHub: .env → ❌
# Personne ne doit avoir ce fichier

# ❌ Logger la clé
cat("API Response:", response)  # Si response contient clé = BAD
```

---

## ✅ Bonnes Pratiques

```r
# ✅ Charger au démarrage
source("config_api_secure_v2.R")

# ✅ Utiliser les wrappers
response <- gemini(prompt)
response <- call_gemini_api(prompt)

# ✅ Afficher config de manière sécurisée
print_config_safe()  # Affiche: AIzaSy****[MASKED]

# ✅ Tester avant déploiement
test_gemini_connection()

# ✅ Erreurs sont auto-masquées
tryCatch({
  gemini("bad prompt with @#$%")
}, error = function(e) {
  # Message d'erreur masque la clé automatiquement
  message(conditionMessage(e))
})
```

---

## 🚨 En Cas de Problème

### Problem 1: "API Key missing"
**Cause:** `.env` non trouvé ou clé manquante  
**Solution:**
```bash
# Vérifier .env existe dans répertoire courant
ls -la | grep .env

# Vérifier contenu
cat .env | grep GOOGLE_API_KEY
```

### Problem 2: Erreur "Invalid API Key"
**Cause:** Clé expirée ou révoquée  
**Solution:**
1. Aller dans Google AI Studio: https://aistudio.google.com/
2. Générer nouvelle clé
3. Mettre à jour `.env` avec nouvelle clé

### Problem 3: Rate Limit atteint
**Cause:** Trop d'appels API (~60 req/min)  
**Solution:**
- Ajouter délais entre appels: `Sys.sleep(1)`
- Implémenter caching de réponses
- Utiliser batch processing

### Problem 4: Timeout
**Cause:** Appel API prend trop longtemps  
**Solution:**
```r
# Augmenter timeout
call_gemini_api(
  prompt,
  config = GEMINI_CONFIG,
  timeout = 60  # Passer à 60s au lieu de 30s
)
```

---

## 📊 Architecture de Sécurité

```
┌─────────────────────────────────────┐
│  app_v2_final.R                     │
│  source("config_api_secure_v2.R")   │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  config_api_secure_v2.R             │
│  ├─ Charge GEMINI_CONFIG            │
│  ├─ Expose gemini()                 │
│  └─ Masque clé auto                 │
└──────────────┬──────────────────────┘
               │
        ┌──────┴──────┐
        ▼             ▼
   ┌────────┐   ┌──────────────┐
   │  .env  │   │ Gemini API   │
   │ (LOCAL)│   │ (Google)     │
   └────────┘   └──────────────┘
   🔴 SECRET    🌐 Internet

FLUX SÉCURISÉ:
1. .env → GEMINI_CONFIG (mémoire, clé masquée)
2. gemini(prompt) → call_gemini_api()
3. Clé utilisée UNIQUEMENT pour requête HTTP
4. Clé JAMAIS loggée/affichée
5. Réponse reçue sans exposer clé
```

---

## 🔄 Workflow de Développement

### 1. Démarrage Session RStudio

```r
# Console
source("config_api_secure_v2.R")

# Output:
# ╔═════════════════════════════════════════════════════════════╗
# ║  🔐 Configuration API Gemini Chargée Avec Sécurité         ║
# ...
```

### 2. Vérifier Connexion

```r
test_gemini_connection()

# Output:
# 🧪 Test de connexion API Gemini...
# 1️⃣  Vérification configuration... ✅ OK
# 2️⃣  Vérification packages... ✅ OK
# 3️⃣  Test d'appel API... ✅ OK
# ✅ Connexion API fonctionnelle!
```

### 3. Utiliser API

```r
# Simple
response <- gemini("Explique l'IA")

# Avancé
response <- call_gemini_api(
  prompt = "...",
  max_tokens = 2048,
  temperature = 0.5
)
```

### 4. Avant Push Git

```bash
# Terminal
git status
# Vérifier que .env n'est PAS dans les fichiers à commiter

git add .
git commit -m "Feature: Implémentation module IA"
git push origin feature-branch
```

---

## 🎓 Ressources

| Ressource | Lien |
|-----------|------|
| **Google AI Studio** | https://aistudio.google.com/ |
| **Gemini API Docs** | https://ai.google.dev |
| **API Key Management** | https://console.cloud.google.com/apis/credentials |
| **Rate Limits** | https://ai.google.dev/docs/limits |

---

## ❓ FAQ

**Q: Puis-je partager mon `.env` avec mon équipe?**  
R: ❌ NON. Chaque personne doit avoir sa propre clé API générée dans Google AI Studio.

**Q: Où stocker la clé en production?**  
R: Utiliser des secrets management (Posit Connect Secrets, AWS Secrets Manager, etc.). PAS dans `.env`.

**Q: Comment revisualize la clé?**  
R: Vous ne pouvez pas. Google AI Studio ne réaffiche jamais une clé existante. Générer une nouvelle.

**Q: Puis-je utiliser une seule clé pour plusieurs environments?**  
R: ✅ Oui, techniquement. Mais recommandé d'avoir 1 clé = 1 environment pour sécurité.

**Q: Qu'est-ce qui se passe si quelqu'un compromise la clé?**  
R: 1) Allez dans Google AI Studio 2) Révoquez la clé 3) Générez une nouvelle 4) Mettez à jour `.env`

---

**Status:** ✅ Sécurité Activée  
**Dernière mise à jour:** March 25, 2026  
**Version:** 1.0 Production Ready
