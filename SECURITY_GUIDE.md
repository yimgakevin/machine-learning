# 🔐 Guide de Sécurité - NutriAnalyse Pro v2

## ⚠️ CRITICAL: Protection de la Clé API Google

Votre clé API Google est **SENSIBLE** et doit être protégée à tout prix.

### 1. ✅ Protections Mises en Place

#### A. Fichier `.env` (Local Only)
```
.env  ← Contient votre clé API
      ← JAMAIS ajouté à Git
      ← JAMAIS partagé
```

**Contenu:**
```
GOOGLE_API_KEY=REDACTED_GEMINI_KEY
GEMINI_MODEL=gemini-2.5-flash
API_RATE_LIMIT=60
API_TIMEOUT=30
```

#### B. Fichier `.gitignore`
Empêche que `.env` soit commité accidentellement:
```
.env
.env.local
.secrets/
```

#### C. Fonction `config_api_secure.R`
Charge la clé de façon sécurisée:
```r
source("config_api_secure.R")
config <- load_secure_config()  # Charge depuis .env
```

**Mécanismes de sécurité:**
- ✅ Clé en mémoire SEULEMENT, jamais en logs
- ✅ Masquage automatique en cas d'erreur: `AIzaSy[...]****MASKED****`
- ✅ Message de chargement sécurisé (sans exposer la clé)
- ✅ Validation stricte présence/format clé

---

## 2. 🚫 Ce qu'il NE FAUT PAS FAIRE

| ❌ À ÉVITER | ✅ À FAIRE |
|----------|----------|
| `print(config$api_key)` | `print_config_safe(config)` |
| `cat("Key:", api_key)` | `call_gemini_api(config, prompt)` |
| Commiter `.env` | `.env` dans `.gitignore` |
| Hardcoder la clé dans le code | Utiliser `config_api_secure.R` |
| Partager `.env` par email/chat | Ne JAMAIS partager `.env` |
| Pusher sur GitHub/GitLab | Vérifier `.gitignore` avant push |
| Partager console output (logs) | Masquer clé dans messages d'erreur |
| Laisser clé en clair dans code | Utiliser variables d'environnement |

---

## 3. 🔑 Gestion de la Clé API

### Initialisation (DONE ✅)
```bash
# La clé est dans .env (fichier local, git-ignoré)
```

### Utilisation dans app
```r
# app_v2_final.R ou autre module

source("config_api_secure.R")

# ✅ CORRECT: Charger config sécurisée
config <- load_secure_config()

# ✅ CORRECT: Utiliser config
response <- call_gemini_api(config, prompt)

# ✅ CORRECT: Afficher status
print_config_safe(config)

# ❌ JAMAIS: print(config$api_key)
# ❌ JAMAIS: cat("API Key:", config$api_key)
```

### En cas de compromission
Si la clé est accidentellement exposée:
1. **IMMÉDIATEMENT**: Allez dans Google AI Studio
2. Révoquez la clé compromise
3. Générez une nouvelle clé
4. Mettez à jour `.env` avec nouvelle clé
5. Cherchez les logs/historique qui l'auraient exposée

---

## 4. 🛡️ Checklist de Sécurité

Avant chaque commit:
```
☐ .env n'est PAS dans Git (vérifier git status)
☐ Aucune clé en clair dans le code source
☐ Aucun hardcode API_KEY dans les fichiers .R
☐ Logs/console ne contiennent pas la clé
☐ Pas de messages d'erreur exposant la clé
```

Vérifier:
```bash
# Vérifier que .env est ignoré
git check-ignore -v .env
# Output: .env  .gitignore  ✅

# Chercher clés potentiellement exposées
grep -r "AIzaSy" --include="*.R" --include="*.Rmd"
# Output: (vide = bon)
```

---

## 5. 📝 Workflow Recommandé

### 1️⃣ Démarrage Projet
```r
# app_v2_final.R
source("config_api_secure.R")  # Charge depuis .env

GEMINI_CONFIG <- load_secure_config()
```

### 2️⃣ Appels API
```r
# Partout où vous utiliserez Gemini
response <- call_gemini_api(
  GEMINI_CONFIG,
  prompt = "Votre prompt ici"
)
```

### 3️⃣ Gestion Erreurs
```r
tryCatch({
  response <- call_gemini_api(GEMINI_CONFIG, prompt)
}, error = function(e) {
  # L'erreur est AUTOMATIQUEMENT masquée
  message("Error:", conditionMessage(e))
})
```

### 4️⃣ Avant Push Git
```bash
git add .
git status  # Vérifier que .env n'y est PAS
git commit -m "Message"
git push origin main
```

---

## 6. 🔍 Audit de Sécurité Régulier

Mensuel:
- [ ] Vérifier clé Google AI Studio toujours valide
- [ ] Vérifier pas d'usage anormal (quotas Google Cloud)
- [ ] Vérifier pas de commits accidentels de `.env`
- [ ] Vérifier logs serveur pour clé exposée

---

## 7. 📞 Support Émergence

**Si clé compromise:**
1. Revoke immédiatement dans Google AI Studio
2. Générer nouvelle clé
3. Mettre à jour `.env`
4. Alerter équipe
5. Vérifier historique Git pour expositions accidentelles

**Contacts:**
- Google Cloud Security: https://cloud.google.com/trust-center
- Gemini API Docs: https://ai.google.dev

---

## 8. Fichiers de Configuration Créés

| Fichier | Rôle | Sensibilité |
|---------|------|------------|
| `.env` | Stocke clé API | 🔴 SECRET |
| `.gitignore` | Exclut `.env` de Git | 🟡 Important |
| `config_api_secure.R` | Charge config sécurisée | 🟢 Public |
| `SECURITY_GUIDE.md` | Ce document | 🟢 Public |

---

**Dernière mise à jour:** March 25, 2026  
**Status:** ✅ Configuration de sécurité activée
