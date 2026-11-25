# ✅ Checklist Sécurité Finale

## Avant de Coder Quoi Que Ce Soit

### 1️⃣ Vérifier Fichiers Créés
- [x] `.env` existe (contient GOOGLE_API_KEY)
- [x] `.gitignore` existe (contient .env)
- [x] `config_api_secure_v2.R` existe
- [x] `SECURITY_GUIDE.md` existe
- [x] `API_SETUP_INSTRUCTIONS.md` existe
- [x] `QUICK_START_API.md` existe

### 2️⃣ Tester Configuration

```r
# Exécuter dans la console R
source("config_api_secure_v2.R")

# Doit afficher: Configuration chargée ✅
# Doit afficher: AIzaSy****[MASKED]
print_config_safe()

# Doit afficher: ✅ Connexion API fonctionnelle!
test_gemini_connection()
```

### 3️⃣ Vérifier Git Protection

```bash
# Terminal ou Git Bash
cd "C:/Users/Yimga kevin/Documents/Shiny_entreprise"

# Vérifier .env est ignoré
git check-ignore -v .env
# Output: .env  .gitignore  ✅

# Vérifier .env n'est pas staged
git status
# Ne doit PAS lister .env
```

### 4️⃣ Tester Premier Appel API

```r
source("config_api_secure_v2.R")

# Test simple
response <- gemini("Qui a créé Google?")
print(response)
# Doit afficher réponse de Gemini

# Test avec paramètres
response <- call_gemini_api(
  prompt = "Explique l'IA en 30 mots",
  max_tokens = 100,
  temperature = 0.7
)
```

### 5️⃣ Vérifier Masquage de Clé

```r
# Console
test <- "Erreur avec clé VOTRE_CLE_GEMINI"
masked <- mask_api_key(test)
print(masked)
# Doit afficher: "Erreur avec clé ****[GOOGLE_API_KEY_MASKED]****"

# Jamais de clé en clair!
```

---

## ⚠️ Avant Chaque Push Git

### 6️⃣ Vérifier Pas de .env

```bash
# Terminal
git status
# ❌ NE DOIT PAS CONTENIR: .env

# Si .env apparaît:
git rm --cached .env
git commit -m "Remove .env from tracking"
```

### 7️⃣ Chercher Clés Exposées

```bash
# Vérifier aucune clé en dur dans code
grep -r "AIzaSy" --include="*.R" .
# Output: (vide = bon!)

# Vérifier dans Git history
git log -p | grep AIzaSy
# Output: (vide = bon!)
```

### 8️⃣ Vérifier Config Sécurisée

```r
# JAMAIS de:
print(GEMINI_CONFIG$api_key)     # ❌
cat(GEMINI_CONFIG$api_key)       # ❌

# À LA PLACE:
print_config_safe()               # ✅
gemini(prompt)                    # ✅
call_gemini_api(prompt)           # ✅
```

---

## En Production (Shiny Déployée)

### 9️⃣ Gérer .env en Déploiement

**Option 1: Posit Connect (Recommandé)**
```
Ne pas inclure .env dans déploiement
Utiliser Posit Connect Secrets Manager
Injector clé via variables d'environnement
```

**Option 2: Docker/Conteneur**
```
Ne pas inclure .env dans image
Passer clé via ENV vars au runtime
Documenter dans Dockerfile
```

**Option 3: AWS Secrets Manager**
```
Stocker clé dans AWS Secrets Manager
Récupérer à runtime via SDK
Ne pas faire circuler .env
```

### 🔟 Audit Régulier (Mensuel)

- [ ] Clé Google AI Studio toujours valide?
- [ ] Aucun usage anormal (quotas OK)?
- [ ] Aucun commit accidentel .env?
- [ ] Aucune clé exposée dans logs?
- [ ] Toute l'équipe suit bonnes pratiques?

---

## 🚨 En Cas de Problème

### Si .env a été Commité

```bash
# 1. Révoquer immédiatement la clé
#    → Google AI Studio → Delete key

# 2. Générer nouvelle clé
#    → Google AI Studio → Create new key

# 3. Mettre à jour .env
#    → GOOGLE_API_KEY=AIzaSy[NOUVELLE]

# 4. Nettoyer Git history
git filter-branch --tree-filter 'rm -f .env' -- --all
git push origin --force --all

# ⚠️ Danger: force push! Coordonner avec équipe
```

### Si Clé est dans Console/Logs

```r
# Vérifier historique
cat(.Rhistory)
# Si clé visible:

# 1. Révoquer clé dans Google AI Studio
# 2. Générer nouvelle
# 3. Mettre à jour .env
# 4. Redémarrer R session

rm(list=ls())  # Vider mémoire
quit()         # Quitter R
# Redémarrer R
source("config_api_secure_v2.R")
```

---

## 📋 Checklist Quotidienne

Avant de coder:
- [ ] .env existe et contient clé valide
- [ ] source("config_api_secure_v2.R") fonctionne
- [ ] print_config_safe() affiche AIzaSy****[MASKED]
- [ ] test_gemini_connection() retourne ✅

Avant de push:
- [ ] git status ne liste PAS .env
- [ ] Aucune clé en dur dans code
- [ ] Pas de print(GEMINI_CONFIG$api_key)
- [ ] Tous les appels via gemini() ou call_gemini_api()

Avant de déployer:
- [ ] .env n'est PAS dans déploiement
- [ ] Clé injectée via secrets manager
- [ ] Logs ne contiennent PAS clé
- [ ] Configuration Posit Connect prête

---

## 📞 Support

**Documentation:**
- SECURITY_GUIDE.md (8 sections complètes)
- API_SETUP_INSTRUCTIONS.md (10 sections avec exemples)
- QUICK_START_API.md (démarrage rapide)
- config_api_secure_v2.R (code annoté)

**Ressources Externes:**
- Google AI Studio: https://aistudio.google.com/
- Gemini API Docs: https://ai.google.dev
- Cloud Console: https://console.cloud.google.com

---

## ✅ Signature d'Approbation

| Aspect | Status | Verificateur | Date |
|--------|--------|-------------|------|
| Config chargée | ✅ | Système | 2026-03-25 |
| Masquage test | ✅ | Système | 2026-03-25 |
| API test | ⏳ | Utilisateur | - |
| Git protection | ✅ | Système | 2026-03-25 |
| Production ready | ⏳ | Utilisateur | - |

---

**IMPORTANT:** Avant tout développement, cocher toutes les cases ci-dessus!

**Status:** 🟡 Checklist à Valider  
**Date:** March 25, 2026
