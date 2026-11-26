# 🚀 Instructions de Lancement - NutriAnalyse Pro v2 Niveau 3

**Date:** March 25, 2026  
**Status:** 🟢 Production Ready

---

## ❌ NON - Ne pas utiliser ceci

```bash
# ❌ Ceci ne marche PAS dans la console R
Rscript LAUNCH.R
# Erreur: symbole inattendu
```

---

## ✅ OUI - Utilisez l'une de ces méthodes

### **Méthode 1: RStudio (RECOMMANDÉE)** 🎯

**Étape 1:** Ouvrez RStudio

**Étape 2:** Ouvrez le fichier `LAUNCH_SIMPLE.R` ou `app_v2_final.R`

**Étape 3:** Cliquez sur **"Run App"** (bouton vert en haut à droite)

OU

**Étape 3 (Alternative):** Exécutez dans la console R:
```r
setwd("C:/Users/Yimga kevin/Documents/Shiny_entreprise")
source("LAUNCH_SIMPLE.R")
```

---

### **Méthode 2: Terminal PowerShell/CMD** 💻

**Étape 1:** Ouvrez **PowerShell** ou **Command Prompt** (pas la console R)

**Étape 2:** Naviguez au dossier:
```bash
cd "C:\Users\Yimga kevin\Documents\Shiny_entreprise"
```

**Étape 3:** Lancez l'app:
```bash
Rscript LAUNCH_SIMPLE.R
```

---

### **Méthode 3: Console R directement** ✨

**Étape 1:** Dans RStudio Console, exécutez:

```r
setwd("C:/Users/Yimga kevin/Documents/Shiny_entreprise")
source("app_v2_final.R")
shinyApp(ui, server)
```

---

## 📱 Une fois l'app lancée

Vous verrez:
```
╔═══════════════════════════════════╗
║  Démarrage serveur Shiny...       ║
║                                   ║
║  📱 URL: http://localhost:3838    ║
║                                   ║
╚═══════════════════════════════════╝
```

**Ouvrez dans navigateur:** `http://localhost:3838`

---

## 🛑 Pour arrêter l'app

Appuyez sur:
- **ESC** (dans RStudio)
- **Ctrl+C** (dans Terminal)

---

## ⚠️ Si vous avez encore des erreurs

### Erreur: "plotly.yaml file not found"
```r
# Solution: Utiliser LAUNCH_SIMPLE.R au lieu de LAUNCH.R
source("LAUNCH_SIMPLE.R")
```

### Erreur: "Working directory"
```r
# Vérifier le répertoire courant
getwd()

# Changer vers le bon répertoire
setwd("C:/Users/Yimga kevin/Documents/Shiny_entreprise")
```

### Erreur: "File not found"
```r
# Vérifier que vous êtes dans le bon dossier
list.files()
# Vous devez voir: app_v2_final.R, LAUNCH_SIMPLE.R, etc.
```

---

## 🎯 Checklist Avant Lancement

- [ ] RStudio est ouvert
- [ ] Console R visible (pas en mode source)
- [ ] Répertoire courant = `C:/Users/Yimga kevin/Documents/Shiny_entreprise`
- [ ] Fichiers existent: `app_v2_final.R`, `LAUNCH_SIMPLE.R`, `.env`
- [ ] Clé API configurée dans `.env`

---

## 📊 Vérifier que tout est prêt

Exécutez dans la console R:

```r
setwd("C:/Users/Yimga kevin/Documents/Shiny_entreprise")

# Vérifier fichiers
cat("✅ Fichiers existent:\n")
cat("   app_v2_final.R:", file.exists("app_v2_final.R"), "\n")
cat("   .env:", file.exists(".env"), "\n")
cat("   config_api_secure_v2.R:", file.exists("config_api_secure_v2.R"), "\n")

# Vérifier répertoire
cat("\n✅ Répertoire courant:\n")
cat("   ", getwd(), "\n")

# Vérifier config API
cat("\n✅ Configuration API:\n")
source("config_api_secure_v2.R")
```

Si tout affiche ✅, vous êtes prêt à lancer!

---

## 🚀 Lancement Final

Exécutez:
```r
source("LAUNCH_SIMPLE.R")
```

L'app s'ouvrira automatiquement dans le navigateur!

---

## 📞 Support

| Problème | Solution |
|----------|----------|
| Erreur "symbole inattendu" | Vous êtes dans console R, pas terminal. Utilisez `source()` au lieu de `Rscript` |
| App ne démarre pas | Vérifier répertoire courant avec `getwd()` |
| Port 3838 occupé | Fermer autres instances Shiny, ou spécifier autre port |
| Clé API invalide | Vérifier `.env` contient clé valide |

---

**Status:** 🟢 Production Ready  
**Version:** 1.0  
**Date:** March 25, 2026
