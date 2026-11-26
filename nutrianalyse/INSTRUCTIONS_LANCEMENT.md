# Instructions de lancement — NutriAnalyse Pro v2

Ce guide décrit le lancement de l'application depuis le dossier
`nutrianalyse/`. Pour l'installation complète, la configuration et la
méthodologie, voir [README.md](README.md).

---

## 1. Prérequis

- R **4.5.2** (version verrouillée par `renv.lock`) ;
- RStudio ou Positron (recommandé), ou `Rscript` dans le `PATH` ;
- Pandoc pour les exports Word/PDF/HTML (fourni par RStudio/Quarto) ;
- Chrome ou Chromium pour l'export PDF via `pagedown::chrome_print()`.

---

## 2. Installation de l'environnement

Depuis la racine du dépôt :

```bash
cd nutrianalyse
Rscript -e 'renv::restore()'
```

Sous Windows, si `Rscript` n'est pas reconnu :

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "renv::restore()"
```

La restauration installe les dépendances verrouillées dans `renv.lock`. Elle
peut prendre plusieurs minutes au premier lancement.

---

## 3. Configuration locale

Créer le fichier `.Renviron` à partir du modèle :

```bash
cp .Renviron.example .Renviron
```

Sous Windows PowerShell :

```powershell
Copy-Item .Renviron.example .Renviron
```

Renseigner si nécessaire :

```text
GEMINI_API_KEY=...
# NUTRI_ENV=production
# NUTRI_USERS_FILE=config/users.rds
```

Sans `GEMINI_API_KEY`, l'application démarre normalement ; seuls les agents IA
sont désactivés. Ne jamais committer `.Renviron` ni `config/users.rds`.

---

## 4. Lancement

### Méthode recommandée — terminal

Depuis `nutrianalyse/` :

```bash
Rscript tools/run_app.R
```

L'application démarre sur **http://127.0.0.1:8765** et ouvre le navigateur.

Changer le port :

```bash
NUTRI_PORT=3838 Rscript tools/run_app.R
```

Sous Windows PowerShell :

```powershell
$env:NUTRI_PORT = "3838"
Rscript tools/run_app.R
```

### Méthode RStudio

1. Ouvrir `nutrianalyse/Shiny_entreprise.Rproj` ou `nutrianalyse/app.R`.
2. Vérifier que le répertoire de travail est `nutrianalyse/` (`getwd()`).
3. Cliquer sur **Run App**.

### Méthode console R

```r
setwd("chemin/vers/nutrianalyse")
shiny::runApp(".")
```

---

## 5. Arrêt

- RStudio : bouton **Stop** ou `Échap` ;
- terminal : `Ctrl+C`.

---

## 6. Vérifications rapides

```bash
Rscript tools/check_app.R
Rscript tools/check_server.R
Rscript tools/run_tests.R
Rscript tools/check_expert_modules.R
Rscript tools/launch_test.R
```

Résultats attendus : `CHECK_OK`, `CHECK_SERVER_OK`, 54 tests sans échec,
`CHECK_EXPERT_MODULES_OK` (10 modules), `HTTP_OK`.

---

## 7. Dépannage

| Problème | Cause probable | Solution |
|---|---|---|
| `Rscript` introuvable | R absent du `PATH` | Utiliser le chemin complet ou RStudio |
| `There is no package called …` | `renv` non restauré | `Rscript -e 'renv::restore()'` |
| `renv::status()` non synchronisé | Bibliothèque incohérente | `renv::restore()` puis `renv::status()` |
| Export PDF en échec | Chrome/Chromium absent | Installer Chrome ou exporter en Word/HTML |
| Pandoc introuvable | `RSTUDIO_PANDOC` non défini | Installer RStudio/Quarto ou définir la variable |
| Agents IA indisponibles | `GEMINI_API_KEY` vide/invalide | Renseigner `.Renviron` |
| Démarrage refusé en production | `config/users.rds` absent | `Rscript tools/manage_users.R add …` |
| Port 8765 occupé | Autre instance Shiny | `NUTRI_PORT=3838` |

---

## 8. En production

1. Générer les comptes :

   ```bash
   Rscript tools/manage_users.R add alice "Alice D."
   ```

2. Définir `NUTRI_ENV=production` et `GEMINI_API_KEY` dans l'environnement du
   serveur (jamais dans le dépôt).
3. Vérifier que les comptes de démonstration sont refusés.
4. Servir l'application derrière HTTPS et restreindre les exports.

Voir [SECURITY_GUIDE.md](SECURITY_GUIDE.md) pour la procédure complète.

---

*Dernière mise à jour : novembre 2026.*
