# 🔐 Guide de Sécurité — NutriAnalyse Pro v2

## 1. Clé API Gemini

La clé n'est **jamais écrite dans le code**. Elle est lue au démarrage :

```r
Sys.getenv("GEMINI_API_KEY")
```

### Configuration locale

```bash
cp .Renviron.example .Renviron
# puis renseigner GEMINI_API_KEY dans .Renviron
```

`.Renviron` est ignoré par git (`.gitignore`). Ne jamais le committer, ni le
partager par email ou messagerie.

### Déploiement

Définir `GEMINI_API_KEY` dans les variables d'environnement du serveur
(Shiny Server, Posit Connect, Docker, etc.). Sans clé, l'application démarre
normalement : seuls les agents IA sont désactivés.

### Vérification

```r
source("config_api_secure_v2.R")
print_config_safe()          # affiche ****[MASKED], jamais la clé
test_gemini_connection()     # test réel, facultatif
```

### En cas de compromission

1. Révoquer immédiatement la clé dans Google AI Studio / Google Cloud.
2. Générer une nouvelle clé.
3. La placer dans `.Renviron` ou dans l'environnement du serveur.
4. Vérifier l'historique git et les logs pour détecter d'autres expositions.

---

## 2. Authentification et rôles

Les utilisateurs ne sont pas définis dans le code source.

| Élément | Rôle | Sensibilité |
|---------|------|-------------|
| `config/users.rds` | Comptes et hashes `sodium` | 🔴 Secret, hors git |
| `tools/manage_users.R` | Création / rotation des comptes | 🟢 Public |
| `auth_config.R` | Chargement et permissions | 🟢 Public |
| `.Renviron` | Clé API et chemins | 🔴 Secret, hors git |
| `.Renviron.example` | Modèle sans secret | 🟢 Public |

### Gérer les comptes

```bash
Rscript tools/manage_users.R list
Rscript tools/manage_users.R add alice analyste "Alice D."
Rscript tools/manage_users.R passwd alice
Rscript tools/manage_users.R remove alice
```

Les mots de passe sont hachés avec `sodium` (argon2), jamais stockés en clair.
Le mot de passe peut être fourni par `NUTRI_NEW_PASSWORD` (pratique en CI) ou
saisi interactivement (12 caractères minimum).

### Mode démonstration

Sans `config/users.rds`, l'application utilise trois comptes de démonstration
dont les mots de passe sont connus. Ce mode est **refusé** quand
`NUTRI_ENV=production` : le démarrage échoue avec un message explicite.

### Rôles

- `admin` : tous les onglets, export, suppression, IA, cartographie.
- `analyste` : analyse et export, pas de suppression.
- `lecteur` : consultation seule, sans IA ni cartographie.

---

## 3. Diffusion des résultats

Les exports CSV/Excel appliquent par défaut une règle de petits effectifs :

- strate de moins de 30 observations → prévalences et IC remplacés par `NA` ;
- strate comportant 1 à 4 cas → cas et prévalences masqués ;
- une colonne `Diffusion` documente chaque ligne (`Libre`, `n < 30`,
  `1-4 cas`).

La case « Masquer les petits effectifs » est activée par défaut dans chaque
module expert. Elle ne doit être désactivée que pour un usage interne documenté.

---

## 4. Checklist avant commit

```
☐ .Renviron absent de git                → git check-ignore -v .Renviron
☐ config/users.rds absent de git         → git check-ignore -v config/users.rds
☐ Aucune clé AIzaSy dans le code/docs    → grep -rn "AIzaSy" --include="*.R" --include="*.md"
☐ Aucun mot de passe en clair            → grep -rn "password_store(\"" --include="*.R"
☐ Tests et vérifications passent         → Rscript tools/run_tests.R
```

---

## 5. Fichiers de référence

| Fichier | Rôle |
|---------|------|
| `config_api_secure_v2.R` | Lecture de la clé, masquage, appels API |
| `auth_config.R` | Utilisateurs, rôles, permissions |
| `tools/manage_users.R` | Gestion des comptes |
| `.Renviron.example` | Modèle de configuration locale |
| `SECURITY_CHECKLIST.md` | Checklist opérationnelle |

**Dernière mise à jour :** novembre 2026
