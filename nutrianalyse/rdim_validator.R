# ============================================================================
# VALIDATEUR & FILTREUR VARIABLES POUR RÉDUCTION DE DIMENSION
# ============================================================================
# Assure que les variables sélectionnées correspondent à la méthode
# Filtre automatiquement les variables inappropriées
# ============================================================================

library(tidyverse)

# ==============================================================================
# 1. DÉTECTEURS DE TYPE DE VARIABLE
# ==============================================================================

#' Détecter si une variable est numérique
is_numeric_var <- function(x) {
  is.numeric(x) && !all(is.na(x))
}

#' Détecter si une variable est catégorique
is_categorical_var <- function(x) {
  (is.factor(x) || is.character(x)) && !all(is.na(x))
}

#' Détecter si une variable est logique (traiter comme catégorique)
is_logical_var <- function(x) {
  is.logical(x) && !all(is.na(x))
}

#' Obtenir le type de variable (simplifié)
get_var_type <- function(x) {
  if (is_numeric_var(x)) return("numeric")
  if (is_categorical_var(x) || is_logical_var(x)) return("categorical")
  if (all(is.na(x))) return("empty")
  return("unknown")
}

# ==============================================================================
# 2. CLASSIFICATION DES VARIABLES DANS UN DATA FRAME
# ==============================================================================

#' Classifier toutes les variables d'un data frame
classify_variables <- function(df) {
  
  classification <- sapply(df, get_var_type)
  
  list(
    numeric = names(classification)[classification == "numeric"],
    categorical = names(classification)[classification == "categorical"],
    empty = names(classification)[classification == "empty"],
    all_numeric = all(classification %in% c("numeric", "empty")),
    all_categorical = all(classification %in% c("categorical", "empty")),
    is_mixed = !all(classification %in% c("numeric", "empty")) &&
               !all(classification %in% c("categorical", "empty"))
  )
}

# ==============================================================================
# 3. VALIDATEURS PAR MÉTHODE
# ==============================================================================

#' Valider ACP (variables numériques uniquement)
validate_pca <- function(df, vars) {
  
  if (is.null(vars) || length(vars) == 0) {
    return(list(valid = FALSE, error = "Aucune variable sélectionnée"))
  }
  
  vars_in_df <- intersect(vars, names(df))
  
  if (length(vars_in_df) < 2) {
    return(list(valid = FALSE, error = "Sélectionnez au moins 2 variables"))
  }
  
  # Extraire les variables
  d_sub <- df[, vars_in_df, drop = FALSE]
  
  # Vérifier qu'elles sont TOUTES numériques
  numeric_vars <- names(which(sapply(d_sub, is_numeric_var)))
  
  if (length(numeric_vars) < 2) {
    non_numeric <- setdiff(vars_in_df, numeric_vars)
    return(list(
      valid = FALSE,
      error = sprintf(
        "ACP nécessite des variables NUMÉRIQUES uniquement.\n❌ Problème: %s\n✅ Utilisez: %s",
        paste(non_numeric, collapse = ", "),
        ifelse(length(numeric_vars) > 0, 
               paste("Variables numériques disponibles:", paste(numeric_vars, collapse = ", ")),
               "Aucune variable numérique disponible")
      )
    ))
  }
  
  # Vérifier après suppression des NA
  d_complete <- d_sub[, numeric_vars, drop = FALSE] |> drop_na()
  
  if (nrow(d_complete) < 5) {
    return(list(
      valid = FALSE,
      error = sprintf(
        "Trop peu d'observations complètes (%d) après suppression des NA.\nMin requis: 5",
        nrow(d_complete)
      )
    ))
  }
  
  return(list(
    valid = TRUE,
    error = NULL,
    vars_clean = numeric_vars,
    n_obs = nrow(d_complete),
    n_vars = length(numeric_vars)
  ))
}

#' Valider ACM (variables catégorielles uniquement)
validate_mca <- function(df, vars) {
  
  if (is.null(vars) || length(vars) == 0) {
    return(list(valid = FALSE, error = "Aucune variable sélectionnée"))
  }
  
  vars_in_df <- intersect(vars, names(df))
  
  if (length(vars_in_df) < 2) {
    return(list(valid = FALSE, error = "Sélectionnez au moins 2 variables catégorielles"))
  }
  
  # Extraire et convertir en facteur
  d_sub <- df[, vars_in_df, drop = FALSE] |>
    mutate(across(everything(), function(x) {
      if (is.numeric(x)) return(NA)  # Exclure numériques
      if (is.logical(x)) x <- factor(x)
      else if (is.character(x)) x <- factor(x)
      x
    }))
  
  # Garder que les colonnes qui restent des facteurs
  cat_vars <- names(which(sapply(d_sub, is.factor)))
  
  if (length(cat_vars) < 2) {
    non_cat <- setdiff(vars_in_df, cat_vars)
    return(list(
      valid = FALSE,
      error = sprintf(
        "ACM nécessite des variables CATÉGORIELLES uniquement.\n❌ Problème: %s (numériques)\n✅ Utilisez des facteurs/caractères",
        paste(non_cat, collapse = ", ")
      )
    ))
  }
  
  # Vérifier après suppression des NA
  d_complete <- d_sub[, cat_vars, drop = FALSE] |> drop_na()
  
  if (nrow(d_complete) < 5) {
    return(list(
      valid = FALSE,
      error = sprintf(
        "Trop peu d'observations complètes (%d) après suppression des NA.\nMin requis: 5",
        nrow(d_complete)
      )
    ))
  }
  
  return(list(
    valid = TRUE,
    error = NULL,
    vars_clean = cat_vars,
    n_obs = nrow(d_complete),
    n_vars = length(cat_vars)
  ))
}

#' Valider FAMD (variables mixtes)
validate_famd <- function(df, vars) {
  
  if (is.null(vars) || length(vars) == 0) {
    return(list(valid = FALSE, error = "Aucune variable sélectionnée"))
  }
  
  vars_in_df <- intersect(vars, names(df))
  
  if (length(vars_in_df) < 2) {
    return(list(valid = FALSE, error = "Sélectionnez au moins 2 variables"))
  }
  
  # Extraire et préparer
  d_sub <- df[, vars_in_df, drop = FALSE] |>
    mutate(across(where(is.character), as.factor))
  
  # Compter types
  n_num <- sum(sapply(d_sub, is.numeric))
  n_cat <- sum(sapply(d_sub, is.factor))
  
  if (n_num < 1 || n_cat < 1) {
    return(list(
      valid = FALSE,
      error = sprintf(
        "FAMD nécessite des variables MIXTES (numériques + catégorielles).\nTrouvé: %d numériques, %d catégorielles.\nMin requis: 1 de chaque type",
        n_num, n_cat
      )
    ))
  }
  
  # Vérifier après suppression des NA
  d_complete <- d_sub |> drop_na()
  
  if (nrow(d_complete) < 5) {
    return(list(
      valid = FALSE,
      error = sprintf(
        "Trop peu d'observations complètes (%d) après suppression des NA.\nMin requis: 5",
        nrow(d_complete)
      )
    ))
  }
  
  return(list(
    valid = TRUE,
    error = NULL,
    vars_clean = names(d_sub),
    n_obs = nrow(d_complete),
    n_vars = length(vars_in_df),
    n_numeric = n_num,
    n_categorical = n_cat
  ))
}

#' Valider AFC (2 variables catégorielles)
validate_ca <- function(df, var1, var2) {
  
  if (is.null(var1) || is.null(var2)) {
    return(list(valid = FALSE, error = "Sélectionnez 2 variables"))
  }
  
  if (!var1 %in% names(df) || !var2 %in% names(df)) {
    return(list(valid = FALSE, error = "Variables non trouvées dans les données"))
  }
  
  # Vérifier catégorielles
  if (!is_categorical_var(df[[var1]]) || !is_categorical_var(df[[var2]])) {
    return(list(
      valid = FALSE,
      error = "AFC nécessite 2 variables CATÉGORIELLES"
    ))
  }
  
  # Créer tableau
  tbl <- table(df[[var1]], df[[var2]])
  
  if (nrow(tbl) < 2 || ncol(tbl) < 2) {
    return(list(
      valid = FALSE,
      error = sprintf(
        "Tableau de contingence insuffisant (%d×%d).\nMin requis: 2×2",
        nrow(tbl), ncol(tbl)
      )
    ))
  }
  
  return(list(
    valid = TRUE,
    error = NULL,
    n_rows = nrow(tbl),
    n_cols = ncol(tbl),
    table_dim = sprintf("%d × %d", nrow(tbl), ncol(tbl))
  ))
}

# ==============================================================================
# 4. FILTREUR AUTOMATIQUE DE VARIABLES PAR MÉTHODE
# ==============================================================================

#' Filtrer automatiquement les variables selon la méthode choisie
auto_filter_vars <- function(df, method, selected_vars = NULL) {
  
  class_result <- classify_variables(df)
  
  switch(method,
    "pca" = {
      # ACP : garder que les numériques
      available <- class_result$numeric
      if (length(available) == 0) {
        list(vars = NULL, message = "❌ Aucune variable numérique disponible pour ACP")
      } else {
        list(
          vars = available,
          message = sprintf("✅ ACP: %d variables numériques disponibles", length(available))
        )
      }
    },
    
    "mca" = {
      # ACM : garder que les catégorielles
      available <- class_result$categorical
      if (length(available) == 0) {
        list(vars = NULL, message = "❌ Aucune variable catégorielle disponible pour ACM")
      } else {
        list(
          vars = available,
          message = sprintf("✅ ACM: %d variables catégorielles disponibles", length(available))
        )
      }
    },
    
    "famd" = {
      # FAMD : accepter mix
      num <- class_result$numeric
      cat <- class_result$categorical
      if (length(num) == 0 || length(cat) == 0) {
        list(
          vars = NULL,
          message = sprintf("❌ FAMD: Besoin mix (numériques: %d, catégorielles: %d)",
                           length(num), length(cat))
        )
      } else {
        list(
          vars = c(num, cat),
          message = sprintf("✅ FAMD: %d numériques + %d catégorielles", length(num), length(cat))
        )
      }
    },
    
    "ca" = {
      # AFC : garder que catégorielles (pour sélection dans UI)
      available <- class_result$categorical
      if (length(available) < 2) {
        list(vars = NULL, message = "❌ AFC: Min 2 variables catégorielles")
      } else {
        list(
          vars = available,
          message = sprintf("✅ AFC: %d variables catégorielles", length(available))
        )
      }
    },
    
    "auto" = {
      # Auto : accepter tout, décision sera prise ultérieurement
      all_vars <- c(class_result$numeric, class_result$categorical)
      if (length(all_vars) < 2) {
        list(vars = NULL, message = "❌ Min 2 variables pour tout type d'analyse")
      } else {
        list(
          vars = all_vars,
          message = sprintf("✅ Auto: %d variables (sera déterminée par type)",
                           length(all_vars))
        )
      }
    },
    
    # Défaut
    list(vars = NULL, message = "Méthode non reconnue")
  )
}

# ==============================================================================
# 5. VALIDATEUR GLOBAL (une seule fonction)
# ==============================================================================

#' Valider complètement la sélection pour une méthode donnée
validate_rdim <- function(df, method, vars = NULL, var1 = NULL, var2 = NULL) {
  
  # Pré-validation : données valides ?
  if (is.null(df) || nrow(df) < 5) {
    return(list(valid = FALSE, error = "Données insuffisantes (min 5 lignes)"))
  }
  
  # Valider selon la méthode
  switch(method,
    "pca" = validate_pca(df, vars),
    "mca" = validate_mca(df, vars),
    "famd" = validate_famd(df, vars),
    "ca" = validate_ca(df, var1, var2),
    "auto" = {
      # Auto : déduire la méthode et valider
      class_result <- classify_variables(df)
      
      if (class_result$all_numeric) {
        actual_method <- "pca"
        validate_pca(df, vars)
      } else if (class_result$all_categorical) {
        actual_method <- "mca"
        validate_mca(df, vars)
      } else if (class_result$is_mixed) {
        actual_method <- "famd"
        validate_famd(df, vars)
      } else {
        list(valid = FALSE, error = "Impossible déduire la méthode")
      }
    },
    
    # Défaut
    list(valid = FALSE, error = "Méthode non reconnue")
  )
}

# ==============================================================================
# 6. MESSAGE D'ERREUR UTILISATEUR-FRIENDLY
# ==============================================================================

#' Générer message d'erreur formaté pour Shiny
format_rdim_error <- function(validation_result) {
  
  if (validation_result$valid) {
    return(NULL)
  }
  
  error_text <- validation_result$error
  
  # Enrober dans une notif Shiny colorée
  HTML(sprintf(
    '<div style="color: #c9161d; background: #ffebee; padding: 12px; border-radius: 4px; border-left: 4px solid #c9161d;">
      <strong>⚠️ Erreur:</strong><br/>
      %s
    </div>',
    error_text
  ))
}
