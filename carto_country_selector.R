# ============================================================================
# SÉLECTEUR GÉOGRAPHIQUE HIÉRARCHIQUE : Pays → Région → Département → Arrondissement
# ============================================================================
# Module Shiny pour sélection cascade de zones géographiques
# Support mondial : n'importe quel pays du monde avec GADM
# ============================================================================

# Helper : TRUE si l'input est non-NULL, longueur > 0 et non vide
# Toujours scalaire (évite les erreurs if(NA) / if(logical(0)) de Shiny)
.has_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  x <- x[[1]]
  if (length(x) != 1 || is.na(x)) return(FALSE)
  nzchar(as.character(x))
}

# ==============================================================================
# 1. UI MODULE
# ==============================================================================

carto_country_selector_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    tags$div(
      style = "background: linear-gradient(90deg, #00426A 0%, #008DC9 100%); 
               color: white; padding: 14px; border-radius: 6px; margin-bottom: 14px;",
      tags$h4(style = "margin: 0; display: flex; align-items: center; gap: 8px;",
              tags$span("⚙️"), "Configuration Géographique"),
      tags$small(style = "opacity: 0.9; display: block; margin-top: 4px;",
                 "Sélectionnez votre pays et niveaux administratifs")
    ),
    
    # ==== NIVEAU 0 : CONTINENT ====
    tags$div(
      style = "margin-bottom: 12px;",
      tags$label(style = "font-weight: 600; color: #00426A; display: block; margin-bottom: 4px;",
                 "1️⃣  Continent"),
      selectInput(
        ns("continent"),
        label = NULL,
        choices = c("-- Sélectionner un continent --" = ""),
        width = "100%",
        selectize = FALSE
      ),
      tags$small(style = "color: #666;", "Commencez par choisir votre continent")
    ),
    
    # ==== NIVEAU 1 : PAYS ====
    tags$div(
      style = "margin-bottom: 12px;",
      tags$label(style = "font-weight: 600; color: #00426A; display: block; margin-bottom: 4px;",
                 "2️⃣  Pays"),
      uiOutput(ns("country_ui")),
      tags$small(style = "color: #666;", "Puis choisissez votre pays")
    ),
    
    # ==== NIVEAU 2 : RÉGION ====
    tags$div(
      style = "margin-bottom: 12px;",
      tags$label(style = "font-weight: 600; color: #008DC9; display: block; margin-bottom: 4px;",
                 "3️⃣  Région / Province"),
      uiOutput(ns("region_ui")),
      tags$small(style = "color: #666;", "Sélectionnez une région (GADM L1)")
    ),
    
    # ==== NIVEAU 3 : DÉPARTEMENT ====
    tags$div(
      style = "margin-bottom: 12px;",
      tags$label(style = "font-weight: 600; color: #008DC9; display: block; margin-bottom: 4px;",
                 "4️⃣  Département / District"),
      uiOutput(ns("department_ui")),
      tags$small(style = "color: #666;", "Sélectionnez un département (GADM L2)")
    ),
    
    # ==== NIVEAU 4 : ARRONDISSEMENT (Optionnel) ====
    tags$div(
      style = "margin-bottom: 12px;",
      tags$label(style = "font-weight: 600; color: #1D9E75; display: block; margin-bottom: 4px;",
                 "5️⃣  Arrondissement (Optionnel)"),
      uiOutput(ns("district_ui")),
      tags$small(style = "color: #666;", "Affinage géographique supplémentaire (GADM L3)")
    ),
    
    # ==== AFFICHAGE SÉLECTION COURANTE ====
    tags$div(
      style = "background: #e8f5e9; padding: 12px; border-radius: 6px; border-left: 4px solid #4dac26; margin-bottom: 12px;",
      uiOutput(ns("selection_display"))
    ),
    
    # ==== BOUTONS D'ACTION ====
    fluidRow(
      column(
        6,
        actionButton(
          ns("load_maps"),
          "📥 Charger les cartes",
          class = "btn btn-oms",
          width = "100%",
          style = "background: #008DC9; border: none; color: white; font-weight: 600; padding: 10px;"
        )
      ),
      column(
        6,
        actionButton(
          ns("clear_selection"),
          "❌ Réinitialiser",
          class = "btn btn-default",
          width = "100%"
        )
      )
    ),
    
    # ==== BARRE DE PROGRESSION TÉLÉCHARGEMENT ====
    uiOutput(ns("progress_ui")),
    
    # ==== STATUT ====
    uiOutput(ns("status_message"))
  )
}

# ==============================================================================
# 2. SERVER MODULE
# ==============================================================================

carto_country_selector_Server <- function(id, default_country = "CMR") {
  moduleServer(id, function(input, output, session) {
    
    # ========== STATE : données GADM chargées ==========
    gadm_data <- reactiveValues(
      l1 = NULL,   # régions
      l2 = NULL,   # départements
      l3 = NULL,   # arrondissements
      loaded_country = NULL,
      load_error = NULL
    )
    
    # ========== RÉACTIFS DE BASE (pas de GADM ici) ==========
    
    continents <- reactive({ list_continents() })
    
    available_countries <- reactive({
      if (!.has_value(input$continent)) return(NULL)
      # list_countries_by_continent retourne c("Nom pays" = "ISO3", ...)
      list_countries_by_continent(input$continent)
    })
    
    # Régions (L1) — utilise gadm_name_col() pour robustesse
    available_regions <- reactive({
      if (is.null(gadm_data$l1)) return(NULL)
      col <- gadm_name_col(gadm_data$l1, 1)
      if (is.null(col)) return(NULL)
      nms <- sort(unique(gadm_data$l1[[col]]))
      nms <- nms[!is.na(nms) & nchar(nms) > 0]
      setNames(nms, nms)
    })

    available_departments <- reactive({
      if (is.null(gadm_data$l2) || !.has_value(input$region)) return(NULL)
      col1 <- gadm_name_col(gadm_data$l2, 1)
      col2 <- gadm_name_col(gadm_data$l2, 2)
      if (is.null(col1) || is.null(col2)) return(NULL)
      filtered   <- gadm_data$l2[gadm_data$l2[[col1]] == input$region, ]
      dept_names <- sort(unique(filtered[[col2]]))
      dept_names <- dept_names[!is.na(dept_names) & nchar(dept_names) > 0]
      setNames(dept_names, dept_names)
    })

    available_districts <- reactive({
      if (is.null(gadm_data$l3) || !.has_value(input$department)) return(NULL)
      col2 <- gadm_name_col(gadm_data$l3, 2)
      col3 <- gadm_name_col(gadm_data$l3, 3)
      if (is.null(col2) || is.null(col3)) return(NULL)
      filtered       <- gadm_data$l3[gadm_data$l3[[col2]] == input$department, ]
      district_names <- sort(unique(filtered[[col3]]))
      district_names <- district_names[!is.na(district_names) & nchar(district_names) > 0]
      setNames(district_names, district_names)
    })
    
    # ========== CHARGER GADM au clic sur "Charger les cartes" ==========
    
    observeEvent(input$load_maps, {
      if (!.has_value(input$country)) {
        showNotification("⚠️ Sélectionnez d'abord un pays.", type = "warning")
        return()
      }
      
      country <- input$country
      
      # Barre de progression
      output$progress_ui <- renderUI({
        tags$div(
          style = "margin-top: 12px;",
          tags$div(style = "margin-bottom: 6px;",
                   tags$strong(sprintf("⬇️ Chargement GADM %s...", get_country_name(country)))),
          tags$div(class = "progress", style = "height: 20px; border-radius: 4px;",
            tags$div(class = "progress-bar progress-bar-striped active",
                     role = "progressbar", style = "width: 100%; background: #008DC9;"))
        )
      })
      
      gadm_data$load_error <- NULL
      gadm_data$l1 <- NULL
      gadm_data$l2 <- NULL
      gadm_data$l3 <- NULL

      # Vérifier le niveau max configuré pour ce pays
      max_lvl <- get_available_levels(country)
      cfg     <- GADM_WORLDWIDE[[country]]

      # ── Charger L1 (régions) — obligatoire ──────────────────────────────────
      tryCatch({
        gadm_data$l1 <- load_gadm_level(country, level = 1)
      }, error = function(e) {
        gadm_data$load_error <- paste("Régions (L1):", e$message)
      })

      # ── Charger L2 (départements) — si pays a au moins L2 ───────────────────
      if (max_lvl >= 2 && !is.null(gadm_data$l1)) {
        tryCatch({
          gadm_data$l2 <- load_gadm_level(country, level = 2)
        }, error = function(e) {
          # L2 absent : pas une erreur fatale
          gadm_data$l2 <- NULL
        })
      }

      # ── Charger L3 (arrondissements) — si pays a au moins L3 ────────────────
      if (max_lvl >= 3 && !is.null(gadm_data$l1)) {
        tryCatch({
          gadm_data$l3 <- load_gadm_level(country, level = 3)
        }, error = function(e) {
          gadm_data$l3 <- NULL
        })
      }

      gadm_data$loaded_country <- country
      output$progress_ui <- renderUI(NULL)

      if (is.null(gadm_data$l1)) {
        # Échec critique : même L1 n'a pas pu être chargé
        output$status_message <- renderUI({
          tags$div(
            style = "background:#ffebee;color:#c62828;padding:10px;border-radius:6px;
                     border-left:4px solid #c62828;margin-top:12px;",
            tags$strong("❌ Échec du chargement"),
            tags$br(),
            tags$small(gadm_data$load_error %||% "Connexion internet requise."),
            tags$br(), tags$br(),
            tags$small(style = "color:#555;",
              "💡 Vérifiez votre connexion et ré-essayez. ",
              "Les données seront mises en cache pour une utilisation hors-ligne.")
          )
        })
        showNotification("❌ Impossible de charger les données GADM. Connexion requise.",
                         type = "error", duration = 8)
      } else {
        # Succès (partiel ou total)
        n_reg   <- if (!is.null(gadm_data$l1)) {
          col1 <- gadm_name_col(gadm_data$l1, 1)
          if (!is.null(col1)) length(unique(gadm_data$l1[[col1]])) else 0
        } else 0

        n_dept  <- if (!is.null(gadm_data$l2)) {
          col2 <- gadm_name_col(gadm_data$l2, 2)
          if (!is.null(col2)) length(unique(gadm_data$l2[[col2]])) else 0
        } else 0

        n_arr   <- if (!is.null(gadm_data$l3)) {
          col3 <- gadm_name_col(gadm_data$l3, 3)
          if (!is.null(col3)) length(unique(gadm_data$l3[[col3]])) else 0
        } else 0

        detail <- paste0(
          n_reg, " ", cfg$level_names["L1"],
          if (n_dept > 0) paste0(", ", n_dept, " ", cfg$level_names["L2"]) else "",
          if (n_arr  > 0) paste0(", ", n_arr,  " ", cfg$level_names["L3"]) else ""
        )

        output$status_message <- renderUI({
          tags$div(
            style = "background:#c8e6c9;color:#2e7d32;padding:10px;border-radius:6px;
                     border-left:4px solid #4dac26;margin-top:12px;",
            tags$strong("✅ Cartes chargées !"),
            tags$br(),
            tags$small(paste0(get_country_name(country), " — ", detail))
          )
        })
        showNotification(
          sprintf("✅ %s : %s", get_country_name(country), detail),
          type = "message", duration = 5
        )
      }
    })
    
    # ========== INITIALISATION CONTINENTS ==========
    
    observe({
      choices_cont <- c("-- Sélectionner un continent --" = "", continents())
      updateSelectInput(session, "continent", choices = choices_cont)
    })
    
    # ========== RENDER PAYS ==========
    
    output$country_ui <- renderUI({
      countries <- available_countries()
      if (is.null(countries) || length(countries) == 0) {
        selectInput(session$ns("country"), label = NULL,
                    choices = c("-- D'abord choisir un continent --" = ""),
                    width = "100%", selectize = FALSE)
      } else {
        selectInput(session$ns("country"), label = NULL,
                    choices = c("-- Sélectionner un pays --" = "", countries),
                    width = "100%", selectize = FALSE)
      }
    })
    
    # Helper : noms de niveaux localisés depuis le config (ou fallback générique)
    level_label <- function(country_code, n) {
      fallback <- c("1"="Région","2"="Département","3"="Arrondissement")[as.character(n)]
      if (!.has_value(country_code)) return(fallback)
      cfg <- GADM_WORLDWIDE[[country_code]]
      if (is.null(cfg)) return(fallback)
      nms <- cfg$level_names
      key <- paste0("L", n)
      if (!is.null(nms) && key %in% names(nms)) nms[[key]] else fallback
    }

    # ========== RENDER RÉGION ==========

    output$region_ui <- renderUI({
      regions <- available_regions()
      lbl <- if (.has_value(input$country)) level_label(input$country, 1) else "Région"

      if (is.null(regions) || length(regions) == 0) {
        msg <- if (!.has_value(input$country)) {
          "-- D'abord choisir un pays --"
        } else if (is.null(gadm_data$l1)) {
          paste0("-- Cliquez 'Charger les cartes' --")
        } else {
          paste0("-- Aucune ", lbl, " disponible --")
        }
        selectInput(session$ns("region"), label = NULL,
                    choices = c(setNames("", msg)), width = "100%", selectize = FALSE)
      } else {
        selectInput(session$ns("region"), label = NULL,
                    choices = c(setNames("", paste0("-- Sélectionner une ", lbl, " --")), regions),
                    width = "100%", selectize = FALSE)
      }
    })

    # ========== RENDER DÉPARTEMENT ==========

    output$department_ui <- renderUI({
      depts <- available_departments()
      lbl <- if (.has_value(input$country)) level_label(input$country, 2) else "Département"

      if (is.null(depts) || length(depts) == 0) {
        msg <- if (!.has_value(input$region)) {
          paste0("-- D'abord choisir une ", level_label(input$country, 1), " --")
        } else if (is.null(gadm_data$l2)) {
          paste0("-- Non disponible pour ce pays --")
        } else {
          paste0("-- Aucun(e) ", lbl, " --")
        }
        selectInput(session$ns("department"), label = NULL,
                    choices = c(setNames("", msg)), width = "100%", selectize = FALSE)
      } else {
        selectInput(session$ns("department"), label = NULL,
                    choices = c(setNames("", paste0("-- Sélectionner un(e) ", lbl, " --")), depts),
                    width = "100%", selectize = FALSE)
      }
    })

    # ========== RENDER ARRONDISSEMENT ==========

    output$district_ui <- renderUI({
      districts <- available_districts()
      lbl <- if (.has_value(input$country)) level_label(input$country, 3) else "Arrondissement"

      if (is.null(districts) || length(districts) == 0) {
        msg <- if (is.null(gadm_data$l3)) {
          "-- Non disponible pour ce pays --"
        } else if (!.has_value(input$department)) {
          paste0("-- D'abord choisir un(e) ", level_label(input$country, 2), " --")
        } else {
          "-- Optionnel --"
        }
        selectInput(session$ns("district"), label = NULL,
                    choices = c(setNames("", msg)), width = "100%", selectize = FALSE)
      } else {
        selectInput(session$ns("district"), label = NULL,
                    choices = c(setNames("", paste0("-- Sélectionner un(e) ", lbl, " (opt.) --")), districts),
                    width = "100%", selectize = FALSE)
      }
    })
    
    # ========== AFFICHAGE SÉLECTION COURANTE ==========
    
    output$selection_display <- renderUI({
      continent_name <- if (.has_value(input$continent)) input$continent else "---"
      country_name <- if (.has_value(input$country)) get_country_name(input$country) else "---"
      region_name <- if (.has_value(input$region)) input$region else "---"
      dept_name <- if (.has_value(input$department)) input$department else "---"
      dist_name <- if (.has_value(input$district)) input$district else "---"
      
      has_dist <- .has_value(input$district)
      
      tagList(
        tags$strong(style = "color: #00426A; font-size: 14px; display: block; margin-bottom: 6px;",
                    "✅ Sélection courante :"),
        tags$div(
          style = "font-size: 13px; line-height: 1.6; color: #333;",
          tags$span(style = "color: #666;", "📍 "),
          tags$strong(continent_name), " → ",
          tags$strong(country_name), " → ",
          tags$strong(region_name), " → ",
          tags$strong(dept_name),
          if (has_dist) {
            tags$span(style = "color: #666;", " → ")
          } else {
            tags$span(style = "color: #999;", " → ")
          },
          tags$span(style = if (has_dist) "color: #333;" else "color: #ccc;",
                    dist_name)
        )
      )
    })
    
    # ========== BOUTON : RÉINITIALISER ==========
    
    observeEvent(input$clear_selection, {
      gadm_data$l1 <- NULL
      gadm_data$l2 <- NULL
      gadm_data$l3 <- NULL
      gadm_data$loaded_country <- NULL
      gadm_data$load_error <- NULL
      
      updateSelectInput(session, "continent", selected = "")
      updateSelectInput(session, "country", selected = "")
      updateSelectInput(session, "region", selected = "")
      updateSelectInput(session, "department", selected = "")
      updateSelectInput(session, "district", selected = "")
      
      output$progress_ui <- renderUI(NULL)
      output$status_message <- renderUI(NULL)
      
      showNotification("🔄 Sélection réinitialisée", type = "message", duration = 3)
    })
    
    # ========== RETOUR : RÉACTIF SÉLECTION GLOBALE ==========
    
    # Exporter la sélection pour usage dans app principale
    selected_config <- reactive({
      list(
        continent = input$continent,
        country_code = input$country,
        country_name = if (.has_value(input$country)) get_country_name(input$country) else NA,
        region = input$region,
        department = input$department,
        district = if (.has_value(input$district) && input$district != "---") input$district else NA,
        is_complete = (.has_value(input$country) && .has_value(input$region) && .has_value(input$department))
      )
    })
    
    return(selected_config)
  })
}

# ==============================================================================
# 3. NOTE : les fonctions utilitaires (list_continents, list_countries_by_continent,
#    get_country_name, get_gadm_url, is_country_supported, get_available_levels)
#    sont définies dans config_gadm_worldwide.R — sourced avant ce fichier.
# ==============================================================================
