# Extrait de app_parts/ui/ui_body.R — onglet import
ui_tab_import <- function() {
      tabItem(tabName="import",
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("magic"), " Import & Mappeur intelligent"),
              tabsetPanel(id = "import_tabs",

                # ── Onglet 1 : Import fichier local (existant) ──
                tabPanel(tags$span(icon("file-upload"), " Fichier local"),
                  tags$p(style="font-size:13px;color:#555;margin:12px 0;",
                    tags$span(class="badge-ok","🟢 Détecté"),
                    " automatiquement |",
                    tags$span(class="badge-warn","🟡 À vérifier"),
                    " |",
                    tags$span(class="badge-err","🔴 Non trouvé"),
                    " — cliquez sur une ligne pour corriger"
                  ),
                  fluidRow(
                    column(6, uiOutput("mapper_col1")),
                    column(6, uiOutput("mapper_col2"))
                  ),
                  # ── Vérification avec le dictionnaire OMS ──
                  tags$hr(style="margin:18px 0 10px;"),
                  tags$div(
                    style="display:flex;align-items:center;gap:10px;margin-bottom:10px;",
                    tags$div(
                      style="width:40px;height:40px;border-radius:10px;
                             background:linear-gradient(135deg,#0077B6,#00838F);
                             display:flex;align-items:center;justify-content:center;",
                      icon("book-open", style="color:white;font-size:18px;")
                    ),
                    tags$div(
                      tags$h5(style="margin:0;color:#1e293b;font-weight:700;font-size:14px;",
                              "Vérification avec le dictionnaire OMS"),
                      tags$p(style="margin:0;color:#64748b;font-size:12px;",
                             "Comparez vos colonnes avec les alias reconnus pour vérifier et corriger les équivalences.")
                    )
                  ),
                  DTOutput("dict_verification_table")
                ),

                # ── Onglet 2 : KoboToolbox API ──
                tabPanel(tags$span(icon("cloud-download-alt"), " KoboToolbox / ODK"),
                  tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin:12px 0;font-size:12px;",
                    tags$b("KoboToolbox / ODK Connect"), " — Connectez-vous directement à votre compte KoboToolbox ",
                    "pour importer les données d'enquête. Compatible avec les serveurs OCHA (humanitarianresponse.info) ",
                    "et KoboToolbox.org. ",
                    tags$a(href="https://support.kobotoolbox.org/api.html", target="_blank",
                           style="color:#0077B6;", "Comment obtenir votre token API ?")
                  ),
                  fluidRow(
                    column(4,
                      tags$div(style="padding:8px;",
                        tags$div(class="section-header", "Connexion"),
                        selectInput("kobo_server", "Serveur :",
                                    choices = KOBO_SERVERS, selected = KOBO_SERVERS[1], width = "100%"),
                        conditionalPanel("input.kobo_server == 'custom'",
                          textInput("kobo_custom_url", "URL personnalisée :",
                                    placeholder = "https://votre-serveur.com", width = "100%")
                        ),
                        passwordInput("kobo_token", "Token API :",
                                      placeholder = "Collez votre token ici", width = "100%"),
                        fluidRow(
                          column(6,
                            actionButton("kobo_test", "🔌 Tester",
                                         class="btn btn-info btn-block", width="100%",
                                         style="font-size:12px;")
                          ),
                          column(6,
                            actionButton("kobo_list_forms", "📋 Lister les formulaires",
                                         class="btn btn-primary btn-block", width="100%",
                                         style="font-size:12px;")
                          )
                        ),
                        uiOutput("kobo_connection_status"),
                        hr(),
                        tags$div(class="section-header", "Formulaire sélectionné"),
                        uiOutput("kobo_form_select_ui"),
                        actionButton("kobo_download", "📥 Télécharger les données",
                                     class="btn btn-oms btn-block", width="100%",
                                     style="font-size:13px;padding:8px;margin-top:8px;"),
                        hr(),
                        uiOutput("kobo_download_status")
                      )
                    ),
                    column(8,
                      tags$div(style="padding:8px;",
                        tags$div(class="section-header", "Formulaires disponibles"),
                        uiOutput("kobo_forms_list"),
                        hr(),
                        tags$div(class="section-header", "Mapping automatique ODK → SMART"),
                        uiOutput("kobo_mapping_html"),
                        hr(),
                        tags$div(class="section-header", "Aperçu des données"),
                        DTOutput("kobo_preview_table")
                      )
                    )
                  )
                ),

                # ── Onglet 3 : Import fichier ODK/Kobo export ──
                tabPanel(tags$span(icon("file-csv"), " Fichier ODK Export"),
                  tags$div(style="background:#f0f8ff;border-left:4px solid #27ae60;padding:10px 14px;border-radius:0 8px 8px 0;margin:12px 0;font-size:12px;",
                    tags$b("Import d'un export KoboToolbox/ODK"), " — Importez un fichier CSV ou Excel ",
                    "exporté depuis KoboToolbox. Les colonnes système seront automatiquement nettoyées ",
                    "et les variables SMART détectées."
                  ),
                  fluidRow(
                    column(4,
                      fileInput("kobo_file_upload", "Fichier d'export Kobo (CSV/Excel) :",
                                accept = c(".csv", ".xlsx", ".xls"),
                                buttonLabel = "Choisir", placeholder = "Aucun fichier",
                                width = "100%"),
                      actionButton("kobo_file_import", "📥 Importer et nettoyer",
                                   class="btn btn-oms btn-block", width="100%",
                                   style="font-size:13px;padding:8px;"),
                      hr(),
                      uiOutput("kobo_file_status"),
                      uiOutput("kobo_file_mapping")
                    ),
                    column(8,
                      DTOutput("kobo_file_preview")
                    )
                  )
                ),

                # ── Onglet 4 : Vérification conformité OMS ──
                tabPanel(tags$span(icon("check-double"), " Conformité OMS"),
                  value = "oms_conform",
                  tags$div(
                    style = "background:linear-gradient(135deg,#f0fff4 0%,#e8f8f0 100%);
                             border-left:4px solid #00838F;padding:14px 18px;
                             border-radius:0 8px 8px 0;margin:12px 0;",
                    tags$div(style="display:flex;align-items:center;gap:12px;",
                      tags$div(
                        style = "width:48px;height:48px;border-radius:12px;
                                 background:linear-gradient(135deg,#00838F,#0B3954);
                                 display:flex;align-items:center;justify-content:center;",
                        icon("stethoscope", style="color:white;font-size:22px;")
                      ),
                      tags$div(
                        tags$h4(style="margin:0;color:#1e293b;font-weight:700;font-size:16px;",
                                "Vérification de Conformité OMS / DHIS2"),
                        tags$p(style="margin:2px 0 0;color:#64748b;font-size:12px;",
                               "Analyse automatique de chaque colonne : détection des variables ",
                               "standards OMS, UNICEF, PAM, SMART/ENA et DHIS2. ",
                               "Corrigez les correspondances manuellement si nécessaire.")
                      )
                    )
                  ),
                  # Score global
                  uiOutput("oms_conform_score"),
                  # Tableau de conformité
                  tags$div(style="margin-top:12px;",
                    DTOutput("oms_conform_table")
                  ),
                  # Corrections manuelles
                  fluidRow(style="margin-top:16px;",
                    column(4,
                      selectInput("oms_fix_col", "Colonne à corriger :",
                                  choices = NULL, width = "100%")
                    ),
                    column(4,
                      selectInput("oms_fix_var", "Assigner à variable OMS :",
                                  choices = NULL, width = "100%")
                    ),
                    column(4,
                      tags$div(style="padding-top:25px;",
                        actionButton("oms_fix_apply",
                                     tags$span(icon("link"), " Assigner"),
                                     class="btn btn-oms",
                                     style="padding:8px 20px;font-size:13px;"),
                        actionButton("oms_fix_reset",
                                     tags$span(icon("undo"), " Réinitialiser"),
                                     class="btn btn-default",
                                     style="padding:8px 16px;font-size:13px;margin-left:8px;")
                      )
                    )
                  ),
                  # ── Panneau détail dictionnaire (apparaît au clic sur une ligne) ──
                  uiOutput("dict_selected_aliases")
                )
              )
          )
        ),
        fluidRow(
          box(width=8, status="info", solidHeader=TRUE,
              title="Aperçu des données",
              DTOutput("import_preview")),
          box(width=4, status="success", solidHeader=TRUE,
              title="Résumé de l'import",
              uiOutput("import_summary"),
              br(),
              actionButton("apply_map", "✅ Appliquer le mappeur",
                           class="btn btn-oms btn-block", width="100%")
          )
        )
      )
}
