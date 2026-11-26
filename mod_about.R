# Module Shiny — Onglet À propos (contenu statique, aucun input/output)
# Aucun serveur n'est nécessaire : l'onglet ne contient aucun identifiant.

mod_about_ui <- function(id) {
  ns <- shiny::NS(id)

      tabItem(tabName="about",
        tags$div(style="max-width:960px;margin:0 auto;",

          # Hero banner
          tags$div(
            style="background:linear-gradient(135deg,#0077B6 0%,#00B4D8 50%,#48CAE4 100%);
                   color:white;padding:48px 40px;border-radius:16px;margin-bottom:28px;
                   box-shadow:0 8px 32px rgba(0,119,182,.25);position:relative;overflow:hidden;",
            # Decorative circles
            tags$div(style="position:absolute;top:-40px;right:-40px;width:180px;height:180px;
                           border-radius:50%;background:rgba(255,255,255,.06);"),
            tags$div(style="position:absolute;bottom:-60px;left:30%;width:240px;height:240px;
                           border-radius:50%;background:rgba(255,255,255,.04);"),
            fluidRow(
              column(8,
                tags$div(
                  style="display:inline-flex;align-items:center;justify-content:center;
                         width:64px;height:64px;border-radius:16px;margin-bottom:16px;
                         background:rgba(255,255,255,.15);backdrop-filter:blur(10px);
                         font-size:28px;font-weight:900;color:white;letter-spacing:-1px;
                         box-shadow:0 4px 12px rgba(0,0,0,.15);",
                  "DS"
                ),
                tags$h1(style="margin:0;font-weight:800;font-size:32px;letter-spacing:-.5px;",
                         "DSAMID"),
                tags$p(style="margin:6px 0 0;font-size:18px;opacity:.9;font-weight:300;",
                       "Data Science & Analysis — Machine Intelligence for Decisions"),
                tags$p(style="margin:16px 0 0;font-size:14px;opacity:.75;line-height:1.7;max-width:520px;",
                       "Un collectif de chercheurs et praticiens passionnés par la science des données, ",
                       "unis par la conviction que des décisions meilleures naissent de données mieux comprises.")
              ),
              column(4, style="text-align:right;padding-top:20px;",
                tags$div(style="font-size:100px;opacity:.12;font-weight:900;line-height:1;letter-spacing:-4px;",
                         "DS")
              )
            )
          ),

          # Mission + Vision
          fluidRow(
            column(6,
              tags$div(
                style="background:white;border-radius:12px;padding:28px;margin-bottom:20px;
                       box-shadow:0 2px 12px rgba(0,0,0,.06);border-left:4px solid #0077B6;
                       min-height:260px;",
                tags$div(style="display:flex;align-items:center;gap:10px;margin-bottom:14px;",
                  tags$div(style="width:40px;height:40px;border-radius:10px;display:flex;
                                  align-items:center;justify-content:center;
                                  background:linear-gradient(135deg,#0077B6,#00B4D8);
                                  color:white;font-size:18px;",
                           icon("bullseye")),
                  tags$h3(style="margin:0;font-weight:700;color:#0077B6;font-size:18px;",
                           "Notre mission")
                ),
                tags$p(style="color:#444;line-height:1.8;font-size:13.5px;",
                  "Transformer les données brutes en connaissances actionnables. ",
                  "Nous développons des outils open-source de niveau professionnel qui rendent ",
                  "l'analyse statistique avancée accessible aux praticiens de terrain — ",
                  "nutritionnistes, épidémiologistes, humanitaires — sans compromis sur la rigueur scientifique."
                ),
                tags$p(style="color:#444;line-height:1.8;font-size:13.5px;",
                  "Chaque algorithme implémenté dans NutriAnalyse Pro respecte les méthodologies ",
                  "standardisées (SMART, ENA, standards OMS 2006) tout en les rendant intuitives."
                )
              )
            ),
            column(6,
              tags$div(
                style="background:white;border-radius:12px;padding:28px;margin-bottom:20px;
                       box-shadow:0 2px 12px rgba(0,0,0,.06);border-left:4px solid #48CAE4;
                       min-height:260px;",
                tags$div(style="display:flex;align-items:center;gap:10px;margin-bottom:14px;",
                  tags$div(style="width:40px;height:40px;border-radius:10px;display:flex;
                                  align-items:center;justify-content:center;
                                  background:linear-gradient(135deg,#48CAE4,#90E0EF);
                                  color:white;font-size:18px;",
                           icon("eye")),
                  tags$h3(style="margin:0;font-weight:700;color:#0096C7;font-size:18px;",
                           "Notre vision")
                ),
                tags$p(style="color:#444;line-height:1.8;font-size:13.5px;",
                  "Un monde où la qualité de l'analyse ne dépend plus de la taille du budget. ",
                  "Où un nutritionniste au Tchad dispose des mêmes outils analytiques qu'un ",
                  "chercheur à Genève. Où les décisions qui sauvent des vies sont fondées sur ",
                  "des preuves statistiques solides, pas sur l'intuition seule."
                ),
                tags$p(style="color:#444;line-height:1.8;font-size:13.5px;",
                  "Nous croyons à la démocratisation de la data science — puissante, ",
                  "mais jamais élitiste."
                )
              )
            )
          ),

          # Expertise pillars
          tags$div(
            style="background:white;border-radius:12px;padding:28px 28px 16px;margin-bottom:20px;
                   box-shadow:0 2px 12px rgba(0,0,0,.06);",
            tags$h3(style="margin:0 0 20px;font-weight:700;color:#0B3954;font-size:18px;text-align:center;",
                     icon("layer-group"), "  Nos piliers d'expertise"),
            fluidRow(
              column(3,
                tags$div(style="text-align:center;padding:16px 10px;",
                  tags$div(style="width:56px;height:56px;border-radius:14px;margin:0 auto 12px;
                                  display:flex;align-items:center;justify-content:center;
                                  background:linear-gradient(135deg,#0077B6,#00B4D8);
                                  color:white;font-size:24px;box-shadow:0 4px 12px rgba(0,119,182,.3);",
                           icon("chart-bar")),
                  tags$h4(style="font-weight:700;color:#0B3954;font-size:14px;margin:0 0 6px;",
                           "Data Visualisation"),
                  tags$p(style="font-size:12px;color:#666;line-height:1.6;",
                    "Cartographies interactives, tableaux de bord exécutifs, ",
                    "graphiques publication-ready. Rendre le complexe lisible.")
                )
              ),
              column(3,
                tags$div(style="text-align:center;padding:16px 10px;",
                  tags$div(style="width:56px;height:56px;border-radius:14px;margin:0 auto 12px;
                                  display:flex;align-items:center;justify-content:center;
                                  background:linear-gradient(135deg,#48CAE4,#90E0EF);
                                  color:white;font-size:24px;box-shadow:0 4px 12px rgba(72,202,228,.3);",
                           icon("flask")),
                  tags$h4(style="font-weight:700;color:#0B3954;font-size:14px;margin:0 0 6px;",
                           "Statistique Avancée"),
                  tags$p(style="font-size:12px;color:#666;line-height:1.6;",
                    "Tests inférentiels, analyse de survie, épidémiologie. ",
                    "La rigueur méthodologique au service du terrain.")
                )
              ),
              column(3,
                tags$div(style="text-align:center;padding:16px 10px;",
                  tags$div(style="width:56px;height:56px;border-radius:14px;margin:0 auto 12px;
                                  display:flex;align-items:center;justify-content:center;
                                  background:linear-gradient(135deg,#023E8A,#0077B6);
                                  color:white;font-size:24px;box-shadow:0 4px 12px rgba(2,62,138,.3);",
                           icon("brain")),
                  tags$h4(style="font-weight:700;color:#0B3954;font-size:14px;margin:0 0 6px;",
                           "Machine Learning"),
                  tags$p(style="font-size:12px;color:#666;line-height:1.6;",
                    "Modèles prédictifs, clustering, réduction de dimension. ",
                    "L'intelligence artificielle appliquée à la santé publique.")
                )
              ),
              column(3,
                tags$div(style="text-align:center;padding:16px 10px;",
                  tags$div(style="width:56px;height:56px;border-radius:14px;margin:0 auto 12px;
                                  display:flex;align-items:center;justify-content:center;
                                  background:linear-gradient(135deg,#00838F,#2E7D52);
                                  color:white;font-size:24px;box-shadow:0 4px 12px rgba(29,158,117,.3);",
                           icon("globe-africa")),
                  tags$h4(style="font-weight:700;color:#0B3954;font-size:14px;margin:0 0 6px;",
                           "Impact Terrain"),
                  tags$p(style="font-size:12px;color:#666;line-height:1.6;",
                    "Enquêtes SMART, sécurité alimentaire, surveillance nutritionnelle. ",
                    "Des outils pensés pour l'Afrique Centrale et au-delà.")
                )
              )
            )
          ),

          # NutriAnalyse Pro section
          tags$div(
            style="background:linear-gradient(135deg,#f8fbff,#eef5fc);border-radius:12px;
                   padding:28px;margin-bottom:20px;border:1px solid rgba(0,119,182,.1);",
            tags$h3(style="margin:0 0 16px;font-weight:700;color:#0B3954;font-size:18px;",
                     icon("heartbeat"), "  NutriAnalyse Pro v2"),
            tags$p(style="color:#444;line-height:1.8;font-size:13.5px;margin-bottom:14px;",
              "NutriAnalyse Pro est le produit phare de DSAMID : une plateforme intégrée d'analyse ",
              "nutritionnelle conçue pour les enquêtes SMART, la surveillance nutritionnelle et ",
              "la sécurité alimentaire. Elle rassemble en un seul outil ce qui nécessitait ",
              "auparavant une demi-douzaine de logiciels distincts."
            ),
            fluidRow(
              column(4,
                tags$div(style="padding:8px;",
                  tags$div(style="font-size:36px;font-weight:800;color:#0077B6;", "15+"),
                  tags$div(style="font-size:12px;color:#666;font-weight:600;", "Modules d'analyse"),
                  tags$div(style="font-size:11px;color:#888;margin-top:2px;",
                           "Z-scores, prévalences, cartographie, épidémiologie...")
                )
              ),
              column(4,
                tags$div(style="padding:8px;",
                  tags$div(style="font-size:36px;font-weight:800;color:#0096C7;", "20k+"),
                  tags$div(style="font-size:12px;color:#666;font-weight:600;", "Lignes de code R"),
                  tags$div(style="font-size:11px;color:#888;margin-top:2px;",
                           "Architecture modulaire, maintenable et extensible")
                )
              ),
              column(4,
                tags$div(style="padding:8px;",
                  tags$div(style="font-size:36px;font-weight:800;color:#48CAE4;", "100%"),
                  tags$div(style="font-size:12px;color:#666;font-weight:600;", "Standards respectés"),
                  tags$div(style="font-size:11px;color:#888;margin-top:2px;",
                           "OMS 2006, SMART, PAM/FAO, méthodologies validées")
                )
              )
            )
          ),

          # ── Auteur + Stack technologique ──────────────────────────────────────
          fluidRow(

            # Carte auteur principale
            column(5,
              tags$div(
                style="background:linear-gradient(160deg,#0B3954 0%,#0077B6 100%);
                       border-radius:16px;padding:32px 28px;margin-bottom:20px;
                       box-shadow:0 8px 28px rgba(0,66,106,.3);color:white;",

                # Avatar initiales
                tags$div(
                  style="width:72px;height:72px;border-radius:50%;margin:0 auto 16px;
                         background:rgba(255,255,255,.15);backdrop-filter:blur(8px);
                         border:3px solid rgba(255,255,255,.35);
                         display:flex;align-items:center;justify-content:center;
                         font-size:26px;font-weight:800;letter-spacing:-1px;",
                  "YK"
                ),

                tags$h3(style="margin:0;font-weight:800;font-size:19px;text-align:center;
                               letter-spacing:.3px;",
                        "YIMGA WANDJI KEVIN"),
                tags$p(style="margin:6px 0 0;font-size:12px;opacity:.8;text-align:center;
                              font-weight:500;letter-spacing:.5px;text-transform:uppercase;",
                       "Data Scientist · Développeur Shiny"),
                tags$p(style="margin:4px 0 16px;font-size:12px;opacity:.65;text-align:center;",
                       icon("map-marker-alt"), " Cameroun · Afrique Centrale"),

                tags$hr(style="border-color:rgba(255,255,255,.2);margin:16px 0;"),

                # Liens sociaux
                tags$div(
                  style="display:flex;flex-direction:column;gap:10px;",

                  # LinkedIn
                  tags$a(
                    href   = "https://www.linkedin.com/in/kevin-l-agneau-yimga-wandji-b6959b2a7/",
                    target = "_blank",
                    style  = "display:flex;align-items:center;gap:10px;
                               background:rgba(255,255,255,.12);border-radius:8px;
                               padding:10px 14px;color:white;text-decoration:none;
                               transition:background .2s;font-size:13px;font-weight:500;",
                    tags$span(style="font-size:18px;", HTML("&#xF08C;")),   # icône LinkedIn simulée
                    tags$span(
                      tags$div(style="font-size:11px;opacity:.7;line-height:1;","LinkedIn"),
                      tags$div("YIMGA WANDJI KEVIN", style="line-height:1.4;")
                    )
                  ),

                  # GitHub
                  tags$a(
                    href   = "https://github.com/yimgakevin",
                    target = "_blank",
                    style  = "display:flex;align-items:center;gap:10px;
                               background:rgba(255,255,255,.12);border-radius:8px;
                               padding:10px 14px;color:white;text-decoration:none;
                               transition:background .2s;font-size:13px;font-weight:500;",
                    tags$span(style="font-size:18px;", HTML("&#xF09B;")),
                    tags$span(
                      tags$div(style="font-size:11px;opacity:.7;line-height:1;","GitHub"),
                      tags$div("NutriAnalyse Pro & projets open-source", style="line-height:1.4;")
                    )
                  ),

                  # ORCID
                  tags$a(
                    href   = "https://orcid.org/0009-0006-1000-7339",
                    target = "_blank",
                    style  = "display:flex;align-items:center;gap:10px;
                               background:rgba(255,255,255,.12);border-radius:8px;
                               padding:10px 14px;color:white;text-decoration:none;
                               transition:background .2s;font-size:13px;font-weight:500;",
                    tags$span(
                      style = "width:22px;height:22px;border-radius:50%;flex-shrink:0;
                               background:#A6CE39;display:inline-flex;
                               align-items:center;justify-content:center;
                               font-size:9px;font-weight:900;color:white;",
                      "iD"
                    ),
                    tags$span(
                      tags$div(style="font-size:11px;opacity:.7;line-height:1;", "ORCID"),
                      tags$div("0009-0006-1000-7339",
                               style="line-height:1.4;font-family:monospace;font-size:12px;")
                    )
                  ),

                  # Email contact — à renseigner quand dsamid.com sera créé
                  tags$div(
                    style  = "display:flex;align-items:center;gap:10px;
                               background:rgba(255,255,255,.08);border-radius:8px;
                               padding:10px 14px;color:rgba(255,255,255,.45);
                               font-size:13px;font-style:italic;",
                    icon("envelope"),
                    tags$span(
                      tags$div(style="font-size:11px;opacity:.6;line-height:1;","Email"),
                      tags$div("contact@dsamid.com  (à venir)", style="line-height:1.4;")
                    )
                  )
                )
              )
            ),

            # Stack technologique + stats
            column(7,
              tags$div(
                style="background:white;border-radius:16px;padding:28px;margin-bottom:16px;
                       box-shadow:0 2px 12px rgba(0,0,0,.06);",
                tags$h3(style="margin:0 0 16px;font-weight:700;color:#0B3954;font-size:16px;",
                         icon("code"), "  Stack technologique"),
                tags$div(
                  style="display:flex;flex-wrap:wrap;gap:8px;",
                  lapply(
                    c("R 4.5", "Shiny", "tidyverse", "ggplot2 / plotly",
                      "Leaflet / sf", "gtsummary", "FactoMineR", "xgboost",
                      "shinydashboard", "shinyauthr", "promises / future",
                      "Google Gemini API", "KoboToolbox API", "GADM 4.1",
                      "renv", "testthat"),
                    function(tech) tags$span(
                      style="display:inline-block;padding:5px 13px;border-radius:20px;
                             font-size:12px;font-weight:600;
                             background:linear-gradient(135deg,#f0f8ff,#e8f4fc);
                             color:#0077B6;border:1px solid rgba(0,119,182,.15);",
                      tech
                    )
                  )
                )
              ),

              # Chiffres clés
              tags$div(
                style="background:white;border-radius:16px;padding:24px 28px;
                       box-shadow:0 2px 12px rgba(0,0,0,.06);",
                tags$h3(style="margin:0 0 16px;font-weight:700;color:#0B3954;font-size:16px;",
                        icon("chart-bar"), "  NutriAnalyse Pro v2 en chiffres"),
                fluidRow(
                  column(4, tags$div(style="text-align:center;padding:8px;",
                    tags$div(style="font-size:32px;font-weight:800;color:#0077B6;","28+"),
                    tags$div(style="font-size:11px;color:#666;font-weight:600;","Modules d'analyse"),
                    tags$div(style="font-size:10px;color:#888;margin-top:2px;","Z-scores, prévalences, IA...")
                  )),
                  column(4, tags$div(style="text-align:center;padding:8px;",
                    tags$div(style="font-size:32px;font-weight:800;color:#00838F;","175"),
                    tags$div(style="font-size:11px;color:#666;font-weight:600;","Pays cartographiés"),
                    tags$div(style="font-size:10px;color:#888;margin-top:2px;","GADM 4.1 · Cache local")
                  )),
                  column(4, tags$div(style="text-align:center;padding:8px;",
                    tags$div(style="font-size:32px;font-weight:800;color:#D98B14;","4"),
                    tags$div(style="font-size:11px;color:#666;font-weight:600;","Agents IA autonomes"),
                    tags$div(style="font-size:10px;color:#888;margin-top:2px;","Gemini 2.5 Flash · Multi-tour")
                  ))
                )
              )
            )
          ),

          # Footer
          tags$div(
            style="text-align:center;padding:20px 16px 8px;color:#888;font-size:11px;",
            tags$div(style="margin-bottom:8px;",
              tags$span("NutriAnalyse Pro v2 · Développé par "),
              tags$span(style="font-weight:700;color:#0077B6;", "YIMGA WANDJI KEVIN"),
              tags$span(" · DSAMID Data Science & Analysis · 2024–2026")
            ),
            tags$div(
              style="display:flex;align-items:center;justify-content:center;
                     gap:14px;margin-bottom:10px;flex-wrap:wrap;",
              tags$a(href="https://www.linkedin.com/in/kevin-l-agneau-yimga-wandji-b6959b2a7/",
                     target="_blank",
                     style="color:#0077B6;font-size:11px;text-decoration:none;",
                     icon("linkedin"), " LinkedIn"),
              tags$span(style="color:#ddd;","|"),
              tags$a(href="https://github.com/yimgakevin",
                     target="_blank",
                     style="color:#333;font-size:11px;text-decoration:none;",
                     icon("github"), " GitHub"),
              tags$span(style="color:#ddd;","|"),
              tags$a(href="https://orcid.org/0009-0006-1000-7339",
                     target="_blank",
                     style="color:#A6CE39;font-size:11px;text-decoration:none;font-weight:600;",
                     tags$span(
                       style="display:inline-flex;align-items:center;justify-content:center;
                              width:14px;height:14px;border-radius:50%;background:#A6CE39;
                              color:white;font-size:7px;font-weight:900;margin-right:3px;",
                       "iD"),
                     " ORCID")
            ),
            tags$div(style="font-style:italic;opacity:.6;",
              "\"Les données ne mentent pas — encore faut-il savoir les écouter.\"" 
            )
          )
      ),
      )
}
