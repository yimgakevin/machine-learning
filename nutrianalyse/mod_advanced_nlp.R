# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_advanced_nlp.R — NLP & Sentiment Analysis sur commentaires libres       ║
# ║  Extraction thèmes, analyse sentiments, nuage mots, classification textes    ║
# ║  Niveau: Doctoral Research Grade — Natural Language Processing              ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(shiny)
library(tidyverse)
# library(tm)  # Optional: install.packages("tm")
# library(SnowballC)  # Optional: install.packages("SnowballC")
# library(wordcloud)  # Optional: install.packages("wordcloud")
# library(topicmodels)  # Optional: install.packages("topicmodels")
# library(sentimentr)  # Optional: install.packages("sentimentr")
# library(tidytext)  # Optional: install.packages("tidytext")
library(plotly)
library(DT)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

advancedNLPUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #008DC9; border-radius: 6px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#008DC9;",
              icon("comments"), tr("nlp_title", lang)),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             tr("nlp_desc", lang),
             )
    ),
    fluidRow(
      box(width = 3, status = "info", solidHeader = TRUE, title = tr("nlp_config", lang),
          uiOutput(ns("nlp_text_col_ui")),
          hr(),
          tags$div(class = "section-header", tr("nlp_processing_hdr", lang)),
          checkboxInput(ns("nlp_lowercase"), tr("nlp_lowercase", lang), TRUE),
          checkboxInput(ns("nlp_remove_punct"), tr("nlp_remove_punct", lang), TRUE),
          checkboxInput(ns("nlp_remove_stopwords"), tr("nlp_remove_stop", lang), TRUE),
          checkboxInput(ns("nlp_stem"), tr("nlp_stem", lang), FALSE),
          hr(),
          tags$div(class = "section-header", tr("nlp_analysis_hdr", lang)),
          selectInput(ns("nlp_analysis"), tr("nlp_type_label", lang),
                      choices = setNames(c("sentiment","lda","wordcloud","frequency","ngrams"), c(tr("nlp_sentiment",lang),tr("nlp_lda",lang),tr("nlp_wordcloud",lang),tr("nlp_frequency",lang),tr("nlp_ngrams",lang))),
                      selected = "sentiment", width = "100%"),
          numericInput(ns("nlp_n_topics"), tr("nlp_n_topics", lang),
                       value = 5, min = 2, max = 15, width = "100%"),
          hr(),
          actionButton(ns("nlp_analyze"), tr("nlp_btn_run", lang),
                       class = "btn btn-oms btn-block", width = "100%",
                       style = "font-size:13px;padding:10px;")
      ),
      box(width = 9, status = "primary", solidHeader = TRUE,
          title = tr("nlp_viz_title", lang),
          tabsetPanel(
            tabPanel(tr("nlp_tab_sentiment", lang),
              fluidRow(
                column(6, plotlyOutput(ns("nlp_sentiment_dist"), height = "400px")),
                column(6, plotlyOutput(ns("nlp_sentiment_timeline"), height = "400px"))
              )
            ),
            tabPanel(tr("nlp_tab_lda", lang),
              plotlyOutput(ns("nlp_topics_plot"), height = "500px")
            ),
            tabPanel(tr("nlp_tab_keywords", lang),
              fluidRow(
                column(6, plotOutput(ns("nlp_wordcloud"), height = "400px")),
                column(6, plotlyOutput(ns("nlp_freq_plot"), height = "400px"))
              )
            ),
            tabPanel(tr("nlp_tab_ngrams", lang),
              plotlyOutput(ns("nlp_ngrams_plot"), height = "500px")
            )
          )
      )
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tr("nlp_sent_dist", lang),
          DTOutput(ns("nlp_sentiment_table"))
      ),
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tr("nlp_topics_det", lang),
          DTOutput(ns("nlp_topics_table"))
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = tr("nlp_examples", lang),
          DTOutput(ns("nlp_examples_table"))
      )
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = tags$span(icon("download"), " Exports NLP"),
          fluidRow(
            column(3, downloadButton(ns("nlp_dl_sentiment_csv"), "📊 Sentiments CSV",
                                     style = "background:#008DC9;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("nlp_dl_topics_csv"), "🏷️ Topics CSV",
                                     style = "background:#4dac26;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("nlp_dl_wordfreq"), "📝 Mots-clés CSV",
                                     style = "background:#e88400;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("nlp_dl_report"), "📄 Rapport HTML",
                                     style = "background:#9b59b6;color:white;border:none;width:100%;"))
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "Rapport d'analyse textuelle",
          verbatimTextOutput(ns("nlp_report"))
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

advancedNLPServer <- function(id, data_reactive, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Column selection
    output$nlp_text_col_ui <- renderUI({
      d <- data_reactive(); req(d)
      text_cols <- grep("comment|text|remark|obs|note|texte|remarque",
                       names(d), ignore.case = TRUE, value = TRUE)
      selectInput(ns("nlp_text_col"), "Colonne texte à analyser :",
                  choices = c("— Sélectionner —" = "", names(d)),
                  selected = if (length(text_cols) > 0) text_cols[1] else "",
                  width = "100%")
    })

    # Extract and clean texts
    texts_clean <- reactive({
      req(data_reactive(), input$nlp_text_col, input$nlp_analyze)

      withProgress(message = "Nettoyage textes...", value = 0, {
        texts <- data_reactive()[[input$nlp_text_col]] |>
          as.character() |>
          na.omit()

        if (length(texts) == 0) return(NULL)

        incProgress(0.2)

        # Lowercase
        if (input$nlp_lowercase) {
          texts <- tolower(texts)
        }

        incProgress(0.4)

        # Remove punctuation
        if (input$nlp_remove_punct) {
          texts <- gsub("[[:punct:]]", " ", texts)
        }

        incProgress(0.6)

        # Remove stopwords
        if (input$nlp_remove_stopwords) {
          # French stopwords
          fr_stops <- tm::stopwords("french")
          texts <- sapply(texts, function(t) {
            words <- unlist(strsplit(t, " "))
            words <- words[!words %in% fr_stops & nchar(words) > 2]
            paste(words, collapse = " ")
          }, USE.NAMES = FALSE)
        }

        incProgress(0.8)

        # Stemming
        if (input$nlp_stem) {
          texts <- sapply(texts, function(t) {
            words <- unlist(strsplit(t, " "))
            words <- tm::stemDocument(words, language = "french")
            paste(words, collapse = " ")
          }, USE.NAMES = FALSE)
        }

        incProgress(1.0)
        texts
      })
    })

    # Sentiment analysis
    sentiment_data <- reactive({
      req(texts_clean(), input$nlp_analyze)

      if (input$nlp_analysis != "sentiment") return(NULL)

      withProgress(message = "Analyse sentiments...", value = 1, {
        texts <- texts_clean()
        
        # Analyse sentiments (sentimentr)
        sentiments <- sentimentr::sentiment(texts)
        
        tibble(
          text = texts,
          sentiment_score = sentiments$sentiment,
          sentiment_category = case_when(
            sentiments$sentiment < -0.5 ~ "Très négatif",
            sentiments$sentiment < -0.1 ~ "Négatif",
            sentiments$sentiment < 0.1 ~ "Neutre",
            sentiments$sentiment < 0.5 ~ "Positif",
            TRUE ~ "Très positif"
          ),
          word_count = sapply(strsplit(texts, " "), length)
        )
      })
    })

    # Sentiment distribution
    output$nlp_sentiment_dist <- renderPlotly({
      req(sentiment_data())
      d <- sentiment_data()

      p <- ggplot(d, aes(x = sentiment_category, fill = sentiment_category)) +
        geom_bar(alpha = 0.7) +
        scale_fill_manual(values = c(
          "Très négatif" = "#c9161d",
          "Négatif" = "#e88400",
          "Neutre" = "#999999",
          "Positif" = "#4dac26",
          "Très positif" = "#1D9E75"
        )) +
        labs(title = "Distribution des sentiments",
             x = "Catégorie", y = "Fréquence") +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(p)
    })

    # Sentiment timeline
    output$nlp_sentiment_timeline <- renderPlotly({
      req(sentiment_data())
      d <- sentiment_data() |>
        mutate(index = row_number()) |>
        mutate(window = cut(index, breaks = 10))

      p <- ggplot(d, aes(x = index, y = sentiment_score, color = sentiment_category)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "loess", se = TRUE, color = "darkblue", alpha = 0.2) +
        scale_color_manual(values = c(
          "Très négatif" = "#c9161d",
          "Négatif" = "#e88400",
          "Neutre" = "#999999",
          "Positif" = "#4dac26",
          "Très positif" = "#1D9E75"
        )) +
        labs(title = "Évolution sentiments (trends)",
             x = "Texte (ordre)", y = "Score sentiment") +
        theme_minimal()

      ggplotly(p)
    })

    # Sentiment table
    output$nlp_sentiment_table <- renderDT({
      req(sentiment_data())
      d <- sentiment_data()

      d |>
        group_by(sentiment_category) |>
        summarise(
          N = n(),
          `% Total` = round(n() / nrow(d) * 100, 1),
          `Score moyen` = round(mean(sentiment_score), 3),
          `Mots moyen` = round(mean(word_count), 1),
          .groups = "drop"
        ) |>
        datatable(rownames = FALSE, options = list(dom = "t"),
                  class = "compact stripe")
    })

    # Topic modeling (LDA)
    topic_data <- reactive({
      req(texts_clean(), input$nlp_analyze, input$nlp_n_topics)

      if (input$nlp_analysis != "lda") return(NULL)

      withProgress(message = "LDA topic modeling...", value = 0, {
        texts <- texts_clean()

        incProgress(0.3)

        # Create DTM
        corpus <- Corpus(VectorSource(texts))
        dtm <- DocumentTermMatrix(corpus)

        incProgress(0.6)

        # LDA
        lda_model <- topicmodels::LDA(dtm, k = input$nlp_n_topics,
                                      control = list(seed = 1234))

        incProgress(0.9)

        # Extract topics
        topics <- topicmodels::topics(lda_model)
        terms <- topicmodels::terms(lda_model, 10)

        incProgress(1.0)

        list(
          model = lda_model,
          topics = topics,
          terms = terms,
          dtm = dtm
        )
      })
    })

    # Topics visualization
    output$nlp_topics_plot <- renderPlotly({
      req(topic_data())
      td <- topic_data()

      topic_freq <- table(td$topics)

      p <- ggplot(data.frame(topic = names(topic_freq),
                            freq = as.numeric(topic_freq)),
                 aes(x = reorder(topic, -freq), y = freq, fill = topic)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        labs(title = "Fréquence topics (LDA)",
             x = "Topic", y = "Documents") +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(p)
    })

    # Topics table
    output$nlp_topics_table <- renderDT({
      req(topic_data())
      td <- topic_data()

      terms_df <- as.data.frame(td$terms)
      names(terms_df) <- paste0("Topic_", 1:ncol(terms_df))

      datatable(terms_df, rownames = TRUE, options = list(dom = "tp", pageLength = 10),
                class = "compact stripe")
    })

    # Word cloud
    output$nlp_wordcloud <- renderPlot({
      req(texts_clean(), input$nlp_analysis)

      if (input$nlp_analysis != "wordcloud") return(NULL)

      texts <- texts_clean()
      words <- unlist(strsplit(paste(texts, collapse = " "), " "))
      word_freq <- table(words)
      word_freq <- sort(word_freq, decreasing = TRUE)[1:100]

      wordcloud::wordcloud(names(word_freq), as.numeric(word_freq),
                           scale = c(4, 0.5),
                           colors = RColorBrewer::brewer.pal(8, "Dark2"),
                           random.order = FALSE)
    })

    # Word frequency
    output$nlp_freq_plot <- renderPlotly({
      req(texts_clean(), input$nlp_analysis)

      if (input$nlp_analysis != "frequency") return(NULL)

      texts <- texts_clean()
      words <- unlist(strsplit(paste(texts, collapse = " "), " "))
      word_freq <- sort(table(words), decreasing = TRUE)[1:20]

      p <- ggplot(data.frame(word = names(word_freq),
                            freq = as.numeric(word_freq)),
                 aes(x = reorder(word, freq), y = freq, fill = freq)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        coord_flip() +
        scale_fill_viridis_c() +
        labs(title = "Top 20 mots-clés",
             x = "Mot", y = "Fréquence") +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(p)
    })

    # N-grams
    output$nlp_ngrams_plot <- renderPlotly({
      req(texts_clean(), input$nlp_analyze)

      if (input$nlp_analysis != "ngrams") return(NULL)

      texts <- texts_clean()
      
      # Bigrammes
      bigrams <- NULL
      for (text in texts) {
        words <- unlist(strsplit(text, " "))
        if (length(words) >= 2) {
          bigrams <- c(bigrams,
                      paste(words[-length(words)], words[-1], sep = " "))
        }
      }

      bigram_freq <- sort(table(bigrams), decreasing = TRUE)[1:15]

      p <- ggplot(data.frame(bigram = names(bigram_freq),
                            freq = as.numeric(bigram_freq)),
                 aes(x = reorder(bigram, freq), y = freq, fill = freq)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        coord_flip() +
        scale_fill_viridis_c() +
        labs(title = "Top 15 bigrammes",
             x = "Bigramme", y = "Fréquence") +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(p)
    })

    # Examples texts
    output$nlp_examples_table <- renderDT({
      req(sentiment_data())
      d <- sentiment_data()

      examples <- bind_rows(
        d |> filter(sentiment_category == "Très positif") |> slice_head(n = 2) |>
          mutate(source = "Très positif"),
        d |> filter(sentiment_category == "Positif") |> slice_head(n = 2) |>
          mutate(source = "Positif"),
        d |> filter(sentiment_category == "Neutre") |> slice_head(n = 2) |>
          mutate(source = "Neutre"),
        d |> filter(sentiment_category == "Négatif") |> slice_head(n = 2) |>
          mutate(source = "Négatif"),
        d |> filter(sentiment_category == "Très négatif") |> slice_head(n = 2) |>
          mutate(source = "Très négatif")
      )

      examples |>
        select(source, text, sentiment_score) |>
        rename("Catégorie" = source, "Texte" = text, "Score" = sentiment_score) |>
        datatable(rownames = FALSE, options = list(dom = "tp"),
                  class = "compact stripe")
    })

    # Report
    output$nlp_report <- renderPrint({
      req(texts_clean())
      texts <- texts_clean()

      cat("═══════════════════════════════════════════════════════════\n")
      cat("RAPPORT D'ANALYSE NLP\n")
      cat("═══════════════════════════════════════════════════════════\n\n")
      
      cat("STATISTIQUES CORPUS\n")
      cat("Nombre textes:", length(texts), "\n")
      cat("Total mots:", sum(sapply(strsplit(texts, " "), length)), "\n")
      cat("Mots uniques:", length(unique(unlist(strsplit(texts, " ")))), "\n")
      cat("Longueur moyenne:", 
          round(mean(sapply(strsplit(texts, " "), length)), 1), "mots\n\n")

      if (!is.null(sentiment_data())) {
        cat("ANALYSE SENTIMENTS\n")
        sd <- sentiment_data()
        cat("Score moyen:", round(mean(sd$sentiment_score), 3), "\n")
        cat("Écart-type:", round(sd(sd$sentiment_score), 3), "\n")
        cat("Positive:", round(mean(sd$sentiment_score > 0.1) * 100, 1), "%\n")
        cat("Neutre:", round(mean(abs(sd$sentiment_score) <= 0.1) * 100, 1), "%\n")
        cat("Négatif:", round(mean(sd$sentiment_score < -0.1) * 100, 1), "%\n\n")
      }
    })

    # Downloads
    output$nlp_dl_sentiment_csv <- downloadHandler(
      filename = function() { paste0("sentiments_", Sys.Date(), ".csv") },
      content = function(file) {
        req(sentiment_data())
        write.csv(sentiment_data(), file, row.names = FALSE)
      }
    )

    output$nlp_dl_topics_csv <- downloadHandler(
      filename = function() { paste0("topics_lda_", Sys.Date(), ".csv") },
      content = function(file) {
        req(topic_data())
        write.csv(data.frame(topic = topic_data()$topics), file, row.names = FALSE)
      }
    )

    output$nlp_dl_wordfreq <- downloadHandler(
      filename = function() { paste0("wordfreq_", Sys.Date(), ".csv") },
      content = function(file) {
        req(texts_clean())
        texts <- texts_clean()
        words <- unlist(strsplit(paste(texts, collapse = " "), " "))
        word_freq <- sort(table(words), decreasing = TRUE)
        write.csv(data.frame(word = names(word_freq), freq = as.numeric(word_freq)),
                 file, row.names = FALSE)
      }
    )

    output$nlp_dl_report <- downloadHandler(
      filename = function() { paste0("rapport_nlp_", Sys.Date(), ".html") },
      content = function(file) {
        req(texts_clean(), sentiment_data())
        
        html_content <- "<html><head><meta charset='UTF-8'></head><body>"
        html_content <- paste0(html_content,
          "<h1>Rapport d'Analyse NLP</h1>",
          "<h2>Statistiques Corpus</h2>",
          "<p>Textes analysés: ", length(texts_clean()), "</p>",
          "<h2>Sentiments</h2>",
          "<p>Score moyen: ", round(mean(sentiment_data()$sentiment_score), 3), "</p>",
          "</body></html>")
        
        write(html_content, file)
      }
    )
  })
}
