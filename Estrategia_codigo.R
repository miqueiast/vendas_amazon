#==============================================================================
# ARQUIVO ÚNICO SHINY APP - ANÁLISE AMAZON
# Data: 2024
#==============================================================================

#------------------------------------------------------------------------------
# BIBLIOTECAS NECESSÁRIAS
#------------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(DT)
library(tm)
library(wordcloud)
library(stringr)
library(tidyr)
library(RColorBrewer)

#------------------------------------------------------------------------------
# FUNÇÕES DE TRATAMENTO DE DADOS
#------------------------------------------------------------------------------
carregar_dados <- function() {
  dados_amazon <- read.csv("data/dados_amazon.csv", stringsAsFactors = FALSE)
  
  dados_amazon <- dados_amazon %>%
    mutate(
      preco_real = as.numeric(gsub("[^0-9.]", "", preco_real)),
      desconto = as.numeric(gsub("[^0-9.]", "", desconto)),
      avaliacao = as.numeric(avaliacao)
    ) %>%
    na.omit()
  
  return(dados_amazon)
}

processar_nuvem <- function() {
  dados <- read.csv("data/amazon.csv")
  
  corpus <- Corpus(VectorSource(dados$review_content))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  tdm <- TermDocumentMatrix(corpus)
  matriz <- as.matrix(tdm)
  
  frequencias <- sort(rowSums(matriz), decreasing = TRUE)
  palavras <- data.frame(word = names(frequencias), freq = frequencias)
  
  return(palavras)
}

calcular_metricas <- function(dados) {
  metricas <- dados %>%
    summarise(
      media_preco = mean(preco_real, na.rm = TRUE),
      media_avaliacao = mean(avaliacao, na.rm = TRUE),
      total_produtos = n()
    )
  return(metricas)
}

#------------------------------------------------------------------------------
# UI - INTERFACE DO USUÁRIO
#------------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
            .logo-container {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 10px;
                background-color: #232F3E;
            }
            .logo { 
                height: 80px; 
                margin: 0 10px; 
            }
            .main-title { 
                color: white; 
                text-align: center; 
                flex-grow: 1;
                font-size: 32px;
                font-weight: 700;
            }
            .wordcloud-container {
                background-color: white;
                border-radius: 8px;
                padding: 15px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
        "))
  ),
  
  # Header com logo
  div(class = "logo-container",
      img(src = "amazon.png", class = "logo"),
      h1("Amazon India Analytics", class = "main-title"),
      img(src = "india.png", class = "logo")
  ),
  
  # Corpo principal
  navbarPage(
    title = "",
    
    # Tab da Nuvem de Palavras
    tabPanel("Análise de Reviews",
             fluidRow(
               column(6, 
                      div(class = "wordcloud-container",
                          plotOutput("wordcloud_plot", height = "800px")
                      )
               )
             )
    )
    # Adicione mais tabs conforme necessário
  )
)

#------------------------------------------------------------------------------
# SERVER - LÓGICA DO SERVIDOR
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  # Carregar dados
  dados_amazon <- carregar_dados()
  dados_nuvem <- processar_nuvem()
  
  # Nuvem de palavras
  output$wordcloud_plot <- renderPlot({
    set.seed(1234)
    wordcloud(words = dados_nuvem$word, 
              freq = dados_nuvem$freq, 
              min.freq = 2,
              max.words = 100, 
              scale = c(5, 0.5),
              random.order = FALSE, 
              rot.per = 0.35, 
              colors = brewer.pal(8, "Dark2"))
  })
  
  # Adicione mais outputs conforme necessário
}

#------------------------------------------------------------------------------
# INICIAR APLICAÇÃO
#------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

#==============================================================================
# FIM DO ARQUIVO
#==============================================================================