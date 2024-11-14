library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(shinythemes)
library(scales)
library(wordcloud)
library(tm)
library(stringr)
library(RColorBrewer)

# Carregar os dados
dados_amazon <- read.csv("dados_amazon.csv", stringsAsFactors = FALSE)

# UI
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
    height: 80px;  # Aumentado de 50px para 80px
    margin: 0 10px; 
  }
  .main-title { 
    color: white; 
    text-align: center; 
    flex-grow: 1;
    font-family: 'Amazon Ember', Arial, sans-serif;
    font-size: 32px;
    font-weight: 700;
    letter-spacing: 1px;
    text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
  }
  .wordcloud-container {
    background-color: white;
    border-radius: 8px;
    padding: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
"))
  ),
  
  div(class = "logo-container",
      img(src = "amazon.png", class = "logo"),
      h2(class = "main-title", "Dashboard Amazon India"),
      img(src = "india.png", class = "logo")
  ),
  
  navbarPage(
    title = "",
    
    tabPanel("Visão Geral",
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput("categoria",
                                    "Categoria Principal:",
                                    choices = c("Todos", unique(dados_amazon$X1))),
                        sliderInput("preco_range",
                                    "Faixa de Preço:",
                                    min = 0,
                                    max = max(dados_amazon$preco_real, na.rm = TRUE),
                                    value = c(0, max(dados_amazon$preco_real, na.rm = TRUE))),
                        sliderInput("avaliacao_range",
                                    "Avaliação:",
                                    min = 1,
                                    max = 5,
                                    value = c(1, 5),
                                    step = 0.1)
                      )
               ),
               column(9,
                      fluidRow(
                        column(6, plotlyOutput("preco_dist")),
                        column(6, plotlyOutput("avaliacao_dist"))
                      ),
                      fluidRow(
                        column(12, plotlyOutput("desconto_avaliacao"))
                      ),
                      fluidRow(
                        column(12, plotlyOutput("top_produtos"))
                      )
               )
             )
    ),
    
    tabPanel("Análise de Preços",
             fluidRow(
               column(12,
                      DTOutput("tabela_precos")
               )
             )
    ),
    
    tabPanel("Análise de Avaliações",
             fluidRow(
               column(6, 
                      div(class = "wordcloud-container",
                          plotOutput("wordcloud_plot", height = "600px")
                      )
               ),
               column(6, plotlyOutput("avaliadores_freq"))
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- dados_amazon
    
    if (input$categoria != "Todos") {
      data <- data[data$X1 == input$categoria,]
    }
    
    data <- data[data$preco_real >= input$preco_range[1] & 
                   data$preco_real <= input$preco_range[2] &
                   data$avaliacao >= input$avaliacao_range[1] &
                   data$avaliacao <= input$avaliacao_range[2],]
    
    return(data)
  })
  
  # Distribuição de Preços
  output$preco_dist <- renderPlotly({
    dados_filtrados <- filtered_data()
    
    p <- plot_ly(dados_filtrados, x = ~preco_real, type = "histogram",
                 marker = list(color = "#FF9900"),
                 name = "Distribuição de Preços") %>%
      layout(title = "Distribuição dos Preços",
             xaxis = list(title = "Preço (INR)"),
             yaxis = list(title = "Frequência"))
    
    return(p)
  })
  
  # Distribuição de Avaliações
  output$avaliacao_dist <- renderPlotly({
    dados_filtrados <- filtered_data()
    
    p <- plot_ly(dados_filtrados, x = ~avaliacao, type = "histogram",
                 marker = list(color = "#146EB4"),
                 name = "Distribuição de Avaliações") %>%
      layout(title = "Distribuição das Avaliações",
             xaxis = list(title = "Avaliação"),
             yaxis = list(title = "Frequência"))
    
    return(p)
  })
  
  # Relação entre Desconto e Avaliação
  output$desconto_avaliacao <- renderPlotly({
    dados_filtrados <- filtered_data()
    
    p <- plot_ly(dados_filtrados, x = ~desconto, y = ~avaliacao, type = "scatter",
                 mode = "markers",
                 marker = list(color = "#232F3E"),
                 name = "Desconto vs Avaliação") %>%
      layout(title = "Relação entre Desconto e Avaliação",
             xaxis = list(title = "Desconto (%)"),
             yaxis = list(title = "Avaliação"))
    
    return(p)
  })
  
  # Top Produtos
  output$top_produtos <- renderPlotly({
    dados_filtrados <- filtered_data()
    
    top_10 <- dados_filtrados %>%
      group_by(nome_produto) %>%
      summarise(avg_rating = mean(avaliacao, na.rm = TRUE),
                count = n()) %>%
      arrange(desc(avg_rating), desc(count)) %>%
      head(10)
    
    p <- plot_ly(top_10, x = ~reorder(nome_produto, avg_rating),
                 y = ~avg_rating, type = "bar",
                 marker = list(color = "#FF9900"),
                 name = "Top Produtos") %>%
      layout(title = "Top 10 Produtos por Avaliação",
             xaxis = list(title = "Produto",
                          tickangle = 45),
             yaxis = list(title = "Avaliação Média"))
    
    return(p)
  })
  
  # Tabela de Preços
  output$tabela_precos <- renderDT({
    dados_filtrados <- filtered_data()
    
    tabela <- dados_filtrados %>%
      select(nome_produto, preco_real, preco_original, desconto, avaliacao) %>%
      arrange(desc(preco_real))
    
    datatable(tabela,
              options = list(pageLength = 10,
                             scrollX = TRUE),
              colnames = c("Produto", "Preço Real", "Preço Original",
                           "Desconto (%)", "Avaliação"))
  })
  
  # Nuvem de Palavras
  output$wordcloud_plot <- renderPlot({
    # Ler os dados diretamente do arquivo amazon.csv
    dados <- read.csv("amazon.csv")
    
    # Converter a coluna review_content em um corpus de texto
    corpus <- Corpus(VectorSource(dados$review_content))
    
    # Pré-processamento do texto
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Criar uma matriz de termos
    tdm <- TermDocumentMatrix(corpus)
    matriz <- as.matrix(tdm)
    
    # Somar as frequências de cada palavra
    frequencias <- sort(rowSums(matriz), decreasing = TRUE)
    palavras <- data.frame(word = names(frequencias), freq = frequencias)
    
    # Gerar a nuvem de palavras
    set.seed(1234)
    wordcloud(words = palavras$word, 
              freq = palavras$freq, 
              min.freq = 2,
              max.words = 100, 
              random.order = FALSE, 
              rot.per = 0.35, 
              colors = brewer.pal(8, "Dark2"))
  })
  
  # Frequência de Avaliadores
  output$avaliadores_freq <- renderPlotly({
    dados_filtrados <- filtered_data()
    
    freq_avaliadores <- dados_filtrados %>%
      group_by(avaliacao) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    plot_ly(freq_avaliadores,
            x = ~avaliacao,
            y = ~count,
            type = "bar",
            marker = list(color = "#146EB4")) %>%
      layout(title = "Distribuição das Avaliações",
             xaxis = list(title = "Avaliação"),
             yaxis = list(title = "Quantidade"),
             showlegend = FALSE)
  })
}

shinyApp(ui = ui, server = server)