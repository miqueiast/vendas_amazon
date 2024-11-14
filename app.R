library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(shinythemes)
library(scales)

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
      .logo { height: 50px; margin: 0 10px; }
      .main-title { color: white; text-align: center; flex-grow: 1; }
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
                                    max = max(dados_amazon$preco_real),
                                    value = c(0, max(dados_amazon$preco_real))),
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
               column(6, plotlyOutput("wordcloud_plot")),
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
  
  output$preco_dist <- renderPlotly({
    plot_ly(data = filtered_data(),
            x = ~preco_real,
            type = "histogram",
            name = "Preço Original",
            marker = list(color = "#FF9900")) %>%
      add_histogram(x = ~preco_descontado,
                    name = "Preço com Desconto",
                    marker = list(color = "#146EB4")) %>%
      layout(barmode = "overlay",
             title = "Distribuição de Preços",
             xaxis = list(title = "Preço (INR)"),
             yaxis = list(title = "Frequência"))
  })
  
  output$avaliacao_dist <- renderPlotly({
    plot_ly(data = filtered_data(),
            x = ~avaliacao,
            type = "histogram",
            marker = list(color = "#146EB4")) %>%
      layout(title = "Distribuição das Avaliações",
             xaxis = list(title = "Avaliação"),
             yaxis = list(title = "Frequência"))
  })
  
  output$desconto_avaliacao <- renderPlotly({
    plot_ly(data = filtered_data(),
            x = ~percentual_desconto,
            y = ~avaliacao,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#FF9900"),
            text = ~nome_produto) %>%
      layout(title = "Relação entre Desconto e Avaliação",
             xaxis = list(title = "Percentual de Desconto"),
             yaxis = list(title = "Avaliação"))
  })
  
  output$top_produtos <- renderPlotly({
    top_10 <- filtered_data() %>%
      arrange(desc(contagem_avaliacao)) %>%
      head(10)
    
    plot_ly(data = top_10,
            x = ~reorder(nome_produto, contagem_avaliacao),
            y = ~contagem_avaliacao,
            type = "bar",
            marker = list(color = "#146EB4")) %>%
      layout(title = "Top 10 Produtos Mais Avaliados",
             xaxis = list(title = "Produto"),
             yaxis = list(title = "Número de Avaliações"))
  })
  
  output$tabela_precos <- renderDT({
    filtered_data() %>%
      select(nome_produto, preco_real, preco_descontado, 
             percentual_desconto, avaliacao, contagem_avaliacao) %>%
      datatable(options = list(pageLength = 10),
                colnames = c("Produto", "Preço Original", "Preço com Desconto",
                             "Desconto (%)", "Avaliação", "Qtd. Avaliações"))
  })
  
  output$avaliadores_freq <- renderPlotly({
    top_avaliadores <- filtered_data() %>%
      group_by(nome) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)
    
    plot_ly(data = top_avaliadores,
            x = ~reorder(nome, count),
            y = ~count,
            type = "bar",
            marker = list(color = "#FF9900")) %>%
      layout(title = "Top 10 Avaliadores",
             xaxis = list(title = "Avaliador"),
             yaxis = list(title = "Número de Avaliações"))
  })
  
  # Removendo o gráfico wordcloud que estava indefinido
  output$wordcloud_plot <- renderPlotly({
    plot_ly() %>%
      layout(title = "Análise de Palavras",
             xaxis = list(showticklabels = FALSE),
             yaxis = list(showticklabels = FALSE))
  })
}

shinyApp(ui = ui, server = server)