library(dplyr)
library(readr)
library(skimr)  # Para uma análise mais detalhada

# Leitura dos dados
dados_amazon <- read.csv("dados_amazon.csv")

# Visão geral básica dos dados
glimpse(dados_amazon)

# Estrutura mais detalhada
str(dados_amazon)

# Resumo estatístico
summary(dados_amazon)

# Análise mais detalhada com skimr
skim(dados_amazon)

# Primeiras linhas dos dados
head(dados_amazon)

# Verificar valores únicos em colunas categóricas
unique_categories <- dados_amazon %>%
  summarise(across(where(is.character), n_distinct))
print(unique_categories)

# Verificar valores missing
missing_values <- colSums(is.na(dados_amazon))
print(missing_values)