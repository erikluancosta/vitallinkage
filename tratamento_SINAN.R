library(readstata13)
library(tidyverse)
library(pdftools)
library(data.table)
library(dplyr)
library(readxl)
library(foreign)
library(stringi)
library(SoundexBR) #soundex em portugues
library(janitor)
library(lubridate)
library(writexl)
devtools::load_all(".")


# leitura da base especifica para cada caso
sinan <- read.dbf('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SINAN/VIOLENET.DBF')


namestand<- vitallinkage::namestand
rm(namestand)
#
sinan <- sinan |>
  vitallinkage::drop_duplicados_sinan_1() |>  # Dropa as colunas duplicadas inicialmente
  vitallinkage::criar_coluna_id("SINAN")|> # Criando colunas com ID "SINAN_1 ... " e adiciona coluna com nome do banco
  vitallinkage::padroniza_variaveis(namestand,"SINAN") |> # Padroniza variáveis baseado no df nomestand
  vitallinkage::ds_raca_sinan() |> # Cria coluna descrevendo as raça no sinan
  vitallinkage::corrige_sg_sexo() |> # Corrige os registros de sexo Ignorado
  vitallinkage::nu_idade_anos_sinan() # Adiciona a coluna com a idade calculada

# remove colunas com dados pessoais
sinan_anon <- sinan |>
  vitallinkage::sinan_anon()


# Tirar Dúvidas:
data.frame(lapply(sinan, as.character), stringsAsFactors = FALSE)
#
sinan <- sinan %>%
  mutate(id_sinan=paste0(row.names(sinan),nu_not))

