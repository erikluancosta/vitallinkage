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
sinan <- read.dbf('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SINAN/VIOLENET.DBF', as.is = TRUE)


sinan_2 <- sinan

# pipeline
sinan_2 <- sinan_2 |>
  #vitallinkage::upper_case_char() |> # As colunas com texto passam a ficar com letra maiuscula
  vitallinkage::drop_duplicados_sinan_1() |>  # Dropa as colunas duplicadas inicialmente
  vitallinkage::padroniza_variaveis(namestand,"SINAN") |> # Padroniza variáveis baseado no df nomestand - ADICIONAR O CRIA COLUNA ID
  vitallinkage::ajuste_txt() |> # Ajusta os textos de nomes
  vitallinkage::ds_raca_sinan() |> # Cria coluna descrevendo as raça no sinan
  vitallinkage::corrige_sg_sexo() |> # Corrige os registros de sexo Ignorado
  vitallinkage::nu_idade_anos_sinan() |> # Adiciona a coluna com a idade calculada
  vitallinkage::as_char() |>  # Transformar todos em texto
  vitallinkage::corrige_datas() |>  # Transforma em data as colunas que comecem com "dt_"
  vitallinkage::ajuste_cd_to_int()

# remove colunas com dados pessoais
sinan_anon <- sinan |>
  vitallinkage::sinan_anon()

a <- sinan[grepl("@", sinan$NM_PACIENT), ]
b <- sinan_2[grepl("@", sinan_2$ds_nome_pac), ]


