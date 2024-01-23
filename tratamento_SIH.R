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


load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SIH/sih_final_2016_2021.Rdata')


sih <- sih_final_csv

sih_2 <- sih |>
  vitallinkage::padroniza_variaveis(namestand,'SIH') |> # Padroniza as variáveis
  vitallinkage::ds_raca_sih() |> # Cria coluna ds_raca
  vitallinkage::nu_idade_anos_sih() |> # Cria coluna de anos
  vitallinkage::faixa_etaria() |> # Cria coluna de faixa etária
  vitallinkage::dias_int_sih() |> # Dias de internação no SIH
  vitallinkage::as_char() # Transforma todos em character

sih_anon <-
  sih_2 |>
  vitallinkage::sih_anon() # Anonimiza
