## Preparando as bases do SIM

library(readstata13)
library(tidyverse)
library(pdftools)
library(data.table)
library(dplyr)
library(readxl)
library(foreign)
library(stringi)
library(SoundexBR) #soundex
library(janitor)
library(lubridate)
library(writexl)
devtools::load_all(".")

sim_2 <- sim |>
  vitallinkage::limpa_ignorados_sim() |> # Remove textos de ignorado
  vitallinkage::padroniza_variaveis(namestand, "SIM") |> # Padronizando os nomes das variáveis
  vitallinkage::ajuste_data(tipo_data = 1) |> # Ajustando o formato de data
  vitallinkage::ano_sim() |> # Adicionando o ano
  vitallinkage::as_char() |> # Transformando todos em character
  vitallinkage::ajuste_txt() |> # Ajusta as variáveis que contem "nome" na composição
  vitallinkage::soundex_linkage("ds_nome_pac") |> # Criando colunas com SOUNDEX
  vitallinkage::soundex_linkage("ds_nome_pai") |> # Criando colunas com SOUNDEX
  vitallinkage::soundex_linkage("ds_nome_mae") |> # Criando colunas com SOUNDEX
  vitallinkage::ajuste_res() |> # Ajusta as variáveis que contem "_res" na composição
  vitallinkage::drop_duplicados_sim_padronizado() |> # Série de lógicas para remover duplicados
  ## NOVAS VARIÁVEIS
  vitallinkage::ds_raca_sim() |> # Ajustando a raça/cor
  vitallinkage::corrige_sg_sexo() |> # Ajustando a variável sg_sexo
  vitallinkage::nu_idade_anos_sim() |> # Ajustanso a idade em anos
  vitallinkage::as_char()  # Tudo como character

usethis::use_data(sim, overwrite = TRUE)
