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

# Specify the file path
path <- 'C:/vitalstrategies/data_sicence/TCC/SCRIPTS_LINKAGE/Erik/SIM'

# lista de arquivos com fim .DBF
dbf_files <- list.files(path, pattern = "\\.DBF$", full.names = TRUE)

# Data Frame para adicionar os dados carregados
sim_raw <- data.frame()

# Loop para carregar e concatenar os dbf
for (file in dbf_files) {
  # Lendo arquivo DBF
  sim_temp <- read.dbf(file) |>
    mutate(ANO=substr(DTOBITO,5,8))

  sim_raw <- bind_rows(sim_raw,sim_temp)
  rm(sim_temp)
}

sim<-sim_raw

sim_2 <- sim |>
  vitallinkage::ano_sim() |> # Adicionando o ano
  vitallinkage::ajuste_data(tipo_data = 1) |> # Ajustando o formato de data
  vitallinkage::as_char() |> # Transformando todos em character
  vitallinkage::variaveis_principais_sim() |> # Seleção das principais variáveis do SIM
  vitallinkage::limpa_ignorados_sim() |> # Remove textos de ignorado
  #vitallinkage::ajusta_encoding_sim() |> # Ajusta o encoding para UTF-8 ASCII
  #vitallinkage::upper_case_char() |> # As colunas com texto passam a ficar com letra maiuscula
  #vitallinkage::tratamentos_txt_sim() |> # Ajustes nos textos das variaveis char
  vitallinkage::ajuste_txt() |>
  base::unique() |>  # Novos valores únicos após o tratamento
  vitallinkage::soundex_nome_sim() |>  # novas colunas: NOME1_SOUND, NOME2_SOUND, NOME3_SOUND
  vitallinkage::soundex_nomemae_sim() |> # novas colunas: NOMEMAE1_SOUND, NOMEMAE2_SOUND, NOMEMAE3_SOUND
  vitallinkage::soundex_nomepai_sim() |>  # novas colunas: NOMEPAI1_SOUND, NOMEPAI2_SOUND, NOMEPAI3_SOUND
  vitallinkage::drop_duplicados_sim() |> # Aplicando métodos para dropar duplicados
  vitallinkage::variaveis_principais_sim() |> # Seleção das principais variáveis do SIM
  vitallinkage::padroniza_variaveis(namestand, "SIM") |> # Padronizando os nomes das variáveis
  vitallinkage::ds_raca_sim() |> # Ajustando a raça/cor
  vitallinkage::corrige_sg_sexo() |> # Ajustando a variável sg_sexo
  vitallinkage::nu_idade_anos_sim() |> # Ajustanso a idade em anos
  vitallinkage::as_char() # Tudo como character

# Anonimização
sim_anon <- sim_2  |>
  vitallinkage::sim_anon()


a <- sim[grepl("[0-9]", sim$NOME), ]
b <- sim_2[grepl("[0-9]", sim_2$ds_nome_pac), ]


vitallinkage::ajuste_data

a <- sim_2 |> filter(ds_rua_res =='sitio santo antonio')
b <- sim |> filter(ENDRES =='sitio santo antonio') |> upper_case_char()
