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
library(vitaltable)
devtools::load_all(".")
options(scipen = 999)

## ACOMPANHAMENTO DAS REGRAS
# Cria lista de colunas que começam com "par_c"
colunas_par_c <- grep("^par_c6", names(df_t), value = TRUE)

# Dataframe com colunas selecionadas para acompanhamento
acompanhamento <- df_t |>  select(
  par_1, par_c64,
  #all_of(colunas_par_c),
  ds_nome_pac, dt_nasc, ds_nome_mae,
  nu_cpf, nu_cns, cd_mun_not ,banco
)

# Casos novos identificados na regra
novos <- vitallinkage::casos_novos(df_t, par_c64)


# Registros identificados na regra que já foram identificados antes
complementares <-
  vitallinkage::filtro_par_c_especifico(df_t, 'par_c64') |>
  select(
    par_1, all_of(colunas_par_c),
    ds_nome_pac, dt_nasc, dt_obito, ds_nome_mae,
    nu_cpf, nu_cns, cd_mun_not ,banco
  )




## ACOMPANHAMENTO DE DUPLICADOS NO SIM
colunas_par_c <- grep("^par_c", names(df_2), value = TRUE)
sim_contagem <- df_2 |> filter(banco == 'SIM') |>  group_by(par_1) |> summarise(contagem = n()) |> filter(contagem>1, !is.na(par_1))
sim_duplicado <- df_2 |> filter(par_1%in%sim_contagem$par_1) |>
  select(
    par_1,#all_of(colunas_par_c),
    banco, ds_nome_pac, dt_nasc, dt_obito,
    ds_nome_mae, nu_cns, nu_doc,nu_do, nu_tel, cd_causabas, ds_rua_res, cd_atestante
  ) |>
  filter(banco == 'SIM')


## ACOMPANHAMENTO DE POSSIVEIS ERROS
# mais de 20x o valor de par_1
muitos_registros <- df_2 |>
  filter(!is.na(par_1)) |>
  group_by(par_1) |>
  summarise(contagem = n()) |>
  filter(contagem > 20) |>
  arrange(-contagem)


# df com os maiores de par_1
mais_frequentes <- df_2 |>
  filter(par_1%in%muitos_registros$par_1) |>
  select(par_1, banco, ds_nome_pac, dt_nasc, dt_obito,
         ds_nome_mae, nu_cns, nu_doc,nu_do, nu_tel, cd_causabas, ds_rua_res, cd_atestante)

