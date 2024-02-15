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
# Record linkage


# do_linkage_meiodecampo3 <- function(df) {
#
#   df <- df |>
#     group_by(par_2) |>
#     mutate(N = n()) |>
#     mutate(par_2 = ifelse(N == 1, NA, par_2)) |>
#     mutate(par_2 = ifelse(N != 0, par_2 + ifelse(all(is.na(par_1)), 0, max(par_1, na.rm=T)), par_2)) |>
#     select(-N) |>
#     mutate(par_1 = ifelse(!is.na(par_2) & is.na(par_1), par_2, par_1)) |>
#     arrange(par_1) |>
#     group_by(par_1) |>
#     mutate(N_p = n()) |>
#     mutate(par_1 = ifelse(N_p == 1, NA, par_1)) |>
#     ungroup()
#
#
#   return(df)
# }

load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData')

concat <-
  concat |> vitallinkage::ajuste_data(2)

concat$nu_cns <- as.integer(concat$nu_cns)

concat$cd_autorizador_doc <- as.integer(concat$cd_autorizador_doc)

concat_2 <- subset(concat, grepl("^A", ds_nome_pac, ignore.case = TRUE))

gc()
# 1º condição
df <- concat_2 |>
  arrange(ds_nome_pac, dt_nasc) |>
  group_by(ds_nome_pac, dt_nasc) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_pac) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))), n(), NA))  |>
  mutate(regra1 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_1 = cur_group_id() * (N_par > 1)) |>
  ungroup() |>
  select(-N_par) |>
  mutate(par_c1 = par_1) |>
  arrange(par_1, banco)


# 2º condição
df <- df |>
  arrange(dt_nasc, ds_nome_mae_sound) |>
  group_by(dt_nasc, ds_nome_mae_sound) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_mae_sound) &
                          ((ds_nome_mae_sound == lag(ds_nome_mae_sound)) | (ds_nome_mae_sound == lead(ds_nome_mae_sound))),
                        n(), NA))  |>
  mutate(regra2 = ifelse(!is.na(N_par) & N_par > 1, 1, NA),
        par_2 = ifelse(!is.na(N_par) & N_par > 1, cur_group_id() + max_par_1, NA)) |>
  ungroup() |>
  mutate(par_c2 = par_2)

par_2_t <- df |>  vitallinkage::meio_de_campo()


# 3º condição
