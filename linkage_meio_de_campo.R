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

# Carregando a base
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData')

# Ajustando e criando um sample
concat <-
  concat |> vitallinkage::ajuste_data(2)

concat$nu_cns <- as.integer(concat$nu_cns)

concat$cd_autorizador_doc <- as.integer(concat$cd_autorizador_doc)

concat_2 <- subset(concat, grepl("^A", ds_nome_pac, ignore.case = TRUE))


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

max_par_1 <- max(df$par_1, na.rm = TRUE)

# 2º condição
df <- df |>
  arrange(dt_nasc, ds_nome_mae_sound) |>
  group_by(dt_nasc, ds_nome_mae_sound) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_mae_sound) &
                          ((ds_nome_mae_sound == lag(ds_nome_mae_sound)) | (ds_nome_mae_sound == lead(ds_nome_mae_sound))),
                        n(), NA))  |>
  mutate(regra2 = ifelse(!is.na(N_par) & N_par > 1, 1, NA),
         par_2 = ifelse(!is.na(N_par) & N_par > 1, cur_group_id() + max_par_1, NA)) |>
  ungroup()

gc()



unique_par_temp <-
unique_par_2 <-

# Encontrar valores que são iguais em ambas as colunas
valores_iguais <- intersect(unique(h$par_temp), unique(h$par_2))


# meio de campo
par_list <- df |>
  distinct(par_1, par_2) |>
  filter(!is.na(par_1) ) |>
  rename('par_temp' = par_1)

h <- df |>select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2)

c <- df |> filter(ds_nome_pac_sound %in% c('A342B465S410', 'A254F655L220', 'A655F253S412', 'A535C250S410')) |> select(par_1, par_2, ds_nome_pac,ds_nome_pac_sound, dt_nasc, banco,regra1, regra2)


g <- left_join(c, par_list, by = "par_2", type = "inner") |>
  mutate(par_final = coalesce(par_temp, par_2))




# Identify rows with duplicate values in par_2 in the left data frame (c)
duplicate_rows_left <- c %>%
  group_by(par_2) %>%
  filter(n() > 1)

# Identify rows with duplicate values in par_2 in the right data frame (par_list)
duplicate_rows_right <- par_list %>%
  group_by(par_2) %>%
  filter(n() > 1)


a <- df |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
b <- a |> filter(is.na(regra1) & regra2 == 1)

c <- df |> filter(ds_nome_pac_sound %in% c('A342B465S410', 'A254F655L220', 'A655F253S412')) |> select(par_1, par_2, ds_nome_pac, dt_nasc, banco,regra1, regra2)


