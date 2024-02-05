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
  mutate(regra2 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, cur_group_id(), NA)) |>
  ungroup() #|>
 #  select(-N_par) #|>
  # vitallinkage::meio_de_campo()

par_2_t <-df |>  meio_de_campo()

df_2 <- df |> traduzir_stata()


par_2_t$par_1
a <- df |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
b <- a |> filter(is.na(regra2) & regra1 == 1)
c <- a |> filter(ds_nome_pac_sound%in%c("A535C256L500"))


d <- df_2 |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
f <- d |> filter(is.na(regra1) & regra2 == 1)
e <- d |> filter(ds_nome_pac_sound%in%c("A535C256L500"))

vitaltable::tab_1()





#
#
#
#
#
# # Cria uma nova variável N que conta o número de observações por par_2
# data <- df %>%
#   group_by(par_2) %>%
#   mutate(N = n())
#
# # Substitui par_2 por NA se N for igual a 1
# data$par_2[data$N == 1] <- NA
#
# # Substitui par_2 por par_2 + max(par_1) se N for diferente de 0
# data$par_2[data$N != 0] <- data$par_2 + max(data$par_1, na.rm = TRUE)
#
# # Remove as variáveis N_par e N
# data <- data %>%
#   select(-N_par, -N)
#
# # Loop que substitui par_1 por par_1 da observação anterior se as condições forem atendidas
# while(sum(data$par_1 != dplyr::lag(data$par_1) & data$par_2 == dplyr::lag(data$par_2) & !is.na(data$par_2) & dplyr::lag(data$par_1) != NA, na.rm = TRUE) > 0) {
#   data$par_1[data$par_1 != dplyr::lag(data$par_1) & data$par_2 == dplyr::lag(data$par_2) & !is.na(data$par_2) & dplyr::lag(data$par_1) != NA] <- dplyr::lag(data$par_1)
# }
#
# # Loop que substitui par_1 por par_1 da próxima observação se as condições forem atendidas
# while(sum(data$par_1 != dplyr::lead(data$par_1) & data$par_2 == dplyr::lead(data$par_2) & !is.na(data$par_2) & dplyr::lead(data$par_1) != NA, na.rm = TRUE) > 0) {
#   data$par_1[data$par_1 != dplyr::lead(data$par_1) & data$par_2 == dplyr::lead(data$par_2) & !is.na(data$par_2) & dplyr::lead(data$par_1) != NA] <- dplyr::lead(data$par_1)
# }
#
# # Substitui par_1 por par_2 se par_2 for diferente de NA e par_1 for igual a NA
# data$par_1[!is.na(data$par_2) & is.na(data$par_1)] <- data$par_2
#
# # Cria uma nova variável N_p que conta o número de observações por par_1
# data <- data %>%
#   group_by(par_1) %>%
#   mutate(N_p = n())
#
# # Substitui par_1 por NA se N_p for igual a 1
# data$par_1[data$N_p == 1] <- NA
#
# # Remove a variável N_p
# data <- data %>%
#   select(-N_p)
#
#
#
#
# a <- data |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
# b <- a |> filter(is.na(regra1) & regra2 == 1)
# c <- data |> filter(ds_nome_pac_sound %in% c('A342B465S410', 'A254F655L220')) |> select(par_1, par_2, ds_nome_pac, dt_nasc, banco,regra1, regra2)



# df <- df %>%
#   mutate(par_final = ifelse(!is.na(par_2), par_2,
#                             ifelse(!is.na(par_1), par_1, NA)),
#          par_final = ifelse(is.na(par_final), par_1, par_final))




concat_3 <- do_linkage_meiodecampo3(df)

#a <- df |> select(par_1, par_2, chave_unica, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )


