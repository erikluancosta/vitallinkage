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
dados_para_linkage_a <- concat_2
save(dados_para_linkage_a, file="C:/vitalstrategies/data_sicence/TCC/SCRIPTS_LINKAGE/Erik/base_para_linkage_letra_a.RData")
# 1º condição
df <- concat |>
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
  ungroup() |>
  mutate(par_c2 = par_2)

gc()

df <- df |>
  vitallinkage::meio_de_campo()

df |>


l <-par_list |> filter(par_2 %in% c(44515, 79820) | par_temp %in% c(79820, 44515)) |> select(par_temp, par_2)

# Encontrar valores que são iguais em ambas as colunas
valores_iguais <- intersect(unique(par_list$par_temp), unique(par_list$par_2))
duplicated_values <- par_list$par_2[duplicated(par_list$par_2)]

################
# MEIO DE CAMPO
################
par_list <- df |>
  distinct(par_1, par_2) |>
  filter(!is.na(par_1), !is.na(par_2)) |>
  rename('par_temp' = par_1)
nova_linha <- data.frame(matrix(NA, ncol = ncol(par_list), nrow = 1))
# Atribuir nomes de coluna à nova linha, se necessário
colnames(nova_linha) <- colnames(par_list)
# Adicionar a nova linha ao final do dataframe existente
par_list <- rbind(par_list, nova_linha)

par_list_2 <- par_list |> filter(par_temp%in%c(869,949))

par_list_2 <- par_list_2 %>%
  mutate(N_par = ifelse((par_temp == lag(par_temp)) | (par_2 == lead(par_2)), n(), NA))

par_list_2 <- par_list_2 %>%
  group_by(par_2) %>%
  mutate(
    par_final = ifelse(
      (par_2 == lead(par_2) | (par_temp == lead(par_temp))) | (par_2 == lag(par_2) | (par_temp == lag(par_temp) & N_par>1)),
      lead(par_temp),
      par_2
    ),
    par_final = ifelse(
      is.na(par_final),  # Se o valor de par_final for NA
      ifelse(
        !is.na(par_final) & par_final == lead(par_temp),  # E se par_temp for igual ao valor anterior ou posterior de par_temp
        par_temp,  # Preenche com par_temp
        par_final  # Caso contrário, mantém o valor de par_2
      ),
      par_final  # Se par_final já tiver um valor, mantém esse valo
    )
  )


par_list_2 <- par_list_2 |>
  mutate(
    par_final_final = coalesce(par_final, par_temp, par_2)
  ) |>
  distinct(par_final_final, par_2)

# Encontre as linhas onde par_2 é repetido
indices_repetidos <- duplicated(par_list$par_2)

# Filtrar o dataframe para incluir apenas as linhas onde par_2 é repetido
par_list_2 <- par_list[duplicated(par_list$par_2) | duplicated(par_list$par_2, fromLast = TRUE), ]

c <- df |> filter(ds_nome_pac_sound %in% c('A342B465S410', 'A254F655L220', 'A655F253S412', 'A535C250S410', 'A535R362S410', 'A342B260S410') | par_2 %in% c(48133, 18801, 406590, 441681)| par_1%in%c(869,949)) |> select(par_1, par_c1, par_2, par_final, ds_nome_pac,ds_nome_pac_sound, dt_nasc, banco,regra1, regra2)


g <- left_join(df, par_list, by = "par_2", relationship = "many-to-many") |>
  mutate(par_final = coalesce(par_temp, par_1, par_2))

h <- df |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, ds_nome_mae, dt_nasc, banco,regra1, regra2) |> filter(par_1%in%c(869,949))



v <- g |> filter(is.na(regra2) & regra1 == 1)


# Identificar as linhas problemáticas no dataframe df
linhas_problematicas_df <- df %>%
  semi_join(par_list, by = "par_2") %>%
  filter(duplicated(par_2) | duplicated(par_1))

# Identificar as linhas problemáticas no dataframe par_list
linhas_problematicas_par_list <- par_list %>%
  semi_join(df, by = "par_2") %>%
  filter(duplicated(par_2) | duplicated(par_temp))

par_list |> filter(par_2 == 242715)

duplicated(par_list$par_2)

par_fil %>%
  group_by(par_2) %>%
  mutate(N = n())
par_list





