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
devtools::install_github('https://github.com/GitVitalStrategiesBr/VitalBrEpiTools')

vitaltable::tab_1()

load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData')


concat$nu_doc <- gsub("[^0-9]", NA, concat$nu_doc)
concat$nu_doc[concat$nu_doc == ""] <- NA

# Regra para iniciar o linkage
df <- concat |>
  vitallinkage::start_linkage(c('ds_nome_pac', 'dt_nasc'), "ds_nome_pac")

# Regra 2
df_2 <- df |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae_sound', 'ds_nome_pai_sound', 'dt_nasc'),
    c('ds_nome_pac'),
    2
  )


# Regra 3
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae_sound', 'ds_nome_pai_sound', 'dt_nasc'),
    c('ds_nome_pac'),
    3
  )


# Regra 5
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'nu_doc'),
    c('ds_nome_pac_sound'),
    5
  )



gc()
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


# 2º condição
df <- df |>
  arrange(dt_nasc, ds_nome_mae_sound) |>
  group_by(dt_nasc, ds_nome_mae_sound) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_mae_sound) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))),
                        n(), NA))  |>
  mutate(regra2 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c2 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()


# 3º condição
df <- df |>
  arrange(ds_nome_mae_sound, ds_nome_pai_sound, dt_nasc) |>
  group_by(ds_nome_mae_sound, ds_nome_pai_sound, dt_nasc) |>
  mutate(N_par = ifelse(!is.na(ds_nome_mae_sound) & !is.na(ds_nome_pai_sound) & !is.na(dt_nasc) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))),
                        n(), NA))  |>
  mutate(regra3 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c3 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()


# 4º condição
df <- df |>
  arrange(ds_nome_pac_sound, dt_nasc, nu_doc) |>
  group_by(ds_nome_pac_sound, dt_nasc, nu_doc) |>
  mutate(N_par = ifelse(!is.na(ds_nome_pac_sound) & !is.na(dt_nasc) & !is.na(nu_doc) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))),
                        n(), NA))  |>
  mutate(regra4 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c4 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()

# 5º condição
df <- df |>
  arrange(ds_nome_pac_sound, nu_doc) |>
  group_by(ds_nome_pac_sound, nu_doc) |>
  mutate(N_par = ifelse(!is.na(ds_nome_pac_sound) & !is.na(nu_doc) &
                          ((ds_nome_pac_sound == lag(ds_nome_pac_sound)) | (ds_nome_pac_sound == lead(ds_nome_pac_sound))),
                        n(), NA))  |>
  mutate(regra5 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c5 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()

# 6º condição
df_r6 <- df |>
  arrange(nu_doc) |>
  group_by(nu_doc) |>
  mutate(N_par = ifelse(!is.na(nu_doc) &
                          ((ds_nome_pac_sound == lag(ds_nome_pac_sound)) | (ds_nome_pac_sound == lead(ds_nome_pac_sound))),
                        n(), NA))  |>
  mutate(regra6 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c6 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()




# Criando dados fictícios
dados <- data.frame(
  nome = c(NA, "JOAO PEDRO", "MARIA SANTOS", "MARIA SANTOS","PEDRO", "ANA MARIA", "AMA MARIA", "CARLOS ADAO", "JOAO P", "JAO PEDRO"),
  data_nascimento = c("1995-01-30","1990-05-15", "1985-10-20",NA, "1988-07-12", "1992-03-08", "1992-08-03", "1995-01-30", "1990-05-15", "1990-05-15"),
  nome_mae = c("ISABEL SILVA","MARIA CLARA", "SANDRA",'SANDRA', "LUCIA MARIA", "FERNANDA", "FERNANDA", "ISABEL SILVA", NA, "MARIA CLARA"),
  banco = c('SIM', "SIH", "SINAN", 'SIH', 'SIH','SIH', 'SINAN','SIH', 'SINAN', 'SIM')
)

dados_2 <- dados |> vitallinkage::soundex_linkage('nome') |>
  vitallinkage::soundex_linkage('nome_mae') |>
  select(-nome1, -nome2, -nome3, -nome2_sound, -nome3_sound, -nome_mae1, -nome_mae2, -nome_mae3,-nome_mae1_sound, -nome_mae2_sound, -nome_mae3_sound )

dados_2 <- dados_2 |> vitallinkage::start_linkage(c("data_nascimento"), c('nome_sound'))
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("nome_sound"), c('nome_mae_sound'), 2)
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("nome_mae_sound"), c('nome_sound'), 3)
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("data_nascimento"), c('nome_mae_sound'), 4)
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("data_nascimento"), c('nome1_sound'), 5)
dados_2 <- dados_2 |> select(par_1, everything()) |> arrange(par_1)


dados_3 <- dados_2 |>
  group_by(par_1) |>
  mutate(
    data_nascimento_corrigida = ifelse(!is.na(par_1), max(data_nascimento, na.rm=TRUE), data_nascimento),
    nome_corrigido = ifelse(!is.na(par_1), max(nome, na.rm=TRUE), nome),
    nome_mae_corrigido = ifelse(!is.na(par_1), max(nome_mae, na.rm = TRUE), nome_mae)
  )

dados_final <- dados_3 |> select(par_1, nome_corrigido, nome_mae_corrigido, data_nascimento_corrigida, banco)

d <- vitaltable::tab_1(dados_final,nome_corrigido)
