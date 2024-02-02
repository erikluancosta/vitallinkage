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

########
### SIM
########
# leitura da base especifica para cada caso
# Minha pasta com os dados
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

sim <- vitallinkage::padroniza_SIM(sim_raw)
rm(sim_raw)
sim$ds_nome_pac
########
### SIH
########
# leitura da base especifica para cada caso
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SIH/sih_final_2016_2021.Rdata')

sih <- vitallinkage::padroniza_SIH(sih_final_csv)
rm(sih_final_csv)
#sih_2 <- sih |> select(ds_nome_pac, nome_original, ds_nome_mae, nome_mae_original)

##########
### SINAN
##########
# leitura da base especifica para cada caso
sinan <- read.dbf('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SINAN/VIOLENET.DBF', as.is = TRUE)

sinan <- vitallinkage::padroniza_SINAN(sinan)

#################
### CONCATENANDO
#################

list_of_dfs <- list(sih,sim,sinan)  # Add more data frames if needed
concat <- bind_rows(list_of_dfs)
rm(list_of_dfs)

save(concat, file = "C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData")

concat <-
  concat |> vitallinkage::ajuste_data(2)

concat$nu_cns <- as.integer(concat$nu_cns)

concat$nu_cns <- as.integer(concat$nu_cns)
concat$cd_autorizador_doc <- as.integer(concat$cd_autorizador_doc)


concat_2 <- subset(concat, grepl("^A", ds_nome_pac, ignore.case = TRUE))
concat_2 |> select(nu_doc)


#####
# b <-concat |> select(nu_cns)
#
# a<- vitallinkage::namestand |> select(fonte,var_names_orig, stanard_name)
#
# # VERIFICAÇÕES
# a <- tab_2(concat, ds_nome_pac, banco)
#
# b <-  a[grepl("GEMEL", a$ds_nome_pac), ]
#
# ## DOCUMENTOS
# c <- tab_2(concat, ds_nome_pac, banco)
#
# d <-  a[grepl("GEMEL", a$ds_nome_pac), ] # deixar paraa próxima semana

# Tratar os documentos. CPF em primeiro momento, CNS em segundo

#####

# regra númeo 1 - data de nascimento e cpf igual. Usar nu_cns
# TENTATIVA DA PRIMEIRA REGRA

dados <- concat_2 |>
  arrange(ds_nome_pac, dt_nasc) |>
  group_by(ds_nome_pac, dt_nasc) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_pac) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))), n(), NA)) |>
  filter(!is.na(N_par)) |>
  mutate(par_1 = cur_group_id() * (N_par > 1),
         par_c1 = par_1) |>
  arrange(par_1, banco) |>
  select(-N_par) |>
  ungroup() |>
  select(par_1, id_SIH, id_SIM, id_SINAN) |>
  right_join(concat_2, by = c('id_SIH', 'id_SIM', 'id_SINAN'))

table(regra_2$par_2)


#########################

regra_2 <- concat_2 |>
  arrange(dt_nasc, dt_obito, morreu, ds_nome_pac_sound) |>
  group_by(dt_nasc, dt_obito, morreu, ds_nome_pac_sound) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_pac_sound) &
                          ((ds_nome_pac_sound == lag(ds_nome_pac_sound)) | (ds_nome_pac_sound == lead(ds_nome_pac_sound))), n(), NA)) |>
  filter(!is.na(N_par)) |>
  mutate(par_1 = cur_group_id() * (N_par > 1),
         par_c1 = par_1) |>
  arrange(par_1, banco) |>
  select(-N_par) |>
  ungroup() |>
  select(par_1, id_SIH, id_SIM, id_SINAN) |>
  right_join(concat_2, by = c('id_SIH', 'id_SIM', 'id_SINAN'))

##########################
dados$dt_nasc
dados <- dados |>
  vitallinkage::ajuste_doc()

# Parte 1
dados <- dados |>
  arrange(dt_nasc, dt_obito, morreu, ds_nome_pac_sound) |>
  group_by(dt_nasc, dt_obito, morreu, ds_nome_pac_sound) |>
  mutate(
    N_par = ifelse(
      !is.na(dt_nasc) & !is.na(dt_obito) & !is.na(morreu) & !is.na(ds_nome_pac_sound) &
        ((nu_doc - lag(nu_doc) == 0) | (nu_doc - lag(nu_doc) == nu_doc) | (nu_doc - lag(nu_doc) == lag(nu_doc))),
      n(), NA
    ),
    par_2 = row_number() * (sum(!is.na(N_par) & N_par > 1) > 0)
  ) |>
  ungroup() |>
  filter(!is.na(N_par)) |>
  arrange(par_2) |>
  select(-N_par) |>
  right_join(dados, by = c('id_SIH', 'id_SIM', 'id_SINAN'))

a <- dados |> select(par_1.x, par_1.y)


# Parte 2 (do_linkage_meiodecampo_evento_unico)
dados <- dados |>
  group_by(par_2) |>
  mutate(N = n()) |>
  mutate(par_2 = ifelse(N == 1, NA, par_2)) |>
  summarise(max_par_2 = max(par_2)) |>
  mutate(par_2 = par_2 + max_par_2) |>
  select(-max_par_2) |>
  ungroup() |>
  select(-N) |>
  mutate(par_c3 = par_2)

# Parte 3 (do_linkage_meiodecampo_evento_unico)
dados <- dados |>
  group_by(par_1) |>
  mutate(N_p = n()) |>
  mutate(par_1 = ifelse(N_p == 1, NA, par_1)) |>
  select(-N_p)



dados$nu_doc

dados_select <- dados |> ungroup() |> select(par_1,id_SIH, id_SIM, id_SINAN)


teste <- dados |> filter(is.na(par_1))

teste_2 <- teste |> select(par_1,ds_nome_pac, ds_nome_mae, dt_notific, nu_cns, dt_nasc, dt_obito, banco, id_SIH, id_SIM, id_SINAN)
teste$ds_nome_p
teste$ds_nome_p

a <- dados |> filter(par_1%in%c(3134))
