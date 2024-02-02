library(dplyr)
library(data.table)
devtools::load_all(".")


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


# 2º condição
df <- df |>
  arrange(dt_nasc, ds_nome_mae_sound) |>
  group_by(dt_nasc, ds_nome_mae_sound) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_mae_sound) &
                          ((ds_nome_mae_sound == lag(ds_nome_mae_sound)) | (ds_nome_mae_sound == lead(ds_nome_mae_sound))),
                        n(), NA))  |>
  mutate(regra2 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, cur_group_id(), NA)) |>
  ungroup() |>
   select(-N_par) #|>
  # vitallinkage::meio_de_campo()

par_2_t <-df |>  meio_de_campo()




par_2_t$par_1
a <- df |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
b <- a |> filter(is.na(regra2) & regra1 == 1)
c <- a |> filter(ds_nome_pac_sound%in%c("A535C256L500"))


d <- df_2 |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
f <- d |> filter(is.na(regra1) & regra2 == 1)
e <- d |> filter(ds_nome_pac_sound%in%c("A535C256L500"))

vitaltable::tab_1()







library(data.table)
traduzir_stata <- function(data) {
  data <- as.data.table(data)
  # Ordenar por par_2 e criar uma nova variável N
  data[, N := .N, by = par_2]

  # Substituir par_2 por . se N for igual a 1
  data[N == 1, par_2 := .]

  # Resumir par_1
  data[, sum(par_1), by = par_2]

  # Substituir par_2 por par_2 + r(max) se r(N) for diferente de 0
  data[r(N) != 0, par_2 := par_2 + r(max)]

  # Remover N e N_par
  data[, c("N", "N_par") := NULL]

  # Contar o número de linhas em que par_1 é diferente de par_1[_n-1] e par_2 é igual a par_2[_n-1] e par_2 é diferente de . e par_1[_n-1] é diferente de .
  n_anterior <- data[1, par_1]
  n_atual <- data[, par_1]
  n_proximo <- data[, .N, by = par_2]
  data[, count := (n_atual != n_anterior & par_2 == par_2[.I-1] & par_2 != . & n_anterior != .)]

  # Loop para substituir par_1 por par_1[_n-1] se as condições forem satisfeitas
  while (any(data$count > 0)) {
    data[count > 0, par_1 := n_anterior]
    n_anterior <- data[1, par_1]
    data[, count := (n_atual != n_anterior & par_2 == par_2[.I-1] & par_2 != . & n_anterior != .)]
  }

  # Contar o número de linhas em que par_1 é diferente de par_1[_n+1] e par_2 é igual a par_2[_n+1] e par_2 é diferente de . e par_1[_n+1] é diferente de .
  n_anterior <- data[nrow(data), par_1]
  n_atual <- data[, par_1]
  n_proximo <- data[, .N, by = par_2]
  data[, count := (n_atual != n_proximo & par_2 == par_2[.I+1] & par_2 != . & n_proximo != .)]

  # Loop para substituir par_1 por par_1[_n+1] se as condições forem satisfeitas
  while (any(data$count > 0)) {
    data[count > 0, par_1 := n_proximo]
    n_anterior <- data[nrow(data), par_1]
    data[, count := (n_atual != n_proximo & par_2 == par_2[.I+1] & par_2 != . & n_proximo != .)]
  }

  # Substituir par_1 por par_2 se par_2 for diferente de . e par_1 for igual a .
  data[par_2 != . & par_1 == ., par_1 := par_2]

  # Ordenar por par_1 e criar uma nova variável N_p
  data[, N_p := .N, by = par_1]

  # Remover N_p
  data[, N_p := NULL]

  # Retornar o resultado
  data
}




data <- data %>%
  group_by(par_2) %>%
  mutate(N = n()) %>%
  mutate(par_2 = ifelse(N == 1, NA, par_2))

summary <- data %>%
  summarise(par_1_summary = summary(par_1))

data <- data %>%
  group_by(par_2) %>%
  mutate(par_2 = ifelse(n() != 0, par_2 + max(summary$par_1_summary[4]), par_2))

data <- data %>%
  select(-N_par, -N)

while(sum(data$par_1 != lag(data$par_1) & data$par_2 == lag(data$par_2) & !is.na(lag(data$par_2)) & lag(data$par_1) != NA) > 0) {
  data <- data %>%
    mutate(par_1 = ifelse(par_1 != lag(par_1) & par_2 == lag(par_2) & !is.na(lag(par_2)) & lag(par_1) != NA, lag(par_1), par_1))

  data <- data %>%
    group_by(par_2) %>%
    mutate(N = n()) %>%
    filter(par_1 != lag(par_1) & par_2 == lag(par_2) & !is.na(lag(par_2)) & lag(par_1) != NA)
}

while(sum(data$par_1 != lead(data$par_1) & data$par_2 == lead(data$par_2) & !is.na(lead(data$par_2)) & lead(data$par_1) != NA) > 0) {
  data <- data %>%
    mutate(par_1 = ifelse(par_1 != lead(par_1) & par_2 == lead(par_2) & !is.na(lead(par_2)) & lead(par_1) != NA, lead(par_1), par_1))

  data <- data %>%
    group_by(par_2) %>%
    mutate(N = n()) %>%
    filter(par_1 != lead(par_1) & par_2 == lead(par_2) & !is.na(lead(par_2)) & lead(par_1) != NA)
}

data <- data %>%
  mutate(par_1 = ifelse(is.na(par_2) & !is.na(par_1), par_2, par_1))

data <- data %>%
  group_by(par_1) %>%
  mutate(N_p = n()) %>%
  mutate(par_1 = ifelse(N_p == 1, NA, par_1))

data <- data %>%
  select(-N_p)




# df <- df %>%
#   mutate(par_final = ifelse(!is.na(par_2), par_2,
#                             ifelse(!is.na(par_1), par_1, NA)),
#          par_final = ifelse(is.na(par_final), par_1, par_final))




concat_3 <- do_linkage_meiodecampo3(df)

a <- df |> select(par_1, par_2, chave_unica, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
d<- a |> filter(dt_nasc == "1923-02-23")

b <- a |> filter(is.na(regra2) & regra1 == 1)

c <- df |> filter(par_2 == 227) |> select(par_1, par_2, ds_nome_pac, dt_nasc, banco,regra1, regra2)
