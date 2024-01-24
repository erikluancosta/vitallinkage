ajustes_var_df <- function(df){

  df$dt_obito <- dmy(df$dt_obito)
  df$ano <- year((df$dt_obito))
  df$dt_nasc <- dmy(df$dt_nasc)
  df$cd_idade <- as.character(df$cd_idade)
  df$nu_cns <- as.character(df$nu_cns)
  df$cd_cnes_unid_not <- as.character(df$cd_cnes_unid_not)
  df$cd_mun_res <- as.integer(df$cd_mun_res)
  df$cd_cep_res <- as.character(df$cd_cep_res)

}
