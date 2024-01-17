#' Padroniza as variáveis
#'
#' @param df Nome do dataframe
#' @param nome_base nome da base, exemplo: "SINAN", "SIM" ou "SIH"
#'
#' @return Retora os nomes das variáveis padronizadas de acordo com a lista namestand
#'
#' @examples
padroniza_variaveis <- function(df, df2 = stanard_name,nome_base) {
  df2<-vitallinkage::df2
  # nomes das variáveis originais e padronizadas filtradas para o SINAN
  names <- namestand |>
    dplyr::filter(fonte == nome_base) |>
    dplyr::select(var_names_orig, df2)

  # Seleciona as variáveis do dataframe original
  df <- df |>
    dplyr::select(names$var_names_orig)

  # Renomeia as colunas do dataframe com os valores de stanard_name
  names(df) <- names$df2

  return(df)
}
