#' Padroniza os nomes das variáveis
#'
#' @param df Nome do dataframe
#' @param df2 Nome da tabela com os nomes padronizados
#' @param nome_base Nome da base a ser padronizada, ex: "SIM", "SINAN" ou "SIH"
#'
#' @return Retorna o dataframe com as colunas padronizadas
#' @export
padroniza_variaveis <- function(df, df2, nome_base) {

  # nomes das variáveis originais e padronizadas filtradas para o SINAN
  names <- df2 |>
    dplyr::filter(fonte == nome_base) |>
    dplyr::select(var_names_orig, stanard_name)

  # Seleciona as variáveis do dataframe original
  df <- df |>
    dplyr::select(names$var_names_orig)

  # Renomeia as colunas do dataframe com os valores de stanard_name
  names(df) <- names$stanard_name

  return(df)
}
