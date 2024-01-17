#' Cria coluna com o id único para cada registro ('nome da base + 1:n)
#'
#' @param df Nome do dataframe
#' @param nome_base Nome da base, exemplos: "SINAN", "SIM" ou "SIH"
#'
#' @return Ao aplicar a função, retornará uma nova coluna composta por "id" + nome da base indicada no parâmetro 'nome_base'
#' @export
#'
#' @examples
criar_coluna_id <- function(df, nome_base) {

  coluna_id <- paste0("id_", nome_base)
  n <- nrow(df)
  df[[coluna_id]] <- paste0(nome_base,'_', 1:n)
  return(df)

}
