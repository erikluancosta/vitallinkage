#' Cria a coluna ANO a aprtir de DTOBITO
#'
#' @param df Nome do data frame
#'
#' @return Retorna uma coluna nova chamada ANO
#' @export
ano_sim <- function(df){

  df<- df |>
    dplyr::mutate(ANO=substr(DTOBITO,5,8))

}
