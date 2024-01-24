#' Cria faixa etária
#'
#' @param df nome do Data frame
#'
#' @return Retorna uma coluna chamada faixa_etaria com as idades de
#' 10 a 19 anos, 20 a 29 anos, 30 a 59 anos e 60+
#' @export
faixa_etaria <- function(df){
  df <- df |>
    dplyr::mutate(faixa_etaria=dplyr::case_when(
      nu_idade_anos<10~"0 a 9 anos",
      nu_idade_anos>09&nu_idade_anos<20~"10 a 19 anos",
      nu_idade_anos>19&nu_idade_anos<30~"20 a 29 anos",
      nu_idade_anos>29&nu_idade_anos<60~"30 a 59 anos",
      nu_idade_anos>59~"60+",
      T~as.character(nu_idade_anos)
    )
    )

}
