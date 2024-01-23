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
      cd_idade<10~"0 a 9 anos",
      cd_idade>09&cd_idade<20~"10 a 19 anos",
      cd_idade>19&cd_idade<30~"20 a 29 anos",
      cd_idade>29&cd_idade<60~"30 a 59 anos",
      cd_idade>59~"60+",
      T~as.character(cd_idade)
    )
    )

}
