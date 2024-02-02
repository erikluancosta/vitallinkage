#' Função meio_de_campo
#'
#' Esta função manipula um quadro de dados com base em condições e operações específicas.
#'
#' @param df Um data frame contendo os dados de entrada.
#'
#' @return Um data frame modificado com ajustes feitos de acordo com as condições especificadas.
#'
#' @export
meio_de_campo <- function(df) {

  df <- df |>
    dplyr::group_by(par_2) |>
    mutate(N = n()) |>
    dplyr::mutate(par_2 = ifelse(N == 1, NA, par_2)) |>
    dplyr::mutate(par_2 = ifelse(N != 0, par_2 + ifelse(all(is.na(par_1)), 0, max(par_1, na.rm=T)), par_2)) |>
    dplyr::select(-N) |>
    dplyr::mutate(par_1 = ifelse(!is.na(par_2) & is.na(par_1), par_2, par_1)) |>
    dplyr::mutate(par_1 = ifelse(!is.na(par_1), par_1, ifelse(!is.na(par_2) & is.na(par_1), par_2, par_1))) |>
    dplyr::arrange(par_1) |>
    dplyr::group_by(par_1) |>
    dplyr::mutate(N_p = n()) |>
    dplyr::mutate(par_1 = ifelse(N_p == 1, NA, par_1)) |>
    dplyr::ungroup()


  return(df)
}
