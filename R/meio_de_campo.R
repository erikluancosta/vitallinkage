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

  par_list <- df |>
    dplyr::distinct(par_1, par_2) |>
    dplyr::filter(!is.na(par_1), !is.na(par_2)) |>
    dplyr::rename('par_temp' = par_1)

  par_list <- par_list |>
    dplyr::mutate(
      N_par = ifelse((par_temp == dplyr::lag(par_temp)) | (par_2 == dplyr::lead(par_2)), n(), NA)
      )

  par_list <- par_list |>
    dplyr::arrange(par_2) |>
    dplyr::group_by(par_2) |>
    dplyr::mutate(
      par_final = ifelse(
        (par_2 == dplyr::lead(par_2) | (par_temp == dplyr::lead(par_temp))) | (par_2 == dplyr::lag(par_2) | (par_temp == dplyr::lag(par_temp) & N_par>1)),
        dplyr::lead(par_temp),
        par_2
      ),
      par_final = ifelse(
        is.na(par_final),  # Se o valor de par_final for NA
        ifelse(
          !is.na(par_final) & par_final == dplyr::lead(par_temp),  # E se par_temp for igual ao valor anterior ou posterior de par_temp
          par_temp,  # Preenche com par_temp
          par_final  # Caso contrário, mantém o valor de par_2
        ),
        par_final  # Se par_final já tiver um valor, mantém esse valo
      )
    )

  par_list <- par_list |>
    dplyr::mutate(
      par_final_final = dplyr::coalesce(par_final, par_temp, par_2)
    ) |>
    dplyr::distinct(par_final_final, par_2)


  nova_linha <- data.frame(matrix(NA, ncol = ncol(par_list), nrow = 1))
  # Atribuir nomes de coluna à nova linha, se necessário
  colnames(nova_linha) <- colnames(par_list)
  # Adicionar a nova linha ao final do dataframe existente
  par_list <- rbind(par_list, nova_linha)

  df <- dplyr::left_join(df, par_list, by = "par_2") |>
    dplyr::mutate(par_final = dplyr::coalesce(par_final_final, par_1, par_2)) |>
    dplyr::select(-par_1) |>
    dplyr::rename("par_1" = par_final_final)

  return(df)
}
