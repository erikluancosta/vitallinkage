#' Ajuste nas colunas de texto
#'
#' @param df Dataframe a ser ajustado
#'
#' @return Retorna o dataframe com os ajustes realizados
#' @export
ajuste_txt <- function(df){

  # Remove mais de um espaço e ajusta nas colunas de texto
  for (coluna in names(df)) {
    if (is.character(df[[coluna]])) {
      # Verifica se o nome da coluna contém "nome" ou "NOME"
      if (grepl("nome", coluna, ignore.case = TRUE)) {
        # Translitera caracteres acentuados
        df[[coluna]] <- iconv(df[[coluna]], "UTF-8", "ASCII")
        df[[coluna]] <- toupper(df[[coluna]])

        # Remove espaços extras
        df[[coluna]] <- gsub("\\s{2,}", " ", df[[coluna]])

        # Ajustes específicos para colunas contendo "nome" ou "NOME"
        df[[coluna]] <- gsub("RECEM NASCIDO |RN NASCIDO |NATIMORTO", "", df[[coluna]])
        df[[coluna]] <- stringr::str_squish(df[[coluna]])
        df[[coluna]] <- gsub(" FILHO| NETO| SOBRINHO| JUNIOR", "", df[[coluna]])
        df[[coluna]] <- gsub(" DE | DOS | DA | DOS | DAS | DO | DDAS ", " ", df[[coluna]])
        # Remove caracteres não alfabéticos
        df[[coluna]] <- gsub("[^A-Za-z ]", "", df[[coluna]])
        # Troca novamente 2 ou mais espaços para um único espaço
        df[[coluna]] <- gsub("\\s{2,}", " ", df[[coluna]])
        # Se a letra repete mais de duas vezes, remove uma
        df[[coluna]] <- gsub("(.)\\1{2,}", "\\1", df[[coluna]], perl = TRUE)

      }
    }
  }

  return(df)
}
