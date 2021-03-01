#' @export
#'

# TODO - é necessário estabelecer critérios mais claros dos grupos de países.

comerciobr_grupos_paises <- function(grupo) {
  lista_blocos <- comerciobr::dic_blocos %>%
    dplyr::distinct(no_bloco) %>%
    dplyr::pull(no_bloco)

  if (grupo %in% lista_blocos) {
    paises <- comerciobr::dic_blocos %>%
      dplyr::filter(no_bloco == grupo) %>%
      dplyr::distinct(no_pais) %>%
      dplyr::pull(no_pais)
    return(paises)
  }

  else {
    print("Bloco não identificado, tente ajustar o grupo de acordo com a lista.")
  }

}

# comerciobr::dic_blocos %>%
#   dplyr::filter(stringr::str_detect(no_bloco, "União Europeia"))
#
# comerciobr::dic_blocos %>%
#   dplyr::distinct(no_bloco) %>%
#   dplyr::filter(no_bloco != "Brasil")
