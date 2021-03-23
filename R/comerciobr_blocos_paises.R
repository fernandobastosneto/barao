#'
#' Lista de países a partir de um bloco
#'
#' @param bloco o nome de um bloco, de acordo com a base dic_blocos do pacote "comerciobr"
#'
#' @return lista de países associados a determinado bloco
#'
#' @export

comerciobr_blocos_paises <- function(bloco) {

  lista_blocos <- comerciobr::dic_blocos %>%
    dplyr::distinct(no_bloco) %>%
    dplyr::pull(no_bloco)

  if (bloco %in% lista_blocos) {
    paises <- comerciobr::dic_blocos %>%
      dplyr::filter(no_bloco == bloco) %>%
      dplyr::distinct(no_pais) %>%
      dplyr::pull(no_pais)
    return(paises)
  }

  else {
    print("Bloco n\u00e3o identificado, tente ajustar o grupo de acordo com a lista.")
  }

}
