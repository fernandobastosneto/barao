#' Função que devolve um bloco associado a um país (ou lista de países)
#'
#' @param paises um paíes ou vetor de países
#'
#' @export
#'

get_bloco <- function(paises) {

  bloco <- comerciobr::dic_blocos %>%
    dplyr::filter(no_pais %in% paises) %>%
    dplyr::distinct(no_bloco) %>%
    dplyr::pull(no_bloco)

  bloco
}
