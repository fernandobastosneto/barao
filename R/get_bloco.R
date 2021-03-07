#' Fun\u00e7\u00e3o que devolve um bloco associado a um pa\u00eds (ou lista de pa\u00edses)
#'
#' @param paises um pa\u00eds ou vetor de pa\u00edses
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
