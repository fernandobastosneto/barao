#' \u00daltimo ano da base de dados
#'
#' @param pais um pa\u00eds
#'
#' @return o \u00faltimo ano dispon\u00edvel na base de dados "comerciomundo"
#'
#' @export

comerciomundo_get_ultimoano <- function(pais) {

  comerciomundo_dados_corrente(pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::distinct(year) %>%
    dplyr::pull(year)

}
