#' @export

comerciomundo_get_ultimoano <- function(pais) {

  comerciomundo_dados_corrente(pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::distinct(year) %>%
    dplyr::pull(year)

}
