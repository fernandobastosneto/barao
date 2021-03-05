#' Último ano da base de dados
#'
#' @param pais um país
#'
#' @return o último ano disponível na base de dados "comerciomundo"
#'
#' @export

comerciomundo_get_ultimoano <- function(pais) {

  comerciomundo_dados_corrente(pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::distinct(year) %>%
    dplyr::pull(year)

}
