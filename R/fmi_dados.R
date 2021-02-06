#' @export
fmi_dados <- function(pais, indicador) {

  paisoriginal <- pais
  pais_fmi <- get_pais(pais, "fmi")

  indicador %>%
    dplyr::filter(pais == pais_fmi) %>%
    dplyr::mutate(pais = paisoriginal)

}
