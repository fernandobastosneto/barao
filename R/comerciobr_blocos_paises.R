#'
#' Lista de países a partir de um bloco
#'
#' @param bloco o nome de um bloco, de acordo com a base dic_blocos do pacote "comerciobr2"
#'
#' @return lista de países associados a determinado bloco
#'
#' @export
comerciobr_blocos_paises <- function(bloco) {

    paises <- comerciobr2::dic_blocos %>%
      dplyr::filter(no_bloco == bloco) %>%
      dplyr::distinct(no_pais) %>%
      dplyr::pull(no_pais)

}


