#' @export
#'
#' Consolida o nome dos blocos.
#'

get_bloco <- function(paises) {

  bloco <- comerciobr::dic_blocos %>%
    dplyr::filter(no_pais %in% paises) %>%
    dplyr::distinct(no_bloco) %>%
    dplyr::pull(no_bloco)

  bloco
}
