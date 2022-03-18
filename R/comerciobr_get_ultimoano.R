#' Último ano da base de dados
#'
#' @return o último ano disponível na base de dados "comerciobr"
#'
#' @export
comerciobr_get_ulimoano <- function() {

  comerciobr::sh1_df %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::distinct(co_ano) %>%
    dplyr::pull(co_ano)

}
