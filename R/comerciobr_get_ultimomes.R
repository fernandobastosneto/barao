#' Último mês disponível do último ano disponível em comerciobr
#'
#' @export
comerciobr_get_ultimomes <- function() {

  ultimomes <- comerciobr::sh1_df %>%
    # dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::filter(co_mes == max(co_mes)) %>%
    dplyr::distinct(co_mes) %>%
    dplyr::mutate(co_mes = as.integer(co_mes)) %>%
    dplyr::pull(co_mes)

  ultimomes
}
