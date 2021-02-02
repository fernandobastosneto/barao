comerciobr_get_ultimomes <- function() {

  ultimomes <- comerciobr_dados_corrente("China", "mensal") %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_mes == max(co_mes)) %>%
    dplyr::distinct(co_mes) %>%
    dplyr::pull(co_mes)

  ultimomes
}
