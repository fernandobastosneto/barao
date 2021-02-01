comerciobr_get_ultimomes <- function() {

  ultimomes <- comerciobr_dados_corrente("China", "mensal") %>%
    dplyr::ungroup() %>%
    dplyr::filter(CO_MES == max(CO_MES)) %>%
    dplyr::distinct(CO_MES) %>%
    dplyr::pull(CO_MES)

  ultimomes
}
