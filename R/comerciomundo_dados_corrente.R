#' Dados de fluxo de comércio bilateral entre o países alvo e o mundo
#'
#' @param pais um países
#'
#' @export

comerciomundo_dados_corrente <- function(pais) {

  comerciomundo::comtrade %>%
    dplyr::filter(reporter_code == barao::get_pais(pais, "comtrade")) %>%
    dplyr::filter(partner_code == 0) %>%
    dplyr::group_by(year, trade_flow_code) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::mutate(trade_flow_code = dplyr::case_when(trade_flow_code == 1 ~ "Importacoes",
                                                     trade_flow_code == 2 ~ "Exportacoes",
                                                     TRUE ~ NA_character_)) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(names_from = trade_flow_code, values_from = value) %>%
    dplyr::mutate(Corrente = .data$Importacoes + .data$Exportacoes,
                  Saldo = .data$Exportacoes - .data$Importacoes) %>%
    tidyr::pivot_longer(.data$Importacoes:.data$Saldo, names_to = "trade_flow", values_to = "value")

}


