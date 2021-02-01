comerciobr_grafico_corrente <- function(pais, periodo) {

  comerciobr_dados_corrente(pais, periodo)

}

comerciobr_dados_corrente("China", "anual") %>%
  tidyr::pivot_wider(names_from = path, values_from = value) %>%
  dplyr::mutate(Corrente = EXP + IMP,
                Saldo = EXP - IMP) %>%
  dplyr::rename(Exportações = "EXP", Importações = "IMP") %>%
  tidyr::pivot_longer(Exportações:Saldo, names_to = "trade_flow", values_to = "value")
