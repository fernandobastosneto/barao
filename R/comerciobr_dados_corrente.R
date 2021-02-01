comerciobr_dados_corrente <- function(pais, periodo) {

  df <- barao::sh1_df %>%
    dplyr::filter(NO_PAIS == pais)

  if (periodo == "anual") {

    df <- df %>%
      dplyr::group_by(CO_ANO, path) %>%
      dplyr::summarise(value = sum(value))
  }

  else {

    ultimo_mes <- df %>%
      dplyr::filter(CO_ANO == max(CO_ANO)) %>%
      dplyr::mutate(CO_MES = as.numeric(CO_MES)) %>%
      dplyr::filter(CO_MES == max(CO_MES)) %>%
      dplyr::distinct(CO_MES) %>%
      dplyr::pull(CO_MES)

    df <- df %>%
      dplyr::mutate(CO_MES = as.numeric(CO_MES)) %>%
      dplyr::filter(CO_MES <= ultimo_mes) %>%
      dplyr::group_by(CO_ANO, CO_MES, path) %>%
      dplyr::summarise(value = sum(value))

  }

  df <- df %>%
    tidyr::pivot_wider(names_from = path, values_from = value) %>%
    dplyr::mutate(Corrente = EXP + IMP,
                  Saldo = EXP - IMP) %>%
    dplyr::rename(Exportações = "EXP", Importações = "IMP") %>%
    tidyr::pivot_longer(Exportações:Saldo, names_to = "trade_flow", values_to = "value")

  df
}
