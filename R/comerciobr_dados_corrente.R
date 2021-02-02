#' @export

comerciobr_dados_corrente <- function(pais, periodo) {

  df <- comerciobr::sh1_df %>%
    dplyr::filter(no_pais == pais)

  if (periodo == "anual") {

    df <- df %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::summarise(value = sum(value))
  }

  else {

    ultimo_mes <- df %>%
      dplyr::filter(co_ano == max(co_ano)) %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes == max(co_mes)) %>%
      dplyr::distinct(co_mes) %>%
      dplyr::pull(co_mes)

    df <- df %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= ultimo_mes) %>%
      dplyr::group_by(co_ano, co_mes, path) %>%
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
