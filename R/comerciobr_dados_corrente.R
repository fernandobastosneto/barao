#' @export

comerciobr_dados_corrente <- function(pais, periodo) {

  df <- comerciobr::sh1_df %>%
    dplyr::filter(no_pais == pais)

  if (periodo == "anual") {

    df <- df %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::summarise(value = sum(value))
  }

  else {

    df <- df %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= barao::comerciobr_get_ultimomes()) %>%
      dplyr::group_by(co_ano, path) %>%
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
