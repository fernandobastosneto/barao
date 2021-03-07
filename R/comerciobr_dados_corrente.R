#' Dados de fluxo de com\u00e9rcio bilateral entre Brasil e um determinado pa\u00eds
#'
#' @param pais um pa\u00eds
#' @param periodo "anual" ou "mensal"
#'
#' @return tibble com dados de com\u00e9rcio bilateral de 2010 em diante.
#'
#' @export
comerciobr_dados_corrente <- function(pais, periodo) {

  df <- comerciobr::sh1_df %>%
    dplyr::filter(no_pais %in% pais)

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
    dplyr::mutate(Corrente = .data$EXP + .data$IMP,
                  Saldo = .data$EXP - .data$IMP) %>%
    dplyr::rename(`Exportacoes` = "EXP", `Importacoes` = "IMP") %>%
    tidyr::pivot_longer(.data$`Exportacoes`:.data$Saldo, names_to = "trade_flow", values_to = "value")

  df
}
