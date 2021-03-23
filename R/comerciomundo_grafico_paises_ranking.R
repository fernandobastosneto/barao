#' Gráfico de comparação do fluxo de comércio de um país com seus 10 principais parceiros
#'
#' @param pais um país
#'
#' @export

comerciomundo_grafico_paises_ranking <- function(pais) {
  nome_pais <- pais

  max_ano <- barao::comerciomundo_dados_paises(pais) %>%
    dplyr::distinct(year) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::pull(max(year))

  barao::comerciomundo_dados_paises(pais) %>%
    dplyr::group_by(trade_flow_code, year) %>%
    dplyr::slice_max(value, n = 10) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::mutate(trade_flow_code = as.character(trade_flow_code),
                  trade_flow_code = dplyr::case_when(trade_flow_code == "1" ~ "Importa\u00e7\u00f5es",
                                                     trade_flow_code == "2" ~ "Exporta\u00e7\u00f5es")) %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = value, y = tidytext::reorder_within(no_pais, value, trade_flow_code),
                                   fill = trade_flow_code),
                      show.legend = F) +
    ggplot2::facet_wrap(~ trade_flow_code, scales = "free") +
    ggplot2::labs(title = glue::glue("{nome_pais}-Mundo, principais parceiros comerciais em {max_ano}"),
                  x = NULL, y = NULL, caption = "Fonte: COMTRADE-ONU") +
    ggthemes::scale_fill_tableau() +
    tidytext::scale_y_reordered() +
    ggplot2::scale_x_continuous(labels = scales::label_number_si()) +
    ggplot2::theme_minimal()

}

