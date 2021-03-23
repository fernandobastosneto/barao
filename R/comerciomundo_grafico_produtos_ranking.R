#' Gráfico dos principais produtos comercializados por um país no último ano disponível
#'
#' @param pais um país
#'
#' @export


comerciomundo_grafico_produtos_ranking <- function(pais) {

  nome_pais <- paste0(pais)

  dic <- comerciobr::dic_sh6_sh2 %>%
    dplyr::rename(commodity_code = co_sh2) %>%
    dplyr::select(-co_sh6) %>%
    dplyr::distinct() %>%
    dplyr::filter(stringr::str_length(commodity_code) < 3)

  df <- barao::comerciomundo_dados_produtos(pais) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::group_by(commodity_code, trade_flow_code) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::group_by(trade_flow_code) %>%
    dplyr::slice_max(value, n = 5) %>%
    dplyr::left_join(dic) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_sh2_por = dplyr::case_when(
      stringr::str_length(no_sh2_por) > 20 ~ paste0(stringr::str_sub(no_sh2_por, 1, 20), ".."),
      TRUE ~ no_sh2_por)) %>%
    dplyr::filter(trade_flow_code == 1 | trade_flow_code == 2)

  df %>%
    dplyr::mutate(trade_flow_code = as.character(trade_flow_code),
                  trade_flow_code = dplyr::case_when(trade_flow_code == "1" ~ "Importa\u00e7\u00f5es",
                                                     trade_flow_code == "2" ~ "Exporta\u00e7\u00f5es")) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(value, tidytext::reorder_within(no_sh2_por,
                                                   value,
                                                   trade_flow_code),
                                   fill = trade_flow_code), show.legend = F) +
    ggplot2::facet_wrap(~ trade_flow_code, scales = "free") +
    tidytext::scale_y_reordered() +
    ggplot2::theme_minimal() +
    ggthemes::scale_fill_tableau() +
    ggplot2::scale_x_continuous(labels = scales::label_number_si()) +
    ggplot2::labs(title = glue::glue("{nome_pais}-mundo, principais produtos comercializados"),
                  x = NULL, y = NULL, caption = "Fonte: COMTRADE-ONU")

}

