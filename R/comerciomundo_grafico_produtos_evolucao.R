#' Gráfico da evolução dos principais produtos comercializados por um país com o mundo no último ano disponível
#'
#' @param pais um país
#'
#' @export

comerciomundo_grafico_produtos_evolucao <- function(pais) {
  ano_min <- comerciomundo_dados_produtos(pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::distinct(year) %>%
    dplyr::pull(min(year))

  ano_max <- comerciomundo_dados_produtos(pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::distinct(year) %>%
    dplyr::pull(max(year))

  dic <- comerciobr::dic_sh6_sh2 %>%
    dplyr::rename(commodity_code = co_sh2) %>%
    dplyr::select(-co_sh6) %>%
    dplyr::distinct() %>%
    dplyr::filter(stringr::str_length(commodity_code) < 3)

  barao::comerciomundo_dados_produtos(pais) %>%
    dplyr::group_by(trade_flow_code, commodity_code, year) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    # dplyr::rename(CO_SH2 = commodity_code) %>%
    dplyr::left_join(dic) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_sh2_por = dplyr::case_when(
      stringr::str_length(no_sh2_por) > 20 ~ paste0(stringr::str_sub(no_sh2_por, 1, 20), ".."),
      TRUE ~ no_sh2_por)) %>%
    dplyr::filter(trade_flow_code == 1 | trade_flow_code == 2) %>%
    tidyr::unite("commodity_code", c("commodity_code", "no_sh2_por"), sep = " - ") %>%
    dplyr::mutate(trade_flow_code = as.character(trade_flow_code),
                  trade_flow_code = dplyr::case_when(trade_flow_code == "1" ~ "Importa\u00e7\u00f5es",
                                                     trade_flow_code == "2" ~ "Exporta\u00e7\u00f5es")) %>%
    tidyr::drop_na() %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(year, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(color = commodity_code,
                                    group = commodity_code),
                       size = 1.5,
                       # linetype = 4,
                       show.legend = F) +
    ggrepel::geom_label_repel(data = . %>%
                                dplyr::filter(.data$year == max(year) |
                                                .data$year == min(year) |
                                                .data$year == max(year)-5),
                              ggplot2::aes(year, value, label = commodity_code),
                              size = 1.2, show.legend = F) +
    ggplot2::facet_wrap(~ trade_flow_code, scales = "free",
                        nrow = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = glue::glue("{pais}-Mundo, produtos comercializados, evolu\u00e7\u00e3o"),
                  caption = "Fonte: COMTRADE-ONU",
                  x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_continuous(limits = c(ano_min, ano_max),
                                breaks = scales::breaks_pretty()) +
    ggplot2::scale_color_manual(values=c(ggthemes::ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]$`Tableau 20`$value))

}
