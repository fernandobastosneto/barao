#' Gr\u00e1fico de evolu\u00e7\u00e3o do fluxo de com\u00e9rcio de um pa\u00eds com seus cinco principais parceiros
#'
#' @param pais um pa\u00eds
#'
#' @export

comerciomundo_grafico_paises_evolucao <- function(pais) {

  ano_min <- comerciomundo_dados_paises(pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::distinct(year) %>%
    dplyr::pull(min(year))

  ano_max <- comerciomundo_dados_paises(pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::distinct(year) %>%
    dplyr::pull(max(year))
# browser()
  barao::comerciomundo_dados_paises(pais) %>%
    # dplyr::group_by(year, trade_flow_code) %>%
    # dplyr::slice_max(value, n = 5) %>%
    dplyr::mutate(trade_flow_code = as.character(trade_flow_code),
                  trade_flow_code = dplyr::case_when(trade_flow_code == "1" ~ "Importa\u00e7\u00f5es",
                                                     trade_flow_code == "2" ~ "Exporta\u00e7\u00f5es")) %>%
    tidyr::drop_na() %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(year, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(color = no_pais,
                                    group = no_pais),
                       size = 1.5,
                       # linetype = 4,
                       show.legend = F) +
    ggrepel::geom_label_repel(data = . %>%
                                dplyr::filter(.data$year == max(year) |
                                                .data$year == min(year) |
                                                .data$year == max(year)-5),
                              ggplot2::aes(year, value, label = no_pais),
                              size = 1.2, show.legend = F) +
    ggplot2::facet_wrap(~ trade_flow_code, scales = "free",
                        nrow = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = glue::glue("{pais}-Mundo, evolu\u00e7\u00e3o do com\u00e9rcio"),
                  caption = "Fonte: COMTRADE-ONU",
                  x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_continuous(limits = c(ano_min, ano_max),
                                breaks = scales::breaks_pretty()) +
    ggplot2::scale_color_manual(values=c(ggthemes::ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]$`Tableau 20`$value))

}


