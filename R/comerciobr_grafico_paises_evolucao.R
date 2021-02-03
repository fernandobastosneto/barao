#' @export
comerciobr_grafico_paises_evolucao <- function(pais, periodo) {

  ano_min <- comerciobr_dados_paises(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == min(co_ano)) %>%
    dplyr::distinct(co_ano) %>%
    dplyr::pull(min(co_ano))

  ano_max <- comerciobr_dados_paises(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::distinct(co_ano) %>%
    dplyr::pull(max(co_ano))

  comerciobr_dados_paises(pais, periodo) %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(no_pais = stringr::str_sub(no_pais, 1, 15)) %>%
    # dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot(ggplot2::aes(co_ano, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(color = no_pais,
                                    group = no_pais),
                       size = 1.5,
                       linetype = 4,
                       show.legend = F) +
    ggplot2::geom_line(data = . %>%
                         dplyr::filter(.data$no_pais == pais),
                       ggplot2::aes(color = no_pais,
                                    group = no_pais),
                       size = 3,
                       show.legend = F) +
    ggrepel::geom_label_repel(data = . %>%
                                dplyr::filter(.data$co_ano == max(co_ano) |
                                              .data$co_ano == min(co_ano) |
                                              .data$co_ano == max(co_ano)-(max(co_ano)-min(co_ano))/2),
                              ggplot2::aes(co_ano, value, label = no_pais),
                              size = 2, show.legend = F) +
    ggplot2::facet_wrap(~ path, scales = "free",
                        nrow = 2) +
    ggthemes::scale_color_tableau() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = glue::glue("Brasil-{pais}, evolução do comércio"),
                  caption = "Fonte: Ministério da Economia",
                  x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_continuous(limits = c(ano_min, ano_max),
                                breaks = scales::breaks_pretty())

}

# comerciobr_grafico_paises_evolucao("Argentina", "anual")