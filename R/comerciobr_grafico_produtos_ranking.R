#' @export
comerciobr_grafico_produtos_ranking <- function(pais, periodo) {

  comerciobr_dados_produtos(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_sh4_por = stringr::str_sub(no_sh4_por, 1, 15)) %>%
    dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot(ggplot2::aes(co_ano, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(color = no_sh4_por,
                                    group = no_sh4_por), size = 2, show.legend = F) +
    ggrepel::geom_label_repel(data = . %>%
                 dplyr::filter(co_ano == max(co_ano)),
               ggplot2::aes(co_ano, value, label = no_sh4_por),
               size = 2, show.legend = F) +
    ggplot2::facet_wrap(~ path, scales = "free",
                        nrow = 2) +
    ggthemes::scale_color_pander() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = glue::glue("Brasil-{pais}, evolução dos produtos comercializados"),
                  caption = "Fonte: Ministério da Economia",
                  x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number_si())

}
