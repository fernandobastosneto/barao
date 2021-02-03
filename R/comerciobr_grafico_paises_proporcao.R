comerciobr_grafico_paises_proporcao <- function(pais, periodo) {

  df <- barao::comerciobr_dados_paises(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::mutate(prop = value/total) %>%
    dplyr::mutate(no_pais = dplyr::case_when(stringr::str_length(no_pais) > 20 ~ paste0(stringr::str_sub(no_pais, 1, 20), ".."),
                                             TRUE ~ no_pais)) %>%
    dplyr::ungroup()

  df %>%
    ggplot2::ggplot(ggplot2::aes(prop, rank, label = no_pais)) +
    ggplot2::geom_point() +
    # ggrepel::geom_label_repel(data = . %>%
    # dplyr::filter(co_ano == max(co_ano)),
    # ggplot2::aes(co_ano, value, label = no_sh4_por),
    # size = 2, show.legend = F) +
    ggplot2::geom_point(data = df %>% dplyr::filter(df$no_pais == pais),
                        ggplot2::aes(color = no_pais), show.legend = F) +
    ggrepel::geom_label_repel() +
    ggplot2::facet_wrap(~path, scales = "free") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.01)) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = glue::glue("Brasil-{pais}, ranking e proporção de comercio"),
                  caption = "Fonte: Ministério da Economia",
                  x = NULL, y = NULL)

}
