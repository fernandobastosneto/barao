comerciobr_grafico_produtos <- function(pais, periodo) {
  df <- comerciobr_dados_produtos(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_sh4_por = dplyr::case_when(stringr::str_length(no_sh4_por) > 30 ~
                                              paste0(stringr::str_sub(no_sh4_por, 1, 30), ".."),
                                              TRUE ~ no_sh4_por))
  if (periodo == "anual") {
    df <- df %>%
      dplyr::filter(co_ano == max(co_ano)-1)
  }

  else {
    df <- df %>%
      dplyr::filter(co_ano == max(co_ano))
  }

  ano <- df %>%
    dplyr::distinct(co_ano) %>%
    dplyr::pull(co_ano)


  df %>%
    # dplyr::mutate(no_sh4_por = forcats::fct_reorder(no_sh4_por, value)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(tidytext::reorder_within(no_sh4_por, value, path, sum),
                                   value, fill = path),
                      show.legend = F) +
    ggplot2::facet_wrap(~ path, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggthemes::scale_fill_tableau() +
    tidytext::scale_x_reordered() +
    ggplot2::labs(title = glue::glue("Brasil-{pais}, pauta comercial em {ano}"),
                  x = NULL, y = NULL, caption = "Fonte: Ministério da Economia")

}
