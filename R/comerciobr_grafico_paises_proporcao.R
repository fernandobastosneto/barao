#' Gráfico de ranking e proporção do fluxo de comércio do brasil com um determinado país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export
comerciobr_grafico_paises_proporcao <- function(pais, periodo) {

  if (periodo == "mensal") {

    df <- barao::comerciobr_dados_paises(pais, periodo)
    frase <- paste0(barao::comerciobr_get_ulimoano(), " at\u00e9 ", barao::meses(barao::comerciobr_get_ultimomes()))

  }

  else {

    df <- barao::comerciobr_dados_paises(pais, periodo) %>%
      dplyr::filter(co_ano <= max(co_ano)-1)
    frase <- paste0("em ", barao::comerciobr_get_ulimoano()-1)
  }

  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::mutate(prop = value/.data$total) %>%
    dplyr::mutate(no_pais = dplyr::case_when(stringr::str_length(no_pais) > 20 ~ paste0(stringr::str_sub(no_pais, 1, 20), ".."),
                                             TRUE ~ no_pais)) %>%
    dplyr::ungroup()

  df %>%
    ggplot2::ggplot(ggplot2::aes(.data$prop, rank, label = no_pais)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = df %>% dplyr::filter(df$no_pais == pais),
                        ggplot2::aes(color = no_pais), show.legend = F) +
    ggrepel::geom_label_repel() +
    ggplot2::facet_wrap(~path, scales = "free") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.01)) +
    ggplot2::scale_y_reverse(breaks = scales::breaks_pretty(),
                             labels = scales::label_ordinal()) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = glue::glue("Brasil-{pais}, ranking e propor\u00e7\u00e3o de comercio, {frase}"),
                  caption = "Fonte: Minist\u00e9rio da Economia",
                  x = NULL, y = NULL)

}
