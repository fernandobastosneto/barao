#' Gráfico de evolução dos produtos comercializados do Brasil com um determinado país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export
comerciobr_grafico_produtos_ranking <- function(pais, periodo) {

    if (length(pais) > 1) {
      nome_pais <- get_bloco(pais)
    }

    else {
      nome_pais <- pais
    }


  if (periodo == "mensal") {

    df <- comerciobr_dados_produtos(pais, periodo) %>%
      dplyr::ungroup()

    frase <- paste0("agregado at\u00e9 ", barao::meses(barao::comerciobr_get_ultimomes()))

  }

  else {
    df <- comerciobr_dados_produtos(pais, periodo) %>%
      dplyr::ungroup() %>%
      dplyr::filter(co_ano <= max(co_ano))

    frase <- paste0("at\u00e9 ", barao::comerciobr_get_ulimoano())

  }

  df %>%
    dplyr::mutate(no_sh4_por = stringr::str_sub(no_sh4_por, 1, 15)) %>%
    # dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot(ggplot2::aes(co_ano, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(color = no_sh4_por,
                                    group = no_sh4_por), size = 2, show.legend = F) +
    ggrepel::geom_label_repel(data = . %>%
                 dplyr::filter(co_ano == max(co_ano) |
                                 co_ano == min(co_ano) |
                                 co_ano == max(co_ano)-5),
               ggplot2::aes(co_ano, value, label = no_sh4_por),
               size = 2, show.legend = F) +
    ggplot2::facet_wrap(~ path, scales = "free",
                        nrow = 2) +
    ggthemes::scale_color_pander() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = glue::glue("Brasil-{nome_pais}, evolu\u00e7\u00e3o do com\u00e9rcio, {frase}"),
                  caption = "Fonte: Minist\u00e9rio da Economia",
                  x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty())

}
