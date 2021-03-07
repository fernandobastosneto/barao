#' Gr\u00e1fico de Fluxo de Com\u00e9rcio pa\u00eds-mundo
#'
#' Com base na fun\u00e7\u00e3o \code{comerciomundo_dados_corrente}.
#'
#' @param pais um pa\u00eds
#'
#' @export

comerciomundo_grafico_corrente <- function(pais) {

  if (length(pais) > 1) {
    nome_pais <- get_bloco(pais)
  }

  else {
    nome_pais <- pais
  }

  frase <- paste0("at\u00e9 ", barao::comerciomundo_get_ultimoano(pais))

  comerciomundo_dados_corrente(pais) %>%
    dplyr::mutate(year = as.character(year)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(year, value, fill = .data$trade_flow),
                      show.legend = F) +
    ggplot2::facet_wrap(~factor(.data$trade_flow,
                                levels = c("Exporta\u00e7\u00f5es", "Importa\u00e7\u00f5es",
                                           "Corrente", "Saldo")),
                        scales = "free_x") +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_discrete(breaks = scales::breaks_pretty(n = 7)) +
    ggplot2::labs(title = glue::glue("{nome_pais}-Mundo, Fluxo de Com\u00e9rcio {frase}"),
                  x = NULL, y = NULL, caption = "Fonte: Minist\u00e9rio da Economia") +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()

}


