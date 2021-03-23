#' Gráfico de Fluxo de Comércio Brasil-país
#'
#' Com base na função \code{comerciobr_dados_corrente}.
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export
comerciobr_grafico_corrente <- function(pais, periodo) {

  if (length(pais) > 1) {
    nome_pais <- get_bloco(pais)
  }

  else {
    nome_pais <- pais
  }

  if (periodo == "mensal") {

    ultimoano <- barao::comerciobr_get_ulimoano()
    frase <- paste0("agregado at\u00e9 ", meses(barao::comerciobr_get_ultimomes()))

  }

  else {

    frase <- paste0("at\u00e9 ", barao::comerciobr_get_ulimoano()-1)

  }

  comerciobr_dados_corrente(pais, periodo) %>%
    dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(co_ano, value, fill = .data$trade_flow),
                      show.legend = F) +
    ggplot2::facet_wrap(~factor(.data$trade_flow,
                                levels = c("Exportacoes", "Importacoes",
                                           "Corrente", "Saldo")),
                        scales = "free_x") +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_discrete(breaks = scales::breaks_pretty()) +
    ggplot2::labs(title = glue::glue("Brasil-{nome_pais}, Fluxo de Com\u00e9rcio {frase}"),
                  x = NULL, y = NULL, caption = "Fonte: Minist\u00e9rio da Economia") +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()

}
