#' @export
comerciobr_grafico_corrente <- function(pais, periodo) {

  comerciobr_dados_corrente(pais, periodo) %>%
    dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(co_ano, value, fill = trade_flow),
                      show.legend = F) +
    ggplot2::facet_wrap(~factor(trade_flow,
                                levels = c("Exportações", "Importações",
                                           "Corrente", "Saldo")),
                        scales = "free_x") +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_discrete(breaks = scales::breaks_pretty()) +
    ggplot2::labs(title = glue::glue("Brasil-{pais}, Fluxo de Comércio"),
                  x = NULL, y = NULL, caption = "Fonte: Ministério da Economia") +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()

}
