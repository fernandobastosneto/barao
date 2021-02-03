#' @export
comerciobr_grafico_paises <- function(pais, periodo) {

  barao::comerciobr_dados_paises(pais, periodo) %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
    # dplyr::filter(no_pais != "Egito"),
    ggplot2::aes(tidytext::reorder_within(no_pais, value, path), value, fill = path),
    show.legend = F) +
    ggplot2::geom_col(data = . %>%
    dplyr::filter(.data$no_pais == pais),
    ggplot2::aes(tidytext::reorder_within(no_pais, value, path), value, fill = "red"),
    show.legend = F) +
    ggplot2::geom_label(ggplot2::aes(tidytext::reorder_within(no_pais, value, path), value,
    label = rank)) +
    ggplot2::facet_wrap(~path, scales = "free_y") +
    ggplot2::labs(title = glue::glue("Brasil-{pais}, parceiros comerciais próximos"),
    x = NULL, y = NULL, caption = "Fonte: Ministério da Economia") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggthemes::scale_fill_tableau() +
    tidytext::scale_x_reordered()

  }
