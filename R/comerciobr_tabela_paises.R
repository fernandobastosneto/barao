#' @export
comerciobr_tabela_paises <- function(pais, periodo) {

  barao::comerciobr_dados_paises(pais, periodo)  %>%
    dplyr::group_by(no_pais, path) %>%
    dplyr::arrange(desc(co_ano), .by_group = T) %>%
    dplyr::mutate(pct_var = value/dplyr::lead(value)-1) %>%
    dplyr::mutate(pct_prop = (value/total)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano >= max(co_ano)-3) %>%
    dplyr::select(-c(rank, total)) %>%
    dplyr::group_by(co_ano, path) %>%
    dplyr::arrange(desc(value), .by_group = T) %>%
    dplyr::arrange(desc(co_ano)) %>%
    dplyr::relocate(co_ano, path, no_pais, value, pct_var, pct_prop) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("val"), scales::label_number_si(accuracy = 0.01))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("pct_") , scales::label_percent(decimal.mark = ",", accuracy = .01))) %>%
    kableExtra::kbl(booktabs = T, col.names = c("Ano", "Direção", "País", "Valor", "Variação", "Proporção")) %>%
    kableExtra::kable_styling(font_size = 7, full_width = T, latex_options = c("hold_position")) %>%
    kableExtra::column_spec(3, width = "20em") %>%
    kableExtra::collapse_rows(columns = 1:2, latex_hline = "full", valign = "top",
                              row_group_label_position = "stack", target = 2)

}
