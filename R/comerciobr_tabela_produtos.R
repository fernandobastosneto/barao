#' @export
comerciobr_tabela_produtos <- function(pais, periodo) {

  if (periodo == "mensal") {

    frase <- paste0("Dados Agregados até ", barao::meses(barao::comerciobr_get_ultimomes()))

  }

  else {

    frase <- paste0("Dados Anuais")

  }

  barao::comerciobr_dados_produtos(pais, periodo) %>%
    dplyr::group_by(path) %>%
    dplyr::arrange(no_sh4_por) %>%
    dplyr::group_by(no_sh4_por) %>%
    dplyr::mutate(pct_var = value/dplyr::lag(value)-1) %>%
    dplyr::mutate(pct_prop = value/total) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano >= max(co_ano)-3) %>%
    dplyr::select(-c(rank, total)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(co_ano, path) %>%
    dplyr::arrange(desc(value), .by_group = T) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano >= max(co_ano)-4) %>%
    dplyr::arrange(desc(co_ano)) %>%
    dplyr::relocate(co_ano, path, no_sh4_por, co_sh4, value, pct_var, pct_prop) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("val"), scales::label_number_si(accuracy = 0.01))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("pct_") , scales::label_percent(decimal.mark = ",", accuracy = .1))) %>%
    dplyr::mutate(no_sh4_por = dplyr::case_when(stringr::str_length(no_sh4_por) > 50 ~
                                           paste0(stringr::str_sub(no_sh4_por, 1, 50), ".."),
                                         TRUE ~ no_sh4_por)) %>%
    kableExtra::kbl(booktabs = T, col.names = c("Ano", "Direção", "Produto (SH4)", "Código (SH4)", "Valor", "Variação", "Proporção")) %>%
    kableExtra::kable_styling(font_size = 7, full_width = T, latex_options = c("hold_position")) %>%
    kableExtra::column_spec(3, width = "30em") %>%
    kableExtra::collapse_rows(columns = 1:2, latex_hline = "full", valign = "top",
                              row_group_label_position = "stack", target = 2) %>%
    kableExtra::add_header_above(header = c(setNames(7, frase)))

}
