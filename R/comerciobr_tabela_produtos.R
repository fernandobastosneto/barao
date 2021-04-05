#' Tabela de fluxo comercial por produtos
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export
comerciobr_tabela_produtos <- function(pais, periodo) {

  if (periodo == "mensal") {

    frase <- paste0("Dados Agregados at\u00e9 ", barao::meses(barao::comerciobr_get_ultimomes()))
    df <- barao::comerciobr_dados_produtos(pais, periodo) %>%
      dplyr::ungroup()

  }

  else {

    frase <- paste0("Dados Anuais")
    df <- barao::comerciobr_dados_produtos(pais, periodo) %>%
      dplyr::ungroup() %>%
      dplyr::filter(co_ano <= max(co_ano))

  }

  df %>%
    dplyr::group_by(path) %>%
    dplyr::arrange(no_sh4_por) %>%
    dplyr::group_by(no_sh4_por) %>%
    dplyr::mutate(pct_var = value/dplyr::lag(value)-1) %>%
    dplyr::mutate(pct_prop = value/.data$total) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano >= max(co_ano)-3) %>%
    dplyr::select(-c(.data$total)) %>%
    # dplyr::select(-c(rank, total)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(co_ano, path) %>%
    dplyr::arrange(dplyr::desc(value), .by_group = T) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano >= max(co_ano)-4) %>%
    dplyr::arrange(dplyr::desc(co_ano)) %>%
    dplyr::relocate(co_ano, path, no_sh4_por, co_sh4, value, .data$pct_var, .data$pct_prop) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("val"), scales::label_number_si(accuracy = 0.01))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("pct_") , scales::label_percent(decimal.mark = ",", accuracy = .1))) %>%
    dplyr::mutate(no_sh4_por = dplyr::case_when(stringr::str_length(no_sh4_por) > 50 ~
                                           paste0(stringr::str_sub(no_sh4_por, 1, 50), ".."),
                                         TRUE ~ no_sh4_por)) %>%
    kableExtra::kbl(booktabs = T, col.names = c("Ano", "Dire\u00e7\u00e3o", "Produto (SH4)", "C\u00f3digo (SH4)", "Valor", "Varia\u00e7\u00e3o", "Propor\u00e7\u00e3o")) %>%
    kableExtra::kable_styling(font_size = 7, full_width = T, latex_options = c("hold_position")) %>%
    kableExtra::column_spec(3, width = "30em") %>%
    kableExtra::collapse_rows(columns = 1:2, latex_hline = "full", valign = "top",
                              row_group_label_position = "stack", target = 2) %>%
    kableExtra::add_header_above(header = c(setNames(7, frase)))

}
