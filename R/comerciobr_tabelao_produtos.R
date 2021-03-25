#' Tabelao de principais produtos exportados e importados pelo Brasil a um determinado pa\\\\u00eds
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

comerciobr_tabelao_produtos <- function(pais, periodo) {

  max_ano <- barao::comerciobr_get_ulimoano()-1

  produtos <- comerciobr::dic_sh6_sh4 %>%
    dplyr::select(no_sh4_por, co_sh4) %>%
    dplyr::distinct(no_sh4_por, .keep_all = T)

  agregados <- barao::comerciobr_dados_corrente(pais, periodo) %>%
    dplyr::filter(co_ano > 2015) %>%
    tidyr::pivot_wider(names_from = co_ano, values_from = value) %>%
    dplyr::mutate(dplyr::across(2:tidyselect::last_col(), scales::label_number_si(accuracy = 0.01)))

  exp <- comerciobr::sh4_df %>%
    dplyr::filter(no_pais == pais) %>%
    dplyr::filter(co_ano == max(co_ano)-1) %>%
    dplyr::group_by(path, co_ano, co_sh4) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::group_by(path) %>%
    dplyr::slice_max(value, n = 10) %>%
    dplyr::ungroup() %>%
    dplyr::filter(path == "EXP") %>%
    tidyr::pivot_wider(names_from = path, values_from = value) %>%
    dplyr::mutate(posicao = dplyr::row_number()) %>%
    dplyr::left_join(produtos) %>%
    dplyr::select(-c(co_ano))

  imp <- comerciobr::sh4_df %>%
    dplyr::filter(no_pais == pais) %>%
    dplyr::filter(co_ano == max(co_ano)-1) %>%
    dplyr::group_by(path, co_ano, co_sh4) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::group_by(path) %>%
    dplyr::slice_max(value, n = 10) %>%
    dplyr::ungroup() %>%
    dplyr::filter(path == "IMP") %>%
    tidyr::pivot_wider(names_from = path, values_from = value) %>%
    dplyr::mutate(posicao = dplyr::row_number()) %>%
    dplyr::left_join(produtos) %>%
    dplyr::select(-c(co_ano))

  frase <- data.frame(x = glue::glue("Produtos comercializados em {max_ano}"),
                                     y = 7)

  produtos <- exp %>%
    dplyr::left_join(imp, by = "posicao") %>%
    dplyr::mutate(dplyr::across(c(2, 6), scales::label_number_si(accuracy = 0.01))) %>%
    dplyr::mutate(dplyr::across(5:7, ~ tidyr::replace_na(.x, " "))) %>%
    dplyr::select(.data$posicao, .data$co_sh4.x, .data$no_sh4_por.x, .data$EXP,
                  .data$co_sh4.y, .data$no_sh4_por.y, .data$IMP)

  kableExtra::kbl(produtos, format = "latex", booktabs = TRUE,
      col.names = c("Posi\u00e7\u00e3o", "C\u00f3digo SH4", "Produto SH4", "Exporta\u00e7\u00f5es",
                    "C\u00f3digo SH4", "Produto SH4", "Importa\u00e7\u00f5es")) %>%
    kableExtra::kable_styling(latex_options = c("striped", "scale_down", "hold_position"),
                              font_size = 13) %>%
    kableExtra::add_header_above(c(" ", "Exportacoes" = 3, "Importacoes" = 3), bold = TRUE) %>%
    kableExtra::add_header_above(frase, bold = TRUE) %>%
    kableExtra::collapse_rows(columns = 1, latex_hline = "major") %>%
    kableExtra::column_spec(3, width = "23em") %>%
    kableExtra::column_spec(6, width = "23em") %>%
    kableExtra::footnote(symbol = c("Fonte: Minist\u00e9rio da Economia"))

}
