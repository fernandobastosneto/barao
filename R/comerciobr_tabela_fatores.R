#' @export
comerciobr_tabela_fatores <- function(pais, periodo, fator) {

  if (fator == "cuci") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::select(no_pais, path, no_cuci_sec, value) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(no_pais, path) %>%
      dplyr::arrange(desc(value), .by_group = T) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-no_pais) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top",
                                row_group_label_position = "stack") %>%
      kableExtra::add_header_above(header = setNames(3, glue::glue("{pais}-{fator}")))

  }

  else if (fator == "isic") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::select(no_pais, path, no_isic_secao, value) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(no_pais, path) %>%
      dplyr::arrange(desc(value), .by_group = T) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-no_pais) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top",
                                row_group_label_position = "stack") %>%
      kableExtra::add_header_above(header = setNames(3, glue::glue("{pais}-{fator}")))

  }

  else if (fator == "cgce") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::select(no_pais, path, no_cgce_n1, value) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(no_pais, path) %>%
      dplyr::arrange(desc(value), .by_group = T) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-no_pais) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top",
                                row_group_label_position = "stack") %>%
      kableExtra::add_header_above(header = setNames(3, glue::glue("{pais}-{fator}")))

  }

  else if (fator == "fator") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::select(no_pais, path, no_fat_agreg, value) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(no_pais, path) %>%
      dplyr::arrange(desc(value), .by_group = T) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-no_pais) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top",
                                row_group_label_position = "stack") %>%
      kableExtra::add_header_above(header = setNames(3, glue::glue("{pais}-{fator}")))

  }
}
