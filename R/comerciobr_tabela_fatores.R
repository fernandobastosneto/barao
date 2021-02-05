#' @export
comerciobr_tabela_fatores <- function(pais, periodo, fator) {

  if (periodo == "mensal") {
    frase <- paste0(barao::comerciobr_get_ulimoano(), ", agregado até ", barao::meses(barao::comerciobr_get_ultimomes()))
  }

  else {
    frase <- paste0(barao::comerciobr_get_ulimoano()-1)
  }

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
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação CUCI", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::add_header_above(header = c(setNames(3,frase))) %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")

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
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação ISIC", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")


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
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação CGCE", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")


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
      kableExtra::kbl(booktabs = T, col.names = c("Direção", "Classificação Fator Agregado", "Valor")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")

  }
}
