#' Tabela de dados de comércio por níveis de agregação
#'
#' @param pais um países
#' @param periodo "anual" ou "mensal"
#' @param fator "isic", "fator", "cgce" ou "cuci"
#'
#' @export
comerciobr_tabela_fatores <- function(pais, periodo, fator) {

  if (periodo == "mensal") {
    frase <- paste0(barao::comerciobr_get_ulimoano(), ", agregado at\u00e9 ", barao::meses(barao::comerciobr_get_ultimomes()))
  }

  else {
    frase <- paste0(barao::comerciobr_get_ulimoano()-1)
  }

  if (fator == "cuci") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(no_cuci_sec, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(path) %>%
      dplyr::arrange(dplyr::desc(value), .by_group = T) %>%
      dplyr::mutate(total = sum(value),
                    prop = value/.data$total) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("value"), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("prop"), scales::label_percent(accuracy = 0.1, decimal.mark = ","))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(.data$total)) %>%
      dplyr::relocate(path, .before = no_cuci_sec) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Dire\u00e7\u00e3o", "Classifica\u00e7\u00e3o CUCI", "Valor", "%")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")

  }

  else if (fator == "isic") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(no_isic_secao, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(path) %>%
      dplyr::arrange(dplyr::desc(value), .by_group = T) %>%
      dplyr::mutate(total = sum(value),
                    prop = value/.data$total) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("value"), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("prop"), scales::label_percent(accuracy = 0.1, decimal.mark = ","))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(.data$total)) %>%
      dplyr::relocate(path, .before = no_isic_secao) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Dire\u00e7\u00e3o", "Classifica\u00e7\u00e3o ISIC", "Valor", "%")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::add_header_above(header = c(setNames(4,frase))) %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")


  }

  else if (fator == "cgce") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(no_cgce_n1, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(path) %>%
      dplyr::arrange(dplyr::desc(value), .by_group = T) %>%
      dplyr::mutate(total = sum(value),
                    prop = value/.data$total) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("value"), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("prop"), scales::label_percent(accuracy = 0.1, decimal.mark = ","))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(.data$total)) %>%
      dplyr::relocate(path, .before = no_cgce_n1) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Dire\u00e7\u00e3o", "Classifica\u00e7\u00e3o CGCE", "Valor", "%")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")


  }

  else if (fator == "fator") {


    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(no_fat_agreg, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::arrange(path) %>%
      dplyr::group_by(path) %>%
      dplyr::arrange(dplyr::desc(value), .by_group = T) %>%
      dplyr::mutate(total = sum(value),
                    prop = value/.data$total) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("value"), scales::label_number_si(accuracy = 0.1))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("prop"), scales::label_percent(accuracy = 0.1, decimal.mark = ","))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(.data$total)) %>%
      dplyr::relocate(path, .before = no_fat_agreg) %>%
      kableExtra::kbl(booktabs = T, col.names = c("Dire\u00e7\u00e3o", "Classifica\u00e7\u00e3o Fator Agregado", "Valor", "%")) %>%
      kableExtra::collapse_rows(columns = 1, latex_hline = "full", valign = "top") %>%
      kableExtra::kable_styling(full_width = T, font_size = 6) %>%
      kableExtra::column_spec(2, width = "55em")

  }
}
