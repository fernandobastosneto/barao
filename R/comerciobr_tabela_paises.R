#' Tabela de fluxo comercial por país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

# gera uma tabela formatada com dados agregados de comércio entre países, exibindo valores, variações
# e proporções em relação a um período de tempo específico.
# O resultado final é uma tabela formatada que contém seis colunas: "Ano", "Direção", "País", "Valor",
# "Variação" e "Proporção". A tabela é organizada de acordo com as opções selecionadas pelo usuário,
# incluindo o período de tempo (mensal ou anual) e o país específico de interesse. A tabela apresenta
# informações agregadas sobre o comércio entre países, incluindo a direção do comércio, o país parceiro
# de comércio, o valor total do comércio, a variação percentual em relação ao período anterior e a
# proporção do valor total do comércio.

comerciobr_tabela_paises <- function(pais, periodo) {

  if (periodo == "mensal") {

    frase <- paste0("Dados Agregados at\u00e9 ", barao2::meses(barao2::comerciobr_get_ultimomes()))
    df <- barao2::comerciobr_dados_paises(pais, periodo) %>%
      dplyr::ungroup()
  }

  else {

    frase <- paste0("Dados Anuais")
    df <- barao2::comerciobr_dados_paises(pais, periodo) %>%
      dplyr::ungroup() %>%
      dplyr::filter(co_ano <= max(co_ano)-1)

  }

  df <- df %>%
      dplyr::group_by(no_pais, path) %>%
      dplyr::arrange(dplyr::desc(co_ano), .by_group = T) %>%
      dplyr::mutate(pct_var = value/dplyr::lead(value)-1) %>%
      dplyr::mutate(pct_prop = (value/.data$total)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(co_ano >= max(co_ano)-3) %>%
      dplyr::select(-c(rank, .data$total)) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::arrange(dplyr::desc(value), .by_group = T) %>%
      dplyr::arrange(dplyr::desc(co_ano)) %>%
      dplyr::relocate(co_ano, path, no_pais, value, .data$pct_var, .data$pct_prop) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("val"), scales::label_number_si(accuracy = 0.01))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("pct_") , scales::label_percent(decimal.mark = ",", accuracy = .01)))

  df %>%
    kableExtra::kbl(booktabs = T, col.names = c("Ano", "Dire\u00e7\u00e3o", "Pa\u00eds", "Valor", "Varia\u00e7\u00e3o", "Propor\u00e7\u00e3o")) %>%
    kableExtra::kable_styling(font_size = 7, full_width = T, latex_options = c("hold_position")) %>%
    kableExtra::column_spec(3, width = "20em") %>%
    kableExtra::collapse_rows(columns = 1:2, latex_hline = "full", valign = "top",
                              row_group_label_position = "stack", target = 2) %>%
    kableExtra::add_header_above(header = c(setNames(6,frase)), bold = T) %>%
    kableExtra::row_spec(which(df$no_pais == pais), bold = T) %>%
    kableExtra::column_spec(1:2, background = "white", color = "black")

}
