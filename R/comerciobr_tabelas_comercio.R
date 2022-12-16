#' Tabela Intraindustria
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export


comerciobr_tabelas_comercio <- function(pais, periodo){

  if (periodo == "mensal") {

    frase <- paste0("Dados Agregados at\u00e9 ", barao2::meses(barao2::comerciobr_get_ultimomes()))
    dados_hh <- barao2::comerciobr_dados_concentracao(pais, periodo, "total") %>%
      dplyr::ungroup()
    dados_hh_exp <- barao2::comerciobr_dados_concentracao(pais, periodo, "imp")%>%
      dplyr::ungroup()
    dados_hh_imp <- barao2::comerciobr_dados_concentracao(pais, periodo, "exp")%>%
      dplyr::ungroup()
    dados_intraindustria <- barao2::comerciobr_dados_intraindustria(pais, periodo) %>%
      dplyr::ungroup()

  }else {

    frase <- paste0("Dados Anuais")
    dados_hh <- barao2::comerciobr_dados_concentracao(pais, periodo, "total") %>%
      dplyr::ungroup()
    dados_hh_exp <- barao2::comerciobr_dados_concentracao(pais, periodo, "imp")%>%
      dplyr::ungroup()
    dados_hh_imp <- barao2::comerciobr_dados_concentracao(pais, periodo, "exp")%>%
      dplyr::ungroup()
    dados_intraindustria <- barao2::comerciobr_dados_intraindustria(pais, periodo) %>%
      dplyr::ungroup()
  }

  df.anual <- dplyr::inner_join(dados_hh , dados_hh_exp ,by = "co_ano")
  df.anual <- dplyr::inner_join(df.anual ,dados_hh_imp ,by = "co_ano")
  df.anual <- dplyr::inner_join(df.anual ,dados_intraindustria ,by = "co_ano")

  tabela<-as.data.frame(t(df.anual))
  colnames(tabela) <- tabela[1,]
  tabela <- tabela[-1, ]
  rownames(tabela) <- c("Concentração comercial- Total","Concentração comercial- Exportação", "Concentração comercial- Importação", "Comércio Intraindustria")
  options(OutDec= ",")

  tabela <- split(1:(ncol(tabela)), sort(rep_len(1:2, ncol(tabela)))) %>%
    purrr::map(~ dplyr::select(tabela, .)) %>%
    purrr::map(~ kableExtra::kbl(.x, format = "latex", booktabs = TRUE, align = "r", escape = T, digits = 3)) %>%
    purrr::map(~ kableExtra::kable_styling(.x, position = "center", full_width = FALSE, latex_options = "hold_position"))
  # %>%
  # purrr::walk(print)

  tabela



}
