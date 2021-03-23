#' Tabela de Corrente de Comércio
#'
#' em formato latex.
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export
comerciobr_tabela_corrente <- function(pais, periodo) {

  tabela_prep <- barao::comerciobr_dados_corrente(pais, periodo)  %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = .data$trade_flow, values_from = value) %>%
    dplyr::mutate(var_exp = .data$Exportacoes/dplyr::lag(.data$Exportacoes)-1) %>%
    dplyr::mutate(var_imp = .data$Importacoes/dplyr::lag(.data$Importacoes)-1) %>%
    dplyr::mutate(var_saldo = .data$Saldo/abs(dplyr::lag(.data$Saldo))-1) %>%
    dplyr::mutate(var_corrente = .data$Corrente/dplyr::lag(.data$Corrente)-1) %>%
    dplyr::filter(co_ano != min(co_ano)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("var"), scales::label_percent())) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("var"), ~ paste0("(", .x, ")"))) %>%
    dplyr::mutate(co_ano = as.character(co_ano)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si())) %>%
    tidyr::unite("Exportacoes", c("Exportacoes","var_exp"), sep = " ") %>%
    tidyr::unite("Importacoes", c("Importacoes", "var_imp"), sep = " ") %>%
    tidyr::unite("Saldo", c("Saldo", "var_saldo"), sep = " ") %>%
    tidyr::unite("Corrente", c("Corrente", "var_corrente"), sep = " ") %>%
    dplyr::relocate(co_ano, .data$Exportacoes, .data$Importacoes, .data$Saldo, .data$Corrente) %>%
    dplyr::arrange(dplyr::desc(co_ano)) %>%
    tidyr::pivot_longer(.data$Exportacoes:.data$Corrente, names_to = "trade_flow", values_to = "value") %>%
    tidyr::pivot_wider(names_from = co_ano, values_from = value) %>%
    dplyr::rename(" " = .data$trade_flow) %>%
    dplyr::select(1:11) %>%
    tibble::column_to_rownames(var = " ")

  tabela <- split(1:(ncol(tabela_prep)), sort(rep_len(1:2, ncol(tabela_prep)))) %>%
    purrr::map(~ dplyr::select(tabela_prep, .)) %>%
    purrr::map(~ kableExtra::kbl(.x, format = "latex", booktabs = TRUE, align = "r", escape = T)) %>%
    purrr::map(~ kableExtra::kable_styling(.x, position = "center", full_width = FALSE, latex_options = "hold_position"))
  # %>%
  #   purrr::walk(print)

  tabela

}

