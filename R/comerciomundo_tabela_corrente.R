#' Tabela de Corrente de Comércio país-mundo
#'
#' em formato latex.
#'
#' @param pais um país
#'
#' @export

comerciomundo_tabela_corrente <- function(pais) {

  tabela_prep <- barao::comerciomundo_dados_corrente(pais)  %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = .data$trade_flow, values_from = value) %>%
    dplyr::mutate(var_exp = .data$Exportacoes/dplyr::lag(.data$Exportacoes)-1) %>%
    dplyr::mutate(var_imp = .data$Importacoes/dplyr::lag(.data$Importacoes)-1) %>%
    dplyr::mutate(var_saldo = .data$Saldo/abs(dplyr::lag(.data$Saldo))-1) %>%
    dplyr::mutate(var_corrente = .data$Corrente/dplyr::lag(.data$Corrente)-1) %>%
    dplyr::filter(.data$year != min(.data$year)) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("var"), scales::label_percent())) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("var"), ~ paste0("(", .x, ")"))) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si(accuracy = 0.01))) %>%
    tidyr::unite("Exportacoes", c("Exportacoes","var_exp"), sep = " ") %>%
    tidyr::unite("Importacoes", c("Importacoes", "var_imp"), sep = " ") %>%
    tidyr::unite("Saldo", c("Saldo", "var_saldo"), sep = " ") %>%
    tidyr::unite("Corrente", c("Corrente", "var_corrente"), sep = " ") %>%
    dplyr::relocate(.data$year, .data$Exportacoes, .data$Importacoes, .data$Saldo, .data$Corrente) %>%
    dplyr::arrange(dplyr::desc(year)) %>%
    tidyr::pivot_longer(.data$Exportacoes:.data$Corrente, names_to = "trade_flow", values_to = "value") %>%
    tidyr::pivot_wider(names_from = year, values_from = value) %>%
    dplyr::rename(" " = .data$trade_flow) %>%
    dplyr::select(1:tidyselect::last_col()) %>%
    tibble::column_to_rownames(var = " ")

  tabela <- split(1:(ncol(tabela_prep)), sort(rep_len(1:2, ncol(tabela_prep)))) %>%
    purrr::map(~ dplyr::select(tabela_prep, .)) %>%
    purrr::map(~ kableExtra::kbl(.x, format = "latex", booktabs = TRUE, align = "r", escape = T)) %>%
    purrr::map(~ kableExtra::kable_styling(.x, position = "center", full_width = FALSE, latex_options = "hold_position"))

  tabela

}
