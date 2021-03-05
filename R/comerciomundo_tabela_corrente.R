#' @export
#'

comerciomundo_tabela_corrente <- function(pais) {

  tabela_prep <- barao::comerciomundo_dados_corrente(pais)  %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = trade_flow, values_from = value) %>%
    dplyr::mutate(var_exp = Exportações/dplyr::lag(Exportações)-1) %>%
    dplyr::mutate(var_imp = Importações/dplyr::lag(Importações)-1) %>%
    dplyr::mutate(var_saldo = Saldo/abs(dplyr::lag(Saldo))-1) %>%
    dplyr::mutate(var_corrente = Corrente/dplyr::lag(Corrente)-1) %>%
    dplyr::filter(year != min(year)) %>%
    dplyr::mutate(dplyr::across(starts_with("var"), scales::label_percent())) %>%
    dplyr::mutate(dplyr::across(starts_with("var"), ~ paste0("(", .x, ")"))) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si(accuracy = 0.01))) %>%
    tidyr::unite("Exportações", c("Exportações","var_exp"), sep = " ") %>%
    tidyr::unite("Importações", c("Importações", "var_imp"), sep = " ") %>%
    tidyr::unite("Saldo", c("Saldo", "var_saldo"), sep = " ") %>%
    tidyr::unite("Corrente", c("Corrente", "var_corrente"), sep = " ") %>%
    dplyr::relocate(year, Exportações, Importações, Saldo, Corrente) %>%
    dplyr::arrange(desc(year)) %>%
    tidyr::pivot_longer(Exportações:Corrente, names_to = "trade_flow", values_to = "value") %>%
    tidyr::pivot_wider(names_from = year, values_from = value) %>%
    dplyr::rename(" " = trade_flow) %>%
    dplyr::select(1:tidyselect::last_col()) %>%
    tibble::column_to_rownames(var = " ")

  tabela <- split(1:(ncol(tabela_prep)), sort(rep_len(1:2, ncol(tabela_prep)))) %>%
    purrr::map(~ dplyr::select(tabela_prep, .)) %>%
    purrr::map(~ kableExtra::kbl(.x, format = "latex", booktabs = TRUE, align = "r", escape = T)) %>%
    purrr::map(~ kableExtra::kable_styling(.x, position = "center", full_width = FALSE, latex_options = "hold_position"))

  tabela

}
