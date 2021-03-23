#' Tabelao da corrente de comércio do Brasil com um determinado país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

comerciobr_tabelao_corrente <- function(pais, periodo) {

  agregados <- barao::comerciobr_dados_corrente(pais, periodo) %>%
    dplyr::filter(co_ano > 2015) %>%
    tidyr::pivot_wider(names_from = co_ano, values_from = value) %>%
    dplyr::mutate(dplyr::across(2:tidyselect::last_col(), scales::label_number_si(accuracy = 0.01))) %>%
    dplyr::rename(`Fluxo` = .data$trade_flow)

  kableExtra::kbl(agregados, format = "latex", booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>%
    kableExtra::add_header_above(c("Fluxos Comerciais" = 6), bold = TRUE) %>%
    kableExtra::collapse_rows(columns = 1, latex_hline = "major") %>%
    kableExtra::footnote(symbol = c("Fonte: Minist\u00e9rio da Economia"))

}
