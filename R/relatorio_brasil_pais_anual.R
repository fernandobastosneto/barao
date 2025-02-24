#' Relatório de relações bilaterais comerciais Brasil-país
#'
#'
#' A função gera um relatório sobre os dados de comércio do Brasil com o país especificado somente com a parte anual
#'
#' @export



relatorio_brasil_pais_anual <- function(pais) {
  rmarkdown::render(system.file("rmd", "comerciobr_report_pais_anual.Rmd", package = "barao2"),
                    params = list(
                      title = paste0("Brasil-", pais, " , Dados Comerciais"),
                      pais = pais
                    ),
                    # intermediates_dir = "temp",
                    output_dir = here::here("data/relatorios_comerciobr"),
                    output_file = paste0("comerciobr_", pais))
}
