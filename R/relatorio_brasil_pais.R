#' Relat\u00f3rio de rela\u00e7\u00f5es bilaterais comerciais Brasil-pa\u00eds
#'
#' @param pais um pa\u00eds
#'
#' @export

relatorio_brasil_pais <- function(pais) {
  rmarkdown::render(system.file("rmd", "comerciobr_report_pais.Rmd", package = "barao"),
                    params = list(
                      title = paste0("Brasil-", pais, " , Dados Comerciais"),
                      pais = pais
                    ),
                    # intermediates_dir = "temp",
                    output_dir = here::here("data/relatorios_comerciobr"),
                    output_file = paste0("comerciobr_", pais))
}
