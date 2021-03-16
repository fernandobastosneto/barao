#' Relat\u00f3rio de rela\u00e7\u00f5es bilaterais comerciais Pa\u00eds-Mundo
#'
#' @param pais um pa\u00eds
#'
#' @export

relatorio_pais_mundo <- function(pais) {
  rmarkdown::render(system.file("rmd", "comerciomundo_report_pais.Rmd", package = "barao"),
                    params = list(
                      title = paste0(pais, "-Mundo , Dados Comerciais"),
                      pais = pais
                    ),
                    # intermediates_dir = here"temp",
                    # output_dir = here::here(),
                    output_file = paste0("comerciomundo_", pais, "_",
                                         barao::comerciobr_get_ulimoano(), "_",
                                         barao::meses(barao::comerciobr_get_ultimomes())))
}
