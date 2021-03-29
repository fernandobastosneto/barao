#' Relatório de relações bilaterais comerciais Brasil-país
#'
#' @param pais um país
#'
#' @export

relatorio_barao <- function(pais) {
  rmarkdown::render(system.file("rmd", "comerciobr_report_pais.Rmd", package = "barao"),
                    params = list(
                      title = paste0("Brasil-", pais, " , Dados Comerciais"),
                      pais = pais
                    ),
                    # intermediates_dir = "temp",
                    output_dir = here::here(),
                    output_file = paste0("comerciobr_", pais, "_",
                                         barao::comerciobr_get_ulimoano(), "_",
                                         barao::meses(barao::comerciobr_get_ultimomes())))
}
