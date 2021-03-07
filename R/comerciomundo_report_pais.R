#' Relat\u00f3rio de rela\u00e7\u00f5es bilaterais comerciais pa\u00eds-mundo
#'
#' @param pais um pa\u00eds
#'
#' @export

comerciomundo_report_pais <- function(pais) {

  rmarkdown::render(system.file("rmd", "comerciomundo_report_pais.Rmd", package = "barao"),
                    params = list(
                      title = paste0(pais, "-Mundo", " , Dados Comerciais"),
                      pais = pais
                    ),
                    # intermediates_dir = "temp",
                    output_dir = here::here(),
                    output_file = paste0("comerciomundo_", pais, "_",
                                         barao::comerciomundo_get_ultimoano(pais), "_")
                    )

}
