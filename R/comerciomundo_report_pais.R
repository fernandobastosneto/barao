#' Relatório de relações bilaterais comerciais país-mundo
#'
#' @param pais um país
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
