#' Relat\u00f3rio de rela\u00e7\u00f5es bilaterais comerciais Pa\u00eds-Mundo
#'
#' O relatório deve ser usado em conjunto com o objeto "dic_comtrade_mdic",
#' do pacote comerciomundo .
#'
#' @param pais um pa\u00eds
#'
#' @export

relatorio_pais_mundo <- function(pais) {

  pais_port <- comerciomundo::dic_comtrade_mdic %>%
    dplyr::filter(id == barao::get_pais(pais, "comtrade")) %>%
    dplyr::pull(no_pais)

  rmarkdown::render(system.file("rmd", "comerciomundo_report_pais.Rmd", package = "barao"),
                    params = list(
                      title = paste0(pais, "-Mundo , Dados Comerciais"),
                      pais = pais_port
                    ),
                    # intermediates_dir = here"temp",
                    # output_dir = here::here(),
                    output_dir = here::here("data/relatorios_comerciomundo"),
                    output_file = paste0("comerciomundo_", pais_port))
}
