#' Relatório de relações bilaterais comerciais Brasil-país em formato
#' tabelão
#'
#' @param pais um país
#'
#' @export

relatorio_brasil_pais_tabelao <- function(pais) {
  rmarkdown::render(system.file("rmd", "comerciobr_report_pais_tabelao.Rmd", package = "barao"),
                    params = list(
                      title = paste0("Brasil-", pais, " , Dados Comerciais"),
                      pais = pais
                    ),
                    # intermediates_dir = "temp",
                    output_dir = here::here("data/relatorios_comerciobr_tabelao"),
                    output_file = paste0("comerciobr_", pais, "_tabela"))
}
