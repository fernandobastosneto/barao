#' Relatório de relações bilaterais comerciais Brasil-país
#'
#' @param pais um país
#'
#' @export
#'
#' A função gera um relatório sobre os dados de comércio do Brasil para o bloco de países especificado usando o R Markdown.

relatorio_brasil_bloco <- function(bloco) {
  rmarkdown::render(system.file("rmd", "comerciobr_report_pais_bloco", package = "barao2"),
                    params = list(
                      title = paste0("Brasil-", bloco, " , Dados Comerciais"),
                      bloco = bloco
                    ),
                    # intermediates_dir = "temp",
                    output_dir = here::here("data/relatorios_comerciobr_blocos"),
                    output_file = paste0("comerciobr_", bloco))
}
