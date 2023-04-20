#' Lista de países a partir de um bloco
#'
#' @param bloco o nome de um bloco, de acordo com a base dic_blocos do pacote "comerciobr2"
#'
#' @return lista de países associados a determinado bloco
#'
#' @export

# tentativa de automatizar a geração de relatorio para blocos. Tentou-se gerar uma lista de blocos na qual conteria uma lista de países para cada bloco

comerciobr_lista_lista <- function(x) {

  lista_blocos <- comerciobr2::dic_blocos %>%
    dplyr::distinct(no_bloco) %>%
    dplyr::pull(no_bloco)

  final <- purrr::map(lista_blocos, barao2::comerciobr_blocos_paises)

}
