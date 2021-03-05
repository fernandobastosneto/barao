#' Normaliza nomes dos países em diferentes bases de dados
#'
#' @param pais nome do país desejado (seja em português ou em inglês)
#' @param base base de dados de referência (pode ser "fmi", "mecon" ou "comtrade")
#' @return retorna o nome do país como adequado para a base em referência.
#'
#' @export

get_pais <- function(pais, base) {

  if (base == "fmi") {
    pais_escolhido <- barao::dic_paises_fmi %>%
      dplyr::filter_all(dplyr::any_vars(. == pais)) %>%
      dplyr::pull(.data$ing_fmi)
    if (length(pais_escolhido) < 1) {
      print("deu ruim")
    }
  }

  else if (base == "mecon") {
    pais_escolhido <- barao::dic_paises_fmi %>%
      dplyr::filter_all(dplyr::any_vars(. == pais)) %>%
      dplyr::pull(.data$port)
  }

  else if (base == "comtrade") {

    pais_escolhido <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter_all(dplyr::any_vars(. == pais)) %>%
      dplyr::pull(id)
  }

  else {
    print("escolha 'fmi', 'comtrade', ou 'mecon' como base de dados")
  }
  pais_escolhido
}
