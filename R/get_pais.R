#' @export
get_pais <- function(pais, base) {

  if (base == "fmi") {
    pais_escolhido <- barao::dic_paises_fmi %>%
      dplyr::filter_all(dplyr::any_vars(. == pais)) %>%
      dplyr::pull(ing_fmi)
    if (length(pais_escolhido) < 1) {
      print("deu ruim")
    }
  }

  else if (base == "mecon") {
    pais_escolhido <- barao::dic_paises_fmi %>%
      dplyr::filter_all(dplyr::any_vars(. == pais)) %>%
      dplyr::pull(port)
  }

  else {
    print("escolha 'fmi' ou 'mecon' como base de dados")
  }
  pais_escolhido
}
