#' Dados de comércio bilateral de um país em relação a seus semelhantes
#'
#' Dados de comércio do Brasil com um determinado país, comparado com países
#' em condições semelhantes (selecionados pelo ranking).
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export
comerciobr_dados_paises <- function(pais, periodo) {

  if (periodo == "anual") {

    df <- comerciobr::sh1_df %>%
      dplyr::group_by(co_ano, no_pais, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::group_by(co_ano, path)
  }

  else {

    df <- comerciobr::sh1_df %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= barao::comerciobr_get_ultimomes()) %>%
      dplyr::group_by(co_ano, no_pais, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::group_by(co_ano, path)

  }

  # identificando a posi\u00e7\u00e3o do pais no ranking - exp

  rank_pais_exp <- df %>%
    dplyr::filter(path == "EXP") %>%
    dplyr::filter(no_pais == pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::pull(rank)

  # identificando a posi\u00e7\u00e3o do pais no ranking - imp

  rank_pais_imp <- df %>%
    dplyr::filter(path == "IMP") %>%
    dplyr::filter(no_pais == pais) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::pull(rank)

  # fazer um semi join para extrair apenas os pa\u00edses com os quais o pa\u00eds deve ser comparado
  ## tem que ser semi join porque queremos o ranking e o path
  ## queremos um elemento com duas caracter\u00edsticas

  rank_paises <- tibble::tibble(path = c(rep("EXP", 5),
                                         rep("IMP", 5)),
                                rank = c(rank_pais_exp,
                                         rank_pais_exp-1,
                                         rank_pais_exp+1,
                                         rank_pais_exp+2,
                                         rank_pais_exp-2,
                                         rank_pais_imp,
                                         rank_pais_imp-1,
                                         rank_pais_imp+1,
                                         rank_pais_imp+2,
                                         rank_pais_imp-2))

  get_paises <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::semi_join(rank_paises, by = c("path", "rank")) %>%
    dplyr::select(no_pais, path)

  df %>%
    dplyr::ungroup() %>%
    dplyr::semi_join(get_paises)

}


