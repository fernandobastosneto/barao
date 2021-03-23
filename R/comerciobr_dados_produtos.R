#' Dados de principais produtos comercializados entre o Brasil e um determinado país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

comerciobr_dados_produtos <- function(pais, periodo) {

  df <- comerciobr::sh4_df %>%
    dplyr::filter(no_pais %in% pais)

  sh4 <- comerciobr::dic_sh6_sh4 %>%
    dplyr::select(no_sh4_por, co_sh4) %>%
    dplyr::distinct()

  if (periodo == "mensal") {

    ultimomes <- comerciobr_get_ultimomes()

    df <- df %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= ultimomes) %>%
      dplyr::group_by(co_ano, co_sh4, path) %>%
      dplyr::summarise(value = sum(value))
  }

  else {

    df <- df %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::group_by(co_ano, co_sh4, path) %>%
      dplyr::summarise(value = sum(value))
  }

  dezmais <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::group_by(path) %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::filter(rank < 6) %>%
    dplyr::select(co_sh4, path)

  df %>%
    dplyr::group_by(co_ano, path) %>%
    dplyr::mutate(total = sum(value)) %>%
    dplyr::group_by(co_ano) %>%
    dplyr::semi_join(dezmais) %>%
    dplyr::left_join(sh4)

}
