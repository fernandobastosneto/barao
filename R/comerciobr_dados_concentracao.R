#' Dados de concentração
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

comerciobr_dados_concentracao <- function(pais, periodo) {

  if(periodo == "anual"){

    dados_hh <- comerciobr::sh4_df %>%
      dplyr::filter (no_pais == pais) %>%
      dplyr::mutate(co_sh2 = str_sub(co_sh4, 1, 2)) %>%
      dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::group_by(co_ano, co_sh2) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::mutate(total_ano = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(porcentagem = value/total_ano) %>%
      dplyr::mutate(HH = porcentagem^2) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::summarise(HH = sum(HH)) %>%
      dplyr::arrange(desc(co_ano))

  }else {

      dados_hh <- comerciobr::sh4_df %>%
        dplyr::filter (no_pais == pais) %>%
        dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
        dplyr::filter(co_mes <= barao::comerciobr_get_ultimomes()) %>%
        dplyr::mutate(co_sh2 = str_sub(co_sh4, 1, 2)) %>%
        dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
        dplyr::group_by(co_ano, co_sh2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::mutate(total_ano = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(porcentagem = value/total_ano) %>%
        dplyr::mutate(HH = porcentagem^2) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::summarise(HH = sum(HH)) %>%
        dplyr::arrange(desc(co_ano))
      }
}
