#' Dados Intraindustria
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export


comerciobr_dados_intraindustria <- function(pais, periodo) {

  if(periodo == "anual"){

    dados_intraindustria <- comerciobr2::sh4_df %>%
      dplyr::filter (no_pais == pais) %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
      dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
      dplyr::mutate(fator = dplyr::case_when(co_sh2 > 15 & co_sh2 < 68 ~ "Produto Industrializado",
                               co_sh2 > 71 & co_sh2 < 90 ~ "Produto Industrializado",
                               TRUE ~ "Outro")) %>%
      dplyr::filter(fator == "Produto Industrializado") %>%
      dplyr::group_by(co_ano, path, co_sh2) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = path, values_from = value, values_fill = 0) %>%
      dplyr::mutate(soma = EXP + IMP,
             modulo = abs(EXP - IMP)) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::summarise(indiceGL = 1 - (sum(modulo+1e-10)/sum(soma+1e-10)),
                media_ponderada = soma/sum(EXP+IMP),
                indiceGL_ponderado = media_ponderada * indiceGL) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::summarise(indiceGL_ponderado = sum(indiceGL_ponderado))%>%
      dplyr::arrange(desc(co_ano))
  }else{

    dados_intraindustria <- comerciobr2::sh4_df %>%
      dplyr::filter (no_pais == pais) %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
      dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
      dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
      dplyr::mutate(fator = dplyr::case_when(co_sh2 > 15 & co_sh2 < 68 ~ "Produto Industrializado",
                               co_sh2 > 71 & co_sh2 < 90 ~ "Produto Industrializado",
                               TRUE ~ "Outro")) %>%
      dplyr::filter(fator == "Produto Industrializado") %>%
      dplyr::group_by(co_ano, path, co_sh2) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = path, values_from = value, values_fill = 0) %>%
      dplyr::mutate(soma = EXP + IMP,
             modulo = abs(EXP - IMP)) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::summarise(indiceGL = 1 - (sum(modulo+1e-10)/sum(soma+1e-10)),
                media_ponderada = soma/sum(EXP+IMP),
                indiceGL_ponderado = media_ponderada * indiceGL) %>%
      dplyr::group_by(co_ano) %>%
      dplyr::summarise(indiceGL_ponderado = sum(indiceGL_ponderado))%>%
      dplyr::arrange(desc(co_ano))
  }

}
