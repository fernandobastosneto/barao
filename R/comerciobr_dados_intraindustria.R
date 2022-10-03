#' Dados Intraindustria
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export


comerciobr_dados_intraindustria <- function(pais, periodo) {

  if(periodo == "anual"){

    dados_intraindustria <- comerciobr2::sh4_df %>%
      filter (no_pais == pais) %>%
      filter(co_ano <= max(co_ano)-1) %>%
      mutate(co_sh2 = str_sub(co_sh4, 1, 2)) %>%
      mutate(co_sh2 = as.numeric(co_sh2)) %>%
      mutate(fator = case_when(co_sh2 > 15 & co_sh2 < 68 ~ "Produto Industrializado",
                               co_sh2 > 71 & co_sh2 < 90 ~ "Produto Industrializado",
                               TRUE ~ "Outro")) %>%
      filter(fator == "Produto Industrializado") %>%
      group_by(co_ano, path, co_sh2) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      pivot_wider(names_from = path, values_from = value, values_fill = 0) %>%
      mutate(soma = EXP + IMP,
             modulo = abs(EXP - IMP)) %>%
      group_by(co_ano) %>%
      summarise(indiceGL = 1 - (sum(modulo+1e-10)/sum(soma+1e-10)),
                media_ponderada = soma/sum(EXP+IMP),
                indiceGL_ponderado = media_ponderada * indiceGL) %>%
      group_by(co_ano) %>%
      summarise(indiceGL_ponderado = sum(indiceGL_ponderado))%>%
      arrange(desc(co_ano))
  }else{

    dados_intraindustria <- comerciobr2::sh4_df %>%
      filter (no_pais == pais) %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
      mutate(co_sh2 = str_sub(co_sh4, 1, 2)) %>%
      mutate(co_sh2 = as.numeric(co_sh2)) %>%
      mutate(fator = case_when(co_sh2 > 15 & co_sh2 < 68 ~ "Produto Industrializado",
                               co_sh2 > 71 & co_sh2 < 90 ~ "Produto Industrializado",
                               TRUE ~ "Outro")) %>%
      filter(fator == "Produto Industrializado") %>%
      group_by(co_ano, path, co_sh2) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      pivot_wider(names_from = path, values_from = value, values_fill = 0) %>%
      mutate(soma = EXP + IMP,
             modulo = abs(EXP - IMP)) %>%
      group_by(co_ano) %>%
      summarise(indiceGL = 1 - (sum(modulo+1e-10)/sum(soma+1e-10)),
                media_ponderada = soma/sum(EXP+IMP),
                indiceGL_ponderado = media_ponderada * indiceGL) %>%
      group_by(co_ano) %>%
      summarise(indiceGL_ponderado = sum(indiceGL_ponderado))%>%
      arrange(desc(co_ano))
  }

}
