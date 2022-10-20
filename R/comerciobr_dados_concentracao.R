#' Dados de concentração
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

comerciobr_dados_concentracao <- function(pais, periodo, fluxo) {

  if(periodo == "anual"){

    if(fluxo == "total"){
      dados_hh <- comerciobr2::sh4_df %>%
        dplyr::filter (no_pais == pais) %>%
        dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
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

    }else if(fluxo == "exp"){
      dados_hh_exp <- comerciobr2::sh4_df %>%
        dplyr::filter (no_pais == pais)%>%
        dplyr::filter(path == "EXP")%>%
        dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
        dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
        dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
        dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
        dplyr::filter(co_ano <= max(co_ano)-1) %>%
        dplyr::group_by(co_ano, co_sh2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::mutate(total_ano = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(porcentagem2 = value/total_ano) %>%
        dplyr::mutate(HH_exp = porcentagem2^2) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::summarise(HH_exp = sum(HH_exp)) %>%
        dplyr::arrange(desc(co_ano))

    }else if(fluxo == "imp"){
      dados_hh_imp <- comerciobr2::sh4_df %>%
        dplyr::filter (no_pais == pais)%>%
        dplyr::filter(path == "IMP")%>%
        dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
        dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
        dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
        dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
        dplyr::filter(co_ano <= max(co_ano)-1) %>%
        dplyr::group_by(co_ano, co_sh2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::mutate(total_ano = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(porcentagem3 = value/total_ano) %>%
        dplyr::mutate(HH_imp = porcentagem3^2) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::summarise(HH_imp = sum(HH_imp)) %>%
        dplyr::arrange(desc(co_ano))
    }

  } else {

    if(fluxo == "total"){
      dados_hh <- comerciobr2::sh4_df %>%
        dplyr::filter (no_pais == pais) %>%
        dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
        dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
        dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
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

    }else if(fluxo == "exp"){

      dados_hh_exp <- comerciobr2::sh4_df %>%
        dplyr::filter (no_pais == pais)%>%
        dplyr::filter(path == "EXP")%>%
        dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
        dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
        dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
        dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
        dplyr::group_by(co_ano, co_sh2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::mutate(total_ano = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(porcentagem2 = value/total_ano) %>%
        dplyr::mutate(HH_exp = porcentagem2^2) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::summarise(HH_exp = sum(HH_exp)) %>%
        dplyr::arrange(desc(co_ano))

    }else if(fluxo == "imp"){

      dados_hh_imp <- comerciobr2::sh4_df %>%
        dplyr::filter (no_pais == pais)%>%
        dplyr::filter(path == "IMP")%>%
        dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
        dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
        dplyr::mutate(co_sh2 = stringr::str_sub(co_sh4, 1, 2)) %>%
        dplyr::mutate(co_sh2 = as.numeric(co_sh2)) %>%
        dplyr::group_by(co_ano, co_sh2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::mutate(total_ano = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(porcentagem3 = value/total_ano) %>%
        dplyr::mutate(HH_imp = porcentagem3^2) %>%
        dplyr::group_by(co_ano) %>%
        dplyr::summarise(HH_imp = sum(HH_imp)) %>%
        dplyr::arrange(desc(co_ano))
    }

  }

}
