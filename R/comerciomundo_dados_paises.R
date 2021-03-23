#' Dados de comércio de um país com todos os demais
#'
#' @param pais um país
#'
#' @export

comerciomundo_dados_paises <- function(pais) {

  comtrade_mdic <- comerciomundo::dic_comtrade_mdic %>%
    dplyr::select(no_pais, text)

  df <- comerciomundo::comtrade %>%
    dplyr::filter(reporter_code == barao::get_pais(pais, "comtrade")) %>%
    dplyr::filter(partner_code != 0)


  dez_principais_paises_ultimo_ano <-  df %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::group_by(partner_code, trade_flow_code) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::group_by(trade_flow_code)  %>%
    dplyr::slice_max(value, n = 5) %>%
    dplyr::select(partner_code, trade_flow_code)

  df %>%
    dplyr::semi_join(dez_principais_paises_ultimo_ano) %>%
    dplyr::group_by(year, trade_flow_code, partner_code) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::rename(id = partner_code) %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::left_join(comerciomundo::dic_partners) %>%
    dplyr::left_join(comtrade_mdic) %>%
    dplyr::ungroup()
    # dplyr::select(-c(id, co_pais_isoa3, no_pais_ing, text))
    # dplyr::rename(no_pais = text)

}
