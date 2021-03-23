#' Dados dos principais produtos comercializados de um país com o mundo
#'
#' @param pais um país
#'
#' @export

comerciomundo_dados_produtos <- function(pais) {

  df <- comerciomundo::comtrade %>%
    dplyr::filter(reporter_code == barao::get_pais(pais, "comtrade")) %>%
    dplyr::filter(partner_code != 0)

  dez_principais_produtos_ultimo_ano <-  df %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::group_by(commodity_code, trade_flow_code) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::group_by(trade_flow_code)  %>%
    dplyr::slice_max(value, n = 5) %>%
    dplyr::select(commodity_code, trade_flow_code)

  df <- df %>%
    dplyr::semi_join(dez_principais_produtos_ultimo_ano)
  df

}




