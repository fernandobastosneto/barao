#' @export

comerciomundo_dados_paises <- function(pais) {

  comerciomundo::comtrade %>%
    dplyr::filter(reporter_code == barao::get_pais(pais, "comtrade")) %>%
    dplyr::filter(partner_code != 0) %>%
    dplyr::group_by(trade_flow_code, partner_code, year) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(id = partner_code) %>%
    dplyr::left_join(comerciomundo::dic_comtrade_mdic, by = "id")

}
