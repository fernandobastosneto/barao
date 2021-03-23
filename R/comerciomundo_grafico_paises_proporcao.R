#' Gráfico de ranking e proporção do fluxo de comércio do país com os demais país
#'
#' @param pais um país
#'
#' @export

comerciomundo_grafico_paises_proporcao <- function(pais) {

  max_ano <- barao::comerciomundo_dados_paises(pais) %>%
    dplyr::distinct(year) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::pull(max(year))

  comtrade_mdic <- comerciomundo::dic_comtrade_mdic %>%
    dplyr::select(no_pais, text)

  comerciomundo::comtrade %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::filter(reporter_code == barao::get_pais(pais, "comtrade")) %>%
    dplyr::filter(partner_code != 0) %>%
    dplyr::filter(trade_flow_code == 1 | trade_flow_code == 2) %>%
    dplyr::group_by(year, trade_flow_code, partner_code) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::rename(id = partner_code) %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::left_join(comerciomundo::dic_partners) %>%
    dplyr::left_join(comtrade_mdic) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cor = dplyr::case_when(no_pais != "Brasil" & trade_flow_code == 1 ~ "#4E79A7",
                                         no_pais != "Brasil" & trade_flow_code == 2 ~ "#F28E2B",
                                         TRUE ~ "#2ca02c")) %>%
    dplyr::mutate(trade_flow_code = as.character(trade_flow_code),
                  trade_flow_code = dplyr::case_when(trade_flow_code == "1" ~ "Importa\u00e7\u00f5es",
                                                     trade_flow_code == "2" ~ "Exporta\u00e7\u00f5es")) %>%
    treemap::treemap(index = c("trade_flow_code", "no_pais"),
                     vSize = "value",
                     vColor = "cor",
                     type = "color",
                     align.labels=list(c("center", "center"), c("left", "top")),
                     # palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                     title = glue::glue("{pais} - Principais parceiros comerciais, propor\u00e7\u00e3o, em {max_ano}"))
}



