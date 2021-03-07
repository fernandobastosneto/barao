#' Gr\u00e1fico, em propor\u00e7\u00e3o, dos principais produtos comercializados por um pa\u00eds com o mundo no \u00faltimo ano dispon\u00edvel
#'
#' @param pais um pa\u00eds
#'
#' @export

comerciomundo_grafico_produtos_proporcao <- function(pais) {

  max_ano <- barao::comerciomundo_dados_paises(pais) %>%
    dplyr::distinct(year) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::pull(max(year))

  comerciomundo::comtrade %>%
    dplyr::filter(reporter_code == barao::get_pais(pais, "comtrade")) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::group_by(commodity_code, trade_flow_code) %>%
    dplyr::summarise(value = sum(trade_value_us)) %>%
    dplyr::rename(CO_SH2 = commodity_code) %>%
    dplyr::left_join(comerciobr::dic_sh6_sh2) %>%
    dplyr::mutate(NO_SH2_POR = dplyr::case_when(
      stringr::str_length(NO_SH2_POR) > 20 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 20), ".."),
      TRUE ~ NO_SH2_POR)) %>%
    tidyr::unite("commodity_code", c("CO_SH2", "NO_SH2_POR"), sep = " - ") %>%
    dplyr::mutate(trade_flow_code = as.character(trade_flow_code),
                  trade_flow_code = dplyr::case_when(trade_flow_code == "1" ~ "Importa\u00e7\u00f5es",
                                                     trade_flow_code == "2" ~ "Exporta\u00e7\u00f5es")) %>%
    treemap::treemap(index = c("trade_flow_code", "commodity_code"),
                     vSize = "value",
                     type = "index",
                     palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                     align.labels=list(c("center", "center"), c("left", "top")),
                     title = glue::glue("{pais}-Mundo Principais produtos comercializados, propor\u00e7\u00e3o, em {max_ano}"))

}
