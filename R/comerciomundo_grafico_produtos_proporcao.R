#' Gráfico, em proporção, dos principais produtos comercializados por um país com o mundo no último ano disponível
#'
#' @param pais um país
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
    dplyr::rename(co_sh2 = commodity_code) %>%
    dplyr::left_join(comerciobr::dic_sh6_sh2) %>%
    dplyr::mutate(no_sh2_por = dplyr::case_when(
      stringr::str_length(no_sh2_por) > 20 ~ paste0(stringr::str_sub(no_sh2_por, 1, 20), ".."),
      TRUE ~ no_sh2_por)) %>%
    tidyr::unite("commodity_code", c("co_sh2", "no_sh2_por"), sep = " - ") %>%
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
