#' @export
#'

comerciomundo_grafico_paises_proporcao <- function(pais) {

  max_ano <- barao::comerciomundo_dados_paises(pais) %>%
    dplyr::distinct(year) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::pull(max(year))

  barao::comerciomundo_dados_paises(pais) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::mutate(cor = dplyr::case_when(no_pais != "Brasil" & trade_flow_code == 1 ~ "#4E79A7",
                                         no_pais != "Brasil" & trade_flow_code == 2 ~ "#F28E2B",
                                         TRUE ~ "#2ca02c")) %>%
    dplyr::mutate(trade_flow_code = as.character(trade_flow_code),
                  trade_flow_code = dplyr::case_when(trade_flow_code == "1" ~ "Importações",
                                                     trade_flow_code == "2" ~ "Exportações")) %>%
    treemap::treemap(index = c("trade_flow_code", "no_pais"),
                     vSize = "value",
                     vColor = "cor",
                     type = "color",
                     align.labels=list(c("center", "center"), c("left", "top")),
                     # palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                     title = glue::glue("{pais} - Principais parceiros comerciais, proporção, em {max_ano}"))
}



