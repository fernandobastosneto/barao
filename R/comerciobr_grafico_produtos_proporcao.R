#' @export
comerciobr_grafico_produtos_proporcao <- function(pais, periodo) {

  ano_min <- comerciobr_dados_produtos(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == min(co_ano)) %>%
    dplyr::distinct(co_ano) %>%
    dplyr::pull(min(co_ano))

  ano_max <- comerciobr_dados_produtos(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::distinct(co_ano) %>%
    dplyr::pull(max(co_ano))

  df <- barao::comerciobr_dados_produtos(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::mutate(prop = value/total) %>%

    dplyr::select(-c(value, total, rank)) %>%
    dplyr::mutate(no_sh4_por = dplyr::case_when(stringr::str_length(no_sh4_por) > 20 ~ paste0(stringr::str_sub(no_sh4_por, 1, 20), ".."),
                                         TRUE ~ no_sh4_por)) %>%
    dplyr::group_by(co_ano, path) %>%
    dplyr::mutate(total_prop = abs(sum(prop)-1)) %>%
    dplyr::ungroup()

  exp <- df %>%
    dplyr::filter(path == "EXP") %>%
    dplyr::distinct(total_prop) %>%
    dplyr::pull(total_prop)
  imp <- df %>%
    dplyr::filter(path == "IMP") %>%
    dplyr::distinct(total_prop) %>%
    dplyr::pull(total_prop)

    df %>%
      tibble::add_row(co_ano = ano_max,
                      co_sh4 = "0000",
                      path = "EXP",
                      no_sh4_por = "Outros",
                      prop = exp,
                      total_prop = NA) %>%
      tibble::add_row(co_ano = ano_max,
                      co_sh4 = "0000",
                      path = "IMP",
                      no_sh4_por = "Outros",
                      prop = imp,
                      total_prop = NA) %>%
    treemap::treemap(index = c("path", "no_sh4_por"),
                     vSize = "prop",
                     type = "index",
                     palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                     title = glue::glue("Brasil-{pais}, Proporção de Exportações e Importações"))
}
