#' Gráfico de índice de Concentração de Comércio - Índice de Herfindahl-Hirschman (HH) e Índice Intraindústria - Índice de Grubel-Lloyd (GG)
#'
#' @param pais um país
#'
#' @export

comerciobr_grafico_concentracao_intraindustria <- function(periodo, pais){

  if (periodo == "mensal") {

    frase <- paste0("Dados Agregados at\u00e9 ", barao2::meses(barao2::comerciobr_get_ultimomes()))
    dados_hh <- barao2::comerciobr_dados_concentracao(pais, periodo, "total")
    dados_hh_exp <- barao2::comerciobr_dados_concentracao(pais, periodo, "exp")
    dados_hh_imp <- barao2::comerciobr_dados_concentracao(pais, periodo,"imp")
    dados_intraindustria <- barao2::comerciobr_dados_intraindustria(pais, periodo)

    }else {

    frase <- paste0("Dados Anuais")
    dados_hh <- barao2::comerciobr_dados_concentracao(pais, periodo, "total")
    dados_hh_exp <- barao2::comerciobr_dados_concentracao(pais, periodo, "exp")
    dados_hh_imp <- barao2::comerciobr_dados_concentracao(pais, periodo,"imp")
    dados_intraindustria <- barao2::comerciobr_dados_intraindustria(pais, periodo)
  }

  df.anual <- dplyr::inner_join(dados_hh , dados_intraindustria ,by = "co_ano")
  df.anual <- dplyr::inner_join(df.anual ,dados_hh_exp ,by = "co_ano")
  df.anual <- dplyr::inner_join(df.anual ,dados_hh_imp ,by = "co_ano")

  df.TESTE <- df.anual %>%
    tidyr::pivot_longer(cols = HH:HH_imp, names_to = "tipo", values_to = "value")


  df.TESTE$tipo <- ifelse(df.TESTE$tipo == "HH", "Concentração Comercial - Total",df.TESTE$tipo)
  df.TESTE$tipo <- ifelse(df.TESTE$tipo == "HH_exp", "Concentração Comercial - Importação",df.TESTE$tipo)
  df.TESTE$tipo <- ifelse(df.TESTE$tipo == "HH_imp", "Concentração Comercial - Exportação",df.TESTE$tipo)
  df.TESTE$tipo <- ifelse(df.TESTE$tipo == "indiceGL_ponderado", "Comércio Intraindústria",df.TESTE$tipo)

  df.TESTE %>%
    dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(co_ano, value, fill =df.TESTE$tipo),
                      show.legend = F) +
    ggplot2::labs( x = NULL, y = NULL, subtitle = NULL ,caption = "Fonte: MDIC-ComexStat" ,fill="Anos" ) +
    ggplot2::facet_wrap(~factor(df.TESTE$tipo), scales = "fixed") +
    ggplot2::scale_y_continuous(breaks = c(0,0.25,0.50,0.75,1), limits = c(0,1)) +
    ggplot2::scale_x_discrete(breaks = scales::breaks_pretty()) +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()
}
