#' Gráfico de índice de Concentração de Comércio - Índice de Herfindahl-Hirschman (HH) e Índice Intraindústria - Índice de Grubel-Lloyd (GG)
#'
#' @param pais um país
#'
#' @export

comerciobr_grafico_concentracao_intraindustria <- function(periodo, pais){

  if (periodo == "mensal") {

    frase <- paste0("Dados Agregados at\u00e9 ", barao::meses(barao::comerciobr_get_ultimomes()))
    dados_hh <- barao::comerciobr_dados_concentracao(pais, periodo)
    dados_intraindustria <- barao::comerciobr_dados_intraindustria(pais, periodo)
  }else {

    frase <- paste0("Dados Anuais")
    dados_hh <- barao::comerciobr_dados_concentracao(pais, periodo)
    dados_intraindustria <- barao::comerciobr_dados_intraindustria(pais, periodo)
  }

  df.anual <- inner_join(dados_hh, dados_intraindustria ,by = "co_ano")

  df.TESTE <- df.anual %>%
    pivot_longer(cols = HH:indiceGL_ponderado, names_to = "tipo", values_to = "value")

  df.TESTE$tipo <- ifelse(df.TESTE$tipo == "HH", "Índice de Concentração de Comércio - Índice HH","Índice Intraindústria - Índice de Grubel-Lloyd")

  df.TESTE %>%
    dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(co_ano, value, fill =df.TESTE$tipo),
                      show.legend = F) +
    ggplot2::labs( x = NULL, y = NULL, subtitle = NULL ,caption = "Ministério da Economia" ,
                   title = "Índice de Concentração Comércial e Comércio Intraindustrial",fill="Anos" ) +
    ggplot2::facet_wrap(~factor(df.TESTE$tipo), scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_discrete(breaks = scales::breaks_pretty()) +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()
}
