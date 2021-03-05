#' Gráfico de Composição de Comércio por Agregação
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#' @param fator "isic", "fator", "cgce", "cuci"
#'
#' @export
comerciobr_grafico_fatores <- function(pais, periodo, fator) {

  if (periodo == "mensal") {
    frase <- paste0("agregado até ", barao::meses(barao::comerciobr_get_ultimomes()))
  }

  else {

    frase <- paste0("em ", barao::comerciobr_get_ulimoano()-1)
  }

  if (fator == "cuci") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_cuci_sec"),
                       vSize = "value",
                       type = "index",
                       align.labels=list(c("center", "center"), c("left", "top")),
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Classificação CUCI {frase}"))

    }

  else if (fator == "isic") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_isic_secao"),
                       vSize = "value",
                       type = "index",
                       align.labels=list(c("center", "center"), c("left", "top")),
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Classificação ISIC {frase}"))

    }

  else if (fator == "cgce") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_cgce_n1"),
                       vSize = "value",
                       type = "index",
                       align.labels=list(c("center", "center"), c("left", "top")),
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Classificação CGCE {frase}"))

    }

  else if (fator == "fator") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_fat_agreg"),
                       vSize = "value",
                       type = "index",
                       align.labels=list(c("center", "center"), c("left", "top")),
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Classificação Fator Agregado {frase}"))

    }
}



