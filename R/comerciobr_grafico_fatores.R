#' @export
comerciobr_grafico_fatores <- function(pais, periodo, fator) {

  if (fator == "cuci") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_cuci_sec"),
                       vSize = "value",
                       type = "index",
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Comercio Brasil-{pais} - Classificação CUCI"))

    }

  else if (fator == "isic") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_isic_secao"),
                       vSize = "value",
                       type = "index",
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Comercio Brasil-{pais} - Classificação ISIC"))

    }

  else if (fator == "cgce") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_cgce_n1"),
                       vSize = "value",
                       type = "index",
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Comercio Brasil-{pais} - Classificação CGCE"))

    }

  else if (fator == "fator") {

    barao::comerciobr_dados_fatores(pais, periodo, fator) %>%
      treemap::treemap(index = c("path", "no_fat_agreg"),
                       vSize = "value",
                       type = "index",
                       palette = ggthemes::tableau_color_pal('Tableau 10')(10),
                       title = glue::glue("Comercio Brasil-{pais} - Classificação Fator Agregado"))

    }
}



