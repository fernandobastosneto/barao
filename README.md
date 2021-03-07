
<!-- README.md is generated from README.Rmd. Please edit that file -->

# barao

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/fernandobastosneto/barao/workflows/R-CMD-check/badge.svg)](https://github.com/fernandobastosneto/barao/actions)
<!-- badges: end -->

<img src="barao_logo.png" height="300"/>

O pacote barão busca agregar, em um único lugar, um conjunto de dados e
funções relevantes para a análise de dados em relações internacionais,
com foco particular em dados de economia internacional.

<!-- ```{r} -->

<!-- imgurl <- fs::fs_path("barao.png") -->

<!-- hexSticker::sticker(imgurl, package="barão", s_x = 0.9, s_y = 0.95,  s_width = 0.65, s_height = 0.6, p_color = "#ffe066", p_y = 0.45, h_fill = "#247ba0", h_color = "#ffe066", p_family = "DotGothic", filename="barao_logo.png") -->

<!-- ``` -->

## Instalação

``` r
# install.packages("devtools")
devtools::install_github("fernandobastosneto/barao")
```

## Funcionalidades

  - ✅ - Comércio do Brasil com o Mundo (base de dados MEcon)
  - ⏳ - Comércio Internacional (base de dados COMTRADE-ONU)
  - ❌ - Investimentos (base de dados Banco Central, BNDES e UNCTAD)
  - ❌ - Dados Macroeconômicos (base de dados World Economic Report, FMI)
  - ❌ - Dados Tarifários (base de dados MACMAP-ONU)
