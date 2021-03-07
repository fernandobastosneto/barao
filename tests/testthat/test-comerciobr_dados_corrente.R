test_that("dados exportações estão corretos", {

  ano_final <- barao::comerciobr_get_ulimoano()-1
  ano_inicial <- 2010

  result <- paste0("http://api.comexstat.mdic.gov.br/general?filter=%7B%22yearStart%22:%22", ano_inicial,
                   "%22,%22yearEnd%22:%22", ano_final,
                   "%22,%22typeForm%22:1,%22typeOrder%22:1,%22filterList%22:%5B%7B%22id%22:%22noPaispt%22,%22text%22:%22Pa%C3%ADs%22,%22route%22:%22/pt/location/countries%22,%22type%22:%221%22,%22group%22:%22gerais%22,%22groupText%22:%22Gerais%22,%22hint%22:%22fieldsForm.general.noPais.description%22,%22placeholder%22:%22Pa%C3%ADses%22%7D%5D,%22filterArray%22:%5B%7B%22item%22:%5B%22160%22%5D,%22idInput%22:%22noPaispt%22%7D%5D,%22rangeFilter%22:%5B%5D,%22detailDatabase%22:%5B%7B%22id%22:%22noPaispt%22,%22text%22:%22Pa%C3%ADs%22%7D%5D,%22monthDetail%22:false,%22metricFOB%22:true,%22metricKG%22:false,%22metricStatistic%22:false,%22monthStart%22:%2201%22,%22monthEnd%22:%2212%22,%22formQueue%22:%22general%22,%22langDefault%22:%22pt%22,%22monthStartName%22:%22Janeiro%22,%22monthEndName%22:%22Dezembro%22%7D")

  teste <- httr::GET(result) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(simplifyDataFrame = T)

  teste <- teste[1] %>%
    tibble::as_tibble() %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::rename(co_ano = coAno, no_pais = noPaispt, value = vlFob) %>%
    dplyr::select(-no_pais) %>%
    dplyr::mutate(co_ano = as.double(co_ano),
                  value = as.double(value)) %>%
    dplyr::arrange(co_ano) %>%
    dplyr::pull(value)

  base_normal <- barao::comerciobr_dados_corrente("China", "anual") %>%
    dplyr::filter(trade_flow == "Exportacoes") %>%
    dplyr::select(-trade_flow) %>%
    dplyr::ungroup() %>%
    dplyr::pull(value)

  purrr::walk(1:10, ~ testthat::expect_equal(base_normal[.x], teste[.x]))
})

test_that("dados importações estão corretos", {

  ano_final <- barao::comerciobr_get_ulimoano()-1
  ano_inicial <- 2010

  result <- paste0("http://api.comexstat.mdic.gov.br/general?filter=%7B%22yearStart%22:%22", ano_inicial,
                   "%22,%22yearEnd%22:%22", ano_final,
                   "%22,%22typeForm%22:2,%22typeOrder%22:1,%22filterList%22:%5B%7B%22id%22:%22noPaispt%22,%22text%22:%22Pa%C3%ADs%22,%22route%22:%22/pt/location/countries%22,%22type%22:%221%22,%22group%22:%22gerais%22,%22groupText%22:%22Gerais%22,%22hint%22:%22fieldsForm.general.noPais.description%22,%22placeholder%22:%22Pa%C3%ADses%22%7D%5D,%22filterArray%22:%5B%7B%22item%22:%5B%22160%22%5D,%22idInput%22:%22noPaispt%22%7D%5D,%22rangeFilter%22:%5B%5D,%22detailDatabase%22:%5B%7B%22id%22:%22noPaispt%22,%22text%22:%22Pa%C3%ADs%22%7D%5D,%22monthDetail%22:false,%22metricFOB%22:true,%22metricKG%22:false,%22metricStatistic%22:false,%22monthStart%22:%2201%22,%22monthEnd%22:%2212%22,%22formQueue%22:%22general%22,%22langDefault%22:%22pt%22,%22monthStartName%22:%22Janeiro%22,%22monthEndName%22:%22Dezembro%22%7D")

  teste <- httr::GET(result) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(simplifyDataFrame = T)

  teste <- teste[1] %>%
    tibble::as_tibble() %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::rename(co_ano = coAno, no_pais = noPaispt, value = vlFob) %>%
    dplyr::select(-no_pais) %>%
    dplyr::mutate(co_ano = as.double(co_ano),
                  value = as.double(value)) %>%
    dplyr::arrange(co_ano) %>%
    dplyr::pull(value)

  base_normal <- barao::comerciobr_dados_corrente("China", "anual") %>%
    dplyr::filter(trade_flow == "Importacoes") %>%
    dplyr::select(-trade_flow) %>%
    dplyr::ungroup() %>%
    dplyr::pull(value)

  purrr::walk(1:10, ~ testthat::expect_equal(base_normal[.x], teste[.x]))
})
