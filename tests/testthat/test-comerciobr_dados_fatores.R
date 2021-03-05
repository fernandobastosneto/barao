test_that("dados de fator isic estão corretos", {

  ano_final <- barao::comerciobr_get_ulimoano()-1
  ano_inicial <- 2010

  result <- paste0("http://api.comexstat.mdic.gov.br/general?filter=%7B%22yearStart%22:%22", ano_inicial,
                   "%22,%22yearEnd%22:%22", ano_final,
                   "%22,%22typeForm%22:1,%22typeOrder%22:1,%22filterList%22:[{%22id%22:%22noPaispt%22,%22text%22:%22Pa%C3%ADs%22,%22route%22:%22/pt/location/countries%22,%22type%22:%221%22,%22group%22:%22gerais%22,%22groupText%22:%22Gerais%22,%22hint%22:%22fieldsForm.general.noPais.description%22,%22placeholder%22:%22Pa%C3%ADses%22},{%22id%22:%22noIsicSectionpt%22,%22text%22:%22ISIC%20Se%C3%A7%C3%A3o%22,%22route%22:%22/pt/product-classification/section%22,%22type%22:%221%22,%22group%22:%22isic%22,%22groupText%22:%22Classifica%C3%A7%C3%A3o%20Internacional%20Padr%C3%A3o%20por%20Atividades%20Econ%C3%B4micas%20(ISIC)%22,%22hint%22:%22fieldsForm.general.noIsicSection.description%22,%22placeholder%22:%22ISIC%20Se%C3%A7%C3%A3o%22}],%22filterArray%22:[{%22item%22:[%22160%22],%22idInput%22:%22noPaispt%22},{%22item%22:[],%22idInput%22:%22noIsicSectionpt%22}],%22rangeFilter%22:[],%22detailDatabase%22:[{%22id%22:%22noPaispt%22,%22text%22:%22Pa%C3%ADs%22},{%22id%22:%22noIsicSectionpt%22,%22text%22:%22ISIC%20Se%C3%A7%C3%A3o%22,%22parentId%22:%22coIsicSection%22,%22parent%22:%22C%C3%B3digo%20ISIC%20Se%C3%A7%C3%A3o%22}],%22monthDetail%22:false,%22metricFOB%22:true,%22metricKG%22:false,%22metricStatistic%22:false,%22monthStart%22:%2201%22,%22monthEnd%22:%2212%22,%22formQueue%22:%22general%22,%22langDefault%22:%22pt%22,%22monthStartName%22:%22Janeiro%22,%22monthEndName%22:%22Dezembro%22}")

  teste <- httr::GET(result) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(simplifyDataFrame = T)

  teste <- teste[1] %>%
    tibble::as_tibble() %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::rename(co_ano = coAno, no_pais = noPaispt, value = vlFob,
                  no_isic = noIsicSectionpt) %>%
    dplyr::filter(coIsicSection == "A") %>%
    dplyr::select(-c(no_pais, coIsicSection, no_isic)) %>%
    dplyr::filter(co_ano == max(co_ano)) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::pull(value)

  base_normal <- barao::comerciobr_dados_fatores("China", "anual", "isic") %>%
    dplyr::filter(path == "EXP") %>%
    dplyr::filter(no_isic_secao == "Agropecuária") %>%
    dplyr::pull(value)

  expect_equal(teste, base_normal)
})
