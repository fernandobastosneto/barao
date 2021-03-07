test_that("dados de comerciomundo paises estao corretos", {

  # # Dados adquiridos
  #
  # imp_resultado <- comerciomundo_dados_paises("Estados Unidos") %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(year == max(year)) %>%
  #   dplyr::filter(trade_flow_code == 1) %>%
  #   dplyr::slice_max(value, n = 1) %>%
  #   dplyr::pull(value)
  #
  # exp_resultado <- comerciomundo_dados_paises("Estados Unidos") %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(year == max(year)) %>%
  #   dplyr::filter(trade_flow_code == 2) %>%
  #   dplyr::slice_max(value, n = 1) %>%
  #   dplyr::pull(value)
  #
  #
  # # Dados esperados
  #
  # query_imp <- list(
  #     "type" = "C",
  #     "freq" = "A",
  #     "px" = "HS",
  #     "ps" = comerciomundo_get_ultimoano("Estados Unidos"),
  #     "r" = 842,
  #     "p" = "all",
  #     "rg" = 1,
  #     "cc" = "TOTAL",
  #     "uitoken" = "d9b6d8c834c370a98eeefc352d918f84")
  #
  # url <- "https://comtrade.un.org/api/get?"
  # request_imp <- httr::GET(url, query = query_imp)
  # request_conteudo_imp <- httr::content(request_imp)
  # res_imp <- request_conteudo_imp$dataset
  # dado_imp_esperado <- res_imp %>%
  #   purrr::transpose() %>%
  #   tibble::as_tibble() %>%
  #   tidyr::unnest(cols = c(pfCode, yr, period, periodDesc, aggrLevel, IsLeaf, rgCode,
  #                          rgDesc, rtCode, rtTitle, rt3ISO, ptCode, ptTitle, pt3ISO,
  #                          ptTitle2, pt3ISO2, cstCode, cstDesc, motCode, motDesc,
  #                          cmdCode, cmdDescE, qtCode, qtDesc, qtAltDesc,
  #                          TradeQuantity, NetWeight, TradeValue,
  #                          estCode)) %>%
  #   dplyr::filter(ptTitle != "World") %>%
  #   dplyr::slice_max(TradeValue) %>%
  #   dplyr::pull(TradeValue)
  #
  # query_exp <- list(
  #   "type" = "C",
  #   "freq" = "A",
  #   "px" = "HS",
  #   "ps" = comerciomundo_get_ultimoano("Estados Unidos"),
  #   "r" = 842,
  #   "p" = "all",
  #   "rg" = 2,
  #   "cc" = "TOTAL",
  #   "uitoken" = "d9b6d8c834c370a98eeefc352d918f84")
  #
  # url <- "https://comtrade.un.org/api/get?"
  # request_exp <- httr::GET(url, query = query_exp)
  # request_conteudo_exp <- httr::content(request_exp)
  # res_exp <- request_conteudo_exp$dataset
  # dado_exp_esperado <- res_exp %>%
  #   purrr::transpose() %>%
  #   tibble::as_tibble() %>%
  #   tidyr::unnest(cols = c(pfCode, yr, period, periodDesc, aggrLevel, IsLeaf, rgCode,
  #                          rgDesc, rtCode, rtTitle, rt3ISO, ptCode, ptTitle, pt3ISO,
  #                          ptTitle2, pt3ISO2, cstCode, cstDesc, motCode, motDesc,
  #                          cmdCode, cmdDescE, qtCode, qtDesc, qtAltDesc,
  #                          TradeQuantity, NetWeight, TradeValue,
  #                          estCode)) %>%
  #   dplyr::filter(ptTitle != "World") %>%
  #   dplyr::slice_max(TradeValue) %>%
  #   dplyr::pull(TradeValue)
  #
  #
  #
  # expect_equal(imp_resultado, dado_imp_esperado)
  # expect_equal(exp_resultado, dado_exp_esperado)

})
