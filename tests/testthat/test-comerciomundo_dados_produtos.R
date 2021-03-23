test_that("comerciomundo dados produtos estão corretos", {
  # imp_resultado <- comerciomundo_dados_produtos("Uruguay") %>%
  #   dplyr::filter(year == max(year)) %>%
  #   dplyr::filter(trade_flow_code == 1) %>%
  #   dplyr::slice_max(trade_value_us) %>%
  #   dplyr::pull(trade_value_us)
  #
  # exp_resultado <- comerciomundo_dados_produtos("Uruguay") %>%
  #   dplyr::filter(year == max(year)) %>%
  #   dplyr::filter(trade_flow_code == 2) %>%
  #   dplyr::slice_max(trade_value_us) %>%
  #   dplyr::pull(trade_value_us)
  #
  # query_imp <- list(
  #   "type" = "C",
  #   "freq" = "A",
  #   "px" = "HS",
  #   "ps" = comerciomundo_get_ultimoano("Uruguay"),
  #   "r" = 858,
  #   "p" = "all",
  #   "rg" = 1,
  #   "cc" = "AG2",
  #   "uitoken" = "d9b6d8c834c370a98eeefc352d918f84")
  # url <- "https://comtrade.un.org/api/get?"
  # request_imp <- httr::GET(url, query = query_imp)
  # request_conteudo_imp <- httr::content(request_imp)
  # res_imp <- request_conteudo_imp$dataset
  #
  # imp_esperado <- res_imp %>%
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
  #   "ps" = comerciomundo_get_ultimoano("Uruguay"),
  #   "r" = 858,
  #   "p" = "all",
  #   "rg" = 2,
  #   "cc" = "AG2",
  #   "uitoken" = "d9b6d8c834c370a98eeefc352d918f84")
  # url <- "https://comtrade.un.org/api/get?"
  # request_exp <- httr::GET(url, query = query_exp)
  # request_conteudo_exp <- httr::content(request_exp)
  # res_exp <- request_conteudo_exp$dataset
  #
  # exp_esperado <- res_exp %>%
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
  # expect_equal(imp_resultado, imp_esperado)
  # expect_equal(exp_resultado, exp_esperado)
})




