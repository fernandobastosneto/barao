test_that("dados comerciomundo corrente estão corretos", {

  # # Dados da função
  #
  # dado_imp <- comerciomundo_dados_corrente("China") %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(year == max(year)) %>%
  #   dplyr::filter(trade_flow == "Importacoes") %>%
  #   dplyr::pull(value)
  #
  # dado_exp <- comerciomundo_dados_corrente("China") %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(year == max(year)) %>%
  #   dplyr::filter(trade_flow == "Exportacoes") %>%
  #   dplyr::pull(value)
  #
  #
  # # Dados do COMTRADE
  #
  # query_imp <- list(
  #   "type" = "C",
  #   "freq" = "A",
  #   "px" = "HS",
  #   "ps" = comerciomundo_get_ultimoano("China"),
  #   "r" = 156,
  #   "p" = 0,
  #   "rg" = 1,
  #   "cc" = "TOTAL",
  #   "uitoken" = "d9b6d8c834c370a98eeefc352d918f84")
  # url <- "https://comtrade.un.org/api/get?"
  # request_imp <- httr::GET(url, query = query_imp)
  # request_conteudo_imp <- httr::content(request_imp)
  # res_imp <- request_conteudo_imp$dataset
  # dado_imp_esperado <- res_imp[[1]]$TradeValue
  #
  # query_exp <- list(
  #   "type" = "C",
  #   "freq" = "A",
  #   "px" = "HS",
  #   "ps" = comerciomundo_get_ultimoano("China"),
  #   "r" = 156,
  #   "p" = 0,
  #   "rg" = 2,
  #   "cc" = "TOTAL",
  #   "uitoken" = "d9b6d8c834c370a98eeefc352d918f84")
  # url <- "https://comtrade.un.org/api/get?"
  # request_exp <- httr::GET(url, query = query_exp)
  # request_conteudo_exp <- httr::content(request_exp)
  # res_exp <- request_conteudo_exp$dataset
  # dado_exp_esperado <- res_exp[[1]]$TradeValue
  #
  # expect_equal(dado_imp, dado_imp_esperado)
  # expect_equal(dado_exp, dado_exp_esperado)

})
