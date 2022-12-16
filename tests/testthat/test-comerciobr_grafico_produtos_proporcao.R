test_that("grafico produtos proporção funciona", {

  grafico <- barao2::comerciobr_grafico_produtos_proporcao("México", "anual")
  expect_type(grafico, "list")

})
