test_that("grafico produtos proporção funciona", {

  grafico <- barao::comerciobr_grafico_produtos_proporcao("México", "anual")
  expect_type(grafico, "list")

})
