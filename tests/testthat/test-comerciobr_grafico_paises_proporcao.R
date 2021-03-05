test_that("grafico paises proporcao funciona", {

  grafico <- barao::comerciobr_grafico_paises_proporcao("França", "anual")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")

})
