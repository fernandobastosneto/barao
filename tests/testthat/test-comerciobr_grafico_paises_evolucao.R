test_that("grafico paises evolucao funciona", {

  grafico <- barao::comerciobr_grafico_paises_evolucao("Chile", "anual")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")

})
