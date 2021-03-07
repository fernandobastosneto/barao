test_that("comerciomundo grafico produtos evolução funciona ", {
  grafico <- barao::comerciomundo_grafico_produtos_evolucao("Austrália")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")
})
