test_that("multiplication works", {
  grafico <- barao::comerciomundo_grafico_paises_evolucao("Chile")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")
})
