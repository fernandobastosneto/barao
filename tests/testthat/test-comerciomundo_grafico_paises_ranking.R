test_that("comerciomundo grafico paises ranking funciona", {
  grafico <- barao::comerciomundo_grafico_paises_ranking("Chile")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")
})
