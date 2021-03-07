test_that("comerciomundo grafico produtos ranking funciona", {
  grafico <- barao::comerciomundo_grafico_produtos_ranking("Japão")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")
})
