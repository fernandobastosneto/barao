test_that("multiplication works", {
  grafico <- barao::comerciomundo_grafico_corrente("Argentina")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")
})


