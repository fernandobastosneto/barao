test_that("grafico corrente funciona", {

  grafico <- barao::comerciobr_grafico_corrente("Argentina", "anual")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")

})
