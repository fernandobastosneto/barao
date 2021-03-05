test_that("grafico produtos funciona", {

  grafico <- barao::comerciobr_grafico_produtos("Espanha", "anual")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")

})
