test_that("grafico produtos ranking funciona", {

  grafico <- barao::comerciobr_grafico_corrente("Japão", "anual")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")

})
