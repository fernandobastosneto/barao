test_that("grafico paises funciona", {

  grafico <- barao::comerciobr_grafico_paises("Paraguai", "anual")
  expect_type(grafico, "list")
  expect_s3_class(grafico, "ggplot")

})
