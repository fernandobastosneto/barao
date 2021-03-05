test_that("grafico fatores funciona", {

  grafico <- barao::comerciobr_grafico_fatores("Argentina", "anual", "isic")
  expect_type(grafico, "list")

})
