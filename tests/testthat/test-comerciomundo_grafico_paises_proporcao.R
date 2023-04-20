test_that("grafico comerciomundo paises proporcao funciona", {
  grafico <- barao2::comerciomundo_grafico_paises_proporcao("Argentina")
  expect_type(grafico, "list")
})
