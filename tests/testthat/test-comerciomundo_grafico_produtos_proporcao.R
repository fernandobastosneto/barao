test_that("comerciomundo grafico produtos proporcao funciona", {
  grafico <- barao::comerciomundo_grafico_produtos_proporcao("Portugal")
  expect_type(grafico, "list")
})
