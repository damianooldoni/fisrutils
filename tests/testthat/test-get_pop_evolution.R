test_that("df is a data.frame", {
  testthat::expect_error(get_pop_evolution(df = 1, species = "wild boar", locality = "Flanders"))
})
