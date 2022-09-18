test_that("df is a data.frame", {
  testthat::expect_error(get_pop_evol(df = 1, species = "wild boar", locality = "Flanders"))
})

test_that("df contains all required columns", {
  testthat::expect_error(
    get_pop_evol(df = pop_dyn[,1:3], species = "wild boar", locality = "Flanders"),
  paste0(
    "One or more required columns missing in df. ",
    "Required columns: locality, species, lifestage, reproduction, survival."
    )
  )
})

test_that("species is one of the species in df", {
  testthat::expect_error(
    get_pop_evol(df = pop_dyn, species = "dog", locality = "Flanders"),
    paste("Invalid value for species parameter:",
          "dog.\nValid inputs are: wild boar and deer."
          )
  )
})
