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
  # wrong species
  testthat::expect_error(
    get_pop_evol(df = pop_dyn, species = "dog", locality = "Flanders"),
    paste("Invalid value for species parameter:",
          "dog.\nValid inputs are: wild boar and deer."
    )
  )
  # NULL is not allowed
  testthat::expect_error(
    get_pop_evol(df = pop_dyn, species = NULL, locality = "Flanders"),
    paste("Invalid value for species parameter:",
          "NULL.\nValid inputs are: wild boar and deer."
          )
  )
})

test_that("locality is one of the localities in df", {
  # wrong locality
  testthat::expect_error(
    get_pop_evol(df = pop_dyn, species = "deer", locality = "Vidalengo"),
    paste("Invalid value for locality parameter:",
          "Vidalengo.\nValid inputs are: Flanders, Sweden and Wallonia."
    )
  )
  # NULL is not allowed
  testthat::expect_error(
    get_pop_evol(df = pop_dyn, species = "deer", locality = NULL),
    paste("Invalid value for locality parameter:",
          "NULL.\nValid inputs are: Flanders, Sweden and Wallonia."
    )
  )
})

test_that("check n is a positive integer", {
  # character
  testthat::expect_error(
    get_pop_evol(df=  pop_dyn,
                 species = "deer",
                 locality = "Flanders",
                 n = "5"),
    "n must be a number."
  )
  # negative number
  testthat::expect_error(
    get_pop_evol(df=  pop_dyn,
                 species = "deer",
                 locality = "Flanders",
                 n = -5),
    "n must be a positive number."
  )
  # n is a decimal number
  testthat::expect_error(
    get_pop_evol(df=  pop_dyn,
                 species = "deer",
                 locality = "Flanders",
                 n = 5.2),
    paste("Number of individuals per life stage category, n,",
          "must be an integer. No decimal numbers are allowed."
    )
  )
})

test_that("check years is a positive integer", {
  # character
  testthat::expect_error(
    get_pop_evol(df=  pop_dyn,
                 species = "deer",
                 locality = "Flanders",
                 years = "5"),
    "years must be a number."
  )
  # negative number
  testthat::expect_error(
    get_pop_evol(df=  pop_dyn,
                 species = "deer",
                 locality = "Flanders",
                 years = -5),
    "years must be a positive number."
  )
  # years is a decimal number
  testthat::expect_error(
    get_pop_evol(df=  pop_dyn,
                 species = "deer",
                 locality = "Flanders",
                 years = 5.2),
    paste("years must be an integer. No decimal numbers are allowed."
    )
  )
})

test_that("Check warning and return NULL if no data left", {
  testthat::expect_warning(out <-
    get_pop_evol(df=  pop_dyn,
                 species = "deer",
                 locality = "Flanders",
                 years = 5),
    "No data left for the combination: deer (species) and Flanders (locality).",
    fixed = TRUE
    )
  testthat::expect_null(out)
})

test_that("Check no duplicates in lifestage", {
  pop_dyn_duplicates <- pop_dyn
  pop_dyn_duplicates[8,3] <- "juvenile"
  pop_dyn_duplicates[10,3] <- "adult"
  testthat::expect_error(
    get_pop_evol(df =  pop_dyn_duplicates,
                 species = "deer",
                 locality = "Wallonia"),
    paste("Duplicate life stage values for species deer in location",
          "Wallonia: adult, juvenile."
    )
  )
})
