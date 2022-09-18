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
  # return NULL if nothing to calculate
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

test_that("Check warning when n_colours > n_lifestage", {
  # n_colours = 6 > n_lifestage = 4
  testthat::expect_warning(out <-
    get_pop_evol(df =  pop_dyn,
                 species = "deer",
                 locality = "Wallonia",
                 colours = c("#3a5c1e",
                             "#3a5c2f",
                             "#fac3c1",
                             "#ffc3c4",
                             "#61ff30",
                             "#083a39")),
    paste("Number of colours (6) greater than",
          "number of lifestage values (4).",
          "Only the first 4 colours are used."
    ),
    fixed = TRUE
  )
  # A ggplot2 plot is returned
  testthat::expect_s3_class(a, c("gg", "ggplot"))
})

test_that("Check warning when n_colours < n_lifestage", {
  # n_colours = 2 < n_lifestage = 4
  testthat::expect_warning(
    out <- get_pop_evol(
      df =  pop_dyn,
      species = "deer",
      locality = "Wallonia",
      colours = c("#3a5c8e", "#fa51e")
    ),
    paste("Number of colours (2) less than",
          "number of lifestage values (4).",
          "Colours are recycled."
    ),
    fixed = TRUE
  )
  # A ggplot2 plot is returned
  testthat::expect_s3_class(a, c("gg", "ggplot"))
})

test_that("Check message and output plot for one color", {
  # n_colours = 1
  testthat::expect_message(
    out <- get_pop_evol(
      df =  pop_dyn,
      species = "deer",
      locality = "Wallonia",
      colours = c("#3a5c8e")
    ),
    paste("Number of colours (2) less than",
          "number of lifestage values (4).",
          "Colours are recycled."
    ),
    fixed = TRUE
  )
  # A ggplot2 plot is returned
  testthat::expect_s3_class(out, c("gg", "ggplot"))
})

test_that("Check returned plot", {
  # A ggplot2 plot is returned
  testthat::expect_s3_class(out, c("gg", "ggplot"))
})
