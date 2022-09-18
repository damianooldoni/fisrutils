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
  testthat::expect_s3_class(out, c("gg", "ggplot"))
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
  testthat::expect_s3_class(out, c("gg", "ggplot"))
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
    "Same color for all life stage classes: #3a5c8e."
  )
  # A ggplot2 plot is returned
  testthat::expect_s3_class(out, c("gg", "ggplot"))
})

test_that("Check scaling effect while changing n", {
  # default: n = 100
  out100 <- get_pop_evol(
    df =  pop_dyn,
    species = "deer",
    locality = "Wallonia",
    colours = c("#3a5c8e", "#abc123", "#fba000", "#888888")
  )
  # n = 200 (double)
  out200 <- get_pop_evol(
    df =  pop_dyn,
    species = "deer",
    locality = "Wallonia",
    n = 200,
    colours = c("#3a5c8e", "#abc123", "#fba000", "#888888")
  )
  # Same lifestage, year and label column
  testthat::expect_equal(out100$data$lifestage,out200$data$lifestage)
  testthat::expect_equal(out100$data$year,out200$data$year)
  testthat::expect_equal(out100$data$label,out200$data$label)
  # n is scaled (double)
  testthat::expect_equal(out100$data$n*2,out200$data$n)
})

test_that("Check number of years is equal to number of x values", {
  # n = 20
  out20 <- get_pop_evol(
    df =  pop_dyn,
    species = "deer",
    locality = "Wallonia",
    years = 20,
    colours = c("#3a5c8e", "#abc123", "#fba000", "#888888")
  )
  # The df slot of the plot has 0 <= years < 20
  testthat::expect_equal(unique(out20$data$year), factor(0:19, ordered = TRUE))
})

test_that("Check some output plot slots", {
  # 1 color
  out1 <- get_pop_evol(
    df =  pop_dyn,
    species = "deer",
    locality = "Wallonia",
    colours = c("#3a5c8e")
  )
  # 4 colors
  out4 <- get_pop_evol(
    df =  pop_dyn,
    species = "deer",
    locality = "Wallonia",
    colours = c("#3a5c8e", "#005c01", "#f00c02", "#aaac00")
  )
  # A ggplot2 plot is returned
  testthat::expect_s3_class(out1, c("gg", "ggplot"))
  testthat::expect_s3_class(out4, c("gg", "ggplot"))

  # The df in slot data is not dependent on colours
  testthat::expect_equal(out1$data, out4$data)
  # The df in slot data contains the right columns
  testthat::expect_equal(names(out1$data), c("lifestage", "year", "n", "label"))

  # The slot labels is a list with the right values
  testthat::expect_true(is.list(out1$labels))
  testthat::expect_equal(
    out1$labels$title,
    "species: deer - locality : Wallonia"
  )
  testthat::expect_equal(
    out1$labels$subtitle,
    "population growth rate: 0.971"
  )
  testthat::expect_equal(out1$labels$x, c("year"))
  testthat::expect_equal(out1$labels$y, c("n"))
  testthat::expect_equal(out1$labels$group,c("lifestage"))
  testthat::expect_equal(out1$labels$colour,c("lifestage"))
  testthat::expect_equal(out1$labels$label,c("label"))

  # The plot has no legend
  testthat::expect_true(is.list(out1$theme))
  testthat::expect_equal(out1$theme$legend.position, c("none"))
})
