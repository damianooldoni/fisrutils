#' Calculate and plot population evolution
#'
#' Function to calculate the evolution of a population given a dataframe with
#' population dynamic parameters for different locations and species.
#'
#' @param df A dataframe with population dynamic parameters. It must contain (at
#'   least) the following columns:
#'   1. `locality`: locations.
#'   2. `species`: species identifier. Typically a vernacular or scientific
#'   name.
#'   3. `lifestage`
#'   4. `reproduction`: probability to reproduce within a year. Real number
#'   equal or above 0.
#'   5. `survival`: probability to survive within a year. Real number between 0
#'   (the animal dies for sure) and 1 (the animal survives for sure).
#' @param species Character with the species name. One of the species in column
#'   `species` of `df`.
#' @param locality Character with the locality name. One of the localities in
#'   column `locality` of `df`.
#' @param n Number of individuals per  life stage class. Default: 100
#'   individuals for each class.
#' @param years Number of years for the prediction. Default: 10 years.
#' @param colours Vector with hex colour codes, one per life stage. If one
#'   value is passed, all life stage categories have same color in the returned
#'   line plot. Default: black (`"#000000"`) for all life stages.
#' @return A ggplot2 line plot with years on the x-axis, number of animals on
#'   the y-axis and the lifestage as colour and label. The lambda value is shown
#'   in the subtitle. If no data are left in `df` for the given combination
#'   `species`/`location`, `NULL` is returned.
#'
#' @export
#' @importFrom dplyr %>% .data
#' @family population dynamics
#' @examples
#' library(fisrutils)
#' get_pop_evol(pop_dyn, species = "wild boar", locality = "Flanders")
get_pop_evol <- function(df,
                         species,
                         locality,
                         n = 100,
                         years  = 10,
                         colours = "#000000") {
  # Check dataframe with population dynamic parameters
  assertthat::assert_that(is.data.frame(df))
  names_cols <- c("locality", "species", "lifestage", "reproduction", "survival")
  assertthat::assert_that(
    all(names_cols %in% names(df)),
    msg = glue::glue(
      "One or more required columns missing in df. Required columns: {names_cols_collapse}.",
      names_cols_collapse = paste(names_cols, collapse = ", ")
    )
  )

  # Check species is one of the values in column df$species
  spp <- unique(df$species)
  check_value(species, spp, "species", null_allowed = FALSE)

  # check locality is one of the values in column df$locality
  localities <- unique(df$locality)
  check_value(locality, localities, "locality", null_allowed = FALSE)

  # Check n is a number
  assertthat::assert_that(is.numeric(n),
                          msg = "n must be a number."
  )
  # Check n is positive
  assertthat::assert_that(n >= 0,
                          msg = "n must be a positive number."
  )
  # Check n is an integer
  assertthat::assert_that(
    all(n == as.integer(n)),
    msg = paste("Number of individuals per life stage category, n,",
                "must be an integer. No decimal numbers are allowed.")
  )

  # Check years
  assertthat::assert_that(is.numeric(years),
                          msg = "years must be a number."
  )
  # Check years is positive
  assertthat::assert_that(years >= 0,
                          msg = "years must be a positive number."
  )
  # Check years is an integer
  assertthat::assert_that(
    all(years == as.integer(years)),
    msg = paste("years must be an integer. No decimal numbers are allowed.")
  )

  # Filter by species and locality
  sp <- species
  loc <- locality
  df <-
    df %>%
    dplyr::filter(.data$species == sp & .data$locality == loc)

  # Returns a warning if  there are no data left and return NULL
  if (nrow(df) == 0) {
    warning(glue::glue(
      "No data left for the combination: {sp} (species) and {loc} (locality)."
      )
    )
    return(NULL)
  }

  # Check lifestage values are unique for the selected species/location
  duplicates_lifestage <-
    df %>%
    dplyr::group_by(.data$lifestage) %>%
    dplyr::count() %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::distinct(.data$lifestage) %>%
    dplyr::pull(.data$lifestage)

  # Check life stage values are unique
  assertthat::assert_that(
    length(duplicates_lifestage) == 0,
    msg = glue::glue(
      "Duplicate life stage values for species {sp} in location {loc}:",
      " {duplicates_lifestage_collapse}.",
      duplicates_lifestage_collapse = paste(
        duplicates_lifestage,
        collapse = ", "
      )
    )
  )

  p <- NULL
  return(p)
}
