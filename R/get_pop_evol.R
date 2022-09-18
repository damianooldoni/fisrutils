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
#' @return A ggplot2 line plot with years on the x-axis (`year`), number of
#'   animals on the y-axis (`n`) and the life stage classes as colour and label.
#'   The lambda value (population growth rate) is shown in the subtitle. If no
#'   data are left in `df` for the given combination `species`/`location`,
#'   `NULL` is returned. Vector with colors is recycled if less colours than
#'   life stage classes are used.
#'
#' @export
#' @importFrom dplyr %>% .data
#' @family population dynamics
#' @examples
#' library(fisrutils)
#' # deafult color and initial number of individuals
#' get_pop_evol(pop_dyn, species = "wild boar", locality = "Flanders")
#'
#' # Use a not default color
#' get_pop_evol(
#'   pop_dyn,
#'   species = "deer",
#'   locality = "Wallonia",
#'   colour = "#005c01"
#' )
#'
#' # Use as many colours as life stage classes
#' colours_vec <- c("#3a5c8e", "#005c01", "#f00c02")
#' get_pop_evol(
#'   pop_dyn,
#'   species = "wild boar",
#'   locality = "Sweden",
#'   colours = colours_vec
#' )
#'
#' # Use a not default initial number of individuals
#' get_pop_evol(pop_dyn, species = "deer", locality = "Wallonia", n = 40)
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
  assertthat::assert_that(all(is.numeric(n)),
                          msg = "n must be a number."
  )
  # Check n is positive
  assertthat::assert_that(all(n >= 0),
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

  # Check colours
  lifestage_values <- unique(df$lifestage)
  n_lifestage_values <- length(lifestage_values)
  n_colours <- length(colours)
  if (n_colours > n_lifestage_values) {
    warning(glue::glue(
      "Number of colours ({n_colours}) greater than ",
      "number of lifestage values ({n_lifestage_values}). ",
      "Only the first {n_lifestage_values} colours are used."
      )
    )
    colours <- colours[1:n_lifestage_values]
  }

  if (n_colours > 1 & n_colours < n_lifestage_values) {
    warning(glue::glue(
      "Number of colours ({n_colours}) less than ",
      "number of lifestage values ({n_lifestage_values}). ",
      "Colours are recycled."
      )
    )
    colours <- rep(colours, length = n_lifestage_values)
  }

  if (n_colours == 1) {
    message(glue::glue("Same color for all life stage classes: {colours}."))
    colours <- rep(colours, n_lifestage_values)
  }

  # Initialize population matrix
  pop_matrix <-matrix(nrow = n_lifestage_values,
                      ncol = n_lifestage_values)

  # Add reproduction values
  pop_matrix[1,] <- df$reproduction

  # Add survivals
  for (i in 2:n_lifestage_values) {
    pop_matrix[i, i-1] <- df$survival[i-1]
  }
  pop_matrix[n_lifestage_values, n_lifestage_values] <- df$survival[n_lifestage_values]
  # Replace NAs with 0
  pop_matrix[is.na(pop_matrix)] <- 0

  # Get population stage vectors
  populations <- popbio::pop.projection(
    A = pop_matrix,
    n = rep(n, n_lifestage_values),
    iterations = years
  )

  # Get lambda value (population growth rate)
  lambda_value <- populations$lambda

  pop_evol <- populations$stage.vectors
  rownames(pop_evol) <- lifestage_values
  pop_evol <- dplyr::as_tibble(pop_evol, rownames = "lifestage")
  pop_evol$lifestage <- factor(pop_evol$lifestage,
                               levels = lifestage_values,
                               ordered = TRUE
  )

  # Pivot longer: years from cols to rows
  pop_evol <- tidyr::pivot_longer(
    data = pop_evol,
    cols = !dplyr::starts_with("lifestage"),
    names_to = "year",
    values_to = "n"
  )

  # Set year as factor
  pop_evol$year <- factor(pop_evol$year, levels = 0:(years-1), ordered = TRUE)

  # Add labels for plot
  pop_evol$label <- NA
  pop_evol$label[pop_evol$year == (years - 1)] <- lifestage_values

  # Create basic line plot
  p <- ggplot2::ggplot(
    data = pop_evol,
    mapping = ggplot2::aes(x = .data$year,
                           y = .data$n,
                           group = .data$lifestage,
                           color = .data$lifestage
                           )
    ) +
    ggplot2::geom_line()

  # Add label
  p <- p +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = .data$label),
      nudge_x = 1,
      na.rm = TRUE
    )

  # Map colors, remove legend
  p <- p +
    ggplot2::scale_color_manual(
      values = colours,
      limits = lifestage_values
    )

  # Add title
  p <- p +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(
      label = glue::glue("species: {sp} - locality : {loc}"),
      subtitle = glue::glue("population growth rate: {lambda_rounded}",
                            lambda_rounded = round(lambda_value, digits = 3)
      )
    )

  return(p)
}
