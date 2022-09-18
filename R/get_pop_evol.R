#' Calculate and plot population evolution
#'
#' Function to calculate the evolution of a population given a dataframe with
#' population dynamic parameters for different locations and species.
#'
#' @param df A dataframe with population dynamic parameters. It must contain (at least) the following columns:
#' 1. `locality`
#' 2. `species`: vernacular name.
#' 3. `lifestage`
#' 4. `reproduction`: probability to reproduce within a year. Real number
#' equal or above 0.
#' 5. `survival`: probability to survive within a year. Real number
#' between 0 (the animal dies for sure) and 1 (the animal survives for sure).
#' @param species Character with the species name. One of the species in column
#'   `species` of `df`.
#' @param locality Character with the locality name. One of the localities in
#'   column `locality` of `df`.
#' @param n Number of individuals per  life stage class. Default: 100
#'   individuals for each class.
#' @param years Number of years for the prediction. Default: 10 years.
#' @param colours: vector with hex colour codes, one per life stage. If one
#'   value is passed, all life stage categories have same color in the returned
#'   line plot. Default: black for all life stages.
#' @return A ggplot2 line plot with years on the x-axis, number of animals on
#'   the y-axis and the lifestage as colour and label. The lambda value is shown
#'   in the subtitle.
#' @export
#' @examples
#' library(fisrutils)
#' get_pop_evol(pop_dyn, species = "wild boar", locality = "Flanders")
get_pop_evol <- function(df,
                         species,
                         locality,
                         n = 100,
                         years  = 10,
                         colours = "#000000") {
  assertthat::assert_that(is.data.frame(df))
  p <- NULL
  return(p)
}
