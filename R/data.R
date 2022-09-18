#' Sample of Population Dynamic Parameters
#'
#' A tibble (dataframe) containing a sample of Population Dynamic Parameters.
#'
#' Columns:
#' 1. `locality`
#' 2. `species`: vernacular name.
#' 3. `lifestage`
#' 4. `reproduction`: probability to reproduce within a year. Real number
#' equal or above 0.
#' 5. `survival`: probability to survive within a year. Real number
#' between 0 (the animal dies for sure) and 1 (the animal survives for sure).
#'
#' @family sample data
#' @source
#' <https://github.com/tdwg/camtrap-dp/tree/ad0278ef86ef518dacfb306c598dce97667cfb81/example>
#' @examples
#' \dontrun{
#' # pop_dyn.rda was created with the code below.
#' pop_dyn <- read.csv2(
#'   system.file(
#'     "tests/testthat/data/",
#'     "pop_dyn.csv",
#'     package = "fisrutils"
#'   ),
#'   sep = ";"
#' )
#' save(pop_dyn, file = "data/pop_dyn.rda")
#' }
"pop_dyn"
