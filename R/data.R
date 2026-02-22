#' US state distance matrix
#'
#' A 56 × 56 matrix of pairwise geographic distances (in metres) between
#' US states and territories, computed from boundary geometries via
#' \code{sf::st_distance}.
#'
#' @format A numeric matrix with 56 rows and 56 columns.
#'   Row and column names are state/territory names.
#' @source Computed from US Census TIGER/Line shapefiles using the
#'   \pkg{tigris}, \pkg{sf}, and \pkg{geosphere} packages.
#'   See Walker (2024), Pebesma (2018), Hijmans (2022).
"dist_matrix"
