#' sic_categories - See the names of SIC industrial categories and their codes
#' Easy way to view, in RStudio console, the SIC categories.
#'   SIC all are 4-digit codes, like 7218 - Industrial launderers
#' @export
#'
#' @seealso   [SIC] [naics_categories]
sic_categories <- function() {
  cbind(EJAM::SIC)
}
