#' see names of datasets a package offers
#'
#' @param pkg character string name of an installed package
#'
#' @return vector of character string names of what is seen by data(package=pkg)
#'
#' @examples 
#'  datasetlist("datasets")
#'  datasetlist("MASS")
#' @export
datasetlist <- function(pkg) {
  data(package=pkg)$results[,'Item']
  }
