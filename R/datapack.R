#' Print list of data sets in package(s)
#' Wrapper for data(), for convenience
#' @param pkg a character vector giving the package(s) to look in for data sets
#' @param len number of characters to limit Title to, making it easier to see in the console.
#'
#' @return data.frame with Item and Title as columns
#' @export
#'
datapack <- function(pkg=NULL, len=30) {
  data.frame(
    Item  = substr(data(package=pkg)$results[ , c('Item')], 1, 999),
    Title = substr(data(package=pkg)$results[ , c('Title')], 1, len)
  )
}
