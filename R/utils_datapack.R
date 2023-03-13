#' See info about the data sets in one or more packages
#' Wrapper for data() - just shows info in console and silently returns a data.frame
#' @param pkg a character vector giving the package(s) to look in for data sets
#' @param len Only affects what is printed to console - specifies the
#'   number of characters to limit Title to, making it easier to see in the console.
#'
#' @return data.frame with Item and Title as columns
#' @examples 
#'  datapack("datasets")
#'  datapack("MASS")
#'  datapack(ejampackages)
#' @export
#'
datapack <- function(pkg=ejampackages, len=30) {
  # browser()
  n <- length(pkg)
  ok <- rep(FALSE, n)
  for (i in 1:n) {
    ok[i] <- 0 != length(find.package(pkg[i], quiet = TRUE))
  }
  pkg <- pkg[ok]
  # browser()
  if (length(pkg) > 0) {
    zrows <- as.data.frame(data(package=pkg)$results)
    zrows <- zrows[ , c("Package", "Item", "Title")]
    zrows_narrow <- zrows
    zrows_narrow$Title  <- substr(zrows_narrow$Title, 1, len)
  } else {
    return(NULL)
  }
  print(zrows_narrow)
  invisible(zrows)
}
