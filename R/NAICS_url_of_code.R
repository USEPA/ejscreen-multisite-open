#' Get URL for page with info about industry sector(s) by NAICS
#' See [naics.com] for more information on NAICS codes
#' 
#' @param naics vector of one or more NAICS codes, like 11,"31-33",325
#'
#' @return vector of URLs as strings like https://www.naics.com/six-digit-naics/?v=2017&code=22
#' @export
#' 
naics_url_of_code <- function(naics) {
  paste0("https://www.naics.com/six-digit-naics/?v=2017&code=", naics)
}

