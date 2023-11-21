#' url_countyhealthrankings
#'
#' @param fips vector of fips codes of counties, 5 characters each, like "10003"
#' @param year 2023
#'
#' @return vector of URLs
#' @export
#'
#' @examples
url_countyhealthrankings <- function(fips, year = 2023) {
  
  statename  <- tolower(EJAM::fips2statename( fips))
  countyname <- tolower(EJAM::fips2countyname(fips, includestate = ""))
  countyname <- trimws(gsub(" county", "", countyname))
  countyname <- gsub(" ", "-", countyname)
  # https://www.countyhealthrankings.org/explore-health-rankings/maryland/montgomery?year=2023
  baseurl <- "https://www.countyhealthrankings.org/explore-health-rankings/"
  url <- paste0(baseurl, statename, "/", countyname, "?year=", year)
  return(url)
}
