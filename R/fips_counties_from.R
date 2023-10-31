#' fips_state_from_state_abbrev
#'
#' @param ST vector of state abbreviations like c("NY","GA")
#'
#' @return vector of 2-digit state FIPS codes like c("10", "44", "44"),
#'   same length as input, so including any duplicates
#' @export
#'
#' @examples fips_state_from_state_abbrev("DE", "DE", "RI")
fips_state_from_state_abbrev <- function(ST) {
  stateinfo$FIPS.ST[match(ST, stateinfo$ST )] 
  # returns one per input, including repeats etc
  # retuns NA if no matching state abbrev found
  
  # not 
  # stateinfo$FIPS.ST[stateinfo$ST %in% ST]
  # note state_from_fips() is not really the inverse, though - see help on that function
}


#' fips_state_from_statename
#'
#' @param statename vector of state names like c("New York","Georgia")
#'
#' @return vector of 2-digit state FIPS codes like c("10", "44", "44"), 
#'   same length as input, so including any duplicates
#' @export
#'
#' @examples
#'   fips_state_from_statename("Delaware")
fips_state_from_statename <- function(statename) {
  # EJAM :: stateinfo
  stateinfo$FIPS.ST[match(statename, stateinfo$statename)]
  # returns one per input, including repeats etc
  # retuns NA if no matching state  found
}



#' fips_counties_from_statefips
#' Very similar to list_counties(state) from the tigris package.
#' @param statefips vector of 2-digit state FIPS codes like c("10", "44", "44") or c(10,44)
#'
#' @return vector of 5-digit character string county FIPS of all unique counties in those states
#' @export
#'
#' @examples
#'   fips_counties_from_statefips(c(10,44,44))
#'   fips_counties_from_statefips("10")
fips_counties_from_statefips <- function(statefips) {
  # EJAM :: blockgroupstats
  countyfips <- unique(substr(blockgroupstats$bgfips,1,5))
  countyfips_in_state <- unique(countyfips[substr(countyfips,1,2) %in% statefips])
  return(countyfips_in_state)
}

#' fips_counties_from_state_abbrev
#'
#' @param ST vector of state abbreviations like c("NY","GA")
#'
#' @return vector of 5-digit character string county FIPS of all unique counties in those states
#' @export
#'
#' @examples 
#'   fips_counties_from_state_abbrev("DE")
#'   fips_counties_from_state_abbrev("RI", "RI")
fips_counties_from_state_abbrev <- function(ST) {
  # countyfips <- unique(substr(blockgroupstats$bgfips,1,5))
  statefips <- fips_state_from_state_abbrev(ST)
  fips_counties_from_statefips(statefips = statefips)
  # countyfips_in_state <- unique(countyfips[substr(countyfips,1,2) %in% statefips])
  # return(countyfips_in_state)
}

#' fips_counties_from_statename
#'
#' @param statename vector of state names like c("New York","Georgia")
#'
#' @return vector of 5-digit character string county FIPS of all unique counties in those states
#' @export
#'
#' @examples fips_counties_from_statename("Delaware")
fips_counties_from_statename <- function(statename) {
  statefips <- fips_state_from_statename(statename)
  fips_counties_from_statefips(statefips = statefips)
}
