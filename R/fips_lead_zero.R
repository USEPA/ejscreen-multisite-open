#' fips_lead_zero
#' Add leading zeroes to fips codes if missing, replace with NA if length invalid
#' Note it does NOT VALIDATE FIPS - 
#'   It does NOT check if FIPS is valid other than checking its length seems OK, 
#'   i.e., it might be a state, county, tract, blockgroup, or block FIPS code.
#' @param fips vector of numeric or character US FIPS codes
#'
#' @return vector of same length 
#' @export
#'
#' @examples
fips_lead_zero <- function(fips) {
  
  #	TRY TO CLEAN UP vector of FIPS AND INFER GEOGRAPHIC SCALE
  # # Very similar to ejanalysis::clean.fips()
  
  fips[nchar(fips) == 0]	<- NA
  # 1 or 2 characters is state fips
  fips[nchar(fips) == 1]	<- paste0("0", fips[nchar(fips) == 1])
  # 3 is bad
  fips[nchar(fips) == 3]	<- NA
  # 4 or 5 is county
  fips[nchar(fips) == 4]	<- paste0("0", fips[nchar(fips) == 4])
  # 6-9 are bad
  fips[nchar(fips) == 6]	<- NA
  fips[nchar(fips) == 7]	<- NA
  fips[nchar(fips) == 8]	<- NA
  fips[nchar(fips) == 9]	<- NA  
  # 10 or 11 is tract
  fips[nchar(fips) == 10]	<- paste0("0", fips[nchar(fips) == 10])
  # 12 is blockgroup
  # 13 is bad
  fips[nchar(fips) == 13]	<- NA
  # 14-15 is block
  fips[nchar(fips) == 14]	<- paste0("0", fips[nchar(fips) == 14])
  fips[nchar(fips) >= 16]	<- NA
  
  # MAYBE COULD Remove if State code is invalid?
  
  return(fips)
}
