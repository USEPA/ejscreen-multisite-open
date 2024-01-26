#' Identify what type of Census geography is each FIPS code (block, county, etc.)
#'
#' @param fips vector of one or more Census FIPS with or without leading zeroes, as strings or numeric
#' 
#' @return vector of types: "block", "blockgroup", "tract", "county", or "state"
#' @export
#'
#' @examples
#'  fips_counties_from_statename(c("Connecticut", "Delaware") )
#'  # [1] "09001" "09003" "09005" "09007" "09009" "09011" "09013" "09015" "10001" "10003" "10005"
#'  fipstype(9001)
#'  fipstype("10001")
#'  # note blockid2fips is a large file, but can be obtained via [dataload_from_pins()]
#'  \dontrun{
#'  fipsexamples <- c(
#'    fips_state_from_statename("Alaska"),
#'    fips_counties_from_state_abbrev("DE")[1],
#'     substr(blockid2fips$blockfips[1],1,11),
#'     blockgroupstats$bgfips[1],
#'     blockid2fips$blockfips[1]
#'  )
#'  cbind(fipsexamples, type = fipstype(fipsexamples))
#' }
fipstype <- function(fips) {
  fips <- fips_lead_zero(fips = fips)
  ftype <- rep(NA, length(fips))
  ftype[nchar(fips) == 15] <- "block"
  ftype[nchar(fips) == 12] <- "blockgroup"
  ftype[nchar(fips) == 11] <- "tract"
  ftype[nchar(fips) ==  7] <- "city"  # e.g, 5560500 is Oshkosh, WI
  ftype[nchar(fips) ==  5] <- "county"
  ftype[nchar(fips) ==  2] <- "state"
  
  if (anyNA(ftype)) {
    warning("some fips do not seem to be block, blockgroup, tract, county, or state FIPS (lengths with leading zeroes should be 15,12,11,5,2 respectively")
  }
  return(ftype)
}
############################### 
