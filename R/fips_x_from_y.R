
# Named all fips related functions starting with "fips...", so 
# if they return the fips, call the function                         fips_from_xyz()
# if they start from fips, call the function                         fips2xyz()
#   (not  countyname_from_countyfips etc.)

############################################################################# #
#     fips_from_xyz()  functions
############################################################################# #

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
############################################################################# #

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
############################################################################# #

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
############################################################################# #

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
############################################################################# #

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

#' fipsbg_from_anyfips
#' convert any FIPS codes to the FIPS of all the blockgroups that are
#'   among or within or containing those FIPS
#' @details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#' 
#' Takes a vector of one or more FIPS that could be State (2-digit), County (5-digit),
#'   Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips). 
#'   
#'   Returns unique vector of FIPS of all US blockgroups (including DC and Puerto Rico)
#'   that contain any specified blocks, are equal to any specified blockgroup fips, 
#'   or are contained within any provided tract/county/state FIPS. 
#'   
#' @param fips vector of US FIPS codes, as character or numeric,
#'   with or without their leading zeroes, each with as many characters
#' @seealso [fips_lead_zero()]
#' @return vector of blockgroup FIPS (or NA values) that may be much longer than the 
#'   vector of fips passed to this function.
#' @export
#'
#' @examples 
#'   # all blockgroups in one state
#'   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#'   length(fipsbg_from_anyfips("72"))
#'   # all blockgroups in this one county
#'   fipsbg_from_anyfips(30001)
#'   # all blockgroups that contain any of these 6 blocks (just one bg)
#'   fipsbg_from_anyfips( blockid2fips$blockfips[1:6])
#'   # 2 counties
#'   fipsbg_from_anyfips(c(36009,36011))
fipsbg_from_anyfips <- function(fips) {
  
  fips <- fips_lead_zero(fips) 
  
  # if smaller than bg (i.e., block fips), return just the parent bgs
  fips <- unique(substr(fips,1,12))
  
  # if bigger than bg, return all the child bgs
  all_us_bgfips <- blockgroupstats$bgfips
  
  # if nchar==2, state, so get all bg starting with that
  # if nchar is N, get all bg starting with those N characters
  
  len <- nchar(fips)
  bgfips <- fips[len == 12]
  nonbg <- fips[len !=  12]
  
  extrabgs <- sapply(nonbg, FUN = function(z) all_us_bgfips[startsWith(all_us_bgfips, z)])
  # extrabgs <- list(rep(NA, length(nonbg)))
  # for (thisone in nonbg) {
  #   extrabgs[[i]] <-   all_us_bgfips[startsWith(all_us_bgfips, thisone)]
  # }
  # extrabgs <- do.call(c,extrabgs)
  
  return(unlist(union(bgfips, extrabgs)))
}
############################################################################# #


############################################################################# #
#     fips2xyz()  functions
############################################################################# #


#' fips2state_abbrev - convert Census FIPS to 2-character state abbreviation
#'
#' @param fips vector of FIPS
#'
#' @return vector of abbreviations like "NY", "LA", "DE", etc.
#' @export
#'
#' @examples 
#'   cbind(fips_lead_zero(1:80), fips2state_abbrev(1:80), fips2statename(1:80))
fips2state_abbrev <- function(fips) {
  stateinfo$ST[match(substr(fips_lead_zero(fips), 1, 2), stateinfo$FIPS.ST)]
  # confirm returns same length as input, and check how it handles nonmatches
}
############################################################################# #

#' fips2state_fips  -  get state FIPS codes from any other FIPS codes like County FIPS
#' Tells you which State contains each County (or tract or blockgroup or block)
#' @param fips vector of FIPS
#'
#' @return vector of State FIPS 2 characters each
#' @export
#'
#' @examples
#'   fips2state_fips(fips_counties_from_statename(c("Delaware", "Rhode Island")))
fips2state_fips <- function(fips) {
  substr(fips_lead_zero(fips), 1, 2) 
}
############################################################################# #

#' fips2statename  -  get state names from Census FIPS codes
#'
#' @param fips vector of FIPS
#'
#' @return vector os state names
#' @export
#'
#' @examples
#'   cbind(fips_lead_zero(1:80), fips2state_abbrev(1:80), fips2statename(1:80))
fips2statename <- function(fips) {
  stateinfo$statename[match(substr(fips_lead_zero(fips), 1, 2), stateinfo$FIPS.ST)]
}
############################################################################# #

#' fips2countyname  -  Get full name of each county based on its Census FIPS code
#'
#' @param fips vector of US Census FIPS codes for Counties (5 digits each). can be string or numeric, with or without leading zeroes.
#' @param includestate can be ST, Statename, "", or TRUE to specify what if anything comes after county name and comma
#' 
#' @return vector of county names, optionally with comma and 2-character abbreviation or full state name.
#' @export
#'
#' @examples
#'   # names of all counties in ME and NY
#'   fips2countyname(fips_counties_from_state_abbrev(c("ME", "NY")), includestate = "ST")
#'    fips_counties_from_state_abbrev(c("AK", "LA"))  
fips2countyname <- function(fips, includestate = c("ST", "Statename", "")[1]) {
  # bad idea unless sticks to just countyfips input, countyname output, 1 to 1.
  # would be   general, and just need to be clear a state fips would result in all the counties in that state.
  # but a function returning countynames (instead of fips) like this is a bad idea since not unique IDs, espec without state abbrev as part of name
  ftype <- fipstype(fips)
  # if "state" , return all counties in state, and would be most clear but harder to work with if returned a list of hits for each element of input vector. simpler to return unique vector of whatever length is needed, so not 1 to 1 in/out necessarily.
  # if "county" return 1 for 1 in/out, not unique only?
  # if "tract", "blockgroup", "block",  return parent county once, not again for each tract inside the county ?
  out <- rep(NA, length(fips))
  out[ftype == "county"] <- blockgroupstats$countyname[match(fips[ftype == "county"], substr(blockgroupstats$bgfips,1,5))]  # 
  if (any(ftype != "county")) {
    warning("this function should only be used to convert county fips to county name, 1 to 1 - returning NA for fips that are not countyfips")
  }
  if (includestate == TRUE) {includestate <- "Statename"}
  if (includestate == "Statename") {addon <- fips2statename(fips)} else {
    if (includestate == "ST") {addon <- fips2state_abbrev(fips)} else {
      addon <- ""
    }
  }
  if (all(addon == "")) {return(out)} else {
    out <- paste(out, addon, sep = ", ")
    return(out)
  }
} 
############################################################################# #

#' fips2name
#'
#' @param fips vector of US Census FIPS codes for Counties (5 digits each) or States (2 digits).
#'   Can be string or numeric, with or without leading zeroes.
#' @param ... passed to fips2countyname() to control whether it appends something like , NY or , New York
#'   after county name
#'
#' @return vector of state and/or county names, 
#'   where county names optionally have comma and 2-character abbreviation or full state name.
#' @export
#'
#' @examples 
#'   fips2name(fips_counties_from_state_abbrev(c("AK", "LA"))  )
#'   fips2name(c(22, 02013))  # can have mix where some are a whole state and others are a county.
fips2name  <- function(fips, ...) {
  #   # more general than fips2countyname() or fips2statename() ... does either/both 
  fips <- fips_lead_zero(fips)
  
  out <- rep(NA, length(fips))
  out[fipstype(fips) == "state"]  <- fips2statename(fips = fips[fipstype(fips) == "state"])
  out[fipstype(fips) == "county"] <- fips2countyname(fips = fips[fipstype(fips) == "county"], ...)
  return(out)
}
############################################################################# #
