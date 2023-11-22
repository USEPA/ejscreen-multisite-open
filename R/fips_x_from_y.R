
# Named all fips-related functions to start with "fips..."

################################## #

#   if func RETURNS the fips, named the function  fips_from_xyz()
# 
#   fips <- fips_from_xyz(xyz)  functions

#    fips_state_from_state_abbrev()
#    fips_state_from_statename()     # should it be statename or state_name
# fips_counties_from_statefips(   )  # should it be statefips or state_fips
# fips_counties_from_state_abbrev()
# fips_counties_from_statename(   )  # should it be statename or state_name
#       fips_bg_from_anyfips()
#            bg_from_county()      # inconsistent naming?
#       fips_bg_from_countyfips()  # inconsistent naming?

#     regions_as_sites()  would be a name that makes sense
#    counties_as_sites()          # creates table like getblocksnearby() would
#      states_as_sites()  would be a name that makes sense
#      cities_as_sites()  would be a name that makes sense
#    tracts_as_ and blockgroups_as_  maybe useful?

################################## #

#   if func STARTS from fips, named the function  fips2xyz()
# 
#   xyz <- fips2xyz(fips)  functions
# 
#fips_st2eparegion()  # but not eparegion2statefips() ?? or 
#   fips2state_fips(    )     #  fips2statefips would be a more consistent name but not this func existed in EJAMejscreenapi and ejamlite
#   fips2state_abbrev(  )
#   fips2statename(     ) # should it be statename or state_name
#   fips2countyname()
#   fips2name()

################################## #

#   (NOT a naming scheme something like countyname_from_countyfips etc.)

############################################################################# #




############################################################################# #
#     fips <- fips_from_xyz(xyz)  functions ####
############################################################################# #


#' fips_state_from_state_abbrev - Get state fips for each state abbrev
#' Get state fips of each state abbrev
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

#' fips_state_from_statename - Get state fips for each state name
#' Get state fips for each state name
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

#' fips_counties_from_statefips - Get ALL county fips in specified states
#' Get all county fips in specified states
#' @details Very similar to list_counties(state) from the tigris package.
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

#' fips_counties_from_state_abbrev - Get ALL county fips in specified states
#' Get all county fips in specified states
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

#' fips_counties_from_statename - Get ALL county fips in specified states
#' Get all county fips in specified states
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
############################################################################# #


#' fips_bg_from_anyfips - Get unique blockgroup fips in or containing specified fips of any type
#' Convert any FIPS codes to the FIPS of all the blockgroups that are
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
#'   length(fips_bg_from_anyfips("72"))
#'   # all blockgroups in this one county
#'   fips_bg_from_anyfips(30001)
#'   # all blockgroups that contain any of these 6 blocks (just one bg)
#'   fips_bg_from_anyfips( blockid2fips$blockfips[1:6])
#'   # 2 counties
#'   fips_bg_from_anyfips(c(36009,36011))
fips_bg_from_anyfips <- function(fips) {
  
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


#' bg_from_county - Analyze US Counties as if they were sites, to get EJ indicators summary for each county
#' @details This function provides one row per blockgroup.
#'    [getblocksnearby_from_fips()] provides one row per block.
#' @param fips County FIPS vector (ideally as character not numeric values)
#' @seealso [getblocksnearby_from_fips()]
#' @return data.table with one row per blockgroup in these counties, or
#'   all pairs of county fips - bgid, and a unique siteid assigned to each county 
#' @export
#'
#' @examples 
#'  bg_from_county(c('01001','72153'))
#'  # Largest US Counties by ACS Population Totals:
#'  blockgroupstats[ , .(ST = ST[1], countypop = sum(pop)),
#'   by=.(FIPS = substr(bgfips,1,5))][order(-countypop),][1:20, .(
#'   CountyPopulation = prettyNum(countypop, big.mark = ","), FIPS, ST)]
#'  
bg_from_county <- function(fips) {
  if (any(is.numeric(fips))) {
    message("leading zeroes being inferred since FIPS was provided as numbers not character class")
    
  }
  # accept county fips vector
  # return counties2bgs table of pairs so doaggregate_blockgroups() or whatever can take that and do full EJ stats.
  
  county2bg <- bgpts[substr(bgfips,1,5) %in% fips, .(countyfips = substr(bgfips,1,5), bgid) ]
  county2bg[, siteid := .GRP , by = "countyfips"]
  county2bg[, .(siteid, countyfips, bgid)]
}
############################################### #

#' fips_bg_from_countyfips
#' @export
#' @inherit bg_from_county
fips_bg_from_countyfips <- bg_from_county
############################################################################# #

#' counties_as_sites
#' @export
#' @inherit bg_from_county
counties_as_sites <- bg_from_county

# out put 

############################################################################# #

# but see   getblocksnearby_from_fips() 
# which does a related thing for any type of fips





############################################################################# #
#     xyz <- fips2xyz(fips)  functions ####
############################################################################# #

# fips_st2eparegion()
# fips2state_abbrev()
# fips2state_fips()
# fips2statename()
# fips2countyname()
# fips2name()
############################################################################# #

#' fips_st2eparegion - Get EPA Region number from state FIPS code
#' Get EPA Region number from state FIPS code
#' @param stfips vector of one or more state fips codes (numbers or as strings)
#'
#' @return vector of numbers representing US EPA Regions
#' @export
#'
fips_st2eparegion <- function(stfips) {
  EJAM::stateinfo$REGION[match(stfips, EJAM::stateinfo$FIPS.ST)]
}
############################################################################# #

#' fips2state_abbrev - Get state abbreviations from any type of FIPS codes
#' Get state abbreviations from any type of FIPS codes
#' @param fips vector of FIPS
#'
#' @return vector of abbreviations like "NY", "LA", "DE", etc.
#' @export
#'
#' @examples 
#'   cbind(
#'     stfips    = fips_lead_zero(1:80), 
#'     ST     = fips2state_abbrev(1:80), 
#'     statename = fips2statename(1:80)
#'   )
fips2state_abbrev <- function(fips) {
  stateinfo$ST[match(substr(fips_lead_zero(fips), 1, 2), stateinfo$FIPS.ST)]
  # confirm returns same length as input, and check how it handles nonmatches
}
############################################################################# #

#' fips2state_fips - Get state FIPS codes from any type of FIPS codes
#' Get state FIPS codes from any type of FIPS codes
#' @details Tells you which State contains each County (or tract or blockgroup or block)
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


#' fips2statename - Get state names from any type of FIPS codes
#' Get state names from any type of FIPS codes
#' @param fips vector of FIPS
#'
#' @return vector of state names
#' @export
#'
#' @examples
#'   cbind(fips_lead_zero(1:80), fips2state_abbrev(1:80), fips2statename(1:80))
fips2statename <- function(fips) {
  stateinfo$statename[match(substr(fips_lead_zero(fips), 1, 2), stateinfo$FIPS.ST)]
}
############################################################################# #


#' fips2countyname - Get county names from county FIPS codes
#' Get county names from county FIPS codes
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

#' fips2name - Get county or state names from county or state FIPS codes
#' Get county or state names from county or state FIPS codes
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
