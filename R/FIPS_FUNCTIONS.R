

############################################################################# #
# Named all fips-related functions to start with "fips..."
############################################################################# #

# fipstype()
# fips_lead_zero()
# fips_from_table()

#   if func RETURNS the fips, named the function  fips_from_xyz()
#
#          fips_from_   functions are as follows:

#    fips_state_from_state_abbrev()
#    fips_state_from_statename()     # should it be statename or state_name
# fips_counties_from_statefips(   )  # should it be statefips or state_fips
# fips_counties_from_state_abbrev()
# fips_counties_from_statename(   )  # should it be statename or state_name
#       fips_bg_from_anyfips()
# and
#   see   getblocksnearby_from_fips() which uses  fips_bg_from_anyfips()

#     regions_as_sites()  would be a name that makes sense?
#    counties_as_sites()          # creates table like getblocksnearby()   and could get used by EJAM/R/mod_fips_picker-DRAFT.R
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

#   (and we are NOT using a naming scheme something like countyname_from_countyfips etc.)

############################################################################# #
############################################################################# #
############################################################################# #



############################################################################# #
#     misc. fips-related functions ####
############################################################################# #


#' FIPS - Identify what type of Census geography is each FIPS code (block, county, etc.)
#'
#' @param fips vector of one or more Census FIPS with or without leading zeroes, as strings or numeric
#'
#' @return vector of types: "block", "blockgroup", "tract", "county", or "state"
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
#'
#' @export
#'
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
############################################################################# #


#' FIPS - Add leading zeroes to fips codes if missing, replace with NA if length invalid
#'
#' Ensures FIPS has the leading zero, but does NOT VALIDATE FIPS -
#' It does NOT check if FIPS is valid other than checking its length.
#' fips could be a state, county, tract, blockgroup, or block FIPS code.
#'
#' @param fips vector of numeric or character US FIPS codes
#'
#' @return vector of same length
#'
#' @examples fips_lead_zero(c(1,"01",1234,"1234","12345",123456))
#'
#' @export
#'
fips_lead_zero <- function(fips) {
  
  if (any(as.numeric(fips) != fips, na.rm = T) | any(is.na(as.numeric(fips)))) {
    warning('some fips cannot be interpreted as numbers (e.g., are text or NA or logical')
    }
  
  #	TRY TO CLEAN UP vector of FIPS AND INFER GEOGRAPHIC SCALE
  # # Very similar to ejanalysis package file clean.fips()

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
####################################################### #


#' FIPS - Read and clean FIPS column from a table, after inferring which col it is
#'
#' Just read the codes in one column of a table obtained from something like read.csv, or excel, etc.
#'
#' @param fips_table data.frame or data.table of FIPS codes for counties, states, or tracts,
#'   for example, in a column whose name can be interpreted as FIPS
#'   (is one of the aliases like fips, countyfips, etc.)
#'   Aliases are: c("FIPS", "fips", "fips_code", "fipscode", "Fips", "statefips",
#'   "countyfips", "ST_FIPS", "st_fips", "ST_FIPS", "st_fips", "FIPS.ST",
#'   "FIPS.COUNTY", "FIPS.TRACT")
#' @param addleadzeroes whether to add leading zeroes where needed as for a State whose FIPS starts with "01"
#' @param inshiny used by server during shiny app
#'
#' @return vector of fips codes
#' @seealso [fips_bg_from_anyfips()] [fips_lead_zero()] [getblocksnearby_from_fips()] [fips_from_table()]
#'
#' @export
#'
fips_from_table <- function(fips_table, addleadzeroes=TRUE, inshiny=FALSE) {

  # fips_table can be data.frame or data.table, as long as colnames has one alid fips alias
  ## create named vector of FIPS codes (names used as location id)
  fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips',
                  'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
  if (any(tolower(colnames(fips_table)) %in% fips_alias)) {
    firstmatch <- intersect(fips_alias, colnames(fips_table))[1]
    if (addleadzeroes) {
      fips_vec <- fips_lead_zero(as.character(fips_table[[firstmatch]]))
    }
    names(fips_vec) <- as.character(fips_vec)

  } else {
    if (inshiny) {  # IF IN A SHINY REACTIVE:

      fips_vec <- NULL
    } else {
      # outside shiny:
      stop(    paste0('No FIPS column found. Please use one of the following names: ', paste0(fips_alias, collapse = ', ')))
    }
  }
  return(fips_vec)
}
####################################################### #


############################################################################# #
#     fips <- fips_from_xyz(xyz)  functions ####
############################################################################# #


#' FIPS - Get state fips for each state abbrev
#'
#' @param ST vector of state abbreviations like c("NY","GA")
#'
#' @return vector of 2-digit state FIPS codes like c("10", "44", "44"),
#'   same length as input, so including any duplicates
#'
#' @examples fips_state_from_state_abbrev("DE", "DE", "RI")
#'
#' @export
#'
fips_state_from_state_abbrev <- function(ST) {

  stateinfo$FIPS.ST[match(ST, stateinfo$ST )]
  # returns one per input, including repeats etc
  # retuns NA if no matching state abbrev found

  # not
  # stateinfo$FIPS.ST[stateinfo$ST %in% ST]
  # note state_from_fips() is not really the inverse, though - see help on that function
}
############################################################################# #


#' FIPS - Get state fips for each state name
#'
#' @param statename vector of state names like c("New York","Georgia")
#'
#' @return vector of 2-digit state FIPS codes like c("10", "44", "44"),
#'   same length as input, so including any duplicates
#' @examples
#'   fips_state_from_statename("Delaware")
#'
#' @export
#'
fips_state_from_statename <- function(statename) {

  # EJAM :: stateinfo
  stateinfo$FIPS.ST[match(statename, stateinfo$statename)]
  # returns one per input, including repeats etc
  # retuns NA if no matching state  found
}
############################################################################# #


#' FIPS - Get ALL county fips in specified states
#'
#' @details Very similar to list_counties(state) from the tigris package.
#' @param statefips vector of 2-digit state FIPS codes like c("10", "44", "44") or c(10,44)
#'
#' @return vector of 5-digit character string county FIPS of all unique counties in those states
#'
#' @examples
#'   fips_counties_from_statefips(c(10,44,44))
#'   fips_counties_from_statefips("10")
#'
#' @export
#'
fips_counties_from_statefips <- function(statefips) {

  # EJAM :: blockgroupstats
  countyfips <- unique(substr(blockgroupstats$bgfips,1,5))
  countyfips_in_state <- unique(countyfips[substr(countyfips,1,2) %in% statefips])
  return(countyfips_in_state)
}
############################################################################# #


#' FIPS - Get ALL county fips in specified states
#'
#' @param ST vector of state abbreviations like c("NY","GA")
#'
#' @return vector of 5-digit character string county FIPS of all unique counties in those states
#'
#' @examples
#'   fips_counties_from_state_abbrev("DE")
#'   fips_counties_from_state_abbrev("RI", "RI")
#'
#' @export
#'
fips_counties_from_state_abbrev <- function(ST) {

  # countyfips <- unique(substr(blockgroupstats$bgfips,1,5))
  statefips <- fips_state_from_state_abbrev(ST)
  fips_counties_from_statefips(statefips = statefips)
  # countyfips_in_state <- unique(countyfips[substr(countyfips,1,2) %in% statefips])
  # return(countyfips_in_state)
}
############################################################################# #


#' FIPS - Get ALL county fips in specified states
#'
#' @param statename vector of state names like c("New York","Georgia")
#'
#' @return vector of 5-digit character string county FIPS of all unique counties in those states
#'
#' @examples fips_counties_from_statename("Delaware")
#'
#' @export
#'
fips_counties_from_statename <- function(statename) {

  statefips <- fips_state_from_statename(statename)
  fips_counties_from_statefips(statefips = statefips)
}
############################################################################# #


#' FIPS - Get unique blockgroup fips in or containing specified fips of any type
#'
#' Convert any FIPS codes to the FIPS of all the blockgroups that are
#'   among or within or containing those FIPS
#'
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
#'
#' @examples
#'
#'   # all blockgroups in one state
#'   fips_counties_from_state_abbrev("DE")
#'   fips_bg_from_anyfips( fips_counties_from_state_abbrev("DE") )
#'
#'   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#'   length(fips_bg_from_anyfips("72"))
#'
#'   # all blockgroups in this one county
#'   fips_bg_from_anyfips(30001)
#'   fips_bg_from_anyfips("30001")
#'   fips_bg_from_anyfips(fips_counties_from_statename("Rhode Island")[1])
#'
#'   # all blockgroups that contain any of these 6 blocks (i.e., just one bg)
#'   fips_bg_from_anyfips( blockid2fips$blockfips[1:6])
#'
#'   # 2 counties
#'   fips_bg_from_anyfips(c(36009,36011))
#'
#' @export
#'
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


#' FIPS - Analyze US Counties as if they were sites, to get EJ indicators summary for each county
#'
#' @details This function provides one row per blockgroup.
#'    [getblocksnearby_from_fips()] provides one row per block.
#'    See more below under "Value"
#' @param fips County FIPS vector (ideally as character not numeric values),
#'   like fips_counties_from_state_abbrev("DE")
#' @seealso [getblocksnearby_from_fips()]
#' @return provides table similar to the output of getblocksnearby(),
#'   data.table with one row per blockgroup in these counties, or
#'   all pairs of county fips - bgid, and ejam_uniq_id (1 through N) assigned to each county
#'   but missing blockid and distance so not ready for doaggregate().
#' @examples
#'  counties_as_sites(c('01001','72153'))
#'  # Largest US Counties by ACS Population Totals:
#'  blockgroupstats[ , .(ST = ST[1], countypop = sum(pop)),
#'   by=.(FIPS = substr(bgfips,1,5))][order(-countypop),][1:20, .(
#'   CountyPopulation = prettyNum(countypop, big.mark = ","), FIPS, ST)]
#'
#' @export
#'
counties_as_sites <- function(fips) {

  if (any(is.numeric(fips))) {
    message("leading zeroes being inferred since FIPS was provided as numbers not character class")
    fips <- fips_lead_zero(fips)
  }
  # accept county fips vector
  # return counties2bgs table of pairs so doaggregate_blockgroups() or whatever can take that and do full EJ stats.

  county2bg <- bgpts[substr(bgfips,1,5) %in% fips, .(countyfips = substr(bgfips,1,5), bgid) ]
  county2bg[, ejam_uniq_id := .GRP , by = "countyfips"]
  county2bg[, .(ejam_uniq_id, countyfips, bgid)]
}
############################################### #


#' FIPS - Analyze US States as if they were sites, to get EJ indicators summary
#'
#' @details This function provides one row per blockgroup.
#'    [getblocksnearby_from_fips()] provides one row per block.
#'    See more below under "Value"
#' @param fips State FIPS vector, like c("01", "02") or
#'   fips_state_from_state_abbrev(c("DE", "RI"))
#'
#' @return provides table similar to the output of getblocksnearby(),
#'   data.table with one row per blockgroup in these states, or
#'   all pairs of states fips - bgid, and ejam_uniq_id (1 through N) assigned to each state
#'   but missing blockid and distance so not ready for doaggregate().
#' @examples
#'   s2b <- states_as_sites(fips_state_from_state_abbrev(c("DE", "RI")))
#'
#' @export
#'
states_as_sites <- function(fips) {

  if (any(is.numeric(fips))) {
    message("leading zeroes being inferred since FIPS was provided as numbers not character class")
    fips <- fips_lead_zero(fips)
  }
  valids <- stateinfo2$FIPS.ST[!is.na(stateinfo2$FIPS.ST)]
  if (!all(fips %in% valids)) {stop('some fips provided are not valid state fips')}
  # if (!all(fipstype(fips) == 'state')) {stop('some fips provided are not valid state fips')} # that only checks if 2 characters() with leading zeroes)

  # accept state fips vector
  # return counties2bgs table of pairs so doaggregate_blockgroups() or whatever can take that and do full EJ stats.

  state2bg <- bgpts[substr(bgfips,1,2) %in% fips, .(statefips = substr(bgfips,1,2), bgid) ]
  state2bg[, ejam_uniq_id := .GRP , by = "statefips"]
  state2bg[, .(ejam_uniq_id, statefips, bgid)]
}
############################################### #


############################################################################# #


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


#' FIPS - Get EPA Region number from state FIPS code
#'
#' @param stfips vector of one or more state fips codes (numbers or as strings)
#'
#' @return vector of numbers representing US EPA Regions
#'
#' @export
#'
fips_st2eparegion <- function(stfips) {

  EJAM::stateinfo$REGION[match(stfips, EJAM::stateinfo$FIPS.ST)]
}
############################################################################# #


#' FIPS - Get state abbreviations from any type of FIPS codes
#'
#' @param fips vector of FIPS
#' @return vector of abbreviations like "NY", "LA", "DE", etc.
#'
#' @examples
#'   cbind(
#'     stfips    = fips_lead_zero(1:80),
#'     ST     = fips2state_abbrev(1:80),
#'     statename = fips2statename(1:80)
#'   )
#'
#' @export
#'
fips2state_abbrev <- function(fips) {

  stateinfo$ST[match(substr(fips_lead_zero(fips), 1, 2), stateinfo$FIPS.ST)]
  # confirm returns same length as input, and check how it handles nonmatches
}
############################################################################# #


#' FIPS - Get state FIPS codes from any type of FIPS codes
#'
#' @details Tells you which State contains each County (or tract or blockgroup or block)
#' @param fips vector of FIPS
#' @return vector of State FIPS 2 characters each
#'
#' @examples
#'   fips2state_fips(fips_counties_from_statename(c("Delaware", "Rhode Island")))
#'
#' @export
#'
fips2state_fips <- function(fips) {

  substr(fips_lead_zero(fips), 1, 2)
}
############################################################################# #


#' FIPS - Get state names from any type of FIPS codes
#'
#' @param fips vector of FIPS
#'
#' @return vector of state names
#'
#' @examples
#'   cbind(fips_lead_zero(1:80), fips2state_abbrev(1:80), fips2statename(1:80))
#'
#' @export
#'
fips2statename <- function(fips) {

  stateinfo$statename[match(substr(fips_lead_zero(fips), 1, 2), stateinfo$FIPS.ST)]
}
############################################################################# #


#' FIPS - Get county names from county FIPS codes
#'
#' @param fips vector of US Census FIPS codes for Counties (5 digits each). can be string or numeric, with or without leading zeroes.
#' @param includestate can be ST, Statename, "", or TRUE to specify what if anything comes after county name and comma
#' @return vector of county names, optionally with comma and 2-character abbreviation or full state name.
#'
#' @examples
#'   # names of all counties in ME and NY
#'   fips2countyname(fips_counties_from_state_abbrev(c("ME", "NY")), includestate = "ST")
#'    fips_counties_from_state_abbrev(c("AK", "LA"))
#'
#' @export
#'
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


#' FIPS - Get county or state names from county or state FIPS codes
#'
#' @param fips vector of US Census FIPS codes for Counties (5 digits each) or States (2 digits).
#'   Can be string or numeric, with or without leading zeroes.
#' @param ... passed to fips2countyname() to control whether it appends something like , NY or , New York
#'   after county name
#' @return vector of state and/or county names,
#'   where county names optionally have comma and 2-character abbreviation or full state name.
#'
#' @examples
#'   fips2name(fips_counties_from_state_abbrev(c("AK", "LA"))  )
#'   fips2name(c(22, 02013))  # can have mix where some are a whole state and others are a county.
#'
#' @export
#'
fips2name  <- function(fips, ...) {

  #   # more general than fips2countyname() or fips2statename() ... does either/both
  fips <- fips_lead_zero(fips)

  out <- rep(NA, length(fips))
  out[fipstype(fips) == "state"]  <- fips2statename(fips = fips[fipstype(fips) == "state"])
  out[fipstype(fips) == "county"] <- fips2countyname(fips = fips[fipstype(fips) == "county"], ...)
  return(out)
}
############################################################################# #

