############################################################################# #
# Named all fips-related functions to start with "fips..."
############################################################################# #

################################## #     misc fips-related functions 

# fips_valid()
# fipstype()
# fips_lead_zero()
# counties_as_sites() # creates table like getblocksnearby() does. and could use in mod_fips_picker-DRAFT.R
#   states_as_sites()   
# is.island()

################################## #    fips_ from_ 

###  if func is the OUTPUT, names are like   fips <- fips_from_x(x)

#          fips_from_table()
#     name2fips() and names2fips()  # inconsistent naming, but useful aliases
#          fips_from_name()  # same as name2fips()
#    fips_state_from_state_abbrev()
#    fips_state_from_statename()     # should it be statename or state_name
# fips_counties_from_statefips(   )  # should it be statefips or state_fips
# fips_counties_from_state_abbrev()
# fips_counties_from_statename(   )  # should it be statename or state_name
# fips_counties_from_countyname() 
# fips_counties_from_countynamefull()  internal helper
#       fips_bg_from_anyfips()
### and
###   see   getblocksnearby_from_fips() which uses  fips_bg_from_anyfips()

##NOT  cities_as_sites()  would be a name that makes sense but not used.
##NOT regions_as_sites()  would be a name that makes sense but not used.
##NOT   tracts_as_ and blockgroups_as_  maybe useful?

################################## #    fips2...

####  if fips is the INPUT, names are like   x <- fips2x(fips) 
#
# fips_st2eparegion()  # but not eparegion2statefips() ?? or
#    fips2state_fips(    )     #  fips2statefips would be a more consistent name ?
#    fips2state_abbrev(  )
#    fips2statename(     ) # should it be statename or state_name
#    fips2countyname()
#    fips2name()    # inverse of name2fips()
############################################################################# #

############################################################################# #
#     misc functions ####
############################################################################# #

fips_valid <- function(fips) {
  
  # before using this, one should clean fips using 
  #   fips <- fips_lead_zero(fips)
  
  ok <- rep(FALSE, length(fips))
  suppressWarnings({kind <- fipstype(fips)})
  kind[is.na(kind)] <- "fail"
  ok[kind == "state"]      <- fips[kind == "state"]  %in% stateinfo2$FIPS.ST[!is.na(stateinfo2$FIPS.ST)]
  ok[kind == "county"]     <- fips[kind == "county"] %in% substr(blockgroupstats$bgfips[!is.na(blockgroupstats$bgfips)], 1, 5)
  ok[kind == "tract"]      <- fips[kind == "tract"]  %in%  substr(blockgroupstats$bgfips[!is.na(blockgroupstats$bgfips)], 1, 11)
  ok[kind == "blockgroup"] <- fips[kind == "blockgroup"] %in%  substr(blockgroupstats$bgfips[!is.na(blockgroupstats$bgfips)], 1, 12)
  # note not all bgfips were in bgpts table:  setdiff_yx(bgpts$bgfips, blockgroupstats$bgfips[!is.na(blockgroupstats$bgfips)])
  if (any(kind == "block")) {
    if (!exists("blockid2fips")) {
      dataload_from_pins("blockid2fips")
    }
    ok[kind == "block"] <- fips[kind == "block"] %in% blockid2fips$blockfips
  }
  return(ok)
}
############################################################################# #


#' FIPS - Identify what type of Census geography is each FIPS code (block, county, etc.)
#' @details Note a number of length 11 is an ambiguous case - might be a tract fips
#'   but might be a blockgroup fips with a missing leading zero. 
#' @param fips vector of one or more Census FIPS with or without leading zeroes, as strings or numeric
#'
#' @return vector of types: "block", "blockgroup", "tract", "county", or "state"
#'
#' @examples
#'  testfips <- c("1", "12", "123", "1234", "12345", "", NA, "words")
#'  fipstype(testfips)
#'  
#'  testfips16 = c("1", "12", "123", "1234", "12345", "123456", "1234567", "12345678", "123456789",
#'                 "1234567890", "12345678901", "123456789012", "1234567890123", "12345678901234",
#'                                "123456789012345", "1234567890123456")
#'  cbind(fipstype(testfips16), fips_lead_zero(testfips16), testfips16)
#'
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
  
  fips <- fips_lead_zero(fips = fips) # cleans them so each is NA or a valid nchar() string
  ftype <- rep(NA, length(fips))
  
  ftype[nchar(fips, keepNA = FALSE) == 15] <- "block"
  ftype[nchar(fips, keepNA = FALSE) == 12] <- "blockgroup"
  ftype[nchar(fips, keepNA = FALSE) == 11] <- "tract"
  ftype[nchar(fips, keepNA = FALSE) ==  7] <- "city"  # e.g, 5560500 is Oshkosh, WI
  ftype[nchar(fips, keepNA = FALSE) ==  5] <- "county"
  ftype[!is.na(fips) & nchar(fips) ==  2] <- "state"
  
  if (anyNA(ftype)) {
    
    howmanyna = sum(is.na(ftype))
    warning("NA returned for ", howmanyna," fips that do not seem to be block, blockgroup, tract, county, or state FIPS (lengths with leading zeroes should be 15,12,11,5,2 respectively")
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
#' @seealso [fips_valid()] [fipstype()]
#' @return vector of same length
#'
#' @examples
#' testfips1 <- c(1,"01",1234,"1234","12345",123456)
#' testfips <- c(1, "1", "12", "123", "1234", "12345", "", NA, "words")
#' fips_lead_zero(testfips1)
#' fips_lead_zero(testfips)
#'
#' @export
#'
fips_lead_zero <- function(fips) {
  
  # if there are decimal places, negative signs, spaces, etc. then treat those fips as NA values
  just_numerals = function(x) {!grepl("[^0123456789]", x)}
  fips[!just_numerals(fips)] <- NA 
  # if (anyNA(fips)) {warning('some fips cannot be interpreted as numbers (e.g., are text or NA or logical')}
  
  #	TRY TO CLEAN UP vector of FIPS AND INFER GEOGRAPHIC SCALE
  
  ## keepNA = FALSE means that nchar() returns the number 2 instead of NA, which makes this work right.
  
  fips[nchar(fips, keepNA = FALSE) == 0]	<- NA
  # 1 or 2 characters is state fips
  fips[nchar(fips, keepNA = FALSE) == 1]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 1])
  # 3 is bad
  fips[nchar(fips, keepNA = FALSE) == 3]	<- NA
  # 4 or 5 is county
  fips[nchar(fips, keepNA = FALSE) == 4]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 4])
  # 6-9 are bad
  fips[nchar(fips, keepNA = FALSE) == 6]	<- NA
  fips[nchar(fips, keepNA = FALSE) == 7]	<- NA
  fips[nchar(fips, keepNA = FALSE) == 8]	<- NA
  fips[nchar(fips, keepNA = FALSE) == 9]	<- NA
  # 10 or 11 is tract
  fips[nchar(fips, keepNA = FALSE) == 10]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 10])
  # 12 is blockgroup
  # 13 is bad
  fips[nchar(fips, keepNA = FALSE) == 13]	<- NA
  # 14-15 is block
  fips[nchar(fips, keepNA = FALSE) == 14]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 14])
  fips[nchar(fips, keepNA = FALSE) >= 16]	<- NA
  
  # convert to NA any other things like text that is not actually a number
  suppressWarnings({fips[is.na(as.numeric(fips))] <- NA})
  
  # MAYBE should remove or set to NA when State or County code is invalid? another function can check for that.
  
  if (anyNA(fips)) {
    howmanyna = sum(is.na(fips))
    warning(howmanyna, " fips had invalid number of characters (digits) or were NA values")
  }
  
  return(fips)
}
####################################################### #


#' FIPS - Analyze US Counties as if they were sites, to get EJ indicators summary for each county
#'
#' @details This function provides one row per blockgroup.
#'    [getblocksnearby_from_fips()] provides one row per block.
#'    See more below under "Value"
#' @param fips County FIPS vector,

#'   like fips_counties_from_state_abbrev("DE")
#' @seealso [getblocksnearby_from_fips()]
#' @return provides table similar to the output of getblocksnearby(),
#'   data.table with one row per blockgroup in these counties, or
#'   all pairs of county fips - bgid, and ejam_uniq_id (1 through N) assigned to each county
#'   but missing blockid and distance so not ready for doaggregate().
#' @examples
#'  
#'  # compare counties within a state:
#'  fipsRI = fips_counties_from_state_abbrev("RI")
#'  x = counties_as_sites(fipsRI)
#'  out = doaggregate(x) # similar to ejamit()
#'  ejam2barplot_sites(out, "pop", names.arg = fipsRI)
#'  
#'  # compare two specific counties:
#'  counties_as_sites(c('01001','72153'))
#'  
#'  # Largest US Counties by ACS Population Totals:
#'  topcounties = blockgroupstats[ , .(ST = ST[1], countypop = sum(pop)),
#'   by = .(FIPS = substr(bgfips,1,5))][order(-countypop),][1:20, .(
#'     CountyPopulation = prettyNum(countypop, big.mark = ","), FIPS, ST)]
#'  
#'  myfips = topcounties$FIPS
#'  
#'  # simplest map of top counties
#'  map_shapes_leaflet(shapes = shapes_counties_from_countyfips(myfips))
#'  
#'  # simplest way to get and map EJ stats on counties
#'  out_c1 = ejamit(fips = myfips)
#'  mapfastej_counties(out_c1$results_bysite)
#'    
#'  # another way to get and map EJ stats on counties
#'  s2b = counties_as_sites(myfips)
#'  out_c2 = doaggregate(s2b) 
#'  # but without URLs/links to reports
#'  bysite = out_c2$results_bysite
#'  bysite$ejam_uniq_id <- myfips
#'  mapfastej_counties(bysite)
#'   
#' @export
#'
counties_as_sites <- function(fips) {
  
  if (any(is.numeric(fips))) {
    message("leading zeroes being inferred since FIPS was provided as numbers not character class")
    fips <- fips_lead_zero(fips)
  }
  
  county2bg <- bgpts[substr(bgfips,1,5) %in% fips, .(countyfips = substr(bgfips,1,5), bgid) ]
  if (NROW(county2bg) == 0) {warning("no valid fips, so returning empty data.table of 0 rows")}
  county2bg[, ejam_uniq_id := .GRP , by = "countyfips"]
  
  county2bg[ , blockid := bgid] # fix this to use the 1st block of each bgid
  county2bg[ , blockwt := 1]
  county2bg[ , distance := 0]
  county2bg[ , distance_unadjusted := 0]
  county2bg$blockid = blockwts[county2bg, .(blockid = blockid[1]), on = "bgid", by = "bgid"]$blockid
  
  county2bg[, .(ejam_uniq_id, countyfips, bgid, blockid, blockwt, distance, distance_unadjusted)]
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
  is_ok_stfips <- (fipstype(fips) == "state" & fips_valid(fips))
  # valids <- stateinfo2$FIPS.ST[!is.na(stateinfo2$FIPS.ST)]
  # if (!all(fips %in% valids)) {
  howmanyna <- sum(!is_ok_stfips) 
  if (howmanyna > 0) { 
    warning(howmanyna, ' fips provided are not valid state fips')
  }
  fips <- fips[is_ok_stfips]
  
  # accept state fips vector
  # return counties2bgs table of pairs so doaggregate_blockgroups() or whatever can take that and do full EJ stats.
  
  state2bg <- bgpts[substr(bgfips,1,2) %in% fips, .(statefips = substr(bgfips,1,2), bgid) ]
  state2bg[, ejam_uniq_id := .GRP , by = "statefips"]
  state2bg[, .(ejam_uniq_id, statefips, bgid)]
}
############################################### #


#' which fips, state names, or state abbreviations are island areas
#'
#' @param ST optional vector of 2 letter state abbreviations
#' @param statename optional vector of statenames like "texas" or "Delaware"
#' @param fips optional vector of FIPS codes (first 2 characters get used)
#' @seealso [latlon_is.islandareas()]
#' @examples 
#'   is.island(c("PR", "DE", "AS", NA))
#'   is.island(statename = c("Guam", "New York", "american samoa", NA))
#'   is.island(fips = c(21001, 60, "60", "600010000000"))
#'   tail(cbind(stateinfo2[ , c("statename", "is.island.areas")], is.island(stateinfo2$ST)),10)
#' @return logical vector of same length as the input
#' 
#' @export
#'
is.island <- function(ST=NULL, statename=NULL, fips=NULL) {
  
  if (sum(!is.null(ST), !is.null(statename), !is.null(fips)) != 1) {
    stop("one and only one of ST, statename, fips parameters must be provided, not NULL")
  }
  if (!is.null(ST)) {
    fips <- fips_state_from_state_abbrev(ST)
    return(
      stateinfo2$is.island.areas[match(fips, stateinfo2$FIPS.ST)]
      # stateinfo2$is.island.areas[match(toupper(ST), toupper(stateinfo2$ST))]
    )
  } 
  if (!is.null(statename)) {
    fips <- fips_state_from_statename(statename)
    return(
      stateinfo2$is.island.areas[match(fips, stateinfo2$FIPS.ST)]
      # stateinfo2$is.island.areas[match(toupper(statename), toupper(stateinfo2$statename))]
    )
  }
  if (!is.null(fips)) {
    fips <- fips2state_fips(fips)
    return(
      stateinfo2$is.island.areas[match(fips, stateinfo2$FIPS.ST)]
    )
  }
}
############################################################################# #


############################################################################# #
#  fips_ from_  ####
############################################################################# #


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
#' @examples
#'  fips_from_table( data.frame(countyfips=0, FIPS=1, bgfips=2, other=3, fips=4))
#' 
#' @export
#'
fips_from_table <- function(fips_table, addleadzeroes=TRUE, inshiny=FALSE) {
  
  # fips_table can be data.frame or data.table, as long as colnames has one valid fips alias
  ## create named vector of FIPS codes (names used as location id)
  
  fips_alias <- c('fips', 'FIPS', 'Fips', 'fips_code', 'fipscode',
                  'blockfips', 
                  'bgfips', 'blockgroupfips', 'blockgroup_fips', 'blockgroup_fips_code',
                  'FIPS.TRACT', 'tractfips', 'tract_fips',
                  'countyfips', 'FIPS.COUNTY',
                  'statefips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST'
  )
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
      warning(    paste0('No FIPS column found. Please use one of the following names: ', paste0(fips_alias, collapse = ', ')))
      fips_vec <- NULL
    }
  }
  return(fips_vec)
}
####################################################### #


#' Get FIPS codes from names of states or counties
#' inverse of fips2name(), 1-to-1 map statename, ST, countyname to FIPS of each
#' @aliases fips_from_name names2fips
#' @param x vector of 1 or more exact names of states or ST abbreviations or
#'   countynames that include the comma and state abbrev., like
#'   "Harris County, TX"
#'   (not the same as where ST is separate in [fips_counties_from_countyname()])
#'   Ignores case.
#' @return vector of character fips codes
#' @examples 
#' name2fips(c("de", "NY"))
#' name2fips("rhode island")
#' name2fips(c("delaware", "NY"))
#' 
#' @export
#'
name2fips = function(x) {
  
  # figure out if x is ST, statename, countyname.
  fips = fips_state_from_state_abbrev(x) # NA if not a state abbrev. ignores case.
  fips[is.na(fips)] <- fips_state_from_statename(x[is.na(fips)]) # only tries for those that were not a ST abbrev
  
  fips[is.na(fips)] <- fips_counties_from_countynamefull(x[is.na(fips)])
  # fips[is.na(fips)] = substr(blockgroupstats$bgfips,1,5)[match(x[is.na(fips)]), blockgroupstats$countyname]
  # only tries for those that were neither ST nor statename
  
  # if (any(toupper(ST) %in% c("AS", "GU","MP", "UM", "VI"))) {
  #   message("note some of ST are among AS, GU, MP, UM, VI")
  # }
  # if (any(substr(fips,1,2) %in% c("60" "66" "69" "74" "78"))) {
  #   
  # }
  return(fips)
}
############################################################################# #

#' @export
names2fips <- function(x) {
  # this is just an alias where "names" is plural instead of singular "name"
  name2fips(x = x)}
############################################################################# #

#' @export
fips_from_name = function(x) {
  # name2fips is a useful alias, though not consistent, so keep fips_from_name() also just in case
  name2fips(x = x)    
}
############################################################################# #


#' FIPS - Get state fips for each state abbrev
#'
#' @param ST vector of state abbreviations like c("NY","GA"), ignores case
#'
#' @return vector of 2-digit state FIPS codes like c("10", "44", "44"),
#'   same length as input, so including any duplicates
#'
#' @examples fips_state_from_state_abbrev("DE", "DE", "RI")
#'
#' @export
#'
fips_state_from_state_abbrev <- function(ST) {
  
  if (any(toupper(ST) %in% c("AS", "GU","MP", "UM", "VI"))) {
    message("note some of ST are among AS, GU, MP, UM, VI")
  }
  x <- stateinfo2$FIPS.ST[match(toupper(ST), toupper(stateinfo2$ST))] # using match is ok since only 1st match returned per element of ST but stateinfo has only 1 match per value of ST
  if (anyNA(x)) {
    howmanyna = sum(is.na(x))
    warning("NA returned for ", howmanyna," ST that failed to match")
  }
  return(x)
  # returns one per input, including repeats etc
  # retuns NA if no matching state abbrev found
  
  # not
  # stateinfo2$FIPS.ST[stateinfo2$ST %in% ST]
  # note state_from_fips() is not really the inverse, though - see help on that function
}
############################################################################# #


#' FIPS - Get state fips for each state name
#'
#' @param statename vector of state names like c("New York","Georgia"),
#'   ignoring case
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
  
  x <- stateinfo2$FIPS.ST[match(tolower(statename), tolower(stateinfo2$statename))] # using match is ok since only 1st match returned per element of statename but stateinfo has only 1 match per value of statename
  if (anyNA(x)) {
    howmanyna = sum(is.na(x))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  return(x)
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
  
  ftype = fipstype(statefips)
  if ( any(ftype[!is.na(ftype)] != "state")) {
    # fipstype() already provides warning about NA
     warning("Some of the supplied statefips values were NA or otherwise not recognized as State FIPS codes")
    
  }
  statefips <- statefips[!is.na(ftype) & ftype == "state"]
  if (length(statefips) == 0) {return(NA)}
  
  # EJAM :: blockgroupstats  has all the usable FIPS codes in bgfips
  countyfips <- unique(substr(blockgroupstats$bgfips,1,5))
  countyfips <- countyfips[!is.na(countyfips)]
  countyfips_in_state <- unique(countyfips[substr(countyfips,1,2) %in% statefips])
  if (length(countyfips_in_state) == 0) {return(NA)}
  return(countyfips_in_state)
}
############################################################################# #


#' FIPS - Get ALL county fips in specified states
#'
#' @param ST vector of state abbreviations like c("NY","GA"), ignoring case
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
  statefips <- fips_state_from_state_abbrev(toupper(ST))
  fips_counties_from_statefips(statefips = statefips)
  # countyfips_in_state <- unique(countyfips[substr(countyfips,1,2) %in% statefips])
  # return(countyfips_in_state)
}
############################################################################# #


#' FIPS - Get ALL county fips in specified states
#'
#' @param statename vector of state names like c("New York","Georgia"),
#'   ignoring case
#'
#' @return vector of 5-digit character string county FIPS of all unique counties in those states
#'
#' @examples fips_counties_from_statename("Delaware")
#'
#' @export
#'
fips_counties_from_statename <- function(statename) {
  
  # statename = snakecase::to_title_case(statename)
  
  statefips <- fips_state_from_statename(statename) # now ignores case
  fips_counties_from_statefips(statefips = statefips)
}
############################################################################# #


#' get FIPS for a county based on part of the countyname and state abbrev
#'
#' @param countyname_start first few letters of countyname to look for via grep("^x", )
#'   like "Johnson" or "Johnson County". Ignores case.
#' @param ST two letter abbreviation of State, such as "TX" -- Can only be
#'   omitted if the 1st parameter has the full name and ST like
#'   "Harris County, TX". Ignores case.
#'
#' @return the county FIPS (5 digits long with leading zero if needed, as character)
#'   but can return more than one guess per input name
#' @examples 
#'  fips2name(fips_counties_from_countyname("Har", "TX")) # finds 4 matches
#'  fips_counties_from_countyname("Har",               "TX")    # finds 4 matches
#'  fips_counties_from_countyname("Harris",            "TX")    # finds 2 matches
#'  fips_counties_from_countyname("Harris ",           "TX")    # finds 1 match
#'  fips_counties_from_countyname("Harris County",     "TX")    # same
#'  fips_counties_from_countyname("harris county, tx", "TX")    # same
#'  fips_counties_from_countyname("Harris County, Texas", "TX") # finds 0 if state spelled out
#'  fips_counties_from_countyname("harris county, tx") # can omit ST param like this
#'  fips_counties_from_countyname("Harris County TX")  # needs comma
#'  
#' @export
#'
fips_counties_from_countyname <- function(countyname_start, ST=NULL) {
  
  if (missing(countyname_start)) {stop("countyname_start parameter is required but missing")}
  
  if (all(is.null(ST))) {
    out <-     fips_counties_from_countynamefull(countyname_start)
    message("ST not specified, so tried to find exact matches to Countyname, ST")
    return(out)
  } else {
    if (length(countyname_start) != length(ST)) {stop("the two parameters must be equal in length")}
  }
  if (any(is.na(ST))) {
    if (!all(is.na(ST))) { 
      # make sure if ST is na then so is countyname_start, since confusing results if valid names sometimes have ST and sometimes do not
      if (any(is.na(ST) & !is.na(countyname_start))) {stop("Some but not all ST values are NA where countyname_start was provided")}
    }
    out <- fips_counties_from_countynamefull(countyname_start[is.na(ST)])
    message("When ST not specified, tries to find exact matches of countyname_state to Countyname, ST")
  } else {
    out = NULL
  }
  
  # stopifnot(length(ST) == 1, is.atomic(ST), length(countyname_start) == 1, is.atomic(countyname_start))
  
  allcfips <- fips_counties_from_state_abbrev(ST[!is.na(ST)]) # ignores case # would fail if any ST is NA.  works for vector
  allcnames <- fips2countyname(allcfips)
  
  matching_countyfips <- allcfips[grep(paste0("^", countyname_start[!is.na(ST)]), allcnames, ignore.case = TRUE)]
  
  out <- c(out, matching_countyfips)
  # out <- out[!is.na(out)]  # returning some NA values sort of implies a 1 to 1 input-output but that is not how this func works... ***
  return(out)
}
############################################################################# #


#' helper function - get county FIPS from exact countyname including, ST abbrev
#' used by fips_counties_from_countyname()
#' @param countyname_start exact (case-insensitive) name of 
#'   county comma state abbreviation, 
#'   like "Johnson County, TX". Ignores case.
#' @seealso [fips_counties_from_countyname()]
#' @return the county FIPS (5 digits long with leading zero if needed, as character)
#'   of each, or NA for non matches
#'  
#' @keywords internal
#'
fips_counties_from_countynamefull <- function(fullname) {
  
  # this internal function just supports fips_counties_from_countyname()
  # This requires exact match to "county name, ST" but case-insensitive
  
  #    examples 
  # fips2name(fips_counties_from_countynamefull("Harris County, TX"))
  # y <- fips_counties_from_countynamefull(c("Harris County, TX", "Harrison County, TX"))
  # y
  # fips2name(y)
  # fips_counties_from_countynamefull("Harris County, tx")
  
  x <- substr(
    blockgroupstats$bgfips,1,5
  )[match(
    tolower(fullname), 
    tolower(paste0(blockgroupstats$countyname, ", ", blockgroupstats$ST))
  )]
  if (anyNA(x)) {
    howmanyna = sum(is.na(x))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  return(x)
}
############################################################################# #


#' FIPS - Get unique blockgroup fips in or containing specified fips of any type
#'
#' Convert any FIPS codes to the FIPS of all the blockgroups that are
#'   among or within or containing those FIPS
#'
#' @details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#' 
#'  This function is not optimized for speed -- it is slow for large queries.
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
  
  x <- fips_lead_zero(fips)
  
  if (anyNA(x)) {
    howmanyna = sum(is.na(x))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  fips <- x[!is.na(x)]
  
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


############################################################################# #
# fips2...   ####
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
  
  stfips <- fips_lead_zero(stfips)
  
  x <- EJAM::stateinfo2$REGION[match(stfips, EJAM::stateinfo2$FIPS.ST)] # using match is ok since only 1st match returned per element of query but there is only 1 match possible
  if (anyNA(x)) {
    howmanyna = sum(is.na(x))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  return(x)
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
  
  abb <- stateinfo2$ST[match(substr(fips_lead_zero(fips), 1, 2), stateinfo2$FIPS.ST)] # using match is ok
  
  # confirm returns same length as input, and check how it handles nonmatches
  x = abb
  if (anyNA(x)) {
    howmanyna = sum(is.na(x))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  return(x)
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
  
  stfips <- substr(fips_lead_zero(fips), 1, 2)
  if (anyNA(stfips)) { 
    howmanyna = sum(is.na(stfips))
    warning(howmanyna, " fips could not be converted to state fips - returning NA for those")
  }
  return(stfips)
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
  
  stfips = substr(fips_lead_zero(fips), 1, 2)
  stfips[is.na(stfips)] <- "" # because NA would match the code in this table for United States, which has NA listed there as its fips
  stname <- stateinfo2$statename[match(stfips, stateinfo2$FIPS.ST)] # using match is ok
  
  if (anyNA(stname)) {
    howmanyna = sum(is.na(stname))
    warning(howmanyna, " fips could not be converted to state name - returning NA for those")
  }
  return(stname)
}
############################################################################# #


#' FIPS - Get county names from county FIPS codes
#'
#' @param fips vector of US Census FIPS codes for Counties (5 digits each). can be string or numeric, with or without leading zeroes.
#' @param includestate can be ST, Statename, "", or TRUE to specify what if anything comes after county name and comma
#' @return vector of county names, optionally with comma and 2-character abbreviation or full state name.
#' @details NOTE THAT ISLAND AREAS WORK DIFFERENTLY SINCE THEIR FIPS ARE NOT QUITE LIKE COUNTY FIPS
#'   - FIRST 5 LETTERS OF FIPS ARE NOT THE UNIQUE "COUNTY" CODE IN Northern Mariana Islands
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
  
  ## *** need to handle NA values here since out[NA] <-  fails as cannot have NA in subset assignment
  out[!is.na(ftype) & ftype == "county"] <- blockgroupstats$countyname[match(
    fips[!is.na(ftype) & ftype == "county"],
    substr(blockgroupstats$bgfips,1,5))]  #
  # using match is OK since 
  # you want 1 countyname returned per countyfips in query, so the fact that only 1st match gets returned is actually good.
 
  if (all(is.na(ftype)) || any(ftype != "county")) {
    warning("this function should only be used to convert county fips to county name, 1 to 1 - returning NA for fips that are not countyfips")
  }
  if (includestate == TRUE) {includestate <- "Statename"}
  if (includestate == "Statename") {addon <- fips2statename(fips)} else {
    if (includestate == "ST") {addon <- fips2state_abbrev(fips)} else {
      addon <- ""
    }
  }
  if (all(addon == "") | all(is.na(out))) {
    return(out)
  } else {
    out[!is.na(out)] <- paste(out[!is.na(out)], addon[!is.na(out)], sep = ", ")
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
#' @seealso [countyname2fips()]
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
  
  ## *** need to handle NA values here since out[NA] <-  fails as cannot have NA in subset assignment
  out[!is.na(fips) & fipstype(fips) == "state"]  <- fips2statename(fips = fips[!is.na(fips) & fipstype(fips) == "state"])
  out[!is.na(fips) & fipstype(fips) == "county"] <- fips2countyname(fips = fips[!is.na(fips) & fipstype(fips) == "county"], ...)
  if (anyNA(out)) {
    howmanyna = sum(is.na(out))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  return(out)
}
############################################################################# #
