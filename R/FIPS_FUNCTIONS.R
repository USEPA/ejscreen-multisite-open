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
#       fips_bgs_in_fips()
### and
###   see   getblocksnearby_from_fips() which uses  fips_bgs_in_fips()

##NOT  cities_as_sites()  would be a name that makes sense but not used.
##NOT regions_as_sites()  would be a name that makes sense but not used.
##NOT   tracts_as_ and blockgroups_as_  maybe useful?

################################## #    fips2...

####  if fips is the INPUT, names are like   x <- fips2x(fips) 
#
# fips_st2eparegion()  # and see fips_states_in_eparegion() 
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
#'     "01001020100", # substr(blockgroupstats$bgfips[1],1,11),
#'     blockgroupstats$bgfips[1],
#'     "010010201001000" # blockid2fips$blockfips[1]
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
  ftype[nchar(fips, keepNA = FALSE) ==  7] <- "city" # ACTUALLY IT IS "place" as in censusplaces$placename or $fips  # e.g, 5560500 is Oshkosh, WI
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
#' @seealso [fips_bgs_in_fips()] [fips_lead_zero()] [getblocksnearby_from_fips()] [fips_from_table()]
#' @examples
#'  fips_from_table( data.frame(countyfips=0, FIPS=1, bgfips=2, other=3, fips=4))
#' 
#' @export
#'
fips_from_table <- function(fips_table, addleadzeroes=TRUE, inshiny=FALSE) {
  
  # fips_table can be data.frame or data.table, as long as colnames has one valid fips alias
  ## create named vector of FIPS codes (names used as location id)
  # *** see also fixnames_aliases() and fixcolnames_infer()
  fips_alias <- c('fips', 'FIPS', 'Fips', 'fips_code', 'fipscode',
                  'statefips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST',
                  'countyfips', 'FIPS.COUNTY',
                  'FIPS.TRACT', 'tractfips', 'tract_fips',
                  'bgfips', 'blockgroupfips', 'blockgroup_fips', 'blockgroup_fips_code',
                  'blockfips'
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
####################################################### #

shapes_places_from_placenames <- function(place_st) {
  
  getst = function(x) {gsub(".*(..)", "\\1", x)}  # only works if exact format right like x = c("Port Chester, NY", "White Plains, NY", "New Rochelle, NY")
  st = unique(getst(place_st))
  place_nost = gsub("(.*),.*", "\\1", place_st) # keep non-state parts, only works if exact format right
  
  shp <- tigris::places(st) %>% tigris::filter_place(place_nost) # gets shapefile of those places boundaries from census via download
  return(shp)
}
####################################################### #

## examples
#
# place_st = c("Port Chester, NY", "White Plains, NY", "New Rochelle, NY")
# shp = shapes_places_from_placenames(place_st)
# 
# out <- ejamit(shapefile = shp)
# map_shapes_leaflet(shapes = shp, 
#                    popup = popup_from_ejscreen(out$results_bysite))
# ejam2excel(out, save_now = F, launchexcel = T)

#   fips = fips_place_from_placename("Port Chester, NY")
# seealso [shapes_places_from_placefips()] [shapes_places_from_placenames()]
#   [fips_place2placename()] [fips_place_from_placename()] [censusplaces]


# also see 
#  https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf
#  https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2023/TGRSHP2023_TechDoc_Ch3.pdf 
#  https://github.com/walkerke/tigris?tab=readme-ov-file#readme
#  https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#tigris-workflows
#
# For demographic data (optionally pre-joined to tigris geometries), see the tidycensus package.
# NAD 1983 is what the tigris pkg uses -- it only returns feature geometries for US Census data that default to NAD 1983 (EPSG: 4269) coordinate reference system (CRS).
#   For help deciding on appropriate CRS, see the crsuggest package.


## used by name2fips or fips_from_name 
# see https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf


shapes_places_from_placefips <- function(fips) {
  
  if (!all(fips %in% censusplaces$fips)) {stop("check fips - some are not found in censusplaces$fips")}
  
  st <- fips2state_abbrev(fips)
  place_nost <- fips_place2placename(fips, append_st = FALSE)
  
  shp <- tigris::places(st) %>% tigris::filter_place(place_nost) # gets shapefile of those places boundaries from census via download
  return(shp)
}
####################################################### #

fips_place2placename = function(fips, append_st = TRUE) {
  
  if (!all(fips %in% censusplaces$fips)) {stop("check fips - some are not found in censusplaces$fips")}
  
  place_nost <- censusplaces$placename[match(fips, censusplaces$fips)]
  
  if (append_st) {
    suppressWarnings({
    st <- fips2state_abbrev(fips)
    })
    place_st =  paste0(place_nost, ", ", st)
    place_st[is.na(place_nost) | is.na(st)] <- NA
    return(place_st)
  } else {
    return(place_nost)
  }
}
####################################################### #


#' search using names of cities, towns, etc. to try to find matches and get FIPS
#' helper used by name2fips()
#' @details
#' 
#' helper used by [name2fips()]
#' 
#' Finding places by name is tricky because the master list [censusplaces] names places
#' using the words city, town, township, village, borrough, and CDP
#' while most people will not think to include that qualifier as part of a query.
#' 
#' Also, about 300 places like "Salt Lake City" have the word "City" as an essential part
#' of their actual name, so those are listed in that table in the format, "Salt Lake City city"
#' 
#' Also, in some cases the exact same town or township name occurs more than once in a State so
#'  a query by name and state is not always naming a unique place. This function does not 
#' currently distinguish between those. This is relatively rare - out of 38,000 place names,
#' fewer than 600 unique place-state pairs appear more than once, and fewer than 150 of those appear
#' more than twice in the same state.
#' Cases with 4+ duplicates in a state arise only for towns and townships.
#' Chula Vista CDP, TX and San Antonio comunidad, PR each occur three times.
#' All other duplicates are where a CDP, borough, etc. occurs twice in a state.
#' Almost all duplicates are in PA, WI, MI or MN.
#' Pennsylvania in particular has many frequently reused township names:
#' In that state, these place names occur more than 15 times each:
#' Franklin township, Union township, Washington township, Jackson township.
#' There are more than 500 unique name-state pairs that are reused within a state.
#' 
#' @param place_st vector of place names in format like "yonkers, ny" or "Chelsea city, MA"
#' @param geocoding set to TRUE to use a geocoding service to try to find hits
#' @param exact  FALSE is to allow partial matching 
#' @param usegrep if exact=T, usegrep if TRUE will use the helper function fips_place_from_placename_grep()
#' @param verbose prints more to console about possible hits for each queried place name
#' @return prints a table of possible hits but returns just the vector of fips
#' 
#' @keywords internal
#'
fips_place_from_placename = function(place_st, geocoding = FALSE, exact = FALSE, usegrep = FALSE, verbose = TRUE) {
  
  # CAUTION - currently assumes a placename,ST occurs only once per state,
  # but there are exceptions like Denver township, MI occurs twice for example, in 2 different counties of same state.
  
  #   exact = FALSE  option will use grep() instead of match()
  
  ## examples 
  ## Search for place fips based on partial name of place
  # fips_place_from_placename(c('denver',  "new york" ), exact = F)
  # fips_place_from_placename('chelsea,ma', exact = F)
  # fips = fips_place_from_placename('chelsea city, MA', exact = T)
  #  # 2513205
  # mapview(  shapes_places_from_placefips(fips_place_from_placename('chelsea city, MA', exact = T) ))

  ## examples
  # place_st = 
  ## used by name2fips or fips_from_name 
  
  # seealso [shapes_places_from_placefips()] [fips_place2placename()] [fips_place_from_placename()] [censusplaces]
  
  # see https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf
  if (length(place_st) == 0) {return(NULL)}
  if (any(!grepl(",", place_st))) {warning("place_st should be in form of placename, ST like Port Chester, NY")}
  
  ### for exact=F, could recode to query name using grep within given ST, separately?
  # pname = pre_comma(place_st)
  # st = EJAM:::fips2state_abbrev(fips_state_from_statename( post_comma(place_st)))
  
  ######################################################################################## #
  # words to ignore like "city" ####
  
  # placetypes = unique(gsub(".* ", "",         substr(censusplaces$placename,nchar(censusplaces$placename) - 15,99)))
  # placetypes = unique(gsub(".* (.*$)", "\\1", substr(censusplaces$placename,nchar(censusplaces$placename) - 15,99))) 
  # grep('UT$', censusplaces$placename, value = T)
  # grep('urbana$', censusplaces$placename, value = T)
  
  placetypes <- 
    c("city", "CDP", "municipality", "borough", "town", "defined", 
      "(balance)", "village", "government", "county", "plantation", 
      "UT", "Reservation", "gore", "township", "157-30", "158-30", 
      "County", "location", "grant", "purchase", "City", "urbana", 
      "comunidad", "corporation")
  
  kept_terms <- c('County',  # but not the lower case version?
                  'City', 'city',  # HANDLED SEPARATELY BELOW AS A SPECIAL CASE
                  'defined', '(balance)', 'gore', 
                  'urbana', "comunidad",
                  "157-30", "158-30",
                  "municipality", "borough", "location", "grant", "purchase")
  
  ignored_terms <- placetypes
  ignored_terms <- ignored_terms[!(ignored_terms %in% kept_terms)]
  
  # ignored_terms <-  c(
  #    "county", "CDP", "town", "township", "village",
  #   "plantation", "Reservation", "UT", "government", "corporation")
  
  all_place_st <- paste(censusplaces$placename, censusplaces$ST, sep = ", ")
  
  rgx <- paste0(paste0(" ", ignored_terms, ","), collapse = "|")
  all_place_st_dont_say_cdp <- gsub(rgx, ",", all_place_st)
  place_st_dont_say_cdp     <- gsub(rgx, ",", place_st)
  
  ##### special cases like "Salt Lake City city" 
  # Normally we want to remove/ignore the word "city" because the master list uses it for every city even though we almost always omit the word "city" in a query, 
  # such as where "Chelsea city, MA" is in master list but "Chelsea, MA" would be a typical query.
  # However, about 302 cities must retain the word "city" and are seen as "...City city" in censusplaces$placename. 
  #   sum(grepl("city city", censusplaces$placename, ignore.case = T))
  # If we removed "city," in those special cases,
  # the original "salt lake city city" "UT" in master list  becomes   "salt lake city, UT"  which is ok but 
  # then a query that said "salt lake city, ut" would lose the " city" and be just "salt lake, UT" and fail to match "salt lake city, UT" !
  # If you instead remove "city" from the 10,164 census places with that word, 
  #   sum(grepl("city", censusplaces$placename))
  # and not from any query terms, that means you
  # would not find "Chelsea, MA" without asking for "Chelsea city, MA" which is not intuitive.
  ## So we will convert "x city city" to just "x city" in censusplaces$placename, but
  ## in any query terms, we will convert "x city" to "x" UNLESS "x city" is among about 300 specialcase places with "city" as an essential part of their name:
  
  specialcase = grep("city city", all_place_st_dont_say_cdp, ignore.case = T)
  # 
  # drop 1 word "city" from master list even for places like "salt lake city city"
  all_place_st_dont_say_cdp_or_city <- gsub(" city,", ",", all_place_st_dont_say_cdp, ignore.case = T)
  # but drop 1 word "city" from query in all but the special cases where query is a place that has to say "city" once, as part of the name, and matches that way:
  specialquery <- tolower(place_st_dont_say_cdp) %in% tolower(all_place_st_dont_say_cdp_or_city[specialcase]) # looks for exact matches assuming query may say "salt lake city, UT" and special list now says the same.
  place_st_dont_say_cdp_or_city <- place_st_dont_say_cdp
  place_st_dont_say_cdp_or_city[!specialquery]     <- gsub(" city,", ",", place_st_dont_say_cdp_or_city[!specialquery],     ignore.case = T) # in case not geocoding
  
  ######################################################################################## #

  if (geocoding) {
    if (!exists("geocoding")) {
      warning("Need to load the AOI package for geocoding to work. Using geocoding=FALSE instead, here.")
    } else {
      # geocoding fails sometimes when CDP is part of the name
      place_st_dont_say_cdp <- gsub(" CDP,", ",", place_st)
      
      arcgis_address_xy <- geocode(place_st_dont_say_cdp)  # or _or_city ?
      setDT(arcgis_address_xy)
      place_st <- arcgis_address_xy[ , .(best = arcgis_address[1]), by = "request"]$best
      # now place_st are the best guesses via geocoding, ready to look for matches in table of fips and place names
      cat("Names based on geocoding:\n", paste0(head(place_st, 30), collapse = ", "), ifelse(length(place_st) > 30, " ...etc. ", ""), "\n")
    }
  }


  ## would output of geocoding require same handling of ignored terms??
  ######################################################################################## #
  
  # remove/ignore a space after comma
  
  # all_place_st_dont_say_cdp = gsub(", ", ",", all_place_st_dont_say_cdp) # why not _or_city ?
  all_place_st_dont_say_cdp_or_city = gsub(", ", ",", all_place_st_dont_say_cdp_or_city)
  
  # place_st_dont_say_cdp     = gsub(", ", ",", place_st_dont_say_cdp)     # why not _or_city ?
  place_st_dont_say_cdp_or_city = gsub(", ", ",", place_st_dont_say_cdp_or_city)
  
  if (!exact) {
    
    ### for exact=F, could recode to query name using grep within given ST, separately?
    
    ########################### # ########################### # ########################### # ########################### # 
    ########################### # ########################### # ########################### # ########################### # 
    
    # utility to query city/CDP, ST via grep to get FIPS
    # search each of those parts in censusplaces$placename and $ST
    # so this searches for and finds only places not counties or states
    
    fips_place_from_placename_grep <- function(tx, all_placename = censusplaces$placename, all_ST = censusplaces$ST) {
      
      # examples
      #   fips_place_from_placename_grep(c('white plains, ny', 'queens,new york'))
      #   fips_place_from_placename_grep('white plains')
      
      hits = list()
      ###################### #  ###################### #  
      for (i in seq_along(tx)) {
        
        queryfull <- tx[i]
        query_city   <- pre_comma(queryfull, trim = T)
        if (grepl(',', queryfull) ) {
          query_state <- post_comma(queryfull, trim = T)
          query_state <- statename2st(query_state)
        } else {
          query_state <- "" # NA would never match on state since blank. "" matches any state.   # assume if no comma they meant city but not ST is specified
        }
        city_matched  = !is.na(query_city) & grepl( query_city, all_placename, ignore.case = T)
        state_matched = !is.na(query_state) & grepl(query_state, all_ST, ignore.case = T)
        found_careful = censusplaces[city_matched & state_matched, ]
        
        if (NROW(found_careful) == 0) {
          empty = censusplaces[0,]
          empty = empty[1,]
          hits[[i]] <- data.frame(query = tx[i], empty)
        } else {
          hits[[i]] <- data.frame(query = tx[i], found_careful)
          rownames(hits[[i]]) <- NULL # ?
        }
        
        hits[[i]] <- cbind(hits[[i]],
                           count_city_matched = sum(city_matched),
                           # count_state_matched = sum(state_matched),
                           count_city_state_matched = sum(city_matched & state_matched))
      }
      ###################### #  ###################### #  
      
      hits <- data.table::rbindlist(hits)
      cat("\nSummary of hits per query term\n\n")
      print(hits[ , .(count_city_matched = count_city_matched[1], 
                      # count_state_matched = count_state_matched[1], 
                      count_city_state_matched = count_city_state_matched[1]
      ), by = 'query'])
      cat("\n\n")
      hits[, `:=`(eparegion = NULL, stfips = NULL, countyfips = NULL) ]
      setcolorder(hits, c('query', 'placename', 'ST', 'countyname', 'fips', 'count_city_matched', 'count_city_state_matched'))
      print(hits)
      # invisible(hits$fips)
      return(hits[])
    }
    ########################### # ########################### # ########################### # ########################### # 
    ########################### # ########################### # ########################### # ########################### # 
    
    
    ### Should try better query than below, using newer fips_place_from_placename_grep() 
    ### that does split of city,ST and searching each part, after removing words like "city" etc.  :
    
    if (usegrep) { 
      
    results <- fips_place_from_placename_grep(place_st_dont_say_cdp_or_city,
                                              all_placename = pre_comma(all_place_st_dont_say_cdp_or_city, trim = TRUE),
                                              all_ST = post_comma(all_place_st_dont_say_cdp_or_city, trim = TRUE))
    results <- results[, .( query,placename,ST,countyname,fips, count_city_matched, count_city_state_matched)]

    ### Get back a table of candidates,
    ###   but where do we check for exact match ? and where to choose which of possible hits is best ?
    
  
    } else {
      
      ### the way it was done before  fips_place_from_placename_grep() was drafted:
      
      
      ## IF USING fips_place_from_placename_grep  below... then need this:
      # all_place_st_dont_say_cdp_or_city_PRECOMMA <- pre_comma(all_place_st_dont_say_cdp_or_city)
 
    
    
    results <- list()
    
    for (i in 1:length(place_st_dont_say_cdp_or_city)) {
      
      # query was NA so just return NA values as result for that input
      if (is.na(place_st_dont_say_cdp_or_city[i])) {
        results[[i]] <- censusplaces[1,] # to get the right colnames
        results[[i]][1,] <- rep(NA, NCOL(results[[i]]))
        results[[i]]$query <- place_st[i]
      } else {
        
        # first check if nearly exact match does work (ignoring cdp and city words) 
        ## but using match() returns only 1st hit and that misses dupes like in PA
        # exactresult <- censusplaces[match(tolower(place_st_dont_say_cdp_or_city[i]), tolower(all_place_st_dont_say_cdp_or_city), nomatch = NA, incomparables = NA), ]
        exactresult <- censusplaces[tolower(all_place_st_dont_say_cdp_or_city) %in% tolower(place_st_dont_say_cdp_or_city[i]), ]
        if (NROW(exactresult) == 1) {
          results[[i]] <-   exactresult
          results[[i]]$query <- place_st[i]
          next  # done with this query term 
        }
        if (NROW(exactresult) > 1) {
          # looks like there are duplicates, where same township name appears twice or more in a single state like in PA
          results[[i]] <- exactresult
          results[[i]]$query <- place_st[i]
          
        } else  {
          
          # no exact match, so use grepl()
          # This can return multiple rows for a single input queried place, 
          # so it will not be 1-to-1 in/output:
          
          results[[i]]  <- censusplaces[grepl(place_st_dont_say_cdp_or_city[i], all_place_st_dont_say_cdp_or_city, ignore.case = TRUE), ]
        
          ## or else maybe at least try now: (but better to do this whole thing at once outside this loop)
          # results[[i]]  <- fips_place_from_placename_grep(tx = place_st_dont_say_cdp_or_city[i],
          #                                                 all_placename = all_place_st_dont_say_cdp_or_city_PRECOMMA,
          #                                                 all_ST = censusplaces$ST)
          
          
          }
        if (NROW(results[[i]]) > 20) {
          # too many hits - ignore most, like if query was "California" ?
          # warning("large number of apparent matches?")
        }
        if (NROW(results[[i]]) == 0) {
          # zero results for this query term, by exact match and by grepl(), so return NA values for this input
          results[[i]] <- censusplaces[1,] # to get the right colnames
          results[[i]][1,] <- rep(NA, NCOL(results[[i]]))
        } 
        results[[i]]$query <- place_st[i]
      }
    }
    
    }
    #################################################### # 
    ## compile those findings and print to show possible hits, duplicates, county info, etc.
    
    if (is.data.frame(results[[1]])) {
      results <- data.frame(rbindlist(results))
      rownames(results) <- NULL
    } else {
      results = data.frame() # and results$fips will be NULL and NROW is 0
    }
    
    
    if (verbose) {
      if (NROW(results) == 0) {
        cat(paste0('\n\nyou can also try, for example:\n  censusplaces[grep("', place_st[1], '", censusplaces$placename, ignore.case = T), ]\n\n'))
      }
    }
    # but show this even if !verbose :
    if (NROW(results[!is.na(results$fips), ]) != 0) {
      cat("\nExact match, or Cities/CDPs showing any multiple possible matches, etc. (excluding if no match):\n\n")
      multihit = results$query %in% results$query[duplicated(results$query)]
      multihit = multihit[!is.na(results$fips)]
      print(data.frame(
        results[!is.na(results$fips), ], 
        multiple = ifelse(multihit, "yes", "")
      ))
      cat("\n\n")
    }
    #################################################### # 
    
    # TRY TO RETURN THE ONE BEST GUESS FOR EACH QUERIED TERM
    
    if (NROW(results) != 0) {
      place_st_notna = place_st[!is.na(place_st)]
      
      rownums2drop <- NULL
      for (i in 1:length(place_st_notna)) {
        theserows <- results$query == place_st_notna[i] & !is.na(results$fips)
        theserownumbers = which(theserows)
        these = results[theserows, ]
        if (NROW(these) > 1) {
          # TRY TO RETURN THE ONE BEST GUESS (already have returned near exact match if one was found)
          perfect = (
            tolower(paste0(gsub(" city| CDP", "", these$placename, ignore.case = T), ", ", these$ST)) ==
              tolower(gsub(" city| CDP", "", these$query[1], ignore.case = T))
          )
          
          # xyz city ?
          # perfect <- tolower(gsub(these$query[1] , '', these$placename , ignore.case = TRUE)) %in% c(" city")
          
          if (sum(perfect) > 1 ) {
            # just use the first of multiple perfect-ish hits
            perfect[perfect][2:length(perfect[perfect])] <- FALSE
          }
          
          if (sum(perfect) == 0) {
            # no ideal match so far, so try to match on first 2 letters:
            perfect <- tolower(substr(these$placename,1,2)) == tolower(substr(these$query[1],1,2))
            if (sum(perfect) > 1) {
              # just use the first of multiple perfect-ish hits 
              perfect[perfect][2:length(perfect[perfect])] <- FALSE
            }  
            if (sum(perfect) == 0) {
              # just use the first of multiple ok hits 
              perfect <- c(TRUE, rep(FALSE, length(perfect) - 1))
            } 
          }
          # drop all but one best guess or 1st guess
          rownums2drop = c(rownums2drop, theserownumbers[!perfect])
        }
      }
      if (!is.null(rownums2drop)) {
        results <- results[-rownums2drop, ]
      }
    }
  } else {
    # exact results are 1-to-1
    results <- censusplaces[match(tolower(place_st_dont_say_cdp), tolower(all_place_st_dont_say_cdp), nomatch = NA, incomparables = NA), ]
    results$query  <- place_st 
    
  }
  
  # DROPPING NA VALUES  WOULD MEAN IT IS NOT 1-TO-1 WHEN ANY NA VALUES 
  # results <- results[!is.na(results$fips), ]
  # cat("\nNon-NA results:\n\n")
  # print(results[!is.na(results$fips), ])
  
  rownames(results) <- NULL
  
  if (verbose & NROW(results) > 0) {
    cat("\nCities/CDPs including NA values but only 1 best guess per queried place:\n\n")
    print(results)
  }
  return(results$fips)
}
####################################################### ######################################################## #


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
  suppressWarnings({ # do not need to get warned that x is not a ST abbrev here
    # figure out if x is ST, statename, countyname.
    fips = fips_state_from_state_abbrev(x) # NA if not a state abbrev. ignores case.
    fips[is.na(fips)] <- fips_state_from_statename(x[is.na(fips)]) # only tries for those that were not a ST abbrev
    fips[is.na(fips)] <- fips_counties_from_countynamefull(x[is.na(fips)])
  })
  # fips[is.na(fips)] = substr(blockgroupstats$bgfips,1,5)[match(x[is.na(fips)]), blockgroupstats$countyname]
  # only tries for those that were neither ST nor statename
  if (any(is.na(fips))) {
  fips[is.na(fips)] <- fips_place_from_placename(x[is.na(fips)])
  }
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

# convert placename, ST to placename, statename if possible
# Harris County, TX becomes Harris County, Texas
# place_st2place_statename('Harris County, TX')
# place_st2place_statename(paste0(censusplaces$countyname, ", ", censusplaces$ST)[sample(1:3000,10)])

place_st2place_statename = function(fullname) {
  
  # split into place part and state part
  nonstatetext = gsub("(.*),(.*)", "\\1", fullname)
  statetext    = gsub("(.*),(.*)", "\\2", fullname)
  nonstatetext = trimws(nonstatetext)
  statetext = trimws(statetext)
  suppressWarnings({
    # convert ST to full statenames if possible
    statefips = fips_state_from_state_abbrev(statetext)
    statename = fips2statename(statefips)
  })
  statetext[!is.na(statename)] <- statename[!is.na(statename)]
  # reassemble
  fullname = paste0(nonstatetext, ", ", statetext)
  return(fullname)
}
############################################################################# #

# convert placename, statename to placename, ST if possible
# Harris County, Texas becomes Harris County, TX
# place_statename2place_st('Harris County, Texas')
# place_statename2place_st(
#   place_st2place_statename(
#     paste0(censusplaces$countyname, ", ", censusplaces$ST)[sample(1:3000,10)]))

place_statename2place_st = function(fullname) {  
  
  # split into place part and state part
  nonstatetext = gsub("(.*),(.*)", "\\1", fullname)
  statetext    = gsub("(.*),(.*)", "\\2", fullname)
  nonstatetext = trimws(nonstatetext)
  statetext = trimws(statetext)
  suppressWarnings({
    # convert full statenames to 2letter abbreviations if possible
    statefips = fips_state_from_statename(statetext)
    ST = fips2state_abbrev(statefips)
  })
  statetext[!is.na(ST)] <- ST[!is.na(ST)]
  # reassemble
  fullname = paste0(nonstatetext, ", ", statetext)
  return(fullname)
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
  # note state_from_fips_bybg() is not really the inverse, though - see help on that function
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


#' FIPS - Get state fips for all States in EPA Region(s)
#'
#' @param region vector of 1 or more EPA Regions (numbers 1 through 10)
#'
#' @return vector of 2-digit state FIPS codes like c("10", "44", "44"),
#'   same length as input, so including any duplicates
#' @examples
#'   fips_states_in_eparegion(2)
#'   fips_states_in_eparegion(6)
#'   fips2state_abbrev(fips_states_in_eparegion(6))
#' @export
#'
fips_states_in_eparegion <- function(region) {
  
  region = unique(as.numeric(region))
  if (anyNA(region) || !all(region %in% 1:10)) {stop("invalid region number(s)")}
  
  x <- stateinfo2$FIPS.ST[!is.na(stateinfo2$REGION) & (stateinfo2$REGION %in% region)] # 1 region to many states, so 
  x <- unique(x)
  if (anyNA(x))  {stop("invalid region number(s)")}
  
  return(x)
  # returns several state fips per input, no repeats
  # error if any or all are nonmatches
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
#' @param exact set TRUE to require exact matches, FALSE to allow partial matches
#'   in which case outputs might differ from inputs in length and not be 1-to-1
#' @return the county FIPS (5 digits long with leading zero if needed, as character)
#'   but can return more than one guess per input name!
#' @examples 
#'  fips2name(fips_counties_from_countyname("Har", "TX")) # finds 5 matches
#'  fips_counties_from_countyname("Har",               "TX")    # finds 5  matches
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
fips_counties_from_countyname <- function(countyname_start, ST = NULL, exact=FALSE) {
  
  # out = rep(NA, length(countyname_start)) 
  # do not try to keep 1-to-1 in to out ?? since a partial matching means we may return 2+ counties per input query term
  
  if (missing(countyname_start)) {stop("countyname_start parameter is required but missing")}
  
  if (is.null(ST)) {
    suppressWarnings({
      out <-     fips_counties_from_countynamefull(countyname_start)
    })
    # message("ST not specified, so tried to find exact matches to Countyname, ST")
    return(out)
  } else {
    if (length(countyname_start) != length(ST)) {
      stop("the two parameters must be equal in length")
    }
  }
  isnast <- is.na(ST)
  out <- rep(NA, length(ST))
  if (any(isnast) && !all(isnast)) {  #  some are NA  
    # make sure if ST is na then so is countyname_start, since confusing results if valid names sometimes have ST and sometimes do not
    if (any(isnast & !is.na(countyname_start))) {stop("Some but not all ST values are NA where countyname_start was provided")}
    suppressWarnings({
      out[isnast] <- fips_counties_from_countynamefull(countyname_start[isnast])
      # out <- out[!is.na(out)]
      if (all(is.na(out))) {out <- NULL}
    })
    message("When ST not specified, tries to find exact matches of countyname_state to Countyname, ST")
  } else {
    out <- NULL
  }  
  # stopifnot(length(ST) == 1, is.atomic(ST), length(countyname_start) == 1, is.atomic(countyname_start))
  
  countyname_start <- countyname_start[!isnast]
  stnow <- ST[!isnast]
  
  cfull = paste0(countyname_start, ", ", stnow)
  suppressWarnings({
    # first quickly get all the exact matches (ignoring case)
    exactmatches = fips_counties_from_countynamefull(cfull)
  })
  ### cfull = cfull[!is.na(exactmatches)]
  countyname_start_unmatched = countyname_start[is.na(exactmatches)]
  # stnow = stnow[is.na(exactmatches)]
  # 
  # exactmatches <- exactmatches[!is.na(exactmatches)]
  
  out[!isnast] <-  exactmatches 
  
  if (exact) {
    return(out)
    
  } else {
    suppressWarnings({
      # all names of counties in universe of possible hits (all in any of the queried states)
      allcfips <- fips_counties_from_state_abbrev(stnow) # ignores case # would fail if any ST is NA.  works for vector
      allcnames <- fips2countyname(allcfips)
    })
    compiledhits = NULL
    if (length(countyname_start_unmatched) > 0) {
      for (i in 1:length(countyname_start_unmatched)) {
        # inefficient but ok since query like universe would rarely have hundreds of counties and never >3500
        # also could consider    startsWith()  and   pmatch()
        newhits = grep(paste0("^", countyname_start_unmatched[i]), allcnames, ignore.case = TRUE, value = TRUE)
        compiledhits = c(compiledhits, newhits)
      }
      suppressWarnings({
        matching_countyfips <- fips_counties_from_countyname(compiledhits)
        matching_countyfips <- matching_countyfips[!is.na(matching_countyfips)]
      })
      out <- c(out, matching_countyfips)
    }
    out <- out[!is.na(out)]  # returning some NA values sort of implies a 1 to 1 input-output but that is not how this func works... ***
    out <- unique(out)
    return(out)
  }
}
############################################################################# #


#' helper function - get county FIPS from exact countyname including, ST abbrev
#' used by fips_counties_from_countyname()
#' @param fullname exact (case-insensitive) name of 
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
  # but now handles full statename too, not just ST abbrev
  #    examples 
  # fips2name(fips_counties_from_countynamefull("Harris County, TX"))
  # y <- fips_counties_from_countynamefull(c("Harris County, TX", "Harrison County, TX"))
  # y
  # fips2name(y)
  # fips_counties_from_countyname("Harris County, tx")
  # fips2countyname(fips_counties_from_countyname("harris county,texas"))
  
  ### see also  geocode()  and  censusplaces  and  EJAM/R/mod_fips_picker-DRAFT.R  
  
  fullname <- place_statename2place_st(fullname)
  
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
  abb[abb == "US"] <- NA
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
  if (any(!is.na(fips) & fipstype(fips) == "state")) {
    out[!is.na(fips) & fipstype(fips) == "state"]  <- fips2statename(fips = fips[!is.na(fips) & fipstype(fips) == "state"])
  }
  if (any(!is.na(fips) & fipstype(fips) == "county")) { # this prevents irrelevant warning "this function should only be used to convert county fips to county name..."
    out[!is.na(fips) & fipstype(fips) == "county"] <- fips2countyname(fips = fips[!is.na(fips) & fipstype(fips) == "county"], ...)
  }
  if (anyNA(out)) {
    howmanyna = sum(is.na(out))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  return(out)
}
############################################################################# #
