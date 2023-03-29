#' Random points on the map
#' Get data.table of Random Points (lat lon) for Testing/ Benchmarking/ Demos, weighted in various ways. 
#'   The weighting can be specified so that each point reflects the average EPA-regulated 
#'   facility, blockgroup, block, place on the map, or US resident.
#' @param n Number of points needed (sample size)
#' @param weighting word indicating how to weight the random points (some synonyms are allowed, in addition to those shown here): 
#' 
#'   - pop or people = Average Person: random person among all US residents (block point of residence per 2020 Census) 
#'   
#'   - frs or facility = Average Facility: random EPA-regulated facility from actives in Facility Registry Services (FRS)
#'   
#'   - bg = Average Blockgroup: random US Census block group (internal point like a centroid)
#'   
#'   - block = Average Block: random US Census block (internal point like a centroid)
#'   
#'   - area or place = Average Place: random point on a map (internal point of avg blockgroup weighted by its square meters size)
#'   
#' @param dt logical, whether to return a data.table (DEFAULT) instead of normal data.frame
#'
#' @return data.frame or data.table with columns lat, lon in decimal degrees, and 
#'   any other columns that are in the table used (based on weighting)
#' @param ST_of_blockgroup optional, can be a character vector of 2 letter State abbreviations to pick from only some States,
#'   but this only works if the weighting is "bg" or "area" (or synonyms) right now.
#'   
#' @import data.table
#' @export
#'
#' @examples \dontrun{
#' n=2
#' for (d in c(TRUE,FALSE)) {
#'   for (w in c('frs', 'pop', 'area', 'bg', 'block')) {
#'     cat("n=",n,"  weighting=",w, "  dt=",d,"\n\n")
#'     print(testpoints_n(n,w,d))
#'     cat('\n')
#'   }
#' }
#' }
#'  
testpoints_n <- function(n=10, weighting=c('frs', 'pop', 'area', 'bg', 'block'), dt=TRUE, ST_of_blockgroup=NULL) {
  
  if (NROW(n)  == 1) {
    if (n == 1e6) {warning('a million used to sound like a lot')}
    if (n > 1e6) {warning('more than a million, yikes')}
    if (n > 1e7) {warning('ridiculous n value OMG')}
    if (n == 1) {warning('just want one point? whatever.')}
    if (n == 0) {warning('zero points. srsly?')}
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if ( NROW(n) > 1  || !is.numeric(n) || is.na(n) || !is.wholenumber(n) || is.infinite(n) || n < 0 || n > 5e7) {
    stop('n must be a single whole number, where <0 is unfathomable and >50 million is too hard to keep track of')
  }
  
  # Handle weighting synonyms and default
  if (missing(weighting)) weighting <- 'bg'  # faster than other options
  if (any( length(weighting) != 1, class(weighting) != "character")) {stop("invalid weighting parameter for testpoints_n")}
  weighting <- tolower(weighting)
  if (weighting %in% c('frs', 'facility', 'facilities', 'facil', 'fac', 'frsid', 'regid')) weighting <- "frs"
  if (weighting %in% c('blockgroup', 'blockgroups', 'bg', 'bgs', 'block group', 'block groups', 'bgid', 'bgfips', "FIPS")) {weighting <- "bg"}
  if (weighting %in% c('block', 'blocks', 'blockpoints', 'blockid', 'blockfips')) {weighting <- 'block'}
  if (weighting %in% c('area', 'map', 'square meters', 'square meter', 'square mile', 'place')) {weighting <- "area"}
  if (weighting %in% c('person', 'pop', 'people', 'resident', 'population', 'residents'))  weighting <- 'pop'
  if (!(weighting %in% c('frs', 'pop', 'area', 'bg', 'block'))) {stop("invalid weighting parameter for testpoints_n")} 
  
  # RANDOM FACILITIES (EPA-regulated facilities in FRS)
  if (weighting == "frs") {
    if (!is.null(ST_of_blockgroup)) {warning("Ignoring ST_of_blockgroup ! ")}
    rownum <- sample.int(EJAMfrsdata::frs[,.N], size = n, replace = FALSE)
    if (!dt) {x=copy(frs); setDF(x); return(x[rownum, ])}
    return(frs[rownum, ] )
  }
  
  # RANDOM BLOCKGROUPS
  if (weighting == 'bg') {
    if (!is.null(ST_of_blockgroup)) {
      stfips <- EJAM::stateinfo$FIPS.ST[match(ST_of_blockgroup, EJAM::stateinfo$ST)]
      bg_filtered_by_state <- copy(EJAM::bgpts[substr(bgfips,1,2) %in% stfips, ])
      rownum <- sample.int(bg_filtered_by_state[,.N], size = n, replace = FALSE)
      if (!dt) {setDF(bg_filtered_by_state); return(bg_filtered_by_state[rownum, ])}
      return(bg_filtered_by_state[rownum, ] )
    } else {
      rownum <- sample.int(EJAM::bgpts[,.N], size = n, replace = FALSE)
      if (!dt) {x=copy(bgpts); setDF(x); return(x[rownum, ])}
      return(bgpts[rownum, ] )
    }
  }
  
  # RANDOM BLOCKS
  if (weighting == 'block') {
    if (!is.null(ST_of_blockgroup)) {warning("Ignoring ST_of_blockgroup ! ")}
    rownum <- sample.int(EJAMblockdata::blockpoints[,.N], size = n, replace = FALSE)
    if (!dt) {x=copy(blockpoints); setDF(x); return(x[rownum, ])}
    return(blockpoints[rownum, ] )
  }
  
  # RANDOM POINTS ON THE MAP
  if (weighting == 'area') {
    # stop('blockpoints$area needs to be added to blockpoints')
    if (!is.null(ST_of_blockgroup)) {
      # stfips <- EJAM::stateinfo$FIPS.ST[match(ST_of_blockgroup, EJAM::stateinfo$ST)]
      bg_filtered_by_state <- copy(EJAM::blockgroupstats[ST %in% ST_of_blockgroup, .(bgfips, bgid, ST, pop, arealand, areawater) ])
      rownum <- sample.int(bg_filtered_by_state[,.N], size = n, replace = FALSE)
      
      if (!dt) {
        bg_filtered_by_state <- bgpts[bg_filtered_by_state[rownum, ], .(lat, lon,  bgfips, bgid, ST, pop, arealand, areawater)]
        setDF(bg_filtered_by_state)
        return(bg_filtered_by_state)
      }
      return(bgpts[bg_filtered_by_state[rownum,] ,  .(lat, lon,  bgfips, bgid, ST, pop, arealand, areawater), on = "bgid"])
    } else {
      rownum <- sample.int(blockgroupstats[,.N], size = n, replace = FALSE, prob = blockgroupstats$arealand)
      if (!dt) {x=copy(bgpts[blockgroupstats[rownum, ], .(lat, lon, bgfips, bgid, ST, pop, arealand, areawater), on = "bgid"]); setDF(x); return(x)}
      return(          bgpts[blockgroupstats[rownum,],  .(lat, lon, bgfips, bgid, ST, pop, arealand, areawater), on = "bgid"])
    }
  }
  
  # RANDOM US RESIDENTS
  if (weighting == 'pop') {
    if (!is.null(ST_of_blockgroup)) {warning("Ignoring ST_of_blockgroup ! ")}
    rownum <- sample.int(EJAMblockdata::blockwts[,.N], size = n, replace = FALSE, prob = blockwts$blockwt)
    if (!dt) {x=copy(blockpoints[blockwts[rownum,], on="blockid"]); setDF(x); return(x)}
    # all(blockpoints[,blockid] == blockwts[,blockid])
    # [1] TRUE
    return(blockpoints[blockwts[rownum, ],])
  }
  
}
