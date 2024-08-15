#' Random points in USA - average resident, facility, BG, block, or square mile
#' 
#' Get data.table of Random Points (lat lon) for Testing/ Benchmarking/ Demos, weighted in various ways. 
#'   The weighting can be specified so that each point reflects the average EPA-regulated 
#'   facility, blockgroup, block, place on the map, or US resident.
#'   
#' @param n Number of points needed (sample size)
#' @param weighting word indicating how to weight the random points (some synonyms are allowed, in addition to those shown here): 
#'   
#'   Note the default is frs, but you may want to use pop even though it is slower.
#'   
#'   - pop or people = Average Person: random person among all US residents (block point of residence per 2020 Census) 
#'   
#'   - frs or facility = Average Facility: random EPA-regulated facility from actives in Facility Registry Service (FRS)
#'   
#'   - bg = Average Blockgroup: random US Census block group (internal point like a centroid)
#'   
#'   - block = Average Block: random US Census block (internal point like a centroid)
#'   
#'   - area or place = Average Place: random point on a map (internal point of avg blockgroup weighted by its square meters size)
#'   
#' @param dt logical, whether to return a data.table (DEFAULT) instead of normal data.frame
#' @param validonly return only points with valid lat/lon coordinates. Defaults to TRUE.
#'
#' @return data.frame or data.table with columns lat, lon in decimal degrees, and 
#'   any other columns that are in the table used (based on weighting)
#' @param ST_needed optional, can be a character vector of 2 letter State abbreviations to pick from only some States.
#'   
#' @import data.table
#' @export
#' @examples 
#' mapfast(testpoints_n(300, ST_needed = c('LA','MS')) )
#' \dontrun{
#' n=2
#' for (d in c(TRUE,FALSE)) {
#'   for (w in c('frs', 'pop', 'area', 'bg', 'block')) {
#'     cat("n=",n,"  weighting=",w, "  dt=",d,"\n\n")
#'     print(x <- testpoints_n(n,w,d)); print(class(x))
#'     cat('\n')
#'   }
#' }
#' }
#'  
testpoints_n <- function(n=10, weighting=c('frs', 'pop', 'area', 'bg', 'block'), dt=TRUE, ST_needed=NULL, validonly = TRUE) {
  
  if (NROW(n)  == 1) {
    if (n == 1e6) {warning('a million used to sound like a lot')}
    if (n > 1e6) {warning('more than a million, yikes')}
    if (n > 1e7) {warning('ridiculous n value OMG')}
    if (n == 1) {warning('just want one point? whatever.')}
    if (n == 0) {warning('zero points. srsly?')}
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if ( NROW(n) > 1  || !is.numeric(n) || is.na(n) || !is.wholenumber(n) || is.infinite(n) || n < 0 || n > 5e7) {
    warning('n must be a single whole number, where <0 is unfathomable and >50 million is too hard to keep track of')
    return(NULL)
  }
  
  # Handle weighting synonyms and default
  if (missing(weighting)) weighting <- 'frs'  # faster than most other options
  if (any( length(weighting) != 1, class(weighting) != "character")) {
    warning("invalid weighting parameter for testpoints_n")
    return(NULL)
  }
  weighting <- tolower(weighting)
  if (weighting %in% c('frs', 'facility', 'facilities', 'facil', 'fac', 'frsid', 'regid')) weighting <- "frs"
  if (weighting %in% c('blockgroup', 'blockgroups', 'bg', 'bgs', 'block group', 'block groups', 'bgid', 'bgfips', "FIPS")) {weighting <- "bg"}
  if (weighting %in% c('block', 'blocks', 'blockpoints', 'blockid', 'blockfips')) {weighting <- 'block'}
  if (weighting %in% c('area', 'map', 'square meters', 'square meter', 'square mile', 'place')) {weighting <- "area"}
  if (weighting %in% c('person', 'pop', 'people', 'resident', 'population', 'residents'))  weighting <- 'pop'
  if (!(weighting %in% c('frs', 'pop', 'area', 'bg', 'block'))) {
    warning("invalid weighting parameter for testpoints_n")
    return(NULL)
  } 
  
  # RANDOM FACILITIES (EPA-regulated facilities in FRS)
  if (weighting == "frs") {
    
    if (!exists("frs")) dataload_from_pins("frs")
    
    if (!is.null(ST_needed)) {
      statecount = length(ST_needed)
      
      # this should be written as a recursive function but didnt have time to do that:
      extrasize =  150 * statecount * n # try to find n in 1 state must on avg check on 52n, but check 150n to be very likely to have enough.
      rowtried <- sample.int(frs[,.N], size = extrasize, replace = FALSE)
      
      rowinstate <- rowtried[state_from_latlon(lat = frs$lat[rowtried], lon = frs$lon[rowtried])$ST %in% ST_needed]
      stillneed <- n - length(rowinstate)
      if (stillneed > 0) warning('did not find enough within specified state(s) in this attempt')
      if (stillneed < 0 ) rowinstate <- rowinstate[1:n]
      # extrasize =  70 * statecount * stillneed
      # rowtried2 <- sample.int( frs[-rowtried, .N], size = extrasize, replace = FALSE)
      # rowinstate2 <- c(rowinstate, rowtried2[state_from_latlon(lat = frs$lat[rowtried2], lon = frs$lon[rowtried2]) %in% ST_needed])
      # 
      # rownum <- sample.int( frs[rowinstate,.N], size = n, replace = FALSE)
      if (!dt) {
        x = data.table::copy(frs[rowinstate,] )
        setDF(x)
        x$sitenumber <- seq_len(nrow(x))
        if(validonly){
          message('Returning only sites with valid lat/lons')
          return(x[latlon_is.valid(x$lat, x$lon),])
        } else {
          return(x)
        }
        }
      x <- frs[rowinstate,]; x$sitenumber <- seq_len(nrow(x))
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
    }
    rownum <- sample.int(frs[,.N], size = n, replace = FALSE)
    if (!dt) {x = data.table::copy(frs[rownum, ]); setDF(x);  x$sitenumber <- seq_len(nrow(x))
    if(validonly){
      message('Returning only sites with valid lat/lons')
      return(x[latlon_is.valid(x$lat, x$lon),])
    } else {
      return(x)
    }
    }
    x <- frs[rownum,]; x$sitenumber <- seq_len(nrow(x))
    if(validonly){
      message('Returning only sites with valid lat/lons')
      return(x[latlon_is.valid(x$lat, x$lon),])
    } else {
      return(x)
    }
  }
  
  # RANDOM BLOCKGROUPS
  if (weighting == 'bg') {
    if (!is.null(ST_needed)) {
      stfips <- EJAM::stateinfo$FIPS.ST[match(ST_needed, EJAM::stateinfo$ST)]
      bg_filtered_by_state <- data.table::copy(EJAM::bgpts[substr(bgfips,1,2) %in% stfips, ])
      rownum <- sample.int(bg_filtered_by_state[,.N], size = n, replace = FALSE)
      if (!dt) {
        setDF(bg_filtered_by_state)
        #return(bg_filtered_by_state[rownum, ])
        if(validonly){
          message('Returning only sites with valid lat/lons')
          return(bg_filtered_by_state[rownum, ][latlon_is.valid(x$lat, x$lon),])
        } else {
          return(bg_filtered_by_state[rownum, ] )
        }
        }
      
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(bg_filtered_by_state[rownum, ][latlon_is.valid(x$lat, x$lon),])
      } else {
        return(bg_filtered_by_state[rownum, ] )
      }
    } else {
      rownum <- sample.int(EJAM::bgpts[,.N], size = n, replace = FALSE)
      if (!dt) {
        x = data.table::copy(bgpts[rownum, ])
        setDF(x)
        x$sitenumber <- seq_len(nrow(x))
        if(validonly){
          message('Returning only sites with valid lat/lons')
          return(x[latlon_is.valid(x$lat, x$lon),])
        } else {
          return(x)
        }
      }
      x <- bgpts[rownum, ]; x$sitenumber <- seq_len(nrow(x)) 
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
    }
  }
  
  # RANDOM BLOCKS
  if (weighting == 'block') {
    cat('loading blockpoints dataset\n')
    if (!is.null(ST_needed)) {
      staterownums <- which(state_from_blockid(blockpoints$blockid) %in% ST_needed  )
      rownum <- sample.int(length(staterownums), size = n, replace = FALSE)
      if (!dt) {x = data.table::copy(blockpoints[staterownums,][rownum,])
      setDF(x)
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
      }
      
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(blockpoints[staterownums,][rownum,][latlon_is.valid(blockpoints[staterownums,][rownum,]$lat, blockpoints[staterownums,][rownum,]$lon),])
      } else {
        return(blockpoints[staterownums,][rownum,] )
      }
    } else {
      rownum <- sample.int( blockpoints[,.N], size = n, replace = FALSE)
      if (!dt) {x = data.table::copy(blockpoints[rownum,]); setDF(x)
      x$sitenumber <- seq_len(nrow(x))
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
     
      }
      x <- blockpoints[rownum, ] ; x$sitenumber <- seq_len(nrow(x))
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
    }
  }
  
  # RANDOM POINTS ON THE MAP
  if (weighting == 'area') {
    # stop('blockpoints$area needs to be added to blockpoints')
    if (!is.null(ST_needed)) {
      # stfips <- EJAM::stateinfo$FIPS.ST[match(ST_needed, EJAM::stateinfo$ST)]
      bg_filtered_by_state <- data.table::copy(EJAM::blockgroupstats[ST %in% ST_needed, .(bgfips, bgid, ST, pop, area) ])
      rownum <- sample.int(bg_filtered_by_state[,.N], size = n, replace = FALSE)
      
      if (!dt) {
        bg_filtered_by_state <- bgpts[bg_filtered_by_state[rownum, ], .(lat, lon,  bgfips, bgid, ST, pop, area)]
        setDF(bg_filtered_by_state)
        bg_filtered_by_state$sitenumber <- seq_len(nrow(bg_filtered_by_state))
        
        if(validonly){
          message('Returning only sites with valid lat/lons')
          return(bg_filtered_by_state[latlon_is.valid(bg_filtered_by_state$lat, bg_filtered_by_state$lon),])
        } else {
          return(bg_filtered_by_state)
        }
      }
      x <- bgpts[bg_filtered_by_state[rownum,] ,  .(lat, lon,  bgfips, bgid, ST, pop, area), on = "bgid"]
      x$sitenumber <- seq_len(nrow(x))
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
      
    } else {
      rownum <- sample.int(blockgroupstats[,.N], size = n, replace = FALSE, prob = blockgroupstats$area)
      if (!dt) {
        x = data.table::copy(bgpts[blockgroupstats[rownum, ], .(lat, lon, bgfips, bgid, ST, pop, area), on = "bgid"])
        setDF(x); x$sitenumber <- seq_len(nrow(x))
        if(validonly){
          message('Returning only sites with valid lat/lons')
          return(x[latlon_is.valid(x$lat, x$lon),])
        } else {
          return(x)
        }
      }
      x <-           bgpts[blockgroupstats[rownum,],  .(lat, lon, bgfips, bgid, ST, pop, area), on = "bgid"]
      x$sitenumber <- seq_len(nrow(x))
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
    }
  }
  
  # RANDOM US RESIDENTS
  if (weighting == 'pop') {
    if (!is.null(ST_needed)) {  # limited by State
      staterownums <- which(state_from_blockid(blockpoints$blockid) %in% ST_needed  )
      
      rownum <- sample.int(length(staterownums), size = n, replace = FALSE, prob = blockwts$blockwt[staterownums])
      if (!dt) {x = data.table::copy(blockpoints[staterownums,][rownum,]); setDF(x); x$sitenumber <- seq_len(nrow(x))
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
      }
      x <- blockpoints[blockwts[staterownums,][rownum,] , ,on = "blockid"]
      x$sitenumber <- seq_len(nrow(x))
      if(validonly){
        message('Returning only sites with valid lat/lons')
        return(x[latlon_is.valid(x$lat, x$lon),])
      } else {
        return(x)
      }
      # warning("Ignoring ST_needed ! ")
    }
    rownum <- sample.int( blockwts[,.N], size = n, replace = FALSE, prob = blockwts$blockwt)
    if (!dt) {x = data.table::copy(blockpoints[blockwts[rownum,], on = "blockid"]); setDF(x); x$sitenumber <- seq_len(nrow(x))
    if(validonly){
      message('Returning only sites with valid lat/lons')
      return(x[latlon_is.valid(x$lat, x$lon),])
    } else {
      return(x)
    }
    }
    # all(blockpoints[,blockid] == blockwts[,blockid])
    # [1] TRUE
    x <- blockpoints[blockwts[rownum, ],]
    x$sitenumber <- seq_len(nrow(x))
    if(validonly){
      message('Returning only sites with valid lat/lons')
      return(x[latlon_is.valid(x$lat, x$lon),])
    } else {
      return(x)
    }
  }
  
}
