#' getblocksnearby_from_fips
#' Actually finds all blocks within each of the FIPS codes provided
#' @param fips vector of FIPS codes identifying blockgroups, tracts, counties, or states.
#'   This is useful if -- instead of gettings stats on and comparing circular buffers or polygons --
#'    one will be getting stats on one or more tracts, 
#'   or analyzing and comparing blockgroups in a county, 
#'   or comparing whole counties to each other, within a State.
#' @param need_blockwt set to FALSE to speed it up if you do not need blockwt
#' @return same as for [getblocksnearby] but one row per FIPS, and the distance column is irrelevant
#' @export
#'
#' @examples
#'   x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"))
#'   counties_ej <- doaggregate(x, sites2states_or_latlon = unique(x[ , .(siteid, lat, lon)]))
#'   mapfast(counties_ej$results_bysite)
#'   y =  ejamit(fips=fips_counties_from_statename("Delaware"))
#'   
#'   # x=getblocksnearby_from_fips("482011000011") # one blockgroup only
#'   # y=doaggregate(x)
#'   
getblocksnearby_from_fips_OLD <- function(fips, need_blockwt=TRUE) {
  
  fips.char <- fips_lead_zero( fips)  # adds leading zeroes and returns as character, 5 characters if seems like countyfips, etc. 
  fipslengths <- nchar(fips.char)
  if (!(length(unique(fipslengths)) == 1)) {
    # might recode to allow this but it is complicated
    stop('fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
  }
  # fipstype <- "notfips"
  # fipstype[fipslengths == 2] <- "statefips"
  # fipstype[fipslengths == 5] <- "countyfips"
  # fipstype[fipslengths == 11] <- "tractfips"
  # fipstype[fipslengths == 12] <- "bgfips"
  # fipstype[fipslengths == 15] <- "blockfips"
  # > length(unique(substr(blockid2fips$blockfips,1,12)))
  # [1] 242335
  # > length(unique(substr(blockid2fips$blockfips,1,11)))
  # [1] 85395
  # > length(unique(substr(blockid2fips$blockfips,1,5)))
  # [1] 3221
  # 
  if (length(fips) == 1) {
    fips <- c(fips, NA) # quick workaround since code below failed if only 1 FIPS provided, like 1 state or only 1 county
  }
  ## create two-column dataframe with bgs (values) and original fips (ind) 
  # fipsbg_from_anyfips() returns all blockgroup fips codes contained within each fips provided
  all_bgs <- stack(sapply(fips, fipsbg_from_anyfips))
  names(all_bgs) <- c('bgfips', 'siteid')
  # This used to say bgid not bgfips, which was sort of incorrect, in sense it was incompatible with how most of EJAM means by bgid.
  # We do not need bgfips except as 1 way to obtain bgid, which we do need.
  # We want actual bgid integer, so you can have bgid as needed later in doaggregate() 
  
  # Would be more efficient to replace fipsbg_from_anyfips() or make new func to provide bgid_from_anyfips()  ***
  # instead of getting back fips and needing to look up bgid by bgfips 
  all_bgs$bgid <- bgid2fips[match(all_bgs$bgfips, bgfips), bgid]
  
  # is this really needed? :
  all_bgs$siteid <- as.character(all_bgs$siteid) # siteid in this case actually is the fips provided, like a state fips or county fips vector
  
  fips_blockpoints <- dplyr::left_join( # WOULD data.table join or merge be faster than dplyr here? ***
    all_bgs, 
    ## create 12-digit column inline (original table not altered)
    # dont actually need blockfips here except to join on its first 12 chars  
    blockid2fips[, .(blockid, blockfips, blockfips12 = substr(blockfips,1,12))], 
    by=c('bgfips'='blockfips12'), multiple='all') %>% 
    dplyr::left_join(blockpoints) %>% 
    dplyr::mutate(distance=0) %>%   # or do I want distance to be null or missing or NA or 0.001, or what? note approximated block_radius_miles is sometimes zero, in blockwts
    data.table::as.data.table()
  
  if (need_blockwt) {
    # provide blockwt to be consistent with getblocksnearby() and doaggregate() understands it if you want to use it after this.
    #fips_blockpoints[,blockwt := 1] # since doaggregate() uses blockwt even though we know the resulting bgwt will be 1 in every case if used FIPS codes bigger than blocks (blockgroups, tracts, counties, states, whatever)
    fips_blockpoints <- merge(fips_blockpoints, blockwts[,.(blockid, blockwt)], on = "blockid")
  }
  
  ## remove any invalid latlon values 
  #...
  fips_blockpoints <- na.omit(fips_blockpoints)
  
  # normal output of  getblocksnearby() is data.table with  
  #  siteid, blockid, distance, blockwt, bgid
  # but do not really need to return bgfips, blockfips, lat, lon here.
  setcolorder(fips_blockpoints, c('siteid', 'blockid', 'distance', 'blockwt', 'bgid'))
  fips_blockpoints[ , bgfips := NULL]
  fips_blockpoints[ , blockfips := NULL]
  fips_blockpoints[ , lat := NULL]
  fips_blockpoints[ , lon := NULL]
  
  return(fips_blockpoints)
}
