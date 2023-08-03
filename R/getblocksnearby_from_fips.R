#' getblocksnearby_from_fips
#'
#' @param fips vector of FIPS codes like for States or all counties in a State
#'
#' @return same as for [getblocksnearby] but one row per FIPS and the distance column is irrelevant
#' @export
#'
#' @examples
#'   x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"))
#'   counties_ej <- doaggregate(x, sites2states_or_latlon = unique(x[ , .(siteid, lat, lon)]))
#'   mapfast(counties_ej$results_bysite)
#'   y =  ejamit(fips=fips_counties_from_statename("Delaware"))
#'   
getblocksnearby_from_fips <- function(fips) {
  if (length(fips) == 1) {
    fips <- c(fips, NA) # quick workaround since code below failed if only 1 FIPS provided, like 1 state or only 1 county
  }
  ## create two-column dataframe with bgs (values) and original fips (ind)
  all_bgs <- stack(sapply(fips, fipsbg_from_anyfips))
  names(all_bgs) <- c('bgid','siteid')
  all_bgs$siteid <- as.character(all_bgs$siteid)
  
  fips_blockpoints <- dplyr::left_join(
    all_bgs, 
    ## create 12-digit column inline (original table not altered)
    blockid2fips[, .(blockid, blockfips, blockfips12 = substr(blockfips,1,12))], 
    by=c('bgid'='blockfips12'), multiple='all') %>% 
    dplyr::left_join(blockpoints) %>% 
    dplyr::mutate(distance=0) %>% 
    data.table::as.data.table()
  
  ## remove any invalid latlon values 
  #...
  fips_blockpoints <- na.omit(fips_blockpoints)
  return(fips_blockpoints)
}
