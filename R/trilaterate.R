#' latlon_join_on_blockid - get lat,lon of each block internal point via blockid
#' get expanded version of sites2blocks data.table, with new lat,lon columns
#' @param s2b like [sites2blocks_example_2pts_1miles], output of outputs of [getblocksnearby()]
#'
#' @return returns the input data.table but with lat,lon columns added as block coordinates
#' @export
#'
#' @examples 
#'  s2b = copy(sites2blocks_example_2pts_1miles)
#'  latlon_join_on_blockid(s2b) # done by trilaterate also
#'  
latlon_join_on_blockid = function(s2b) {
  if (all(c('lat','lon') %in% names(s2b))) {message('already has lat,lon'); return(s2b)}
 return(
   merge(s2b, blockpoints , on = "blockid")
   # better via a join, though right? could modify param by reference without even expicitly returning anything then
 )
}

 
#' trilaterate_sites2blocks - Estimate lat,lon of each siteid, from outputs of getblocksnearby()
#' get data.table with siteid, lat,lon of each site (eg for when you did not save sitepoints info)
#' @param s2b like [sites2blocks_example_2pts_1miles]
#'
#' @return a data.table with one row per unique siteid from input dt, 
#'   plus lat,lon columns  
#' @export
#'
#' @examples 
#' s2b = copy(sites2blocks_example_2pts_1miles)
#' s2b_located = latlon_join_on_blockid(s2b) # done by trilaterate also
#' inferred_sites = trilaterate_sites2blocks(s2b)
#' inferred_sites
#' plotblocksnearby(s2b_located)
#' 
trilaterate_sites2blocks <- function(s2b) {
  s2b = copy(s2b) # otherwise it modifies s2b in parent env by reference
  
  # example:
  # s2b = copy(sites2blocks_example_2pts_1miles)
  # latlon_join_on_blockid(s2b) # done by trilaterate also
  # x = trilaterate_sites2blocks(s2b)
  # x

  # trilaterate() would improve calculation in plotblocksnearby() 
  #  and also help in doaggregate() when missing original sitepoints or pts table
  
  ######################################################################### #
  trilaterate_dt <- function(dt) {
    
    # dt table has one row per siteid - it gets modified by reference here, new cols added - altered in parent env?
    # for which we want to estimate lat,lon 
    # y1 y2 y3 vectors are the latitudes of the 3 most distant blocks, 
    # x1 x2 x3 vectors are the longitudes of the 3 most distant blocks
    # d1, d2, d3 vectors are distance values of the 3 most distant blocks
    #   # returns a dt with one row per sitepoint lon=x and lat=y for each siteid surrounded by many blocks in sites2blocks
    
    dt[,AA := 2 * (x2 - x1)]
    dt[,BB := 2 * (y2 - y1)]
    dt[,DD := 2 * (x3 - x2)]
    dt[,EE := 2 * (y3 - y2)]
    dt[,CC := d1^2 - d2^2 - x1^2 + x2^2 - y1^2 + y2^2]
    dt[,FF := d2^2 - d3^2 - x2^2 + x3^2 - y2^2 + y3^2]
    dt[,lon := (CC * EE - FF * BB) / (EE * AA - BB * DD)]
    dt[,lat := (CC * DD - AA * FF) / (BB * DD - AA * EE)]
    
    return(dt[ , .(siteid, lat, lon)])
  }
  ######################################################################### #
  
  setorder(s2b, -distance) # descending distance order 
  s2b[ , c('blockwt', 'bgid') := NULL] # drop those 2 cols
  
  # MUST HAVE lat, lon  of each block, first... 
  # merge or join in the lat,lon of each block so we can map them - could try join instead?? ####
  if (!all(c('lat','lon') %in% names(s2b))) {
  # s2b <- merge(s2b, blockpoints , on = "blockid")
  s2b <- latlon_join_on_blockid(s2b)
  }
  
  far3block_bysite <- s2b[ ,  .(d1 = distance[1], d2 = distance[2], d3 = distance[3], 
                                x1 = lon[1], x2 = lon[2], x3 = lon[3],
                                y1 = lat[1], y2 = lat[2], y3 = lat[3]
  ), by = "siteid"]
  # DT[, .SD[2], by=x]                    # get 2nd row of each group
  # DT[, tail(.SD,3), by=x]               # last 3 rows of each group
  
  return(
    trilaterate_dt(far3block_bysite) # changes far3block_bysite by ref which wont matter, and then returns only 3 cols
    )
}

