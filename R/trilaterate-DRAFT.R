
#' DRAFT - Estimate lat,lon of each ejam_uniq_id, from outputs of getblocksnearby()
#' 
#' get data.table with ejam_uniq_id, lat,lon of each site (eg for when you did not save sitepoints info)
#' @param s2b like [testoutput_getblocksnearby_10pts_1miles]
#'
#' @return a data.table with one row per unique ejam_uniq_id from input dt, 
#'   plus lat,lon columns  
#' @export
#'
#' @examples 
#' s2b = copy(testoutput_getblocksnearby_10pts_1miles)
#' s2b_located = latlon_join_on_blockid(s2b) # done by trilaterate also
#' inferred_sites = trilaterate_sites2blocks(s2b)
#' inferred_sites
#' plotblocksnearby(s2b_located)
#' 
trilaterate_sites2blocks <- function(s2b) {
  s2b = copy(s2b) # otherwise it modifies s2b in parent env by reference
  
  # example:
  # s2b = copy(testoutput_getblocksnearby_10pts_1miles)
  # latlon_join_on_blockid(s2b) # done by trilaterate also
  # x = trilaterate_sites2blocks(s2b)
  # x
  
  # trilaterate() would improve calculation in plotblocksnearby() 
  #  and also help in doaggregate() when missing original sitepoints or pts table
  
  ######################################################################### #
  trilaterate_dt <- function(dt) {
    
    warning("This formula does not provide a good estimate, or maybe the distance estimates are wrong.")
    return(NULL)
    
    # dt table has one row per ejam_uniq_id - it gets modified by reference here, new cols added - altered in parent env?
    # for which we want to estimate lat,lon 
    # y1 y2 y3 vectors are the latitudes of the 3 most distant blocks, 
    # x1 x2 x3 vectors are the longitudes of the 3 most distant blocks
    # d1, d2, d3 vectors are distance values of the 3 most distant blocks
    #   # returns a dt with one row per sitepoint lon=x and lat=y for each ejam_uniq_id surrounded by many blocks in sites2blocks
    
    # *** THIS FORMULA HAS A PROBLEM - WRONG ANSWER IS BEING PRODUCED. 
    # Trilateration requires good distance approximations to function properly.
    # A better alternative would be to use the "modified weighted centroid localization" algorithm as presented 
    # in this paper: https://ieeexplore.ieee.org/document/4447528 . This algorithm is generally more forgiving when using noisy RSS samples.
    # https://math.stackexchange.com/questions/3837573/finding-the-intersection-of-three-circles
    
    dt[, lat := -1 * ((x2 - x3) * (x2^2 - x1^2 +  y2^2 - y1^2 +  d1^2 - d2^2) -
                    (x1 - x2) * (x3^2 - x2^2 +  y3^2 - y2^2 +  d2^2 - d3^2) ) / 
         (2 * ((y1 - y2) * (x2 - x3) - (y2 - y3) * (x1 - x2) ))]
    
    dt[, lon := -1 * ((y2 - y3) * (y2^2 - y1^2 +  x2^2 - x1^2 +  d1^2 - d2^2) -
                      (y1 - y2) * (y3^2 - y2^2 +  x3^2 - x2^2 +  d2^2 - d3^2) ) / 
         (2 * ((x1 - x2) * (y2 - y3) - (x2 - x3) * (y1 - y2) ))
    ]
    
    # dt[,AA := 2 * (x2 - x1)]
    # dt[,BB := 2 * (y2 - y1)]
    # dt[,DD := 2 * (x3 - x2)]
    # dt[,EE := 2 * (y3 - y2)]
    # dt[,CC := d1^2 - d2^2 - x1^2 + x2^2 - y1^2 + y2^2]
    # dt[,FF := d2^2 - d3^2 - x2^2 + x3^2 - y2^2 + y3^2]
    # dt[,lon := (CC * EE - FF * BB) / (EE * AA - BB * DD)]
    # dt[,lat := (CC * DD - AA * FF) / (BB * DD - AA * EE)]
    
    return(dt[ , .(ejam_uniq_id, lat, lon)])
  }
  ######################################################################### #
  
  s2b[ , c('blockwt', 'bgid') := NULL] # drop those 2 cols
  
  # MUST HAVE lat, lon  of each block, first... 
  # merge or join in the lat,lon of each block so we can map them - could try join instead?? ####
  if (!all(c('lat','lon') %in% names(s2b))) {
    # s2b <- merge(s2b, blockpoints , on = "blockid")
    s2b <- latlon_join_on_blockid(s2b) # messes up sort order
  }
  setorder(s2b, ejam_uniq_id, -distance) # descending distance order 
  
  far3block_bysite <- s2b[ ,  .(d1 = distance[1], d2 = distance[2], d3 = distance[3], 
                                x1 = lon[1], x2 = lon[2], x3 = lon[3],
                                y1 = lat[1], y2 = lat[2], y3 = lat[3]
  ), by = "ejam_uniq_id"]
  # DT[, .SD[2], by=x]                    # get 2nd row of each group
  # DT[, tail(.SD,3), by=x]               # last 3 rows of each group
  
  return(
    trilaterate_dt(far3block_bysite) # changes far3block_bysite by ref which wont matter, and then returns only 3 cols
  )
}

