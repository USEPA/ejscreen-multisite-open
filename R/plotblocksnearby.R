#' Map view of Census blocks (their centroids) near one or more sites
#' Utility to quickly view one or more facility points on map with the blocks found nearby
#' @details Uses [getblocksnearby()] if lat,lon points provided as sitepoints,
#'  but skips it if looks like user passed output of getblocksnearby(),
#'  and then displays a map of those blocks near the specified point.
#' @param sitepoints table of points with lat, lon in decimal degrees (data.frame or data.table),
#'   but also could just be the output of getblocksnearby() if that has already been done.
#' @param radius in miles
#' @param usemapfast optional. simpler plot if FALSE
#' @param ... passed to mapfast() or plot() depending on usemapfast
#'
#' @return invisibly returns sites2blocks like getblocksnearby() does
#' @export
#'
#' @examples 
#'   # At 2 sites see all Census Blocks within 1 mile
#'   plotblocksnearby(sites2blocks_example_2pts_1miles)
#'   \dontrun{
#'   # See one randomly selected regulated facility from FRS and all Census Blocks within 2 miles:
#'   plotblocksnearby(testpoints_n(1), 2) 
#'   # See two sites and all Census Blocks within 5 kilometers
#'   plotblocksnearby(testpoints_2, radius = convert_units(5, from = "km", towhat = "miles"))
#'   # See 100 sites and all blocks within 1 mile of each - 
#'   # Note you have to specify radius here or it uses default that may not match intent 
#'   # - and this is a bit slow
#'   plotblocksnearby(testdata_ejamit_output_100pts_1mile$results_bysite[,.(siteid, lat,lon)], radius = 1) 
#'   }
plotblocksnearby <- function(sitepoints, radius=3, usemapfast=TRUE, ...) {
  if (radius > 32) {stop("cannot use radius >32 miles here")}
  
  if ( all(c("blockid", "distance", "siteid") %in% names(sitepoints))) {
    really_sitepoints <- FALSE
    # instead of a list of lat,lon points, the user has just provided the output of getblocksnearby() already run, aka sites2blocks
    # so skip getblocksnearby() ... do not have to do it again if already done.
    sites2blocks <- sitepoints
    if (length(unique(sites2blocks$siteid)) > 100) {
      warning("this mapping function is not intended for so many sites - showing only the first 100")
      sites2blocks <- sites2blocks[siteid %in% unique(siteid)[1:100]]
    }
    # but will need to approximate the lat,lon of each site, below, after we get lat lon of each block.
  } else {
    really_sitepoints <- TRUE
  if (nrow(sitepoints) > 100) {
    warning("this mapping function is not intended for so many sites - showing only the first 100")
    sitepoints <- sitepoints[1:100,]
    }
    # Use getblocksnearby() since we will assume the user provided a list of lat,lon points.
    if (!("siteid" %in% names(sitepoints))) {sitepoints$siteid <- seq_len(nrow(sitepoints))}
    sites2blocks <- getblocksnearby(sitepoints, radius = radius, ...)
  }
  
  # Get the lat,lon of each block so we can map them - could try join instead?? ####
  bl <- latlon_join_on_blockid(sites2blocks) # now checks 1st to see if lat lon already there.  #old:   merge( sites2blocks, blockpoints , on = "blockid")
  setDT(bl)
  setnames(bl, 'lat', 'blocklat')
  setnames(bl, 'lon', 'blocklon')
 
  # in the scenario where we got only output of getblocksnearby() not actual sitepoints,
  # use block lat,lon values to approximate the lat,lon of each site, since we were not given that 
  if (!really_sitepoints) {
    # infer radius approximately ####
    if (missing(radius)) {radius <- round(max(bl$distance, na.rm = TRUE),1)}
    
    # VERY roughly infer sitepoints lat,lon of sites from info in sites2blocks table... just for a rough map... but 
    # *** CAN FIX/ IMPROVE THIS APPROXIMATION BY USING trilaterate() to 
    #  really more accurately recreate the sitepoints lat,lon info from distances and lat,lon of blocks! (once trilaterate() is debugged/checked)
    sitepoints <- bl[ , list(lat = mean(blocklat), lon = mean(blocklon)), by = "siteid"]
  }
  
  # Put site point(s) (which have lat,lon) and surrounding block points (which have blocklat,blocklon) into one table
  setDT(sitepoints)
  x <- merge(sitepoints, bl, by = "siteid")
  
  if (usemapfast) {
    
    # BLOCKS SURROUNDING A SITE
    xb <- copy(x)
    xb[, lat := blocklat]
    xb[, lon := blocklon]
    xb <- unique(xb)
    
    # SITE AT CENTER OF A CIRCLE
    xpt <- copy(x)
    xpt <- xpt[ !duplicated(xpt[,.(lat,lon)]),] # only one point per siteid
    xpt$distance <- 0
    xpt$bgid    <- NA
    xpt$blockwt <- NA; xpt$blockid  <- NA; xpt$blocklat <- NA; xpt$blocklon <- NA
      
    mapinfo <- rbind(xpt, xb)
    
    z <- mapfast(mapinfo, radius = 0.005, ...)  %>% 
      # overall circle centered on each site, to show radius of search
      addCircles(lng = xpt$lon, lat=xpt$lat, radius = meters_per_mile * radius, color = "gray", fillOpacity = 0.06, fillColor = "gray", opacity = 0.7  )  %>% # overall circle
      # site point in center of each circle
      addCircleMarkers(lng = xpt$lon, lat=xpt$lat, radius = 10, color = "red", opacity = 0.75) %>%  # in pixels for center of circle=point 
      # Map popup info for each site and blocks surrounding the site
      addCircles(lng = mapinfo$lon, lat = mapinfo$lat, fillOpacity = 0.1, 
                 popup = popup_from_df(setDF(mapinfo)), radius = 10)
    print(z)
    # setnames(bl, 'blocklat', 'lat')
    # setnames(bl, 'blocklon', 'lon')
    # mapfast(rbind(bl, unique(x[,.(lon,lat)])))
  } else {
    bplot=function(x,   ... ) {
      plot(x$blocklon , x$blocklat , ... )  
      points(x$lon , x$lat  , col = "red")
    }
    # bplot(x, sample(1:nrow(sitepoints), 1)) # plots a random site surrounded by nearby block points
    bplot(x, main="Site and surrounding block centroids", xlab="",ylab="" )
    
  }
  invisible(sites2blocks)
}

