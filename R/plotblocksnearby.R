#' plotblocksnearby - Map view of Census blocks (their centroids) near one or more sites
#' Utility to quickly view one or more facility points on map with the blocks found nearby
#' @details Uses [getblocksnearby()] if lat,lon points provided as sitepoints,
#'  but skips it if looks like user passed output of getblocksnearby(),
#'  and then displays a map of those blocks near the specified point.
#' @param sitepoints table of points with lat, lon in decimal degrees (data.frame or data.table),
#'   but also could just be the output of getblocksnearby() if that has already been done.
#' @param sites2blocks If provided, used as sites2blocks like [testoutput_getblocksnearby_10pts_1miles]
#'    If neither sites2blocks nor sitepoints is provided it cannot plot and returns error.
#'    If sites2blocks and sitepoints are both provided, it uses them both to plot blocks and sites (centers of circles).
#'    If sites2blocks not provided, but sitepoints alone is provided, checks if sitepoints is actually sites2blocks, and uses as such.
#'    If sites2blocks not provided, but sitepoints alone is provided, and sitepoints is really sitepoints, it runs getblocksnearby() to create sites2blocks.
#'    If sites2blocks is provided, but sitepoints is not, it could only do a bad approximation of sitepoints so it will not draw the circle or site at center of the circle.
#' @param radius in miles
#' @param usemapfast optional. simpler plot if FALSE
#' @param ... passed to mapfast() or plot() depending on usemapfast
#'
#' @return invisibly returns sites2blocks like getblocksnearby() does
#' @export
#'
#' @examples 
#'   #  see all Census Blocks within 1 mile of 1 site, if already had run getblocksnearby()
#'    z =  plotblocksnearby(sitepoints = testpoints_10, 
#'    sites2blocks = testoutput_getblocksnearby_10pts_1miles[siteid == 1,], radius = 1  )
#'    # see two sites if already had run getblocksnearby()
#'    z =  plotblocksnearby(sitepoints = testpoints_10[c(4,10),], 
#'    sites2blocks = testoutput_getblocksnearby_10pts_1miles[siteid %in% c(4,10),], radius = 1  )
#'   \dontrun{
#'   # See one randomly selected regulated facility from FRS and all Census Blocks within 2 miles:
#'     plotblocksnearby(testpoints_n(1), 2) 
#'   # See two sites and all Census Blocks within 5 kilometers
#'     plotblocksnearby(testpoints_2, radius = convert_units(5, from = "km", towhat = "miles"))
#'   # See 100 sites and all blocks within 1 mile of each - 
#'   # Note you have to specify radius here or it uses default that may not match intent 
#'   # - and this is a bit slow
#'   plotblocksnearby(testdata_ejamit_output_100pts_1miles$results_bysite[,.(siteid, lat,lon)], radius = 1) 
#'   }
plotblocksnearby <- function(sitepoints, radius=3, sites2blocks,  usemapfast=TRUE, ...) {
  if (radius > 32) {stop("cannot use radius >32 miles here")}
  if (missing(sitepoints) &  missing(sites2blocks)) {stop('must provide either sitepoints or sites2blocks or both')}
  if (missing(sitepoints) & !missing(sites2blocks)) {
    warning('sitepoints missing so will not try to guess site at center of circle - drawing surrounding blocks only')
  }
  if (!missing(sitepoints) & !missing(sites2blocks)) {
    # good
  }
  
  if ( all(c("blockid", "distance", "siteid") %in% names(sitepoints))) {
    really_sitepoints <- FALSE
    # instead of a list of lat,lon points, the user has just provided the output of getblocksnearby() already run, aka sites2blocks
    # so skip getblocksnearby() ... do not have to do it again if already done.
    sites2blocks <- sitepoints # which ignores what they might have also passed as sites2blocks = xyz redundantly
    if (length(unique(sites2blocks$siteid)) > 100) {
      warning("this mapping function is not intended for so many sites - showing only the first 100")
      sites2blocks <- sites2blocks[siteid %in% unique(siteid)[1:100]]
    }
    # but will need to approximate the lat,lon of each site, below, after we get lat lon of each block.
    # and the approximation done here is pretty bad so the circle drawn around that bad approximation of sitepoints is not covering the right blocks !
  } else {
    really_sitepoints <- TRUE
    if (nrow(sitepoints) > 100) {
      warning("this mapping function is not intended for so many sites - showing only the first 100")
      sitepoints <- sitepoints[1:100,]
    }
    if (missing(sites2blocks)) {
      # Use getblocksnearby() since   user only provided a list of lat,lon points.
      if (!("siteid" %in% names(sitepoints))) {sitepoints$siteid <- seq_len(nrow(sitepoints))}
      sites2blocks <- getblocksnearby(sitepoints, radius = radius, ...)
    }
  }
  
  # Get the lat,lon of each block so we can map them - could try join instead?? ####
  bl <- latlon_join_on_blockid(sites2blocks) # now checks 1st to see if lat lon already there.  #old:   merge( sites2blocks, blockpoints , on = "blockid")
  setDT(bl)
  setnames(bl, 'lat', 'blocklat')
  setnames(bl, 'lon', 'blocklon')
  
  # in the scenario where we got only output of getblocksnearby() not actual sitepoints,
  # we could use block lat,lon values to approximate the lat,lon of each site, since we were not given that 
  # but this approximation would be really bad...
  if (!really_sitepoints) {
    # infer radius approximately??? ####
    if (missing(radius)) {radius <- round(max(bl$distance, na.rm = TRUE),1)}
    # VERY roughly infer sitepoints lat,lon of sites from info in sites2blocks table... just for a rough map... but 
    # *** CAN FIX/ IMPROVE THIS APPROXIMATION BY USING trilaterate() to 
    #  really more accurately recreate the sitepoints lat,lon info from distances and lat,lon of blocks! (once trilaterate() is debugged/checked)
    # sitepoints <- bl[ , list(lat = mean(blocklat), lon = mean(blocklon)), by = "siteid"]
    # create dummy empty info for now
    sitepoints <- data.frame(siteid = -999)
  }
  
  # Put site point(s) (which have lat,lon) and surrounding block points (which have blocklat,blocklon) into one table
  setDT(sitepoints)
  x <- merge(sitepoints, bl, by = "siteid", all.y = TRUE) # all.y is true to keep all blocks in case dummy sitepoints was used and has no matching siteid rows
  
  if (usemapfast) {
    
    # BLOCKS SURROUNDING A SITE
    xb <- copy(x)
    xb[, lat := blocklat]
    xb[, lon := blocklon]
    xb <- unique(xb)
    
    if (really_sitepoints) {
      
      # SITE AT CENTER OF A CIRCLE
      xpt <- copy(x)
      xpt <- xpt[ !duplicated(xpt[,.(lat,lon)]),] # only one point per siteid, since the block coordinates in x had been named blocklat, blocklon, so the lat,lon here were just the site coordinates
      xpt$distance <- 0
      xpt$bgid    <- NA
      xpt$blockwt <- NA; xpt$blockid  <- NA; xpt$blocklat <- NA; xpt$blocklon <- NA
      
      mapinfo <- rbind(xpt, xb)
    } else {
      mapinfo <- xb
    }
    
    z <- mapfast(mapinfo, radius = 0.005, ...) #  %>% 
    
    if (really_sitepoints) {
      # overall circle centered on each site, to show radius of search
      z <-  addCircles(z, lat = xpt$lat, lng = xpt$lon, radius = meters_per_mile * radius, color = "gray",
                 fillOpacity = 0.06, fillColor = "gray", opacity = 0.7  ) #  %>% # overall circle
        # site point in center of each circle
        z <-  addCircleMarkers( z, lat = xpt$lat, lng = xpt$lon, radius = 10, color = "red", opacity = 0.75) # %>%  # in pixels for center of circle=point 
    }
    # Map popup info for each site (if available) and blocks surrounding the site
    z <- addCircles(z, lat = mapinfo$lat, lng = mapinfo$lon, fillOpacity = 0.1, 
               popup = popup_from_df(setDF(mapinfo)), radius = 10)
    
    print(z)
    # setnames(bl, 'blocklat', 'lat')
    # setnames(bl, 'blocklon', 'lon')
    # mapfast(rbind(bl, unique(x[,.(lat, lon)])))
  } else {
    bplot = function(x,   ... ) {
      plot(x = x$blocklon, y = x$blocklat , ... )
      if (really_sitepoints) {
        points(x = x$lon, y = x$lat  , col = "red")
      }
    }
    # bplot(x, sample(1:nrow(sitepoints), 1)) # plots a random site surrounded by nearby block points
    bplot(x, main = "Site and surrounding block centroids", xlab = "", ylab = "" )
    
  }
  invisible(sites2blocks)
}

