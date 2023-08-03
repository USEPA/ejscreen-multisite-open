#' Map view of Census blocks (their centroids) near one or more sites
#' Utility to quickly view one or more facility points on map with the blocks found nearby
#' @param sitepoints table of points with lat, lon in decimal degrees (data.frame or data.table)
#' @param radius in miles
#' @param usemapfast optional. simpler plot if FALSE
#' @param ... passed to mapfast() or plot() depending on usemapfast
#'
#' @return invisibly returns sites2blocks like getblocksnearby() does
#' @export
#'
#' @examples plotblocksnearby(testpoints_n(1), 2) 
plotblocksnearby <- function(sitepoints, radius=3, usemapfast=TRUE, ...) {
  if (radius > 32) {stop("cannot use radius >32 miles here")}
  if (nrow(sitepoints) > 100) {stop("this mapping function is not intended for so many points")}
 if (!("siteid" %in% names(sitepoints))) {sitepoints$siteid <- seq_len(nrow(sitepoints))}
 sites2blocks <- getblocksnearby(sitepoints, radius = radius, ...)
 setDT(sitepoints)
   bl <- merge( sites2blocks, blockpoints , on = "blockid")
   setDT(bl)
   setnames(bl, 'lat', 'blocklat')
  setnames(bl, 'lon', 'blocklon')
  x <- merge(sitepoints, bl, by = "siteid")
  if (usemapfast) {
     xb=copy(x)
    xb[, lat := blocklat]
    xb[, lon := blocklon]
    xb <- unique(xb)
    xpt = copy(x)
    xpt <- xpt[ !duplicated(xpt[,.(lat,lon)]),]

        mapinfo <- rbind(xpt, xb)
    z <- mapfast(mapinfo, radius = 0.005, ...)  %>% 
      addCircles(lng = xpt$lon, lat=xpt$lat, radius = meters_per_mile * radius, color = "gray", fillOpacity = 0.06, fillColor = "gray", opacity = 0.7  )  %>% # overall circle
      addCircleMarkers(lng = xpt$lon, lat=xpt$lat, radius = 10, color = "red", opacity = 0.75) %>%  # in pixels for center of circle=point 
      addCircles(lng = mapinfo$lon, lat = mapinfo$lat, fillOpacity = 0.1, popup = popup_from_df(setDF(mapinfo)), radius = 10)
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
