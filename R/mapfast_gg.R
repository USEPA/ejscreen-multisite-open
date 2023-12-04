#' mapfast_gg - A very simple ggplot2 map of points in the USA
#'
#' @param mydf data.frame with columns named lat and lon 
#' @param dotsize optional, size of dot representing a point
#' @param ptcolor optional, color of dot 
#' @param xlab optional, text for x label
#' @param ylab optional, text for y label
#' @param ... optional, passed to [ggplot2::labs()]
#'
#' @return a ggplot() object
#' @export
#'
#' @examples \dontrun{
#'   mapfast_gg(EJAM::testpoints_10)
#'   
#'   pts <- read.table(textConnection(
#'   "lat lon 
#'   39.5624775 -119.7410994 
#'   42.38748056 -94.61803333"
#'   ),
#'   header = TRUE, 
#'   as.is = TRUE
#'   )
#'   mapfast_gg(pts)
#'   # str(pts) # lon, not long
#'   }
#'   
mapfast_gg <- function(mydf=data.frame(lat = 40, lon = -100)[0,], dotsize = 1, ptcolor = "black", xlab = "Longitude", ylab = "Latitude", ...) {
  plotout <- ggplot2::ggplot() +
    # The usa data.frame has cols long, lat, group, etc.
    ggplot2::geom_polygon(data = ggplot2::map_data("usa"), ggplot2::aes(x = long, y = lat, group = group), fill = "gray", alpha = 0.75) + 
    ggplot2::geom_point(  data = mydf, ggplot2::aes(lon, lat), color = ptcolor, size = dotsize) + 
    ggplot2::labs(x = xlab, y = ylab, ...)
  return(plotout)
}
############################ #
