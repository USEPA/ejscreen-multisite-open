#' add buffer around shape
#' @details Just a wrapper for [sf::st_buffer()]
#'
#' @param shape spatial object like areas at high risk or areas with facilities to be analyzed
#' @param radius width of buffer to add to shape. 
#'   (in case dist is a units object, it should be 
#'   convertible to arc_degree if x has geographic coordinates, 
#'   and to st_crs(x)$units otherwise)
#' @param ... passed to st_buffer()
#'
#' @export
#' 
get_shape_buffered <- function(shape, radius, ...) {
  buff <- sf::st_buffer(shape, dist = radius, ...)
  return(buff)
}
