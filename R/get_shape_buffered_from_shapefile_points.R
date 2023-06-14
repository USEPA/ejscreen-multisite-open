#' add buffer around shape (points, here)
#' @details Just a wrapper for [sf::st_buffer()]
#'
#' @param shapefile_points spatial object like areas at high risk or areas with facilities to be analyzed
#' @param radius.miles width of buffer to add to shapefile_points 
#'   (in case dist is a units object, it should be 
#'   convertible to arc_degree if x has geographic coordinates, 
#'   and to st_crs(x)$units otherwise)
#' @param ... passed to st_buffer()
#' @import sf
#' @seealso [get_blockpoints_in_shape()] [get_shapefile_from_sitepoints()] [get_shape_buffered_from_shapefile_points]
#' @export
#' 
get_shape_buffered_from_shapefile_points <- function(shapefile_points, radius.miles, ...) {
  
return(sf::st_buffer(shapefile_points %>%  sf::st_transform(4269), # was "ESRI:102005" but want 4269
              dist = units::set_units(radius.miles, "mi"), ...))

}

# @export
# @inherit get_shape_buffered_from_shapefile_points
#get_circles_from_spatialpoints <- get_shape_buffered_from_shapefile_points
