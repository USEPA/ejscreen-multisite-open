#' convert table of lat,lon points/sites into sf:: shapefile
#' Creates a simple feature (sf) dataframe from points
#' @param sitepoints a data.table or data.frame with columns called lat,lon
#' @import sf
#' @return A shapefile via [sf::st_as_sf()]
#' @seealso [get_blockpoints_in_shape()] [get_shapefile_from_sitepoints()] [get_shape_buffered_from_shapefile_points()]
#' @export
#'
get_shapefile_from_sitepoints <- function(sitepoints) {
  #data.table::setDF(sitepoints)
  shpcoord<-sf::st_as_sf(sitepoints, coords= c('lon', 'lat'), crs = 4269)
  return(shpcoord) # but want 4269
}
 
