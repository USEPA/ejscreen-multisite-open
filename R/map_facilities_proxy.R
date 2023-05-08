#' map_facilities_proxy
#' @description update a leaflet map within the EJAM shiny app with uploaded points.
#' @param mymap, leafletProxy map object to be added to
#' @param rad, a size for drawing each circle (buffer search radius)
#' @param highlight, a logicial for whether to highlight overlapping points (defaults to FALSE)
#' @param clustered, a vector of T/F values for each point, indicating if they overlap with another 
#' @param popup_vec, a vector of popup values to display when points are clicked. Length should match number of rows in the dataset.
#'
#' @return a leaflet map with circles, circleMarkers, and basic popup
#' @export
#'

map_facilities_proxy <- function(mymap, rad = 3, highlight = FALSE, clustered = FALSE, popup_vec = NULL){
  
  ## map settings
  base_color      <- '#000080'
  cluster_color   <- 'red'
  circleweight <- 4
  
  ## if checkbox to highlight clusters is checked
  if(highlight == TRUE){
    ## compare latlons using is_clustered() reactive
    circle_color <- ifelse(clustered == TRUE, cluster_color, base_color)
  } else {
    circle_color <- base_color
  }
  
  
  ## add to leafletProxy call from Shiny app
  mymap <- mymap %>%
    clearShapes() %>%
    clearMarkerClusters() %>%
    addCircles(
      radius = rad * meters_per_mile,
      color = circle_color, fillColor = circle_color, 
      fill = TRUE, weight = circleweight,
      group = 'circles',
      popup = popup_vec
    ) %>% 
    addCircleMarkers(
      radius = 0,
      color = circle_color, fillColor = circle_color, 
      fill = TRUE, weight = circleweight,
      clusterOptions = markerClusterOptions(),
      popup = popup_vec
    ) %>% 
    groupOptions(group = 'markers', zoomLevels = 1:6) %>% 
    groupOptions(group = 'circles', zoomLevels = 7:20) %>% 
    leaflet.extras::addFullscreenControl()
  
  ## return map
  mymap
  
}