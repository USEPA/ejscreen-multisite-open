#' map_facilities
#' @description make a leaflet map of uploaded points
#' @param mypoints, data frame of uploaded points
#' @param rad, a size for drawing each circle (buffer search radius)
#' @param highlight, a logicial for whether to highlight overlapping points (defaults to FALSE)
#' @param clustered, a vector of T/F values for each point, indicating if they overlap with another 
#'
#' @return a leaflet map with circles, circleMarkers, and basic popup
#' @export
#'
map_facilities <- function(mypoints, rad = 3, highlight = FALSE, clustered) {#, map_units = 'miles'){
  
  ## map settings
  base_color      <- '#000080'
  cluster_color   <- 'red'
  highlight_color <- 'orange'   #  NOT USED YET
  circleweight <- 4
  
  ## convert units to miles for circle size
  # if (map_units == 'kilometers'){
  #   rad = rad * 0.62137119
  # }
  
  ## if checkbox to highlight clusters is checked
  if (highlight == TRUE) {
    ## compare latlons using is_clustered() reactive
    circle_color <- ifelse(clustered == TRUE, cluster_color, base_color)
  } else {
    circle_color <- base_color
  }
  # print(head(mypoints))
  
  if (length(mypoints) != 0) {
    #isolate({ # do not redraw entire map and zoom out and reset location viewed just because radius changed?
      
      #if(circle_type == 'circles'){
      mymap <- leaflet::leaflet(mypoints) %>% 
        addTiles()  %>%
        addCircles(
          #radius = input$radius * meters_per_mile,
          radius = rad * meters_per_mile,
          color = circle_color, fillColor = circle_color, 
          fill = TRUE, weight = circleweight,
          group = 'circles',
          popup = popup_from_any(mypoints)
          #popup = EJAMejscreenapi::popup_from_df(mypoints)
        ) %>% 
        addCircleMarkers(
          #radius = input$radius * meters_per_mile,
          radius = rad,
          color = circle_color, fillColor = circle_color, 
          fill = TRUE, weight = circleweight,
          clusterOptions = markerClusterOptions(),
          popup = popup_from_any(mypoints)
          ## possible way to use circleMarkers - need conversion of meters to pixels so they scale properly
          #meters_per_px <- 156543.03392 * cos(mean(m$x$limits$lat) * pi/180) / m$x$options
        ) %>% 
        groupOptions(group = 'markers', zoomLevels = 1:6) %>% 
        groupOptions(group = 'circles', zoomLevels = 6:20) %>% 
        leaflet.extras::addFullscreenControl()
      
      ## return map
      mymap
      
    #})
  } else {  # length(mypoints) == 0
    mymap <- leaflet() %>% 
      addTiles() %>% 
      setView(-110, 46, zoom = 3)
    mymap
  }
  ### Button to print map ####
  leaflet.extras2::addEasyprint(map = mymap, options = leaflet.extras2::easyprintOptions(exportOnly = TRUE, title = 'Save Map Snapshot'))
}
