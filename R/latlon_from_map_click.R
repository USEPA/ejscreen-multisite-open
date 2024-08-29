
# example of basics of getting lat/lon from a map click in shiny

# library(shiny)
# library(leaflet)

latlon_from_map_click <- function() {
  
ui <- shiny::fluidPage(
  leaflet::leafletOutput("mymap")
)

server <- function(input, output, session) {
  output$mymap <- leaflet::renderLeaflet({
    # Use leaflet() here, and only include aspects of map that won't need to change dynamically 
    # (at least, not unless the entire map is being torn down and recreated).
    leaflet::leaflet() %>% 
      leaflet::setView(-99, 40, zoom = 4)  %>% 
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(noWrap = TRUE)
      )
  })
  
  shiny::observe({
    # Each kind of Incremental change to the map should be performed in its own observer.
    leaflet::leafletProxy("mymap") %>% clearPopups() 
    event <- input$mymap_click
    if (is.null(event)) {return()}
    isolate({
      leaflet::leafletProxy("mymap") %>% 
        leaflet::addMarkers(lng = input$mymap_click$lng, lat = input$mymap_click$lat) %>%
        leaflet::addPopups(lng = input$mymap_click$lng, lat = input$mymap_click$lat, 
                           popup = paste0("lat,lon: ", input$mymap_click$lat, ", ", input$mymap_click$lng))
    })
  })
}


shiny::shinyApp(ui, server)


}
