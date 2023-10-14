#' get_shape_from_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_get_shape_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    ## upload shapefile ####
    shiny::fileInput(inputId = ns('shapefile'),
                     placeholder = 'test_shapefile.shp', multiple = FALSE,
                     # accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain"),
                     label = 'Upload shapefile of areas to analyze',
                     # add hover tips here maybe, or even a button to get examples of valid formats and details on that.
                     )
  )
}
    
#' get_shape_from_upload Server Functions
#'
#' @noRd 
mod_get_shape_upload_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    myshape <- shiny::reactive({
      # THIS GETS UPDATED WHEN THERE IS A CHANGE IN input$shapefile
      ## if not uploaded yet, have default example?? ####
      if (is.null(input$shapefile)) {
        shapefile_contents <- NULL # default_shapefile_shown_at_startup  # would be defined in global 
      } else {
        shapefile_contents <- sf::st_read(input$shapefile$datapath)
      }
    shapefile_contents
  })
})
}
    
## in the UI
# mod_get_shape_upload_ui("get_shape_from_upload_1")
    
## in the server
# mod_get_shape_upload_server("get_shape_from_upload_1")
