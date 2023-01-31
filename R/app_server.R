#' EJAM app server
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # Note: how to avoid or still have a global.R file in the golem approach, 
  # https://github.com/ThinkR-open/golem/issues/6
  
  
  ## update ss_select_NAICS input options
  updateSelectizeInput(session, inputId = 'ss_select_naics', 
                       choices = EJAM::NAICS, server = TRUE)
  
  ## reactive: process uploaded latlon
  data_latlon <- reactive({
    ## wait for file to be uploaded
    req(input$ss_upload_latlon)
    
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_latlon$name)
    
    ## if acceptable file type, read in; if not, send warning text
    ext <- switch(ext,
      csv =  read.csv(input$ss_upload_latlon$datapath),
      xls = read.table(input$ss_upload_latlon$datapath),
      xlsx = read.table(input$ss_upload_latlon$datapath),
      shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
  })
  
  ## pull up modal with ECHO information on button click
  observeEvent(input$ss_search_echo, {
    showModal(
      modalDialog(title = "Use ECHO facility search tools to specify list of sites",
                  echo_message,
                  shiny::HTML(paste('<a href=\"', echo_url, '\", target=\"_blank\">', echo_url,  '</a>', sep = '')),
                  easyClose = TRUE)
    )
  })
  
  ## REMOVE later - print first few rows of uploaded latlon data
  output$upload_check <- renderTable({
    head(data_latlon())
  })
  
  ## output: leaflet map of uploaded points
  output$an_leaf_map <- leaflet::renderLeaflet({
    
    #req(input$ss_upload_latlon)
    
    ## function in global.R, will go in own script later
    ## once name is settled
    plot_facilities(data_latlon())
  })
  
  ## output: display number of uploaded sites
  output$an_map_text <- renderText({
    req(input$ss_upload_latlon)
    
    paste0(nrow(data_latlon()), ' points uploaded')
  })
  
  ## output: display barplot
  output$summ_display_bar <- renderPlot({
    
    ## placeholder code for barplot
    barplot(table(InsectSprays$spray))
  })
  
  ## output: display histogram
  output$summ_display_hist <- renderPlot({
    
    ## placeholder code for histogram
    hist(InsectSprays$count)
  })
}

