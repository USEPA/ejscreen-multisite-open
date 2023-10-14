library(shiny)
library(dplyr)

require(rhandsontable)

### from here: https://stackoverflow.com/questions/75967173/how-do-i-make-a-simple-user-editable-table-in-an-r-shiny-app-using-modular-desig 
# but also see for a more complete package that helps provide and excel-like interface:  https://dillonhammill.github.io/DataEditR/

######################################################################## # 
# UI of ediTable module ####

MODULE_UI_latlontypedin <- function(id, ...) {
  ns <- NS(id)
  tagList(
    # Display the table
    rhandsontable::rHandsontableOutput(outputId = ns("typedin_latlon"), ...),
    
  )
}
######################################################################## # 
# Server of ediTable module ####

MODULE_SERVER_latlontypedin <- function(id,
                                      reactdat,
                                      allowColumnEdit = FALSE,
                                      allowRowEdit    = TRUE,
                                      manualRowMove   = TRUE,
                                      ...) {
  moduleServer(id,
               function(input, output, session) {
                 
                 output$typedin_latlon <- rhandsontable::renderRHandsontable({
                   
                   tmp <- isolate(reactdat()) # isolate this or it will cause an infinite loop
                   # Necessary to avoid the issue described [here](https://github.com/jrowen/rhandsontable/issues/166)
                   rownames(tmp) <- NULL
                   
                   rhandsontable::rhandsontable(
                     tmp,
                     allowRowEdit    = allowRowEdit,
                     allowColumnEdit = allowColumnEdit,
                     manualRowMove = manualRowMove,
                     ...
                   )
                   
                 })
                 
                 #Update reactive value for this user-manipulated data to pass back to main environment
                 observeEvent(input$typedin_latlon, {
                   tmp <- rhandsontable::hot_to_r(input$typedin_latlon)
                   reactdat(tmp)
                 })
               })
}

######################################################################## # 
# in UI for the main application 

ui <- fluidPage(
  # Application title
  titlePanel("Demo of editable module, a table widget"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Motivation:"),
      p("Sometimes you just want to allow users to edit a table and then save that input data."),
      p("This is basically a shallow, more intuitive wrapper for rhandsontable.")
    ),
    
    mainPanel(
      ########################## # 
      h3("Enter latitude(s) and longitude(s) for the point(s) to analyze"),
      p("Click or doubleclick on a cell to edit."),
      p("Right-click to undo, add or remove rows, etc."),
      
      # Display table ready for data entry ####
      MODULE_UI_latlontypedin(id = "pts_entry_table1"),  # this shows the data entry table on this webpage, not in a popup modal dialog box
      # actionButton('latlontypedin_submit_button', label='Done entering points', class = 'usa-button usa-button--outline'),
      
      ########################## # 
      # to display the edited outputs ####
      h3("Outputting the edited data for lat lon table"),
      shiny::tableOutput("data1"),  # this shows the resulting output, the edited table, live on this webpage
      
      # h1("Table 2: independently reactive module"),
      # MODULE_UI_latlontypedin(id = "tab2"),   
      # h3("Outputting the edited data  "),
      # tableOutput("data2"),
      
      br()
      
    )
  )
)
######################################################################## # 
# Define server logic for the main application
server <- function(input, output) {
  
  testpoints_template <- EJAMejscreenapi::testpoints_5[1:2, ]
  reactive_data1 <-  reactiveVal(testpoints_template)
  MODULE_SERVER_latlontypedin(id = "pts_entry_table1", reactdat = reactive_data1)
  
  observe({
    tmp <- reactive_data1()  # when the typed data changes, this would update the output table, if you need to display one.
    output$data1 <- tmp %>% renderTable()
  })
  
  # reactive_data2 <-  reactiveVal(testpoints_template)
  # MODULE_SERVER_latlontypedin(id = "tab2",  rd = reactive_data2)
  
  # observe({
  #   tmp2 <- reactive_data2()
  #   output$data2 <- tmp2 %>% renderTable()
  # })
  
}
######################################################################## # 
# Run the application
shinyApp(ui = ui, server = server)
