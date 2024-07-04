############################################################### #
## example of reading data from a pins board into a shiny app
## see more at https://pins.rstudio.com/articles/posit-connect.html
if (FALSE) {
  library(shiny)
  ui <- fluidPage(
    tableOutput("table")
  )
  
  server <- function(input, output, session) {
    #board <- board_local()
    data <- pin_reactive_read(board, "blockgroupstats_arrow", interval = 1000)
    output$table <- renderTable(data()[1:100,1:10])
  }
  shinyApp(ui, server)
}
