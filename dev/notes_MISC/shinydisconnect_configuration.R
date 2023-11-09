library(shiny)
library(shinydisconnect)
ui <- fluidPage(
  disconnectMessage(
    text = "Sorry ... This app has stopped because of an error or a timeout. The app is still being tested and debugged. Also, if the app is left open with no interaction, then once a time limit is reached the server will disconnect to save resources.",
    refresh = "Click to Restart the App",
    background = "#FFFFFF",
    colour = "#444444",
    refreshColour = "darkgreen", # "#337AB7",
    overlayColour = "#000000",
    overlayOpacity = 0.5,
    width = 450,
    top = 50,
    size = 20,
    css = ""
  ),
  br(),
  actionButton("disconnect", "Disconnect the app")
)
server <- function(input, output, session) {
  observeEvent(input$disconnect, {
    session$close()
  })
}
shinyApp(ui, server)
