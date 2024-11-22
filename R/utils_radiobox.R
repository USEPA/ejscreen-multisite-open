
## (This function can also be defined as an RStudio addin with a keyboard shortcut in case that is useful)


#' DRAFT - shiny gadget for interactive RStudio use - lets you pick 1 option via radio buttons in popup dialog
#'
#' @param choiceNames vector of options displayed
#' @param choiceValues vector of corresponding values as returned by the function
#' @param label  Appears just above the list of choices
#' @param title Appears at top of dialog box and between cancel and done
#' @param height of box in pixels
#' @param width of box in pixels
#' @details uses shiny::runGadget()
#' 
#'   *** Note: Cannot really use within nontrivial scripts or functions as currently written
#'   because the stopApp() seems to interrupt other processes and cause problems -
#'   and seems related to a quirk seen if a script or function calls radiobox() twice - 
#'   it will work the first time but show a blank popup window the 2d time...
#'   e.g., if you  try to do this:
#'   radius1 <- radiobox()
#'   radius2 <- radiobox()
#'   May all be related to this: https://github.com/rstudio/rstudio/issues/13394
#'   
#' @return one of the choiceValues (if not cancelled/ error)
#'
#' @examples
#' \dontrun{
#' radius <- EJAM:::radiobox(c("Far (3 miles)", "Medium (2 miles)", "Near (1 mile)"), c(3,2,1),
#'   label = "Radius")
#' cat("The radius will be", radius, "miles. \n")
#' }
#' 
#' @keywords internal
#'
radiobox <- function(choiceNames  = c("Points", "Shapes", "FIPS"), # what is shown
                     choiceValues = c("latlon", "shp", "fips"),    # what is returned
                     label = "Choose one:",  #
                     title = "", # 
                     height = 250, width = 100) {
  
  if (!missing(choiceNames) & missing(choiceValues)) {
    # this way, the user can specify only the 1st parameter, like  x = radiobox(1:3)
    choiceValues <- choiceNames
  }
  ######################################## #
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title),
    miniUI::miniContentPanel(
      shiny::radioButtons('radiobuttons', 
                          label = label, 
                          choiceNames  = choiceNames, 
                          choiceValues = choiceValues) 
    )
  )
  ######################################## #
  
  server <- function(input, output, session) {
    
    shiny::observeEvent(input$done, {
      value_chosen <- input$radiobuttons
      # return(value_chosen) # this would not stop the app
      shiny::stopApp(returnValue = value_chosen)
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp() # returns NULL upon cancel button press, and no error
      # shiny::stopApp(returnValue = NULL) # returns NULL upon cancel button press, and no error
      # but clicking the x to close the window returns nothing at all, and no error code.
    })
  }
  ########################################## #
  
  dviewer <- shiny::dialogViewer(dialogName = title, height = height, width = width)
  shiny::runGadget(app = ui, server = server,
                             stopOnCancel = FALSE, # To handle input$cancel via our own observeEvent 
                             viewer = dviewer
  )
}
