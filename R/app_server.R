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
}

