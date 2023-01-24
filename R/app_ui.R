#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources, see end of this source file.
    golem_add_external_resources(),
    
    ## begin app UI
    fluidPage(
      
      ## add title for app and browser tab
      titlePanel(title = "EJAM (Environmental Justice Analysis Multi-site) Tool",
                 windowTitle = "EJAM (Environmental Justice Analysis Multi-site) Tool"
      ),
      
      ## create tabsetPanel with tabs for different sections
      tabsetPanel(
        id = 'all_tabs',
        
        ## site selection tab
        tabPanel(title = 'Site Selection',
                 
           ## input: Select NAICS from list
           selectInput(
             inputId = "ss_select_naics",
             label = htmltools::h6("Select industry of interest"),
             # choose from named numeric vector on server-side
             ## number is NAICS like 31182, names are like "31182 - Cookie, Cracker, and Pasta Manufacturing" 
             choices = NULL, 
             selected = NULL,
             width = 400,
             multiple = TRUE
           )         
        ),
        
        ## analysis settings tab
        tabPanel(title = 'Analysis Settings'),
        
        ## buffering tools tab
        tabPanel(title = 'Buffering Tools'),
        
        ## summaries tab
        tabPanel(title = 'Summaries'),
        
        ## report generation tab
        tabPanel(title = 'Generate Report')
      )
      
    ) ## end fluidPage
  )
} ########################################################################### #

#' Add external Resources to App (from golem package code)
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {   # (adds external Resources to App) ####
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "EJAM"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    
  )
}
