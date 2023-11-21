 
if (1 == 0) {
  
  
  
  # exploring an app module 
  # to select census places for analysis
  # like selected cities or all counties in a state
  
  
  
  # library(shiny)
  # library(data.table)
  # 
  
  
  
  warning( " work in progress -- not working right yet, including reset button and county list")

  
    
  ############# # 
  ############# # 
  ############# # 
  ############# # 
  ############# # 
  
  
  warning("SOMETHING IS WRONG WITH THE COUNTY AND CO_FIPS COLS OF THE censusplaces dataset? ")
  
  
  # unique(censusplaces[censusplaces$STATE == "DE", 1:5])
  # EPA_REGION STATE ST_FIPS                         COUNTY CO_FIPS
  # 4222          3    DE      10              New Castle County   10014
  # 4223          3    DE      10              New Castle County   10015
  # 4224          3    DE      10              New Castle County   10016
  # 4225          3    DE      10              New Castle County   10041
  # 4226          3    DE      10              New Castle County   10046
  # 4227          3    DE      10                  Sussex County   10056
  # 4228          3    DE      10                  Sussex County   10058
  # 4229          3    DE      10                  Sussex County   10067
  
  
  ############# # 
  ############# # 
  ############# # 
  ############# # 
  
  

  
  
  
  
  
  
  
  
  
  ######################################## # 
  # start UI ####
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    # Application title
    titlePanel("Cities and other Census Places"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        selectInput("type2analyze", multiple = FALSE,
                    label = "What kinds of areas do you want to compare?", 
                    choices = c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places"), 
                    selected = "Select Regions, States, Counties, or Cities"),
        
        
        checkboxGroupInput("regionpicker2", label = "EPA Region", 
                           choices = sort(unique(censusplaces$EPA_REGION)), 
                           selected = sort(unique(censusplaces$EPA_REGION)), 
                           inline = TRUE),
        
        checkboxInput("allstatesinregion", "Compare all States in EPA Region(s)?", value = FALSE),
        
        selectizeInput("statepicker", label = "Select State", 
                       choices = NULL, # unique(censusplaces$STATE), 
                       multiple = T, 
                       selected = ""),
        
        checkboxInput("allcountiesinstate", "Compare all Counties in State(s)?", value = FALSE),
        
        selectizeInput("countypicker", label = "Select Counties", 
                       choices = NULL, # unique(censusplaces$COUNTY),
                       multiple = T,
                       selected = ""),
        
        checkboxInput("allplacesincounty", "Compare all Cites/Places in County?", value = FALSE),
        
        selectizeInput("placespicker", 
                       label = "Select Places", 
                       choices = NULL, # censusplaces$PLACE,
                       multiple = T,
                       selected = ""),
        
        actionButton("reset", label = "Clear selections")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("placestable")
      )
    )
  )
  # end UI ####
  ######################################################### # 
  # start server ####
  
  server <- function(input, output, session) {
    
    regioninfo = data.frame(
      REGION = 1:10, 
      Statecount = as.vector(table(EJAM::stateinfo2$REGION)), 
      States = sapply(1:10, function(n) paste0( EJAM::stateinfo2$ST[EJAM::stateinfo2$REGION %in% n], collapse = ", "))
    )
    
    shinyjs::hide("regionpicker2")
    shinyjs::hide("allstatesinregion")
    shinyjs::hide("statepicker")
    shinyjs::hide("allcountiesinstate")
    shinyjs::hide("countypicker")
    shinyjs::hide("allplacesincounty")
    shinyjs::hide("placespicker")
    
    observe({
      
      if (input$type2analyze ==  "Select Regions, States, Counties, or Cities") {
        shinyjs::hide("regionpicker2")
        shinyjs::hide("allstatesinregion")
        shinyjs::hide("statepicker")
        shinyjs::hide("allcountiesinstate")
        shinyjs::hide("countypicker")
        shinyjs::hide("allplacesincounty")
        shinyjs::hide("placespicker")
      }
      if (input$type2analyze == "EPA Regions") {
        shinyjs::show("regionpicker2")
        shinyjs::hide("allstatesinregion")
        shinyjs::hide("statepicker")
        shinyjs::hide("allcountiesinstate")
        shinyjs::hide("countypicker")
        shinyjs::hide("allplacesincounty")
        shinyjs::hide("placespicker")
      }
      if (input$type2analyze == "States") {
        shinyjs::show("regionpicker2")
        shinyjs::show("allstatesinregion")
        shinyjs::show("statepicker")
        shinyjs::hide("allcountiesinstate")
        shinyjs::hide("countypicker")
        shinyjs::hide("allplacesincounty")
        shinyjs::hide("placespicker")
      }
      if (input$type2analyze == "Counties") {
        shinyjs::show("regionpicker2")
        shinyjs::hide("allstatesinregion")
        shinyjs::show("statepicker")
        shinyjs::show("allcountiesinstate")
        shinyjs::show("countypicker")
        shinyjs::hide("allplacesincounty")
        shinyjs::hide("placespicker")
      }
      if (input$type2analyze == "Cities or Places") {
        shinyjs::show("regionpicker2")
        shinyjs::hide("allstatesinregion")
        shinyjs::show("statepicker")
        shinyjs::hide("allcountiesinstate")
        shinyjs::show("countypicker")
        shinyjs::show("allplacesincounty")
        shinyjs::show("placespicker")
      }
    })
    
    # RESET BUTTON
    observe({
      updateCheckboxGroupInput(inputId = "regionpicker2", selected = sort(unique(censusplaces$EPA_REGION)))
      updateSelectizeInput(session, inputId = "statepicker",  server = TRUE, selected = "")
      updateSelectizeInput(session, inputId = "countypicker", server = TRUE, selected = "")
      updateSelectizeInput(session, inputId = "placespicker", server = TRUE, selected = "")
    }) |>
      bindEvent(input$reset)
    
    observe({
      updateSelectizeInput(session, inputId = "statepicker", server = TRUE, 
                           choices = unique(censusplaces$STATE[censusplaces$EPA_REGION %in% input$regionpicker2]))
    })
    observe({
      updateSelectizeInput(session, inputId = "countypicker", server = TRUE, 
                           choices = unique(censusplaces$COUNTY[censusplaces$STATE %in% input$statepicker]))
    })
    observe({
      updateSelectizeInput(session, inputId = "placespicker", server = TRUE, 
                           choices = censusplaces$PLACE[censusplaces$COUNTY %in% input$countypicker],
                           selected = character(0))
    })
    
    # observe({
    #   req(input$statepicker)
    #   if (length(input$statepicker) == 1 & input$statepicker != "") {
    #     shinyjs::showElement("allcountiesinstate")
    #   } else {
    #     shinyjs::hideElement("allcountiesinstate")
    #   }
    #   # does the value remain T/F while hidden, and does that create problems in changing selections while it is hidden?
    # })
    
    # observe({
    #   req(input$countypicker)
    #   if (length(input$countypicker) == 1 & input$countypicker != "") {
    #     shinyjs::showElement("allplacesincounty")} else {shinyjs::hideElement("allplacesincounty")
    #     }
    # })
    
    # FILTERS THAT LIMIT OPTIONS BASED ON SELECTIONS AT LARGER UNIT SIZE ####
    # LIKE TO SHOW ONLY COUNTIES IN THE SELECTED STATES
    observe({
      if (input$allcountiesinstate != "" ) {
        isolate(
          updateSelectizeInput(
            session = session, 
            inputId = "countypicker", 
            selected = unique(censusplaces$COUNTY[censusplaces$STATE %in% input$statepicker])
          ))
        shinyjs::hideElement("placespicker")
      } else {
        if (input$type2analyze == "Cities or Places") {
          # i think we want to remove all picks if this checkbox changes value to FALSE ?
          updateSelectizeInput(session = session, 
                               inputId = "countypicker", 
                               selected = character(0))
          shinyjs::showElement("placespicker")
        }
      }
    })
    ######################################################### # 
    # FIGURE OUT WHICH CENSUS UNITS ARE SELECTED  ####
    #  A SET OF FIPS OF ONE TYPE:  REGIONS, STATES, COUNTIES, OR PLACES/CITIES
    
    selectedrows <- reactiveVal()
    displaytable <- reactiveVal() # to show here
    bgstable <- reactiveVal()  # all the blockgroup fips to analyze and siteid = units to analyze
    
    observe({
      
      # c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places")
      
      if (input$type2analyze == "EPA Regions") {
        displaytable(
          regioninfo[regioninfo$REGION %in% input$regionpicker2, ]
        )
      }
      
      if (input$type2analyze == "States") {
        if (input$allstatesinregion) {
          shiny::updateSelectizeInput(session, inputId = "statepicker",
                                      selected = stateinfo2$ST[stateinfo2$REGION %in% input$regionpicker2])
        }
        varnames = c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr" ,"is.usa" ,"is.state" ,"is.contiguous.us", "is.island.areas", "area.sqmi",  "landarea.sqmi", "waterarea.sqmi" )
        displaytable(
          stateinfo2[stateinfo2$ST %in% input$statepicker, varnames]
        )
      }
      
      if (input$type2analyze == "Counties") {
        if (input$allcountiesinstate) {
          shiny::updateSelectizeInput(session, inputId = "countypicker",
                                      selected = unique(censusplaces$COUNTY[censusplaces$STATE %in% input$statepicker]))
        }
        displaytable(
          unique(censusplaces[censusplaces$COUNTY %in% input$countypicker, 1:5])
          # bg_from_county(fips = censusplaces$CO_FIPS[censusplaces$COUNTY %in% input$countypicker])
          )
      }
      
      
      if (input$type2analyze == "Cities or Places") {
        if (input$allplacesincounty) {
          shiny::updateSelectizeInput(session, inputId = "placespicker", 
                                      selected = censusplaces$PLACE[censusplaces$COUNTY %in% input$countypicker])
        }
        displaytable(
          censusplaces[censusplaces$PLACE %in% input$placespicker, ]
        )
        # bgstable(
        #   # ??? 
        #   # EJAM::fipsbg_from_anyfips(fips = NA) # NOT SURE PLACES FIPS NEATLY FALL INTO BLOCKGROUPS
        # )
      }
      
      if (input$type2analyze == 0) {
        displaytable(NULL)
        
      }
    })
    
    # THE TABLE DISPLAYED ############# 
    
    output$placestable <- renderTable({
      displaytable()
    })
    
    # return( bgstable() )
    
  }
  
  ######################################################### # 
  
  
  shinyApp(ui = ui, server = server)
  
  
}

