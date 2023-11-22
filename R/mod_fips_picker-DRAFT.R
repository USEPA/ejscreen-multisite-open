 
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
   
  # SOME CITIES/PLACES OVERLAP WITH 2+ COUNTIES 
  
  
  ############# # 
  ############# # 
  ############# # 
  ############# # 
  
  

  
  countiesall <- unique(data.frame(
    countyfips = substr(blockgroupstats$bgfips,1,5), 
    countyname = blockgroupstats$countyname, 
    ST = blockgroupstats$ST))
   
  regioninfo <- data.frame(
    REGION = 1:10, 
    Statecount = as.vector(table(EJAM::stateinfo2$REGION)), 
    States = sapply(1:10, function(n) paste0( EJAM::stateinfo2$ST[EJAM::stateinfo2$REGION %in% n], collapse = ", "))
  )
  
  
  
  
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
      instates = countiesall$ST %in% input$statepicker
      mychoices <-  countiesall$countyfips[instates]
      names(mychoices) <- paste(countiesall$countyname[instates], countiesall$ST[instates], sep = ",")
      updateSelectizeInput(session, inputId = "countypicker", server = TRUE, 
                           choices = mychoices)
                           # choices = unique(paste(blockgroupstats$countyname, blockgroupstats$ST, sep = ","))[blockgroupstats$ST %in% input$statepicker])
                            
    })
    observe({
      updateSelectizeInput(session, inputId = "placespicker", server = TRUE, 
                           
                           # ********   FIX
                           
                           choices = censusplaces$PLACE[tolower(paste(censusplaces$COUNTY, censusplaces$STATE, sep = ",")) %in% tolower(input$countypicker)],
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
            selected = unique(censusplaces$COUNTY[censusplaces$STATE %in% input$statepicker])  # ********   FIX
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
    
    # selectedrows <- reactiveVal()
    displaytable <- reactiveVal() # to show here
    bgstable <- reactiveVal()  # all the blockgroup fips to analyze and siteid = units to analyze
    
    observe({
      
      # c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places")
      
      #################### #
      if (input$type2analyze == "EPA Regions") {
        displaytable(
          regioninfo[regioninfo$REGION %in% input$regionpicker2, ]
        )
        # bgstable(
        #   # should use cached results of ejamit(radius = 1,3,5,6.2  eg) for 10 regions, not redo
        # )
      }
      
      #################### #
      if (input$type2analyze == "States") {
        if (input$allstatesinregion) {
          shiny::updateSelectizeInput(session, inputId = "statepicker",
                                      selected = stateinfo2$ST[stateinfo2$REGION %in% input$regionpicker2])
        }
        varnames = c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr" ,"is.usa" ,"is.state" ,"is.contiguous.us", "is.island.areas", "area.sqmi",  "landarea.sqmi", "waterarea.sqmi" )
        displaytable(
          stateinfo2[stateinfo2$ST %in% input$statepicker, varnames]
        )
        # bgstable(
          # EJAM::fips_bg_from_anyfips(fips = displaytable()$FIPS.ST)
        # )
      }
      
      #################### #
      if (input$type2analyze == "Counties") {
        if (input$allcountiesinstate) {
          shiny::updateSelectizeInput(session, inputId = "countypicker",
                                      selected = countiesall$countyfips[  countiesall$ST  %in% input$statepicker] )
        }

          # unique(censusplaces[censusplaces$COUNTY %in% input$countypicker, 1:5])
           
    displaytable(
      countiesall[countiesall$countyfips  %in% input$countypicker]
        )
        # bgstable(
        # counties_as_sites(displaytable()$CO_FIPS)
        # )
      }
      
      #################### #
      if (input$type2analyze == "Cities or Places") {
        if (input$allplacesincounty) {
          shiny::updateSelectizeInput(session, inputId = "placespicker", 
                                      selected = censusplaces$PLACE[censusplaces$STATE %in% input$statepicker])
        }
        displaytable(
          censusplaces[censusplaces$PLACE %in% input$placespicker, ]
        )
        # bgstable(
        #   # ??? 
        #   # EJAM::fips_bg_from_anyfips(fips = NA) # NOT SURE PLACES FIPS NEATLY FALL INTO BLOCKGROUPS
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

