
# Draft shiny module or app to help select census places for analysis
# like selected cities, or all counties in a state, or all states in EPA Region

if (1 == 0) {
  
  #                  to try it out
  # RUN THIS MINI APP ####
  runApp(list(ui = ui_placespicker, server = server_placespicker),
         test.mode = TRUE, quiet = TRUE)
  
}
######################################################### # 

# server_placespicker ####

server_placespicker <- function(input, output, session) {
  
  # library(shiny)
  # library(data.table)
  
  warning( " work in progress -- not working right yet, including reset button and county list")
  
  ################# #   
  # Tables created, of places, counties, states, regions info ####
  
  statetable <- stateinfo2[stateinfo2$ST != "US", c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr", 
                                                    "is.state", "is.contiguous.us", "is.island.areas")] # etc. etc.
  
  regiontable <- data.frame(
    REGION = 1:10,
    Statecount = as.vector(table(statetable$REGION)), 
    States = sapply(1:10, function(n) paste0(statetable$ST[statetable$REGION %in% n], collapse = ", "))
  )
  
  placetable = data.table(censusplaces)[, .(eparegion, ST, countyname, placename, fips)]
  placetable$countyname_ST <- paste0(placetable$countyname, ", ", placetable$ST)
  
  countytable <- unique(placetable[, .(eparegion, ST, countyname_ST)])
  countytable$countyfips <- fips_counties_from_countyname(countytable$countyname_ST)
  
  # *** Note placetable (from censusplaces) and blockgroupstats have slightly 
  # different county name lists,
  # and also lower case "city" is "City" in censusplaces.
  
  # countytable <- unique(data.frame(
  #   stfips = substr(blockgroupstats$bgfips,1,2),
  #   ST = blockgroupstats$ST,
  #   countyfips = substr(blockgroupstats$bgfips,1,5), 
  #   countyname = gsub(" city$", " City", blockgroupstats$countyname)
  # ))
  # countytable$countyname_ST = paste0( countytable$countyname, ", ",  countytable$ST)
  
  #  countytable$countyname_ST
  
  ## Note SOME CITIES/PLACES OVERLAP WITH 2+ COUNTIES !! ####
  
  ################# #   
  
  # Show/Hide pickers for different levels ####
  
  ## default starting view:
  updateCheckboxGroupInput(inputId = "regionpicker2", choices = sort(regiontable$REGION), selected = sort(regiontable$REGION))
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
      # shinyjs::show("allplacesincounty")
      shinyjs::show("placespicker")
    }
  })
  
  # "Reset" button ####
  observe({
    updateSelectizeInput(session, inputId = "placespicker", server = TRUE, selected = "")
    updateSelectizeInput(session, inputId = "countypicker", server = TRUE, selected = "")
    updateSelectizeInput(session, inputId = "statepicker",  server = TRUE, selected = "")
    updateCheckboxGroupInput(inputId = "regionpicker2", selected = sort(regiontable$REGION))
  }) |>
    bindEvent(input$reset)
  
  # UPDATE OPTIONS AND PRIOR SELECTIONS IF SCOPE CHANGES #### 
  
  # WILL THESE OBSERVERS OF LOCATION PICKS INTERFERE WITH 
  # THE OBSERVERS OF LEVEL SPECIFIED ? OR THE "ALL" BOX ?
  
  
  
  ## update STATE options and prior selections based on region(s) picked ####
  observe({
    req(input$regionpicker2)
    print("here")
    inregions <- statetable$REGION %in% input$regionpicker2 # placetable$eparegion %in% input$regionpicker2
    mychoices <- statetable$ST[inregions]  # unique(placetable$ST[inregions])
    names(mychoices) <- statetable$statename[inregions] # fips2statename(fips_state_from_state_abbrev(mychoices))
    
    updateSelectizeInput(session, inputId = "statepicker", server = TRUE, 
                         choices = mychoices
    )
    
    isolate({hadbeenselected <- input$statepicker})
    if (length(hadbeenselected) > 0) {
      updateSelectizeInput(session, inputId = "statepicker", server = TRUE, 
                           # only show prior picks that are in new set of regions, since the regions picks just changed
                           selected = hadbeenselected[hadbeenselected %in% mychoices] 
      )
    }
  })
  
  ## update COUNTY options and prior selections based on state(s) picked ####
  observe({
    req(input$statepicker)
    instates  <- countytable$ST %in% input$statepicker
    mychoices <- countytable$countyfips[instates] 
    names(mychoices) <- countytable$countyname_ST[instates]
    
    updateSelectizeInput(session, inputId = "countypicker", server = TRUE, 
                         choices = mychoices
    )
    
    isolate({hadbeenselected <- input$countypicker})
    if (length(hadbeenselected) > 0) {
      updateSelectizeInput(session, inputId = "countypicker", server = TRUE, 
                           # only show prior picks that are in new set of states, since the state picks just changed
                           selected = hadbeenselected[hadbeenselected %in% mychoices]
      )
      
    }
  })
  
  ## update CITY/PLACE options and prior selections based on county(ies) picked ####
  observe({
    req(input$countypicker)
    if (length(input$countypicker) == 0)  {
      mychoices <- ""
    } else {
      # changed county pick(s), so update places in county(ies)
      countyname_ST_picked <- fips2countyname(input$countypicker, includestate = "ST")
      incounties <- placetable$countyname_ST %in% countyname_ST_picked
      mychoices <- placetable$fips[incounties]
      names(mychoices) <- paste0(placetable$placename[incounties], ", ", placetable$countyname_ST[incounties], " (",mychoices,")")
    }
    updateSelectizeInput(session, inputId = "placespicker", server = TRUE, # much faster if server = TRUE for such a long list
                         choices = mychoices
    )
    
    isolate({hadbeenselected <- input$placespicker})
    if (length(hadbeenselected) > 0) {
      updateSelectizeInput(session, inputId = "placespicker", server = TRUE,
                           selected = hadbeenselected[hadbeenselected %in% mychoices]
                           # only show prior picks that are in new set of states, since the state picks just changed
      )
    }
  })
  ################################### # 
  
  ######################################################### # 
  
  # SHOW TABLE OF DATA when level changes ####
  # ... & react to "ALL" Button ####
  
  ############################## # 
  # c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places")
  
  ## REGIONS / All Regions ################### 
  observe({
    if (input$type2analyze == "EPA Regions") {
      displaytable(
        # show regions selected
        regiontable[regiontable$REGION %in% input$regionpicker2, ]
      )
      # bgstable(
      #   # should use cached results of ejamit(radius = 1,3,5,6.2  eg) for 10 regions, not redo
      # )
    }
  })
  
  ## STATES / ALL STATES (in given Region(s)) ################### 
  observe({
    if (input$type2analyze == "States") {
      
      if (input$allstatesinregion) {
        inregion <- statetable$REGION %in% input$regionpicker2
        shiny::updateSelectizeInput(session, inputId = "statepicker",
                                    choices = statetable$ST[inregion],
                                    selected = statetable$ST[inregion]
        )
      } else {
        # ? do we want it to reset all when the user UNCHECKS "all in" ?
      }
      
      varnames = c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr" , "is.state" ,"is.contiguous.us", "is.island.areas")
      displaytable(
        # show states selected
        statetable[statetable$ST %in% input$statepicker, varnames]
      )
      # bgstable(
      # EJAM::fips_bg_from_anyfips(fips = displaytable()$FIPS.ST)
      # )
    }
  })
  
  ## COUNTIES / All Counties (in given State(s)) ###################
  observe({ 
    if (input$type2analyze == "Counties") {
      
      if (input$allcountiesinstate) {
        instate <- countytable$ST %in% input$statepicker
        mychoices <- countytable$countyfips[instate]
        names(mychoices) <- countytable$countyname_ST[instate]
        shiny::updateSelectizeInput(session, inputId = "countypicker",
                                    choices = mychoices,
                                    selected = mychoices
        )
      } else {
        # ? do we want it to reset all when the user UNCHECKS "all in" ?
      }
      
      displaytable(
        countytable[countytable$countyfips %in% input$countypicker, ]
      )
      # bgstable(
      # counties_as_sites(displaytable()$CO_FIPS)
      # )
    }
  })
  
  ## All Cities/Places (in given counties) ###################
  observe({ 
    if (input$type2analyze == "Cities or Places") {
      
      countyname_ST_picked <- fips2countyname(input$countypicker, includestate = "ST")
      incounties <- placetable$countyname_ST %in% countyname_ST_picked
      
      if (input$allplacesincounty) {
        
        mychoices <- placetable$fips[incounties]
        names(mychoices) <- paste0(placetable$placename[incounties], ", ", placetable$countyname_ST[incounties], " (",mychoices,")")
        
        shiny::updateSelectizeInput(session, inputId = "placespicker", 
                                    choices = mychoices,
                                    selected = mychoices)
      } else {
        # ? do we want it to reset all when the user UNCHECKS "all in" ?
      }
      
      displaytable(
        placetable[placetable$fips %in% input$placepicker, ]
      )
      
      # bgstable(
      #   # ??? 
      #   # EJAM::fips_bg_from_anyfips(fips = NA) # NOT SURE PLACES FIPS NEATLY FALL INTO BLOCKGROUPS
      # )
    }
  })
  
  observe({
    if (input$type2analyze == 0) {
      displaytable(NULL)
    }
  })
  
  ## renderTable ############# 
  displaytable <- reactiveVal() # to show here
  output$placestable <- renderTable({
    displaytable()
  })
  # selectedrows <- reactiveVal()
  # bgstable <- reactiveVal()  # all the blockgroup fips to analyze and ejam_uniq_id = units to analyze
  
  
  ## OLDER?
  # observe({
  #   if (input$allcountiesinstate != "" ) {
  #     isolate({
  #       instate <- placetable$ST %in% input$statepicker
  #       countyname_ST = unique(placetable[instate, 'countyname_ST'])
  #       mychoices <- fips_counties_from_countyname(countyname_ST)
  #       names(mychoices) <- countyname_ST
  #       updateSelectizeInput(
  #         session = session, 
  #         inputId = "countypicker", 
  #         choices = mychoices,
  #         selected = mychoices
  #       )})
  #     shinyjs::hideElement("placespicker")
  #   } else {
  #     if (input$type2analyze == "Cities or Places") {
  #       # i think we want to remove all picks if this checkbox changes value to FALSE ?
  #       updateSelectizeInput(session = session, 
  #                            inputId = "countypicker", 
  #                            selected = character(0))
  #       shinyjs::showElement("placespicker")
  #     }
  #   }
  # })
  # 
  
  # return( bgstable() )
  
}

######################################################### # 
# ~ ####

# ui_placespicker ####

ui_placespicker <- fluidPage(
  
  shinyjs::useShinyjs(),
  titlePanel("Cities and other Census Places"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("type2analyze", multiple = FALSE,
                  label = "What kinds of areas do you want to compare?", 
                  choices = c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places"), 
                  
                  selected = "Cities or Places"), # default to assuming they want to pick places.
      # selected = "Select Regions, States, Counties, or Cities"),
      
      checkboxGroupInput("regionpicker2", label = "EPA Region", 
                         choices = NULL,  # regiontable is available in server not in UI function # sort(regiontable$REGION), # sort(unique(placetable$eparegion)), 
                         selected = NULL, # set these in server
                         inline = TRUE),
      
      checkboxInput("allstatesinregion", "Compare all States in EPA Region(s)?", value = FALSE),
      
      selectizeInput("statepicker", label = "Select State", 
                     choices = NULL, # unique(placetable$ST),
                     multiple = T, 
                     selected = ""),
      
      checkboxInput("allcountiesinstate", "Compare all Counties in State(s)?", value = FALSE),
      
      selectizeInput("countypicker", label = "Select Counties", 
                     choices = NULL, # unique(placetable$countyname_ST),
                     multiple = T,
                     selected = ""),
      
      checkboxInput("allplacesincounty", "Compare all Cites/Places in County? [not enabled]", value = FALSE),
      
      ## (40,000 PLACES !)  
      
      selectizeInput("placespicker", 
                     label = "Select Places", 
                     choices = NULL, # loads faster if NULL and then update it via server... to placetable$placename,
                     multiple = T,
                     selected = ""),
      
      actionButton("reset", label = "Clear selections [not implemented yet]")
    ),
    
    ## Show table of selected locations 
    mainPanel(
      tableOutput("placestable")
    )
  )
)

