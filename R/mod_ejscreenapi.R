#' ejscreenapi UI Function
#'
#' @description A shiny Module to get EJScreen results via the EJScreen API
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ejscreenapi_ui <- function(id){
  ns <- NS(id)
  tagList(
    # _____ RESULTS TABLE _____ (below) ####
    fluidRow(
      column(
        12,
        br(),
        shiny::actionButton(NS(id,'runbutton_api'), label = 'Start (get EJScreen results via API)'),
        br(),
        br(),
        
        ## button to download table ####
        # shiny::uiOutput(NS(id,'downloadButton_ui')),
        
        ## HTML(paste('<a href=\"', 'example_webpage.html', '\", target=\"_blank\">', 'Example of how a Webpage Link could go Here',  '</a>', sep = '')), # placeholder in case want this later
        # ## pick names for columns ####
        # shiny::uiOutput('renameButton_ui'),
        radioButtons(NS(id,'usewhichnames'), label = "Variable name style", 
                     choices = c('long', 'short'), selected = 'short', inline = TRUE),
        br(),
        br(),
        h3("Results:"),
        br(),
        # #    show  DTOutput
        DT::DTOutput(outputId = NS(id,'rendered_results_table')),
        h4('(end of results table)')
      )
    )
    
  )
}

#' ejscreenapi Server Functions
#'
#' @noRd 
mod_ejscreenapi_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    results_table <- reactive({
      
      as.data.frame(testoutput) 
      
      # ejscreenit_for_ejam(testpoints_100[1:3, ]) # for testing
      
      
      
    })  %>%
      bindEvent(input$runbutton_api, label = "runbutton_api")   # ignoreInit = TRUE, 
    
    
    # results_table <- reactiveVal(NULL) # init to store results
    # observeEvent(input$runbutton_api, {  # (if api start button is pressed) 
    # #### showNotification('Processing sites now!', type = 'message', duration = 0.5)
    # out <- data.frame(a=1:10,b=1:10)
    # ##### assign doaggregate output to data_processed reactive 
    # results_table(out)
    # })
    
    
    #################################################################################### # 
    
    output$rendered_results_table <- DT::renderDT({
      req(results_table())
      # params: input$usewhichnames map_headernames and results_table() and 
      
      # _Toggle column names ####
      if (is.null(input$usewhichnames)) {usewhichnames <- 'long'} else {
        usewhichnames <- input$usewhichnames
      }
      
      table_as_displayed <- results_table()
      
      print( 'ok')
      
      
      # almost identical to code in ejscreenapi_plus() and ejamit() which relies on that:
      
      if (1 == 0) {   # disabled while developing this *******************************************************
        
        #_Rename columns *temporarily* to make this bit of code work below that is written to use R-friendly names ####
        names(table_as_displayed) <- fixcolnames(
          names_table_as_displayed=names(table_as_displayed), 
          towhichnames='friendly', 
          mapping_for_names=map_headernames
        )
        
        ############################################################## #
        # **add NA placeholders for supplementary columns while API does not provide those columns yet ####
        suppnames = c(
          "Demog.Index.Supp",                           "lowlifex", 
          "pctile.Demog.Index.Supp",             "pctile.lowlifex", 
          "avg.Demog.Index.Supp",                   "avg.lowlifex" , 
          "state.pctile.Demog.Index.Supp", "state.pctile.lowlifex",
          "state.avg.Demog.Index.Supp",       "state.avg.lowlifex"    
        )
        suppnames <- suppnames[!(suppnames %in% names(table_as_displayed))]
        placeholders = data.frame(as.list(rep(NA, length(suppnames))))
        names(placeholders) = suppnames
        table_as_displayed <- cbind(table_as_displayed, placeholders)
        ############################################################## #
        # Add Ratio to us or state average here? 
        # **add ratio columns that EJAM has ####
        # USA
        usratios <- ratios_to_avg(table_as_displayed) # this was not designed to analyze state percentiles ?
        eratios <- round(usratios$ratios_e, 4)
        dratios <- round(usratios$ratios_d, 4)    # lacks subgroups and supplementary ? 
        names(eratios) <- map_headernames$newnames_ejscreenapi[ map_headernames$varlist == "names_e_ratio_to_avg"] # names_e_ratio_to_avg  # lacks state percentiles here ****
        names(dratios) <- map_headernames$newnames_ejscreenapi[ map_headernames$varlist == "names_d_ratio_to_avg"] # names_d_ratio_to_avg  # lacks state percentiles here ****
        table_as_displayed <- cbind(table_as_displayed, dratios, eratios)
        # STATES
        st_ratios <- ratios_to_avg(table_as_displayed, zone.prefix = "state.") # USE THE STATE AVERAGES
        eratios <- round(st_ratios$ratios_e, 4)
        dratios <- round(st_ratios$ratios_d, 4)    # lacks subgroups and supplementary ? 
        names(eratios) <- map_headernames$newnames_ejscreenapi[ map_headernames$varlist == "names_e_ratio_to_state_avg"]
        names(dratios) <- map_headernames$newnames_ejscreenapi[ map_headernames$varlist == "names_d_ratio_to_state_avg"]
        table_as_displayed <- cbind(table_as_displayed, dratios, eratios)
        
        ############################################################## #
        
        #_Rename columns to what user requested ####
        # not working, even with example data ****************************************
        ## print(usewhichnames) # disabled while developing this
        # colnames(table_as_displayed) <- fixcolnames(
        #   names_table_as_displayed = colnames(table_as_displayed), 
        #   towhichnames = usewhichnames, 
        #   mapping_for_names = map_headernames
        # )
        
      } # disabled while developing this ****************************************
      
      
      #-_Commas for pop count ####
      if ('totalPop' %in% names(table_as_displayed)) table_as_displayed$totalPop <- prettyNum(round(table_as_displayed$totalPop, 0), big.mark = ',') 
      if ('pop'      %in% names(table_as_displayed)) table_as_displayed$pop      <- prettyNum(round(table_as_displayed$pop, 0),      big.mark = ',') 
      
      DT::datatable( table_as_displayed, escape = FALSE)
      
      ############################################################## #
      # TRICKY SETTING OPTIONS FOR TABLE VIEW (filter per col, fixed header, etc.)
      #
      # DT::renderDT aka DT::renderDataTable() is used here. 
      #   If instead use DT::renderDT()????, easier to set some options like filter, 
      #   like this      DT::datatable(table_as_displayed, filter = 'top')
      #   but then DT::renderDT() ignores options set below!
      # https://datatables.net/reference/option/fixedHeader.header  # to fix header row in place during scrolling down list (like freeze panes for first row)
      # https://datatables.net/reference/option/searchPanes  # a search/filter box above each column
      
    }, # render rendered_results_table
    
    options = list(
      # selection = 'multiple',
      # dom = 'rtip', # default table has 5 DOM elements: search box, length menu, info summary, pagination control, table. Display a subset of elements via dom= 
      # scrollX = TRUE, 
      # # fixedHeader = TRUE,                 # does not seem to work.  (also, does it need  scrollY = TRUE, ?)
      # searchPanes=TRUE                      # does not seem to work
    ),
    # filter='top', orderCellsTop = TRUE),  # does not seem to work. 
    # filter=list(position='top')           # does not seem to work.  is this how?
    # server = TRUE, # should already be the default for DT::renderDataTable, and is better if table is large
    
    escape = FALSE
    ) # end render rendered_results_table options
    ################################################################################## # 
    
  } # end moduleServer
  ) # end moduleServer function
} # end server overall

################################################ #
## copied in the UI
# mod_ejscreenapi_ui("ejscreenapi_1")

## copied in the server
# mod_ejscreenapi_server("ejscreenapi_1")


################################################ #
# TEST THE MODULE 
################################################ #

testing <- FALSE # so that installation will not source this and launch the module as a mini app

if (testing) {
  library(shiny); library(magrittr)
  ui <- fluidPage(
    h2('Test of Module for EJScreen API Batch tool within EJAM'), 
    mod_ejscreenapi_ui("ejscreenapi_1")
  )
  server <- function(input, output, session) {
    mod_ejscreenapi_server("ejscreenapi_1")
  }
  
  shinyApp(ui, server) # Test the module, as a mini-app
  
  ################################################ #
  # test data like the output from ejscreenit(), to use here in testing the module  ####
  testoutput <- as.data.frame(
    structure(
      list(
        id = c(1, 2), 
        EJScreenPDF = c("<a href=\"https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry=%7B%22spatialReference%22:%7B%22wkid%22:4326%7D,%22x%22:-91.132107,%22y%22:30.494982%7D&distance=1&unit=9035&areatype=&areaid=&f=report\", target=\"_blank\">EJScreen Report 1</a>", 
                        "<a href=\"https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry=%7B%22spatialReference%22:%7B%22wkid%22:4326%7D,%22x%22:-91.09,%22y%22:30.45%7D&distance=1&unit=9035&areatype=&areaid=&f=report\", target=\"_blank\">EJScreen Report 2</a>"
        ), 
        EJScreenMAP = c("<a href=\"https://ejscreen.epa.gov/mapper/index.html?wherestr=30.494982,-91.132107\", target=\"_blank\">EJScreen Map 1</a>", 
                        "<a href=\"https://ejscreen.epa.gov/mapper/index.html?wherestr=30.45,-91.09\", target=\"_blank\">EJScreen Map 2</a>"
        ), 
        siteid = c(1, 2), sitename = c("example site 1", "example site 2"), lon = c(-91.132107, -91.09), lat = c(30.494982, 30.45), 
        
        pctmin = c(96,49), pctlowinc = c(75, 24), pctlths = c(23, 7), pctlingiso = c(1,1), pctunder5 = c(8, 5), pctover64 = c(10, 19), pctunemployed = c(18,3), Demog.Index = c(86, 37), 
        pctpre1960 = c(0.22, 0.27), dpm = c(0.464,0.479), cancer = c(51, 58), resp = c(0.7, 0.6), traffic.score = c(980,830), proximity.npdes = c(0.41, 1.7), proximity.npl = c(0.091,0.056), proximity.rmp = c(2, 0.62), proximity.tsdf = c(3.8, 2.1), o3 = c(38, 38.1), pm = c(10.2, 10.3), ust = c(2.9, 2.9), 
        
        state.avg.pctmin = c(41,41), state.avg.pctlowinc = c(39, 39), state.avg.pctlths = c(15,15), state.avg.pctlingiso = c(2, 2), state.avg.pctunder5 = c(7,7), state.avg.pctover64 = c(15, 15), state.avg.pctunemployed = c(6,6), state.avg.Demog.Index = c(40, 40), 
        state.avg.pctpre1960 = c(0.2,0.2), state.avg.dpm = c(0.298, 0.298), state.avg.cancer = c(41,41), state.avg.resp = c(0.45, 0.45), 
        state.avg.traffic.score = c(560,560), state.avg.proximity.npdes = c(0.42, 0.42), state.avg.proximity.npl = c(0.086,0.086), state.avg.proximity.rmp = c(0.91, 0.91), state.avg.proximity.tsdf = c(1.4, 1.4), state.avg.o3 = c(37.2, 37.2), state.avg.pm = c(9.22, 9.22), state.avg.ust = c(2, 2), 
        
        state.pctile.pctmin = c(93, 63), state.pctile.pctlowinc = c(94, 27), state.pctile.pctlths = c(79, 28), state.pctile.pctlingiso = c(67, 67), state.pctile.pctunder5 = c(66,41), state.pctile.pctover64 = c(27, 74), state.pctile.pctunemployed = c(93,31), state.pctile.Demog.Index = c(96, 52),
        state.pctile.pctpre1960 = c(70,76), state.pctile.dpm = c(81, 82), state.pctile.cancer = c(87,92), state.pctile.resp = c(98, 96), 
        state.pctile.traffic.score = c(85,83), state.pctile.proximity.npdes = c(93, 96), state.pctile.proximity.npl = c(72,53), state.pctile.proximity.rmp = c(85, 60), state.pctile.proximity.tsdf = c(91, 75), state.pctile.o3 = c(64, 67), state.pctile.pm = c(93,98), state.pctile.ust = c(76, 76), state.pctile.EJ.DISPARITY.pctpre1960.eo = c(90, 36), 
        state.pctile.EJ.DISPARITY.dpm.eo = c(95, 60), state.pctile.EJ.DISPARITY.cancer.eo = c(96, 56), state.pctile.EJ.DISPARITY.resp.eo = c(98, 57), 
        state.pctile.EJ.DISPARITY.traffic.score.eo = c(95, 73), state.pctile.EJ.DISPARITY.proximity.npdes.eo = c(96, 94), state.pctile.EJ.DISPARITY.proximity.npl.eo = c(92, 61), state.pctile.EJ.DISPARITY.proximity.rmp.eo = c(95, 69), state.pctile.EJ.DISPARITY.proximity.tsdf.eo = c(97, 74), state.pctile.EJ.DISPARITY.o3.eo = c(94, 55), state.pctile.EJ.DISPARITY.pm.eo = c(95,56), state.pctile.EJ.DISPARITY.ust.eo = c(91, 78), 
        
        region.avg.pctmin = c(52,52), region.avg.pctlowinc = c(36, 36), region.avg.pctlths = c(15,15), region.avg.pctlingiso = c(6, 6), region.avg.pctunder5 = c(7,7), region.avg.pctover64 = c(13, 13), region.avg.pctunemployed = c(5,5), region.avg.Demog.Index = c(44, 44), 
        region.avg.pctpre1960 = c(0.16,0.16), region.avg.dpm = c(0.219, 0.219), region.avg.cancer = c(32,32), region.avg.resp = c(0.37, 0.37), 
        region.avg.traffic.score = c(470,470), region.avg.proximity.npdes = c(0.5, 0.5), region.avg.proximity.npl = c(0.08,0.08), region.avg.proximity.rmp = c(0.83, 0.83), region.avg.proximity.tsdf = c(0.8,0.8), region.avg.o3 = c(41.1, 41.1), region.avg.pm = c(9.32, 9.32), region.avg.ust = c(2, 2), 
        
        region.pctile.pctmin = c(91, 49), region.pctile.pctlowinc = c(95, 34), region.pctile.pctlths = c(75, 33),region.pctile.pctlingiso = c(42, 42), region.pctile.pctunder5 = c(62, 36), region.pctile.pctover64 = c(41, 79), region.pctile.pctunemployed = c(96, 33), region.pctile.Demog.Index = c(97, 44), 
        region.pctile.pctpre1960 = c(75, 79), region.pctile.dpm = c(95, 96), region.pctile.cancer = c(97, 98), region.pctile.resp = c(99, 99), 
        region.pctile.traffic.score = c(88, 86), region.pctile.proximity.npdes = c(95, 98), region.pctile.proximity.npl = c(76, 62), region.pctile.proximity.rmp = c(89, 60), region.pctile.proximity.tsdf = c(97, 89), region.pctile.o3 = c(29, 30), region.pctile.pm = c(83, 87), region.pctile.ust = c(74, 75),
        
        region.pctile.EJ.DISPARITY.pctpre1960.eo = c(87, 22), region.pctile.EJ.DISPARITY.dpm.eo = c(94, 53), region.pctile.EJ.DISPARITY.cancer.eo = c(92, 49), region.pctile.EJ.DISPARITY.resp.eo = c(94, 50), 
        region.pctile.EJ.DISPARITY.traffic.score.eo = c(92, 66), region.pctile.EJ.DISPARITY.proximity.npdes.eo = c(97, 94), region.pctile.EJ.DISPARITY.proximity.npl.eo = c(86,53), region.pctile.EJ.DISPARITY.proximity.rmp.eo = c(91,60), region.pctile.EJ.DISPARITY.proximity.tsdf.eo = c(97, 74), region.pctile.EJ.DISPARITY.o3.eo = c(83, 47), region.pctile.EJ.DISPARITY.pm.eo = c(86, 48), region.pctile.EJ.DISPARITY.ust.eo = c(85, 69), 
        
        avg.pctmin = c(40, 40), avg.pctlowinc = c(31, 31), avg.pctlths = c(12, 12), avg.pctlingiso = c(5, 5), avg.pctunder5 = c(6, 6), avg.pctover64 = c(16,16), avg.pctunemployed = c(5, 5),  avg.Demog.Index = c(36, 36), 
        avg.pctpre1960 = c(0.28, 0.28), avg.dpm = c(0.295, 0.295), avg.cancer = c(29, 29), avg.resp = c(0.36, 0.36), 
        avg.traffic.score = c(710,710), avg.proximity.npdes = c(12, 12), avg.proximity.npl = c(0.13,0.13), avg.proximity.rmp = c(0.75, 0.75), avg.proximity.tsdf = c(2.2,2.2), avg.o3 = c(42.6, 42.6), avg.pm = c(8.74, 8.74), avg.ust = c(3.9,3.9), 
        
        pctile.pctmin = c(93, 65), pctile.pctlowinc = c(96,43), pctile.pctlths = c(84, 43), pctile.pctlingiso = c(50,50), pctile.pctunder5 = c(71, 45), pctile.pctover64 = c(30, 71), pctile.pctunemployed = c(96, 33), pctile.Demog.Index = c(98,60), 
        pctile.pctpre1960 = c(56, 60), pctile.dpm = c(84, 86), pctile.cancer = c(99, 99), pctile.resp = c(99, 98), 
        pctile.traffic.score = c(82,79), pctile.proximity.npdes = c(90, 94), pctile.proximity.npl = c(63,46), pctile.proximity.rmp = c(90, 64), pctile.proximity.tsdf = c(83,71), pctile.o3 = c(21, 22), pctile.pm = c(86, 87), pctile.ust = c(66,66), 
        pctile.EJ.DISPARITY.pctpre1960.eo = c(86, 43), pctile.EJ.DISPARITY.dpm.eo = c(93,65), pctile.EJ.DISPARITY.cancer.eo = c(96, 64), pctile.EJ.DISPARITY.resp.eo = c(97,65), 
        pctile.EJ.DISPARITY.traffic.score.eo = c(93, 74), pctile.EJ.DISPARITY.proximity.npdes.eo = c(95,93), pctile.EJ.DISPARITY.proximity.npl.eo = c(88, 65), pctile.EJ.DISPARITY.proximity.rmp.eo = c(95,72), pctile.EJ.DISPARITY.proximity.tsdf.eo = c(93, 75), pctile.EJ.DISPARITY.o3.eo = c(89,63), pctile.EJ.DISPARITY.pm.eo = c(92, 64), pctile.EJ.DISPARITY.ust.eo = c(86,76), 
        
        ST = c("LA", "LA"), statename = c("LOUISIANA", "LOUISIANA"), REGION = c(6, 6), pop = c(11970, 6236), NUM_NPL = c(0, 0), NUM_TSDF = c(2, 0), statLayerCount = c(18, 8), statLayerZeroPopCount = c(0, 0), weightLayerCount = c(200, 173), timeSeconds = c(0.7290357, 0.4920234), 
        radius.miles = c(1, 1), unit = c(9035, 9035), statlevel = c("blockgroup", "blockgroup"), inputAreaMiles = c(3.14,3.14), 
        lon.1 = c(-91.132107, -91.09), lat.1 = c(30.494982,30.45)
      )
    )
  )
}
