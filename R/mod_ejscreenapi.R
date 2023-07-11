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
        # shiny::actionButton('runbutton_api', label = 'Start (get EJScreen results via API)'),
        
        ## button to download table ####
        # shiny::uiOutput('downloadButton_ui'),
        ## HTML(paste('<a href=\"', 'example_webpage.html', '\", target=\"_blank\">', 'Example of how a Webpage Link could go Here',  '</a>', sep = '')), # placeholder in case want this later
        # ## pick names for columns ####
        ## shiny::uiOutput('renameButton_ui'),
        
        # #    show  
        # DT::DTOutput('rendered_results_table')
        #### DT::dataTableOutput('rendered_results_table')   
        #### shiny::uiOutput('table_ui')  
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
    
    
    
    
    output$rendered_results_table <- DT::renderDT({ # params: input$usewhichnames map_headernames and results_table() and 
      
      # _Toggle column names ####
      if (is.null(input$usewhichnames)) {usewhichnames <- 'long'} else {
        
        usewhichnames <- input$usewhichnames
      }
      table_as_displayed <- results_table()
      
      # almost identical to code in ejscreenapi_plus() and ejamit() which relies on that:
      
      
      
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
      names(table_as_displayed) <- fixcolnames(
        names_table_as_displayed=names(table_as_displayed), 
        towhichnames=usewhichnames, 
        mapping_for_names=map_headernames
      )
      
      
      
      #-_Commas for pop count ####
      if ('totalPop' %in% names(table_as_displayed)) table_as_displayed$totalPop <- prettyNum(round(table_as_displayed$totalPop, 0), big.mark = ',') 
      if ('pop'      %in% names(table_as_displayed)) table_as_displayed$pop      <- prettyNum(round(table_as_displayed$pop, 0),      big.mark = ',') 
      
      table_as_displayed
      
      ############################################################## #
      # TRICKY SETTING OPTIONS FOR TABLE VIEW (filter per col, fixed header, etc.)
      #
      # DT::renderDT aka DT::renderDataTable() is used here. 
      #   If instead use DT::datatable(), easier to set some options like filter, 
      #   like this      DT::datatable(table_as_displayed, filter = 'top')
      #   but then DT::renderDT() ignores options set below!
      # https://datatables.net/reference/option/fixedHeader.header  # to fix header row in place during scrolling down list (like freeze panes for first row)
      # https://datatables.net/reference/option/searchPanes  # a search/filter box above each column
    }, 
    options = list(
      selection = 'multiple',
      dom = 'rtip', # default table has 5 DOM elements: search box, length menu, info summary, pagination control, table. Display a subset of elements via dom= 
      scrollX = TRUE, 
      # fixedHeader = TRUE,                 # does not seem to work.  (also, does it need  scrollY = TRUE, ?)
      searchPanes=TRUE                      # does not seem to work
    ),
    # filter='top', orderCellsTop = TRUE),  # does not seem to work. 
    # filter=list(position='top')           # does not seem to work.  is this how?
    # server = TRUE, # should already be the default for DT::renderDataTable, and is better if table is large
    
    escape = FALSE
    )
    
    
    
    
  })
}

## To be copied in the UI
# mod_ejscreenapi_ui("ejscreenapi_1")

## To be copied in the server
# mod_ejscreenapi_server("ejscreenapi_1")
