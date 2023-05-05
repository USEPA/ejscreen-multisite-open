#' EJAM app server
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import DT
#' @import data.table
#' @importFrom data.table ":="
#' @import foreach
#' @import ggplot2
#' @import glue
#' @import golem
#' @import leaflet
#' @import readxl
#' @import RMySQL
#' @import SearchTrees
#' @import shinyBS
#' @import shinycssloaders
#' @import shinyjs
#' @import sf
#' @import sp
#' @import tidyverse
#' @import magrittr
#' @import tidyr
#' @import dplyr
#' 
app_server <- function(input, output, session) {
  
  ## to profile parts of the shiny app for performance
  # callModule(profvis_server, "profiler")
  
  # Note: how to avoid or still have a global.R file in the golem approach, 
  # https://github.com/ThinkR-open/golem/issues/6
  
  #############################################################################  # 
  
  ## ** US AVERAGE for each indicator  
  
  ## ______ SELECT SITES ________####
  # ~ ####
  
  # update ss_select_NAICS input options ###
  updateSelectizeInput(session, inputId = 'ss_select_naics',
                       ## use named list version, grouped by first two code numbers
                       #choices = naics_as_list, # need to keep formatting
                       choices = EJAM::NAICS, # named list of codes
                       server = TRUE)
  
  ## hide advanced settings tab by default
  hideTab(inputId = 'all_tabs', target = 'Advanced Settings')
  
  ## show advanced settings tab on button click (button in 'About EJAM' tab)
  observeEvent(input$ui_show_advanced_settings,
               {
                 showTab(inputId = 'all_tabs', target = 'Advanced Settings')
               })
  observeEvent(input$ui_hide_advanced_settings,
               {
                 hideTab(inputId = 'all_tabs', target = 'Advanced Settings')
               })
  
  ## show welcome modal on app startup - commented out for now
  # showModal(
  #   modalDialog(
  #     title = 'Welcome to EJAM!', 
  #     "Add basic message here",
  #     footer = actionButton('ui_close_welcome', 'Close')
  #   )
  # )
  ## close welcome modal on button click
  # observeEvent(input$ui_close_welcome,{
  #   removeModal()
  # })
  #############################################################################  # 
  
  ## define current upload method using radio button ####
  current_upload_method <- reactive({
    switch(
      input$ss_choose_method,
      latlon = "latlon",  # 'Location (lat/lon)',
      FRS =  "FRS", # 'FRS (facility ID)',
      ECHO = "ECHO", # 'ECHO Search Tools',
      NAICS = "NAICS" # 'NAICS (industry name or code)'
    )
  })
  #############################################################################  # 
  ## reactive: data uploaded as latlon ####
  
  data_up_latlon <- reactive({
    
    ## wait for file to be uploaded
    req(input$ss_upload_latlon)
    
    # >this part could be replaced by  latlon_from_anything() #### 
    #  ext <- latlon_from_anything(input$ss_upload_latlon$datapath)
    
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_latlon$name)
    ## if acceptable file type, read in; if not, send warning text
    ext <- switch(ext,
                  csv =  read.csv(input$ss_upload_latlon$datapath),
                  xls = read_excel(input$ss_upload_latlon$datapath),
                  xlsx = read_excel(input$ss_upload_latlon$datapath),
                  shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    
    ## if column names are found in lat/long alias comparison, process
    if(any(tolower(colnames(ext)) %in% lat_alias) & any(tolower(colnames(ext)) %in% lon_alias)){
      ext %>% 
        EJAM::latlon_df_clean() %>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
        data.table::as.data.table()
      
    } else {
      ## if not matched, show this message instead
      shiny::validate('No coordinate columns found.')
    }
  })
  
  #############################################################################  # 
  ## reactive: data uploaded by FRS registry IDs ####
  
  data_up_frs <- reactive({
    
    ## wait for file to be uploaded
    req(input$ss_upload_frs)
    ##  >this part could be replaced by  latlon_from_anything() ####
    # ext <- latlon_from_anything(input$ss_upload_latlon$datapath)
    
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_frs$name)
    ## if acceptable file type, read in; if not, send warning text
    read_frs <- switch(ext,
                       csv =  read.csv(input$ss_upload_frs$datapath),
                       xls = read_excel(input$ss_upload_frs$datapath),
                       xlsx = read_excel(input$ss_upload_frs$datapath),
                       shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    ) # returns a data.frame
    
    #include frs_is_valid verification check function, must have colname REGISTRY_ID
    if (frs_is_valid(read_frs)){
      #converts registry id to character if not already in that class ( frs registry ids are character)
      if(class(read_frs$REGISTRY_ID) != "character"){
        read_frs$REGISTRY_ID = as.character(read_frs$REGISTRY_ID)
      }
      frs_lat_lon <- frs_from_regid(read_frs$REGISTRY_ID)
      # read_frs_dt <- data.table::as.data.table(read_frs)
      data.table::setDT(frs_lat_lon) # same but less memory/faster?
    } else {
      shiny::validate('Records with invalid Registry IDs')
    }
    ## return merged dataset
    frs_lat_lon
  })
  
  #############################################################################  # 
  ## reactive: data uploaded by NAICS ####
  
  data_up_naics <- reactiveVal(NULL)
  
  ## when NAICS submit button is pressed ------------   work in progress ====== not tested
  observeEvent(input$submit_naics, {
    
    ## check if anything has been selected or entered
    req(shiny::isTruthy(input$ss_enter_naics) || shiny::isTruthy(input$ss_select_naics))
    
    #define inputs
    naics_user_wrote_in_box <- input$ss_enter_naics
    naics_user_picked_from_list <- input$ss_select_naics
    add_naics_subcategories <- input$add_naics_subcategories 
    # q: IS IT BETTER TO USE THIS IN naics_from_any() OR IN frs_from_naics() BELOW ??
    
    # naics_validation function to check for non empty NAICS inputs
    if(naics_validation(input$ss_enter_naics,input$ss_select_naics)){
      inputnaics = {}
      #splits up comma separated list if user manually inserts list
      if(nchar(input$ss_enter_naics)>0){
        # print('test2')
        #checks for non-numeric values in text box; if they are not numeric, then search by name
        if(!grepl("^\\d+(,\\d+)*$",input$ss_enter_naics)){
          # print('test')
          
          #   1. GET NAICS CODES VIA QUERY OF TEXT IN NAMES OR THE NUMBERS
          
          inputnaics = naics_from_any(input$ss_enter_naics)[,code] # this used to be naics_find()
          
          if(length(inputnaics) == 0 | all(is.na(inputnaics))){
            ################ Should output something saying no valid results returned ######## #
            shiny::validate('No Results Returned')
          }        
        } else {
          naics_wib_split <- as.list(strsplit(naics_user_wrote_in_box, ",")[[1]])
          print(naics_wib_split)
        }
      } else {
        naics_wib_split <- ""
      }
      # if not empty, assume its pulled using naics_from_any() or older naics_find() above
      if(length(inputnaics) == 0 | rlang::is_empty(inputnaics)) {
        #construct regex expression and finds sites that align with user-selected naics codes
        inputnaics <- c(naics_wib_split, naics_user_picked_from_list)
        inputnaics <- unique(inputnaics[inputnaics != ""])
        # inputnaics <- paste("^", inputnaics, collapse="|")   ### the NAICS specified by user
        # inputnaics <- stringr::str_replace_all(string = inputnaics, pattern = " ", replacement = "")
        print(inputnaics)
        
        #merge user-selected NAICS with FRS facility location information
        # sitepoints <- frs_by_naics[NAICS %like% inputnaics ,  ]
        
        #   2. GET FACILITY LAT/LON INFO FROM NAICS CODES  
        
        sitepoints <- frs_from_naics(inputnaics, children=add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx
        # print(sitepoints)
        if(rlang::is_empty(sitepoints) | nrow(sitepoints) == 0){
          ################ Should output something saying no valid results returned ######## #
          shiny::validate('No Results Returned')
        }
      } else{  
        sitepoints <- frs_from_naics(inputnaics, children=add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx
        # print(sitepoints)
        showNotification('Points submitted successfully!', duration = 1)
      }
    } else {
      ################ Should output something saying no valid results returned ######## #
      shiny::validate('Invalid NAICS Input')
    }
    cat("SITE COUNT VIA NAICS: ", NROW(sitepoints), "\n")
    
    ## assign final value to data_up_naics reactive variable
    data_up_naics(sitepoints)
  })
  
  ## if NAICS radio button is toggled between dropdown/enter, empty the other one
  observeEvent(input$naics_ul_type, {
    if(input$naics_ul_type == 'dropdown'){
      shinyjs::reset(id = 'ss_select_naics')
    } else if(input$naics_ul_type == 'enter'){
      shinyjs::reset(id = 'ss_enter_naics')
    }
  })
  
  #############################################################################  # 
  ## reactive: data uploaded by ECHO ####
  # 
  data_up_echo <- reactive({
    ## depends on ECHO upload - which may use same file upload as latlon
    req(input$ss_upload_echo)
    
    ## >this part could be replaced by ####################################
    # ext <- latlon_from_anything(input$ss_upload_echo$datapath)
    
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_echo$name)
    
    ## if acceptable file type, read in; if not, send warning text
    ext <- switch(ext,
                  csv =  read.csv(input$ss_upload_echo$datapath),
                  xls = read_excel(input$ss_upload_echo$datapath),
                  xlsx = read_excel(input$ss_upload_echo$datapath),
                  shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    
    ## only process if lats and lon (or aliases) exist in uploaded data  
    ## if column names are matched to aliases, process it    
    if(any(tolower(colnames(ext)) %in% lat_alias) & any(tolower(colnames(ext)) %in% lon_alias)){
      ext %>% 
        EJAM::latlon_df_clean() %>% 
        data.table::as.data.table()
    } else {
      ## if not matched, return this message
      shiny::validate('No coordinate columns found.')
    }
  })
  
  #############################################################################  # 
  ## reactive: count data upload methods currently used ####
  num_ul_methods <- reactive({
    shiny::isTruthy(input$ss_upload_latlon) +
      shiny::isTruthy(input$ss_upload_frs) +
      (shiny::isTruthy(input$ss_enter_naics) ||  shiny::isTruthy(input$ss_select_naics)) +
      shiny::isTruthy(input$ss_upload_echo)
  })
  
  ## reactive: hub for any/all uploaded data, gets passed to processing ####
  data_uploaded <- reactive({
    print("data_uploaded reactive was updated!")
    cat("method is ", current_upload_method(), "\n")
    
    ## send message if no data uploaded
    validate(
      need(num_ul_methods() > 0, "Please upload a data set")
    )
    
    ## if more than 1 upload method used, it will try to use the one
    ## that is currently selected by the ss_choose_method radio button
    
    ## if using lat/lon upload
    if(current_upload_method() == 'latlon'){
      
      data_up_latlon() #%>% 
      
    } else if(current_upload_method() == 'NAICS'){
      print('setting data_uploaded() to data_up_naics() ... ')
      data_up_naics()
      
    } else if(current_upload_method() == 'FRS'){
      
      data_up_frs()
      
    } else if(current_upload_method() == 'ECHO'){
      
      data_up_echo() 
      
    }
    
  })
  #############################################################################  # 
  
  ## disable run button until there is data uploaded; then enable
  observe({
    if(num_ul_methods() == 0){
      shinyjs::disable(id = 'bt_get_results' )
    } else {
      shinyjs::enable(id = 'bt_get_results')
    }
  })
  
  #############################################################################  # 
  # ~ ####
  # ______ VIEW UPLOADED / SELECTED POINTS ####
  # ~ ####
  
  ## How many valid points? warn if no valid lat/lon ####
  output$an_map_text <- renderUI({
    req(data_uploaded())
    #separate inputs with valid/invalid lat/lon values
    num_na    <- nrow(data_uploaded()[ (is.na(data_uploaded()$lat) | is.na(data_uploaded()$lon)),])
    num_notna <- nrow(data_uploaded()[!(is.na(data_uploaded()$lat) | is.na(data_uploaded()$lon)),])
    ## if invalid data found, send modal to screen
    if(num_na > 0){
      showModal(modalDialog(title = 'Invalid data found', 'FYI, some of your data was not valid.', size = 's'))
    }
    HTML(paste0('Current upload method: <strong>',  current_upload_method(), '</strong><br>', 
                'Total site(s) uploaded: <strong>', prettyNum(nrow(data_uploaded()), big.mark=','),'</strong><br>',
                'Valid site(s) uploaded: <strong>', prettyNum(num_notna, big.mark=','),'</strong><br>',
                'Site(s) with invalid lat/lon values: <strong>', prettyNum(num_na,big.mark=','), '</strong>'))
  })
  
  ## See table of uploaded points ####
  
  output$print_test2_dt <- DT::renderDT({
    req(data_uploaded())
    
    dt <- data_uploaded() # now naics-queried sites format is OK to view, since using different function to get sites by naics
    
    # if(current_upload_method() == 'NAICS'){
    
    ###takes NAICS codes selected, finds NAICS descriptions, and presents them  
    # dt_result_by_naic = data_uploaded()[, .(Count = .N), by = NAICS]
    # naics_desc = EJAM::NAICS[EJAM::NAICS %in% dt_result_by_naic$NAICS]
    # dt_names = data.frame("NAICS"=naics_desc,"Description"=names(naics_desc))
    # naicsdt = merge(x = dt_result_by_naic, y = dt_names, by='NAICS')
    # naics_reorder = data.frame(naicsdt$Description,naicsdt$Count)
    # colnames(naics_reorder) = c("NAICS Code","Facility Count")
    # dt <- naics_reorder
    ###print(naics_reorder,row.names=FALSE)
    # } else {
    # dt <- data_uploaded()
    # }
    
    DT::datatable(dt, options = list(pageLength = 100, scrollX = TRUE, scrollY = '500px'), escape = FALSE) # escape=FALSE may add security issue but makes links clickable in table
  })
  
  ## reactive: check if uploaded points are clustered (may double-count people) ####
  # note this had been done in EJAMejscreenapi::addlinks_clusters_and_sort_cols() 
  is_clustered <- shiny::reactive({
    req(data_uploaded())
    
    # which sites have residents that might also be near others sites?
    # circles overlap if 2 facilities are twice the radius apart  # in miles
    EJAMejscreenapi::near_eachother(
      lon = data_uploaded()$lon, 
      lat = data_uploaded()$lat,
      distance = 2 * input$bt_rad_buff
      ## if switching units between miles and km - not currently used
      # distance = ifelse(input$radius_units == 'miles', 
      #                   2 * input$bt_rad_buff,
      #                   2 * input$bt_rad_buff * 0.62137119
      #)
    ) 
  })
  
  #############################################################################  # 
  # ~ ####
  # ______ MAP UPLOADED POINTS ________####
  # ~ ####  
  
  ## update map radius label based on button ####
  
  observe({
    val <- input$bt_rad_buff
    lab <- paste0('Radius of circular buffer: ', val, ' mi ','(',round(val / 0.62137119, 2), ' km)')
    
    updateSliderInput(session, inputId = 'bt_rad_buff', label = lab)
    
    ## if switching units between miles and km - not currently used
    #req(input$radius_units)
    #lab <- input$radius_units
    # updateSliderInput(session, inputId = 'bt_rad_buff',
    #                   label = paste0('Radius of circular buffer (', lab, ')'),
    #                   min = 0.25, val = val, step = 0.25, max = 10)
  })
  
  orig_leaf_map <- reactive({
    req(data_uploaded())
    max_pts <- max_points_can_map
    ## don't draw map if > 5000 points are uploaded
    if(nrow(data_uploaded()) < max_pts){
      suppressMessages(
        map_facilities(mypoints = as.data.frame(data_uploaded()), 
                       rad = input$bt_rad_buff, 
                       highlight = input$an_map_clusters,
                       clustered = is_clustered())
      )
    } else {
      validate(paste0('Too many points (> ',prettyNum(max_pts, big.mark=','),') uploaded for map to be displayed'))
    }
  })
  
  ## output: draw map of uploaded points ####
  output$an_leaf_map <- leaflet::renderLeaflet({
    req(data_uploaded())
    orig_leaf_map()
  })
  
  #############################################################################  # 
  #############################################################################  # 
  #############################################################################  # 
  
  # ~ ####
  # ______ RUN ANALYSIS  (button is pressed) ________####
  # ~ ####
  
  ## initialize data_processed reactive variable 
  ##  to hold results of doaggregate()
  data_processed <- reactiveVal(NULL)
  
  ## initialize data_summarized data 
  ##  to hold results of batch.summarize()
  data_summarized <- reactiveVal(NULL)
  
  # THIS IS VERY VERY SIMILAR TO THE CODE IN ejamit() and perhaps could just rely on one set of code for both. ***xxx
  # >this part could be replaced by ejamit() or something like that ####
  
  observeEvent(input$bt_get_results, {  # (button is pressed) 
    
    showNotification('Processing sites now!', type = 'message', duration = 0.5)
    
    ## progress bar setup overall for 3 operations   
    # (getblocksnearby, doaggregate, batch.summarize)
    progress_all <- shiny::Progress$new(min = 0, max = 1)
    progress_all$set(value = 0, message = 'Step 1 of 3', detail = 'Getting nearby census blocks')
    
    #############################################################################  # 
    ## 1) **EJAM::getblocksnearby()** ####
    
    sites2blocks <- getblocksnearby(
      sitepoints = data_uploaded(),
      cutoff = input$bt_rad_buff,
      quadtree = localtree
    )
    ## progress bar update overall  
    progress_all$inc(1/3, message = 'Step 2 of 3', detail = 'Aggregating')
    ## create progress bar to show doaggregate status
    progress_doagg <- shiny::Progress$new(min = 0, max = 1)
    ## function for updating progress bar, to pass in to doaggregate function  
    updateProgress_doagg <- function(value = NULL, message_detail = NULL, message_main = NULL) {
      # Create a callback function - When called, it sets progress bar to value.
      if (is.null(value)) { # - If value is NULL, it will move the progress bar 1/5 of the remaining distance.
        value <- progress_doagg$getValue()
        value <- value + (progress_doagg$getMax() - value) / 5
      }
      progress_doagg$set(value = value, message = message_main, detail = message_detail)
    }
    
    #############################################################################  # 
    ## 2) **EJAM::doaggregate()** ####
    # blah=data_uploaded()
    # save(blah, file='testup.rda'); save(sites2blocks, file = 'testin.rda')
    out <- suppressWarnings(doaggregate(
      sites2blocks = sites2blocks, 
      sites2states = data_uploaded(),
      ## pass progress bar function as argument
      updateProgress = updateProgress_doagg
    ))
    # provide sitepoints table provided by user aka data_uploaded(), (or could pass only lat,lon and ST -if avail- not all cols?)
    # and doaggregate() decides where to pull ST info from - 
    # ideally from ST column, 
    # second from fips of block with smallest distance to site, 
    # third from lat,lon of sitepoints intersected with shapefile of state bounds
    
    ## close doaggregate progress bar
    progress_doagg$close()
    
    ################################################################ # 
    
    # add URLs (should be a function)  ####
    # (to site by site table)
    
    #  >this should be a function, and is used by both server and ejamit() ###  #
    # duplicated almost exactly in ejamit() but reactives are not reactives there
    # maybe use url_4table() - see ejamit() code
    
    #if ("REGISTRY_ID" %in% names(out$results_bysite)) {
    # echolink = url_echo_facility_webpage(out$results_bysite$REGISTRY_ID, as_html = FALSE)
    #} else {
    # echolink = url_echo_facility_webpage(out$results_bysite$REGISTRY_ID, as_html = FALSE)
    #}
    ## the registry ID column is only found in uploaded ECHO/FRS/NAICS data -
    ## it is not passed to doaggregate output at this point, so pull the column from upload to create URLS
    if("REGISTRY_ID" %in% names(data_uploaded())){
      echolink = url_echo_facility_webpage(data_uploaded()$REGISTRY_ID, as_html = FALSE)
    } else if("RegistryID" %in% names(data_uploaded())){
      echolink = url_echo_facility_webpage(data_uploaded()$RegistryID, as_html = FALSE)
    } else {
      echolink = rep(NA,nrow(out$results_bysite))
    }
    out$results_bysite[ , `:=`(
      `EJScreen Report` = url_ejscreen_report(    lat = data_uploaded()$lat, lon = data_uploaded()$lon, distance = input$bt_rad_buff, as_html = FALSE), 
      `EJScreen Map`    = url_ejscreenmap(        lat = data_uploaded()$lat, lon = data_uploaded()$lon,                               as_html = FALSE), 
      `ACS Report`      = url_ejscreen_acs_report(lat = data_uploaded()$lat, lon = data_uploaded()$lon, distance = input$bt_rad_buff, as_html = FALSE),
      `ECHO report` = echolink
    )]
    # out$results_overall[ , `:=`(
    #   `EJScreen Report` = NA, 
    #   `EJScreen Map`    = NA, 
    #   `ACS Report`      = NA,
    #   `ECHO report`     = NA
    # )]
    newcolnames <- c(
      "EJScreen Report", 
      "EJScreen Map", 
      "ACS Report", 
      "ECHO report"
    )
    # put those up front as first columns
    setcolorder(out$results_bysite, neworder = newcolnames)
    #setcolorder(out$results_bysite, neworder = newcolnames)
    out$longnames <- c(newcolnames, out$longnames)
    
    #############################################################################  # 
    
    ################################################################ # 
    
    # add radius to results tables (in server and in ejamit() ####
    out$results_bysite[      , radius.miles := input$bt_rad_buff]
    out$results_overall[     , radius.miles := input$bt_rad_buff]
    out$results_bybg_people[ , radius.miles := input$bt_rad_buff]
    out$longnames <- c(out$longnames , "Radius (miles)")
    
    ################################################################ # 
    
    ## assign doaggregate output to data_processed reactive 
    data_processed(out)
    
    ## update overall progress bar
    progress_all$inc(1/3, message='Step 3 of 3', detail = 'Summarizing')
    
    #############################################################################  # 
    
    # 3) **batch.summarize()** on already processed data ####
    
    outsum <- EJAMbatch.summarizer::batch.summarize(
      sitestats = data.frame(data_processed()$results_bysite),
      popstats =  data.frame(data_processed()$results_bysite),
      ## user-selected quantiles to use
      #probs = as.numeric(input$an_list_pctiles),
      threshold = list(95) # compare variables to 95th %ile
    )
    
    ## update overall progress bar
    progress_all$inc(1/3, message = 'Done processing! Loading results now', detail=NULL)
    
    # assign batch.summarize output to data_summarized reactive ### #
    data_summarized(outsum)
    
    ## close overall progress bar
    progress_all$close()
    
    ## wait  then switch tabs and jump to top of screen 
    Sys.sleep(0.2) ### why wait? ### #
    shinyjs::js$toTop();
    updateTabsetPanel(session, "all_tabs", "Summary Report")
  })
  #############################################################################  # 
  #############################################################################  # 
  #############################################################################  # 
  
  
  #############################################################################  # 
  
  ## static preview of uploaded dataset - not used currently
  # output$print_test2 <- renderTable({
  #     req(data_uploaded())
  #     
  #     if(current_upload_method() == 'NAICS'{
  #       #takes NAICS codes selected, finds NAICS descriptions, and presents them  
  #       dt_result_by_naic = data_uploaded()[, .(Count = .N), by = NAICS]
  #       naics_desc = EJAM::NAICS[EJAM::NAICS %in% dt_result_by_naic$NAICS]
  #       dt_names = data.frame("NAICS"=naics_desc,"Description"=names(naics_desc))
  #       naicsdt = merge(x = dt_result_by_naic, y = dt_names, by='NAICS')
  #       naics_reorder = data.frame(naicsdt$Description,naicsdt$Count)
  #       colnames(naics_reorder) = c("NAICS Code","Facility Count")
  #       
  #       naics_reorder
  #       #print(naics_reorder,row.names=FALSE)
  #     } else {
  #       head(data_uploaded())
  #     }
  #   })
  
  
  #############################################################################  # 
  # ~ ####
  # ______ SUMMARY IN TALL FORMAT  ####
  
  ################################################################ # 
  # just a nicer looking tall version of overall results
   # output$overall_results_tall <- renderDT({
   #   format_results_overall(results_overall = data_processed()$results_overall, longnames =  data_processed()$longnames)
   # })
  # output$overall_results_tall <- renderDT({
  #   tallout <- cbind(overall = round(unlist(data_processed()$results_overall), 3))
  #   rownames(tallout) <- fixnames_to_type(rownames(tallout), "newnames_ejscreenapi", "longname_tableheader")
  #   # rownames(tallout) <- fixnames_to_type(rownames(tallout), "jsondoc_Rfieldname", "longname_tableheader")
  #   tallout
  # })
  
  #############################################################################  # 
  # ~ ####
  # ______ AVERAGES & RATIOS TO AVG - FOR PLOTS ####
  # ~ ####
  
  # already done in doaggregate() !! - ALREADY IN data_processed()  !  and (avg.in.us) is a constant, in data.frame format.
  
  # do not bother making a copy of the state averages that are in statestats
  # EJAM::statestats[ EJAM::statestats$PCTILE == "mean", ]
  # and the overall mean and site by site means are in  unlist( data_processed()$results_overall[ , ..names_state_avg_these] )
  
  ## ** RATIOS OVERALL TO US or state D AVG ####
  #
  ## uses (doaggregate output results_overall ) / (EJAM::usastats mean in USA)
  ## (batch.summarize 'Average person' / EJAM::usastats mean in USA   )
  ## this needs further verification
  
  ratio.to.us.d    <- reactive({unlist(data_processed()$results_overall[ , c(..names_d_ratio_to_avg,       ..names_d_subgroups_ratio_to_avg)]) }) # ???
  ratio.to.state.d <- reactive({unlist(data_processed()$results_overall[ , c(..names_d_ratio_to_state_avg, ..names_d_subgroups_ratio_to_state_avg)]) }) # ???
  # ratio.to.us.d_TEST <- reactive({
  #   unlist(data_processed()$results_overall[1, ]) / 
  #     avg.in.us[, c(names_d, names_d_subgroups)]
  # })
  
  ## ** RATIOS OVERALL TO US or state E AVG  ####
  #  *****************   but these are now already calculated in doaggregate() right? as 
  ## (batch.summarize 'Average person' / EJAM::usastats mean in USA   )
  ## this needs further verification
  # we already have avg.in.us and ratios also?  
  
  ratio.to.us.e <- reactive({unlist(data_processed()$results_overall[ , ..names_e_ratio_to_avg]) })                     # ???
  # ratio.to.us.e_TEST <- reactive({ unlist(data_summarized()$rows['Average person', names_e]) / 
  #     avg.in.us[, names_e]
  #doaggregate results_overall output, if needed: unlist(data_processed()$results_overall[1, ..names_e])
  # })
  ratio.to.state.e <- reactive({unlist(data_processed()$results_overall[ , ..names_e_ratio_to_state_avg]) })                     # ???
  
  #   in plots for ejscreenapi, it may do this:
  # out <- results_table()
  # names(out) <- fixnames(names(out), mapping_for_names = map_headernames)
  # us.ratios    <- ratios_to_avg(out)
  # c(EJAM::names_d_avg, EJAM::names_d_subgroups_avg)
  # 
  # names_d_us_ratio 
  
  #############################################################################  # 
  # ~ ####
  # ______ SUMMARY REPORT_________ ####
  # ~ ####
  
  #############################################################################  # 
  ## *MAP for summary report ####
  
  report_map <- reactive({
    circle_color <- '#000080'
    
    ## similar to previous map but remove controls
    ## and only add circles, not circleMarkers
    leaflet(data_uploaded(),
            options = leafletOptions(zoomControl = FALSE, minZoom = 4)) %>% 
      addTiles()  %>%
      addCircles(
        radius = 1 * meters_per_mile,
        color = circle_color, fillColor = circle_color, 
        fill = TRUE, weight = 4,
        group = 'circles'
      )
  })
  
  ## output: summary report map  
  # output$quick_view_map <- leaflet::renderLeaflet({
  #   req(data_uploaded())
  #   
  #   ## use separate report map
  #   report_map()
  #   
  #   ## or can keep same map as on Site Selection tab
  #   # orig_leaf_map()
  # })
  
  ## update leaflet map when inputs change
  ## this is currently resetting map too often in response to checkbox
  observeEvent(eventExpr = {
    input$bt_rad_buff
    input$an_map_clusters
    is_clustered()
    #input$radius_units
  }, {
    base_color      <- '#000080'
    cluster_color   <- 'red'
    
    req(input$bt_rad_buff)
    ## convert units to miles for circle size
    # if(input$radius_units == 'kilometers'){
    #   rad <- input$bt_rad_buff * meters_per_mile * 0.62137119
    # } else {
    rad <- input$bt_rad_buff * meters_per_mile
    #}
    
    if(input$an_map_clusters == TRUE){
      ## compare latlons using is_clustered() reactive
      circle_color <- ifelse(is_clustered() == TRUE, cluster_color, base_color)
    } else {
      circle_color <- base_color
    }
    
    # if(input$circle_type == 'circles'){
    suppressMessages(
      leafletProxy(mapId = 'an_leaf_map', session, data = data_uploaded()) %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addCircles(
          radius = rad,
          color = circle_color, fillColor = circle_color,
          fill = TRUE, weight = 4,
          group = 'circles',
          # next version should use something like EJAMejscreenapi::popup_from_ejscreen(), but with EJAM column names
          popup = EJAMejscreenapi::popup_from_df(data_uploaded() %>% as.data.frame())
        )  %>%
        addCircleMarkers(
          radius = input$bt_rad_buff,
          color = circle_color, fillColor = circle_color,
          fill = TRUE, weight = 4,
          clusterOptions = markerClusterOptions(),
          group = 'markers'
          #popup = EJAMejscreenapi::popup_from_df(data_uploaded())
        ) %>%
        ## show circleMarkers (aggregated) at zoom levels 1:6
        groupOptions(group = 'markers', zoomLevels = 1:6) %>%
        ## show circles and popups at zoom levels 7:20
        groupOptions(group = 'circles', zoomLevels = 7:20) %>%
        ## allow fullscreen map view ([ ] button)
        leaflet.extras::addFullscreenControl()
    )
  })
  
  # #############################################################################  # 
  # ## *Header info on summary report ####
  # output$view1_total_pop <- renderUI({
  #   
  #   req(data_processed())
  #   
  #   ## paste header information together
  #   title_text <- paste0('<div style="font-weight: bold; font-size: 11pt; text-align: center;">',
  #                        input$analysis_title, '<br>',
  #                        'Residents within ', 
  #                        #input$bt_rad_buff, ' ', input$radius_units, ' of any of the ', 
  #                        input$bt_rad_buff, ' miles of any of the ',
  #                        prettyNum( NROW(data_processed()$results_bysite), big.mark = ","), ' sites analyzed<br>',
  #                        #    "in the xxx source category or sector<br>",
  #                        'Estimated total population: ', prettyNum( total_pop(), big.mark = ","), '</div>'
  #   )
  #   ## return formatted HTML text
  #   HTML(title_text)
  # })
  
  ## * Total population count ####
  total_pop <- reactive({
    
    req(data_processed())
    ## format and return total population
    ## some of these numbers seem very large! possible double-counting??
    round(data_processed()$results_overall$pop, 0 )
  })
  
  #############################################################################  # 
  ## *Demographic summary table #### 
  
  v1_demog_table <- reactive({
    
    req(data_processed())
    
    ## create dataframe  - vars (indicator names), value (raw indicator value),
    ## state_avg (State Avg indicator value), state_pctile (State Pctile for indicator),
    ## usa_avg (US Avg indicator value), usa_pctile (US Pctile for indicator)
    # and ratio_to_us is now included also
    
    tab_data_d <- data.frame(
      
      var_names =  c(names_d_friendly, names_d_subgroups_friendly),
      value = data_processed()$results_overall[, c(..names_d, ..names_d_subgroups)] %>% t, 
      
      'state_avg' = (data_processed()$results_overall[, c(..names_d_state_avg, ..names_d_subgroups_state_avg)] %>% t), 
      'state_pctile' = (data_processed()$results_overall[, c(..names_d_state_pctile, ..names_d_subgroups_state_pctile)] %>% t), 
      
      # 'usa_avg' = EJAM::usastats %>% filter(PCTILE == 'mean') %>% select(all_of(c(names_d, names_d_subgroups))) %>% t,     # xxx
      'usa_avg' = data.frame(usa_avg = unlist(avg.in.us[, c(names_d,names_d_subgroups)])) ,
      'usa_pctile'  = (data_processed()$results_overall[, c(..names_d_pctile, ..names_d_subgroups_pctile)]  %>% t),
      
      # note these have subgroups too already in them:
      # "state_ratio" = unlist(ratio.to.state.d()),  
      "state_ratio" = (data_processed()$results_overall[, c(..names_d_ratio_to_state_avg, ..names_d_subgroups_ratio_to_state_avg)] %>% t),     # xxx
      # "usa_ratio"   = unlist(ratio.to.us.d() )     
      "usa_ratio"   =  (data_processed()$results_overall[, c(..names_d_ratio_to_avg, ..names_d_subgroups_ratio_to_avg)] %>% t )     # xxx
    )
    
    # need to verify percentile should be rounded here or use ceiling() maybe? try to replicate EJScreen percentiles as they report them.
    tab_data_d$usa_pctile   <- round(tab_data_d$usa_pctile ,0)
    tab_data_d$state_pctile <- round(tab_data_d$state_pctile ,0)
    
    ## set colors for table
    my_cell_color <- '#dce6f0'
    my_border_color <- '#0070c0'
    
    ## apply function to format as 'gt' table  
    table_out_d <- format_gt_table(df = tab_data_d, type = 'demog',
                                   my_cell_color = my_cell_color,
                                   my_border_color = my_border_color)
    ## return table
    table_out_d
  })
  
  ## output: show table of indicators in view 1
  # output$view1_demog_table <- gt::render_gt({
  #   v1_demog_table()
  # })
  #############################################################################  # 
  ## *Environmental indicator table #### 
  
  v1_envt_table <- reactive({
    req(data_processed())
    
    ## create dataframe with 6 columns - vars (indicator names), value (raw indicator value),
    ## state_avg (State Avg indicator value), state_pctile (State Pctile for indicator),
    ## usa_avg (US Avg indicator value), usa_pctile (US Pctile for indicator)
    ## and ratios to averages for state then us
    tab_data_e <- data.frame(
      
      var_names = names_e_friendly,
      value = data_processed()$results_overall[, names_e, with=FALSE] %>% t,
      
      'state_avg' =    data_processed()$results_overall[, ..names_e_state_avg] %>% t,
      'state_pctile' = data_processed()$results_overall[, ..names_e_state_pctile] %>% t,   
      
      'usa_avg'     =   data.frame(usa_avg = unlist(avg.in.us[ , names_e])), # is a constant already 
      'usa_pctile'   = data_processed()$results_overall[, ..names_e_pctile] %>% t,  # xxx
      
      # 'state_ratio' = unlist(ratio.to.state.e()) ,
      "state_ratio" = data_processed()$results_overall[, ..names_e_ratio_to_state_avg] %>% t,
      # 'usa_ratio' = unlist(ratio.to.us.e()) 
      "usa_ratio"   =  data_processed()$results_overall[, ..names_e_ratio_to_avg] %>% t
    )
    
    
    # NEED TO CONFIRM HOW TO ROUND TO REPLICATE EJSCREEN 
    tab_data_e$usa_pctile   <- round(tab_data_e$usa_pctile,  0)
    tab_data_e$state_pctile <- round(tab_data_e$state_pctile,0)
    
    ## join long indicator names and move them to first column - done already 
    # tab_data_e <- tab_data_e %>% 
    #   left_join(long_names_e, by = c('vars') ) %>% 
    #   relocate(var_names, .before = 1) %>% 
    #   select(-vars)
    
    ## set colors for table
    my_cell_color <- '#dce6f0'
    my_border_color <- '#0070c0'
    
    ## apply function to format as 'gt' table  
    tab_out_e <- format_gt_table(df = tab_data_e, type = 'envt',
                                 my_cell_color = my_cell_color,
                                 my_border_color = my_border_color)
    ## return final table
    tab_out_e
  })
  
  ## output: show environmental indicator table
  # output$view1_envt_table <- gt::render_gt({
  #   v1_envt_table()
  # })
  #############################################################################  # 
  ## *BOXPLOTS/barplot? of demographic ratios vs US average ####
  

    # https://exts.ggplot2.tidyverse.org/gallery/
  
  
  v1_summary_plot <- reactive({
    req(data_summarized())
    
    # input$plotkind_1pager     # trying out box, bar, ridgeline
    
    if (input$plotkind_1pager == 'bar') {  # do BARPLOT NOT BOXPLOT
      
      ratio.to.us.d.overall <- ratio.to.us.d() # reactive already available
      #supershortnames <- substr(gsub(" |-|age","",gsub("People of Color","POC", c(names_d_friendly, names_d_subgroups_friendly))),1,6)
      supershortnames <- gsub(' \\(.*', '', gsub("People of Color","POC", c(names_d_friendly, names_d_subgroups_friendly)))
      names(ratio.to.us.d.overall) <- supershortnames
      ratio.to.us.d.overall[is.infinite(ratio.to.us.d.overall)] <- 0
      # use yellow/orange/red for ratio >= 1x, 2x, 3x  #  work in progress
      mycolors <- c("gray", "yellow", "orange", "red")[1+findInterval(ratio.to.us.d.overall, c(1.01, 2, 3))] 
      
  
      # barplot(ratio.to.us.d.overall,
      #         main = 'Ratio vs. US Average for Demographic Indicators',
      #         cex.names = 0.7,
      #         col = mycolors)
      
      #abline(h=1, col="gray")
      
      data.frame(name = names(ratio.to.us.d.overall),
                 value = ratio.to.us.d.overall,
                 color = mycolors) %>%
        ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = color)) +
        ggplot2::geom_bar(stat='identity') +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = 'Indicator', y = 'Ratio vs. US Average') +
        #scale_x_discrete(labels = scales::label_wrap(7)) +
        #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05), add = c(0, 0))) +
        ggplot2::theme(plot.margin=ggplot2::unit(c(1,2,1,1), "cm"), axis.text.x = ggplot2::element_text(angle = -30, hjust = 0, vjust = 1)) + # # try to do that via ggplot...
      NULL
      
        # ggplot2::ggplot(
      #   ratio.to.us.d.overall,
      #   aes(x = indicator, y = value)
      # ) +
      #   geom_boxplot() + 
      #   geom_hline(aes(yintercept = 1)) +
      #   labs(x = "",
      #        y = "Ratio of Indicator values for avg. person in selected locations\n vs. US average value",
      #        title = 'Ratio vs. US Average for Demographic Indicators') 
    } else if (input$plotkind_1pager == 'ridgeline') {
      
      # https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color
      # https://r-graph-gallery.com/294-basic-ridgeline-plot.html#shape
      # library(ggplot2)
      library(ggridges)
      library(viridis)
      library(hrbrthemes)
      
      ## ratios by site  (demog each site / demog avg in US)
      ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
        ..names_d_ratio_to_avg, 
        ..names_d_subgroups_ratio_to_avg
      )]
      ## assign column names (could use left_join like elsewhere)
      names(ratio.to.us.d.bysite) <-  c(
        names_d_friendly, 
        names_d_subgroups_friendly
      ) # long_names_d$var_names[match( names_d_fixed, long_names_d$vars)]
      ## pivot data from wide to long - now one row per indicator 
      ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>% 
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'indicator') %>% 
        ## replace Infs with NAs - these happen when indicator at a site is equal to zero
        dplyr::mutate(value = dplyr::na_if(value, Inf)) #%>% 
      # NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!
      
      # ridgeline Plot 
      ggplot(ratio.to.us.d.bysite, aes(x = `value`, y = `indicator`, fill = ..x..)) +
        geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
        scale_fill_viridis(name = "Ratio to US Overall Value", option = "C") +
        labs(title = 'Ratio to US Overall for each Demographic Indicator across these Sites') +
        theme_ipsum() +
        theme(
          legend.position="none",
          panel.spacing = unit(0.1, "lines"),
          strip.text.x = element_text(size = 8)
        )
    } else if (input$plotkind_1pager == "box") {
      # do BOXPLOT NOT BARPLOT
      
      # ****************************************************************************
      
      ## ratios by site  (demog each site / demog avg in US)
      ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
        ..names_d_ratio_to_avg, 
        ..names_d_subgroups_ratio_to_avg
      )]
      ## assign column names (could use left_join like elsewhere)
      names(ratio.to.us.d.bysite) <-  c(
        names_d_friendly, 
        names_d_subgroups_friendly
      ) # long_names_d$var_names[match( names_d_fixed, long_names_d$vars)]
      ## pivot data from wide to long - now one row per indicator 
      ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>% 
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'indicator') %>% 
        ## replace Infs with NAs - these happen when indicator at a site is equal to zero
        dplyr::mutate(value = dplyr::na_if(value, Inf)) #%>% 
      # NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!
      
      ## find max of ratios 
      max.ratio.d.bysite <- max(ratio.to.us.d.bysite$value, na.rm = TRUE)
      max.name.d.bysite <- ratio.to.us.d.bysite$indicator[which.max(ratio.to.us.d.bysite$value)]
      ## specify  upper bound for ratios (will drop values above this from graphic)
      q75.maxof75s <- max(quantile(ratio.to.us.d.bysite$value, 0.75, na.rm=TRUE),na.rm = TRUE)
      ylimit <- ceiling(q75.maxof75s) # max of 75th pctiles rounded up to nearest 1.0x?   
      max_limit <- max(3, ylimit, na.rm = TRUE) #   
      # perhaps want a consistent y limits to ease comparisons across multiple reports the user might run.
      #  If the max value of any ratio is say 2.6, we might want ylim to be up to 3.0, 
      #  if the max ratio is 1.01, do we still want ylim to be up to 3.0??
      #  if the max ratio or even max of 95th pctiles is >10, don't show it, but 
      #  what if the 75th pctile value of some indicator is >10? expand the scale to always include all 75ths.
      
      ## find 75th %ile of ratios for the indicator with the max ratio 
      q75.ratio.d.bysite <- quantile(ratio.to.us.d.bysite$value[ratio.to.us.d.bysite$indicator == max.name.d.bysite], 0.75, na.rm=TRUE)
      
      # to use for dot showing the mean ratio of each indicator
      meanratios <- data.frame(
        indicator = c(names_d_friendly, names_d_subgroups_friendly), 
        value = unlist(ratio.to.us.d()[c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg)])
      )
      
      ## paste subtitle for boxplot
      subtitle <- paste0('Within ', input$bt_rad_buff,' miles of one site, ', 
                         max.name.d.bysite, ' is ', round(max.ratio.d.bysite,1), 'x the US average\n' #,
                         # 'and 1 in 4 sites is at least ',round(q75.ratio.d.bysite,2), 'x the US average' 
      )
      ## specify # of characters to wrap indicator labels
      n_chars_wrap <- 13
      towhat_nicename <- "US Average"
      mymaintext <- paste0("Ratios to ", towhat_nicename, ", as distributed across these sites")
      
      
      ##################################################################################### #
      ## much of this is plotting code is based on EJAMejscreenapi::boxplots_ratios
      
      
      ggplot2::ggplot(
        ratio.to.us.d.bysite  ,
        # mydata, 
        aes(x = indicator, y = value )) + #, fill = indicator)) +
        ## draw boxplots
        geom_boxplot() +
        
        #  show average persons ratio to US,  for each boxplot column 
        # xxx
        # geom_point(
        #   data =  meanratios,
        #   aes(x = reorder(indicator, meanratios), y = value), colour = "orange", size=2
        # ) +
        
        ## wrap indicator labels on x axis
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set limits for ratio on y axis - use hard limit at 0, make upper limit 5% higher than max limit
        scale_y_continuous(limits = c(0,max_limit), expand = expansion(mult = c(0, 0.05))) +
        ## alternate version that clips top and bottom axes exactly at (0, max_limit)
        # scale_y_continuous(limits = c(0,max_limit), expand = c(0,0)) +
        
        ## add horizontal line at 1
        geom_hline(aes(yintercept = 1)) +
        ## set plot axis labels and titles
        labs(x = "",
             y = "Ratio of Indicator values in selected locations\n vs. US average value",
             subtitle = subtitle,
             title = 'Ratio vs. US Average for Demographic Indicators') +
        
        ## draw individual dot per site? at least for small datasets?/few facilities - removed as they cover up boxplots with large datasets
        #geom_jitter(color = 'black', size=0.4, alpha=0.9, ) +
        
        ## set color scheme ?
        # actually do not need each a color, for boxplot.
        # scale_fill_brewer(palette = 'Dark2') +
        ## alternate color scheme
        # viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        
        theme_bw() +
        theme(
          ## set font size of text
          text = ggplot2::element_text(size = 14),
          #axis.text  = ggplot2::element_text(size = 16),
          ## set font size of axis titles
          axis.title = ggplot2::element_text(size=16),
          ## center and resize plot title
          plot.title = ggplot2::element_text(size=22, hjust = 0.5),
          ## center subtitle
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          ## hide legend
          legend.position = 'none'
        )  # end of ggplot section
    } # box
    
  })
  
  ## output: show box/barplot of indicator ratios in Summary Report # 
  # output$view1_summary_plot <- renderPlot({
  #   v1_summary_plot()
  # })
  #############################################################################  # 
  # ~--------------------------- ####
  # >>>>> SUMMARY REPORT DOWNLOAD ________ ####
  # ~--------------------------- ####
  
  ## make summary report directly in shiny app and render on Summary report tab
  #summary_report_params <- eventReactive(input$gen_summary_report, {
  summary_report_params <- reactive({
      list(testmode=FALSE,
         sitecount = nrow(data_processed()$results_bysite), 
         distance = paste0(input$bt_rad_buff,' miles'), #input$radius_units),
         total_pop = prettyNum( total_pop(), big.mark = ","),
         analysis_title = input$analysis_title,
         # results     = data_processed(),  # NOT NEEDED HERE IF PASSING MAP, TABLES, AND PLOT AS PARAMS
         map         = report_map(),
         envt_table   = v1_envt_table(),
         demog_table  = v1_demog_table(),
         summary_plot = v1_summary_plot() #%>% print()
         )
  })
  
  output$rendered_summary_report <- renderUI({
   HTML(
      includeHTML(
        rmarkdown::render(app_sys('report','brief_summary.Rmd'),
                          output_dir = tempdir(),
                          params = summary_report_params())
      )
   )
  })
  
  # 1-3-page summary comparable to EJScreen report  
  output$summary_download <- downloadHandler(
    filename = ifelse(input$format1pager == "pdf", "summary_report.pdf", 'summary_report.html') ,
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "brief_summary.Rmd")
      ## copy Rmd from inst/report to temp folder
      file.copy(from = app_sys('report', 'brief_summary.Rmd'), 
                to = tempReport, overwrite = TRUE)
      #file.copy("../www/test_report1pager.Rmd", tempReport, overwrite = TRUE)
      
      isolate({  # need someone to confirm this is needed/helpful and not a problem, to isolate this.
        # Set up parameters to pass to Rmd document
        params <- list(testmode=FALSE,
                       sitecount = nrow(data_processed()$results_bysite), 
                       distance = paste0(input$bt_rad_buff,' miles'), #input$radius_units),
                       total_pop = prettyNum( total_pop(), big.mark = ","),
                       analysis_title = input$analysis_title,
                       # results     = data_processed(),  # NOT NEEDED HERE IF PASSING MAP, TABLES, AND PLOT AS PARAMS
                       map         = report_map(),
                       envt_table   = v1_envt_table(),
                       demog_table  = v1_demog_table(),
                       summary_plot = v1_summary_plot())
      })
      # [TEMPORARILY SAVE PARAMS FOR TESTING] ####
      # saveRDS(params, file="./inst/testparamsSHORT.RDS") # ############################### TEMPORARILY SAVE PARAMS FOR TESTING###### # 
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      # if (input$format1pager == "html") {
      output_format <- "html_document"
      # }
      if (input$format1pager == "pdf") {  output_format <- "pdf_document"}
      rmarkdown::render(tempReport, 
                        output_format = output_format, # 'html_document' or pdf_document
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        intermediates_dir = tempdir()
      )
      # if (input$format1pager == "pdf") {  # was not working to just set output_format="pdf_document"
      #   ## alternative ways to save PDFs #### 
      #   # - these go to local folders
      #   ## add line to save html output as pdf
      #     pdf(file = 'summary_report.pdf')
      #     file
      #     dev.off()
      #   # webshot::webshot(url = file, file = file.path(tempdir(), 'summary_report.pdf'))
      # }
    }
  )
  
  #############################################################################  # 
  # ~ ####
  # ______ SOME EXEC SUM TEXT  ####
   
  ## output: demographic executive summary text ### #
  # output$exec_summ_d <- renderUI({
  #   
  #   req(data_summarized())
  #   
  #   ## can use a dropdown to select indicator, or..
  #   ## or just use the most extreme one
  #   type <- 'max' # 'choose'
  #   
  #   ## if only care about max indicator - this is not currently used in favor of the 
  #   ## dropdown option (type == 'choose')
  #   if(type == 'max'){
  #     max.ratio.d <- max(ratio.to.us.d())
  #     max.name.d <- names(ratio.to.us.d())[which.max(ratio.to.us.d())]
  #     
  #     max.name.d.friendly <- EJAMbatch.summarizer::names_d_batch_friendly[which.max(ratio.to.us.d())]  # xxx
  #     
  #     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',max.name.d)]
  #     
  #     exec_text_d <-paste0(
  #       'Key demographic factor: <strong>',
  #       max.name.d.friendly, 
  #       '</strong><br>',
  #       'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
  #       ' of any of) these ', nrow(data_processed()$results_bysite),
  #       ' sites are <strong>', round(max.ratio.d, 1), ' times</strong> as likely to be ', 
  #       max.name.d.friendly, ' as the average person in the US (',
  #       round(100 * data_summarized()$rows['Average person', max.name.d]), '% vs. ', 
  #       round(100 * avg.in.us[,max.name.d]),
  #       '%). The other demographic indicators have lower ratios.',
  #       '<br>',
  #       'The median (50th percentile) site here is at the <strong>', 
  #       scales::label_ordinal()(median.pctile.in.us),
  #       '</strong> percentile of all US residents for ', max.name.d.friendly
  #     )
  #   } else if(type == 'choose'){
  #     
  #     cur.name.d <-  input$key_ind_d # names(ratio.to.us.d())[which.max(ratio.to.us.d())]
  #     cur.ratio.d <- ratio.to.us.d()[cur.name.d]
  #     
  #     ## get friendly version of D name
  #     cur.name.d.friendly <- EJAMejscreenapi::map_headernames %>%        # replace with EJAM::names_d_friendly, names_d_subgroups_friendly  XXX
  #       filter(newnames_ejscreenapi == cur.name.d) %>% 
  #       pull(names_friendly)
  #     
  #     ## grab D percentile of median site ##  ##
  #     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',cur.name.d)]
  #     
  #     exec_text_d <-paste0(
  #       'Key demographic factor: <strong>',
  #       cur.name.d.friendly, 
  #       '</strong><br>',
  #       'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
  #       ' of any of) these ', nrow(data_processed()$results_bysite),
  #       ' sites have, on average, <strong>', round(cur.ratio.d, 1), ' times</strong> as high indicator values for <strong>', 
  #       cur.name.d.friendly, '</strong> as the average person in the US (',
  #       round(100 * data_summarized()$rows['Average person', cur.name.d]), '% vs. ', 
  #       round(100 * avg.in.us[,cur.name.d]),
  #       '%)',
  #       '<br>',
  #       'The median (50th percentile) site here is at the <strong>', 
  #       scales::label_ordinal()(median.pctile.in.us),
  #       '</strong> percentile of all US residents for <strong>', cur.name.d.friendly, '</strong>'
  #     )
  #   }
  #   
  #   HTML(exec_text_d)
  #   ## need to add state multiplier?
  #   ## need to add top X% stat?
  # })
  # 
  # ## _  output: environmental executive summary text ###  #
  # output$exec_summ_e <- renderUI({
  #   
  #   req(data_summarized())
  #   
  #   ## can use a dropdown to select indicator
  #   ## or just use the most extreme one
  #   type <- 'choose' #'max'
  #   
  #   
  #   ## if only care about max indicator - this is not currently used in favor of the 
  #   ## dropdown option (type == 'choose')
  #   if(type == 'max'){
  #     max.ratio.e <- max(ratio.to.us.e())
  #     max.name.e <- names(ratio.to.us.e())[which.max(ratio.to.us.e())]
  #     
  #     max.name.e.friendly <- EJAM::names_e_friendly[which.max(ratio.to.us.e())]
  #     
  #     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',max.name.e)]    # *** SHOULD DOUBLE CHECK THIS 
  #     
  #     exec_text_e <-paste0(
  #       'Key environmental factor: <strong>',
  #       max.name.e.friendly, 
  #       '</strong><br>',
  #       'People who live near (within ', input$bt_rad_buff,' miles', #input$radius_units,
  #       ' of any of) these ', nrow(data_processed()$results_bysite),
  #       ' sites have, on average, <strong>', round(max.ratio.e, 1), ' times</strong> as high indicator values for <strong>', 
  #       max.name.e.friendly, '</strong> as the average person in the US (',
  #       
  #       round(data_summarized()$rows['Average person', max.name.e], 2), ' vs. ',    # *** SHOULD DOUBLE CHECK THIS 
  #       round(avg.in.us[,max.name.e], 2),
  #       
  #       '). The other environmental indicators have lower ratios.',
  #       '<br>',
  #       'The median (50th percentile) site here is at the <strong>', 
  #       scales::label_ordinal()(median.pctile.in.us),
  #       '</strong> percentile of all US residents for <strong>', max.name.e.friendly
  #     )
  #     
  #   } else if(type == 'choose'){
  #     cur.name.e <- input$key_ind_e # names(ratio.to.us.d())[which.max(ratio.to.us.d())]
  #     cur.ratio.e <- ratio.to.us.e()[cur.name.e]
  #     
  #     cur.name.e.friendly <- EJAMejscreenapi::map_headernames %>%    #  # replace with EJAM::names_e_friendly XXX
  #       filter(newnames_ejscreenapi == cur.name.e) %>% 
  #       pull(names_friendly)
  #     
  #     ## _ grab E percentile of median site ###  #
  #     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',cur.name.e)]
  #     
  #     exec_text_e <-paste0(
  #       'Key environmental factor: <strong>',
  #       cur.name.e.friendly, 
  #       '</strong><br>',
  #       'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
  #       ' of any of) these ', nrow(data_processed()$results_bysite),
  #       ' sites have, on average, <strong>', round(cur.ratio.e, 1), ' times</strong> as high indicator values for ', 
  #       cur.name.e.friendly, ' as the average person in the US (',
  #       round( data_summarized()$rows['Average person', cur.name.e], 2), ' vs. ', 
  #       round(avg.in.us[,cur.name.e], 2),
  #       ')',
  #       '<br>',
  #       'The median (50th percentile) site here is at the <strong>', 
  #       scales::label_ordinal()(median.pctile.in.us),
  #       '</strong> percentile of all US residents for <strong>', cur.name.e.friendly, '</strong>'
  #     )
  #   }
  #   
  #   HTML(exec_text_e)
  #   ## need to add state multiplier?
  #   ## need to add top X% stat?
  # })
  
  #############################################################################  # 
  # ~ ####
  # ______ SITE-BY-SITE TABLE ________ ####
  #output: site by site datatable 
  output$view3_table <- DT::renderDT(server = TRUE, expr = {
    req(data_processed())
    
    # --------------------------------------------------- #
    # cols_to_select <- names(data_processed)
    # friendly_names <- longnames???
    cols_to_select <- c('siteid',  'pop', 'EJScreen Report', 'EJScreen Map', 'ACS Report', 'ECHO report',
                        EJAMbatch.summarizer::names_all)
    friendly_names <- c('Site ID', 'Est. Population',  'EJScreen Report', 'EJScreen Map', 'ACS Report', 'ECHO report',
                        EJAMbatch.summarizer::names_all_friendly, 
                        'State', 'EPA Region', '# of indicators above 95% threshold')
    # --------------------------------------------------- #
    
    # dt_overall <- data_processed()$results_overall %>% 
    #   as.data.frame() %>% 
    #   dplyr::mutate(siteid = 'All sites', ST = NA,
    #          across(where(is.numeric), .fns = function(x) {round(x, digits=2)})) %>% 
    #   dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    dt <- data_processed()$results_bysite %>% 
      as.data.frame() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), .fns = function(x) {round(x, digits=2)})#,
             #siteid = as.character(siteid)
             ) %>%
      dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    dt$`EJScreen Report` <- EJAMejscreenapi::url_linkify(dt$`EJScreen Report`, text = 'EJScreen Report')
    dt$`EJScreen Map` <- EJAMejscreenapi::url_linkify(dt$`EJScreen Map`, text = 'EJScreen Map')
    dt$`ACS Report` <- EJAMejscreenapi::url_linkify(dt$`ACS Report`, text = 'ACS Report')
    dt$`ECHO report` <- ifelse(!is.na(dt$`ECHO report`), EJAMejscreenapi::url_linkify(dt$`ECHO report`, text = 'ECHO Report'), 
                                      'N/A')
    
    # dt_avg <- data_summarized()$rows[c('Average person','Average site'),] %>% 
    #   dplyr::mutate(siteid = c('Average person', 'Average site'), ST = NA,
    #                 dplyr::across(dplyr::where(is.numeric), .fns = function(x) {round(x, digits=2)}),
    #          siteid = as.character(siteid)) %>%
    #   dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    dt_final <- dt %>% 
      dplyr::bind_cols(data_summarized()$cols) %>% 
      ## hide summary rows from table
      #dplyr::bind_rows(dt_avg) %>% 
      #dplyr::bind_rows(dt_overall) %>% 
      ## sort by Site ID - as numeric index
      dplyr::arrange(siteid) %>% 
      #dplyr::arrange(dplyr::desc(pop)) %>% 
      dplyr::mutate(pop = prettyNum(pop, big.mark = ',')) %>% 
      dplyr::left_join(EJAM::stateinfo %>% dplyr::select(ST, statename, REGION), by = 'ST') %>% 
      dplyr::select(-ST, -Max.of.variables)
    
    colnames(dt_final) <- friendly_names
    
    dt_final <- dt_final %>% 
      dplyr::relocate(c(State, 'EPA Region', '# of indicators above 95% threshold'), .before = 2)
    
    n_cols_freeze <- 5
    
    ## format data table
    # see also  EJAM/inst/notes_MISC/DT_datatable_tips_options.R
    
    DT::datatable(dt_final, rownames = FALSE, 
                  ## add column filters 
                  filter = 'top',
                  ## allow selection of one row at a time (remove to allow multiple)
                  selection = 'single',
                  ## add-in for freezing columns
                  extensions = c('FixedColumns'),
                  options = list(
                    ## column width
                    autoWidth = TRUE,
                    ## freeze header row when scrolling down
                    fixedHeader = TRUE, 
                    fixedColumns = list(leftColumns = n_cols_freeze),
                    pageLength = 100, 
                    ## allow scroll left-to-right
                    scrollX = TRUE, 
                    ## set scroll height up and down
                    scrollY = '500px'
                  ),
                  ## set overall table height
                  height = 1000, 
                  escape = FALSE  # escape=FALSE may add security issue but makes links clickable in table
    )
    ## code for bolding certain rows - not currently used
    #           ) %>% 
    # DT::formatStyle(
    #   valueColumns = 'Site ID',
    #   target = 'row', columns = 'all',
    #   fontWeight = DT::styleEqual(c('All sites','Average person','Average site'), values = 'bold')
    # )
    
  })
  #############################################################################  # 
  
  # ______ EXCEL DOWNLOAD of site-by-site results ####
  
  output$download_results_table <- downloadHandler(
    filename = function(){'results_table.xlsx'},
    content = function(fname){
      
      ## previous - use EJAMejscreenapi::xls_formatting_api approach
      # 
      #table_as_displayed <- data_processed()$results_bysite
      # pctile_colnums <- which(EJAMejscreenapi::map_headernames$jsondoc_shortvartype[match(names(table_as_displayed), 
      #                                                                                     EJAMejscreenapi::map_headernames$newnames_ejscreenapi)] == 'pctile')
      # longnames <- EJAMejscreenapi::map_headernames$longname_tableheader[match(names(data_processed()$results_bysite),
      #                                                                          EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
      # 
      #names(table_as_displayed) <- ifelse(!is.na(longnames), longnames, names(table_as_displayed))
      # wb_out <- xls_formatting(df = table_as_displayed,
      #                               heatmap_colnames = names(table_as_displayed)[pctile_colnums],
      #                               heatmap_cuts=c(80, 90, 95),
      #                               heatmap_colors=c('yellow', 'orange', 'red'))
      
      ## use EJAM::workbook_ouput_styled approach
      ## future: can add other sheets from doaggregate output
      
      # Recode this to avoid making copies which slows it down:?
      table_overall <- copy(data_processed()$results_overall)
      table_bysite  <- copy(data_processed()$results_bysite)
      # table_summarized <- copy(data_processed()$results_summarized)
      # table_bybg_people <- data_processed()$results_bybg_people   # large table !!
      table_overall <- table_overall %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
      table_bysite <- table_bysite %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
      
      ## attempt to clean up some column names xxx - CHECK THIS 
      # longnames_TEST <- EJAMejscreenapi::map_headernames$longname_tableheader[match(names(data_processed()$results_bysite),
      # EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
      longnames <- data_processed()$longnames
      longnames_no_url <- setdiff(longnames, c('EJScreen Report','EJScreen Map','ACS Report','ECHO report'))
      names(table_overall) <- ifelse(!is.na(longnames_no_url), longnames_no_url, names(table_overall))
      names(table_bysite)  <- ifelse(!is.na(longnames), longnames, names(table_bysite))
      # table_summarized
      # names(table_bybg_people) # CANNOT REALLY TREAT THIS THE SAME - HAS DIFFERENT LIST OF INDICATORS THAN THE OTHER TABLES
      
      ## format excel workbook
      wb_out <- xls_formatting2(
        overall = table_overall, 
        eachsite = table_bysite  # the function does not handle the other two
        # eachblockgroup = table_bybg_people, 
        # summaryofoverall = table_summarized # could use longnames for it
        # data_processed()$results_bybg_people 
      )
      #        
      
      # > names( data_processed() )
      # [1] "results_overall"                     "results_bysite"                     
      # [3] "results_bybg_people"                 "longnames"                          
      # [5] "count_of_blocks_near_multiple_sites" "results_summarized"  
      
      ## add v1_summary_plot() to 'plot' sheet of Excel download
      ## will be moved to eventual merged 'xls_formatting' function
      ggsave(filename = paste0(tempdir(), '/', 'summary_plot.png'), plot = v1_summary_plot(),
             height = 7, width = 9, units = 'in')
      openxlsx::insertImage(wb_out, sheet = 'plot', 
                            file = paste0(tempdir(), '/', 'summary_plot.png'),
                            width = 9, height = 7)
      
      ## save file and return for downloading
      openxlsx::saveWorkbook(wb_out, fname)
    }
  )
  
  #############################################################################  # 
  # ~ ###  #
  # ______ MAP RESULTS ______ sites selected from site-by-site summary table ####
  
  # data_sitemap <- reactiveVal(NULL)
  # 
  # observeEvent(input$view3_table_rows_selected,{
  #   req(data_processed())
  #   data_sitemap(data_uploaded()[input$view3_table_rows_selected,])
  # })
  # 
  # output$v3_sitemap <- leaflet::renderLeaflet({
  #   ## wait for row to be selected
  #   ## note: summary rows are currently mapped but don't have a point location to map
  #   validate(
  #     need(!is.null(input$view3_table_rows_selected),
  #          'Select a specific site in the table to see its location'
  #     )
  #   )
  #   ## zoom in from original map to show single point (can zoom out and see others)
  #   orig_leaf_map() %>% 
  #     leaflet::setView(lng = data_sitemap()$lon, lat = data_sitemap()$lat, zoom = 8)
  #   ## alternate: plot single point individually on map (cannot zoom out and see others)
  #   # leaflet(data_sitemap()) %>%
  #   #   setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 13) %>%
  #   #   addTiles() %>%
  #   #   addCircles(radius = 1 *  meters_per_mile)
  # })
  #############################################################################  # 
  # ~ ####
  # ______ BARPLOT _____ ####
  # ~ ####
  
  
  # https://exts.ggplot2.tidyverse.org/gallery/
  
  
  # output: 
  output$summ_display_bar <- renderPlot({
    req(data_summarized())
    
    ## set indicator group column names
    mybarvars <- switch(input$summ_bar_ind,
                        'Demographic'   = c(names_d, names_d_subgroups),
                        'Environmental' = EJAM::names_e,
                        'EJ'            = EJAM::names_ej 
    )
    
    ## set indicator group friendly names  
    mybarvars.friendly <- switch(input$summ_bar_ind,
                                 'Demographic'   = c(names_d_friendly, names_d_subgroups_friendly),
                                 'Environmental' = names_e_friendly,
                                 'EJ'            = names_ej_friendly
    )
    
    ## only using average for now
    mybarvars.stat <- 'avg' #'med'
    
    ## defaulting to average only in this version of EJAM
    mybarvars.sumstat <- c('Average site', 'Average person')
    
    ## if adding median ('med') back in future, can use this
    #mybarvars.stat <- input$summ_bar_stat
    # mybarvars.sumstat <- switch(input$summ_bar_stat,
    #                             'med' =  c('Median site', 'Median person'),
    #                             'avg' = c('Average site','Average person')
    # )
    
    ## filter to necessary parts of batch.summarize output
    barplot_data <- data_summarized()$rows %>% 
      tibble::rownames_to_column(var = 'Summary') %>% 
      dplyr::filter(Summary %in% mybarvars.sumstat)
    
    ## set ggplot theme elements for all versions of barplot
    ggplot_theme_bar <- theme_bw() +
      theme(legend.position = 'top',
            axis.text = ggplot2::element_text(size = 16),
            axis.title = ggplot2::element_text(size = 16),
            legend.title = ggplot2::element_text(size = 16),
            legend.text = ggplot2::element_text(size = 16),
            strip.text = element_blank(),
            strip.background = element_blank()
      )
    
    ## raw data 
    if(input$summ_bar_data == 'raw'){
      
      ## pivot from wide to long, 1 row per indicator 
      barplot_data_raw <- barplot_data %>% 
        dplyr::select(Summary, dplyr::all_of( mybarvars)) %>% 
        tidyr::pivot_longer(cols = -1, names_to = 'indicator') %>% 
        dplyr::mutate(type = 'raw')
      
      ## median - not currently displayed
      if(mybarvars.stat == 'med'){
        barplot_usa_med <- EJAM::usastats %>% 
          dplyr::filter(REGION == 'USA', PCTILE == 50) %>% # for median
          dplyr::mutate(Summary = 'Median person in US') %>% 
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
          tidyr::pivot_longer(-Summary, names_to = 'indicator')
        
        ## NOTE: Median Person calculations are all 0s for now!
        barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_med)
        
        ## average  
      } else {
        barplot_usa_avg <- EJAM::usastats %>% 
          dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>% 
          dplyr::mutate(Summary = 'Average person in US') %>% 
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
          tidyr::pivot_longer(-Summary, names_to = 'indicator')
        
        barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_avg)
      }
      
      ## set # of characters to wrap labels
      n_chars_wrap <- 15
      
      ## merge with friendly names and plot
      barplot_input %>% 
        left_join( data.frame(indicator = mybarvars, indicator_label = mybarvars.friendly)) %>% 
        ggplot() +
        geom_bar(aes(x = indicator_label, y = value, fill = Summary), stat='identity', position='dodge') +
        #viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        scale_fill_brewer(palette = 'Dark2') +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
        facet_wrap(~indicator_label, ncol = 4, scales = 'free_x') +
        labs(x = '', y = 'Indicator Value') +
        ggplot_theme_bar
      
      ## future: add % scaling and formatting for demographic indicators
      ## see ggplot2::scale_y_continuous and scales::label_percent
      
      ## ratio to us  
    } else if(input$summ_bar_data == 'ratio'){
      
      barplot_data_raw <- barplot_data %>% 
        dplyr::select(Summary, dplyr::all_of( mybarvars)) %>% 
        tidyr::pivot_longer(cols = -1, names_to = 'indicator') 
      
      ## average
      if(mybarvars.stat == 'avg'){
        ## pull US average values from EJAM::usastats to compute ratios
        barplot_usa_avg <-  dplyr::bind_rows(
          EJAM::usastats %>% 
            dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>% 
            dplyr::mutate(Summary = 'Average person') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          EJAM::usastats %>% 
            dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>% 
            dplyr::mutate(Summary = 'Average site') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
        )
        
        ## combine raw data with US averages 
        barplot_input <- dplyr::left_join(
          barplot_data_raw, 
          barplot_usa_avg
        ) %>% 
          ## divide to get ratios
          dplyr::mutate(ratio = value / usa_value) %>% 
          ## add row of all 1s to represent US average ratio being constant at 1
          dplyr::bind_rows(
            data.frame(Summary = 'Average person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
          )
        
      } else {
        ## median - not currently displayed
        barplot_usa_med <-  dplyr::bind_rows(
          EJAM::usastats %>% 
            dplyr::filter(REGION == 'USA', PCTILE == 50) %>% 
            dplyr:: mutate(Summary = 'Median person') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          EJAM::usastats %>% 
            dplyr::filter(REGION == 'USA', PCTILE == 50) %>% 
            dplyr::mutate(Summary = 'Median site') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
        )
        
        barplot_input <- dplyr::left_join(barplot_data_raw, barplot_usa_med) %>% 
          ## calc ratio
          dplyr::mutate(ratio = value / usa_value) %>% 
          dplyr::bind_rows(
            data.frame(Summary = 'Median person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
          )
      }
      
      ## set # of characters to wrap labels
      n_chars_wrap <- 15
      
      ## join and plot
      barplot_input %>% 
        dplyr::left_join( data.frame(indicator = mybarvars, indicator_label =  mybarvars.friendly)) %>% 
        ggplot() +
        ## add bars - position = 'dodge' places the 3 categories next to each other
        geom_bar(aes(x = indicator_label, y = ratio, fill = Summary), stat='identity', position='dodge') +
        ## add horizontal line at 1
        geom_hline(aes(yintercept = 1)) +
        ## set color scheme
        scale_fill_brewer(palette = 'Dark2') +
        ## alternate color scheme
        #viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        ## wrap long indicator labels on x axis
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
        ## set axis labels
        labs(x = '', y = 'Indicator Ratio') +
        ## break plots into rows of 4 
        facet_wrap(~indicator_label, ncol = 4, scales='free_x') +
        ggplot_theme_bar
    }
  })
  
  #############################################################################  # 
  # ______ HISTOGRAM _____  ####
  # ~ ####
  
  
  # https://exts.ggplot2.tidyverse.org/gallery/
  
  
  ## output: 
  output$summ_display_hist <- renderPlot({
    
    req(data_summarized())
    
    ## set font sizes
    ggplot_theme_hist <- theme(
      plot.title = ggplot2::element_text(size=18, hjust=0.5),
      axis.text = ggplot2::element_text(size=16),
      axis.title = ggplot2::element_text(size=16)
    )
    
    ## future settings: bin sizes, reference lines
    
    if(input$summ_hist_distn == 'Sites'){
      if(input$summ_hist_data == 'raw'){
        
        ## subset doaggregate results_bysite to selected indicator
        hist_input <- data_processed()$results_bysite[, input$summ_hist_ind, with=FALSE]
        names(hist_input)[1] <- 'indicator'
        
        ## plot histogram
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x = '',
            y = '',
            title = 'Histogram of Raw Indicator Values Across Sites'
          ) +
          theme_bw() +
          ggplot_theme_hist
        
      } else if(input$summ_hist_data == 'pctile'){
        
        ## subset doaggregate results_bysite to selected indicator
        hist_input <- data_processed()$results_bysite[, paste0('pctile.',input$summ_hist_ind), with=FALSE]
        names(hist_input)[1] <- 'indicator'
        
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          labs(
            x='',
            y = '',
            title = 'Histogram of US Percentile Indicator Values Across Sites'
          ) +
          theme_bw() +
          ggplot_theme_hist
      }
    } else if(input$summ_hist_distn == 'People'){
      if(input$summ_hist_data == 'raw'){
        
        ## subset doaggregate results_bysite to selected indicator
        hist_input <- data_processed()$results_bysite[, c('pop', input$summ_hist_ind), with=FALSE]
        names(hist_input)[2] <- 'indicator'
        
        ## plot population weighted histogram
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator, y = after_stat(density), weight = pop), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x='',
            y='Weighted Density',
            title = 'Population Weighted Histogram of Raw Indicator Values'
          ) +
          theme_bw() +
          ggplot_theme_hist
        
      } else if(input$summ_hist_data == 'pctile'){
        
        ## subset doaggregate results_bysite to selected indicator
        hist_input <- data_processed()$results_bysite[, c('pop',paste0('pctile.',input$summ_hist_ind)), with=FALSE]
        names(hist_input)[2] <- 'indicator'
        
        ## plot population weighted histogram 
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator, y = after_stat(density), weight = pop), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x='',
            y='Weighted Density',
            title = 'Population Weighted Histogram of US Percentile Values'
          ) +
          theme_bw() +
          ggplot_theme_hist
      }
    }
  })
  
  ## format textinput in Full Report tab using current input radius
  output$rg_enter_miles <- renderUI({   #   E.G.,   within 10 miles of
    
    shiny::textInput(inputId = "rg_enter_miles", 
                     label = "Analysis Location:", 
                     value = paste0("within ", input$bt_rad_buff,
                                    ' miles of')#,
                     #input$radius_units, " of")
    )
  })
  
  
  #############################################################################  # 
  # ~--------------------------- ####
  # >>>>>  FULL REPORT DOWNLOAD _________ ####
  # ~--------------------------- ####
  
  
  ## show modal with report outline ####
  # observeEvent(input$show_outline, {    # I DONT THINK THIS POPUP OUTLINE LOOKS GREAT AND IT IS HARD TO KEEP IN SYNC WITH ACTUAL RMD OUTLINE
  #   showModal(
  #     modalDialog(
  #       #HTML(report_outline)
  #       HTML(
  #         stringr::str_replace(report_outline, 
  #                              'Broad overview of findings',
  #                              '<mark>Broad overview of findings</mark>'
  #         )
  #       )
  #     )
  #   )
  # })
  
  ## code for storing all shiny input values - not used currently
  # observeEvent(input$all_tabs == 'Generate Report',
  #  {
  #    list_of_inputs <- reactiveValuesToList(input)
  #  })
  
  ## Create and download FULL static report ####
  output$rg_download <- downloadHandler(
    filename = 'report.doc',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      ## copy Rmd from inst/report to temp folder
      file.copy(from = app_sys('report', 'report.Rmd'),
                to = tempReport, overwrite = TRUE)
      ## pass image and bib files needed for knitting to temp directory
      for(i in list.files(app_sys('report'), pattern = '.png|.bib')){
        file.copy(from = app_sys('report', i),
                  to = file.path(tempdir(), i), 
                  overwrite = TRUE)
      }
      
      # Set up parameters to pass to Rmd document - 
      #  MAKE SURE all parameter names are used (identical names, & all are there) in these 4 places: 
      #  1. input$ ids in app_ui.R, from user, to customize the long report
      #  2. params$ list passed by app_server.R to render the Rmd doc
      #  3. params: accepted in  .Rmd yaml info header 
      #  4. params$  as used within body of  .Rmd text inline and in r code blocks.
      
      isolate({ # need someone to confirm this is needed/helpful and not a problem, to isolate this.
      
        params <- list(
          testmode=TRUE,
          
          #------- WHERE was analyzed? (where/ what sector/zones/types of places)
          
          analysis_title =  input$analysis_title,
          zonetype =  input$rg_zonetype,
          where = input$rg_enter_miles,
          distance = paste0(input$bt_rad_buff,' miles'), #input$radius_units),
          sectorname_short = input$rg_enter_sites,
          ## allow for either or
          in_the_x_zone = ifelse(nchar(input$in_the_x_zone_enter) > 0, 
                                 input$in_the_x_zone_enter,
                                 input$in_the_x_zone),
          facilities_studied = ifelse(nchar(input$facilities_studied_enter) > 0, 
                                      input$facilities_studied_enter,
                                      input$facilities_studied),
          within_x_miles_of = paste0("within ", paste0(input$bt_rad_buff,' miles'), " of"),
          
          in_areas_where = paste0(input$in_areas_where, ' ', input$in_areas_where_enter),
          risks_are_x = input$risks_are_x,
          source_of_latlons = input$source_of_latlons,
          sitecount = nrow(data_processed()$results_bysite),
          
          #------- RESULTS (tables and map and plots)
          
          total_pop  = prettyNum( total_pop(), big.mark = ","),
          results =  data_processed(),  # do we need to pass the entire table? may want to use it in appendices, etc.
          results_formatted =  format_results_overall(data_processed()$results_overall, data_processed()$longnames),  
          map =  report_map(),
          # map_placeholder_png=                 "map_placeholder.png",
          envt_table =  v1_envt_table(),
          # envt_table_placeholder_png=   "envt_table_placeholder.png",
          # envt_table_placeholder_rda=   "envt_table_placeholder.rda",
          demog_table = v1_demog_table(),
           # demog_table_placeholder_png="demog_table_placeholder.png",
          # demog_table_placeholder_rda= "demog_table_placeholder.rda",
          boxplot =     v1_summary_plot(),
          # boxplot_placeholder_png=         "boxplot_placeholder.png",
          # barplot= NA
          # barplot_placeholder_png=         "barplot_placeholder.png",
           
          #------- TEXT PHRASES DESCRIBING AND INTERPRETING RESULT 
          
          demog_how_elevated = input$demog_how_elevated,
          envt_how_elevated = input$envt_how_elevated,
          demog_high_at_what_share_of_sites = input$demog_high_at_what_share_of_sites,
          envt_high_at_what_share_of_sites = input$envt_high_at_what_share_of_sites,
          conclusion1 = input$conclusion1,
          conclusion2 = input$conclusion2,
          conclusion3 = input$conclusion3,
          
          #------- METHODS, AUTHORS, ETC.
          
          authorname1 =    input$rg_author_name,
          authoremail1 =   input$rg_author_email,
          coauthor_names = input$coauthor_names, 
          coauthor_emails = input$coauthor_emails,
          fundingsource = input$fundingsource,   # need to add input
          acs_version =  "2016-2020",
          ejscreen_version =  "2.1"
        )
      })
      # [TEMPORARILY SAVE PARAMS FOR TESTING] ####
      # saveRDS(params, file="./inst/testparams.RDS") ################################ TEMPORARILY SAVE PARAMS FOR TESTING# # 
      
      # Knit report to Word Doc ####
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_format = 'word_document',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        intermediates_dir = tempdir()
      )
    }
  )
}

