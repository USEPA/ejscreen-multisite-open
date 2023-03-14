#' EJAM app server
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom data.table ":="
#' @noRd
app_server <- function(input, output, session) {
  
  # Note: how to avoid or still have a global.R file in the golem approach, 
  # https://github.com/ThinkR-open/golem/issues/6
  
  ## update ss_select_NAICS input options
  updateSelectizeInput(session, inputId = 'ss_select_naics',
                       ## use list version, grouped by first two code numbers
                       #choices = naics_as_list, # need to keep formatting
                       choices = EJAM::NAICS, 
                       server = TRUE)
  
  
  ## hide advanced settings tab by default
  hideTab(inputId = 'all_tabs', target = 'Advanced Settings')
  
  ## show advanced settings tab on button click (button in 'About EJAM' tab)
  observeEvent(input$show_advanced_settings,
               {
                 showTab(inputId = 'all_tabs', target = 'Advanced Settings')
               })
  
  ## show welcome modal on app startup - commented out for now
  # showModal(
  #   modalDialog(
  #     title = 'Welcome to EJAM!', 
  #     "Add basic message here",
  #     footer = actionButton('close_welcome', 'Close')
  #   )
  # )
  ## close welcome modal on button click
  # observeEvent(input$close_welcome,{
  #   removeModal()
  # })

  ## define current upload method using radio button
  current_upload_method <- reactive({
    switch(
      input$ss_choose_method,
      latlon = 'Location (lat/lon)',
      FRS = 'FRS',
      ECHO = 'ECHO',
      NAICS = 'NAICS'
    )
  })
  
  ## reactive: read uploaded latlon
  data_up_latlon <- reactive({
    
    ## wait for file to be uploaded
    req(input$ss_upload_latlon)
    
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
        EJAM::latlon_df_clean() %>% 
        data.table::as.data.table()
      
    }else{
      ## if not matched, show this message instead
      shiny::validate('No coordinate columns found.')
    }
  })
  
  ## reactive: data uploaded by FRS IDs
  data_up_frs <- reactive({
    
    ## wait for file to be uploaded
    req(input$ss_upload_frs)
    
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_frs$name)

    ## if acceptable file type, read in; if not, send warning text
    read_frs <- switch(ext,
                  csv =  read.csv(input$ss_upload_frs$datapath),
                  xls = read_excel(input$ss_upload_frs$datapath),
                  xlsx = read_excel(input$ss_upload_frs$datapath),
                  shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    
    #include frs_is_valid verification check function
    if (frs_is_valid(read_frs)){
    
      read_frs_dt <- data.table::as.data.table(read_frs)
    
      #converts registry id to character if not already in that class (EJAMfrsdata::frs registry ids are character)
      if(class(read_frs_dt$REGISTRY_ID) != "character"){
        read_frs_dt$REGISTRY_ID = as.character(read_frs_dt$REGISTRY_ID)
      }
      
      frs_lat_lon <- merge(x = read_frs_dt, y = EJAMfrsdata::frs, by.x='REGISTRY_ID', by.y='REGISTRY_ID', all.x=TRUE)
      
    }else{
      shiny::validate('Records with invalid Registry IDs')
    }
    
    ## return merged dataset
    frs_lat_lon
   
  })
  
  ## reactive: data uploaded by NAICS
  data_up_naics <- reactiveVal(NULL)
  
  ## when NAICS submit button is pressed
  observeEvent(input$submit_naics, {

    ## check if anything has been selected or entered
    req(shiny::isTruthy(input$ss_enter_naics) || shiny::isTruthy(input$ss_select_naics))

    #define inputs
    naics_user_wrote_in_box <- input$ss_enter_naics
    naics_user_picked_from_list <- input$ss_select_naics
    
    #NAICS validation function to check for non empty NAICS inputs
    if(NAICS_validation(input$ss_enter_naics,input$ss_select_naics)){
      inputnaics = {}
      #splits up comma separated list if user manually inserts list
      if(nchar(input$ss_enter_naics)>0){
        print('test2')
        #checks for non-numeric values in text box; if they are not numeric, then search by name
        if(!grepl("^\\d+(,\\d+)*$",input$ss_enter_naics)){
          print('test')
          inputnaics = naics_find(input$ss_enter_naics)
          if(length(inputnaics) == 0 | all(is.na(inputnaics))){
            ################ Should output something saying no valid results returned #########
            shiny::validate('No Results Returned')
          }        
        }else{
          naics_wib_split <- as.list(strsplit(naics_user_wrote_in_box, ",")[[1]])
          print(naics_wib_split)
        }
      }else{
        naics_wib_split <- ""
      }
      # if not empty, assume its pulled using naics_find above
      if(length(inputnaics) == 0 | is_empty(inputnaics)){
        #construct regex expression and finds sites that align with user-selected naics codes
        inputnaics <- c(naics_wib_split, naics_user_picked_from_list)
        inputnaics <- unique(inputnaics[inputnaics != ""])
        inputnaics <- paste("^", inputnaics, collapse="|")   ### the NAICS specified by user
        inputnaics <- stringr::str_replace_all(string = inputnaics, pattern = " ", replacement = "")
        print(inputnaics)
        #merge user-selected NAICS with FRS facility location information
        sitepoints <- EJAMfrsdata::frs_by_naics[NAICS %like% inputnaics ,  ]
        print(sitepoints)
        if(is_empty(sitepoints) | nrow(sitepoints) == 0){
          ################ Should output something saying no valid results returned #########
          shiny::validate('No Results Returned')
        }
      } else{
        sitepoints <- EJAMfrsdata::frs_by_naics[NAICS %in% inputnaics,]
        print(sitepoints)
        showNotification('Points submitted successfully!', duration = 1)
      }
    }else{
      ################ Should output something saying no valid results returned #########
      shiny::validate('Invalid NAIC Input')
    }
    
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
  
  ## reactive: data uploaded by ECHO
  data_up_echo <- reactive({
    ## depends on ECHO upload - which may use same file upload as latlon
    req(input$ss_upload_echo)

    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_echo$name)
    
    ## if acceptable file type, read in; if not, send warning text
    ext <- switch(ext,
                  csv =  read.csv(input$ss_upload_echo$datapath),
                  xls = read_excel(input$ss_upload_echo$datapath),
                  xlsx = read_excel(input$ss_upload_echo$datapath),
                  shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    
    # only process if latm and lon (or aliases) exist in uploaded data
    lat_alias <- c('lat', 'latitude83', 'latitude', 'latitudes', 'faclat', 'lats')
    lon_alias <- c('lon', 'longitude83', 'longitude', 'longitudes', 'faclong', 'long', 'longs', 'lons','lng')

    ## if column names are matched to aliases, process it    
    if(any(tolower(colnames(ext)) %in% lat_alias) & any(tolower(colnames(ext)) %in% lon_alias)){
      
      ext %>% 
        EJAM::latlon_df_clean() %>% 
        data.table::as.data.table()
      
    }else{
      ## if not matched, return this message
      shiny::validate('No coordinate columns found.')
    }

  })
  
  ## reactive: count number of data upload methods currently used
  num_ul_methods <- reactive({
    shiny::isTruthy(input$ss_upload_latlon) +
      shiny::isTruthy(input$ss_upload_frs) +
      (shiny::isTruthy(input$ss_enter_naics) ||  shiny::isTruthy(input$ss_select_naics)) +
      shiny::isTruthy(input$ss_upload_echo)
    
  })

  
  ## reactive: hub for any/all uploaded data, gets passed to processing
  data_uploaded <- reactive({
    
    ## send message if no data uploaded
    validate(
      need(num_ul_methods() > 0, "Please upload a data set")
    )
    
    ## if more than 1 upload method used, it will try to use the one
    ## that is currently selected by the ss_choose_method radio button
    
    ## if using lat/lon upload
    if(current_upload_method() == 'Location (lat/lon)'){
     
      data_up_latlon() #%>% 
     
     } else if(current_upload_method() == 'NAICS'){
      
      data_up_naics()

    } else if(current_upload_method() == 'FRS'){

      data_up_frs()

    } else if(current_upload_method() == 'ECHO'){
      
      data_up_echo() 
  
    }
    
  })
  
  ## disable run button until there is data uploaded; then enable
  observe({
    if(num_ul_methods() == 0){
      shinyjs::disable(id = 'bt_get_results' )
    } else {
      shinyjs::enable(id = 'bt_get_results')
    }
  })
  
  ## initialize data_processed reactive variable 
  ## this will hold results from doaggregate
  data_processed <- reactiveVal(NULL)
  
  ## initialize data_summarized data 
  ## this will hold results from batch.summarize
  data_summarized <- reactiveVal(NULL)
  
  ## when 'Run Analysis' button is pressed
  observeEvent(input$bt_get_results, {
    
    showNotification('Processing sites now!', type = 'message', duration = 0.5)
    
    ## overall progress bar for 3 operatoins (getblocksnearby, doaggregate, batch.summarize)
    progress_all <- shiny::Progress$new(min = 0, max = 1)
    progress_all$set(value = 0, message = 'Step 1 of 3', detail = 'Getting nearby census blocks')
    
    ## run EJAM::getblocksnearby
    sites2blocks <- EJAM::getblocksnearby(
      sitepoints = data_uploaded(),
      cutoff = input$bt_rad_buff,
      quadtree = localtree
    )
    
    ## update overall progress bar
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
    
    ## run new version of doaggregate
    out <- suppressWarnings(doaggregate(
      sites2blocks = sites2blocks, 
      ## pass progress bar function as argument
      updateProgress = updateProgress_doagg
    ))
    
    ## close doaggregate progress bar
    progress_doagg$close()
    
    ## assign doaggregate output to data_processed reactive
    data_processed(out)
    
    ## update overall progress bar
    progress_all$inc(1/3, message='Step 3 of 3', detail = 'Summarizing')
    
    ## run EJAMbatch.summarizer::batch.summarize on already processed data
    outsum <- EJAMbatch.summarizer::batch.summarize(
      sitestats = data.frame(data_processed()$results_bysite),
      popstats =  data.frame(data_processed()$results_bysite),
      ## user-selected quantiles to use
      #probs = as.numeric(input$an_list_pctiles),
      threshold = list(95) # compare variables to 95th %ile
    )
    
    ## update overall progress bar
    progress_all$inc(1/3, message = 'Done processing! Loading results now', detail=NULL)
    
    ## assign batch.summarize output to data_summarized reactive
    data_summarized(outsum)
    
    ## close overall progress bar
    progress_all$close()
    
    ## wait 0.5 seconds, then switch tabs and jump to top of screen 
    Sys.sleep(0.5)
    shinyjs::js$toTop();
    updateTabsetPanel(session, "all_tabs", "Summary Report")
    
  })
  
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
  #     }else{
  #       head(data_uploaded())
  #     }
  #     
  #   })
  
  ## interactive data table of raw upload
  output$print_test2_dt <- DT::renderDT({
    req(data_uploaded())
    
    if(current_upload_method() == 'NAICS'){
      
      #takes NAICS codes selected, finds NAICS descriptions, and presents them  
      dt_result_by_naic = data_uploaded()[, .(Count = .N), by = NAICS]
      naics_desc = EJAM::NAICS[EJAM::NAICS %in% dt_result_by_naic$NAICS]
      dt_names = data.frame("NAICS"=naics_desc,"Description"=names(naics_desc))
      naicsdt = merge(x = dt_result_by_naic, y = dt_names, by='NAICS')
      naics_reorder = data.frame(naicsdt$Description,naicsdt$Count)
      colnames(naics_reorder) = c("NAICS Code","Facility Count")
      
      dt <- naics_reorder
      #print(naics_reorder,row.names=FALSE)
    }else{
      dt <- data_uploaded()
    }
    
    DT::datatable(dt, options = list(pageLength = 100, scrollX = TRUE, scrollY = '500px'))
    
  })
  
  
  ## reactive: check if uploaded points are clustered (may double-count people)
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
  
  ## update map radius label based on ratio button
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
  
  ## create map of uploaded data
  orig_leaf_map <- reactive({
    
    req(data_uploaded())
    
    max_pts <- 5000
    
    ## don't draw map if > 5000 points are uploaded
    if(nrow(data_uploaded()) < max_pts){
      suppressMessages(
        plot_facilities(mypoints = as.data.frame(data_uploaded()), 
                        rad = input$bt_rad_buff, 
                        highlight = input$an_map_clusters,
                        clustered = is_clustered())
      )
    } else {
      validate(paste0('Too many points (> ',prettyNum(max_pts, big.mark=','),') uploaded for map to be displayed'))
    }
    
  })
  
  ## output: leaflet map of uploaded points
  output$an_leaf_map <- leaflet::renderLeaflet({
    
    req(data_uploaded())
    orig_leaf_map()
  })
  
  ## reactive for map used for summary report
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
  output$quick_view_map <- leaflet::renderLeaflet({
    req(data_uploaded())
    
    ## use separate report map
    report_map()
    
    ## or can keep same map as on Site Selection tab
    # orig_leaf_map()
  })
  
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
            # next version should use something like EJAMejscreenapi::make.popup.api, but with EJAM column names
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
  
  ## reactive for estimated total population
  total_pop <- reactive({
    
    req(data_processed())
    ## format and return total population
    ## some of these numbers seem very large! possible double-counting??
    prettyNum(data_processed()$results_overall$pop, big.mark=',')
    
  })
  
  ## output: show header information for summary report
  output$view1_total_pop <- renderUI({

    req(data_processed())
    
    ## paste header information together
    title_text <- paste0('<div style="font-weight: bold; font-size: 11pt; text-align: center;">',
                         input$analysis_title, '<br>',
                         'Residents within ', 
                         #input$bt_rad_buff, ' ', input$radius_units, ' of any of the ', 
                         input$bt_rad_buff, ' miles of any of the ',
           nrow(data_processed()$results_bysite), ' sites analyzed<br>',
           #'in the xxx source category or sector<br>',
           'Estimated total population: ', total_pop(),'</div>'
           )
    
    ## return formatted HTML text
    HTML(title_text)
  })
  
  ## reactive: demographic summary table
  v1_demog_table <- reactive({
    
    ## to add more demographic indicators (such as supp/subgroups)
    ## need to include them in these vectors
    names_d_fixed <- gsub('VSI.eo','Demog.Index' ,EJAM::names_d)
    names_d_pctile_fixed <- gsub('VSI.eo','Demog.Index' , EJAM::names_d_pctile)
    
    req(data_processed())
    
    ## create dataframe with 6 columns - vars (indicator names), value (raw indicator value),
    ## state_avg (State Avg indicator value), state_pctile (State Pctile for indicator),
    ## usa_avg (US Avg indicator value), usa_pctile (US Pctile for indicator)
    tab_data_d <- data.frame(
      vars = names_d_fixed,
      value = data_processed()$results_overall[, ..names_d_fixed] %>% t, 
      ## state averages and percentiles are not included at all yet!!
      ## to include them, need state-level info for each site and then add to these 
      ## columns, using something like EJAM::statestats
      'state_avg' = as.numeric(NA),
      'state_pctile' = as.numeric(NA),
      ## us average pulled from EJAM::usastats
      'usa_avg' = EJAM::usastats %>% filter(PCTILE == 'mean') %>% select(all_of(names_d)) %>% t, 
      'usa_pctile' = data_processed()$results_overall[, ..names_d_pctile_fixed] %>% t) 
    
    ## get display names from map_headernames file
    long_names_d <- EJAMejscreenapi::map_headernames %>% 
      filter(newnames_ejscreenapi %in% names_d_fixed) %>% 
      select(vars = newnames_ejscreenapi, var_names = longname_tableheader)
    
    ## join long indicator names and move them to first column
    tab_data_d <- tab_data_d %>% 
      left_join(long_names_d, by = c('vars')) %>% 
      relocate(var_names, .before = 1) %>% 
      select(-vars)
    
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
  output$view1_demog_table <- gt::render_gt({
    v1_demog_table()
  })
  
  ## reactive: environmental summary table
  v1_envt_table <- reactive({
    req(data_processed())
    
    ## create dataframe with 6 columns - vars (indicator names), value (raw indicator value),
    ## state_avg (State Avg indicator value), state_pctile (State Pctile for indicator),
    ## usa_avg (US Avg indicator value), usa_pctile (US Pctile for indicator)
    tab_data_e <- data.frame(
      vars = names_e,  
      value = data_processed()$results_overall[, EJAM::names_e, with=FALSE] %>% t,
      ## state averages and percentiles are not included at all yet!!
      ## to include them, need state-level info for each site and then add to these 
      ## columns, using something like EJAM::statestats
      'state_avg' = as.numeric(NA),
      'state_pctile' = as.numeric(NA),
      'usa_avg' = EJAM::usastats %>% filter(PCTILE == 'mean') %>% select(all_of(names_e)) %>% t, 
      'usa_pctile' = data_processed()$results_overall[, EJAM::names_e_pctile, with=FALSE] %>% t
    ) 
    
    ## get display names from map_headernames file
    long_names_e <- EJAMejscreenapi::map_headernames %>% 
      filter(newnames_ejscreenapi %in% names_e) %>% 
      select(vars = newnames_ejscreenapi, var_names = longname_tableheader)
    
    ## join long indicator names and move them to first column
    tab_data_e <- tab_data_e %>% 
      left_join(long_names_e, by = c('vars') ) %>% 
      relocate(var_names, .before = 1) %>% 
      select(-vars)
    
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
  output$view1_envt_table <- gt::render_gt({
    v1_envt_table()
  })
  
  
  ## reactive: boxplot of demographic indicator ratios vs US average
  v1_boxplot <- reactive({
    req(data_summarized())
    
    names_d_fixed <- gsub('VSI.eo','Demog.Index' ,EJAM::names_d)
    #names_d_pctile_fixed <- gsub('VSI.eo','Demog.Index' , EJAM::names_d_pctile)
    
    ## get national average stats for each indicator
    avg.in.us <- EJAM::usastats %>% 
      filter(REGION == 'USA', PCTILE == 'mean') %>% 
      rename('Demog.Index' = 'VSI.eo')

    ## ratios for overall set of sites   
    ## uses (doaggregate output results_overall ) / (EJAM::usastats mean in USA)
    ratio.to.us.d <- 
      unlist(data_processed()$results_overall[1, ..names_d_fixed]) / 
      avg.in.us[, names_d_fixed]
    
    ## ratios by site
    ratio.to.us.d.bysite <- data.frame()
    
    ## ratios for individual sites   
    ## uses (doaggregate output results_bysite) / (EJAM::usastats mean in USA)
    ## could probably be simplified instead of using a loop
    for(i in 1:nrow(data_processed()$results_bysite)){
    
      ratio.to.us.d.bysite <- rbind(ratio.to.us.d.bysite,
                                    unlist(data_processed()$results_bysite[i, ..names_d_fixed]) /
                                      avg.in.us[, names_d_fixed]
      )
    }
    
    ## get display names from map_headernames file
    long_names_d <- EJAMejscreenapi::map_headernames %>% 
      filter(newnames_ejscreenapi %in% names_d_fixed) %>% 
      select(vars = newnames_ejscreenapi, var_names = longname_tableheader)
    
    ## assign column names
    names(ratio.to.us.d.bysite) <- long_names_d$var_names
    
    ## pivot data from wide to long - now one row per indicator
    ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>% 
      pivot_longer(cols = everything(), names_to = 'indicator') %>% 
      ## replace Infs with NAs - these happen when indicator at a site is equal to zero
      mutate(value = na_if(value, Inf))
    
    towhat_nicename <- "US Average"
    mymaintext <- paste0("Ratios to ", towhat_nicename, 
                         ", as distributed across these sites")
    
    ## find max of ratios
    max.ratio.d.bysite <- max(ratio.to.us.d.bysite$value, na.rm = TRUE)
  
    max.name.d.bysite <- ratio.to.us.d.bysite$indicator[which.max(ratio.to.us.d.bysite$value)]
    
    ## find 75th %ile of ratios
    q75.ratio.d.bysite <- quantile(ratio.to.us.d.bysite$value[ratio.to.us.d.bysite$indicator == max.name.d.bysite], 0.75, na.rm=TRUE)
    
    ## paste subtitle for boxplot
    subtitle <- paste0('Within ', input$bt_rad_buff,' miles of at least one site, ', 
                       max.name.d.bysite, ' is ', round(max.ratio.d.bysite,1), 'x the US average\n',
                       'and 1 in 4 sites is at least ',round(q75.ratio.d.bysite,2), 'x the US average' 
    )
    
    ## specify # of characters to wrap indicator labels
    n_chars_wrap <- 15
    
    ## specify upper bound for ratios (will drop values above this from graphic)
    max_limit <- 10
    
    ## much of this is plotting code is based on EJAMejscreenapi::boxplots_ratios
    ggplot(ratio.to.us.d.bysite, aes(x = indicator, y = value, 
                                     fill = indicator)) +
      ## draw boxplots
        geom_boxplot() +
      ## draw points - removed as they cover up boxplots with large datasets
      #geom_jitter(color = 'black', size=0.4, alpha=0.9, ) +
      ## set color scheme
        scale_fill_brewer(palette = 'Dark2') +
      ## alternate color scheme
        # viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
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
           y = "Ratio of Indicator values at uploaded sites\n vs. US average value",
           subtitle = subtitle,
           title = 'Ratio vs. National Average for Demographic Indicators') +
      theme_bw() +
      theme(
      ## set font size of text
        text = element_text(size = 14),
        #axis.text  = ggplot2::element_text(size = 16),
      ## set font size of axis titles
        axis.title = element_text(size=16),
      ## center and resize plot title
        plot.title = element_text(size=24, hjust = 0.5),
      ## center subtitle
        plot.subtitle = element_text(hjust = 0.5),
      ## hide legend
        legend.position = 'none'
      ) 
    
  })
  
  ## output: show boxplot of indicator ratios in Summary Report
  output$view1_boxplot <- renderPlot({
    v1_boxplot()
  })
  
  ## output: show site-by-site table in Tabular Results
  output$view3_table <- DT::renderDT({
    req(data_processed())
    
    
    include_states <- FALSE
    
    ## this code with include_states = TRUE used a different version
    ## of doaggregate (now doaggregate_with_states) that retained state info
    ## if that is ever the case in future, could use this section to add State
    ## and EPA region columns to DT table
    if(include_states){
      
      cols_to_select <- c('siteid', 'pop', EJAMbatch.summarizer::names_all)
      friendly_names <- c('Site ID', 'Est. Population',  EJAMbatch.summarizer::names_all_friendly, 'State', 'EPA Region',
                          '# of indicators above 95% threshold')
      
      
      dt_overall <- data_processed()$results_overall %>% 
        as.data.frame() %>% 
        mutate(siteid = 'All sites', ST = NA,
               across(where(is.numeric), .fns = function(x) {round(x, digits=2)})) %>% 
        select(all_of(cols_to_select), ST)
      
      dt <- data_processed()$results_bysite %>% 
        as.data.frame() %>%
        mutate(across(where(is.numeric), .fns = function(x) {round(x, digits=2)}),
               siteid = as.character(siteid)) %>%
        select(all_of(cols_to_select), ST)
      
      dt_avg <- data_summarized()$rows[c('Average person','Average site'),] %>% 
        mutate(siteid = c('Average person', 'Average site'), ST = NA,
               across(where(is.numeric), .fns = function(x) {round(x, digits=2)}),
               siteid = as.character(siteid)) %>%
        select(all_of(cols_to_select), ST)
      
      dt_final <- dt %>% 
        bind_cols(data_summarized()$cols) %>% 
        bind_rows(dt_avg) %>% 
        bind_rows(dt_overall) %>% 
        arrange(desc(pop)) %>% 
        mutate(pop = prettyNum(pop, big.mark = ',')) %>% 
        left_join(EJAM::stateinfo %>% select(ST, statename, REGION), by = 'ST') %>% 
        select(-ST, -Max.of.variables)
      
      colnames(dt_final) <- friendly_names
      
      dt_final <- dt_final %>% 
        relocate(c(State, 'EPA Region', '# of indicators above 95% threshold'), .before = 2)
      
      n_cols_freeze <- 5
      
    } else {
      
      ## vector of column names to keep
      cols_to_select <- c('siteid', 'pop', EJAMbatch.summarizer::names_all)
      ## vector of nicer names to use in table header
      friendly_names <- c('Site ID', 'Est. Population',  EJAMbatch.summarizer::names_all_friendly,
                          '# of indicators above 95% threshold')
      
      ## format doaggregate results_overall output - use as summary row
      dt_overall <- data_processed()$results_overall %>% 
        as.data.frame() %>% 
        mutate(siteid = 'All sites',
               across(where(is.numeric), .fns = function(x) {round(x, digits=2)})) %>% 
        select(all_of(cols_to_select))
      
      ## format doaggregate results_bysite output
      dt <- data_processed()$results_bysite %>% 
        as.data.frame() %>%
        mutate(across(where(is.numeric), .fns = function(x) {round(x, digits=2)}),
               siteid = as.character(siteid)) %>%
        select(all_of(cols_to_select))
      
      ## format batch.summarize Average person and Average site output - use as summary row
      dt_avg <- data_summarized()$rows[c('Average person','Average site'),] %>% 
        mutate(siteid = c('Average person', 'Average site'), 
               across(where(is.numeric), .fns = function(x) {round(x, digits=2)}),
               siteid = as.character(siteid)) %>%
        select(all_of(cols_to_select))
      
      dt_final <- dt %>% 
        ## add summary columns - # of indicators > threshold
        bind_cols(data_summarized()$cols) %>% 
        ## add summary rows
        bind_rows(dt_avg) %>% 
        bind_rows(dt_overall) %>% 
        ## sort by site ID so summary rows end up at top of table
        arrange(desc(siteid)) %>% 
        #arrange(desc(pop)) %>% 
        ## format population
        mutate(pop = prettyNum(pop, big.mark = ',')) %>% 
        ## drop column
        select(-Max.of.variables)
      
      ## change to nicer names
      colnames(dt_final) <- friendly_names
      
      ## move this column earlier in table
      dt_final <- dt_final %>% 
        relocate(c('# of indicators above 95% threshold'), .before = 2)
      
      ## number of columns to keep on left of table when scrolling 
      ## currently set to 3 for Site ID, Population, # of indicators above 95% threshold'
      n_cols_freeze <- 3
    }
        
    ## format data table
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
                  height = 1000
    )
    ## code for bolding certain rows - not currently used
      #           ) %>% 
      # DT::formatStyle(
      #   valueColumns = 'Site ID',
      #   target = 'row', columns = 'all',
      #   fontWeight = DT::styleEqual(c('All sites','Average person','Average site'), values = 'bold')
      # )
    
  })
  
  ## download excel spreadsheet of site-by-site results
  output$download_results_table <- downloadHandler(
    filename = function(){'results_table.xlsx'},
    content = function(fname){
      
      ## previous - use EJAMejscreenapi::prep_for_excel approach
      
      #table_as_displayed <- data_processed()$results_bysite
      # pctile_colnums <- which(EJAMejscreenapi::map_headernames$jsondoc_shortvartype[match(names(table_as_displayed), 
      #                                                                                     EJAMejscreenapi::map_headernames$newnames_ejscreenapi)] == 'pctile')
      # longnames <- EJAMejscreenapi::map_headernames$longname_tableheader[match(names(data_processed()$results_bysite),
      #                                                                          EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
      # 
      #names(table_as_displayed) <- ifelse(!is.na(longnames), longnames, names(table_as_displayed))
      # wb_out <- prep_EJAM_for_excel(df = table_as_displayed,
      #                               heatmap_colnames = names(table_as_displayed)[pctile_colnums],
      #                               heatmap_cuts=c(80, 90, 95),
      #                               heatmap_colors=c('yellow', 'orange', 'red'))
      
      ## use EJAM::workbook_ouput_styled approach
      ## future: can add other sheets from doaggregate output
      table_overall <- data_processed()$results_overall
      table_bysite <- data_processed()$results_bysite
      
      ## attempt to clean up some column names
      longnames <- EJAMejscreenapi::map_headernames$longname_tableheader[match(names(data_processed()$results_bysite),
                                                                               EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
      names(table_overall) <- ifelse(!is.na(longnames), longnames, names(table_overall))
      names(table_bysite) <- ifelse(!is.na(longnames), longnames, names(table_bysite))
      
      ## format excel workbook
      wb_out <- workbook_output_styled(overall = table_overall, eachsite = table_bysite)
      
      ## save file and return for downloading
      openxlsx::saveWorkbook(wb_out, fname)
      
    }
    
  )
  
  ## reactive: map of sites selected from site-by-site summary table
  data_sitemap <- reactiveVal(NULL)
  
  observeEvent(input$view3_table_rows_selected,{
      req(data_processed())
      
      data_sitemap(data_uploaded()[input$view3_table_rows_selected,])
  })
  
  output$v3_sitemap <- renderLeaflet({
    
    ## wait for row to be selected
    ## note: summary rows are currently mapped but don't have a point location to map
    validate(
      need(!is.null(input$view3_table_rows_selected),
             'Select a specific site in the table to see its location'
           )
    )
    
    ## zoom in from original map to show single point (can zoom out and see others)
    orig_leaf_map() %>% 
      setView(lng = mean(data_sitemap()$lon), lat = mean(data_sitemap()$lat), zoom = 11)
    
    
    ## alternate: plot single point individually on map (cannot zoom out and see others)
    # leaflet(data_sitemap()) %>%
    #   setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 13) %>%
    #   addTiles() %>%
    #   addCircles(radius = 1 *  meters_per_mile)
  })
  
  ## output: display number of uploaded sites
  output$an_map_text <- renderUI({
      req(data_uploaded())
    
      #separate inputs with valid/invalid lat/lon values
      num_na <- nrow(data_uploaded()[(is.na(data_uploaded()$lat) | is.na(data_uploaded()$lon)),])
      num_notna <- nrow(data_uploaded()[!(is.na(data_uploaded()$lat) | is.na(data_uploaded()$lon)),])

      ## if invalid data found, send modal to screen      
      if(num_na > 0){
        showModal(
          modalDialog(title = 'Invalid data found', 'FYI, some of your data was not valid.', size = 's')
        )
      }
      
      ## paste summary text together
      HTML(paste0('Current upload method: <strong>', current_upload_method(), '</strong><br>', 
                  'Total site(s) uploaded: <strong>',prettyNum(nrow(data_uploaded()), big.mark=','),'</strong><br>',
                  'Valid site(s) uploaded: <strong>',prettyNum(num_notna, big.mark=','),'</strong><br>',
                  'Site(s) with invalid lat/lon values: <strong>', prettyNum(num_na,big.mark=','), '</strong>'))
  })
  
  ## output: display barplot
  output$summ_display_bar <- renderPlot({
    
    req(data_summarized())
    
    ## set indicator group column names
    mybarvars <- switch(input$summ_bar_ind,
                        'Demographic'   = names_d_fixed,
                        'Environmental' = EJAM::names_e,
                        'EJ'            = EJAM::names_ej 
    )
    
    ## set indicator group friendly names  
    mybarvars.friendly <- switch(input$summ_bar_ind,
                                 'Demographic'   = EJAMbatch.summarizer::names_d_friendly,
                                 'Environmental' = EJAMbatch.summarizer::names_e_friendly,
                                 'EJ'            = EJAMbatch.summarizer::names_ej_friendly
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
      rownames_to_column(var = 'Summary') %>% 
      filter(Summary %in% mybarvars.sumstat)
    
    ## set ggplot theme elements for all versions of barplot
    ggplot_theme_bar <- theme_bw() +
      theme(legend.position = 'top',
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            strip.text = element_blank(),
            strip.background = element_blank()
            )
    
    ## raw data 
    if(input$summ_bar_data == 'raw'){
      
      ## pivot from wide to long, 1 row per indicator 
      barplot_data_raw <- barplot_data %>% 
        select(Summary, all_of( mybarvars)) %>% 
        pivot_longer(cols = -1, names_to = 'indicator') %>% 
        mutate(type = 'raw')
      
      ## median - not currently displayed
      if(mybarvars.stat == 'med'){
        barplot_usa_med <- EJAM::usastats %>% 
          filter(REGION == 'USA', PCTILE == 50) %>% # for median
          rename('Demog.Index'=VSI.eo) %>% 
          mutate(Summary = 'Median person in US') %>% 
          select(Summary, all_of(mybarvars)) %>% 
          pivot_longer(-Summary, names_to = 'indicator')
        
        ## NOTE: Median Person calculations are all 0s for now!
        barplot_input <- bind_rows(barplot_data_raw, barplot_usa_med)
        
      ## average  
      } else {
        barplot_usa_avg <- EJAM::usastats %>% 
          filter(REGION == 'USA', PCTILE == 'mean') %>% 
          rename('Demog.Index'=VSI.eo) %>% 
          mutate(Summary = 'Average person in US') %>% 
          select(Summary, all_of(mybarvars)) %>% 
          pivot_longer(-Summary, names_to = 'indicator')
        
        barplot_input <- bind_rows(barplot_data_raw, barplot_usa_avg)
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
          scale_x_discrete(labels = function(x) str_wrap(x, n_chars_wrap)) +
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
        select(Summary, all_of( mybarvars)) %>% 
        pivot_longer(cols = -1, names_to = 'indicator') 
      
      ## average
      if(mybarvars.stat == 'avg'){
        ## pull US average values from EJAM::usastats to compute ratios
        barplot_usa_avg <-  bind_rows(
          EJAM::usastats %>% 
            filter(REGION == 'USA', PCTILE == 'mean') %>% 
            rename('Demog.Index'=VSI.eo) %>% 
            mutate(Summary = 'Average person') %>%
            select(Summary, all_of(mybarvars)) %>% 
            pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          EJAM::usastats %>% 
            filter(REGION == 'USA', PCTILE == 'mean') %>% 
            rename('Demog.Index'=VSI.eo) %>% 
            mutate(Summary = 'Average site') %>%
            select(Summary, all_of(mybarvars)) %>% 
            pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
        )
        
        ## combine raw data with US averages 
        barplot_input <- left_join(
          barplot_data_raw, 
          barplot_usa_avg
        ) %>% 
          ## divide to get ratios
          mutate(ratio = value / usa_value) %>% 
          ## add row of all 1s to represent US average ratio being constant at 1
          bind_rows(
            data.frame(Summary = 'Average person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
          )
        
      } else {
        ## median - not currently displayed
        barplot_usa_med <-  bind_rows(
          EJAM::usastats %>% 
            filter(REGION == 'USA', PCTILE == 50) %>% 
            rename('Demog.Index'=VSI.eo) %>% 
            mutate(Summary = 'Median person') %>%
            select(Summary, all_of(mybarvars)) %>% 
            pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          EJAM::usastats %>% 
            filter(REGION == 'USA', PCTILE == 50) %>% 
            rename('Demog.Index'=VSI.eo) %>% 
            mutate(Summary = 'Median site') %>%
            select(Summary, all_of(mybarvars)) %>% 
            pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
        )
        
        barplot_input <- left_join(barplot_data_raw, barplot_usa_med) %>% 
          ## calc ratio
          mutate(ratio = value / usa_value) %>% 
          bind_rows(
            data.frame(Summary = 'Median person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
          )
      }
      
      ## set # of characters to wrap labels
      n_chars_wrap <- 15
      
      ## join and plot
      barplot_input %>% 
        left_join( data.frame(indicator = mybarvars, indicator_label =  mybarvars.friendly)) %>% 
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
          scale_x_discrete(labels = function(x) str_wrap(x, n_chars_wrap)) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          ## set axis labels
          labs(x = '', y = 'Indicator Ratio') +
          ## break plots into rows of 4 
          facet_wrap(~indicator_label, ncol = 4, scales='free_x') +
          ggplot_theme_bar
      
    }
    
  })
  
  ## output: demographic executive summary text
  output$exec_summ_d <- renderUI({
    
    req(data_summarized())
    
    ## can use a dropdown to select indicator
    ## or just use the most extreme one
    type <- 'choose' #'max'
    
    ## pull US averages for each indicator
    avg.in.us <- EJAM::usastats %>% 
      filter(REGION == 'USA', PCTILE == 'mean') %>% 
      rename('Demog.Index' = 'VSI.eo')
    
    ## ratios here computed as 
    ## (batch.summarize 'Average person' / EJAM::usastats mean in USA region )
    ## this needs further verification
    ratio.to.us.d <- unlist(data_summarized()$rows['Average person', names_d_fixed]) / 
      avg.in.us[, names_d_fixed]
      #doaggregate results_overall output, if needed: unlist(data_processed()$results_overall[1, ..names_d_batch_fix])
    
    ## if only care about max indicator - this is not currently used in favor of the 
    ## dropdown option (type == 'choose')
    if(type == 'max'){
      max.ratio.d <- max(ratio.to.us.d)
      max.name.d <- names(ratio.to.us.d)[which.max(ratio.to.us.d)]
      
      max.name.d.friendly <- EJAMbatch.summarizer::names_d_friendly[which.max(ratio.to.us.d)]
      
      median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',max.name.d)]
      
      exec_text_d <-paste0(
        'Key demographic factor: <strong>',
        max.name.d.friendly, 
        '</strong><br>',
        'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
        ' of any of) these ', nrow(data_processed()$results_bysite),
        ' sites are <strong>', round(max.ratio.d, 1), ' times</strong> as likely to be ', 
        max.name.d.friendly, ' as the average person in the US (',
        round(100 * data_summarized()$rows['Average person', max.name.d]), '% vs. ', 
        round(100 * avg.in.us[,max.name.d]),
        '%). The other demographic indicators have lower ratios.',
        '<br>',
        'The median (50th percentile) site here is at the <strong>', 
        scales::label_ordinal()(median.pctile.in.us),
        '</strong> percentile of all US residents for ', max.name.d.friendly
      )
    } else if(type == 'choose'){
        
        cur.name.d <- input$key_ind_d # names(ratio.to.us.d)[which.max(ratio.to.us.d)]
        cur.ratio.d <- ratio.to.us.d[cur.name.d]
        
        ## get friendly version of name
        cur.name.d.friendly <- EJAMejscreenapi::map_headernames %>% 
          filter(newnames_ejscreenapi == cur.name.d) %>% 
          pull(names_friendly)
        
        ## grab percentile of median site
        median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',cur.name.d)]
        
        exec_text_d <-paste0(
          'Key demographic factor: <strong>',
          cur.name.d.friendly, 
          '</strong><br>',
          'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
          ' of any of) these ', nrow(data_processed()$results_bysite),
          ' sites have, on average, <strong>', round(cur.ratio.d, 1), ' times</strong> as high indicator values for <strong>', 
          cur.name.d.friendly, '</strong> as the average person in the US (',
          round(100 * data_summarized()$rows['Average person', cur.name.d]), '% vs. ', 
          round(100 * avg.in.us[,cur.name.d]),
          '%)',
          '<br>',
          'The median (50th percentile) site here is at the <strong>', 
          scales::label_ordinal()(median.pctile.in.us),
          '</strong> percentile of all US residents for <strong>', cur.name.d.friendly, '</strong>'
        )
      
    }
    
    HTML(exec_text_d)
    ## need to add state multiplier?
    ## need to add top X% stat?
  })
  
  ## output: environmental executive summary text
  output$exec_summ_e <- renderUI({
    
    req(data_summarized())
    
    ## can use a dropdown to select indicator
    ## or just use the most extreme one
    type <- 'choose' #'max'
    
    ## pull US averages for each indicator
    avg.in.us <- EJAM::usastats %>% 
      filter(REGION == 'USA', PCTILE == 'mean') 
    
    ## ratios here computed as 
    ## (batch.summarize 'Average person' / EJAM::usastats mean in USA region )
    ## this needs further verification
    ratio.to.us.e <- unlist(data_summarized()$rows['Average person', names_e]) / 
      avg.in.us[, names_e]
      #doaggregate results_overall output, if needed: unlist(data_processed()$results_overall[1, ..names_e])
    
    ## if only care about max indicator - this is not currently used in favor of the 
    ## dropdown option (type == 'choose')
    if(type == 'max'){
      max.ratio.e <- max(ratio.to.us.e)
      max.name.e <- names(ratio.to.us.e)[which.max(ratio.to.us.e)]
      
      max.name.e.friendly <- EJAMbatch.summarizer::names_e_friendly[which.max(ratio.to.us.e)]
      
      median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',max.name.e)]
      
      exec_text_e <-paste0(
        'Key environmental factor: <strong>',
        max.name.e.friendly, 
        '</strong><br>',
        'People who live near (within ', input$bt_rad_buff,' miles', #input$radius_units,
        ' of any of) these ', nrow(data_processed()$results_bysite),
        ' sites have, on average, <strong>', round(max.ratio.e, 1), ' times</strong> as high indicator values for <strong>', 
        max.name.e.friendly, '</strong> as the average person in the US (',
        round(data_summarized()$rows['Average person', max.name.e], 2), ' vs. ', 
        round(avg.in.us[,max.name.e], 2),
        '). The other environmental indicators have lower ratios.',
        '<br>',
        'The median (50th percentile) site here is at the <strong>', 
        scales::label_ordinal()(median.pctile.in.us),
        '</strong> percentile of all US residents for <strong>', max.name.e.friendly
      )
      
    } else if(type == 'choose'){
      cur.name.e <- input$key_ind_e # names(ratio.to.us.d)[which.max(ratio.to.us.d)]
      cur.ratio.e <- ratio.to.us.e[cur.name.e]
      
      cur.name.e.friendly <- EJAMejscreenapi::map_headernames %>% 
        filter(newnames_ejscreenapi == cur.name.e) %>% 
        pull(names_friendly)
      
      ## grab percentile of median site
      median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',cur.name.e)]
      
      exec_text_e <-paste0(
        'Key environmental factor: <strong>',
        cur.name.e.friendly, 
        '</strong><br>',
        'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
        ' of any of) these ', nrow(data_processed()$results_bysite),
        ' sites have, on average, <strong>', round(cur.ratio.e, 1), ' times</strong> as high indicator values for ', 
        cur.name.e.friendly, ' as the average person in the US (',
        round( data_summarized()$rows['Average person', cur.name.e], 2), ' vs. ', 
        round(avg.in.us[,cur.name.e], 2),
        ')',
        '<br>',
        'The median (50th percentile) site here is at the <strong>', 
        scales::label_ordinal()(median.pctile.in.us),
        '</strong> percentile of all US residents for <strong>', cur.name.e.friendly, '</strong>'
      )
    }
    
    HTML(exec_text_e)
    ## need to add state multiplier?
    ## need to add top X% stat?
  })
  
  ## output: display histogram
  output$summ_display_hist <- renderPlot({
    
    req(data_summarized())
    
    ## set font sizes
    ggplot_theme_hist <- theme(
      plot.title = element_text(size=18, hjust=0.5),
      axis.text = element_text(size=16),
      axis.title = element_text(size=16)
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
  output$rg_enter_miles <- renderUI({
    
    shiny::textInput(inputId = "rg_enter_miles", 
                     label = "Analysis Location:", 
                     value = paste0("within ", input$bt_rad_buff,
                                    ' miles of')#,
                                    #input$radius_units, " of")
    )
  })
  
  ## download 1-page summary comparable to EJScreen report
  output$summary_download <- downloadHandler(
    # For PDF output, change this to "summary_report.pdf"
    filename = 'brief_summary.html',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "brief_summary.Rmd")
      file.copy("../www/brief_summary.Rmd", tempReport, overwrite = TRUE)
      #file.copy("../www/test_report1pager.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(n_sites = nrow(data_processed()$results_bysite), 
                     distance = paste0(input$bt_rad_buff,' miles'), #input$radius_units),
                     total_pop = total_pop(),
                     analysis_title = input$analysis_title,
                     cur_data = data_processed(),
                     map = report_map(),
                     envt_table = v1_envt_table(),
                     demog_table = v1_demog_table(),
                     boxplot = v1_boxplot())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, 
                        ## for pdf output, change to 'pdf_document'
                        output_format = 'html_document',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        intermediates_dir = tempdir()
      )
      ## alternative ways to save PDFs - these go to local folders
      ## add line to save html output as pdf
      # pdf(file = 'summary_report.pdf')
      # file
      # dev.off()
      #webshot::webshot(url = file, file = file.path(tempdir(), 'summary_report.pdf'))
    }
  )
  
  ## show modal with report outline
  observeEvent(input$show_outline, {
    showModal(
      modalDialog(
        #HTML(report_outline)
        HTML(
          str_replace(report_outline, 
                      'Broad overview of findings',
                      '<mark>Broad overview of findings</mark>'
                      )
        )
      )
    )
  })
  
  ## code for storing all shiny input values - not used currently
  # observeEvent(input$all_tabs == 'Generate Report',
  #  {
  #    list_of_inputs <- reactiveValuesToList(input)
  #  })
  
  ## download full static report
  output$rg_download <- downloadHandler(
    filename = 'report.doc',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("../www/report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        authorname1 = input$rg_author_name,
        authoremail1 = input$rg_author_email,
        radius = paste0(input$bt_rad_buff,' miles'), #input$radius_units),
        where = input$rg_enter_miles,
        sectorname_short = input$rg_enter_sites,
        #facilities_analyzed = input$rg_enter_fac,
        sitecount = nrow(data_processed()$results_bysite),
        ## allow for either or
        in_the_x_zone = ifelse(nchar(input$in_the_x_zone_enter) > 0, 
                               input$in_the_x_zone_enter,
                               input$in_the_x_zone),
        facilities_studied = ifelse(nchar(input$facilities_studied_enter) > 0, 
                                    input$facilities_studied_enter,
                                    input$facilities_studied),
        in_areas_where = paste0(input$in_areas_where, ' ', input$in_areas_where_enter),
        risks_are_x = input$risks_are_x,
        demog_how_elevated = input$demog_how_elevated,
        envt_how_elevated = input$envt_how_elevated,
        demog_high_at_what_share_of_sites = input$demog_high_at_what_share_of_sites,
        envt_high_at_what_share_of_sites = input$envt_high_at_what_share_of_sites,
        source_of_latlons = input$source_of_latlons,
        fundingsource = input$fundingsource,
        conclusion1 = input$conclusion1,
        conclusion2 = input$conclusion2,
        conclusion3 = input$conclusion3
      )
      
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

