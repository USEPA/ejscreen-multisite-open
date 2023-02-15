#' EJAM app server
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # Note: how to avoid or still have a global.R file in the golem approach, 
  # https://github.com/ThinkR-open/golem/issues/6
  
  
  ## output: html page header using OneEPA template code
  output$html_header <- renderUI({
    isolate(
      source('html_header.R')
    )
  })
  
  ## output: html page footer using OneEPA template code
  output$html_footer <- renderUI({
    isolate(
      source('html_footer.R')
    )
  })
  
  ## update ss_select_NAICS input options
  updateSelectizeInput(session, inputId = 'ss_select_naics', 
                       choices = EJAM::NAICS, server = TRUE)
  
  ## reactive: read uploaded latlon
  data_up_latlon <- reactive({
    ## wait for file to be uploaded
    req(input$ss_upload_latlon)
    
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_latlon$name)
    
    ## if acceptable file type, read in; if not, send warning text
    ext <- switch(ext,
                  csv =  read.csv(input$ss_upload_latlon$datapath),
                  xls = read.table(input$ss_upload_latlon$datapath),
                  xlsx = read.table(input$ss_upload_latlon$datapath),
                  shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    
    ext
  })
  
  ## reactive: data uploaded by FRS IDs
  data_up_frs <- reactive({
    req(input$ss_upload_frs)
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_frs$name)
    
    read_frs <- switch(ext,
                       csv =  read.csv(input$ss_upload_frs$datapath),
                       xls = read.table(input$ss_upload_frs$datapath),
                       xlsx = read_excel(input$ss_upload_frs$datapath),
                       shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    
    #include frs_is_valid verification check function
    if (frs_is_valid(read_frs)){
      
      read_frs_dt <- data.table::as.data.table(read_frs)
      
      frs_lat_lon <- merge(x = read_frs_dt, y = EJAMfrsdata::frs, by.x='REGISTRY_ID', by.y='REGISTRY_ID', all.x=TRUE)
      
    }else{
      shiny::validate('Records with invalid Registry IDs')
    }
    
    ## depends on FRS file upload
    #req(input$ss_upload_frs)
  })
  
  ## reactive: data uploaded by NAICS
  data_up_naics <- reactive({
    ## depends on NAICS entering or selection
    req(shiny::isTruthy(input$ss_enter_naics) || shiny::isTruthy(input$ss_select_naics))
    
    #define inputs
    naics_user_wrote_in_box <- input$ss_enter_naics
    naics_user_picked_from_list <- input$ss_select_naics
    
    #NAICS validation function to check for non empty NAICS inputs
    if(NAICS_validation(input$ss_enter_naics,input$ss_select_naics)){
      
      #splits up comma separated list if user manually inserts list
      if(nchar(input$ss_enter_naics)>0){
        naics_wib_split <- as.list(strsplit(naics_user_wrote_in_box, ",")[[1]])
      }else{
        naics_wib_split <- ""
      }
      
      #construct regex expression and finds sites that align with user-selected naics codes
      
      inputnaics <- c(naics_wib_split, naics_user_picked_from_list)
      inputnaics <- unique(inputnaics[inputnaics != ""])
      inputnaics <- paste("^", inputnaics, collapse="|")   ### the NAICS specified by user
      inputnaics <- stringr::str_replace_all(string = inputnaics, pattern = " ", replacement = "")
      
      #merge user-selected NAICS with FRS facility location information
      sitepoints <- EJAMfrsdata::frs_by_naics[NAICS %like% inputnaics ,  ] 
      
    }else{
      shiny::validate('Invalid NAIC Input')
    }
    
    sitepoints
  })
  
  ## reactive: data uploaded by ECHO
  data_up_echo <- reactive({
    ## depends on ECHO upload - which may use same file upload as latlon/FRS
    req(input$ss_upload_echo)
    
    ## check if file extension is appropriate
    ext <- tools::file_ext(input$ss_upload_echo$name)
    
    ## if acceptable file type, read in; if not, send warning text
    ext <- switch(ext,
                  csv =  read.csv(input$ss_upload_echo$datapath),
                  xls = read.table(input$ss_upload_echo$datapath),
                  xlsx = read.table(input$ss_upload_echo$datapath),
                  shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    
    ext
  })
  
  ## reactive: count number of data upload methods currently used
  num_ul_methods <- reactive({
    shiny::isTruthy(input$ss_upload_latlon) +
      shiny::isTruthy(input$ss_upload_frs) +
      shiny::isTruthy(input$ss_enter_naics) +
      shiny::isTruthy(input$ss_select_naics) +
      shiny::isTruthy(input$ss_upload_echo)
    
  })
  
  ## reactive: hub for any/all uploaded data, gets passed to processing
  data_uploaded <- reactive({
    
    
    ## check for values from all upload reactives
    #req(shiny::isTruthy(input$ss_upload_latlon) || shiny::isTruthy(input$ss_upload_echo))
    #req(input$ss_upload_latlon)
    
    ## count number of data reactives being used
    # num_data <- shiny::isTruthy(data_up_latlon()) +
    #   shiny::isTruthy(data_up_frs()) +
    #   shiny::isTruthy(data_up_naics()) +
    #   shiny::isTruthy(data_up_echo())
    
    #print(num_ul_methods())
    
    ## send message if more than 1 upload method used
    # shiny::validate(
    #   need(num_ul_methods() <= 1, message = "Please only use 1 upload method at a time")
    # )
    validate(
      need(num_ul_methods() > 0, "Please upload a data set")
    )
    
    ## if using lat/lon upload
    if(isTruthy(input$ss_upload_latlon)){
      
      data_up_latlon() %>% 
        ## search for lat/lon-like column names and rename them
        EJAM::latlon_df_clean() %>% 
        ## convert to data.table format
        data.table::as.data.table()
      
    } else if(isTruthy(input$ss_select_naics)){
      
      data_up_naics()
      
    } else if(isTruthy(input$ss_upload_frs)){
      
      data_up_frs()
      
    } else if(isTruthy(input$ss_upload_echo)){
      data_up_echo() %>% 
        ## search for lat/lon-like column names and rename them
        EJAM::latlon_df_clean() %>% 
        ## convert to data.table format
        data.table::as.data.table()
    }
    
    ## needs to account for checklists of facility types to limit
    #req(input$ss_limit_fac1, input$ss_limit_fac2)
  })
  
  data_processed <- reactiveVal(NULL)
  
  observeEvent(input$bt_get_results, {
    
    showNotification('Processing facilities now!', type = 'message', duration = 0.5)
    
    ## run EJAM::getblocksnearby
    sites2blocks <- EJAM::getblocksnearby(
      sitepoints = data_uploaded(),
      cutoff = input$bt_rad_buff,
      quadtree = localtree
    )
    
    #print('Running doaggregate')
    #showNotification('Processing facilities now!', type = 'message', duration = 0.5)
    
    ## run EJAM::doaggregate
    out <- suppressWarnings(EJAM::doaggregate(
      sites2blocks = sites2blocks
    ))
    
    showNotification('Processing complete!', type = 'message', duration = 3)
    
    #print('Processing complete')
    
    ## return output object
    data_processed(out)
    
  })
  
  ## reactive: process uploaded data
  #data_processed <- reactive({
  # data_processed <- eventReactive(input$bt_get_results, {  
  #   ## requires data_uploaded reactive to exist
  #   #req(isTruthy(data_uploaded()), input$bt_rad_buff)
  #   #req(isTruthy(data_uploaded()))
  # 
  #   print('Processing facilities now')
  # 
  #   ## run EJAM::getblocksnearby
  #   sites2blocks <- EJAM::getblocksnearby(
  #                     sitepoints = data_uploaded(),
  #                     cutoff = input$bt_rad_buff,
  #                     quadtree = localtree
  #                   )
  #   
  #   print('Running doaggregate')
  # 
  #   ## run EJAM::doaggregate
  #   out <- EJAM::doaggregate(
  #     sites2blocks = sites2blocks
  #   )
  # 
  #   print('Processing complete')
  # 
  #   ## return output object
  #   out
  #   
  #   out
  #   #data_processed() <- out    
  # })
  
  ## reactive: summarize processed data
  data_summarized <- reactive({
    ## run EJAMbatch.summarizer::batch.summarize on already processed data
    # a) processed in-app: 
    
    ## need to determine difference between sitestats and popstats
    outsum <- EJAMbatch.summarizer::batch.summarize(
      sitestats = data_processed()$results_bysite,
      popstats =  data_processed()$results_bysite,
      ## user-selected quantiles to use
      probs = input$an_list_pctiles
    )
    
    ## return output
    outsum
    
    # b) processed and downloaded another time, to be re-uploaded now
    # req(input$bt_upload_adj, input$bt_upload_std)
  })
  
  ## reactive: data for use in static report
  data_report <- reactive({
    ## this is to take in the summarized results and 
    ## reduce them down to only the pieces that will
    ## go into the static report
    # data_summarized()
  })
  
  
  ## TESTING - 
  output$print_test <- renderText({
    #req(isTruthy(data_uploaded()))
    str(data_up_naics())
    
  })
  
  ## TESTING
  output$print_test2 <- renderPrint({
    
    #req(input$ss_select_naics)
    head(data_up_naics())    
  })
  
  
  ## pull up modal with ECHO information on button click
  observeEvent(input$ss_search_echo, {
    showModal(
      modalDialog(title = "Use ECHO facility search tools to specify list of sites",
                  echo_message,
                  shiny::HTML(paste('<a href=\"', echo_url, '\", target=\"_blank\">', echo_url,  '</a>', sep = '')),
                  easyClose = TRUE)
    )
  })
  
  ## REMOVE later - print first few rows of uploaded latlon data
  output$upload_check <- renderTable({
    #req(input$ss_upload_latlon)
    req(data_uploaded())
    
    head(data_uploaded())
  })
  
  ## reactive: check if uploaded points are clustered (may double-count people)
  is_clustered <- shiny::reactive({
    # which sites have residents that might also be near others sites?
    # circles overlap if 2 facilities are twice the radius apart  # in miles
    EJAMejscreenapi::near_eachother(
      lon = data_uploaded()$lon, 
      lat = data_uploaded()$lat, 
      distance = 2 * input$bt_rad_buff
    ) 
  })
  
  ## output: leaflet map of uploaded points
  output$an_leaf_map <- leaflet::renderLeaflet({
    
    #req(data_uploaded())
    
    ## function in global.R, will go in own script later
    ## once name is settled
    suppressMessages(
      plot_facilities(data_uploaded(), rad = input$bt_rad_buff, highlight = input$an_map_clusters,
                      clustered = is_clustered())
    )
  })
  
  ## update leaflet map when inputs change
  observe({
    
    base_color      <- 'blue'
    cluster_color   <- 'red'
    
    if(input$an_map_clusters == TRUE){
      ## compare latlons using is_clustered() reactive
      circle_color <- ifelse(is_clustered() == TRUE, cluster_color, base_color)
    } else {
      circle_color <- base_color
    }
    suppressMessages(
      leafletProxy(mapId = 'an_leaf_map', session, data = data_uploaded()) %>% 
        clearShapes() %>% 
        addCircles(
          radius = input$bt_rad_buff * meters_per_mile,
          color = circle_color, fillColor = circle_color, 
          fill = TRUE, weight = 4, 
          #popup = popup_to_show()
        )
    )
  })
  
  ## output: show total population from doaggregate output
  output$view1_total_pop <- renderText({
    
    req(data_processed())
    
    pop_num <- prettyNum(data_processed()$results_overall$pop, big.mark=',')
    
    paste0('Estimated total population: ', pop_num)
  })
  
  ## output: show table of indicators in view 1
  output$view1_demog_table <- DT::renderDT({
    req(data_processed())
    
    dt <- cbind('Value at Uploaded Sites' = as.list( 100*round(data_processed()$results_overall[ , ..names_d], 2)),
                'State Percentile' = as.list(round(data_processed()$results_overall[, ..names_d_state_pctile], 2)),
                'National Percentile' = as.list(round(data_processed()$results_overall[, ..names_d_pctile], 2)))
    
    rownames(dt) <- EJAMbatch.summarizer::names_d_friendly[pmatch(names_d, EJAMbatch.summarizer::names_d_batch)]
    rownames(dt)[1] <- 'Demographic Index'
    
    dt <- as.data.frame(dt) %>% rownames_to_column(var = 'Indicator')
    
    DT::datatable(dt, rownames = FALSE, 
                  options = list(autoWidth = TRUE))
  })
  
  ## output: show table of indicators in view 1
  output$view1_envt_table <- DT::renderDT({
    req(data_processed())
    
    #data_processed()[, ..EJAMbatch.summarizer::names_d_batch]
    dt <- cbind('Value at Uploaded Sites' = 
                  as.list(round(data_processed()$results_overall[ , ..names_e],2)),
                'State Percentile' = as.list(round(data_processed()$results_overall[, ..names_e_state_pctile],2)),
                'Percentile in USA' = as.list(round(data_processed()$results_overall[, ..names_e_pctile],2)))
    
    #scales::percent_format(accuracy = 0.1)()
    rownames(dt) <- EJAMbatch.summarizer::names_e_friendly[pmatch(names_e, EJAMbatch.summarizer::names_e_batch)]
    
    dt <- as.data.frame(dt) %>% rownames_to_column(var = 'Indicator')
    
    DT::datatable(dt, rownames=FALSE,
                  options = list(autoWidth = TRUE))
  })
  
  ## output: show site-by-site table in View 3
  output$view3_table <- DT::renderDT({
    req(data_processed())
    
    DT::datatable(round(data_processed()$results_bysite,digits=2), rownames = FALSE, 
                  #filter = 'top',
                  selection = 'single',
                  #extensions = c('FixedColumns','Buttons'),
                  extensions = 'FixedColumns',
                  options = list(
                    #buttons = c('csv','excel'),
                    autoWidth = TRUE,
                    fixedHeader = TRUE, fixedColumns = list(leftColumns = 2),
                    pageLength = 25, scrollX = TRUE, scrollY = '250px')
    ) 
    
  })
  
  observe({
    
    req(data_processed())
    site_ids <- data_processed()$results_bysite$siteid
    names(site_ids) <- paste0('Site ', site_ids)
    
    
    ## update v4_site_dropdown input options
    updateSelectizeInput(session, inputId = 'v4_site_dropdown', 
                         choices = site_ids, server = TRUE)
    
  })
  
  
  ## output: dropdown of site IDs for view 4
  # output$v4_site_dropdown <- renderUI({
  #   req(data_processed())
  #   
  #   site_ids <- data_processed()$results_bysite$siteid
  #   names(site_ids) <- paste0('Site ', site_ids)
  #  
  #   selectInput(inputId = 'v4_site_dropdown',
  #               label = 'Select a site to explore',
  #               choices = site_ids
  #   )
  # })
  
  ## output: display number of uploaded sites
  output$an_map_text <- renderText({
    req(data_uploaded())
    
    paste0(nrow(data_uploaded()), ' points uploaded')
  })
  
  ## output: display barplot
  output$summ_display_bar <- renderPlot({
    
    ## placeholder code for barplot
    barplot(table(InsectSprays$spray))
  })
  
  ## output: display histogram
  output$summ_display_hist <- renderPlot({
    
    req(isTruthy(data_processed()))
    
    ## placeholder code for histogram
    #hist(InsectSprays$count)
    
    ## need to add logic to switch column being shown based on radio buttons
    # ind_column <- switch(
    #   
    # )
    
    hist(data_processed()$results_bysite$pctile.traffic.score, 
         breaks = input$summ_hist_bins,
         xlab="Local traffic scores (expressed as a percentile)", 
         ylab="count of sites in each bin", 
         freq = TRUE,
         main="Actual distribution of indicators nearby, as percentiles, vs flat line = USA overall")
    abline(h=nrow(data_processed()$results_bysite)/10)
  })
  
  ## download 1-page summary comparable to EJScreen report
  output$summary_download <- downloadHandler(
    filename = 'brief_summary.html',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "brief_summary.Rmd")
      file.copy("www/brief_summary.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = 3, cur_data = data_processed() %>% as.data.frame())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_format = 'html_document',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ## download full static report
  output$rg_download <- downloadHandler(
    filename = 'report.docx',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = 3)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_format = 'word_document',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

