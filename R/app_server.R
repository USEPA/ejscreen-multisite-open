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
    ## depends on FRS file upload
    #req(input$ss_upload_frs)
  })
  
  ## reactive: data uploaded by NAICS
  data_up_naics <- reactive({
    ## depends on NAICS entering or selection
    #req(shiny::isTruthy(input$ss_enter_naics) || shiny::isTruthy(input$ss_select_naics))
    
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
    
    print(num_ul_methods())
    
    ## send message if more than 1 upload method used
    # shiny::validate(
    #   need(num_ul_methods() <= 1, message = "Please only use 1 upload method at a time")
    # )
    
    
    ## if using lat/lon upload
    if(isTruthy(data_up_latlon())){
      
      data_up_latlon() %>% 
        ## search for lat/lon-like column names and rename them
        EJAM::latlon_df_clean() %>% 
        ## convert to data.table format
        data.table::as.data.table()
      
    } #else if(isTruthy(data_up_echo())){
    #   print('check')
    #   data_up_echo() %>%
    #     ## search for lat/lon-like column names and rename them
    #     EJAM::latlon_df_clean() %>%
    #     ## convert to data.table format
    #     data.table::as.data.table()
    # }
    
    ## needs to account for checklists of facility types to limit
    #req(input$ss_limit_fac1, input$ss_limit_fac2)
  })
  
  ## reactive: process uploaded data
  data_processed <- reactive({
    
    ## requires data_uploaded reactive to exist
    #req(isTruthy(data_uploaded()), input$bt_rad_buff)
    #req(isTruthy(data_uploaded()))
    
    print('Processing facilities now')
    
    ## run EJAM::getblocksnearby
    sites2blocks <- EJAM::getblocksnearby(
      sitepoints = data_uploaded(),
      cutoff = input$bt_rad_buff,
      quadtree = localtree
    )
    
    print('Running doaggregate')
    
    ## run EJAM::doaggregate
    out <- EJAM::doaggregate(
      sites2blocks = sites2blocks
    )
    
    print('Processing complete')
    
    ## return output object
    out
    #data_processed() <- out    
  })
  
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
    plot_facilities(data_uploaded(), rad = input$bt_rad_buff, highlight = input$an_map_clusters,
                    clustered = is_clustered())
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
    
    leafletProxy(mapId = 'an_leaf_map', session, data = data_uploaded()) %>% 
      clearShapes() %>% 
      addCircles(
        radius = input$bt_rad_buff * meters_per_mile,
        color = circle_color, fillColor = circle_color, 
        fill = TRUE, weight = 4, 
        #popup = popup_to_show()
      )
  })
  
  
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

