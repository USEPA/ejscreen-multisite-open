#' app_server - EJAM app server
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
#' @import shinycssloaders
#' @importFrom shinyjs reset
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @import sf
#' @import sp
#' @import tidyverse
#' @import tidyr
#' @importFrom magrittr '%>%'
#' @importFrom grDevices dev.off png
#' @importFrom graphics abline barplot legend points
#' @importFrom methods Summary as
#' @importFrom stats aggregate density na.omit quantile runif setNames
#' @importFrom utils data download.file installed.packages object.size read.csv stack tail
#' @rawNamespace import(dplyr, except = c(first, last, between))
#' 
app_server <- function(input, output, session) {
  # notes ####
  #############################################################################  #
  # Note: whether/how to avoid or still use a global.R file in the golem approach. and/or use options and yaml file or golem approaches to options/settings...
  # https://github.com/ThinkR-open/golem/issues/6 then https://github.com/ThinkR-open/golem/issues/920 then https://github.com/ThinkR-open/golem/discussions/932 
  # but latest seems to be here: https://cran.r-project.org/web/packages/golem/vignettes/c_deploy.html
  #   Here is the methodology for integrating what you've got in globals.R inside your golem app:
  # - all the datasets that are fixed should be added as a package dataset 
  # - functions should be integrated as package functions 
  # - things that need to be launched at runtime should be listed in the app_server function
  # + You can also use golem_opts for things needed at runtime, to have something like run_app(param = this). 
  #############################################################################  #
  # Key reactives = data_uploaded(), data_processed(), data_summarized() ####
  ##  data_uploaded   reactive holds selected latlon points or shapefiles. It is defined later.
  ##  data_processed  reactive holds results of analysis, like output of doaggregate(getblocksnearby(points)) 
  ##  data_summarized reactive holds results of batch.summarize()
  ##  Note that  ejamit(points)  would do all of those steps in one function, essentially. 
  data_processed <-  reactiveVal(NULL) # initialized so it can be set later in reaction to an event, using data_processed(newvalue)
  data_summarized <- reactiveVal(NULL) # initialized so it can be set later in reaction to an event, using 
  #
  # provide nice message if disconnected, via shinydisconnect package
  observeEvent(input$disconnect, {session$close()})
  ################################################################### #
  
  # *** As of Shiny 1.6.0, we recommend using bindEvent() instead of eventReactive() and observeEvent(). 
  # When bindEvent() is used with reactive() and observe(), it does the same as eventReactive() and observeEvent().
  # When bindEvent() is used with reactive(), it creates a new reactive expression object.
  # When bindEvent() is used with observe(), it alters the observer in place. It can only be used with observers which have not yet executed.
  # In many cases, it makes sense to use bindEvent() along with bindCache(), because they each can reduce the amount of work done on the server. For example, you could have sliderInputs x and y and a reactive() that performs a time-consuming operation with those values. Using bindCache() can speed things up, especially if there are multiple users. But it might make sense to also not do the computation until the user sets both x and y, and then clicks on an actionButton named go.
  # To use both caching and events, the object should first be passed to bindCache(), then bindEvent(). 
  # and can also be used with render functions (like renderText() and renderPlot()).
  # To use both caching and events, the object should first be passed to bindCache(), then bindEvent(). For example:
  #   
  #   r <- reactive({
  #     Sys.sleep(2)  # Pretend this is an expensive computation
  #     input$x * input$y
  #   }) %>%
  #     bindCache(input$x, input$y) %>%
  #     bindEvent(input$go)
  #   Anything that consumes r() will take a reactive dependency on the event expression given to bindEvent(), and not the cache key expression given to bindCache(). In this case, it is just input$go.
  
  
  #. ####
  # ___ BUTTONS/TABS (events: Go to tab/ Help/ Start analysis) ####
  
  ## outline of tabs  
  # at one point was this: 
  #
  # tabsetPanel(                         id = 'all_tabs',     ##
  #   tabPanel(title = 'About EJAM', 
  #   tabPanel(title = 'Site Selection', 
  #   tabPanel(title =   , # ??
  #   tabsetPanel(                       id = 'results_tabs', ##
  #       tabPanel(title = 'Summary', 
  #       tabPanel(title = 'Details',
  #          tabPanel(title = 'Site-by-Site Table',
  #          tabPanel(title = 'Plot Average Scores', 
  #          tabPanel(title = 'Plot Full Range of Scores', 
  #       tabPanel(title = 'Written Report',
  #   tabPanel(title = 'EJScreen Batch Tool',  
  #   tabPanel(title = 'Advanced Settings', 
  
  ## buttons to switch tabs (used to navigate if the clickable tab controls themselves are "hidden") ---------------------- # 
  
  ## "About" button/link
  observeEvent(input$link_to_about_page, {updateTabsetPanel(session, inputId = "all_tabs", selected = "About EJAM")})
  ## "back to site selection" buttons (to go back to site selection tab from results tab)
  observeEvent(input$back_to_site_sel,  {updateTabsetPanel(session, inputId = 'all_tabs', selected = 'Site Selection')})
  observeEvent(input$back_to_site_sel2, {updateTabsetPanel(session, inputId = 'all_tabs', selected = 'Site Selection')})
  ## "return to results" button (to go from site selection to results)
  observeEvent(input$return_to_results, {updateTabsetPanel(session, inputId = "all_tabs", selected = "See Results")})  # updateTabsetPanel(session, 'results_tabs', 'Summary')
  ## "return to results" button, shown once data_processed() 
  observe({
    if (isTruthy(data_processed())) {
      shinyjs::show(id = 'return_to_results')
    } else {
      shinyjs::hide(id = 'return_to_results')
    }
  })
  
  ## hide vs show ADVANCED tab at start  ---------------------- #   ***
  
  if (default_hide_advanced_settings) {
    hideTab(inputId = 'all_tabs', target = 'Advanced Settings')}
  ## hide vs show ADVANCE tab on button click (button in 'About EJAM' tab) ***
  observeEvent(input$ui_show_advanced_settings,
               {showTab(inputId = 'all_tabs', target = 'Advanced Settings')})
  observeEvent(input$ui_hide_advanced_settings,
               {hideTab(inputId = 'all_tabs', target = 'Advanced Settings')})
  
  ## buttons to see help info  ---------------------- #  
  
  observeEvent(input$latlon_help, {
    showModal(modalDialog(HTML(latlon_help_msg), easyClose = TRUE))})
  observeEvent(input$frs_help, {
    showModal(modalDialog(HTML(frs_help_msg),    easyClose = TRUE))})
  observeEvent(input$epa_program_help, {
    showModal(modalDialog(HTML(epa_program_help_msg), easyClose = TRUE))})
  observeEvent(input$fips_help, {
    showModal(modalDialog(HTML(fips_help_msg),   easyClose = TRUE))})
  observeEvent(input$shp_help, {
    showModal(modalDialog(HTML(shp_help_msg),    easyClose = TRUE))})
  # . --------------------------------------------------------------- ####       
  
  #. ## ##
  
  # ______ SELECT SITES ________####
  #. ####
  
  # update ss_select_NAICS input options ###
  observeEvent(input$add_naics_subcategories, {
    #req(input$add_naics_subcategories)
    
    ## switch labels based on subcategory radio button
    if(input$add_naics_subcategories){
      naics_choices <- setNames(naics_counts$NAICS,naics_counts$label_w_subs)
    } else{
      naics_choices <- setNames(naics_counts$NAICS,naics_counts$label_no_subs)
    }
    
    vals <- input$ss_select_naics
    # update ss_select_NAICS input options ###
    updateSelectizeInput(session, inputId = 'ss_select_naics',
                         ## use named list version, grouped by first two code numbers
                         choices = naics_choices, # need to keep formatting
                         selected = vals,
                         #choices = NAICS, # named list of codes, data loaded with EJAM package
                         server = TRUE)
  })
 
  
  # update ss_select_SIC input options ###
  updateSelectizeInput(session, inputId = 'ss_select_sic',
                       choices = SIC, # named list of codes
                       server = TRUE)
  
  #############################################################################  # 
  
  # SELECT Facility Type vs UPLOAD Latlon/ id/ fips/ shape (radio button) ####
  # keep track of currently used method of site selection
  current_upload_method <- reactive({
    x <- switch(
      input$ss_choose_method,
      'dropdown' = switch(input$ss_choose_method_drop,
                          NAICS =          "NAICS",     # 'NAICS (industry name or code)'
                          EPA_PROGRAM =    "EPA_PROGRAM_sel",
                          SIC =            "SIC" ,
                          MACT =           "MACT"),
      'upload'   = switch(input$ss_choose_method_upload,
                          SHP =              "SHP",
                          latlon =           "latlon",    # 'Location (lat/lon)',
                          #latlontypedin =   "latlontypedin",
                          EPA_PROGRAM =      "EPA_PROGRAM_up",
                          FRS =              "FRS",       # 'FRS (facility ID)',
                          #ECHO =            "ECHO",      # 'ECHO Search Tools',
                          FIPS =             "FIPS")
    )
    cat('current_upload_method reactive is ', x, '\n')
    
    cat('current input$ss_choose_method      is ', input$ss_choose_method,      '\n')
    if (input$ss_choose_method == "dropdown") {cat('current input$ss_choose_method_drop is ', input$ss_choose_method_drop, '\n')}
    if (input$ss_choose_method == "upload") {cat('current input$ss_choose_method_upload is ', input$ss_choose_method_upload, '\n')}
    
    x
  })
  # reactive to keep track of data type used in last analysis
  submitted_upload_method <- reactiveVal(NULL)
  observeEvent(input$bt_get_results, {
    submitted_upload_method(current_upload_method())
  })
  
  observeEvent(input$show_data_preview,
               {
                showModal( shiny::modalDialog(title = 'Selected location data', size = 'l', easyClose=TRUE, helpText('View or download data corresponding to your upload/selections.'),
                                    ## use download buttons for speed and handling larger data
                                    downloadButton('download_preview_data_csv', label = 'CSV', class = 'usa-button'),
                                    downloadButton('download_preview_data_xl', label = 'Excel', class = 'usa-button'),
                                    br(),br(),
                                    DT::DTOutput('print_test2_dt', width = '100%')))
               })
  
  #############################################################################  # 
  
  ## HTML for alert for invalid sites
  #invalid_alert <- reactiveVal(NULL)
  
  invalid_alert <- reactiveValues('latlon'=0,'NAICS'=0,'SIC'=0,
                                       'FRS'=0,'EPA_PROGRAM_up'=0,
                                  'EPA_PROGRAM_sel'=0,
                                       'MACT'=0,'FIPS'=0,'SHP'=0)
  
  ## reactive: SHAPEFILES uploaded ####
  
  data_up_shp <- reactive({
    ##
    req(input$ss_upload_shp)
    infiles <- input$ss_upload_shp$datapath # get path and temp (not original) filename of the uploaded file
    print(infiles)
    
    ####### SHOULD REPLACE WITH CODE IN NEW FUNCTIONS that avoid saving uploaded shapefiles locally on server LIKE shapefile_from_filepaths() ***
    # 
    if ("working_HERE?" == "working_NOT_IN_SHINY_ONLY") { # new way. works outside shiny, at least.
      
      if (!shapefile_filepaths_valid(filepaths = infiles)) { # done by shapefile_from_filepaths() too but this allows shiny validate()
        disable_buttons[['SHP']] <- TRUE
        validate('Not all required file extensions found.')
      }
      shp <- shapefile_from_filepaths(infiles, cleanit = FALSE) # cleanit = FALSE allows shiny to handle that with messages
      # numna <- nrow(shp[!sf::st_is_valid(shp),])
      numna <- nrow(shp[!sf::st_is_valid(shp) | sf::st_is_empty(shp), ]) # now counts and removes polygons that are not valid but also if "empty"
      invalid_alert[['SHP']] <- numna # this updates the value of the reactive invalid_alert()
      shp <- shapefile_clean(shp) # uses default crs=4269;  drops invalid rows or return NULL if none valid  # shp <- sf::st_transform(shp, crs = 4269) # done by shapefile_clean() 
      shp$valid <- !sf::st_is_empty(shp)
      
      #shp[!sf::st_is_empty(shp), ] # *** remove this if shapefile_clean() will do it
      
      #shp <- shp[!sf::st_is_empty(shp), ] # *** remove this if shapefile_clean() will do it
      if (is.null(shp)) {
        invalid_alert[['SHP']] <- 0 # hides the invalid site warning
        an_map_text_shp(HTML(NULL)) # hides the count of uploaded shapes
        disable_buttons[['SHP']] <- TRUE
        shiny::validate('No shapes found in file uploaded.')
      }
      disable_buttons[['SHP']] <- FALSE
      
      shp <- cbind(ejam_uniq_id = 1:nrow(shp), shp)
      class(shp) <- c(class(shp), 'data.table')
      shp
      ####### #    #######     ####### #
    } else {
      
      # older way  
      
      infile_ext <- tools::file_ext(infiles)
      if (!all(c('shp','shx','dbf','prj') %in% infile_ext)) {
        disable_buttons[['SHP']] <- TRUE
        shiny::validate('Not all required file extensions found.')
      }
      
      ########################################## #
      # ---- This renames file from ugly tempfile name to original name as selected on user's drive - but why bother? --- # 
      dir <- unique(dirname(infiles)) # get folder (a temp one created by shiny for the uploaded file)
      outfiles <- file.path(dir, input$ss_upload_shp$name) # create new path\name from temp dir plus original filename of file selected by user to upload
      name <- strsplit(input$ss_upload_shp$name[1], "\\.")[[1]][1] # ??? get filename minus extension, of 1 file selected by user to upload
      purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files from ugly tempfilename to original filename of file selected by user to upload
      shp <- sf::read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
      ########################################## # 
      
      if (nrow(shp) > 0) {
        numna <- nrow(shp[!sf::st_is_valid(shp),])
        invalid_alert[['SHP']] <- numna # this updates the value of the reactive invalid_alert()
        #shp_valid <- shp[sf::st_is_valid(shp),] #determines valid shapes
        shp_valid <- dplyr::mutate(shp, siteid = row_number())
        shp_proj <- sf::st_transform(shp_valid,crs = 4269)
      } else {
        invalid_alert[['SHP']] <-0 # hides the invalid site warning
        an_map_text_shp(HTML(NULL)) # hides the count of uploaded sites/shapes
        disable_buttons[['SHP']] <- TRUE
         ## if not matched, return this message
        shiny::validate('No shapes found in file uploaded.')
      }
      disable_buttons[['SHP']] <- FALSE
      shp_proj$valid <- sf::st_is_valid(shp_proj)#!sf::st_is_empty(shp_proj)
      shp_proj <- cbind(ejam_uniq_id = 1:nrow(shp_proj), shp_proj)
      shp_proj$invalid_msg <- NA
      shp_proj$invalid_msg[shp_proj$valid==F] <- sf::st_is_valid(shp_proj[shp_proj$valid==F,], reason = TRUE)
      shp_proj$invalid_msg[is.na(shp_proj$geometry)] <- 'bad geometry'
      class(shp_proj) <- c(class(shp_proj), 'data.table')
      shp_proj
    }
    
  }) # END OF SHAPEFILE UPLOAD
  
  #############################################################################  # 
  
  ## *** note: repeated file reading code below could be replaced by  latlon_from_anything() #### 
  #  ext <- latlon_from_anything(input$ss_upload_latlon$datapath)
  
  
  ## reactive: latlon is typed in on-screen via MODULE *** ####
  # see also  EJAM/R/mod_dataentry_EXAMPLE.R
  #############################################################################  #   #############################################################################  # 
  #############################################################################  #   #############################################################################  # 
  
  # # ***   DISABLE UNTIL WORKING RIGHT?
  ####      IDEA IS TO LET USER TYPE FROM SCRATCH LAT LONS, AND 
  ####      ALSO MAYBE EDIT LATLONS ALREADY UPLOADED FROM A FILE!
  
  # # Use a default initial template of lat lon values table ready for user to type into  
  # # and then the module updates that reactive_data1 object as the user types
  # latlon_template <- data.table(lat = 0, lon = 0, siteid = 1, sitename = "")  # default_points_shown_at_startup[1:2, ] # EJAMejscreenapi::testpoints_5[1:2, ] # could  be put in global.R 
  # reactive_data1 <-  reactiveVal(latlon_template)
  # ## or... try something like this:   Try to pass to module as param the last uploaded pts() ?
  # observe(
  #   # if data_up_latlon() gets updated, then also update this reactive for use in the edited module
  #   reactive_data1(data_up_latlon())
  # )
  # 
  # MODULE_SERVER_latlontypedin(id = "pts_entry_table1", reactdat = reactive_data1) # pass points table that is reactive_data1(), but must pass it with NO parens 
  # 
  # If a module needs to use any reactive expressions, the outer function should take the reactive expression as a parameter. 
  # If a module needs to access an input that isn’t part of the module, the 
  #   containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
  #   myModule("myModule1", reactive(input$checkbox1))
  # If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.
  
  # #   )
  # # }) %>%
  # #   bindEvent(input$latlontypedin_submit_button) # (updates only when the "Done entering points" button is pressed)  
  
  # DISABLED UNTIL FIXED?
  
#   data_typedin_latlon <- reactive({
#     #   ## wait for typed in data to be submitted, then return cleaned lat lon table data.frame, as data_typedin_latlon() which eventually becomes data_uploaded()
#       req(reactive_data1() )
#     ext <- reactive_data1()  # NEED TO TEST THAT THIS IS ACTUALLY THE USER-EDITED OUTPUT OF THE MODULE   # ss_typedin_latlon()
#     #   # ext <- data.frame( siteid=1, lat=0, lon=0) # dummy data for testing
#     ###   # another approach, not used:   # ext <- DataEditR::data_edit(latlon_template)
# cat("COUNT OF ROWS IN TYPED IN DATA: ", NROW(ext),"\n")
#     ## Validate the lat lon values. If column names are found in lat/long alias comparison, clean and return the table of lat lon values
#     if (any(tolower(colnames(ext)) %in% lat_alias) & any(tolower(colnames(ext)) %in% lon_alias)) {
#       sitepoints <- ext %>%
#         EJAM::latlon_df_clean() #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
#       cat("COUNT OF VALID LAT/LON POINTS IN TYPED IN DATA: ", NROW(sitepoints),"\n")
#       sitepoints
#       # returns it here, as the last thing in the reactive
#     } else {
#       ## if not matched, show this message instead
#       shiny::validate('No lat lon coordinate columns found.')
#     }
#   })
  #############################################################################  #   #############################################################################  # 
  #############################################################################  #   #############################################################################  # 
  
  
  
  #############################################################################  # 
  ## reactive: latlon is in file uploaded ####
  
  data_up_latlon <- reactive({
    
    ## wait for file to be uploaded
    req(input$ss_upload_latlon)
    
    ## check if file extension is appropriate
    ext <- tolower(tools::file_ext(input$ss_upload_latlon$name))
    ## if acceptable file type, read in; if not, send warning text
    sitepoints <- switch(ext,
                         csv = data.table::fread(input$ss_upload_latlon$datapath),
                         xls  = readxl::read_excel(input$ss_upload_latlon$datapath) %>% data.table::as.data.table(),
                         xlsx = readxl::read_excel(input$ss_upload_latlon$datapath) %>% data.table::as.data.table(),
                         shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    cat("ROW COUNT IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")
    ## if column names are found in lat/long alias comparison, process
    if (any(tolower(colnames(sitepoints)) %in% lat_alias) & any(tolower(colnames(sitepoints)) %in% lon_alias)) {
      
      sitepoints[, ejam_uniq_id := .I]
      data.table::setcolorder(sitepoints, 'ejam_uniq_id')
      
      sitepoints <- sitepoints %>% 
        latlon_df_clean() #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
      
      sitepoints$invalid_msg <- NA
      
      sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
      #data.table::as.data.table()
      cat("ROW COUNT after latlon_df_clean(): ", NROW(sitepoints), "\n")
      disable_buttons[['latlon']] <- FALSE
      sitepoints
    } else {
      invalid_alert[['latlon']] <- 0 # hides the invalid site warning
      an_map_text_pts[['latlon']] <- NULL # hides the count of uploaded sites
      disable_buttons[['latlon']] <- TRUE
      ## if not matched, show this message instead
      shiny::validate('No coordinate columns found.')
    }
  })
  
  #############################################################################  # 
  ## reactive: latlon by FRS registry IDs ####
  
  data_up_frs <- reactive({
    ## wait for file to be uploaded
    req(input$ss_upload_frs)
    ##  >this part could be replaced by  latlon_from_anything() *** and each time it is repeated below
    # ext <- latlon_from_anything(input$ss_upload_latlon$datapath)
    
    ## check if file extension is appropriate
    ext <- tolower(tools::file_ext(input$ss_upload_frs$name))
    ## if acceptable file type, read in; if not, send warning text
    read_frs <- switch(ext,
                       csv =  read.csv(input$ss_upload_frs$datapath),
                       xls = readxl::read_excel(input$ss_upload_frs$datapath),
                       xlsx = readxl::read_excel(input$ss_upload_frs$datapath),
                       shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    ) # returns a data.frame
    cat("ROW COUNT IN FILE THAT SHOULD provide FRS REGISTRY_ID: ", NROW(read_frs), "\n")
    #include frs_is_valid verification check function, must have colname REGISTRY_ID
    if (frs_is_valid(read_frs)) {
      if ("siteid" %in% colnames(read_frs)) {
        colnames(read_frs) <- gsub("siteid", "REGISTRY_ID", colnames(read_frs))
      }
      #converts registry id to character if not already in that class ( frs registry ids are character)
      if (('REGISTRY_ID' %in% colnames(read_frs)) & (class(read_frs$REGISTRY_ID) != "character")) {
        read_frs$REGISTRY_ID = as.character(read_frs$REGISTRY_ID)
      }
      
      sitepoints <- dplyr::left_join(read_frs, frs_from_regid(read_frs$REGISTRY_ID))
      #sitepoints <- frs_from_regid(read_frs$REGISTRY_ID)
      # read_frs_dt <- data.table::as.data.table(read_frs)
      data.table::setDT(sitepoints) # same but less memory/faster?
   
      ## add ejam_uniq_id and valid T/F columns
      sitepoints[, ejam_uniq_id := .I]
      data.table::setcolorder(sitepoints, 'ejam_uniq_id')
      #site_is_invalid(sitepoints, type = 'FRS')
      sitepoints[,valid := !(REGISTRY_ID == 'NA' | is.na(lon) | is.na(lat))]
      sitepoints$invalid_msg <- NA
  
      sitepoints$invalid_msg[sitepoints$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
      sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
      sitepoints
       } else {
      invalid_alert[['FRS']] <- 0 # hides the invalid site warning
      an_map_text_pts[['FRS']] <- NULL # hides the count of uploaded sites
      disable_buttons[['FRS']] <- TRUE
      shiny::validate('Records with invalid Registry IDs')
    }
    ## return merged dataset
    cat("SITE COUNT VIA FRS frs_is_valid() looking for REGISTRY_ID: ", NROW(sitepoints), "\n")
    disable_buttons[['FRS']] <- FALSE
    sitepoints
  })
  
  #############################################################################  # 
  ## reactive: latlon by NAICS ####
  data_up_naics <- reactive({  
    ## check if anything has been selected or entered
    req(isTruthy(input$ss_select_naics))
   
    #define inputs
    naics_user_picked_from_list <- input$ss_select_naics
    add_naics_subcategories <- input$add_naics_subcategories 
    # q: IS IT BETTER TO USE THIS IN naics_from_any() OR IN frs_from_naics() BELOW ??
    
    # naics_validation function to check for non empty NAICS inputs
    if (naics_validation(naics_enter = '', naics_select = input$ss_select_naics)) {
      #if (naics_validation(input$ss_enter_naics,input$ss_select_naics)) {
      inputnaics = {}
      
      # if not empty, assume its pulled using naics_from_any() or older naics_find() above
      if (length(inputnaics) == 0 | rlang::is_empty(inputnaics)) {
        #construct regex expression and finds sites that align with user-selected naics codes
        inputnaics <- naics_user_picked_from_list
        inputnaics <- unique(inputnaics[inputnaics != ""])
        
        print(inputnaics)
        
        #merge user-selected NAICS with FRS facility location information
        # sitepoints <- frs_by_naics[NAICS %like% inputnaics ,  ]
        
        #   2. GET FACILITY LAT/LON INFO FROM NAICS CODES  
        
        sitepoints <- frs_from_naics(inputnaics, children = add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx
        
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        # print(sitepoints)
        if (rlang::is_empty(sitepoints) | nrow(sitepoints) == 0) {
          ################ Should output something saying no valid results returned ######## #
          invalid_alert[['NAICS']] <- 0 # hides the invalid site warning
          an_map_text_pts[['NAICS']] <- NULL # hides the count of uploaded sites
          disable_buttons[['NAICS']] <- TRUE
          shiny::validate('No valid locations found under this NAICS code.')
        }
      } else{  
        sitepoints <- frs_from_naics(inputnaics, children = add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx
        
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid[is.na(sitepoints$NAICS)] <- 'bad NAICS Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        # print(sitepoints)
        showNotification('Points submitted successfully!', duration = 1)
      }
    } else {
      invalid_alert[['NAICS']] <- 0 # hides the invalid site warning
      an_map_text_pts[['NAICS']] <- NULL # hides the count of uploaded sites
      disable_buttons[['NAICS']] <- TRUE
      ################ Should output something saying no valid results returned ######## #
      shiny::validate('Invalid NAICS Input')
      
    }
    cat("SITE COUNT VIA NAICS from frs_from_naics: ", NROW(sitepoints), "\n")
    ## assign final value to data_up_naics reactive variable
    sitepoints <- sitepoints %>% latlon_df_clean()
    sitepoints$invalid_msg <- NA
    sitepoints$invalid[is.na(sitepoints$NAICS)] <- 'bad NAICS Code'
    sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
    cat("SITE COUNT VIA NAICS after latlon_df_clean: ", NROW(sitepoints), "\n")
    disable_buttons[['NAICS']] <- FALSE
    sitepoints
  })
  
 
  
  #############################################################################  # 
  ## reactive: latlon by EPA Program IDs ####
  
  data_up_epa_program_up <- reactive({
    ## wait for file to be uploaded
    req(isTruthy(input$ss_upload_program))
    #req(input$submit_program)
    
    #if (input$ss_choose_method_upload == 'EPA_PROGRAM') {
      #if (input$program_ul_type == 'upload') {
      req(input$ss_upload_program)
      
      ## check if file extension is appropriate
      ext <- tolower(tools::file_ext(input$ss_upload_program$name))
      ## if acceptable file type, read in; if not, send warning text
      read_pgm <- switch(ext,
                         csv  =  data.table::fread(input$ss_upload_program$datapath),
                         xls  = readxl::read_excel(input$ss_upload_program$datapath) %>% data.table::as.data.table(),
                         xlsx = readxl::read_excel(input$ss_upload_program$datapath) %>% data.table::as.data.table(),
                         shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
      ) # returns a data.frame
      cat("ROW COUNT IN file that should have program, pgm_sys_id: ", NROW(read_pgm), "\n")
      ## error if no columns provided
      if (!any(c('program','pgm_sys_id') %in% tolower(colnames(read_pgm)))) {
        invalid_alert[['EPA_PROGRAM_up']] <- 0 # hides the invalid site warning
        an_map_text_pts[['EPA_PROGRAM_up']] <- NULL# hides the count of uploaded sites
        disable_buttons[['EPA_PROGRAM_up']] <- TRUE
        validate('Please add a file with at least these two columns: program, pgm_sys_id \n and possibly these columns as well: REGISTRY_ID,lat,lon')
      }
      
      ## convert pgm_sys_id and REGISTRY_ID columns to character before joining
      if (('pgm_sys_id' %in% colnames(read_pgm)) & (class(read_pgm$pgm_sys_id) != "character")) {
        read_pgm$pgm_sys_id = as.character(read_pgm$pgm_sys_id)
      }
      if (('REGISTRY_ID' %in% colnames(read_pgm)) & (class(read_pgm$REGISTRY_ID) != "character")) {
        read_pgm$REGISTRY_ID = as.character(read_pgm$REGISTRY_ID)
      }
      
      ## add check for program and pgm_sys_id else validate
      
      ## look for program in list from unique(frs_by_programid$program)
      
      if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")
      
      ## if any of these columns already exist, join by all of them
      if (any(c('REGISTRY_ID','lat','lon') %in% colnames(read_pgm))) {
        pgm_out <- dplyr::left_join(
          read_pgm, frs_by_programid#,
          #by = c("program", "pgm_sys_id")
        )
      } else {
        pgm_out <- dplyr::left_join(
          read_pgm, frs_by_programid,
          by = c("program", "pgm_sys_id")
        )
      }
      
      pgm_out[, ejam_uniq_id := .I]
      data.table::setcolorder(pgm_out, 'ejam_uniq_id')
      
      ## clean so that any invalid latlons become NA
      pgm_out <- pgm_out %>% 
        latlon_df_clean()
      
      pgm_out$invalid_msg <- NA
      pgm_out$invalid_msg[pgm_out$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
      pgm_out$invalid_msg[is.na(pgm_out$lon) | is.na(pgm_out$lat)] <- 'bad lat/lon coordinates'
      #} else if (input$program_ul_type == 'dropdown') {
    #} 
    ## return output dataset
    cat("SITE COUNT VIA PROGRAM ID: ", NROW(pgm_out), "\n")
    disable_buttons[['EPA_PROGRAM_up']] <- FALSE
    pgm_out
  })
  
  data_up_epa_program_sel <- reactive({
    ## wait for file to be uploaded
    #req(isTruthy(input$ss_select_program))
    #req(input$submit_program)
    
    if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")
    
   #if (input$ss_choose_method_drop == 'EPA_PROGRAM') { 
      req(isTruthy(input$ss_select_program))
      ## filter frs_by_programid to currently selected program
      pgm_out <- frs_by_programid[ program == input$ss_select_program]
      
      if(nrow(pgm_out) > 0){
        
      
      pgm_out[, ejam_uniq_id := .I]
      data.table::setcolorder(pgm_out, 'ejam_uniq_id')
      
      ## clean so that any invalid latlons become NA
      pgm_out <- pgm_out %>% 
        latlon_df_clean()
      
      pgm_out$invalid_msg <- NA
      pgm_out$invalid_msg[pgm_out$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
      pgm_out$invalid_msg[is.na(pgm_out$lon) | is.na(pgm_out$lat)] <- 'bad lat/lon coordinates'
    #}
    ## return output dataset
    cat("SITE COUNT VIA PROGRAM ID: ", NROW(pgm_out), "\n")
    disable_buttons[['EPA_PROGRAM_sel']] <- FALSE
    pgm_out
    } else {
      invalid_alert[['EPA_PROGRAM_sel']] <- 0
      an_map_text_pts[['EPA_PROGRAM_sel']] <- NULL
      disable_buttons[['EPA_PROGRAM_sel']] <- TRUE
      validate('No valid locations found under this EPA program.')
    }
  })
  
  #############################################################################  # 
  ## reactive: latlon by SIC ####
  data_up_sic <- reactive({  
    ## check if anything has been selected or entered
    req(isTruthy(input$ss_select_sic))
    #req(shiny::isTruthy(input$ss_enter_sic) || shiny::isTruthy(input$ss_select_sic))
    
    #define inputs
    add_sic_subcategories <- FALSE #input$add_naics_subcategories 
    # q: IS IT BETTER TO USE THIS IN naics_from_any() OR IN frs_from_naics() BELOW ?? ***
    
    # naics_validation function to check for non empty SIC inputs
    if (naics_validation('', input$ss_select_sic)) {
      inputsic = {}
      # if not empty, assume its pulled using naics_from_any() or older naics_find() above
      if (length(inputsic) == 0 | rlang::is_empty(inputsic)) {
        #construct regex expression and finds sites that align with user-selected SIC codes
        inputsic <- input$ss_select_sic #c(sic_wib_split, input$ss_select_sic)
        inputsic <- unique(inputsic[inputsic != ""])
        cat("selected SIC:  ")
        print(inputsic)
        #merge user-selected NAICS with FRS facility location information
        #sitepoints <- frs_by_sic[SIC %like% inputsic ,  ]
        
        #   2. GET FACILITY LAT/LON INFO FROM SIC CODES
        
        # print('testb')
        sitepoints <- frs_from_sic(inputsic, children = add_sic_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,SIC)] # xxx
       
        sitepoints[, `:=`(ejam_uniq_id = .I, 
                          valid = !is.na(lon) & !is.na(lat))]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid[is.na(sitepoints$SIC)] <- 'bad SIC Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        # print(sitepoints)
        if (rlang::is_empty(sitepoints) | nrow(sitepoints) == 0) {
          ################ Should output something saying no valid results returned ######## #
          
          invalid_alert[['SIC']] <- 0 # hides the invalid site warning
          an_map_text_pts[['SIC']] <- NULL # hides the count of uploaded sites
          disable_buttons[['SIC']] <- TRUE
          shiny::validate('No valid locations found under this SIC code.')
        }
      } else {
        sitepoints <- frs_from_sic(inputsic, children = add_sic_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,SIC)] # xxx
        sitepoints[, `:=`(ejam_uniq_id = .I, 
                        valid = !is.na(lon) & !is.na(lat))]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid[is.na(sitepoints$SIC)] <- 'bad SIC Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        showNotification('Points submitted successfully!', duration = 1)
      }
    } else {
      invalid_alert[['SIC']] <- 0 # hides the invalid site warning
      an_map_text_pts[['SIC']] <- NULL # hides the count of uploaded sites
      ################ Should output something saying no valid results returned ######## #
      disable_buttons[['SIC']] <- TRUE
      shiny::validate('Invalid SIC Input')
    }
    cat("SITE COUNT VIA SIC: ", NROW(sitepoints), "\n")
    ## assign final value to data_up_naics reactive variable
    disable_buttons[['SIC']] <- FALSE
    return(sitepoints)
  })
  
  ## reactive: latlon by FIPS ####
  
  data_up_fips <- reactive({
    req(input$ss_upload_fips)
    
    ## check if file extension is appropriate
    ext <- tolower(tools::file_ext(input$ss_upload_fips$name))
    ## if acceptable file type, read in; if not, send warning text
    fips_dt <- switch(ext,
                      csv  =  data.table::fread(input$ss_upload_fips$datapath),
                      xls  = readxl::read_excel(input$ss_upload_fips$datapath) |>  data.table::as.data.table(),
                      xlsx = readxl::read_excel(input$ss_upload_fips$datapath) |>  data.table::as.data.table(),
                      shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
    )
    cat("COUNT OF ROWS IN FIPS FILE: ", NROW(fips_dt),"\n")
    
    ################################################################################### # 
    if (1 == 1) {
      
      fips_vec <- fips_from_table(fips_table = fips_dt, addleadzeroes = TRUE, inshiny = TRUE)
      #fips_vec <- fips_out$vec

      if(is.null(fips_vec)){
        disable_buttons[['FIPS']] <- TRUE
        invalid_alert[['FIPS']] <-0  # hides the invalid site warning
        an_map_text_fips(HTML(NULL)) # hides the count of uploaded sites
        fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
        
        validate(paste0('No FIPS column found. Please use one of the following names: ', paste0(fips_alias, collapse = ', ')))
      } else{
        disable_buttons[['FIPS']] <- FALSE
        cat("COUNT OF FIPS via fips_from_table(): ", length(fips_vec), '\n')
        # now let ejamit() do the rest for the FIPS case
        fips_vec
      }
     
    } else {  # OLDER VERSION NOT USING FUNCTIONS
      
      ## create named vector of FIPS codes (names used as siteid)
      fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
      if (any(tolower(colnames(fips_dt)) %in% fips_alias)) {
        firstmatch <- intersect(fips_alias, colnames(fips_dt))[1]
        fips_vec <- fips_lead_zero(as.character(fips_dt[[firstmatch]]))
        names(fips_vec) <- as.character(fips_vec)
      } else {
        invalid_alert[['FIPS']] <-0  # hides the invalid site warning
        an_map_text_fips(HTML(NULL)) # hides the count of uploaded sites
        disable_buttons[['FIPS']] <- TRUE
        validate(paste0('No FIPS column found. Please use one of the following names: ', paste0(fips_alias, collapse = ', ')))
      }
      ## create two-column dataframe with bgs (values) and original fips (ind)
      all_bgs <- stack(sapply(fips_vec, fips_bg_from_anyfips))
      names(all_bgs) <- c('bgfips','siteid') 
      all_bgs$siteid <- as.character(all_bgs$siteid) # because stack() always creates a factor column. data.table might have a faster reshaping approach? ***
      
      ## only process blockgroups exist for uploaded data
      
      # **** find a way to avoid using blockid2fips if possible, since it is so huge in memory 
      
      if (nrow(all_bgs) > 0) {
        fips_blockpoints <- dplyr::left_join(all_bgs, 
                                             ## create 12-digit column inline (original table not altered)
                                             blockid2fips[, .(blockid, blockfips, blockfips12 = substr(blockfips,1,12))], 
                                             by = c('bgfips' = 'blockfips12'), multiple = 'all') |> 
          dplyr::left_join(blockpoints) |>  
          dplyr::mutate(distance = 0) |>  
          data.table::as.data.table()
        ## remove any invalid latlon values 
        cat("COUNT OF blocks BASED ON FIPS: ", NROW(fips_blockpoints), '\n')
        disable_buttons[['FIPS']] <- FALSE
        
         return(fips_blockpoints)
      } else {
        invalid_alert[['FIPS']] <- 0 # hides the invalid site warning
        an_map_text_fips(HTML(NULL)) # hides the count of uploaded sites
        disable_buttons[['FIPS']] <- TRUE
        
        ## if not matched, return this message
        shiny::validate('No blockgroups found for these FIP codes.')
      }
    }
  }) # END OF FIPS UPLOAD
  ################################################################################### # 
    
  
  ## reactive: latlon by MACT subpart ####
  
  data_up_mact <- reactive({
    
    req(isTruthy(input$ss_select_mact))
    
    if (!exists("frs_by_mact")) dataload_from_pins("frs_by_mact")
    
    ## filter frs_by_mact to currently selected subpart
    mact_out <- frs_by_mact[ subpart == input$ss_select_mact]
    cat("COUNT OF FACILITIES BY MACT: ", NROW(mact_out), "\n")
    ## remove any facilities with invalid latlons before returning
    #mact_out <- mact_out[!is.na(lat) & !is.na(lon),]
    cat("COUNT OF FACILITIES BY MACT with lat lon values: ", NROW(mact_out), "\n")
    if (all(is.na(mact_out$lat)) & all(is.na(mact_out$lon))) {
      invalid_alert[['MACT']] <- nrow(mact_out)
      an_map_text_pts[['MACT']] <- NULL
      disable_buttons[['MACT']] <- TRUE
      validate('No valid locations found under this MACT subpart')
    } else {
      disable_buttons[['MACT']] <- FALSE
      
      # mact_out[, `:=`(ejam_uniq_id = .I, 
      #                 valid = !is.na(lon) & !is.na(lat))]
      mact_out$ejam_uniq_id <- 1:nrow(mact_out)
      mact_out$valid <- !is.na(mact_out$lon) & !is.na(mact_out$lat)
      data.table::setcolorder(mact_out, 'ejam_uniq_id')
      
      mact_out$invalid_msg <- NA
      mact_out$invalid_msg[is.na(mact_out$lon) | is.na(mact_out$lat)] <- 'bad lat/lon coordinates'
      ## return output dataset
      mact_out
    }
  }) 
  ##################################################### # 
  
  # PLACES ARE READY TO ANALYZE ####
  
  ## data_uploaded() reactive holds the points or shapefiles ####
  
  data_uploaded <- reactive({
    
    ## if >1 upload method used, use the one currently indicated by radio button ss_choose_method 
    
    if        (current_upload_method() == 'latlon'        ) {data_up_latlon()
    #} else if (current_upload_method() == 'latlontypedin' ) {data_typedin_latlon()
    } else if (current_upload_method() == 'NAICS'         ) {data_up_naics()
    } else if (current_upload_method() == 'FRS'           ) {data_up_frs()
      #} else if (current_upload_method() == 'ECHO'          ) {data_up_echo()
    } else if (current_upload_method() == 'EPA_PROGRAM_sel'   ) {data_up_epa_program_sel()
    } else if (current_upload_method() == 'EPA_PROGRAM_up'   ) {data_up_epa_program_up()
    } else if (current_upload_method() == 'SIC'           ) {data_up_sic()
    } else if (current_upload_method() == 'FIPS'          ) {data_up_fips() # blocks nearby, not lat/lon values
    } else if (current_upload_method() == 'MACT'          ) {data_up_mact()
    } else if (current_upload_method() == 'SHP'           ) {data_up_shp()  # shapefiles, not lat/lon values
    }
    
  })
  #############################################################################  # 
  
  disable_buttons <- reactiveValues('FIPS'=TRUE,'SHP'=TRUE,
                                    'latlon'=TRUE,'FRS'=TRUE,
                                    'EPA_PROGRAM_up'=TRUE,'EPA_PROGRAM_sel'=TRUE,
                                    'NAICS'=TRUE,'SIC'=TRUE,
                                    'MACT'=TRUE)
  
  ## disable run, hide preview button, until an upload. input$ss_upload_latlon or whatever and current_upload_method() say there is data uploaded; then enable and show
  observe({
    
    if (current_upload_method() == 'latlon') {
      #if (!isTruthy(input$ss_upload_latlon)) {
      if(disable_buttons[['latlon']]){
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    # } else if (current_upload_method() == 'latlontypedin') {
    #   if (!isTruthy(input$ss_typedin_latlon)) {              #  
    #     shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
    #   } else {
    #     shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
    #   }
      
    } else if (current_upload_method() == 'FRS') {
      #if (!isTruthy(input$ss_upload_frs)) {
      if(disable_buttons[['FRS']]){
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
        } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'NAICS') {
      #if (!isTruthy(input$ss_select_naics)) {
      if(disable_buttons[['NAICS']]){
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
        } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'EPA_PROGRAM_up') {
      if(disable_buttons[['EPA_PROGRAM_up']]){
      #if ((input$ss_choose_method == 'upload' & !isTruthy(input$ss_upload_program)) |
      #    (input$ss_choose_method == 'dropdown' & !isTruthy(input$ss_select_program))) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
        } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'EPA_PROGRAM_sel') {
      if(disable_buttons[['EPA_PROGRAM_sel']]){
        #if ((input$ss_choose_method == 'upload' & !isTruthy(input$ss_upload_program)) |
        #    (input$ss_choose_method == 'dropdown' & !isTruthy(input$ss_select_program))) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
    } else if (current_upload_method() == 'SIC') {
      if(disable_buttons[['SIC']]){
      #if (!isTruthy(input$ss_select_sic)) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
         } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'FIPS') {
      #if (!isTruthy(input$ss_upload_fips)) {
       if(disable_buttons[['FIPS']]){
        #if(isTruthy(data_up_fips())){
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_fips(HTML(NULL))
        } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'MACT') {
      if(disable_buttons[['MACT']]){
      #if (!isTruthy(input$ss_select_mact)) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
        } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'SHP') {
      if(disable_buttons[['SHP']]){
      #if (!isTruthy(input$ss_upload_shp)) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_shp <- NULL
        } else {
        shinyjs::enable(id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
    }
    
    cat("Enabled/disabled button to get results, and showed/hid data preview based on current_upload_method() ==  ", current_upload_method(), "\n\n")
  })
  
  #############################################################################  # 
  #. ####
  # ______ VIEW SITES (uploaded; not yet processed) ####
  #. ####
  
  output$invalid_sites_alert2 <- renderUI({
    req(invalid_alert[[current_upload_method()]])
    if (invalid_alert[[current_upload_method()]] > 0) {
      if( input$ss_choose_method == 'dropdown'){
        HTML(paste0(
          '<section
  class="usa-site-alert usa-site-alert--emergency usa-site-alert--slim"
  aria-label="Site alert,,,,,,"
>
  <div class="usa-alert">
    <div class="usa-alert__body">
      <p class="usa-alert__text">
        <strong>', ' Warning! ','</strong>', 'There are ', prettyNum(invalid_alert[[current_upload_method()]],big.mark = ","), ' selected sites without associated lat/lon information.',
'</p>
    </div>
  </div>
</section>'))
      } else if( input$ss_choose_method == 'upload'){
        
      
      HTML(paste0(
        '<section
  class="usa-site-alert usa-site-alert--emergency usa-site-alert--slim"
  aria-label="Site alert,,,,,,"
>
  <div class="usa-alert">
    <div class="usa-alert__body">
      <p class="usa-alert__text">
        <strong>', 'Warning! ','</strong>', 'There are ', invalid_alert[[current_upload_method()]], ' invalid location(s) in your dataset.',
'</p>
    </div>
  </div>
</section>'))
      }
    } else {
      HTML(NULL)
    }
    
  })
  
  ## initialize reactive for count of uploaded shapefiles
  an_map_text_shp <- reactiveVal(HTML(NULL))
  
  ## update count of uploaded shapefiles
  observe({
    req(data_uploaded())
    if (current_upload_method() == "SHP") {
      
      shp <- data_uploaded()#[['shape']]
      num_na <- 0 # we do not keep track of invalid shapefile polygons uploaded
      num_notna <- NROW(data_uploaded()) #[['shape']])
      
      
      msg <- HTML(paste0(
        "<span style='border: 1px solid #005ea2; padding: 10px;'>Total shape(s) uploaded: <strong>", 
        prettyNum(num_na + num_notna, big.mark = ","),"</strong></span>"
      ))
      an_map_text_shp(msg)
      
    }
  })
  
  ## initialize reactive for count of uploaded FIPS
  an_map_text_fips <- reactiveVal(HTML(NULL))
  
  observe({
    req(data_uploaded())
    
    if(current_upload_method() == "FIPS"){
    
    num_na <- 0 # we do not keep track of invalid FIPS uploaded
    num_locs <- NROW(data_uploaded())
    
     msg <- HTML(paste0(
      "<span style='border: 1px solid #005ea2; padding: 10px;'>Total location(s) uploaded by FIPS: <strong>", 
      prettyNum(num_locs, big.mark = ","),"</strong></span>"
     ))
     an_map_text_fips(msg)
    }
  })
  
  ## initialize reactive for count of uploaded points
  #an_map_text_pts <- reactiveVal(NULL)
  
  an_map_text_pts <-  reactiveValues('latlon'=NULL,
                                     'NAICS'=NULL,'SIC'=NULL,
                                     'FRS'=NULL,'EPA_PROGRAM_up'=NULL,
                                     'EPA_PROGRAM_sel'=NULL,
                                     'MACT'=NULL)
  
  observe({
    req(data_uploaded())
    if(!current_upload_method() %in% c('FIPS','SHP')){
    
    lat_or_lon.na <- (is.na(data_uploaded()$lat) | is.na(data_uploaded()$lon))
    if (nrow(data_uploaded()) > 1) {
      num_na <- NROW(data_uploaded()[lat_or_lon.na,  ]) # if uploaded multiple rows (points)
    } else {
      num_na <- NROW(data_uploaded()[lat_or_lon.na])   # if uploaded only one row (point) (or does that already have invalid ones removed?)
    } 
    totalcount <- NROW(data_uploaded())
    num_notna <- totalcount - num_na
    
    ## if invalid data found, set invalid_alert() otherwise closeAlert()
    cat("Number of points:  "); cat(totalcount, 'total,', num_notna, 'valid,', num_na, ' invalid \n')
    if (num_na > 0) {
      #invalid_alert(num_na)
      invalid_alert[[current_upload_method()]] <- num_na
    } else {
      #invalid_alert(NULL)
      invalid_alert[[current_upload_method()]] <- 0
    }
    
    msg <- HTML(paste0(
      "<span style='border: 1px solid #005ea2; padding: 10px;'>Total location(s) uploaded: <strong>", prettyNum(num_na + num_notna, big.mark = ","),"</strong></span>"
      #"<br>","Site(s) with invalid lat/lon values: <strong>", prettyNum(num_na,big.mark=","), "</strong>","</span>"
    ))
    an_map_text_pts[[current_upload_method()]] <- msg
    }
  })
  
  ## Which points are valid?(and  how many; warn if 0) ####
  output$an_map_text <- renderUI({

    req(data_uploaded())
    if(current_upload_method() == 'SHP'){
      an_map_text_shp()
    }else if(current_upload_method() == 'FIPS' ){
      an_map_text_fips()
    } else if(current_upload_method() %in% c('MACT','latlon','FRS','NAICS','SIC',
                                             'EPA_PROGRAM_up','EPA_PROGRAM_sel')){
      an_map_text_pts[[current_upload_method()]]
    } else {
      HTML(NULL)
    }
   
  })
  
  ## Which points are clustered? (may double-count people) ####
  # note this had been done in EJAMejscreenapi::addlinks_clusters_and_sort_cols() 
  # is_clustered <- shiny::reactive({
  #   req(data_uploaded())
  #   
  #   # which sites have residents that might also be near others sites?
  #   # circles overlap if 2 facilities are twice the radius apart  # in miles
  #   EJAMejscreenapi::near_eachother(
  #     lon = data_uploaded()$lon, 
  #     lat = data_uploaded()$lat,
  #     distance = 2 * input$bt_rad_buff
  #     ## if switching units between miles and km - not currently used
  #     # distance = ifelse(input$radius_units == 'miles', 
  #     #                   2 * input$bt_rad_buff,
  #     #                   2 * input$bt_rad_buff * 0.62137119
  #     #)
  #   ) 
  # })
  ######################################  #
  
  # *TABLE of uploaded points ####
  
  output$print_test2_dt <- DT::renderDT(
    ## server = FALSE forces download to include all rows, not just visible ones
    server = TRUE, {
      req(data_uploaded())
      
      #dt <- data_uploaded() # now naics-queried sites format is OK to view, since using different function to get sites by naics
      if (current_upload_method() == "SHP") {
        dt <- data_uploaded()#[['shape']]
      } else if (current_upload_method() == 'FIPS'){
        dt <- data.table(FIPS = data_uploaded())[, .(FIPS, type = fipstype(FIPS), name = fips2name(FIPS))]
      } else {
        dt <- data_uploaded()
      }
      # if (current_upload_method() == 'NAICS') {
      
      ###takes NAICS codes selected, finds NAICS descriptions, and presents them  
      # dt_result_by_naic = data_uploaded()[, .(Count = .N), by = NAICS]
      # naics_desc = EJAM::NAICS[EJAM::NAICS %in% dt_result_by_naic$NAICS]
      # dt_names = data.frame("NAICS"=naics_desc,"Description"=names(naics_desc))
      # naicsdt = merge(x = dt_result_by_naic, y = dt_names, by='NAICS')
      # naics_reorder = data.frame(naicsdt$Description,naicsdt$Count)
      # colnames(naics_reorder) = c("NAICS Code","Facility Count")
      # dt <- naics_reorder
      ###print(naics_reorder,row.names = FALSE)
      # } else {
      # dt <- data_uploaded()
      # }
      
      DT::datatable(dt, 
                    ## to add download buttons
                    #extensions = 'Buttons',
                    
                    ##                                   keep rownames in display table
                    rownames = TRUE,                     ### ??? really ?
                    
                    options = list(pageLength = 100, 
                                   scrollX = TRUE, 
                                   scrollY = '500px',
                                   
                                   columnDefs = list(
                                     ##                label rownames to remove from download  ??? really ?
                                     list(
                                       targets = 0, className = "rownames"
                                     )
                                   )#,
                                   
                                   ## to specify button placement - "B"= buttons, see https://datatables.net/reference/option/dom 
                                   #dom ='Brtip',
                                   # buttons = list(
                                   #   ## customize CSV button
                                   #   list(extend = 'csv',
                                   #        ## name of downloaded file
                                   #        filename = 'ejam_raw_data_download',
                                   #        ## drop rownames for download
                                   #        exportOptions = list(columns = ":not(.rownames)")
                                   #   ),
                                   #   ## customize Excel button
                                   #   list(extend = 'excel',
                                   #        ## name of downloaded file
                                   #        filename = 'ejam_raw_data_download',
                                   #        ## drop title row from download
                                   #        title = NULL,
                                   #        ## drop rownames for download
                                   #        exportOptions = list(columns = ":not(.rownames)")
                                   #    )
                                   #  )
                                   
                    ), # end options
                    
                    escape = FALSE) # escape = FALSE may add security issue but makes links clickable in table
    })
  ######################################  #
  
  ## use external download buttons for preview data
  ## this allows loading the table on the server-side which improves speed and avoids
  ## crashes with larger datasets
  output$download_preview_data_xl <- downloadHandler(filename = 'epa_raw_data_download.xlsx',
                                                     content = function(file) {
                                                       dt <- data_uploaded()
                                                       writexl::write_xlsx(dt, file)})
  output$download_preview_data_csv <- downloadHandler(filename = 'epa_raw_data_download.csv',
                                                      content = function(file) {
                                                        dt <- data_uploaded()
                                                        data.table::fwrite(dt, file, append = F)})
  
  #############################################################################  # 
  
  # *MAP of uploaded points ####
  
  ## disable radius slider when FIPS is selected
  observe({
    if(current_upload_method() == 'FIPS'){
      shinyjs::disable(id = 'bt_rad_buff')
    } else {
      shinyjs::enable(id = 'bt_rad_buff')
    }
  })
  
  ## create different radius values for each site selection type
  current_slider_val <- reactiveValues('latlon'=1,'NAICS'=1,'SIC'=1,
                                       'FRS'=1,'EPA_PROGRAM_up'=1,
                                       'EPA_PROGRAM_sel'=1,
                                       'MACT'=1,'FIPS'=0,'SHP'=0)
  ## update stored radius when slider changes
  observeEvent(
    input$bt_rad_buff,
    current_slider_val[[current_upload_method()]] <- input$bt_rad_buff
  )
  
  ## restore previous radius value when returning to site selection type
  observeEvent(eventExpr = {
    current_upload_method()
  },{
    updateSliderInput(session, inputId = 'bt_rad_buff', 
                      val = current_slider_val[[current_upload_method()]])
  })
  
  ## add warning and disable button if radius is set to 0 for points
  observe({
    if(!(current_upload_method() %in% c('FIPS','SHP'))){
      if(current_slider_val[[current_upload_method()]] == 0){  
      shinyjs::disable(id = 'bt_get_results')
        showNotification(id = 'radius_warning', session=session,
                         duration=NULL,type='warning',closeButton = F,
                        'Please use a radius greater than 0 for analyzing points.')
      } else {
        shinyjs::enable(id ='bt_get_results')
        removeNotification(id='radius_warning', session=session)
      }
    } else {
      shinyjs::enable(id ='bt_get_results')
      removeNotification(id='radius_warning', session=session)
    }
  })
  
  ## Create separate radius label to allow line break
  
  output$radius_label <- renderUI({
    val <- input$bt_rad_buff
    lab <- paste0('<b>Radius of circular buffer: <br/>', val, ' miles ','(',round(val / 0.62137119, 2), ' km)</b>')
    
    HTML(lab)
  })
  
  orig_leaf_map <- reactive({
    
    # ***
    ## or... 
    # mapfast(data_uploaded(), radius = input$bt_rad_buff, column_names = "ej")
    ## or...
    # map_facilities(mypoints = data_uploaded(), #as.data.frame(data_uploaded()), 
    #                rad = input$bt_rad_buff, 
    #                highlight = input$an_map_clusters,
    #                clustered = is_clustered())
    
      if (current_upload_method() == "SHP") {
        # ---------------------------------------------- MAPPING SHAPES 
        req(data_uploaded())
        ## start map zoomed to the bbox / bounding box that encompasses all the points to be shown
      
        bbox <- sf::st_bbox(data_uploaded())
        leaflet() %>% addTiles() %>%
          fitBounds(
            lng1 = as.numeric(bbox[1]), lng2 = as.numeric(bbox[3]),
            lat1 = as.numeric(bbox[2]), lat2 = as.numeric(bbox[4])
          )
        #d_upload <- data_uploaded()[['points']]
        #max_pts <- max_points_can_map_poly
        
      } else if (current_upload_method() == 'FIPS') {
        # ---------------------------------------------- MAPPING FIPS CENSUS UNITS ?
        #req(data_uploaded())
        max_pts <- max_points_can_map_poly
        validate('Mapping for FIPS codes not yet available') # ***
        # leaflet() %>% addTiles() %>% 
        #   fitBounds(lng1 = min(data_uploaded()$lon, na.rm=T),
        #             lng2 = max(data_uploaded()$lon, na.rm=T),
        #             lat1 = min(data_uploaded()$lat, na.rm=T),
        #             lat2 = max(data_uploaded()$lat, na.rm=T))
        
      } else {
        # ---------------------------------------------- MAPPING LAT LON POINTS 
        d_upload <- data_uploaded()
        max_pts <- max_points_can_map # at some point, this will be edited so it is set by global in a way that allows advanced tab to modify it (like ejscreenapi module handled it)
        
        if (nrow(data_uploaded()) > max_pts) {
          ## Max allowed points was exceeded! see code in ejscreenapi that handled that case using  input$max_pts_map 
          validate(paste0('Too many points (> ', prettyNum(max_pts, big.mark = ','),') uploaded for full map to be displayed - will try to show a subset'))
          
          
          # add code here to show just a subset not > max points allowed ***
          
          
          
        } else {
          
          ## If more than one valid point...
          if (sum((
            !is.na(data_uploaded()$lat) & 
            !is.na(data_uploaded()$lon)
          )) > 1) {
            leaflet() %>% addTiles() %>% 
              fitBounds(lng1 = min(data_uploaded()$lon, na.rm = T),
                        lng2 = max(data_uploaded()$lon, na.rm = T),
                        lat1 = min(data_uploaded()$lat, na.rm = T),
                        lat2 = max(data_uploaded()$lat, na.rm = T))
            ## if only one valid point
          } else {
            leaflet() %>% addTiles() %>% 
              setView(lat = data_uploaded()$lat, lng = data_uploaded()$lon, zoom = 10)
          }
        }
      }
      
    
  })
  ######################################  #######################################  #
  
  ## output: draw map of uploaded points
  
  output$an_leaf_map <- leaflet::renderLeaflet({
    ## check if map of uploaded points exists
    
      tryCatch({
        orig_leaf_map()
      },
        shiny.silent.error = function(e) {
          
        ## if does not exist, use blank map of US
        leaflet() %>% addTiles() %>% setView(lat = 39.8283, lng = -98.5795, zoom = 4)
          
      })

  })
  
  ## add FIPS placeholder text - remove when FIPS mapping is made available
  output$fips_placeholder <- renderUI({
    if(current_upload_method() == 'FIPS'){
      helpText('Mapping for FIPS codes not yet available')
    } else {
      HTML(NULL)
    }
  })
  
  #############################################################################  # 
  # . --------------------------------------------------------------- # ###       
  
  #. ####
  # ______ RUN ANALYSIS  (when button is pressed) ________####
  #. ####
  
  ## data_processed()  reactive holds results of doaggregate()
  ## data_summarized() reactive holds results of batch.summarize()
  
  # THIS IS VERY VERY SIMILAR TO THE CODE IN ejamit() and perhaps could just rely on one set of code for both. ***xxx
  # >this part could be replaced by ejamit() or something like that ####
  
  observeEvent(input$bt_get_results, {  # (button is pressed)  
    
    showNotification('Processing sites now!', type = 'message', duration = 1)
    
    ## progress bar setup overall for 3 operations  (getblocksnearby, doaggregate, batch.summarize)
    progress_all <- shiny::Progress$new(min = 0, max = 1)
    progress_all$set(value = 0, message = 'Step 1 of 3', detail = 'Getting nearby census blocks')
    
    #############################################################################  # 
    # 1) **EJAM::getblocksnearby()** ####
    
    ################################################# # 
    
    ## get blocks in FIPS   ####
    
    if (submitted_upload_method() == 'FIPS') {  # if FIPS, do everything in 1 step right here.
      d_upload <- data_uploaded()
      fips_valid <- sapply(d_upload, function(x) !all(is.na(fips_bg_from_anyfips(x))))
      d_upload <- data.table(ejam_uniq_id=d_upload, valid = fips_valid)
      #d_upload[, ejam_uniq_id := .I]
      #setcolorder(d_upload, 'ejam_uniq_id')
      d_upload$invalid_msg <- NA
      d_upload$invalid_msg[!d_upload$valid] <- 'bad FIPS code'

      
      out <- ejamit(fips = data_uploaded(), 
                    radius = 999, # because FIPS analysis
                    maxradius = input$maxradius,
                    avoidorphans = input$avoidorphans,
                    quadtree = localtree,
                    # countcols = NULL,
                    # popmeancols = NULL,
                    # calculatedcols = NULL,
                    subgroups_type = input$subgroups_type,
                    include_ejindexes   = (input$include_ejindexes == "TRUE"), # it was character not logical because of how input UI done 
                    calculate_ratios = input$calculate_ratios,
                    extra_demog = input$extra_demog,
                    need_proximityscore = FALSE, #input$need_proximityscore, # not relevant for FIPS
                    # infer_sitepoints = FALSE,
                    # need_blockwt = TRUE,
                    threshold1 = input$an_thresh_comp1, # list(input$an_thresh_comp1) # not sure this is needed or works here
                    # updateProgress = ??? , # not sure this is needed or works here
                    in_shiny = TRUE, # not sure this is needed or works here
                    # quiet = TRUE,
                    # parallel = FALSE,
                    silentinteractive = TRUE,
                    # called_by_ejamit = TRUE, # not sure this is needed or works here
                    testing = input$testing 
      )
      
      
      if(is.null(out)){
        validate('No valid blockgroups found matching these FIPS codes.')
      } else {
        out$results_bysite <- merge(d_upload[, .(ejam_uniq_id, valid, invalid_msg)],
                                    out$results_bysite, 
                                    by='ejam_uniq_id', all=T)
        
        
        #incorporate new longnames into FIPS data
        newcolnames <- c(
          "valid",
          "invalid_msg"
        )
        
        newcolnames1 <- c(
          "EJScreen Report",
          "EJScreen Map",
          "ECHO report"
        )
        
        
       
        # put those up front as first columns
        data.table::setcolorder(out$results_bysite, neworder = c('ejam_uniq_id', newcolnames))
        data.table::setcolorder(out$results_overall, neworder = c('ejam_uniq_id'))
        #setcolorder(out$results_bysite, neworder = newcolnames)
        # move ejam_uniq_id to front of longnames vector
        
        out$longnames<- c('ejam_uniq_id',newcolnames, out$longnames[out$longnames != 'ejam_uniq_id'])
        
        
        
      }
      
     # out
      ################################################# # 
    } else { #  everything other than FIPS code analysis
      #############################################################################  # 
      
      ## get blocks in POLYGONS / SHAPEFILES ####
      
      if (submitted_upload_method() == "SHP") {
        shp_valid <- data_uploaded()[data_uploaded()$valid==T, ] # *** remove this if shapefile_clean() will do it
        
        if (input$bt_rad_buff > 0) {
          #if (!silentinteractive) {
            cat('Adding buffer around each polygon.\n')
            #}
          

          shp <- shape_buffered_from_shapefile(
            shapefile = shp_valid, 
            radius.miles =  input$bt_rad_buff
          ) # default crs
        } else {
          shp <- shp_valid
        }
        sites2blocks <- (get_blockpoints_in_shape(shp))$pts
        d_upload     <- sites2blocks
      }
      ################################################# # 
      
      ## get blocks near LAT/LON  POINTS  facilities/latlon # ####
      
      if (!(submitted_upload_method() %in% c('SHP', 'FIPS'))) {  # if LATITUDE AND LONGITUDE (POINTS), find blocks nearby
        
        d_upload <- data_uploaded()[!is.na(lat) & !is.na(lon),]
        
        ## progress bar to show getblocksnearby status
        progress_getblocks <- shiny::Progress$new(min = 0, max = 1)
        updateProgress_getblocks <- function(value = NULL, message_detail=NULL, message_main = '0% done'){
          if (is.null(value)) { # - If value is NULL, it will move the progress bar 1/20 of the remaining distance.
            value <- progress_getblocks$getValue()
            value <- value + (progress_getblocks$getMax() - value) / 20
            message_main = paste0(value*100, '% done')
          }
          progress_getblocks$set(value = value, message = message_main, detail = message_detail)
        }
          
        sites2blocks <- getblocksnearby(
          ## remove any invalid latlons before running 
          sitepoints = d_upload,
          radius = input$bt_rad_buff,
          quadtree = localtree, 
          avoidorphans = input$avoidorphans,
          maxradius = input$maxradius,
          quiet = TRUE,
          updateProgress = updateProgress_getblocks
        )

        progress_getblocks$close()
        
       # data_uploaded()[!(ejam_uniq_id %in% sites2blocks$ejam_uniq_id),'valid'] <- F
        dup <- data_uploaded()
        dup$valid <- dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id
       
        dup$invalid_msg[!(dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id)] <- 'no blocks found nearby'
        data_uploaded <- dup
      } # end LAT LON finding blocks nearby, now ready for latlon and shapefiles to do aggregation
      #############################################################################  # 
      
      ## progress bar update overall
      progress_all$inc(1/3, message = 'Step 2 of 3', detail = 'Aggregating')
      ## progress bar to show doaggregate status
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
      
    if (submitted_upload_method() != "FIPS") {  # if LAT LON or SHAPEFILE, now have blocks nearby and ready to aggregate
    
    #############################################################################  # 
    # 2) **EJAM::doaggregate()** ####

    out <- suppressWarnings(
      doaggregate(
        sites2blocks = sites2blocks,
        sites2states_or_latlon = d_upload,
        radius = input$bt_rad_buff,
        #countcols = 0, popmeancols = 0, calculatedcols = 0, # *** if using defaults of doaggregate()
        subgroups_type = input$subgroups_type, # nh, alone, or both # or use default of doaggregate() based on whatever subgroups_d etc are now ***   
        include_ejindexes = (input$include_ejindexes == "TRUE"), # it was character not logical because of how input UI done 
        calculate_ratios = input$calculate_ratios,
        extra_demog = input$extra_demog,
        need_proximityscore = input$need_proximityscore,
        infer_sitepoints = FALSE,
        called_by_ejamit = FALSE,
        updateProgress = updateProgress_doagg, ## pass progress bar function as argument
        testing = input$testing
      )
    )

    #data_uploaded()[!(ejam_uniq_id %in% out$results_bysite$ejam_uniq_id),'valid'] <- F
    dup <- data_uploaded()
    #dup[,valid := ejam_uniq_id %in% out$results_bysite$ejam_uniq_id]
    dup$valid <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id
    dup$invalid_msg[!(dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id)] <- 'dropped from doaggregate'
    
    data_uploaded <- dup
    # provide sitepoints table provided by user aka data_uploaded(), (or could pass only lat,lon and ST -if avail- not all cols?)
    # and doaggregate() decides where to pull ST info from - 
    # ideally from ST column, 
    # second from fips of block with smallest distance to site, 
    # third from lat,lon of sitepoints intersected with shapefile of state bounds
    ## close doaggregate progress bar
    progress_doagg$close()
    
    ################################################################ # 
    
    # add URLs >>(should be a function)  ####
    #  
    #  >this should be a function, and is used by both server and ejamit() ###  #
    # duplicated almost exactly in ejamit() but reactives are not reactives there
    # maybe use url_4table() - see ejamit() code
    #
    #if ("REGISTRY_ID" %in% names(out$results_bysite)) {
    # echolink = url_echo_facility_webpage(out$results_bysite$REGISTRY_ID, as_html = FALSE)
    #} else {
    # echolink = url_echo_facility_webpage(out$results_bysite$REGISTRY_ID, as_html = FALSE)
    #}
    ## the registry ID column is only found in uploaded ECHO/FRS/NAICS data -
    ## it is not passed to doaggregate output at this point, so pull the column from upload to create URLS
   
    if(submitted_upload_method() %in% c('MACT','FRS','latlon','EPA_PROGRAM_up',
                                        'EPA_PROGRAM_sel','NAICS','SIC')){
      #print(names(data_uploaded()))
      #print(head(names(data_processed()$results_bysite)))
      out$results_bysite <- merge(data_uploaded()[, .(ejam_uniq_id, valid, invalid_msg)],
                                  out$results_bysite, 
                                  by='ejam_uniq_id', all=T)
    } else if(submitted_upload_method() == 'SHP'){
      out$results_bysite <- merge(data_uploaded()[, c('ejam_uniq_id','valid','invalid_msg')],
                                  #merge(data_uploaded()[, .(ejam_uniq_id, valid)],
                                  out$results_bysite, 
                                  by='ejam_uniq_id', all=T) %>% 
        sf::st_drop_geometry()
    }


   # if (nrow(d_upload) != nrow(out$results_bysite)) {
   #    out$results_bysite[, `:=`(
   #      `EJScreen Report` = rep('N/A', nrow(out$results_bysite)),
   #      `EJScreen Map`    = rep('N/A', nrow(out$results_bysite)),
   #      # `ACS Report`      = rep('N/A', nrow(out$results_bysite)),  # will drop this one
   #      `ECHO report`     = rep('N/A', nrow(out$results_bysite))
   #    )]
   #  } else {
      
      #if ("REGISTRY_ID" %in% names( d_upload)) {
      if("REGISTRY_ID" %in% names(data_uploaded())){
        #echolink = url_echo_facility_webpage( d_upload$REGISTRY_ID, as_html = TRUE, linktext = 'ECHO Report')
        echolink = url_echo_facility_webpage( data_uploaded()$REGISTRY_ID, as_html = TRUE, linktext = 'ECHO Report')
    } else if ("RegistryID" %in% names(data_uploaded())) {
        #echolink = url_echo_facility_webpage( d_upload$RegistryID, as_html = TRUE, linktext = 'ECHO Report')
        echolink = url_echo_facility_webpage( data_uploaded()$RegistryID, as_html = TRUE, linktext = 'ECHO Report')
    } else {
        echolink = rep('N/A',nrow(out$results_bysite))
    }
    
    if(submitted_upload_method() != 'SHP'){
      out$results_bysite[ , `:=`(
        `EJScreen Report` = ifelse(valid == T, url_ejscreen_report(    lat = d_upload$lat, lon =  d_upload$lon, radius = input$bt_rad_buff, as_html = TRUE), 'N/A'),
        `EJScreen Map`    = ifelse(valid == T, url_ejscreenmap(        lat = d_upload$lat, lon =  d_upload$lon,                             as_html = TRUE),  'N/A'),
        # `ACS Report`      = url_ejscreen_acs_report(lat = d_upload$lat, lon =  d_upload$lon, radius = input$bt_rad_buff, as_html = TRUE),
        `ECHO report` = ifelse(valid==T, echolink, 'N/A')
      )]
      out$results_overall[, `:=`(
        `EJScreen Report` = NA,
        `EJScreen Map` = NA,
        `ECHO report` = NA
      )]
    } else {
      ## setting shapefile URLs to NA for now
      out$results_bysite <- out$results_bysite %>% 
        dplyr::mutate(
            `EJScreen Report` = 'N/A',#ifelse(valid == T, url_ejscreen_report(    lat = d_upload$lat, lon =  d_upload$lon, radius = input$bt_rad_buff, as_html = TRUE), 'N/A'),
            `EJScreen Map`    = 'N/A',#ifelse(valid == T, url_ejscreenmap(        lat = d_upload$lat, lon =  d_upload$lon,                             as_html = TRUE),  'N/A'),
            # `ACS Report`      = url_ejscreen_acs_report(lat = d_upload$lat, lon =  d_upload$lon, radius = input$bt_rad_buff, as_html = TRUE),
            `ECHO report` = 'N/A'#ifelse(valid==T, echolink, 'N/A')
        )
      out$results_overall <- out$results_overall %>% 
        dplyr::mutate(
          `EJScreen Report` = NA,
          `EJScreen Map` = NA,
          `ECHO report` = NA
        )
    }
    #}
    
    
    newcolnames <- c(
      'valid','invalid_msg',
      "EJScreen Report",
      "EJScreen Map",
      "ECHO report"
    )
    newcolnames_overall <- c(
      "EJScreen Report",
      "EJScreen Map",
      "ECHO report"
    )

    # put those up front as first columns
    out$results_bysite <- dplyr::relocate(out$results_bysite, c('ejam_uniq_id', newcolnames), .before=1)
    out$results_overall <- dplyr::relocate(out$results_overall, newcolnames_overall, .before=2)
   
    # move ejam_uniq_id to front of longnames vector
    out$longnames <- c('ejam_uniq_id',newcolnames, out$longnames[out$longnames != 'ejam_uniq_id'])
    #############################################################################  # 
    
    # add radius to results tables (in server and in ejamit() ####
    # out$results_bysite[      , radius.miles := input$bt_rad_buff]
    # out$results_overall[     , radius.miles := input$bt_rad_buff]
    # out$results_bybg_people[ , radius.miles := input$bt_rad_buff] # probably will not export this big table in excel downloads
    # 
    out$results_bysite$radius.miles <- input$bt_rad_buff
    out$results_overall$radius.miles <- input$bt_rad_buff
    out$results_bybg_people$radius.miles <- input$bt_rad_buff
    
        # out$longnames <- NA # see ejamit()
        # out$formatted <- table_tall_from_overall(out$results_overall, out$longnames)
        #   # see ejamit()
        
      } # end of non fips, ie all latlon or shapefile aggregation 
      
      
      
    } # done with all ways of analyzing ...  latlon, Shapefiles, and FIPS codes
    
    ## assign doaggregate output to data_processed reactive 
    data_processed(out)
    
    ## update overall progress bar
    progress_all$inc(1/3, message = 'Step 3 of 3', detail = 'Summarizing')
    
    #############################################################################  # 
    
    # 3) **batch.summarize()** on already processed data ####
    if(submitted_upload_method() == 'SHP'){
      outsum <- EJAMbatch.summarizer::batch.summarize(
        sitestats = data.frame(data_processed()$results_bysite %>% 
                                 sf::st_drop_geometry()),
        popstats =  data.frame(data_processed()$results_bysite %>% 
                                 sf::st_drop_geometry()),
        ## user-selected quantiles to use
        #probs = as.numeric(input$an_list_pctiles),
        threshold = list(input$an_thresh_comp1) # compare variables to 90th %ile or other percentile, like threshold1 param in ejamit()
      )
    } else {
    outsum <- EJAMbatch.summarizer::batch.summarize(
      sitestats = data.frame(data_processed()$results_bysite),
      popstats =  data.frame(data_processed()$results_bysite),
      ## user-selected quantiles to use
      #probs = as.numeric(input$an_list_pctiles),
      threshold = list(input$an_thresh_comp1) # compare variables to 90th %ile or other percentile, like threshold1 param in ejamit()
    )
    }
    ## update overall progress bar
    progress_all$inc(1/3, message = 'Done processing! Loading results now', detail = NULL)
    
    # assign batch.summarize output to data_summarized reactive ### #
    data_summarized(outsum)
    
    ## close overall progress bar
    progress_all$close()
    
    ## switch tabs and jump to top of screen 
    shinyjs::js$toTop();
    updateTabsetPanel(session, inputId = "all_tabs",     selected = "See Results")
    updateTabsetPanel(session, inputId = 'results_tabs', selected = 'Community Report')
  })  # end of observeEvent based on Start analysis button called input$bt_get_results
  
  #############################################################################  # 
  # if (input$calculate_ratios) {  ## ratios can be dropped from output table of results but are used by summary report, plots, etc. so simplest is to still calculate them
  #############################################################################  # 
  # . 4) ratios,  also avail from ejamit()####
  # ______ AVERAGES and RATIOS TO AVG - ALREADY done by doaggregate() and kept in data_processed()
  # Also the overall mean and site by site means are in  unlist( data_processed()$results_overall[ , ..names_these_state_avg] )
  # and (avg.in.us) is a constant for convenience with that same info, in data.frame format.
  # but did not bother making a copy of the state averages that are in statestats
  # EJAM::statestats[ EJAM::statestats$PCTILE == "mean", ]
  # and averages were used create ratios in doaggregate()
  ############################# #
  ##   d RATIOS of overall scores to US or state D AVG ####
  #  *****************   but these are now already calculated in doaggregate()
  ## as (doaggregate output results_overall ) / (EJAM::usastats mean in USA)
  ## or (batch.summarize 'Average person' / EJAM::usastats mean in USA   )
  ## this needs further verification
  
  ratio.to.us.d    <- reactive({unlist(
    data_processed()$results_overall[ , c(..names_d_ratio_to_avg,       ..names_d_subgroups_ratio_to_avg      )]
  ) }) # ???
  ratio.to.state.d <- reactive({unlist(
    data_processed()$results_overall[ , c(..names_d_ratio_to_state_avg, ..names_d_subgroups_ratio_to_state_avg)]
  ) }) # ???
  # ratio.to.us.d_TEST <- reactive({
  #   unlist(data_processed()$results_overall[1, ]) / 
  #     avg.in.us[, c(names_d, names_d_subgroups)]
  # })
  ############################# #
  ##   e RATIOS of overall scores to US or state E AVG  ####
  #  *****************   but these are now already calculated in doaggregate()  as
  ## (batch.summarize 'Average person' / EJAM::usastats mean in USA   )
  ## this needs further verification
  
  ratio.to.us.e    <- reactive({unlist(data_processed()$results_overall[ , ..names_e_ratio_to_avg]) })                     # ???
  # ratio.to.us.e_TEST <- reactive({ unlist(data_summarized()$rows['Average person', names_e]) / 
  #     avg.in.us[, names_e]
  # doaggregate results_overall output, if needed: unlist(data_processed()$results_overall[1, ..names_e])
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
  # }
  #############################################################################  # 
  # . --------------------------------------------------------------- ####       
  #. ## ##
  # ______ SEE RESULTS _________ ####
  #. ####
  
  # #############################################################################  # 
  
  # ________ COMMUNITY REPORT VIEW __________ ####
  
  # output$LOCATIONSTR <- renderText(
  #   paste0(input$bt_rad_buff, " mile Rings Centered at Selected Points")
  #     # <span id="LOCATIONSTR">{{LOCATIONSTR}} mile Rings Centered at Selected Points</span>
  # )
  # output$TOTALPOP <- renderText(
  #   prettyNum(total_pop(), big.mark = ",")
  # )
  
  
  
  # ______ SUMMARY REPORT ______ ####
  
  ## Header ####
  
  ### ( Total Population count ) ####
  total_pop <- reactive({
    req(data_processed())
    ## format and return total population  ## some of these numbers seem very large! possible double-counting??
    round(data_processed()$results_overall$pop, 0 )
  })
  
  ### ( Title of analysis ) ####
  # Unless user changes it here, use a standard title that has been determined by global.R but then optionally modified by advanced settings tab
  output$analysis_title_ui <- renderUI({
    shiny::textInput('analysis_title', 
                     label = 'Name of Your Analysis',
                     #placeholder = 'EJ Analysis of My List of Places',
                     value = input$standard_analysis_title)
  })
  
  ### summary header is stored in a reactive  
  summary_title <- reactiveVal(NULL)
  
  ### summary header is updated when 'Start Analysis' button clicked
  ### or when Analysis Title is updated after a run
  observeEvent(
    ## allow title to update when either of these inputs change
    eventExpr = {
      input$bt_get_results
      input$analysis_title
    }, handlerExpr = {
    req(data_processed())
    ## paste header information together
    title_text <- paste0('<div style="font-weight: bold; font-size: 11pt; text-align: center;">',
                         input$analysis_title, '<br>')
    
    ## exclude radius info from header text when using FIPS
    if(current_upload_method() != 'FIPS'){
      title_text <- paste0(title_text,
                           'Residents within ',
                           #input$bt_rad_buff, ' ', input$radius_units, ' of any of the ',
                           input$bt_rad_buff, ' miles of any of the '
                           )
    }
        title_text <- paste0(title_text,
                             prettyNum( NROW(data_processed()$results_bysite), big.mark = ","), 
                             ' sites analyzed<br>',
                         #    "in the xxx source category or sector<br>",
                         'Population: ', prettyNum( total_pop(), big.mark = ","), '</div>'
                      )
    ### update summary header reactive variable
    summary_title(title_text)
  })
  
  ### summary header is output as html 
  output$view1_total_pop <- renderUI({
    HTML(summary_title())
  })
  
  #############################################################################  # 
  ## *TABLE DEMOG (for summary report) #### 
  
  v1_demog_table <- reactive({

    req(data_processed())
    # should it check if (input$calculate_ratios) or is it ok to show NA values instead of hiding those columns *** ?

      table_out_d <- table_gt_from_ejamit_overall(data_processed()$results_overall,
                                                  type = 'demog')
      table_out_d
  })
  # 
  # ## output:  gt  view1_demog_table()
  # output$view1_demog_table <- gt::render_gt({
  #   v1_demog_table()
  # })
  #############################################################################  # 
  ## *TABLE ENVT. (for summary report) #### 
  
  v1_envt_table <- reactive({

    req(data_processed())
    # should it check if (input$calculate_ratios) # *** ?

    tab_out_e <- table_gt_from_ejamit_overall(data_processed()$results_overall,
                                              type = "envt")
    tab_out_e
  })
  # 
  # ## output: show environmental indicator table
  # 
  # output$view1_envt_table <- gt::render_gt({
  #   v1_envt_table()
  # })
  
  #############################################################################  # 
  
  ## *MAP (for summary report) ####
  
  report_map <- reactive({
    
    #req(data_processed())
    validate(need(data_processed(), 'Please run an analysis to see results.'))
    circle_color <- '#000080'
      
    #if shapefile, merge geometry and create buffer if nonzero buffer is set
    if (submitted_upload_method() == "SHP") {
      d_up <- data_uploaded()
      #d_up_geo <- d_up[,c("siteid","geometry")]
      #d_merge = merge(d_up_geo,data_processed()$results_bysite, by = "siteid", all.x = FALSE, all.y = TRUE)
      d_up_geo <- d_up[,c("ejam_uniq_id","geometry")]
      d_merge = merge(d_up_geo,data_processed()$results_bysite, by = "ejam_uniq_id", all.x = FALSE, all.y = TRUE)
      if (input$bt_rad_buff > 0) {
        d_uploads <- sf::st_buffer(d_merge, # was "ESRI:102005" but want 4269
                                   dist = units::set_units(input$bt_rad_buff, "mi")) 
        leaflet(d_uploads) %>%  addTiles()  %>%
          addPolygons(color = circle_color) 
      } else {
        data_spatial_convert <- d_merge %>% st_zm() %>% as('Spatial')
        leaflet(data_spatial_convert) %>% addTiles()  %>%
          addPolygons(color = circle_color)
      }
      
    } else { #  not shapefile
      
      if (submitted_upload_method() != "FIPS") {
        
      # this bit of code defining popup_labels was there sep 10 but deleted Oct 14, probably inadvertently, and being put back in oct 23.
      # popup_labels <- c(data_processed()$longnames, 'State Name')
      # popup_labels[popup_labels == ""] <- map_headernames$names_friendly[match(
      #   names(data_processed()$results_bysite)[popup_labels == ""],
      #   EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
      #  
      popup_labels <- map_headernames$names_friendly[match(names(data_processed()$results_bysite),map_headernames$rname)] 
      popup_labels[is.na(popup_labels)] <- names(data_processed()$results_bysite)[is.na(popup_labels)]
      ## similar to previous map but remove controls and only add circles, not circleMarkers
      
      ## switch this to data analyzed in report, not what was uploaded,   in case there are invalid
      leaflet(data_processed()$results_bysite) %>% #,
        #options = leafletOptions(zoomControl = FALSE, minZoom = 4)) %>% 
        addTiles()  %>%
        addCircles(
          radius = 1 * meters_per_mile,
          color = circle_color, fillColor = circle_color, 
          fill = TRUE, weight = input$circleweight_in,
          #group = 'circles',
          #popup = popup_from_any(
          popup = popup_from_df(
            data_processed()$results_bysite %>% 
              dplyr::mutate(dplyr::across(
                dplyr::where(is.numeric), \(x) round(x, digits = 3))) %>% 
              dplyr::select(-valid, -invalid_msg), 
            labels = popup_labels),
          popupOptions = popupOptions(maxHeight = 200)
        )} else {
          # FIPS    *** placeholder blank US map until have time to create FIPS-based map
          leaflet() %>% addTiles() %>% fitBounds(-115, 37, -65, 48)
        }
    }
    
  }) # end of report_map 
  
  ## output: summary report map  
  output$quick_view_map <- leaflet::renderLeaflet({
    #req(data_uploaded())
    
    ## use separate report map
    report_map()
    
    ## or can keep same map as on Site Selection tab
    # orig_leaf_map()
  })
  
  ## update leaflet map when inputs change
  ##   this is currently resetting map too often in response to checkbox  ***
  # observeEvent(eventExpr = {
  #   input$bt_rad_buff
  #   input$an_map_clusters
  #   is_clustered()
  #   #input$radius_units
  # }, {
  observe({
    
    # req(data_uploaded())
    ## This statement needed to ensure map stops if too many points uploaded
    req(isTruthy(orig_leaf_map()))
    
    #clear shapes from map so buffers don't show twice
    leafletProxy(mapId = 'an_leaf_map', session) %>% clearShapes()
    
  
    if (current_upload_method() == "SHP") {
      if (input$bt_rad_buff > 0) {
        d_uploads <- sf::st_buffer(data_uploaded(), # was "ESRI:102005" but want 4269
                                   dist = units::set_units(input$bt_rad_buff, "mi")) 
        leafletProxy(mapId = 'an_leaf_map', session) %>%
          addPolygons(data = d_uploads, color = "red") 
      }
      #d_uploadb <- data_uploaded()[['buffer']]  %>% st_zm() %>% as('Spatial') 
      d_uploads <- data_uploaded() %>% #[['shape']]  
        st_zm() %>% as('Spatial') 
      leafletProxy(mapId = 'an_leaf_map', session) %>%
        # addPolygons(data=d_uploadb, color="red") %>% 
        addPolygons(data = d_uploads, 
                    popup = popup_from_df(d_uploads %>% sf::st_drop_geometry()))
      #leafletProxy(mapId = 'an_leaf_map', session,data=d_uploads) %>% addPolygons()
      
    } else # if (input$circle_type == 'circles') {
      
      if (current_upload_method() == 'FIPS') {
        
        ## initial map code - this plots convex hull polygons of blockpoints, not actual shapes though
        # fips_sf <- sf::st_as_sf(data_uploaded(), coords=c('lon','lat')) %>%
        #   dplyr::group_by(siteid) %>%
        #   dplyr::summarize(geometry = sf::st_combine(geometry)) %>%
        #   sf::st_convex_hull() %>%
        #   sf::st_cast('POLYGON')  %>% as('Spatial')
        # 
        # leafletProxy(mapId = 'an_leaf_map', session, data=fips_sf) %>% addPolygons()
        
      } else {
        
        d_upload <- data_uploaded()
        base_color      <- '#000080'
          cluster_color   <- 'red'
            #req(input$bt_rad_buff)
          
          ## convert units to miles for circle size
          # if (input$radius_units == 'kilometers') {
          #   rad <- input$bt_rad_buff * meters_per_mile * 0.62137119
          # } else {
          # rad <- input$bt_rad_buff * meters_per_mile
          #}
          
          #if (input$an_map_clusters == TRUE) {
          ## compare latlons using is_clustered() reactive
          #circle_color <- ifelse(is_clustered() == TRUE, cluster_color, base_color)
          #} else {
          circle_color <- base_color
          #}
          
          #popup_vec = popup_from_any(d_upload)
          popup_vec = popup_from_df(d_upload %>% 
                                      dplyr::select(-valid, -invalid_msg))
      
          suppressMessages(
            leafletProxy(mapId = 'an_leaf_map', session, data = d_upload) %>%
              map_facilities_proxy(rad = input$bt_rad_buff, 
                                   highlight = TRUE, #input$an_map_clusters, 
                                   popup_vec = popup_vec, 
                                   use_marker_clusters = nrow(d_upload) > marker_cluster_cutoff,
                                   clustered = FALSE)#is_clustered())
          )
      }
  
  }) 
  
  
  #############################################################################  # 
  ## *BARPLOT for short and long reports (avg person D ratios vs US avg) ####
  
  # compare / merge with  EJAM/R/plot_barplot_ratios.R *** 
  # https://exts.ggplot2.tidyverse.org/gallery/
  
  v1_summary_plot <- reactive({
    # req(data_summarized()) # it used to say this is required here but I dont think it actually is used
    req(data_processed())
    # data_processed() needed for ridgeline or boxplot, and ratio.to.us.d() which is made from data_processed() is needed for boxplots, 
    
    if (input$plotkind_1pager == 'bar') { # do BARPLOT NOT BOXPLOT
      
      plot_barplot_ratios(unlist(data_processed()$results_overall[ , c(..names_d_ratio_to_avg , ..names_d_subgroups_ratio_to_avg) ]),
                          names2plot_friendly = data_processed()$longnames[2 + which(names(data_processed()$results_overall) %in% c(names_d, names_d_subgroups))])
      
     
    } else if (input$plotkind_1pager == 'ridgeline') {
      
      ## ratios by site  (demog each site / demog avg in US)
      ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
        ..names_d_ratio_to_avg, 
        ..names_d_subgroups_ratio_to_avg
      )]
      
      plot_ridgeline_ratios(ratio.to.us.d.bysite)
                            
      
    } else {if (input$plotkind_1pager == "box") {
      
      ## *BOXPLOTS for short report (all sites D ratios vs US avg) ####
      
      # compare / merge with  EJAMejscreenapi::boxplots_ratios  *** 
      
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
      ) # long_names_d$var_names[match( names_d_fixed, long_names_d$vars)] # names_d_fixed and long_names_d no longer exist. use names_d_friendly, etc.
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
      q75.maxof75s <- max(quantile(ratio.to.us.d.bysite$value, 0.75, na.rm = TRUE),na.rm = TRUE)
      ylimit <- ceiling(q75.maxof75s) # max of 75th pctiles rounded up to nearest 1.0x?   
      max_limit <- max(3, ylimit, na.rm = TRUE) #   
      # perhaps want a consistent y limits to ease comparisons across multiple reports the user might run.
      #  If the max value of any ratio is say 2.6, we might want ylim to be up to 3.0, 
      #  if the max ratio is 1.01, do we still want ylim to be up to 3.0??
      #  if the max ratio or even max of 95th pctiles is >10, don't show it, but 
      #  what if the 75th pctile value of some indicator is >10? expand the scale to always include all 75ths.
      
      ## find 75th %ile of ratios for the indicator with the max ratio 
      q75.ratio.d.bysite <- quantile(ratio.to.us.d.bysite$value[ratio.to.us.d.bysite$indicator == max.name.d.bysite], 0.75, na.rm = TRUE)
      
      # to use for dot showing the mean ratio of each indicator *** NOT USED? 
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
      ## much of this is plotting code is based on EJAMejscreenapi::boxplots_ratios - should consolidate
      
      ggplot2::ggplot(
        ratio.to.us.d.bysite  ,
        # mydata, 
        aes(x = indicator, y = value )
      ) + #, fill = indicator)) +
        ## draw boxplots
        geom_boxplot() +
        
        #  show average persons ratio to US,  for each boxplot column 
        # xxx   Try to fix / use this: 
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
             title = mymaintext ) +
        # title = 'Ratio vs. US Average for Demographic Indicators') +
        
        ## draw individual dot per site? at least for small datasets?/few facilities - removed as they cover up boxplots with large datasets
        #geom_jitter(color = 'black', size=0.4, alpha=0.9, ) +
        
        ## set color scheme ?
        # actually do not need each a color, for boxplot.
        # scale_fill_brewer(palette = 'Dark2') +
        ## alternate color scheme
        # viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        
      ggplot2::theme_bw() +
        ggplot2::theme(
          ## set font size of text
          text = ggplot2::element_text(size = 14),
          #axis.text  = ggplot2::element_text(size = 16),
          ## set font size of axis titles
          axis.title = ggplot2::element_text(size = 16),
          ## center and resize plot title
          plot.title = ggplot2::element_text(size = 22, hjust = 0.5),
          ## center subtitle
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          ## hide legend
          legend.position = 'none'
        )  # end of ggplot section
    }
    }
    # box
  })
  
  ## output: show box/barplot of indicator ratios in Summary Report # 
  output$view1_summary_plot <- renderPlot({
    v1_summary_plot()
  })
  
  #############################################################################  # 
  
  ## Community report download ##
  output$community_download <- downloadHandler(
    filename = ifelse(input$format1pager == "pdf", "community_report.pdf", 'community_report.html') ,
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      #tempReport <- file.path(tempdir(), "community_summary.html")
      tempReport <- file.path(tempdir(), 'community_report_template.Rmd')
      
      if(!('communityreport.css' %in% list.files(tempdir()))){
        file.copy(from = app_sys('report/community_report/communityreport.css'),
                  to = file.path(tempdir(), 'communityreport.css'), overwrite = TRUE)          
      }
      
      if(!('EPA_logo_white.png') %in% list.files(file.path(tempdir(), 'www'))){
        dir.create(file.path(tempdir(), 'www'))
        file.copy(from = app_sys('report/community_report/EPA_logo_white.png'),
                  to = file.path(tempdir(), 'www', 'EPA_logo_white.png'), overwrite = TRUE)
      }
      
      ## copy Rmd from inst/report to temp folder  (note there had been a similar but not identical .Rmd in EJAM/www/)
      file.copy(from = app_sys('report/community_report/community_report_template.Rmd'),  # treats EJAM/inst/ as root
                to = tempReport, overwrite = TRUE)
      
      # htmltools::save_html(full_html_reactive(),
      #                      file = tempReport)
      
      isolate({  # need someone to confirm this is needed/helpful and not a problem, to isolate this.
        ## pass params to customize .Rmd doc  # ###
        # params <- list(html_content = full_html_reactive(),
        #                map         = report_map(),
        #                summary_plot = v1_summary_plot())
        rad <- data_processed()$results_overall$radius.miles # input$radius can be changed by user and would alter the report text but should just show what was run not what slider currently says
        popstr <- prettyNum(total_pop(), big.mark=',')
        
        if(submitted_upload_method() == 'SHP'){
          location_type <- " selected polygons"
          radiusstr <- paste0(rad, " mile", 
                              ifelse(rad > 1, "s", ""), " of ")
          
        } else if (submitted_upload_method() == 'FIPS'){
          location_type <- " selected shapes"
          radiusstr <- ""
        } else {
          location_type <- " selected points"
          radiusstr <- paste0(rad, " mile", ifelse(rad > 1, "s", ""), " of ")
                              
        }
      
        locationstr <- paste0("Residents within ",
                              radiusstr,
                              "any of the ", NROW(data_processed()$results_bysite[data_processed()$results_bysite$valid == T,]),location_type)
        
        params <- list(
          output_df = data_processed()$results_overall,
          analysis_title = input$analysis_title,
          totalpop = popstr, 
          locationstr = locationstr,
          include_ejindexes = (input$include_ejindexes == 'TRUE'), 
          in_shiny = FALSE,
          filename=NULL,
          map = report_map(),
          summary_plot = v1_summary_plot()
        )
        
      })
      
      rmarkdown::render(tempReport, 
                        output_format ='html_document',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        intermediates_dir = tempdir()
      )
      
      #file.copy(tempReport, file)
      
    }
  )
  
  #############################################################################  # 
  # .  ## ##
  # ______ DETAILED RESULTS ______ ####
  
  
  #############################################################################  # 
  #. ## ##
  # ______ SUMMARY IN TALL FORMAT?  ## ##
  
  ################################################################ # 
  # just a nicer looking tall version of overall results
  # output$overall_results_tall <- renderDT({
  #   table_tall_from_overall(results_overall = data_processed()$results_overall, longnames =  data_processed()$longnames)
  # })
  # output$overall_results_tall <- renderDT({
  #   tallout <- cbind(overall = round(unlist(data_processed()$results_overall), 3))
  #   rownames(tallout) <- fixnames_to_type(rownames(tallout), "newnames_ejscreenapi", "longname_tableheader")
  #   tallout
  # })
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(id, ...))
    }
    inputs
  }
  
  #############################################################################  # 
  #. ## ##
  ## *DATATABLE of sites = output$view3_table from data_processed()  ####
  #output: site by site datatable 
  output$view3_table <- DT::renderDT(server = TRUE, expr = {
    req(data_processed())
    if (input$testing) {cat('view3_table - preparing (most columns of the) site by site table for DT view \n')
    }
    # --------------------------------------------------- #
    # cols_to_select <- names(data_processed)
    # friendly_names <- longnames???
    cols_to_select <- c('ejam_uniq_id', 'invalid_msg',#'siteid', 
                        'pop', 'Community Report',
                        'EJScreen Report', 'EJScreen Map', 'ECHO report', # 'ACS Report', 
                        names_d, names_d_subgroups,
                        names_e #, 
                        # no names here corresponding to number above x threshold, state, region ??
    )
    friendly_names <- c('Site ID', 'Invalid Reason','Est. Population', 'Community Report',
                        'EJScreen Report', 'EJScreen Map', 'ECHO report', #'ACS Report', 
                        names_d_friendly, names_d_subgroups_friendly, 
                        names_e_friendly)
    
    ejcols          <- c(names_ej,          names_ej_state,          names_ej_supp,          names_ej_supp_state)
    ejcols_friendly <- c(names_ej_friendly, names_ej_state_friendly, names_ej_supp_friendly, names_ej_supp_state_friendly)
    which_ejcols_here <- which(ejcols %in% names(data_processed()$results_bysite)  )
    cols_to_select <- c(cols_to_select, ejcols[         which_ejcols_here] )
    friendly_names <- c(friendly_names, ejcols_friendly[which_ejcols_here])
    
    friendly_names <- c(friendly_names, 
                        '# of indicators above 90% threshold', 'State', 'EPA Region')
    # --------------------------------------------------- #
    
    # dt_overall <- data_processed()$results_overall %>% 
    #   as.data.frame() %>% 
    #   dplyr::mutate(siteid = 'All sites', ST = NA,
    #          across(where(is.numeric), .fns = function(x) {round(x, digits=2)})) %>% 
    #   dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    # use data_processed()  
    dt <- data_processed()$results_bysite %>% 
      as.data.frame() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), .fns = function(x) {round(x, digits = 2)})#,  ## *** should follow rounding rules provided via map_headernames$decimals or $sigfigs ?
                    #siteid = as.character(siteid)
      ) %>%
      dplyr::mutate(index = row_number()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        pop = ifelse(valid == T, pop, NA),
        # `EJScreen Report` = ifelse(valid == T, `EJScreen Report`, NA),
        # `ECHO Report` = ifelse(valid == T, `ECHO Report`, NA),
        # `EJScreen Map` = ifelse(valid == T, `EJScreen Map`, NA),
        `Community Report` = ifelse(valid == T, shinyInput(actionButton, 1, id=paste0('button_', index), label = "Generate", 
                                        onclick = paste0('Shiny.onInputChange(\"select_button',index,'\",  this.id)' )
                                        ), '')
      ) %>% 
    
       dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    # dt$`EJScreen Report` <- EJAMejscreenapi::url_linkify(dt$`EJScreen Report`, text = 'EJScreen Report')
    # dt$`EJScreen Map` <- EJAMejscreenapi::url_linkify(dt$`EJScreen Map`, text = 'EJScreen Map')
    #    #### dt$`ACS Report` <- EJAMejscreenapi::url_linkify(dt$`ACS Report`, text = 'ACS Report')
    # dt$`ECHO report` <- ifelse(!is.na(dt$`ECHO report`), EJAMejscreenapi::url_linkify(dt$`ECHO report`, text = 'ECHO Report'), 'N/A')
    
    # dt_avg <- data_summarized()$rows[c('Average person','Average site'),] %>% 
    #   dplyr::mutate(siteid = c('Average person', 'Average site'), ST = NA,
    #                 dplyr::across(dplyr::where(is.numeric), .fns = function(x) {round(x, digits=2)}),
    #          siteid = as.character(siteid)) %>%
    #   dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    # use data_summarized()
    dt_final <- dt %>% 
      dplyr::bind_cols(data_summarized()$cols) %>% 
      ## hide summary rows from table
      #dplyr::bind_rows(dt_avg) %>% 
      #dplyr::bind_rows(dt_overall) %>% 
      ## sort by Site ID - as numeric index
      #dplyr::arrange(siteid) %>% 
      #dplyr::arrange(dplyr::desc(pop)) %>% 
      dplyr::mutate(Number.of.variables.at.above.threshold.of.90 = ifelse(is.na(pop), NA,
                                                                          Number.of.variables.at.above.threshold.of.90)) %>% 
      dplyr::mutate(pop = ifelse(is.na(pop), NA, prettyNum(round(pop), big.mark = ','))) %>% 
      dplyr::left_join(stateinfo %>% dplyr::select(ST, statename, REGION), by = 'ST') %>% 
      dplyr::select(-ST, -Max.of.variables)
    
    colnames(dt_final) <- friendly_names
    
    dt_final <- dt_final %>% 
      dplyr::relocate(c('Invalid Reason',State, 'EPA Region', '# of indicators above 90% threshold'), .before = 2) # *** this cutoff should be dynamic, set by probs.default.values etc./ inputs
    
    ## set # of indicators above threshold to NA if population = 0
    dt_final <- dt_final %>%
      dplyr::mutate(`# of indicators above 90% threshold` = ifelse(`Est. Population` ==0, 'N/A',
                                                                   `# of indicators above 90% threshold`))
    
    n_cols_freeze <- 1
    
    ## format data table of site by site table
    # see also  EJAM/inst/notes_MISC/DT_datatable_tips_options.R
    
    DT::datatable(dt_final, 
                  rownames = FALSE, 
                  ## add column filters (confirm that does work)
                  filter = 'top',
                  ## allow selection of one row at a time (remove to allow multiple)
                  #selection = 'single',
                  selection='none',
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
                  height = 1500, 
                  escape = FALSE  # *** escape = FALSE may add security issue but makes links clickable in table
    ) %>% 
      DT::formatStyle(names(dt_final), 'white-space' = 'nowrap')
    #DT::formatStyle(names(dt_final), lineHeight='80%')
    ## code for bolding certain rows - not currently used
    #           ) %>% 
    # DT::formatStyle(
    #   valueColumns = 'Site ID',
    #   target = 'row', columns = 'all',
    #   fontWeight = DT::styleEqual(c('All sites','Average person','Average site'), values = 'bold')
    # )
  })
  #############################################################################  # 
  
  cur_button <- reactiveVal(NULL)
  
  observeEvent(
    lapply(
      names(input)[grep("select_button[0-9]+",names(input))],
      function(name){
        cur_button(input[[name]])
        input[[name]]
        }),  {
        req(data_processed())
          req(cur_button())
        x <- as.numeric(gsub('button_','', cur_button()))
        if( data_processed()$results_bysite$valid[x] == T){
          #!(submitted_upload_method() %in% c('FIPS')) &
          popstr <- prettyNum(round(data_processed()$results_bysite$pop[x]), big.mark=',')
          
          if(submitted_upload_method() == 'SHP'){
            locationstr <- paste0('Polygon ', data_up_shp()[x,]$OBJECTID_1)
            if(data_processed()$results_bysite[x,]$radius.miles > 0){
              locationstr <- paste0(locationstr, '<br>with ', data_processed()$results_bysite[x,]$radius.miles, ' mile buffer')
            }
          } else if(submitted_upload_method() == 'FIPS'){
            locationstr <- paste0('FIPS Code ', data_processed()$results_bysite[x,]$ejam_uniq_id)
          } else {
            locationstr <- paste0(data_processed()$results_bysite[x,]$radius.miles, ' Mile Ring Centered at ',
                                  data_processed()$results_bysite[x,]$lat, ', ',
                                  data_processed()$results_bysite[x,]$lon, '<br>', 'Area in Square Miles: ',
                                  round(pi* data_processed()$results_bysite[x,]$radius.miles^2,2)
            )
          }
          
          if(!('main.css' %in% list.files(tempdir()))){
            file.copy(from = app_sys('report/community_report/main.css'),
                      to = file.path(tempdir(), 'main.css'), overwrite = TRUE)          
          }
          if(!('communityreport.css' %in% list.files(tempdir()))){
            file.copy(from = app_sys('report/community_report/communityreport.css'),
                      to = file.path(tempdir(), 'communityreport.css'), overwrite = TRUE)          
          }
          
          if(!('EPA_logo_white.png') %in% list.files(file.path(tempdir(), 'www'))){
            dir.create(file.path(tempdir(), 'www'))
            file.copy(from = app_sys('report/community_report/EPA_logo_white.png'),
                      to = file.path(tempdir(), 'www', 'EPA_logo_white.png'), overwrite = TRUE)
          }
          temp_comm_report <- file.path(tempdir(), paste0("comm_report",x,".html"))
          
          tempReport <- file.path(tempdir(), 'community_report_template.Rmd')
          
          ## copy Rmd from inst/report to temp folder  (note there had been a similar but not identical .Rmd in EJAM/www/)
          file.copy(from = app_sys('report/community_report/community_report_template.Rmd'),  # treats EJAM/inst/ as root
                    to = tempReport, overwrite = TRUE)
          
          build_community_report(
            output_df = data_processed()$results_bysite[x,],
            analysis_title = input$analysis_title,
            totalpop = popstr,
            locationstr = locationstr,
            include_ejindexes = input$include_ejindexes,
            in_shiny = F,
            filename = temp_comm_report
          )
          browseURL(temp_comm_report)
          
          ## can also generate reports through knitting Rmd template
          ## this is easier to add in maps and plots but is slower to appear
          
          # isolate({  # need someone to confirm this is needed/helpful and not a problem, to isolate this.
          #   ## pass params to customize .Rmd doc  # ###
          #   # params <- list(html_content = full_html_reactive(),
          #   #                map         = report_map(),
          #   #                summary_plot = v1_summary_plot())
          #   rad <- data_processed()$results_bysite[x,]$radius.miles # input$radius can be changed by user and would alter the report text but should just show what was run not what slider currently says
          #   popstr <- prettyNum(round(data_processed()$results_bysite$pop[x]), big.mark=',')
          #   locationstr <- paste0(data_processed()$results_bysite[x,]$radius.miles, ' Mile Ring Centered at ',
          #                         data_processed()$results_bysite[x,]$lat, ', ',
          #                         data_processed()$results_bysite[x,]$lon, '<br>', 'Area in Square Miles: ',
          #                         round(pi* rad^2,2)
          #   )
          #   params <- list(
          #     output_df = data_processed()$results_bysite[x,],
          #     analysis_title = input$analysis_title,
          #     totalpop = popstr,
          #     locationstr = locationstr,
          #     include_ejindexes = T,
          #     in_shiny = F,
          #     filename = NULL,#temp_comm_report,
          #     map = report_map(),
          #     summary_plot = v1_summary_plot()
          #   )
          #   
          # })
          
          # rmarkdown::render(tempReport, 
          #                   output_format ='html_document',
          #                   output_file = temp_comm_report,
          #                   params = params,
          #                   envir = new.env(parent = globalenv()),
          #                   intermediates_dir = tempdir()
          # )
          
          #browseURL(temp_comm_report)
          
        } else {
          showModal(modalDialog(title='Report not available',
          'Individual site reports not yet available.'))
        }
       
        #showModal(modalDialog("Thanks for pushing the button"))
      })
  
  ## EXCEL DOWNLOAD  ####
  
  # SEE FUNCTION THAT CAN DO THIS AT ?table_xls_from_ejam()
  
  output$download_results_table <- downloadHandler(
    filename = function() {
      
      'results_table.xlsx'
    },
    content = function(fname) {
      
      if (input$testing) {
        cat('starting download code and  table_xls_format() \n') # ; xproc = data_processed(); save(xproc, file = 'table_data_processed-ejam.rda')
      }
      
      #  names( data_processed() )  #  "results_overall"  "results_bysite"  "results_bybg_people"  "longnames"  "count_of_blocks_near_multiple_sites"   "results_summarized"  
      
      ## note analysis type or overview to 'notes' tab
      if (submitted_upload_method() == "SHP") {
        radius_or_buffer_description <- 'Distance from each shape (buffering around each polygon)'
      } else {
        radius_or_buffer_description <- 'Distance from each site (radius of each circular buffer around a point)'
      }
      if (!input$calculate_ratios) {
        ratiocols <- names(data_processed()$results_overall) %in% c(names_d_ratio_to_avg, names_d_ratio_to_state_avg, names_e_ratio_to_avg, names_e_ratio_to_state_avg) 
        keepcols <- !ratiocols
        # grepl("ratio.to", names(data_processed()$results_overall))
        
      } else {
        keepcols <- rep(TRUE, NCOL(data_processed()$results_overall))
        keepcols2 <- rep(TRUE, NCOL(data_processed()$results_bysite))
      }
      
      if ("ready to do this as a function" == "remove this when ready to switch") {
        # having to drop cols via keepcols here is a pain in the neck. is it really useful anyway
        x <- data_processed() # this copy must slow it down a bit, and waste memory, but can we just pass data_processed() reactive as a parameter without func expecting that or it being altered in this envt?
        x$results_overall <- x$results_overall[  , ..keepcols] # needs ..
        x$results_bysite  <- x$results_bysite[   , ..keepcols] # needs ..
        x$longnames       <- x$longnames[            keepcols] # no ..
        # see note below about extra tab for expert users with bg details
        wb_out <- table_xls_from_ejam(
          ejamitout = x, save_now = FALSE,
          
          #### *** to be finished. see ejamlite
        )
        
      } else {
        
        wb_out <- table_xls_format(
          # note they seem to be data.frames, not data.tables, at this point, unlike how ejamit() had been returning results.
          overall = data_processed()$results_overall |> dplyr::select(names( data_processed()$results_overall)[keepcols]),
         
          eachsite = data_processed()$results_bysite |> dplyr::select(names( data_processed()$results_bysite)[keepcols2]),# needs ..  # 1 row per site
          longnames = data_processed()$longnames[           keepcols2], # not need ..       # 1 row, but full plain English column names.  keepcols here should be selecting cols not rows. 
          
          # *** NOTE:  data_processed()$results_bybg_people  # Do not provide this to xlsx by default. It is huge and for expert users only,
          # ***    but useful to create a plot of distance by group. Perhaps that could be created here to avoid passing the entire large table to table_xls_format() just for the plot. ***
          #  And want to give Option of getting the very large tab full of data_processed()$results_bybg_people  ...only for expert users it is useful
          #  Avoid making copies since that slows it down, unless an expert user knows they need it. 


          #mapadd = TRUE,

          hyperlink_colnames = c("EJScreen Report", "EJScreen Map" ,'ECHO report'),  # need to ensure these get formatted right to work as links in Excel
          # heatmap_colnames=names(table_as_displayed)[pctile_colnums], # can use defaults
          # heatmap_cuts=c(80, 90, 95), # can use defaults
          # heatmap_colors=c('yellow', 'orange', 'red') # can use defaults
          ## optional, shiny-specific arguments to go in 'Plot' and 'Notes' sheets
          summary_plot   = v1_summary_plot(),
          ok2plot = input$ok2plot,
          plot_distance_by_group = TRUE,
          bybg = data_processed()$results_bybg_people,
          analysis_title = input$analysis_title,
          buffer_desc    = "Selected Locations", 
          radius_or_buffer_in_miles = input$bt_rad_buff,
          radius_or_buffer_description = radius_or_buffer_description,
          # saveas = fname,
          testing = input$testing
        )
      }    
      ## save file and return for downloading - or do this within table_xls_format( , saveas=fname) ?
      openxlsx::saveWorkbook(wb_out, fname)
      
      
    } # end excel download contents
  ) # end download handler
  
  #############################################################################  # 
  # ~ ###  #
  ## *MAP to find a site clicked in site-by-site table ####
  
  # specify what data will be mapped
  # data_sitemap <- reactiveVal(NULL)
  # 
  # observeEvent(input$view3_table_rows_selected, {
  #   req(data_processed())
  #   #data_sitemap(data_uploaded()[input$view3_table_rows_selected,])
  #   if (submitted_upload_method() == 'SHP') {
  #     data_shp <- dplyr::inner_join(data_uploaded()[, c('siteid', 'geometry')], data_processed()$results_bysite[input$view3_table_rows_selected],
  #                                   by = c('siteid' = 'siteid'))
  #     print(input$view3_table_rows_selected)
  #     print(data_shp)
  #     data_sitemap(data_shp)
  #   } else {
  #     ## link selected row to doaggregate by site output for mapping
  #     data_sitemap(data_processed()$results_bysite[siteid %in% input$view3_table_rows_selected])
  #     data_sitemap(data_processed()$results_bysite[ejam_uniq_id %in% input$view3_table_rows_selected])
  #   }
  #   
  # })
  
  # output$v3_sitemap <- leaflet::renderLeaflet({
  #   ## wait for row to be selected
  #   ## note: summary rows are currently mapped but don't have a point location to map
  #   validate( need(!is.null(input$view3_table_rows_selected),
  #                  'Select a specific site in the table to see its location') )
  #   
  #   ## zoom in from original map to show single point (can zoom out and see others)
  #   
  #   #orig_leaf_map() #%>%
  #   # leaflet::setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 8)
  #   
  #   if (submitted_upload_method() == 'SHP') {
  #     ## alternate: plot single point individually on map (cannot zoom out and see others)
  #     leaflet(data_sitemap() %>% st_as_sf() %>% st_zm() %>% as('Spatial') ) %>%
  #       #setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 13) %>%
  #       addTiles() %>%
  #       addPolygons(popup = popup_from_any(data_sitemap() %>% sf::st_drop_geometry()),
  #                   popupOptions = popupOptions(maxHeight =  200))  
  #   } else {
  #     ## alternate: plot single point individually on map (cannot zoom out and see others)
  #     leaflet(data_sitemap()) %>%
  #       setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 13) %>%
  #       addTiles() # %>%     ######## *** 
  #     # addCircles(radius = 1 *  meters_per_mile, popup = popup_from_any(data_sitemap()), ######## *** 
  #     #            popupOptions = popupOptions(maxHeight =  200))    ######## *** 
  #   }
  #   
  # })
  #############################################################################  # 
  #. ## ##
  ## *BARPLOTS interactive   ####
  #. ## ##
  # see ?plot_barplot_ratios() in EJAM pkg
  # see notes on
  # https://exts.ggplot2.tidyverse.org/gallery/
  
  output$summ_bar_ind <- renderUI({
    if((input$include_ejindexes == "TRUE")){
      radioButtons(inputId = 'summ_bar_ind', 
                   label = h5('Indicator type'), 
                   choices = c('Demographic', 'Environmental', 'EJ','EJ Supplemental'), selected = "Environmental")
    } else {
      radioButtons(inputId = 'summ_bar_ind', 
                   label = h5('Indicator type'), 
                   choices = c('Demographic', 'Environmental'), 
                   selected = "Environmental")
    }
  })
  
  # output: 
  output$summ_display_bar <- renderPlot({
    req(data_summarized())
    req(input$summ_bar_ind)
    ## set indicator group column names
    mybarvars <- switch(input$summ_bar_ind,
                        'Demographic'   = c(names_d, names_d_subgroups),
                        'Environmental' = names_e,
                        'EJ'            = names_ej,
                        'EJ Supplemental'      = names_ej_supp
    )
    
    ## set indicator group friendly names  
    mybarvars.friendly <- switch(input$summ_bar_ind,
                                 'Demographic'   = c(names_d_friendly, names_d_subgroups_friendly),
                                 'Environmental' = names_e_friendly,
                                 'EJ'            = names_ej_friendly, 
                                 'EJ Supplemental'      = names_ej_supp_friendly
    )
    
    ## only using average for now
    mybarvars.stat <- 'avg' #"med"
    
    ## defaulting to average only in this version of EJAM
    mybarvars.sumstat <- c('Average site', 'Average person at these sites')
    
    ## if adding median ('med') back in future, can use this
    #mybarvars.stat <- input$summ_bar_stat
    # mybarvars.sumstat <- switch(input$summ_bar_stat,
    #                             'med' =  c('Median site', 'Median person'),
    #                             'avg' = c('Average site','Average person')
    # )
    
    ## filter to necessary parts of batch.summarize output
    barplot_data <- data_summarized()$rows %>% 
      tibble::rownames_to_column(var = 'Summary') %>% 
      dplyr::mutate(Summary = gsub('Average person', 
                                   'Average person at these sites',Summary)) %>% 
      dplyr::filter(Summary %in% mybarvars.sumstat)
    
    ## set ggplot theme elements for all versions of barplot
    ggplot_theme_bar <- ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'bottom',
                     axis.text = ggplot2::element_text(size = 16),
                     axis.title = ggplot2::element_text(size = 16),
                     legend.title = ggplot2::element_text(size = 16),
                     legend.text = ggplot2::element_text(size = 16),
                     strip.text = element_blank(),
                     strip.background = element_blank()
      )
    
    ## raw data 
    if (input$summ_bar_data == 'raw') {
      
      ## pivot from wide to long, 1 row per indicator 
      barplot_data_raw <- barplot_data %>% 
        dplyr::select(Summary, dplyr::all_of( mybarvars)) %>% 
        tidyr::pivot_longer(cols = -1, names_to = 'indicator') %>% 
        dplyr::mutate(type = 'raw')
      
      ## median - not currently displayed
      if (mybarvars.stat == 'med') {
        barplot_usa_med <- usastats %>% 
          dplyr::filter(REGION == 'USA', PCTILE == 50) %>% # for median
          dplyr::mutate(Summary = 'Median person in US') %>% 
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
          tidyr::pivot_longer(-Summary, names_to = 'indicator')
        
        ## NOTE: Median Person calculations are all 0s for now!
        barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_med)
        
        ## average  
      } else {
        barplot_usa_avg <- usastats %>% 
          dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>% 
          dplyr::mutate(Summary = 'Average person in US') %>% 
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
          tidyr::pivot_longer(-Summary, names_to = 'indicator')
        
        barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_avg)
      }
      
      ## set # of characters to wrap labels
      n_chars_wrap <- 15
      
      barplot_input$Summary <- factor(barplot_input$Summary, levels = c('Average person in US','Average site','Average person at these sites'))
      
      ## merge with friendly names and plot
      p_out <- barplot_input %>% 
        dplyr::left_join( data.frame(indicator = mybarvars, indicator_label = gsub(' \\(.*', '', mybarvars.friendly))) %>% 
        ggplot() +
        geom_bar(aes(x = indicator_label, y = value, fill = Summary), stat = 'identity', position = 'dodge') +
        #viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        scale_fill_manual(values = c('Average person in US' = 'lightgray', 'Average person at these sites' = '#62c342',
                                     'Average site' = '#0e6cb5')) +
        #scale_fill_brewer(palette = 'Dark2') +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
      
      ## let environmental raw values have their own y axis
      if (input$summ_bar_ind == 'Environmental') {
        p_out <- p_out + facet_wrap(~indicator_label, 
                                    #ncol = 4, 
                                    scales = 'free')
      } else {
        p_out <- p_out + facet_wrap(~indicator_label, 
                                    #ncol = 4, 
                                    scales = 'free_x')
      }
      
      p_out +
        labs(x = NULL, y = 'Indicator Value', fill = 'Legend') +
          ggplot_theme_bar
      
      ## future: add % scaling and formatting for demographic indicators
      ## see ggplot2::scale_y_continuous and scales::label_percent
      
      ## ratio to us  
    } else if (input$summ_bar_data == 'ratio') {
      
      barplot_data_raw <- barplot_data %>% 
        dplyr::select(Summary, dplyr::all_of( mybarvars)) %>% 
        tidyr::pivot_longer(cols = -1, names_to = 'indicator') 
      
      ## average
      if (mybarvars.stat == 'avg') {
        ## pull US average values from usastats to compute ratios
        barplot_usa_avg <-  dplyr::bind_rows(
          usastats %>% 
            dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>% 
            dplyr::mutate(Summary = 'Average person at these sites') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          usastats %>% 
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
          usastats %>% 
            dplyr::filter(REGION == 'USA', PCTILE == 50) %>% 
            dplyr::mutate(Summary = 'Median person') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>% 
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          usastats %>% 
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
      
      barplot_input$Summary <- factor(barplot_input$Summary, levels = c(
        'Average person in US',
        'Average site',
        'Average person at these sites'
      ))
      
      ## join and plot
      barplot_input %>% 
        dplyr::left_join( data.frame(indicator = mybarvars, indicator_label =  gsub(' \\(.*', '', mybarvars.friendly))) %>% 
        ggplot() +
        ## add bars - position = 'dodge' places the 3 categories next to each other
        geom_bar(aes(x = indicator_label, y = ratio, fill = Summary), stat = 'identity', position = 'dodge') +
        ## add horizontal line at 1
        geom_hline(aes(yintercept = 1)) +
        ## set color scheme
        scale_fill_manual(values = c(
          'Average person in US'          = 'lightgray', 
          'Average person at these sites' = '#62c342',
          'Average site'                  = '#0e6cb5'
        )) +
        # scale_fill_brewer(palette = 'Dark2') +
        ## alternate color scheme
        #viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        ## wrap long indicator labels on x axis
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
        ## set axis labels
        labs(x = '', y = 'Indicator Ratio', fill = 'Legend') +
        ## break plots into rows of 4 
        facet_wrap(~indicator_label, 
                   #ncol = 4, 
                   scales = 'free_x') +
        ggplot_theme_bar
    }
  }) # end of summ_display_bar  for barplot
  
  #############################################################################  # 
  ## *HISTOGRAM interactive    ####
  #. ####
  
  # other plot ideas
  # https://exts.ggplot2.tidyverse.org/gallery/
  
  output$summ_hist_ind <- renderUI({
    
    req(input$include_ejindexes)
    req(input$summ_hist_data)
    
    if((input$include_ejindexes == "TRUE")){
      
      if(input$summ_hist_data == 'pctile'){
        nms <-  c(names_d_pctile,
                  names_d_subgroups_pctile,
                  names_e_pctile, names_ej_pctile,
                  names_ej_supp_pctile)
        
      } else if(input$summ_hist_data == 'raw'){
        nms <-  c(names_d,
                  names_d_subgroups,
                  names_e, names_ej, names_ej_supp)
       
      }
      friendly_nms <- c(names_d_friendly, names_d_subgroups_friendly, names_e_friendly,
                        names_ej_friendly, names_ej_supp_friendly)
    } else {
      if(input$summ_hist_data == 'pctile'){
        nms <-  c(names_d_pctile,
                  names_d_subgroups_pctile,
                  names_e_pctile) 
      } else if(input$summ_hist_data == 'raw'){
        nms <-  c(names_d,
                  names_d_subgroups,
                  names_e) 
      }
      friendly_nms <- c(names_d_friendly, names_d_subgroups_friendly, names_e_friendly) 
      
    }
  selectInput('summ_hist_ind', label = 'Choose indicator',
              choices = setNames(
                object = nms,
                nm = friendly_nms
              ) # end setNames
  ) # end selectInput
  })
  
  ## output: 
  output$summ_display_hist <- renderPlot({
    
    # req(data_summarized()) # it used to say this was required here but i dont think it is anymore
    req(data_processed())
    req(input$summ_hist_ind)
    ## set font sizes
    ggplot_theme_hist <- theme(
      plot.title = ggplot2::element_text(size = 18, hjust = 0.5),
      axis.text  = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 16)
    )
    
    ## future settings: bin sizes, reference lines
    
    if (input$summ_hist_distn == 'Sites') {
      if (input$summ_hist_data == 'raw') {
        
        ## subset doaggregate results_bysite to selected indicator
        hist_input <- data_processed()$results_bysite[, input$summ_hist_ind, with = FALSE]
        names(hist_input)[1] <- 'indicator'
        
        ## plot histogram
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x = 'Percentile',
            y = 'Number of sites',
            title = 'Histogram of Raw Indicator Values Across Sites'
          ) +
          theme_bw() +
          ggplot_theme_hist
        
      } else if (input$summ_hist_data == 'pctile') {
        
        ## subset doaggregate results_bysite to selected indicator
        #hist_input <- data_processed()$results_bysite[, paste0('pctile.',input$summ_hist_ind), with = FALSE]
        hist_input <- data_processed()$results_bysite[, input$summ_hist_ind, with = FALSE]
        
        names(hist_input)[1] <- 'indicator'
        
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          labs(
            x = 'Percentile',
            y = 'Number of Sites',
            title = 'Histogram of US Percentile Indicator Values Across Sites'
          ) +
          theme_bw() +
          ggplot_theme_hist
      }
    } else if (input$summ_hist_distn == 'People') {
      if (input$summ_hist_data == 'raw') {
        
        ## subset doaggregate results_bysite to selected indicator
        hist_input <- data_processed()$results_bysite[, c('pop', input$summ_hist_ind), with = FALSE]
        names(hist_input)[2] <- 'indicator'
        
        ## plot population weighted histogram
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator, y = after_stat(density), weight = pop), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x = '',
            y = 'Weighted Density',
            title = 'Population Weighted Histogram of Raw Indicator Values'
          ) +
          theme_bw() +
          ggplot_theme_hist
        
      } else if (input$summ_hist_data == 'pctile') {
        
        ## subset doaggregate results_bysite to selected indicator
        #hist_input <- data_processed()$results_bysite[, c('pop',paste0('pctile.',input$summ_hist_ind)), with = FALSE]
        hist_input <- data_processed()$results_bysite[, c('pop', input$summ_hist_ind), with = FALSE]
        names(hist_input)[2] <- 'indicator'
        
        ## plot population weighted histogram 
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator, y = after_stat(density), weight = pop), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x = '',
            y = 'Weighted Density',
            title = 'Population Weighted Histogram of US Percentile Values'
          ) +
          theme_bw() +
          ggplot_theme_hist
      }
    }
  }) # end of summ_display_hist for histogram
  
  
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
  # ~--------------------------- ### 
  # ______ FULL REPORT (Word doc) _________ ####
  # .  ## ##
  
  ## code for storing all shiny input values - not used currently
  # observeEvent(input$all_tabs == 'Generate Report',
  #  {
  #    list_of_inputs <- reactiveValuesToList(input)
  #  })
  
  ## Create and download FULL static report 
  output$rg_download <- downloadHandler(
    filename = 'report.doc',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      ## copy Rmd from inst/report to temp folder
      file.copy(from = app_sys('report/written_report/report.Rmd'),  # treats EJAM/inst/ as root
                to = tempReport, overwrite = TRUE)
      ## pass image and bib files needed for knitting to temp directory
      for (i in list.files(app_sys('report/written_report'), pattern = '.png|.bib')) {   # treats what was in source/EJAM/inst/report/ as installed/EJAM/report/  once pkg is installed
        file.copy(from = app_sys('report/written_report', i),    # source/EJAM/inst/report/ = installed/EJAM/report/
                  to = file.path(tempdir(), i), 
                  overwrite = TRUE)
      }
      
      # Set up parameters to pass to Rmd document - 
      #  MAKE SURE all parameter names are used (identical names, and all are there) in these 4 places: 
      #  1. input$ ids in app_ui.R, from user, to customize the long report
      #  2. params$ list passed by app_server.R to render the Rmd doc
      #  3. params: accepted in  .Rmd yaml info header 
      #  4. params$  as used within body of  .Rmd text inline and in r code blocks.
      
      isolate({ # need someone to confirm this is needed/helpful and not a problem, to isolate this.
        
        params <- list(
          testmode = FALSE,
          
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
          results_formatted =  table_tall_from_overall(data_processed()$results_overall, data_processed()$longnames),  
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
          acs_version =  "2017-2021",
          ejscreen_version =  "2.2"
        )
      })
      # [TEMPORARILY SAVE PARAMS FOR TEST ING] ## ##
      # if (input$testing) {saveRDS(params, file = "./inst/testparams.RDS")} ################################ TEMPORARILY SAVE PARAMS FOR TESTING# # 
      
      # Knit report to Word Doc ## ##
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_format = 'word_document',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        intermediates_dir = tempdir()
      )
    } # end of download function
  ) # end of long report download handler
  
  ## 
    
    ## build community report page with HTML
    output$comm_report_html <- renderUI({
      req(data_processed())
      
      rad <- data_processed()$results_overall$radius.miles # input$radius can be changed by user and would alter the report text but should just show what was run not what slider currently says
      popstr <- prettyNum(total_pop(), big.mark=',')
      
      if(submitted_upload_method() == 'SHP'){
        location_type <- " selected polygons"
        radiusstr <- paste0(rad, " mile", 
                            ifelse(rad > 1, "s", ""), " of ")
        
      } else if (submitted_upload_method() == 'FIPS'){
        location_type <- " selected shapes"
        radiusstr <- ""
      } else {
        location_type <- " selected points"
        radiusstr <- paste0(rad, " mile", ifelse(rad > 1, "s", ""), " of ")
                            
      }
      
      locationstr <- paste0("Residents within ",
                            radiusstr,
                            "any of the ", NROW(data_processed()$results_bysite[data_processed()$results_bysite$valid == T,]),location_type)
      
      
     
        print(dim(data_processed()$results_overall))
      ## generate full HTML using external functions
      full_page <- build_community_report(
        output_df = data_processed()$results_overall,
        analysis_title = input$analysis_title,
        totalpop = popstr, 
        locationstr = locationstr,
        include_ejindexes = (input$include_ejindexes == 'TRUE'), 
        in_shiny = TRUE
      )
      ## return generated HTML
      full_page
    })
     # end of observer that send results of calculation to UI
  
  
  #. ####
  #############################################################################  # 
  #  
  # ______ ejscreenapi MODULE _________ ####
  # (to get batch via API)
  
  # create UI part for main EJAM app here rather than in app_ui because we need access to input$ radius of main app to pass that to module that uses it as the initial radius shown on its slider
  # pass from server code of app to server code of module by creating the main radius UI in server of app (cannot access the input$ in UI of app)
  # Not sure if I can pass a reactive or need to pass reactive value so it can get updated by the module without needing to return a list of values from the module?
  # default_radius_react_passed <- reactiveVal() # initialize/create the variable that will store the latest radius set by outer app
  # observe(  
  #   default_radius_react_passed(input$bt_rad_buff) # update the value of this reactiveVal anytime outer app slider is adjusted
  # )
  
  
  # output$mod_ejscreenapi_ui_TO_SHOW_IN_APP_UI <- renderUI({
  #   mod_ejscreenapi_ui("x2", 
  #                      simpleradius_default_for_ui = 1 # , 
  #                      # default_radius_react = default_radius_react_passed
  #                      ) # reactive object gets passed without parentheses. pass a reactive radius HERE to server not ui.  
  # })
  
  
  
  # default_radius_react_passed <- reactiveVal(input$bt_rad_buff) # pass to UI of module not server code of module
  # default_points_react_passed <- reactiveVal() # initialize it empty
  # observe(
  #   default_points_react_passed(  data_uploaded()  ) # update default_points_react_passed when data_uploaded() changes
  # )
  # table_as_displayed_reactive <- reactive(
  #   
  #   mod_ejscreenapi_server(
  #     "x2", 
  #     default_points_shown_at_startup_react = default_points_react_passed, #reactive(testpoints_5[1:2,]), 
  #     use_ejscreenit = T # use_ejscreenit_tf
  #   )
  #   
  #   # mod_ejscreenapi_server("x2", 
  #   #                        # default_points = testpoints_5[1:2,],
  #   #                        default_radius_react = default_radius_react_passed,
  #   #                        default_points_shown_at_startup_react = default_points_react_passed  #reactive value object gets passed without parentheses
  #   # )
  # )
  
  # NOTE:
  # If a module needs to use a reactive expression, the outer function should take the reactive expression as a parameter. 
  # If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.
  # If a module needs to access an input that isn’t part of the module, the 
  #   containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
  #   myModule("myModule1", reactive(input$checkbox1))
  # 
  # x = 0
  # x <- reactive({
  #   req(data_uploaded())
  #   mod_ejscreenapi_server("x2", default_points_react = data_uploaded())
  # }) %>% 
  #   bindCache(req(data_uploaded, ))
  # if (is.reactive(x)) {cat("API module output is a reactive value \n")} else {cat("API module output is not reactive \n")}
  
  # Not sure if or when control would get passed back to EJAM main app code or when x might be assigned or what***
  #  I think x will be a reactive that is the output table?? ***
  # try to pass (to module) the data_uploaded() points already uploaded in EJAM app points 
  # try to get output here? and do what? display or pass back to the EJAM app code that can show all the info and download it?
  
  #. ####
  #. ####
  
  
} # end of app_server

