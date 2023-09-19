#' The application User-Interface
#' @noRd
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom shinyjs useShinyjs extendShinyjs
#' 
app_ui  <- function(request) {
  tagList(
    # golem_add_external_resources() ####
    # Leave this function for adding external resources, specifying title of app, see end of this source file.
    golem_add_external_resources(),
    # ~ ####
    # _____App UI fluidPage starts here _______ ####
    fluidPage(
      ## to profile parts of the Shiny app instead of all:
      # profvis_ui("profiler") # and see module in server
      
      ### enable JavaScript   ####
      #   functionality (such as resetting inputs) etc.
      shinyjs::useShinyjs(),
      ## javascript function for jumping to top of screen
      shinyjs::extendShinyjs(text = "shinyjs.toTop = function() {window.scrollTo(0, 0);}", functions = "toTop"),
      ## change selected tab color - #005ea2 matches blue on rest of website
      includeCSS('www/ejam_styling.css'),
      ### html header inserted from global.R ####
      html_header_fmt,
      
      ### title (for app and browser tab) ####
      titlePanel(title = "EJAM (Environmental Justice Analysis Multi-site) Tool",
                 windowTitle = "EJAM (Environmental Justice Analysis Multi-site) Tool"
      ),
      
      ## ______ create tabsetPanel with tabs for different sections ####
      tabsetPanel( # up to line 1101 or so
        id = 'all_tabs',
        type = 'hidden',
        selected = 'Site Selection',
        # ~ ####
        # ABOUT ####
        tabPanel(title = 'About EJAM',
                 br(), ## vertical space
                 
                 fluidRow(
                   column(8,
                          
                          ## html intro text from global.R  
                          intro_text,
                          actionButton('back_to_site_sel2', 
                                       label = div(icon('play', 
                                                        style='transform: rotate(180deg);'), 
                                                   HTML('&nbsp;'), 'Return to Site Selection'),
                                       class='usa-button'),
                          
                          br(),br(),
                          ## button to reveal Advanced Settings tab 
                          actionButton('ui_show_advanced_settings','Show Advanced Settings Tab', 
                                       class='usa-button'),
                          actionButton('ui_hide_advanced_settings','Hide Advanced Settings Tab', 
                                       class='usa-button')
                   ),
                   column(4,
                          htmltools::img( id = "biglogo", src = "www/ejamhex4.png")
                   )
                 )
        ), # end About EJAM tab
        
        ######################################################################################################### #
        # ~ ####
        # SITE SELECTION   ####
        # ~ ### #
        
        tabPanel(
          title = 'Site Selection',
          
          br(), ## vertical space
          
          #h3('Welcome to EJAM'),
          span('EJAM lets you explore the demographics and environmental conditions in any list of places, such as for anyone who lives within 1 mile of a certain type of EPA-regulated site. You can learn more about EJAM at the ',
               actionLink('link_to_about', label = 'About EJAM page.')),
          
          hr(), ## horizontal line
          
          ## fluidRow container for upload method (left column) and map (right column) ####
          fluidRow( # through about line 441 
            
            ## upload-methods column ####
            column(4,  # through about line 359
                   h4('Specify Locations to Analyze', style='text-align: center;'),
                   
                   ### input: choose between picking by categories vs uploading places ####
                   div(style='border: 1px solid #005ea2; padding: 10px;',
                       radioButtons(inputId = 'ss_choose_method', 
                                    label = 'How would you like to identify locations?',
                                    choiceNames = c('Select a category of locations',
                                                    'Upload specific locations'),
                                    choiceValues = c('dropdown', 'upload')),
                   ),
                   br(),
                   ### choose category type (NAICS, SIC, etc.) ####
                   conditionalPanel(
                     condition = 'input.ss_choose_method == "dropdown"',
                     div(style='border: 1px solid #005ea2; padding: 10px;',
                         
                         selectInput(inputId = 'ss_choose_method_drop', 
                                     label = tags$span('How would you like to select catgories?'
                                                       
                                     ),
                                     choices = c('by Industry (NAICS) Code'='NAICS',
                                                 'by Industry (SIC) Code'='SIC',
                                                 'by EPA Program'='EPA_PROGRAM',
                                                 'by MACT subpart'='MACT'))
                     )
                   ), # end conditional choose category type 
                   
                   ## input: choose what type of IDs/data to upload ####
                   conditionalPanel(
                     condition = 'input.ss_choose_method == "upload"',
                     div(style='border: 1px solid #005ea2; padding: 10px;',
                         selectInput(inputId = 'ss_choose_method_upload',
                                     #label = 'What type of data are you uploading?',
                                     label = tags$span(
                                       'What type of data are you uploading?'
                                       
                                     ),
                                     choices = c('Latitude/Longitude'='latlon',
                                                 'EPA Facility ID (FRS Identifiers)'='FRS',
                                                 'EPA Program IDs'='EPA_PROGRAM',
                                                 'FIPS Codes'='FIPS',
                                                 'Shapefile of polygons'='SHP'))
                     )
                   ), # end conditional pick what type of IDs to upload
                   
                   br(),
                   # __wellPanel start ___----------------------------------------------------------------------
                   wellPanel(
                     style = 'background-color: #e5f2f5; min-height: 500px',
                     
                     fluidRow(
                       column(
                         12,
                         ## input: choose among facility dropdown options, conditional panel ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'NAICS'",
                           
                           div(style='border: 1px solid #005ea2; padding: 10px; background-color: white',
                               radioButtons('add_naics_subcategories', "Add all subcategories of NAICS?",
                                            choiceNames = c("Yes","No"),
                                            choiceValues = c(TRUE,FALSE),
                                            selected = TRUE)
                           ),
                           br(),
                         ), # end conditional panel
                         
                         #offset=3, 
                         ## latlon conditional panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'latlon'",
                           
                           ## input: Upload list of facility lat/longs
                           fileInput(inputId = 'ss_upload_latlon',  
                                     label = 'Upload a file with lat-long coordinates',
                                     multiple = FALSE,
                                     accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")
                                     # add hover tips here maybe, or even a button to view examples of valid formats and details on that.
                           ),
                           
                           tags$span(
                             
                             tags$ul(
                               tags$li('Required filetype: .csv, .xls, or .xlsx'),
                               tags$li('Required Columns: lat, lon'),
                               tags$li('Optional Columns: unique ID')
                             )
                             
                           ),
                           actionButton('latlon_help', label='More Info', 
                                        class = 'usa-button usa-button--outline')
                           #HTML(latlon_help_msg)
                         ), # end latlong conditionalPanel
                         
                         ## NAICS conditional panel  ------------------------------------- -####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'NAICS'",
                           
                           selectizeInput(
                             inputId = "ss_select_naics",
                             label = htmltools::h6("Select industry of interest"),
                             # choose from named numeric vector on server-side
                             ## number is NAICS like 31182, names are like "31182 - Cookie, Cracker, and Pasta Manufacturing" 
                             choices = NULL, 
                             selected = NULL,
                             width = 400,
                             multiple = TRUE,
                             ## add X to remove selected options from list
                             options = list('plugins' = list('remove_button'))
                           ),#, # xxx
                           
                           br(), ## vertical space
                           
                         ), # end NAICS conditionalPanel overall
                         
                         ## FRS conditional panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'FRS'",
                           ## input: Upload list of FRS identifiers
                           shiny::fileInput(
                             inputId = 'ss_upload_frs',
                             label = 'Upload a file with FRS identifiers',
                             accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                           ), # xxx
                           
                           tags$span(
                             tags$ul(
                               tags$li('Required filetype: .csv, .xls, or .xlsx'),
                               tags$li('Required Columns: REGISTRY_ID'),
                               tags$li('Optional Columns: siteid, lat, lon')
                             )
                           ),
                           
                           actionButton('frs_help', label='More Info', class='usa-button usa-button--outline')
                           
                         ), # end FRS conditionalPanel 
                         
                         ## EPA program dropdown conditional panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'EPA_PROGRAM'",
                           
                           
                           span('More info about these programs can be found here: ', a('https://www.epa.gov/frs/frs-data-sources',href='https://www.epa.gov/frs/frs-data-sources', target='_blank')),
                           br(),
                           
                           ## input: select an EPA program from list ------------------------------------- - ------------------------------------- -
                           selectizeInput(inputId = 'ss_select_program', label = 'Pick an EPA program',
                                          ## named vector in global.R - values are acronyms, 
                                          ## names include # of rows corresponding to that program
                                          choices = epa_programs,
                                          ## add X to remove selected options from list
                                          options = list('plugins' = list('remove_button'))),
                           
                         ), # end conditional panel EPA programs
                         
                         ## EPA program upload conditional panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'EPA_PROGRAM'",
                           ## input: upload an EPA program ID file
                           fileInput(inputId = 'ss_upload_program',
                                     label = 'Upload a file with program IDs'),
                           
                           tags$ul(
                             tags$li('Required filetype: .csv, .xls, or .xlsx'),
                             tags$li('Required columns: program, pgm_sys_id'),
                             tags$li('Optional columns: siteid, REGISTRY_ID, lat, lon')
                           ),
                           
                           actionButton('epa_program_help', label='More Info', class='usa-button usa-button--outline')
                           
                           
                           
                         ), #end EPA program upload conditional panel
                         
                         ## SIC conditional panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'SIC'",
                           
                           # radioButtons('add_sic_subcategories', "Add all subcategories of SIC?",
                           #              choiceNames = c("Yes","No"),
                           #              choiceValues = c(TRUE,FALSE),
                           #              selected = TRUE),
                           
                           ## input: Select SIC from list
                           selectizeInput(
                             inputId = "ss_select_sic",
                             label = htmltools::h6("Select industry of interest"),
                             # choose from named numeric vector on server-side
                             ## number is NAICS like 31182, names are like "31182 - Cookie, Cracker, and Pasta Manufacturing" 
                             choices = NULL, 
                             selected = NULL,
                             width = 400,
                             multiple = TRUE,
                             ## add X to remove selected options from list
                             options = list('plugins' = list('remove_button'))
                           ), #, # xxx
                           #),  # end dropdown SIC sub- conditionalPanel
                           
                           br(), ## vertical space
                           
                         ), # end SIC conditionalPanel
                         
                         ## FIPS conditional panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'FIPS'",
                           
                           ## input: Upload list of facility lat/longs
                           fileInput(inputId = 'ss_upload_fips',  
                                     label = 'Upload a list of FIPS codes',
                                     multiple = FALSE,
                                     accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")
                                     # add hover tips here maybe, or even a button to view examples of valid formats and details on that.
                           ),
                           
                           tags$ul(
                             tags$li('Required filetype: .csv, .xls, or .xlsx'),
                             tags$li('Required columns: FIPS or alias'),
                             tags$li('Optional columns: siteid')
                           ),
                           
                           actionButton('fips_help', label='More Info', class='usa-button usa-button--outline')
                           
                         ), # end FIPS conditionalPanel
                         
                         ## Shapefile Upload conditional panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'SHP'",
                           ## input: Upload list of FRS identifiers
                           shiny::fileInput(
                             inputId = 'ss_upload_shp',
                             label = 'Upload a shapefile',
                             accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj"),multiple=TRUE
                           ),
                           
                           tags$ul(
                             tags$li('Required files: .shp, .shx, .dbf, .prj'),
                             tags$li('Required fields: geometry')
                           ),
                           
                           actionButton('shp_help', label='More Info', class='usa-button usa-button--outline')
                           
                           #, # xxx
                         ), # end Shapefile conditionalPanel
                         
                         ## MACT panel ------------------------------------- - ####
                         conditionalPanel(
                           condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'MACT'",
                           
                           ## input: choose MACT subpart from dropdown list
                           selectInput(inputId = 'ss_select_mact',
                                       label = 'Choose a MACT subpart',
                                       choices = setNames(mact_table$subpart,
                                                          mact_table$dropdown_label)
                                       
                           )
                         )#, # end MACT conditionalPanel
                         
                         
                       ) # end column
                     ) # end fluidRow
                   ), # __ wellPanel end ___------------------------------------------------------
                   
                   br(),#br(),
                   ## input: return to results tab if analysis has been run  ####
                   shinyjs::hidden(
                     actionButton(inputId = 'return_to_results',
                                  label = div(icon('play', 
                                                   style='transform: rotate(180deg);'),
                                              HTML('&nbsp;'), 
                                              'Return to Previous Results'),
                                  class = 'usa-button')
                   )
            ), # end of upload-methods column
            # ----------------------------------------------------------------------- --
            
            
            column(8,
                   uiOutput('invalid_sites_alert2'),
                   br(),
                   ## text and button to view uploaded data  ####
                   fluidRow(
                     column(6,
                            br(),
                            ## output: display number of uploaded sites
                            htmlOutput(outputId = 'an_map_text')#, # xxx
                     ),
                     
                     column(6,
                            ## add button and modal to show uploaded data interactively
                            actionButton('show_data_preview', label = 'Review selected sites',
                                         class = 'usa-button usa-button--outline'),
                            ## on button click, show modal with DT table of uploaded
                            shinyBS::bsModal(id = 'view_data_modal', 
                                             title = 'Selected location data', 
                                             trigger = 'show_data_preview',
                                             size = 'large',
                                             helpText('View or download data corresponding to your upload/selections.'),
                                             ## use download buttons for speed and handling larger data
                                             downloadButton('download_preview_data_csv', label = 'CSV', class='usa-button'),
                                             downloadButton('download_preview_data_xl', label = 'Excel', class='usa-button'),
                                             br(),br(),
                                             DT::DTOutput('print_test2_dt', width = '100%'))
                     )
                   ), # end view data uploads
                   
                   h4('Selected Location Map'),
                   helpText('Red circles indicate overlapping sites.'),
                   
                   ## output: show leaflet map of uploaded points  ####
                   shinycssloaders::withSpinner(
                     leaflet::leafletOutput(outputId = 'an_leaf_map', 
                                            height = '500px', 
                                            width = '100%')
                   ),
                   
                   br(), 
                   ## RADIUS SLIDER 
                   fluidRow(
                     column(6,
                            ## separated label from slider to allow for line break - shiny does not support it
                            ## in update*Input: https://github.com/rstudio/shiny/issues/3678
                            htmlOutput('radius_label'),
                            ## input: RADIUS SLIDER for circular buffer ####
                            shiny::sliderInput(inputId = 'bt_rad_buff',
                                               ## label is updated in server
                                               label = "",#htmltools::h5('Within what distance of a site?'),
                                               #label = htmltools::h5("Radius of circular buffer in miles"),
                                               value = 1.0, step = 0.25,
                                               min = 0, max = 10,
                                               post = ' miles'
                            ),
                            
                            
                     ),
                     column(6,
                            ## input: title for analysis (goes in report header) ####
                            shiny::textInput('analysis_title', 
                                             label = 'Name of Your Analysis',
                                             placeholder = 'EJ Analysis of My List of Places',
                                             value = 'EJ Analysis of My List of Places'),
                            ## input: run analysis button     ####
                            shiny::actionButton(inputId = 'bt_get_results', 
                                                label = div('Start Analysis',HTML('&nbsp;'), icon('play')),
                                                class = 'usa-button'
                                                ## extra formatting - optional
                                                #style = 'color: #fff; background-color: #005ea2; height: 50px; width: 100px; border-radius: 5%;')
                            )
                            
                     ),
                   )
                   
                   
                   
            ) # end of column with map
          ), # end of fluidRow container for upload method (left column) and map (right column)
          
          ## not used currently - button to switch units and update slider
          ## input: switch units to km for radius slider
          # radioButtons(inputId = 'radius_units',
          #              label = 'Choose units for radius',
          #              choices = c('miles','kilometers'),
          #              selected = 'miles')
          
        ), # end Site Selection tab panel
        # . #### 
        # >>>>tabPanel "See Results"^^^^^^^^^^^^^^^^^^^^^^^^^^ ####
        tabPanel(title = 'See Results',
                 
                 br(),
                 
                 actionButton('back_to_site_sel', label = div(icon('play', style='transform: rotate(180deg);'), 
                                                              HTML('&nbsp;'), 'Return to Site Selection'),
                              class='usa-button'),  
                 br(),br(),
                 
                 h4('Overall Results'),
                 helpText('The results of your analysis can be viewed in 3 forms: a summary report, interactive details, or a full written report.'),
                 # >>>>tabsetPanel for results_tabs - contains tabPanel Summary, etc.^^^^^^ ####
                 #tags$div( class = 'results_tabs_theme',
                 tabsetPanel(id = 'results_tabs',
                             
                             #type = 'pills',
                             
                             ######################################################################################################### #
                             # ~ ####
                             # SHORT SUMMARY REPORT TAB ####
                             # ~ ####
                             
                             tabPanel(title = 'Summary',
                                      
                                      br(), ## vertical space
                                      
                                      htmlTemplate(app_sys('report', 'summary_report_tab.html'),   # treats EJAM/inst/ as root
                                                   pop_header = htmlOutput(outputId = 'view1_total_pop'),
                                                   demog_table = shinycssloaders::withSpinner(
                                                     gt::gt_output(outputId = 'view1_demog_table')
                                                   ),
                                                   demog_plot =fluidRow(
                                                     column(
                                                       12,
                                                       align = 'center',
                                                       shinycssloaders::withSpinner(
                                                         plotOutput(outputId = 'view1_summary_plot', width = '100%', height = '400px') #width = '100%', height='700px')
                                                       )
                                                     )
                                                   ),
                                                   
                                                   map = shinycssloaders::withSpinner(
                                                     leaflet::leafletOutput('quick_view_map', width = '1170px', height = '627px')
                                                   ), 
                                                   env_table = shinycssloaders::withSpinner(
                                                     gt::gt_output(outputId = 'view1_envt_table')
                                                   ),
                                                   dl_button = tags$div(
                                                     shiny::downloadButton(outputId = 'summary_download', 
                                                                           label = 'Download Summary Report',
                                                                           class='usa-button'),
                                                     style = 'text-align: center;'
                                                   ),
                                                   format_button = NULL
                                                   # ,
                                                   
                                      )
                                      
                                      
                             ), # end of Summary tab
                             
                             # DETAILED RESULTS - TABs group -------------------------- ####
                             
                             tabPanel(title = 'Details',
                                      
                                      br(),
                                      
                                      div(class='navbar1',
                                          navbarPage(
                                            title=NULL,
                                            #   navlistPanel(
                                            #   "Results Pages",
                                            #   well = FALSE,
                                            fluid = FALSE,
                                            # widths = c(2,10),
                                            
                                            ######################################################################################################### #
                                            # ~ ####
                                            ## SITE BY SITE (Table and Map)    ####
                                            
                                            tabPanel(title = 'Site-by-Site Table',
                                                     ## _button: Excel Download ####
                                                     
                                                     h4('About this Table'),
                                                     helpText('This table shows results for each Location in the analysis. It includes location and EJScreen demographic, environmental, and EJ indicator information.\n
       The download available includes more columns than our displayed here.'),
       
       fluidRow(
         column(6,
                #h3(id = 'site_by_site', 'Site-by-Site Table'),
         ),
         column(6,
                ## button to download excel Table of Sites/Results - uses xls_formatting2
                downloadButton('download_results_table', 'Download Results Table',
                               class = 'usa-button'
                )
         )
       ),
       br(), ## vertical space
       
       ## _output: Interactive Table of Sites/Results ####
       shinycssloaders::withSpinner(
         DT::DTOutput(outputId = 'view3_table', width = '100%')
       ),
       ## _output: Map 1 site selected from table ####
       shinycssloaders::withSpinner(
         leaflet::leafletOutput(outputId = 'v3_sitemap')
       )
                                            ), # end site by site table 
       # . ####
       ## PLOT AVG SCORES /TAB ####
       # . ####
       
       tabPanel('Plot Average Scores', 
                h4('About this Chart'),
                helpText('These charts show how each demographic group and environmental stressor, in the analyzed locations, compares to its US average.'),
                
                wellPanel(
                  style='width: 100%;',
                  ### _BARPLOT 
                  br(),
                  #h3(id = 'barplot','Compare Across Indicators'),
                  
                  #fluidRow(
                  ## input: Barplot setting - indicator type
                  # column(2, 
                  shinycssloaders::withSpinner(
                    ## output: display barplot
                    plotOutput(outputId = 'summ_display_bar', height='600px')
                  ),
                  
                  fluidRow(
                    column(4,
                           radioButtons(inputId = 'summ_bar_ind', 
                                        label = h5('Indicator type'), 
                                        choices = c('Demographic', 'Environmental','EJ'), selected = "Environmental"),
                    ),
                    column(4,
                           ## input: Barplot setting - data type
                           radioButtons(inputId = 'summ_bar_data', label = 'Data Type', 
                                        choiceValues = c('ratio',      'raw'),      # no 'pctile' at this time
                                        choiceNames  = c('Ratio to US','Raw data'), # no 'Percentile of population' at this time
                                        selected = 'ratio'), 
                    )
                  ),
                  
                  
                  
                  ## hiding this option for now - defaulting to Average
                  ## input: Barplot setting - statistic type
                  # radioButtons(inputId = 'summ_bar_stat', 'Statistic', 
                  #              choiceValues = c('avg', 'med'),
                  #              choiceNames = c('Average', 'Median'))
                  #  ),
                  
                  
                  h4('Definitions'),
                  HTML("<strong>Average site</strong> = the average site's average resident (the average resident's score is calculated at each site as the site-specific population-weighted mean, and then the arithmetic mean of those site-specific scores is calculated)
          <br><strong>Average person at these sites</strong> = the average person among all the residents who are at any one or more of the sites, counting each person only once even if they live near more than one site.")
                ),
          
          br(), ## vertical space
          br(),
          ######################################################################################################### #
          
       ), 
       
       ## PLOT FULL RANGE OF SCORES /TAB ####
       
       tabPanel('Plot Full Range of Scores',
                ### _HISTOGRAM 
                
                #h3(id = 'histogram',"Explore Indicator Distributions"),
                h4('About this Chart'),
                helpText('This chart shows the spread of indicator values across all locations in your analysis.'),
                
                wellPanel(
                  style='width: 100%;',
                  ## row of histogram settings
                  
                  fluidRow(
                    
                    column(2, 
                           ## input: Histogram settings - distribution across sites or people
                           radioButtons(inputId = 'summ_hist_distn',
                                        label = h5('Distribution across sites or people (pop.wtd.)'),
                                        choices = c('Sites', 'People'), selected = 'People' ),
                           
                           ## input; Histogram settings - data type
                           radioButtons(inputId = 'summ_hist_data', label = h5('Data type'),
                                        choiceNames = c('Percentile of US', 'Raw data'),
                                        choiceValues = c('pctile',          'raw')),
                           
                           ## input: Histogram settings - number of bins
                           sliderInput(inputId = 'summ_hist_bins', label = h5('Bins'),
                                       min = 5, max = 50, step = 5, value = 10),
                    ),
                    column(10, align='center',
                           ## output: display histogram
                           shinycssloaders::withSpinner(
                             plotOutput(outputId = 'summ_display_hist') 
                           ),
                           
                           fluidRow(
                             column(6, offset=3,
                                    ## input: indicator dropdown for histogram
                                    selectInput('summ_hist_ind', label = 'Choose indicator',
                                                choices = setNames(c(EJAM::names_d,
                                                                     EJAM::names_d_subgroups,
                                                                     EJAM::names_e, 
                                                                     EJAM::names_ej_pctile, names_ej_supp_pctile, names_ej_state_pctile, names_ej_supp_state_pctile),
                                                                   c(EJAM::names_d_friendly,
                                                                     EJAM::names_d_subgroups_pctile_friendly, names_ej_supp_pctile_friendly, names_ej_state_pctile_friendly, names_ej_supp_state_pctile_friendly,
                                                                     EJAM::names_e_friendly, 
                                                                     EJAM::names_ej_friendly
                                                                   ))
                                    )
                             )
                           )
                    ) #end column with hist 
                  ) #end fluidrow
                  
                ) # end wellpanel
       )
                                          )
                                      )
       
                             ), # end Tabular results tab
       
       ######################################################################################################### #
       # . ####
       
       # FULL REPORT - TAB  ------------- ####
       
       tabPanel(title = 'Written Report',
                
                #  MAKE SURE all parameter names are used (identical names, & all are there) in these 4 places: 
                #  1. input$ ids in app_ui.R, from user, to customize the long report
                #  2. params$ list passed by app_server.R to render the Rmd doc
                #  3. params: accepted in  .Rmd yaml info header 
                #  4. params$  as used within body of  .Rmd text inline and in r code blocks.
                
                br(), ## vertical space
                
                wellPanel(       
                  br(), ## vertical space
                  
                  ## arrange text and buttons
                  fluidRow(
                    column(6,
                           ## add text above report settings
                           p('Edit report settings below to tailor the full report to your specific analysis.')
                    ),
                    column(6,
                           ## output: button to download static report
                           shiny::downloadButton(outputId = 'rg_download', 
                                                 label = 'Download report',
                                                 class='usa-button'),
                           
                           ## button to launch modal with outline of report
                           ## this could be added throughout the page to show where different text components would be included
                           ## I DONT THINK THE OUTLINE LOOKS GREAT AND IS HARD TO KEEP IN SYNC WITH ACTUAL RMD DOC OUTLINE
                           # actionButton(inputId = 'show_outline', label = 'Show Report Outline',
                           #              style = 'color: #fff; background-color: #005ea2;'),
                    )
                  ), ######################################################### # 
                  
                  #------- WHERE was analyzed? (where/ what sector/zones/types of places)
                  
                  #?  # analysis_title =  input$analysis_title,
                  # zonetype =  input$rg_zonetype,   ### names differ by   rg_
                  # where = input$rg_enter_miles,   ############# names differ
                  # distance = paste0(input$bt_rad_buff,' miles'), #input$radius_units),   #############  param derived from input
                  # sectorname_short = input$rg_enter_sites,                 ############# names differ
                  # ## allow for either or
                  # in_the_x_zone = ifelse(nchar(input$in_the_x_zone_enter) > 0,     ######  _enter  and derived from inputs
                  #                        input$in_the_x_zone_enter,
                  #                        input$in_the_x_zone),
                  # facilities_studied = ifelse(nchar(input$facilities_studied_enter) > 0,    ####   _enter and derived from inputs
                  #                             input$facilities_studied_enter,
                  #                             input$facilities_studied),
                  # within_x_miles_of = paste0("within ", paste0(input$bt_rad_buff,' miles'), " of"),   ##### param derived from input
                  # 
                  # in_areas_where = paste0(input$in_areas_where, ' ', input$in_areas_where_enter),   ######   _enter
                  # risks_are_x = input$risks_are_x,                      ### names match
                  # source_of_latlons = input$source_of_latlons,          ### names match
                  # sitecount = nrow(data_processed()$results_bysite),      ### param derived from data
                  
                  # put input$analysis_title   here??? 
                  
                  fluidRow(          #    param is called  where
                    column(4,
                           ## input: analysis location - uses current value of radius slider      
                           uiOutput('rg_enter_miles')
                    )),
                  
                  # param distance is based on input$bt_rad_buff
                  
                  fluidRow(     
                    column(4,
                           ## input:  - which sites analyzed  #    param is called   sectorname_short 
                           textInput(inputId = "rg_enter_sites", 
                                     label = "Describe sites analyzed:", 
                                     value = "facilities in the _____ source category"),
                    )
                  ),
                  
                  fluidRow(
                    column(4,
                           ## input:   # zonetype =  input$rg_zonetype
                           selectInput(inputId = 'rg_zonetype', 
                                       label = 'Zone Type (How are zones defined?)',
                                       choices = c('General' = 'zone_is_named_x','Proximity'= 'zone_is_nearby',
                                                   'Risk' = 'zone_is_risk_x'))
                    ),
                    column(4,
                           ## input:   #  based on  input$bt_rad_buff
                           selectInput(inputId = 'within_x_miles_of', 
                                       label = 'Near to',
                                       choices = c('near the','nearby',''))
                    )
                  ),
                  
                  fluidRow(
                    column(4,
                           ## input:    # in_areas_where calculated from input$in_areas_where, and input$in_areas_where_enter
                           selectInput(inputId = 'in_areas_where', 
                                       label = 'Describe the surrounding area',
                                       choices = c('in areas with',
                                                   'where','in block groups where')
                           )
                    ),
                    column(4,
                           ## input: 
                           textInput(inputId = 'in_areas_where_enter', 
                                     label = 'Add area details', 
                                     value = '')
                    )
                  ),
                  fluidRow(
                    column(8,
                           ## input:  
                           selectInput(inputId = 'risks_are_x', 
                                       label = 'Risk level',
                                       choices = c("risk is at or above 1 per million (lifetime individual cancer risk due to inhalation of air toxics from this source category)",
                                                   "risk is above 1 per million",
                                                   "the area is in nonattainment",
                                                   "PM2.5 levels are in the highest decile",
                                                   "ozone concentrations are at least 70 ppb")
                           )
                    )
                  ),
                  fluidRow(
                    column(4,
                           ## input:  
                           selectInput(inputId = 'in_the_x_zone', 
                                       label = 'General study location',
                                       choices = c('in the study area' = 'area', 'in the analyzed locations' = 'locs',
                                                   'in [State X] (specify)' = 'state', 
                                                   'in EPA Region [XX] (specify)' = 'region')
                           )
                    ),
                    column(4,
                           ## add free text box if certain values chosen from radio button
                           conditionalPanel(
                             condition = "input.in_the_x_zone == 'state' || input.in_the_x_zone == 'region'",
                             textInput(inputId = 'in_the_x_zone_enter', 
                                       label = 'Other - please specify',
                                       value = 'in ')
                           )
                    )
                  ),
                  
                  fluidRow(
                    column(4,
                           ## input:  
                           selectInput(inputId = 'facilities_studied', 
                                       label = 'Facilities Studied',
                                       choices = c('facilities subject to this proposed rule' = 'rule',
                                                   'analyzed facilities' = 'fac','analyzed sites' = 'sites',
                                                   'facilities in the xxxx source category' = 'cat',
                                                   'facilities in the xxxx sector (NAICS code xxxx)' = 'sector')
                           )
                    ), 
                    column(4,
                           ## add free text box if certain values chosen 
                           conditionalPanel(
                             condition = "input.facilities_studied == 'cat' || input.facilities_studied == 'sector' || input.facilities_studied == 'rule'",
                             textInput(inputId = 'facilities_studied_enter', 
                                       label = 'Other - please specify')
                           )
                    )
                  ), 
                  
                  fluidRow(
                    column(4,
                           ## input:  
                           textInput(inputId = 'source_of_latlons', 
                                     label = 'Source of Points',
                                     placeholder = "EPA's Facility Registry Service (FRS)"),
                    )
                  ),
                  
                  
                  #------- METHODS, AUTHORS, ETC.
                  
                  # authorname1: "The US EPA"
                  # authoremail1: ""
                  # coauthor_names: NA
                  # coauthor_emails: NA
                  # fundingsource: NA
                  # acs_version: "2017-2021"
                  # ejscreen_version: "2.2"
                  
                  fluidRow(
                    column(2, 
                           ## input:  
                           textInput(inputId = "rg_author_name", 
                                     label = "Author Name(s):", 
                                     value = "FirstName LastName")
                    ),
                    column(2,
                           ## input:  
                           textInput(inputId = "rg_author_email", 
                                     label = "Author Email(s):", 
                                     value = "author@email.org")
                    ),
                    column(2,
                           ## input: checkbox to add line for coauthor information
                           checkboxInput(inputId = 'rg_add_coauthors',
                                         label = 'Add co-authors?',
                                         value = FALSE)                            
                    ) 
                  ),   
                  ## if checkbox is checked, add textinputs for co-author name and email
                  conditionalPanel(
                    condition = 'input.rg_add_coauthors == 1',
                    fluidRow(
                      column(2, 
                             ## input: 
                             textInput(inputId = 'coauthor_names', 'Co-Author Name(s)')
                      ), 
                      column(2,
                             ## input:  
                             textInput(inputId = 'coauthor_emails', 'Co-Author Email(s)')
                      )
                    )
                  ),
                  fluidRow(
                    ## input: 
                    textInput(inputId = 'fundingsource', 
                              label = 'Funding Source',
                              placeholder = "The Inflation Reduction Act (for example)"),
                    ## input:  
                    textInput(inputId = 'acs_version', 
                              label = 'Version of ACS data (years)',
                              placeholder = "2017-2021"),
                    ## input:  
                    textInput(inputId = 'ejscreen_version', 
                              label = 'Version of EJScreen',
                              placeholder = "2.2") 
                  ),
                  ############################ # 
                  
                  #------- RESULTS (tables and map and plots)
                  
                  # total_pop: NA
                  # results: NA
                  # results_formatted: NA
                  # map: NA
                  # map_placeholder_png:                 "map_placeholder.png"
                  # envt_table: NA
                  # envt_table_placeholder_png:   "envt_table_placeholder.png"
                  # envt_table_placeholder_rda:   "envt_table_placeholder.rda"
                  # demog_table: NA
                  # demog_table_placeholder_png: "demog_table_placeholder.png"
                  # demog_table_placeholder_rda: "demog_table_placeholder.rda"
                  # boxplot: NA
                  # boxplot_placeholder_png:         "boxplot_placeholder.png"
                  # barplot: NA
                  # barplot_placeholder_png:         "barplot_placeholder.png"
                  # 
                  
                  
                  #------- TEXT PHRASES DESCRIBING AND INTERPRETING RESULT 
                  
                  # demog_how_elevated: NA
                  # envt_how_elevated: NA
                  # demog_high_at_what_share_of_sites: NA
                  # envt_high_at_what_share_of_sites: NA
                  # conclusion1: NA
                  # conclusion2: NA
                  # conclusion3: NA
                  
                  fluidRow(
                    column(4,
                           ## input:  
                           textInput(inputId = 'demog_how_elevated', 
                                     label = 'Elevation of Demographic Indicators',
                                     placeholder = 'moderately elevated'),
                    ),
                    column(4,
                           ## input:  
                           textInput(inputId = 'envt_how_elevated', 
                                     label = 'Elevation of Environmental Indicators',
                                     placeholder = 'moderately elevated'),
                    )
                  ),
                  fluidRow(
                    column(4,
                           ## input:  
                           selectInput(inputId = 'demog_high_at_what_share_of_sites',
                                       label = 'Demographic indicators high at what share of sites?',
                                       choices = c('a surprisingly large share of these sites',
                                                   'some of these sites, just as it varies nationwide',
                                                   'a relatively small share of these sites'),
                                       selected = 'some of these sites, just as it varies nationwide'),
                    ), 
                    column(4,
                           ## input:  
                           selectInput(inputId = 'envt_high_at_what_share_of_sites',
                                       label = 'Environmental indicators high at what share of sites?',
                                       choices = c('a surprisingly large share of these sites',
                                                   'some of these sites, just as it varies nationwide',
                                                   'a relatively small share of these sites'),
                                       selected = 'some of these sites, just as it varies nationwide'),
                    )
                  ),
                  fluidRow(
                    column(8,
                           ## input: conclusion 1 -  
                           textAreaInput(inputId = 'conclusion1',
                                         label = 'Conclusion 1',
                                         placeholder = "The people living near these sites are 40% more likely to be in Limited-English Households than the average US resident. (for example)"
                           )
                    )
                  ),
                  fluidRow(
                    column(8,
                           ## input: conclusion 2-  
                           textAreaInput(inputId = 'conclusion2', 
                                         label = 'Conclusion 2',
                                         placeholder = "The % low income among these residents is 2.4 times the rate in the US overall. (for example)")
                    )
                  ),
                  fluidRow(
                    column(8,
                           ## input: conclusion 3 -  
                           textAreaInput(inputId = 'conclusion3', 
                                         label = 'Conclusion 3',
                                         placeholder = "The average resident near these sites is 1.5 times as likely to be Hispanic as the average person in their State overall. (for example)")
                    )
                  ),
                ) # end wellpanel
       ), # end written report  tab
       # ______ ####
       
       ######################################################################################################### #
       # ~ ####
       # ADVANCED SETTINGS - TAB  ####
       
       # - hidden by default but can be activated by a button (see About EJAM tab)
       tabPanel(title = 'Advanced Settings',
                
                ## Get ejscreen reports via API ??? ####
                mod_ejscreenapi_ui("ejscreenapi_1"),
                
                radioButtons("testing", "TESTING APP? testing= ", choices = c(Yes=TRUE, No=FALSE),
                             inline = TRUE,
                             selected=default_testing),
                
                
                ## input: Type of plot for 1page report
                shiny::radioButtons(inputId = "plotkind_1pager", 
                                    label = "Type of plot for 1page report",
                                    choices = list(Bar="bar", Box="box", Ridgeline="ridgeline"), 
                                    selected = "bar"),
                
                ## _radio button on format of short report ####
                # DISABLED UNTIL PDF KNITTING IS DEBUGGED
                radioButtons("format1pager", "Format", choices = c(html="html", html="pdf"), inline = TRUE),
                
                numericInput(inputId = 'maxradius',
                             label = 'Max distance in miles to search for closest single block if site has none within normal radius',
                             value =  default_maxradius,  # 50000 / meters_per_mile, # 31.06856 miles !!
                             min = 0, max = 50000 / meters_per_mile, step = 1), 
                
                radioButtons(inputId = "avoidorphans", 
                             label =  "Avoid orphans (by searching for nearest one out to maxradius, instead of reporting NA when no block is within radius)", 
                             choices = c(Yes = TRUE, No = FALSE), 
                             inline = TRUE,
                             selected = default_avoidorphans),
                
                ## input: Name for 1st set of comparisons
                shiny::textInput(inputId = 'an_name_comp1', 
                                 label='Name for 1st set of comparisons',
                                 ## this will need to be changed later
                                 value = '',
                                 placeholder = threshgroup.default['comp1']
                ),
                
                ## input: Threshold value(s) for 1st set of comparisons
                numericInput(inputId = 'an_thresh_comp1', 
                             label='Threshold value(s) for 1st set of comparisons (e.g. %ile 1-100):', 
                             value = threshold.default['comp1']
                ),
                
                ## input: Name for 1st set of comparisons
                shiny::textInput(inputId = 'an_name_comp2', 
                                 label='Name for 2nd set of comparisons',
                                 ## this will need to be changed later
                                 value = '',
                                 placeholder = threshgroup.default['comp2']
                ),
                
                ## input: Threshold value(s) for 2nd set of comparisons
                numericInput(inputId = 'an_thresh_comp2', 
                             label='Threshold value(s) for 2nd set of comparisons (e.g. %ile 1-100):', 
                             value = threshold.default['comp2']
                ),
                br(), ## vertical space
                
                ## input: upload batch buffer output - standard report stats
                shiny::fileInput(inputId = 'bt_upload_adj',
                                 label = 'Upload batch buffer output - with standard report stats',
                                 accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                ),
                
                ## input: upload batch buffer output - adjusted for double counting
                shiny::fileInput(inputId = 'bt_upload_std',
                                 label = 'Upload batch buffer output - adjusted for double counting',
                                 accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                ),
                
                
                ## input: Limit to facilities where selected NAICS is found w/in EPA list
                shiny::checkboxGroupInput(
                  inputId = "ss_limit_fac1", 
                  label = "Limit to facilities where selected NAICS is found within these EPA lists: (all are searched by default)",
                  choices  = epa_programs,
                  selected = epa_programs,
                  inline = TRUE
                ),
                
                ## input: Limit to facilities in these EPA programs
                shiny::checkboxGroupInput(
                  inputId = "ss_limit_fac2",
                  label = "Limit to facilities on these EPA lists (all included by default)",
                  choices  = epa_programs,
                  selected = epa_programs,
                  inline = TRUE
                ),
                
                ## input:  
                shiny::selectInput('subgroups_type', 
                                   #    "nh" for non-hispanic race subgroups as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh; 
                                   #    "alone" for EJScreen v2.2 style race subgroups as in    White Alone, wa and others in names_d_subgroups_alone; 
                                   #    "both" for both versions. 
                                   label = "Which definition of demographic race ethnicity subgroups to include?",
                                   choices = list(NonHispanicAlone = 'nh', Alone = 'alone', Both = 'both'),
                                   selected = default_subgroups_type),
                shiny::radioButtons(inputId = "need_proximityscore", 
                                    label = "Need proximity score?",
                                    choices = list(Yes=TRUE, No=FALSE ), 
                                    selected = default_need_proximityscore),
                shiny::radioButtons(inputId = "include_ejindexes", 
                                    label = "Need EJ Indexes",
                                    choices = list(Yes=TRUE, No=FALSE ), 
                                    selected = default_include_ejindexes),
                shiny::radioButtons(inputId = "more3", 
                                    label = "more3",
                                    choices = list(A="a", B="b", C="c"), 
                                    selected = "a")  # ,
                
                
       ) # end Advanced Settings tab ####
       ################################################################################ #
       
       
       
                 ) # >>>>end of tabset panel results_tabs ^^^^^^^^^^^^^^^^^  ####
       
        ),  # >>>>end of tab panel See Results ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ####
       
       
       
       
       
      ), # end tabset panel from line 37 or so ####
      
      ## add HTML footer - defined in global.R
      html_footer_fmt
    ) ## end fluidPage
  ) # end tag list
} # end app_ui

########################################################################### #
# ~ ####
# ___________ App UI ends here ________ ####

#' Add external Resources to App (from golem package code)
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {   # (adds external Resources to App) ####
  golem::add_resource_path(
    "www",
    app_sys("app/www") #   points to  EJAM/inst/app/www  actually, not EJAM/www nor EJAM/app/www
  )
  tags$head(
    
    ### insert this in head of index.html (or use tags$link() as below) to make all favicon versions work
    ### using this set of icons for various platforms/sizes: 
    #
    # <link rel="shortcut icon"    href="/inst/www/favicon.png"> # this is the only one set up by golem::favicon() and was .ico in the example notes but png is bigger higher res here
    #
    # <link rel="apple-touch-icon"                 sizes="180x180" href="/inst/www/apple-touch-icon.png">
    # <link rel="icon"            type="image/png" sizes="32x32"   href="/inst/www/favicon-32x32.png">
    # <link rel="icon"            type="image/png" sizes="16x16"   href="/inst/www/favicon-16x16.png">
    # <link rel="manifest"                                         href="/inst/www/site.webmanifest">
    # <link rel="mask-icon"                                        href="/inst/www/safari-pinned-tab.svg"  color="#5bbad5">
    #
    # <meta name="msapplication-TileColor"  content="#2d89ef">
    # <meta name="msapplication-config"     content="/inst/www/browserconfig.xml">
    # <meta name="theme-color"              content="#ffffff">
    
    golem::favicon(ext = 'png'), # but see note on favicons set 
    
    tags$head(tags$link(rel="apple-touch-icon",                sizes="180x180", href="/inst/www/apple-touch-icon.png")),
    tags$head(tags$link(rel="icon",           type="image/png",sizes="32x32" ,  href="/inst/www/favicon-32x32.png")),
    tags$head(tags$link(rel="icon",           type="image/png",sizes="16x16" ,  href="/inst/www/favicon-16x16.png")),
    tags$head(tags$link(rel="manifest",                                         href="/inst/www/site.webmanifest")),
    tags$head(tags$link(rel="mask-icon" ,                                       href="/inst/www/safari-pinned-tab.svg",  color="#5bbad5")),
    tags$meta(name = "msapplication-TileColor",  content="#2d89ef"),
    tags$meta(name="msapplication-config",     content="/inst/www/browserconfig.xml"),
    tags$meta(name="theme-color",              content="#ffffff"),
    
    # this specifies app title
    golem::bundle_resources(
      path = app_sys("app/www"),  #   points to  EJAM/inst/app/www  actually, not EJAM/www nor EJAM/app/www
      app_title = "EJAM"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    
  )
}
########################################################################### #
