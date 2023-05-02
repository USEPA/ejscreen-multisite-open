#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui  <- function(request) {
  tagList(
    # golem_add_external_resources() ####
    # Leave this function for adding external resources, see end of this source file.
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
      tags$style(HTML("
        .tabbable > .nav > li[class=active] > a {
           background-color: #005ea2;
           font-weight: bold;
        }")),
      
      ### html header inserted from global.R ####
      html_header_fmt,
      
      ### title (for app and browser tab) ####
      titlePanel(title = "EJAM (Environmental Justice Analysis Multi-site) Tool",
                 windowTitle = "EJAM (Environmental Justice Analysis Multi-site) Tool"
      ),
      
      ## create tabsetPanel with tabs for different sections
      tabsetPanel(
        id = 'all_tabs',
        type = 'pills',
        selected = 'Site Selection',
        # ~ ####
        # ABOUT ####
        tabPanel(title = 'About EJAM',
                 
                 br(), ## vertical space
                 
                 ## html intro text from global.R  
                 intro_text,
                 
                 ## button to reveal Advanced Settings tab 
                 actionButton('ui_show_advanced_settings','Show Advanced Settings Tab', 
                              style ='color: #fff; background-color: #005ea2;'),
                 actionButton('ui_hide_advanced_settings','Hide Advanced Settings Tab', 
                              style ='color: #fff; background-color: #005ea2;')
        ),
        ######################################################################################################### #
        # ~ ####
        # SITE SELECTION   ####
        
        tabPanel(title = 'Site Selection',
                 
                 br(), ## vertical space
                 br(),
                 
                 ## container for upload method (left column) and map (right column)
                 fluidRow(
                   
                   ## upload method column
                   column(5,
                          h3('Specify Locations to Analyze', style='text-align: center;'),
                          
                          hr(), ## horizontal line
                          
                          ## input: selecting upload method
                          radioButtons(inputId = 'ss_choose_method',
                                       label = 'How would you like to identify facilities?',
                                       choiceValues = c('latlon', 
                                                        'NAICS',
                                                        'FRS',
                                                        'ECHO'),
                                       choiceNames = c('Upload Location (latitude/longitude) file',
                                                       'Select by Industry (NAICS) Code',
                                                       'Upload EPA Facility ID (FRS Identifers) file',
                                                       'Search using ECHO database'),
                                       width = '400px'),
                          
                          ## latlon conditional panel
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'latlon'",
                            
                            ## input: Upload list of facility lat/longs
                            fileInput(inputId = 'ss_upload_latlon',  
                                      label = 'Upload a list of sites in a spreadsheet (.csv, .xls, or .xlsx with lat & lon as table headers)',
                                      #placeholder = 'test_input_latlon.csv', 
                                      multiple = FALSE,
                                      accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")
                                      # add hover tips here maybe, or even a button to view examples of valid formats and details on that.
                            ),
                          ), # end latlong conditionalPanel
                          
                          ## NAICS conditional panel
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'NAICS'",
                            
                            radioButtons('naics_ul_type', 'Choose how to enter NAICS codes',
                                         choiceNames = c('Select codes from dropdown' ,
                                                         'Enter text or code to search'),
                                         choiceValues = c('dropdown',
                                                          'enter')
                            ), 
                            radioButtons('add_naics_subcategories', "Add all subcategories of NAICS?",
                                         choiceNames = c("Yes","No"),
                                         choiceValues = c(TRUE,FALSE),
                                         selected = TRUE),
                            
                            ### conditional sub- panel if entering naics
                            conditionalPanel(
                              condition = "input.naics_ul_type == 'enter'",
                              ## input: Enter NAICS code manually       
                              textInput(
                                inputId = "ss_enter_naics",
                                label = htmltools::h6(
                                  "Enter Industry NAICS codes - ",
                                  HTML(paste0('<a href=\"', 'https://www.census.gov/naics', '\", target=\"_blank\">', 'Look up NAICS', '</a>')),
                                  # htmltools::a("Look up NAICS", href ="https://www.census.gov/naics", ),
                                  htmltools::a(htmltools::img(id = "ibutton",src = "www/i.png",height = 15,width = 15),
                                               href = "www/ibutton_help.html#help_naicslist",target = "_blank")
                                ),
                                value = "",
                                width = 400,
                                placeholder = NULL
                              )
                            ), # end manually entry of NAICS sub- conditionalPanel
                            
                            ### conditional sub- panel if using NAICS dropdown
                            conditionalPanel(
                              condition = "input.naics_ul_type == 'dropdown'",
                              ## input: Select NAICS from list
                              selectInput(
                                inputId = "ss_select_naics",
                                label = htmltools::h6("Select industry of interest"),
                                # choose from named numeric vector on server-side
                                ## number is NAICS like 31182, names are like "31182 - Cookie, Cracker, and Pasta Manufacturing" 
                                choices = NULL, 
                                selected = NULL,
                                width = 400,
                                multiple = TRUE
                              )#, # xxx
                            ),  # end dropdown NAICS sub- conditionalPanel
                            
                            br(), ## vertical space
                            
                            ## input: button to submit NAICS codes that were entered/selected
                            actionButton(inputId = 'submit_naics', label = 'Submit NAICS / Find Facilities',
                                         style = 'color: #fff; background-color: #005ea2;')#, # xxx
                          ), # end NAICS conditionalPanel overall
                          
                          ## FRS conditional panel
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'FRS'",
                            ## input: Upload list of FRS identifiers
                            shiny::fileInput(
                              inputId = 'ss_upload_frs',
                              label = 'Upload a file with FRS identifiers',
                              accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                            )#, # xxx
                          ), # end FRS conditionalPanel
                          
                          ## ECHO upload conditional panel
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'ECHO'",
                            
                            br(), ## vertical space
                            
                            ## input: Upload list of ECHO facilities
                            shiny::fileInput(
                              inputId = 'ss_upload_echo',
                              label = 'Upload list of ECHO facilities',
                              accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                            ), 
                            #br(),
                          ), #end ECHO conditional panel
                          
                          hr(), ## horizontal line
                          
                          h4('Processing uploaded data'),
                          
                          ## arrange title box and run button
                          fluidRow(
                            column(8, 
                                   ## input: title for analysis (goes in report header)
                                   shiny::textInput('analysis_title', 
                                                    label = 'Name of Your Analysis',
                                                    placeholder = 'EJ Analysis of My List of Places',
                                                    value = 'EJ Analysis of My List of Places'),
                            ), 
                            column(4, style='padding: 10px',
                                   ## input: run analysis button     
                                   shiny::actionButton(inputId = 'bt_get_results', 
                                                       label = 'Start Analysis',
                                                       ## extra formatting - optional
                                                       style = 'color: #fff; background-color: #005ea2; height: 50px; width: 100px; border-radius: 5%;')
                            )
                          )
                   ), #end upload column 
                   
                   ## map column
                   column(7,
                          ## add vertical line between columns
                          style = 'border-left: 1px solid;',
                          
                          # h3('Review selected sites & Pick a distance', style='text-align: center;'),
                          
                          hr(), ## horizontal line
                          
                          ## arrange summary text and button to view uploaded data
                          fluidPage(
                            column(8,
                                   ## output: display number of uploaded sites
                                   htmlOutput(outputId = 'an_map_text')#, # xxx
                            ),
                            
                            column(4,
                                   ## add button and modal to show uploaded data interactively
                                   actionButton('show_data_preview', label = 'Review selected sites',
                                                style = 'color: #fff; background-color: #005ea2;'),
                                   
                                   ## on button click, show modal with DT table of uploaded
                                   shinyBS::bsModal(id = 'view_data_modal', 
                                                    title = 'Uploaded data', 
                                                    trigger = 'show_data_preview',
                                                    size = 'large',
                                                    DT::DTOutput('print_test2_dt', width = '100%'))
                            )
                          ),
                          
                          ## output: show leaflet map of uploaded points
                          shinycssloaders::withSpinner(
                            leaflet::leafletOutput(outputId = 'an_leaf_map', 
                                                   height = '500px', 
                                                   width = '100%')
                          ),
                          
                          ## arrange map options above or below map
                          fluidRow(
                            column(8,
                                   align = 'center',
                                   ## input: Specify radius of circular buffer       
                                   shiny::sliderInput(inputId = 'bt_rad_buff',
                                                      ## label is updated in server
                                                      label = 'Within what distance of a site?',
                                                      #label = htmltools::h5("Radius of circular buffer in miles"),
                                                      value = 1.0, step = 0.25,
                                                      min = 0.25, max = 10,
                                                      post = ' miles'
                                   )#,  # xxx
                            ),
                            column(4,
                                   ## input: highlight clusters on map? yes/no
                                   shiny::checkboxInput(inputId = 'an_map_clusters', 
                                                        label = 'Highlight\n overlaps?'
                                   )
                            )
                            ## not used currently - button to switch units and update slider
                            ## input: switch units to km for radius slider
                            # radioButtons(inputId = 'radius_units',
                            #              label = 'Choose units for radius',
                            #              choices = c('miles','kilometers'),
                            #              selected = 'miles')
                            
                            # uiOutput('bt_rad_buff')
                          )
                   ) #end map column
                 ), #end fluidRow for top part of page
                 
                 ## add and format tooltip to dropdown for additional info
                 tags$style(HTML("
                .tooltip > .tooltip-inner {
                background-color: #005ea2;
                }
                ")
                 ), 
                shinyBS::bsTooltip(id = 'ss_choose_method', title = 'Please read the upload instructions given below.',
                                   placement = 'right', trigger = 'hover'),
                
                hr(), ## horizontal line
                
                ## conditional Panels to show help pages for each selection method
                
                ## NAICS help page
                conditionalPanel(
                  condition = "input.ss_choose_method == 'NAICS'",
                  shinyBS::bsCollapse(
                    id = 'naics_help', 
                    open = 'Read more about NAICS',
                    shinyBS::bsCollapsePanel(title = 'Read more about NAICS',
                                             style = 'primary',
                                             h3('Select industry'),
                                             helpText('You may define your universe of interest by selecting specific industries that you wish to query.'),
                                             htmltools::a('NAICS definitions at Census', href='https://www.census.gov/naics', target='_blank')
                    )
                  )
                ),
                ## FRS help page
                conditionalPanel(
                  condition = "input.ss_choose_method == 'FRS'",
                  shinyBS::bsCollapse(
                    id = 'frs_help', open = 'FRS file upload instructions',
                    shinyBS::bsCollapsePanel(title = 'FRS file upload instructions',
                                             style = 'primary',
                                             ## FRS help text - in global.R
                                             frs_help_msg
                    )
                  )
                ),
                ## latlon help page
                conditionalPanel(
                  condition = "input.ss_choose_method == 'latlon'",
                  ## read more about latlon 
                  shinyBS::bsCollapse(
                    id = 'latlon_help', open = 'Location file upload instructions',
                    shinyBS::bsCollapsePanel(title = 'Location file upload instructions',
                                             style = 'primary',
                                             HTML(latlon_help_msg)
                    )
                  )
                ),
                ## ECHO help page
                conditionalPanel(
                  condition = "input.ss_choose_method == 'ECHO'",
                  ## collapsible ECHO help panel 
                  shinyBS::bsCollapse(
                    id = 'echo_help', open = 'ECHO upload instructions',
                    
                    shinyBS::bsCollapsePanel(title = 'ECHO upload instructions',
                                             style = 'primary',
                                             ## echo help text - in global.R
                                             echo_message)
                  )
                ) #, # xxx #end ECHO conditionalPanel
        ), # end Site Selection tab
        
        ######################################################################################################### #
        # ~ ####
        # SHORT REPORT ####
        # - similar to EJSCREEN standard report  
        # | ####
        
        tabPanel(title = 'Summary Report',
                 ## _Header, pop count, etc. *********************************####
                 
                 br(), ## vertical space
                 
                 ## button to trigger (re-)generation of summary report using eventReactive
                 ## otherwise, it will update when any of the inputs are changed
                 #actionButton('gen_summary_report', 'Generate Report'),
                 
                 # 
                 # shiny::radioButtons(inputId = 'state_or_us_1pager',
                 #                     label = 'Plot State or US Percentiles?',
                 #                     choices = c(State='state', National='usa')),
                 
                 ## display rendered report as HTML in the app
                 shinycssloaders::withSpinner(
                  uiOutput('rendered_summary_report')
                 ),
                 
                
                 ## _button to download short report ####
                 
                 tags$div(
                   shiny::downloadButton(outputId = 'summary_download', 
                                         label = 'Download Summary Report',
                                         style = 'color: #fff; background-color: #005ea2'),
                   style = 'text-align: center;'
                 ) , # ,
                 ## _radio button on format of short report ####
                 # DISABLED UNTIL PDF KNITTING IS DEBUGGED
                 radioButtons("format1pager", "Format", choices = c(html="html", html="pdf"), inline = TRUE)  # fix 
        ),
        
        ######################################################################################################### #
        # ~ ####
        # OVERALL RESULTS TALL FORMAT ####
        ###   THIS WAS MEANT TO BE Another way to quickly see all the indicators, in one long list. 
        # This could be kept here (but look better) if removed from the site by site tab, to keep that table focused on 1 row/site.
        # not sure if is useful enough to keep. they already appear in the "site by site plus overall" tab, and in excel download.
        # tabPanel(title = 'Results Overall',
        # 
        #          br(), ## vertical space
        # 
        #          h3('Overall Results (avg person'),
        # 
        #          DTOutput("overall_results_tall", height="100%")
        # ),
        
        ######################################################################################################### #
        # ~ ####
        # SITE BY SITE (AND OVERALL) TABLE   ####
        # | ####
        tabPanel(title = 'Table of Results',
                 br(), 
                 
                 ############################################################################### # 
                 ###   THIS WAS MEANT TO BE A TINY BIT OF EXECUTIVE-SUMMARY-STYLE TEXT BUT INTERACTIVE
                 ###    IT DID NOT LOOK GREAT WHERE IT WAS, BUT THE IDEA COULD BE REIMPLEMENTED SOMEWHERE
                 # h3('Key Indicators'),
                 # wellPanel(
                 ############ # 
                 #  ## _input: Demog. indicator (dropdown) to use in summary text ####
                 #   selectInput(
                 #     'key_ind_d', label = 'Choose a demographic indicator',
                 #     choices = setNames(c(names_d, names_d_subgroups), c(names_d_friendly, names_d_subgroups_friendly))
                 #   ),
                 #   ## _output: Demog. Exec. Summary Text ####
                 #   shinycssloaders::withSpinner(htmlOutput('exec_summ_d')),
                 #   br(), br(), 
                 ############ # 
                 #   ## _input: Envt. indicator (dropdown) to use in summary text ####
                 #   selectInput(
                 #     'key_ind_e', label = 'Choose an environmental indicator',
                 #     choices = setNames(names_e, names_e_friendly)
                 #   ),
                 #   ## _output: Envt. Exec. Summary Text ####
                 #   shinycssloaders::withSpinner(htmlOutput('exec_summ_e'))
                 ############ # 
                 # ), br(),
                 ############################################################################### # 
                 
                 ## _button: Excel Download ####
                 fluidRow(
                   column(6,
                          h3('Site-by-Site Table'),
                   ),
                   column(6,
                          ## button to download excel table of results - uses xls_formatting2
                          downloadButton('download_results_table', 'Download Results Table',
                                         style = 'color: #fff; background-color: #005ea2;')
                   )
                 ),
                 br(), ## vertical space
                 
                 ## _output: Interactive Table of Sites ####
                 shinycssloaders::withSpinner(
                   DT::DTOutput(outputId = 'view3_table', width = '100%')
                 ),
                 ## _output: Map 1 site selected from table ####
                 # shinycssloaders::withSpinner(
                 #   leaflet::leafletOutput(outputId = 'v3_sitemap')
                 # ),
        ), # end Tabular results tab
        ######################################################################################################### #
        # ~ ####
        
        # GRAPHICS  (barplots, histograms) ####
        
        tabPanel(title = 'Graphical Results',
                 h3('Compare Across Indicators'),
                 
                 wellPanel(
                   
                   ## _BARPLOT ####
                   
                   fluidRow(
                     ## input: Barplot setting - indicator type
                     column(2,  
                            radioButtons(inputId = 'summ_bar_ind', 
                                         label = h5('Indicator type'), 
                                         choices = c('Demographic', 'Environmental','EJ'), selected = "Environmental"),
                            ## input: Barplot setting - data type
                            radioButtons(inputId = 'summ_bar_data', label = 'Data Type', 
                                         choiceValues = c('ratio',      'raw'),      # no 'pctile' at this time
                                         choiceNames  = c('Ratio to US','Raw data'), # no 'Percentile of population' at this time
                                         selected = 'ratio'), 
                            
                            ## hiding this option for now - defaulting to Average
                            ## input: Barplot setting - statistic type
                            # radioButtons(inputId = 'summ_bar_stat', 'Statistic', 
                            #              choiceValues = c('avg', 'med'),
                            #              choiceNames = c('Average', 'Median'))
                     ),
                     
                     column(10, align='center',
                            shinycssloaders::withSpinner(
                              ## output: display barplot
                              plotOutput(outputId = 'summ_display_bar', height='600px')
                            )
                     )
                   )
                 ),
                 
                 br(), ## vertical space
                 br(),
                 ######################################################################################################### #
                 
                 ## _HISTOGRAM ####
                 
                 h3("Explore Indicator Distributions"),
                 
                 wellPanel(
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
                                                                      EJAM::names_ej),
                                                                    c(EJAM::names_d_friendly,
                                                                      EJAM::names_d_subgroups_friendly,
                                                                      EJAM::names_e_friendly, 
                                                                      EJAM::names_ej_friendly
                                                                    ))
                                     )
                              )
                            )
                     ) #end column with hist 
                   ) #end fluidrow
                 ) # end wellpanel
        ), # end graphical results tab
        
        ######################################################################################################### #
        # ~ ####
        
        # FULL REPORT   ####
        
        tabPanel(title = 'Full Report',
                 
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
                                                  style = 'color: #fff; background-color: #005ea2;'),
                            
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
                   # acs_version: "2016-2020"
                   # ejscreen_version: "2.1"
                   
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
                               placeholder = "2016-2020"),
                     ## input:  
                     textInput(inputId = 'ejscreen_version', 
                               label = 'Version of EJScreen',
                               placeholder = "2.1") 
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
        ), # end report generation tab
        
        ######################################################################################################### #
        # ~ ####
        # Advanced settings   ####
        
        # - hidden by default but can be activated by a button (see About EJAM tab)
        tabPanel(title = 'Advanced Settings',
                 
                 ## input: Type of plot for 1page report
                 shiny::radioButtons(inputId = "plotkind_1pager", 
                                     label = "Type of plot for 1page report",
                                     choices = list(Bar="bar", Box="box", Ridgeline="ridgeline"), 
                                     selected = "bar"),
                 
                 ## input: Name for 1st set of comparisons
                 shiny::textInput(inputId = 'an_name_comp1', 
                                  label='Name for 1st set of comparisons',
                                  ## this will need to be changed later
                                  value = '',
                                  placeholder =threshgroup.default['comp1']
                 ),
                 
                 ## input: Threshold value(s) for 1st set of comparisons
                 numericInput(inputId = 'an_thresh_comp1', 
                              label='Threshold value(s) for 1st set of comparisons (e.g. %ile 1-100):', 
                              value=threshold.default['comp1']
                 ),
                 
                 ## input: Name for 1st set of comparisons
                 shiny::textInput(inputId = 'an_name_comp2', 
                                  label='Name for 2nd set of comparisons',
                                  ## this will need to be changed later
                                  value = '',
                                  placeholder =threshgroup.default['comp2']
                 ),
                 
                 ## input: Threshold value(s) for 2nd set of comparisons
                 numericInput(inputId = 'an_thresh_comp2', 
                              label='Threshold value(s) for 2nd set of comparisons (e.g. %ile 1-100):', 
                              value=threshold.default['comp2']
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
                   choices = epa_programs,
                   selected = epa_programs,
                   inline = TRUE
                 ),
                 
                 ## input: Limit to facilities in these EPA programs
                 shiny::checkboxGroupInput(
                   inputId = "ss_limit_fac2",
                   label = "Limit to facilities on these EPA lists (all included by default)",
                   choices = epa_programs,
                   selected = epa_programs,
                   inline = TRUE
                 ),
                 
        ) # end Advanced Settings tab
      ), # end tagList
      
      ## add HTML footer - defined in global.R
      html_footer_fmt
    ) ## end fluidPage
  )
}

########################################################################### #
# ~ ####
# ___________ App UI ends here ________ ####

#' Add external Resources to App (from golem package code)
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {   # (adds external Resources to App) ####
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    golem::favicon(ext = 'png'),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "EJAM"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    
  )
}
########################################################################### #
