#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources, see end of this source file.
    #golem_add_external_resources(),
    
    ## begin app UI
    fluidPage(
      shinyjs::useShinyjs(),
      tags$style(HTML("
        .tabbable > .nav > li[class=active] > a {
           background-color: #005ea2;
           font-weight: bold;
        }")),
      ## add HTML header as ui output
      #uiOutput(outputId = 'html_header'),
      html_header_fmt,
      
      ## add title for app and browser tab
      titlePanel(title = "EJAM (Environmental Justice Analysis Multi-site) Tool",
                 windowTitle = "EJAM (Environmental Justice Analysis Multi-site) Tool"
      ),
      
      ## create tabsetPanel with tabs for different sections
      tabsetPanel(
        id = 'all_tabs',
        type = 'pills',
        selected = 'Site Selection',
        ## introduction tab
        tabPanel(title = 'About EJAM',
                 
                 br(),
                
                 ## html intro text - in global.R
                 intro_text
        ),
        
        ## site selection tab
        tabPanel(title = 'Site Selection',
               
           ## REMOVE later - vertical space
           br(),
           
           h3('Specifying Locations to Analyze'),
           
           ## upload help text - in global.R
           #HTML(upload_help_msg),
           
           
           radioButtons(inputId = 'ss_choose_method',
                        label = 'Please pick one of the following facility selection methods',
                        choiceValues = c('NAICS','FRS','latlon','ECHO'),
                        choiceNames = c('Select by Industry (NAICS) Code',
                                         'Upload EPA Facility ID (FRS Identifers) file',
                                         'Upload Location (latitude/longitude) file',
                                         'Upload ECHO file'),
                        width = '400px'),
           
           ## input: dropdown to choose upload method
           # selectInput(inputId = 'ss_choose_method',
           #             label = 'Please choose an upload method:',
           #             choices = c('NAICS code selection' = 'NAICS',
           #                         'Upload facility FRS ID file' = 'FRS',
           #                         'Upload facility lat/lon file' = 'latlon',
           #                         'Upload ECHO dataset' = 'ECHO')),
           
           ## add tooltip to dropdown for additional info
           tags$style(HTML("
                .tooltip > .tooltip-inner {
                background-color: #005ea2;
                }
                ")), 
           shinyBS::bsTooltip(id = 'ss_select_naics', title = 'Please read the upload instructions given below.',
                              placement = 'right', trigger = 'hover'),
           
           
           ## conditional Panels to show/hide other selection methods
           
           ## NAICS condition
           conditionalPanel(
             condition = "input.ss_choose_method == 'NAICS'",
             
             radioButtons('naics_ul_type', 'Choose how to enter NAICS codes',
                         choiceNames = c('Select codes from dropdown' ,
                                     'Enter text or code to search'),
                         choiceValues = c('dropdown','enter')),
             
             conditionalPanel(
               condition = "input.naics_ul_type == 'enter'",
               ## input: Enter NAICS code manually       
               textInput(
                 inputId = "ss_enter_naics",
                 label = htmltools::h6(
                   "Enter NAICS codes of interest - ",
                   htmltools::a("Look up NAICS", href ="https://www.census.gov/naics"),
                   htmltools::a(htmltools::img(id = "ibutton",src = "www/i.png",height = 15,width = 15),
                                href = "www/ibutton_help.html#help_naicslist",target = "_blank")
                 ),
                 value = "",
                 width = 400,
                 placeholder = NULL
               )
             ),
             
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
               ),
               
             ),
             
             
             br(),
             actionButton(inputId = 'submit_naics', label = 'Submit entries'),
             br(),br(),
             
             ## input: Limit to facilities where selected NAICS is found w/in EPA list
             # shiny::checkboxGroupInput(
             #   inputId = "ss_limit_fac1",
             #   label = "Limit to facilities where selected NAICS is found within these EPA lists: (all are searched by default)",
             #   choices = epa_programs,
             #   selected = epa_programs,
             #   inline = TRUE
             # ),
             # 
             # ## input: Limit to facilities in these EPA programs
             # shiny::checkboxGroupInput(
             #   inputId = "ss_limit_fac2",
             #   label = "Limit to facilities on these EPA lists (all included by default)",
             #   choices = epa_programs,
             #   selected = epa_programs,
             #   inline = TRUE
             # ),
             
             
             ## read more about NAICS - expandable/collabsible panel
             shinyBS::bsCollapse(
               id = 'naics_help', open = 'Read more about NAICS',
               shinyBS::bsCollapsePanel(title = 'Read more about NAICS',
                               style = 'success',
                               h3('Select industry'),
                               helpText('You may define your universe of interest by selecting specific industries that you wish to query.'),
                               htmltools::a('NAICS definitions at Census', href='https://www.census.gov/eos/www/naics/index.html', target='_blank')
                               )
             )
           ), # end NAICS conditionalPanel
           
           ## FRS conditional panel
           conditionalPanel(
             condition = "input.ss_choose_method == 'FRS'",
             ## input: Upload list of FRS identifiers
             shiny::fileInput(
               inputId = 'ss_upload_frs',
               label = 'Upload list of FRS identifiers',
               accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
             ),
             
             ## read more about FRS - expandable/collabsible panel
             shinyBS::bsCollapse(
               id = 'frs_help', open = 'FRS file upload details',
               bsCollapsePanel(title = 'FRS file upload details',
                               style = 'success',
                               ## FRS help text - in global.R
                               frs_help_msg
             )
           )), # end FRS conditionalPanel
           
           ## latlon conditional panel
           conditionalPanel(
             condition = "input.ss_choose_method == 'latlon'",
             
             ## input: Upload list of facility lat/longs
             fileInput(inputId = 'ss_upload_latlon',  
                       label = 'Upload file of site to buffer and summarize (.csv, .xls, or .xlsx) with lat & lon as column headers in row 1',
                       #placeholder = 'test_input_latlon.csv', 
                       multiple = FALSE,
                       accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")
                       # add hover tips here maybe, or even a button to view examples of valid formats and details on that.
             ),
             
             ## read more about latlon 
             shinyBS::bsCollapse(
               id = 'latlon_help', open = 'Location file upload details',
               shinyBS::bsCollapsePanel(title = 'Location file upload details',
                               style = 'success',
                               HTML(latlon_help_msg)
               )
              )
             
           ), # end latlong conditionalPanel
           ## ECHO conditional panel
           conditionalPanel(
             condition = "input.ss_choose_method == 'ECHO'",
             ## input: Find sites via ECHO
             # shiny::actionButton(inputId = 'ss_search_echo', label = 'Find sites via ECHO'),
             # 
             # ## vertical space
             # br(),
             
             ## input: Upload list of ECHO facilities
             shiny::fileInput(
               inputId = 'ss_upload_echo',
               label = 'Upload list of ECHO facilities',
               accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
             ),
             
             br(),
             
             shinyBS::bsCollapse(
               id = 'echo_help', open = 'ECHO upload details',
              
                shinyBS::bsCollapsePanel(title = 'ECHO upload details',
                                         style = 'success',
                                         ## echo help text - in global.R
                                         echo_message
                                      
                                      )
             )
           ),
           

           ## TEST - print output from getblocksnearby 
           #verbatimTextOutput(outputId = 'print_test'),
           
           ## vertical space
           br()
           
           
           ## REMOVE later - check for uploaded latlon dataset
           #tableOutput('upload_check')
        ),
        
        ## analysis settings tab
        tabPanel(title = 'Analysis Settings',
                 
                 ## vertical space
                 br(),  
                 
                 h4('Previewing uploaded data'),
                 
                 ## testing 
                 verbatimTextOutput('print_test2'),
                 
                 ## output: display number of uploaded sites
                 textOutput(outputId = 'an_map_text'),
                 
                 fluidRow(
                   column(6, 
                          radioButtons(inputId = 'circle_type', 
                                       label = 'Circle Type', choices = c('circles', 'circleMarkers'),
                                       inline = TRUE
                          ),
                   ), column(6,
                 ## input: highlight clusters on map? yes/no
                 shiny::checkboxInput(inputId = 'an_map_clusters', 
                                      label = 'Highlight overlaps?'
                                      ),
                      )
                 ),
                 
                 ## vertical space
                 br(),
                 
                 ## output: show leaflet map of uploaded points
                 leaflet::leafletOutput(outputId = 'an_leaf_map', 
                                        height = '500px', 
                                        width = '80%'),
                 fluidRow(
                   column(8,
                          ## input: Specify radius of circular buffer       
                          shiny::sliderInput(inputId = 'bt_rad_buff',
                                             label = htmltools::h5("Radius of circular buffer in miles"),
                                             value = 1.0, step = 0.25,
                                             min = 0.25, max = 10
                          )
                          #uiOutput('bt_rad_buff')
                        ),
                   column(4,
                          ## input: switch units to km for radius slider
                          radioButtons(inputId = 'radius_units', 
                                       label = 'Choose units for radius',
                                       choices = c('miles','kilometers'),
                                       selected = 'miles')
                          )
                 ),
                
                 
                 br(), 
                 hr(),
                 br(),
                 h4('Processing uploaded data'),
                 
                 ## input: button to run batch processing code
                 shiny::actionButton(inputId = 'bt_get_results', 
                                     label = 'Process Facilities'),
                
                 
                 ## input: Percentiles of sites & residents to calculate
                 shiny::checkboxGroupInput(inputId = 'an_list_pctiles', 
                                           label = 'Percentiles of sites & residents to calculate:', 
                                           #choices = probs.default.choices, 
                                           ## use choiceNames and choiceValues to avoid converting to numeric later
                                           choiceNames = probs.default.names,
                                           choiceValues = probs.default.values,
                                           selected = probs.default.selected,
                                           inline = TRUE),
                 
                 ## input: button to run batch processing code
                 shiny::actionButton(inputId = 'bt_get_summary', 
                                     label = 'Summarize Buffer Output'),
                 
        ),
        
        ## First view - similar to EJSCREEN standard report
        tabPanel(title = 'First View',
                 
                 ## vertical space
                 br(),
                 
                 ## output: button to download summary report
                 shiny::downloadButton(outputId = 'summary_download', 
                                       label = 'Download this summary'),
                 ## vertical space
                 br(),
                 
                 h3('Overall Summary Report'),
                 
                 
                 ## show count of population among selected sites
                 textOutput(outputId = 'view1_total_pop'),
                 
                 ## vertical space
                 br(),
                 
                 h4('Demographic Indicators'),
                 
                 ## output: table of overall demographic indicators
                 DT::DTOutput(outputId = 'view1_demog_table'),
                 
                 br(),
                 
                 h4('Environmental Indicators'),
                 ## output: table of overall environmental indicators
                 DT::DTOutput(outputId = 'view1_envt_table'),
                 
                 br(),
                 
                 plotOutput(outputId = 'view1_boxplot')
                 
                 ),
        
        ## Second view - summary stats about distribution of scores across people
        tabPanel(title = 'Second View',
                 
                 helpText('Goal: to show summary stats about the distribution of scores across people'),
                 
                 ),
        
        ## Third view - site-by-site table
        tabPanel(title = 'Third View',
                 
                 br(),
                 
                 h3('Site-by-Site Table'),
                 br(),
                 DT::DTOutput(outputId = 'view3_table', width = '80%', height='500px'),
                 br(),
                 leaflet::leafletOutput(outputId = 'v4_sitemap')
                 ),
        
        ## Fourth view - drill down to single site
        tabPanel(title = 'Fourth view',
            helpText('Goal: drill down to single site details'),     
            #uiOutput(outputId = 'v4_site_dropdown')
            selectInput(
              inputId = "v4_site_dropdown",
              label = htmltools::h6("Choose a site to explore"),
              # choose from named vector on server-side
              choices = NULL, 
              selected = NULL,
              width = 400
            )
        ),
        
        ## buffering tools tab
        tabPanel(title = 'Buffering Tools',
            
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
            
           ## REMOVE LATER - vertical space     
            br(),
                 
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
           
        ),
        
        ## summaries tab
        tabPanel(title = 'Summaries',
              
                 ## row of histogram settings
                 fluidRow(
                   h4("Histogram settings"),
                   ## input: Histogram settings - distribution across sites or people
                   column(
                     2,
                     radioButtons(inputId = 'summ_hist_distn', 
                                  label = h5('Distribution across sites or people (pop.wtd.)'), 
                                  choices = c('Sites', 'People'), selected = 'People' )
                   ),
                   ## input: Histogram settings - percentile zone
                   column(
                     2,
                     radioButtons(inputId = 'summ_hist_perc', label = h5('Percentile Zone'), 
                                  choiceNames = c('US', 'State'), choiceValues = c('us', 'state'),
                                  selected = 'us')
                   ),
                   ## input; Histogram settings - data type
                   column(
                     2, 
                     radioButtons(inputId = 'summ_hist_data', label = h5('Data type'), 
                                  choiceNames = c('Percentile of population', 'Raw data'),
                                  choiceValues = c('pctile', 'raw'))
                   ),
                   ## input: Histogram settings - number of bins
                   column(
                     2,
                     sliderInput(inputId = 'summ_hist_bins', label = h5('Bins'), 
                                 min = 5, max = 100, step = 5, value = 10)
                   )
                 ),
                 
                 ## output: display histogram
                 plotOutput(outputId = 'summ_display_hist'),     
                 
            ## row of barplot settings        
             fluidRow(
               h4('Barplot settings'),
               ## input: Barplot setting - indicator type
               column(4, 
                      radioButtons(inputId = 'summ_bar_ind', 
                                   label = h5('Indicator type'), 
                                   choices = c('Demographic', 'Environmental','EJ'))
                      ),
               ## input: Barplot setting - data type
               column(3, 
                      radioButtons(inputId = 'summ_bar_data', label = 'Data Type', 
                                      choiceValues = c('ratio', 'pctile','raw'), 
                                      choiceNames = c('Ratio to US','Percentile of population', 'Raw data'))
                      ),
               ## input: Barplot setting - statistic type
               column(3, 
                      radioButtons(inputId = 'summ_bar_stat', 'Statistic', 
                                   choiceValues = c('avg', 'med'),
                                   choiceNames = c('Average', 'Median')))
             ),
            
              ## output: display barplot
              plotOutput(outputId = 'summ_display_bar'),
            
            
            
            ## row of executive summary settings
            fluidRow(
              ## input: exec summary options - adjust demog percentile
              numericInput(inputId = 'summ_exec_demog', 
                           label = 'Demographic Percentile threshold:', 
                           value = 95, min = 0, max = 100, step = 5),
              
              ## input: executive summary options - adjust env factors
              selectInput(inputId = 'summ_exec_env', 
                          label = h5('Environmental factor:'), 
                          choices = EJAMbatch.summarizer::names_e_friendly,
                          selected=1),
              
              ## input: executive summary options - adjust EJ percentile
              numericInput(inputId = 'summ_exec_ej', 
                           label = 'EJ Percentile threshold:', 
                           value = 95, min = 0, max = 100, step = 5)
            
            )
        ),
        
        ## report generation tab
        tabPanel(title = 'Generate Report',
                
          br(),       
          wellPanel(       
           ## REMOVE LATER - add space
           br(),
                    
           fluidRow(
             column(2, 
                    ## input: add author information - name       
                    shiny::textInput(inputId = "rg_author_name", 
                                     label = "Author Name(s):", 
                                     value = "FirstName LastName")
             ),
             
             column(2,
                    ## input: add author information - email
                    shiny::textInput("rg_author_email", 
                                     label = "Author Email(s):", 
                                     value = "author@email.org")
             ),
             column(2,
                    checkboxInput('add_coauthors',label = 'Add co-authors?',
                                  value = FALSE)
             )
           ),   
           
           conditionalPanel(
             condition = 'input.add_coauthors == 1',
             fluidRow(
               column(2, 
                      textInput('coauthor_names', 'Co-Author Name(s)')
               ), 
               column(2,
                      textInput('coauthor_emails', 'Co-Author Email(s)')
               )
             )
           ),
           
           fluidRow(
             column(4,
                    
              uiOutput('rg_enter_miles')
           ## input: enter analysis details - "within xyz miles of"
           # shiny::textInput(inputId = "rg_enter_miles", 
           #                  label = "Analysis Location:", 
           #                  value = "within Xyz miles of")
           #   )
           )),
           
           fluidRow(
             column(4,
           ## input: enter analysis details - which sites analyzed
           shiny::textInput(inputId = "rg_enter_sites", 
                            label = "Describe sites analyzed:", 
                            value = "facilities in the polymers and resins I source category"),
             ), column(6,
           ## input: enter analysis details - which facilities analyzed
           # shiny::textInput(inputId = "rg_enter_fac", 
           #                  label = "Describe facilities_analyzed:", 
           #                  value = "facilities subject to this proposed rule (for example)"),
             )),
           
           fluidRow(
             column(4,
                    selectInput(inputId = 'zonetype', label = 'Zone Type (How are zones defined?)',
                                choices = c('General' = 'zone_is_named_x','Proximity'= 'zone_is_nearby',
                                            'Risk' = 'zone_is_risk_x'))
                    ),
             column(4,
                    selectInput(inputId = 'within_x_miles_of', label = 'Near to',
                                choices = c('near the','nearby',''))
             )
           ),
           
           fluidRow(
             column(4,
                    selectInput(inputId = 'in_areas_where', 
                                label = 'Describe the surrounding area',
                                choices = c('in areas with',
                                'where','in block groups where')
                                )
                    ),
             column(4,
                    textInput(inputId = 'in_areas_where_enter',
                              label = 'Add area details', value = '')
                    )
           ),
           
           fluidRow(
             column(4,
                    selectInput(inputId = 'in_the_x_zone', label = 'General study location',
                                choices = 
                                  c('in the study area' = 'area', 'in the analyzed locations' = 'locs',
                                    'in [State X] (specify)' = 'state', 
                                    'in EPA Region [XX] (specify)' = 'region')
                    )
             ),
             column(4,
                    
                    conditionalPanel(
                      condition = "input.in_the_x_zone == 'state' || input.in_the_x_zone == 'region'",
                      textInput('in_the_x_zone_enter', label = 'Other - please specify',
                                value = 'in ')
                    )
             )
           ),
           
           
           #uiOutput('in_the_x_zone_custom'),
           
           # fluidRow(
           #   column(8,
           # selectInput(inputId = 'within_x_miles_of', label = 'Near to',
           #             choices = c('near the','nearby',''))
           #   )
           # ),
           
           fluidRow(
             column(4,
                    selectInput(inputId = 'facilities_studied', label = 'Facilities Studied',
                                choices = c('facilities subject to this proposed rule' = 'rule',
                                            'analyzed facilities' = 'fac','analyzed sites' = 'sites',
                                            'facilities in the xxxx source category' = 'cat',
                                            'facilities in the xxxx sector (NAICS code xxxx)' = 'sector')
                    )
             ), 
             column(4,
                    conditionalPanel(
                      condition = "input.facilities_studied == 'cat' || input.facilities_studied == 'sector' || input.facilities_studied == 'rule'",
                      textInput('facilities_studied_enter', label = 'Other - please specify')
                    )
             )
           ), 
           
           fluidRow(
             column(8,
           selectInput('risks_are_x', label = 'Risk level',
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
           textInput('demog_how_elevated', label = 'Elevation of Demographic Indicators',
                     placeholder = 'moderately elevated'),
             ),
           column(4,
           textInput('envt_how_elevated', label = 'Elevation of Environmental Indicators',
                     placeholder = 'moderately elevated'),
           )),
           
           fluidRow(
             column(4,
           selectInput('demog_high_at_what_share_of_sites',
                       label = 'Demographic indicators high at what share of sites?',
                       choices = c('a surprisingly large share of these sites',
                                   'some of these sites, just as it varies nationwide',
                                   'a relatively small share of these sites'),
                       selected = 'some of these sites, just as it varies nationwide'
           ),
             ), column(4,
           selectInput('envt_high_at_what_share_of_sites',
                       label = 'Environmental indicators high at what share of sites?',
                       choices = c('a surprisingly large share of these sites',
                                   'some of these sites, just as it varies nationwide',
                                   'a relatively small share of these sites'),
                       selected = 'some of these sites, just as it varies nationwide'
           ),
             )),
           
           fluidRow(
             column(4,
           textInput('source_of_latlons',
                     label = 'Source of Points',
                     placeholder = "EPA's Facility Registry Service (FRS)"),
             ), 
           column(4,
           textInput('fundingsource',
                     label = 'Funding Source',
                     placeholder = "The Inflation Reduction Act (for example)"),
             )),
           
           fluidRow(
             column(8,
           textAreaInput('conclusion1',
                         label = 'Conclusion 1',
                         placeholder = "The people living near these sites are 40% more likely to be in Limited-English Households than the average US resident. (for example)"
           )
             )),
           fluidRow(
             column(8,
           textAreaInput('conclusion2', label = 'Conclusion 2',
                         
                         placeholder = "The % low income among these residents is 2.4 times the rate in the US overall. (for example)"
           )
             )),
           fluidRow(
             column(8,
           textAreaInput('conclusion3', label = 'Conclusion 3',
                        
                         placeholder = "The average resident near these sites is 1.5 times as likely to be Hispanic as the average person in their State overall. (for example)"
           )
             )),
           
            ## TEST - button to show PDF preview     
            # actionButton(inputId = "pdf_preview", "Preview PDF"),
            #uiOutput(outputId = "pdfview")
          ),
           ## output: button to download static report
           shiny::downloadButton(outputId = 'rg_download', 
                                 label = 'Download report'),
           
           actionButton(inputId = 'show_outline', label = 'Show Report Outline')
        )
      ),
      
      ## add HTML footer as ui output
      #uiOutput(outputId = 'html_footer'),
      html_footer_fmt
    ) ## end fluidPage
  )
} ########################################################################### #

