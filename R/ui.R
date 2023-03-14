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
      ## enable JavaScript functionality (such as resetting inputs)
      shinyjs::useShinyjs(),
      
      ## javascript function for jumping to top of screen
      shinyjs::extendShinyjs(text = "shinyjs.toTop = function() {window.scrollTo(0, 0);}",
                             functions = "toTop"),
      
      ## change selected tab color - #005ea2 matches blue on rest of website
      tags$style(HTML("
        .tabbable > .nav > li[class=active] > a {
           background-color: #005ea2;
           font-weight: bold;
        }")),

      ## add HTML header - in global.R
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
                 
                 ## vertical space
                 br(),
                
                 ## html intro text - in global.R
                 intro_text,
                 
                 ## button to reveal Advanced Settings tab 
                 actionButton('show_advanced_settings','Show Advanced Settings Tab', 
                              style ='color: #fff; background-color: #005ea2;')
        ),
        
        ## site selection tab
        tabPanel(title = 'Site Selection',
                 
           ## vertical space
           br(),br(),
           
           
           ## container for upload method (left column) and map (right column)
           fluidRow(
             
             ## upload method column
             column(5, 
                    
                    h3('Specifying Locations to Analyze', style='text-align: center;'),
                    
                    ## horizontal line
                    hr(),
                    
                    ## input: selecting upload method
                    radioButtons(inputId = 'ss_choose_method',
                                 label = 'Please use one of the following facility selection methods',
                                 choiceValues = c('latlon', 'NAICS','FRS','ECHO'),
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
                                label = 'Upload file of site to buffer and summarize (.csv, .xls, or .xlsx) with lat & lon as column headers in row 1',
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
                                   choiceValues = c('dropdown','enter')),
                      
                      ## conditional panel if entering naics
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
                      
                      
                      ## conditional panel if using NAICS dropdown
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
                      
                      ## vertical space
                      br(),
                      
                      ## input: button to submit NAICS codes that were entered/selected
                      actionButton(inputId = 'submit_naics', label = 'Submit entries',
                                   style = 'color: #fff; background-color: #005ea2;'),
                      
                      
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
                      
                    ), # end FRS conditionalPanel
                    
                    
                    ## ECHO upload conditional panel
                    conditionalPanel(
                      condition = "input.ss_choose_method == 'ECHO'",
                      
                      # ## vertical space
                      br(),
                      
                      ## input: Upload list of ECHO facilities
                      shiny::fileInput(
                        inputId = 'ss_upload_echo',
                        label = 'Upload list of ECHO facilities',
                        accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                      ), 
                      
                      #br(),
                      
                      
                    ), #end ECHO conditional panel
                    
                    ## horizontal line
                    hr(),
                    
                    h4('Processing uploaded data'),
                    
                    ## arrange title box and run button
                    fluidRow(
                      column(8, 
                             ## input: title for analysis (goes in report header)
                             shiny::textInput('analysis_title', 
                                              label = 'Specify Analysis Title',
                                              placeholder = 'My EJ Analysis',
                                              value = 'My EJ Analysis'),
                      ), 
                      column(4, style='padding: 10px',
                             ## input: run analysis button     
                             shiny::actionButton(inputId = 'bt_get_results', 
                                                 label = 'Run Analysis',
                                                 ## extra formatting - optional
                                                 style = 'color: #fff; background-color: #005ea2; height: 50px; width: 100px; border-radius: 5%;')
                        )
                      )
                    
                   
                    
             ), #end upload column 
             
             ## map column
             column(7,
                    ## add vertical line between columns
                    style = 'border-left: 1px solid;',
                    
                    h3('Previewing uploaded data', style='text-align: center;'),
                    
                    ## horizontal line
                    hr(),
                    
                    ## arrange summary text and button to view uploaded data
                    fluidPage(
                      column(8,
                             ## output: display number of uploaded sites
                             htmlOutput(outputId = 'an_map_text'),
                      ),
                      
                      column(4,
                             ## add button and modal to show uploaded data interactively
                             actionButton('show_data_preview', label = 'View uploaded data',
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
                    
                    ## arrange map options below map
                    fluidRow(
                      column(8,
                             align = 'center',
                             ## input: Specify radius of circular buffer       
                             shiny::sliderInput(inputId = 'bt_rad_buff',
                                                ## label is updated in server
                                                label = 'Radius of circular buffer',
                                                #label = htmltools::h5("Radius of circular buffer in miles"),
                                                value = 1.0, step = 0.25,
                                                min = 0.25, max = 10,
                                                post = ' miles'
                             ),
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
           
           
           ## horizontal line
           hr(),
           
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
               bsCollapsePanel(title = 'FRS file upload instructions',
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
           ), #end ECHO conditionalPanel
           
        ), # end Site Selection tab
        
        ## Summary Report tab - similar to EJSCREEN standard report
        tabPanel(title = 'Summary Report',
                 
                 ## vertical space
                 br(),
                 
                 ## add images to top of screen
                 HTML('<div id="logorow" class="row">
                        <div class="col-xs-6" style="text-align: left;">
                          <img src="https://ejscreen.epa.gov/mapper/images/epalogo_ej.png">
                        </div>
                        <div class="col-xs-6" style="text-align: right;">
                         <img src="https://ejscreen.epa.gov/mapper/images/ejlogo.png">
                        </div>
                      </div>'),  
                 
                 ## output: button to download summary report
                 tags$div(
                   shiny::downloadButton(outputId = 'summary_download', 
                                         label = 'Download Summary Report',
                                         style = 'color: #fff; background-color: #005ea2'),
                   style = 'text-align: center;'
                 ),
                 
                 ## vertical space
                 br(),
                 
                   
                 HTML('<div style="font-weight: bold; font-size: 14pt; font-family: Tahoma; text-align: center;">EJAM Report (EJScreen v2.1)</div>'),
                 
                 ## show count of population among selected sites
                 htmlOutput(outputId = 'view1_total_pop'),
                 
                 ## vertical space
                 br(),
                 

                 ## output: table of overall demographic indicators
                 shinycssloaders::withSpinner(
                   gt::gt_output(outputId = 'view1_demog_table')
                 ),
                 ## previous version of table - interactive
                 #DT::DTOutput(outputId = 'view1_demog_table'),
                 
                 
                 ## vertical space
                 br(),
                 
                 ## output: boxplots of demographic indicators
                   fluidRow(
                     column(
                       12, 
                       align = 'center',
                       shinycssloaders::withSpinner(
                         plotOutput(outputId = 'view1_boxplot', width = '900px', height='500px')
                       )
                     )
                   ),
                    
                # ),
                 
                ## vertical space
                 br(),
                 
                ## add formatted text
                 HTML("<p  style='font-size: 8pt; color: navy; font-family: sans-serif;'>This report shows the values
                 for environmental and demographic indicators.
                 It shows environmental and demographic raw data (e.g., the estimated concentration of ozone in the air), 
                 and also shows what percentile each raw data value represents. These percentiles provide perspective on
                 how the selected block group or buffer area compares to the entire state or nation. For example, if a 
                 given location is at the 95th percentile nationwide, this means that only 5 percent of US block groups 
                 have a higher value than the average person in the location being analyzed. The years for which the data
                 are available, and the methods used, vary across these indicators. Important caveats and uncertainties 
                 apply to this screening-level information, so it is essential to understand the limitations on appropriate 
                 interpretations and applications of these indicators. 
                      Please see EJScreen documentation for discussion of these issues before using reports.</p>"),
                 
                ## output: show report map
                fluidRow(
                  column(12,
                         align = 'center',
                         shinycssloaders::withSpinner(
                           leaflet::leafletOutput('quick_view_map', width = '75%')
                         )
                         )
                ),
                 
                 ## vertical space
                 br(),br(),
                 
                 
                 ## output: table of overall environmental indicators
                 shinycssloaders::withSpinner(
                   gt::gt_output(outputId = 'view1_envt_table')
                 ),
                ## previous version: interactive version of table
                #DT::dataTableOutput(outputId = 'view1_envt_table'),
                
                ## vertical space
                 br(),
                 
                ## add formatted text
                 HTML("<p style='font-size: 8pt; color: navy; font-family: sans-serif;'>*Diesel particulate matter, air toxics cancer risk, 
                        and air toxics respiratory hazard index are 
                        from the EPA's Air Toxics Data Update, which is the Agency's ongoing, comprehensive evaluation of 
                        air toxics in the United States. This effort aims to prioritize air toxics, emission sources, and 
                        locations of interest for further study. It is important to remember that the air toxics data presented 
                        here provide broad estimates of health risks over geographic areas of the country, not definitive risks 
                        to specific individuals or locations. Cancer risks and hazard indices from the Air Toxics Data Update 
                        are reported to one significant figure and any additional significant figures here are due to rounding. 
                        More information on the Air Toxics Data Update can be found at:
                      <a href='https://www.epa.gov/haps/air-toxics-data-update' target = '_blank'>https://www.epa.gov/haps/air-toxics-data-update</a></p>"
                 ),
                 
                ## add link
                 HTML('For additional information, see: <a>https://www.epa.gov/environmentaljustice</a>'),
                 
                ## horizontal line
                hr(),
                
                ## vertical space
                 br(),
                 
                ## add formatted text
                 HTML("<p style='font-size: 8pt; color: navy; font-family: sans-serif;'>Users should keep in mind that screening tools 
                 are subject to substantial uncertainty in their 
                 demographic and environmental data, particularly when looking at small geographic areas. Important caveats 
                 and uncertainties apply to this screening-level information, so it is essential to understand the limitations 
                 on appropriate interpretations and applications of these indicators. Please see EJScreen documentation for 
                 discussion of these issues before using reports. This screening tool does not provide data on every environmental
                 impact and demographic factor that may be relevant to a particular location. EJScreen outputs should be supplemented 
                 with additional information and local knowledge before taking any action to address potential EJ concerns.</p>"),
                 
        ),
        
        ## Third view - site-by-site table
        tabPanel(title = 'Tabular Results',
                 
                 ## vertical space
                 br(),
                 
                 h3('Key Indicators'),
                 wellPanel(
                  
                   ## input: dropdown for demographic indicator used in summary text
                   selectInput('key_ind_d', label = 'Choose a demographic indicator',
                               choices = setNames(names_d_fixed,
                                                  EJAMejscreenapi::map_headernames$names_friendly[match(names_d_fixed, EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
                               )
                   ),
                               
                   ## output: Demographic executive summary text
                   shinycssloaders::withSpinner(
                     htmlOutput('exec_summ_d')
                   ),

                   ## vertical space
                   br(),br(),
                   
                   ## input: dropdown for demographic indicator used in summary text
                   selectInput('key_ind_e', 
                               label = 'Choose an environmental indicator',
                               choices = setNames(EJAM::names_e,
                                                  EJAMbatch.summarizer::names_e_friendly
                                                  )
                    ),
                  
                   ## output: Environmental executive summary text
                   shinycssloaders::withSpinner(
                    htmlOutput('exec_summ_e')
                   )
                 ), 
                 
                 ## vertical space
                 br(),
                 
                 ## arrange header and Excel download button
                 fluidRow(
                   column(6,
                          h3('Site-by-Site Table'),
                          
                   ),
                   column(6,
                          ## button to download excel table of results - uses workbook_output_styled
                          downloadButton('download_results_table', 'Download Results Table',
                                         style = 'color: #fff; background-color: #005ea2;')
                          )
                 ),
                 
                 ## vertical space
                 br(),
                 
                 ## output: interactive table
                 shinycssloaders::withSpinner(
                     DT::DTOutput(outputId = 'view3_table', width = '100%')
                 ),
                 ## output: map of single site selected from table
                 shinycssloaders::withSpinner(
                    leaflet::leafletOutput(outputId = 'v3_sitemap')
                 ),

        ), # end Tabular results tab
        
        ## graphical results tab - barplots and histograms
        tabPanel(title = 'Graphical Results',
                 h3('Compare Across Indicators'),
                 
                 wellPanel(
                   
                   ## row of barplot settings        
                   fluidRow(
                     ## input: Barplot setting - indicator type
                     column(2,  
                            radioButtons(inputId = 'summ_bar_ind', 
                                         label = h5('Indicator type'), 
                                         choices = c('Demographic', 'Environmental','EJ')),
                            ## input: Barplot setting - data type
                            radioButtons(inputId = 'summ_bar_data', label = 'Data Type', 
                                         choiceValues = c('ratio','raw'), # no 'pctile' at this time
                                         choiceNames = c('Ratio to US','Raw data'), # no 'Percentile of population' at this time
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
                 
                 ## vertical space
                 br(),br(),
                 
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
                                         choiceValues = c('pctile', 'raw')),
                            
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
                                                 choices = setNames(c(gsub('VSI.eo', 'Demog.Index',EJAM::names_d), EJAM::names_e, EJAM::names_ej),
                                                                    c(EJAMbatch.summarizer::names_d_friendly, 
                                                                      EJAMbatch.summarizer::names_e_friendly, 
                                                                      EJAMbatch.summarizer::names_ej_friendly))
                                                )
                                     )
                            )
                            
                            
                     ) #end column with hist 
                   ) #end fluidrow
                 ) # end wellpanel
        ), # end graphical results tab
        
        ## full report generation tab
        tabPanel(title = 'Full Report',
                
          ## vertical space       
          br(),       
          
          wellPanel(       
           ## vertical space
           br(),
           
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
                    actionButton(inputId = 'show_outline', label = 'Show Report Outline',
                                 style = 'color: #fff; background-color: #005ea2;'),
                    
                    )
           ),
          
           
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
                    ## input: checkbox to add line for coauthor information
                    checkboxInput('add_coauthors',label = 'Add co-authors?',
                                  value = FALSE)
             )
           ),   
           
           ## if checkbox is checked, add textinputs for co-author name and email
           conditionalPanel(
             condition = 'input.add_coauthors == 1',
             fluidRow(
               column(2, 
                      ## input: coauthor name
                      textInput('coauthor_names', 'Co-Author Name(s)')
               ), 
               column(2,
                      ## input: coauthor email
                      textInput('coauthor_emails', 'Co-Author Email(s)')
               )
             )
           ),
           
           fluidRow(
             column(4,
              ## input: analysis location - uses current value of radius slider      
              uiOutput('rg_enter_miles')
        
           )),
           
           fluidRow(
             column(4,
                       ## input: enter analysis details - which sites analyzed
                       shiny::textInput(inputId = "rg_enter_sites", 
                            label = "Describe sites analyzed:", 
                            value = "facilities in the polymers and resins I source category"),
             )
           ),
           
           fluidRow(
             column(4,
                    ## input: zone type
                    selectInput(inputId = 'zonetype', label = 'Zone Type (How are zones defined?)',
                                choices = c('General' = 'zone_is_named_x','Proximity'= 'zone_is_nearby',
                                            'Risk' = 'zone_is_risk_x'))
                    ),
             column(4,
                    ## input: near to
                    selectInput(inputId = 'within_x_miles_of', label = 'Near to',
                                choices = c('near the','nearby',''))
             )
           ),
           
           fluidRow(
             column(4,
                    ## input: surrounding area
                    selectInput(inputId = 'in_areas_where', 
                                label = 'Describe the surrounding area',
                                choices = c('in areas with',
                                'where','in block groups where')
                                )
                    ),
             column(4,
                    ## input: area details
                    textInput(inputId = 'in_areas_where_enter',
                              label = 'Add area details', value = '')
                    )
           ),
           
           fluidRow(
             column(4,
                    ## input: choose study location
                    selectInput(inputId = 'in_the_x_zone', label = 'General study location',
                                choices = 
                                  c('in the study area' = 'area', 'in the analyzed locations' = 'locs',
                                    'in [State X] (specify)' = 'state', 
                                    'in EPA Region [XX] (specify)' = 'region')
                    )
             ),
             column(4,
                    ## add free text box if certain values chosen from radio button
                    conditionalPanel(
                      condition = "input.in_the_x_zone == 'state' || input.in_the_x_zone == 'region'",
                      textInput('in_the_x_zone_enter', label = 'Other - please specify',
                                value = 'in ')
                    )
             )
           ),
          
           fluidRow(
             column(4,
                    ## input: choose facilities studied
                    selectInput(inputId = 'facilities_studied', label = 'Facilities Studied',
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
                      textInput('facilities_studied_enter', label = 'Other - please specify')
                    )
             )
           ), 
           
           fluidRow(
             column(8,
                    ## input: risk levels
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
                    ## input: elevation of demog indicators
                  textInput('demog_how_elevated', label = 'Elevation of Demographic Indicators',
                     placeholder = 'moderately elevated'),
             ),
             column(4,
                  ## input: elevation of envt indicators
                  textInput('envt_how_elevated', label = 'Elevation of Environmental Indicators',
                     placeholder = 'moderately elevated'),
             )
           ),
           
           fluidRow(
             column(4,
                  ## input: demog indicator high share
                  selectInput('demog_high_at_what_share_of_sites',
                       label = 'Demographic indicators high at what share of sites?',
                       choices = c('a surprisingly large share of these sites',
                                   'some of these sites, just as it varies nationwide',
                                   'a relatively small share of these sites'),
                       selected = 'some of these sites, just as it varies nationwide'),
             ), 
             column(4,
                  ## input: ent indicator high share
                  selectInput('envt_high_at_what_share_of_sites',
                       label = 'Environmental indicators high at what share of sites?',
                       choices = c('a surprisingly large share of these sites',
                                   'some of these sites, just as it varies nationwide',
                                   'a relatively small share of these sites'),
                       selected = 'some of these sites, just as it varies nationwide'),
             )
            ),
           
           fluidRow(
             column(4,
                    ## input: source of latlon data
                    textInput('source_of_latlons',
                       label = 'Source of Points',
                       placeholder = "EPA's Facility Registry Service (FRS)"),
             ), 
           column(4,
                  ## input: funding source
                  textInput('fundingsource',
                     label = 'Funding Source',
                     placeholder = "The Inflation Reduction Act (for example)"),
             )
           ),
           
           fluidRow(
             column(8,
                    ## input: conclusion 1 - not linked to results at this time
                    textAreaInput('conclusion1',
                         label = 'Conclusion 1',
                         placeholder = "The people living near these sites are 40% more likely to be in Limited-English Households than the average US resident. (for example)"
                                  )
             )
           ),
           
           fluidRow(
             column(8,
                    ## input: conclusion 2- not linked to results at this time
                    textAreaInput('conclusion2', label = 'Conclusion 2',
                         placeholder = "The % low income among these residents is 2.4 times the rate in the US overall. (for example)")
             )
           ),
           
           fluidRow(
             column(8,
                    ## input: conclusion 3 - not linked to results at this time
                    textAreaInput('conclusion3', label = 'Conclusion 3',
                         placeholder = "The average resident near these sites is 1.5 times as likely to be Hispanic as the average person in their State overall. (for example)")
             )
           ),
           
         ) # end wellpanel
          
        ), # end report generation tab
        
        ## advanced settings tab - this is hidden by default
        ## but can be activated by a button (see About EJAM tab)
        tabPanel(title = 'Advanced Settings',
                 
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
                 
                 ## vertical space     
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
} ########################################################################### #

