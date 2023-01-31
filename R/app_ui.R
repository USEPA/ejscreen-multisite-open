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
      
      ## add title for app and browser tab
      titlePanel(title = "EJAM (Environmental Justice Analysis Multi-site) Tool",
                 windowTitle = "EJAM (Environmental Justice Analysis Multi-site) Tool"
      ),
      
      ## create tabsetPanel with tabs for different sections
      tabsetPanel(
        id = 'all_tabs',
        
        ## site selection tab
        tabPanel(title = 'Site Selection',
                 
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
                 ),
                 
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
                 
                 ## input: Upload list of facility lat/longs
                 fileInput(inputId = 'ss_upload_latlon',  
                           label = 'Upload file of site to buffer and summarize (.csv, .xls, or .xlsx) with lat & lon as column headers in row 1',
                           #placeholder = 'test_input_latlon.csv', 
                           multiple = FALSE,
                           accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")
                           # add hover tips here maybe, or even a button to view examples of valid formats and details on that.
                 ),
                 
                 ## input: Upload list of FRS identifiers
                 shiny::fileInput(
                   inputId = 'ss_upload_frs',
                   label = 'Upload list of FRS identifiers',
                   accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                 ),
                 
                 ## TESTING - need to validate uploaded files
                 ## input: upload shapefile with facilities
                 shiny::fileInput(inputId = "ss_upload_shp", 
                                  label = "Upload Shapefile", 
                                  accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                                  multiple=TRUE),
                 
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
                 
                 ## REMOVE later - add space
                 br(),
                 
                 ## input: Find sites via ECHO
                 shiny::actionButton(inputId = 'ss_search_echo', label = 'Find sites via ECHO'),
                 
                 
                 ## REMOVE later - check for uploaded latlon dataset
                 tableOutput('upload_check')
        ),
        
        ## analysis settings tab
        tabPanel(title = 'Analysis Settings',
                 
                 ## REMOVE later - add space
                 br(),  
                 
                 ## input: Percentiles of sites & residents to calculate
                 shiny::checkboxGroupInput(inputId = 'an_list_pctiles', 
                                           label = 'Percentiles of sites & residents to calculate:', 
                                           #choices = probs.default.choices, 
                                           ## use choiceNames and choiceValues to avoid converting to numeric later
                                           choiceNames = probs.default.names,
                                           choiceValues = probs.default.values,
                                           selected = probs.default.selected,
                                           inline = TRUE),
                 
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
                 
                 ## input: highlight clusters on map? yes/no
                 shiny::checkboxInput(inputId = 'an_map_clusters', 
                                      label = 'Highlight overlaps?'
                 ),
                 
                 ## output: display number of uploaded sites
                 textOutput(outputId = 'an_map_text'),
                 
                 ## REMOVE LATER - add space
                 br(),
                 
                 ## output: show leaflet map of uploaded points
                 leaflet::leafletOutput(outputId = 'an_leaf_map', 
                                        height = '500px', 
                                        width = '500px')
                 
        ),
        
        ## buffering tools tab
        tabPanel(title = 'Buffering Tools',
                 
                 ## input: Specify radius of circular buffer       
                 shiny::sliderInput(inputId = 'bt_rad_buff',
                                    label = htmltools::h5("Radius of circular buffer in miles"),
                                    value = 1.0, step = 0.25,
                                    min = 0.25, max = 10 
                 ),
                 
                 ## input: button to request buffer results
                 shiny::actionButton(inputId = 'bt_get_results', 
                                     label = 'Process Facilities'),
                 
        ),
        
        ## summaries tab
        tabPanel(title = 'Summaries',
                 
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
                 
                 ## REMOVE LATER - add space
                 br(),
                 
                 ## input: add author information - name       
                 shiny::textInput(inputId = "rg_author_name", 
                                  label = "Author Name(s):", 
                                  value = "authorname1 goes here"),
                 
                 ## input: add author information - email
                 shiny::textInput("rg_author_email", 
                                  label = "Author Email(s):", 
                                  value = "authoremail1 goes here"),
                 
                 ## input: enter analysis details - "within xyz miles of"
                 shiny::textInput(inputId = "rg_enter_miles", 
                                  label = "Analysis Location:", 
                                  value = "within Xyz miles of"),
                 
                 ## input: enter analysis details - which sites analyzed
                 shiny::textInput(inputId = "rg_enter_sites", 
                                  label = "Describe sites analyzed:", 
                                  value = "facilities in the polymers and resins I source category (for example)"),
                 
                 ## input: enter analysis details - which facilities analyzed
                 shiny::textInput(inputId = "rg_enter_fac", 
                                  label = "Describe facilities_analyzed:", 
                                  value = "facilities subject to this proposed rule (for example)"),
                 
                 
                 ## output: button to download static report
                 shiny::downloadButton(outputId = 'rg_download', 
                                       label = 'Download report')
        )
      )
      
    ) ## end fluidPage
  )
} ########################################################################### #

#' Add external Resources to App (from golem package code)
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {   # (adds external Resources to App) ####
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "EJAM"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    
  )
}
