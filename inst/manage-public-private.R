# These are the items that are toggled depending on whether the version is Public or Private
# if run_app(isPublic = TRUE), then it's Public
# Most items toggled in app_server.R, unless otherwise specified

# About tab
default_hide_about_tab <- isTRUE(golem_opts$isPublic)

# Histograms tab
default_hide_plot_histo_tab <- isTRUE(golem_opts$isPublic)

# Advanced settings
default_hide_advanced_settings <- isTRUE(golem_opts$isPublic)

# Written Report
default_hide_written_report <- TRUE

# Barplots - Plot Average Scores
default_hide_plot_barplots_tab <- FALSE

# default_hide_ejscreenapi_tab <- isTRUE(golem_opts$isPublic)  # This doesn't seem used

choices_for_type_of_site_category = if (isTRUE(golem_opts$isPublic)) {
  c('by Industry (NAICS) Code' = 'NAICS')
} else {
  c(
    'by Industry (NAICS) Code' = 'NAICS',
    'by Industry (SIC) Code'   = 'SIC',
    'by EPA Program'           = 'EPA_PROGRAM',
    'by MACT subpart'          = 'MACT'
  )
}

choices_for_type_of_site_upload <- if (isTRUE(golem_opts$isPublic)) {
  c(
    'Latitude/Longitude file upload'                = 'latlon',
    'EPA Facility IDs (FRS Identifiers)'            = 'FRS',
    'Shapefile of polygons'                         = 'SHP'
  )
} else {
  c(
    'Latitude/Longitude file upload'               = 'latlon',
    'EPA Facility ID (FRS Identifiers)'            = 'FRS',
    'EPA Program IDs'                              = 'EPA_PROGRAM',
    'FIPS Codes'                                   = 'FIPS',
    'Shapefile of polygons'                        = 'SHP'
  )
}

.app_title <-  ifelse(isTRUE(golem_opts$isPublic), "EJScreen Multisite", "EJAM")

.community_report_title <- ifelse(isTRUE(golem_opts$isPublic), "EJScreen-EJAM Multisite Report", "EJAM Multisite Report")
