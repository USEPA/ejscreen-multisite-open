# These are the items that are toggled depending on whether the version is Public or Private
# if run_app(isPublic = TRUE), then it's Public
# In all cases, items are hidden when isPublic = TRUE and are toggled in app_server.R, unless otherwise specified

# About tab

# Plot Barplots tab - Public (show), Private (hide)

# Histograms tab

# Advanced settings

# Written Report

# default_hide_ejscreenapi_tab <- golem_opts$isPublic  # This doesn't seem used

choices_for_type_of_site_category = ifelse(
  isTruthy(golem_opts$isPublic),
  c('by Industry (NAICS) Code' = 'NAICS'),
  c(
    'by Industry (NAICS) Code' = 'NAICS',
    'by Industry (SIC) Code'   = 'SIC',
    'by EPA Program'           = 'EPA_PROGRAM',
    'by MACT subpart'          = 'MACT'
  )
)

choices_for_type_of_site_upload <- ifelse(
  isTruthy(golem_opts$isPublic),
  c(
    'Latitude/Longitude file upload'                = 'latlon',
    'EPA Facility IDs (FRS Identifiers)'            = 'FRS',
    'Shapefile of polygons'                         = 'SHP'
  ),
  c(
    'Latitude/Longitude file upload'               = 'latlon',
    'EPA Facility ID (FRS Identifiers)'            = 'FRS',
    'EPA Program IDs'                              = 'EPA_PROGRAM',
    'FIPS Codes'                                   = 'FIPS',
    'Shapefile of polygons'                        = 'SHP'
  )
)

.app_title <-  ifelse(isTruthy(golem_opts$isPublic), "EJScreen Multisite", "EJAM")

.community_report_title <- ifelse(isTruthy(golem_opts$isPublic), "EJScreen-EJAM Multisite Report", "EJAM Multisite Report")
