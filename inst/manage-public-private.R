default_hide_about_tab <- golem_opts$isPublic 
default_hide_plot_barplots_tab <- !golem_opts$isPublic
default_hide_plot_histo_tab <- golem_opts$isPublic
default_hide_advanced_settings <- golem_opts$isPublic
default_hide_written_report <- golem_opts$isPublic  # May be just commented out
default_hide_ejscreenapi_tab <- golem_opts$isPublic  # May be just commented out

choices_for_type_of_site_category = if_else(
  golem_opts$isPublic,
  'by Industry (NAICS) Code' = 'NAICS',
  c('by Industry (NAICS) Code' = 'NAICS',
    'by Industry (SIC) Code'   = 'SIC',
    'by EPA Program'           = 'EPA_PROGRAM',
    'by MACT subpart'          = 'MACT')
)

choices_for_type_of_site_upload <- if_else(
  golem_opts$isPublic,
  c(
    'Latitude/Longitude file upload'                = 'latlon',
    'EPA Facility IDs (FRS Identifiers)'            = 'FRS',
    'Shapefile of polygons'                         = 'SHP'
  ),
  choices = c('Latitude/Longitude file upload'               = 'latlon',
              'EPA Facility ID (FRS Identifiers)'            = 'FRS',
              'EPA Program IDs'                              = 'EPA_PROGRAM',
              'FIPS Codes'                                   = 'FIPS',
              'Shapefile of polygons'                        = 'SHP')   # , selected = 'latlon'   # would set initial value but default is 1st in list
)
