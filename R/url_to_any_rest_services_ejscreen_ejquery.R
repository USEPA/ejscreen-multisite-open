url_to_any_rest_services_ejscreen_ejquery <- function(servicenumber=7, ...) {
  # generic function wrapping ejscreen/ejquery API calls ####
  
  # Disadvantage of this generic approach is it does not help you by showing a list of parameters, since those are specific to the service like 7 vs 71. 
  
  # see links to documentation on using APIs here: 
  # 
  #   EJAM/dev/intersect-distance/arcgis/ArcGIS REST API basics.txt
  
  ################################################################################# # 
  ## notes - examples #### 
  if (1==0) {
    url_getacs_epaquery(  objectIds=1:4,                 outFields = 'STCNTRBG', justurl = TRUE)
    t(url_getacs_epaquery(objectIds=sample(1:220000,2),  outFields = '*'))
    t(url_getacs_epaquery(objectIds=sample(1:220000,2)))
    url_getacs_epaquery(  objectIds=sample(1:220000,10), outFields = c('STCNTRBG', 'STATE', 'COUNTY', 'TRACT', 'BLKGRP'), justurl = FALSE)
    y <- url_get_via_url(url_to_any_rest_services_ejscreen_ejquery(         servicenumber=71, lat=30.494982, lon=-91.132107, miles = 1))
    x <- url_get_via_url(url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71(lat=30.494982, lon=-91.132107, miles = 1))
    z <- url_get_via_url(url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7())
  }
  

  ################################################################################# #
    
  
  
  params_with_NULL   <- c(as.list(environment()))
  params_with_NULL <- subset(params_with_NULL, names(params_with_NULL) != 'servicenumber')
  params_with_NULL <- lapply(params_with_NULL, function(x) paste(x, collapse = ','))
  params_text_with_NULL <- paste(paste0(names(params_with_NULL), '=', params_with_NULL), collapse = '&')
  
  baseurl <- paste0(
    'https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/',
    servicenumber,  # 7 is ACS 2019 block groups
    '/query?'
  )
  queryurl <- paste0(
    baseurl,  
    params_text_with_NULL
  )
  return(queryurl)
}
