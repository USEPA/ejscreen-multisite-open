url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71 <- function(
    lat, lon, 
    miles, 
    outFields='GEOID10,OBJECTID,POP_WEIGHT', 
    returnCountOnly='false') {
  
  # function for service 71, nearby blockweights ####
  ## notes - examples #### 
  if (1 == 0) {
    url_getacs_epaquery(  objectIds = 1:4,                 outFields = 'STCNTRBG', justurl = TRUE)
    t(url_getacs_epaquery(objectIds = sample(1:220000,2),  outFields = '*'))
    t(url_getacs_epaquery(objectIds = sample(1:220000,2)))
    url_getacs_epaquery(  objectIds = sample(1:220000,10), outFields = c('STCNTRBG', 'STATE', 'COUNTY', 'TRACT', 'BLKGRP'), justurl = FALSE)
    y <- url_get_via_url(url_to_any_rest_services_ejscreen_ejquery(         servicenumber = 71, lat = 30.494982, lon = -91.132107, miles = 1))
    x <- url_get_via_url(url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71(lat = 30.494982, lon = -91.132107, miles = 1))
    z <- url_get_via_url(url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7())
  }
  # Documentation of format and examples of input parameters:
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  
  
  
  baseurl <- "https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/71/query?"
  params <- paste0( 
    'outFields=', outFields,
    '&geometry=', lon, '%2C', lat, #  -91.0211604%2C30.4848044',
    '&distance=', miles,
    '&returnCountOnly=', returnCountOnly, 
    '&where=',
    '&text=',
    '&objectIds=',
    '&time=',
    '&timeRelation=esriTimeRelationOverlaps',
    '&geometryType=esriGeometryPoint',
    '&inSR=',
    '&spatialRel=esriSpatialRelContains',
    '&units=esriSRUnit_StatuteMile',
    '&relationParam=',
    '&returnGeometry=false',
    '&returnTrueCurves=false',
    '&maxAllowableOffset=',
    '&geometryPrecision=',
    '&outSR=',
    '&havingClause=',
    '&returnIdsOnly=false',
    '&orderByFields=',
    '&groupByFieldsForStatistics=',
    '&outStatistics=',
    '&returnZ=false',
    '&returnM=false',
    '&gdbVersion=',
    '&historicMoment=',
    '&returnDistinctValues=false',
    '&resultOffset=',
    '&resultRecordCount=',
    '&returnExtentOnly=false',
    '&sqlFormat=none&datumTransformation=',
    '&parameterValues=',
    '&rangeValues=',
    '&quantizationParameters=',
    '&featureEncoding=esriDefault',
    '&f=pjson')
  url <- paste0(baseurl, params)
  return(url)
}


