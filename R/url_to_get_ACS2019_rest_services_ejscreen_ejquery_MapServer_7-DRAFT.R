
url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7 <- function(
    
  ## function to make URL for map service 7 (ACS 2019 BLOCKGROUPS) ### #
  #   to get ACS data from EPA server via API  ###   #
  
  # where (what places)
  
  objectIds=NULL, 
  sqlFormat='none', 
  text=NULL, 
  where=NULL, 
  havingClause=NULL, 
  
  # select what stats
  
  outFields=NULL, 
  orderByFields=NULL, 
  groupByFieldsForStatistics=NULL, 
  outStatistics=NULL, 
  f='pjson',
  returnGeometry='true', 
  returnIdsOnly='false', 
  returnCountOnly='false', 
  returnExtentOnly='false', 
  returnDistinctValues='false', 
  returnTrueCurves='false', 
  returnZ='false', 
  returnM='false', 
  
  # geoprocessing like buffer/intersect, etc.
  
  geometry=NULL, 
  geometryType='esriGeometryEnvelope', 
  featureEncoding='esriDefault', 
  spatialRel='esriSpatialRelIntersects', 
  units='esriSRUnit_Foot', 
  distance=NULL, 
  inSR=NULL, 
  outSR=NULL, 
  relationParam=NULL, 
  geometryPrecision=NULL, 
  gdbVersion=NULL, 
  datumTransformation=NULL, 
  parameterValues=NULL, 
  rangeValues=NULL, 
  quantizationParameters=NULL, 
  maxAllowableOffset=NULL, 
  resultOffset=NULL, 
  resultRecordCount=NULL, 
  historicMoment=NULL, 
  time=NULL,
  timeRelation='esriTimeRelationOverlaps'
) {
  
  # Documentation of format and examples of input parameters:
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  # notes - useful map services to query ####
  #https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Map_Service/02ss0000006v000000/
  # 'https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/'
  # MaxRecordCount: 1000
  # https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/7/2?f=pjson 
  
  
  
  # ASSEMBLE URL BY PUTTING ALL THE PARAMETERS INTO ONE LONG STRING IN CORRECT FORMAT:
  # params_with_NULL <- c(as.list(environment()), list(...))  # if ... was among function parameter options
  params_with_NULL   <- c(as.list(environment()))
  params_with_NULL <- lapply(params_with_NULL, function(x) paste(x, collapse = ','))
  params_text_with_NULL <- paste(paste0(names(params_with_NULL), '=', params_with_NULL), collapse = '&')
  
  baseurl <- paste0(
    'https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/',
    7,  # servicenumber 7 is ACS 2019 block groups
    '/query?'
  )
  queryurl <- paste0(
    baseurl,  
    params_text_with_NULL
  )
  return(queryurl)  
}
