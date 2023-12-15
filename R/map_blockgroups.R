#' map_blockgroups - helper function using API to get boundaries of blockgroups
#' @details This is useful mostly 
#' @param bgfips max of 50 for now. one or more block group FIPS codes as 12-character strings in a vector
#' @param outFields can be "*" for all, or can be 
#'   just some variables like SQMI, POPULATION_2020, etc., or none
#' @param myservice URL of feature service to get shapes from. 
#' 
#'   "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/
#'   EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0/query"
#'   
#'   for example provides EJScreen indicator values, NPL_CNT, TSDF_CNT, EXCEED_COUNT_90, etc.
#'   @seealso [map_blockgroups_over_blocks()]
#' @return spatial object via sf::read_sf()
#' @export
#'
#' @examples
#' dontrun{
#'   #bgs <- fips_bg_from_anyfips(fips_counties_from_state_abbrev("NY")[2])
#'   # 10 blockgroups in NY with highest score for this indicator 
#'   bgs <-  blockgroupstats[!is.na(Demog.Index.Supp) &
#'     ST == "NY", ][order(Demog.Index.Supp), tail(bgfips, 10)]
#'   # top 10 in 1 county in LA
#'   bgs <-  blockgroupstats[!is.na(Demog.Index.Supp) &
#'     countyname == "East Baton Rouge Parish", ][order(Demog.Index.Supp), tail(bgfips, 10)]
#'   DT::datatable(blockgroupstats[bgfips %in%  bgs, ])
#'   x <- map_blockgroups(bgs)
#'   leaflet(x) %>% addPolygons(color = "red") %>% addTiles()
#'    }
map_blockgroups <- function(bgfips = '010890029222', outFields = "",
                            myservice = c(
                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5/query",
                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Block_Groups/FeatureServer/0/query",
                              "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0/query")[1]
                            ) {
  
  # for a vector of blockgroup FIPS, use arcgis API to obtain map boundaries of just those blockgroups
  
  if (length(bgfips) > 50) {
    warning("Cannot get so many blockgroup shapes in one query, via this API, as coded! Using first 50 only.")
    bgfips <- bgfips[1:50]}
  
  if (grepl("ejscreen", myservice, ignore.case = TRUE)) {FIPSVARNAME <- "ID"} else {FIPSVARNAME <- "FIPS"}
  myurl <- httr2::url_parse(myservice)
  myurl$query <- list(
    where = paste0(paste0(FIPSVARNAME, "='", bgfips, "'"), collapse = " OR "),  ########################### # 
    outFields = outFields,
    returnGeometry = "true",
    f = "geojson")
  request <- httr2::url_build(myurl)
  mymapdata <- sf::read_sf(request)
  
  #mapview(mymapdata, col.regions = 'green', map.types = "OpenStreetMap")
  
  return(mymapdata)
}
########################### # ########################### # ########################### # ########################### # 


#' map_blockgroups_over_blocks - Overlay blockgroups near 1 site, after plotblocksnearby()
#'
#' @param y  output of [plotblocksnearby()]
#'
#' @return leaflet map widget
#' @export
#' @seealso [map_blockgroups()]
#' @examples dontrun{
#'  y <- plotblocksnearby(testpoints_10[5,], 
#'         radius = 3,
#'         returnmap = TRUE)
#'  map_blockgroups_over_blocks(y)
#'   }
map_blockgroups_over_blocks <- function(y) {
  # y is output of plotblocksnearby()
  bgids <-  unique(as.vector(sapply( y$x$calls[[2]]$args[[7]], function(z)   gsub(   ".*bgid: ([0-9]*)<.*", "\\1", z))))
  if (!exists("bgid2fips")) dataload_from_pins("bgid2fips")
  bgfips <- bgid2fips[bgid %in% bgids, bgfips] 
  x <- map_blockgroups(bgfips) # but not for 60+ fips!
  # add those FIPS shapes to the leaflet htmlwidget map 
  mymap <-   y %>% 
    leaflet::addGeoJSON(geojsonio::geojson_json(x), color = "green", group = "Blockgroups", data = x) %>% 
    addLayersControl(overlayGroups = "Blockgroups")
  return(mymap)
}
########################### # ########################### # ########################### # ########################### # 

#################### #

# https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0
# ID is the field storing blockgroup FIPS
# "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0/query?where=ID%3D220790116003+OR+ID%3D220790116002&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token="
# NPL_CNT (type: esriFieldTypeSmallInteger, alias: NPL_CNT, SQL Type: sqlTypeOther, nullable: true, editable: true)
# TSDF_CNT
# etc.

#################### #

# https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5
# FIPS is the field storing blockgroup FIPS as 12 characters
# also has SQMI, POPULATION_2020, etc.

# OBJECTID (type: esriFieldTypeOID, alias: OBJECTID, SQL Type: sqlTypeOther, length: 0, nullable: false, editable: false)
# STATE_ABBR (type: esriFieldTypeString, alias: State Abbreviation, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
# STATE_FIPS (type: esriFieldTypeString, alias: State FIPS, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
# COUNTY_FIPS (type: esriFieldTypeString, alias: County FIPS, SQL Type: sqlTypeOther, length: 3, nullable: true, editable: true)
# STCOFIPS (type: esriFieldTypeString, alias: State and County FIPS, SQL Type: sqlTypeOther, length: 5, nullable: true, editable: true)
# TRACT_FIPS (type: esriFieldTypeString, alias: Tract FIPS, SQL Type: sqlTypeOther, length: 6, nullable: true, editable: true)
# BLOCKGROUP_FIPS (type: esriFieldTypeString, alias: Block Group FIPS, SQL Type: sqlTypeOther, length: 1, nullable: true, editable: true)
# FIPS (type: esriFieldTypeString, alias: FIPS Code, SQL Type: sqlTypeOther, length: 12, nullable: true, editable: true)
# POPULATION (type: esriFieldTypeInteger, alias: 2022 Total Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP_SQMI (type: esriFieldTypeDouble, alias: 2022 Population per square mile, SQL Type: sqlTypeOther, nullable: true, editable: true)
# SQMI (type: esriFieldTypeDouble, alias: Area in square miles, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POPULATION_2020 (type: esriFieldTypeInteger, alias: 2020 Total Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP20_SQMI (type: esriFieldTypeDouble, alias: 2020 Population per square mile, SQL Type: sqlTypeOther, nullable: true, editable: true)
# Shape__Area (type: esriFieldTypeDouble, alias: Shape__Area, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# Shape__Length (type: esriFieldTypeDouble, alias: Shape__Length, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# 
#################### #

# "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Block_Groups/FeatureServer/0"
# FIPS is the field storing blockgroup FIPS

# OBJECTID (type: esriFieldTypeOID, alias: OBJECTID, SQL Type: sqlTypeOther, length: 0, nullable: false, editable: false)
# STATE_FIPS (type: esriFieldTypeString, alias: State FIPS, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
# CNTY_FIPS (type: esriFieldTypeString, alias: CNTY_FIPS, SQL Type: sqlTypeOther, length: 3, nullable: true, editable: true)
# STCOFIPS (type: esriFieldTypeString, alias: County FIPS, SQL Type: sqlTypeOther, length: 5, nullable: true, editable: true)
# TRACT (type: esriFieldTypeString, alias: Tract, SQL Type: sqlTypeOther, length: 6, nullable: true, editable: true)
# BLKGRP (type: esriFieldTypeString, alias: Block Group, SQL Type: sqlTypeOther, length: 1, nullable: true, editable: true)
# FIPS (type: esriFieldTypeString, alias: FIPS, SQL Type: sqlTypeOther, length: 12, nullable: true, editable: true)
# POP2010 (type: esriFieldTypeInteger, alias: POP2010, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP10_SQMI (type: esriFieldTypeDouble, alias: POP10_SQMI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP2012 (type: esriFieldTypeInteger, alias: POP2020, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP12_SQMI (type: esriFieldTypeDouble, alias: POP20_SQMI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# WHITE (type: esriFieldTypeInteger, alias: WHITE, SQL Type: sqlTypeOther, nullable: true, editable: true)
# BLACK (type: esriFieldTypeInteger, alias: BLACK, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AMERI_ES (type: esriFieldTypeInteger, alias: AMERI_ES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# ASIAN (type: esriFieldTypeInteger, alias: ASIAN, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HAWN_PI (type: esriFieldTypeInteger, alias: HAWN_PI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HISPANIC (type: esriFieldTypeInteger, alias: HISPANIC, SQL Type: sqlTypeOther, nullable: true, editable: true)
# OTHER (type: esriFieldTypeInteger, alias: OTHER, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MULT_RACE (type: esriFieldTypeInteger, alias: MULT_RACE, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MALES (type: esriFieldTypeInteger, alias: MALES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# FEMALES (type: esriFieldTypeInteger, alias: FEMALES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_UNDER5 (type: esriFieldTypeInteger, alias: AGE_UNDER5, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_5_9 (type: esriFieldTypeInteger, alias: AGE_5_9, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_10_14 (type: esriFieldTypeInteger, alias: AGE_10_14, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_15_19 (type: esriFieldTypeInteger, alias: AGE_15_19, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_20_24 (type: esriFieldTypeInteger, alias: AGE_20_24, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_25_34 (type: esriFieldTypeInteger, alias: AGE_25_34, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_35_44 (type: esriFieldTypeInteger, alias: AGE_35_44, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_45_54 (type: esriFieldTypeInteger, alias: AGE_45_54, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_55_64 (type: esriFieldTypeInteger, alias: AGE_55_64, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_65_74 (type: esriFieldTypeInteger, alias: AGE_65_74, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_75_84 (type: esriFieldTypeInteger, alias: AGE_75_84, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_85_UP (type: esriFieldTypeInteger, alias: AGE_85_UP, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MED_AGE (type: esriFieldTypeDouble, alias: MED_AGE, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MED_AGE_M (type: esriFieldTypeDouble, alias: MED_AGE_M, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MED_AGE_F (type: esriFieldTypeDouble, alias: MED_AGE_F, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HOUSEHOLDS (type: esriFieldTypeInteger, alias: HOUSEHOLDS, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AVE_HH_SZ (type: esriFieldTypeDouble, alias: AVE_HH_SZ, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HSEHLD_1_M (type: esriFieldTypeInteger, alias: HSEHLD_1_M, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HSEHLD_1_F (type: esriFieldTypeInteger, alias: HSEHLD_1_F, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MARHH_CHD (type: esriFieldTypeInteger, alias: MARHH_CHD, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MARHH_NO_C (type: esriFieldTypeInteger, alias: MARHH_NO_C, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MHH_CHILD (type: esriFieldTypeInteger, alias: MHH_CHILD, SQL Type: sqlTypeOther, nullable: true, editable: true)
# FHH_CHILD (type: esriFieldTypeInteger, alias: FHH_CHILD, SQL Type: sqlTypeOther, nullable: true, editable: true)
# FAMILIES (type: esriFieldTypeInteger, alias: FAMILIES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AVE_FAM_SZ (type: esriFieldTypeDouble, alias: AVE_FAM_SZ, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HSE_UNITS (type: esriFieldTypeInteger, alias: HSE_UNITS, SQL Type: sqlTypeOther, nullable: true, editable: true)
# VACANT (type: esriFieldTypeInteger, alias: VACANT, SQL Type: sqlTypeOther, nullable: true, editable: true)
# OWNER_OCC (type: esriFieldTypeInteger, alias: OWNER_OCC, SQL Type: sqlTypeOther, nullable: true, editable: true)
# RENTER_OCC (type: esriFieldTypeInteger, alias: RENTER_OCC, SQL Type: sqlTypeOther, nullable: true, editable: true)
# SQMI (type: esriFieldTypeDouble, alias: SQMI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# Shape__Area (type: esriFieldTypeDouble, alias: Shape__Area, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# Shape__Length (type: esriFieldTypeDouble, alias: Shape__Length, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# 


