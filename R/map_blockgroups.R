
# SEVERAL FUNCTIONS & EXAMPLES ARE IN THIS FILE ####
#
## + see extensive notes in sf_for_shapefiles-NOTES.R ####
# at  EJAM/dev/notes_GIS_maps_distance_etc/sf_for_shapefiles-NOTES.R 

########################## # ########################## #   ########################## # ########################## # 

# Functions here: 
#
# mapfastej_counties()
# map_blockgroups_over_blocks()
# map_shapes_plot()
# map_shapes_leaflet()
# map_shapes_mapview()
# shapes_counties_from_countyfips()
# shapes_blockgroups_from_bgfips()

########################## # ########################## #   ########################## # ########################## # 


# ~ ####
### Examples here show various ways to 
# 1) get census unit boundaries and then 
# 2) view maps of Counties, tracts, or blockgroups.
# 
## (but EJScreen map services are probably a better way to do this at least for blockgroups).

# EXAMPLES ####

if ("example" == "script to run now") {
  
  ##    GET ALL FIPS CODES WITHIN SOME ZONE ####
  #
  # fips_de <- fips_counties_from_state_abbrev("DE")
  fips_ky <- fips_counties_from_statename("Kentucky")
  
  #    GET BOUNDARIES ####
  #
  # ##mymapdata <- shapes_counties_from_countyfips(fips_de) # 3 counties are in Delaware
  mymapdata <- shapes_counties_from_countyfips(fips_ky) # 120 counties are in Kentucky
  # ###mymapdata <- shapes_blockgroups_from_bgfips(fips_bg_from_anyfips(fips_de[1]))
  
  #    GET EJ STATS BY COUNTY FIPS CODE ####
  
  x <- ejamit(fips = fips_ky, radius = 0)
  
  ########################## # ########################## # 
  
  #    DRAW STATIC MAPS using map_shapes_plot(), based on plot() ####
  #
  ## see color-coding of a percentile variable:
  
  pal <- leaflet::colorBin(palette = c("yellow","yellow", "orange", "red"), bins = 80:100)
  shading <- pal(x$results_bysite$pctile.Demog.Index.Supp)
  map_shapes_plot(mymapdata, main = "Selected Counties") # or just # plot(mymapdata)
  plot(mymapdata, col = shading, add = TRUE)
  flagged <- which.max(x$results_bysite$Demog.Index.Supp)
  plot(mymapdata[flagged, ], col = "purple", add = TRUE)
  ## also could use ggplot to make a map.
  
  ########################## # ########################## # 
  
  #    DRAW HTML MAPS USING map_shapes_leaflet(), based on leaflet ####
  
  x <- ejamit(fips = fips_ky, radius = 0)
  
  # Very simple leaflet map with very simple popups
  popup1 <- popup_from_ejscreen(x$results_bysite)
  map_shapes_leaflet(mymapdata, popup = popup1)
  
  # Better leaflet map:   Note this is mostly built into mapfastej_counties() now.
  # Popups with Countynames, decimal rounding, friendly labels,
  #  and Color-coded map:
  
  ## see color-coding of a percentile variable:
  
  myindicators <- c("pctile.Demog.Index.Supp", names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg)
  myindicators <- c(names(x$results_bysite)[1:9], myindicators)
  popindicators <- x$results_bysite[ , ..myindicators]
  popindicators <- table_round(popindicators) # decimal places set
  countynames <- fips2countyname(x$results_bysite$ejam_uniq_id)
  popindicators <- cbind(County = countynames, popindicators) 
  poplabels <- fixcolnames(names(popindicators), 'r', 'long') # friendly labels for indicators
  popup2 <- popup_from_any(popindicators, labels = poplabels)
  ## see color-coding of one percentile variable:
  pal <- leaflet::colorBin(palette = c("yellow","yellow", "orange", "red"), bins = 80:100)
  shading <- pal(x$results_bysite$pctile.Demog.Index.Supp)
  
  mymap <- map_shapes_leaflet(mymapdata, popup = popup2, color = shading)
  mymap %>% leaflet::addLegend(colors = c("yellow", "orange", "red"), labels = c(80, 90, 100), title = "Demog.Index.Supp. %ile")
  
  # #bgs <- fips_bg_from_anyfips(fips_counties_from_state_abbrev("NY")[2])
  # # 10 blockgroups in NY with highest score for this indicator
  # bgs <-  blockgroupstats[!is.na(Demog.Index.Supp) &
  #   ST == "NY", ][order(Demog.Index.Supp), tail(bgfips, 10)]
  # # top 10 in 1 county in LA
  # bgs <-  blockgroupstats[!is.na(Demog.Index.Supp) &
  #   countyname == "East Baton Rouge Parish", ][order(Demog.Index.Supp), tail(bgfips, 10)]
  # DT::datatable(blockgroupstats[bgfips %in%  bgs, ])
  
  # using mapview
  ## map_shapes_mapview(mymapdata)  # requires the mapview package
}
########################### # ########################### # ########################### # ########################### # 
# ~ ####

# FUNCTIONS DEFINED ####

# mapfastej_counties()
# map_blockgroups_over_blocks()
# map_shapes_plot()
# map_shapes_leaflet()
# map_shapes_mapview()
# shapes_counties_from_countyfips()
# shapes_blockgroups_from_bgfips()



#' mapfastej_counties - Static or HTML/leaflet map of counties
#'
#' @param mydf something like  ejamit(fips = fips_counties_from_statename("Kentucky"), radius = 0)$results_bysite
#' @param colorvarname colname of indicator in mydf that drives color-coding
#' @param static_not_leaflet set TRUE to use [map_shapes_plot()] instead of [map_shapes_leaflet()]
#' @param main title for map
#' @param ... passed to map_shapes_plot() if relevant
#'
#' @return leaflet html widget (but if static_not_leaflet=T, 
#'   returns just shapes_counties_from_countyfips(mydf$siteid)) 
#' @export
#'
#' @examples \dontrun{
#'  fips_ky <- fips_counties_from_statename("Kentucky")
#'  x <- ejamit(fips = fips_ky, radius = 0)
#'  mapfastej_counties(x$results_bysite)
#'  }
mapfastej_counties <- function(mydf, colorvarname = "pctile.Demog.Index.Supp", 
                               static_not_leaflet = FALSE, main = "Selected Counties", ...) {
  
  # *** CANNOT HANDLE colorvarname = ANYTHING ELSE BESIDES THOSE SCALED 0 TO 100, SO FAR 
  
  # fips_ky <- fips_counties_from_statename("Kentucky")
  # x <- ejamit(fips = fips_ky, radius = 0)
  # mydf <- x$results_bysite
  
  mymapdata <- shapes_counties_from_countyfips(mydf$siteid)
  
  ## see color-coding of one percentile variable:
  pal <- colorBin(palette = c("yellow","yellow", "orange", "red"), bins = 80:100)
  shading <- pal(as.vector(unlist(mydf[ , ..colorvarname]))) 
  
  if (static_not_leaflet) {
    
    map_shapes_plot(mymapdata, main = main, ...) # or just # plot(mymapdata)
    plot(mymapdata, col = shading, add = TRUE)
    # to color code and flag the max value county:
    # flagged <- which.max(df[ , ..colorvarname])
    # plot(mymapdata[flagged, ], col = "purple", add = TRUE)
    mymap <- mymapdata # if ggplot, youd return the plot object but with plot() you cannot I think do that
    legend("topright", legend = c(80, 90, 100), fill = c("yellow", "orange", "red"), title = fixcolnames(colorvarname, 'rname', 'long'))
    
  } else {
    
    myindicators <- c(colorvarname, names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg)
    myindicators <- c(names(mydf)[1:9], myindicators)
    popindicators <- mydf[ , ..myindicators]
    popindicators <- table_round(popindicators) # decimal places set
    countynames <- fips2countyname(mydf$siteid)
    popindicators <- cbind(County = countynames, popindicators)
    poplabels <- fixcolnames(names(popindicators), 'r', 'long') # friendly labels for indicators
    popup2 <- popup_from_any(popindicators, labels = poplabels)
    
    
    mymap <- map_shapes_leaflet(mymapdata, popup = popup2, color = shading)
    mymap <- mymap %>% leaflet::addLegend(
      colors = c("yellow", "orange", "red"), 
      labels = c(80, 90, 100), 
      title = fixcolnames(colorvarname, 'rname', 'long'))
  }
  
  return(mymap)
}
########################### # ########################### # ########################### # ########################### # 


#' map_blockgroups_over_blocks - Overlay blockgroups near 1 site, after plotblocksnearby
#' Overlay blockgroups near 1 site, after plotblocksnearby(returnmap = TRUE)
#' @param y  output of [plotblocksnearby()] but with returnmap = TRUE
#'
#' @return leaflet map widget
#' @export
#' @seealso [plotblocksnearby()]  [map_shapes_mapview()]  [map_shapes_leaflet()]  [map_shapes_plot()]
#' @examples dontrun{
#'  y <- plotblocksnearby(testpoints_10[5,], 
#'         radius = 3,
#'         returnmap = TRUE)
#'  map_blockgroups_over_blocks(y)
#'   }
map_blockgroups_over_blocks <- function(y) {
  # y is output of plotblocksnearby(returnmap = TRUE)
  if ("leaflet" %in% class(y)) {
  # This is to extract bgids from the output of the leaflet htmlwidget map object y, 
  #   as from  y = plotblocksnearby(testpoints_10[1,], returnmap = TRUE)
  bgids <-  unique(as.vector(sapply( y$x$calls[[2]]$args[[7]], function(z)   gsub(   ".*bgid: ([0-9]*)<.*", "\\1", z))))
  } else {
    # can we still work with y if it was created with returnmap = FALSE ? 
    # bgids <- unique(y$bgid)
    stop('y must be output of something like plotblocksnearby(testpoints_10[1,], returnmap = TRUE)')
  }
  
  if (!exists("bgid2fips")) dataload_from_pins("bgid2fips")
  bgfips <- bgid2fips[bgid %in% bgids, bgfips] 
  x <- shapes_blockgroups_from_bgfips(bgfips) # but not for 60+ fips!  SLOW
  # add those FIPS shapes to the leaflet htmlwidget map 
  mymap <-   y %>% 
    leaflet::addGeoJSON(geojsonio::geojson_json(x), color = "green", group = "Blockgroups", data = x) %>% 
    leaflet::addLayersControl(overlayGroups = "Blockgroups")
  cat("Turn off the blockgroup boundaries layer using the map layer control button, to enable popup info for each block point.\n")
  return(mymap)
}
########################### # ########################### # ########################### # ########################### # 


#' map_shapes_plot
#'
#' @param shapes like from shapes_counties_from_countyfips()
#' @param main title for map
#' @param ... passed to plot()
#'
#' @return Just draws map using plot()
#' @export
#'
map_shapes_plot <- function(shapes, main = "Selected Census Units", ...) {
  plot(sf::st_combine(shapes), main = main, ...)
}
########################### # ########################### # ########################### # ########################### # 

#' map_shapes_leaflet
#'
#' @param shapes like from shapes_counties_from_countyfips()
#' @param color passed to leaflet::addPolygons()
#' @param popup  passed to leaflet::addPolygons()
#'
#' @return html widget from leaflet::leaflet()
#' @export
#'
map_shapes_leaflet <- function(shapes, color = "green", popup = shapes$NAME) {
  mymap <- leaflet(shapes) %>% addPolygons(color = color, popup = popup) %>% addTiles()
  return(mymap)
}
########################### # ########################### # ########################### # ########################### # 

#' map_shapes_mapview
#'
#' @param shapes like from shapes_counties_from_countyfips()
#' @param col.regions passed to mapview() from mapview package
#' @param map.types  passed to mapview() from mapview package
#'
#' @export
#'
map_shapes_mapview <- function(shapes, col.regions = "green", map.types = "OpenStreetMap") {
  message("this function is a nice way to map counties etc. but requires the mapview package, which EJAM does not load")
  mapview(shapes, col.regions = col.regions, map.types = map.types)
}
########################### # ########################### # ########################### # ########################### # 

#' use API to get boundaries of US Counties to map them
#'
#' @param countyfips FIPS codes as 5-character strings (or numbers) in a vector 
#' @param outFields can be "*" for all, or can be 
#'   just some variables like SQMI, POPULATION_2020, etc., or none
#' @param myservice URL of feature service to get shapes from.
#'   Only default was tested
#'
#' @return spatial object via sf::read_sf()
#' @export
#'
shapes_counties_from_countyfips <- function(countyfips = '10001', outFields = "",
                                            myservice = c(
                                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/2/query",
                                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_and_States_with_PR/FeatureServer/0/query",
                                              "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_22_US_Percentiles_Tracts/FeatureServer/query")[1]
) {
  # for a vector of  FIPS, use arcgis API to obtain map boundaries of just those census units
  
  if (length(countyfips) > 50) {
    # The API does let you get >50 at once but instead of figuring out that syntax, this function works well enough
    batchsize <- 50
    batches <- 1 + (length(countyfips) %/% batchsize)
    # ***  add code here to handle 50 at a time and assemble them
    out <- list()
    for (i in 1:batches) {
      first <- 1 + ((i - 1) * batchsize)
      last <- min(first + batchsize - 1, length(countyfips))
      out[[i]] <- shapes_counties_from_countyfips(countyfips[first:last], outFields = outFields, myservice = myservice)
    }
    out <- do.call(rbind, out)
    return(out)
  }
  
  if (grepl("ejscreen", myservice, ignore.case = TRUE)) {FIPSVARNAME <- "ID"} else {FIPSVARNAME <- "FIPS"}
  myurl <- httr2::url_parse(myservice)
  myurl$query <- list(
    where = paste0(paste0(FIPSVARNAME, "='", countyfips, "'"), collapse = " OR "),  ########################### # 
    outFields = outFields,
    returnGeometry = "true",
    f = "geojson")
  request <- httr2::url_build(myurl)
  mymapdata <- sf::read_sf(request)
  return(mymapdata)
}
########################### # ########################### # ########################### # ########################### # 

#' use API to get boundaries of blockgroups
#' @details This is useful mostly for small numbers of blockgroups.  
#'   The EJScreen map services provide other ways to map blockgroups and see EJScreen data.
#' @param bgfips one or more block group FIPS codes as 12-character strings in a vector
#' @param outFields can be "*" for all, or can be 
#'   just some variables like SQMI, POPULATION_2020, etc., or none
#' @param myservice URL of feature service to get shapes from. 
#' 
#'   "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/
#'   EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0/query"
#'
#'   for example provides EJScreen indicator values, NPL_CNT, TSDF_CNT, EXCEED_COUNT_90, etc.
#'   
#' @return spatial object via sf::read_sf()
#' @export
#'
shapes_blockgroups_from_bgfips <- function(bgfips = '010890029222', outFields = "",
                                           myservice = c(
                                             "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5/query",
                                             "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Block_Groups/FeatureServer/0/query",
                                             "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0/query")[1]
) {
  # for a vector of blockgroup FIPS, use arcgis API to obtain map boundaries of just those blockgroups
  
  if (length(bgfips) > 50) {
    
    # The API does let you get >50 at once but instead of figuring out that syntax, this function works well enough
    batchsize <- 50
    batches <- 1 + (length(bgfips) %/% batchsize)
    # ***  add code here to handle 50 at a time and assemble them
    out <- list()
    for (i in 1:batches) {
      first <- 1 + ((i - 1) * batchsize)
      last <- min(first + batchsize - 1, length(bgfips))
      out[[i]] <- shapes_blockgroups_from_bgfips(bgfips[first:last], outFields = outFields, myservice = myservice)
    }
    out <- do.call(rbind, out)
    return(out)
    # warning("Cannot get so many blockgroup shapes in one query, via this API, as coded! Using first 50 only.")
    # bgfips <- bgfips[1:50]
  }
  
  if (grepl("ejscreen", myservice, ignore.case = TRUE)) {FIPSVARNAME <- "ID"} else {FIPSVARNAME <- "FIPS"}
  myurl <- httr2::url_parse(myservice)
  myurl$query <- list(
    where = paste0(paste0(FIPSVARNAME, "='", bgfips, "'"), collapse = " OR "),  ########################### # 
    outFields = outFields,
    returnGeometry = "true",
    f = "geojson")
  request <- httr2::url_build(myurl)
  mymapdata <- sf::read_sf(request)
  return(mymapdata)
}
########################### # ########################### # ########################### # ########################### # 


# ~ ####

# NOTES on map services ####

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


