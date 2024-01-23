
# SEVERAL FUNCTIONS ARE IN THIS FILE ####
#
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
#'   returns just shapes_counties_from_countyfips(mydf$ejam_uniq_id)) 
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
  
  mymapdata <- shapes_counties_from_countyfips(mydf$ejam_uniq_id)
  
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
    countynames <- fips2countyname(mydf$ejam_uniq_id)
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
#' Create a new leaflet map from shapefile data
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
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

#' map_shapes_leaflet_proxy
#' update existing leaflet map by adding shapefile data
#' @param mymap map like from leafletProxy()
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param color passed to leaflet::addPolygons()
#' @param popup passed to leaflet::addPolygons()
#'
#' @return html widget like from leaflet::leafletProxy()
#' @export
#'
map_shapes_leaflet_proxy <- function(mymap, shapes, color = "green", popup = shapes$NAME)  {
  mymap <- mymap %>% 
    addPolygons(data = shapes, color = color,  popup = popup) %>% 
    addTiles() 
  return(mymap)
  }
########################### # ########################### # ########################### # ########################### # 

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


