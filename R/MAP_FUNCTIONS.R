
# SEVERAL FUNCTIONS ARE IN THIS FILE ####
#
# See source document outline for list of functions defined here ####

#  seealso mapfast() mapfastej() ejam2map()  

########################### # ########################### # ########################### # ########################### #


#' Map - points - Create leaflet map of points, in shiny app
#'
#' @description make a leaflet map of uploaded points such as facilities
#' @param mypoints, data frame of uploaded points
#' @param rad, a size for drawing each circle (buffer search radius)
#' @param highlight, a logicial for whether to highlight overlapping points (defaults to FALSE)
#' @param clustered, a vector of T/F values for each point, indicating if they overlap with another
#'
#' @return a leaflet map with circles, circleMarkers, and basic popup
#' @seealso [mapfast()] [map_facilities_proxy()]
#'
#' @export
#'
map_facilities <- function(mypoints, rad = 3, highlight = FALSE, clustered) {

  #, map_units = 'miles'){

  ## map settings
  base_color      <- '#000080'
    cluster_color   <- 'red'
      highlight_color <- 'orange'   #  NOT USED YET
        circleweight <- 4

        ## convert units to miles for circle size
        # if (map_units == 'kilometers'){
        #   rad = rad * 0.62137119
        # }

        ## if checkbox to highlight clusters is checked
        if (highlight == TRUE) {
          ## compare latlons using is_clustered() reactive
          circle_color <- ifelse(clustered == TRUE, cluster_color, base_color)
        } else {
          circle_color <- base_color
        }
        # print(head(mypoints))

        if (length(mypoints) != 0) {
          #isolate({ # do not redraw entire map and zoom out and reset location viewed just because radius changed?

          #if (circle_type == 'circles'){
          mymap <- leaflet::leaflet(mypoints) %>%
            addTiles()  %>%
            addCircles(
              #radius = input$radius * meters_per_mile,
              radius = rad * meters_per_mile,
              color = circle_color, fillColor = circle_color,
              fill = TRUE, weight = circleweight,
              group = 'circles',
              popup = popup_from_any(mypoints)

            ) %>%
            addCircleMarkers(
              #radius = input$radius * meters_per_mile,
              radius = rad,
              color = circle_color, fillColor = circle_color,
              fill = TRUE, weight = circleweight,
              clusterOptions = markerClusterOptions(),
              popup = popup_from_any(mypoints)
              ## possible way to use circleMarkers - need conversion of meters to pixels so they scale properly
              #meters_per_px <- 156543.03392 * cos(mean(m$x$limits$lat) * pi/180) / m$x$options
            ) %>%
            groupOptions(group = 'markers', zoomLevels = 1:6) %>%
            groupOptions(group = 'circles', zoomLevels = 6:20) %>%
            leaflet.extras::addFullscreenControl()

          ## return map
          mymap

          #})
        } else {  # length(mypoints) == 0
          mymap <- leaflet() %>%
            addTiles() %>%
            setView(-110, 46, zoom = 3)
          mymap
        }
        ### Button to print map 
        leaflet.extras2::addEasyprint(map = mymap, options = leaflet.extras2::easyprintOptions(exportOnly = TRUE, title = 'Save Map Snapshot'))
}
########################### # ########################### # ########################### # ########################### #


#' Map - points - Update leaflet map of points, in shiny app
#'
#' @description update a leaflet map within the EJAM shiny app with uploaded points such as facilities
#' @param mymap, leafletProxy map object to be added to
#' @param rad, a size for drawing each circle (buffer search radius)
#' @param highlight, a logicial for whether to highlight overlapping points (defaults to FALSE)
#' @param clustered, a vector of T/F values for each point, indicating if they overlap with another
#' @param popup_vec, a vector of popup values to display when points are clicked. Length should match number of rows in the dataset.
#' @param use_marker_clusters, boolean for whether to group points into markerClusters. Uses logic from shiny app to only implement when n > 1000.
#' @seealso [map_facilities()]
#' @return a leaflet map with circles, circleMarkers, and basic popup
#'
#' @export
#'
map_facilities_proxy <- function(mymap, rad = 3, highlight = FALSE, clustered = FALSE,
                                 popup_vec = NULL, use_marker_clusters = FALSE) {

  ## map settings
  base_color      <- '#000080'
    cluster_color   <- 'red'
      circleweight <- 4

      ## if checkbox to highlight clusters is checked
      if (highlight == TRUE) {
        ## compare latlons using is_clustered() reactive
        circle_color <- ifelse(clustered == TRUE, cluster_color, base_color)
      } else {
        circle_color <- base_color
      }

      if (use_marker_clusters == FALSE) {
        ## add to leafletProxy call from Shiny app
        mymap <- mymap %>%
          clearShapes() %>%
          addCircles(
            radius = rad * meters_per_mile,
            color = circle_color, fillColor = circle_color,
            fill = TRUE, weight = circleweight,
            group = 'circles',
            popup = popup_vec
          ) %>%
          leaflet.extras::addFullscreenControl()
      } else {
        ## add to leafletProxy call from Shiny app
        mymap <- mymap %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          addCircles(
            radius = rad * meters_per_mile,
            color = circle_color, fillColor = circle_color,
            fill = TRUE, weight = circleweight,
            group = 'circles',
            popup = popup_vec
          ) %>%
          addCircleMarkers(
            radius = 0,
            color = circle_color, fillColor = circle_color,
            fill = TRUE, weight = circleweight,
            clusterOptions = markerClusterOptions(),
            popup = popup_vec
          ) %>%
          groupOptions(group = 'markers', zoomLevels = 1:6) %>%
          groupOptions(group = 'circles', zoomLevels = 7:20) %>%
          leaflet.extras::addFullscreenControl()
      }
      ## return map
      mymap
}
########################### # ########################### # ########################### # ########################### #


#' Map - County polygons / boundaries - Create leaflet or static map
#'
#' @param mydf something like  ejamit(fips = fips_counties_from_statename("Kentucky"), radius = 0)$results_bysite
#' @param colorvarname colname of indicator in mydf that drives color-coding
#' @param static_not_leaflet set TRUE to use [map_shapes_plot()] instead of [map_shapes_leaflet()]
#' @param main title for map
#' @param ... passed to map_shapes_plot() if relevant
#' @details THIS ASSUMES THAT mydf$ejam_unique_id is the county FIPS codes
#' @seealso [mapfastej()] [map_shapes_leaflet()]
#' @return leaflet html widget (but if static_not_leaflet=T,
#'   returns just shapes_counties_from_countyfips(mydf$ejam_uniq_id))
#' @examples \dontrun{
#'  fips_ky <- fips_counties_from_statename("Kentucky")
#'  x <- ejamit(fips = fips_ky, radius = 0)
#'  mapfastej_counties(x$results_bysite)
#'  }
#'  # map_shapes_leaflet(shapes = shapes_counties_from_countyfips(fips_ky))
#'
#' @export
#'
mapfastej_counties <- function(mydf, colorvarname = "pctile.Demog.Index.Supp",
                               static_not_leaflet = FALSE, main = "Selected Counties", ...) {

  # *** CANNOT HANDLE colorvarname = ANYTHING ELSE BESIDES THOSE SCALED 0 TO 100, SO FAR

  # fips_ky <- fips_counties_from_statename("Kentucky")
  # x <- ejamit(fips = fips_ky, radius = 0)
  # mydf <- x$results_bysite

  if(!(colorvarname %in% names(mydf))){
    warning('Selected value for "colorvarname" not found. Please try a different indicator.')
    return(NULL)
  }
  
  mymapdata <- shapes_counties_from_countyfips(mydf$ejam_uniq_id)
setDT(mydf)
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
    legend("topright", legend = c(80, 90, 100), fill = c("yellow", "orange", "red"), title = fixcolnames(colorvarname, 'rname', 'shortlabel'))

  } else {

    myindicators <- c(colorvarname, names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg)
    myindicators <- c(names(mydf)[1:9], myindicators)
    popindicators <- mydf[ , ..myindicators]
    popindicators <- table_round(popindicators) # decimal places set
    countynames <- fips2countyname(mydf$ejam_uniq_id)
    popindicators <- cbind(County = countynames, popindicators)
    poplabels <- fixcolnames(names(popindicators), 'r', 'shortlabel') # friendly labels for indicators
    popup2 <- popup_from_any(popindicators, labels = poplabels)


    mymap <- map_shapes_leaflet(mymapdata, popup = popup2, color = shading)
    mymap <- mymap %>% leaflet::addLegend(
      colors = c("yellow", "orange", "red"),
      labels = c(80, 90, 100),
      title = fixcolnames(colorvarname, 'rname', 'shortlabel'))
  }

  return(mymap)
}
########################### # ########################### # ########################### # ########################### #


#' Map - Blockgroup polygons / boundaries near 1 site - Create leaflet map
#'
#' Overlay blockgroups near 1 site, after plotblocksnearby(returnmap = TRUE)
#'
#' @param y  output of [plotblocksnearby()] but with returnmap = TRUE
#'
#' @return leaflet map widget
#' @seealso [plotblocksnearby()]  [map_shapes_mapview()]  [map_shapes_leaflet()]  [map_shapes_plot()]
#' @examples
#'  y <- plotblocksnearby(testpoints_10[5,],
#'         radius = 3,
#'         returnmap = TRUE)
#'  map_blockgroups_over_blocks(y)
#'
#' @export
#'
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


#' Map - polygons - Use base R plot() to map polygons
#'
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param main title for map
#' @param ... passed to plot()
#'
#' @return Just draws map using plot()
#'
#'
map_shapes_plot <- function(shapes, main = "Selected Census Units", ...) {

  plot(sf::st_combine(shapes), main = main, ...)
}
########################### # ########################### # ########################### # ########################### #


#' Map - polygons - Create leaflet map from shapefile, in shiny app
#'
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param color passed to leaflet::addPolygons()
#' @param popup  passed to leaflet::addPolygons()
#'
#' @return html widget from leaflet::leaflet()
#'
#' @export
#'
map_shapes_leaflet <- function(shapes, color = "green", popup = shapes$NAME) {

  mymap <- leaflet(shapes) %>% addPolygons(color = color, popup = popup) %>% addTiles()
  return(mymap)
}
########################### # ########################### # ########################### # ########################### #

#' Map - polygons - Update leaflet map by adding shapefile data, in shiny app
#'
#' @param mymap map like from leafletProxy()
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param color passed to leaflet::addPolygons()
#' @param popup passed to leaflet::addPolygons()
#'
#' @return html widget like from leaflet::leafletProxy()
#'
#' @export
#'
map_shapes_leaflet_proxy <- function(mymap, shapes, color = "green", popup = shapes$NAME)  {

  mymap <- mymap %>%
    addPolygons(data = shapes, color = color,  popup = popup) %>%
    addTiles()
  return(mymap)
  }
########################### # ########################### # ########################### # ########################### #


#' Map - polygons - Use mapview package if available
#'
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param col.regions passed to [mapview::mapview()]
#' @param map.types  passed to  [mapview::mapview()]
#' @return like output of mapview function [mapview::mapview()],
#'   if mapview package is installed,
#'   when used with an input that is a spatial object as via [sf::read_sf()]
#' @examples \dontrun{
#'   map_shapes_mapview(
#'     shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#'   )
#' }
#'
#' @export
#'
map_shapes_mapview <- function(shapes, col.regions = "green", map.types = "OpenStreetMap") {

  if (!"package:mapview" %in% search()) {
    message("this function is a nice way to map counties etc. but requires the mapview package, which EJAM does not load")
    warning("mapview package would be needed and is not attached - checking if installed")
    junk <- try(find.package("mapview"), silent = TRUE)
    if (inherits(junk, "try-error")) {
      warning("mapview package does not appear to be installed")
      return(NULL)
    } else {
      warning("mapview package appears to be installed but not attached. Try using library(mapview) or require(mapview)")
      return(NULL)
    }
  } else {
    mapview(shapes, col.regions = col.regions, map.types = map.types)
  }
}
########################### # ########################### # ########################### # ########################### #


#' Get Counties boundaries via API, to map them
#'
#' @details Used [sf::read_sf()], which is an alias for [sf::st_read()]
#'   but with some modified default arguments.
#'   read_sf is quiet by default/ does not print info about data source, and
#'   read_sf returns an sf-tibble rather than an sf-data.frame
#'   
#'   Also note the tidycensus and tigris R packages.
#'
#' @param countyfips FIPS codes as 5-character strings (or numbers) in a vector
#'   as from fips_counties_from_state_abbrev("DE")
#' @param outFields can be "*" for all, or can be
#'   just some variables like SQMI, POPULATION_2020, etc., or none
#' @param myservice URL of feature service to get shapes from.
#'   Only default was tested
#'
#' @return spatial object via [sf::st_read()]
#'
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
  mymapdata <- sf::st_read(request) # st_read returns data.frame, read_sf returns tibble
  return(mymapdata)
}
########################### # ########################### # ########################### # ########################### #


#' Get blockgroups boundaries, via API, to map them
#'
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
#' @seealso [mapfast()]
#' @return spatial object via [sf::st_read()] # sf-data.frame, not sf-tibble like [sf::read_sf()]
#'
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
  mymapdata <- sf::st_read(request) # data.frame not tibble
  return(mymapdata)
}
########################### # ########################### # ########################### # ########################### #


#' Map - points - ggplot2 map of points in the USA - very basic map
#'
#' @param mydf data.frame with columns named lat and lon
#' @param dotsize optional, size of dot representing a point
#' @param ptcolor optional, color of dot
#' @param xlab optional, text for x label
#' @param ylab optional, text for y label
#' @param ... optional, passed to [ggplot2::labs()]
#'
#' @return a ggplot() object
#'
#' @examples \dontrun{
#'   mapfast_gg(EJAM::testpoints_10)
#'
#'   pts <- read.table(textConnection(
#'   "lat lon
#'   39.5624775 -119.7410994
#'   42.38748056 -94.61803333"
#'   ),
#'   header = TRUE,
#'   as.is = TRUE
#'   )
#'   mapfast_gg(pts)
#'   # str(pts) # lon, not long
#'   }
#'
#' @export
#'
mapfast_gg <- function(mydf=data.frame(lat = 40, lon = -100)[0,],
                       dotsize = 1, ptcolor = "black",
                       xlab = "Longitude", ylab = "Latitude", ...) {

  plotout <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = ggplot2::map_data("usa"), 
                          # Note the ggplot2 "usa" dataset 
                          # longitude is called "long" but mydf calls it "lon"
                          ggplot2::aes(x = long, y = lat, group = group), fill = "gray", alpha = 0.75) +
    ggplot2::geom_point(  data = mydf, 
                          ggplot2::aes(x = lon, y = lat), color = ptcolor, size = dotsize) +
    ggplot2::labs(x = xlab, y = ylab, ...)
  return(plotout)
}
############################ #


# color coded map by FIPS code
#
# state resolution map example is from  https://leafletjs.com/examples/choropleth/
#
# var map = L.map('map').setView([37.8, -96], 4);
#
# var tiles = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
#   maxZoom: 19,
#   attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
# }).addTo(map);
#
# L.geoJson(statesData).addTo(map);
#
############################ #

