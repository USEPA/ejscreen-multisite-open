

#' Map - points - Create leaflet html widget map of points using EJAM results with EJ stats
#'
#' Like [mapfast()] but with column_names = "ej"
#'
#' @inheritParams mapfast
#' @inherit ejam2map examples
#' @return like what [mapfast()] returns
#' @export
#'
mapfastej <- function(mydf, radius = 3, column_names = 'ej', labels = column_names, browse = FALSE, color = "#03F") {

  mapfast(mydf = mydf, radius = radius, column_names = column_names, labels = labels, browse = browse, color = color)
}
############################################################################ #


#' Map - points - Create leaflet html widget map of points using table with lat lon
#'
#' @param mydf data.frame or data.table with lat and lon columns or
#'   columns that [latlon_infer()] can infer to be that,
#'   or where ejam_uniq_id values are US County FIPS codes
#' @param radius in miles, converted to meters and passed to leaflet::addCircles()
#' @param column_names If "ej" then nice popup made based on just key EJScreen
#'   indicators. If "all" then every column in the entire mydf table is shown
#'   in the popup. If a vector of colnames, only those are shown in popups.
#' @param labels The labels used before the column_names, for map popups,
#'   like  label: column_name  (ignored if column_names is ej or all)
#' @param browse optional logical, set to TRUE if you want the function to
#'   launch a default browser window to show the map
#'   and print the temp filepath and filename in the console.
#'   Normally the map would be shown in the default RStudio viewer pane.
#' @param color color of circles, default is "#03F"
#' @seealso [ejam2map()] [popup_from_any()] [mapfastej()]
#' @return plots a leaflet map with popups with all the columns from mydf,
#'   and returns html widget
#' @import leaflet
#' @import leaflet.extras2
#' @inherit ejam2map examples
#'
#' @export
#'
mapfast <- function(mydf, radius = 3, column_names='all', labels = column_names, browse = FALSE, color = "#03F") {
  
  if (data.table::is.data.table(mydf)) {mydf <- as.data.frame(mydf)} # in case it was a data.table
  
  # colnames ####
  ## infer lat lon cols (if they exist) ####
  # use new names for lat and lon just to check those values and to use this as data sent to leaflet, but show fixcolnames for names in popup
  renamed <- mydf 
  names(renamed) <- latlon_infer(names(renamed))
  ## fixcolnames() ####
  # use standardized format of indicator names in case they are the long versions of ejamit variable names
  names(mydf) <- fixcolnames(names(mydf), "long", "r") # if already r, this does nothing. if long, it makes them r format so popup_from_ejscreen() will work
  ## 
  # I think this is ok, since it is just for the popups to work right if mydf was output of analysis but used the long form variable names as headers
  # popup_from_ejscreen() code was written to assume rnames (as from ejscreenapi_plus) not longnames (as from ejscreenit),
  # so try to accomodate that here if user provided output of ejscreenit() or long names in general
  # popup_from_ejscreen() needs to flexibly allow long format names as input.
  # ejscreenit() and app_server already handle this issue by renaming to rnames before calling popup_from_ejscreen()
  
  ######################################################### #
  
  # popup text ####
  
  if (column_names[1] == 'ej') {
    
    ejcols <- c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state)
    if (!all(ejcols %in% names(mydf))) {
      warning('Not all EJ index columns found. Using NA values for all EJ indexes in map popups.')
      ejna <- data.frame(matrix(ncol = length(ejcols), nrow = NROW(mydf)))
      names(ejna) <- ejcols
      mydf <- cbind(mydf, ejna)
    }
    
    mypop <- popup_from_ejscreen(sf::st_drop_geometry(mydf))
    
  } else if (column_names[1] == 'all') {
    mypop <- popup_from_df(sf::st_drop_geometry(mydf))
  } else {
    if (!all(column_names %in% names(sf::st_drop_geometry(mydf)))) {
      warning('Not all column_names found. Using actual colnames of table.')
      mypop <- popup_from_df(sf::st_drop_geometry(mydf))
    } else {
      mypop <- popup_from_df(sf::st_drop_geometry(mydf), column_names = column_names, labels = labels)
    }
  }
  ######################################################### #
  
  xok = FALSE
  
  ######################################################### #
  
  # *SHP ####
  ## map polygons 
  # ignore latlon and FIPS if shapefile was provided
  
  if ("sf" %in% class(mydf)) {
    
    # cat('For analysis and map of shapefile data, you can try something like this:
    #   
    #       shp <- shapefile_from_any()
    #       m <- map_shapes_leaflet(shp)
    #       # or
    #       out <- ejamit(shapefile = shp)
    #       ejam2map(out)
    #       # or
    #        mapfast(out)
    #       # or
    #       shp <- ejam2shapefile(out, save = FALSE)
    #       map_shapes_leaflet(shp, popup = popup_from_ejscreen(out))
    #       
    #       ')
    
    # # for now, assume if most of the colnames in mydf are found in ejamit output table, treat popups like from ejamit
    # if (length(intersect(names(mydf), names(testoutput_ejamit_10pts_1miles$results_overall))) > length(names(mydf)) / 2 ) {
    #   pop <- popup_from_ejscreen(sf::st_drop_geometry(mydf))
    # } else {
    #   pop <- popup_from_any(sf::st_drop_geometry(mydf))
    # }
    
    x <- map_shapes_leaflet(mydf, popup = mypop)
    xok = TRUE
  }
  
  ######################################################### #
  
  # *LATLON not shp (ignore FIPS) ####
  
  if (!("sf" %in% class(mydf)) &
      !all(is.na(renamed$lat)) && !all(is.na(renamed$lon))) {
    
    radius.meters <- radius * meters_per_mile # data loaded by pkg
    # units are meters for addCircles, and pixels for addCircleMarkers
    
    x <- leaflet::leaflet(data = renamed) |> leaflet::addTiles() |>
      leaflet::addCircles(lng = ~lon, lat = ~lat, radius = radius.meters, color = color,
                          popupOptions = list(maxHeight = 400, maxWidth = 850),
                          popup = mypop) |>
      leaflet.extras2::addEasyprint( ) # button to print or print to pdf and save
    xok = TRUE
    # now x is a map
  }
  ######################################################### #
  
  # *FIPS not shp not latlon ####
  
  if (!("sf" %in% class(mydf)) &
      all(is.na(renamed$lat)) || all(is.na(renamed$lon))) {
    
    # _County  ####
    # get boundaries  
    
    if ("ejam_uniq_id" %in% names(mydf) &&
        all(fipstype(mydf$ejam_uniq_id) %in% 'county')) {
      x <- mapfastej_counties(mydf) # handles the popups itself, ignores params above
      xok = TRUE
      # now x is a map
    }
    
    # _City/CDP  #### 
    # get boundaries 
    
    if ("ejam_uniq_id" %in% names(mydf) &&
        all(fipstype(mydf$ejam_uniq_id) %in% 'place')) {
      mydf <- shapes_places_from_placefips(mydf$ejam_uniq_id)
      # now mydf is a shapefile
      x <- map_shapes_leaflet(mydf, popup = mypop)
      xok = TRUE
      # now x is a map
    }
    
    # blockgroup FIPS?? 
    if ("ejam_uniq_id" %in% names(mydf) &&
        all(fipstype(mydf$ejam_uniq_id) %in% 'blockgroup')) {
      # could map if 1st get bounds as in map_blockgroups_over_blocks()
      warning('mapping blockgroup boundaries here is not implemented\n')
    }
    
    # tract FIPS??
    if ("ejam_uniq_id" %in% names(mydf) &&
        all(fipstype(mydf$ejam_uniq_id) %in% 'tract')) {
      # could map if 1st get bounds 
      warning('mapping tract boundaries here is not implemented\n')
    }
    
  }
  ######################################################## #
  
  # ?CANT MAP?  ####
  # if not SHP and no latlon and no usable FIPS, 
  # geocode street addresses?  handle that outside this function.
  
  if (!xok) {
    
    warning('no valid lat lon values to map')
    return(NA)
  }
  
  ######################################################## #
  
  # see in browser ####
  
  if (browse) {
    # map2browser() would do the same
    htmlwidgets::saveWidget(x, file = fname <- tempfile("mapfast_", fileext = ".html"))
    # htmltools::save_html(x, file = fname <- tempfile("mapfast_", fileext = ".html"))  # might work also?
    browseURL(fname)
    cat(fname, "\n")
  }
  
  return(x)
}
############################################################################ #
