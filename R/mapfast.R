

#' Map - points - Create leaflet html widget map of points using EJAM results with EJ stats
#'
#' Like [mapfast()] but with column_names = "ej"
#'
#' @inheritParams mapfast
#' @return like what [mapfast()] returns
#' @export
#'
mapfastej <- function(...) {

  mapfast(..., column_names = 'ej')
}
############################################################################ #


#' Map - points - Create leaflet html widget map of points using table with lat lon
#'
#' @param mydf data.frame or data.table with lat and lon columns or
#'   columns that [latlon_infer()] can infer to be that
#' @param radius in miles, converted to meters and passed to leaflet::addCircles()
#' @param column_names If "ej" then nice popup made based on just key EJScreen
#'   indicators. If "all" then every column in the entire mydf table is shown
#'   in the popup. If a vector of colnames, only those are shown in popups.
#' @param labels The labels used before the column_names, for map popups,
#'   like  label: column_name  (ignored if column_names is ej or all)
#' @seealso [ejam2map()] [popup_from_df()] [mapfastej()]
#' @return plots a leaflet map with popups with all the columns from mydf,
#'   and returns html widget
#' @import leaflet
#' @import leaflet.extras2
#' @examples
#'  mapfast(testpoints_500)
#'  \dontrun{
#'  
#'  mapfastej(testoutput_ejamit_1000pts_1miles$results_bysite, radius = 1)
#'  
#'  ejam2map(testoutput_ejamit_1000pts_1miles)
#'  
#'  # see random sample of 1k FRS facilities
#'  if (!exists("frs")) {
#'    x <- try(require(EJAM)); if (!inherits(x, "try-error")) {
#'      dataload_from_pins("frs")
#'    }
#'    mydf <-  frs[sample(1:NROW( frs), 1000), 1:5]
#'    mapfast(mydf)
#'  }
#'  # out <- EJAMejscreenapi::testoutput_ejscreenit_50$table
#'  # names(out) <- fixcolnames(names(out), 'long', 'r')
#'  out <- testoutput_ejscreenapi_plus_50
#'  mapfastej(out)
#'  mapfast(out, column_names = 'ej')
#'  mapfast(out)
#'
#'  # Save .html file and view it in your browser
#'
#'  mytable <- testoutput_ejscreenit_5$table
#'  # names(mytable) <- fixcolnames(names(mytable), 'long', 'r')
#'  ## now, that renaming is done by mapfast function
#'  mymap <- mapfastej(mytable)
#'  tfile <- paste0(tempfile(), '.html')
#'  htmltools::save_html(mymap, file = tfile)
#'  browseURL(tfile)
#'   }
#'
#' @export
#'
mapfast <- function(mydf, radius = 3, column_names='all', labels = column_names) {

  if (data.table::is.data.table(mydf)) {mydf <- as.data.frame(mydf)} # in case it was a data.table
  renamed <- mydf # use new names for lat and lon if necessary, but show original names in popup
  names(renamed) <- latlon_infer(names(renamed))
  if (all(is.na(renamed$lat)) | all(is.na(renamed$lon))) {
    warning('no valid lat lon values to map')
    # maybe could infer FIPS from siteid here and do choropleth color coded map?

    return(NA)
  }

  if (column_names[1] == 'ej') {
    
    ejcols <- c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state)
    if(!all(ejcols %in% names(mydf))){
      warning('Not all EJ columns found. Please provide a different dataset.')
      return(NA)
    }
    # popup_from_ejscreen() code was written to assume rnames (as from ejscreenapi_plus) not longnames (as from ejscreenit),
    # so try to accomodate that here if user provided output of ejscreenit() or long names in general
    # popup_from_ejscreen() needs to flexibly allow long format names as input.
    # ejscreenit() and app_server already handle this issue by renaming to rnames before calling popup_from_ejscreen()
    names(mydf) <- fixcolnames(names(mydf), "long", "r") # if already r, this does nothing. if long, it makes them r format so popup_from_ejscreen() will work
    mypop <- popup_from_ejscreen(mydf)
  } else if (column_names[1] == 'all') {
    mypop <- popup_from_df(mydf)
  } else {
      if(!all(column_names %in% names(mydf))){
        warning('Not all column_names found. Mapping without popups. Please provide a different list to include popups.')
        mypop <- NULL
      } else {
        mypop <- popup_from_df(mydf, column_names = column_names, labels = labels)
      }
  }

  radius.meters <- radius * meters_per_mile # data loaded by pkg EJAMejscreenapi
  # units are meters for addCircles, and pixels for addCircleMarkers

  leaflet::leaflet(data = renamed) |> leaflet::addTiles() |>
    leaflet::addCircles(lng = ~lon, lat = ~lat, radius = radius.meters,
                        popupOptions = list(maxHeight = 400, maxWidth = 850),
                        popup = mypop) |>
    leaflet.extras2::addEasyprint( ) # button to print or print to pdf and save
}
############################################################################ #
