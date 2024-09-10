

#' ejamit_sitetype_check helper for ejamit() to check what type of sites were specified
#'
#' @param sitepoints  parameter as was passed to [ejamit()]
#' @param fips  parameter as was passed to [ejamit()]
#' @param shapefile parameter as was passed to [ejamit()]
#'
#' @return either "latlon", "fips", or "shp",
#'   or errors if 2 or 3 types were specified at once
#' 
#' @keywords internal
#' @noRd
#'
ejamit_sitetype_check <- function(sitepoints, fips=NULL, shapefile=NULL) { 
  
  if (!is.null(shapefile)  ) {
    sitetype <- "shp"
  } else if (!is.null(fips)  ) {
    sitetype <- "fips"
  } else {
    sitetype <- "latlon" # if none of 3 is specified, tries to interactively select file of latlon
  }
  if (sum(!missing(sitepoints), !is.null(shapefile), !is.null(fips)) > 1) {
    stop("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. ")
    ## or, if we want to warn instead of stop, could use only latlon when avail even if fips and/or shp was also erroneously specified, & use shp if only shp&fips specified.
    # if (!missing(sitepoints)) {sitetype <- "latlon"} 
    # warning("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. Using sitepoints if provided. If not, ignoring fips and using shapefile.")
  }
  
  if (sitetype == "latlon" && missing(sitepoints) && interactive() && !isRunning()) {
    message("ejamit() will try to help select a latlon file")
  }
  return(sitetype)
}
############################ ############################# #
