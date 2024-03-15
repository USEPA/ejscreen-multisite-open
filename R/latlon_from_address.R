

#' geocode, but only if AOI package is already installed and loaded and attached
#'
#' @param address vector of addresses, but tested only for 1
#' @param xy set it to TRUE if you want only x,y returned, see help for AOI pkg
#' @param pt  see help for AOI pkg, return geometry if set to TRUE, allowing map.
#'   param as provided is ignored and set to TRUE if aoimap=TRUE
#' @param aoimap  see help for AOI pkg, create map if set to TRUE
#' @param ...  passed to geocode() see help for AOI pkg
#'
#' @return returns tibble table of x,y or lat,lon values or geometries.
#'   see helpf for AOI pkg
#' @examples 
#'   # only works if AOI package installed already and attached too
#'   # x <- geocode(c("1200 Pennsylvania Ave, NW Washington DC", "Research Triangle Park"))
#'   # out <- ejamit(x, radius = 3)
#' 
#' @export
#'
latlon_from_address <- function(address, xy=FALSE, pt = FALSE, aoimap=FALSE, ...) {
  
  ############################################## #
  # make AOI package only optional ####
  ### all these are imported by AOI pkg that were not yet needed by EJAM:
  #
  #   c("datasets", "fipio", "htmlwidgets", "jsonlite", "leaflet.extras", 
  #   "rnaturalearth", "rvest", "shiny", "terra", "tidygeocoder", "units")
  #
  # if (!require("AOI")) { 
  #   remotes::install_github("mikejohnson51/AOI") #
  # } 
  x <- try(find.package("AOI"))
  if (inherits(x, "try-error")) {
    warning("AOI package not available")
    x <- NULL 
    ############################################## #
  } else {
    cat('for this to work you would need to use library(', 'AOI', ') first\n')
    # how to make it attached or used without triggering renv or packrat to think we want to import or depend on it?
    
    # x <- geocode(c("1200 Pennsylvania Ave, NW Washington DC", "Dupont Circle", "Research Triangle Park"))
    
    if (length(address) > 25) {stop("only 25 max supported in this function until decide if more ok")}
    
    if (aoimap) {
      x <- geocode(address, pt = TRUE, ...)   |> aoi_map()   # AOI:: # avoid making renv think we require it
    } else {
      x <- geocode(address, pt = pt, ...) 
    }
    # out = ejamit(x, radius = 1)
  }
  
  return(x)
}
