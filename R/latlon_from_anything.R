
#' Get lat/lon flexibly - from file, data.frame, data.table, or lat/lon vectors
#'
#' @description Try to figure out if user provided latitude / longitude
#'   as vectors, data.frame, file, or interactively pick file.
#'
#' @details 
#' Also see closely related function [sitepoints_from_any()].
#' 
#' This function relies on
#'
#'  [read_csv_or_xl()]  and
#'
#'  [latlon_df_clean()] 
#'  which in turn uses [latlon_infer()] [latlon_as.numeric()] [latlon_is.valid()]
#'
#'
#'  A draft function [read_and_clean_points()] would a more general way to get points,
#'  
#'  but is still work in progress... it is similar to latlon_from_anything()
#'
#'   except it also uses these functions:
#'
#'   [latlon_from_regid()],  [latlon_from_programid()]
#'   
#'   and could eventually use  _from_naics() etc.
#'   
#'   Even more generally, FIPS and shapefile inputs could be read through a 
#'   single wrapper function at some point.
#'
#' @param anything If missing and interactive mode in RStudio, prompts user for file. Otherwise,
#'   this can be a filename (csv or xlsx, with path), or data.frame/ data.table/ matrix,
#'  or vector of longitudes (in which case y must be the latitudes).
#'   File or data.frame/data.table/matrix must have columns called lat and lon, or names that can
#'   be inferred to be that by latlon_infer()
#' @param lon_if_used If anything parameter is a vector of longitudes, lon_if_used must be the latitudes. Ignored otherwise.
#' @seealso [sitepoints_from_any()] which is like this but also adds ejam_uniq_id column, 
#'   and see [read_csv_or_xl()] and [latlon_df_clean()]
#' @return A data.frame that has at least columns lon and lat (and others if they were in anything),
#'   and a logical column called "valid"
#' @examples
#'  latlon_from_anything(testpoints_10)
#'  latlon_from_anything(testpoints_10$lat, testpoints_10$lon)
#'  pts = c("33,-100", "32,-101")
#'  latlon_from_anything(pts)
#'  pts = data.frame(Longitude = testpoints_10$lon, Latitude = testpoints_10$lat)
#'  latlon_from_anything(pts)
#'  pts = data.table(Lat = testpoints_10$lat, Long = testpoints_10$lon)
#'  latlon_from_anything(pts)
#'  \dontrun{
#'  if (interactive()) {
#'    pts <- latlon_from_anything()
#'  }}
#'  \dontrun{
#'  pts = system.file("testdata/latlon/testpoints_10.xlsx", package = "EJAM")
#'  latlon_from_anything(pts)
#'  }
#'   
#' @aliases latlon_any_format
#'
#' @export
#'
latlon_from_anything <- function(anything, lon_if_used) {
  x <- anything
  # y <- lon_if_used
  if (missing(x) || is.null(x) || all(length(x) == 0) || "" %in% x) {
    if (interactive() && !shiny::isRunning()) {
      x <- rstudioapi::selectFile(caption = "Select xlsx or csv with lat,lon values", path = '.' ) 
      } else {
      if (shiny::isRunning()) {
        warning("file path/name needed but not provided")
        return(NULL)
      } else {
        stop("file path/name needed but not provided")
      }
    }}
  
  # figure out if x is a filename or data.table or data.frame
  # of lat, lon values, and clean it up for use.
  # otherwise, do the same assuming anything,lon_if_used are lat,lon values as vectors.
  if (data.table::is.data.table(x)) data.table::setDF(x) # syntax is easier here this way. note that a data.table is also a list and data.frame
  if (is.list(x) & !is.data.frame(x)) {x <- as.data.frame(x)} # like if x <- list(lon = 1:5, lat = 1:5)
  if (is.matrix(x) | is.array(x) ) {x <- as.data.frame(x)}
  
  if (!is.data.frame(x)) { #   (but not if a matrix or a non-df-list or array or vector)
    
    if (length(x) == 1 && all(is.character(x)) && all(!grepl(",", x))) {
      # the grepl to check for comma is to make sure it is not a single csv like "30,-100"
      # seems to be a file name with path, so read it
      if (file.exists(x)) {
        pts <- read_csv_or_xl(x) # from EJAMejscreenapi ::
      } else {
        if (shiny::isRunning()) {
          warning(paste0(x, ' is not a filepath/name that exists, and otherwise must be a vector of latitudes or a table of points'))
          return(NULL)
        } else {
          stop(paste0(x, ' is not a filepath/name that exists, and otherwise must be a vector of latitudes or a table of points'))
        }
      }
    } else {
      # x aka anything was not a file, and is still not a data.frame, so 
      # a) first 2 input params should be lat,lon vectors, or
      # b) x should be a vector of csv pairs and lon_if_used should be missing
      
      if (missing(lon_if_used)) {
        # either x is a vector of csv pairs or nothing worked.
        if (is.atomic(x) && all(grepl(",", x))) {
          pts <- latlon_from_vectorofcsvpairs(x)
        } else {
          # FAILED
          if (shiny::isRunning()) {
            warning('the first input parameter could not be interpreted as a valid input')
            return(NULL)
          } else {
            stop('the first input parameter could not be interpreted as a valid input')
          }
        }
        
      } else {
        #  anything,lon_if_used should be lat,lon vectors, or nothing worked
        
        if (is.atomic(x) && is.atomic(lon_if_used) && is.vector(x) && is.vector(lon_if_used)) {
          x <- as.numeric(x)
          lon_if_used <- as.numeric(lon_if_used)
          pts <- data.frame(lat = x, lon = lon_if_used)        
        } else {
          # FAILED
          if (shiny::isRunning()) {
            warning('the input parameters could not be interpreted as valid inputs')
            return(NULL)
          } else {
            stop('the input parameters could not be interpreted as valid inputs')
          }
        }
      }
    }
  } else {
    # x is now a data.frame
    pts <- x
  }
  
  pts <- latlon_df_clean(pts) # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()

  return(pts)
}
########################################################### #


#' Get lat/lon flexibly - from file, data.frame, data.table, or lat/lon vectors
#' @inherit latlon_from_anything
#' @return A data.frame that has at least columns lon and lat (and others if they were in x)
#' @export
#'
latlon_any_format <- latlon_from_anything

########################################################### #
