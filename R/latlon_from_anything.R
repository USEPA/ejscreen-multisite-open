
#' Get lat/lon flexibly - from file, data.frame, data.table, or lat/lon vectors
#'
#' @description Try to figure out if user provided latitude / longitude
#'   as vectors, data.frame, file, or interactively pick file.
#'
#' @details This function relies on
#'
#'  [read_csv_or_xl()]  and
#'
#'  [latlon_df_clean()] which in turn uses [latlon_infer()] [latlon_as.numeric()] [latlon_is.valid()]
#'
#'
#'  [read_and_clean_points()] from EJAMejscreenapi
#'
#'   would be the most general / flexible broadest way to get points, but is still work in progress
#'
#'   is similar to what is done by latlon_from_anything()
#'
#'   except it also uses these functions:
#'
#'   [latlon_from_regid()]
#'
#'   [latlon_from_programid()]   but not  _from_naics() ?
#'
#'
#' @param x If missing and interactive mode in RStudio, prompts user for file. Otherwise,
#'   this can be a filename (csv or xlsx, with path), or data.frame/ data.table/ matrix,
#'  or vector of longitudes (in which case y must be the latitudes).
#'  Note that even though it is called latlon_etc the
#'   lon is x and comes before the lat among parameters x,y (unlike in most other functions here using lat,lon)
#'   File or data.frame/data.table/matrix must have columns called lat and lon, or names that can
#'   be inferred to be that by latlon_infer()
#' @param y If x is a vector of longitudes, y must be the latitudes. Ignored otherwise.
#' @seealso read_csv_or_xl() latlon_df_clean()
#' @return A data.frame that has at least columns lon and lat (and others if they were in x)
#' @examples   \dontrun{
#'  if (interactive()) {
#'  pts <- latlon_from_anything()
#'  }}
#'  latlon_from_anything(testpoints_100[1:6,] )
#'  latlon_from_anything(testpoints_100[1:6, c('lat','lon')] )
#'  latlon_from_anything(x=testpoints_100$lon[1:6], y=testpoints_100$lat[1:6] )
#' @aliases latlon_any_format
#'
#' @export
#'
latlon_from_anything <- function(x,y) {

  if (missing(x)) {
    if (interactive()) { x <- rstudioapi::selectFile(caption = "Select xlsx or csv with lat,lon values", path = '.' ) } else {
      if (shiny::isRunning()) {
        warning("file path/name needed but not provided")
        return(NULL)
      } else{
        stop("file path/name needed but not provided")
      }
    }}

  # figure out if x is a filename or data.table or data.frame
  # of lat, lon values, and clean it up for use.
  # otherwise, do the same assuming x,y are lon,lat values as vectors.
  if (data.table::is.data.table(x)) data.table::setDF(x) # syntax is easier here this way. note that a data.table is also a list and data.frame
  if (is.list(x) & !is.data.frame(x)) {x <- as.data.frame(x)} # like if x <- list(lon = 1:5, lat = 1:5)
  if (is.matrix(x) | is.array(x) ) {x <- as.data.frame(x)}
  if (!is.data.frame(x)) { # also if data.table (but not if a matrix or a non-df-list or array or vector)
    if (is.character(x) & length(x) == 1) {
      # seems to be a file name with path, so read it
      if (!file.exists(x)) {
        if (shiny::isRunning()) {
          warning(paste0(x, ' is not a filepath/name that exists, and otherwise must be a vector of longitudes or a table of points'))
          return(NULL)
        } else{
          stop(paste0(x, ' is not a filepath/name that exists, and otherwise must be a vector of longitudes or a table of points'))
        }
      }
      pts <- read_csv_or_xl(x) # from EJAMejscreenapi ::
    } else {
      # Not a file, not a data.frame, so x,y should be lon,lat vectors

      if (missing(y)) {

        warning("if x is not a data.frame or file, then x and y must be longitude and latitude vectors respectively. Using x as lon and y as lat")
      }
      # SHOULD CHECK HERE THAT x is a numeric vector !
      pts <- data.frame(lon = x, lat = y)
    }
  } else {
    # It is a data.frame (or data.table as well)
    pts <- try(data.frame(x)) # in case it was also a data.table, make it a simple data.frame only??
  }
  pts <- latlon_df_clean(pts) # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
  ## WHAT SHOULD BE RETURNED IF COLUMNS lat lon EXIST now but NO VALID coordinates AT ALL? all lat and all lon can be set to NA and that should work down the line?
  ## WHAT SHOULD BE RETURNED IF NO COLUMNS CAN BE INTERPRETED AS lat lon ? Need to check for that where this function is used!
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
