#' Find and clean up latitude and longitude columns in a data.frame
#' @description Utility to identify lat and lon columns, renaming and cleaning them up.
#' @details Tries to figure out which columns seem to have lat lon values, renames those in the data.frame. 
#'   Cleans up lat and lon values (removes extra characters, makes numeric)
#' @param df data.frame With columns lat and lon or names that can be interpreted as such
#'
#' @seealso Used by [latlon_from_anything()]. Uses [latlon_infer()] [latlon_is.valid()] [latlon_as.numeric()]
#' @return Returns the same data.frame but with relevant colnames changed to lat and lon,
#'    and invalid lat or lon values cleaned up if possible or else replaced with NA
#' @export
#'
#' @examples #  x <- latlon_df_clean(x)
latlon_df_clean <- function(df) {
  
  # figure out which columns seem to have lat lon values, rename those in the data.frame
  names(df) <- latlon_infer(names(df)) 
  
  # Clean up lat and lon values (remove extra characters, make numeric)
  if ('lat' %in% names(df) & 'lon' %in% names(df)) {
    df$lon <- latlon_as.numeric(df$lon)
    df$lat <- latlon_as.numeric(df$lat)
  } else {
    warning('lat or lon column cannot be inferred from colnames of df')
    }
  
  # validate, to some extent -- are the lat lon plausible values?
  ok <- latlon_is.valid(lat = df$lat, lon = df$lon)
  if (any(!ok)) {
    # warning and console msg are done in latlon_is.valid()

    ## convert invalid latlons to NA
    df[!ok, c('lat','lon')] <- NA
  }
  return(df)
}


