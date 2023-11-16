#' latlon_df_clean - Find and clean up latitude and longitude columns in a data.frame
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
<<<<<<< HEAD
  names(df) <- latlon_infer(names(df)) 
  
  # Clean up lat and lon values (remove extra characters, make numeric)
=======
  # $ signifies the end of a string, so only will be removed if at end
  names(df) <- latlon_infer(gsub(".1$", "", names(df)))
  
  # Cleans up lat and lon values (removes extra characters, makes numeric)
>>>>>>> testing
  if ('lat' %in% names(df) & 'lon' %in% names(df)) {
    df$lon <- latlon_as.numeric(df$lon)
    df$lat <- latlon_as.numeric(df$lat)
  } else {
<<<<<<< HEAD
    warning('lat or lon column cannot be inferred from colnames of df')
=======
    warning("Dataframe does not have both lat and lon columns")
    # removed since latlon_infer already creates warning
    #warning('lat or lon column cannot be inferred from colnames of df')
    }
  
  # validate to some extent (are the lat lon plausible values)
  if (any(!latlon_is.valid(lat = df$lat, lon = df$lon))) {
    warning('Some lat or lon values seem invalid - NA or number outside expected range')
    ## convert invalid latlons to NA
    df[!latlon_is.valid(lat = df$lat, lon = df$lon), c('lat','lon')] <- NA
>>>>>>> testing
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


