#' Get lat lon columns and clean up those latitude and longitude columns in a data.frame
#' 
#' @description Utility to identify lat and lon columns, renaming and cleaning them up.
#' 
#' @details Tries to figure out which columns seem to have lat lon values, renames those in the data.frame. 
#'   Cleans up lat and lon values (removes extra characters, makes numeric)
#' @param df data.frame With columns lat and lon or names that can be interpreted as such
#'
#' @seealso Used by [latlon_from_anything()]. Uses [latlon_infer()] [latlon_is.valid()] [latlon_as.numeric()]
#' @return Returns the same data.frame but with relevant colnames changed to lat and lon,
#'    and invalid lat or lon values cleaned up if possible or else replaced with NA
#'
#' @examples #  x <- latlon_df_clean(x)
#'
#' @keywords internal
#'
latlon_df_clean <- function(df) {
  if (missing(df)) {
    warning('No value provided for argument "df".')
    return(NULL)
  }
  # figure out which columns seem to have lat lon values, rename those in the data.frame
  # $ signifies the end of a string, so only will be removed if at end
  names(df) <- latlon_infer(gsub(".1$", "", names(df)))
  
  # Cleans up lat and lon values (removes extra characters, makes numeric)
  if ('lat' %in% names(df) & 'lon' %in% names(df)) {
    df$lon <- latlon_as.numeric(df$lon)
    df$lat <- latlon_as.numeric(df$lat)
  } else {
    warning("Dataframe does not have both lat and lon columns")
    # removed since latlon_infer already creates warning
    #warning('lat or lon column cannot be inferred from colnames of df')
  }
  
  ## start with assuming all valid and change ones that are not  
  df$valid <- TRUE
  
  # validate to some extent (are the lat lon plausible values)
  ok <- latlon_is.valid(lat = df$lat, lon = df$lon)
  if (any(!ok)) {
    # warning and console msg are done in latlon_is.valid()
    
    ## convert invalid latlons to NA
    df[!ok, c('lat','lon')] <- NA
    ## change valid column to FALSE
    df[!ok, 'valid'] <- FALSE
  }
  return(df)
}


