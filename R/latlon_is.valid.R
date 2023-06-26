#' Validate latitudes and longitudes
#' @description Check each latitude and longitude value to see if they are 
#'   NA or outside expected numeric ranges 
#'   (based on approx ranges of lat lon seen among block internal points dataset)
#'   lat must be between 17.5 and 71.5, and 
#'   lon must be ( between -180 and -65) OR (between 172 and 180) 
#' @param lat vector of latitudes in decimal degrees
#' @param lon numeric vector of longitudes in decimal degrees, same length
#'
#' @return logical vector, one element per lat lon pair (location)
#' @seealso latlon_df_clean() latlon_infer() latlon_is.valid() latlon_as.numeric()
#' @examples  \dontrun{
#'  table(latlon_is.valid(lat =  blockpoints$lat, lon =  blockpoints$lon))
#'   ##      TRUE 
#'   ## 8,174,955
#'   }
#' @export
#'
latlon_is.valid <- function(lat, lon) {
  
  #     valid ranges determined like this:  
  #   > range(blockpoints$lat)
  # [1] 17.88513 71.39840
  # > # lat must be between 17.5 and 71.5
  # 
  # > min(blockpoints$lon[blockpoints$lon > -65])
  # [1] 172.5912 # and must be < 180 (one is at 179.6212)
  # > max(blockpoints$lon[blockpoints$lon < 0])
  # [1] -65.20799 # and must be > -180 (min seen is -179.1084)
  # > # lon must be ( between -180 and -65) OR (between 172 and 180) 
  
  # TRUE only if both lat and lon seem valid
  if( any(is.na(lat)) || any(is.na(lon)) ) {
    bad <- is.na(lat) | is.na(lon)
  } else {
    bad <- rep(FALSE, length(lat))
  }
  bad <- bad | lat < 17.5 | lat > 71.5 | lon < -180 | (lon > -65 & lon < 172) | lon > 180
  return(!bad)
}
