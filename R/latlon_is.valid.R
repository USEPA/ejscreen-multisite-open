
#' Check if lat lon not NA using !is.na()
#' 
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @return logical vector, one element per lat lon pair (location)
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'
#' @keywords internal
#'
latlon_is.available  <- function(lat, lon) {
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }

  !is.na(lat) & !is.na(lon)
}
############################################### # 


#' Check lat lon coordinates to see if each is approx. in general area of USA excluding Island Areas
#' 
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @return logical vector, one element per lat lon pair (location)
#'   Indicates the point is approximately in one of the 
#'   rough bounding boxes that includes the USA without 
#'   the Island Areas Guam, American Samoa, USVI, N Marianas Islands.
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'
#' @keywords internal
#'
latlon_is.usa <- function(lat, lon) {
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  !(
    (lat < 17.5 | lat > 71.5) |   (lon > -64 & lon < 172) |  (lon > 180 | lon < -180)
  )
}
############################################### # 


#' Check if lat lon between -180 and +180
#' 
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @return logical vector, one element per lat lon pair (location)
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'
#' @keywords internal
#'
latlon_is.possible   <- function(lat, lon) {
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  (lat < 180 & lat > -180  &  lon < 180 & lon > -180)
}
############################################### # 


#' Check lat lon coordinates to see if each is approx. in general area of US Island Areas Guam, USVI, Amer Samoa or N Marianas
#' 
#' See [islandareas]
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @return vector of TRUE / FALSE values indicating a given lat lon pair
#'   is approximately in one of the rough bounding boxes that includes the 4 Island Areas.
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#' @examples
#' \dontrun{
#' # this would require the testpoints_1000 data from the EJAM package:
#'   isles <- which(latlon_is.islandareas(lat = testpoints_1000$lat, lon = testpoints_1000$lon))
#'   mapfast(testpoints_1000[isles, ]) # c(213,785) 
#'   which(!(latlon_is.usa(lat = testpoints_1000$lat, lon = testpoints_1000$lon)))
#' }
#'
#' @keywords internal
#'
latlon_is.islandareas <- function(lat, lon)  {
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  x <- islandareas
  states <- unique(x$ST)
  # ok <- rep(TRUE, length(states))
  ok  <- list()
  for (i in 1:length(states)) {
    ok[[i]] <- (lat > x$lat[x$limit == "min" & x$ST == states[i]]) &
      (lat < x$lat[x$limit == "max" & x$ST == states[i]]) &
      (lon > x$lon[x$limit == "min" & x$ST == states[i]]) &
      (lon < x$lon[x$limit == "max" & x$ST == states[i]])
  }
  ok <- apply(do.call(rbind, ok), 2, any)
  return(ok)
}
############################################### # 


#' Check if lat lon are OK -- validate latitudes and longitudes
#' 
#' @description Check each latitude and longitude value to see if they are valid.
#' @details  
#'   NA or outside expected numeric ranges 
#'   
#'   (based on approx ranges of lat lon seen among block internal points dataset)
#'   
#'   But note Guam, American Samoa, Northern Mariana Islands, and U.S. Virgin Islands are outside these ranges!
#'   EJScreen 2.2 does not provide demographic data in those locations anyway, but can map sites there.
#'   
#'   lat must be between 17.5 and 71.5, and 
#'   
#'   lon must be ( between -180 and -64) OR (between 172 and 180) 
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @param quiet optional logical, if TRUE, show list of bad values in console
#' @return logical vector, one element per lat lon pair (location)
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'   [latlon_df_clean()] [latlon_infer()] [latlon_is.valid()] [latlon_as.numeric()] 
#' @examples  \dontrun{
#'  # this would only work using the EJAM package datasets frs and blockpoints:
#'    if (!exists("frs")) dataload_from_pins("frs")
#'  table(latlon_is.valid(lat =  frs$lat, lon =  frs$lon))
#'  # blockpoints may need to be downloaded using dataload_from_pins()
#'  table(latlon_is.valid(lat =  blockpoints$lat, lon =  blockpoints$lon))
#'   }
#'
#' @export
#'
latlon_is.valid <- function(lat, lon, quiet = TRUE) {
  
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  if(is.null(lat) | is.null(lon)){
    warning('No lat or lon column found')
    return(FALSE)
  }

  # assume none bad until proven otherwise
  bad         <- rep(FALSE, length(lat))
  
  unavailable <- !latlon_is.available(lat = lat, lon = lon) # is.na(lat) | is.na(lon)
  
  impossible  <-  !latlon_is.possible(lat = lat, lon = lon) # lat > 180 | lat < -180  |  lon > 180 | lon < -180
  
  roughly_in_core_usa_excluding_islandareas <- latlon_is.usa(lat = lat, lon = lon)
  #   !(
  #   (lat < 17.5 | lat > 71.5) |   (lon > -64 & lon < 172) |  (lon > 180 | lon < -180) 
  # )
  
  in_islandareas <- latlon_is.islandareas(lat = lat, lon = lon)
  if (any(in_islandareas, na.rm = TRUE)) {
    message("Some points appear to be in US Island Areas, which may lack some data such as demographic data here")
  }
  
  bad <- unavailable | impossible | 
    ( !roughly_in_core_usa_excluding_islandareas & !in_islandareas )
  
  if (any(bad)) {
    warning('Some lat or lon values are invalid - NA or number entirely outside expected ranges (US including Island Areas)')
    if (!quiet) {
      cat('\nInvalid lat lon points:\n\n ')
      print(data.table(lat = lat[bad], lon = lon[bad]))
      cat('\n\n')
    }
  }
  
  return(!bad)
  
  # sort(unique(substr(bgpts$bgfips,1,2)))     # bgpts has PR not island areas
  # sort(unique(substr(blockid2fips$blockfips,1,2))) #  has PR not island areas
  ## same for bgid2fips
  # sort(unique(substr(blockgroupstats$bgfips,1,2))) # has island areas too: "60" "66" "69" "72" "78" (but not lat,lon)
  
  #     
  #   > range(blockpoints$lat)
  # [1] 17.88513 71.39840
  # > # lat must be between 17.5 and 71.5
  # 
  # > min(blockpoints$lon[blockpoints$lon > -65])
  # [1] 172.5912 # and must be < 180 (one is at 179.6212)
  # > max(blockpoints$lon[blockpoints$lon < 0])
  # [1] -65.20799 # and must be > -180 (min seen is -179.1084)
  # > # lon must be ( between -180 and -65) OR (between 172 and 180) -- but actually US VI might reach almost -64 longitude
  
  #    NOTE THAT Guam etc. have points outside these ranges!
  
  # BUT NOTE FRS SEEMS TO HAVE PROBLEMATIC LAT LON VALUES
  # 
  # > range(frs$lat)
  # [1] -43.78711  88.07278
  # > range(frs$lon)
  # [1] -179.3000  179.2599
  
  # > table(latlon_is.valid(lat = frs$lat, lon=frs$lon))
  # 
  # FALSE    TRUE 
  # 1384 3454658
  # 
}
