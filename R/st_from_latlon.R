#' find what state is where each point is located
#' 
#' @param lat latitudes vector
#' @param lon longitudes vector
#' @param shapefile shapefile of US States, in package already
#'
#' @return Returns data.frame: ST, statename, FIPS.ST, REGION,
#'   each as long as lat or lon
#' @export
#'
state_from_latlon <- function(lat, lon, states_shapefile=EJAM::states_shapefile) {
  # library(sf)
  pts <- data.frame(lon=lon, lat=lat) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(states_shapefile))
  pts <- pts |> sf::st_join(states_shapefile)
  pts <- as.data.frame(pts)[,c("STUSPS", "NAME", "STATEFP")]
  colnames(pts) <- c("ST", "statename", "FIPS.ST")
  pts$REGION <- EJAM::stateinfo$REGION[match(pts$statename, stateinfo$statename)]
  return(pts)
  
# library(sf)
# pts <- EJAMejscreenapi::testpoints_50
# pts <- pts |>
#   st_as_sf(coords = c("lon", "lat"), crs =st_crs(states_shapefile))
# plot(pts)
# # download.file("https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=States+%28and+equivalent%29")
# # states_shapefile = st_read("~/../../R/mysource/EJAM/data-raw/shp")
# str(states_shapefile)
# states_shapefile$NAME
# states_shapefile$STUSPS
# ptsinfo <- pts |> st_join(states_shapefile)
# x = as.data.frame(ptsinfo)[,c("siteid", "sitename", "STUSPS", "NAME")]
# x

}
