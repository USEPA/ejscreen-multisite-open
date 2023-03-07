#' find what state is where each point is located
#' Takes 3 seconds to find state for 1k points, so a faster alternative would be useful
#' @param lat latitudes vector
#' @param lon longitudes vector
#' @param shapefile shapefile of US States, in package already
#' @seealso [states_shapefile] [get_blockpoints_in_shape()]
#' @return Returns data.frame: ST, statename, FIPS.ST, REGION,
#'   each as long as lat or lon
#' @export
#'
state_from_latlon <- function(lat, lon, states_shapefile=EJAM::states_shapefile) {
  # pts[is.na(lat), lat := 0] 
  # pts[is.na(lon), lon := 0] 
  lat[is.na(lat)] <- 0
  lon[is.na(lon)] <- 0 # will ensure NA is returned by the join for those points with missing coordinates
  pts <- data.frame(lon=lon, lat=lat) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(states_shapefile))
  pts <- pts |> sf::st_join(states_shapefile)
  # pts <- as.data.frame(statename = pts$facility_state)  
  pts <- as.data.frame(pts)[,c("STUSPS", "NAME", "STATEFP")]
  colnames(pts) <- c("ST", "statename", "FIPS.ST")
  pts$REGION <- EJAM::stateinfo$REGION[match(pts$statename, stateinfo$statename)]
  return(pts)
}

state_from_latlon_compiled <- compiler::cmpfun(state_from_latlon)

# checking speed

# library(sf)
# pts <- EJAMejscreenapi::testpoints_1000

# # pts <- testpoints_100_dt[ , .(lat,lon, siteid)]
# # n=100
# # pts <- frs[sample(1:nrow(frs), n), .(lat,lon, REGISTRY_ID)]
# pts$idn <- 1:nrow(pts)

# # Which state contains each site
# library(tigris)
# library(sf)
# states_shapefile2 <- tigris::states() %>% sf::st_as_sf() %>%
#   sf::st_transform(crs="ESRI:102005") %>%
#   dplyr::select('NAME') %>%
#   dplyr::rename(facility_state = NAME)
# 
# system.time({
# pts2 <- st_as_sf(pts, coords = c("lon", "lat"), crs =st_crs(states_shapefile2))
# bystate2 <- sf::st_join(pts2, states_shapefile2, join=sf::st_intersects, largest = TRUE)
# })
# system.time({
#   pts  <- st_as_sf(pts, coords = c("lon", "lat"), crs =st_crs(states_shapefile))
#   bystate <- sf::st_join(pts, states_shapefile, join=sf::st_intersects, largest = TRUE)
# })

# 
# facility_buff <- sf::st_join(mycircles, states_shapefile, join=sf::st_intersects, largest = TRUE)
# rm(states_shapefile)





# 
# # IS ONE   MUCH SLOWER?
# 
# 
# ptsf <- pts |>
#   st_as_sf(coords = c("lon", "lat"), crs =st_crs(states_shapefile))
# # plot(ptsf[ , "siteid"])
# 
# # maybe the best? 
# system.time({
#   ptsf2  <- ptsf |> st_join(states_shapefile)
#   x1 = as.data.frame(ptsf2)[ , c("siteid", "sitename", "REGION", "STATEFP", "NAME")]
# })
# # x1
# 
#   system.time({x2 = state_from_latlon(lat = pts$lat, lon = pts$lon)})
# # x2
# 
#   # maybe the best? 
#   system.time({x3 = state_from_latlon_compiled(lat = pts$lat, lon = pts$lon)})
# #
#   
#   system.time({
#   lat=pts$lat; lon=pts$lon
#   lat[is.na(lat)] <- 0
#   lon[is.na(lon)] <- 0 # will ensure NA is returned by the join for those points with missing coordinates
#   pts <- data.frame(lon=lon, lat=lat) |>
#     sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(states_shapefile))
#   pts <- pts |> sf::st_join(states_shapefile)
#   pts <- as.data.frame(pts)[,c("STUSPS", "NAME", "STATEFP")]
#   colnames(pts) <- c("ST", "statename", "FIPS.ST")
#   pts$REGION <- EJAM::stateinfo$REGION[match(pts$statename, stateinfo$statename)]
#   x4=pts
#   })
# 
