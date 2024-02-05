
# state_from_latlon()
# state_from_blocktable()
# state_from_blockid()
# state_from_fips()
## and see elsewhere states_as_sites()


########################################### # 

#' state_from_latlon - find what state is where each point is located
#' Takes 3 seconds to find state for 1k points, so a faster alternative would be useful
#' @param lon longitudes vector
#' @param lat latitudes vector
#' @param shapefile shapefile of US States, in package already
#' @seealso [states_shapefile] [get_blockpoints_in_shape()] [states_infer()]
#' @return Returns data.frame: ST, statename, FIPS.ST, REGION, n
#'   as many rows as elements in lat or lon
#' @export
#' @examples 
#'  myprogram <- "CAMDBS" # 739 sites
#'  pts <- frs_from_program(myprogram)[ , .(lat, lon, REGISTRY_ID,  PRIMARY_NAME)]
#'  # add a column with State abbreviation
#'  pts[, ST := state_from_latlon(lat=lat, lon = lon)$ST]
#'  #map these points
#'  mapfast(pts[ST == 'TX',], radius = 1) # 1 miles radius circles
#'  
state_from_latlon <- function(lat, lon, states_shapefile=EJAM::states_shapefile) {
  
  if (suppressWarnings({
      any(is.na(as.numeric(lat)) & is.na(as.numeric(lon))) }) ) {
    warning("Some Latitude and Longitude could not be coerced to a number.")
  } else {
    if (suppressWarnings({
      any(is.na(as.numeric(lat))) })) {
      warning("Some Latitude could not be coerced to a number")
    }
    if (suppressWarnings({  
      any(is.na(as.numeric(lon))) }) ) {
      warning("Some Longitude could not be coerced to a number.")
    }
  }
  lat[is.na(as.numeric(lat))] <- NA
  lon[is.na(as.numeric(lon))] <- NA
  
  lat[is.na(lat)] <- 0
  lon[is.na(lon)] <- 0 # will ensure NA is returned by the join for those points with missing coordinates
  pts <- data.frame(lat = lat, lon = lon) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(states_shapefile))  # st_as_sf wants lon,lat not lat,lon
  pts <- pts |> sf::st_join(states_shapefile)
  
  pts <- as.data.frame(pts)[,c("STUSPS", "NAME", "STATEFP")]
  colnames(pts) <- c("ST", "statename", "FIPS.ST")
  pts$REGION <- EJAM::stateinfo$REGION[match(pts$statename, stateinfo$statename)]
  pts$n <- 1:NROW(pts)
  
  if (suppressWarnings({
    any(is.na(pts$statename ))})
    ) {warning("Some latitude / longitude were provided that are not found in any state")}
  return(pts)
}
##################################################################################################### #


#' state_from_blocktable - was used only in some special cases of using testpoints_n() 
#' given data.table with blockid column, get state abbreviation of each - not used?
#' @param dt_with_blockid 
#'
#' @return vector of ST info like AK, CA, DE, etc.
#' @export
#'
#' @examples state_from_blocktable(blockpoints[45:49,])
state_from_blocktable <- function(dt_with_blockid) {
  stateinfo$ST[match(blockid2fips[dt_with_blockid, substr(blockfips,1,2), on = "blockid"], stateinfo$FIPS.ST)]
}
##################################################################################################### #


#' state_from_blockid
#' given vector of blockids, get state abbreviation of each
#' @param blockid vector of blockid values as from EJAM in a table called blockpoints
#'
#' @return vector of ST info like AK, CA, DE, etc.
#' @export
#'
#' @examples state_from_blockid(c(8174952, blockpoints$blockid[5:6]))
state_from_blockid <- function(blockid) {
  if (!exists('blockid2fips')) {
    dataload_from_pins(varnames = 'blockid2fips')
  }
  if (!exists('blockid2fips')) {return(rep(NA, length(blockid)))}
  stateinfo$ST[match(blockid2fips[blockid, substr(blockfips,1,2)], stateinfo$FIPS.ST)]
}
##################################################################################################### #


#' state_from_fips - Get FIPS of ALL BLOCKGROUPS in the States or Counties
#' Get the State abbreviations of ALL blockgroups within the input FIPS
#' @details Returns a vector of 2-letter State abbreviations that is 
#'   one per blockgroup that matches the input FIPS, 
#'   not necessarily a vector as long as the input vector of FIPS codes!, 
#'   and not just a short list of unique states!
#' @param fips Census FIPS codes vector, numeric or char, 2-digit, 5-digit, etc. OK
#' @param uniqueonly If set to TRUE, returns only unique results. 
#'   This parameter is here mostly to remind user that default is not uniques only.
#' @return vector of 2-character state abbreviations like CA,CA,CA,MD,MD,TX
#' @export
#'
state_from_fips <- function(fips, uniqueonly=FALSE) {
  fips <- fips_bg_from_anyfips(fips) # returns all the blockgroups fips codes that match, such as all bg in the state or county
  x <- stateinfo$ST[match(substr(fips,1,2), stateinfo$FIPS.ST)]
  if (uniqueonly) {return(unique(x))} else {return(x)}
}
##################################################################################################### #


# checking speed


# pts <- EJAMejscreenapi::testpoints_1000
# library(data.table)
# # pts <- data.table(testpoints_100)[ , .(lat,lon, sitenumber)]
# # n=100
# # pts <- frs[sample(1:nrow(frs), n), .(lat,lon, REGISTRY_ID)]
# pts$idn <- 1:nrow(pts)

# # Which state contains each site


# states_shapefile2 <- tigris # :: # states() %>% sf::st_as_sf() %>%
#   sf::st_transform(crs ="ESRI:102005") %>%
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
# # plot(ptsf[ , "sitenumber"])
# 
# # maybe the best? 
# system.time({
#   ptsf2  <- ptsf |> st_join(states_shapefile)
#   x1 = as.data.frame(ptsf2)[ , c("sitenumber", "sitename", "REGION", "STATEFP", "NAME")]
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
#   pts <- data.frame(lat=lat, lon=lon) |>
#     sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(states_shapefile))
#   pts <- pts |> sf::st_join(states_shapefile)
#   pts <- as.data.frame(pts)[,c("STUSPS", "NAME", "STATEFP")]
#   colnames(pts) <- c("ST", "statename", "FIPS.ST")
#   pts$REGION <- EJAM::stateinfo$REGION[match(pts$statename, stateinfo$statename)]
#   x4=pts
#   })
# 
