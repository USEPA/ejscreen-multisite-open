
# state_from_latlon()
# state_from_blocktable()
# state_from_blockid()
# state_from_fips()
## and see elsewhere states_as_sites()


########################################### #

#' Find what state is where each point is located
#'
#' Takes 3 seconds to find state for 1k points, so a faster alternative would be useful
#' @param lon longitudes vector
#' @param lat latitudes vector
#' @param shapefile shapefile of US States, in package already
#' @seealso [states_shapefile] [get_blockpoints_in_shape()] [states_infer()]
#' @return Returns data.frame: ST, statename, FIPS.ST, REGION, n
#'   as many rows as elements in lat or lon
#' @examples
#'  myprogram <- "CAMDBS" # 739 sites
#'  pts <- frs_from_program(myprogram)[ , .(lat, lon, REGISTRY_ID,  PRIMARY_NAME)]
#'  # add a column with State abbreviation
#'  pts[, ST := state_from_latlon(lat=lat, lon = lon)$ST]
#'  #map these points
#'  mapfast(pts[ST == 'TX',], radius = 1) # 1 miles radius circles
#'
#' @export
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


#' state_from_blocktable was used only in some special cases of using testpoints_n()
#'
#' given data.table with blockid column, get state abbreviation of each - not used?
#' @param dt_with_blockid
#'
#' @return vector of ST info like AK, CA, DE, etc.
#'
#' @examples EJAM:::state_from_blocktable(blockpoints[45:49,])
#'
#' @keywords internal
#'
state_from_blocktable <- function(dt_with_blockid) {

  stateinfo$ST[match(blockid2fips[dt_with_blockid, substr(blockfips,1,2), on = "blockid"], stateinfo$FIPS.ST)]
}
##################################################################################################### #


#' given vector of blockids, get state abbreviation of each
#'
#' @param blockid vector of blockid values as from EJAM in a table called blockpoints
#'
#' @return vector of ST info like AK, CA, DE, etc.
#'
#' @examples EJAM:::state_from_blockid(c(8174952, blockpoints$blockid[5:6]))
#'
#' @keywords internal
#'
state_from_blockid <- function(blockid) {

  if (!exists('blockid2fips')) {
    dataload_from_pins(varnames = 'blockid2fips')
  }
  if (!exists('blockid2fips')) {return(rep(NA, length(blockid)))}
  stateinfo$ST[match(blockid2fips[blockid, substr(blockfips,1,2)], stateinfo$FIPS.ST)]
}
##################################################################################################### #



#' Get FIPS of ALL BLOCKGROUPS in the States or Counties
#'
#' Get the State abbreviations of ALL blockgroups within the input FIPS
#'
#' @details Returns a vector of 2-letter State abbreviations that is
#'   one per blockgroup that matches the input FIPS,
#'   not necessarily a vector as long as the input vector of FIPS codes!,
#'   and not just a short list of unique states!
#' @param fips Census FIPS codes vector, numeric or char, 2-digit, 5-digit, etc. OK
#' @param uniqueonly If set to TRUE, returns only unique results.
#'   This parameter is here mostly to remind user that default is not uniques only.
#' @return vector of 2-character state abbreviations like CA,CA,CA,MD,MD,TX
#'
#' @export
#'
state_from_fips <- function(fips, uniqueonly=FALSE) {

  fips <- fips_bg_from_anyfips(fips) # returns all the blockgroups fips codes that match, such as all bg in the state or county
  x <- stateinfo$ST[match(substr(fips,1,2), stateinfo$FIPS.ST)]
  if (uniqueonly) {return(unique(x))} else {return(x)}
}
##################################################################################################### #

