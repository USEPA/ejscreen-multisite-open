#' @name states_shapefile
#' @docType data
#' @title US States boundaries 2020 shapefile from TIGER
#' @description This is used to figure out which state contains each point (facility/site).
#' @seealso [state_from_latlon()] [get_blockpoints_in_shape()]
#' @details This is used by [state_from_latlon()] to find which state is associated with each point 
#'   that the user wants to analyze. That is needed to report indicators in 
#'   the form of State-specific percentiles 
#'   (e.g., a score that is at the 80th percentile within Texas).
#'   It is created by the package via EJAM/data-raw/make_states_shapefile.R
#'  Created roughly as follows: 
#'  
#'  setwd("~/../../R/mysource/EJAM/data-raw")
#'  download.file("https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=States+%28and+equivalent%29", destfile = "tl_2020_us_state.zip" )
#'  dir.create("./shp")
#'  unzip("tl_2020_us_state.zip", exdir = "./shp")
#'  states_shapefile <- sf::st_read("./shp", quiet = FALSE)
#'  class(states_shapefile)
#'  
NULL
