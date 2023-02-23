#' @name states_shapefile
#' @docType data
#' @title US States boundaries 2020 shapefile from TIGER
#' @details
#'  
#'  setwd("~/../../R/mysource/EJAM/data-raw")
#'  
#'  download.file("https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=States+%28and+equivalent%29", destfile = "tl_2020_us_state.zip" )
#'  
#'  dir.create("./shp")
#'  
#'  unzip("tl_2020_us_state.zip", exdir = "./shp")
#'  
#'  states_shapefile <- sf::st_read("./shp")
#' 
NULL
