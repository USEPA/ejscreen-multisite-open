# obtain shapefile to be used to see Which state contains each site 

# help("states_shapefile")

setwd("~/../../R/mysource/EJAM/data-raw")
download.file("https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=States+%28and+equivalent%29", destfile = "tl_2020_us_state.zip" )
dir.create("./shp")
unzip("tl_2020_us_state.zip", exdir = "./shp")
states_shapefile <- sf::st_read("./shp")

usethis::use_data(states_shapefile, overwrite = TRUE)


## alternative way, from EJSCREENbatch:   (format differs)
# library(tigris)
# library(sf)
# states_shapefile2 <- tigris::states() %>% sf::st_as_sf() %>%
#   sf::st_transform(crs ="ESRI:102005") %>%
#   dplyr::select('NAME') %>%
#   dplyr::rename(facility_state = NAME)
# #    facility_buff =  Polygons representing buffered areas of interest
# 



