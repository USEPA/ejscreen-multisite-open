# obtain shapefile to be used to see Which state contains each site 

  help("states_shapefile")

stop("must be in local source package EJAM/data-raw folder when running this script")
# setwd("~/../../R/mysource/EJAM/data-raw")

download.file("https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=States+%28and+equivalent%29", destfile = "tl_2020_us_state.zip" )
dir.create("./shp")
unzip("tl_2020_us_state.zip", exdir = "./shp")
states_shapefile <- sf::st_read("./shp")

file.remove("tl_2020_us_state.zip")
file.remove("./shp")

# add the dataset to this package as a dataset to be installed with the package and lazy loaded when needed
attr(states_shapefile, "date_downloaded") <- Sys.Date()
usethis::use_data(states_shapefile, overwrite = TRUE)


################################################################### # 
## alternative way, from EJSCREENbatch   (format differs)
# library # ( # tigris)
# library(sf)
# states_shapefile2 <- tigris # :: # states() %>% sf::st_as_sf() %>%
#   sf::st_transform(crs ="ESRI:102005") %>%
#   dplyr::select('NAME') %>%
#   dplyr::rename(facility_state = NAME)
# #    facility_buff =  Polygons representing buffered areas of interest
# 


################################################################### # 
#  methods for downloading other
# TIGER/Line Shapefiles 
# from the U.S. Census Bureau.


# ** Website Interface
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php  
# allows download of only 1 state at a time if block resolution
# •	For detailed instructions, please see the educational brochure on Downloading TIGER/Line Shapefiles.
# https://www2.census.gov/geo/pdfs/education/tiger/Downloading_TIGERLine_Shp.pdf
# •	Note: Not all versions of TIGER/Line Shapefiles are available through the web interface. 


# ** Direct from FTP site (or via FTP client) to access the full set of files.
# ftp://ftp2.census.gov/geo/tiger/


# ** Direct from Data.gov 
# We have not yet added the ability download Shapefiles directly on data.census.gov, (as of 5/2023)

