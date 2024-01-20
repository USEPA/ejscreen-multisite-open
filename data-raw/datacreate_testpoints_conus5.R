###################################################### # 

# create testpoints_conus5  ####
# to use for mapping whole continental united states range 
# as in data entry module example

CONUS5 <- data.frame( rbind(
  c(47,      -123, 1,	'Site in upper northwest'),
  c(46,     	-69, 2,	'Site in Maine'),
  c(33.7477, -118, 3,	'Site near Los Angeles'),
  c(26,	     -81,  4,	'Site in south FL'),
  c(40.814, -96.7, 5,	'Site near Lincoln Nebraska')
))
names(CONUS5) <- c('lat', 'lon', 'sitenumber', 'sitename')
CONUS5$lat <- as.numeric(CONUS5$lat)
CONUS5$lon <- as.numeric(CONUS5$lon)
CONUS5$sitenumber <- as.numeric(CONUS5$sitenumber)

# CONUS5
# dput(CONUS5)

# CONUS5 <- 
# structure(list(
#   lat = c(47,    46, 33.7477, 26, 40.814), 
#   lon = c(-123, -69, -118,   -81, -96.7), 
#   sitenumber = c(1, 2, 3, 4, 5), 
#   sitename = c("Site in upper northwest", "Site in Maine", "Site near Los Angeles", "Site in south FL", "Site near Lincoln Nebraska")
#   ), row.names = c(NA, -5L), class = "data.frame")

testpoints_conus5 <- CONUS5
usethis::use_data(testpoints_conus5, overwrite = TRUE)

# Document the dataset ####

filecontents <- "
#' @name testpoints_10 
#' @docType data
#' @title test points data.frame with columns sitenumber, lat, lon
NULL"

# prefix documentation file names with "data_" 
writeChar(filecontents, con = paste0("./R/data_", "testpoints_conus5", ".R"))       ############# #

# SAVE AS EXCEL FILE  ####
#
writexl::write_xlsx(list(testpoints = testpoints_conus5), 
                    path = paste0("./inst/testdata/latlon/", "testpoints_conus5", ".xlsx"))    ############# #
###################################################### # 
