# update the test ponts and test output examples to latest version of package

# TESTPOINTS CAN TAKE TIME PLUS LONGER FOR API.
# 20 MINUTES FOR THE 500 POINTS, ROUGHLY
######################################################################################################## # 

# testids_program_sys_id ####
# already made

# testpoints_5 <- structure(list(
#   siteid = c(1, 2, 3, 4, 5), 
#   sitename = c("site 1", "site 2", "site 3", "site 4", "site 5"), 
#   lon = c(-111.9040233, -73.7917865, -73.9131946, -101.349867, -117.5039426), 
#   lat = c(33.5604162, 41.0613821, 40.8304508, 33.6014344, 33.7420278)
# ), row.names = c(NA, -5L), class = "data.frame")

# testids_program_sys_id
######################################################################################################## # 

# testpoints were already made ##################
# 
# # load the existing testpoints
data("testpoints_5",   package = "EJAMejscreenapi")
data("testpoints_50",  package = "EJAMejscreenapi")
data("testpoints_500", package = "EJAMejscreenapi")

file.exists('./R/data_testpoints_5.R')
file.exists('./R/data_testpoints_50.R')
file.exists('./R/data_testpoints_500.R')

######################################################################################################## # 

# ejscreenRESTbroker ? ##################

# > testpoints_5$lon[1]
# [1] -111.904
# > testpoints_5$lat[1]
# [1] 33.56042
testoutput_ejscreenRESTbroker_1pts_1miles <- ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = 1)
testoutput_ejscreenRESTbroker_1pts_1miles <- EJAM:::metadata_add(testoutput_ejscreenRESTbroker_1pts_1miles)
usethis::use_data(testoutput_ejscreenRESTbroker_1pts_1miles,   overwrite = TRUE)

# SAVE DOCUMENTATION AS A FILE ####

filecontents <- paste0(
  "#' @name ", "testoutput_ejscreenRESTbroker_1pts_1miles", " 
#' @docType data
#' @title test data, output from this function
#' @details 
#'  Just for convenience, installed with the package,
#'  the equivalent of results of 
#'   `ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = 1)`
#' @seealso 
#'   [testpoints_5] [testpoints_50] [testpoints_500]
#'   
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]
#'   
#'   [testoutput_ejscreenapi_plus_5] [testoutput_ejscreenapi_plus_50] [testoutput_ejscreenapi_plus_500] 
'testoutput_ejscreenRESTbroker_1pts_1miles'
"
)
fname = paste0("./R/data_", "testoutput_ejscreenRESTbroker_1pts_1miles", ".R")
writeChar(filecontents, con = fname)             ############# #
file.exists(fname)


######################################################################################################## # 

# ejscreenapi  ##################

testoutput_ejscreenapi_1pts_1miles <- ejscreenapi(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = 1, unit = "miles", wkid = 4326,
                                                  report_every_n = 25, # report_every_n = 1000,
                                                  save_when_report = FALSE, format_report_or_json = "pjson", on_server_so_dont_save_files = FALSE, ipurl = "ejscreen.epa.gov",
                                                  updateProgress = NULL, drop_redundant_indicators = FALSE)

testoutput_ejscreenapi_1pts_1miles <- EJAM:::metadata_add(testoutput_ejscreenapi_1pts_1miles)
usethis::use_data(testoutput_ejscreenapi_1pts_1miles,   overwrite = TRUE)

# SAVE DOCUMENTATION AS A FILE ####

filecontents <- paste0(
  "#' @name ", "testoutput_ejscreenapi_1pts_1miles", " 
#' @docType data
#' @title test data, output from this function
#' @details 
#'  Just for convenience, installed with the package,
#'  the equivalent of results of 
#'  
#'  `ejscreenapi(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1]`,
#'     
#'      radius = 1, unit = 'miles', wkid = 4326,
#'     
#'      report_every_n = 25, # report_every_n = 1000,
#'     
#'      save_when_report = FALSE, format_report_or_json =  'pjson', on_server_so_dont_save_files = FALSE, 
#'      ipurl = 'ejscreen.epa.gov',
#'     
#'      updateProgress = NULL, drop_redundant_indicators = FALSE)
#'     
#' @seealso 
#'   [testpoints_5] [testpoints_50] [testpoints_500]
#'   
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]
#'   
#'   [testoutput_ejscreenapi_plus_5] [testoutput_ejscreenapi_plus_50] [testoutput_ejscreenapi_plus_500] 
'testoutput_ejscreenapi_1pts_1miles'
"
)
fname = paste0("./R/data_", "testoutput_ejscreenapi_1pts_1miles", ".R")
writeChar(filecontents, con = fname)             ############# #
file.exists(fname)



######################################################################################################## # 

# ejscreenapi_plus ##################

testoutput_ejscreenapi_plus_5   <- ejscreenapi_plus(testpoints_5 ,  radius = 1, save_when_report = F, on_server_so_dont_save_files = T, verbose = F)
testoutput_ejscreenapi_plus_5 <- EJAM:::metadata_add(testoutput_ejscreenapi_plus_5)
usethis::use_data(testoutput_ejscreenapi_plus_5,   overwrite = TRUE)

testoutput_ejscreenapi_plus_50  <- ejscreenapi_plus(testpoints_50,  radius = 1, save_when_report = F, on_server_so_dont_save_files = T, verbose = F)
testoutput_ejscreenapi_plus_50  <- EJAM:::metadata_add(testoutput_ejscreenapi_plus_50)
usethis::use_data(testoutput_ejscreenapi_plus_50,  overwrite = TRUE)

testoutput_ejscreenapi_plus_500 <- ejscreenapi_plus(testpoints_500, radius = 1, save_when_report = F, on_server_so_dont_save_files = T, verbose = F)
testoutput_ejscreenapi_plus_500 <- EJAM:::metadata_add(testoutput_ejscreenapi_plus_500)
usethis::use_data(testoutput_ejscreenapi_plus_500, overwrite = TRUE)

# for (object_name in c("testoutput_ejscreenapi_plus_5", "testoutput_ejscreenapi_plus_50", "testoutput_ejscreenapi_plus_500")) {
# filecontents <- paste0("
# #' @name 'object_name'
# #' @docType data
# #' @title test data examples of output from [ejscreenapi_plus()] using testpoints, radius = 1
# #' @details 
# #'  Just for convenience, installed with the package.
# #'  Has header row, plus a row for each point, and about 300+ columns of buffer summary results.
# #' @seealso 
# #'   [testpoints_5] [testpoints_50] [testpoints_500]
# #'   
# #'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]
# #'   
# #'   [testoutput_ejscreenapi_plus_5] [testoutput_ejscreenapi_plus_50] [testoutput_ejscreenapi_plus_500] 
# 'object_name'
# ")
# writeChar(filecontents, con = paste0("./R/data_", object_name, ".R"))             ############# #
# }

## confirm documentation exists already ####
if (!file.exists("./R/data_testoutput_ejscreenapi_plus_5.R")) {warning("did not find file ./R/data_testoutput_ejscreenapi_plus_5.R")}
if (!file.exists("./R/data_testoutput_ejscreenapi_plus_50.R")) {warning("did not find file ./R/data_testoutput_ejscreenapi_plus_50.R")}
if (!file.exists("./R/data_testoutput_ejscreenapi_plus_500.R")) {warning("did not find file ./R/data_testoutput_ejscreenapi_plus_500.R")}
################################################################################################### # 

# ejscreenit ##################

testoutput_ejscreenit_5               <- ejscreenit(testpoints_5,   radius = 1, nosee = T, nosave = T, save_map = F, save_plot = F, save_table = F, interactiveprompt = F)
testoutput_ejscreenit_5 <- EJAM:::metadata_add(testoutput_ejscreenit_5)
usethis::use_data(testoutput_ejscreenit_5,         overwrite = TRUE)

testoutput_ejscreenit_50              <- ejscreenit(testpoints_50,  radius = 1, nosee = T, nosave = T, save_map = F, save_plot = F, save_table = F, interactiveprompt = F)
testoutput_ejscreenit_50 <- EJAM:::metadata_add(testoutput_ejscreenit_50)
usethis::use_data(testoutput_ejscreenit_50,        overwrite = TRUE)

testoutput_ejscreenit_500             <- ejscreenit(testpoints_500, radius = 1, nosee = T, nosave = T, save_map = F, save_plot = F, save_table = F, interactiveprompt = F)
testoutput_ejscreenit_500 <- EJAM:::metadata_add(testoutput_ejscreenit_500)
usethis::use_data(testoutput_ejscreenit_500,       overwrite = TRUE)

# for (object_name in c("testoutput_ejscreenit_5", "testoutput_ejscreenit_50", "testoutput_ejscreenit_50")) {
# filecontents <- paste0("
# #' @name 'object_name'
# #' @docType data
# #' @title test data examples of output from [ejscreenit()] using testpoints, radius = 1
# #' @details 
# #'  Just for convenience, installed with the package.
# #'  A list of outputs, named "table" "map"   "plot" where table is a data.frame like from ejscreenapi_plus() 
# #' @seealso 
# #'   [testpoints_5] [testpoints_50] [testpoints_500]
# #'   
# #'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]
# #'   
# #'   [testoutput_ejscreenapi_plus_5] [testoutput_ejscreenapi_plus_50] [testoutput_ejscreenapi_plus_500] 
# 'object_name'
# ")
# writeChar(filecontents, con = paste0("./R/data_", object_name, ".R"))             ############# #
# }

## confirm documentation exists already ####
if (!file.exists("./R/data_testoutput_ejscreenit_5.R")) {warning("did not find file ./R/data_testoutput_ejscreenit_5.R")}
if (!file.exists("./R/data_testoutput_ejscreenit_50.R")) {warning("did not find file ./R/data_testoutput_ejscreenit_50.R")}
if (!file.exists("./R/data_testoutput_ejscreenit_500.R")) {warning("did not find file ./R/data_testoutput_ejscreenit_500.R")}


# clean up ###############################

rm(
  testpoints_5,  
  testpoints_50,  
  testoutput_ejscreenit_50, 
  testoutput_ejscreenapi_plus_50, 
  
  testpoints_500,
  testoutput_ejscreenit_500,
  testoutput_ejscreenapi_plus_500
)
