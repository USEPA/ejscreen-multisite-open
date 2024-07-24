## code to prepare datasets 

testpoints_5                 <- read_csv_or_xl('./inst/testdata/testpoints_5.xlsx')
testpoints_50                <- read_csv_or_xl('./inst/testdata/testpoints_50.xlsx')
testpoints_500               <- read_csv_or_xl('./inst/testdata/testpoints_500.xlsx')

testpoints_5$ejscreenmap <- NULL  
testpoints_50$ejscreenmap <- NULL # works ok in excel but not once saved as dataset and imported for use in ejscreenit() etc.
testpoints_500$ejscreenmap <- NULL 

usethis::use_data(testpoints_5,                 overwrite = TRUE)
usethis::use_data(testpoints_50,                overwrite = TRUE)
usethis::use_data(testpoints_500,               overwrite = TRUE)
# 
# # load the existing testpoints
# data("testpoints_5",   package = "EJAMejscreenapi")
# data("testpoints_50",  package = "EJAMejscreenapi")
# data("testpoints_500", package = "EJAMejscreenapi")
# 
# file.exists('./R/testpoints_5.R')
# file.exists('./R/testpoints_50.R')
# file.exists('./R/testpoints_500.R')


# SAVE AS EXCEL FILE?  ####
#
writexl::write_xlsx(list(testpoints = testpoints_5),   path = paste0("./inst/testdata/", "testpoints_5",   ".xlsx"))    ############# #
writexl::write_xlsx(list(testpoints = testpoints_50),  path = paste0("./inst/testdata/", "testpoints_50",  ".xlsx"))    ############# #
writexl::write_xlsx(list(testpoints = testpoints_500), path = paste0("./inst/testdata/", "testpoints_500", ".xlsx"))    ############# #

# SAVE DOCUMENTATION AS A FILE ####
for (testpoints_name in c("testpoints_5", "testpoints_50", "testpoints_500")) {
  filecontents <- paste0(
    "#' @name ", testpoints_name, " 
#' @docType data
#' @title test points data.frame with columns sitenumber, lat, lon
#' @description Examples of what could be input to functions
#'   that needs points specified by lat lon
#' @details 
#'  Just for convenience, these are installed with the package, and are
#'  the equivalent of results of reading the .xlsx test data files.
#' @seealso 
#'   [testpoints_5] [testpoints_50] [testpoints_500]
#'   
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]
#'   
#'   [testoutput_ejscreenapi_plus_5] [testoutput_ejscreenapi_plus_50] [testoutput_ejscreenapi_plus_500] 
NULL
"
  )
  writeChar(filecontents, con = paste0("./R/data_", testpoints_name, ".R"))             ############# #
}
## confirm documentation exists  ####
file.exists("./R/data_testpoints_5.R")
file.exists("./R/data_testpoints_50.R")
file.exists("./R/data_testpoints_500.R")
