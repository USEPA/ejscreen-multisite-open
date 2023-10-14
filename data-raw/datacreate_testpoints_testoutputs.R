
## code to prepare testpoints datasets -- 
# 
# - points data objects, 
# - points excel files, 
# - sites2blocks data objects, and 
# - documentation files for all those

if (basename(getwd()) != "EJAM") {stop('do this from EJAM source package folder')}
library(EJAM) # does this need to be here? will it possibly be a problem in some situation like before the package is installed but source can be loaded, or while changes are being made and not yet reinstalled with updates, etc.? 
#  EJAM package must be loaded or at least the functions available 

nvalues <-   c(10, 100, 1000, 10000) # numbers of point locations, picked from FRS points.
myrad <- 1 # radius in miles. Larger would create MUCH larger versions of sites2blocks example objects

for (n in nvalues) {
  
  # CREATE TESTPOINTS DATA #### 
  #
  testpoints_name  <- paste0("testpoints_", n)  
  if (file.exists(paste0("/data/", testpoints_name, ".rda"))) {
    cat("Found and will not recreate", paste0("/data/", testpoints_name, ".rda"))
    load(paste0("/data/", testpoints_name, ".rda")) # in case not in global env right now, such as pkg not rebuilt or not attached yet
    if (!exists(testpoints_name)) {stop('missing', testpoints_name)}
  } else {
    testpoints_data <- EJAM::testpoints_n(n=n, weighting = "frs", dt = FALSE)               ############# #
    # WITH DUMMY VALUES FOR MOST COLUMNS ####
    testpoints_data$sitename = paste0("Example Site ", 1:n)
    # Drop other columns to just use lat lon siteid sitename
    # testpoints_data$NAICS = NULL # 722410# testpoints_data$SIC = NULL # 5992  # testpoints_data$REGISTRY_ID = NULL # c(EJAM::test_xtrac, rep(NA,n))[1:n] #  # testpoints_data$PGM_SYS_ACRNMS = NULL
    testpoints_data  <- testpoints_data[ , c("lat", "lon", "siteid", "sitename")]   
    assign(testpoints_name, testpoints_data)    #        put the data into an object of the right name
  }
  
  # SAVE AS DATA IN PACKAGE ####
  #
  text_to_do <- paste0("usethis::use_data(", testpoints_name, ", overwrite=TRUE)")
  eval(parse(text = text_to_do)) 
  
  # SAVE AS EXCEL FILE  ####
  #
  # testpoints_data$EJScreenMap = url_ejscreenmap(lat = testpoints_data$lat, lon = testpoints_data$lon, as_html = FALSE) # NOT CLICKABLE IN EXCEL THIS WAY BUT OK
  # testpoints_data$REGISTRY_ID <- as.character(testpoints_data$REGISTRY_ID)  # for excel just save as character not number, or could write as special zip code format in excel
  # Note the old links work on popup map but not in excel, if as_html=T 
  # testpoints_data$EJScreenMap = url_ejscreenmap(lat = testpoints_data$lat, lon = testpoints_data$lon, as_html = T)
  #
  writexl::write_xlsx(list(testpoints = testpoints_data), 
                      path = paste0("./inst/testdata/latlon/", testpoints_name, ".xlsx"))    ############# #
  
  # SAVE DOCUMENTATION AS A FILE ####
  #
  filecontents <- paste0(
    "#' @name ", testpoints_name, " 
#' @docType data
#' @title test points data.frame with columns siteid, lat, lon
NULL
"
  )
  writeChar(filecontents, con = paste0("./R/data_", testpoints_name, ".R"))             ############# #
  
  
  ################################## #   ################################## #   ################################## #   
  
  ## CREATE OUTPUT EXAMPLES - getblocksnearby() outputs #### 
  
  if (n < 10000) { # do not save huge sites2blocks files, as for larger sets of sites
    
    out_data_getblocks <- EJAM::getblocksnearby(testpoints_data, radius = myrad)                     ############# #
    
    namebase <- "testoutput_getblocksnearby_"
    
    out_varname_getblocks = paste0(namebase,
                         n, "pts_", myrad, "miles")
    assign(out_varname_getblocks, out_data_getblocks)
    
    if (n < 10000) {
      
      # SAVE AS DATA IN PACKAGE ####
      #
      text_to_do = paste0("usethis::use_data(", out_varname_getblocks, ", overwrite=TRUE)")
      eval(parse(text = text_to_do))                                             ############# #
      
      # SAVE DOCUMENTATION AS A FILE ####
      #
      filecontents <- paste0(
        "#' @name ", out_varname_getblocks, " 
#' @docType data
#' @title test output of getblocksnearby(), and is an input to doaggregate() 
#' @details This is the output of getblocksnearby(", testpoints_name,", radius = ", myrad,")
#' @seealso [getblocksnearby()]  [doaggregate()]  [", testpoints_name,"]
NULL
"
      )
      # prefix documentation file names with "data_" 
      writeChar(filecontents, con = paste0("./R/data_", out_varname_getblocks, ".R"))       ############# #
      
      ################################## #  
      ##   AND MAKE AN ALIAS TOO ####
      #    for convenience / might be easier to remember / holdover from when it was called that
      out_data_getblocks_alias <- out_data_getblocks
      out_varname_getblocks_alias <- paste0("sites2blocks_example",
                                  n, "pts_", myrad, "miles")
      assign(out_varname_getblocks_alias, out_data_getblocks_alias)
      text_to_do = paste0("usethis::use_data(", out_varname_getblocks_alias, ", overwrite=TRUE)")
      eval(parse(text = text_to_do))                                             ############# #
      filecontents <- paste0(
        "#' @name ", out_varname_getblocks_alias, " 
#' @docType data
#' @title test output of getblocksnearby(), and is an input to doaggregate() 
#' @details This is the output of getblocksnearby(", testpoints_name,", radius = ", myrad,")
#'   This is the same as  [", out_varname_getblocks,"]
#' @seealso [getblocksnearby()]  [doaggregate()]  [", testpoints_name,"]
NULL
"
      )
      # prefix documentation file names with "data_" 
      writeChar(filecontents, con = paste0("./R/data_", out_varname_getblocks_alias, ".R"))       ############# #
      # end of making alias version
      ################################## #        
    } # end of if n <
    
  } # end of the if n == 
  
  
  ################################## #   ################################## #   ################################## #   
  
  ## CREATE OUTPUT EXAMPLES - doaggregate() outputs #### 
  
  if (n < 10000) {
    
    out_data_doagg <- doaggregate(out_data_getblocks, sites2states_or_latlon = testpoints_data, radius = myrad) 
    
    namebase <- "testoutput_doaggregate_"
    
    out_varname_doagg = paste0(namebase,
                               n, "pts_", myrad, "miles")
    assign(out_varname_doagg, out_data_doagg)
    
    # SAVE AS DATA IN PACKAGE ####
    #
    text_to_do = paste0("usethis::use_data(", out_varname_doagg, ", overwrite=TRUE)")
    eval(parse(text = text_to_do))                                             ############# #
    
    # SAVE DOCUMENTATION AS A FILE ####
    #
    filecontents <- paste0(
      "#' @name ", out_varname_doagg, " 
#' @docType data
#' @title test output of doaggregate()
#' @details This is the output of doaggregate(", out_varname_getblocks,", sites2states_or_latlon = ", testpoints_name,", radius = ", myrad,")
#' @seealso [doaggregate()] [ejamit()] [", out_varname_getblocks,"] [", testpoints_name,"]
NULL
"
    )
    # prefix documentation file names with "data_" 
    writeChar(filecontents, con = paste0("./R/data_", out_varname_doagg, ".R"))       ############# #
    
    # SAVE AS EXCEL FILE  ####
    #
    junk = xls_formatting2(overall = out_data_doagg$results_overall, 
                           eachsite =       out_data_doagg$results_bysite, 
                           longnames =      out_data_doagg$longnames,
                           bybg =           out_data_doagg$results_bybg_people,
                           analysis_title = "Example of outputs of doaggregate() being formatted and saved using xls_formatting2()",  
                           buffer_desc = paste0("Within ", myrad, " miles"),
                           plotlatest = FALSE, 
                           saveas = paste0("./inst/testdata/examples_of_output/", out_varname_doagg, ".xlsx") 
    )
    
    # writexl::write_xlsx(as.list(out_data_doagg), 
    #                     path = paste0("./inst/testdata/examples_of_output/", out_varname_doagg, ".xlsx"))    ############# #
    
  }
  
  ################################## #   ################################## #   ################################## #   
  
  ##  CREATE OUTPUT EXAMPLES - ejamit() outputs #### 
  
  if (n < 10000) {
    
    out_data_ejamit <- ejamit(testpoints_data, radius = myrad)
    
    # testdata_ejamit_output_1000pts_1mile
    # testdata_ejamit_output_100pts_1mile
    
    namebase <- "testoutput_ejamit_"
    
    out_varname_ejamit = paste0(namebase,
                                n, "pts_", myrad, "miles")
    assign(out_varname_ejamit, out_data_ejamit)
    
    # SAVE AS DATA IN PACKAGE ####
    #
    text_to_do = paste0("usethis::use_data(", out_varname_ejamit, ", overwrite=TRUE)")
    eval(parse(text = text_to_do))                                             ############# #
    
    # SAVE DOCUMENTATION AS A FILE ####
    #
    filecontents <- paste0( 
      "#' @name ", out_varname_ejamit, " 
#' @docType data
#' @title test output of ejamit()
#' @details This is the output of ejamit(", testpoints_name,", radius = ", myrad,")
#' @seealso [doaggregate()] [ejamit()] [", out_varname_doagg,"] [", testpoints_name,"]
NULL
"
    )
    # prefix documentation file names with "data_" 
    writeChar(filecontents, con = paste0("./R/data_", out_varname_ejamit, ".R"))       ############# #
    
    # SAVE AS EXCEL FILE  ####
    #
    junk = xls_formatting2(overall =        out_data_ejamit$results_overall, 
                           eachsite =       out_data_ejamit$results_bysite, 
                           longnames =      out_data_ejamit$longnames,
                           bybg =           out_data_ejamit$results_bybg_people,
                           analysis_title = "Example of outputs of ejamit() being formatted and saved using xls_formatting2()",  
                           buffer_desc = paste0("Within ", myrad, " miles"),
                           plotlatest = FALSE, 
                           saveas = paste0("./inst/testdata/examples_of_output/", out_varname_ejamit, ".xlsx") 
    )
  }
  
  ################################## #   ################################## #   ################################## #   
  
} # end of loop over point counts

############################################# # 
# for the API that EJScreen provides, for comparison:
testpoints_name <- "testpoints_10"
myrad = 1
testoutput_ejscreenit_10pts_1miles <- EJAMejscreenapi::ejscreenit(testpoints_10, radius = 1, nosave = T, nosee = T, interactiveprompt = F, calculate_ratios = T)
usethis::use_data(testoutput_ejscreenit_10pts_1miles, overwrite = TRUE)

# SAVE DOCUMENTATION AS A FILE ####
#
filecontents <- paste0( 
  "#' @name ", "testoutput_ejscreenit_10pts_1miles", " 
#' @docType data
#' @title test output of ejscreenit(), using the EJScreen API
#' @details This is the output of 
#' 
#'  EJAMejscreenapi::ejscreenit(
#' 
#'    testpoints_10, radius = 1, 
#' 
#'    nosave = T, nosee = T, interactiveprompt = F, calculate_ratios = T
#'  )
#' 
#'  testoutput_ejscreenit_10pts_1miles$table
#'  
#'  should be very similar to
#'  
#'  testoutput_ejamit_10pts_1miles$results_bysite
#' 
NULL
"
)
# prefix documentation file names with "data_" 
writeChar(filecontents, con = paste0("./R/data_", "testoutput_ejscreenit_10pts_1miles", ".R"))       ############# #


