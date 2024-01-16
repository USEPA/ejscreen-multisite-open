
## code to prepare testpoints datasets -- 
# 
# - points data objects, 
# - points excel files, 
# - sites2blocks data objects, and 
# - documentation files for all those

## one-time swap to rename column siteid to sitenumber
# ttt = grep("test", datapack("EJAM")$Item, value = T) 
# for (this in ttt) {cat('is siteid in names(', this, ')? ')
#   cat("siteid" %in% names(get(this)), '\n')}
# is siteid in names( testoutput_getblocksnearby_1000pts_1miles )? TRUE 
# is siteid in names( testoutput_getblocksnearby_100pts_1miles )? TRUE 
# is siteid in names( testoutput_getblocksnearby_10pts_1miles )? TRUE 
# is siteid in names( testpoints_10 )? TRUE 
# is siteid in names( testpoints_100 )? TRUE 
# is siteid in names( testpoints_1000 )? TRUE 
# is siteid in names( testpoints_10000 )? TRUE 
# is siteid in names( testpoints_100_dt )? TRUE 
# is siteid in names( testpoints_conus5 )? TRUE
# names(testpoints_10) <- gsub('siteid', 'sitenumber', names(testpoints_10))
# names(testpoints_100) <- gsub('siteid', 'sitenumber', names(testpoints_100))
# names(testpoints_1000) <- gsub('siteid', 'sitenumber', names(testpoints_1000))
# names(testpoints_10000) <- gsub('siteid', 'sitenumber', names(testpoints_10000))
# names(testpoints_100_dt) <- gsub('siteid', 'sitenumber', names(testpoints_100_dt))
# names(testpoints_conus5) <- gsub('siteid', 'sitenumber', names(testpoints_conus5))
# usethis::use_data(testpoints_10 , overwrite = TRUE)
# usethis::use_data(testpoints_100 , overwrite = TRUE)
# usethis::use_data(testpoints_1000 , overwrite = TRUE)
# usethis::use_data(testpoints_10000 , overwrite = TRUE)
# usethis::use_data(testpoints_100_dt , overwrite = TRUE)
# usethis::use_data(testpoints_conus5 , overwrite = TRUE)


creatingnew_testpoints_data   <- TRUE  #done. TO REPLACE THE ACTUAL TEST POINTS (can be false and still do other steps below)
resaving_testpoints_rda       <- TRUE
resaving_testpoints_excel     <- TRUE
resaving_testpoints_helpdocs  <- TRUE

recreating_getblocksnearby    <- TRUE  # eg if block data changed, or if recreating_doaggregate_output = TRUE below
resaving_getblocksnearby_rda  <- TRUE
resaving_getblocksnearby_helpdocs <- TRUE

recreating_doaggregate_output <- TRUE # eg if other indicators added to outputs
 if (recreating_doaggregate_output) {recreating_getblocksnearby <- TRUE} # needed
resaving_doaggregate_rda      <- TRUE
resaving_doaggregate_helpdocs <- TRUE # just in case
resaving_doaggregate_excel    <- TRUE

recreating_ejamit_output      <- TRUE # eg if format or list of indicators changes
resaving_ejamit_rda           <- TRUE
resaving_ejamit_helpdocs      <- TRUE
resaving_ejamit_excel         <- TRUE

redoing_ejscreenit_10_for_ejam_to_have  <- TRUE 
# and  there are these:  5, 50, 500  ##  


if (basename(getwd()) != "EJAM") {stop('do this from EJAM source package folder')}
library(EJAM) # does this need to be here? will it possibly be a problem in some situation like before the package is installed but source can be loaded, or while changes are being made and not yet reinstalled with updates, etc.? 
#  EJAM package must be loaded or at least the functions available 

nvalues <-   c(10, 100, 1000, 10000) # numbers of point locations, picked from FRS points.
myrad <- 1 # radius in miles. Larger would create MUCH larger versions of sites2blocks example objects

for (n in nvalues) {
  
  # CREATE TESTPOINTS DATA #### 
  
  testpoints_name  <- paste0("testpoints_", n)  
  if (
    exists(testpoints_name)
    # file.exists(paste0("/data/", testpoints_name, ".rda"))
                & !creatingnew_testpoints_data) {
    cat("Found and will not recreate", paste0("/data/", testpoints_name, ".rda"))
    load(paste0("/data/", testpoints_name, ".rda")) # in case not in global env right now, such as pkg not rebuilt or not attached yet
    if (!exists(testpoints_name)) {stop('missing', testpoints_name)}
  } else {
    cat("creating ", n, "new random points\n")
    testpoints_data <- EJAM::testpoints_n(n = n, weighting = "frs", dt = FALSE)               ############# #
    # WITH DUMMY VALUES FOR MOST COLUMNS ####
    testpoints_data$sitename = paste0("Example Site ", 1:n)
    # Drop other columns to just use lat lon sitenumber sitename
    # testpoints_data$NAICS = NULL # 722410# testpoints_data$SIC = NULL # 5992  # testpoints_data$REGISTRY_ID = NULL # c(EJAM::test_xtrac, rep(NA,n))[1:n] #  # testpoints_data$PGM_SYS_ACRNMS = NULL
    testpoints_data  <- testpoints_data[ , c("lat", "lon", "sitenumber", "sitename")]   
    assign(testpoints_name, testpoints_data)    #        put the data into an object of the right name 
    if (n == 100) {
      testpoints_100_dt <- data.table(testpoints_100)
      if (resaving_testpoints_rda) {
        usethis::use_data(testpoints_100_dt , overwrite = TRUE)
      }
    }
  }
  
  # SAVE AS DATA IN PACKAGE ####
  if (resaving_testpoints_rda) {
  text_to_do <- paste0("usethis::use_data(", testpoints_name, ", overwrite=TRUE)")
  eval(parse(text = text_to_do))
  }
  # SAVE AS EXCEL FILE  ####
  if (resaving_testpoints_excel) {
  # testpoints_data$EJScreenMap = url_ejscreenmap(lat = testpoints_data$lat, lon = testpoints_data$lon, as_html = FALSE) # NOT CLICKABLE IN EXCEL THIS WAY BUT OK
  # testpoints_data$REGISTRY_ID <- as.character(testpoints_data$REGISTRY_ID)  # for excel just save as character not number, or could write as special zip code format in excel
  # Note the old links work on popup map but not in excel, if as_html=T 
  # testpoints_data$EJScreenMap = url_ejscreenmap(lat = testpoints_data$lat, lon = testpoints_data$lon, as_html = T)
  #
  writexl::write_xlsx(list(testpoints = testpoints_data), 
                      path = paste0("./inst/testdata/latlon/", testpoints_name, ".xlsx"))    ############# #
  }
  
  # SAVE DOCUMENTATION AS A FILE ####
  if (resaving_testpoints_helpdocs) {
  filecontents <- paste0(
    "#' @name ", testpoints_name, " 
#' @docType data
#' @title test points data.frame with columns sitenumber, lat, lon
NULL
"
  )
  writeChar(filecontents, con = paste0("./R/data_", testpoints_name, ".R"))             ############# #
  if (n == 100) {
    writeChar(gsub("testpoints_100", "testpoints_100_dt", filecontents), con = paste0("./R/data_", "testpoints_100_dt", ".R"))   
  }
  }
  
  ################################## #   ################################## #   ################################## #   
  
  ## CREATE OUTPUT EXAMPLES - getblocksnearby() outputs #### 
  
  if (n < 10000) { # do not save huge sites2blocks files, as for larger sets of sites
  namebase <- "testoutput_getblocksnearby_"
    out_varname_getblocks = paste0(namebase,
                         n, "pts_", myrad, "miles") 
    out_varname_getblocks_alias <- paste0("sites2blocks_example",
                                          n, "pts_", myrad, "miles")
    
    if (recreating_getblocksnearby) {
    out_data_getblocks <- EJAM::getblocksnearby(testpoints_data, radius = myrad)                     ############# #
    assign(out_varname_getblocks, out_data_getblocks)
    ################################## #  
    ##   AND MAKE AN ALIAS TOO ####
    #    for convenience / might be easier to remember / holdover from when it was called that
    out_data_getblocks_alias <- out_data_getblocks

    assign(out_varname_getblocks_alias, out_data_getblocks_alias)
    }
    if (n < 10000) {
      
      if (resaving_getblocksnearby_rda) {
      # SAVE AS DATA IN PACKAGE ####
      #
      text_to_do = paste0("usethis::use_data(", out_varname_getblocks, ", overwrite=TRUE)")
      eval(parse(text = text_to_do))                                             ############# #
      text_to_do = paste0("usethis::use_data(", out_varname_getblocks_alias, ", overwrite=TRUE)")
      eval(parse(text = text_to_do))         
      }
      # SAVE DOCUMENTATION AS A FILE ####
      if (resaving_getblocksnearby_helpdocs) {
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
      
      }
      
    } # end of if n <
    
  } # end of the if n == 
  
  
  ################################## #   ################################## #   ################################## #   
  
  ## CREATE OUTPUT EXAMPLES - doaggregate() outputs #### 
  
  if (n < 10000) {
    namebase <- "testoutput_doaggregate_"
    
    out_varname_doagg = paste0(namebase,
                               n, "pts_", myrad, "miles") 
    
    if (recreating_doaggregate_output) {
      
      # DEFAULTS: 
      #       sites2blocks, sites2states_or_latlon = NA, radius = NULL,
      #       countcols = NULL, popmeancols = NULL, calculatedcols = NULL,
      #       testing = FALSE, include_ejindexes = FALSE, updateProgress = NULL,
      #       need_proximityscore = FALSE, calculate_ratios = TRUE, silentinteractive = TRUE,
      #       called_by_ejamit = FALSE, subgroups_type = "nh", extra_demog = TRUE,
      #       infer_sitepoints = FALSE, ...)
      
    out_data_doagg <- doaggregate(out_data_getblocks, sites2states_or_latlon = testpoints_data, radius = myrad,
                                  include_ejindexes = TRUE) # not the default but want to test this way 
    assign(out_varname_doagg, out_data_doagg)
    }
    # SAVE AS DATA IN PACKAGE ####
    if (resaving_doaggregate_rda) {
    text_to_do = paste0("usethis::use_data(", out_varname_doagg, ", overwrite=TRUE)")
    eval(parse(text = text_to_do))                                             ############# #
    }
    # SAVE DOCUMENTATION AS A FILE ####
    if (resaving_doaggregate_helpdocs) {
    filecontents <- paste0(
      "#' @name ", out_varname_doagg, " 
#' @docType data
#' @title test output of doaggregate()
#' @details This is the output of doaggregate(", out_varname_getblocks,", sites2states_or_latlon = ", testpoints_name,", radius = ", myrad,", include_ejindexes = TRUE)
#' @seealso [doaggregate()] [ejamit()] [", out_varname_getblocks,"] [", testpoints_name,"]
NULL
"
    )
    # prefix documentation file names with "data_" 
    writeChar(filecontents, con = paste0("./R/data_", out_varname_doagg, ".R"))       ############# #
    }
    # SAVE AS EXCEL FILE  ####
    
    #  Q: do we want to redo this using table_xls_from_ejam() as now done below with ejamit() output? ***
    
    if (resaving_doaggregate_excel) {
    junk = table_xls_format(overall = out_data_doagg$results_overall, 
                           eachsite =       out_data_doagg$results_bysite, 
                           longnames =      out_data_doagg$longnames,
                           bybg =           out_data_doagg$results_bybg_people,
                           analysis_title = "Example of outputs of doaggregate() being formatted and saved using table_xls_format()",  
                           buffer_desc = paste0("Within ", myrad, " miles"),
                           plotlatest = FALSE, 
                           saveas = paste0("./inst/testdata/examples_of_output/", out_varname_doagg, ".xlsx") 
    )
    
    # writexl::write_xlsx(as.list(out_data_doagg), 
    #                     path = paste0("./inst/testdata/examples_of_output/", out_varname_doagg, ".xlsx"))    ############# #
    }
  }
  
  ################################## #   ################################## #   ################################## #   
  
  ##  CREATE OUTPUT EXAMPLES - ejamit() outputs #### 
  
  if (n < 10000) {
namebase <- "testoutput_ejamit_"
    
    out_varname_ejamit = paste0(namebase,
                                n, "pts_", myrad, "miles")    
    if (recreating_ejamit_output) {
    
      # DEFAULTS: 
      #        sitepoints, radius = 3, maxradius = 31.07, avoidorphans = FALSE,           
      #        quadtree = NULL, quiet = TRUE, parallel = FALSE, fips = NULL,                    
      #        shapefile_folder = NULL, in_shiny = FALSE, need_blockwt = TRUE,                  
      #        countcols = NULL, popmeancols = NULL, calculatedcols = NULL,                     
      #        testing = FALSE, include_ejindexes = FALSE, updateProgress = NULL,               
      #        need_proximityscore = FALSE, calculate_ratios = TRUE, silentinteractive = FALSE, 
      #        called_by_ejamit = TRUE, subgroups_type = "nh", extra_demog = TRUE,              
      #        infer_sitepoints = FALSE, threshold1 = 90)
       
    out_data_ejamit <- ejamit(testpoints_data, radius = myrad, 
                              include_ejindexes = TRUE) #  # include_ejindexes = FALSE was the default but we want to test with them included
    
    # testdata_ejamit_output_1000pts_1miles
    # testdata_ejamit_output_100pts_1miles
    
    assign(out_varname_ejamit, out_data_ejamit)
    }
    if (resaving_ejamit_rda) {
    # SAVE AS DATA IN PACKAGE ####
    #
    text_to_do = paste0("usethis::use_data(", out_varname_ejamit, ", overwrite=TRUE)")
    eval(parse(text = text_to_do))                                             ############# #
    }
    
    if (resaving_ejamit_helpdocs) {
      # SAVE DOCUMENTATION AS A FILE ####
    #
    filecontents <- paste0(
      "#' @name ", out_varname_ejamit, " 
#' @docType data
#' @title test output of ejamit()
#' @details This is the output of ejamit(", testpoints_name,", radius = ", myrad,", include_ejindexes = TRUE)
#' @seealso [doaggregate()] [ejamit()] [", out_varname_doagg,"] and [", testpoints_name,"]
NULL
"
    )
    # prefix documentation file names with "data_" 
    writeChar(filecontents, con = paste0("./R/data_", out_varname_ejamit, ".R"))       ############# #
    }
    
    if (resaving_ejamit_excel) {
      
    # SAVE AS EXCEL FILE  ####
    #
      junk = table_xls_from_ejam(out_data_ejamit,
                                 in.analysis_title = "Example of outputs of ejamit() being formatted and saved using table_xls_from_ejam()",
                                 radius_or_buffer_in_miles = myrad,
                                 buffer_desc = paste0("Within ", myrad, " miles"),
                                 fname = paste0("./inst/testdata/examples_of_output/", out_varname_ejamit, ".xlsx")
      )
    }
  }
  
  ################################## #   ################################## #   ################################## #   
  
} # end of loop over point counts

############################################# # 
if (redoing_ejscreenit_10_for_ejam_to_have) {
# using the API that EJScreen provides, for comparison, to have available as data saved in EJAM pkg:
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
#'  Also see
#'  
#'  testoutput_ejamit_10pts_1miles$results_bysite
#' 
NULL
"
)
# prefix documentation file names with "data_" 
writeChar(filecontents, con = paste0("./R/data_", "testoutput_ejscreenit_10pts_1miles", ".R"))       ############# #

}

cat('
    REMEMBER TO RECREATE PACKAGE DOCUMENTATION: 
    devtools::document()  # for help files. or Clean and INSTALL package
    devtools::build_manual()  # for pdf manual
    postdoc::render_package_manual()  # for html manual
    \n')



