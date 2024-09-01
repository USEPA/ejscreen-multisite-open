# system.time({
#   #    ABOUT 7 MINUTES TO RUN TESTS (if large datasets had not yet been loaded)
#   source("./tests/manual_nonalphabetical.R") # answering Yes to running ALL tests
# })
######################################################## #
if (1 == 0) { # do this part manually if needed

  # TO RELOAD / ITERATE WHILE DEBUGGING:
  #   TAKES 20 seconds even though supposed to be slower if reset = T,  if not loading block datasets on attach
  
  devtools::load_all(reset = TRUE)
  # dataload_from_pins("all")

  # TO OPEN SOME KEY TEST FILES:
  
# rstudioapi::navigateToFile("./tests/testthat/test-FIPS_FUNCTIONS.R")
# rstudioapi::navigateToFile("./tests/testthat/test-state_from_fips_bybg.R")
# rstudioapi::navigateToFile("./tests/testthat/test-doaggregate.R")
# rstudioapi::navigateToFile("./tests/testthat/test-ejamit.R")
# rstudioapi::navigateToFile("./tests/testthat/test-latlon_df_clean.R")

}

######################################################## #
# simple checks for easy/basic case, main functions, without actually running unit tests with testthat

if (1 == 0) { # do this part manually if needed

  # latlon
x <- ejamit(testpoints_5[1:2,], radius = 1)
# names(x)
ejam2table_tall(x)
ejam2barplot(x)
ejam2barplot_sites(x)
ejam2tableviewer(x)
junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
# ejam2report(x)   #  report not yet working if not in shiny app
ejam2map(x)
fname = ejam2shapefile(x, folder = tempdir())
 shpin = shapefile_from_any(fname)
 map_shapes_leaflet(shpin)
 
# shapefile

shp <- shape_buffered_from_shapefile( shapefile_from_sitepoints(testpoints_5[1:2,]), radius.miles = 1)
                                      # or use test data  shp <- shapefile_from_any()
shp <- shapefile_from_any(system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM"))[1:3, ]  
# x <- ejamit( shapefile = shp )  ## NOT WORKING RIGHT NOW
# names(x)
# ejam2table_tall(x)
# ejam2barplot(x)
# ejam2barplot_sites(x)
# ejam2tableviewer(x)
# junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
# ejam2report(x)   #  report not yet working if not in shiny app
## ejam2map(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
## ejam2shapefile(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
  # map_shapes_leaflet(x)  


# fips
x <- ejamit(fips = fips_bg_from_anyfips(fips_counties_from_state_abbrev("DE")[1])[1:2]) # just 2 blockgroups
names(x)
ejam2table_tall(x)
ejam2barplot(x)
ejam2barplot_sites(x)
ejam2tableviewer(x)
junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
# ejam2report(x)   #  report not yet working if not in shiny app
# ejam2map(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
# ejam2shapefile(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()

x <- ejamit(fips = fips_counties_from_state_abbrev("DE"))  #   3 Counties
mapfastej_counties(x$results_bysite) # not (x)


}
####################### #


########################################## # ########################################## # 
# Setup ####
# !diagnostics off ## to disable diagnostics in this document
#        thisfile = "./tests/manual_nonalphabetical.R"
# source(thisfile) to test the local source pkg, by group of functions, quietly, summarized
# test_local()     to test the local source pkg
# test_package()   to test installed version of package
# test_check()     to test installed version of package, the way used by R CMD check or check()
library(testthat)
library(data.table) # used in functions here
library(magrittr)
library(dplyr)
consoleclear <- function() if (interactive() & rstudioapi::isAvailable()) {rstudioapi::executeCommand("consoleClear")}
consoleclear()
# if (interactive()) {
#   useloadall <- askYesNo(msg = "Do you want to load and test the current source code files version of EJAM (via devtools::load_all() etc.,
#                       rather than testing the installed version)?", default = TRUE)
#   if (useloadall) {devtools::load_all()} else {suppressPackageStartupMessages({   library(EJAM) }) } #??
# } else {suppressPackageStartupMessages({   library(EJAM)   }) } #??
dataload_from_pins("all")

if (file.exists("./tests/testthat/setup.R")) {
  # rstudioapi::navigateToFile("./tests/testthat/setup.R")
  source("./tests/testthat/setup.R") #   asks if need load_all or library
} else {
  cat("Need to source the setup.R file first \n")    
}
########################################## #
  
  ## Find the tests ####
update_list_of_tests <- TRUE
if (update_list_of_tests) {
  sdir <- getwd()
  test_files_found <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
  ########################################## # 
  
  ## GROUP the tests ####
  
  testlist = list( 
    
    test_fips = c(
      # "test-fips_lead_zero.R",   
      # "test-fips_bg_from_anyfips.R",    #   test_file("tests/testthat/test-fips_bg_from_anyfips.R")
      "test-FIPS_FUNCTIONS.R",
      "test-state_from_fips_bybg.R",  
      "test-state_from_latlon.R"   
    ),
    test_naics = c(
      "test-naics_categories.R",   
      "test-naics_findwebscrape.R", 
      "test-naics_from_any.R",   
      "test-naics_from_code.R",  
      "test-naics_from_name.R",   
      "test-naics_subcodes_from_code.R",
      "test-naics_validation.R",  
      "test-naics2children.R"    
    ),
    test_frs = c(
      "test-regid_from_naics.R", 
      "test-frs_from_naics.R",    
      "test-frs_from_programid.R",  
      "test-frs_from_regid.R",  
      "test-frs_from_sic.R",   
      "test-frs_is_valid.R"   
    ),
    test_latlon = c(
      "test-latlon_infer.R",        
      "test-latlon_as.numeric.R",
      "test-latlon_df_clean.R",
      "test-latlon_is.valid.R",   
      "test-latlon_from_anything.R",   
      "test-latlon_from_sic.R",
      "test-address_xyz.R", 
      "test-latlon_from_address.R",
      "test-latlon_from_vectorofcsvpairs.R",
      "test-state_from_sitetable.R"
    ),
    test_maps = c(
      "test-MAP_FUNCTIONS.R"
    ),
    test_shape = c(
      "test-shapefile_xyz.R"  
    ),
    test_getblocks = c(
      "test-radius_inferred.R",              # this is SLOW THOUGH
      "test-getblocks_summarize_blocks_per_site.R",  
      "test-getblocksnearby.R",        
      "test-getblocksnearby_from_fips.R", 
      "test-getblocksnearbyviaQuadTree.R"    #    -------------- NEEDS MORE TESTS ***
    ),
    test_fixcolnames = c(
      "test-fixcolnames.R",
      "test-fixnames.R",
      "test-fixnames_to_type.R",
      "test-fixcolnames_infer.R",
      "test-varinfo.R",
      "test-utils_metadata_add.R"
    ),
    test_doag = c(
      "test-pctile_from_raw_lookup.R",  
      "test-doaggregate.R"     
    ),
    test_ejamit = c(
      "test-ejamit.R",
      "test-ejam2barplot_sites.R",
      "test-ejamit_compare_distances.R",
      "test-ejamit_compare_groups_of_places.R",
      "test-ejamit_sitetype_check.R"
    ),
    test_ejscreenapi = c(
      "test-ejscreenapi.R",
      "test-ejscreenapi_plus.R",
      "test-ejscreenapi1.R",
      "test-ejscreenit.R",
      "test-ejscreenRESTbroker-functions.R"
    ),
    test_mod = c(
      "test-mod_save_report.R",    
      "test-mod_specify_sites.R",  
      "test-mod_view_results.R"    
    ),
    test_app = c(
      "test-golem_utils_server.R", # not really used  
      "test-golem_utils_ui.R",     # not really used
      "test-ui_and_server.R"   
    )
  )
  ########################################## # 
  groupnames = names(testlist)
  test_all <- as.vector(unlist(testlist))
  ########################################## # 
  ## Check if all Grouped ####
  {
    
    if (!all(TRUE == all.equal(sort(test_all), sort(test_files_found)))) {
      cat("\n\n   test files found in folder does not match test_files_found list  \n")
      print(all.equal(sort(test_all), sort(test_files_found))) 
      cat("\n\n")
    }
    
    if (length(setdiff(test_files_found, test_all)) > 0) {
      cat("These are in test folder as files but not in list of groups above: \n\n")
      print(setdiff(test_files_found, test_all))
      cat("\n")
      stop("fix list of files")
    }
    
    if (length(setdiff(test_all, test_files_found)) > 0) {
      cat("These are in list of groups above but not in test folder as files: \n\n")
      print(setdiff(test_all, test_files_found))
      cat("\n")
      stop("fix list of test files")
    }
    
    if (any(duplicated(test_all))) {
      cat("some are listed >1 group\n")
      stop("some are listed >1 group")
    }
    
    cat("\n\n")
    ########################################## # 
  }
} # end if, update_list_of_tests
## Define Functions that run tests ####
{
##     BY GROUP, WITH SUCCINCT SUMMARY

test1group <- function(fnames = test_all, 
                       reporter = "minimal", # some of the code below now only works if using this setting
                       # reporter = default_compact_reporter(), # 
                       print = FALSE
) {
  xtable <- list()
  # tfile <- tempfile("junk", fileext = "txt")
  for (i in 1:length(fnames)) {
    cat(".")
    suppressWarnings(suppressMessages({
      junk <- testthat::capture_output_lines({
        x <- testthat::test_file(
          file.path("./tests/testthat/", fnames[i]), 
          reporter = reporter)
      }, print = print)
    }))
    # testthat_print(junk)
    x <- as.data.frame(x)
    x$tests <- x$nb
    x$nb <- NULL
    x$flag <- x$tests - x$passed
    x$err  <- x$tests - x$passed - x$warning
    x$error_cant_test <- ifelse(x$error, 1, 0)
    x$error <- NULL
    x$skipped <- ifelse(x$skipped, 1, 0)
    x <- x[, c('file',  'test', 
               'tests', 'passed', 'failed',  'err',
               'warning', 'flag', 'skipped', 'error_cant_test'
    )]
    x$test <- substr(x$test, 1, 50) # some are long
    xtable[[i]] <- data.table::data.table(x)
  }
  cat("\n")
  xtable <- data.table::rbindlist(xtable)
  print(colSums(xtable[, .(tests, passed, failed, err,
                           warning, flag, 
                           skipped, error_cant_test)]))
  return(xtable)
}
########################### #

##     ONE SPECIFIC GROUP OF TESTS AT A TIME

testbygroup <- function(testlist, ...) {
  # the ... can be print=TRUE, and possibly reporter=default_compact_reporter()
  # testlist[[(names(testlist)[[1]])]] is the same as get(names(testlist)[[1]]), vector of filenames
  xtable <- list()
  i <- 0
  for (tgroupname in names(testlist)) {
    i <- i + 1
    fnames = unlist(testlist[[tgroupname]])
    cat("", tgroupname, "group has", length(fnames), "test files ")
    # print(data.frame(files = testlist[[tgroupname]]))
    xtable[[i]] <- data.table::data.table(testgroup = tgroupname, 
                                          test1group(testlist[[tgroupname]], ...) )
  }
  xtable <- data.table::rbindlist(xtable)
  return(xtable)
}
}   #   done defining functions
########################### #  ########################################## #
# RUN JUST 1 FILE OR GROUP? ####

y = askYesNo("Run only some tests now?")
if (is.na(y)) {y = FALSE}
if (y) {
  fnames = unlist(testlist)
  shortgroupnames = gsub("^test_(.*)","\\1", names((testlist)))
  tname = rstudioapi::showPrompt(
    "WHICH TEST OR GROUPS COMMA-SEP LIST",
    paste0(shortgroupnames, collapse = ","),
    # "fips,naics,frs,latlon,maps,shape,getblocks,fixcolnames,doag,ejamit,ejscreenapi,mod,app"
    )
  tname <- unlist(strsplit(gsub(" ", "", tname), ","))
  tname = paste0("test_", tname)
  #    test_file("./tests/testthat/test-MAP_FUNCTIONS.R" )
  partial_testlist <-  testlist[names(testlist) %in% tname] 

  consoleclear()
  
  x = testbygroup(testlist = partial_testlist)
  
  cat("\n\n                                         RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")
  print(x[x$flag + x$error_cant_test > 0,])
} 
########################### #  ########################################## #

# RUN ALL TESTS (slow)  ####

y = askYesNo("RUN ALL TESTS NOW?")
if (is.na(y)) {y = FALSE}
if (y) {
  ## takes almost 5 minutes 
  consoleclear()
  
  z <- system.time({
    testall <- testbygroup(testlist = testlist)
  })
  print(z)
  #   user  system elapsed    5 minutes
  # 217.80   18.68  395.68 
  ########################### #  ########################################## #
  x <- testall
  ## save results of testing ####
  
  y = askYesNo("Save results of unit testing?") 
  if (is.na(y)) {y = FALSE}
  if (y) {
    fname <- paste0("./tests/results_of_unit_testing_", as.character(Sys.Date()), ".rda")
    save(testall, file = fname)
  } # end if - save
}
########################### #  ########################################## #
########################### #  ########################################## #

# SEE WHAT PASSED / FAILED ####

y = askYesNo("View results of unit testing?") 
if (is.na(y)) {y = FALSE}
if (y) {
  consoleclear()
  ########################### #  ########################### #
  
  cat("TEST RESULTS AS OF "); cat(as.character(Sys.Date()))
  
  # HOW MANY TOTAL PASS/FAIL?
  
  cat("\n\n\n")
  cat("COUNT PASS / FAIL \n\n")
  print(colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)]))
  cat("\n")
  cat("PERCENT PASS / FAIL ")
  cat("\n\n")
  print(round(100 * colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
              / sum(x$tests), 1))
  
  # about 1.6 % of unit tests have errors   as of 7/1/24   ***********
  # (and ~1% have warnings)   ***********
  ########################### #  ########################### #
  
  ## KEY GROUPS - WHICH TEST GROUPS or FILES HAVE THE MOST FAILING TESTS?
  
  x[ , flag_byfile := sum(flag), by = "file"]
  x[ , err_byfile  := sum(err),  by = "file"]
  x[ , flag_bygroup := sum(flag), by = "testgroup"]
  x[ , err_bygroup  := sum(err),  by = "testgroup"]
  setorder(x, -err_bygroup, testgroup, -flag, -failed, file)
  
  bygroup <- x[ , .(err = sum(err), warning = sum(warning), flag = sum(flag), 
                    passed = sum(passed), tests = sum(tests)), by = "testgroup"]
  cat("\n\n\n")
  cat("KEY GROUPS OF FILES")
  cat("\n\n\n")
  print(bygroup)
  ########################### #  ########################### #
  
  ## WHICH FILES HAVE THE MOST FAILING TESTS?
  
  byfile <- x[ , .(
    err_byfile = err_byfile[1], 
    err_bygroup = err_bygroup[1],
    testgroup = testgroup[1]
  ), 
  by = "file"]
  setorder(byfile, -err_bygroup, testgroup, -err_byfile, file)
  setcolorder(byfile, neworder = c("testgroup", "err_bygroup", "file", "err_byfile"))
  
  cat("\n\n")
  cat("KEY FILES")
  cat("\n\n")
  print(byfile)
  cat("TEST RESULTS AS OF "); cat(as.character(Sys.Date()))  
  ########################### #
  
  # WHICH TESTS? 
  
  cat("\n\n\n")
  cat("PRIORITIZED TESTS")
  cat("\n\n\n")
  
  prioritize = x[order(-x$flag, -x$failed), 1:11]
  prioritize <- prioritize[prioritize$tests != prioritize$passed | prioritize$error_cant_test > 0, ]
  print(prioritize)

  cat("\n\n")
  # cat("MORE KEY TESTS DETAILS")
  # cat("\n\n")
  # xx = x[x$error_cant_test > 0 | x$test != x$passed, ]
  # print(xx)
  
} # end of big if - viewing results
########################### #  ########################################## #

