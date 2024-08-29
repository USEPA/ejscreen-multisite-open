########################################## # 
# setup ####
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
  source("./tests/testthat/setup.R") #   asks if need load_all or library
} else {
  cat("Need to source the setup.R file first \n")    
}
########################################## #
update_list_of_test_files <- TRUE
if (update_list_of_tests) {
  
  # TESTS FOUND ####
  
  sdir <- getwd()
  test_files_found <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
  ########################################## # 
  
  # GROUP the TESTS ####
  
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
      "test-ejamit_sitetype_check"
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
  # GROUPED THEM ALL? ####
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
# Define Functions that run tests ####
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
    cat("", tgroupname, "group has", length(testlist[[tgroupname]]), "test files ")
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
  tname = rstudioapi::showPrompt(
    "WHICH TEST OR GROUPS COMMA-SEP LIST",
    'fips, naics, frs, latlon, maps, shape, getblocks,
    fixcolnames, doag, ejamit, mod, app')
  tname <- unlist(strsplit(gsub(" ", "", tname), ","))
  tname = paste0("test_", tname)
  #    test_file("./tests/testthat/test-MAP_FUNCTIONS.R" )
  partial_testlist <- lapply(tname, get) 
  names(partial_testlist) <- tname
  print(partial_testlist)
  
  # [1] "2024-08-24"
  # testgroup err_bygroup                                       file err_byfile
  # <char>       <int>                                     <char>      <int>
  # 1:      test_ejamit          12                  test-ejam2barplot_sites.R          4
  # 2:      test_ejamit          12            test-ejamit_compare_distances.R          4
  # 3:      test_ejamit          12                              test-ejamit.R          2
  # 4:      test_ejamit          12     test-ejamit_compare_groups_of_places.R          2
  
  # 5:        test_doag          11                         test-doaggregate.R          9
  # 6:        test_doag          11              test-pctile_from_raw_lookup.R          2
  # 7:        test_fips          10                      test-FIPS_FUNCTIONS.R          9
  # 8:        test_fips          10                test-state_from_fips_bybg.R          1
  # 9:        test_fips          10                   test-state_from_latlon.R          0
  
  # 10: test_fixcolnames          10                  test-utils_metadata_add.R          9
  # 11: test_fixcolnames          10                   test-fixcolnames_infer.R          1
  # 12: test_fixcolnames          10                         test-fixcolnames.R          0
  # 13: test_fixcolnames          10                            test-fixnames.R          0
  # 14: test_fixcolnames          10                    test-fixnames_to_type.R          0
  # 15: test_fixcolnames          10                             test-varinfo.R          0
  
  # 16:       test_naics           7                     test-naics_from_code.R          4
  # 17:       test_naics           7                    test-naics_validation.R          2
  # 18:       test_naics           7                     test-naics_from_name.R          1
  # 19:       test_naics           7                      test-naics2children.R          0
  # 20:       test_naics           7                    test-naics_categories.R          0
  # 21:       test_naics           7                 test-naics_findwebscrape.R          0
  # 22:       test_naics           7                      test-naics_from_any.R          0
  # 23:       test_naics           7            test-naics_subcodes_from_code.R          0
  
  # 24:         test_frs           5                    test-regid_from_naics.R          3
  # 25:         test_frs           5                  test-frs_from_programid.R          1
  # 26:         test_frs           5                        test-frs_is_valid.R          1
  # 27:         test_frs           5                      test-frs_from_naics.R          0
  # 28:         test_frs           5                      test-frs_from_regid.R          0
  # 29:         test_frs           5                        test-frs_from_sic.R          0
  
  # 30:      test_latlon           5                test-latlon_from_anything.R          5
  # 31:      test_latlon           5                         test-address_xyz.R          0
  # 32:      test_latlon           5                   test-latlon_as.numeric.R          0
  # 33:      test_latlon           5                     test-latlon_df_clean.R          0
  # 34:      test_latlon           5                 test-latlon_from_address.R          0
  # 35:      test_latlon           5                     test-latlon_from_sic.R          0
  # 36:      test_latlon           5        test-latlon_from_vectorofcsvpairs.R          0
  # 37:      test_latlon           5                        test-latlon_infer.R          0
  # 38:      test_latlon           5                     test-latlon_is.valid.R          0
  # 39:      test_latlon           5                test-state_from_sitetable.R          0
  
  # 40:   test_getblocks           4 test-getblocks_summarize_blocks_per_site.R          3
  # 41:   test_getblocks           4                     test-getblocksnearby.R          1
  # 42:   test_getblocks           4           test-getblocksnearby_from_fips.R          0
  # 43:   test_getblocks           4          test-getblocksnearbyviaQuadTree.R          0
  # 44:   test_getblocks           4                     test-radius_inferred.R          0
  
  # 45: test_ejscreenapi           3                    test-ejscreenapi_plus.R          2
  # 46: test_ejscreenapi           3                          test-ejscreenit.R          1
  # 47: test_ejscreenapi           3        test-ejscreenRESTbroker-functions.R          0
  # 48: test_ejscreenapi           3                         test-ejscreenapi.R          0
  # 49: test_ejscreenapi           3                        test-ejscreenapi1.R          0
  
  # 50:        test_maps           3                       test-MAP_FUNCTIONS.R          3
  
  # 51:         test_app           1                       test-ui_and_server.R          1
  
  # 52:         test_mod           0                     test-mod_save_report.R          0
  # 53:         test_mod           0                   test-mod_specify_sites.R          0
  # 54:         test_mod           0                    test-mod_view_results.R          0
  # 55:       test_shape           0                       test-shapefile_xyz.R          0
  # testgroup err_bygroup                                       file err_byfile
  # [1] "2024-08-24"
  
  consoleclear()
  
  x = testbygroup(testlist = partial_testlist)
  
  cat("\n\n                                         RESULTS THAT FAILED/WARNED/CANT     \n\n")
  print(x[x$flag + x$error_cant_test > 0,])
  
  # partial_testlist <- list(
  #   
  #   # test_fips = test_fips,
  #   # test_naics,
  #   # test_frs,
  #   # test_latlon,
  #   # test_maps = test_maps,
  #   # test_shape,
  #   # test_getblocks,
  #   # test_fixcolnames,
  #   # test_doag,
  #   # test_ejamit,
  #   # test_mod,
  #   test_app = test_app
  # )
  
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

# do this part manually?
y = askYesNo("View results of unit testing?") 
if (is.na(y)) {y = FALSE}
if (y) {
  consoleclear()
  ########################### #  ########################### #
  
  # HOW MANY TOTAL PASS/FAIL?
  
  colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
  print(round(100 * colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
              / sum(x$tests), 1))
  print(Sys.Date())
  
  # about 1.6 % of unit tests have errors   as of 7/1/24   ***********
  # (and ~1% have warnings)   ***********
  
  ########################### #  ########################### #
  
  ## WHICH TEST GROUPS or FILES HAVE THE MOST FAILING TESTS?
  
  x[ , flag_byfile := sum(flag), by = "file"]
  x[ , err_byfile  := sum(err),  by = "file"]
  
  x[ , flag_bygroup := sum(flag), by = "testgroup"]
  x[ , err_bygroup  := sum(err),  by = "testgroup"]
  
  setorder(x, -err_bygroup, testgroup, -flag, -failed, file)
  keygroups <- x[x$tests != x$passed | x$error_cant_test > 0, 1:11]
  print(keygroups )
  
  # 7/1/24
  #          testgroup                                   file                                               test tests passed failed   err warning  flag skipped error_cant_test
  # 
  #  1:      test_doag                     test-doaggregate.R   error if in inputs are null, empty, NA, or blank     7      1      3     3       3     6       0               0
  
  prioritize = x[order(-x$flag, -x$failed), 1:11]
  prioritize <- prioritize[prioritize$tests != prioritize$passed | prioritize$error_cant_test > 0, ]
  print(prioritize)
  # 7/1/24
  # >   prioritize
  #         testgroup                                   file                                               test tests passed failed   err warning  flag skipped error_cant_test
  # 1:      test_doag                     test-doaggregate.R   error if in inputs are null, empty, NA, or blank     7      1      3     3       3     6       0               0
  # 2:       test_frs                test-regid_from_naics.R    no crash (but fails to warn)  for invalid NAICS     4      2      2     2       0     2       0               0
  
  
  ########################### #
  
  ## WHICH GROUPS (OF FILES)? 
  #
  bygroup <- x[ , .(err = sum(err), warning = sum(warning), flag = sum(flag), 
                    passed = sum(passed), tests = sum(tests)), by = "testgroup"]
  print(bygroup)
  print(Sys.Date())
  
  #         testgroup   err warning  flag passed tests    as of 7/1/24 
  
  # 1:     test_naics     7       1     8     69    77
  
  # 2:       test_frs     4       0     4     56    60
  # 3:      test_doag     4       4     8     50    58
  # 4:    test_ejamit     4       0     4     43    47
  # 5: test_getblocks     2       0     2     19    21
  
  # 6:      test_fips     1       0     1    320   321
  # 7:    test_latlon     1       2     3     69    72
  # 8:      test_maps     1       1     2     15    17
  
  # 9:     test_shape     0       0     0     67    67
  #10:       test_mod     0       0     0      0     0
  #11:       test_app     0       0     0     10    10
  
  ## same thing a diff way:
  # y <- x[,  lapply(.(err, warning, flag, passed, tests), sum), by = "testgroup"]
  # setnames(y, new = c("testgroup","err", "warning", "flag", "passed", "tests"))
  # setorder(y, -err)
  
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
  print(byfile)
  print(Sys.Date())
  
  #          testgroup err_bygroup                                       file err_byfile
  
  #  1:     test_naics           7                     test-naics_from_code.R          4    ***  as of 7/1/24
  #  2:     test_naics           7                    test-naics_validation.R          2  ***
  #  9:      test_doag           4                         test-doaggregate.R          4  ***
  # 12:    test_ejamit           4            test-ejamit_compare_distances.R          3  ***
  # 16:       test_frs           4                    test-regid_from_naics.R          3  ***
  
  # 30:    test_latlon           1                test-latlon_from_anything.R          1
  # 38:      test_maps           1                       test-MAP_FUNCTIONS.R          1
  
  #          testgroup err_bygroup                                       file err_byfile
  
  
  
  #  WHICH TESTS, EXACTLY?
  
  
  print(x)
  
} # end of big if - viewing results

########################### #  ########################################## #

