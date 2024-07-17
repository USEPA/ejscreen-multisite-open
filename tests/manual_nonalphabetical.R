# !diagnostics off
##   disables diagnostics within this document


# test_local()   would test the local source pkg

# test_package() would test an installed package

# test_check()   would test an installed package in the way that is used by R CMD check or check()


## Setting testing_in_logical_order to TRUE 
## and source-ing this whole file (or step by step)
## would run tests quietly, 
## one group of functions at a time, and print summaries to console.

testing_in_logical_order <- TRUE
# testing_in_logical_order <- FALSE


# which test is run first? alphabetical is default but
# a line in DESCRIPTION can control order the package tests happen
# see https://testthat.r-lib.org/articles/parallel.html


########################################## # 
########################################## # 


if (testing_in_logical_order) {
  
  library(testthat)
  
  if (interactive()) {
    useloadall = askYesNo("Do you want to load and test the current source code files version of EJAM (via devtools::load_all() etc.,
                        rather than testing the installed version)?", default = TRUE)
    if (useloadall) {
      devtools::load_all()
    } else {
      suppressPackageStartupMessages({
        library(EJAM)  # need or not?
      })
    }
  } else {
    suppressPackageStartupMessages({
      library(EJAM)  # need or not?
    })
  }
  
  dataload_from_pins("all")
  if (file.exists("./tests/testthat/setup.R")) {
    source("./tests/testthat/setup.R") #   asks if need load_all or library
  } else {
    cat("Need to source the setup.R file first \n")    
  }
  
  
  ########################################## # 
  
  # GET FULL LATEST LIST OF TEST FILES ####
  
  sdir <- getwd()
  tnames1 <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
  {
    tnames2 <- c(
      "test-FIPS_FUNCTIONS.R",
      
      "test-state_from_fips.R",
      "test-state_from_latlon.R",
      
      # "test-fips_lead_zero.R",
      # "test-fips_bg_from_anyfips.R",
      
      "test-ui_and_server.R",   
      "test-ejamit.R",           
      "test-doaggregate.R",     
      "test-getblocksnearby.R",  
      "test-getblocksnearbyviaQuadTree.R",    #    -------------- NEEDS MORE TESTS ***
      "test-getblocksnearby_from_fips.R",
      "test-getblocks_summarize_blocks_per_site.R",
      "test-pctile_from_raw_lookup.R",
      "test-radius_inferred.R",             # this is SLOW THOUGH
      
      "test-shapefile_xyz.R",       
      
      "test-ejam2barplot_sites.R",
      "test-ejamit_compare_distances.R",
      "test-ejamit_compare_groups_of_places.R",
      
      "test-naics_from_name.R", 
      "test-naics_from_any.R",   
      "test-naics_categories.R",   # not useful tests
      "test-naics_subcodes_from_code.R",
      "test-naics2children.R",
      "test-naics_validation.R",    
      "test-naics_from_code.R",
      "test-naics_findwebscrape.R",
      "test-regid_from_naics.R",
      
      "test-frs_is_valid.R",    
      "test-frs_from_regid.R",
      "test-frs_from_programid.R",
      "test-frs_from_naics.R",
      "test-frs_from_sic.R",
      
      "test-latlon_infer.R",
      "test-latlon_from_sic.R",
      "test-latlon_is.valid.R",
      "test-latlon_as.numeric.R",
      "test-latlon_df_clean.R",
      "test-latlon_from_anything.R",
      "test-address_xyz.R", 
      "test-latlon_from_address.R",
      "test-latlon_from_vectorofcsvpairs.R",
      
      "test-MAP_FUNCTIONS.R",
      
      "test-fixcolnames.R",
      "test-fixcolnames_infer.R",
      "test-varinfo.R",
      
      "test-mod_save_report.R",
      "test-mod_view_results.R",
      "test-mod_specify_sites.R",
      "test-golem_utils_ui.R",         #  not used
      "test-golem_utils_server.R"      #  not used
    )
  }
  ########################################## # 
  ## CHECK IF ALL ARE LISTED
  setdiff(tnames1, tnames2)
  setdiff(tnames2, tnames1)
  if (!setequal(tnames1, tnames2)) {
    cat("need to add new files to list above and below\n")
    stop("add new files to list above and below")
  }
  ########################################## # 
  {
    # PUT TEST FILES INTO LOGICAL GROUPS ####
    
    test_fips <- c(
      
      # "test-fips_lead_zero.R",   
      # "test-fips_bg_from_anyfips.R",    #   test_file("tests/testthat/test-fips_bg_from_anyfips.R")
      "test-FIPS_FUNCTIONS.R",
      "test-state_from_fips.R",  
      "test-state_from_latlon.R"   
    )
    test_naics <- c(
      "test-naics_categories.R",   
      "test-naics_findwebscrape.R", 
      "test-naics_from_any.R",   
      "test-naics_from_code.R",  
      "test-naics_from_name.R",   
      "test-naics_subcodes_from_code.R",
      "test-naics_validation.R",  
      "test-naics2children.R"    
    )
    test_frs <- c(
      "test-regid_from_naics.R", 
      "test-frs_from_naics.R",    
      "test-frs_from_programid.R",  
      "test-frs_from_regid.R",  
      "test-frs_from_sic.R",   
      "test-frs_is_valid.R"   
    )
    test_latlon <- c(
      "test-latlon_infer.R",        
      "test-latlon_as.numeric.R",
      "test-latlon_df_clean.R",
      "test-latlon_is.valid.R",   
      "test-latlon_from_anything.R",   
      "test-latlon_from_sic.R",
      "test-address_xyz.R", 
      "test-latlon_from_address.R",
      "test-latlon_from_vectorofcsvpairs.R"
    )
    
    test_maps <- c(
      "test-MAP_FUNCTIONS.R"
    )
    
    test_shape <- c(
      "test-shapefile_xyz.R"  
    )
    test_getblocks <- c(
      "test-radius_inferred.R",  
      "test-getblocks_summarize_blocks_per_site.R",  
      "test-getblocksnearby.R",        
      "test-getblocksnearby_from_fips.R", 
      "test-getblocksnearbyviaQuadTree.R" 
    )
    
    test_fixcolnames <- c(
      "test-fixcolnames.R",
      "test-fixcolnames_infer.R"
    )
    
    test_doag <- c(
      "test-varinfo.R",  
      "test-pctile_from_raw_lookup.R",  
      "test-doaggregate.R"     
    )
    test_ejamit <- c(
      "test-ejamit.R",
      "test-ejam2barplot_sites.R",
      "test-ejamit_compare_distances.R",
      "test-ejamit_compare_groups_of_places.R"
    )
    test_mod <- c(
      "test-mod_save_report.R",    
      "test-mod_specify_sites.R",  
      "test-mod_view_results.R"    
    )
    test_app <- c(
      "test-golem_utils_server.R", # not really used  
      "test-golem_utils_ui.R",     # not really used
      "test-ui_and_server.R"   
    )
  }

 # {
   ########################################## # 
    groupnames = c(
      "test_fips",
      "test_naics",
      "test_frs",
      "test_latlon",
      "test_maps",
      "test_shape",
      "test_getblocks",
      "test_fixcolnames",
      "test_doag",
      "test_ejamit",
      "test_mod",
      "test_app"
    )
  # }
  testlist <- sapply(groupnames  , FUN = get)
  ########################################## # 
  
  # CHECK THEY ARE ALL HERE AND IN GROUPS
  
  test_all <- as.vector(unlist(testlist))
  cat("\n\n   All test files found in folder are listed here and in a group = ",
      all.equal(sort(test_all), sort(tnames1)), "\n\n")
  if (!setequal(tnames1, test_all)) {
    if (any(duplicated(test_all))) {
      cat("some are listed in >1 group\n")
      warning("some are listed in >1 group")
    }
    cat("some are not listed in any group\n")
    stop("some are not listed in any group")
  }
  if (any(duplicated(test_all))) {
    cat("some are listed >1 group\n")
    stop("some are listed >1 group")
  }
  ########################################## # 
  
  #################################################### #
  
  library(testthat)
  library(data.table) # used in functions here
  library(magrittr)
  library(dplyr)
  ########################### #
  
  # FUNCTIONS TO COMPILE MINIMAL REPORTS ON GROUPS OF test FILES ####
  
  test1group <- function(fnames = test_all, 
                         reporter = "minimal" # some of the code below now only works if using this setting
                         # reporter = default_compact_reporter() # 
  ) {
    xtable <- list()
    tfile <- tempfile("junk", fileext = "txt")
    for (i in 1:length(fnames)) {
      cat(".")
      suppressWarnings(suppressMessages({
        
        junk <- capture_output({
          x <- testthat::test_file(
            file.path("./tests/testthat/", fnames[i]), 
            reporter = reporter)
        })
        
      }))
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
  
  # FUNCTION THAT RUNS ONE SPECIFIC GROUP OF TESTS AT A TIME
  
  testbygroup <- function(testlist) {
    
    # testlist[[(names(testlist)[[1]])]] is the same as get(names(testlist)[[1]]), vector of filenames
    xtable <- list()
    i <- 0
    for (tgroupname in names(testlist)) {
      i <- i + 1
      cat("", tgroupname, "group has", length(testlist[[tgroupname]]), "test files ")
      # print(data.frame(files = testlist[[tgroupname]]))
      xtable[[i]] <- data.table::data.table(testgroup = tgroupname, test1group(testlist[[tgroupname]]) )
    }
    xtable <- data.table::rbindlist(xtable)
    return(xtable)
  }
  ########################### #
  
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
} # end if, setup

########################### #  ########################################## #
########################### #  ########################################## #

if (testing_in_logical_order) {
  
  
  ## to TEST JUST ONE FILE OR ONE GROUP:
  
  #    test_file("./tests/testthat/test-MAP_FUNCTIONS.R" )
  ## or
  
  partial_testlist <- list(
    
    # test_fips = test_fips,
    # test_naics,
    # test_frs,
    # test_latlon,
    test_maps = test_maps,
    # test_shape,
    # test_getblocks,
    # test_fixcolnames,
    # test_doag,
    # test_ejamit,
    # test_mod,
    test_app = test_app
  )
  # or 
  #   partial_testlist <- list(test_fips =  c( "test-state_from_fips.R"))
  
  ## uncomment this to use it: 
  # 
  #        test_partial <- testbygroup(testlist = partial_testlist)
  
  ########################### #  ########################################## #
  
  # RUN ALL THE TESTS  ####
  
  ## takes almost 5 minutes 
  
  x <- system.time({
    testall <- testbygroup(testlist = testlist)
  })
  print(x)
  #   user  system elapsed    5 minutes
  # 217.80   18.68  395.68 
  
  
} # end  if - run

########################### #  ########################################## #
########################### #  ########################################## #
########################### #  ########################################## #


if (testing_in_logical_order) {
  
  # save(testall, file = "./tests/results_of_unit_testing_2024-05-30.rda")
  fname <- paste0("./tests/results_of_unit_testing_", as.character(Sys.Date()), ".rda")
  save(testall, file = fname)
  
} # end if - save

########################### #  ########################################## #
########################### #  ########################################## #
########################### #  ########################################## #


if (testing_in_logical_order) {
  
  # LOOK AT RESULTS SUMMARIES ####
  
  ########################### #  ########################### #
  
  # HOW MANY TOTAL PASS/FAIL?
  
  x <- testall
  colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
  print(round(100 * colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
              / sum(x$tests), 1))
  print(Sys.Date())
  
   # 7/1/24
  
  # err         warning           tests          passed          failed         warning            flag         skipped error_cant_test 
  # 14              11             892             867              12              11              25               2               7 

  # err         warning           tests          passed          failed         warning            flag         skipped error_cant_test 
  # 1.6             1.2           100.0            97.2             1.3             1.2             2.8             0.2             0.8 
  
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
  keygroups
  
  # 7/1/24
  #          testgroup                                   file                                               test tests passed failed   err warning  flag skipped error_cant_test
  # 
  #  1:      test_doag                     test-doaggregate.R   error if in inputs are null, empty, NA, or blank     7      1      3     3       3     6       0               0
  #  2:      test_doag                     test-doaggregate.R radius param to doag that is 1.5x as big as radius     2      0      1     1       1     2       0               0
  #  3:      test_doag                     test-doaggregate.R same result if radius requested is 32 or 50, since     2      1      0     0       1     1       0               0
  #  4:      test_doag                     test-doaggregate.R still same exact results_bysite as previously save     0      0      0     0       0     0       0               1
  #  5:      test_doag                     test-doaggregate.R still same exact results_bybg_people as previously     0      0      0     0       0     0       0               1
  #  6:      test_doag                     test-doaggregate.R     still same exact longnames as previously saved     0      0      0     0       0     0       0               1

    #  7:       test_frs                test-regid_from_naics.R    no crash (but fails to warn)  for invalid NAICS     4      2      2     2       0     2       0               0
  #  8:       test_frs                    test-frs_is_valid.R colname not REGISTRY_ID but seems to be ok alias,      1      0      1     1       0     1       0               0
  #  9:       test_frs                test-regid_from_naics.R no crash (but fails to warn) if not present in dat     2      1      1     1       0     1       0               0
  # 10:       test_frs                test-regid_from_naics.R                             lookup works correctly     1      1      0     0       0     0       0               1
  # 11:       test_frs                test-regid_from_naics.R                                      works in list     1      1      0     0       0     0       0               1
  # 12:    test_ejamit        test-ejamit_compare_distances.R                   ejamit_compare_distances() works     5      4      1     1       0     1       0               0
  # 13:    test_ejamit test-ejamit_compare_groups_of_places.R                              works if only 1 point     1      0      0     1       0     1       1               0
  # 14:     test_naics                test-naics_validation.R fake NAICS in naics_validation() should report tha     3      1      1     1       1     2       0               0
  # 15:     test_naics                test-naics_validation.R is multiple values for naics_validation(naics_sele     1      0      1     1       0     1       0               0
  # 16: test_getblocks                 test-getblocksnearby.R            getblocksnearby() same results as saved     1      0      1     1       0     1       0               0
  # 17: test_getblocks                 test-radius_inferred.R Estimate of radius, inferred from reported distanc     2      1      0     0       1     1       0               0
  # 18:    test_latlon                     test-address_xyz.R                          latlon_from_address works     3      2      0     0       1     1       0               0
  # 19:    test_latlon            test-latlon_from_anything.R                latlon_from_anything works with csv     2      1      0     0       1     1       0               0
  # 20:    test_latlon            test-latlon_from_anything.R               latlon_from_anything works with xlsx     1      0      0     1       0     1       1               0
  # 21:    test_latlon               test-latlon_as.numeric.R                            empty vector returns NA     0      0      0     0       0     0       0               1
  # 22:      test_maps                   test-MAP_FUNCTIONS.R                         mapfastej_counties() works     4      2      0     0       2     2       0               0
  # 23:     test_shape                   test-shapefile_xyz.R        shapefile_from_any(testfilenameset_4) works     0      0      0     0       0     0       0               1
  #          testgroup                                   file                                               test tests passed failed   err warning  flag skipped error_cant_test
  
  
  prioritize = x[order(-x$flag, -x$failed), 1:11]
  prioritize <- prioritize[prioritize$tests != prioritize$passed | prioritize$error_cant_test > 0, ]
  prioritize
  # 7/1/24
  # >   prioritize
  #         testgroup                                   file                                               test tests passed failed   err warning  flag skipped error_cant_test
  # 1:      test_doag                     test-doaggregate.R   error if in inputs are null, empty, NA, or blank     7      1      3     3       3     6       0               0
  # 2:       test_frs                test-regid_from_naics.R    no crash (but fails to warn)  for invalid NAICS     4      2      2     2       0     2       0               0
  # 3:      test_doag                     test-doaggregate.R radius param to doag that is 1.5x as big as radius     2      0      1     1       1     2       0               0
  # 4:     test_naics                test-naics_validation.R fake NAICS in naics_validation() should report tha     3      1      1     1       1     2       0               0
  # 5:      test_maps                   test-MAP_FUNCTIONS.R                         mapfastej_counties() works     4      2      0     0       2     2       0               0
  # 6:       test_frs                    test-frs_is_valid.R colname not REGISTRY_ID but seems to be ok alias,      1      0      1     1       0     1       0               0
  # 7:       test_frs                test-regid_from_naics.R no crash (but fails to warn) if not present in dat     2      1      1     1       0     1       0               0
  # 8:    test_ejamit        test-ejamit_compare_distances.R                   ejamit_compare_distances() works     5      4      1     1       0     1       0               0
  # 9:     test_naics                test-naics_validation.R is multiple values for naics_validation(naics_sele     1      0      1     1       0     1       0               0
  #10: test_getblocks                 test-getblocksnearby.R            getblocksnearby() same results as saved     1      0      1     1       0     1       0               0
  #11:      test_doag                     test-doaggregate.R same result if radius requested is 32 or 50, since     2      1      0     0       1     1       0               0
  #12:    test_ejamit test-ejamit_compare_groups_of_places.R                              works if only 1 point     1      0      0     1       0     1       1               0
  #13: test_getblocks                 test-radius_inferred.R Estimate of radius, inferred from reported distanc     2      1      0     0       1     1       0               0
  #14:    test_latlon                     test-address_xyz.R                          latlon_from_address works     3      2      0     0       1     1       0               0
  #15:    test_latlon            test-latlon_from_anything.R                latlon_from_anything works with csv     2      1      0     0       1     1       0               0
  #16:    test_latlon            test-latlon_from_anything.R               latlon_from_anything works with xlsx     1      0      0     1       0     1       1               0
  #17:      test_doag                     test-doaggregate.R still same exact results_bysite as previously save     0      0      0     0       0     0       0               1
  #18:      test_doag                     test-doaggregate.R still same exact results_bybg_people as previously     0      0      0     0       0     0       0               1
  #19:      test_doag                     test-doaggregate.R     still same exact longnames as previously saved     0      0      0     0       0     0       0               1
  #20:       test_frs                test-regid_from_naics.R                             lookup works correctly     1      1      0     0       0     0       0               1
  #21:       test_frs                test-regid_from_naics.R                                      works in list     1      1      0     0       0     0       0               1
  #22:    test_latlon               test-latlon_as.numeric.R                            empty vector returns NA     0      0      0     0       0     0       0               1
  #23:     test_shape                   test-shapefile_xyz.R        shapefile_from_any(testfilenameset_4) works     0      0      0     0       0     0       0               1
  #testgroup                                   file                                               test tests passed failed   err warning  flag skipped error_cant_test
  #
  
  
  ########################### #
  
  ## WHICH GROUPS (OF FILES)? 
  #
  bygroup <- x[ , .(err = sum(err), warning = sum(warning), flag = sum(flag), 
                    passed = sum(passed), tests = sum(tests)), by = "testgroup"]
  bygroup
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
  # 
  
  byfile <- x[ , .(
    err_byfile = err_byfile[1], 
    err_bygroup = err_bygroup[1],
    testgroup = testgroup[1]
  ), 
  by = "file"]
  setorder(byfile, -err_bygroup, testgroup, -err_byfile, file)
  setcolorder(byfile, neworder = c("testgroup", "err_bygroup", "file", "err_byfile"))
  byfile
  print(Sys.Date())
  
  #          testgroup err_bygroup                                       file err_byfile
  
  #  1:     test_naics           7                     test-naics_from_code.R          4    ***  as of 7/1/24
  #  2:     test_naics           7                    test-naics_validation.R          2  ***
  #  3:     test_naics           7                     test-naics_from_name.R          1
  #  4:     test_naics           7                      test-naics2children.R          0
  #  5:     test_naics           7                    test-naics_categories.R          0
  #  6:     test_naics           7                 test-naics_findwebscrape.R          0
  #  7:     test_naics           7                      test-naics_from_any.R          0
  #  8:     test_naics           7            test-naics_subcodes_from_code.R          0
  
  #  9:      test_doag           4                         test-doaggregate.R          4  ***
  # 10:      test_doag           4              test-pctile_from_raw_lookup.R          0
  # 11:      test_doag           4                             test-varinfo.R          0
  
  # 12:    test_ejamit           4            test-ejamit_compare_distances.R          3  ***
  # 13:    test_ejamit           4     test-ejamit_compare_groups_of_places.R          1
  # 14:    test_ejamit           4                  test-ejam2barplot_sites.R          0
  # 15:    test_ejamit           4                              test-ejamit.R          0
  
  # 16:       test_frs           4                    test-regid_from_naics.R          3  ***
  # 17:       test_frs           4                        test-frs_is_valid.R          1
  # 18:       test_frs           4                      test-frs_from_naics.R          0
  # 19:       test_frs           4                  test-frs_from_programid.R          0
  # 20:       test_frs           4                      test-frs_from_regid.R          0
  # 21:       test_frs           4                        test-frs_from_sic.R          0
  
  # 22: test_getblocks           2 test-getblocks_summarize_blocks_per_site.R          1
  # 23: test_getblocks           2                     test-getblocksnearby.R          1
  # 24: test_getblocks           2           test-getblocksnearby_from_fips.R          0
  # 25: test_getblocks           2          test-getblocksnearbyviaQuadTree.R          0
  # 26: test_getblocks           2                     test-radius_inferred.R          0
  
  # 27:      test_fips           1                      test-FIPS_FUNCTIONS.R          1
  # 28:      test_fips           1                     test-state_from_fips.R          0
  # 29:      test_fips           1                   test-state_from_latlon.R          0
  
  # 30:    test_latlon           1                test-latlon_from_anything.R          1
  # 31:    test_latlon           1                         test-address_xyz.R          0
  # 32:    test_latlon           1                   test-latlon_as.numeric.R          0
  # 33:    test_latlon           1                     test-latlon_df_clean.R          0
  # 34:    test_latlon           1                 test-latlon_from_address.R          0
  # 35:    test_latlon           1                     test-latlon_from_sic.R          0
  # 36:    test_latlon           1                        test-latlon_infer.R          0
  # 37:    test_latlon           1                     test-latlon_is.valid.R          0
  
  # 38:      test_maps           1                       test-MAP_FUNCTIONS.R          1
  
  # 39:       test_app           0                       test-ui_and_server.R          0
  # 40:       test_mod           0                     test-mod_save_report.R          0
  # 41:       test_mod           0                   test-mod_specify_sites.R          0
  # 42:       test_mod           0                    test-mod_view_results.R          0
  # 43:     test_shape           0                       test-shapefile_xyz.R          0
  
  #          testgroup err_bygroup                                       file err_byfile
  
  
  
  #  WHICH TESTS, EXACTLY?
  
  
  x
  
} # end of big if - viewing results

########################### #  ########################################## #

