# which test is run first? alphabetical is default but
# a line in DESCRIPTION can control order the package tests happen
# see https://testthat.r-lib.org/articles/parallel.html

testing_in_logical_order <- FALSE
# testing_in_logical_order <- TRUE
if (testing_in_logical_order) {
  
  ########################################## # 
  
  # GET FULL LATEST LIST OF TEST FILES ####
  
  sdir <- getwd()
  tnames1 <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
  {
    tnames2 <- c(
      "test-ui_and_server.R",   
      "test-ejamit.R",           
      "test-doaggregate.R",     
      "test-getblocksnearby.R",  
      "test-getblocksnearbyviaQuadTree.R",    #    -------------- NEEDS MORE TESTS ***
      "test-getblocksnearby_from_fips.R",
      "test-getblocks_summarize_blocks_per_site.R",
      "test-pctile_from_raw_lookup.R",
      "test-radius_inferred.R",             # this is SLOW THOUGH
      
      "test-state_from_fips.R",
      "test-state_from_latlon.R",
      "test-shapefile_xyz.R",       
      
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
      
      "test-fips_lead_zero.R",
      "test-fips_bg_from_anyfips.R",
      
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
  if (!setequal(tnames1, tnames2)) {
    cat("need to add new files to list above and below")
    stop("add new files to list above and below")
  }
  ########################################## # 
  {
    # PUT TEST FILES INTO LOGICAL GROUPS ####
    
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
      "test-latlon_from_address.R"  
    )
    test_fips <- c(
      "test-fips_lead_zero.R",   
      "test-fips_bg_from_anyfips.R",    #   test_file("tests/testthat/test-fips_bg_from_anyfips.R")
      "test-state_from_fips.R",  
      "test-state_from_latlon.R"   
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
    test_doag <- c(
      "test-varinfo.R",  
      "test-pctile_from_raw_lookup.R",  
      "test-doaggregate.R"     
    )
    test_ejamit <- c(
      "test-ejamit.R"  
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
 {
   ########################################## # 
    groupnames = c(
      "test_naics",
      "test_frs",
      "test_latlon",
      "test_fips",
      "test_shape",
      "test_getblocks",
      "test_doag",
      "test_ejamit",
      "test_mod",
      "test_app"
    )
  }
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
  # these should get done by the setup.R file in test folder actually
  library(data.table) # used in functions here
  library(magrittr)
  library(testthat)
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
        sink(file = "junk.txt", append = FALSE)
        on.exit({suppressWarnings(sink(NULL)); file.remove("junk.txt")})
        x = testthat::test_file(
          file.path("./tests/testthat/", fnames[i]), 
          reporter = reporter)
        suppressWarnings(sink(NULL))
      }))
      x = as.data.frame(x)
      x = x[, c( 'file' ,  'test', 
                 'skipped', 'error',
                 'nb', 'warning' ,'passed')]
      x$not <- x$nb - x$passed
      x$test = substr(x$test, 1, 50) # some are long
      xtable[[i]] <- data.table::data.table(x)
    }
    cat("\n")
    xtable <- data.table::rbindlist(xtable)
    print(colSums(xtable[, .(nb, warning, passed, not)]))
    return(xtable)
  }
  ########################### #
  
  # THIS RUNS ONE SPECIFIC GROUP OF TESTS AT A TIME
  testbygroup <- function(testlist) {
    # testlist[[(names(testlist)[[1]])]] is the same as get(names(testlist)[[1]]), vector of filenames
    xtable = list()
    i = 0
    for (tgroupname in names(testlist)) {
      i = i + 1
      cat("", tgroupname, "group has", length(testlist[[tgroupname]]), "test files ")
      # print(data.frame(files = testlist[[tgroupname]]))
      xtable[[i]] <- data.table::data.table(testgroup = tgroupname, test1group(testlist[[tgroupname]]) )
    }
    xtable = data.table::rbindlist(xtable)
    return(xtable)
  }
  ########################### #
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  # RUN ALL THE TESTS  ####
  
  Sys.time()
  
  x = testbygroup(testlist = testlist)
  
  Sys.time()  
  
  # save(x, file = "./tests/results_of_unit_testing_2024-05-20b.rda")
  
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  # LOOK AT RESULTS SUMMARIES ####
  
  ########################### #  ########################### #
  # HOW MANY TOTAL PASS/FAIL?
  
  colSums(x[, .(nb,warning,passed, not)])
  #  nb warning  passed     not 
  # 395      23     346      49  LOOKS LIKE ROUGHLY  7 % OF TESTS FAIL WITH ERROR AND 6 % WARN 
  
  ########################### #  ########################### #
  
  ## WHICH TEST GROUPS HAVE THE MOST FAILING TESTS?
  
   x[ , fails_byfile := sum(not), by = "file"]
   x[ , fails_bygroup := sum(not), by = "testgroup"]
   setorder(x, -fails_bygroup)
   # x[1:40, ]
   bygroup = x[ , .(err = sum(not) - sum(warning), warning = sum(warning), pass =  sum(nb) - sum(not), tests = sum(nb)), by = "testgroup"]
   bygroup 
   
   #          testgroup   err warning  pass tests
   #  
   # 1:      test_fips    15      17    88   120   *** test-fips_lead_zero.R (2digit, 3digit, etc.),  and
                                              #      test-fips_bg_from_anyfips.R ("what if inputs are invalid text strings") have the problematic tests
   # 2:     test_naics     7       1    69    77   *** test-naics_from_code.R mostly ("warn if given naics code that is NA or ...")
   
   # 3: test_getblocks     1       4    21    26   test-getblocksnearby_from_fips.R mostly
   # 4:    test_latlon     2       1    47    50   test-address_xyz.R mostly
   # 5:       test_frs     1       0    63    64
   # 6:      test_doag     0       0    23    23
   # 7:    test_ejamit     0       0     9     9
   # 8:       test_app     0       0    10    10
   # 9:     test_shape     0       0    16    16
   #10:       test_mod     0       0     0     0
  
# same thing a diff way:  
    
  y = x[,  lapply(.(nb, warning, passed, not), sum), by = "testgroup"]
  
  setorder(y, -V4)
  setnames(y, new = c("testgroup", "nb", "warning", "passed", "notpassed"))
  y$err = y$notpassed - y$warning
  y
  #          testgroup    nb warning passed notpassed  err
  
  # 1:      test_fips   120      17     88        32    15
  # 2:     test_naics    77       1     69         8     7
  # 3: test_getblocks    26       4     21         5     1
  # 4:    test_latlon    50       1     47         3     2
  # 5:       test_frs    64       0     63         1     1
  # 6:     test_shape    16       0     16         0     0
  # 7:      test_doag    23       0     23         0     0
  # 8:    test_ejamit     9       0      9         0     0
  # 9:       test_mod     0       0      0         0     0
  # 10:       test_app    10       0     10         0     0
  
  
  ########################### #  ########################### #
  ## WHICH INDIVIDUAL FILES HAVE THE MOST FAILING TESTS?
  # 
  # x[file %in% "test-naics_from_code.R", ]
  #     testgroup                   file                                               test skipped  error    nb warning passed   not fails_byfile fails_bygroup
  
  # 1: test_naics test-naics_from_code.R                no warning for standard code lookup   FALSE   TRUE     0       0      0     0            4             8
  # 2: test_naics test-naics_from_code.R results of subcategories only output when children   FALSE   TRUE     0       0      0     0            4             8
  
  # 3: test_naics test-naics_from_code.R warn if given naics code that is NA or text not li   FALSE   TRUE     8       0      4     4            4             8
  
  # 4: test_naics test-naics_from_code.R             list of queries returns joined results   FALSE   TRUE     0       0      0     0            4             8
  

  junk  = x[,  lapply(.(nb, warning, passed, not), sum), by = "file"]
  
  byfile <- x[ , .(fails_byfile = fails_byfile[1], fails_bygroup = fails_bygroup[1], testgroup = testgroup[1]), by = "file"]
  setorder(byfile, -fails_bygroup, -fails_byfile)
  byfile
  #                                          file fails_byfile fails_bygroup      testgroup
  
  # 1:                      test-fips_lead_zero.R           17            32      test_fips
  # 2:                test-fips_bg_from_anyfips.R           12            32      test_fips
  # 3:                     test-state_from_fips.R            3            32      test_fips
  # 4:                   test-state_from_latlon.R            0            32      test_fips
  
  # 5:                     test-naics_from_code.R            4             8     test_naics
  # 6:                    test-naics_validation.R            2             8     test_naics
  # 7:                     test-naics_from_name.R            1             8     test_naics
  # 8:                 test-naics_findwebscrape.R            1             8     test_naics
  # 9:                      test-naics_from_any.R            0             8     test_naics
  # 10:            test-naics_subcodes_from_code.R            0             8     test_naics
  # 11:                      test-naics2children.R            0             8     test_naics
  # 12:                    test-naics_categories.R            0             8     test_naics
  
  # 13:           test-getblocksnearby_from_fips.R            3             5 test_getblocks
  # 14:                     test-getblocksnearby.R            1             5 test_getblocks
  # 15:                     test-radius_inferred.R            1             5 test_getblocks
  # 16: test-getblocks_summarize_blocks_per_site.R            0             5 test_getblocks
  # 17:          test-getblocksnearbyviaQuadTree.R            0             5 test_getblocks
  
  # 18:                         test-address_xyz.R            3             3    test_latlon
  # 19:                     test-latlon_df_clean.R            0             3    test_latlon
  # 20:                   test-latlon_as.numeric.R            0             3    test_latlon
  # 21:                        test-latlon_infer.R            0             3    test_latlon
  # 22:                     test-latlon_is.valid.R            0             3    test_latlon
  # 23:                     test-latlon_from_sic.R            0             3    test_latlon
  # 24:                 test-latlon_from_address.R            0             3    test_latlon
  
  # 25:                        test-frs_is_valid.R            1             1       test_frs
  # 26:                      test-frs_from_naics.R            0             1       test_frs
  # 27:                    test-regid_from_naics.R            0             1       test_frs
  # 28:                  test-frs_from_programid.R            0             1       test_frs
  # 29:                      test-frs_from_regid.R            0             1       test_frs
  # 30:                        test-frs_from_sic.R            0             1       test_frs
  
  
  byfile = x[,  lapply(.(nb, warning, passed, not), sum), by = "file"]
  setorder(byfile, -V4)
  setnames(byfile, new = c("file", "nb", "warning", "passed", "notpassed"))
  byfile
  #                                          file    nb warning passed notpassed
  #        
  # 1:                      test-fips_lead_zero.R    41       2     24        17  ****
  # 2:                test-fips_bg_from_anyfips.R    59      12     47        12  ****
  
  # 3:                     test-naics_from_code.R     8       0      4         4
  # 4:                         test-address_xyz.R    19       1     16         3
  # 5:                     test-state_from_fips.R     9       3      6         3
  # 6:           test-getblocksnearby_from_fips.R     6       3      3         3
  # 7:                    test-naics_validation.R     8       0      6         2
  # 8:                 test-naics_findwebscrape.R     6       1      5         1
  # 9:                     test-naics_from_name.R     2       0      1         1
  #10:                        test-frs_is_valid.R     2       0      1         1
  #11:                     test-radius_inferred.R     2       1      1         1
  #12:                     test-getblocksnearby.R    12       0     11         1
  
  # 13:                    test-naics_categories.R     6       0      6         0
  # 14:                      test-naics_from_any.R    26       0     26         0
  # 15:            test-naics_subcodes_from_code.R    11       0     11         0
  # 16:                      test-naics2children.R    10       0     10         0
  # 17:                    test-regid_from_naics.R    14       0     14         0
  # 18:                      test-frs_from_naics.R    28       0     28         0
  # 19:                  test-frs_from_programid.R     2       0      2         0
  # 20:                      test-frs_from_regid.R     2       0      2         0
  # 21:                        test-frs_from_sic.R    16       0     16         0
  # 22:                        test-latlon_infer.R     0       0      0         0
  # 23:                   test-latlon_as.numeric.R     3       0      3         0
  # 24:                     test-latlon_df_clean.R     0       0      0         0
  # 25:                     test-latlon_is.valid.R     2       0      2         0
  # 26:                     test-latlon_from_sic.R    16       0     16         0
  # 27:                 test-latlon_from_address.R    10       0     10         0
  # 28:                   test-state_from_latlon.R    11       0     11         0
  # 29:                       test-shapefile_xyz.R    16       0     16         0
  # 30: test-getblocks_summarize_blocks_per_site.R     3       0      3         0
  # 31:          test-getblocksnearbyviaQuadTree.R     3       0      3         0
  # 32:                             test-varinfo.R     9       0      9         0
  # 33:              test-pctile_from_raw_lookup.R    11       0     11         0
  # 34:                         test-doaggregate.R     3       0      3         0
  # 35:                              test-ejamit.R     9       0      9         0
  # 36:                     test-mod_save_report.R     0       0      0         0
  # 37:                   test-mod_specify_sites.R     0       0      0         0
  # 38:                    test-mod_view_results.R     0       0      0         0
  # 39:                       test-ui_and_server.R    10       0     10         0
  #                                           file    nb warning passed notpassed
  
  
  ########################### #  ########################### #
  ## which tests somehow cause multiple warnings or errors?
  
 
#          testgroup                             file                                               test skipped  error    nb warning passed   not
  
#             <char>                           <char>                                             <char>  <lgcl> <lgcl> <int>   <int>  <int> <int>
  
#  1:      test_fips      test-fips_bg_from_anyfips.R            what if inputs are invalid text strings   FALSE  FALSE    16      12      4    12 ****
#  2:     test_naics           test-naics_from_code.R warn if given naics code that is NA or text not li   FALSE   TRUE     8       0      4     4 **
#  3:      test_fips           test-state_from_fips.R actually does not warn but does not crash, for inv   FALSE  FALSE     7       3      4     3 **
#  4: test_getblocks test-getblocksnearby_from_fips.R             getblocksnearby_from_fips works at all   FALSE  FALSE     6       3      3     3 **
#  5:      test_fips            test-fips_lead_zero.R should warn for text that cant be coerced into num   FALSE  FALSE     3       2      1     2 **

  
  
} # end of big if
########################### #  ########################################## #

