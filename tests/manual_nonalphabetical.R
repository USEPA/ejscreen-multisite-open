# which test is run first? alphabetical is default but
# a line in DESCRIPTION can control order the package tests happen
# see https://testthat.r-lib.org/articles/parallel.html

#                                          file    nb warning passed notpassed
# 
# 1:                      test-FIPS_FUNCTIONS.R   318       0    309         9

# 2:           test-getblocksnearby_from_fips.R     9       6      3         6

# 3:                       test-MAP_FUNCTIONS.R    21       6     15         6


# 4:                         test-address_xyz.R    19       1     16         3

# 5:                    test-naics_validation.R    11       0      9         2

# 6:                     test-getblocksnearby.R    12       0     11         1

# 7:                        test-frs_is_valid.R     8       0      7         1

# 8:                         test-doaggregate.R     4       1      3         1

########################################## # 
########################################## # 

testing_in_logical_order <- FALSE
# testing_in_logical_order <- TRUE
if (testing_in_logical_order) {
  
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
      
      "test-MAP_FUNCTIONS.R",
      
      
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
    cat("need to add new files to list above and below")
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
      "test-latlon_from_address.R"  
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
    test_doag <- c(
      "test-varinfo.R",  
      "test-pctile_from_raw_lookup.R",  
      "test-doaggregate.R"     
    )
    test_ejamit <- c(
      "test-ejamit.R"  ,
      "test-ejam2barplot_sites.R",
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
  
  ## to TEST JUST ONE GROUP:
  
  partial_testlist = list(test_maps = "test-MAP_FUNCTIONS.R")
  # or 
  partial_testlist = list(test_fips = test_fips)
                            # c("test-FIPS_FUNCTIONS.R", "test-state_from_fips.R", "test-state_from_latlon.R"))
 
   test_partial = testbygroup(testlist = partial_testlist)
  
  
  
  
  
  # RUN ALL THE TESTS  ####

    ## takes 7 to 8 minutes or so
  
  system.time({
    testall = testbygroup(testlist = testlist)
  })

  save(testall, file = "./tests/results_of_unit_testing_2024-05-30.rda")
  
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  # LOOK AT RESULTS SUMMARIES ####
  
  ########################### #  ########################### #
  # HOW MANY TOTAL PASS/FAIL?
  
  x = testall
  x$err = x$nb - x$passed - x$warning
  colSums(x[, .(nb, passed, not, warning, err)])
  # >   colSums(x[, .(nb, passed, not, warning, err)])
  #  nb  passed     not warning     err 
  # 749     720      29      14      15   only 2% errors now
  ########################### #  ########################### #
  
  ## WHICH TEST GROUPS HAVE THE MOST FAILING TESTS?
  
   x[ , fails_byfile := sum(not), by = "file"]
   x[ , fails_bygroup := sum(not), by = "testgroup"]
   setorder(x, -fails_bygroup)
   # x[1:40, ]
   bygroup = x[ , .(err = sum(not) - sum(warning), warning = sum(warning), pass =  sum(nb) - sum(not), tests = sum(nb)), by = "testgroup"]
   bygroup 
   # 
   #         testgroup   err warning  pass tests 
   # 1:      test_fips     9       0   326   335   ********
   # 2: test_getblocks     1       6    23    30
   # 3:      test_maps     0       6    15    21
   # 4:    test_latlon     2       1   114   117
   # 5:     test_naics     2       0   109   111
   # 6:       test_frs     1       0    69    70
   # 7:      test_doag     0       1    23    24
   # 8:     test_shape     0       0    16    16
   # 9:    test_ejamit     0       0     9     9
   # 10:       test_mod     0       0     6     6
   # 11:       test_app     0       0    10    10
  
# same thing a diff way:  
    
  y = x[,  lapply(.(nb, warning, passed, not), sum), by = "testgroup"]
  
  setorder(y, -V4)
  setnames(y, new = c("testgroup", "nb", "warning", "passed", "notpassed"))
  y$err = y$notpassed - y$warning
  y
  #           testgroup    nb warning passed notpassed   err
  # <char> <int>   <int>  <int>     <int> <int>
  # 1:      test_fips   335       0    326         9     9
  # 2: test_getblocks    30       6     23         7     1
  # 3:      test_maps    21       6     15         6     0
  # 4:    test_latlon   117       1    114         3     2
  # 5:     test_naics   111       0    109         2     2
  # 6:       test_frs    70       0     69         1     1
  # 7:      test_doag    24       1     23         1     0
  # 8:     test_shape    16       0     16         0     0
  # 9:    test_ejamit     9       0      9         0     0
  # 10:      test_mod     6       0      6         0     0
  # 11:      test_app    10       0     10         0     0
  
  
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
  # <char>        <int>         <int>         <char>
  # 1:                      test-FIPS_FUNCTIONS.R            9             9      test_fips ***
  # 2:                     test-state_from_fips.R            0             9      test_fips
  # 3:                   test-state_from_latlon.R            0             9      test_fips
  # 4:           test-getblocksnearby_from_fips.R            6             7 test_getblocks  ***
  # 5:                     test-getblocksnearby.R            1             7 test_getblocks  **
  # 6:                     test-radius_inferred.R            0             7 test_getblocks
  # 7: test-getblocks_summarize_blocks_per_site.R            0             7 test_getblocks
  # 8:          test-getblocksnearbyviaQuadTree.R            0             7 test_getblocks
  # 9:                       test-MAP_FUNCTIONS.R            6             6      test_maps  ***
  # 10:                         test-address_xyz.R            3             3    test_latlon  **
  # 11:                        test-latlon_infer.R            0             3    test_latlon
  # 12:                   test-latlon_as.numeric.R            0             3    test_latlon
  # 13:                     test-latlon_df_clean.R            0             3    test_latlon
  # 14:                     test-latlon_is.valid.R            0             3    test_latlon
  # 15:                     test-latlon_from_sic.R            0             3    test_latlon
  # 16:                 test-latlon_from_address.R            0             3    test_latlon
  # 17:                    test-naics_validation.R            2             2     test_naics  **
  # 18:                    test-naics_categories.R            0             2     test_naics
  # 19:                 test-naics_findwebscrape.R            0             2     test_naics
  # 20:                      test-naics_from_any.R            0             2     test_naics
  # 21:                     test-naics_from_code.R            0             2     test_naics
  # 22:                     test-naics_from_name.R            0             2     test_naics
  # 23:            test-naics_subcodes_from_code.R            0             2     test_naics
  # 24:                      test-naics2children.R            0             2     test_naics
  # 25:                        test-frs_is_valid.R            1             1       test_frs  **
  # 26:                         test-doaggregate.R            1             1      test_doag  **
  # 27:                    test-regid_from_naics.R            0             1       test_frs
  # 28:                      test-frs_from_naics.R            0             1       test_frs
  # 29:                  test-frs_from_programid.R            0             1       test_frs
  # 30:                      test-frs_from_regid.R            0             1       test_frs
  # 31:                        test-frs_from_sic.R            0             1       test_frs
  # 32:                             test-varinfo.R            0             1      test_doag
  # 33:              test-pctile_from_raw_lookup.R            0             1      test_doag
  # 34:                       test-shapefile_xyz.R            0             0     test_shape
  # 35:                              test-ejamit.R            0             0    test_ejamit
  # 36:                     test-mod_save_report.R            0             0       test_mod
  # 37:                   test-mod_specify_sites.R            0             0       test_mod
  # 38:                    test-mod_view_results.R            0             0       test_mod
  # 39:                       test-ui_and_server.R            0             0       test_app
  # file fails_byfile fails_bygroup      testgroup
  
  
  byfile = x[,  lapply(.(nb, warning, passed, not), sum), by = "file"]
  setorder(byfile, -V4)
  setnames(byfile, new = c("file", "nb", "warning", "passed", "notpassed"))
  byfile
  
  #                                          file    nb warning passed notpassed
  # 
  # 1:                      test-FIPS_FUNCTIONS.R   318       0    309         9
  
  # 2:           test-getblocksnearby_from_fips.R     9       6      3         6
  
  # 3:                       test-MAP_FUNCTIONS.R    21       6     15         6
  
  # 4:                         test-address_xyz.R    19       1     16         3
  
  # 5:                    test-naics_validation.R    11       0      9         2
  
  # 6:                     test-getblocksnearby.R    12       0     11         1
  
  # 7:                        test-frs_is_valid.R     8       0      7         1
  
  # 8:                         test-doaggregate.R     4       1      3         1
   
  
  
   
  
} # end of big if
########################### #  ########################################## #

