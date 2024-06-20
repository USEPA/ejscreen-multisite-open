






# test_local()   would test the local source pkg

# test_package() would test an installed package

# test_check()   would test an installed package in the way that is used by R CMD check or check()





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
        x = testthat::test_file(
          file.path("./tests/testthat/", fnames[i]), 
          reporter = reporter)
        })

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
  
  # FUNCTION THAT RUNS ONE SPECIFIC GROUP OF TESTS AT A TIME
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
  
  ## to TEST JUST ONE FILE OR ONE GROUP:
  
  partial_testlist = list(test_ejamit = test_ejamit)
# or 
  partial_testlist = list(test_fips = test_fips)
                            # c("test-FIPS_FUNCTIONS.R", "test-state_from_fips.R", "test-state_from_latlon.R"))
 
  
   test_partial = testbygroup(testlist = partial_testlist)
  
  
  
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  
  # RUN ALL THE TESTS  ####

    ## takes 5 minutes or so
  
  system.time({
    testall = testbygroup(testlist = testlist)
  })
  #   user  system elapsed    5 minutes
  # 217.80   18.68  395.68 
  
  # save(testall, file = "./tests/results_of_unit_testing_2024-05-30.rda")
  save(testall, file = "./tests/results_of_unit_testing_2024-06-18_donutbranch.rda")
  
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
  # 790     756      34      14      20 
  
    #   2.5% of tests have errors and 1.7% have warnings   ***********
  
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
   
   # 1:      test_fips     9       0   330   339 
   
   # 2: test_getblocks     1       6    23    30
   # 3:      test_maps     0       6    15    21
   # 4:       test_frs     4       0    62    66
   # 5:     test_naics     2       1   109   112
   # 6:    test_latlon     2       1   112   115
   # 7:    test_ejamit     2       0    50    52
   
   # 8:     test_shape     0       0    16    16
   # 9:      test_doag     0       0    23    23
   #10:       test_mod     0       0     6     6
   #11:       test_app     0       0    10    10
  
# same thing a diff way:  
    
  y = x[,  lapply(.(nb, warning, passed, not), sum), by = "testgroup"]
  
  setorder(y, -V4)
  setnames(y, new = c("testgroup", "nb", "warning", "passed", "notpassed"))
  y$err = y$notpassed - y$warning
  y
  #         testgroup    nb warning passed notpassed   err
  
  # 1:      test_fips   339       0    330         9     9  ****
  # 2: test_getblocks    30       6     23         7     1
  # 3:      test_maps    21       6     15         6     0
  # 4:       test_frs    66       0     62         4     4 **
  # 5:     test_naics   112       1    109         3     2
  # 6:    test_latlon   115       1    112         3     2
  # 7:    test_ejamit    52       0     50         2     2
  
  
  ########################### #  ########################### #
  ## WHICH INDIVIDUAL FILES HAVE THE MOST FAILING TESTS?
  # 
    x[file %in% "test-naics_from_code.R", ]
  #     testgroup                   file                                               test skipped  error    nb warning passed   not fails_byfile fails_bygroup
  
  # 1: test_naics test-naics_from_code.R                no warning for standard code lookup   FALSE   TRUE     0       0      0     0            4             8
  # 2: test_naics test-naics_from_code.R results of subcategories only output when children   FALSE   TRUE     0       0      0     0            4             8
  
  # 3: test_naics test-naics_from_code.R warn if given naics code that is NA or text not li   FALSE   TRUE     8       0      4     4            4             8
  
  # 4: test_naics test-naics_from_code.R             list of queries returns joined results   FALSE   TRUE     0       0      0     0            4             8
  

  junk  = x[,  lapply(.(nb, warning, passed, not), sum), by = "file"]
  
  byfile <- x[ , .(fails_byfile = fails_byfile[1], fails_bygroup = fails_bygroup[1], testgroup = testgroup[1]), by = "file"]
  setorder(byfile, -fails_bygroup, -fails_byfile)
  byfile
  #                                           file fails_byfile fails_bygroup      testgroup
 
  #  1:                      test-FIPS_FUNCTIONS.R            9             9      test_fips
  #  4:           test-getblocksnearby_from_fips.R            6             7 test_getblocks
  
  #  5:                     test-getblocksnearby.R            1             7 test_getblocks
  #  9:                       test-MAP_FUNCTIONS.R            6             6      test_maps
  # 10:                    test-regid_from_naics.R            3             4       test_frs
  # 11:                        test-frs_is_valid.R            1             4       test_frs
  # 16:                         test-address_xyz.R            3             3    test_latlon
  # 17:                    test-naics_validation.R            2             3     test_naics
  # 18:                 test-naics_findwebscrape.R            1             3     test_naics
  # 31:            test-ejamit_compare_distances.R            1             2    test_ejamit
  # 32:     test-ejamit_compare_groups_of_places.R            1             2    test_ejamit
  # file fails_byfile fails_bygroup      testgroup
  
  
  
  byfile = x[,  lapply(.(nb, warning, passed, not), sum), by = "file"]
  setorder(byfile, -V4)
  setnames(byfile, new = c("file", "nb", "warning", "passed", "notpassed"))
  byfile
  
  # >   byfile
  #                                          file    nb warning passed notpassed  6/24
  #
  # 1:                      test-FIPS_FUNCTIONS.R   322       0    313         9  ****
  # 2:           test-getblocksnearby_from_fips.R     9       6      3         6  ****
  
  # 3:                       test-MAP_FUNCTIONS.R    21       6     15         6  ****
  
  # 4:                    test-regid_from_naics.R    10       0      7         3  **
  # 6:                    test-naics_validation.R    11       0      9         2  **
  
  # 5:                         test-address_xyz.R    19       1     16         3  **
  
  # 7:                     test-getblocksnearby.R    12       0     11         1
  # 8:                        test-frs_is_valid.R     8       0      7         1
  # 9:                 test-naics_findwebscrape.R     6       1      5         1
 # 10:            test-ejamit_compare_distances.R    31       0     30         1
 # 11:     test-ejamit_compare_groups_of_places.R     6       0      5         1
   
  
  
   
  
} # end of big if
########################### #  ########################################## #

