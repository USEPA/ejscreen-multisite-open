# which test to run first? alphabetical is default
# a line in DESCRIPTION can control order the package tests happen
# see https://testthat.r-lib.org/articles/parallel.html

testing_in_logical_order <- FALSE  # testing_in_logical_order <- TRUE
if (testing_in_logical_order) {


  sdir <- getwd()
  tnames1 <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
  tnames2 <- c(
    "test-ui_and_server.R",    #      [ FAIL 0 | WARN 1 | SKIP 0 | PASS 11 ]
    "test-ejamit.R",            #     [ FAIL 2 | WARN 2 | SKIP 0 | PASS 7 ]
    "test-doaggregate.R",       #     [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ]
    "test-getblocksnearby.R",    #    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]
    "test-getblocksnearbyviaQuadTree.R",    #    -------------- NEEDS MORE TESTS ***
    "test-getblocksnearby_from_fips.R",
    "test-getblocks_summarize_blocks_per_site.R",
    "test-pctile_from_raw_lookup.R",
    "test-radius_inferred.R",             # this is SLOW THOUGH

    "test-state_from_fips.R",
    "test-state_from_latlon.R",
    "test-shapefile_xyz.R",          #           [ FAIL 2 | WARN 0 | SKIP 0 | PASS 14 ]

    "test-naics_from_name.R",   #  -------------  [ FAIL 11 | WARN 2 | SKIP 0 | PASS 11 ]
    "test-naics_from_any.R",    #  -------------  [ FAIL 9 | WARN 16 | SKIP 0 | PASS 22 ]
    "test-naics_categories.R",   # not useful tests
    "test-naics_subcodes_from_code.R",
    "test-naics2children.R",
    "test-naics_validation.R",    #     [ FAIL 3 | WARN 0 | SKIP 0 | PASS 6 ]
    "test-naics_from_code.R",
    "test-naics_findwebscrape.R",
    "test-regid_from_naics.R",

    "test-frs_is_valid.R",           # [ FAIL 0 | WARN 7 | SKIP 0 | PASS 8 ]
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

    "test-fips_lead_zero.R",
    "test-fips_bg_from_anyfips.R",

    "test-varinfo.R",

    "test-mod_save_report.R",
    "test-mod_view_results.R",
    "test-mod_specify_sites.R",
    "test-golem_utils_ui.R",         #  not used
    "test-golem_utils_server.R"      #  not used
  )
  # setequal(tnames1,tnames2)
  # [1] TRUE


  test_naics <- c(
    "test-naics_categories.R",   # 0/8 fail
    "test-naics_findwebscrape.R", # 0/5
    "test-naics_from_any.R",   # 1/27
    "test-naics_from_code.R",  # 2/17
    "test-naics_from_name.R",  # 2 fail
    "test-naics_subcodes_from_code.R", # 3/12
    "test-naics_validation.R",  # 2/9
    "test-naics2children.R"     # 2/13
  )
  test_frs <- c(
    "test-regid_from_naics.R", # 3/14
    "test-frs_from_naics.R",    # 0/28
    "test-frs_from_programid.R", # 1
    "test-frs_from_regid.R",   # 1/2
    "test-frs_from_sic.R",    # 1/16
    "test-frs_is_valid.R"       #####   # 7/15
  )
  test_latlon <- c(
    "test-latlon_infer.R",        ##### # 5/19
    "test-latlon_as.numeric.R",
    "test-latlon_df_clean.R",
    "test-latlon_is.valid.R",   # 0/12
    "test-latlon_from_anything.R",   #####  # 6/42
    "test-latlon_from_sic.R"     # 0/18
  )
  test_fips <- c(
    "test-fips_lead_zero.R",   # 2/150
    "test-fips_bg_from_anyfips.R",  ##### # 6/43
    "test-state_from_fips.R",   # 3/7
    "test-state_from_latlon.R"  # 0/13
  )
  test_shape <- c(
    "test-shapefile_xyz.R"  # 0/12
  )
  test_getblocks <- c(
    "test-radius_inferred.R",  # 1/2
    "test-getblocks_summarize_blocks_per_site.R", # 0 /3
    "test-getblocksnearby.R",       # 0/16
    "test-getblocksnearby_from_fips.R", # 0/3
    "test-getblocksnearbyviaQuadTree.R" # 0/3
  )
  test_doag <- c(
    "test-varinfo.R",  #0/9
    "test-pctile_from_raw_lookup.R", ##### # 8/17
    "test-doaggregate.R"    # 2 / 42
  )
  test_ejamit <- c(
    "test-ejamit.R"  # 0 / 11
  )
  test_mod <- c(
    "test-mod_save_report.R",   # 1/6
    "test-mod_specify_sites.R", # 1/6
    "test-mod_view_results.R"   # 0/5
  )
  test_app <- c(
    "test-golem_utils_server.R", # 0/13
    "test-golem_utils_ui.R",  # 0/51
    "test-ui_and_server.R"    # 1/12
  )
  test_all <- c(
    test_naics,
    test_frs,
    test_latlon,
    test_fips,
    test_shape,
    test_getblocks,
    test_doag,
    test_ejamit,
    test_mod,
    test_app
  )
  setequal(tnames1, tnames2)
  setequal(tnames1, test_all)


#################################################### #

  # suppressPackageStartupMessages({
  #   library(EJAM)
  # })
  library(data.table)
  library(magrittr)
  library(testthat)
  library(dplyr)

  ########################### #
  # list.files("./tests/testthat")
  ## something like this would not really work:
    devtools::load_all()  # needed if you want to manually test, via these scripts, all the non-exported functions
  # sdir <- getwd()
  # source(file = file.path(sdir, "tests/testthat/setup.R"))
  # for (fname in list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-")) {
  #   print(fname)
  #   test_file(fname)
  # }
  ########################### #


  tgroup <- function(fnames = test_all, reporter = default_compact_reporter()) {
    for (i in 1:length(fnames)) {
      testthat::test_file(file.path("./tests/testthat/", fnames[i]),
                          reporter = reporter )
      cat("\n\nFinished testing ", fnames[i]," ##########################################\n\n")
    }
  }
  ########################### #

  # THIS RUNS ONE SPECIFIC GROUP OF TESTS AT A TIME

  tgroup(test_frs)
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 14 ]  Finished testing  test-regid_from_naics.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 28 ]  Finished testing  test-frs_from_naics.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]   Finished testing  test-frs_from_programid.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]   Finished testing  test-frs_from_regid.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]  Finished testing  test-frs_from_sic.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 8 ]   Finished testing  test-frs_is_valid.R

  tgroup(test_latlon) #
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 14 ] # Finished testing  test-latlon_infer.R         #- could not find function "latlon_infer"
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ] # Finished testing  test-latlon_as.numeric.R   #- could not find function "latlon_as.numeric"
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 43 ] # Finished testing  test-latlon_df_clean.R     #- could not find function "latlon_df_clean"
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 12 ] # Finished testing  test-latlon_is.valid.R
  # [ FAIL 0 |  WARN 1 | SKIP 0 | PASS 0 ]  # Finished testing  test-latlon_from_anything.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ] # Finished testing  test-latlon_from_sic.R

  tgroup(test_fips)   #
  # [ FAIL 1 | WARN 0 | SKIP 0 | PASS 148 ] # Finished testing  test-fips_lead_zero.R  #----  fails to warn if given text that cannot be interpreted as numeric  fips
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 47 ]  # Finished testing  test-fips_bg_from_anyfips.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]   # Finished testing  test-state_from_fips.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 11 ]  # Finished testing  test-state_from_latlon.R

  tgroup(test_shape)  #
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ] Finished testing  test-shapefile_xyz.R

  tgroup(test_getblocks) #

  # [ FAIL 0 |  WARN 1 | SKIP 0 | PASS 1 ]  Finished testing  test-radius_inferred.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]  Finished testing  test-getblocks_summarize_blocks_per_site.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]   Finished testing  test-getblocksnearby.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]  Finished testing  test-getblocksnearby_from_fips.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]   Finished testing  test-getblocksnearbyviaQuadTree.R

  tgroup(test_doag)      # 2?
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 9 ]  Finished testing  test-varinfo.R
  # [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ] Finished testing  test-doaggregate.R  --------------------------------

  tgroup(test_ejamit)    #
  # [ FAIL 2 | WARN 1 | SKIP 0 | PASS 7 ]  Finished testing  test-ejamit.R  --------------------------------

  tgroup(test_mod)       #
  # [ FAIL 0 |  WARN 1 | SKIP 0 | PASS 5 ] Finished testing  test-mod_save_report.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ] Finished testing  test-mod_specify_sites.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ] Finished testing  test-mod_view_results.R

  tgroup(test_app)
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]  Finished testing  test-golem_utils_server.R
  # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]  Finished testing  test-golem_utils_ui.R
  # [ FAIL 0 |  WARN 1 | SKIP 0 | PASS 11 ]  Finished testing  test-ui_and_server.R   Error in switch: EXPR must be a length 1 vector   line 80 approx

 tgroup(test_naics )  # 12  failed
 # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]  Finished testing  test-naics_categories.R
 # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ]  Finished testing  test-naics_findwebscrape.R
 # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 27 ] Finished testing  test-naics_from_any.R
 # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 24 ] Finished testing  test-naics_from_code.R
 # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 18 ] Finished testing  test-naics_from_name.R
 # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 11 ] Finished testing  test-naics_subcodes_from_code.R
 # [ FAIL 3 | WARN 0 | SKIP 0 | PASS 6 ]  Finished testing  test-naics_validation.R -----------------------------
 # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 10 ] Finished testing  test-naics2children.R

}
########################### #  ########################################## #

#  as of 5/13/2024
# 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 14 ] Finished testing  test-regid_from_naics.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 28 ] Finished testing  test-frs_from_naics.R 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]  Finished testing  test-frs_from_programid.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]  Finished testing  test-frs_from_regid.R 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ] Finished testing  test-frs_from_sic.R 
 # [ FAIL 1 | WARN 0 | SKIP 0 | PASS 7 ] Finished testing  test-frs_is_valid.R <<<<<<<<<<<# ── Failure (test-frs_is_valid.R:54:3): colname not REGISTRY_ID but seems to be ok alias, so check done for invalid numbers, so returns FALSE if invalid number ── # 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 14 ] Finished testing  test-latlon_infer.R 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ] Finished testing  test-latlon_as.numeric.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 43 ] Finished testing  test-latlon_df_clean.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 12 ] Finished testing  test-latlon_is.valid.R
 # [ FAIL 0 | WARN 1 | SKIP 0 | PASS 0 ]  Finished testing  test-latlon_from_anything.R   warn
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ] Finished testing  test-latlon_from_sic.R
 # [ FAIL 1 | WARN 0 | SKIP 0 | PASS 148 ] Finished testing  test-fips_lead_zero.R <<<<<<<<<<< # ── Failure (test-fips_lead_zero.R:336:3): should warn for text that cant be coerced into numeric FIPS ──
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 47 ] Finished testing  test-fips_bg_from_anyfips.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]  Finished testing  test-state_from_fips.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 11 ] Finished testing  test-state_from_latlon.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ] Finished testing  test-shapefile_xyz.R
 # [ FAIL 0 | WARN 1 | SKIP 0 | PASS 1 ] Finished testing  test-radius_inferred.R      Warning (test-radius_inferred.R:24:7): Estimate of radius, inferred from reported distances from getblocksnearby(), is accurate if radius specified with 2 or 3 decimals ──
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]  Finished testing  test-getblocks_summarize_blocks_per_site.
# [ FAIL 0 | WARN 0 | SKIP 1 | PASS 11 ] Finished testing  test-getblocksnearby.R 
 # [ FAIL 0 | WARN 2 | SKIP 0 | PASS 3 ]  Finished testing  test-getblocksnearby_from_fips.R    Warning 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]  Finished testing  test-getblocksnearbyviaQuadTree.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 9 ]  Finished testing  test-varinfo.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 11 ] Finished testing  test-pctile_from_raw_lookup.R
 # [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ] Finished testing  test-doaggregate.R  <<<<<<<<<<< # ── Error (test-doaggregate.R:49:1): NEED TO RECREATE THE TESTOUTPUT DATA NOW TO MAKE THIS PASS.
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 9 ] Finished testing  test-ejamit.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ] Finished testing  test-mod_save_report.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ] Finished testing  test-mod_specify_sites.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ] Finished testing  test-mod_view_results.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ] Finished testing  test-golem_utils_server.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ] Finished testing  test-golem_utils_ui.R
 # [ FAIL 0 | WARN 1 | SKIP 0 | PASS 11 ] Finished testing  test-ui_and_server.R   [test-ui_and_server.R#83]
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]  Finished testing  test-naics_categories.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ]  Finished testing  test-naics_findwebscrape.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 26 ] Finished testing  test-naics_from_any.R
 # [ FAIL 0 | WARN 1 | SKIP 0 | PASS 18 ] Finished testing  test-naics_from_name.R          warning
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 11 ] Finished testing  test-naics_subcodes_from_code.R
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 10 ] Finished testing  test-naics2children.R 
  # [ FAIL 4 | WARN 0 | SKIP 0 | PASS 21 ] Finished testing  test-naics_from_code.R  <<<<< THESE TESTS WERE FIXED SUBSEQUENTLY & passed

 # [ FAIL 2 | WARN 0 | SKIP 0 | PASS 6 ] Finished testing  test-naics_validation.R  <<<<<  naics_validation() fails some and is not a good function <<<<< but also added good function naics_is.valid() that tests OK


# done
########################### #  ########################################## #

