testing_in_logical_order <- FALSE  # testing_in_logical_order <- TRUE
if (testing_in_logical_order) {
  
  library(data.table)
  library(magrittr)
  library(testthat)
  library(dplyr)
  
  # list.files("./tests/testthat")
  
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
    "test-siteid_from_naics.R", # 3/14 
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
  
  tgroup <- function(fnames = test_all, reporter = default_compact_reporter()) {
    for (i in 1:length(fnames)) {
      testthat::test_file(file.path("./tests/testthat/", fnames[i]),
                          reporter = reporter )
      cat("\n\n##########################################\n\n")
    }
  }

  
  tgroup(test_naics)  # 12  failed  
  tgroup(test_frs)    # 6 
  tgroup(test_latlon) # 
  tgroup(test_fips)   # 
  tgroup(test_shape)  # 
  tgroup(test_getblocks) # 
  tgroup(test_doag)      # 2?
  tgroup(test_ejamit)    # 
  tgroup(test_mod)       # 
  tgroup(test_app)
  
  
}



