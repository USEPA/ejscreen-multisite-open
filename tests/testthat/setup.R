############################### #
cat("Starting setup.R for testing \n")

# # This script gets run before any test, so fixtures created here will be available to all the tests.
# The file already does library(EJAM) and that should do .onAttach() and dataload_from_pins() and indexblocks()

# When tests try to test the shiny app, the app should handle doing source(system.file("global.R", package = "EJAM")).

############################### #
# Keep track and alert us if any functions in tests have
#  changed global options, a side effect we probably want functions to avoid

set_state_inspector(function() {
  list(options = options())
})

################################## #
# GET DATA AND BUILD INDEX JUST IN CASE
# to run tests interactively, you also need to do
# require(testthat)
# require(data.table)
# require(magrittr)
#   require(EJAM)

dataload_from_pins("all") # needs frs, etc.
if (!exists("frs")) {stop('needs frs etc.')}
indexblocks()

## needs these? from global?
# default_hide_advanced_settings
# html_header_fmt

############################### #
# Create ejamoutnow here in setup.R, since some tests are using it.

if (exists("ejamit") & exists("blockgroupstats") & exists("testpoints_10")) {
  if (!exists("ejamoutnow")) {
  suppressMessages(  suppressWarnings({  ejamoutnow <- try(
    ejamit(testpoints_10, radius = 1,
           quiet = TRUE, silentinteractive = TRUE,
           include_ejindexes = TRUE)) # include_ejindexes = FALSE was the default but we want to test with them included
  }))
  }
  # DEFAULTS:
  #        sitepoints, radius = 3, maxradius = 31.07, avoidorphans = FALSE,
  #        quadtree = NULL, quiet = TRUE, parallel = FALSE, fips = NULL,
  #        shapefile_folder = NULL, in_shiny = FALSE, need_blockwt = TRUE,
  #        countcols = NULL, popmeancols = NULL, calculatedcols = NULL,
  #        testing = FALSE,
  # include_ejindexes = FALSE,
  # updateProgress = NULL,
  #        need_proximityscore = FALSE, calculate_ratios = TRUE, silentinteractive = FALSE,
  #        called_by_ejamit = TRUE, subgroups_type = "nh", extra_demog = TRUE,
  #        infer_sitepoints = FALSE, threshold1 = 90)

} else {
  warning("missing ejamit() or blockgroupstats, so using pre-calculated results in tests")
  if (exists("testoutput_ejamit_10pts_1miles")) {
    ejamoutnow <- testoutput_ejamit_10pts_1miles
  } else {
    stop("cannot run tests - see file setup.R")
  }
}

############################### #
## Create some test cases we can use for inputs error checking:

bad_numbers <- list(
  num0len          = numeric(0L),  # these might be OK
  matrix_1x1       = matrix(1),    #
  array1           = array(1),     #
  NA1              = NA,
  NULL1            = NULL,
  TRUE1            = TRUE, # these  might be acceptable if you need a single number, for some functions, since can do math/ they could be coerced
  text1            = "1",
  character1       = "A",
  list1            = list(1),
  listempty        = list(),
  df1              = data.frame(1),
  vector2          = 1:2,
  array2           = array(1:2),
  matrix_1row_4col = matrix(1:4, nrow = 1),
  matrix_4row_1col = matrix(1:4, nrow = 4),
  matrix_2x2       = matrix(1:4, nrow = 2)
)

### to look at this list of objects:

#nix <- sapply(1:length(bad_numbers), function(z) {cat( "\n\n\n------------------------\n\n  ", names(bad_numbers)[z], "\n\n\n" ); print( bad_numbers[z][[1]] )}); rm(nix)

## to look at which ones are length >1, numeric, atomic, or ok to use in math:

# x <- data.frame(
#   length0or1 = sapply(bad_numbers, function(z) try(length(z) < 2)),
#   isnumeric  = sapply(bad_numbers, function(z) try(is.numeric(z))),
#   isatomic   = sapply(bad_numbers, function(z) try(is.atomic( z))),
#   canadd     = sapply(bad_numbers, function(z) try(is.numeric(z + 9)))
#   )
# x
# rm(x)
############################### #

# which test to run first? alpha is default
# a line in DESCRIPTION can control order the package tests happen
 # see https://testthat.r-lib.org/articles/parallel.html
#
# # devtools::load_all()
# sdir = mysource()
# source(file = file.path(sdir, "tests/testthat/setup.R"))
# for (fname in list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-")) {
#   print(fname)
# test_file(fname)
# }

# test-ui_and_server.R          [ FAIL 0 | WARN 1 | SKIP 0 | PASS 11 ]
# test-ejamit.R                 [ FAIL 2 | WARN 2 | SKIP 0 | PASS 7 ]
# test-doaggregate.R            [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ]
# test-getblocksnearby.R        [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]
# test-getblocksnearbyviaQuadTree.R #    -------------- NEEDS MORE TESTS ***
# test-getblocksnearby_from_fips.R
# test-getblocks_summarize_blocks_per_site.R
# test-pctile_from_raw_lookup.R
# test-state_from_fips.R
# test-state_from_latlon.R
# test-shapefile_xyz.R                     [ FAIL 2 | WARN 0 | SKIP 0 | PASS 14 ]
# test-naics_from_name.R #  -------------  [ FAIL 11 | WARN 2 | SKIP 0 | PASS 11 ]
# test-naics_from_any.R  #  -------------  [ FAIL 9 | WARN 16 | SKIP 0 | PASS 22 ]
# test-naics_categories.R # not useful tests
# test-naics_subcodes_from_code.R
# test-naics2children.R
# test-naics_validation.R         [ FAIL 3 | WARN 0 | SKIP 0 | PASS 6 ]
# test-naics_from_code.R
# test-naics_findwebscrape.R
# test-frs_is_valid.R           # [ FAIL 0 | WARN 7 | SKIP 0 | PASS 8 ]
# test-frs_from_regid.R
# test-frs_from_programid.R
# test-frs_from_naics.R
# test-frs_from_sic.R
# test-regid_from_naics.R
# test-latlon_infer.R
# test-latlon_from_sic.R
# test-latlon_is.valid.R
# test-latlon_as.numeric.R
# test-latlon_df_clean.R
# test-latlon_from_anything.R
# test-fips_lead_zero.R
# test-fips_bg_from_anyfips.R
# test-varinfo.R
# test-radius_inferred.R    # this is SLOW THOUGH
# test-mod_save_report.R
# test-mod_view_results.R
# test-mod_specify_sites.R
# test-golem_utils_server #  not used

############################### #
# # Run after all tests
# # Setup code is typically best used to create external resources that are needed by many tests. It’s best kept to a minimum because you will have to manually run it before interactively debugging tests.
# # But, is this right?  it is from the help example but what is cleanup() ?? ***
# # Needs to be fixed:
#
# withr::defer(cleanup(), teardown_env())




#############################################################################  #
## Notes: to profile parts of the shiny app for performance, etc.
## see shinytest2 package, and see profiler:
# shiny::callModule(profvis_server, "profiler")
## and also see  /EJAM/tests/testthat/test-ui_and_server.R
## and see  https://shiny.posit.co/r/articles/improve/debugging/
## etc.
