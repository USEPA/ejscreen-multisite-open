
cat("Starting setup.R for testing")

# # Run before any test
# these will be available for all the tests
# but what if they cannot be created? various test will not be possible but that will be clear from this error message


# Should I do load the package by reading in all data and .R files ?? 
#   That would be the only way to attach the app_server() and app_ui() functions, to test those !!

# Do I instead or also have to do this here? ? how to clean up afterwards?
# stop('check setup.R for testing - not sure how to do this right')
source(system.file("global.R", package = "EJAM"))

# Or should it assume the package was attached via  library(EJAM) ??

# devtools::load_all() 

# - takes roughly 5-10 seconds
# note this relies on loading from disk or from AWS several large block datasets,
# and building the block index if it was not already built
if (exists("ejamit") & exists("blockgroupstats")) {
ejamoutnow <- try(ejamit(testpoints_10, radius = 1), silent = FALSE)
} else {
  warning("missing ejamit() or blockgroupstats, so using pre-calculated results in tests")
  ejamoutnow <- EJAM::testoutput_ejamit_10pts_1miles
}

# # Run after all tests
# withr::defer(rm(ejamoutnow), teardown_env())  # is this right??
# Setup code is typically best used to create external resources that are needed by many tests. It’s best kept to a minimum because you will have to manually run it before interactively debugging tests.
##############


#############################################################################  #
## to profile parts of the shiny app for performance
# callModule(profvis_server, "profiler")
#
# also see  /EJAM/tests/testthat/test-ui_and_server.R
# and see  https://shiny.posit.co/r/articles/improve/debugging/
