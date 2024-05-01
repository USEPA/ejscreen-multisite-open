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
suppressPackageStartupMessages({
  library(EJAM)
})
suppressMessages({suppressWarnings({
  dataload_from_pins("all", silent = TRUE) # needs frs, etc.
})})
if (!exists("frs")) {stop('needs frs etc.')}
suppressMessages({suppressWarnings({
  indexblocks()
})})


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
  # NOTE THE DEFAULT VALUES OF ejamit() !

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

# See

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
