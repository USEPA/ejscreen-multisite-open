
cat("Starting setup.R for testing")


# # Run before any test
# these will be available for all the tests
# but what if they cannot be created? various test will not be possible but that will be clear from this error message


# Should I do load the package by reading in all data and .R files ?? 
#   That would be the only way to attach the app_server() and app_ui() functions, to test those !!
# 
# library(EJAM)
# or 
# devtools::load_all() 

# dataload_from_aws()
# indexblocks()

# Do I instead or also have to do this here?  

source(system.file("global.R", package = "EJAM"))


# No great need to create this ejamoutnow in setup.R, since it is fast enough to do repeatedly in tests, but some tests are using it so deleting this would mess up tests:
if (exists("ejamit") & exists("blockgroupstats")) {
ejamoutnow <- try(ejamit(testpoints_10, radius = 1), silent = FALSE)
} else {
  warning("missing ejamit() or blockgroupstats, so using pre-calculated results in tests")
  ejamoutnow <- EJAM::testoutput_ejamit_10pts_1miles
}


## Some test cases we could use for inputs error checking:

bad_number <- list(
  num0len = numeric(0L), matrix_1x1 = matrix(1), array1 = array(1),  # these might be OK 
  NA1 = NA, NULL1 = NULL, TRUE1 = TRUE, # these  might be acceptable if you need a single number, for some functions, since can do math/ they could be coerced  
  text1 = "1", character1 = "A", 
  list1 = list(1), listempty = list(), 
  df1 = data.frame(1),
  vector2 = 1:2, array2 = array(1:2), matrix_1row_4col = matrix(1:4, nrow = 1), matrix_4row_1col = matrix(1:4, nrow = 4), matrix_2x2 = matrix(1:4, nrow = 2)
  )
## to look at this list of objects:
#nix <- sapply(1:length(bad_number), function(z) {cat( "\n\n\n------------------------\n\n  ", names(bad_number)[z], "\n\n\n" ); print( bad_number[z][[1]] )}); rm(nix)
## which ones are length >1, numeric, atomic, or ok to use in math
# x <- data.frame(
#   length0or1 = sapply(bad_number, function(z) try(length(z) < 2)), 
#   isnumeric  = sapply(bad_number, function(z) try(is.numeric(z))),   
#   isatomic   = sapply(bad_number, function(z) try(is.atomic( z))), 
#   canadd     = sapply(bad_number, function(z) try(is.numeric(z + 9)))
#   )
# x


# # Run after all tests
# # Setup code is typically best used to create external resources that are needed by many tests. It’s best kept to a minimum because you will have to manually run it before interactively debugging tests.
# # But, is this right?  it is from the help example but what is cleanup() ?? ***

# withr::defer(cleanup(), teardown_env())
 
 


#############################################################################  #
## to profile parts of the shiny app for performance
# shiny::callModule(profvis_server, "profiler")
#
# also see  /EJAM/tests/testthat/test-ui_and_server.R
# and see  https://shiny.posit.co/r/articles/improve/debugging/
