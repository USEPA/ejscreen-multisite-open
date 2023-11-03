############################################################################## # 


## example unit tests for EJAM::doaggregate
# Author: MC
## these examples are designed to produce warnings - for now, they only check for any
## warning but can be made more specific to make sure it the correct warning is reported

## need to add many more basic tests   


#  library(EJAM)
# dataload_from_aws()
# indexblocks()
if (!exists('blockwts')) {
  stop('tests cannot run without blockwts dataset being loaded')
  #  dataload_from_aws()
}
############################################################################## # 

test_that('error if in inputs are null, empty, NA, or blank',{
  expect_error(doaggregate(NULL, silentinteractive = TRUE))
  expect_error(doaggregate(NA, silentinteractive = TRUE))
  expect_error(doaggregate())
  expect_error(doaggregate('', silentinteractive = TRUE))
})

# x happens if y inputs and parameters

# no crash when aggregate basic example of sites2blocks
test_that('doaggregate() returns a correctly named list, with no error', {
  expect_no_error({
    val <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, 
                                      radius = max(testoutput_getblocksnearby_10pts_1miles$distance))
  })
  expect_true('list' %in% class(val))
    expect_identical(
    names(val),
    c("results_overall", "results_bysite", "results_bybg_people", 
    "longnames", "count_of_blocks_near_multiple_sites")
  )
})
 ################# # 

# RADIUS 

test_that('warning if ask for radius > 32, and just uses 32 instead', {
  # if (radius > 32) {radius <- 32; warning("Cannot use radius above 32 miles (almost 51 km) here - Returning results for 32 miles!")}
  suppressWarnings(
    expect_warning( 
    doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles , radius = 32.01, silentinteractive = TRUE)
   ))
})

testthat::test_that("same result if radius requested is 32 or 50, since >32 gets treated as if 32", {
  x <- suppressMessages(  suppressWarnings( doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, radius = 50, silentinteractive = TRUE) ))
  y <- suppressMessages(  suppressWarnings( doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, radius = 32, silentinteractive = TRUE) ))
  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("no warning if radius = 32 exactly IF original analysis was for AT LEAST 1/1.5x that radius", {
  x = getblocksnearby(testpoints_10[1,], radius = 1.01 * (32 / 1.5), quiet = TRUE) # 1.5x is where it starts to warn now in doag
  testthat::expect_no_warning(
    doaggregate(sites2blocks = x, radius = 32, silentinteractive = TRUE)
  )
})
test_that("warning if radius = 32 exactly and original analysis was LESS THAN 1/1.5x that radius", {
  x = getblocksnearby(testpoints_10[1,], radius = 0.99 * (32 / 1.5), quiet = TRUE)
  testthat::expect_warning(
    doaggregate(sites2blocks = x, radius = 32, silentinteractive = TRUE)   
  )
})

test_that('warning if ask for radius <= 0', {
  suppressWarnings(   expect_warning(  
    doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles , radius = 0)
  ) )
  #   expect_warning( 
  #   doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles , radius = -0.001)
  # )
 
})

##############

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

cause_no_warn_no_err = list(normalnumber = 1.3)
for (i in 1:length(cause_no_warn_no_err)) {
  cat('\n  Trying radius that is', names(cause_no_warn_no_err)[i], '- Testing to ensure it works... ')
  try({
    test_that(paste0("doaggregate radius like ", names(cause_no_warn_no_err)[i], " should not warn or err!"), {
      
      expect_no_condition(
        doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_no_warn_no_err[[i]])
      )
    })
  } )
}


# cause_something_else =   bad_number[("NA1", "character1" , "df1")] # ????


cause_warn = bad_number[c(   'TRUE1', 'text1','list1',  "NA1",  "NULL1"    )] # TRUE1, text1, list1, NA1, NULL1 - these warn
for (i in 1:length(cause_warn)) {
  cat('\n  Trying radius that is', names(cause_warn)[i], '- Testing to ensure it warns... ')
  try({
    test_that(paste0("doaggregate radius like ", names(cause_warn)[i], " should warn!"), {
    
      expect_warning(
      doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_warn[[i]])
    ) 
  })
 } )
}

# All the others - error 
cause_err = bad_number[c("num0len", "listempty", "vector2", "array2","matrix_1row_4col", "matrix_4row_1col", "matrix_2x2" )]
for (i in 1:length(cause_err)) {
  cat('\n  Trying radius that is', names(cause_err)[i], '- Testing to ensure it reports error... ')
  try({
    test_that(paste0("doaggregate radius like ", names(cause_err)[i], " should report error!"), {
      
      expect_error(
        doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_err[[i]])
      ) 
    })
  } )
}


# radius in doaggregate()


########## 
 
# may want to change this behavior ? ***

test_that('confusingly, warning (but not error) if radius = character string that can be coerced to a single number - does not actually coerce it but uses max seen!', {
expect_warning(
  doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = "1")
)
})
test_that("radius param to doagg that is string/text like '0.25' is not interpreted as the number 0.25 but use radius inferred from output of getblocks", {
  suppressWarnings(    expect_equal(
    doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = "0.25")$results_bysite$radius.miles[1],
   1) # inferred based on sites2blocks
)})

test_that("radius param to doag that is very small relative to radius seen from getblocks get reported and used to filter distances", {
  expect_equal(
    doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = 0.25)$results_bysite$radius.miles[1],
    0.25  
  )   
})
test_that("radius param to doag that is 1.5x as big as radius seen from getblocks gets reported anyway as radius instead of inferring!?!? - do we want that???", {
  
  expect_equal(
    doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = 1.5)$results_bysite$radius.miles[1],
    1.5  
  ) 
})
test_that("radius param to doagg that is MUCH larger than seen from getblocks is ignored and doag uses inferred radius instead", {
 suppressWarnings(  expect_equal(
    doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = 1.6)$results_bysite$radius.miles[1],
    1  
  )  )
})

################### # 

test_that('error if input has column not named distance', {
  wrongnames <- data.table::copy(testoutput_getblocksnearby_10pts_1miles)
  data.table::setnames(wrongnames, 'distance', 'radius')
  expect_warning({doaggregate(sites2blocks = wrongnames)})
})

test_that('warn but no error if input is data.frame but not data.table', {
  df <- data.table::setDF(  data.table::copy(testoutput_getblocksnearby_10pts_1miles) )
  suppressWarnings( 
  expect_no_error(doaggregate(df))
  )
  suppressWarnings( 
    expect_warning(doaggregate(df))
  )
})


# test radius =  negative, NA, character, etc. 
# try specifying columns like countcols, popmeancols, etc.
# try all the other parameters
