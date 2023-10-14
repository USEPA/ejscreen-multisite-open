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

test_that('error if in inputs are null, empty, NA, or blank',{
  expect_error(doaggregate(NULL))
  expect_error(doaggregate(NA))
  expect_error(doaggregate())
  expect_error(doaggregate(''))
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
 
test_that('warning if ask for radius >> distances found by getblocksnearby()', {
  expect_warning({doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, 
                                     radius = 2 * max(testoutput_getblocksnearby_10pts_1miles$distance))})
})
test_that('error if input has column not named distance', {
  wrongnames <- data.table::copy(testoutput_getblocksnearby_10pts_1miles)
  data.table::setnames(wrongnames, 'distance', 'radius')
  expect_warning({doaggregate(sites2blocks = wrongnames)})
})
test_that('no error if input is data.frame but not data.table', {
  df <- data.table::setDF(  data.table::copy(testoutput_getblocksnearby_10pts_1miles) )
  expect_no_error(doaggregate(df))
})


# test radius =  negative, NA, character, etc. 
# try specifying columns like countcols, popmeancols, etc.
# try all the other parameters
