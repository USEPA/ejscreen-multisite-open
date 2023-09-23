## example unit tests for EJAM::doaggregate
# Author: MC
## these examples are designed to produce warnings - for now, they only check for any
## warning but can be made more specific to make sure it the correct warning is reported

## need to add tests   

#  library(EJAM)
# dataload_from_aws()
# indexblocks()
if (!exists('blockwts')) {
  stop('tests cannot run without blockwts dataset being loaded')
    #  dataload_from_aws()
  }

test_that('error if null empty NA or blank input',{
  expect_error(val <- doaggregate(NULL))
  expect_error(val <- doaggregate(NA))
  expect_error(val <- doaggregate())
  expect_error(val <- doaggregate(''))
})
 
# x happens if y inputs and parameters

# no crash when aggregate basic example of sites2blocks
test_that('case simple example, return data.table',{
  expect_no_error(val <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, radius = max(testoutput_getblocksnearby_10pts_1miles$distance)))
  expect_true('data.table' %in% class(val))
})
test_that('ask for distance larger than what was available in dataset', {
  expect_warning(val <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, radius = 2 * max(testoutput_getblocksnearby_10pts_1miles$distance)))
})
test_that('input has column not named distance', {
  wrongnames <- copy(testoutput_getblocksnearby_10pts_1miles)
  setnames(wrongnames, 'distance', 'radius')
  expect_error(val <- doaggregate(sites2blocks = wrongnames))
})
test_that('input is data.frame not data.table ??? ', {
  df <- setDF(  copy(testoutput_getblocksnearby_10pts_1miles) )
  expect_no_error(doaggregate(df))
})

# test radius =  negative, NA, character, etc. 
# try specifying columns like countcols, popmeancols, etc.
# try all the other parameters
