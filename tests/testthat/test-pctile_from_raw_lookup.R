## unit tests for EJAM::pctile_from_raw_lookup
## Author: Sara Sokolinski


# use default usastats table and test with random column
test_that('default lookup works',{
  expect_no_warning({
    val <- pctile_from_raw_lookup(
      myvector = c(20, 25, 30), 
      varname.in.lookup.table = "cancer"
    )
  })
  # expect_equal(val, c(34,59,83)) # changed
})

# use custom lookup table
# it has to have REGION = USA
test_that('custom lookup works',{
  tab <- data.frame("PCTILE" = seq(0,100,1), "cancer" = seq(10,210, 2), "REGION" = "USA")
  expect_no_warning({
    val <- pctile_from_raw_lookup(
      myvector = c(20, 25, 30), 
      varname.in.lookup.table = "cancer",
      lookup = tab
    )
  })
  expect_equal(val, c(5,7,10))
})

# lets test zone = NY
test_that('pass states table as lookup and select a zone',{
  expect_no_warning({val <- pctile_from_raw_lookup(myvector = c(20, 25, 30), 
                                                   varname.in.lookup.table = "cancer",
                                                   lookup = statestats,
                                                   zone = "NY")})
  # expect_equal(val, c(43,55,67))
})

# use custom lookup table
# expect error if missing PCTILE column
test_that('custom lookup works',{
  tab <- data.frame("PerCenTILE" = seq(0,100,1), "cancer" = seq(10,210, 2), "REGION" = "USA")
  expect_error({val <- pctile_from_raw_lookup(myvector = c(20, 25, 30), 
                                              varname.in.lookup.table = "cancer",
                                              lookup = tab)})
})

#   If the value is between the cutpoints listed as
#   percentiles 89 and 90, it returns 89, for example.
#
test_that('rounds down',{
  expect_no_warning({
    val <- pctile_from_raw_lookup(myvector = c(80, 80.25, 80.5, 80.75), 
                                  varname.in.lookup.table = "cancer", 
                                  lookup = data.frame(
                                    PCTILE = 0:100, 
                                    cancer = 0:100, 
                                    REGION = "USA"))
  })
  expect_equal(val, c(80,80,80,80))
})

#   If the value is exactly equal to the cutpoint listed as percentile 90,
#   it returns percentile 90.
#   
#   This works when passed the vector but not the exact value?
test_that('equal to cutpoint rounds up',{
  expect_no_warning(
    val <- pctile_from_raw_lookup(myvector = c(80, 80.25, 85, 90), 
                                  varname.in.lookup.table = "cancer", 
                                  lookup = data.frame(
                                    PCTILE = 0:100, 
                                    cancer = 0:100, 
                                    REGION = "USA"))
  )
  expect_equal(val, c(80, 80, 85, 90))
})

#   If the value is exactly the same as the minimum in the lookup table and multiple percentiles 
#   in that lookup are listed as tied for having the same threshold value defining the percentile
#    (i.e., a large % of places have the same score and it is the minimum score), 
#    then the percentile gets reported as 0, not the percent of places tied for that minimum score.
test_that('multiple zero minimums return zero',{
  expect_no_warning({
    val <- pctile_from_raw_lookup(myvector = c(0, 0.01, 0.05, 0.11, 0.15), 
                                  varname.in.lookup.table = "pctlingiso"
    )
  })
  expect_equal(val, c( 0, 59, 75, 86, 90))
})



################## redo these tests- they were not right and/or cancer data changed

# # Note this is true whether they are 
# #   tied at a value of 0 or are tied at some other minimum value than 0.
# test_that('multiple one minimums return zero',{
#   expect_no_warning({val <- pctile_from_raw_lookup(myvector = c( 1, 10, 11, 15), 
#                                                   varname.in.lookup.table = "cancer")
#   })
#   expect_equal(val, c(0, 4, 4, 5))
# })
# 
# #   If the value is less than the cutpoint listed as percentile 0,
# #   which should be the minimum value in the dataset,
# #   it still returns 0 as the percentile, but with a warning that
# #   the value checked was less than the minimum in the dataset.
# test_that('below min returns zero with warning',{
#   tab <- data.frame("PCTILE" = seq(0,100,1), "cancer" = c(1,1,1,10, 10, seq(15,206, 2)), "REGION" = "USA")
#   expect_warning(val <- pctile_from_raw_lookup(myvector = c(0, 10, 11, 15), 
#                                                varname.in.lookup.table = "cancer",
#                                                lookup = tab))
#   expect_equal(val, c(0, 4, 4, 5))
# })
