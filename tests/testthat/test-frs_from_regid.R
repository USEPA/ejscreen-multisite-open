## unit tests for EJAM::frs_from_regid
## Author: Sara Sokolinski

# function is in the file frs_from_xyz.R
# not much to test here

# does it work with a proper reg id
test_that('lookup works correctly',{
  expect_no_warning(val <- frs_from_regid("110000307695"))
})


# does it give an error when id doesnt exist?
# no it doesn't, just returns an empty data frame
test_that('lookup works correctly',{
  expect_warning(val <- frs_from_regid("fakeid"))
})
