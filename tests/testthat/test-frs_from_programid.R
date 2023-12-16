## unit tests for frs_from_programid
## Author: Sara Sokolinski

# function is in the script frs_from_siteid.R
# however, it is really testing subfuntion latlon_from_programid which has it's own script

# not much to test here

# does it work with a propoer program id
test_that('lookup works correctly',{
  expect_no_warning(val <- frs_from_programid("XJW000012435"))
})


# does it give an error when id doesnt exist?
# no it doesn't, just returns an empty data frame
test_that('lookup works correctly',{
  expect_warning(val <- frs_from_programid("fakeid"))
})
