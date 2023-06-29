## unit tests for EJAM::state_from_fips
## Author: Sara Sokolinski


# does it work?
test_that('lookup works correctly',{
  expect_no_warning(val <- state_from_fips("45"))
  expect_equal(val[1], "SC")
})

# it uses fipsbg_from_anyfips to check if it's valid.
# any warnings should be added there not to this function
test_that('warns for invalid fips',{
  expect_warning(val <- state_from_fips("452"))
  expect_true(is.na(val[1]))
  
  expect_warning(val <- state_from_fips("blue"))
  expect_true(is.na(val[1]))
})

# I added this optional new parameter since it might be useful to somebody
test_that('abbrev works',{
  expect_no_warning(val <- state_from_fips("45", abbrev = FALSE))
  expect_equal(val[1], "South Carolina")
})
