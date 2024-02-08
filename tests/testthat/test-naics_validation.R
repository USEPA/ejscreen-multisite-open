## unit tests for EJAM::NAICS_validation
## Author: Sara Sokolinski

# documentation seems outdated defining the input NAICS but not the distinction between naics_enter and NAICS_select

# test with real NAICS
test_that('real NAICS works',{
  expect_no_warning({val <- naics_validation(naics_enter = "211", naics_select = "1")})
  expect_true(val)
})


#  list of NAICS gives error
test_that('multiple NAICS gives error',{
  expect_error({val <- naics_validation(naics_enter = c("211", "452"), naics_select = "1")})
})
# #################################  validation so far meant just very very limited check
# # test with fake NAICS
# test_that('fake NAICS fails',{
#   expect_no_warning({val <- naics_validation(naics_enter = "LOL", naics_select = "1")}) ## ??
#   expect_false(val)
# })

#  numeric selector works
test_that('numeric selector works',{
  expect_no_warning({val <- naics_validation(naics_enter = "211", naics_select = 1)})
  expect_true(val)
})

#  multiple selectors gives error
test_that('multiple selectors gives error',{
  expect_error({val <- naics_validation(naics_enter = "211", naics_select = c(1,2))})
})

#  No selector gives invalid?
test_that('no selectors gives error',{
  expect_error(naics_validation(naics_enter = "211"))
})

