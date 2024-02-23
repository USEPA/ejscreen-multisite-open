## unit tests for EJAM::NAICS_validation
## Author: Sara Sokolinski

# documentation seems outdated defining the input NAICS but not the distinction between naics_enter and NAICS_select

# test with real NAICS
test_that('real NAICS works',{
  expect_no_warning({val <- naics_validation(naics_enter = "211", naics_select = "1")})
  expect_true(val)
})


#  list of NAICS gives error ?? does it need to for shiny app?
test_that('is multiple NAICS supposed to give error? does it need to for shiny app??', {
  expect_error({
    val <- naics_validation(naics_enter = c("211", "452"), naics_select = "1")
    })

})



# #################################  validation so far meant just very very limited check


test_that('fake NAICS should report that valid is FALSE but ???', {
  expect_no_warning({
val <- naics_validation(naics_enter = "LOL", naics_select = "1")
}) ## ??
  expect_false(val)
})



#  numeric selector works
test_that('numeric selector works',{
  expect_no_warning({val <- naics_validation(naics_enter = "211", naics_select = 1)})
  expect_true(val)
})

#  No selector gives invalid?
test_that('no selectors gives error',{
  expect_error(naics_validation(naics_enter = "211"))
})




#  multiple selectors gives error ??
test_that('is multiple values for naics_select supposed to give error? does it need to for shiny app??',{
  expect_error({val <- naics_validation(naics_enter = "211", naics_select = c(1,2))})
})
