## unit tests for EJAM::fips_lead_zero
## Author: Sara Sokolinski

# Add leading zeroes to fips codes if missing, replace with NA if length invalid
# Note it does NOT VALIDATE FIPS -
#   It does NOT check if FIPS is valid other than checking its length seems OK,
#   i.e., it might be a state, county, tract, blockgroup, or block FIPS code.
# @  param fips vector of numeric or character US FIPS codes
#
# @  return vector of same length
# @  export
#
# @  examples fips_lead_zero(c(1,"01",1234,"1234","12345",123456))

#  example skips over the case of 3 digits which for some reason returns NA not "00123"
# why is 3 digits invalid?

# test with 1 digit
# i think it's meant to only add one zero, to create state-codes
# should it warn if the  value == 0 ? or >50 (check for territory codes)
test_that('1 digit', {
  expect_no_warning({val <- fips_lead_zero("1")})
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero(1)})
  expect_equal(val, "01")

  expect_no_warning({val <- fips_lead_zero("0")})
  expect_equal(val, "00")
  expect_no_warning({val <- fips_lead_zero(0)})
  expect_equal(val, "00")
})


# test with 2 digit
# it doesn't add any zeros since it infers this to be a state-code
test_that('2 digit', {
  expect_no_warning({val <- fips_lead_zero("01")}) # leading zero
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero(10)}) # numeric
  expect_equal(val, "10")
  expect_no_warning({val <- fips_lead_zero(01)}) #leading zero in numeric
  expect_equal(val, "01")

  expect_no_warning({val <- fips_lead_zero("00")}) # zero string
  expect_equal(val, "00")
})

# test with 3 digit
# it doesn't add any zeros since it infers this to be a state-code
# why is 3 digits invalid?
# could add 2 zeros to create county-code

test_that('3 digit', {
  expect_no_warning({val <- fips_lead_zero("001")}) # leading zero
  #expect_equal(val, "011")
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(100)}) # numeric
  #expect_equal(val, "100")
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("000")}) # zero string
  #expect_equal(val, "00")

})

# adds leading zero to create 5-digit county-code
test_that('4 digit', {
  expect_no_warning({val <- fips_lead_zero("0001")}) # leading zero
  expect_equal(val, "00001")
  #expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000)}) # numeric
  expect_equal(val, "01000")
  #expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(0001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("0000")}) # zero string
  expect_equal(val, "00000")
})


# adds no leading zero since it is inferred to be 5 digit county-code
test_that('5 digit', {
  expect_no_warning({val <- fips_lead_zero("00001")}) # leading zero
  expect_equal(val, "00001")
  #expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(10000)}) # numeric
  expect_equal(val, "10000")
  #expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(00001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("00000")}) # zero string
  expect_equal(val, "00000")
})

# 6 digit numeric has problems... unless change options scipen...
#   100,000 gets converted to 1e+05 which is 5 digits and returned
# raise threshold with options(scipen = 999) and the test passes
# should add options(scipen = 0) to return back to default

test_that('6 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("000001")}) # leading zero ---------------------------   returns NA but why?
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(100000)}) # numeric -------------------    1e+05  unless adjust options in which case it is NA and test passes
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(100001)}) # numeric ---------------------------   returns NA ...why?
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})

#16

test_that('7 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("0000001")}) # leading zero
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000000)}) # numeric
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000001)}) # numeric
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(0000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("0000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})

test_that('8 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("00000001")}) # leading zero
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(10000000)}) # numeric
  # expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(10000001)}) # numeric
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(00000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("00000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})

test_that('9 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("000000001")}) # leading zero
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(100000000)}) # numeric
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(100000001)}) # numeric
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("000000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})

# adds a leading zero to create 11 digit census-tract-code
test_that('10 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("0000000001")}) # leading zero
  expect_equal(val, "00000000001")
  expect_no_warning({val <- fips_lead_zero(1000000000)}) # numeric
  expect_equal(val, "01000000000")
  expect_no_warning({val <- fips_lead_zero(1000000001)}) # numeric
  expect_equal(val, "01000000001")

  expect_no_warning({val <- fips_lead_zero(0000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("0000000000")}) # zero string
  expect_equal(val, "00000000000")

  options(scipen = 0)
})

# adds no leading zero since it is inferred as a 11 digit census-tract-code
test_that('11 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("00000000001")}) # leading zero
  expect_equal(val, "00000000001")
  expect_no_warning({val <- fips_lead_zero(10000000000)}) # numeric
  expect_equal(val, "10000000000")
  expect_no_warning({val <- fips_lead_zero(10000000001)}) # numeric
  expect_equal(val, "10000000001")

  expect_no_warning({val <- fips_lead_zero(00000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("00000000000")}) # zero string
  expect_equal(val, "00000000000")

  options(scipen = 0)
})


# adds no leading zero since it is inferred as a 12 digit block-group-code
test_that('12 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("000000000001")}) # leading zero
  expect_equal(val, "000000000001")
  expect_no_warning({val <- fips_lead_zero(100000000000)}) # numeric
  expect_equal(val, "100000000000")
  expect_no_warning({val <- fips_lead_zero(100000000001)}) # numeric
  expect_equal(val, "100000000001")

  expect_no_warning({val <- fips_lead_zero(000000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("000000000000")}) # zero string
  expect_equal(val, "000000000000")

  options(scipen = 0)
})


test_that('13 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("0000000000001")}) # leading zero
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000000000000)}) # numeric
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000000000001)}) # numeric
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(0000000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("0000000000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})

# add leading zero to create 15-digit block-code
test_that('14 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("00000000000001")}) # leading zero
  expect_equal(val, "000000000000001")
  expect_no_warning({val <- fips_lead_zero(10000000000000)}) # numeric
  expect_equal(val, "010000000000000")
  expect_no_warning({val <- fips_lead_zero(10000000000001)}) # numeric
  expect_equal(val, "010000000000001")

  expect_no_warning({val <- fips_lead_zero(00000000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("00000000000000")}) # zero string
  expect_equal(val, "000000000000000")

  options(scipen = 0)
})

# add no leading zero since it is inferred to be a 15-digit block-code
test_that('15 digit', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("000000000000001")}) # leading zero
  expect_equal(val, "000000000000001")
  expect_no_warning({val <- fips_lead_zero(100000000000000)}) # numeric
  expect_equal(val, "100000000000000")
  expect_no_warning({val <- fips_lead_zero(100000000000001)}) # numeric
  expect_equal(val, "100000000000001")

  expect_no_warning({val <- fips_lead_zero(000000000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("000000000000000")}) # zero string
  expect_equal(val, "000000000000000")

  options(scipen = 0)
})

test_that('16 digit +', {

  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("0000000000000001")}) # leading zero
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000000000000000)}) # numeric
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000000000000001)}) # numeric
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(0000000000000001)}) #leading zero in numeric
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero("0000000000000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})

options(scipen = 0)

test_that('should warn for text that cant be coerced into numeric FIPS', {
  expect_warning({
    val <- fips_lead_zero("blue")
    ## revised that function so it now does warn in this case, but does return something like "0blue" still.
    }) 
})
