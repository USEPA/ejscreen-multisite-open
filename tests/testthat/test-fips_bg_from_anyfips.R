## unit tests for EJAM::fips_bg_from_anyfips
## Author: Sara Sokolinski

############################################################################# #
# convert any FIPS codes to the FIPS of all the blockgroups that are
#   among or within or containing those FIPS
# @   details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#
# Takes a vector of one or more FIPS that could be State (2-digit), County (5-digit),
#   Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips).
#
#   Returns unique vector of FIPS of all US blockgroups (including DC and Puerto Rico)
#   that contain any specified blocks, are equal to any specified blockgroup fips,
#   or are contained within any provided tract/county/state FIPS.
#
# @   param fips vector of US FIPS codes, as character or numeric,
#   with or without their leading zeroes, each with as many characters
# @   seealso [fips_lead_zero()]
# @   return vector of blockgroup FIPS (or NA values) that may be much longer than the
#   vector of fips passed to this function.
# @   examples
#   # all blockgroups in one state
#   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#   length(fips_bg_from_anyfips("72"))
#   # all blockgroups in this one county
#   fips_bg_from_anyfips(30001)
#   # all blockgroups that contain any of these 6 blocks (just one bg)
#   fips_bg_from_anyfips( blockid2fips$blockfips[1:6])
#   # 2 counties
#   fips_bg_from_anyfips(c(36009,36011))
############################################################################# #

# test with state code
test_that('by state', {
  expect_no_warning({val <- fips_bg_from_anyfips(36)})
  expect_no_warning({val <- fips_bg_from_anyfips("36")})
  expect_equal(length(val), 16070)
})
################## #

test_that('by county', {
  expect_no_warning({val <- fips_bg_from_anyfips(36071)})
  expect_no_warning({val <- fips_bg_from_anyfips("36071")})
  expect_equal(length(val), 292)

  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "36071"))]
  expect_equal(y, val)

})
################## #

test_that('by  tract', {
  expect_no_warning({val <- fips_bg_from_anyfips(36071000100)})
  expect_no_warning({val <- fips_bg_from_anyfips("36071000100")})
  expect_equal(length(val), 4)

  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "36071000100"))]
  expect_equal(y, val)

})
################## #

test_that('by  block group', {
  expect_no_warning({val <- fips_bg_from_anyfips(360710001001)})
  expect_no_warning({val <- fips_bg_from_anyfips("360710001001")})
  expect_equal(length(val), 1)

  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "360710001001"))]
  expect_equal(y, val)

})
################## #



# returns only UNIQUE blockgroup fips once each, even if 2 inputs contain or are inside same bg (turn into same bgid) - do we want that to be the behavior?

test_that("returns vector of only the UNIQUE blockgroups inside and/or containing specified fips", {

  expect_true({
    length(fips_bg_from_anyfips(c("36071010801"))) == 3 # contains 3 unique blockgroups
  })
  expect_true({
    length(fips_bg_from_anyfips(rep("36071010801", 5))) == 3 # will not return more matches than just unique
  })
  expect_true({
    length(fips_bg_from_anyfips(rep("360710108011", 5))) == 1 # will not return more matches than just unique
  })
  expect_true({
    length(fips_bg_from_anyfips(c(360710108011012, 360710108011006, 360710108011023))) == 1 # one unique bg returned even if it contains multiple blocks provided as query terms
  })
})


test_that('by  block - uniques only - is that behavior we want?', {
  expect_true( {
    length(fips_bg_from_anyfips(rep("360710108011", 5))) == 1
  })
  expect_no_warning({val <- fips_bg_from_anyfips(c(360710108011012, 360710108011006, 360710108011023))})
  expect_no_warning({val <- fips_bg_from_anyfips(c("360710108011012", "360710108011006", "360710108011023"))})
  expect_equal(length(val), 1)


})
################## #

test_that('test leading zero addition', {

  expect_no_warning({val <- fips_bg_from_anyfips("1055")}) # county
  expect_no_warning({val <- fips_bg_from_anyfips(1055)})
  expect_equal(length(val), 90)
  expect_equal(substr(val[1], 1,2) , "01")

  expect_no_warning({val <- fips_bg_from_anyfips("1")}) # state
  expect_no_warning({val <- fips_bg_from_anyfips(1)})
  expect_equal(length(val), 3925)
  expect_equal(substr(val[1], 1,2) , "01")

  expect_no_warning({val <- fips_bg_from_anyfips("1055011002")}) # tract
  expect_no_warning({val <- fips_bg_from_anyfips(1055011002)})
  expect_equal(length(val), 3)
  expect_equal(substr(val[1], 1,2) , "01")



  #
  expect_no_warning({val <- fips_bg_from_anyfips(10690401001010)}) # not a bg
  expect_no_warning({val <- fips_bg_from_anyfips("10690401001010")})
  expect_equal(length(val), 1) #
  expect_equal(substr(val[1], 1,2) , "01")

})
################## #

test_that('returns bgs inside given tract(s)', {

  expect_true(fipstype("10005051900") == "tract") # tract as input,
  expect_true(all(fips_bg_from_anyfips("10005051900") %in% blockgroupstats$bgfips)) # returns actual bg fips
  expect_no_condition({val <- fips_bg_from_anyfips("10005051900")}) # tract that contains 3 bgs
  expect_equal(length(val), 3)
})
################## #

# no warnings for invalid strings, no string cleaning (dashes/dots not removed)
test_that('what if inputs are invalid text strings', {

  expect_no_error({val <- fips_bg_from_anyfips("blue")})
  expect_no_error({val <- fips_bg_from_anyfips("36-071")})
  expect_no_error({val <- fips_bg_from_anyfips("36-07")})
  expect_no_error({val <- fips_bg_from_anyfips("$1001")})
  # > fipstype("blue")
  # [1] "county"
  # > fipstype("sdfsdfsdfasdf0")
  # [1] "block"
  # fips_bg_from_anyfips("36-07")
  # character(0)
})
################## #

