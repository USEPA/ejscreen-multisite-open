## unit tests for EJAM::fips_bg_from_anyfips
## Author: Sara Sokolinski

#' convert any FIPS codes to the FIPS of all the blockgroups that are
#'   among or within or containing those FIPS
#' @details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#' 
#' Takes a vector of one or more FIPS that could be State (2-digit), County (5-digit),
#'   Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips). 
#'   
#'   Returns unique vector of FIPS of all US blockgroups (including DC and Puerto Rico)
#'   that contain any specified blocks, are equal to any specified blockgroup fips, 
#'   or are contained within any provided tract/county/state FIPS. 
#'   
#' @param fips vector of US FIPS codes, as character or numeric,
#'   with or without their leading zeroes, each with as many characters
#' @seealso [fips_lead_zero()]
#' @return vector of blockgroup FIPS (or NA values) that may be much longer than the 
#'   vector of fips passed to this function.
#' @export
#'
#' @examples 
#'   # all blockgroups in one state
#'   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#'   length(fips_bg_from_anyfips("72"))
#'   # all blockgroups in this one county
#'   fips_bg_from_anyfips(30001)
#'   # all blockgroups that contain any of these 6 blocks (just one bg)
#'   fips_bg_from_anyfips( blockid2fips$blockfips[1:6])
#'   # 2 counties
#'   fips_bg_from_anyfips(c(36009,36011))

# test with state code
test_that('by state',{
  expect_no_warning(val <- fips_bg_from_anyfips(36))
  expect_no_warning(val <- fips_bg_from_anyfips("36"))
  expect_equal(length(val), 16070)
})


test_that('by county',{
  expect_no_warning(val <- fips_bg_from_anyfips(36071))
  expect_no_warning(val <- fips_bg_from_anyfips("36071"))
  expect_equal(length(val), 292)
  
  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "36071"))]
  expect_equal(y, val)
  
})

test_that('by  tract',{
  expect_no_warning(val <- fips_bg_from_anyfips(36071000100))
  expect_no_warning(val <- fips_bg_from_anyfips("36071000100"))
  expect_equal(length(val), 4)
  
  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "36071000100"))]
  expect_equal(y, val)
  
})

test_that('by  block group',{
  expect_no_warning(val <- fips_bg_from_anyfips(360710001001))
  expect_no_warning(val <- fips_bg_from_anyfips("360710001001"))
  expect_equal(length(val), 1)
  
  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "360710001001"))]
  expect_equal(y, val)
  
})

test_that('by  block ',{
  expect_no_warning(val <- fips_bg_from_anyfips(c(360710108011012, 360710108011006, 360710108011023)))
  expect_no_warning(val <- fips_bg_from_anyfips(c("360710108011012", "360710108011006", "360710108011023")))
  expect_equal(length(val), 1)
  
  # subsetting not possible since only goes down to block group level
  
})

test_that('test leading zero addition',{
  expect_no_warning(val <- fips_bg_from_anyfips("1055")) # county
  expect_no_warning(val <- fips_bg_from_anyfips(1055))
  expect_equal(length(val), 90)
  expect_equal(substr(val[1], 1,2) , "01")

  expect_no_warning(val <- fips_bg_from_anyfips("1")) # state
  expect_no_warning(val <- fips_bg_from_anyfips(1))
  expect_equal(length(val), 3925)
  expect_equal(substr(val[1], 1,2) , "01")
  
  expect_no_warning(val <- fips_bg_from_anyfips("1055011002")) # tract
  expect_no_warning(val <- fips_bg_from_anyfips(1055011002))
  expect_equal(length(val), 3)
  expect_equal(substr(val[1], 1,2) , "01")
  
 # removed block group level since it assumes it is a census tract code 
  # moved to it's own test
  
  # cant handle leading zero at bg level since it assumes it's a census tract code
  expect_no_warning(val <- fips_bg_from_anyfips(10690401001010)) # bg
  expect_no_warning(val <- fips_bg_from_anyfips("10690401001010"))
  expect_equal(length(val), 1) # looking for "010005051900" and got 3 values like "010005051900"
  expect_equal(substr(val[1], 1,2) , "01")
  
})

test_that('leading zero addition fails for census block group level', {
  
  # cant handle leading zero at bg level since it assumes it's a census tract code
  expect_no_warning(val <- fips_bg_from_anyfips("10005051900")) # bg
  expect_no_warning(val <- fips_bg_from_anyfips("10005051900"))
  expect_equal(length(val), 1) # looking for "010005051900" and got 3 values like "010005051900"
  expect_equal(substr(val[1], 1,2) , "01")
})

# no warnings for invalid strings, no string cleaning (dashes/dots not removed)
test_that('invalid text strings', {
  expect_warning(val <- fips_bg_from_anyfips("blue"))
  expect_warning(val <- fips_bg_from_anyfips("36-071"))
  expect_warning(val <- fips_bg_from_anyfips("36-07"))
  expect_warning(val <- fips_bg_from_anyfips("$1001"))
  
})
