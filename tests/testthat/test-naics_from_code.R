## unit tests for EJAM::naics_from_code
## Author: Sara Sokolinski

# library dplyr
# library EJAM

# test some different syntax
test_that('no warning for standard code lookup', {
  expect_no_warning(naics_from_code(21112))
  expect_no_warning(naics_from_code("21112"))
  expect_no_warning(naics_from_code(c(21112)))
  expect_no_warning(naics_from_code(c("21112")))
  
  })


# there is no warning when a NAICS has subcategories that are not being output
# while it works as detailed, this lack of warning could confuse some users who don't know to enter children = TRUE
# perhaps default children = TRUE or provide some type of message 
# the output are joined properly
test_that('results of subcategories only output when children = TRUE',{
  expect_no_warning(naics_from_code(21222)) # "silver and gold mining"
  expect_no_warning(x <- naics_from_code(21222, children = TRUE)) # "silver and gold mining"
  expect_no_warning(naics_from_code(212221)) # "gold mining"
  expect_no_warning(naics_from_code(212222)) # "silver mining"
  
  expect_equal(length(which(grepl(212221,x$n6))), nrow(naics_from_code(212221))) # 1 gold mining naics
  expect_equal(length(which(grepl(212222,x$n6))), nrow(naics_from_code(212222))) # 1 silver mining naics
  expect_equal(nrow(naics_from_code(21222)), sum(!grepl(212221,x$n6 ) & !grepl(212222, x$n6))) # 1 gold and silver mining
  
})

# error if passed text string
# no errors but returns empty dataframe
test_that('error for query string', {
  expect_error(val <- naics_from_code("gold ore"))
  expect_error(val <- naics_from_code("$100,0"))
  
})

test_that('list of queries returns joined results', {
  expect_no_warning(x <- naics_from_code(c("gold ore",  "silver ore")))
  expect_no_warning( y <- naics_from_code("gold ore"))
  expect_no_warning( z <- naics_from_code("silver ore"))
  expect_equal(x %>% arrange(code), full_join(y,z) %>% arrange(code))
  
})


