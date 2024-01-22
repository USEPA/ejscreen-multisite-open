## unit tests for EJAM::naics2children
## Author: Sara Sokolinski

# library dplyr
# library EJAM

# there are more 2 digit naics now then when the script was written
# table(nchar(as.character(NAICS)))
# There were 17 and are now 24


# test some different syntax
test_that('no warning for standard code lookup', {
  expect_no_warning(naics2children(21112))
  expect_no_warning(naics2children("21112"))
  expect_no_warning(naics2children(c(21112)))
  expect_no_warning(val <- naics2children(c("21112")))
  
  })


# error if passed text string
# no errors but returns numeric zero (named)
test_that('error for query string', {
  expect_error(val <- naics2children("gold ore"))
  expect_error(val <- naics2children("$100,0"))
  
})



test_that('list of queries returns joined results', {
  expect_no_warning(x <- naics2children(c("211",  "452")))
  expect_no_warning(y <- naics2children("211"))
  expect_no_warning(z <- naics2children("452"))
  expect_equal(x, c(y,z))
  
})

test_that('order doesnt matter', {
  expect_no_warning(x <-naics2children(c("211",  "452")))
  expect_no_warning( y <- naics2children(c("452", "211")))
  expect_equal(x ,y)
  
})
