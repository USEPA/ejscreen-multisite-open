## unit tests for EJAM::naics_from_name
## Author: Sara Sokolinski

# library dplyr
# library EJAM

# test some different syntax
test_that('no warning for standard name lookup', {
  expect_no_warning(naics_from_name("gold ore"))
  expect_no_warning(naics_from_name(c("gold ore")))
  
  })


# there is no warning when a NAICS has subcategories that are not being output
# while it works as detailed, this lack of warning could confuse some users who don't know to enter children = TRUE
# perhaps default children = TRUE or provide some type of message 
# the output are joined properly
test_that('results of subcategories only output when children = TRUE',{
  expect_no_warning(naics_from_name("gold ore and silver ore")) # "silver and gold mining"
  expect_no_warning(x <- naics_from_name("gold ore and silver ore", children = TRUE)) # "silver and gold mining"
  expect_no_warning(y <- naics_from_name("gold ore")) # "gold mining"
  expect_no_warning(z <- naics_from_name("silver ore")) # "silver mining"
  
  #expect_equal(length(which(grepl(212221,x$n6))), nrow(naics_from_name("gold ore")[])) # 1 gold mining naics
  #expect_equal(length(which(grepl(212222,x$n6))), nrow(naics_from_name(212222))) # 1 silver mining naics
  expect_equal(nrow(naics_from_name("gold ore and silver ore")), sum(!grepl(212221,x$n6 ) & !grepl(212222, x$n6))) # 1 gold and silver mining
  expect_equal(nrow(naics_from_name("gold ore and silver ore", children = TRUE)), nrow(full_join(x,y) %>% full_join(z)) )# 1 gold and silver mining
  
})

# warn if expect name but passed numeric
# no errors but returns empty dataframe
test_that('warn for query string', {
  expect_warning({val <- naics_from_name(11)})
  expect_warning({val <- naics_from_name("11")})
  
})

test_that('list of queries returns joined results', {
  expect_no_warning({ x <- naics_from_name(c("gold ore",  "silver ore"))  })
  expect_no_warning({ y <- naics_from_name("gold ore")  })
  expect_no_warning({ z <- naics_from_name("silver ore")  })
  expect_equal(x %>% arrange(name), full_join(y,z) %>% arrange(name))
  
})


