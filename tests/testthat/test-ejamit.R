# no crash for basic example 
test_that('case simple example ',{
  expect_no_error(val <- ejamit( sitepoints =   testpoints_10 ))
  # expect_true('data.table' %in% class(val))
  # expect_identical(NROW(val), )
})
