## unit tests for EJAM::regid_from_naics
## Author: Sara Sokolinski


# does it work?
test_that('lookup works correctly',{
  expect_no_warning(val <- regid_from_naics("452"))
  expect_equal(val$NAICS[1], 452)
  expect_no_warning(val <- regid_from_naics(452))
  expect_equal(val$NAICS[1], 452)
})

test_that('warns for invalid NAICS',{
  expect_warning(val <- regid_from_naics("4"))
  expect_equal(nrow(val), 0)
  expect_warning(val <- regid_from_naics("blue"))
  expect_equal(nrow(val), 0)
})

test_that('id_only works',{
  expect_no_warning(val <- regid_from_naics("452", id_only = TRUE))
  expect_true(is.vector(val))
})

test_that('works in list',{
  expect_no_warning(val <- regid_from_naics(c("452", "21")))
  expect_equal(c( 21, 452), sort(unique(val$NAICS)))
})

test_that('warns if not present in dataset',{
  expect_warning(val <- regid_from_naics(c( "9", "101")))
  expect_equal(nrow(val), 0)
})
