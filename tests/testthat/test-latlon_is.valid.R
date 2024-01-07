## unit tests for EJAM::latlon_is.valid
## Author: Sara Sokolinski

# just testing ranges provided in 

# false for lat < 17.5
test_that('warning, invalid lat < 17.5',{
  expect_warning(val <- latlon_is.valid(lat = c(15, 17.9, 30), lon = c(-70, -175, 175)))
  expect_equal(val, c(FALSE, TRUE, TRUE))
  })

# false for lat > 71.5
test_that('warning, invalid lat < 17.5',{
  expect_warning(val <- latlon_is.valid(lat = c(75, 17.9, 30), lon = c(-70, -175, 175)))
  expect_equal(val, c(FALSE, TRUE, TRUE))
})


# false for lon < -180
test_that('warning, invalid lat < 17.5',{
  expect_warning(val <- latlon_is.valid(lat = c(50, 17.9, 30), lon = c(-200, -175, 175)))
  expect_equal(val, c(FALSE, TRUE, TRUE))
})

# false for lon > -65 & lon < 172
test_that('warning, invalid lat < 17.5',{
  expect_warning(val <- latlon_is.valid(lat = c(50, 17.9, 30), lon = c(-50, -175, 150)))
  expect_equal(val, c(FALSE, TRUE, FALSE))
})

# false for lon > 180
test_that('warning, invalid lat < 17.5',{
  expect_warning(val <- latlon_is.valid(lat = c(50, 17.9, 30), lon = c(-70, -175, 200)))
  expect_equal(val, c(TRUE, TRUE, FALSE))
})

# false for either NA
test_that('warning, invalid lat < 17.5',{
  expect_warning(val <- latlon_is.valid(lat = c(50, NA, 30), lon = c(-70, -175, NA)))
  expect_equal(val, c(TRUE, FALSE, FALSE))
})