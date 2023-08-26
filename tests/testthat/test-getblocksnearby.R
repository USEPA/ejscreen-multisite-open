# no crash for basic example 
test_that('case simple example, return data.table',{
  expect_no_error(val <-  getblocksnearby(sitepoints = EJAM::testpoints_100_dt) )
  expect_true('data.table' %in% class(val))
  # expect_identical(NROW(val), )
})


# test_that('case simple example, return data.table - ideally should at least warn when radius zero or >50 miles',{
#   expect_error(val <- EJAM::getblocksnearby(sitepoints = testpoints_50, radius = 0 ))
#  
# })

# try invalid lat lon values

# try data.frame not data.table as input

# try point where there is no block within 2 miles and specified radius of 1 mile 

# etc.
