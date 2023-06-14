## unit tests for EJAM::latlon_df_clean
## Author: Sara Sokolinski

# run the same tests as for latlon_infer since it's used inside

# warns if no alias found.
test_that('warns if no alias found, returns the same data frame',{
  expect_warning(val <- latlon_df_clean(data.frame('trilat' = 1, 'belong' = 2, 'belong' = 3 ))) 
  expect_equal(val, data.frame('trilat' = 1,'belong' = 2,'belong' = 3))
})

# only the best alias is renamed
# only lat and lon get numeric cleaning with latlon_as.numeric
test_that('only the best alias #1',{
  expect_warning(val <- latlon_df_clean(data.frame("a" = "-100.75%", "LONG" = 200, "Longitude" = " 175 ", "lat" = "$35")))
  expect_equal(names(val), c('a', 'LONG', 'lon', 'lat'))
})

# warning if latitude is not valid (outside 17.5 - 71.5)
test_that('warn invalid latitude',{
  expect_warning(val <- latlon_df_clean(data.frame("lon" = " 175 ", "lat" = "$15")))
 
})
# warning if longitude is not valid ( outside -180 - -65 OR 172 - 180)
test_that('warn invalid longitude',{
  expect_warning(val <- latlon_df_clean(data.frame("lon" = " -200 ", "lat" = "$35")))
  
})


# only the best alias is converted/used
test_that('only the best alias #2', {
  expect_warning(val <- latlon_df_clean(data.frame("a" = "-100.75%", "LONGITUDE" = 200, "Long" = " 175 ", "lat" = "$35")))
  expect_equal(names(val), c('a', 'lon', 'Long', 'lat'))
})

# case variants of preferred are left alone only if lowercase one is found
# POTENTIAL ISSUE when either lat/lon is missing, only a warning is provided, but it prevents
# conversion of the present value (in this example 'lat') into a number
test_that('case variants left alone',{
  expect_warning(val <- latlon_df_clean(data.frame("a" = "-100.75%", "longing" = 200, "Lat" = " 175 ", "lat" = "$35", "LAT" = "66.4")))
  expect_equal(names(val), c('a', 'longing','Lat', 'lat','LAT'))
  expect_equal(val$lat, 35)
})

# might want to have an error if latlon_infer fails to find both
test_that('case variants left alone',{
  expect_no_warning(val <- latlon_df_clean(data.frame("lon" = "-100.75%", "longing" = 200, "Lat" = " 175 ", "lat" = "$35", "LAT" = "66.4")))
  expect_equal(names(val), c('lon', 'longing','Lat', 'lat','LAT'))
  expect_equal(val$lon, -100.75)
  expect_equal(val$lat, 35)
})

# case variants of a single alias are converted to preferred word (if pref not found), creating dupes!  warn!
test_that('case variants converted',{
  expect_warning(val <- latlon_df_clean(data.frame('LONG'=" 1 ", 'long'=" 2 ", 'lat'=" 3 ")))
  expect_equal(names(val), c('lon','lon','lat'))
})

# unlike for latlon_infer, duplicate names don't cause a warning, since the 
# data frame automatically renames one to LONG.1
# should try and add a warning if this is the case
test_that('dupes renamed and warn',{
  expect_warning(val <- latlon_df_clean(data.frame('LONG' = " 1 ,", 'LONG' = " 2, ", "lat" = ",  35")))
  expect_equal(names(val), c('lon','lon', "lat"))
})

# same issue as previous test
test_that('dupes left as dupes',{
  expect_warning(val <- latlon_df_clean(data.frame('lat' = " 1 ,", 'lat' = " 2, ", 'lon' = ", 3")))
  expect_equal(names(val), c('lat','lat','lon'))
}) 

# Returns the same data.frame but with relevant colnames changed to lat and lon,
# and invalid lat or lon values cleaned up if possible or else replaced with NA"
test_that('invalid lat/lon that cannot be cleaned up are replaced with NA',{
  expect_warning(val <- latlon_df_clean(data.frame('lat' = "  ,", 'lon' = ",-150 ")))
  expect_true(is.na(val$lat))
  expect_true(val$lon == -150)
  })
  
