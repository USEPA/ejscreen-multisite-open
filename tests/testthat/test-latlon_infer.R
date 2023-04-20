## example unit tests for EJAM::latlon_infer
## these examples are designed to produce warnings - for now, they only check for any
## warning but can be made more specific to make sure it the correct warning is reported

## need to verify if these are the expected outputs of this function, or if it should
## only return two values at most that were matched + converted

# warns if no alias found. Does not warn of dupes in other terms, just preferred term. PM-pushtest
test_that('warns if no alias found',{
  expect_warning(val <- latlon_infer(c('trilat', 'belong', 'belong'))) 
  expect_equal(val, c('trilat','belong','belong'))
})

# only the best alias is converted/used
test_that('only the best alias #1',{
            expect_warning(val <- latlon_infer(c('a', 'LONG', 'Longitude', 'lat')))
            expect_equal(val, c('a', 'LONG', 'lon', 'lat'))
})

# only the best alias is converted/used
test_that('only the best alias #2', {
  expect_warning(val <- latlon_infer(c('a', 'LONGITUDE', 'Long', 'Lat')))
  expect_equal(val, c('a', 'lon', 'Long', 'lat'))
})

# case variants of preferred are left alone only if lowercase one is found
test_that('case variants left alone',{
  expect_warning(val <- latlon_infer(c('a', 'longing', 'Lat', 'lat', 'LAT')))
  expect_equal(val, c('a', 'longing','Lat', 'lat','LAT'))
})

# case variants of a single alias are converted to preferred word (if pref not found), creating dupes!  warn!
test_that('case variants converted',{
  expect_warning(val <- latlon_infer(c('LONG', 'long', 'lat')))
  expect_equal(val, c('lon','lon','lat'))
})

# dupes of an alias are renamed and still are dupes! warn!
test_that('dupes renamed and warn',{
  expect_warning(val <- latlon_infer(c('LONG', 'LONG')))
  expect_equal(val, c('lon','lon'))
})

# dupes left as dupes but warn!
test_that('dupes left as dupes',{
  expect_warning(val <- latlon_infer(c('lat', 'lat', 'Lon')))
  expect_equal(val, c('lat','lat','lon'))
}) 
