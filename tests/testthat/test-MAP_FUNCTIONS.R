# map_facilities()
# map_facilities_proxy()
# mapfastej_counties()
# map_blockgroups_over_blocks()
# map_shapes_plot()
# map_shapes_leaflet_proxy()
# map_shapes_mapview  # If mapview pkg available
# shapes_counties_from_countyfips() #Get Counties boundaries via API, to map them
# shapes_blockgroups_from_bgfips()
# mapfast_gg()

test_that("map_facilities() works", {
  expect_no_error({
    suppressMessages({
      map_facilities(mypoints = testpoints_10[1,])
      map_facilities(mypoints = testpoints_10[1:2,], rad = 1)
      map_facilities(mypoints = testpoints_10[1:3,], rad = 1, highlight = TRUE, clustered = c(TRUE,TRUE,FALSE))
    })
  })
  expect_error({
    map_facilities(mypoints = testpoints_10[1:2,], rad = 1, 
                   highlight = TRUE) # requires clustered param if highlight=T
  })
})
test_that("map_facilities() works with non-data.table data.frame", {
  expect_no_error({
    suppressMessages({
      map_facilities(mypoints = data.frame(testpoints_10[1,]))
      map_facilities(mypoints = data.frame(testpoints_10[1:2,]), rad = 1)
    })
  })
})


test_that("map_facilities_proxy() works", {
  expect_no_error({
    suppressMessages({
      map_facilities_proxy(
        mapfast(testpoints_10[1,]), # only 1 point
        rad = 4, 
        popup_vec = popup_from_any(data.frame(
          newinfo = "text", 
          other = 1
        ))
      ) 
    })
  })
  expect_no_error({
    suppressMessages({
      map_facilities_proxy(
        mapfast(testpoints_10[1:2,]), 
        rad = 4, 
        popup_vec = popup_from_any(data.frame(
          newinfo = c("xyz", "zzz"), 
          other = 1:2
        ))
      ) 
    })
  })
})

############################################## # 

test_that("mapfastej_counties() works", {
  
  # getblocksnearby_from_fips() has warnings here 
  
  suppressMessages({
    
  expect_no_error({
    mydat = ejamit(fips = fips_counties_from_statename("Rhode Island")[1], radius = 0)$results_bysite
    x = mapfastej_counties(mydat)
  })
  expect_true("leaflet" %in% class(x))
})
})

test_that("map_blockgroups_over_blocks() works", {
  expect_no_error({
    y <- plotblocksnearby(testpoints_10[5,],
                          radius = 0.5,
                          returnmap = TRUE)
    z = map_blockgroups_over_blocks(y)
  })
  expect_true(
    "leaflet" %in% class(z)
  )
})

test_that("shapes_counties_from_countyfips() works", {
  # Get Counties boundaries via API, to map them
  expect_no_error({
    myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
  })
})

myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])

test_that("map_shapes_plot() works", {
  
  # myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
  
  expect_no_error({
    map_shapes_plot(myshapes)   
  })
})

# test_that("map_shapes_leaflet_proxy() works", {
# 
#   myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
# 
#   expect_no_error({
#     map_shapes_leaflet_proxy(     )
#   })
# })

test_that("map_shapes_mapview() if mapview pkg available works", {
  
  # myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
  
  expect_no_error({
    # requires mapview pkg be attached by setup.R in tests folders
    map_shapes_mapview(myshapes)
  })
})

test_that("shapes_blockgroups_from_bgfips() works", {
  expect_no_error({
    x = shapes_blockgroups_from_bgfips()
  })
  expect_true("sf" %in% class(x))
})

test_that("mapfast_gg() works", {
  expect_no_error({
    mapfast_gg(testpoints_10)
  })
})
