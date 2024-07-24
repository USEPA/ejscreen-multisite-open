################################ # ################################ # ejscreenit ####
cat('\ntesting ejscreenit()\n')

test_that("ejscreenit() works at all", {
  expect_no_error(  suppressMessages({
    junk <-  ejscreenit(testpoints_5[1:2, ], radius = 0.5, nosave = T, nosee = T, interactiveprompt = F, 
                        calculate_ratios = F)
  })
  )
})
test_that('ejscreenit() does not crash for 2 points x= lons, y = lats', {
  expect_no_error({
    out_ejscreenit_separate_lat_lon <- ejscreenit(
      x = test2lon, y = test2lat, radius = testradius,
      nosave = TRUE, nosee = TRUE, interactiveprompt = FALSE
    )
  } )
})

# SLOW FOR API to run several points
apiref_list <- testoutput_ejscreenit_5 # 5 points, 1 mile radius
apinow_list <- ejscreenit(testpoints_5, radius = 1, nosave = T, nosee = T, interactiveprompt = F, calculate_ratios = T)
apiref = apiref_list$table
apinow = apinow_list$table
apiref$timeSeconds <- NULL # these vary
apinow$timeSeconds <- NULL # these vary
apiref$`Seconds elapsed obtaining data` <- NULL
apinow$`Seconds elapsed obtaining data` <- NULL

test_that("ejscreenit() still returns list with names identical to what it used to return (saved as testoutput_ejscreenit_5)", {
  expect_identical(
    names(apiref_list),   #  "table" "map"   "plot" 
    names(apinow_list)   #  "table" "map"   "plot" 
  )
})
test_that("ejscreenit() still returns a list that includes a table with names identical to the one it used to return (saved as testoutput_ejscreenit_10pts_1miles$table)", {
  expect_identical(
    names(apiref),
    names(apinow)
  )
})
test_that("ejscreenit() still returns a list that includes a table with column classes identical to the ones it used to return (saved as testoutput_ejscreenit_10pts_1miles$table)", {
  expect_identical(
    sapply(apiref, class),
    sapply(apinow, class)
  )
  # all.equal(apiref, apinow)
})
test_that("ejscreenit() still returns identical table contents to what it used to", {
  expect_identical(
    apiref, 
    apinow
  )
})
test_that('ejscreenit() does not crash, for 2 points, x=pts; list of 3 outputs of correct class. table right NROW', {
  expect_no_error({
    out_ejscreenit <- ejscreenit(
      x = pts, radius = testradius,
      nosave = TRUE, nosee = TRUE, interactiveprompt = FALSE
    )}
  )
  # should be a data.frame, etc.
  expect_type(out_ejscreenit, 'list')
  expect_identical(names(out_ejscreenit), c('table', 'map', 'plot'))
  expect_identical(class(out_ejscreenit$table), 'data.frame')
  expect_true('leaflet' %in% class(out_ejscreenit$map))
  expect_true('ggplot' %in% class(out_ejscreenit$plot))
  expect_identical(NROW(out_ejscreenit$table) , NROW(pts))
  
})


# add other test cases here for various input parameters and conditions
