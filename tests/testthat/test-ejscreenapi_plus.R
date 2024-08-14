################################ # ################################ # ejscreenapi_plus ####
cat('\ntesting ejscreenapi_plus()\n')

test_that("ejscreenapi_plus() works at all", {
  expect_no_error(
    suppressWarnings({
      ejscreenapi_plus( testpoints_5[1:2, ], radius = 0.5, 
                        save_when_report = F, on_server_so_dont_save_files = T, 
                        calculate_ratios = F,
                        verbose = FALSE) 
    })
  )
})
test_that('ejscreenapi_plus() does not crash for 1 point', {
  expect_no_error({outplus <- ejscreenapi_plus(x = testlon, y = testlat, radius = testradius, 
                                               on_server_so_dont_save_files = T, save_when_report = F, verbose = FALSE)})
  expect_equal(NROW(outplus), 1)
})
test_that('ejscreenapi_plus() does not crash for 2 points, outputs list (data.frame) of 2 rows', {
  expect_no_error({outplus2 <- ejscreenapi_plus(x = test2lon, y = test2lat, radius = testradius, 
                                                on_server_so_dont_save_files = T, save_when_report = F, verbose = FALSE)}) 
  expect_type(outplus2, "list")
  expect_identical(class(outplus2), 'data.frame')
  expect_equal(NROW(outplus2), 2)
})

# *** add a test for each of these settings in ejscreenapi_plus(

# format_report_or_json = "report"

# calculate_ratios = T # default

# usewhichnames = 'long'

# unit = "km"

# mapping_for_names = map_headernames

