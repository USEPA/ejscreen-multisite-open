################################ # ################################ # ejscreenapi1 ####
cat('\ntesting ejscreenapi1()\n')

test_that('ejscreenapi1() does not crash on 1 point', {
  cat('testing 1 point in slow functionsejscreenapi1 \n')
  # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
  expect_no_error( ejscreenapi1(lat = testpoints_5$lat[1], lon = testpoints_5$lon[1], radius = testradius) )
})
test_that('ejscreenapi() does not crash on 2 points', {
  cat('testing 2 points in slow functionsejscreenapi1 \n')
  # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
  expect_no_error( ejscreenapi(lat = testpoints_5$lat[1:2 ], lon = testpoints_5$lon[1:2 ], radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE))     
})

