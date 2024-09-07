

# test cases/ examples

########################################################################## # 
# IF FILE PROVIDED, DONT ASK USER TO CONFIRM
# IF FOLDER PROVIDED, DONT ASK USER TO CONFIRM
# IF FILE NOT PROVIDED, DOES NOT ASK TO CONFIRM THE DEFAULT FILENAME IS OK 
# IF FOLDER NOT PROVIDED... IT ASKS TO CONFIRM THE DEFAULT FOLDER IS OK IF INTERACTIVE, SO SKIP THAT case IN THESE TESTS.

# ***  note if interactive it would try to prompt for folder even if folder specified  but not if fname specified 
# ## skip because it interactively interrupts testing to confirm folder
# 
# test_that("ejam2shapefile ok if save=T no folder specified", {
#   expect_no_error({
#     suppressWarnings({
#       suppressMessages({
#         junk = capture_output({
#           
#           # save TRUE - note if interactive it tries to prompt for folder
#           x <- ejam2shapefile(testoutput_ejamit_10pts_1miles, save = TRUE )
#           shp = shapefile_from_any(x)
#           # map_shapes_leaflet(shp)
#           
#         })
#       })
#     })
#   })
#   expect_true(file.exists(x))
#   expect_true("sf" %in% class(shp))
#   expect_equal(NROW(shp), 10)
# })

# ## skip because it interactively interrupts testing to confirm folder

# test_that("ejam2shapefile ok if use defaults - interactively pick folder", {
#   expect_no_error({
#     suppressWarnings({
#       suppressMessages({
#         junk = capture_output({
#           
#           # note if interactive it tries to prompt for folder
#           x = ejam2shapefile(testoutput_ejamit_10pts_1miles )
#           # zip::zip_list(x) # not required by EJAM pkg
#           shp <- shapefile_from_any(x)
#           
#         })
#       })
#     })
#   })
#   expect_true(file.exists(x))
#   expect_true("sf" %in% class(shp))
#   expect_equal(NROW(shp), 10)
# })
########################################################################## # 

# no prompt if folder is specified, or if save=F    


test_that("ejam2shapefile ok if save=F", {
  expect_no_error({
    # save FALSE
    shp <- ejam2shapefile(testoutput_ejamit_10pts_1miles, save = FALSE)
    # map_shapes_leaflet(shp)
  })
  expect_true("sf" %in% class(shp))
  expect_equal(NROW(shp), 10)
})

test_that("ejam2shapefile ok if folder=tempdir()", {
  expect_no_error({
    suppressWarnings({
      suppressMessages({
        junk = capture_output({
          
          # provide folder
          x = ejam2shapefile(testoutput_ejamit_10pts_1miles, save = TRUE, folder = tempdir()) # save = T is default
          # zip::zip_list(x)  # not required by EJAM pkg
          # browseURL(dirname(x))
          # dir(dirname(x), pattern = "zip")
          shp <- shapefile_from_any(x)
          # shp[1:3,4:8] 
          
        })
      })
    })
  })
  expect_true(file.exists(x))
  expect_true("sf" %in% class(shp))
  expect_equal(NROW(shp), 10)
})


test_that("ejam2shapefile ok if folder and fname both specified", {
  expect_no_error({
    suppressWarnings({
      suppressMessages({
        junk = capture_output({
          
          # both
          x = ejam2shapefile(testoutput_ejamit_10pts_1miles, fname = 'test.shp', folder = tempdir()) # save = T is default
          # zip::zip_list(x) # not required by EJAM pkg
          shp <- shapefile_from_any(x)
          
        })
      })
    })
  })
  expect_true(file.exists(x))
  expect_true("sf" %in% class(shp))
  expect_equal(NROW(shp), 10)
})



