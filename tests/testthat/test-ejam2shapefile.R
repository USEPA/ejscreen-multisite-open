

# test cases/ examples

########################################################################## # 
# IF FILE PROVIDED, DONT ASK USER TO CONFIRM
# IF FOLDER PROVIDED, DONT ASK USER TO CONFIRM
# IF FILE NOT PROVIDED, DOES NOT ASK TO CONFIRM THE DEFAULT FILENAME IS OK 
# IF FOLDER NOT PROVIDED... IT ASKS TO CONFIRM THE DEFAULT FOLDER IS OK IF INTERACTIVE, SO SKIP THAT case IN THESE TESTS.

if (interactive() & !exists("noquestions")) {
  if ( askYesNo("run tests where you have to interactively specify a folder for shapefiles?")) {
    noquestions = FALSE
  }  else {
    noquestions <- TRUE
  }
} else {
  noquestions = TRUE
}

# ***  note if interactive it normally tries to prompt for shapefile folder in some cases  

test_that("ejam2shapefile ok if save=T", {
  
  testthat::skip_if(noquestions) 
  
  expect_no_error({
    suppressWarnings({
      suppressMessages({
        junk = capture_output({
          
          # save TRUE - note if interactive it tries to prompt for folder
          x <- ejam2shapefile(testoutput_ejamit_10pts_1miles, save = TRUE)
          shp = shapefile_from_any(x)
          # map_shapes_leaflet(shp)
          
        })
      })
    })
  })
  expect_true(file.exists(x))
  expect_true("sf" %in% class(shp))
  expect_equal(NROW(shp), 10)
})

test_that("ejam2shapefile ok if use defaults", {
  
  testthat::skip_if(noquestions) 
  
  expect_no_error({
    suppressWarnings({
      suppressMessages({
        junk = capture_output({
          
          # defaults - note if interactive it tries to prompt for folder
          x = ejam2shapefile(testoutput_ejamit_10pts_1miles)
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

test_that("ejam2shapefile ok if use defaults", {
  
  testthat::skip_if(noquestions) 
  
  expect_no_error({
    suppressWarnings({
      suppressMessages({
        junk = capture_output({
          
          # provide fname - note if interactive it tries to prompt for folder
          x = ejam2shapefile(testoutput_ejamit_10pts_1miles, fname = "test.shp")
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
########################################################################## # 

# no prompt if folder is specified, or if save=F    


test_that("ejam2shapefile ok if save=F", {
  expect_no_error({
    expect_warning( # some specified varnames not found
      # save FALSE
      {shp <- ejam2shapefile(testoutput_ejamit_10pts_1miles, save = FALSE)}
    )
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



