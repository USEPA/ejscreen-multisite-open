# very very basic tests for shapefile_ functions to start with

testthat::test_that("shapefile_ functions do not crash on testdata folder and files", {

  expect_no_error({
    testfolder <- system.file("testdata/shapes/Portland_neighborhoods", package = "EJAM")
  })

  expect_no_error({
    testpaths <- shapefile_filepaths_from_folder(testfolder)
  })
  expect_no_error({
    shapefile_filepaths_valid(testpaths)
  })
  expect_no_error({
    testshape  <- shapefile_from_filepaths(testpaths)
  })
  expect_no_error({
    testshape2 <- shapefile_from_folder(testfolder)
  })
  expect_equal(testshape, testshape2)

  expect_no_error({
    JUNK <- shapefile_clean(testshape)
    })

  expect_no_error(       shapefile_filepaths_from_folder(tempdir()))  # character(0)
  expect_equal(0, length(shapefile_filepaths_from_folder(tempdir())))

  expect_warning(    shapefile_from_folder(tempdir()))
  expect_warning({
    nullresults <- shapefile_from_folder(tempdir())
    })
  suppressWarnings(expect_equal(NULL, nullresults))

  rm(testfolder, testpaths, testshape, testshape2, JUNK)
})

################################################################ #

test_that("shapefile_from_gdb() works", {

  expect_no_error({
    JUNK <- shapefile_from_gdb(system.file("testdata/shapes/portland.gdb", package = "EJAM"))
  })
  expect_true("sf" %in% class(JUNK))
  rm(JUNK)
})
################################################################ #

test_that("shapefile_from_gdbzip() works", {

  expect_no_error({
    JUNK <- shapefile_from_gdbzip(system.file("testdata/shapes/portland.gdb.zip", package = "EJAM"))
  })
  expect_true("sf" %in% class(JUNK))
  rm(JUNK)
})

