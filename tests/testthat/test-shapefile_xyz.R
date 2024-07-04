# very very basic tests for shapefile_ functions to start with

# possible test data:
#
#   testfilename_dirshp <- system.file("testdata/shapes/portland_folder_shp", package = "EJAM")
#   testfilename_gdb    <- system.file("testdata/shapes/portland.gdb",        package = "EJAM")
#   testfilename_gdbzip <- system.file("testdata/shapes/portland.gdb.zip",    package = "EJAM")
#   testfilename_zipdir <- system.file("testdata/shapes/portland_shp.zip",    package = "EJAM") # and stations_shp.zip
#   testfilename_zipshp <- system.file("testdata/shapes/stations.zip",        package = "EJAM")
#   testfilename_json   <- system.file("testdata/shapes/portland.json",       package = "EJAM")
#
#   file.exists(testfilename_dirshp)
#   file.exists(testfilename_gdb)
#   file.exists(testfilename_gdbzip)
#   file.exists(testfilename_zipdir)
#   file.exists(testfilename_zipshp)
# file.exists(testfilename_json)
#
#   x1 <- shapefile_from_any(testfilename_dirshp)    #ok
#   x2 <- shapefile_from_any(testfilename_gdb)       # fails; warns that shapefile_filepaths_valid() needs vector with .shp etc.
#   x3 <- shapefile_from_any(testfilename_gdbzip)   #ok
#   x4 <- shapefile_from_any(testfilename_zipdir)  # reports error in shapefile_from_gdb() must have extension .gdb, but works anyway
#   x5 <- shapefile_from_any(testfilename_zipshp)  # reports error in shapefile_from_gdb() must have extension .gdb, but works anyway
# x6 <- shapefile_from_any(testfilename_json) #
#
#   x1b = shapefile_from_folder(testfilename_dirshp)
#   x2b = shapefile_from_gdb(   testfilename_gdb)
#   x3b = shapefile_from_gdbzip(testfilename_gdbzip)
#   x4b = shapefile_from_zip(   testfilename_zipdir) # reports error but works
#   x5b = shapefile_from_zip(   testfilename_zipshp)
#   x6b = shapefile_from_json(   testfilename_json)

# class(x1); class(x2); class(x3); class(x4); class(x5); class(x6)
# class(x1b); class(x2b); class(x3b); class(x4b); class(x5b); class(x6b)
# rm(x1, x2, x3, x4, x5, x6)
# rm(x1b, x2b, x3b, x4b, x5b, x6b)


testthat::test_that("shapefile_ functions do not crash on testdata folder and files", {

  expect_no_error({
    testfolder <- system.file("testdata/shapes/portland_folder_shp", package = "EJAM")
  })
  # dir.exists(testfolder)
  expect_no_error({
    testpaths <- shapefile_filepaths_from_folder(testfolder)
  })
  # all(file.exists(testpaths))
  expect_no_error({
    shapefile_filepaths_valid(testpaths)
  })

  expect_no_error({
    testshape  <- shapefile_from_filepaths(testpaths) # need to fix
  })
  expect_no_error({
    testshape2 <- shapefile_from_folder(testfolder) # need to fix
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

