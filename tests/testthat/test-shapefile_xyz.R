# tests for shapefile_ functions



testthat::test_that("test data files are available", {
  
  #  test data / test cases to use
  
  #   list.files(system.file("testdata/shapes/",     package = "EJAM"))
  #  ##"portland.gdb"  "portland.gdb.zip"  "portland.json"  
  #  ##"portland_folder_shp"  "portland_folder_shp.zip"
  #  ##"portland_shp.zip"  ### "stations_shp.zip" "stations.zip"
  
  expect_no_error({
    testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp",     package = "EJAM")
    testfilename_gdb       <- system.file("testdata/shapes/portland.gdb",            package = "EJAM")
    testfilename_gdbzip    <- system.file("testdata/shapes/portland.gdb.zip",        package = "EJAM")
    testfilename_zipdir    <- system.file("testdata/shapes/portland_folder_shp.zip", package = "EJAM")
    testfilename_zipdir2   <- system.file("testdata/shapes/portland_shp.zip",        package = "EJAM") # .shp etc basenames are NOT same as  .zip file basename
    testfilename_zipshp    <- system.file("testdata/shapes/stations.zip",            package = "EJAM") # .shp etc basenames ARE IDENTICAL TO .zip file basename
    testfilename_json      <- system.file("testdata/shapes/portland.json",           package = "EJAM")
    testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM") # Neighborhoods_regions.shp
  })
  expect_true(file.exists(testfilename_dirshp))
  expect_true(file.exists(testfilename_gdb))
  expect_true(file.exists(testfilename_gdbzip))
  expect_true(file.exists(testfilename_zipdir))
  expect_true(file.exists(testfilename_zipshp))
  expect_true(file.exists(testfilename_zipshp2))
  expect_true(file.exists(testfilename_json))
  expect_true(file.exists(testfilename_shp_alone))
})
################################################################ #

#   basic tests to start with


test_that("shapefile_from_any()  works", {
  
  testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp",     package = "EJAM")
  testfilename_gdb       <- system.file("testdata/shapes/portland.gdb",            package = "EJAM")
  testfilename_gdbzip    <- system.file("testdata/shapes/portland.gdb.zip",        package = "EJAM")
  testfilename_zipdir    <- system.file("testdata/shapes/portland_folder_shp.zip", package = "EJAM")
  testfilename_zipdir2   <- system.file("testdata/shapes/portland_shp.zip",        package = "EJAM") # .shp etc basenames are NOT same as  .zip file basename
  testfilename_zipshp    <- system.file("testdata/shapes/stations.zip",            package = "EJAM") # .shp etc basenames ARE IDENTICAL TO .zip file basename
  testfilename_json      <- system.file("testdata/shapes/portland.json",           package = "EJAM")
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM") # Neighborhoods_regions.shp
  
  #   x1  <- shapefile_from_any(testfilename_dirshp)    #ok
  #   x2  <- shapefile_from_any(testfilename_gdb)       # fails; warns that shapefile_filepaths_valid() needs vector with .shp etc.
  #   x3  <- shapefile_from_any(testfilename_gdbzip)   #ok
  #   x4  <- shapefile_from_any(testfilename_zipdir)  # reports error in shapefile_from_gdb() must have extension .gdb, but works anyway
  #   x5  <- shapefile_from_any(testfilename_zipshp)  # reports error in shapefile_from_gdb() must have extension .gdb, but works anyway
  #   x52 <- shapefile_from_any(testfilename_zipshp2)  #  
  #   x6  <- shapefile_from_any(testfilename_json) #
  #   x7  <- shapefile_from_any(testfilename_shp_alone)
  #   x8  <- shapefile_from_any(testfilenameset_4)
  # class(x1); class(x2); class(x3); class(x4); class(x5); class(x52); class(x6); class(x7); class(x8)
  # rm(x1, x2, x3, x4, x5, x52, x6,   x7, x8)
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_dirshp)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_gdb)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_gdbzip)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_zipdir)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_zipdir2)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_zipshp)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_json)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilename_shp_alone)
  })
  expect_true("sf" %in% class(JUNK))
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilenameset_4)
  })
  expect_true("sf" %in% class(JUNK))   
  
  rm(JUNK)
  
})
################################################################ #

testthat::test_that("shapefile_from_any()  works", {
  
  testfilenameset_4 <- shapefile_filepaths_validize(testfilename_shp_alone)
  
  expect_no_error({
    JUNK <- shapefile_from_any(testfilenameset_4)
  })
  expect_true("sf" %in% class(JUNK))   
  
})
################################################################ #



testthat::test_that("shapefile_xyz not crash on testdata folder and files", {
  
  expect_no_error({
    testfolder <- system.file("testdata/shapes/portland_folder_shp", package = "EJAM")
  })
  #################### #
  expect_no_error({
    testpaths <- shapefile_filepaths_from_folder(testfolder)
  })
  # all(file.exists(testpaths))  #################### #
  expect_no_error({
    shapefile_filepaths_valid(testpaths)
  })
  #################### #
  expect_no_error({
    testshape  <- shapefile_from_filepaths(testpaths) # need to fix
  })
  #################### #
  expect_no_error({
    testshape2 <- shapefile_from_folder(testfolder) # need to fix
  })
  expect_equal(testshape, testshape2)
  #################### #
  expect_no_error({
    JUNK <- shapefile_clean(testshape)
  })
  #################### #  
  expect_no_error(       shapefile_filepaths_from_folder(tempdir()))  # character(0)
  expect_equal(0, length(shapefile_filepaths_from_folder(tempdir())))
  expect_warning(  shapefile_from_folder(tempdir()))
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
################################################################ #



test_that("shapefile_filepaths_validize() works", {
  
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM") # Neighborhoods_regions.shp
  expect_no_error({
    JUNK <-  testfilenameset_4 <- shapefile_filepaths_validize(testfilename_shp_alone)
  })
  # expect_true("sf" %in% class(JUNK))
  rm(JUNK)
})
################################################################ #



# 
# test_that("shapefile_from_ xxxxxxxxxxxxxxxxxxxxxxxx () works", {
#   
#   expect_no_error({
#     JUNK <- 0
#   })
#   expect_true("sf" %in% class(JUNK))
#   rm(JUNK)
# })
# ################################################################ #














################################################################################ #
 

#   x1b  <- shapefile_from_folder(testfilename_dirshp)
#   x2b  <- shapefile_from_gdb(   testfilename_gdb)
#   x3b  <- shapefile_from_gdbzip(testfilename_gdbzip)
#   x4b  <- shapefile_from_zip(   testfilename_zipdir) # reports error but works
#   x5b  <- shapefile_from_zip(   testfilename_zipshp)
#   x52b <- shapefile_from_zip(   testfilename_zipshp2)
#   x6b  <- shapefile_from_json(  testfilename_json)
#   x7b  <- shapefile_from_filepaths(testfilename_shp_alone) # just one of the names needed- may want this to work too
#   x8b  <- shapefile_from_filepaths(testfilenameset_4)  # vector of names
# class(x1b); class(x2b); class(x3b); class(x4b); class(x52b); class(x6b); class(x7b); class(x8b)
# rm(x1b, x2b, x3b, x4b, x5b, x52b, x6b,   x7b, x8b)

################################################################################ #


