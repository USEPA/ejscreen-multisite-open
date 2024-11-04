
test_that("report_residents_within_xyz ok", {
  
  ############################## # 
  test1 <- list(
    # list('latlon', 0, 1), # cannot occur - zero radius with latlon type
    # list('latlon', 0, 100), # cannot occur - zero radius with latlon type
    list('latlon', 3, 1),
    list('latlon', 3, 100),
    
    list('fips', 0, 1),
    list('fips', 0, 100),
    # list('fips', 3, 1), # cannot occur - nonzero radius with fips type
    # list('fips', 3, 100), # cannot occur - nonzero radius with fips type
    # list( NA, '99 miles', 'seven sites'),  # fails if NA
    
    list('shp', 0, 1),
    list('shp', 0, 100),
    list('shp', 3, 1),
    list('shp', 3, 100),
    
    list('farm',              '99 miles',          'seven'),
    list( "Georgia location", '9.9 kilometers',    "several"),
    list('study location',    "close proximity to",  100),
    
    list('Type X site', 3, 100)   # ok singular / plural 
  )
  ############################## # 
  test2 <- list(
    
    # fix/note singular/plural
    
    list('Type X facility', 3, 100), 
    list('Type X facilities', 3, 100), 
    
    # fix "within" 
    
    list('study location', "at", 100),       # within at
    list('Delaware Counties', "within", 3),  # within 'within' 
    
    # fix "" cases
    
    list( "Georgia location", '9.9 kilometers', ""), 
    list( "", '9.9 kilometers', "several"),
    list( "Georgia location", '', "several"),
    list('', '', '')
  )
  ############################## # 
  test3 <- list(
    #   na values 
    list(NA, 3, 100),
    list('latlon', NA, 100),
    list('latlon', 3, NA)
  )
  ############################## # 
  test4 <- list( 
    # NULL values 
    list(NULL, 3, 100),
    list('latlon', NULL, 100),
    list('latlon', 3, NULL)
  )
  ############################## #   ############################## # 
  
  checkit <- function(mytest) {
    params <- rbindlist(mytest)
    names(params) <- c('sitetype', 'radius', 'nsites')
    data.frame(params,
               text = sapply(mytest, function(z) {
                 report_residents_within_xyz(
                   sitetype = z[[1]], 
                   radius = z[[2]], 
                   nsites = z[[3]]
                 )
               })
    )
  }
  ############################## #   ############################## # 
  
  expect_no_error({
    x = checkit(test1)
  }
  # , label = "test1" 
  )
  
  
  expect_no_error({
    x = checkit(test2)
  })
  
  expect_no_error({
    x = checkit(test3)
  })
  
  expect_no_error({
    x = checkit(test4)
  })
  
  
})
