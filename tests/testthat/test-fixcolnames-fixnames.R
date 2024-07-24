


############  THESE TESTS WERE ORIGINALLY IN THE EJAMejscreenapi package


#################### ## test data ####

test.original <- c(
  "S_E_TRAFFIC_PER","RAW_D_INCOME","N_P5_PM25", 
  "unfound", NA)
test.friendly <- c(
  "state.pctile.traffic.score", "pctlowinc", "pctile.EJ.DISPARITY.pm.supp", 
  "unfound", NA)
test.long <- c(
  "State percentile for Traffic Proximity and Volume (daily traffic count/distance to road)",
  "% Low Income",
  "US percentile for EJ Supplemental Index for Particulate Matter (PM 2.5)", 
  "unfound", NA)
# cbind(test.original, test.friendly, test.long)
all.original <- map_headernames$apiname[!is.na(map_headernames$apiname) & nchar(map_headernames$apiname) > 0]
all.long     <- map_headernames$longname[!is.na(map_headernames$longname) & nchar(map_headernames$longname) > 0]
all.rname    <- map_headernames$rname[!is.na(map_headernames$rname) & nchar(map_headernames$rname) > 0]


################################ # fixnames ####

cat('\n testing fixnames() \n')


test_that('ejscreenapi() output colnames get renamed (at least 1 does)', {
  expect_gt(length(setdiff(fixnames(names(out_api)), names(out_api))) , 0)   # at least 1 got renamed
})
test_that('ejscreenapi() outputs work in fixnames(), returning char vector of right length, totalPop renamed to pop', {
  oldvars <- colnames(out_api) # out_api was from ejscreenapi()
  vars <- fixnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_gt(length(setdiff(vars, names(out_api))) , 0) # at least 1 got renamed
  expect_vector(vars)
  expect_type(vars, "character")
  expect_identical(length(vars),  length(oldvars))
  # totalPop is in outputs and gets renamed as pop
  expect_identical(vars[grepl("totalPop", oldvars)] , 'pop')
})
test_that('fixnames() for ejscreenapi() outputs works same if provided colnames or the whole data.frame, matrix, data.table, list', {
  a = fixnames(names(out_api))
  t1 = fixnames(out_api)
  t2 = fixnames(data.table::as.data.table(out_api))
  # dmatrix = matrix(1, nrow = 2, ncol = NCOL(out_api))
  # colnames(dmatrix) <- colnames(out_api)
  dmatrix <- as.matrix(out_api)
  t3 = fixnames(dmatrix)
  t4 = fixnames(list(totalPop = 1001:1002, other = 1:2))
  expect_identical(a,t1)
  expect_identical(a,t2)
  expect_identical(a,t3)
  expect_identical(c('pop','other'), t4)
})

##################### more for fixnames 

test_that(desc = 'fixnames() output is char vector of right length, for a simple test set of 2 names', {
  oldvars <- c('totalPop', 'y')
  vars <- fixnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_vector(vars)
  expect_type(vars, "character")
  expect_identical(length(vars), length(oldvars))
})
test_that(desc = 'fixnames renames totalPop to pop for correct element', {
  oldvars <- c('totalPop', 'y')
  vars <- fixnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_equal(grepl("totalPop", oldvars), grepl("pop", vars))
})
test_that('fixnames() returns 1 for 1, NA for NA even if all are NA', {
  # renaming works: 1 or more API indicator names including totalPop get renamed as character vector, same length, NA as NA
  expect_identical(fixnames('just_one_item') , 'just_one_item')
  expect_identical(fixnames(c("validword", NA_character_)) , c("validword", NA))
  expect_identical(is.na(fixnames(NA)), TRUE)  
})


#################### #  fixcolnames ####

cat('\n testing fixcolnames() \n')


test_that(desc = 'fixcolnames() output is char vector of right length, for a simple test set of 2 names', {
  oldvars <- c('totalPop', 'y')
  vars <- fixcolnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_vector(vars)
  expect_type(vars, "character")
  expect_identical(length(vars), length(oldvars))
})
test_that(desc = 'fixcolnames renames totalPop to pop for correct element', {
  oldvars <- c('totalPop', 'y')
  vars <- fixcolnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_equal(grepl("totalPop", oldvars), grepl("pop", vars))
})
test_that('fixcolnames() returns 1 for 1, NA for NA even if all are NA', {
  # renaming works: 1 or more API indicator names including totalPop get renamed as character vector, same length, NA as NA
  expect_identical(fixcolnames('just_one_item') , 'just_one_item')
  expect_identical(fixcolnames(c("validword", NA_character_)) , c("validword", NA))
  expect_identical(is.na(fixcolnames(NA)), TRUE)  
})

########### more tests for fixcolnames

testthat::test_that('fixcolnames() no error for all original names', {
  expect_no_error(fixcolnames(all.original, oldtype = 'api', newtype = 'api'))
  expect_no_error({all.r <- fixcolnames(all.original, oldtype = 'api', newtype = 'r')})
  expect_no_error(fixcolnames(all.original, oldtype = 'api', newtype = 'rname'))
  expect_no_error(fixcolnames(all.original, oldtype = 'api', newtype = 'friendly'))
  expect_no_error(fixcolnames(all.original, oldtype = 'api', newtype = 'long'))
  expect_no_error(fixcolnames(all.original, oldtype = 'api', newtype = 'original'))
  expect_no_error(fixcolnames(all.original, newtype = 'original'))
  expect_no_error(fixcolnames(all.original))
  expect_no_error(fixcolnames(all.original, oldtype = 'rname'))
})
testthat::test_that('fixcolnames() no error for all long names', {
expect_no_error(fixcolnames(all.long, oldtype = 'api', newtype = 'api'))
expect_no_error({all.r <- fixcolnames(all.long, oldtype = 'api', newtype = 'r')})
expect_no_error(fixcolnames(all.long, oldtype = 'api', newtype = 'rname'))
expect_no_error(fixcolnames(all.long, oldtype = 'api', newtype = 'friendly'))
expect_no_error(fixcolnames(all.long, oldtype = 'api', newtype = 'long'))
expect_no_error(fixcolnames(all.long, oldtype = 'api', newtype = 'original'))
expect_no_error(fixcolnames(all.long, newtype = 'original'))
expect_no_error(fixcolnames(all.long))
expect_no_error(fixcolnames(all.long, oldtype = 'rname'))
})
testthat::test_that('fixcolnames() no error for all r names', {
  expect_no_error(fixcolnames(all.rname, oldtype = 'api', newtype = 'api'))
  expect_no_error({all.r <- fixcolnames(all.rname, oldtype = 'api', newtype = 'r')})
  expect_no_error(fixcolnames(all.rname, oldtype = 'api', newtype = 'rname'))
  expect_no_error(fixcolnames(all.rname, oldtype = 'api', newtype = 'friendly'))
  expect_no_error(fixcolnames(all.rname, oldtype = 'api', newtype = 'long'))
  expect_no_error(fixcolnames(all.rname, oldtype = 'api', newtype = 'original'))
  expect_no_error(fixcolnames(all.rname, newtype = 'original'))
  expect_no_error(fixcolnames(all.rname))
  expect_no_error(fixcolnames(all.rname, oldtype = 'rname'))
})
 
########### more tests for fixcolnames

testthat::test_that("valid oldtype specified but inputs are not that type, so return all unchanged including NAs", {
  testthat::expect_true({
    # # wrong from, so just returns unchanged including NA as NA: 
    all(fixcolnames(test.friendly,
                    oldtype = 'long', newtype = 'original')     == test.friendly, na.rm = T)
  })
})

testthat::test_that("nonexistent oldtype specified so warn and return all unchanged", {
  testthat::expect_true({
    suppressWarnings( {  all(fixcolnames(test.friendly,
                                         oldtype = 'TYPEDOESNOTEXIST', newtype = 'original')    == test.friendly, na.rm = T)}) # NOW JUST WARNS
  })
  testthat::expect_warning({
    all(fixcolnames(test.friendly,
                    oldtype = 'TYPEDOESNOTEXIST', newtype = 'original')    == test.friendly, na.rm = T) # NOW JUST WARNS
  })
})
testthat::test_that("nonexistent newtype specified so warn and return all unchanged", {
  testthat::expect_true({
    suppressWarnings( {  all(fixcolnames(test.friendly,
                                         oldtype = 'friendly', newtype = 'TYPEDOESNOTEXIST')    == test.friendly, na.rm = T)}) # NOW JUST WARNS
  })
  testthat::expect_warning({
    all(fixcolnames(test.friendly,
                    oldtype = 'friendly', newtype = 'TYPEDOESNOTEXIST')    == test.friendly, na.rm = T) # NOW JUST WARNS
  })
})

###################### #

# turn these into tests: 


# fixcolnames(test.original, 
#             oldtype='original', newtype='friendly') == test.friendly #  
# fixcolnames(test.original, 
#             oldtype='original', newtype='long')     == test.long     #  
# 
# # from=to, so just returns unchanged: 
# fixcolnames(test.original, 
#             oldtype='original', newtype='original') == test.original #  
# # wrong from, so just returns unchanged:
# fixcolnames(test.original, 
#             oldtype='long',     newtype='original') == test.original # 
# 
# expect_error(
#   fixcolnames(test.original,
#               oldtype='wrong',  newtype='original')  # fails
# )
# 
# # from=to, so just returns unchanged:
# fixcolnames(test.friendly, 
#             oldtype='friendly', newtype='friendly') == test.friendly #  
# fixcolnames(test.friendly, 
#             oldtype='friendly', newtype='long')     == test.long #  
# fixcolnames(test.friendly, 
#             oldtype='friendly', newtype='original') == test.original # 

# # from long to friendly returns friendly
# fixcolnames(test.long, 
#             oldtype = 'long', newtype = 'friendly') == test.friendly

# # from=to, so just returns unchanged:
# fixcolnames(test.long, 
#             oldtype = 'long', newtype = 'long')     == test.long
# fixcolnames(test.long, 
#             oldtype = 'long', newtype = 'original') == test.original

 ############################## #


################################ # fixnames_to_type ####

cat('\n testing fixnames_to_type() \n')

test_that("fixnames_to_type() works at all", {
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'longname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'shortlabel')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$apiname, oldtype = 'apiname', newtype = 'longname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$apiname, oldtype = 'apiname', newtype = 'rname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$longname, oldtype = 'longname', newtype = 'rname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$longname, oldtype = 'longname', newtype = 'shortlabel')
  })
})
test_that("no error but warns and fails to rename if alias not actual colname used, unlike fixcolnames() which allows alias", {
  expect_no_error({
    suppressWarnings({
      fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'r', newtype = 'longname')
    })
  })
  expect_warning({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'r', newtype = 'longname')
  })
  # this function does not work except with exact colname, but warns instead of error
  expect_no_error({
    suppressWarnings({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'long')
    })
  })
  expect_warning({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'long')
  })
})

