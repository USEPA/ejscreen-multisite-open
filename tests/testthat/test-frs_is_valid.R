## unit tests for EJAM::frs_is_valid
## Author: Sara Sokolinski

# first I downloaded an FRS EPA query CSV file 
# tested using it in the app
# Invalid Filetype error when I use a CSV even though it lists CSV as a valid type
# Convert to xlsx
# Reads it in now

# works with standard REGISTRY_ID column
test_that('REGISTRY_ID column is recognized',{
  expect_true(val <- frs_is_valid(data.frame("REGISTRY_ID" = 110009441979,
                                             "PRIMARY_NAME" = "CAPITAL EXXON")))
  })

# works with regid column 
# script frs_is_valid trys to handle "regid" but looks to replace "RegistryID"
test_that('regid column is recognized',{
  expect_true(val <- frs_is_valid(data.frame("regid" = 110009441979,
                                                "PRIMARY_NAME" = "CAPITAL EXXON")))
})

# works with RegistryID column
# script frs_is_valid trys to handle "siteid" twice. Was the first one supposed to 
# look for "RegistryID" and this one to look for "regid"?
test_that('RegistryID column is recognized',{
  expect_true(val <- frs_is_valid(data.frame("RegistryID" = 110009441979,
                                             "PRIMARY_NAME" = "CAPITAL EXXON")))
})

# works with siteid column
# regid, RegisttryID and siteid all have issues caused by 
# frs_from_regid ( script name is frs_from_siteid ) returning an empty data frame
test_that('siteid column is recognized',{
  expect_true(val <- frs_is_valid(data.frame("siteid" = 110009441979,
                                             "PRIMARY_NAME" = "CAPITAL EXXON")))
})

# those are the four column names in frs_is_valid