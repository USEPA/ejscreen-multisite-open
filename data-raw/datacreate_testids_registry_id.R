#################################### #
## script to create dataset

#################################### #
# check what was in INSTALLED package so far

### INSTALLED PACKAGE lazy loaded data test objects - for registryid or programid
x <- as.data.frame(data(package = "EJAM")$results)
cat("Already installed: \n\n")
print(x[grepl("id", x$Item), ])

### INSTALLED PACKAGE (not source) test files - for registryid or programid
# print( dir(base::system.file("testdata/registryid", package = "EJAM"), full.names = T) )
# print( dir(base::system.file("testdata/programid", package = "EJAM"), full.names = T) )
### if you dont specify base:: and you had done devtools::load_all() then it actually will look in the source package, not the installed package

#################################### #
# create the dataset

testids_registry_id <- c(
  110071293460, 110070874073, 110070538057, 110044340807,
  110030509215, 110019033810, 110056111559, 110056982323
)
if (anyNA( frs_from_regid(testids_registry_id)$lat)) {stop("some of the testids_registry_id are not in the FRS database or lack lat,lon")}
## or  
# latlon_from_regid(testids_registry_id)

## requires first load_all() or require() or EJAM::: to access the function
testids_registry_id <- metadata_add(testids_registry_id)

## compare the one in memory right now to the already-installed one:
# all.equal(test_regid, EJAM::test_regid)
all.equal(testids_registry_id, EJAM::testids_registry_id)
# all.equal(testids_program_sys_id, EJAM::testids_program_sys_id)


usethis::use_data(testids_registry_id, overwrite = TRUE)


#################################### #
## check what was just created (or was already) in SOURCE package


## test objects - for registryid or programid

dir("./data/", pattern = ".*id.*rda$", ignore.case = T, full.names = T)

# test files - for registryid or programid

cat("\n\n")
dir("./inst/testdata/programid", recursive = TRUE, full.names = T)
cat("\n\n")
dir("./inst/testdata/registryid", recursive = TRUE, full.names = T)
cat("\n\n")

# documentation files - for registryid or programid

dir('./R/', pattern = "data_.*id", ignore.case = T)

#################################### #
