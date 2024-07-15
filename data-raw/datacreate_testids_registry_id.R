
## code to prepare `testids_registry_id` dataset  

testids_registry_id <- c(
  110071293460, 110070874073, 110070538057, 110044340807,
  110030509215, 110019033810, 110056111559, 110056982323
)

testids_registry_id <- metadata_add(testids_registry_id)

usethis::use_data(testids_registry_id, overwrite = TRUE)
 
#see  EJAM function    latlon_from_regid(testids_registry_id)

#################################### #
# check what is created

# data creation scripts

dir("./data-raw/", pattern = "datacreate_.*id")


# lazy loaded data objects in R

x = EJAM:::datapack("EJAM")
x[grepl("id", x$Item), ]


# installed test files

dir(system.file("testdata", package = "EJAM"))
dir(system.file("testdata/registryid", package = "EJAM"))
dir(system.file("testdata/programid", package = "EJAM"))


# documentation files

dir('./R/', pattern = "data_.*id", ignore.case = T)

#################################### #

