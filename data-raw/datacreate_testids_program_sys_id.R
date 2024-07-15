
## code to prepare `testids_program_sys_id` dataset

testids_program_sys_id <- c(
  "7-0540-00003", "354362", "1513529", "485659", "LAG750956", 
  "CAC002995519", "3601252181", "3601439158"
)

testids_program_sys_id <- metadata_add(testids_program_sys_id)

usethis::use_data(testids_program_sys_id, overwrite = TRUE)

# see  EJAM function  latlon_from_programid(testids_program_sys_id)

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

