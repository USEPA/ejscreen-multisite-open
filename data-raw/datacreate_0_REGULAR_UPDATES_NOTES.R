############################################################### #

## Scripts to update / create latest versions of datasets 
# ANNUAL blockgroup data from ACS and EJScreen ####
# see EJAM's github issues about this

if (!dir.exists("./data-raw")) {stop("need to do this from source package folder, EJAM/")}
golem::detach_all_attached()
rm(list = ls())
library(devtools)
load_all() # load source version of functions and data, but will update datasets


######################################### #
# datacreate_map_headernames.R
#
#  MUST UPDATE THE map_headernames.xlsx MANUALLY, FIRST
#  then do this to convert it to dataset for package
#  This just reads spreadsheet, essentially.

source("./data-raw/datacreate_map_headernames.R")
map_headernames <- datacreate_map_headernames(
  './data-raw/map_headernames_2.3.xlsx'
)
map_headernames <- EJAM:::metadata_add(map_headernames)
usethis::use_data(map_headernames, overwrite = TRUE)
######################################### #


######################################### #
# datacreate_names_of_indicators.R
# datacreate_names_pct_as_fraction_.R
#
### this will create but also assign metadata to and save for pkg via use_data()
### unlike other datacreate_  functions that do not do the metadata and use_data steps!
### It is really kind of a script, but packaged as a function here so that
### all the variables created do not show up in the global environment - they get saved in pkg ready for lazy-loading if/when needed
### BUT any subsequent scripts that depend on those will not use the correct new versions unless we do load.all() anyway... 
### metadata is assigned inside this function
### use_data is done inside this function

source("./data-raw/datacreate_names_of_indicators.R")
datacreate_names_of_indicators()    # this does metadata and use_data inside the function
datacreate_names_pct_as_fraction_(map_headernames = map_headernames)  # this does metadata and use_data inside the function
### Now use load_all() to make available those new variable name lists (the source package as just updated, not the version installed)
load_all()
######################################### #


######################################### #
# datacreate_blockgroupstats2.3
# datacreate_blockgroupstats2.3_add_d_acs22columns

source("./data-raw/datacreate_blockgroupstats2.3.R")
source("./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R")


stop(' not finished here ')


blockgroupstats <- datacreate_blockgroupstats2.3(    
  
  ### to be done - might need more parameters
  
  )

blockgroupstats <- datacreate_blockgroupstats2.3_add_d_acs22columns(
  
  blockgroupstats
  
  ### to be done - might need more parameters
  
)

blockgroupstats    <- EJAM:::metadata_add(blockgroupstats)
usethis::use_data(blockgroupstats, overwrite = T)
######################################### #


######################################### #
# datacreate_usastats2.3
# datacreate_usastats2.3_add_dsubgroups


stop(' not finished here ')


source("./data-raw/datacreate_usastats2.3.R")
source("./data-raw/datacreate_usastats2.3_add_dsubgroups.R")

usastats_statestats  <- datacreate_usastats2.3()
usastats_statestats  <- datacreate_usastats2.3_add_dsubgroups.R(usastats_statestats)

usastats   <- usastats_statestats$usastats
statestats <- usastats_statestats$statestats

usastats   <- EJAM:::metadata_add(usastats)
statestats <- EJAM:::metadata_add(statestats)

usethis::use_data(usastats,   overwrite = T)
usethis::use_data(statestats, overwrite = T)
######################################### #


######################################### #
# datacreate_us_avg
#
source("./data-raw/datacreate_us_avg.R")
avg.in.us <- metadata_add(avg.in.us)
usethis::use_data(avg.in.us, overwrite = TRUE)
######################################### #


######################################### #
# high_pctiles_tied_with_min
#
source("./data-raw/datacreate_high_pctiles_tied_with_min.R")
high_pctiles_tied_with_min <- datacreate_high_pctiles_tied_with_min(usastats, statestats)
high_pctiles_tied_with_min <- EJAM:::metadata_add(high_pctiles_tied_with_min)
usethis::use_data(high_pctiles_tied_with_min, overwrite = TRUE)
######################################### #


######################################### #
# datacreate_pins
#
warning("must use VPN to have access to pins board ")

source("./data-raw/datacreate_pins.R")
######################################### #


######################################### #
# datacreate_testpoints_testoutputs



