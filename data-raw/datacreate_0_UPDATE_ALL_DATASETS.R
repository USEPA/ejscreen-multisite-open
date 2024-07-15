############################################################### #

## Scripts to update / create latest versions of datasets 
# ANNUAL blockgroup data from ACS and EJScreen, etc.
# see EJAM's github issues about this

# load latest source functions EJAM/R/*.R and EJAM/data/*.rda files ####

rm(list = ls())
rawdir = "./data-raw"
if (!dir.exists(rawdir)) {stop("need to do this from source package folder, EJAM/")}
golem::detach_all_attached()
library(devtools)
library(rstudioapi)

# load_all() to attach source versions of both exported and internal functions and data 
#  from /R/ and /data/ source folders, like metadata_add(), newly saved .rda files, etc.
#  Otherwise internal functions don't work in scripts, and it would use installed not new source versions.

load_all()

######################################### #
#
# List of datacreate_ files & When to use each ####

## If you want to easily open and edit one of the datacreate_ files, you can use source a line below

fnames <- dir('./data-raw', pattern = 'datacreate_')
cat("\n\n", paste0(paste0(
  "\t documentOpen('", rawdir, "/", fnames, "')"), collapse = "\n"))

if (0 == 1) {

  # with annual ejscreen data updates
  # 
  #  new indicators, variable names
  documentOpen('./data-raw/datacreate_map_headernames.R')       # ok
  documentOpen('./data-raw/datacreate_names_of_indicators.R')   # ok
  documentOpen('./data-raw/datacreate_names_pct_as_fraction.R') # ok
  #  ejscreen demog and envt data on every blockgroup
  documentOpen('./data-raw/datacreate_blockgroupstats2.3.R') # and bgej           # in progress
  documentOpen('./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R')   # in progress
  #  lookup tables
  documentOpen('./data-raw/datacreate_usastats2.3.R')                 # in progress
  documentOpen('./data-raw/datacreate_usastats2.3_add_dsubgroups.R')  # in progress
  documentOpen('./data-raw/datacreate_avg.in.us.R')                   # ok
  documentOpen('./data-raw/datacreate_high_pctiles_tied_with_min.R')  # ok
  #  calculations and examples of outputs
  documentOpen('./data-raw/datacreate_formulas.R')                    # in progress; maybe not used yet
  documentOpen('./data-raw/datacreate_testpoints_testoutputs.R')      # ok
  
  # when census fips codes or boundaries change (& ejscreen updates accordingly)
  #
  # To create and save the datasets from within the EJAM source package root folder,
  # 
  #   blocks
  documentOpen('./data-raw/datacreate_blockwts.R')           # needs Island Areas added
  #   blockgroups
  documentOpen('./data-raw/datacreate_bg_cenpop2020.R')      # confirm if changed since 2020
  documentOpen('./data-raw/datacreate_bgpts.R')              # redundant w bg_cenpop2020, pick one to use
  #   states
  documentOpen('./data-raw/datacreate_states_shapefile.R')   # check if want 2020 or 2022+ file
  documentOpen('./data-raw/datacreate_stateinfo.R')          # ok (missing Island Areas)
  documentOpen('./data-raw/datacreate_stateinfo2.R')         # ok (has Island Areas)
  #   other geo
  documentOpen('./data-raw/datacreate_test_address.R')       # ok
  documentOpen('./data-raw/datacreate_islandareas.R')        # ok
  documentOpen('./data-raw/datacreate_censusplaces.R')       # not used yet
  
  # when frs/naics/sic info is updated
  #
  documentOpen('./data-raw/datacreate_frs_.R')            #
  documentOpen('./data-raw/datacreate_frs_by_mact.R')     #
  documentOpen('./data-raw/datacreate_frs_by_sic.R')      #
  documentOpen('./data-raw/datacreate_frsprogramcodes.R') #
  documentOpen('./data-raw/datacreate_epa_programs.R')    #
  documentOpen('./data-raw/datacreate_naics_counts.R')    # script
  documentOpen('./data-raw/datacreate_naicstable.R')      # script. does date_saved_in_package & use_data
  documentOpen('./data-raw/datacreate_SIC.R')             
  documentOpen('./data-raw/datacreate_sic_counts.R')      
  documentOpen('./data-raw/datacreate_sictable.R')        
  documentOpen('./data-raw/datacreate_testids_program_sys_id.R')  # 
  documentOpen('./data-raw/datacreate_testids_registry_id.R')     #
  
  # when ANY of datasets is updated that needs to be stored in pins
  #
  documentOpen('./data-raw/datacreate_pins.R')             # in progress ***
  
  # misc for package to work
  #
  documentOpen('./data-raw/datacreate_lat_alias.R')
  documentOpen('./data-raw/datacreate_ejampackages.R')
  documentOpen('./data-raw/datacreate_meters_per_mile.R')
  
}

######################################### #
# metadata ####
#
## use simple metadata for data not related to EJScreen or Census, like just frs-related, naics-related, etc.
# attr(x, "date_downloaded")       <- as.character(Sys.Date()) # if relevant
# attr(x, "date_saved_in_package") <- as.character(Sys.Date())

## use full metadata if related to ejscreen or census/acs
# x <- metadata_add(x)
######################################### #

# # new indicators, variable names
# documentOpen('./data-raw/datacreate_map_headernames.R')
# documentOpen('./data-raw/datacreate_names_of_indicators.R')
# documentOpen('./data-raw/datacreate_names_pct_as_fraction.R')
# #  ejscreen demog and envt data on every blockgroup
# documentOpen('./data-raw/datacreate_blockgroupstats2.3.R') # and bgej
# documentOpen('./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R')
# #  lookup tables
# documentOpen('./data-raw/datacreate_usastats2.3.R')
# documentOpen('./data-raw/datacreate_usastats2.3_add_dsubgroups.R')
# documentOpen('./data-raw/datacreate_avg.in.us.R')
# documentOpen('./data-raw/datacreate_high_pctiles_tied_with_min.R')
# documentOpen('./data-raw/datacreate_formulas.R')
# documentOpen('./data-raw/datacreate_testpoints_testoutputs.R')
######################################### ########################################## #

######################################### #
# datacreate_map_headernames.R ####
#
#  MUST UPDATE THE map_headernames.xlsx MANUALLY, FIRST
#  then do this to convert it to dataset for package
#  This just reads spreadsheet, essentially.

# rstudioapi::documentOpen("./data-raw/datacreate_map_headernames.R")
source("./data-raw/datacreate_map_headernames.R")
map_headernames <- datacreate_map_headernames(
  './data-raw/map_headernames_2.3.xlsx'
)
map_headernames <- metadata_add(map_headernames)
usethis::use_data(map_headernames, overwrite = TRUE)

######################################### #
# datacreate_names_of_indicators.R ####
# datacreate_names_pct_as_fraction.R ####
#
### this will create but also assign metadata to and save for pkg via use_data()
### unlike other datacreate_  functions that do not do the metadata and use_data steps!
### It is really kind of a script, but packaged as a function here so that
### all the variables created do not show up in the global environment - they get saved in pkg ready for lazy-loading if/when needed
### BUT any subsequent scripts that depend on those will not use the correct new versions unless we do load.all() anyway... 
### metadata is assigned inside this function
### use_data is done inside this function

# rstudioapi::documentOpen("./data-raw/datacreate_names_of_indicators.R")
# rstudioapi::documentOpen("./data-raw/datacreate_names_pct_as_fraction.R")

source("./data-raw/datacreate_names_of_indicators.R")
datacreate_names_of_indicators()    # this does metadata and use_data inside the function

source("./data-raw/datacreate_names_pct_as_fraction.R")
datacreate_names_pct_as_fraction(map_headernames = map_headernames)  # this does metadata and use_data inside the function
### Now use load_all() to make available those new variable name lists (the source package as just updated, not the version installed)

load_all()
######################################### #

######################################### #
# datacreate_blockgroupstats2.3 ####
# datacreate_blockgroupstats2.3_add_d_acs22columns ####
#
# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.3.R")
# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R")
#
source("./data-raw/datacreate_blockgroupstats2.3.R")

source("./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R")

blockgroupstats    <- metadata_add(blockgroupstats)
usethis::use_data(blockgroupstats, overwrite = T)

bgej <- metadata_add(bgej)
cat("bgej created in globalenv but not saved yet\n")
### do not save this one in the pkg - it is large
## do not save via  usethis::use_data(bgej, overwrite = TRUE)
## Save bgej to pins board as .arrow file
##  using script in    datacreate_pins.R
######################################### #


stop(' not finished here - datacreate_blockgroupstats2.3.R does some or all of usastats etc.
     so those scripts need to get merged, 
     etc.')

stop(' not finished here - see datacreate_blockgroupstats2.3.R also which downloads them')

######################################### #
# datacreate_usastats2.3.R ####
# datacreate_usastats2.3_add_dsubgroups.R ####
#
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.3.R")
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.3_add_dsubgroups.R")

# source("./data-raw/datacreate_usastats2.3.R")
# source("./data-raw/datacreate_usastats2.3_add_dsubgroups.R")
# 
# usastats_statestats  <- datacreate_usastats2.3()
# usastats_statestats  <- datacreate_usastats2.3_add_dsubgroups.R(usastats_statestats)
# 
# usastats   <- usastats_statestats$usastats
# statestats <- usastats_statestats$statestats


usastats   <- metadata_add(usastats)
statestats <- metadata_add(statestats)

usethis::use_data(usastats,   overwrite = T)
usethis::use_data(statestats, overwrite = T)
######################################### #


######################################### #
# datacreate_avg.in.us.R ####
#
# rstudioapi::documentOpen("./data-raw/datacreate_avg.in.us.R")
source("./data-raw/datacreate_avg.in.us.R")
avg.in.us <- datacreate_avg.in.us(usastats = usastats,  # must be the updated/new usastats
                                  longlist = unique(c(  # must use the updated/new indicator variable names
                                    names_e, 
                                    names_d, names_d_subgroups_nh, names_d_subgroups_alone
                                  ))
)
avg.in.us <- metadata_add(avg.in.us)
usethis::use_data(avg.in.us, overwrite = TRUE)
######################################### #


######################################### #
# datacreate_high_pctiles_tied_with_min.R ####
#
# rstudioapi::documentOpen("./data-raw/datacreate_high_pctiles_tied_with_min.R")
source("./data-raw/datacreate_high_pctiles_tied_with_min.R")
high_pctiles_tied_with_min <- datacreate_high_pctiles_tied_with_min(usastats, statestats)
high_pctiles_tied_with_min <- metadata_add(high_pctiles_tied_with_min)
usethis::use_data(high_pctiles_tied_with_min, overwrite = TRUE)
######################################### #


######################################### #
# datacreate_formulas.R ####
# 
# rstudioapi::documentOpen("./data-raw/datacreate_formulas.R")
source("./data-raw/datacreate_formulas.R")

######################################### #
# datacreate_testpoints_testoutputs.R ####

# rstudioapi::documentOpen("./data-raw/datacreate_testpoints_testoutputs.R")

source("./data-raw/datacreate_testpoints_testoutputs.R")

######################################### #


######################################### #
# datacreate_ejampackages.R ####

source('./data-raw/datacreate_ejampackages.R')
# that script does metadata_add() and use_data()


######################################### #
# datacreate_blockwts.R ####
# 
# rstudioapi::documentOpen("./data-raw/datacreate_blockwts.R")

stop("not finished updating 'datacreate_blockwts.R' script to include Island Areas GU VI MP AS")

source('./data-raw/datacreate_blockwts.R') # script that includes metadata_add() and use_data()

# Creates blockwts, blockpoints, etc.,
# but does not have to get updated 
# except when FIPS codes or boundaries change for blocks or blockgroups.
#  or possibly to add data that had been missing, for Island Areas AS, GU, MP, VI ***

######################################### #
# datacreate_bg_cenpop2020.R ####
# datacreate_bgpts.R ####

# rstudioapi::documentOpen("./data-raw/datacreate_bg_cenpop2020.R")
# rstudioapi::documentOpen("./data-raw/datacreate_bgpts.R")

warning("see notes in `./data-raw/datacreate_bg_cenpop2020.R`-- bgpts and bg_cenpop2020 are very, very similar, so may want to consolidate to use only one.")
if (askYesNo("recreate bg_cenpop2020?")) {source("./data-raw/bg_cenpop2020.R")}
if (askYesNo("recreate bgpts?")) {source("./data-raw/datacreate_bgpts.R")}

######################################### #




######################################### #
# datacreate_pins.R ####
#
# rstudioapi::documentOpen("./data-raw/datacreate_pins.R")

warning("must use VPN to have access to pins board ")
# in progress ***
stop(" need to finish/ confirm finished updating datacreate_pins.R")


source("./data-raw/datacreate_pins.R")
######################################### #


