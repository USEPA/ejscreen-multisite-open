############################################################### #

## Scripts to update / create latest versions of datasets 
# ANNUAL blockgroup data from ACS and EJScreen
# NON-ANNUAL (frequent, episodic, etc.) other datasets
# see EJAM's github issues about this

rm(list = ls())

# load latest source functions EJAM/R/*.R and EJAM/data/*.rda files ####

rawdir = "./data-raw"
if (!dir.exists(rawdir)) {stop("need to do this from source package folder, EJAM/")}

golem::detach_all_attached()
library(devtools)
library(rstudioapi)

# load_all() to attach source versions of both exported and internal functions and data 
#  from /R/ and /data/ source folders, like metadata_add(), newly saved .rda files, etc.
#  Otherwise internal functions don't work in scripts, and it would use installed not new source versions.

load_all()

if (!exists("askquestions")) {askquestions <- TRUE}
if (interactive()) {
  utils::askquestions <- askYesNo("Do you want to answer questions interactively like this about what to save where, etc.? (vs running all scripts without pauses)")
  if (is.na("askquestions")) {askquestions <- FALSE}
}

######################################### #
######################################### #
source_maybe <- function(scriptname = NULL, DOIT = TRUE, askquestions = NULL, question = paste0("Do ", scriptname, "?"), folder = NULL) {
  if (is.null(scriptname)) {stop("requires scriptname")}
  if (missing(askquestions)) {
    if (is.null(askquestions)) {askquestions <- TRUE} # else it was taken from parent env
  }
  if (missing(folder)) {
    if (is.null("folder")) {folder <-  "./data-raw"} #  else it was taken from parent env
  }
  # if (!exists("DOIT")) {DOIT <- TRUE}
  if (askquestions && interactive()) {
    DOIT <- askYesNo(question)
    if (!is.na(DOIT) && DOIT) {DOIT <- TRUE}
  }
  if (DOIT) {
    cat(paste0("Doing ", scriptname, " \n"))
    spath = file.path(folder, scriptname)
    if (!file.exists(spath)) {stop("requires valid folder and scriptname")}
    source(spath)
  } else {
    cat("Not doing ", scriptname, "\n")
  }
}
######################################### #
######################################### #

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
  documentOpen("./data-raw/datacreate_metadata4pins.R") # ok
  documentOpen('./data-raw/datacreate_blockgroupstats2.3.R') # and bgej      # ok
  documentOpen('./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R')   # ok
  #  lookup tables
  documentOpen('./data-raw/datacreate_usastats2.3.R')                 # in progress ?
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

######################################### ########################################## #

# check pins board ####

x <- datawrite_to_pins(justchecking = T) # load_all() first or use EJAM:::

# must use VPN to have access to pins board 
#               name                                        title  type file_size             created ejscreen_version varnames
# 1  frs_by_programid frs_by_programid data from EJScreen for EJAM arrow   124.14M 2023-12-11 17:21:17              2.2     TRUE
# 2              bgej             bgej data from EJScreen for EJAM arrow     97.5M 2023-12-05 22:20:29              2.2     TRUE
# 3       frs_by_mact      frs_by_mact data from EJScreen for EJAM arrow     2.31M 2023-12-11 17:22:00              2.2     TRUE
# 4        frs_by_sic       frs_by_sic data from EJScreen for EJAM arrow    20.35M 2023-12-11 17:21:53              2.2     TRUE
# 5      frs_by_naics     frs_by_naics data from EJScreen for EJAM arrow    16.93M 2023-12-11 17:21:48              2.2     TRUE
# 6               frs              frs data from EJScreen for EJAM arrow   145.22M 2023-12-11 17:20:42              2.2     TRUE
# 7          blockwts         blockwts data from EJScreen for EJAM arrow    91.67M 2023-11-17 19:14:23              2.2     TRUE
# 8          quaddata                       quaddata data for EJAM arrow   218.36M 2023-11-17 19:13:32              2.2     TRUE
# 9       blockpoints                    blockpoints data for EJAM arrow   155.97M 2023-11-17 19:12:57              2.2     TRUE
# 10     blockid2fips                   blockid2fips data for EJAM arrow    98.17M 2023-11-17 19:12:36              2.2     TRUE
# 11        bgid2fips                      bgid2fips data for EJAM arrow     2.98M 2023-11-17 19:12:19              2.2     TRUE
pin_seen = x$name
pin_expected = c(
  'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
  'bgej', 'bgid2fips',
  'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"
)
if (length(setdiff2(pin_seen, pin_expected)) > 0 ) {
  message("Expected to see on pin board but not there: ", paste0(setdiff(pin_expected, pin_seen), collapse = ", "))
  message("Seeon on pin board but not expected: ", paste0(setdiff(pin_seen, pin_expected), collapse = ", "))
}
######################################### ########################################## #

######################################### #
# datacreate_map_headernames.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_map_headernames.R")
#  UPDATE map_headernames.xlsx MANUALLY, 
#  then do this to read .xlsx and save as dataset for package
source_maybe("datacreate_map_headernames.R", DOIT = TRUE)

######################################### #
# datacreate_names_of_indicators.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_names_of_indicators.R")
source_maybe("datacreate_names_of_indicators.R")
### this will create but also assign metadata to and save for pkg via use_data() 
### It is a script that mostly uses a function so that
### all the variables created do not show up in the global environment - they get saved in pkg ready for lazy-loading if/when needed
### BUT any subsequent scripts that depend on those will not use the correct new versions unless we do load.all() anyway... 
### metadata is assigned inside this  
### use_data is done inside this  

######################################### #
# datacreate_names_pct_as_fraction.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_names_pct_as_fraction.R")
source_maybe("datacreate_names_pct_as_fraction.R")

######################################### #
# datacreate_metadata4pins.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_metadata4pins.R")
source_maybe("datacreate_metadata4pins.R") # does use_data()
# this just stores title, description of each dataset that gets put in pins board - no dates info
# dates info for pins is generated right when being written to pins board.

######################################### #
### Must use load_all() or build/install, to make available those new variable name lists 
#  and possibly modified  metadata4pins.rda
#  (the source package as just updated, not the version installed)
#  and so all functions will use the new source version 

load_all()

######################################### #
# datacreate_blockgroupstats2.3 ####
# datacreate_blockgroupstats2.3_add_d_acs22columns ####
# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.3.R")
# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R")
source_maybe("datacreate_blockgroupstats2.3.R")
# created bgej
# created blockgroupstats_new as interim object
# created usastats, statestats but not final versions yet
source_maybe("datacreate_blockgroupstats2.3_add_d_acs22columns.R")
# created blockgroupstats 

######################################### #
if (askquestions && interactive()) {
  pinej = askYesNo("write to bgej pins? ")
  if (!is.na(pinej) && pinej) {
    ## do not save via  usethis::use_data(bgej, overwrite = TRUE) - it is a large file
    ## Save bgej to pins board as .arrow file
    datawrite_to_pins("bgej", type = "arrow")
    # defaults should work but anyone doing this needs authentication, access to pins board !
  }}

######################################### #
# datacreate_usastats2.3_add_dsubgroups.R ####
# datacreate_usastats2.3_add_dsubgroups.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.3.R")
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.3_add_dsubgroups.R")
source_maybe("datacreate_usastats2.3.R")
# now usastats and statestats exist
source_maybe("datacreate_usastats2.3_add_dsubgroups.R")

######################################### #
# datacreate_avg.in.us.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_avg.in.us.R")
source_maybe("datacreate_avg.in.us.R")

######################################### #
# datacreate_high_pctiles_tied_with_min.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_high_pctiles_tied_with_min.R")
source_maybe("datacreate_high_pctiles_tied_with_min.R")

######################################### #
# datacreate_formulas.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_formulas.R")
source_maybe("datacreate_formulas.R")

# may want to rebuild/ reinstall the package here, or at least load_all()  ?

load_all()

######################################### #
# datacreate_testpoints_testoutputs.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_testpoints_testoutputs.R")
source_maybe("datacreate_testpoints_testoutputs.R")

######################################### #
# datacreate_stateinfo.R ####
# datacreate_stateinfo2.R ####
# documentOpen('./data-raw/datacreate_stateinfo.R')          # ok (missing Island Areas)
# documentOpen('./data-raw/datacreate_stateinfo2.R')         # ok (has Island Areas)
## ok to update metadata whenever - these should never really change but want to note version 2.3 etc.
source_maybe('datacreate_stateinfo.R')
source_maybe('datacreate_stateinfo2.R')

######################################### #
# datacreate_ejampackages.R ####
source_maybe('datacreate_ejampackages.R')

######################################### #
# datacreate_blockwts.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_blockwts.R")
source_maybe('datacreate_blockwts.R') # script that includes metadata_add() and use_data()
# Creates blockwts, blockpoints, etc.,
# but does not have to get updated 
# except when FIPS codes or boundaries change for blocks or blockgroups.
#  or possibly to add data that had been missing, for Island Areas AS, GU, MP, VI ***

######################################### #
# datacreate_bg_cenpop2020.R ####
# datacreate_bgpts.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_bg_cenpop2020.R")
# rstudioapi::documentOpen("./data-raw/datacreate_bgpts.R")

source_maybe("datacreate_bgpts.R", DOIT = FALSE, folder = rawdir)
source_maybe("datacreate_bg_cenpop2020.R", DOIT = FALSE, folder = rawdir)

######################################### #

# datawrite_to_pins() ####

# varnames = c(
#   'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
#   'bgej', 'bgid2fips',
#   'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
# for (i in seq_along(varnames)) {
#   
datawrite_to_pins() # it will ask interactively to confirm which ones among defaults to save to pins




# cleanup- remove most objects but not blockgroupstats and some others ####
rmost()
########################################## ######################################### # 
