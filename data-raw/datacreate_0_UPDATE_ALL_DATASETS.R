############################################################### #

## Scripts to update / create latest versions of datasets 
# ANNUAL blockgroup data from ACS and EJScreen
# NON-ANNUAL (frequent, episodic, etc.) other datasets
# see EJAM's github issues about this

# SETUP ####

rm(list = ls())
#localfolder <-  "~/../Downloads/ejscreen new ftp downloads"
localfolder <- "~/../Downloads/EJAMbigfiles"
if (interactive()) {localfolder <- rstudioapi::selectDirectory("Confirm where to archive .arrow and other files locally", path = localfolder) }
if (!dir.exists(localfolder)) {stop(paste0("need valid localfolder - ", localfolder, " was not found"))}
if (!exists("td")) {td <- tempdir() }
if (!exists("rawdir")) {rawdir <- './data-raw'}
if (!dir.exists(rawdir)) {stop("need to do this from source package folder, from where it can find a folder at ", rawdir)}
if (!exists("askquestions")) {askquestions <- FALSE}
if (interactive()) {
  askquestions <- askYesNo("Do you want to answer questions interactively like this about what to save where, etc.? (vs running all scripts without pauses)")
  if (is.na("askquestions")) {askquestions <- FALSE}
}
######################################### #
source_maybe <- function(scriptname = NULL,
                         DOIT = TRUE, 
                         question = paste0("Do ", scriptname, "?"),
                         folder = NULL) {
  if (is.null(scriptname)) {stop("requires scriptname")}
  if (!exists('askquestions')) {
    askquestions <- TRUE
  }
  if (exists("rawdir") && missing(folder)) {
    folder <- rawdir
  } else {
    if (missing(folder)) {
      folder <-  "./data-raw"
    }  
  }
  if (askquestions && interactive()) {
    DOIT <- utils::askYesNo(question)
    if (!is.na(DOIT) && DOIT) {DOIT <- TRUE}
  }
  if (DOIT) {
    cat(paste0("Doing ", scriptname, " \n"))
    spath = file.path(folder, scriptname)
    if (!file.exists(spath)) {stop(paste0("requires valid folder and scriptname. Tried: ", spath))}
    source(spath)
  } else {
    cat("Skipping ", scriptname, "\n")
  }
}
######################################### #

golem::detach_all_attached()

require(devtools)
require(rstudioapi)

load_all()

## load_all() ####
# Get latest source functions and data: 
# from  EJAM/R/*.R and EJAM/data/*.rda 
# Attaches exported + internal functions & data 
# like metadata_add(), newly saved .rda files, etc.
#  Otherwise internal functions don't work in scripts, and it would use installed not new source versions.
######################################### ########################################## #
#
## List of datacreate_ files ####
## & when to use each 
# fnames <- dir(rawdir, pattern = 'datacreate_')
# cat("\n \n\n", "To open & edit one of the datacreate_ files,
#     you can source a line below\n\n",
#     paste0(paste0(
#       "\t documentOpen('", rawdir, "/", fnames, "')"), collapse = "\n"))
if (0 == 1) {  # collapsable list
          ####   THESE ARE SORTED INTO GROUPS THAT GO TOGETHER : 
  x <- c("datacreate_0_UPDATE_ALL_DATASETS.R", "datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R",
    "datacreate_map_headernames.R", "datacreate_names_of_indicators.R", "datacreate_names_pct_as_fraction.R", "datacreate_metadata4pins.R", "datacreate_blockgroupstats2.3.R", "datacreate_blockgroupstats2.3_add_d_acs22columns.R", "datacreate_usastats2.3.R", "datacreate_usastats2.3_add_dsubgroups.R", "datacreate_avg.in.us.R", "datacreate_high_pctiles_tied_with_min.R", "datacreate_formulas.R", "datacreate_test_address.R", "datacreate_testpoints_testoutputs.R", 
    "datacreate_default_points_shown_at_startup.R", "datacreate_testpoints_5_50_500.R", "datacreate_ejscreenRESTbroker2table_na_filler.R", "datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R",
    "datacreate_blockwts.R", "datacreate_bg_cenpop2020.R", "datacreate_bgpts.R", "datacreate_states_shapefile.R", "datacreate_stateinfo.R", "datacreate_stateinfo2.R", "datacreate_islandareas.R", "datacreate_censusplaces.R", 
    "datacreate_frs_.R", "datacreate_frs_by_mact.R", "datacreate_frs_by_sic.R", "datacreate_frsprogramcodes.R", "datacreate_epa_programs.R", "datacreate_testids_program_sys_id.R", "datacreate_testids_registry_id.R", "datacreate_naics_counts.R", "datacreate_naicstable.R", "datacreate_SIC.R", "datacreate_sic_counts.R", "datacreate_sictable.R", 
    "datacreate_lat_alias.R", "datacreate_ejampackages.R", "datacreate_meters_per_mile.R"
    )
  setdiff(x, dir(rawdir, pattern = 'datacreate_') )   # confirm the organized list x is completely reflecting current actual files
  setdiff( dir(rawdir, pattern = 'datacreate_'), x )
  cat("\n \n\n", "To open & edit one of the datacreate_ files,
    you can source a line below\n\n",
      paste0(paste0(
        "\t documentOpen('", rawdir, "/", x, "')"), collapse = "\n"))
  # cbind(x)
  rm(x)
  # [1,] "datacreate_0_UPDATE_ALL_DATASETS.R"                       
  # [2,] "datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R"  
  
  # [3,] "datacreate_map_headernames.R"                             
  # [4,] "datacreate_names_of_indicators.R"                         
  # [5,] "datacreate_names_pct_as_fraction.R"                       
  # [6,] "datacreate_metadata4pins.R"                               
  # [7,] "datacreate_blockgroupstats2.3.R"                          
  # [8,] "datacreate_blockgroupstats2.3_add_d_acs22columns.R"       
  # [9,] "datacreate_usastats2.3.R"                                 
  # [10,] "datacreate_usastats2.3_add_dsubgroups.R"                  
  # [11,] "datacreate_avg.in.us.R"                                   
  # [12,] "datacreate_high_pctiles_tied_with_min.R"                  
  # [13,] "datacreate_formulas.R"                                    
  # [14,] "datacreate_test_address.R"                    
  # [15,] "datacreate_testpoints_testoutputs.R"                      
  
  # [16,] "datacreate_default_points_shown_at_startup.R"      
  # [17,] "datacreate_testpoints_5_50_500.R"             
  # [18,] "datacreate_ejscreenRESTbroker2table_na_filler.R"          
  # [19,] "datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R"
  
  # [20,] "datacreate_blockwts.R"                                    
  # [21,] "datacreate_bg_cenpop2020.R"                               
  # [22,] "datacreate_bgpts.R"                                       
  # [23,] "datacreate_states_shapefile.R"                            
  # [24,] "datacreate_stateinfo.R"                                   
  # [25,] "datacreate_stateinfo2.R"                                  
  # [26,] "datacreate_islandareas.R"                                 
  # [27,] "datacreate_censusplaces.R"          
  
  # [28,] "datacreate_frs_.R"                                        
  # [29,] "datacreate_frs_by_mact.R"                                 
  # [30,] "datacreate_frs_by_sic.R"                                  
  # [31,] "datacreate_frsprogramcodes.R"                             
  # [32,] "datacreate_epa_programs.R"                                
  # [33,] "datacreate_testids_program_sys_id.R"                      
  # [34,] "datacreate_testids_registry_id.R"                         
  # [35,] "datacreate_naics_counts.R"                                
  # [36,] "datacreate_naicstable.R"                                  
  # [37,] "datacreate_SIC.R"                                         
  # [38,] "datacreate_sic_counts.R"                                  
  # [39,] "datacreate_sictable.R"             
  
  # [40,] "datacreate_lat_alias.R"                                   
  # [41,] "datacreate_ejampackages.R"                                
  # [42,] "datacreate_meters_per_mile.R"                                                      

  
####################################### # 
  # overall
  documentOpen('./data-raw/datacreate_0_UPDATE_ALL_DATASETS.R')
  documentOpen('./data-raw/datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R')

    # with annual ejscreen data updates
  # 
  ##  new indicators, variable names
  documentOpen('./data-raw/datacreate_map_headernames.R')       # ok
  documentOpen('./data-raw/datacreate_names_of_indicators.R')   # ok
  documentOpen('./data-raw/datacreate_names_pct_as_fraction.R') # ok
  ##  ejscreen demog and envt data on every blockgroup
  ##  + pctile and avg lookup tables
  documentOpen("./data-raw/datacreate_metadata4pins.R") # ok
  documentOpen('./data-raw/datacreate_blockgroupstats2.3.R') # and bgej      # ok
  documentOpen('./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R')   # ok

  documentOpen('./data-raw/datacreate_usastats2.3.R')                 # ok
  documentOpen('./data-raw/datacreate_usastats2.3_add_dsubgroups.R')  # ok
  documentOpen('./data-raw/datacreate_avg.in.us.R')                   # ok
  documentOpen('./data-raw/datacreate_high_pctiles_tied_with_min.R')  # ok
  ##  calculations and examples of outputs
  documentOpen('./data-raw/datacreate_formulas.R')                    # was in progress; maybe not used yet
  documentOpen('./data-raw/datacreate_test_address.R')       # ok
  documentOpen('./data-raw/datacreate_testpoints_testoutputs.R')      # confirm new datasets/functions/indicators work here
  # from the original  EJAMejscreenapi  package
  documentOpen('./data-raw/datacreate_default_points_shown_at_startup.R')            
  documentOpen('./data-raw/datacreate_testpoints_5_50_500.R')            
  documentOpen('./data-raw/datacreate_ejscreenRESTbroker2table_na_filler.R')         
  documentOpen('./data-raw/datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R')
  
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
  documentOpen('./data-raw/datacreate_islandareas.R')        # ok
  documentOpen('./data-raw/datacreate_censusplaces.R')       # not used yet
  
  # when frs info is updated
  
  documentOpen('./data-raw/datacreate_frs_.R')            #  BUT SEE IF THIS HAS BEEN REVISED/ REPLACED  ***
  documentOpen('./data-raw/datacreate_frs_by_mact.R')     #  BUT SEE IF THIS HAS BEEN REPLACED  ***
  documentOpen('./data-raw/datacreate_frs_by_sic.R')      #  BUT SEE IF THIS HAS BEEN REPLACED  ***
  
  documentOpen('./data-raw/datacreate_frsprogramcodes.R') #
  documentOpen('./data-raw/datacreate_epa_programs.R')    #
  documentOpen('./data-raw/datacreate_testids_program_sys_id.R')  # 
  documentOpen('./data-raw/datacreate_testids_registry_id.R')     #
  # NAICS/SIC
  documentOpen('./data-raw/datacreate_naics_counts.R')    # script
  documentOpen('./data-raw/datacreate_naicstable.R')      # script. does date_saved_in_package & use_data
  documentOpen('./data-raw/datacreate_SIC.R')             
  documentOpen('./data-raw/datacreate_sic_counts.R')      
  documentOpen('./data-raw/datacreate_sictable.R')        
  
  # misc
  documentOpen('./data-raw/datacreate_lat_alias.R')
  documentOpen('./data-raw/datacreate_ejampackages.R')
  documentOpen('./data-raw/datacreate_meters_per_mile.R')
  
  ### and then datawrite_to_pins() if those datasets were updated. 
  
} # outline/list of datacreate_ files
######################################### ########################################## #
## metadata notes ####
#
## use simple metadata for data not related to EJScreen or Census, like just frs-related, naics-related, etc.
# attr(x, "date_downloaded")       <- as.character(Sys.Date()) # if relevant
# attr(x, "date_saved_in_package") <- as.character(Sys.Date())

## use full metadata if related to ejscreen or census/acs
# x <- metadata_add(x)
######################################### #
## Verify pins board access ####
x <- datawrite_to_pins(justchecking = T) # load_all() first or use EJAM:::
if (!is.null(x)) {
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
  pin_seen <- x$name
  pin_expected = c(
    'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
    'bgej', 'bgid2fips',
    'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"
  )
  if (length(setdiff2(pin_seen, pin_expected)) > 0 ) {
    message("Expected to see on pin board but not there: ", paste0(setdiff(pin_expected, pin_seen), collapse = ", "))
    message("Seeon on pin board but not expected: ", paste0(setdiff(pin_seen, pin_expected), collapse = ", "))
  }
  rm(pin_seen, pin_expected, x)
}
######################################### ########################################## #
######################################### ########################################## #
# ~------------------------------------------- ####
# *** ANNUAL UPDATES OF EJScreen ####

## Indicator/variable names etc. ####
######################################### #
### datacreate_map_headernames.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_map_headernames.R")
source_maybe("datacreate_map_headernames.R", DOIT = TRUE)
######################################### #
### datacreate_names_of_indicators.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_names_of_indicators.R")
source_maybe("datacreate_names_of_indicators.R")
### this will create but also assign metadata to and save for pkg via use_data() 
### It is a script that mostly uses a function so that
### all the variables created do not show up in the global environment - they get saved in pkg ready for lazy-loading if/when needed
### BUT any subsequent scripts that depend on those will not use the correct new versions unless we do load.all() anyway... 
### metadata is assigned inside this  
### use_data is done inside this  
######################################### #
### datacreate_names_pct_as_fraction.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_names_pct_as_fraction.R")
source_maybe("datacreate_names_pct_as_fraction.R")
######################################### #
### datacreate_metadata4pins.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_metadata4pins.R")
source_maybe("datacreate_metadata4pins.R") # does use_data()
# this just stores title, description of each dataset that gets put in pins board - no dates info
# dates info for pins is generated right when being written to pins board.
######################################### #
### Must use load_all() or build/install, to make available those new variable name lists 
#  and possibly modified  metadata4pins.rda
#  (the source package as just updated, not the version installed)
#  and so all functions will use the new source version 

rmost(notremove = c("askquestions", "localfolder", "td", "rawdir", "source_maybe"))

devtools::load_all()

## Demog + Envt data on blockgroups ####
## + pctile & avg lookup tables (usastats, statestats) ####

######################################### #
### datacreate_blockgroupstats2.3 (also starts making usastats,statestats!!) ####
### ACS22 via datacreate_blockgroupstats2.3_add_d_acs22columns ####
# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.3.R")
source_maybe("datacreate_blockgroupstats2.3.R") # (also starts making usastats,statestats!!)
# created bgej
# created blockgroupstats_new as interim object
# created usastats, statestats but not final versions yet

# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R")  # reads ACS22 extra file of demographics not on ftp site
source_maybe("datacreate_blockgroupstats2.3_add_d_acs22columns.R")  # reads ACS22 extra file of demographics not on ftp site
# created blockgroupstats (now with demog subgroups from ACS22 extra file of demographics not on ftp site)

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
### datacreate_usastats2.3.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.3.R")
source_maybe("datacreate_usastats2.3.R")
# now usastats and statestats exist
######################################### #
### datacreate_usastats2.3_add_dsubgroups.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.3_add_dsubgroups.R")
source_maybe("datacreate_usastats2.3_add_dsubgroups.R")
######################################### #
### datacreate_avg.in.us.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_avg.in.us.R")
source_maybe("datacreate_avg.in.us.R")
######################################### #
### datacreate_high_pctiles_tied_with_min.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_high_pctiles_tied_with_min.R")
source_maybe("datacreate_high_pctiles_tied_with_min.R")
######################################### #
### datacreate_formulas.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_formulas.R")
source_maybe("datacreate_formulas.R")
######################################### #
 
## Test data & examples of outputs ####
######################################### #
### datacreate_test_address.R #### 
# rstudioapi::documentOpen('./data-raw/datacreate_test_address.R')  
source_maybe("datacreate_test_address.R")


############################### pause here


# save.image(file.path(localfolder, "work in progress.rda"))


# may want to rebuild/ reinstall the package here,
# or at least load_all()  ?


devtools::install(quick = TRUE)


# devtools::check() 


# devtools::test()


######################################### #
### datacreate_testpoints_testoutputs.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_testpoints_testoutputs.R")
source_maybe("datacreate_testpoints_testoutputs.R")

# ~------------------------------------------- ####
## Formerly in EJAMejscreenapi package ####
######################################### #

### datacreate_default_points_shown_at_startup.R ####
source_maybe('datacreate_default_points_shown_at_startup.R')
### datacreate_testpoints_5_50_500.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_testpoints_5_50_500.R")
source_maybe('datacreate_testpoints_5_50_500.R')

### datacreate_ejscreenRESTbroker2table_na_filler.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_ejscreenRESTbroker2table_na_filler.R")
source_maybe('datacreate_ejscreenRESTbroker2table_na_filler.R')
### datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R  ####
# rstudioapi::documentOpen("./data-raw/datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R")
source_maybe('datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R')

######################################### ########################################## #



load_all()
# 
# install()


######################################### #
######################################### #
# ~------------------------------------------- ####
# *** FIPS CODE UPDATES or Census Boundary changes (once EJScreen incorporates those) ####

######################################### #
## blocks  ####
# documentOpen('./data-raw/datacreate_blockwts.R')           # needs Island Areas added

######################################### #
### datacreate_blockwts.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_blockwts.R")
source_maybe('datacreate_blockwts.R', DOIT = FALSE) # script that includes metadata_add() and use_data()
# Creates blockwts, blockpoints, etc.,
# but does not have to get updated 
# except when FIPS codes or boundaries change for blocks or blockgroups.
#  or possibly to add data that had been missing, for Island Areas AS, GU, MP, VI ***
######################################### #
## blockgroups ####
# documentOpen('./data-raw/datacreate_bg_cenpop2020.R')      # confirm if changed since 2020
# documentOpen('./data-raw/datacreate_bgpts.R')              # redundant w bg_cenpop2020, pick one to use

### datacreate_bg_cenpop2020.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_bg_cenpop2020.R")
source_maybe("datacreate_bg_cenpop2020.R", DOIT = FALSE, folder = rawdir)
######################################### #
### datacreate_bgpts.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_bgpts.R")
source_maybe("datacreate_bgpts.R", DOIT = FALSE, folder = rawdir)
######################################### #
## states ####
# documentOpen('./data-raw/datacreate_states_shapefile.R')   # check if want 2020 or 2022+ file
# documentOpen('./data-raw/datacreate_stateinfo.R')          # ok (missing Island Areas)
# documentOpen('./data-raw/datacreate_stateinfo2.R')         # ok (has Island Areas)

### datacreate_states_shapefile.R ####
# documentOpen('./data-raw/datacreate_states_shapefile.R')   # check if want 2020 or 2022+ file
source_maybe("datacreate_states_shapefile.R", DOIT = FALSE, folder = rawdir)
######################################### #
### datacreate_stateinfo.R ####
### datacreate_stateinfo2.R ####
# documentOpen('./data-raw/datacreate_stateinfo.R')          # ok (missing Island Areas)
# documentOpen('./data-raw/datacreate_stateinfo2.R')         # ok (has Island Areas)
## ok to update metadata whenever - these should never really change but want to note version 2.3 etc.
source_maybe('datacreate_stateinfo.R', DOIT = FALSE, folder = rawdir)
source_maybe('datacreate_stateinfo2.R', DOIT = FALSE, folder = rawdir)
######################################### #

## other geo ####
# documentOpen('./data-raw/datacreate_islandareas.R')        # ok
# documentOpen('./data-raw/datacreate_censusplaces.R')       # not used yet

### datacreate_islandareas.R ####
# documentOpen('./data-raw/datacreate_islandareas.R')        # ok
source_maybe("datacreate_islandareas.R", DOIT = FALSE, folder = rawdir)
######################################### #
### datacreate_censusplaces.R ####
# documentOpen('./data-raw/datacreate_censusplaces.R')       # not used yet
source_maybe("datacreate_censusplaces.R", DOIT = FALSE, folder = rawdir)


######################################### ########################################## #
# ~------------------------------------------- ####
# *** FRS UPDATES (incl. NAICS/SIC) ####

########################################## #

# FRS ####


## >>> frs functions need cleanup here <<< ####

cat(                                        "frs functions need cleanup here  \n")
warning("frs functions need cleanup here")


## frs_by_ (lat,lon, regid,program,mact) ####

### ? datacreate_frs_.R ####
# rstudioapi::documentOpen('./data-raw/datacreate_frs_.R')            #  BUT SEE IF THIS HAS BEEN REVISED/ REPLACED  ***
# THAT SCRIPT USES frs_update_datasets() to download data, create datasets for pkg, 
# and save them locally, and read them into memory.
# That creates frs, frs_by_programid, frs_by_naics, frs_by_sic, frs_by_mact

source_maybe("datacreate_frs_.R", DOIT = FALSE, folder = rawdir)


### ? datacreate_frs_by_sic.R - is it redundant with frs_update_datasets() ?  SEE IF THIS HAS BEEN REPLACED ? ####
# documentOpen('./data-raw/datacreate_frs_by_sic.R')      #

### ? datacreate_frs_by_mact.R - is it redundant with frs_update_datasets() ?  SEE IF THIS HAS BEEN REPLACED ? ####
# documentOpen('./data-raw/datacreate_frs_by_mact.R')   #  BUT SEE IF THIS HAS BEEN REPLACED  ***


### datacreate_frsprogramcodes.R ####
# documentOpen('./data-raw/datacreate_frsprogramcodes.R') #
source_maybe('datacreate_frsprogramcodes.R')

### datacreate_epa_programs.R ####
# documentOpen('./data-raw/datacreate_epa_programs.R')    #
source_maybe('datacreate_epa_programs.R')

### datacreate_testids_program_sys_id.R ####
# documentOpen('./data-raw/datacreate_testids_program_sys_id.R')  # 
source_maybe('datacreate_testids_program_sys_id.R')

### datacreate_testids_registry_id.R ####
# documentOpen('./data-raw/datacreate_testids_registry_id.R')     #
source_maybe('datacreate_testids_registry_id.R')
########################################## #

# NAICS/SIC ####


## >>>             ADD SCRIPTS HERE? <<< ####

cat(                                        "naics functions not here yet? \n")
warning("naics functions not here yet")


### datacreate_naics_counts.R ####
# documentOpen('./data-raw/datacreate_naics_counts.R')    # script
source_maybe('datacreate_naics_counts.R')
### datacreate_naicstable.R ####
# documentOpen('./data-raw/datacreate_naicstable.R')      # script. does date_saved_in_package & use_data
source_maybe('datacreate_naicstable.R')

### datacreate_SIC.R ####
# documentOpen('./data-raw/datacreate_SIC.R')
source_maybe('datacreate_SIC.R')
### datacreate_sic_counts.R ####
# documentOpen('./data-raw/datacreate_sic_counts.R')
source_maybe('datacreate_sic_counts.R')
### datacreate_sictable.R ####
# documentOpen('./data-raw/datacreate_sictable.R')
source_maybe('datacreate_sictable.R')
######################################### ########################################## #

# misc ####
# probably do not need to update these often or ever, but ok to do so
######################################### #
### datacreate_lat_alias.R ####
source_maybe('datacreate_lat_alias.R')
######################################### #
### datacreate_ejampackages.R ####
source_maybe('datacreate_ejampackages.R')
######################################### #
### datacreate_meters_per_mile.R ####
# documentOpen('./data-raw/datacreate_meters_per_mile.R')
source_maybe("datacreate_meters_per_mile.R")
######################################### # 

######################################### #
######################################### #
# ~------------------------------------------- ####
# *** PINNED DATA UPDATES ####
# For any of the datasets stored on the pins board server,
# Upload the new versions of those (large) data objects whenever they get updated. 

## datawrite_to_pins() ####

################## #
# pindates() & pinned() helper functions were NOT WORKING YET - date format is messed up

pindates  <- function(varnames =  c(
  'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
  'bgej', 'bgid2fips',
  'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"
)) {
  junk <- capture.output({
    x <- dataload_from_pins(justchecking = TRUE, silent = TRUE, 
                            varnames = varnames)
  })
  x <- x[, c("name", "created", "ejscreen_version")]
  if (!missing(varnames))  {
    # only show info for the specific ones queried, not all pinned
    xshell <- data.frame(name = varnames, 
                         # still need to fix date  format... 
                         created = 0, ejscreen_version = NA)
    for (i in seq_along(varnames)) {
      if (varnames[i] %in% x$name) {
        xshell[i, ] <- x[x$name == varnames[i], ]
      }
    }
    x <- xshell
  }
  return(x) 
}
################## #   NOT WORKING YET 
# pinned <- function(varnames = c(
#   'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
#   'bgej', 'bgid2fips',
#   'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"
# )) {
#   x <- pindates(varnames)
#   x$created[is.na(x$created)] <- 0
#   x <- x[x$created != 0,]
#   y = (varnames %in% x$name)
#   names(y) <- varnames[varnames %in% x$name]
#   return(y)
# }
################## #
# data.frame(varname = varnames, 
#   pinned = pinned(varnames),
#   datepinned = pindates(varnames)$created,
#   attached = varnames %in% ls(), 
#   loaded_or_lazyloadable = sapply(varnames, exists), 
#   in_data_folder = varnames %in% tools::file_path_sans_ext(basename(dir("./data")) )
#   )
################## #

# Review the datasets / pins

cat("Which datasets are attached or can be lazyloaded in memory, of those you may want to pin?")
x = dataload_from_pins(justchecking = TRUE, silent = TRUE, 
                       varnames = c(
                         "blockwts", "blockpoints", "blockid2fips", "quaddata", # "localtree",
                         "bgej", "bgid2fips",
                         "frs", "frs_by_programid", "frs_by_naics", "frs_by_sic", "frs_by_mact"
                       )
)
cat("These datasets are currently seen on the pins board: \n")
x[,c("name", "created", "ejscreen_version")]

cat("These datasets get written to pins board by the function dataload_from_pins() \n")
print(formals(dataload_from_pins)$varnames)

cat("\n\n")
#   
datawrite_to_pins() # it will ask interactively to confirm which ones among defaults to save to pins

######################################### #
######################################### #
# ~------------------------------------------- ####
# ~ ####
# CLEANUP - Remove most objects ####

rmost(
  notremove = c(
    "blockwts", "blockpoints", "blockid2fips", "quaddata", "localtree",
    "bgej", "bgid2fips",
    "frs", "frs_by_programid", "frs_by_naics", "frs_by_sic", "frs_by_mact"
  )
)
cat("Running load_all() but you may want to rebuild/install now \n")
cat("You may want to source 'datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R' now \n")
devtools::load_all()

########################################## ######################################### # 
