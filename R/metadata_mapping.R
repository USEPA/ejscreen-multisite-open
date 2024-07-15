#################################################### # 

# default_metadata from DESCRIPTION file ####

description_file <- desc::description$new("DESCRIPTION")

default_metadata <- list(
  
  ### Should we add this? This field cant have text in DESCRIPTION file. "2.3.0" ok, but not "2.3.0-ejscreen2.3"
  # ejam_version          = description_file$get("Version"),
  
  ejscreen_version      = description_file$get("EJScreenVersion"),
  ejscreen_releasedate  = description_file$get("EJScreenReleaseDate"),
  acs_releasedate       = description_file$get("ACSReleaseDate"),
  acs_version           = description_file$get("ACSVersion"),
  census_version        = description_file$get("CensusVersion")
)
#################################################### # 

# metadata_mapping ####

metadata_mapping <- list(
  
  # datacreate_bg_cenpop2020.R
  bg_cenpop2020 =	list(
    source = "https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt", 
    census_version = description_file$get("CensusVersion")
  ),
  
  # datacreate_blockgroupstats2.3.R
  # rstudioapi::documentOpen("./R/datacreate_blockgroupstats2.3.R")
  blockgroupstats =	default_metadata,
  
  # datacreate_usastats2.3.R  and others
  avg.in.us       =	default_metadata,
  statestats      = default_metadata,
  usastats        = default_metadata,
  high_pctiles_tied_with_min =	default_metadata,
  
  # datacreate_bgpts.R
  bgpts =	list(),       ##########  ???
  
  # datacreate_map_headernames.R
  map_headernames = list(
    ejscreen_version      = description_file$get("EJScreenVersion"),
    ejscreen_releasedate  = description_file$get("EJScreenReleaseDate")
  ),
  
  # datacreate_formulas.R
  formulas_all = default_metadata,
  formulas_d   = default_metadata,
  
  # datacreate_frs_by_mact.R
  mact_table = list(),
  frs_by_mact =	list(),
  epa_programs = list(),
  
  # datacreate_usastats_pctile_lookup_add_subgroups_demog.R
  statestats2 =	default_metadata,
  
  # datacreate_censusplaces.R
  censusplaces = list(),       ##########  ???
  
  # datacreate_stateinfo.R
  # datacreate_stateinfo2.R
  #  This includes stateinfo2:
  stateinfo  =	default_metadata,
  
  # datacreate_testpoints_testoutputs.R
  # includes all testpoint datasets
  testpoints =	default_metadata,
  
  # datacreate_test_address.R
  test_address.R = list(),
  
  # datacreate_testids_program_sys_id.R
  testids_program_sys_id = list(),
  
  # datacreate_testids_registry_id.R
  testids_registry_id = list(),
  
  test_metadata_custom = list(
    custominfo = 0, 
    moreinfo = "oldvalue", 
    unchangedinfo = 9
  ),
  
  test_metadata_custom2 = list(
    custominfo = 123, 
    moreinfo = "abc"
  ),

  default = default_metadata
)
#################################################### # 

# get_metadata_mapping ####

get_metadata_mapping <- function(dsname) {
  return(metadata_mapping[[dsname]])
}
#################################################### # 
