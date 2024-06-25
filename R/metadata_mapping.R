default_metadata <- list(
  ejscreen_version      = description_file$get("EJScreenVersion"),
  ejscreen_releasedate  = description_file$get("EJScreenReleaseDate"),
  acs_releasedate       = description_file$get("ACSReleaseDate"),
  acs_version           = description_file$get("ACSVersion"),
  census_version        = description_file$get("CensusVersion")
)

metadata_mapping <- list(
  # datacreate_bg_cenpop2020.R
  bg_cenpop2020 =	list(
    source = "https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt", 
    census_version = description_file$get("CensusVersion")
  ),
  
  # datacreate_blockgroupstats_renamingcols.R
  avg.in.us =	default_metadata,
  blockgroupstats =	default_metadata,
  statestats = default_metadata,
  usastats = default_metadata,
  high_pctiles_tied_with_min =	default_metadata,
  
  # datacreate_bgpts.R
  bgpts =	list(),
  
  # datacreate_map_headernames.R
  map_headernames = list(
    ejscreen_version      = description_file$get("EJScreenVersion"),
    ejscreen_releasedate  = description_file$get("EJScreenReleaseDate")
  ),
  
  # datacreate_formulas.R
  formulas_all =	default_metadata,
  formulas_d =	default_metadata,
  
  # datacreate_frs_by_mact.R
  mact_table= list(),
  frs_by_mact =	list(),
  epa_programs = list(),
  
  # datacreate_usastats_pctile_lookup_add_subgroups_demog.R
  statestats2 =	default_metadata,
  
  # datacreate_censusplaces.R
  censusplaces = list(),
  
  # datacreate_stateinfo.R
  # includes stateinfo2
  stateinfo =	default_metadata,
  
  # datacreate_testpoints_testoutputs.R
  # includes all testpoint datasets
  testpoints =	default_metadata,
  
  # datacreate_test_address.R
  # This one seems out-of-date. It's referencing a dataframe, x, that doesn't exist
  
  # datacreate_testids_program_sys_id.R
  testids_program_sys_id = list(),
  
  # datacreate_testids_registry_id.R
  testids_registry_id = list()
) 

get_metadata_mapping <- function(dsname) {
  return(metadata_mapping[[dsname]])
}