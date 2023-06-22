#################################################################################################### #
# create ejscreen datasets for EJAM etc ####

## FULL SET OF SCRIPTS  to create EJScreen-related datasets, including blockgroupstats.rda for EJAM  

# 0.   EJAM/inst/notes_datasets/0_SCRIPT_overview_get_ejscreendata.R
#       - this list of steps
# 1.   EJAM/inst/notes_datasets/1_SCRIPT_EJAMejscreen_download.R  
#       - to add metadata also see  metadata_add() 
# 2.   EJAM/inst/notes_datasets/2_SCRIPT_FOR_FIPS_ST_TRACT_CNTY.R    
#       - to rename cols and add some fips fields and fix countyname col
# 3.   EJAM/inst/notes_datasets/3_SCRIPT_create_bgDemog_ejscreen2.1_andtracts.R 
#       - to get demog race ethnicity subgroups 
# 4.   EJAM/inst/notes_datasets/4_SCRIPT_ADD_PUERTORICO_DEMOG_SUBGROUPS.R 
#       - to download the PR demog subgroup part
# 5.   EJAM/inst/notes_datasets/5_SCRIPT_merge_demogsubgroups_v2.1.R  
#       - to MERGE SUBGROUPS info TO EJScreen  
# 6.   EJAM/inst/notes_datasets/6_SCRIPT_create_blockgroupstats.R 
#       - to simplify and save as data.table for EJAM::blockgroupstats

#################################################################################################### #
######################################################################## #
# # notes on converting 
# EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI  etc.
# and
# ejscreen::bg22DemographicSubgroups2016to2020  (might not save this separately)
# to
# EJAMejscreendata package file called EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI   etc.
# and 
# EJAM::blockgroupstats
# and
# ejscreen::bg22plus
######################################################################## #


# Redone 9/2022 to start from EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI
# and EJAMejscreendata package file called EJSCREEN_StatePct_with_AS_CNMI_GU_VI
# plus demog race/ethnic subgroups


# Being redone 1/23/23 to retain FIPS-related columns like ST, etc. 
#  and want to pull in new Supplemental indicators, life expectancy, Supplemental EJ index, etc.
#  and want to ensure Island Areas are included, 
#  and create lookup tables for demog race/ethnic subgroups, 
#   and see if those are avail for PR at least.


library(EJAMejscreendata) 
# EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI
# EJAMejscreendata package file called EJSCREEN_StatePct_with_AS_CNMI_GU_VI

stop('need bg22 from ejscreen pkg')
create_blockgroupstats <- function(bg = bg22 , meta) {
  
  ## script to create blockgroupstats.rda for EJAM
  
  if (missing(meta)) {
    meta   <- list(
      census_version = 2020,
      acs_version = '2016-2020',
      acs_releasedate = '3/17/2022',
      ejscreen_version = '2.1',
      ejscreen_releasedate = 'October 2022',
      ejscreen_pkg_data = 'bg22'
    )
  }
  #later: attributes(blockgroupstats) <- c(attributes(blockgroupstats), meta)
  
  b2 <- bg #  # work with it as a data.frame not data.table until a later step
  # b2 <- bg22plus
  
  # drop the bin number for percentiles, which just tells what decile except 10 is 90-95th pctile and bin 11 is 95-100, like ejscreen orange and red map colors in choropleths 
  dropping <- grep('^bin\\.', names(b2), value = TRUE )
  # drop the text labels describing the percentile values as in popups
  dropping <- c(dropping, grep('^pctile\\.text', names(b2), value = TRUE ) ) # certainly do not need pctile.text... cols
  dropping <- c(dropping, grep('^pctile\\.',     names(b2), value = TRUE)) # probably do not need these - pctiles for buffer scores are looked up, not calculated as popwtd means, right?
  # dropping <- c(dropping, "VNI.eo", "VDI.eo") #obsolete, they were basis for alt1 and alt2 EJ Indexes. VNI.eo is just mean of mins and lowinc counts. VSI.eo is mean of pctlowinc and pctmin, simple avg of those 2, treating as if denominator is pop for both.
  
  # dropping <- c(dropping, "FIPS.TRACT", "FIPS.COUNTY", 'countyname', "FIPS.ST", "ST", "statename", "REGION") 
  # none stay useful if just using blockgroupstats for buffer summary since buffer can span multiple states, etc.
  # but they are useful for creating state percentile lookups for new indicators, or other uses, so keep them.
  
  dropping <- c(dropping, "AREALAND", "AREAWATER", 'area') # could be analyzed as a count variable to get total area, but if circular buffer pi * radius^2 is easier
  dropping <- c(dropping,  "OBJECTID") # maybe keep ??
  dropping <- c(dropping, 'Shape_Length') # to avoid issue when merge since both files have these cols
  dropping <- c(dropping,  c('lat', 'lon')) #  this was just lat lon of a point in the blockgroup.
  dropping <- c(dropping,  c('flagged')) # if any EJ index is at 80+ pctile US for this bg?
  
  b2 <- b2[ , !names(b2) %in% dropping]
  
  names(b2) <- gsub('FIPS$', 'bgfips', names(b2))
  names(b2) <- gsub('NPL_CNT', 'count.NPL', names(b2))
  names(b2) <- gsub('TSDF_CNT', 'count.TSDF', names(b2))
  
  #   bgfips unique ID in order by order of bgfips sort,  
  # bgid is for join to blockwts$bgid that uses bgid instead of bgfips, to save space and RAM.
   
  blockgroupstats <- b2
  data.table::setDT(blockgroupstats, key = 'bgid') # by reference only 
  rm(b2); rm(subgroups);rm(dropping)
  

  attributes(blockgroupstats) <- c(attributes(blockgroupstats), meta)
  
  
  print('You can now add it to the package with this:   usethis::use_data(blockgroupstats)')
  invisible(blockgroupstats)
  
  # blockgroupstats uses 116 MB according to tables()
  # without the race eth subgroups it was just 87 MB !
  # save(blockgroupstats, file = './data/blockgroupstats.rda')
  # or try it this way:
  #  usethis::use_data(blockgroupstats)
  
  
  # c("bgfips", "FIPS", "OBJECTID", "povknownratio", "age25up", "hhlds", 
  #   "builtunits", "lowinc", "pctlowinc", "lths", "pctlths", "lingiso", 
  #   "pctlingiso", "under5", "pctunder5", "over64", "pctover64", "pre1960", 
  #   "pctpre1960", "VSI.eo", "VNI.eo", "VDI.eo", "dpm", "cancer", 
  #   "resp", "traffic.score", "proximity.npdes", "proximity.npl", 
  #   "proximity.rmp", "proximity.tsdf", "o3", "pm", "EJ.DISPARITY.pctpre1960.eo", 
  #   "EJ.DISPARITY.dpm.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", 
  #   "EJ.DISPARITY.traffic.score.eo", "EJ.DISPARITY.proximity.npdes.eo", 
  #   "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", 
  #   "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.pm.eo", 
  #   "statename", "ST", "REGION", "pctile.pctmin", "pctile.pctlowinc", 
  #   "pctile.pctlths", "pctile.pctlingiso", "pctile.pctunder5", "pctile.pctover64", 
  #   "pctile.pctpre1960", "pctile.VSI.eo", "pctile.dpm", "pctile.cancer", 
  #   "pctile.resp", "pctile.traffic.score", "pctile.proximity.npdes", 
  #   "pctile.proximity.npl", "pctile.proximity.rmp", "pctile.proximity.tsdf", 
  #   "pctile.o3", "pctile.pm", "pctile.EJ.DISPARITY.pctpre1960.eo", 
  #   "pctile.EJ.DISPARITY.dpm.eo", "pctile.EJ.DISPARITY.cancer.eo", 
  #   "pctile.EJ.DISPARITY.resp.eo", "pctile.EJ.DISPARITY.traffic.score.eo", 
  #   "pctile.EJ.DISPARITY.proximity.npdes.eo", "pctile.EJ.DISPARITY.proximity.npl.eo", 
  #   "pctile.EJ.DISPARITY.proximity.rmp.eo", "pctile.EJ.DISPARITY.proximity.tsdf.eo", 
  #   "pctile.EJ.DISPARITY.o3.eo", "pctile.EJ.DISPARITY.pm.eo", "arealand", 
  #   "areawater", "count.NPL", "count.TSDF", "area", "pop",
  # "hisp",  "nhba", "nhaiana", "nhaa", "nhnhpia", "nhotheralone",   "nhmulti",     "nhwa",
  # "pctmin", "mins", "nonmins",
  # "pcthisp", "pctnhba", "pctnhaiana", "pctnhaa", "pctnhnhpia", "pctnhotheralone", "pctnhmulti",   "pctnhwa")
}
