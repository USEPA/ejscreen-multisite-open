
#################################################################################################### #
# create ejscreen datasets for EJAM etc ####

## FULL SET OF SCRIPTS  to create EJScreen-related datasets, including blockgroupstats.rda for EJAM  

# 0.   EJAM/inst/notes_datasets/0_SCRIPT_overview_get_ejscreendata.R
#       - this list of steps
# 1.   EJAM/inst/notes_datasets/1_SCRIPT_EJAMejscreen_download.R  
#       - to add metadata also see  EJAM::metadata_add()
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

#'     Files available from FTP site as of 1/2023:
#'     
#'   EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip
#'   
#'   EJSCREEN_2022_Full_with_AS_CNMI_GU_VI.csv.zip
#'   EJSCREEN_2022_StatePct_with_AS_CNMI_GU_VI.csv.zip
#'   2022_EJSCREEN_BG_Columns.xlsx
#'   
#'   EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip  
#'   EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv.zip
#'   2022_EJSCREEN_Supplemental_BG_Columns.xlsx
#'   

######################################################################## #
# # notes on converting 
# EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI  etc.
# and
# ejscreen package file called bg22DemographicSubgroups2016to2020  (might not save this separately)
# to
# EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI   etc.
# and 
# EJAM::blockgroupstats
# and
# ejscreen package file called bg22plus
######################################################################## #
