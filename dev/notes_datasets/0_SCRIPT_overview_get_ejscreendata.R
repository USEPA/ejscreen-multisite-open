#################################################################################################### #
# Creating EJScreen 2022 version 2.1 datasets for EJAM, etc. ####

# for info on creating the 2023 version 2.2 datasets, see other files (C:/Users/mcorrale/R/mysource/EJAMejscreenapi/data-raw/update_to_ejscreenv2.2.R)


######################################################################## #
# # These scripts/notes were about converting the 
# EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI  etc.
# and
# ejscreen package file called bg22DemographicSubgroups2016to2020  (might not save this separately)
# to
# EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI   etc.
# and then creating 
# EJAM::blockgroupstats
# and also creating 
# ejscreen package file called bg22plus
######################################################################## #

## FULL SET OF 2022 SCRIPTS  to create EJScreen-related datasets, 
#  including blockgroupstats.rda for EJAM  

# dir(path =    "~/../../R/mysource/EJAM/dev/notes_datasets/" , pattern = "SCRIPT_"  )
# [1] "0_SCRIPT_overview_get_ejscreendata.R"            # this list of steps   
# [2] "1_SCRIPT_EJAMejscreen_download.R"                # download/unzip; and to add metadata also see  EJAM::metadata_add()   
# [3] "2_SCRIPT_FOR_FIPS_ST_TRACT_CNTY.R"               # rename cols and add some fips fields and fix countyname col   
# [4] "3_SCRIPT_create_bgDemog_ejscreen2.1_andtracts.R" #x  get demog race ethnicity subgroups that 2022 version lacked but 2023 has already
# [5] "4_SCRIPT_ADD_PUERTORICO_DEMOG_SUBGROUPS.R"       #x  download the PR demog subgroup part that 2022 version lacked but 2023 has already
# [6] "5_SCRIPT_merge_demogsubgroups_v2.1.R"            #x  MERGE SUBGROUPS info TO EJScreen since 2022 version lacked that
# [7] "6_SCRIPT_create_blockgroupstats.R"               # simplify and save as data.table for EJAM::blockgroupstats
# [8] "8_SCRIPT_make_MeansByGroup_and_Ratios_RRS.US22.R"# ratios stuff
# [9] "9_SCRIPT_PCTILELOOKUPS_READ-CSVS-MID-2022.R"     # get lookup tables of US and State percentiles cutoffs (add subgropus if needed!)
#################################################################################################### #

#'  ######################################################################## #
#'  version 2.1 EJScreen files on FTP site in 2022 (and used until 6/2023):
#'   
#'  At  https://gaftp.epa.gov/EJScreen/2022/  
#'  2022_EJSCREEN_BG_Columns.xlsx
#'  EJSCREEN_2022_Full_with_AS_CNMI_GU_VI.csv.zip
#'  EJSCREEN_2022_StatePct_with_AS_CNMI_GU_VI.csv.zip
#'  2022_EJSCREEN_Supplemental_BG_Columns.xlsx
#'  EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip  
#'  EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv.zip
#'  EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip

