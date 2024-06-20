#  
# Script to download the new 7/2023 and then 8/2023 version 2.2 EJScreen  (  note the 8/2023 version with fixed cancer data. )
 
#  - lookup tables for US and State percentile cutoffs and means,
 
# and save that info as  
#  - new lookup tables in usastats and statestats
 
# Also note the script in  EJAM/data-raw/datacreate_usastats_pctile_lookup_add_subgroups_demog.R

#  ######################################################################## #

#    csv/gdb files: Actual full dataset as csv files and geodatabases 
# not at https://gaftp.epa.gov/EJScreen/2023/  got replaced with fixes 8/2023 and then also in september 2023.
# baseurl = "https://gaftp.epa.gov/EJScreen/2023/2.21_August_UseMe/") # a fixed version that was replaced in sept
# 
#    -  EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv.zip
#    -  EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip   full dataset plus the lookup table of US percentiles

#    -  EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip
#    -  EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip  full dataset plus the lookup table of State percentiles

#    -  Note there is no separate file of supplemental EJ indexes in this version 2.2 set (unlike in the 2022-early 2023 v2.1 folder).

#    csv/gdb files: Data dictionary for downloadable dataset: 
#
#    - 2023_07  ver 2.2 <https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_Columns.xlsx>
#    - Also see intro webpage at https://origin-awswest-www.epa.gov/ejscreen/download-ejscreen-data 
#    - Also see list of changes in variable names (column headers in data files) 
#        for version 2.2 (2023) vs version 2.1 (2022) datasets. ejscreen-2-2-column-name-changes
   
#################################################################### #
# save or download? save xlsx? ####

# data.table format is used below for the blockgroupstats starting point, bg2.2_csv or bg2.2_gdb
# data.frame format is used below for all other files/info


#################################################################### #
##___ SET WORKING DIRECTORY  to save EJAM data ####
# stop('must be in the root of the EJAM source package to update these in that package')
# grep("mysource",  (Sys.getenv()), value = T)
setwd(file.path(Sys.getenv("R_USER"), "EJAM")); getwd() # just make sure this is the right one
print(.packages()) # what is loaded (attached?)
golem::detach_all_attached() # unattach EJAM so cannot lazy load statestats by accident, for example
print(.packages()) #
rm(list = ls())
savexl <- FALSE  #  TRUE # 
downloadnow <- TRUE # FALSE 
#################################################################### #
#################################################################### #
#################################################################### #
 

#################################################################################### # 
#################################################################################### # 

#   PERCENTILE LOOKUP TABLES

# Get lookup tables of US and State percentiles cutoffs ####
#  The gdb files are the only place that provides lookup table of US and State percentiles

########################################################### #

## GET GDB AND EXTRACT LOOKUPS VIA CODE ####

# functions to download, unzip, and read  pctile lookup tables  from gdb file on ftp site 

## note also this tool to download multiple files easily: 
# curl::multi_download(urls = file.path(baseurl, fnames), destfiles = file.path(td, fnames))

options(timeout = max(300, getOption("timeout"))) # default of 60 seconds is not enough

#   EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip           has the state and us percentile lookup tables.
#   EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip  does not have lookup tables, only US gdb does.
ejscreen_download_gdb <- function(
    folder = tempdir(), 
    gdbzipname = "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip",    # gdbzipname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip", 
    gdbname =    "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb",  # gdbname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb", 
    baseurl = "https://gaftp.epa.gov/EJScreen/2023/2.22_September_UseMe/") {
    # baseurl = "https://gaftp.epa.gov/EJScreen/2023/2.21_August_UseMe/") { # https://gaftp.epa.gov/EJScreen/2023/2.21_August_UseMe/ # briefly was https://gaftp.epa.gov/EJScreen/2023/  # baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/") {
  # get percentile lookup tables ####
  # the percentile lookup tables are in the gdb but not provided as csv.zip files on the ftp site
  cat("downloading gdb.zip\n")
  download.file(file.path(baseurl, gdbzipname), destfile = file.path(folder, gdbzipname))
  return(file.path(folder, gdbzipname))
  # "https://gaftp.epa.gov/EJScreen/2023/     ...       EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip" # has dataset of bg but also lookup tables
  # "https://gaftp.epa.gov/EJScreen/2023/     ...       EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip" # do not need this for EJAM
}
ejscreen_unzip_gdb <- function(zipfilepath) {
  cat("unzipping gdb.zip\n")
  unzip(zipfilepath,   exdir = dirname(zipfilepath))
  return( gsub(".zip", "", zipfilepath))
}
ejscreen_read_unzipped_lookups <- function(mypath) {
  cat('reading percentile lookup tables from gdb\n')  
  print(sf::st_layers(mypath))
  States_2023 <- sf::st_read(mypath, 'States')   # aka statestats  # States_2022 <- sf::st_read(mypath, 'States')   # aka statestats
  USA_2023    <- sf::st_read(mypath, 'USA')      # aka usastats    # USA_2022    <- sf::st_read(mypath, 'USA')      # aka usastats
  # "FileGDB" or "OpenFileGDB" is the driver to use.
  return(list(
    States_2023 = States_2023, # sf::st_read(mypath, 'States') # aka statestats # States_2022 = States_2022,
    USA_2023    =  USA_2023    # sf::st_read(mypath, 'USA')      # aka usastats # USA_2022 = USA_2022
  ))
}
# these differ now in 8/2023 vs 7/2023, so use newer ones
ejscreen_pctile_lookups_from_ftp <- function(folder = tempdir(), 
                                             gdbzipname = "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip", # gdbzipname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip", 
                                             gdbname =    "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb",  # gdbname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb", 
                                             baseurl = "https://gaftp.epa.gov/EJScreen/2023/2.22_September_UseMe/") {
                                             # baseurl = "https://gaftp.epa.gov/EJScreen/2023/2.21_August_UseMe/") { # baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/") {
  mypath <- ejscreen_download_gdb(folder, gdbzipname, gdbname, baseurl)
  mypath <- ejscreen_unzip_gdb(mypath)
  return(   ejscreen_read_unzipped_lookups(mypath) )
}
#################################################################### #

if (downloadnow) {
  cat('THIS DOWNLOAD TAKES A LONG TIME ... see progress bar window that pops up behind this window... \n')
  # x <- ejscreen_pctile_lookups_from_ftp()
  x <- ejscreen_pctile_lookups_from_ftp(folder = "~/../../OneDrive - Environmental Protection Agency (EPA)/EJ 2021/EJScreen 2023/FTP 2023-12")
  cat('FINISHED WITH DOWNLOAD\n')
} 
usastats2.2    <- x$USA_2023    # 
statestats2.2  <- x$States_2023 #  
rm(x)
gc()
ls()
# downloading
# trying URL 'https://gaftp.epa.gov/EJScreen/2023//EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip'
# Content type 'application/zip' length 515860820 bytes (492.0 MB)
# downloaded 492.0 MB
# unzipping
# Driver: OpenFileGDB 
#################################################################### #

#################################################################### # 
# as.data.frame(readr::read_csv())  

# 2023-08-21 extra lookup tables for these
#   dir( "~/../EJ 2021/EJScreen 2023/FTP 2023 intranet/documents_20230821 (1)")
#   EJSCREEN_2023_BG_extra_US_lookup.csv   EJSCREEN_2023_BG_extra_state_lookup.csv   
# # OID_	PCTILE	LIFEEXP	LIFEEXPPCT	HEARTDISEASE	ASTHMA	CANCER	DISABILITYPCT	LIMITEDBBPCT	NOHINCPCT	flood_y00	flood_y30	fire_y00	fire_y30
# fixnames(c('OID_',	'PCTILE',	'LIFEEXP',	'LIFEEXPPCT',	'HEARTDISEASE',	'ASTHMA',	'CANCER',	'DISABILITYPCT',	'LIMITEDBBPCT',	'NOHINCPCT',	'flood_y00',	'flood_y30',	'fire_y00',	'fire_y30'), oldtype = 'api', newtype = 'r')
# renames none 

#################################################################### #
# Double check to confirm the tables are identical from gdb or from csv ####
# Compare gdb-sourced vs manually downloaded as csv - lookup tables:

# Fixed versions as of 8/2023 downloaded manually, here, as well as code downloading / extracting from GDB files above: 
# "EJScreen 2023\FTP 2023 intranet\documents_20230821 (1)\EJSCREEN_2023_BG_with_AS_CNMI_GU_VI_lookup.csv"
# "EJScreen 2023\FTP 2023 intranet\documents_20230821 (1)\EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI_lookup.csv"
#  read those too.
# LOOKUP TABLE MANUALLY DOWNLOADED - but see september versions ? these actually match the september ones.
usastats_csv  <-  as.data.frame(readr::read_csv( "~/../EJ 2021/EJScreen 2023/FTP 2023 intranet/documents_20230821 (1)/EJSCREEN_2023_BG_with_AS_CNMI_GU_VI_lookup.csv"))
statestats_csv <- as.data.frame(readr::read_csv( "~/../EJ 2021/EJScreen 2023/FTP 2023 intranet/documents_20230821 (1)/EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI_lookup.csv"))
usastats_csv$OID_ <- NULL
statestats_csv$OID_ <- NULL
# 
# dim(usastats2.2)
# dim(usastats_csv)
# dim(statestats2.2)
# dim(statestats_csv)
all.equal(usastats_csv, usastats2.2)# [1] TRUE
all.equal(statestats_csv, statestats2.2)# [1] TRUE

rm(statestats_csv, usastats_csv)
rm( downloadnow             ,         ejscreen_download_gdb   ,         ejscreen_pctile_lookups_from_ftp, ejscreen_read_unzipped_lookups ,  ejscreen_unzip_gdb)

####################################################################
# ADD A FEW EXTRA INDICATORS WITH LOOKUP INFO ####
# EXTRA INDICATORS
statestats_extra <- as.data.frame(readr::read_csv( "~/../EJ 2021/EJScreen 2023/FTP 2023 intranet/documents_20230821 (1)/EJSCREEN_2023_BG_extra_state_lookup.csv"))
usastats_extra   <- as.data.frame(readr::read_csv( "~/../EJ 2021/EJScreen 2023/FTP 2023 intranet/documents_20230821 (1)/EJSCREEN_2023_BG_extra_US_lookup.csv"))
# > all.equal(usastats_extra$PCTILE,usastats2.2$PCTILE)
# [1] TRUE
# > all.equal(statestats_extra$PCTILE,statestats2.2$PCTILE)
# [1] TRUE
# all.equal(statestats_extra$REGION,statestats2.2$REGION)
# [1] TRUE
intersect(names(usastats2.2), names(usastats_extra))
# [1] "PCTILE"     "LIFEEXPPCT" "CANCER"    # problem - drop redundant lowlifex and rename CANCER to clarify extra csv means adult cancer rate, not air toxics score
names(usastats_extra)    <- gsub("CANCER", "cancer.rate.adults", names(  usastats_extra) )
names(statestats_extra)  <- gsub("CANCER", "cancer.rate.adults", names(statestats_extra) )

usastats2.2   <- cbind(  usastats2.2,   usastats_extra[, !grepl('OID_|PCTILE|LIFEEXPPCT',names(  usastats_extra))])
statestats2.2 <- cbind(statestats2.2, statestats_extra[, !grepl('OID_|PCTILE|LIFEEXPPCT',names(statestats_extra))])
rm(usastats_extra, statestats_extra)
library(EJAMejscreenapi)
names(usastats2.2)   <-  fixcolnames(names(  usastats2.2), oldtype = 'csv', newtype = 'r')
names(statestats2.2) <- fixcolnames(names(statestats2.2), oldtype = 'csv', newtype = 'r')
golem::detach_all_attached()
# get rid of duplicated REGION and PCTILE columns due to cbind done. 
statestats2.2[ , which(names(statestats2.2) == "REGION")[2]] <- NULL
setdiff(names(statestats2.2),   names(usastats2.2))  # good now that dupe REGION col gone

# MAKE THE STATE EJ INDICATORS (RAW SCORES) COLUMNS HAVE STATE PERCENTILE NAMES TO DISTINGUISH FROM US VERSIONS 
# BUT BE SURE THAT LOOKUP CODE TURNING RAW STATE EJ SCORES INTO PCTILES IS USING THE RIGHT NAMES 
names(statestats2.2) <- gsub("EJ.DISPARITY", "state.EJ.DISPARITY", names(statestats2.2))

cbind(names(usastats2.2), names(statestats2.2))

# note there are indicators in lookups that are not in csv per maphead and blockgroupstats, but are actually in other csv files yet to be added
#
# > setdiff(names(usastats2.2), fixcolnames(map_headernames$csvname2.2, 'csv','r')) # PCTILE only
# [1] "PCTILE"             "LIFEEXP"            "HEARTDISEASE"       "ASTHMA"             "cancer.rate.adults" "DISABILITYPCT"      "LIMITEDBBPCT"       "NOHINCPCT"          "flood_y00"         
# [10] "flood_y30"          "fire_y00"           "fire_y30"          
# > setdiff( fixcolnames(names(usastats2.2), 'r','csv'), map_headernames$csvname2.2) # PCTILE only
# [1] "PCTILE"             "LIFEEXP"            "HEARTDISEASE"       "ASTHMA"             "cancer.rate.adults" "DISABILITYPCT"      "LIMITEDBBPCT"       "NOHINCPCT"          "flood_y00"         
# [10] "flood_y30"          "fire_y00"           "fire_y30"    
# setdiff(names(usastats2.2), union( fixcolnames(map_headernames$acsbgname,'acsbgname','r'), fixcolnames(map_headernames$csvname2.2,'csv','r') ))
# [1] "PCTILE"             "LIFEEXP"            "HEARTDISEASE"       "ASTHMA"             "cancer.rate.adults" "DISABILITYPCT"      "LIMITEDBBPCT"       "NOHINCPCT"          "flood_y00"         
# [10] "flood_y30"          "fire_y00"           "fire_y30"

# make rownames less confusing since starting with 1 was for the row where PCTILE == 0, so make them match in USA one at least, but cannot same way for state one since they repeat for each state
rownames(usastats2.2)     <- usastats2.2$PCTILE
rownames(statestats2.2) <- paste0(statestats2.2$REGION, statestats2.2$PCTILE) # NULL

rm(savexl)

# save.image(file = "~/../Downloads/work in progress on usastats 2023-08-30.rda")
save.image(file = "~/../Downloads/work in progress on usastats 2023-12-12.rda")

##########################################################################################################################################

# check them to see if OK before replacing existing datasets in package...

# statestats has some NA values:
# but that is OK, if the function looking up pctiles can handle NA values for a zone! and there is not a better way to indicate missing values.
# pm and ozone had raw and EJ eo and EJ supp indicators in AK, HI, PR all NA values
# also, NPDES all 3 vars all NA in AK only.
# # also, lowlifex all NA in PR only. 
# statestats2.2[statestats2.2$REGION %in% 'AK', c("pm", "o3"  )] # etc.

# also, no Island Areas here at all as rows - maybe add those but with only NA values for all pctiles and mean and all indicators?
# seems inefficient. should fix percentile lookup function to handle cases where a REGION is missing from lookup table.


##########################################################################################################################################
##########  A   PROBLEM HERE  
# LOOKUP TABLES FROM THE GEODATABASES HAD ONLY ABOUT 50 KEY VARIABLES, BUT 
# LACK THE DEMOGRAPHIC SUBGROUPS LOOKUP INFO, FOR EXAMPLE! 
# That is because community report in EJScreen does not report others as percentiles, just raw.
# DEMOG SUBGROUPS NOT HERE AND MANY OTHER NEW INDICATORS NOT IN LOOKUP HERE EITHER but maybe do not need percentiles of those


# one option was to create
# placeholders with 0 values, for now to make code work for now?? ####
# do not have and do not need pctiles for subgroups maybe, since EJScreen does not report them that way. EJAM was doing that though.
# 
# subgroup_placeholders <- data.frame(matrix(0, nrow = NROW(usastats2.2), ncol = length( c(names_d_subgroups_nh, names_d_subgroups_alone))))
# names(subgroup_placeholders) <- c(names_d_subgroups_nh, names_d_subgroups_alone)
# usastats2.2 <- cbind(usastats2.2, subgroup_placeholders)
# 
# subgroup_placeholders <- data.frame(matrix(0, nrow = NROW(statestats2.2), ncol = length( c(names_d_subgroups_nh, names_d_subgroups_alone))))
# names(subgroup_placeholders) <-  c(names_d_subgroups_nh, names_d_subgroups_alone)
# statestats2.2 <- cbind(statestats2.2, subgroup_placeholders)

# Other option is create pctile lookup columns for the demog subgroups, from the full dataset of all blockgroups.

stop()
# at this point, switch over to use script in 
 # EJAM/data-raw/datacreate_usastats_pctile_lookup_add_subgroups_demog.R








############################################################################# #
# Check what States are included in lookup table
#>   setdiff(unique(blockgroupstats$ST), unique(statestats$REGION))
# [1] "AS" "GU" "MP" "VI"

############################################################################# #
 

# ideally would create  pctile lookup info for demog subgroups, in us, states, island areas; 
# except that EJScreen community report does not actually report those as percentiles, only as raw percentages,
# even though EJAM was reporting them as percentiles, so either redo EJAM calc of pctiles table for subgroups
# OR just stop reporting those as percentiles.
# Not sure which is easier to do quickly. 
# Simplifies things to stop reporting those as pctiles, just use NA for that cell in a table, 
# but maybe need to have NA values in cols in usastats and statestats for those indicators since code looks for them.
# 
# and same for any other indicators we want to report as percentiles in EJAM 

# done with lookup percentile tables 

#################################################################### #
