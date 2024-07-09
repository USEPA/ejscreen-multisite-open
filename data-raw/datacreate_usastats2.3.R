#  
# Script to download .gdb file with new 7/2024 version 2.3 EJScreen
# gdb is one place to find the blockgroup data and also has lookup tables for US and State percentile cutoffs and means.
# .gdb is not essential but good double-check on separately downloaded .csv versions of those.
# Also see EJAM/data-raw/datacreate_blockgroupstats2.3 which gets those .csv files.

#  ######################################################################## #

# data.table format is used below for the blockgroupstats starting point 
# data.frame format is used below for all other files/info

#################################################################### #
##___ SET WORKING DIRECTORY  to save EJAM data ####
# grep("mysource",  (Sys.getenv()), value = T)
setwd(file.path(Sys.getenv("R_USER"), "EJAM")); getwd() 
# print(.packages()) # what is loaded (attached?)
# golem::detach_all_attached() 
# print(.packages()) #
# rm(list = ls())
savexl <- FALSE
downloadnow <- TRUE

#################################################################################### # 

#   PERCENTILE LOOKUP TABLES ####

## GET GDB, EXTRACT LOOKUPS ####

# functions to download, unzip, and read  pctile lookup tables  from gdb file on ftp site 

baseurl = "https://gaftp.epa.gov/EJScreen/2024/2.30_July_useMe/"

blockgroupstats_source_usa.gdb.zip   <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb.zip"           # gdb is not essential
blockgroupstats_source_usa.gdb       <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb"
blockgroupstats_source_state.gdb.zip <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip"  # gdb is not essential
blockgroupstats_source_state.gdb     <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb"

gdbzipname       = blockgroupstats_source_usa.gdb.zip
gdbname          = blockgroupstats_source_usa.gdb
gdbzipname_state = blockgroupstats_source_state.gdb.zip
gdbname_state    = blockgroupstats_source_state.gdb

mydir = "~/../../OneDrive - Environmental Protection Agency (EPA)/EJ 2021/EJSCREEN 2024"
if (!dir.exists(mydir)) {stop('mydir needed')}

## note also this tool to download multiple files easily: 
# curl::multi_download(urls = file.path(baseurl, fnames), destfiles = file.path(td, fnames))

options(timeout = max(300, getOption("timeout"))) # default of 60 seconds is not enough


ejscreen_download_gdb <- function(
    folder = tempdir(), 
    gdbzipname = gdbzipname,
    gdbname =    gdbname, 
    baseurl = baseurl) {
  cat("downloading gdb.zip\n")
  download.file(file.path(baseurl, gdbzipname), destfile = file.path(folder, gdbzipname))
  return(file.path(folder, gdbzipname))
}
ejscreen_unzip_gdb <- function(zipfilepath) {
  cat("unzipping gdb.zip\n")
  unzip(zipfilepath,   exdir = dirname(zipfilepath))
  return( gsub(".zip", "", zipfilepath))
}
ejscreen_read_unzipped_lookups <- function(mypath) {
  cat('reading percentile lookup tables from gdb\n')  
  print(sf::st_layers(mypath))
  if ("USA" %in%  sf::st_layers(mypath)$name) {
    usastats_gdb    <- sf::st_read(mypath, 'USA')  
  } else {
    warning("USA is not a table in that gdb")
    usastats_gdb <- NULL
  }
 # 2024-07 version has only USA table in this gdb.
  if ("States" %in%  sf::st_layers(mypath)$name) {
    statestats_gdb    <- sf::st_read(mypath, 'States')  
  } else {
    warning("States is not a table in that gdb")
    statestats_gdb <- NULL
  }
  # "FileGDB" or "OpenFileGDB" is the driver to use.
  return(list(
    statestats_gdb = statestats_gdb, 
    usastats_gdb    =  usastats_gdb 
  ))
}

ejscreen_pctile_lookups_from_ftp <- function(folder = tempdir(), 
                                             gdbzipname = gdbzipname, 
                                             gdbname =    gdbname,
                                             baseurl = baseurl) {

  mypath <- ejscreen_download_gdb(folder, gdbzipname, gdbname, baseurl)
  mypath <- ejscreen_unzip_gdb(mypath)
  return(   ejscreen_read_unzipped_lookups(mypath) )
}
#################################################################### #

if (downloadnow) {
  cat('THIS DOWNLOAD TAKES A LONG TIME ... see progress bar window that pops up behind this window... \n')
  x <- ejscreen_pctile_lookups_from_ftp(folder = mydir, gdbzipname = , gdbname = )
  y <- ejscreen_pctile_lookups_from_ftp(folder = mydir, gdbzipname = , gdbname = )
  cat('FINISHED WITH DOWNLOAD\n')
} else {
  gdbpath <- file.path(mydir, gdbname)
  if (file.exists(gdbpath)) {
    # to just read from gdb:
    x <- ejscreen_read_unzipped_lookups(gdbpath)
    y <- ejscreen_read_unzipped_lookups(gdbpath_state)
  } else {
    stop('not downloading and file not found here')
  }
}
usastats_gdb    <- x$usastats_gdb
statestats_gdb  <- y$statestats_gdb

names(usastats_gdb)   <- fixcolnames(names(usastats_gdb),   'csv', 'r')
names(statestats_gdb) <- fixcolnames(names(statestats_gdb), 'csv', 'r')

dim(usastats_gdb)
dim(statestats_gdb)
all.equal(usastats_gdb,   usastats_new)   
all.equal(statestats_gdb, statestats_new) 

rm(x, y)
gc()
ls()
# downloaded 492.0 MB
#################################################################### #



# ready to compare to usastats_new, statestats_new,
# as a double-check






















################################################################################ #

# make rownames less confusing since starting with 1 was for the row where PCTILE == 0,
# so make them match in USA one at least, but cannot same way for state one since they repeat for each state
rownames(usastats_new)     <- usastats_new$PCTILE
rownames(statestats_new) <- paste0(statestats_new$REGION, statestats_new$PCTILE) 

# MAKE THE STATE EJ INDICATORS (RAW SCORES) COLUMNS HAVE STATE PERCENTILE NAMES TO DISTINGUISH FROM US VERSIONS 
# BUT BE SURE THAT LOOKUP CODE TURNING RAW STATE EJ SCORES INTO PCTILES IS USING THE RIGHT NAMES 

names(statestats_new) <- gsub("EJ.DISPARITY", "state.EJ.DISPARITY", names(statestats_new))

cbind(names(usastats_new), names(statestats_new))

# > setdiff(names(statestats), names(usastats))
# [1] "state.EJ.DISPARITY.pm.eo"                "state.EJ.DISPARITY.pm.supp"          
# "state.EJ.DISPARITY.o3.eo"  "state.EJ.DISPARITY.o3.supp"   
# "state.EJ.DISPARITY.dpm.eo"               "state.EJ.DISPARITY.dpm.supp"
# etc
# [25] "state.EJ.DISPARITY.proximity.npdes.eo"   "state.EJ.DISPARITY.proximity.npdes.supp"

all.equal(names(usastats_new), names(statestats_new))  # yes 
# still same so far, so they need to be adjusted
setdiff(          names(usastats_new), names(statestats_new))
EJAM:::setdiff_yx(names(usastats_new), names(statestats_new))


#  varnames used for state and us pctiles ... make them distinct
cbind(names(EJAM::usastats), names(EJAM::statestats)) 
# no - 
# EJ index vars were adjusted to indicate if state pctile or us pctile

# [26,] "EJ.DISPARITY.pm.eo"                "state.EJ.DISPARITY.pm.eo"               
# [27,] "EJ.DISPARITY.pm.supp"              "state.EJ.DISPARITY.pm.supp"    

# [28,] "EJ.DISPARITY.o3.eo"                "state.EJ.DISPARITY.o3.eo"               
# [29,] "EJ.DISPARITY.o3.supp"              "state.EJ.DISPARITY.o3.supp"   

# [30,] "EJ.DISPARITY.dpm.eo"               "state.EJ.DISPARITY.dpm.eo"              
# [31,] "EJ.DISPARITY.dpm.supp"             "state.EJ.DISPARITY.dpm.supp"    

#  etc.

################################################################################ #

# SWITCH datasets TO NEW VERSIONS  #### 

usastats   <- usastats_new
statestats <- statestats_new

##########################################################################################################################################

save.image(file = "~/../Downloads/work in progress on usastats 2024-07.rda")


stop()

# at this point, switch over to use script in 
# EJAM/data-raw/datacreate_usastats_pctile_lookup_add_subgroups_demog.R










# ADD METADATA #### 
# use_data() to save for PACKAGE ####

usastats           <- EJAM:::metadata_add(usastats)
statestats         <- EJAM:::metadata_add(statestats)
usethis::use_data(usastats,   overwrite = T)
usethis::use_data(statestats, overwrite = T)


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
