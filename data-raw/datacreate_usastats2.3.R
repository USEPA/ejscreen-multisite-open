#  This had been a 
# Script to download .gdb file with new 7/2024 version 2.3 EJScreen
# gdb is one place to find the blockgroup data and also has lookup tables for US and State percentile cutoffs and means.
# .gdb is not essential but good double-check on separately downloaded .csv versions of those.

# But now just see  EJAM/data-raw/datacreate_blockgroupstats2.3 which gets those .csv files for
#  usastats_new, statestats_new

#  ######################################################################## #

# data.table format is used below for the blockgroupstats starting point 
# data.frame format is used below for all other files/info

if (0 == 1) {
  
#################################################################### #
   
   # OBSOLETE ALTERNATIVE SOURCE OF DATA ####
  
##___ SET WORKING DIRECTORY  to save EJAM data  
# grep("mysource",  (Sys.getenv()), value = T)
setwd(file.path(Sys.getenv("R_USER"), "EJAM")); getwd() 
# print(.packages()) # what is loaded (attached?)
# golem::detach_all_attached() 
# print(.packages()) #
# rm(list = ls())
savexl <- FALSE
downloadnow <- TRUE

#################################################################################### # 

#   PERCENTILE LOOKUP TABLES  

## GET GDB, EXTRACT LOOKUPS  

# functions to download, unzip, and read  pctile lookup tables  from gdb file on ftp site 

baseurl = "https://gaftp.epa.gov/EJScreen/2024/2.30_July_useMe/"

blockgroupstats_source_usa.gdb.zip   = "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb.zip"           # gdb is not essential
blockgroupstats_source_usa.gdb       = "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb"
blockgroupstats_source_state.gdb.zip = "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip"  # gdb is not essential
blockgroupstats_source_state.gdb     = "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb"

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
    gdbzipname = blockgroupstats_source_usa.gdb.zip,
    gdbname =    blockgroupstats_source_usa.gdb, 
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
                                             gdbzipname = blockgroupstats_source_usa.gdb.zip, 
                                             gdbname =    blockgroupstats_source_usa.gdb,
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


}

################################################################################ #

# FINISH CREATING usastats, statestats ####


# MAKE THE STATE EJ INDICATORS (RAW SCORES) COLUMNS HAVE STATE PERCENTILE NAMES TO DISTINGUISH FROM US VERSIONS 
# BUT BE SURE THAT LOOKUP CODE TURNING RAW STATE EJ SCORES INTO PCTILES IS USING THE RIGHT NAMES 

setDT(statestats_new)
data.table::setnames(statestats_new, 
                     old = c(names_ej, names_ej_supp), 
                     new = c(names_ej_state, names_ej_supp_state))
setDF(statestats_new)
# but later they will be data.table I think
# cbind(names(usastats_new), names(statestats_new))

# > usastats_new, statestats_new rownames ####
# make rownames less confusing since starting with 1 was for the row where PCTILE == 0,
# so make them match in USA one at least, but cannot same way for state one since they repeat for each state

rownames(usastats_new)     <- usastats_new$PCTILE
rownames(statestats_new) <- paste0(statestats_new$REGION, statestats_new$PCTILE) 

################################################################################ #

# switch to usastats, statestats (from usastats_new, etc.)  #### 

usastats   <- usastats_new
statestats <- statestats_new
rm(statestats_new, usastats_new)
gc()

######################################################################################## ################## #

# save.image(file = "~/../Downloads/work in progress on usastats 2024-07.rda")

cat("at this point, before saving these via use_data(), do script in 
     EJAM/data-raw/datacreate_usastats_pctile_lookup_add_subgroups_demog.R \n")

######################################################################################## ################## #

# check them to see if OK before replacing existing datasets in package?

# statestats has some NA values:
# but that is OK, if the function looking up pctiles can handle NA values for a zone! and there is not a better way to indicate missing values.
# pm and ozone had raw and EJ eo and EJ supp indicators in AK, HI, PR all NA values
# also, NPDES all 3 vars all NA in AK only.
# # also, lowlifex all NA in PR only. 
# statestats2.2[statestats2.2$REGION %in% 'AK', c("pm", "o3"  )] # etc.

# also, no Island Areas here at all as rows - maybe add those but with only NA values for all pctiles and mean and all indicators?
# seems inefficient. should fix percentile lookup function to handle cases where a REGION is missing from lookup table.

########################

# FIX DEMOG INDEX US VS STATE NAMES ####
# use state version when reporting state percentiles, but do not use a special name for them
if ("Demog.Index.State" %in% colnames(statestats)) {
  statestats$Demog.Index      <- statestats$Demog.Index.State
} else {
 warning("did not find statestats$Demog.Index.State so check that Demog.Index.State column is correct there") 
}
if ("Demog.Index.Supp.State" %in% colnames(statestats)) {
statestats$Demog.Index.Supp <- statestats$Demog.Index.Supp.State
} else {
  warning("did not find statestats$Demog.Index.Supp.State so check that Demog.Index.State column is correct there") 
}
#  # these are ok and will not need state versions
# usastats$Demog.Index.State      <- usastats$Demog.Index
# usastats$Demog.Index.Supp.State <- usastats$Demog.Index.Supp



######################################################################################## ################## #

# NEXT SCRIPT HAS TO ADD DEMOG SUBGROUPS ####
