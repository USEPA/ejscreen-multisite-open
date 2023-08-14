# Import ejscreen 2.1 data for EJAMejscreendata package

# Notes on creating the 2023 version 2.2 datasets:  EJAMejscreenapi/data-raw/update_to_ejscreenv2.2.R  
# 
# Below is modified code drafted as an alternative not used - 
#  The original code below for 2022 was based on the data being on the FTP site as Full and Supplemental in 2 separate files, 
#  but the 2023 version was all in 1 file for US pctiles and 1 file for State pctiles, with D2 and D5 (suppl) indicators together.
# So the code as updated below to say 2023 will not work for 2023 unless edited to work with 2023 names of files etc.
#  "https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv.zip"
#  "https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip"

# see EJAMejscreenapi/data-raw/update_to_ejscreenv2.2.R    instead


 #               OBSOLETE AS OF 2023  

# this just downloaded, unzipped, read, 
# renamed variables,
# added metadata. 

#################################################################################### # 
#################################################################################### # 

#   PERCENTILE LOOKUP TABLES

########################################################### #
# functions to download, unzip, and read  pctile lookup tables  from gdb file on ftp site ####

## note this tool to download multiple files easily: 
# curl::multi_download(urls = file.path(baseurl, fnames), destfiles = file.path(td, fnames))

options(timeout = max(300, getOption("timeout"))) # default of 60 seconds is not enough

ejscreen_download_gdb <- function(
    folder = tempdir(), 
    # "https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip"
    gdbzipname = "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip",    # gdbzipname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip", 
    gdbname =    "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb",  # gdbname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb", 
    baseurl = "https://gaftp.epa.gov/EJScreen/2023/") {  # baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/") {
  # get percentile lookup tables ####
  # the percentile lookup tables are in the gdb but not provided as csv.zip files on the ftp site
  cat("downloading\n")
  download.file(file.path(baseurl, gdbzipname), destfile = file.path(folder, gdbzipname))
  return(file.path(folder, gdbzipname))
}

ejscreen_unzip_gdb <- function(zipfilepath) {
  cat("unzipping\n")
  unzip(zipfilepath,   exdir = dirname(zipfilepath))
  return( gsub(".zip", "", zipfilepath))
}

ejscreen_read_unzipped_lookups <- function(mypath) {
  print(sf::st_layers(mypath))
  States_2023 <- sf::st_read(mypath, 'States')   # aka statestats  # States_2022 <- sf::st_read(mypath, 'States')   # aka statestats
  USA_2023    <- sf::st_read(mypath, 'USA')      # aka usastats  # USA_2022    <- sf::st_read(mypath, 'USA')      # aka usastats
  # "FileGDB" or "OpenFileGDB" is the driver to use.
  return(list(
    States_2023 = States_2023, # sf::st_read(mypath, 'States'),   # aka statestats # States_2022 = States_2022,
    USA_2023    =  USA_2023 # sf::st_read(mypath, 'USA')      # aka usastats# USA_2022 = USA_2022
  ))
}

ejscreen_pctile_lookups_from_ftp <- function(folder = tempdir(), 
                                             gdbzipname = "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip", # gdbzipname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip", 
                                             gdbname =    "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb",  # gdbname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb", 
                                             baseurl = "https://gaftp.epa.gov/EJScreen/2023/") {# baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/") {
  mypath <- ejscreen_download_gdb(folder, gdbzipname, gdbname, baseurl)
  mypath <- ejscreen_unzip_gdb(mypath)
  return(   ejscreen_read_unzipped_lookups(mypath) )
}
########################################################### #

#  get PERCENTILE LOOKUP TABLES

x <- ejscreen_pctile_lookups_from_ftp()

USA_2023    <- x$USA_2023 # USA_2022    <- x$USA_2022
States_2023 <- x$States_2023 # States_2022 <- x$States_2022

# maybe change units (1 vs 100?) - percentages are 0 to 1, percentiles are 0 to 100, in these lookup tables. and in blockgroupstats

# ***  ALSO see script in  /EJAM/data-raw/usastats_subgroups.R
# That could be used to add subgroups to these 2 tables before they were saved for the package.


########################################################### #
# RENAME ALL VARIABLES (COLUMN NAMES) ####
# WILL RENAME COLUMNS OF DATA.FRAME TO MATCH WHAT IS USED IN EJAM::   package  
# instead of sticking with variable names from FTP site.

names(USA_2023)    <-  fixnames(names(USA_2023),    oldtype = 'csv', newtype = 'r') # names(USA_2022)    <-  ejscreenformulas$Rfieldname[match(names(USA_2022), ejscreenformulas$gdbfieldname)]
names(States_2023) <-  fixnames(names(States_2023), oldtype = 'csv', newtype = 'r')# names(States_2022) <- ejscreenformulas$Rfieldname[match(names(States_2022), ejscreenformulas$gdbfieldname)]
names(USA_2023)[2] <- "PCTILE" # names(USA_2022)[2] <- "PCTILE"
names(States_2023) <- "PCTILE" # names(States_2022) <- "PCTILE"

########################################################### #
# ADD METADATA #### 

USA_2023                              <- EJAM::metadata_add(USA_2023)    # USA_2022                              <- EJAM::metadata_add(USA_2022)
States_2023                           <- EJAM::metadata_add(States_2023) # States_2022                           <- EJAM::metadata_add(States_2022)

# these get named usastats and statestats for use in EJAM package

########################################################### #



#################################################################################### # 
#################################################################################### # 

#  get BLOCK GROUP DATA FILE(S)


########################################################### #

# DOWNLOAD ZIP FILES ####

baseurl = "https://gaftp.epa.gov/EJScreen/2023/" # "https://gaftp.epa.gov/EJSCREEN/2022/"
# browseURL(baseurl)
td <- tempdir()  # or...   localpath <-  "~/../EJ 2021/EJSCREEN 2022 2.1 DATA late2022/2022-11-07-FINAL-2.1/"
fnames <- c(
   "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv.zip",
   "EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip"
  # "EJSCREEN_2022_with_AS_CNMI_GU_VI.csv.zip",
  # "EJSCREEN_2022_StatePct_with_AS_CNMI_GU_VI.csv.zip",   # do we really need this? it has block level state percentiles but we recalculate those anyway for any given buffer analysis.
  # "EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip",
  # "EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv.zip"  # do we really need this? 
) 

curl::multi_download(urls = file.path(baseurl, fnames), destfiles = file.path(td, fnames))
#   Download status: 4 done; 0 in progress. Total size: 340.12 Mb (100%)... done!             
#   # A tibble: 4 × 10

########################################################### #

# UNZIP files ####

for (i in 1:length(fnames)) {
  print( unzip(file.path(td, fnames[i]),  exdir =  td, overwrite = TRUE)  )
}
########################################################### #

# READ CSV FILES ####

getfile <- function(fname, folder=td) {as.data.frame(readr::read_csv(file.path(folder, fname)))}

# note the 2023 or 2022 is omitted here in how they get renamed which can be confusing:
EJSCREEN_Full_with_AS_CNMI_GU_VI      <- getfile("EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv", td) # getfile("EJSCREEN_2022_Full_with_AS_CNMI_GU_VI.csv", td)
EJSCREEN_StatePct_with_AS_CNMI_GU_VI  <- getfile("EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv", td)# getfile("EJSCREEN_2022_StatePct_with_AS_CNMI_GU_VI.csv", td)
# __but probably want to combine merge the supplemental and other !? ####
# EJSCREEN_2023_Supplemental_with_AS_CNMI_GU_VI          <- getfile("EJSCREEN_2023_Supplemental_with_AS_CNMI_GU_VI.csv", td) #EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI          <- getfile("EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv", td)
# EJSCREEN_2023_Supplemental_StatePct_with_AS_CNMI_GU_VI <- getfile("EJSCREEN_2023_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv", td) #EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <-  getfile("EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv", td)

# check if same varnames used for state and us pctiles, and make them distinct

these = c(
  'EJSCREEN_Full_with_AS_CNMI_GU_VI',
  'EJSCREEN_StatePct_with_AS_CNMI_GU_VI'
  # 'EJSCREEN_2023_Supplemental_with_AS_CNMI_GU_VI', # 'EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI',
  # 'EJSCREEN_2023_Supplemental_StatePct_with_AS_CNMI_GU_VI' # 'EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI'
)
sapply(these, function(x) dim(get(x)))

#  ***********  * * * * * * *    to be completed still  ? 


########################################################### #
# ADD METADATA #### 

EJSCREEN_Full_with_AS_CNMI_GU_VI      <- EJAM::metadata_add(EJSCREEN_Full_with_AS_CNMI_GU_VI )
EJSCREEN_StatePct_with_AS_CNMI_GU_VI  <- EJAM::metadata_add(EJSCREEN_StatePct_with_AS_CNMI_GU_VI ) 
# 
# EJSCREEN_2023_Supplemental_with_AS_CNMI_GU_VI          <- EJAM::metadata_add(EJSCREEN_2023_Supplemental_with_AS_CNMI_GU_VI) # EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI          <- EJAM::metadata_add(EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI)
# EJSCREEN_2023_Supplemental_StatePct_with_AS_CNMI_GU_VI <- EJAM::metadata_add(EJSCREEN_2023_Supplemental_StatePct_with_AS_CNMI_GU_VI) # EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <- EJAM::metadata_add(EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI)
#  

########################################################### #
# RENAME ALL VARIABLES (COLUMN NAMES) ####
# WILL RENAME COLUMNS OF DATA.FRAME TO MATCH WHAT IS USED IN EJAM::   package  
# instead of sticking with variable names from FTP site.

bg <- copy(EJSCREEN_Full_with_AS_CNMI_GU_VI) # copied in case doing this after already part of package and just updating
names(bg) <-  fixnames(names(bg), oldtype = 'csv', newtype = 'r') #  change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_Full_with_AS_CNMI_GU_VI <- copy(bg)

bg <- copy(EJSCREEN_StatePct_with_AS_CNMI_GU_VI)
names(bg) <- fixnames(names(bg), oldtype = 'csv', newtype = 'r') #  change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_StatePct_with_AS_CNMI_GU_VI <- copy(bg)

# stop('required EJAMejscreendata package here but wanted to avoid leaving it in this script to avoid rsconnect thinking that is a dependency, when deploying')
# bg <- copy( EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI) # from the EJAMejscreendata package... not anymore 
# names(bg) <- fixnames(names(bg), oldtype = 'csv', newtype = 'r') # change.fieldnames.ejscreen.csv( names(bg) ) # cc
# EJSCREEN_2023_Supplemental_with_AS_CNMI_GU_VI <- copy(bg) # # EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI <- copy(bg) # 
# 
# bg <- copy( EJSCREEN_2023_Supplemental_StatePct_with_AS_CNMI_GU_VI)   # bg <- copy( EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI)  
# names(bg) <- fixnames(names(bg), oldtype = 'csv', newtype = 'r') # change.fieldnames.ejscreen.csv( names(bg) ) # vv
# EJSCREEN_2023_Supplemental_StatePct_with_AS_CNMI_GU_VI <- copy(bg) # EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <- copy(bg)

#################################################################################

# when ready to use as data in a PACKAGE ####

# usethis::use_data(EJSCREEN_Full_with_AS_CNMI_GU_VI,     overwrite = T)
# usethis::use_data(xyz,     overwrite = T)


#################################################################################

# Look at the data ####

dim(EJSCREEN_Full_with_AS_CNMI_GU_VI)
dim(EJSCREEN_StatePct_with_AS_CNMI_GU_VI)
 
# dput(names(USA_2022))
# c("OBJECTID", "REGION", "PCTILE", "MINORPCT", "LOWINCPCT", "LESSHSPCT", 
#   "LINGISOPCT", "UNDER5PCT", "OVER64PCT", "PRE1960PCT", "UNEMPPCT", 
#   "VULEOPCT", "DSLPM", "CANCER", "RESP", "PTRAF", "PWDIS", "PNPL", 
#   "PRMP", "PTSDF", "OZONE", "PM25", "UST", "D_LDPNT_2", "D_DSLPM_2", 
#   "D_CANCR_2", "D_RESP_2", "D_PTRAF_2", "D_PWDIS_2", "D_PNPL_2", 
#   "D_PRMP_2", "D_PTSDF_2", "D_OZONE_2", "D_PM25_2", "D_UST_2")

# dput(names(EJSCREEN_Full_with_AS_CNMI_GU_VI)) 
