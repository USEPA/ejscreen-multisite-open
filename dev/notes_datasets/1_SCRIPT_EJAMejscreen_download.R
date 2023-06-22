# Import ejscreen 2.1 data for EJAMejscreendata package

########################################################### #
options(timeout = max(300, getOption("timeout"))) # default of 60 seconds is not enough
ejscreen_download_gdb <- function(
    folder = tempdir(), 
    gdbzipname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip", 
    gdbname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb", 
    baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/") {
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
  States_2022 <- sf::st_read(mypath, 'States')   # aka statestats
  USA_2022    <- sf::st_read(mypath, 'USA')      # aka usastats
  # "FileGDB" or "OpenFileGDB" is the driver to use.
  return(list(
    States_2022 = States_2022,
    USA_2022 = USA_2022
  ))
}
ejscreen_pctile_lookups_from_ftp <- function(
    folder = tempdir(), 
    gdbzipname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb.zip", 
    gdbname = "EJSCREEN_2022_with_AS_CNMI_GU_VI.gdb", 
    baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/") {
  
  mypath <- ejscreen_download_gdb(folder, gdbzipname, gdbname, baseurl)
  mypath <- ejscreen_unzip_gdb(mypath)
  return(   ejscreen_read_unzipped_lookups(mypath) )
}
################## #





########################################################### #

# new Supplemental indicators are here - but probably want to combine merge the supplemental and other !? ####
# *LIFE EXPECTANCY (Raw value, pctile, state.pctile, bin, state.bin, textpopup)   lowlifex ... 
# *1 SUPPLEMENTARY DEMOG INDEX BASED ON 5 DEMOG VARS (raw, pctile, etc.)         Demog.Index.Supp   etc
# *12 SUPPLEMENTARY EJ INDEXES BASED ON SUPPL DEMOG IND (raw, pctile, etc.)     EJ.DISPARITY.traffic.score.supp   ...

# DOWNLOAD ZIP FILES ####
baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/"
# browseURL(baseurl)
fnames <- c(
  "EJSCREEN_2022_with_AS_CNMI_GU_VI.csv.zip",
  "EJSCREEN_2022_StatePct_with_AS_CNMI_GU_VI.csv.zip",   # do we really need this? it has block level state percentiles but we recalculate those anyway for any given buffer analysis.
  "EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip",
  "EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv.zip"  # do we really need this? 
)

td <- tempdir()

# localpath <-  "~/../EJ 2021/EJSCREEN 2022 2.1 DATA late2022/2022-11-07-FINAL-2.1/"
 
# new function in curl:: 
curl::multi_download(urls = file.path(baseurl, fnames), destfiles = file.path(td, fnames))

# Download status: 4 done; 0 in progress. Total size: 340.12 Mb (100%)... done!             
#   # A tibble: 4 × 10
#    success status_code resumefrom url                           destf…¹ error type  modified             time headers
#    <lgl>         <int>      <dbl> <chr>                         <chr>   <chr> <chr> <dttm>              <dbl> <list> 
#   1 TRUE            200          0 https://gaftp.epa.gov/EJSCRE… "C:\\U… NA    appl… 2022-11-02 09:50:34  8.31 <chr>  
#   2 TRUE            200          0 https://gaftp.epa.gov/EJSCRE… "C:\\U… NA    appl… 2022-11-02 09:50:30 11.1  <chr>  
#   3 TRUE            200          0 https://gaftp.epa.gov/EJSCRE… "C:\\U… NA    appl… 2022-12-27 16:18:40 19.6  <chr>  
#   4 TRUE            200          0 https://gaftp.epa.gov/EJSCRE… "C:\\U… NA    appl… 2022-12-27 16:18:41 12.6  <chr>  
#   # … with abbreviated variable name  destfile
# for (i in fnames) {
#   a = file.path(baseurl, fnames[i])
#   b = fnames[i]
#   download.file(url=a, destfile = b, exdir = td)
# }
# download.file("https://gaftp.epa.gov/EJSCREEN/2022/EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip", 
#               destfile = "EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip")
# UNZIP ####
for (i in 1:length(fnames)) {
  print( unzip(file.path(td, fnames[i]),  exdir =  td, overwrite = TRUE)  )
}
########################################################### #
# READ CSV FILES ####
getfile <- function(fname, folder=td) {as.data.frame(readr::read_csv(file.path(folder, fname)))}


# note the 2022 is omitted here in how they get renamed which can be confusing:

EJSCREEN_Full_with_AS_CNMI_GU_VI      <- getfile("EJSCREEN_2022_Full_with_AS_CNMI_GU_VI.csv", td)
EJSCREEN_StatePct_with_AS_CNMI_GU_VI  <- getfile("EJSCREEN_2022_StatePct_with_AS_CNMI_GU_VI.csv", td)
# __but probably want to combine merge the supplemental and other !? ####
EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI          <- getfile("EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv", td)
EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <- getfile("EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv", td)

# check if same varnames used for state and us pctiles, and make them distinct

these = c(
'EJSCREEN_Full_with_AS_CNMI_GU_VI',
'EJSCREEN_StatePct_with_AS_CNMI_GU_VI',
'EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI',
'EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI'
)
sapply(these, function(x) dim(get(x)))
# setdiff(names(EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI), names(EJSCREEN_Full_with_AS_CNMI_GU_VI))
# [1] "LIFEEXPCT"   "P_LIFEEXPCT" "B_LIFEEXPCT" "T_LIFEEXPCT"


   #  ***********  * * * * * * *    to be completed still   





x <- ejscreen_pctile_lookups_from_ftp()
USA_2022    <- x$USA_2022
States_2022 <- x$States_2022

# SUPPLEMENTARY NOT THERE YET AS OF 3/29/2023


#  ***********  * * * * * * *    to be completed still   




# maybe change units (1 vs 100?) - percentages are 0 to 1, percentiles are 0 to 100, in these lookup tables. and in blockgroupstats




#  ***********  * * * * * * *    to be completed still   




# ALSO see script in  /EJAM/data-raw/usastats_subgroups.R
# That was used to add subgroups to these 2 tables before they were saved for the package.

# rename indicator variables ####

# LACK lowlifex, Demog.Index.Supp, etc.

# may change names, but which function to use?
# this  works but relies on ejscreen package file  pkg, and see EJAMbatch.summarizer::change... and fixnames and fixcolnames and fixnamestype etc. 
stop('need to load from ejscreen package the file called ejscreenformulas')
names(USA_2022)    <-  ejscreenformulas$Rfieldname[match(names(USA_2022), ejscreenformulas$gdbfieldname)]
# map_headernames$newnames_ejscreenapi[match(names(USA_2022), map_headernames$oldnames)]
names(States_2022) <- ejscreenformulas$Rfieldname[match(names(States_2022), ejscreenformulas$gdbfieldname)]
# names(USA_2022)    <- ejscreen package file ejscreenformulas$Rfieldname[match(names(USA_2022),    ejscreen package file ejscreenformulas$gdbfieldname)]
# names(States_2022) <- ejscreen package file ejscreenformulas$Rfieldname[match(names(States_2022), ejscreen package file ejscreenformulas$gdbfieldname)]
names(USA_2022)[2]
names(USA_2022)[2] <- "PCTILE"
names(States_2022) <- "PCTILE"
gsub("VSI.eo","Demog.Index", names(USA_2022))
gsub("VSI.eo","Demog.Index", names(States_2022))

# asdf <- copy(USA_2022)
# names(asdf) <- ejscreen package file change.fieldnames.ejscreen.csv( names(asdf) )
# USA_2022  <- copy(asdf)
# 
# asdf <- copy(States_2022)
# names(asdf) <- ejscreen package file change.fieldnames.ejscreen.csv( names(asdf) )
# States_2022 <- copy(asdf)

# names(USA_2022)    <- EJAMejscreenapi::map_headernames
# ?EJAMejscreenapi::map_headernames

# usethis::use_data(USA_2022,    overwrite = TRUE)
# usethis::use_data(States_2022, overwrite = TRUE)



# ADD METADATA ####
# see EJAM::metadata_add() and metadata_check()  or set metadata like this instead, E.G. :
#  meta   <- list(
#    census_version = 2020,
#    acs_version = '2016-2020',
#    acs_releasedate = '3/17/2022',
#    ejscreen_version = '2.1',
#    ejscreen_releasedate = 'October 2022',
#    ejscreen_pkg_data = 'bg22'
#  )
#attributes(EJSCREEN_Full_with_AS_CNMI_GU_VI)     <- c(attributes(EJSCREEN_Full_with_AS_CNMI_GU_VI),     meta)
EJSCREEN_Full_with_AS_CNMI_GU_VI      <- EJAM::metadata_add(EJSCREEN_Full_with_AS_CNMI_GU_VI)
EJSCREEN_StatePct_with_AS_CNMI_GU_VI  <- EJAM::metadata_add(EJSCREEN_StatePct_with_AS_CNMI_GU_VI) 

USA_2022                              <- EJAM::metadata_add(USA_2022)
States_2022                           <- EJAM::metadata_add(States_2022)

EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI          <- EJAM::metadata_add(EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI)
EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <- EJAM::metadata_add(EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI)


# what about lookups for supplemental indicators?? ####
# LIFEEXPCT	% low life expectancy


#  ***********  * * * * * * *    to be completed still   


# 
# D_PM25_2	Supplemental EJ Index for Particulate Matter 2.5
# D_OZONE_2	Supplemental EJ Index for Ozone
# D_DSLPM_2	Supplemental EJ Index for Diesel particulate matter
# D_CANCR_2	Supplemental EJ Index for Air toxics cancer risk
# D_RESP_2	Supplemental EJ Index for Air toxics respiratory HI
# D_PTRAF_2	Supplemental EJ Index for Traffic proximity
# D_LDPNT_2	Supplemental EJ Index for Lead paint
# D_PNPL_2	Supplemental EJ Index for Superfund Proximity
# D_PRMP_2	Supplemental EJ Index for RMP Facility Proximity
# D_PTSDF_2	Supplemental EJ Index for Hazardous waste proximity
# D_UST_2	Supplemental EJ Index for Underground storage tanks
# D_PWDIS_2	Supplemental EJ Index for Wastewater discharge












########################################################### #
# RENAME ALL VARIABLES (COLUMN NAMES) ####
# NEW AS OF 1/26/2023 - WILL RENAME COLUMNS OF DATA.FRAME TO MATCH WHAT IS USED IN EJAM:: and in ejscreen package file 
# instead of sticking with variable names from FTP site.
stop('need to get from ejscreen package the function change.fieldnames.ejscreen.csv')
bg <- copy(EJSCREEN_Full_with_AS_CNMI_GU_VI) # copied in case doing this after already part of package and just updating
names(bg) <-  change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_Full_with_AS_CNMI_GU_VI <- copy(bg)

bg <- copy(EJSCREEN_StatePct_with_AS_CNMI_GU_VI)
names(bg) <-  change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_StatePct_with_AS_CNMI_GU_VI <- copy(bg)
stop('requires EJAMejscreendata package here but want to avoid leaving it in this script to avoid rsconnect thinking that is a dependency, when deploying')
bg <- copy( EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI) # from the EJAMejscreendata package
names(bg) <-  change.fieldnames.ejscreen.csv( names(bg) ) # cc
EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI <- copy(bg) # 

bg <- copy( EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI)  
names(bg) <-  change.fieldnames.ejscreen.csv( names(bg) ) # vv
EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <- copy(bg)

#################################################################################

# if need to ADD DATA TO a PACKAGE ####

# usethis::use_data(EJSCREEN_Full_with_AS_CNMI_GU_VI,     overwrite = T)
# usethis::use_data(EJSCREEN_StatePct_with_AS_CNMI_GU_VI, overwrite = T)
# # new:
# usethis::use_data(EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI,          overwrite = T)
# usethis::use_data(EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI, overwrite = T)

#################################################################################
# LOOK AT IT ####

dim(EJSCREEN_Full_with_AS_CNMI_GU_VI)
# [1] 242940    155
dim(EJSCREEN_StatePct_with_AS_CNMI_GU_VI)
# [1] 242940    151

# dput(names(USA_2022))
# c("OBJECTID", "REGION", "PCTILE", "MINORPCT", "LOWINCPCT", "LESSHSPCT", 
#   "LINGISOPCT", "UNDER5PCT", "OVER64PCT", "PRE1960PCT", "UNEMPPCT", 
#   "VULEOPCT", "DSLPM", "CANCER", "RESP", "PTRAF", "PWDIS", "PNPL", 
#   "PRMP", "PTSDF", "OZONE", "PM25", "UST", "D_LDPNT_2", "D_DSLPM_2", 
#   "D_CANCR_2", "D_RESP_2", "D_PTRAF_2", "D_PWDIS_2", "D_PNPL_2", 
#   "D_PRMP_2", "D_PTSDF_2", "D_OZONE_2", "D_PM25_2", "D_UST_2")
 
# dput(names(EJSCREEN_Full_with_AS_CNMI_GU_VI))
# c("OBJECTID", "ID", "STATE_NAME", "ST_ABBREV", "CNTY_NAME", "REGION", 
#   "ACSTOTPOP", "ACSIPOVBAS", "ACSEDUCBAS", "ACSTOTHH", "ACSTOTHU", 
#   "ACSUNEMPBAS", "VULEOPCT", "MINORPOP", "MINORPCT", "LOWINCOME", 
#   "LOWINCPCT", "UNEMPLOYED", "UNEMPPCT", "LINGISO", "LINGISOPCT", 
#   "LESSHS", "LESSHSPCT", "UNDER5", "UNDER5PCT", "OVER64", "OVER64PCT", 
#   "PM25", "OZONE", "DSLPM", "CANCER", "RESP", "PTRAF", "PRE1960", 
#   "PRE1960PCT", "PNPL", "PRMP", "PTSDF", "UST", "PWDIS", "D_PM25_2", 
#   "D_OZONE_2", "D_DSLPM_2", "D_CANCR_2", "D_RESP_2", "D_PTRAF_2", 
#   "D_LDPNT_2", "D_PNPL_2", "D_PRMP_2", "D_PTSDF_2", "D_UST_2", 
#   "D_PWDIS_2", "P_VULEOPCT", "P_MINORPCT", "P_LWINCPCT", "P_UNEMPPCT", 
#   "P_LNGISPCT", "P_LESHSPCT", "P_UNDR5PCT", "P_OVR64PCT", "P_PM25", 
#   "P_OZONE", "P_DSLPM", "P_CANCR", "P_RESP", "P_PTRAF", "P_LDPNT", 
#   "P_PNPL", "P_PRMP", "P_PTSDF", "P_UST", "P_PWDIS", "P_PM25_D2", 
#   "P_OZONE_D2", "P_DSLPM_D2", "P_CANCR_D2", "P_RESP_D2", "P_PTRAF_D2", 
#   "P_LDPNT_D2", "P_PNPL_D2", "P_PRMP_D2", "P_PTSDF_D2", "P_UST_D2", 
#   "P_PWDIS_D2", "B_VULEOPCT", "B_MINORPCT", "B_LWINCPCT", "B_UNEMPPCT", 
#   "B_LESHSPCT", "B_LNGISPCT", "B_UNDR5PCT", "B_OVR64PCT", "B_PM25", 
#   "B_OZONE", "B_DSLPM", "B_CANCR", "B_RESP", "B_PTRAF", "B_LDPNT", 
#   "B_PNPL", "B_PRMP", "B_PTSDF", "B_UST", "B_PWDIS", "B_PM25_D2", 
#   "B_OZONE_D2", "B_DSLPM_D2", "B_CANCR_D2", "B_RESP_D2", "B_PTRAF_D2", 
#   "B_LDPNT_D2", "B_PNPL_D2", "B_PRMP_D2", "B_PTSDF_D2", "B_UST_D2", 
#   "B_PWDIS_D2", "T_VULEOPCT", "T_MINORPCT", "T_LWINCPCT", "T_UNEMPPCT", 
#   "T_LNGISPCT", "T_LESHSPCT", "T_UNDR5PCT", "T_OVR64PCT", "T_PM25", 
#   "T_OZONE", "T_DSLPM", "T_CANCR", "T_RESP", "T_PTRAF", "T_LDPNT", 
#   "T_PNPL", "T_PRMP", "T_PTSDF", "T_UST", "T_PWDIS", "T_PM25_D2", 
#   "T_OZONE_D2", "T_DSLPM_D2", "T_CANCR_D2", "T_RESP_D2", "T_PTRAF_D2", 
#   "T_LDPNT_D2", "T_PNPL_D2", "T_PRMP_D2", "T_PTSDF_D2", "T_UST_D2", 
#   "T_PWDIS_D2", "EXCEED_COUNT_80", "AREALAND", "AREAWATER", "NPL_CNT", 
#   "TSDF_CNT", "Shape_Length", "Shape_Area")
