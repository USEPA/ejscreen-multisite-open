# Import ejscreen 2.1 data for EJAMejscreendata package

########################################################### #

# new Supplemental indicators are here - but probably want to combine merge the supplemental and other !? ####
# *LIFE EXPECTANCY (Raw value, pctile, state.pctile, bin, state.bin, textpopup)
# *1 SUPPLEMENTARY DEMOG INDEX BASED ON 5 DEMOG VARS (raw, pctile, etc.)
# *12 SUPPLEMENTARY EJ INDEXES BASED ON SUPPL DEMOG IND (raw, pctile, etc.)

# DOWNLOAD ZIP FILES ####
# # Obtain the file with something like this or manually download and unzip it:
# download.file(
#  'https://gaftp.epa.gov/EJSCREEN/2022/EJSCREEN_Full_with_AS_CNMI_GU_VI.zip', 
#                           destfile = 'EJSCREEN_Full_with_AS_CNMI_GU_VI.zip')
fnames <- c(
  "EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip",
  "EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv.zip"
)
baseurl = "https://gaftp.epa.gov/EJSCREEN/2022/"
for (i in fnames) {
  a = file.path(baseurl, fnames[i])
  b = fnames[i]
  download.file(url=a, destfile = b, exdir = getwd())
}
# download.file("https://gaftp.epa.gov/EJSCREEN/2022/EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip", 
#               destfile = "EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv.zip")
# UNZIP ####
for (i in fnames) {
  unzip(fnames[i], exdir = getwd())  
}
########################################################### #
# READ CSV FILES ####
EJSCREEN_Full_with_AS_CNMI_GU_VI      <- as.data.frame(readr::read_csv("EJSCREEN_Full_with_AS_CNMI_GU_VI.csv"))
EJSCREEN_StatePct_with_AS_CNMI_GU_VI  <- as.data.frame(readr::read_csv("EJSCREEN_StatePct_with_AS_CNMI_GU_VI.csv"))
USA_2022                              <- as.data.frame(readr::read_csv("USA_2022.csv"))
States_2022                           <- as.data.frame(readr::read_csv("States_2022.csv"))
# __but probably want to combine merge the supplemental and other !? ####
EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI          <- as.data.frame(readr::read_csv("EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI.csv"))
EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <- as.data.frame(readr::read_csv("EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI.csv"))

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





########################################################### #
# RENAME ALL VARIABLES (COLUMN NAMES) ####
# NEW AS OF 1/26/2023 - WILL RENAME COLUMNS OF DATA.FRAME TO MATCH WHAT IS USED IN EJAM:: and in ejscreen::
# instead of sticking with variable names from FTP site.

bg <- copy(EJSCREEN_Full_with_AS_CNMI_GU_VI) # copied in case doing this after already part of package and just updating
names(bg) <- ejscreen::change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_Full_with_AS_CNMI_GU_VI <- copy(bg)

bg <- copy(EJSCREEN_StatePct_with_AS_CNMI_GU_VI)
names(bg) <- ejscreen::change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_StatePct_with_AS_CNMI_GU_VI <- copy(bg)

bg <- copy(USA_2022)
names(bg) <- ejscreen::change.fieldnames.ejscreen.csv( names(bg) )
USA_2022  <- copy(bg)

bg <- copy(States_2022)
names(bg) <- ejscreen::change.fieldnames.ejscreen.csv( names(bg) )
States_2022 <- copy(bg)

bg <- copy(EJAMejscreendata::EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI)
names(bg) <- ejscreen::change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI <- copy(bg)

bg <- copy(EJAMejscreendata::EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI)
names(bg) <- ejscreen::change.fieldnames.ejscreen.csv( names(bg) )
EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI <- copy(bg)

#################################################################################

# ADD DATA TO PACKAGE ####
usethis::use_data(EJSCREEN_Full_with_AS_CNMI_GU_VI,     overwrite = T)
usethis::use_data(EJSCREEN_StatePct_with_AS_CNMI_GU_VI, overwrite = T)
usethis::use_data(USA_2022,    overwrite = T)
usethis::use_data(States_2022, overwrite = T)
# new:
usethis::use_data(EJSCREEN_2022_Supplemental_with_AS_CNMI_GU_VI,          overwrite = T)
usethis::use_data(EJSCREEN_2022_Supplemental_StatePct_with_AS_CNMI_GU_VI, overwrite = T)

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
