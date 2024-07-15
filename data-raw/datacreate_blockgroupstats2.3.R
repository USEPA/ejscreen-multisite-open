

localfolder = "~/../Downloads/ejscreen new ftp downloads"
baseurl = "https://gaftp.epa.gov/EJScreen/2024/2.30_July_useMe/"

# x = function(localfolder = "~/../Downloads/ejscreen new ftp downloads", 
#              baseurl = "https://gaftp.epa.gov/EJScreen/2024/2.30_July_useMe/",
#              
# ) {
#   if (!dir.exists(localfolder)) {dir.create(localfolder)}
#   
#   
#   
#   
# }

# SCRIPT TO UPDATE EJSCREEN BLOCKGROUP DATA AND PERCENTILE LOOKUP TABLES FOR EJAM YEARLY

############################################################################################ #
## older scripts:
# /dev/notes_datasets
# [1] "0_SCRIPT_overview_get_ejscreendata.R"             "1_SCRIPT_EJAMejscreen_download.R"                
# [3] "2_SCRIPT_FOR_FIPS_ST_TRACT_CNTY.R"                "3_SCRIPT_create_bgDemog_ejscreen2.1_andtracts.R" 
# [5] "4_SCRIPT_ADD_PUERTORICO_DEMOG_SUBGROUPS.R"        "5_SCRIPT_merge_demogsubgroups_v2.1.R"            
# [7] "6_SCRIPT_create_blockgroupstats.R"                "8_SCRIPT_make_MeansByGroup_and_Ratios_RRS.US22.R"
# [9] "9_SCRIPT_PCTILELOOKUPS_READ-CSVS-MID-2022.R"      "NOTES_which_states_are_in_which_datasets.R"      
# [11] "PINSURLTRY.R" 
### and had also been info in a file here but that was moved:  EJAMejscreenapi/data-raw/update_to_ejscreenv2.2.R    

############################################################################################ #

#  DOWNLOAD/UPDATE BLOCK GROUP DATA FILE(S)

#################################################################################### # 

# DOWNLOAD ZIP and FILES ####

# EJScreen v 2.3 ftp site files as of 7/5/2024

# baseurl <- "https://gaftp.epa.gov/EJScreen/2024/2.30_July_useMe/"
# browseURL(baseurl)

#   data for   blockgroupstats
blockgroupstats_source_usa.zip   <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.csv.zip"
blockgroupstats_source_usa.csv   <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.csv"   # not for download - it will be available after unzip
blockgroupstats_source_state.zip <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip"
blockgroupstats_source_state.csv <- "EJSCREEN_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv"  # note inconsistent CAPS used there    # not for download - it will be available after unzip

blockgroupstats_source_usa.gdb.zip   <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb.zip"           # gdb is not essential
blockgroupstats_source_usa.gdb       <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb"
blockgroupstats_source_state.gdb.zip <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip"  # gdb is not essential
blockgroupstats_source_state.gdb     <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb"

#   data for   usastats and statestats
usastats_source.csv              <- "EJScreen_2024_BG_National_Lookup.csv"
statestats_source.csv            <- "EJScreen_2024_BG_State_Lookup.csv"
usastats_new_explained.xlsx      <- "EJScreen_2024_BG_Percentiles_Columns.xlsx"
statestats_new_explained.xlsx    <- "EJScreen_2024_BG_State_Percentiles_Columns.xlsx"

td <- tempdir()  # or...   localpath <-  "~/../EJ,,,,,"

fnames <- c(
  blockgroupstats_source_usa.zip,
  blockgroupstats_source_state.zip, # we need this only for its state versions of EJ indexes (used to create state pctiles)
  usastats_source.csv,
  statestats_source.csv,
  usastats_new_explained.xlsx,
  statestats_new_explained.xlsx
  # and the other  .csv files are found inside zip files after download
)

options(timeout = max(300, getOption("timeout"))) # default is 60 seconds 

curl::multi_download(urls = file.path(baseurl, fnames), 
                     destfiles = file.path(td, fnames))

########################################################### #

## if gdb needed:
needgdb = FALSE
if (needgdb) {
  
  curl::multi_download(
    urls = file.path(baseurl, c(blockgroupstats_source_usa.gdb.zip, blockgroupstats_source_state.gdb.zip)),
    destfiles = file.path(td, c(blockgroupstats_source_usa.gdb.zip, blockgroupstats_source_state.gdb.zip))
  )
  unzip(          file.path(td, blockgroupstats_source_usa.gdb.zip),    exdir =  td, overwrite = TRUE)
  unzip(          file.path(td, blockgroupstats_source_state.gdb.zip),  exdir =  td, overwrite = TRUE)
  gdbpath_usa   = file.path(td, blockgroupstats_source_usa.gdb)
  gdbpath_state = file.path(td, blockgroupstats_source_state.gdb)
  file.copy(gdbpath_usa, file.path(mydir, blockgroupstats_source_usa.gdb))
  file.copy(gdbpath_state, file.path(mydir, blockgroupstats_source_state.gdb))
  gdbpath_usa   = file.path(mydir, blockgroupstats_source_usa.gdb)
  gdbpath_state = file.path(mydir, blockgroupstats_source_state.gdb)
  # dir(mydir)
  file.exists(gdbpath_usa)
  file.exists(gdbpath_state)
  
}
######################################################### #

# ARCHIVE  .xlsx files in case needed ####

file.copy(file.path(td, usastats_new_explained.xlsx),   file.path(localfolder, usastats_new_explained.xlsx), overwrite = TRUE)
file.copy(file.path(td, statestats_new_explained.xlsx), file.path(localfolder, statestats_new_explained.xlsx), overwrite = TRUE)

########################################################### #

# UNZIP files ####

znames = fnames[ tools::file_ext(fnames) == "zip"]
pth = 1:length(znames)
for (i in 1:length(znames)) {
  pth[i] =  unzip(file.path(td, znames[i]),  exdir =  td, overwrite = TRUE)
}
# print(pth)
# print(basename(pth))
## check
# setequal(basename(pth), c(blockgroupstats_source_usa.csv, blockgroupstats_source_state.csv))
## TRUE 7/24
########################################################### #

# READ CSV FILES ####

getfile <- function(fname, folder = td) {as.data.frame(readr::read_csv(file.path(folder, fname)))}

blockgroupstats_new       <- getfile(blockgroupstats_source_usa.csv, td)
blockgroupstats_new_state <- getfile(blockgroupstats_source_state.csv, td) 

usastats_new   <- getfile(usastats_source.csv, td)
statestats_new <- getfile(statestats_source.csv, td)

rm(blockgroupstats_source_usa.zip,   blockgroupstats_source_usa.csv,
   blockgroupstats_source_state.zip, blockgroupstats_source_state.csv)
rm(usastats_source.csv, statestats_source.csv)
rm(pth, i, getfile, znames, fnames, baseurl, td)
gc()
# all that takes roughly 1 minute

########################################################### #

# archive unaltered versions (just for convenience, in case needed, to avoid downloading again)
# Later they will get saved for the package as data, or maybe put in pins board

save.image(file = file.path(localfolder, "save.image just after ftp downloads original colnames.rda"))
# load(file = file.path(localfolder, "save.image just after ftp downloads original colnames.rda"))

varnames <- c("blockgroupstats_new", 
              "blockgroupstats_new_state",
              "usastats_new",
              "statestats_new")

savearrow <- function(varnames, localfolder=getwd()) {
  ## to save as .arrow files
  ext <- ".arrow"
  fnames <- paste0(varnames, "_as_on_ftp", ext)
  localpaths  <- paste0(localfolder, '/', fnames)
  # require(arrow)
  for (i in 1:length(varnames)) {
    text_to_do <- paste0("x <- arrow::write_ipc_file(", 
                         varnames[i],", ",  
                         "sink = '", localpaths[i], "')"  
    )
    cat(" ", text_to_do, '\n')
    x <- eval(parse(text = text_to_do)) # executes the command
  }
}

if (interactive()) {
  ASARROW = askYesNo("Save copies as .arrow ? (not .rda)", default = FALSE)
} else {
  ASARROW = FALSE
}
if (ASARROW) {
  savearrow(varnames = varnames, localfolder = localfolder)
  file.exists(file.path(localfolder,"blockgroupstats_new_as_on_ftp.arrow"))
  file.exists(file.path(localfolder,"usastats_new_as_on_ftp.arrow"))
  file.exists(file.path(localfolder,"statestats_new_as_on_ftp.arrow"))
} else {
  ## to save as .rda files
  # ext <- ".rda"
  save(blockgroupstats_new,       file = file.path(localfolder, "blockgroupstats_new_as_on_ftp.rda"))  # about 118 MB on disk
  save(blockgroupstats_new_state, file = file.path(localfolder, "blockgroupstats_new_state_as_on_ftp.rda"))
  save(usastats_new,              file = file.path(localfolder, "usastats_new_as_on_ftp.rda"))
  save(statestats_new,            file = file.path(localfolder, "statestats_new_as_on_ftp.rda"))
  file.exists(file.path(localfolder,"blockgroupstats_new.rda"))
  file.exists(file.path(localfolder,"usastats_new.arrow"))
  file.exists(file.path(localfolder,"statestats_new.arrow"))
}
rm(varnames,   ASARROW)
browseURL(localfolder)
########################################################### #
########################################################### #
########################################################### #

# map_headernames check:

cbind(sort(names(blockgroupstats_new)[fixcolnames(names(blockgroupstats_new),'csv','r') == names(blockgroupstats_new)]))
# 
# [1,] "B_D2_DWATER"    
# [2,] "B_D2_NO2"       
# [3,] "B_D5_DWATER"    
# [4,] "B_D5_NO2"       
# [5,] "B_DISABILITYPCT"
# [6,] "B_DWATER"       
# [7,] "B_NO2"          
# [8,] "REGION"         
# [9,] "Shape_Length"   
# [10,] "T_D2_DWATER"    
# [11,] "T_D2_NO2"       
# [12,] "T_D5_DWATER"    
# [13,] "T_D5_NO2"       
# [14,] "T_DISABILITYPCT"
# [15,] "T_DWATER"       
# [16,] "T_NO2"      

########################################################### #

# RENAME ALL VARIABLES (COLUMN NAMES) ####
# WILL RENAME COLUMNS OF DATA.FRAME TO MATCH WHAT IS USED IN EJAM::   package  
# instead of sticking with variable names from FTP site.

names(blockgroupstats_new)       <-  fixcolnames(names(blockgroupstats_new),       oldtype = 'csv', newtype = 'r') # 
names(blockgroupstats_new_state) <-  fixcolnames(names(blockgroupstats_new_state), oldtype = 'csv', newtype = 'r') # 

names(usastats_new)    <- fixcolnames(names(usastats_new),   oldtype = "csv", newtype = "r")
names(statestats_new)  <- fixcolnames(names(statestats_new), oldtype = "csv", newtype = "r")

# Drop columns not needed by EJAM ####

##  drop all the map bin and text popup columns
##  drop all the percentile columns, since we lookup those using an aggregated (wtdavg) raw indicator value for each location analyzed.
# grep("^pctile|state.pctile|^bin", names(EJAM::blockgroupstats), value = TRUE) # those are not in blockgroupstats, not needed
cols2drop <- grep("^pctile|state.pctile|^bin", names(blockgroupstats_new), value = TRUE)
# extra ones not renamed by fixcolnames() since did not bother to put all Text and Bin columns names in map_headernames
cols2drop <- c(cols2drop, c("B_D2_DWATER", "B_D2_NO2", "B_D5_DWATER", "B_D5_NO2",  "B_DISABILITYPCT", "B_DWATER", "B_NO2", 
                            "T_D2_DWATER", "T_D2_NO2",  "T_D5_DWATER", "T_D5_NO2", "T_DISABILITYPCT", "T_DWATER", "T_NO2")
)  
cat("\n Dropping these columns: \n\n")
print(cbind(cols2drop))

blockgroupstats_new       <- blockgroupstats_new[,       !(names(blockgroupstats_new)       %in% cols2drop)]
blockgroupstats_new_state <- blockgroupstats_new_state[, !(names(blockgroupstats_new_state) %in% cols2drop)]

usastats_new              <- usastats_new[,              !(names(usastats_new)              %in% cols2drop)]
statestats_new            <- statestats_new[,            !(names(statestats_new)            %in% cols2drop)]

dim(blockgroupstats_new); dim(EJAM::blockgroupstats)

dim(usastats_new);   dim(EJAM::usastats)
dim(statestats_new); dim(EJAM::statestats)

# > dim(blockgroupstats_new); dim(EJAM::blockgroupstats)
# [1] 243022     81
# [1] 243021    112
# > dim(usastats_new);   dim(EJAM::usastats)
# [1] 103  52
# [1] 102  77
# > dim(statestats_new); dim(EJAM::statestats)
# [1] 5356   52
# [1] 5304   77

setdiff(          names(blockgroupstats_new), names(EJAM::blockgroupstats))
EJAM:::setdiff_yx(names(blockgroupstats_new), names(EJAM::blockgroupstats))

names(blockgroupstats_new)
# names(usastats_new)

############################################################## #
# rename fips column ?
names(blockgroupstats_new)       <- gsub("id", "OBJECTID", names(blockgroupstats_new))
names(blockgroupstats_new_state) <- gsub("id", "OBJECTID", names(blockgroupstats_new_state))

############################################################## #
# try create bgfips and bgid columns ####

table(EJAM:::fips_valid(blockgroupstats_new$OBJECTID))
blockgroupstats_new$bgfips <- fips_lead_zero(blockgroupstats_new$OBJECTID)
blockgroupstats_new_state$bgfips <- fips_lead_zero(blockgroupstats_new$OBJECTID)


table(EJAM:::fips_valid(blockgroupstats_new$bgfips))
# Warning message:
#   In fips_lead_zero(blockgroupstats_new$OBJECTID) :
#   270 fips had invalid number of characters (digits) or were NA values
# > table(EJAM:::fips_valid(blockgroupstats_new$OBJECTID))
# 
# FALSE   TRUE 
# 3403 239619 
# > blockgroupstats_new$bgfips <- fips_lead_zero(blockgroupstats_new$OBJECTID)
# Warning message:
#   In fips_lead_zero(blockgroupstats_new$OBJECTID) :
#   270 fips had invalid number of characters (digits) or were NA values
# > table(EJAM:::fips_valid(blockgroupstats_new$bgfips))
# 
# FALSE   TRUE 
# 3403 239619 

blockgroupstats_new$OBJECTID <- NULL
blockgroupstats_new_state$OBJECTID <- NULL

blockgroupstats_new$bgid <- bgpts$bgid[match(blockgroupstats_new$bgfips, bgpts$bgfips)]
blockgroupstats_new_state$bgid <- bgpts$bgid[match(blockgroupstats_new_state$bgfips, bgpts$bgfips)]
# > table(is.na(blockgroupstats$bgfips))
# 
# FALSE 
# 243021 
# > table(is.na(blockgroupstats$bgid))
# 
#  FALSE   TRUE    ***   
# 242335    686 

#### see why some are bad fips or na or missing...


# to be done...




########################################################### #
########################################################### #
################################################################################ #

# bgej ####

###### MOVE EJ INDEXES FROM blockgroupstats_new and blockgroupstats_new_state
## to  a consolidated bgej  table 

## merge US EJ with the state.EJ.DISPARITY columns ####
# from the  blockgroupstats_new_state  table

# dataload_from_pins("bgej")
# > names( bgej) 
# [1] "OBJECTID"                                "bgfips"                                  "bgid"
# [4] "ST"                                      "pop"                                     
#      "EJ.DISPARITY.pm.eo"                     
# [7] "EJ.DISPARITY.pm.supp"                    "EJ.DISPARITY.o3.eo"                      "EJ.DISPARITY.o3.supp"
# [10] "EJ.DISPARITY.dpm.eo"                     "EJ.DISPARITY.dpm.supp"                   "EJ.DISPARITY.cancer.eo"
# [13] "EJ.DISPARITY.cancer.supp"                "EJ.DISPARITY.resp.eo"                    "EJ.DISPARITY.resp.supp"
# [16] "EJ.DISPARITY.rsei.eo"                    "EJ.DISPARITY.rsei.supp"                  "EJ.DISPARITY.traffic.score.eo"
# [19] "EJ.DISPARITY.traffic.score.supp"         "EJ.DISPARITY.pctpre1960.eo"              "EJ.DISPARITY.pctpre1960.supp"
# [22] "EJ.DISPARITY.proximity.npl.eo"           "EJ.DISPARITY.proximity.npl.supp"         "EJ.DISPARITY.proximity.rmp.eo"
# [25] "EJ.DISPARITY.proximity.rmp.supp"         "EJ.DISPARITY.proximity.tsdf.eo" "EJ.DISPARITY.proximity.tsdf.supp"     
# [28] "EJ.DISPARITY.ust.eo"                     "EJ.DISPARITY.ust.supp"                   "EJ.DISPARITY.proximity.npdes.eo"
# [31] "EJ.DISPARITY.proximity.npdes.supp"       "state.EJ.DISPARITY.pm.eo"                "state.EJ.DISPARITY.pm.supp"
# [34] "state.EJ.DISPARITY.o3.eo"                "state.EJ.DISPARITY.o3.supp"              "state.EJ.DISPARITY.dpm.eo"
# [37] "state.EJ.DISPARITY.dpm.supp"             "state.EJ.DISPARITY.cancer.eo"            "state.EJ.DISPARITY.cancer.supp"
# [40] "state.EJ.DISPARITY.resp.eo"              "state.EJ.DISPARITY.resp.supp"            "state.EJ.DISPARITY.rsei.eo"
# [43] "state.EJ.DISPARITY.rsei.supp"       "state.EJ.DISPARITY.traffic.score.eo"  "state.EJ.DISPARITY.traffic.score.supp"  
# [46] "state.EJ.DISPARITY.pctpre1960.eo"    "state.EJ.DISPARITY.pctpre1960.supp"  "state.EJ.DISPARITY.proximity.npl.eo"    
# [49] "state.EJ.DISPARITY.proximity.npl.supp" "state.EJ.DISPARITY.proximity.rmp.eo" "state.EJ.DISPARITY.proximity.rmp.supp"
# [52] "state.EJ.DISPARITY.proximity.tsdf.eo"    "state.EJ.DISPARITY.proximity.tsdf.supp"  "state.EJ.DISPARITY.ust.eo"
# [55] "state.EJ.DISPARITY.ust.supp"  "state.EJ.DISPARITY.proximity.npdes.eo" "state.EJ.DISPARITY.proximity.npdes.supp"

blockgroupstats_new_state <- blockgroupstats_new_state[, c("bgid", "bgfips", names_ej, names_ej_supp)]
data.table::setDT(blockgroupstats_new_state)
data.table::setDT(blockgroupstats_new)
data.table::setnames(blockgroupstats_new_state,
                     old =  c("bgid", "bgfips", names_ej, names_ej_supp), 
                     new =  c("bgid", "bgfips", c(names_ej_state, names_ej_supp_state))
)
# > all.equal(blockgroupstats_new$bgid, blockgroupstats_new_state$bgid)
# [1] TRUE
data.table::setDF(blockgroupstats_new)
data.table::setDF(blockgroupstats_new_state)

bgej <- data.table(
  blockgroupstats_new[ , c("bgid", "bgfips", 
                           "ST", "pop", 
                           names_ej, 
                           names_ej_supp)],
  blockgroupstats_new_state[, c(names_ej_state, 
                                names_ej_supp_state)]
)
# all.equal(data.frame(bgej)[, names_ej], blockgroupstats_new[,names_ej])

rm(blockgroupstats_new_state)
blockgroupstats_new[, c(names_ej, names_ej_supp)] <- NULL
setDT(blockgroupstats_new)
setcolorder(blockgroupstats_new, c("bgid", "bgfips", "statename", "ST", "countyname", "REGION",
                                   "pop",
                                   names_d, names_e), before = 1)

############################################### # 


## bgej is left in globalenv by this script -
# later acan Save bgej to pins board as .arrow file
#     # using script in    datacreate_pins.R



########################################################### ############################################################ #


########################################################### #
########################################################### #

# save work in progress as IMAGE ####

save.image(file = file.path(localfolder, "save.image work in progress on blockgroupstats.rda"))

################################################################################ #

########################################################### #
########################################################### #

# Obtain and add RACE ETHNICITY SUBGROUPS   ####

### get via script in
#   "EJAM/data-raw/datacreate_blockgroupstats2.3_add_d_acs22columns.R"


#     "pcthisp"                  "pctba"                    "pctaa"                    "pctaiana"                
# [11] "pctnhpia"                 "pctotheralone"            "pctmulti"                 "pctwa"    "hisp"
# [16] "ba"                       "aa"                       "aiana"  "nhpia"  "otheralone"  
# [21] "multi"  "wa"  "pctnhba"  "pctnhaa"  "pctnhaiana"              
# [26] "pctnhnhpia"               "pctnhotheralone"          "pctnhmulti"  "pctnhwa"                  "nhba"  
# [31] "nhaa"                     "nhaiana"                  "nhnhpia" "nhotheralone"             "nhmulti"  
# [36] "nhwa"                
# "under18"                  "over17" "male"                     "female"                  
# [41] "poor"                     "pctpoor"                  "lan_universe" "lan_eng_na"               "lan_spanish" 
# [46] "lan_ie"                   "lan_api"                  "spanish_li" "pctspanish_li"            "ie_li" 
# [51] "pctie_li"                 "api_li"                   "pctapi_li" "other_li"                 "pctother_li" 
# [56] "occupiedunits"            "ownedunits"               "pctownedunits"            "lifexyears"               "pctmale"
# [61] "pctfemale"                "percapincome"             "pctunder18"               "pctover17"      








# to be done...








########################################################### #
########################################################### #


### Get or create these columns: 
# 
# State EJ count at80th is missing ####
# "state.count.ej.80up"      "state.count.ej.80up.supp"
# 
#  already has these:
# "count.ej.80up"      "count.ej.80up.supp"






# to be done...









################################################################################ #
################################################################################ #

################################################################################ #

# ARCHIVE as IMAGE ####

# save.image(file = file.path(localfolder, "save.image work on NEW blockgroupstats usastats statestats.rda"))

################################################################################ #

