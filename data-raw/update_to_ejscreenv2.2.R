#  ######################################################################## #
#  
# Script to download the new 7/2023 version 2.2 EJScreen  
#  - block group data with US and State percentiles, 
#  - lookup tables for US and State percentile cutoffs and means,
#  - variable name lists (and their descriptions) that are in block group dataset 
#  - variable name lists (and their descriptions) that are in the API outputs, 
#  - etc. 
#
# and save that info as 

#  - new block group data files in blockgroupstats

#  - new lookup tables in usastats and statestats

#  - new variable name lists info in EJAMejscreenapi::map_headernames 
#    (made from xlsx of same name; used by EJAMejscreenapi and probably by EJAM too), 
#    a table of variable names, friendly versions of the names, metadata, etc.

#  - new variable name lists (created from map_headernames, or at least matching it)
#    like names_e, names_e_friendly, etc. (used by EJAM, made by names_of_indicators_creationscript.R)
#    and/or do that just as namez$e, namez$e_friendly, etc. (and recode to use that instead of names_e, etc.)
#    and/or do that only in map_headernames 

#  - metadata on new variables (indicators) like 
#    category/type of indicator, number of significant digits to display, name of units for each indicator, etc.

#  ######################################################################## #

#    csv/gdb files: Actual full dataset as csv files and geodatabases at https://gaftp.epa.gov/EJScreen/2023/

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

#    API: Data dictionary of API variable names: 
#
#    - 2023_07  ver 2.2 <https://ejscreen.epa.gov/mapper/ejsoefielddesc1.html>
#    - old/mini ver 2.2 <https://ejscreen.epa.gov/mapper/ejsoefielddesc.html>

#    API: Simple web interface (e.g., to try it out and see examples of URL for queries and API outputs):
#    
#    - 2023_07         ver 2.2         <https://ejscreen.epa.gov/mapper/ejscreenapi1.html>
#    - old/mini ver 2.2 (fewer indicators) <https://ejscreen.epa.gov/mapper/ejscreenapi.html>
#    
#    API: REST endpoint (for code to make a GET or POST request):
#    
#    - 2023_07  ver 2.2 <https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=>
#    - old/mini ver 2.2 <https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=>

#################################################################### #
# save or download? save xlsx? ####

# data.table format is used below for the blockgroupstats starting point, bg2.2_csv or bg2.2_gdb
# data.frame format is used below for all other files/info

savexl <- FALSE
## Locally saved xlsx files were here:
# dir("~/../../R/mysource/EJAMejscreenapi/data-raw/", pattern = "xls")
downloadnow <- FALSE
## Local downloads of files were here:
# dir("~/../EJ 2021/EJScreen 2023")
# dir("~/../EJ 2021/EJScreen 2023/ejscreen2023_07")
## Check also recently downloaded csv,xlsx,zip files in local Downloads folder:
# x = file.info(list.files(path = "~/../Downloads/", full.names = T, pattern = 'csv|xls|zip'))
# rownames(x) <- gsub('.*Downloads/','', rownames(x))
# rownames(x)[x$mtime > "2023-06-01"]
# rm(x)

############################ #
# SET WORKING DIRECTORY to EJAMejscreenapi ####
# probably want to start in the source package folder that is EJAMejscreenapi which has the map_headernames dataset and related info

setwd(file.path(Sys.getenv("R_USER"), "EJAMejscreenapi")); getwd() # just make sure this is the right one
# .--------------------------------------- ####


#################################################################### #
# gdb: Actual full dataset as geodatabase ####

#   exported from ArcGIS gdb,  downloaded from EJScreen ftp site 
#   of v2.2 EJScreen dataset US, 7/17/2023
#  1st, manually download the geodatabase file and open in ArcGIS, and export the attribute table as csv file
# Files are listed at https://origin-awswest-www.epa.gov/ejscreen/download-ejscreen-data 
# https://gaftp.epa.gov/EJScreen/2023/
#   EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip
#   EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip

bg2.2_gdb      <- data.table::fread("~/../EJ 2021/EJScreen 2023/ejscreen2023_07/EJSCREEN_Full_ExportTable1.csv" )
bg2.2state_gdb <- data.table::fread("~/../EJ 2021/EJScreen 2023/ejscreen2023_07/EJSCREEN_StatePc_ExportTable1.csv")

# confirmed same names used by us and state gdb attribute tables of full block group data
all.equal(names(bg2.2_gdb), names(bg2.2state_gdb)) # true
dim(bg2.2_gdb) == dim(bg2.2state_gdb) # true

gdb_table_export_example_v2.2 <- data.frame(
  gdbname2.2 = names(bg2.2_gdb),
  example = t(bg2.2_gdb[1,])
)
gdb_table_export_example_v2.2_STATES <- data.frame(
  gdbname2.2 = names(bg2.2state_gdb),
  example = t((bg2.2state_gdb[1,]))
)
if (savexl) {
  writexl::write_xlsx(gdb_table_export_example_v2.2,
                      path =   "./data-raw/gdb_table_export_example_v2.2.xlsx")
  writexl::write_xlsx(gdb_table_export_example_v2.2_STATES,
                      path =   "./data-raw/gdb_table_export_example_v2.2_STATES.xlsx")
}
## Dont save complete bg dataset here. Get it from the csv download version.

#################################################################### #
# csv: Actual full dataset as the csv file downloaded from the EJScreen ftp site ####

# of v2.2 EJScreen dataset US, 7/17/2023

url    <- "https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv.zip"
url_st <- "https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip"
csvname    <- "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv"
csvname_st <- 'EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv'
mydir <- "~/../Downloads"
mydir <- "~/../EJ 2021/EJScreen 2023"

if (downloadnow) {
  # download and unzip US file
  tfile <- tempfile()
  download.file(url = url, destfile = tfile)  # zipname <- "EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv.zip"
  unzip(tfile, exdir = mydir)
  # download and unzip ST file (has state percentiles for each indicator in each block group, and repeats all the other columns that are not percentiles too)
  tfile <- tempfile()
  download.file(url = url_st, destfile = tfile)  # zipname <- "EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip"
  unzip(tfile, exdir = mydir)
}
fpath    <- file.path(mydir, csvname)
fpath_st <- file.path(mydir, csvname_st)
# if was downloaded but not unzipped
if (!file.exists(fpath) & file.exists(paste0(fpath, '.zip'))) {unzip(paste0(fpath, '.zip'), exdir = mydir)}
if (!file.exists(fpath_st) & file.exists(paste0(fpath_st, '.zip'))) {unzip(paste0(fpath_st, '.zip'), exdir = mydir)}
bg2.2_csv      <- data.table::fread(file = fpath)
bg2.2state_csv <- data.table::fread(file = fpath_st)
# > dim(bg2.2_csv)
# [1] 243021    224
# > dim(bg2.2state_csv)
# [1] 243021    224

csv_example_v2.2 = data.frame(
  csvname2.2 = names(bg2.2_csv),
  example = t(bg2.2_csv[1,])
)
csv_example_v2.2state = data.frame(
  csvname2.2 = names(bg2.2state_csv),
  example = t(bg2.2state_csv[1,])
)
rm(csvname, csvname_st, mydir, url, url_st, fpath, fpath_st, tfile)

# save spreadsheet
if (savexl) {
  writexl::write_xlsx(csv_example_v2.2,      path = "./data-raw/csv_example_v2.2.xlsx")
  writexl::write_xlsx(csv_example_v2.2state, path = "./data-raw/csv_example_v2.2state.xlsx")
}

# save(bg2.2state_csv, file = 'bg2.2state_csv.rda') # just save to take a look later?

################################## #
# Confirmed that the version downloaded as a csv file on the ftp site 
# is identical to the version was manually exported from the gdb
# > all.equal(bg2.2_gdb, bg2.2_csv)
# [1] TRUE
# all.equal(csv_example_v2.2$csvname2.2, names(bg2.2_csv))
################################# #


#################################################################### #
# will Create blockgroupstats only AFTER map_headernames is updated to include all the new indicator names



#################################################################### #
# List of changes in variable names made during updates v2.1 to v2.2 ####
#
# https://www.epa.gov/system/files/documents/2023-06/ejscreen-2-2-column-name-changes.pdf  

# # cleaned that and saved it as a table here: 
myfile <- "ejscreen-2-2-column-name-changes.xlsx"
mydir <- "~/../../R/mysource/EJAM/data-raw/"
# EJAMejscreenapi/data-raw/
changes <- as.data.frame(readxl::read_xlsx(file.path(mydir, myfile)) )

#   *** Should use this to match full new list to 
#  some old names that had a lot of metadata on them already in map_headernames


#################################################################### #
# Data dictionary for downloadable dataset ####

#   downloaded from EJScreen ftp site 
#   of v2.2 EJScreen dataset US, 7/17/2023
myurl <- "https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_Columns.xlsx"
fname <- "EJSCREEN_2023_BG_Columns.xlsx"

# using download.file() to get this does not work, for some reason, so download to downloads folder manually
# tdir <- tempdir()
tdir = "~/../Downloads"  # getwd()
fpath <- file.path(tdir, fname)
if (downloadnow) {
  # using download.file() to get this does not work, for some reason
  # x <- download.file(myurl, destfile = fpath)
  cat("\n manually save the file in ", tdir, '\n\n')
  browseURL(myurl) # manually save it in 
  junk <- readline(prompt = 'manually save the file')
  rm(junk)
}

csvnames2.2_defined          <- as.data.frame(readxl::read_xlsx(fpath, sheet = 1, skip = 1))
csvnames2.2_state_defined    <- as.data.frame(readxl::read_xlsx(fpath, sheet = 2, skip = 1))
lookupnames2.2_defined       <- as.data.frame(readxl::read_xlsx(fpath, sheet = 3, skip = 1))
lookupnames2.2_state_defined <- as.data.frame(readxl::read_xlsx(fpath, sheet = 4, skip = 1))
# csvnames2.2_defined$`Column Names` == csvnames2.2_state_defined$`GDB Fieldname` # confirmed identically named for US file and State percentiles version.
# lookupnames2.2_defined$`GDB Fieldname` == lookupnames2.2_state_defined$`GDB Fieldname` # confirmed identically named

names(csvnames2.2_defined)       <- c("n", "csvname2.2", "description")
names(csvnames2.2_state_defined) <- c("n", "csvname2.2", "description")
names(lookupnames2.2_defined)    <- c("n", "csvname2.2", "description")
names(lookupnames2.2_state_defined) <- c("n", "csvname2.2", "description")

# file.copy(fpath, file.path('./data-raw/', fname))
csvnames2.2_defined          <- (csvnames2.2_defined)
lookupnames2.2_defined       <- (lookupnames2.2_defined)
csvnames2.2_state_defined    <- (csvnames2.2_state_defined)
lookupnames2.2_state_defined <- (lookupnames2.2_state_defined)

# save spreadsheets
if (savexl) {
  writexl::write_xlsx(   csvnames2.2_defined, "./data-raw/csv_vars_defined_v2.2.xlsx")
  writexl::write_xlsx(lookupnames2.2_defined, "./data-raw/usastats_lookup_vars_defined_v2.2.xlsx")
  ## Do not need to save lookup ones. 
}

rm(fpath,tdir,fname,myurl) 

# .--------------------------------------- ####

#################################################################### #
# get lookup tables of US and State percentiles cutoffs ####
#  Get new tables.
#  The gdb files are the only place that provides lookup table of US and State percentiles

# if (downloadnow) {  }
# https://gaftp.epa.gov/EJScreen/2023/
#   EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.gdb.zip   has the state and us percentile lookup tables.
#   EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip does not have lookup tables, only US gdb does.
#   v2.2 EJScreen dataset US, 7/17/2023
#  1st, manually download the geodatabase file and open in ArcGIS, and export the attribute table as csv file
# explained at https://origin-awswest-www.epa.gov/ejscreen/download-ejscreen-data 

# as.data.frame(readr::read_csv())

usastats2.2   <- read.csv('~/../EJ 2021/EJScreen 2023/ejscreen2023_07/usastats2023.csv', stringsAsFactors = F)
statestats2.2 <- read.csv('~/../EJ 2021/EJScreen 2023/ejscreen2023_07/statestats2023.csv', stringsAsFactors = F)
# > unique(statestats2.2$REGION)
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME"
# [21] "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI"
# [41] "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY" "PR"
# > unique(usastats2.2$REGION)
# [1] "USA"
if (savexl) {
  write.csv(usastats2.2, "./data-raw/usastats2.2.xlsx")
  write.csv(statestats2.2, "./data-raw/statestats2.2.xlsx")
}
## confirmed usa and state ones use same exact names (only the description is different, for REGION)
all(names(usastats2.2) == names(statestats2.2)) # true
# Confirmed all lookup names are in block group dataset (except obvious ones)
setdiff(names(usastats2.2), csv_example_v2.2$csvname2.2)
## [1] "PCTILE"
# Confirmed "mean" is the only PCTILE value other than 0-100, since std.dev is not a row
usastats2.2$PCTILE[is.na(as.numeric(usastats2.2$PCTILE))]
# [1] "mean"


#################################################################### #
# .--------------------------------------- ####


# done with EJScreen block group data and lookup tables, 
# now move on to API variables.


rm(mydir, myfile)

#################################################################### #
# Data dictionary of API variable names per table on website  ####

savexl <- FALSE

for (fullapi in c(TRUE,FALSE)) {
  if (fullapi) {
    myurl <- "https://ejscreen.epa.gov/mapper/ejsoefielddesc1.html" # longer version, (300 vs 148) more indicators
  }  else {
    myurl <- "https://ejscreen.epa.gov/mapper/ejsoefielddesc.html"  # shorter version, fewer indicators
  }
  library(rvest)
  html <- read_html(myurl)
  api <- html %>% 
    html_node("table") %>% 
    html_table()
  api
  api <- as.data.frame(api)
  
  ################################## # 
  # fix all their typos
  spelling::spell_check_text(api$Description )$word
  api$Description[unlist(spelling::spell_check_text(api$Description )$found)]
  x <- api$Description
  x <- gsub("Perentile", "Percentile", x)
  x <- gsub("Superfund Side", "Superfund Sites", x)
  x <- gsub("Communites", "Communities", x)
  x <- gsub("Communties", "Communities", x)
  x <- gsub("Dsiadvantaged", "Disadvantaged", x)
  # "EJ" 
  # "Indo"     
  x <- gsub("languagues", "languages", x)
  x <- gsub("Overlappting", "Overlapping", x)
  x <- gsub("Overlapttion", "Overlapping", x)
  x <- gsub("Pecent", "Percent", x)
  x <- gsub("Perentage", "Percentage", x)
  # "RMP"     
  x <- gsub("Schoolss", "Schools" , x)
  x <- gsub("Slavi", "Slavic", x)
  x <- gsub("Toxci", "Toxic", x)
  # "Toxics"
  api$Description <- x 
  names(api) <- gsub("Description", "description", names(api))
  names(api) <- gsub("JSON Variable", "apiname2.2", names(api))
  names(api) <- gsub("Section", "apitype2.2", names(api))
  rm(x)
  # spelling::spell_check_text(api$Description )$word
  #  done fixing typos 
  ################################## # 
  # save spreadsheets
  if (fullapi) {
    apifull_v2.2_descriptions <- api
    fname = "./data-raw/apifull_v2.2_descriptions.xlsx"
  } else {
    apimini_v2.2_descriptions <- api
    fname = "./data-raw/apimini_v2.2_descriptions.xlsx"
  }
  if (savexl) {writexl::write_xlsx(api, path = fname)}
}

rm(api, html, myurl, fullapi, fname)



#################################################################### #
# API actual output example - not always responding? ####

# This could be replaced by ejscreenapi() when that is working with the new format.
# web UI is here: "https://ejscreen.epa.gov/mapper/ejscreenapi1.html"
# Different output fields if areatype=blockgroup, areaid=some fips, then  it returns areatype and areadid, 
# versus if point and radius specified... returns instead   geometry.spatialReference.wkid, geometry.x , geometry.y  
x <- "https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=471570074001&geometry=&distance=&unit=9035&areatype=blockgroup&areaid=471570074001&f=pjson"
x <-  'https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-100.12966913507033,"y":36.65314015144932}&distance=1&unit=9035&areatype=&areaid=&f=pjson' 
ej.data <- httr::GET(x)
ej.data <- try(data.table::as.data.table(jsonlite::fromJSON(rawToChar(ej.data$content))))
ej.data <- ej.data$data
demographics <- as.data.frame(ej.data[[1]], stringsAsFactors = FALSE)
main <- as.data.frame(t(unlist(ej.data[[2]]))) # awkward
extras <- as.data.frame(t(unlist(ej.data[[3]]))) # awkward # as.data.frame( ej.data[[3]], stringsAsFactors = FALSE)
ej.data <- cbind(main,demographics,extras)
ej.data <- ej.data[, !duplicated(names(ej.data))] # because extras and main duplicated 12 variables, like totalPop and areaid
apifull_example_v2.2 <- data.frame(apiname2.2 = names(ej.data), # c(names(demographics), names(main), names(extras)), 
                                   example = t(ej.data),
                                   stringsAsFactors = FALSE, row.names = NULL)
rm(x, main, extras, demographics, ej.data)
# save spreadsheet
if (savexl) {writexl::write_xlsx(apifull_example_v2.2, path = "./data-raw/apifull_example_v2.2.xlsx")}


stop('save image here?')


# save.image('~/../Downloads/large work in progress file.rda') #xxx




# .--------------------------------------- ####




##################### ##
# COMPARE NAMES IN DATASETS ####
#  ie names of variables as found in various files 

#################################################################### #



##  compare count of names from gdb, csv, etc ####
# and descriptions of variables, for us (same for state) 

#################### #
NCOL(usastats2.2)            #  52     # lookup tables are missing PRE1960, and OBJECTID name differed
NROW(lookupnames2.2_defined) #  53

NCOL(bg2.2_csv)                #  224   
NCOL(bg2.2_gdb)                #  224
NROW(csvnames2.2_defined)      #  225  # has  "Shape" and also 3 names differed

NROW(apifull_v2.2_descriptions)#  300  # has more variables than blockgroup data 
NROW(apifull_example_v2.2)     #  300  # main and extras had duplicated variables in what API returns, but those were removed here ***
#################### #

## US vs ST: variable names are identical in us vs state versions of files ####
#
all.equal(names(bg2.2_csv),  names(bg2.2state_csv)) # TRUE
all.equal(csvnames2.2_defined$csvname2.2, csvnames2.2_state_defined$csvname2.2) # TRUE
all.equal(csv_example_v2.2$csvname2.2,    csv_example_v2.2state$csvname2.2) # TRUE
all.equal(names(bg2.2_gdb),  names(bg2.2state_gdb)) # TRUE
all.equal(gdb_table_export_example_v2.2$gdbname2.2, gdb_table_export_example_v2.2_STATES$gdbname2.2) # TRUE
all.equal(lookupnames2.2_defined$csvname2.2, lookupnames2.2_state_defined$csvname2.2) # TRUE
# API does not have separate files or lists of US versus State indicators, so their variable names make note of whether it is for US or ST pctile.
#
# check these: 
# csv, gdb, examples, descriptions, and lookup tables and their descriptions

# blockgroupstats and examples are identical for csv vs gdb  ####
all.equal(  names(bg2.2_csv),  names(bg2.2_gdb)) # TRUE
all.equal(  names(bg2.2_csv),               csv_example_v2.2$csvname2.2) # TRUE
all.equal(  names(bg2.2_csv),  gdb_table_export_example_v2.2$gdbname2.2) # TRUE

# but table of variable descriptions is different: 
all.equal(  names(bg2.2_csv),     csvnames2.2_defined$csvname2.2  )  # 224 vs 225
# and names in lookup table are different:
all.equal(  names(bg2.2_csv),  lookupnames2.2_defined$csvname2.2  )  # 224 vs 53




## ** csv NOT MATCHING list of names ####
#   found in the list of descriptions of those indicators:
#   (for US & state versions, equally so): 
#
#  *** manually fixing: OBJECTID vs OID_ being fixed manually, and 
#  *** manually fixing: and not sure why EXCEED2_COUNT_80, EXCEED2_COUNT_80_SUP  got used in definitions list. error?
#  and "Shape" appears only in definitions list but not actual blockgroup data as non-gdb file, just attribute table. 
setdiff(csvnames2.2_defined$csvname2.2, csv_example_v2.2$csvname2.2)
# [1] "OBJECTID"    "Shape"   "EXCEED2_COUNT_80"     "EXCEED2_COUNT_80_SUP"
EJAM::setdiff_yx(csvnames2.2_defined$csvname2.2, csv_example_v2.2$csvname2.2)
# [1] "OID_"                "EXCEED_COUNT_80"     "EXCEED_COUNT_80_SUP"


## ** lookup table has a couple differences from all those ####
# "OBJECTID"  is called   "OID_"  in the lookup table itself, for some reason, as exported from gdb.
# and lookup tables are missing PRE1960 which is a raw count used to create PRE1960PCT 
# *** So, easiest is to manually rename OID_ to OBJECTID before trying to rename it to id for use in R as EJAM::usastats or statestats so this does not matter. 

setdiff(names(usastats2.2), csvnames2.2_defined$csvname2.2)
## [1] "OID_"   "PCTILE"
setdiff(lookupnames2.2_defined$csvname2.2, names(usastats2.2) )
# [1] "OBJECTID" "PRE1960" 
EJAM::setdiff_yx(lookupnames2.2_defined$csvname2.2, names(usastats2.2) )
# [1] "OID_"

## ** lookup definitions = blockgroup table names, except obvious differences ####

all.equal(lookupnames2.2_defined$csvname2.2, names(usastats2.2)) # 53 vs 52
# PRE1960 is in lookupnames2.2_defined  but not in usastats2.2

## ** caution: data tables US vs STATE versions use same names!!  ####

# US v State: Identical names are used in list of descriptions of US and State indicators,
# so this will require code that assigns different names or otherwise keeps track of which is which. 


## ** caution: US vs State lookup tables descriptions lists - names are same! ####
# Code must handle that carefully (or else need to use names in statestats that are the state version of the percentile)
# (and descriptions if used for some glossary should distinguish between them). 

  all.equal(lookupnames2.2_defined[ -2, ], lookupnames2.2_state_defined[ -2, ])
## [1] TRUE
# lookupnames2.2_defined[2,]
## n csvname2.2                                    description
## 2 2     REGION Processing group identifier (USA for National)   # this is the only difference
# lookupnames2.2_state_defined[2,]
## n csvname2.2                                       description
## 2 2     REGION Processing group identifier (State abbreviations) # this is the only difference
#


#################################################################### #
## ** caution: Island Areas are not in statestats lookup ####
# lookup tables lack Island Areas but blockgroup datasets include them 
# 
# Code that looks up percentiles will need to allow for a REGION not being found and still return something like NA for each percentile.
#
# 
# ISLAND AREAS ARE NOT IN THIS FILE
 
  setdiff(unique(bg2.2_gdb$ST_ABBREV), unique(statestats2.2$REGION))
# [1] "VI" "MP" "AS" "GU"
# 
# unique(usastats2.2$REGION)
# [1] "USA"
  # unique(statestats2.2$REGION)
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" # [16] "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" # [31] "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" 
# [46] "VT" "VA" "WA" "WV" "WI" "WY" "PR"  **** this does not have  VI MP AS GU 
# > unique(bg2.2_csv$ST_ABBREV)
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" # [16] "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" # [31] "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" 
# [46] "VT" "VA" "WA" "WV" "WI" "WY" "PR" "VI" "MP" "AS" "GU"
# > unique(bg2.2state_csv$ST_ABBREV)
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" # [16] "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" # [31] "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" 
# [46] "VT" "VA" "WA" "WV" "WI" "WY" "PR" "VI" "MP" "AS" "GU"
# > unique(bg2.2_gdb$ST_ABBREV)
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" # [16] "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" # [31] "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" 
# [46] "VT" "VA" "WA" "WV" "WI" "WY" "PR" "VI" "MP" "AS" "GU"
# > unique(bg2.2state_gdb$ST_ABBREV)
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" # [16] "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" # [31] "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT"
# [46] "VT" "VA" "WA" "WV" "WI" "WY" "PR" "VI"   "MP" "AS" "GU"


#################################################################### #
# COMPARE NAMES IN API OUTPUTS ####
##  names in API outputs vs names in blockgroupstats (full dataset) ####

## API names match apifull_v2.2_descriptions ####
# Actual API output fields (from the full API) are identical to those in the apifull_v2.2_descriptions table
# taken from  "https://ejscreen.epa.gov/mapper/ejsoefielddesc1.html" 
# except that
## but geometry field (or what it is turned into here: geometry.x, geometry.y, geometry.spatrialReference.wkid)  
## not in the output of what I saved from the API outputs ####
 setdiff(apifull_example_v2.2$apiname2.2, apifull_v2.2_descriptions$apiname2.2)
 #  "geometry.spatialReference.wkid" "geometry.x"                     "geometry.y" 
 EJAM::setdiff_yx(apifull_example_v2.2$apiname2.2, apifull_v2.2_descriptions$apiname2.2)
# [1] "geometry"  "areaid"   "areatype"  if example is from a point and radius not blockgroup fips
# dont bother with mini version example. assume the apimini_v2.2_descriptions describes all the fields.


## API output provides 300 extra indicators vs bg data or differ ####
# 300 or so are not in csv or gdb files of blockgroup data, or are named differently since US vs State !

z = setdiff(apifull_v2.2_descriptions$apiname2.2, csvnames2.2_defined$csvname2.2)
length(z) # 300 

y = EJAM::setdiff_yx(apifull_v2.2_descriptions$apiname2.2, csvnames2.2_defined$csvname2.2)
length(y) # 225
rm(z,y)




####################################################### #
## old names in mini API vs newer full API outputs ####

# Look at what got added and what got renamed/ replaced by newer (full not mini) API:   

# but also note the file listing changes: https://www.epa.gov/system/files/documents/2023-06/ejscreen-2-2-column-name-changes.pdf  

# also, new API as provided via ejscreenapi() now provides  geometry.spatialReference.wkid, geometry.x, geometry.y

# see list of names in full API output that are not in mini outputs (300 vs 148):
#setdiff(apifull_v2.2_descriptions$apiname2.2, apimini_v2.2_descriptions$apiname2.2)

# see list of names named differently now, that were in mini API outputs but no longer in full/newer API out:
#
# renamed <- EJAM::setdiff_yx(apifull_v2.2_descriptions$apiname2.2, apimini_v2.2_descriptions$apiname2.2)
# renamed <- sort(renamed) 

# none of the old variable names are used in the csv or gdb files, which is good
# renamed %in% names(bg2.2_csv)

# renamed 
#  MINOR as PEOPCOL,
#  _P_ as _P2_ and _P5_, and 
#  "N_D_INDEX" as N_D_DEMOGIDX2 and 5
# and added RSEI, and many other indicators for API outputs

# "RAW_D_INDEX"   "RAW_D_MINOR"  
#   "N_D_INDEX"     "N_D_INDEX_PER"         "N_D_MINOR"     "N_D_MINOR_PER" 
#   "S_D_INDEX"     "S_D_INDEX_PER"          "S_D_MINOR"    "S_D_MINOR_PER" 
# "N_P_CANCER"    "N_P_DIESEL"    "N_P_LEAD"   "N_P_NPDES" "N_P_NPL"  "N_P_O3" "N_P_PM25" "N_P_RESP" "N_P_RMP" "N_P_RSEI_AIR" "N_P_TRAFFIC" "N_P_TSDF" "N_P_UST"     
# "S_P_CANCER"    "S_P_DIESEL"    "S_P_LEAD"   "S_P_NPDES" "S_P_NPL"  "S_P_O3" "S_P_PM25" "S_P_RESP" "S_P_RMP" "S_P_RSEI_AIR"  "S_P_TRAFFIC"   "S_P_TSDF" "S_P_UST"   

#  renamed[grepl('MINOR', renamed)]; sort(grep("PEOPCOL", apifull_v2.2_descriptions$apiname2.2, value=TRUE))
# [1] "N_D_MINOR"      "N_D_MINOR_PER"     "RAW_D_MINOR"     "S_D_MINOR"     "S_D_MINOR_PER"
#   were renamed as: 
# [1] "N_D_PEOPCOLOR"  "N_D_PEOPCOLOR_PER" "RAW_D_PEOPCOLOR" "S_D_PEOPCOLOR" "S_D_PEOPCOLOR_PER"

# renamed[grepl('RAW_D_INDEX', renamed)]   # split into 2-factor and 5-factor demog indexes
# [1] "RAW_D_INDEX"  #  this was replaced by  "RAW_D_DEMOGIDX2" and "RAW_D_DEMOGIDX5" 

# renamed[grepl('S_', renamed)]  # these are State Percentiles 
# [1] "S_D_INDEX"  "S_D_INDEX_PER"   # these 2 were replaced by   "S_D_DEMOGIDX2_PER"  "S_D_DEMOGIDX5_PER"
# "S_D_MINOR"  "S_D_MINOR_PER" # these 2 were renamed from MINOR to PEOPCOL
#         these were replaced by _P2_ and _P5_  names, for the EO2 and the new supplemental EJ indexes: 
#   "S_P_CANCER" "S_P_DIESEL" "S_P_LEAD" "S_P_NPDES" "S_P_NPL" "S_P_O3" "S_P_PM25" "S_P_RESP" 
#  "S_P_RMP" "S_P_RSEI_AIR" "S_P_TRAFFIC" "S_P_TSDF" "S_P_UST"

# apifull_v2.2_descriptions[apifull_v2.2_descriptions$apiname2.2 %in%  grep("S_P[0-9]", apifull_example_v2.2$apiname2.2, value=T), ]

# > renamed[grepl('N_', renamed)]  # same thing as S_D and S_P but for National percentiles not State
# [1] "N_D_MINOR"     "N_D_INDEX"     "N_D_MINOR_PER" "N_D_INDEX_PER"
# "N_P_LEAD"      "N_P_DIESEL"    "N_P_CANCER"    "N_P_RESP"     
# [9] "N_P_TRAFFIC"   "N_P_NPDES"     "N_P_NPL"       "N_P_RMP"       "N_P_TSDF"      "N_P_O3"        "N_P_PM25"      "N_P_UST"      
# [17] "N_P_RSEI_AIR" 

# ** Ensure that any name mapping uses the newer ones.


# rm gdb stuff 

all.equal(bg2.2_gdb, bg2.2_csv)
rm(bg2.2_gdb)
all.equal(bg2.2state_csv, bg2.2state_gdb)
rm(bg2.2state_gdb)
gc()

 save.image('~/../Downloads/large work in progress file.rda') 
 

# .--------------------------------------- ####

# **** CREATE NEW map_headernames ####
#  the table of metadata on all indicators / variables / column headers 


#################################################################### #

# start from csv_example_v2.2 since it exactly matches actual names in US csv file. 
all.equal(names(bg2.2_csv), csv_example_v2.2$csvname2.2) # true

 m <- csv_example_v2.2 
names(m) <- gsub("example", "csv_example", names(m))

# these two tables differ only slightly, so we can first merge those tables and 
#  address those differences one by one when merging.

xx = csvnames2.2_defined[csvnames2.2_defined$csvname2.2 %in% m$csvname2.2, ]
m <- merge(m, xx, all = TRUE)

# setdiff(csvnames2.2_defined$csvname2.2, csv_example_v2.2$csvname2.2)
# # [1] "OBJECTID" "Shape"   "EXCEED2_COUNT_80"     "EXCEED2_COUNT_80_SUP"
# setdiff(  csv_example_v2.2$csvname2.2,  csvnames2.2_defined$csvname2.2)
# # [1] "OID_"               "EXCEED_COUNT_80"     "EXCEED_COUNT_80_SUP"

# csvnames2.2_defined[csvnames2.2_defined$csvname2.2 %in% c('OBJECTID', 'EXCEED2_COUNT_80', 'EXCEED2_COUNT_80_SUP'),]
# n           csvname2.2                                            description
# 1     1             OBJECTID               Unique ID for block group in geodatabase
# 222 222     EXCEED2_COUNT_80           Number of EJ Indexes exceeding 80 percentile
# 223 223 EXCEED2_COUNT_80_SUP Number of Supplemental Indexes exceeding 80 percentile

# >   csvnames2.2_defined[ csvnames2.2_defined$csvname2.2 %in%  c("EXCEED_COUNT_80", grep("P_D2", names(bg2.2_csv), value=T) ) , ]
# n    csvname2.2                                        description
# 94   94     P_D2_PM25     Percentile for Particulate Matter 2.5 EJ Index
# 96   96    P_D2_OZONE                      Percentile for Ozone EJ Index
# 98   98    P_D2_DSLPM Percentile for  Diesel particulate matter EJ Index
# 100 100   P_D2_CANCER    Percentile for  Air toxics cancer risk EJ Index
# 102 102     P_D2_RESP  Percentile for Air toxics respiratory HI EJ Index
# 104 104 P_D2_RSEI_AIR      Percentile for Toxic Releases to Air EJ Index
# 106 106    P_D2_PTRAF          Percentile for Traffic proximity EJ Index
# 108 108    P_D2_LDPNT                 Percentile for Lead paint EJ Index
# 110 110     P_D2_PNPL        Percentile for Superfund proximity EJ Index
# 112 112     P_D2_PRMP     Percentile for RMP Facility Proximity EJ Index
# 114 114    P_D2_PTSDF  Percentile for Hazardous waste proximity EJ Index
# 116 116      P_D2_UST  Percentile for Underground storage tanks EJ Index
# 118 118    P_D2_PWDIS       Percentile for Wastewater discharge EJ Index

# checked some examples and clearly these are reporting how many of the EJ indexes >= 80, 
# for either standard demog index (2-factor) or supplemental one (5-factor)

# bg2.2_csv[,   c("EXCEED_COUNT_80",     grep("P_D2",  names(bg2.2_csv), value=T) ), with=FALSE]
# bg2.2_csv[,   c("EXCEED_COUNT_80_SUP", grep( "P_D5", names(bg2.2_csv), value=T) ), with=FALSE]

m$csv_descriptions_name <- m$csvname2.2
m$csv_descriptions_name[m$csvname2.2 == "OID_"] <- "OBJECTID" 
m$csv_descriptions_name[m$csvname2.2 == "EXCEED_COUNT_80"] <- "EXCEED2_COUNT_80"
m$csv_descriptions_name[m$csvname2.2 == "EXCEED_COUNT_80_SUP"] <- "EXCEED2_COUNT_80_SUP"
m$description[m$csvname2.2 == "OID_"] <- "Unique ID for block group in geodatabase"
m$description[m$csvname2.2 == "EXCEED_COUNT_80"] <- "Number of EJ Indexes exceeding 80 percentile"
m$description[m$csvname2.2 == "EXCEED_COUNT_80_SUP"] <- "Number of Supplemental Indexes exceeding 80 percentile"
# note we will need to create a separate STATE version of each and for all the percentile-related indicators.

m[m$csvname2.2 %in% c( "OID_" ,"EXCEED_COUNT_80"     ,"EXCEED_COUNT_80_SUP" ) ,]
# names(m)
m <- m[,c('n', 'csvname2.2', "csv_descriptions_name", "description", "csv_example")]







########################
# now merge in the R field names ####

# names(map_headernames)
table(m$csvname2.2 %in% map_headernames$ejscreen_csv)
# FALSE  TRUE 
# 133    91 
# table(m$csvname2.2 %in% map_headernames$oldnames)
# FALSE  TRUE 
# 178    46 

library(EJAMejscreenapi)
dim(m)
m <- merge(m, map_headernames[,c('newnames_ejscreenapi', "ejscreen_csv")], 
           by.x = "csvname2.2", by.y = 'ejscreen_csv', 
           all.x = TRUE, all.y = FALSE)

# that added duplicate rows since there are some dupes in map_headernames$ejscreen_csv
m <- unique(m)

names(m) <- gsub('newnames_ejscreenapi', 'rname', names(m))


#########################
#  now merge in or otherwise add rnames that did not already easily match from map_headernames 

more_csvnames <- setdiff(m$csvname2.2, map_headernames$newnames_ejscreenapi)
more_rnames <- setdiff(map_headernames$newnames_ejscreenapi, m$csvname2.2) 

m = merge(m, map_headernames[, c("csvlongname", "newnames_ejscreenapi")], 
          by.x = 'description', by.y = 'csvlongname', all.x = TRUE, all.y = FALSE)  
m = unique(m)

table(is.na(m$rname), is.na(m$newnames_ejscreenapi))
canadd = is.na(m$rname) & !is.na(m$newnames_ejscreenapi)
# this adds 29 we did not have
m$rname[canadd] <- m$newnames_ejscreenapi[canadd]
m$newnames_ejscreenapi <- NULL
m <- m[ , c('n', "csvname2.2", "csv_descriptions_name", "rname", "description", "csv_example")]


more_csvnames <- setdiff(m$csvname2.2, map_headernames$newnames_ejscreenapi)
more_rnames <- setdiff(map_headernames$newnames_ejscreenapi, m$csvname2.2) 

 grep("P_D", more_csvnames, value = T)

####################3 #
# first check the environmental indicators and map rnames of those:

cbind(EJAM::names_e, fixnames_to_type(EJAM::names_e,  "newnames_ejscreenapi", "ejscreen_csv"))

# [,1]              [,2]        
# [1,] "pm"              "PM25"      
# [2,] "o3"              "OZONE"     
# [3,] "cancer"          "CANCER"    
# [4,] "resp"            "RESP"      
# [5,] "dpm"             "DSLPM"     
# [6,] "pctpre1960"      "PRE1960PCT"
# [7,] "traffic.score"   "PTRAF"     
# [8,] "proximity.npl"   "PNPL"      
# [9,] "proximity.rmp"   "PRMP"      
# [10,] "proximity.tsdf"  "PTSDF"     
# [11,] "proximity.npdes" "PWDIS"     
# [12,] "ust"             "UST"       
# [13,] "rsei"            "RSEI_AIR"  
csv_names_e <- fixnames_to_type(EJAM::names_e,  "newnames_ejscreenapi", "ejscreen_csv")
# [1] "PM25"       "OZONE"      "CANCER"     "RESP"       "DSLPM"      "PRE1960PCT" "PTRAF"      "PNPL"       "PRMP"      
# [10] "PTSDF"      "PWDIS"      "UST"        "RSEI_AIR" 
cbind(grep("CANCER" , m$csvname2.2, value=T))
# check them in groups:
# for (blah in olde)   {
#   hits = grep(blah, more_csvnames, value = T)
#   print(hits)
#   print(length(hits))
# }
# that shows that inconsistent names used for lead paint aka percent pre 1960,
# where unlike other indicators of envt, it uses LDPNT for some and PRE1960PCT for the raw score:
  cbind(c("PRE1960PCT", grep('LDPNT', more_csvnames,value = T)), grep('CANCER', more_csvnames,value = T))
# [,1]         [,2]         
# [1,] "PRE1960PCT" "CANCER"    ****** 
# [2,] "D2_LDPNT"   "D2_CANCER"  
# [3,] "D5_LDPNT"   "D5_CANCER"  
# [4,] "B_LDPNT"    "B_CANCER"   
# [5,] "B_D2_LDPNT" "B_D2_CANCER"
# [6,] "B_D5_LDPNT" "B_D5_CANCER"
# [7,] "T_LDPNT"    "T_CANCER"   
# [8,] "T_D2_LDPNT" "T_D2_CANCER"
# [9,] "T_D5_LDPNT" "T_D5_CANCER"
# [10,] "P_LDPNT"    "P_CANCER"   
# [11,] "P_D2_LDPNT" "P_D2_CANCER"
# [12,] "P_D5_LDPNT" "P_D5_CANCER"

m[m$csvname2.2 %in% (cbind(grep("CANCER" , m$csvname2.2, value=T))), c(3:5)]
#     csv_descriptions_name              rname      MANUALLY ADD THESE                          description
# 9                  CANCER             cancer                                        Air toxics cancer risk
# 10              D2_CANCER               <NA>  EJ.DISPARITY.cancer                              Air toxics cancer risk EJ Index
# 11              D5_CANCER               <NA>  EJ.DISPARITY.cancer.supp                     Air toxics cancer risk Supplemental Index
# 34               B_CANCER         bin.cancer                          Map color bin for  Air toxics cancer risk
# 35            B_D2_CANCER               <NA>   bin.EJ.DISPARITY.cancer         Map color bin for  Air toxics cancer risk EJ Index
# 36            B_D5_CANCER               <NA>   bin.EJ.DISPARITY.cancer.supp   Map color bin for  Air toxics cancer risk Supplemental Index
# 83               T_CANCER pctile.text.cancer                         Map popup text for  Air toxics cancer risk
# 84            T_D2_CANCER               <NA> "pctile!!!.text.EJ.DISPARITY.cancer"         Map popup text for  Air toxics cancer risk EJ Index
# 85            T_D5_CANCER               <NA> "pctile!!!.text.EJ.DISPARITY.cancer.supp"  Map popup text for  Air toxics cancer risk Supplemental Index
# 147              P_CANCER      pctile.cancer                        Percentile for  Air toxics cancer risk
# 148           P_D2_CANCER               <NA>  pctile.EJ.DISPARITY.cancer.eo             Percentile for  Air toxics cancer risk EJ Index
# 149           P_D5_CANCER               <NA>  pctile.EJ.DISPARITY.cancer.supp   Percentile for  Air toxics cancer risk Supplemental Index


# text popup variables were all prefixed with pctile.text... except that 
#   map_headernames had been using text.EJ... which is not consistent; and ejscreenformulas used pctile.text.EJ... 
# So I will fix map_headernames to be consistent. Although, it does not really matter since 
# the fields text.EJ... never get used by EJAMejscreenapi or EJAM since they are specific to one blockgroup,
# and we always calculate that for a buffered area and dont need to report it for a single bg.
# 
# "text.EJ.DISPARITY.dpm.supp"                     "text.EJ.DISPARITY.o3.supp"                     
# [431] "text.EJ.DISPARITY.pctpre1960.supp"              "text.EJ.DISPARITY.pm.supp"                     
# [433] "text.EJ.DISPARITY.proximity.npdes.supp"         "text.EJ.DISPARITY.proximity.npl.supp"          
# [435] "text.EJ.DISPARITY.proximity.rmp.supp"           "text.EJ.DISPARITY.proximity.tsdf.supp"         
# [437] "text.EJ.DISPARITY.resp.supp"                    "text.EJ.DISPARITY.rsei.eo"                     
# [439] "text.EJ.DISPARITY.rsei.supp"                    "text.EJ.DISPARITY.traffic.score.supp"          
# [441] "text.EJ.DISPARITY.ust.supp"      



 m[order(m$rname),c("csvname2.2", 'rname')]

for (i in 1:length(csv_names_e )) {
  root_csv <-   csv_names_e[i] ;  root_csv <- gsub('PRE1960PCT',  'LDPNT', root_csv)
  root_r <- (EJAM::names_e)[i]
  
  m$rname[m$csvname2.2 == paste0('D2_', root_csv) ] <- paste0(  "EJ.DISPARITY.", root_r)
  m$rname[m$csvname2.2 == paste0('D5_', root_csv) ] <- paste0(  "EJ.DISPARITY.", root_r, '.supp')
  
  m$rname[m$csvname2.2 == paste0('B_D2_', root_csv) ] <- paste0(  "bin.EJ.DISPARITY.", root_r)
  m$rname[m$csvname2.2 == paste0('B_D5_', root_csv) ] <- paste0(  "bin.EJ.DISPARITY.", root_r, ".supp")
  
  # these two are fixing map_headernames to use pctile.text not just text as prefix:
  m$rname[m$csvname2.2 == paste0('T_D2_', root_csv) ] <- paste0(  "pctile.text.EJ.DISPARITY.", root_r)
  m$rname[m$csvname2.2 == paste0('T_D5_', root_csv) ] <- paste0(  "pctile.text.EJ.DISPARITY.", root_r, ".supp")
  
  m$rname[m$csvname2.2 == paste0('P_D2_', root_csv) ] <- paste0(  "pctile.EJ.DISPARITY.", root_r, ".eo") # note this used .eo unlike others above
  m$rname[m$csvname2.2 == paste0('P_D5_', root_csv) ] <- paste0(  "pctile.EJ.DISPARITY.", root_r, ".supp")
  
}

#################### #
# now look at demographic indicators - DEMOGIDX_2 and DEMOGIDX_5 are
# two separate indicators, like CANCER,etc., but without any EJ indexes,
# so nothing with D2 or D5 in the name:
# 
# [1] "DEMOGIDX_2"   "B_DEMOGIDX_2" "T_DEMOGIDX_2" "P_DEMOGIDX_2"
# >  grep("DEMOGIDX_5", more_csvnames, value = T)
# [1] "DEMOGIDX_5"   "B_DEMOGIDX_5" "T_DEMOGIDX_5" "P_DEMOGIDX_5"  
# >  grep("CANCER", grep("D[25]", more_csvnames, value=T, invert = T), value = T)
# [1] "CANCER"       "B_CANCER"     "T_CANCER"     "P_CANCER"

  m[order(m$rname),c("csvname2.2", 'rname')]
                                   # MANUALLY ADD
  #   56         B_LIFEEXPPCT       bin.lowlifex                                   <NA>
  #   105        T_LIFEEXPPCT       pctile.text.lowlifex                                     <NA>
  
  #   206          DEMOGIDX_5      Demog.Index.Supp                                    <NA>
  #   69         B_DEMOGIDX_5        bin.Demog.Index.Supp                                  <NA>
  #   118        T_DEMOGIDX_5    pctile.text.Demog.Index.Supp                                      <NA>
  #   182        P_DEMOGIDX_5      pctile.Demog.Index.Supp                                    <NA>
  
  #   162        P_DEMOGIDX_2      pctile.Demog.Index                                    <NA>
  
  #   138 EXCEED_COUNT_80_SUP       "count.ej.80up.supp"                                   <NA>
  
m$rname[m$csvname2.2 == "B_LIFEEXPPCT"] <- 'bin.lowlifex'
m$rname[m$csvname2.2 == "T_LIFEEXPPCT"] <- 'pctile.text.lowlifex'

m$rname[m$csvname2.2 == "DEMOGIDX_5"] <- 'Demog.Index.Supp'
m$rname[m$csvname2.2 == "B_DEMOGIDX_5"] <- 'bin.Demog.Index.Supp'
m$rname[m$csvname2.2 == "T_DEMOGIDX_5"] <- 'pctile.text.Demog.Index.Supp'
m$rname[m$csvname2.2 == "P_DEMOGIDX_5"] <- 'pctile.Demog.Index.Supp'

m$rname[m$csvname2.2 == "P_DEMOGIDX_2"] <- 'pctile.Demog.Index'
m$rname[m$csvname2.2 == "EXCEED_COUNT_80_SUP"] <- 'count.ej.80up.supp'
# m$rname[m$csvname2.2 == "EXCEED_COUNT_80"] <- 'count.ej.80up'



  # note this that had been in map_headernames: 
  # "pctile.text.Demog.Index.eo" 
  # to remove the .eo
m$rname[m$csvname2.2 == "T_DEMOGIDX_2"] <- "pctile.text.Demog.Index" 
  

m[order(m$rname),c("csvname2.2", 'rname')]

# now all the csvnames have a correct rname in m, at least for USA-specific not state-specific names






############################################################  # 
# CREATE STATE SPECIFIC VERSIONS OF NAMES where needed in csv names 

# a lot of lack of matches are because the STATE versions of csv names are not there yet
# since the US and ST versions of csvname are the same in the csv files. 
# 

# which columns in bg dataset have a different us vs st version?
# any where names start with ...
# D2_, D5_, which are raw EJ indexes
# P_, percentile  
# B_, bin for map color based on percentile
# T_, text for map popup
# and
# EXCEED_COUNT_80     and  EXCEED_COUNT_80_SUP  



# Although EJAM does not need to have or use the blockgroup indicators that are
# raw EJ, percentile, bin, or popup text (which are the ones where US and STATE versions differ),
# we still will store those header names in m or map_headernames, to be able to 
# keep track just in case. 
# 
## RENAME THE ACTUAL STATE BLOCKGROUPSTATS CSV FILE DATASET HEADERS WITH S_ PREFIX ####
# TO DISTINGUISH FROM NATIONAL.
# The api names use both N_ and S_ prefixes, but wont add N_ prefix to us blockgroupstats here. 
statespecific_csvnames    <- grep("^(D2.*|D5.*|P_.*|B_.*|T_.*)", names(bg2.2_csv), value = T)
cbind(names(bg2.2state_csv), gsub("^(D2.*|D5.*|P_.*|B_.*|T_.*)", "S_\\1", names(bg2.2state_csv)))
names(bg2.2state_csv)    <-  gsub("^(D2.*|D5.*|P_.*|B_.*|T_.*)", "S_\\1", names(bg2.2state_csv))

# RENAME indicators in the state descriptions and examples files ####
# to use state-specific names

fixthese <- csvnames2.2_state_defined$csvname2.2 %in% statespecific_csvnames 
csvnames2.2_state_defined$csvname2.2 <- gsub("^(D2.*|D5.*|P_.*|B_.*|T_.*)", "S_\\1",  
                                             csvnames2.2_state_defined$csvname2.2)
csvnames2.2_state_defined$description[fixthese] <- paste0("State ", csvnames2.2_state_defined$description[fixthese])
csvnames2.2_state_defined$description <- gsub("State State", "State", csvnames2.2_state_defined$description) # fix where had accidentally added/replaced twice
csvnames2.2_state_defined 

csv_example_v2.2state$csvname2.2 <-   gsub("^(D2.*|D5.*|P_.*|B_.*|T_.*)", "S_\\1",  
                                           csv_example_v2.2state$csvname2.2)
################# # 



m

mstate <- m[m$csvname2.2 %in% statespecific_csvnames, ]

# mstate$apiname <- gsub("^N_", "S_", mstate$apiname) # not created yet
mstate$description <- paste0("State ", mstate$description)
mstate$rname <- paste0("state.", mstate$rname)
mstate$csvname2.2 <- paste0("S_", mstate$csvname2.2)
mstate$csv_descriptions_name <- paste0("S_", mstate$csv_descriptions_name)

# ##################################### #
# save.image(file = '~/../Downloads/work in progress.rda')
# ##################################### #

m <- m[ , c("rname", "csvname2.2", "description",  "csv_example", "csv_descriptions_name")]  # "apiname", 
mstate <- mstate[ , c("rname", "csvname2.2", "description",  "csv_example", "csv_descriptions_name")] # "apiname",

mstate

# intersect(m$rname, mstate$rname)

m <- rbind(m, mstate)
 
rm(mstate)




# ##################################### #
save(m, file = '~/../Downloads/m.rda')
save.image(file = '~/../Downloads/work in progress.rda')
# ##################################### #








####################################################### # 
#  NOW PULL IN API NAMES IN A NEW COLUMN BUT MATCHED INTO THE CORRECT ROWS


## API names (variable names) are all totally different than CSV file blockgroupstats variable names 

intersect(csv_example_v2.2$csvname2.2, apifull_example_v2.2$apiname2.2)

# make a table that includes API and CSV names? -- both sets of names ####
# you could just merge but indicators named differently by api vs csv (synonyms) would get put in 2 separate rows:
# x <- merge(apifull_v2.2_descriptions , csvnames2.2_defined, by.x = "apiname2.2", by.y = "csvname2.2", all.x = TRUE, all.y = TRUE)
# names(x) <- c('varname', 'description.api', 'apitype2.2', "n.csv", 'description.csv' )
# x = x[,c('n.csv', 'varname', 'description.csv', 'description.api', 'apitype2.2')]
# # x
#  x$apiname = x$varname
#  x$apiname[is.na(x$description.api)] <- NA
#  x$csvname = x$varname
#  x$csvname[is.na(x$description.csv)] <- NA
# x= x[ , c('varname', 
#           'apiname', 
#           "csvname", 
#           "apitype2.2", "description.api", 
#           "description.csv", "n.csv")]

# ******    BUT - How to identify the synonyms, to merge those as a single row? ####


# csvnames2.2_defined[      csvnames2.2_defined$description %in% grep('Traffic', csvnames2.2_defined$description, ignore.case = T, value = T), 2:3]
# csvnames2.2_state_defined[csvnames2.2_state_defined$description %in% grep('Traffic', csvnames2.2_state_defined$description, ignore.case = T, value = T), 2:3]

csvnames2.2_defined_us_and_state <- unique(rbind(csvnames2.2_defined, csvnames2.2_state_defined))

csvnames2.2_defined_us_and_state[      csvnames2.2_defined_us_and_state$description %in% grep('Traffic', csvnames2.2_defined_us_and_state$description, ignore.case = T, value = T), 2:3]
#       csvname2.2                                                   description
# 37         PTRAF                                             Traffic proximity  **shared by both raw
#
# 57      D2_PTRAF                                    Traffic proximity EJ Index     EJ raw is only in csv not api  
# 58      D5_PTRAF                          Traffic proximity Supplemental Index     EJ raw is only in csv not api
# 87       P_PTRAF                              Percentile for Traffic proximity  **shared by both
# 106   P_D2_PTRAF                     Percentile for Traffic proximity EJ Index  **shared by both
# 107   P_D5_PTRAF           Percentile for Traffic proximity Supplemental Index  **shared by both
# 136      B_PTRAF                           Map color bin for Traffic proximity    ONLY IN csv not api 
# 155   B_D2_PTRAF                  Map color bin for Traffic proximity EJ Index    ONLY IN csv not api 
# 156   B_D5_PTRAF        Map color bin for Traffic proximity Supplemental Index    ONLY IN csv not api
# 185      T_PTRAF                          Map popup text for Traffic proximity    ONLY IN csv not api
# 204   T_D2_PTRAF                 Map popup text for Traffic proximity EJ Index    ONLY IN csv not api
# 205   T_D5_PTRAF       Map popup text for Traffic proximity Supplemental Index    ONLY IN csv not api
#
# 282   S_D2_PTRAF                              State Traffic proximity EJ Index     EJ raw is only in csv not api
# 283   S_D5_PTRAF                    State Traffic proximity Supplemental Index     EJ raw is only in csv not api
# 312    S_P_PTRAF                        State Percentile for Traffic proximity  **shared by both
# 331 S_P_D2_PTRAF               State Percentile for Traffic proximity EJ Index  **shared by both
# 332 S_P_D5_PTRAF     State Percentile for Traffic proximity Supplemental Index  **shared by both
# 361    S_B_PTRAF                     State Map color bin for Traffic proximity    ONLY IN csv not api
# 380 S_B_D2_PTRAF            State Map color bin for Traffic proximity EJ Index    ONLY IN csv not api
# 381 S_B_D5_PTRAF  State Map color bin for Traffic proximity Supplemental Index    ONLY IN csv not api
# 410    S_T_PTRAF                    State Map popup text for Traffic proximity    ONLY IN csv not api
# 429 S_T_D2_PTRAF           State Map popup text for Traffic proximity EJ Index    ONLY IN csv not api
# 430 S_T_D5_PTRAF State Map popup text for Traffic proximity Supplemental Index    ONLY IN csv not api

  
# apifull_v2.2_descriptions[apifull_v2.2_descriptions$description %in% grep(
#   +     'Traffic', apifull_v2.2_descriptions$description, ignore.case = T, value = T), 1:2]
#          apiname2.2                                                 description
# 53    RAW_E_TRAFFIC                                           Traffic Proximity  **shared by both raw
#
# 148     N_E_TRAFFIC                       National Average of Traffic Proximity      avg is ONLY in API 
# 171 N_E_TRAFFIC_PER                    National Percentile of Traffic Proximity  **shared by both
# 184    N_P2_TRAFFIC           National Percentile of Traffic Proximity EJ Index  **shared by both
# 197    N_P5_TRAFFIC National Percentile of Traffic Proximity Supplemental Index  **shared by both
#
# 76      S_E_TRAFFIC                          State Average of Traffic Proximity      avg is ONLY in API 
# 99  S_E_TRAFFIC_PER                       State Percentile of Traffic Proximity  **shared by both
# 112    S_P2_TRAFFIC              State Percentile of Traffic Proximity EJ Index  **shared by both
# 125    S_P5_TRAFFIC    State Percentile of Traffic Proximity Supplemental Index  **shared by both
#


# - *** naming schemes differ (for vs of, etc: e.g.,"Percentile for Traffic proximity" vs  "National Percentile of Traffic Proximity" ) 
# - csv has map bin and map popup text which API does not
# - API has Averages (in state and in usa) which csv file does not
#  
# in csv:           "Percentile for Traffic proximity" and "State Percentile for Traffic proximity"   etc
# in api:  "National Percentile of Traffic Proximity"  and "State Percentile of Traffic Proximity"   etc

   


 # start to create this apiname column


m$apiname <- NA 

api_names_e <- c('PM25', 'O3', 'CANCER', 'RESP', 'DIESEL', 'LEAD', 'TRAFFIC',  'NPL', 'RMP' , 'TSDF', 'NPDES', 'UST', 'RSEI')

# cbind(csv_names_e, api_names_e)
# csv_names_e  api_names_e
# [1,] "PM25"       "PM25"     
# [2,] "OZONE"      "O3"       
# [3,] "CANCER"     "CANCER"   
# [4,] "RESP"       "RESP"     
# [5,] "DSLPM"      "DIESEL"   
# [6,] "PRE1960PCT" "LEAD"     
# [7,] "PTRAF"      "TRAFFIC"  
# [8,] "PNPL"       "NPL"      
# [9,] "PRMP"       "RMP"      
# [10,] "PTSDF"      "TSDF"     
# [11,] "PWDIS"      "NPDES"    
# [12,] "UST"        "UST"      
# [13,] "RSEI_AIR"   "RSEI"  

for (i in 1:length(api_names_e)) {
  apiroot = api_names_e[i]
  csvroot = csv_names_e[i];  if (csvroot == 'PRE1960PCT') {csvroot <-   'LDPNT' }
  
  m$apiname[m$csvname2.2 == csvroot]                  <- paste0('RAW_E_', apiroot)
  
  m$apiname[m$csvname2.2 == paste0('P_',    csvroot)] <- paste0(  'N_E_', apiroot, '_PER')
  m$apiname[m$csvname2.2 == paste0('P_D2_', csvroot)] <- paste0( 'N_P2_', apiroot)
  m$apiname[m$csvname2.2 == paste0('P_D5_', csvroot)] <- paste0( 'N_P5_', apiroot)
  
  m$apiname[m$csvname2.2 == paste0('S_P_',    csvroot)] <- paste0(  'S_E_', apiroot, '_PER')
  m$apiname[m$csvname2.2 == paste0('S_P_D2_', csvroot)] <- paste0( 'S_P2_', apiroot)
  m$apiname[m$csvname2.2 == paste0('S_P_D5_', csvroot)] <- paste0( 'S_P5_', apiroot)

  # m$apiname[m$csvname2.2 == 'PTRAF']      <- RAW_E_TRAFFIC
  # m$apiname[m$csvname2.2 == 'P_PTRAF']    <-   N_E_TRAFFIC_PER
  # m$apiname[m$csvname2.2 == 'P_D2_PTRAF'] <-  N_P2_TRAFFIC
  # m$apiname[m$csvname2.2 == 'P_D5_PTRAF'] <-  N_P5_TRAFFIC  
}
m$apiname[m$csvname2.2 == "PRE1960PCT"] <- 'RAW_E_LEAD'
m[,c("apiname","csvname2.2",'rname','description')]

api_need <- setdiff(apifull_example_v2.2$apiname2.2, m$apiname)

  table(api_need %in% map_headernames$oldnames) # 73 are already in map_headernames$oldnames
  table(api_need %in% map_headernames$newnames_ejscreenapi) 
  #  and 4  in $ejscreen_api, 
  # which can be pulled into m via rnames matching to newnames_ejscreenapi


#################################################################### #

# First try to find which are already in map_headernames, get the R variable names and 
#  then for those not in map_headernames figure out in groups what they are synonyms of, add R names to those,
#  and merge on rnames.

# api_need
  m$apiname2 <- map_headernames$ejscreen_api[  match(m$rname , map_headernames$newnames_ejscreenapi) ]
  
m$apiname[is.na(m$apiname) & !is.na(m$apiname2)] <- m$apiname2[is.na(m$apiname) & !is.na(m$apiname2)]

m$apiname2 <- NULL

api_need <- setdiff(apifull_example_v2.2$apiname2.2, m$apiname)

# lots of api names are not really needed for EJAM to work but 
# we want to add them to map_headernames (aka m) 
# anyway, so when we try to rename variables, it finds them and just renames them to nothing new
# but has a long name (description_api or description?) 

rm(x,y,root_r, out, pts, olde, root_csv, myradius, m2, i, more_csvnames, hits, df, csvroot,blah, canadd)

# 
# 
# # ##################################### #
# save(m, file = '~/../Downloads/m.rda')
# save.image(file = '~/../Downloads/work in progress.rda')
# # ##################################### #

x = apifull_v2.2_descriptions[ apifull_v2.2_descriptions$apiname2.2 %in% api_need, ]
  table(x$apiname2.2 %in% map_headernames$ejscreen_api)
# 
# FALSE  TRUE 
# 141    50

  api_to_get <- intersect(x$apiname2.2, map_headernames$ejscreen_api)
api_to_get

  map_headernames[map_headernames$ejscreen_api %in% api_to_get,
                  c("ejscreen_api", "newnames_ejscreenapi", "longname_tableheader")]
found = map_headernames[map_headernames$ejscreen_api %in% api_to_get,
                        c("ejscreen_api", "newnames_ejscreenapi" )]
  names(found) <- c('apiname', 'rname')
# found

  x = intersect(map_headernames[map_headernames$ejscreen_api %in% found$apiname, 5] , m$rname)
  # [1] "count.NPL"   "count.TSDF"  "rsei"        "pctile.rsei"
   x 
   
 # MANUALLY ENTER THESE 2
 m$apiname[m$csvname2.2 == 'TSDF_CNT'] <- 'NUM_TSDF'
 m$apiname[m$csvname2.2 == 'NPL_CNT'] <- 'NUM_NPL'
 
# fix these  problems
 m$apiname[!grepl('_AIR', m$apiname)] <- gsub('RSEI', 'RSEI_AIR', m$apiname[!grepl('_AIR', m$apiname)])
 m
 
 # fix some more manually
 
 q = "LIFE"
 csvnames2.2_defined_us_and_state[grepl(q, csvnames2.2_defined_us_and_state$csvname2.2, ignore.case = T), ]
 csv_example_v2.2[grepl(q, csv_example_v2.2$csvname2.2),]
 apifull_v2.2_descriptions[grepl(q, apifull_v2.2_descriptions$apiname2.2), ]
 apifull_example_v2.2[grepl(q, apifull_example_v2.2$apiname2.2),]
 m[grepl(q,m$csvname2.2),]
 
    
 m$apiname[m$csvname2.2 == 'LIFEEXPPCT'] <- 'RAW_D_LIFEEXP'  # % low life expectancy or  Limited Life Expectancy
 m$apiname[m$csvname2.2 ==   'P_LIFEEXPPCT'] <- 'N_D_LIFEEXP_PER' # Percentile for Low Life Expectancy  or National Percentile of Limited Life Expectancy
 m$apiname[m$csvname2.2 == 'S_P_LIFEEXPPCT'] <- 'S_D_LIFEEXP_PER' 

 m$apiname[m$csvname2.2 == 'EXCEED_COUNT_80']    <- NA # not in api outputs
 m$apiname[m$csvname2.2 == 'EXCEED_COUNT_80_SUP'] <- NA  # not in api outputs

 m$apiname[m$csvname2.2 == 'OBJECTID'] <- 'ID'
 
 # obsolete apinames that got into m, to fix
 
 m[m$csvname2.2 %in% changes$name2.2 & nchar(m$apiname) > 0 & !is.na(m$apiname) & !(m$apiname %in% apifull_v2.2_descriptions$apiname2.2), 'apiname'] 
 # [1] "RAW_D_MINOR"   "RAW_D_INDEX"   "N_D_MINOR_PER" "N_D_INDEX_PER"
 
 m$apiname[m$apiname == 'RAW_D_MINOR' & !is.na(m$apiname)] <- 'RAW_D_PEOPCOLOR'
 
 m$apiname[m$csvname2.2 ==   'P_PEOPCOLORPCT' & !is.na(m$apiname)] <- 'N_D_PEOPCOLOR_PER' 
 m$apiname[m$csvname2.2 == 'S_P_PEOPCOLORPCT' & !is.na(m$apiname)] <- 'S_D_PEOPCOLOR_PER'
 
 
 m$apiname[m$csvname2.2 == 'DEMOGIDX_2' & !is.na(m$apiname)] <- 'RAW_D_DEMOGIDX2'
 m$apiname[m$csvname2.2 == 'DEMOGIDX_5' & !is.na(m$apiname)] <- 'RAW_D_DEMOGIDX5'
 
 m$apiname[m$csvname2.2 == 'P_DEMOGIDX_2' & !is.na(m$apiname)] <- 'N_D_DEMOGIDX2_PER'
 m$apiname[m$csvname2.2 == 'P_DEMOGIDX_5' & !is.na(m$apiname)] <- 'N_D_DEMOGIDX5_PER'
 
 m$apiname[m$csvname2.2 == 'S_P_DEMOGIDX_2' & !is.na(m$apiname)] <- 'S_D_DEMOGIDX2_PER'
 m$apiname[m$csvname2.2 == 'S_P_DEMOGIDX_5' & !is.na(m$apiname)] <- 'S_D_DEMOGIDX5_PER'
 
 
 


# these are apinames that might possibly need to get matched to csv names but none seem like they are in csv. 
 # but might want in m, to help with friendly naming. 

apifull_v2.2_descriptions[!(apifull_v2.2_descriptions$apiname2.2 %in% m$apiname) & (apifull_v2.2_descriptions$apitype2.2 %in% c('General information', 'Socioeconomic Indicators')) & !(grepl("average", apifull_v2.2_descriptions$description, ignore.case = T)), ]
# 212          NUM_WATERDIS                               Number of Water Discharge Facilities      General information   (not in csv of blockgroups)
# 213           NUM_AIRPOLL                                 Number of Air Pollution Facilities      General information (not in csv of blockgroups)
# 214        NUM_BROWNFIELD                                              Number of Brownfields      General information (not in csv of blockgroups)
# 215               NUM_TRI                                 Number of Toxic Release Facilities      General information   ** (not in csv of blockgroups)
# 216            NUM_SCHOOL                                                  Number of Schools      General information (not in csv of blockgroups)
# 217          NUM_HOSPITAL                                                Number of Hospitals      General information (not in csv of blockgroups)
# 218            NUM_CHURCH                                           Number of Worship Places      General information (not in csv of blockgroups)
# 
# 227             centroidX                                              for internal use only      General information (not in csv of blockgroups)
# 228             centroidY                                              for internal use only      General information (not in csv of blockgroups)
# 229              geometry                                              for internal use only      General information (not in csv of blockgroups)
# 230        statLayerCount                                              for internal use only      General information (not in csv of blockgroups)
# 231 statLayerZeroPopCount                                              for internal use only      General information (not in csv of blockgroups)
# 232      weightLayerCount                                              for internal use only      General information (not in csv of blockgroups)
# 233           timeSeconds                                              for internal use only      General information (not in csv of blockgroups)
# 234              distance                                              for internal use only      General information (not in csv of blockgroups)
# 235                  unit                                              for internal use only      General information (not in csv of blockgroups)
# 236                areaid                                              for internal use only      General information     can be a FIPS but varies and not always output
# 237              areatype                                              for internal use only      General information     eg blockgroup
# 238             statlevel                                              for internal use only      General information (not in csv of blockgroups)
# 239        inputAreaMiles                                      Size for the Area of Interest      General information (not in csv of blockgroups)
# 240             placename                 City or County Plus State for the Area of Interest      General information (not in csv of blockgroups)





# these are missing apiname and possibly need it, but only one was in api outputs actually - HSHOLDS AKA ACSTOTHH AKA hhlds

m$apiname[m$rname == 'hhlds' ] <- 'HSHOLDS' # 24            hhlds     ACSTOTHH                              Households (for limited English speaking)            252              ACSTOTHH        HSHOLDS  


# m[!grepl("pct|bin|text|DISPARITY", m$rname) & (nchar(m$apiname) == 0),]
#               rname   csvname2.2                                                            description    csv_example csv_descriptions_name apiname

# 25       builtunits     ACSTOTHU                                   Housing units (for % built pre-1960)            268              ACSTOTHU      no count in API.  
# 26          pre1960      PRE1960                                        Housing units built before 1960             69               PRE1960       no count in API. 
# 31             lths       LESSHS                                        Less than high school education            101                LESSHS      no count in API. % is   
# 32          lingiso      LINGISO                                    Limited English speaking households              0               LINGISO      no count in API. % is  RAW_D_LING
# 33           lowinc    LOWINCOME                                                             Low income            263             LOWINCOME      no count in API. % is  RAW_D_INCOME
# 139          over64       OVER64                                                            Over age 64            122                OVER64       no count in API.  
# 146            mins    PEOPCOLOR                                                        People of color            106             PEOPCOLOR      no count in API. % is  RAW_D_PEOPCOLOR
# 196         age25up   ACSEDUCBAS                                           Population 25 years and over            489            ACSEDUCBAS      no count in API.   
# 197   povknownratio   ACSIPOVBAS                       Population for whom poverty status is determined            693            ACSIPOVBAS      no count in API.
# 216          under5       UNDER5                                                            Under age 5             19                UNDER5      no count in API.
# 220      unemployed   UNEMPLOYED                                     Unemployed in civilian labor force              7            UNEMPLOYED      no count in API. % is  RAW_D_UNEMPLOYED ?
# 221  unemployedbase  ACSUNEMPBAS Unemployment base--persons in civilian labor force (unemployment rate)            315           ACSUNEMPBAS      no count in API.  

# 222        OBJECTID         OID_                               Unique ID for block group in geodatabase              1              OBJECTID       not in API 
# 15               id           ID                                       Census FIPS code for block group    10010201001                    ID       areaid  sometimes
# 16       countyname    CNTY_NAME                                                            County Name Autauga County             CNTY_NAME      not quite same as API's  placename  
# 27         arealand     AREALAND                                             Land area in square meters        4264299              AREALAND        na
# 227       areawater    AREAWATER                                            Water area in square meters          28435             AREAWATER        na
# 201            area   Shape_Area                                                             Shape area        6047647            Shape_Area   not quite same as API's inputAreaMiles     
# 202    Shape_Length Shape_Length                                                           Shape length        13436.9          Shape_Length        na



# xx =  m[!grepl('popup|color', m$description), ]
# xx = xx[!grepl('^D2|^D5|^S_D2|^S_D5', xx$csvname2.2), ]
# xx = xx[nchar(xx$apiname) == 0 | is.na(xx$apiname), ]
# xx 

# m[1,]


 ####################################### #
# SOME indicators were in map_headernames but not yet in m (the new map_headernames)

 api_need <- setdiff(apifull_example_v2.2$apiname2.2, m$apiname)
 x = apifull_v2.2_descriptions[ apifull_v2.2_descriptions$apiname2.2 %in% api_need, ]
 table(x$apiname2.2 %in% map_headernames$ejscreen_api)
 api_to_get <- intersect(x$apiname2.2, map_headernames$ejscreen_api)
 api_to_get
 found = map_headernames[map_headernames$ejscreen_api %in% api_to_get,
                         c("ejscreen_api", "newnames_ejscreenapi" )]
 names(found) <- c('apiname', 'rname')
found
# need to add these as new rows to m

madd = data.frame(rname=found$rname, csvname2.2=NA, description=NA, csv_example=NA, csv_descriptions_name=NA, apiname=found$apiname)
madd$description <-  map_headernames$longname_tableheader[match(madd$rname, map_headernames$newnames_ejscreenapi)]
m <- rbind(m, madd)


####################################### #
# only the percentiles for EJ indexes are in API outputs, not raw, map bin, or map popup text:
m$apiname[grepl("DISPARITY", m$rname) & !grepl('pctile.EJ.DISPARITY', m$rname)  ] <- NA


m$csvname2.2[is.na(m$csvname2.2)] <- "" # easier to work with if just empty not NA value
m$apiname[is.na(m$apiname)] <- "" # easier to work with if just empty not NA value


 
m2 <- merge(m, map_headernames, all.x = TRUE, all.y = FALSE, by.x = 'rname', by.y = 'newnames_ejscreenapi')

# confirmed the old api names in ejscreen_api are obsolete because the naming system changed.
m2[m2$apiname != m2$ejscreen_api & !is.na(m2$apiname) & !is.na(m2$ejscreen_api) & nchar(m2$ejscreen_api) != 0, c('rname',  'apiname', 'ejscreen_api', 'description')]
m2$ejscreen_api <- NULL
# the same is true of the old csv names in ejscreen_csv 
m2[m2$csvname2.2 != m2$ejscreen_csv, c('rname',  'csvname2.2', 'ejscreen_csv', 'oldnames')]
m2$ejscreen_csv <- NULL

m <- m2
rm(m2)

setwd(file.path(Sys.getenv("R_USER"), "EJAM")); getwd() 


map_headernames <- m

map_headernames$newnames_ejscreenapi <- map_headernames$rname
map_headernames$ejscreen_api <- map_headernames$apiname
map_headernames$oldnames[nchar(map_headernames$apiname) > 0] <- map_headernames$apiname[nchar(map_headernames$apiname) > 0]
map_headernames$ejscreen_csv <- map_headernames$csvname2.2


writexl::write_xlsx(m, path = './data-raw/map_headernames_2.2.xlsx')
writexl::write_xlsx(m, path = './../EJAMejscreenapi/data-raw/map_headernames_2.2.xlsx')


 
 ############################################################  # 
 
 rm(x,zz,found,more_rnames,more_csvnames) 
 rm(apiroot,api_need,api_to_get)
 
 # ##################################### #
 save(m, file = '~/../Downloads/m.rda')
 # save.image(file = '~/../Downloads/work in progress.rda')
 # ##################################### #
 

 
 
  
#################################################################### #
##___ SET WORKING DIRECTORY  to save EJAM data ####
stop('must be in the root of the EJAM source package to update these in that package')
# grep("mysource",  (Sys.getenv()), value = T)
setwd(file.path(Sys.getenv("R_USER"), "EJAM")); getwd() # just make sure this is the right one

#################################################################### #
# **** CREATE NEW lookup tables AS NEW usastats and statestats files in EJAM package ####

usastats   <- usastats2.2
statestats <- statestats2.2

# confirm first that map_headernames has all the right mapping of names for this work !!

setdiff(names(usastats2.2), m$csvname2.2) # PCTILE only

# note the statestats variables are same as in US version even though the percentile it finds in lookup is state.pctile.xyz 
# 
# names(usastats)   <- EJAMejscreenapi::fixnames_to_type(names(usastats),   oldtype = "ejscreen_csv", newtype = "newnames_ejscreenapi", mapping_for_names = EJAMejscreenapi::map_headernames)
# names(statestats) <- EJAMejscreenapi::fixnames_to_type(names(statestats), oldtype = "ejscreen_csv", newtype = "newnames_ejscreenapi", mapping_for_names = EJAMejscreenapi::map_headernames)

names(usastats)   <- EJAMejscreenapi::fixnames_to_type(names(usastats),   oldtype = "csvname2.2", newtype = "rname", mapping_for_names = m ) 
names(statestats)   <- EJAMejscreenapi::fixnames_to_type(names(statestats),   oldtype = "csvname2.2", newtype = "rname", mapping_for_names = m ) 



##########  A BIG PROBLEM HERE for EJAM (not the API) IS THAT THE 
# LOOKUP TABLES FROM THE GEODATABASES HAD ONLY ABOUT 50 KEY VARIABLES, BUT 
# LACK THE DEMOGRAPHIC SUBGROUPS LOOKUP INFO, FOR EXAMPLE!

# names(usastats)
# [1] "OBJECTID"                          "REGION"                            "PCTILE"                            "Demog.Index"                      
# [5] "Demog.Index.Supp"                  "pctmin"                            "pctlowinc"                         "pctunemployed"                    
# [9] "pctlingiso"                        "pctlths"                           "pctunder5"                         "pctover64"                        
# [13] "lowlifex"                          "pm"                                "o3"                                "dpm"                              
# [17] "cancer"                            "resp"                              "rsei"                              "traffic.score"                    
# [21] "pctpre1960"                        "proximity.npl"                     "proximity.rmp"                     "proximity.tsdf"                   
# [25] "ust"                               "proximity.npdes"                   "EJ.DISPARITY.pm"                   "EJ.DISPARITY.pm.supp"             
# [29] "EJ.DISPARITY.o3"                   "EJ.DISPARITY.o3.supp"              "EJ.DISPARITY.dpm"                  "EJ.DISPARITY.dpm.supp"            
# [33] "EJ.DISPARITY.cancer"               "EJ.DISPARITY.cancer.supp"          "EJ.DISPARITY.resp"                 "EJ.DISPARITY.resp.supp"           
# [37] "EJ.DISPARITY.rsei"                 "EJ.DISPARITY.rsei.supp"            "EJ.DISPARITY.traffic.score"        "EJ.DISPARITY.traffic.score.supp"  
# [41] "EJ.DISPARITY.pctpre1960"           "EJ.DISPARITY.pctpre1960.supp"      "EJ.DISPARITY.proximity.npl"        "EJ.DISPARITY.proximity.npl.supp"  
# [45] "EJ.DISPARITY.proximity.rmp"        "EJ.DISPARITY.proximity.rmp.supp"   "EJ.DISPARITY.proximity.tsdf"       "EJ.DISPARITY.proximity.tsdf.supp" 
# [49] "EJ.DISPARITY.ust"                  "EJ.DISPARITY.ust.supp"             "EJ.DISPARITY.proximity.npdes"      "EJ.DISPARITY.proximity.npdes.supp"









testjunk <- 0; EJAM::metadata_add(testjunk); print(attributes(testjunk)); rm(testjunk)
usastats   <- EJAM::metadata_add(usastats)
statestats <- EJAM::metadata_add(statestats)
# check them to see if OK before replacing existing datasets in package...
attributes(usastats)[!("row.names" == names(attributes(usastats)))] 
attributes(statestats)[!("row.names" == names(attributes(statestats)))] 


stop('need pctile lookup info for demog subgroups, in us, states, island areas; and any other indicators we want to report as percentiles in EJAM')





#################################################################### #
# **** CREATE NEW blockgroupstats (AFTER map_headernames is updated) ####

bg <- bg2.2_csv
names(bg) <- fixnames_to_type(names(bg), oldtype = 'csvname2.2', newtype = 'rname', mapping_for_names = m)
# names(bg)[grepl("pctile.text|bin.", names(bg))] # do not need bin or popup text columns
class(bg)
setDF(bg)
bg <- bg[, !grepl("pctile.text|bin.", names(bg)) ]
names(bg) <- gsub('OID_', 'bgfips', names(bg))
 

setdiff(names_d_subgroups , names(bg))
# [1] "pcthisp"         "pctnhba"         "pctnhaa"         "pctnhaiana"      "pctnhnhpia"      "pctnhotheralone" "pctnhmulti"     
# [8] "pctnhwa"   




# THE DEMOG SUBGROUPS DO NOT SEEM TO BE IN bg2.2_csv ??   ********





# note that EJ index (at any percentile) is just 0-100 since EJ Index = demogindex times percentile of Envt = 
#  = DEMOGIDX_2 * P_PM25 for example, which always is demog index 0-1.00 times pctile of 0-100 = 0 to 100.

rm(xx,q,madd, fixthese )


# save(m, file = '~/../Downloads/m.rda')
save.image(file = '~/../Downloads/work in progress.rda')





# stopped here -----------------



# maybe want to fix oldnames column 

# then move to EJAMejscreenapi/data-raw/
# and run script to install that in that pkg.



#################################################################### #
# **** USE  usastats_pctile_lookup_add_subgroups.R ####














#################################################################### #
# **** CREATE NEW blockgroupstats (AFTER map_headernames is updated) ####

## 1st need to enhance it before saving blockgroupstats as new data for package EJAM
## see older scripts here: 
# dir("~/../../R/mysource/EJAM/dev/notes_datasets")
## [1] "0_SCRIPT_overview_get_ejscreendata.R"                 "1_SCRIPT_EJAMejscreen_download.R"                    
## [3] "2_SCRIPT_FOR_FIPS_ST_TRACT_CNTY.R"                    "3_SCRIPT_create_bgDemog_ejscreen2.1_andtracts.R"     
## [5] "4_SCRIPT_ADD_PUERTORICO_DEMOG_SUBGROUPS.R"            "5_SCRIPT_merge_demogsubgroups_v2.1.R"                
## [7] "6_SCRIPT_create_blockgroupstats.R"                    "8_SCRIPT_make_MeansByGroup_and_Ratios_RRS.US22.R"    
## [9] "9_SCRIPT_PCTILELOOKUPS_READ-CSVS-MID-2022.R"          
##  .... "NOTES_which_states_are_in_which_datasets.R"

# grep("[^2|4]", map_headernames$ejscreen_csv, value = T)


# need to get US and state files info?
# 












