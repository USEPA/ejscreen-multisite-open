

# script to download and clean a table of facilities by MACT subpart


oldwd <- getwd()
setwd("~/../Downloads")



# One can download from ECHO all the 14,000 or so CAA Major Actives  
# https://echo.epa.gov/tools/data-downloads
# and then remove the ones without  "MACT" listed in the column called AIRPrograms
# which are the one that have nothing in the column called  AIRMacts.
#  AIRMacts  column is a csv list of codes like DDDDD, EEEE, FFFF, HHHHH, JJJJ, KK, N, UU, ZZZZ
#  that apply to the given facility.
# https://echo.epa.gov/tools/data-downloads/icis-air-download-summary 
# ICIS-Air Datasets

fname = "https://echo.epa.gov/files/echodownloads/ICIS-AIR_downloads.zip"
download.file(fname, "ICIS-AIR_downloads.zip")
unzip("ICIS-AIR_downloads.zip", exdir = getwd() )
# dir(pattern = "ICIS")
# [1] "ICIS-AIR_downloads.zip"         "ICIS-AIR_FACILITIES.csv"       
# [3] "ICIS-AIR_FCES_PCES.csv"         "ICIS-AIR_FORMAL_ACTIONS.csv"   
# [5] "ICIS-AIR_INFORMAL_ACTIONS.csv"  "ICIS-AIR_POLLUTANTS.csv"       
# [7] "ICIS-AIR_PROGRAM_SUBPARTS.csv"  "ICIS-AIR_PROGRAMS.csv"         
# [9] "ICIS-AIR_STACK_TESTS.csv"       "ICIS-AIR_TITLEV_CERTS.csv"     
# [11] "ICIS-AIR_VIOLATION_HISTORY.csv"


###################################################### #
# Get the operating status to see which are Active/Operating or seasonal ?  ignored that here.
#
x = "ICIS-AIR_FACILITIES.csv"
x = read.csv(x, stringsAsFactors = FALSE)
names(x)
# unique(x$AIR_OPERATING_STATUS_DESC)
# "Operating"  "Permanently Closed"  ""  "Temporarily Closed" "Planned Facility" "Seasonal"  "Under Construction"

rm(x)
###################################################### #


# Get the main table with info about MACT NESHAP subparts

y = "ICIS-AIR_PROGRAM_SUBPARTS.csv"
y = read.csv(y, stringsAsFactors = FALSE)

setwd(oldwd)

# names(y)
# > table(y$PROGRAM_CODE == "CAAMACT")
# FALSE   TRUE 
# 113547  67812 
y <- y[y$PROGRAM_CODE == "CAAMACT", ]
y$subpart <- gsub("CAAMACT", "", y$AIR_PROGRAM_SUBPART_CODE)
y <- y[, c("PGM_SYS_ID", "subpart", "AIR_PROGRAM_SUBPART_DESC")]
# unique(y$subpart)
# note that some are 6B or 6J not BBBBBB or JJJJJJ
# cbind(sort(unique(y$subpart)))

# function to convert something like 6B into BBBBBB
expandit <- function(old) {
  acount <-  (substr(old,1,1) %in% 0:9) 
  for (i in 1:length(old)) {
    if (acount[i]) {
      old[i] <-     old[i] <- paste(rep(substr(old[i] ,2,2), as.numeric(  substr(old[i] ,1,1))), collapse = "") 
    } 
  }
  old
}

y$subpartclean <- expandit(y$subpart)
y = y[ , c("PGM_SYS_ID", "subpartclean", "subpart", "AIR_PROGRAM_SUBPART_DESC")]

# double check that against text within the description field
z <- y$AIR_PROGRAM_SUBPART_DESC
# cbind(gsub(".*Subpart (.*) - .*", "\\1", z ), z)
# unique(gsub(".*Subpart (.*) - .*", "\\1", z ) )
z <- gsub("(.*) - .*", "\\1", gsub(".*Subpart (.*) - .*", "\\1", z ))
# z
table(z == y$subpartclean)
rm(z)
# all true

# Extract just the title of the category, without all the duplicative text that field has

y$title <-  gsub((".* Subpart .{1,8} - ([a-zA-Z]*)"), "\\1", y$AIR_PROGRAM_SUBPART_DESC)
# (cbind(y$AIR_PROGRAM_SUBPART_DESC,  gsub((".* Subpart .{1,8} - ([a-zA-Z]*)"), "\\1", y$AIR_PROGRAM_SUBPART_DESC)))[700:710,]
y <- y[ , c("PGM_SYS_ID", "subpartclean", "title")]
names(y) <- c("programid", "subpart", "title")




# Make a table of the unique MACT codes and the title of each category

types <- unique(y[,c("subpart", "title")])
types <- types[order(types$subpart), ]
rownames(types) <- NULL
names(types) <- c("subpart", "title")


############### # 

setwd("~")

# save in the package

data.table::setDT(y, key = c("subpart", "programid"))
frs_by_mact <- data.table::copy(y)
usethis::use_data(frs_by_mact)    # data.table

mact_categories <- types
usethis::use_data(mact_categories)  # data.frame




# save(frs_by_mact, file = "frs_by_mact.rda")
# save(mact_categories, "mact_categories.rda")
 

################################################################################################ #




# Facility/Source Level Identifying Data (ICIS-AIR_FACILITIES.csv)
#
# PGM_SYS_ID   -   this is the key for linking between tables 
# REGISTRY_ID
# SIC_CODES	Char	4000
# NAICS_CODES	Char	4000
# FACILITY_TYPE_CODE	Char	3
# AIR_OPERATING_STATUS_CODE	Char	5   e.g.  OPR  = 	Operating
# AIR_OPERATING_STATUS_DESC	Char	100

# Air Program Subparts (ICIS-AIR_PROGRAM_SUBPARTS.csv)
#
# Element Name	Data Type	Length
# PGM_SYS_ID1	Char	30
# PROGRAM_CODE	Char	9      Code values include:   CAAMACT
# PROGRAM_DESC	Char	100
# AIR_PROGRAM_SUBPART_CODE	Char	20
# AIR_PROGRAM_SUBPART_DESC	Char	200

# AIR_PROGRAM_CODE_SUBPARTS - A field indicating applicable air program subparts. Subpart code values can be found on the ICIS-Air Program Code Subpart Descriptions page.
# https://echo.epa.gov/tools/data-downloads/icis-air-download-summary/air-program-code-subpart-descriptions#caamact








################################################################################################ #
################################################################################################ #



# tried to use web services but without success





# https://echo.epa.gov/tools/web-services

# FRS via ECHO does have a field called AIRMacts with comma separated list of subparts like CC, FFFF, YY
#
#  *** BUT so far I cannot get that type of query to work in their API - using MACT subparts as query of air systems services.
#
# ECHO API has air services that let you query on MACT subpart, but it fails the way I have tried.
# ECHO API example of Air services - fails when I try to search on MACT subparts:
# https://echodata.epa.gov/echo/air_rest_services.get_facilities?output=JSON&p_mact=FFFF&qcolumns=2%2C8%2C22%2C23%2C25%2C26

# https://echodata.epa.gov/echo/air_rest_services.get_facility_info?p_act=Y&p_maj=Y&p_mact=ZZZZZ&qcolumns=2%2C8%2C22%2C23%2C25%2C26

# AIRPrograms	                  AIRMacts
# MACT, NSPS, NSR, SIP, TVP	    DDDDD, EEEE, FFFF, HHHHH, JJJJ, KK, M, ZZZZ


# AIRStatus = Operating
# 
# FacLat	FacLong
# 31.671177	-98.996513
# 
# AIRNsps	 
# NSPS Part 60 - Subpart Dc - SMALL INDUS-COMMER-INSTITUTL STEAM GENERATING UNITS, NSPS Part 60 - Subpart JJJJ - STATIONARY SPARK IGNITION INTERNAL COMBUSTION ENGINES, NSPS Part 60 - Subpart RR - PRESSR-SENST TAPE, LABEL SURFACE COATING OPERATIONS	

stop("stopped here")

library(EJAMejscreenapi)
subpart = 'FFFF'

x <- EJAMejscreenapi::get_facility_info_via_ECHO(
  qcolumns = c(2,8,22,23,25,26), url_not_query = T, otherparameters = "&registry_id=110015778176")

x <- httr::GET(x)  # seems to get stuck for a VERY long wait

# one query worked this way but others do not: - this all needs to be rewritten or dropped.
x <- jsonlite::fromJSON(rawToChar(x$content))$Results$ClusterOutput$ClusterData[,1:8]


#   that does not include a field called AIRMacts
# 
# https://echodata.epa.gov/echo/air_rest_services.metadata
# get_facilities, get_qid, get_map, and get_downoad
# Use get_facilities to validate passed query parameters, obtain summary statistics and to obtain a query_id (QID). QIDs are time sensitive and will be valid for approximately 30 minutes.
# Use get_download, with the returned QID, to generate a Comma Separated Value (CSV) file of facility information that meets the QID query criteria.


# # FRS query on registry ID
# https://ofmpub.epa.gov/frs_public2/frs_rest_services.get_facility_wbd?registry_id=110015778176
# 
# # FRS API query example - within 3 miles of 1 point, find all FRS sites (of certain type)
# # URL for searching SEMS (Superfund) facilities within a 3 mile radius of latitude 38.8/longitude -77.01.
# https://ofmpub.epa.gov/frs_public2/frs_rest_services.get_facilities?latitude83=38.8&longitude83=-77.01&search_radius=3&pgm_sys_acrnm=SEMS&output=JSON
# 
# # FRS QUERY PAGE FOR USERS TO MANUALLY ENTER NAICS TO SEARCH FOR AND GET LIST OF SITES/LAT/LON,WHATEVER
# https://frs-public.epa.gov/ords/frs_public2/ez_frs_column.list?table_name=D_EF_FAC.V_PUB_FRS_NAICS_EZ



# ECHO API has air services that let you query on MACT subpart, but it fails when I try.
# ECHO API example of Air services - fails when I try to search on MACT subparts:
# https://echodata.epa.gov/echo/air_rest_services.get_facilities?output=JSON&p_mact=FFFF&qcolumns=2%2C8%2C22%2C23%2C25%2C26
browseURL("https://echodata.epa.gov/echo/air_rest_services.get_facilities?output=JSON&p_mact=ZZZZ&qcolumns=2%2C8%2C22%2C23%2C25%2C26")
# Key columns to ask for:
# SourceID 2
# RegistryID 8
# FacLat 22
# FacLong 23
# AIRMacts 25
# AIRStatus  26
# 2,8,22,23,25,26

# ECHO API example of Air services downloading csv based on QID already gotten
# https://echodata.epa.gov/echo/air_rest_services.get_download?qid=339&qcolumns=2%2C8%2C22%2C23%2C25%2C26


# "ColumnName": "AIR_MACTS",
# "DataType": "VARCHAR2",
# "DataLength": "4000",
# "ColumnID": "25",
# "ObjectName": "AIRMacts",
# "Description": "The Maximum Achievable Control Technology (MACT) Subpart associated with the facility."

