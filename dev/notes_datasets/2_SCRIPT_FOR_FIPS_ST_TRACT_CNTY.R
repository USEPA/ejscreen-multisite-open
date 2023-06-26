######################################################################## #
# # notes on converting 
# EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI
# and
# ejscreen package file bg22DemographicSubgroups2016to2020
# to
# EJAM::blockgroupstats
# and
# ejscreen package file bg22plus
######################################################################## #

#################################################################################################### #
# create ejscreen datasets for EJAM etc ####

## FULL SET OF SCRIPTS  to create EJScreen-related datasets, including blockgroupstats.rda for EJAM  

# 1.  EJAMejscreendata/inst/SCRIPT_EJAMejscreen_download.R  
#       - to add metadata ( and ejscreen package file metadata_add() )
# 2.   ejscreen/inst/2_SCRIPT_FOR_FIPS_ST_TRACT_CNTY.R    
#       - to rename cols and add some fips fields and fix countyname col
# 3.   ejscreen/inst/3_SCRIPT_create_bgDemog_ejscreen2.1_andtracts.R 
#       - to get demog race ethnicity subgroups 
# 4.   ejscreen/inst/4_SCRIPT_ADD_PUERTORICO_DEMOG_SUBGROUPS.R 
#       - to download the PR demog subgroup part
# 5.   ejscreen/inst/5_SCRIPT_merge_demogsubgroups_v2.1.R  
#       - to MERGE SUBGROUPS info TO EJScreen  

  # [Just put it all in bg22, then can drop bg22plus or bg22Demog...]

# 6.  EJAM/inst/notes/create_blockgroupstats.R 
#       - to simplify/ drop cols, and save as data.table for EJAM::blockgroupstats

#################################################################################################### #


######################################################################## #
# add FIPS leading zeroes (for AS/GU/MP/VI)
# add FIPS.ST 
# add FIPS.TRACT
# rename CNTY_NAME to countyname
######################################################################## #
# 
# library(
#   ejscreen)
# library(
#   EJAMejscreendata) 





# not  done but 
# to start to redo bg22, from EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI


# TEMPORARY/INTERIM FIX TO ADD ST TO EJAM::blockgroupstats while not already there:
# blockgroupstats[, ST := stateinfo$ST[match(substr(bgfips,1,2), stateinfo$FIPS.ST)]]




stop(' need to load EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI')
bgejam <-  EJSCREEN_Full_with_AS_CNMI_GU_VI
# fix variable names to be friendlier
stop('need to load ejscreen package file called change.fieldnames.ejscreen.csv')
names(bgejam) <- change.fieldnames.ejscreen.csv(names(bgejam))


#   NOTE this census2020 block table has PR but lacks "AS" "GU" "MP" "VI"
# while EJScreen 2.1 has those at blockgroup res
# > uniqueN( blockid2fips[,substr(blockfips,1,2)])
# [1] 52
# length(unique(EJAMejscreendata package file called EJSCREEN_Full_with_AS_CNMI_GU_VI$ST_ABBREV))
# [1] 56
#   dim(bgejam)
# [1] 242,940    155
#   dim(ejscreen package file bg22)
# [1] 242,335    157
#  these fips will mess up assumptions about bg fips always having 12 characters:
# > table(bgejam[nchar(bgejam$FIPS) < 12 , 'ST'])
# AS  GU  MP  VI 
# 77  58 135 335 
# island2 <- c("60" ,"66", "78", "69")
# islandst <- c('AS', 'GU', 'VI', 'MP')
# # instead of adding leading zeroes, may want to add trailing zeroes so 2digit FIPS will be kept?
# easier to just leave it the way it is even though FIPS.TRACT and FIPS.COUNTY will not work
# rv <- stringi::stri_reverse
# bgejam$FIPS[bgejam$ST %in% islandst]

bgejam$FIPS <- lead.zeroes(as.numeric(bgejam$FIPS), 12)

# bgejam <- ejanalysis package file addFIPScomponents(bgejam) # not robust to this even if add leading zeroes
bgejam$FIPS.ST <- substr(bgejam$FIPS,1,2)
# fix these 4:
bgejam$FIPS.ST[bgejam$ST == 'AS'] <- '60'
bgejam$FIPS.ST[bgejam$ST == 'GU'] <- '66'
bgejam$FIPS.ST[bgejam$ST == 'MP'] <- '69'
bgejam$FIPS.ST[bgejam$ST == 'VI'] <- '78'  # 00780
# ejanalysis package file get.state.info(c('AS', 'GU', 'VI', 'MP'))$FIPS.ST
#  "60", "66", "78" ,"69" 
table(nchar(bgejam$ST))

# unique(substr(bgejam$FIPS[bgejam$FIPS.ST %in% c("60" ,"66", "78", "69")],1,7))
# [1] "0000060" "0000066" "0000069"    "0078010" "0078020" "0078030"
# Available geographies within each island area:
#   https://www.census.gov/programs-surveys/decennial-census/decade/2020/planning-management/release/2020-island-areas-data-products.html 
#   - American Samoa—Districts, counties, and villages.
# - Commonwealth of the Northern Mariana Islands—Municipalities, districts, and villages. 
# - Guam—Municipalities and census designated places.
# - U.S. Virgin Islands—Islands, subdistricts, estates, towns, and census designated places.

NONISLAND <- !(bgejam$ST %in% islandst)
bgejam$FIPS.TRACT <- NA
bgejam$FIPS.TRACT[NONISLAND] <- get.fips.tract(bgejam$FIPS[NONISLAND])

# and columns differ still
#
#  setdiff(names(bg22), names(bgejam))
# "FIPS.COUNTY" "countyname" 
#   setdiff(  names(bgejam), names(bg22))
# [1] "CNTY_NAME"       "EXCEED_COUNT_80"
# 
# and countyname versus CNTY_NAME differ... one has the State name in it
# maybe fix countyname later?
head(bgejam$CNTY_NAME)
# [1] "Autauga County" "Autauga County" "Autauga County" "Autauga County" "Autauga County"
# [6] "Autauga County"
# > head(ejscreen package file bg22$countyname)
# [1] "Autauga County, Alabama" "Autauga County, Alabama" "Autauga County, Alabama"
# [4] "Autauga County, Alabama" "Autauga County, Alabama" "Autauga County, Alabama"

names(bgejam) <- gsub('CNTY_NAME', 'countyname', names(bgejam))

# NEXT HAVE TO MERGE DEMOG SUBGROUPS INTO THIS, AND STATE PERCENTILES

