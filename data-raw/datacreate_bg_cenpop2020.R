#State/state equivalent entity-based text files containing the 
#mean centers of population for each census block group 
#within a state/state equivalent entity for the 2020 Census.

# 3/23

#The record layout is:
#
#STATEFP:  2-character state FIPS code
#COUNTYFP:  3-character county FIPS code
#TRACTCE:  6-character census tract code
#BLKGRPCE:  1-character census block group code
#POPULATION:  2020 Census population tabulated for the block group
#LATITUDE:  latitude coordinate for the center of population for the block group
#LONGITUDE:  longitude coordinate for the center of population for the block group
#
#STATEFP,COUNTYFP,TRACTCE,BLKGRPCE,POPULATION,LATITUDE,LONGITUDE
#01,001,020100,1,575,+32.464466,-086.486302
#01,001,020100,2,1200,+32.482744,-086.486741

if (interactive()) {
# browseURL("https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt")
}
####################################################### # 


fname <- "https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt" # 10 MB size file
x <- readr::read_csv(fname, col_types = "ccccidd") # keep FIPS as character with leading zeroes  # x <- read.csv(fname)
head(x)
x <- as.data.frame(x)
  length(unique(x$STATEFP))
# [1] 52
 dim(x)
#[1] 242335      7
 rm(fname)

names(x) <- c("FIPS.ST", "fips3county", "fips6tract", "fips1bg", "pop2020", "lat", "lon")

####################################################### # 
# CONSIDER ADDING INFO LIKE ST, 
# and look up bgid based on join on bgfips
x$FIPS.COUNTY <- paste0(x$FIPS.ST,     x$fips3county)
x$FIPS.TRACT  <- paste0(x$FIPS.COUNTY, x$fips6tract)
x$FIPS        <- paste0(x$FIPS.TRACT,  x$fips1bg)
x$bgfips <-  x$FIPS

x$ST <- EJAM::stateinfo$ST[match(x$FIPS.ST, EJAM::stateinfo$FIPS.ST)]
 # x <- x[,c("FIPS.ST", "FIPS.TRACT", "FIPS", "bgfips", "lat", "lon", "pop2020")]
 x <- x[,c("bgfips", "lat", "lon", "pop2020", "ST")]
 
 bg_cenpop2020 <- data.table::as.data.table(x)
rm(x)

#  look up bgid based on join on bgfips
if (!exists("bgid2fips")) dataload_from_pins("bgid2fips")
bg_cenpop2020$bgid <- bgid2fips[bg_cenpop2020,  bgid, on = "bgfips"] # bgid2fips is loaded from aws, e.g. by EJAM pkg
  data.table::setkey(bg_cenpop2020, bgid, bgfips)
 
data.table::setkey(bg_cenpop2020,bgfips)
data.table::setorder(bg_cenpop2020, bgid, bgfips, lat, lon, pop2020, ST)

mapfast(bg_cenpop2020[ST == "LA",], radius = 0.01)
####################################################### # 
#### DROP MOST OF THAT INFO ACTUALLY... 
#  THIS IS 24MB and already have all this in bgpts, except for pop2020 and lat lon of pop2020wtd centroid !

bg_cenpop2020 <- bg_cenpop2020[, .(bgid, lat, lon, pop2020, ST)]

dim(bg_cenpop2020)
dim(EJAM::blockgroupstats)
sum(bg_cenpop2020$pop2020)
sum(blockgroupstats$pop, na.rm = T)
####################################################### # 

bg_cenpop2020 <-  metadata_add(bg_cenpop2020, metadata = list(download_date = Sys.time(), source = fname,  census_version = 2020)) 
usethis::use_data(bg_cenpop2020, overwrite = TRUE)

rm(bg_cenpop2020)
gc()



# > dim(blockgroupstats)
# [1]  243021     86   in v2.2 ? Island Areas
# > dim(bg_cenpop2020)
# [1] 242335      5 
