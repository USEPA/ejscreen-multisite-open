# CREATE OR ELSE READ AND PROCESS THE BLOCK WEIGHTS FILE TO USE IN EJAM and EJAMblockdata R PACKAGES. 
# THIS SCRIPT WAS USED TO IMPORT THE BLOCK WEIGHTS FILE OBTAINED FROM EJSCREEN TEAM 8/2022 and identical one 9/2022.
# That block weights file was created by the EJScreen team.

#   compare to  /EJAM-Census2020download/R/blockdata_prep2020.R

#  vs  /EJAM/inst/datasets/NOTES_read_blockweights2020.R

# BUT still need PR, and then AS VI GU MU

######## #
# BLOCK WEIGHTS ARE used to estimate 
#  what fraction of a given block group residents are in certain blocks, 
#  which is how this estimates what residents are inside a buffer like a circle.
# Block weights are calculated as...
#  BLOCK POPULATION (from CENSUS2020) 
#    AS FRACTION OF 
#  PARENT BLOCK GROUP'S POP (from CENSUS2020)
######## #

############################################################################### #
blockweights_csv_available <- TRUE 

if (!blockweights_csv_available) {

  #   #   create a block weights table from scratch (from census 2020 files): 
  #   # after getting Census 2020 data from the Census Bureau, like this (not fully tested, however- vroom might have some parsing issues?):
  library(data.table)
  require(census2020download) # was previously at   https://github.com/ejanalysis/census2020download
  census2020download::census2020_download('~/../Downloads/census2020zip') #  or to  temp folders 
  # after downloaded, unzip
  census2020download::census2020_unzip(   '~/../Downloads/census2020zip',  '~/../Downloads/census2020out') # 102 files with .pl extension
  
  # read in the 102 files - This can take several minutes:
  system.time({
    blocks <- census2020download::census2020_read(folder =                   '~/../Downloads/census2020out') # not yet a data.table
    #    user  system elapsed                                                       
    # 1006.94   76.19  342.82 
  })
  blocks <- census2020download::census2020_clean(blocks)  # returns a data.table
  # > tables()
  # NAME      NROW NCOL  MB                       COLS KEY
  # 1: blocks 8,132,968    5 807 blockfips,lat,lon,pop,area    
  # Total: 807MB
  # 
  # ejanalysis::get.state.info(unique(substr(blocks$blockfips,1,2)), fields = "ST")
  # 
  # THAT METHOD DOES NOT GET PUERTO RICO data
  #
  ############################################################################### #

  data.table::setnames(x = blocks, old = 'pop', new = 'blockpop') #  RENAME POPULATION FIELD   ----
  data.table::setorder(blocks, blockfips) #    sort by increasing blockfips 
  blocks[, bgfips := substr(blockfips, 1, 12)] # for merging blockgroup indicators to buffered blocks later on
  #  *CREATE blockwts column * #  the blockwt is the blocks share of parent blockgroup census pop count
  blocks[ , bgpop := sum(blockpop), by=bgfips] # Census count population total of parent blockgroup gets saved with each block temporarily
  blocks[ , bgpop := as.integer(bgpop)] # had to be in its own line for some reason
  blocks[ , blockwt := blockpop / bgpop] # ok if the ones with zero population were already removed, but also is.na can fix others?
  blocks[is.na(blockwt), blockwt := 0] 
  # In Census 2020:  1,393 blockgroups had 0 pop in every block, due to 12,922 of the 2,368,443 blocks with 0 pop.
  # put columns in same order as were in blockweights csv from EJScreen (for those cols they have in common)
  data.table::setcolorder(blocks, neworder = c('blockfips', 'bgfips', 'blockwt', 'area', 'lat', 'lon', 'blockpop', 'bgpop'))
 
  save(blocks, file= "blocks_downloaded_2020_pop_area_wts_laton.rda") # MOVED OUT OF PACKAGE FOLDERS - ALMOST 200gB
  
   # Census Bureau explanation of FIPS:
  # blockid: 15-character code that is the concatenation of fields consisting of the 
  # 2-character state FIPS code, the 
  # 3-character county FIPS code, [5 total define a county] the 
  # 6-character census tract code, [11 total define a tract] and the  
  #    [and 12 total define a blockgroup, which uses 1 digit more than a tract]
  # 4-character tabulation block code. [15 total define a block]
  ############################################################################### #  ############################################################################### #

  } else {

  ############################################################################### #  ############################################################################### #
  # SCRIPT TO IMPORT THE BLOCK WEIGHTS FILE OBTAINED FROM EJSCREEN TEAM, FOR USE IN THIS PACKAGE:
  #   also want area, which is not in the file called BLOCK20_CENTROIDS_WEIGHTS.csv (area weight but not actual area is there)
  # so see census2020download:: for that.
    # same for actual census 2020 population count
  
library(data.table)
blocks <- data.table::fread(file = "~/../EJ 2021/EJSCREEN 2022 2.1 DATA late2022/BLOCK20_CENTROIDS_WEIGHTS.csv",
                            colClasses = c(
                              GEOID      = 'character', 
                              STCNTRBG   = 'character', 
                              POP_WEIGHT = 'numeric' ,
                              HOU_WEIGHT = 'numeric', 
                              AREA_WEIGHT= 'numeric', 
                              PLFIPS     = 'character',     
                              LAT        = 'numeric',      
                              LNG        = 'numeric'
                            )
)
# > head(blocks)
#             GEOID    STCNTRBG POP_WEIGHT HOU_WEIGHT AREA_WEIGHT PLFIPS      LAT       LNG
# 1: 10010201001000 10010201001 0.03652174 0.03076923 0.067253643 162328 32.47070 -86.48048
# 6: 10010201001005 10010201001 0.00000000 0.00000000 0.001278905 162328 32.46759 -86.49873  # note zero pop in block

data.table::setnames(
  blocks, 
  names(blocks), 
  c('blockfips', 'bgfips', 'blockwt', 'householdwt', 'areawt', 'plfips', 'lat', 'lon')
  )
############################################################################### #  ############################################################################### #
}



############################################################################### #
##  Notes on dropping blocks with zero pop (no residents) ####
#
# ONE COULD DROP UNPOPULATED BLOCKS...
# *** BUT they were not dropped for Census2020 in this pkg*** (but had been dropped for the Census2020download pkg) 
#  Wastes space but simpler if buffering code finds a zero pop block point and uses zero as the weight
#   rather than pretending those blocks do not exist at all. 
# We do not actually need to retain blockgroups with zero population in blocks datasets if they are in ejscreen/others, to ensure merges dont seem to have problems.
# Not sure we want to keep or drop blocks with zero pop when at least other blocks in bg have pop
# ...  Efficient to drop. complicated if have to retain zero pop blocks only when needed to have a blockgroup appear at all,
# just cannot locate those block points in buffering, but 
# cannot think of why they might be needed for proximity analysis of nearby residents (or find their zero wts)
#
# ZERO POPULATION for CENSUS 2020 in
#  a HUGE SHARE --- 29% OF BLOCKS, OR BETWEEN 1 in 3 and 1 in 4 of 8.2 MILLION blocks.
# > nrow(blockwts)               # [1] 8,174,955
# > dim(blocks)                  # [1] 8,174,955       5
# > blocks[ blockpop == 0, .N]   # [1] 2,368,443
# > blocks[ blockpop != 0, .N]   # [1] 5,806,512
# > blocks[ blockpop == 0, .N]/ NROW(blocks)   # [1] 0.2897194
# > blocks[ is.na(blockpop), .N]   # [1] 0  # no NA values, just zeroes.
# Also, In Census 2020:  1,393 blockGROUPS had 0 pop in every block, due to 12,922 of the 2,368,443 blocks with 0 pop.
# blocks[ , .(tot=sum(blockwt)), by=bgfips][,.(min=min(tot,na.rm = T), max=max(tot,na.rm = T), nas=sum(is.na(tot)))]
# min max  nas    # 1:   1   1 1393
############################################################################### #






############################################################################### ################################################################################ #

############################################################################### #
# BREAK UP BLOCK DATA INTO A FEW SPECIFIC FILES ####
# break into smaller data.tables, for lat/lon (blockpoints) and weights (blockwts) and bgid2fips and blockid2fips
############################################################################### #

blocks[ , blockid := .I] # numbers 1 through 8174955, 1 per row, and already sorted by increasing blockfips

blockid2fips <- data.table::copy(blocks[ , .(blockid, blockfips)])
blockpoints  <- data.table::copy(blocks[ , .(blockid, lat, lon, areawt)]) # added areawt to blockpoints, circa 1/30/2023. actual block area = bgarea * areawt

blockwts     <- data.table::copy(blocks[ , .(blockid, bgfips, blockwt) ])
blockwts[ , bgid := .GRP, by=bgfips] # sorted so bgid starts at 1 with first bgfips in sort by bgfips
bgid2fips <- data.table::copy(blockwts)
bgid2fips <- unique(bgid2fips[ , .(bgid, bgfips)])
blockwts[ , bgfips := NULL] # drop that column from this table

data.table::setcolorder(blockwts, neworder = c('blockid', 'bgid', 'blockwt'))

# do not need to retain bgfips column since it has bgid, and bgfips is kept in bgid2fips
# and blockwts is 170MB if bgfips kept as character, 125MB if bgid used instead of bgfips. 
#    save space to convert bgfips to an integer bgid instead of the huge bgfips character string, as done for blockid vs blockfips
# all.equal(unique(blockwts$bgfips), sort(unique(blockwts$bgfips))) # confirms they are already sorted after unique()

## area: previously did not bother saving columns that had block land area, water area, and plfips , BUT
#    total area is useful for doing proximity scores for each block (and then parent block group) 
#  - area is needed to calculate score when distance is smaller than effective radius of block, per formula in EJScreen tech doc,
#  and as done in proxistat::proxistat()

############################################################################### #
#  CREATE quaddata for fast search for nearby block points ####
#
########### convert block lat lon to XYZ units ########## #
# Note quaddata is used to create localtree  in buffering code or at load of EJAM package,
#  to index the site points around which one is buffering, using the quadtree structure, so
#   quaddata will be saved here - it gets used later to build indexes of block points locations. 
earthRadius_miles <- 3959 # in case it is not already in global envt
radians_per_degree <- pi / 180
quaddata <- data.table::copy(blockpoints)
quaddata[ , BLOCK_LAT_RAD  := lat * radians_per_degree]
quaddata[ , BLOCK_LONG_RAD := lon * radians_per_degree]
coslat <- cos(quaddata$BLOCK_LAT_RAD)
quaddata[ , BLOCK_X := earthRadius_miles * coslat * cos(BLOCK_LONG_RAD)] 
quaddata[ , BLOCK_Y := earthRadius_miles * coslat * sin(BLOCK_LONG_RAD)] 
quaddata[ , BLOCK_Z := earthRadius_miles *          sin( BLOCK_LAT_RAD   )]
quaddata <- quaddata[ , .(BLOCK_X, BLOCK_Z, BLOCK_Y, blockid)]


# localtree HAS TO BE REBUILT IN EVERY R SESSION AND CANNOT SIMPLY BE LOADED FROM A .rda FILE FOR USE IN getblocksnearby()

# localtree <- SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point") 

############################################################################### #
# set key for some of these 

# (can you set more than one key in a table? some documentation said yes, but some 
#  data.table examples seemed to say no and thus need to setindex not setkey for other columns)
data.table::setkey(blockwts,     blockid); data.table::setindex(blockwts, bgid) # not sure the best/right approach is 1 key and 1 index. I think you can have only 1 key col? and 
data.table::setkey(blockpoints,  blockid)
data.table::setkey(blockid2fips, blockid); data.table::setindex(blockid2fips, blockfips)
data.table::setkey(bgid2fips,       bgid); data.table::setindex(bgid2fips,       bgfips)

############################################################################### #
# set attributes to store metadata on vintage

metadata <- list(
  census_version = 2020,
  acs_version = '2016-2020',
  acs_releasedate = '3/17/2022',
  ejscreen_version = '2.1',
  ejscreen_releasedate = 'October 2022',
  ejscreen_pkg_data = 'bg22'
)

bgid2fips     <- EJAM::metadata_add( bgid2fips,    metadata = metadata)
blockid2fips  <- EJAM::metadata_add( blockid2fips, metadata = metadata)
blockpoints   <- EJAM::metadata_add( blockpoints,  metadata = metadata)
blockwts      <- EJAM::metadata_add( blockwts,     metadata = metadata)
quaddata      <- EJAM::metadata_add( quaddata,     metadata = metadata)

usethis::use_data(   bgid2fips,  overwrite = TRUE)
usethis::use_data(blockid2fips,  overwrite = TRUE)
usethis::use_data(blockpoints,   overwrite = TRUE)  
usethis::use_data(blockwts,      overwrite = TRUE)
usethis::use_data(quaddata,      overwrite = TRUE)

############################################################################### #  ############################################################################### #

# sapply(tables()$NAME, function(x) {cat(paste0('\n', x, ' also has index ', indices(get(x)))); cat('\n'); head(get(x),2)})
# 
#            NAME      NROW NCOL  MB                 COLS       KEY
# 1:    bgid2fips   242,335    2  18          bgid,bgfips      bgid
# 2: blockid2fips 8,174,955    2 593    blockid,blockfips   blockid
# 3:  blockpoints 8,174,955    3 156      blockid,lat,lon   blockid
# 4:     blockwts 8,174,955    3 125 blockid,bgid,blockwt   blockid
# 5:     quaddata 8,174,955    4 218  ...
#                      ...BLOCK_X,BLOCK_Z,BLOCK_Y,blockid   blockid
# Total: 1,110MB

# 
# bgid2fips    also has index bgfips
# blockid2fips also has index blockfips
# blockpoints  also has index (none) 
# blockwts     also has index bgid
# 
# $bgid2fips
#    bgid       bgfips
# 1:    1 010010201001
# 2:    2 010010201002
# 
# $blockid2fips
#    blockid       blockfips
# 1:       1 010010201001000
# 2:       2 010010201001001
# 
# $blockpoints
#    blockid      lat       lon
# 1:       1 32.47070 -86.48048
# 2:       2 32.46817 -86.48166
# 
# $blockwts
#    blockid bgid    blockwt
# 1:       1    1 0.03652174
# 2:       2    1 0.05913043

# $quaddata
#     BLOCK_X  BLOCK_Z   BLOCK_Y blockid
# 1: 205.0423 2125.461 -3333.775       1
# 2: 204.9796 2125.314 -3333.872       2

# indices(bgid2fips)    # [1] "bgfips"
# indices(blockid2fips) # [1] "blockfips"
# indices(blockwts)     # [1] "bgid"

# str(bgid2fips)
# str(blockid2fips)
# str(blockwts)
# str(blockpoints)



## Checked on this but not worth doing this to shrink files and memory usage:
##
## could make the tables use less memory if dropped the blockid that is simply the rownumber,
## but would need to recode functions that assumed it is there and 
## saves only 30MB each for 2 tables in RAM,
## saves about 
#  blockpoints_tiny = copy(blockpoints)
#  blockpoints_tiny$blockid <- NULL
#  blockid2fips_tiny = copy(blockid2fips)
#  blockid2fips_tiny$blockid <- NULL
#  tables()
##                 NAME      NROW NCOL    MB                 COLS     KEY
##
## 2:      blockid2fips 8,174,955    2   593    blockid,blockfips blockid
## 3: blockid2fips_tiny 8,174,955    1   561            blockfips        
##
## 4:       blockpoints 8,174,955    3   156      blockid,lat,lon blockid
## 5:  blockpoints_tiny 8,174,955    2   125              lat,lon         
#
# on disk sizes:
# #
# save(blockid2fips,      file = 'big.rda')
# save(blockid2fips_tiny, file = 'tiny.rda')
# paste0(prettyNum(round(file.info('big.rda')$size / 1e6,1), big.mark = ','),'MB')  # [1] "39.5MB"
# paste0(prettyNum(round(file.info('tiny.rda')$size / 1e6,1), big.mark = ','),'MB') # [1] "22.1MB" saves 17.4 MB file size
# file.remove('big.rda')
# file.remove('tiny.rda')
# rm(blockid2fips_tiny)
# 
# save(blockpoints       ,file = 'big.rda')
# save(blockpoints_tiny, file = 'tiny.rda')
# paste0(prettyNum(round(file.info('big.rda')$size / 1e6,1), big.mark = ','),'MB') # [1] "124.6MB"
# paste0(prettyNum(round(file.info('tiny.rda')$size / 1e6,1), big.mark = ','),'MB') # 1] "107.2MB" saves 17.4 MB file size
# file.remove('big.rda')
# file.remove('tiny.rda')
# rm(blockpoints_tiny)
