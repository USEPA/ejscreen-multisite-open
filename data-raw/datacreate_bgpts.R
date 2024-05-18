


#########     THIS IS ONLY A DRAFT -- NEEDS CLEANUP BEFORE USED AGAIN


# How lat lon were estimated for bgpts dataset

help(bgpts, package = "EJAM")

# For Census 2020 blocks, create pop wtd centroids lat lon for each block group ####
#  using  blockwts and   blockpoints
EJAM:::metadata_check 

bgpts_blocks <- copy(blockpoints) # not essential but ok to make sure we do not change blockpoints itself by reference in data.table operations
# all.equal(bgpts$blockid , blockwts$blockid)
bgpts_blocks[ , bgid    := blockwts$bgid]
bgpts_blocks[ , blockwt := blockwts$blockwt]
# get pop wtd mean of lat, and same for lon, by bgid
bgpts <- bgpts_blocks[ , lapply(.SD, FUN = function(x) stats::weighted.mean(x, w = blockwt, na.rm = TRUE)), .SDcols = c('lat', 'lon') , by = 'bgid']
rm( bgpts_blocks)
# add the bgfips column, so it has bgfips, bgid, lat, lon
# all.equal(bgpts$bgid,bgid2fips$bgid)
bgpts[ , bgfips := bgid2fips$bgfips]
# setnames(bgpts, 'bgfips', 'FIPS')

# BUT NOTE this census2020 block table has PR but lacks "AS" "GU" "MP" "VI" ####
# > uniqueN( blockid2fips[,substr(blockfips,1,2)])
# [1] 52
# length(unique(EJSCREEN_Full_with_AS_CNMI_GU_VI$ST_ABBREV)) # which is in the package EJAMejscreendata
# [1] 56
#   dim(bgejam)
# [1] 242,940    155
#   dim(bg22)
# [1] 242,335    157
#
# so how do we get latlon for bg in as/gu/mp/vi ?  ?####

# view those block group points on a map (plot only a subset which is enough)
sam <- sample(seq_along(bgpts$bgid),5000)
plot(x = bgpts$lon[sam], y = bgpts$lat[sam], pch = '.')

# view one state, florida, where 12 are the 1st 2 digits of the FIPS:
# bgpts[bgid2fips[substr(bgfips,1,2) == '12', ], on = 'bgid']
xx <- '12'
mystate <- bgpts[bgid2fips[substr(bgfips, 1, 2) == xx, ], on = 'bgid'][ , .(lon, lat)]
plot(mystate, pch = '.')
rm(mystate, xx)

############################################# # 
# How blockcounts were done:

# need  data.table pkg
bg_blockcounts <- blockwts[ , .(blockcount = uniqueN(.SD)), by = bgid]
sum(bg_blockcounts$blockcount == 1)
# [1] 1874 blockgroups have only 1 block
sum(bg_blockcounts$blockcount == 1000) # the max is 1000 blocks in a bg
# # [1] 22
round(100 * table(bg_blockcounts[blockcount < 20, blockcount]) / nrow(bg_blockcounts), 1)
# about 1 to 3 % of all bg contain 10 blocks, 5 blocks, 2 blocks, etc:
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
# 0.8 1.2 1.3 1.4 1.5 2.1 2.2 2.4 2.6 2.8 2.8 3.0 3.0 2.9 3.0 2.9 2.8 2.7 2.5
all.equal(bgpts$bgid, bg_blockcounts$bgid)
bgpts[ , blockcount := bg_blockcounts$blockcount]
dim(bgpts)
# 242335  x    5

# add metadata -------------------   CHECK THIS AND/OR UPDATE/CHANGE IT TO HAVE BETTER METADATA ***

bgpts <- EJAM:::metadata_add(bgpts, metadata = list(date_updated = Sys.Date()))

# save for EJAM package

usethis::use_data(bgpts, overwrite = TRUE) 
