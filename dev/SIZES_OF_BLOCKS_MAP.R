junk <- data.table::copy(blockwts)
junk <- bgpts[junk, .(blockid, bgid, blockwt, block_radius_miles_round_temp)]

bsample <- junk[ sample(1:nrow(junk), 20000), ]
bsample <- bgpts[bsample, , on = "bgid"]

bsqmi <- junk[block_radius_miles >= 1, ]
bsqmi <- bgpts[bsqmi, , on = "bgid"]

bigonly <- junk[block_radius_miles >= 4, ]
bigonly <- bgpts[bigonly, , on = "bgid"]

biggestonly <- junk[block_radius_miles >= 9, ]     
biggestonly <- bgpts[biggestonly, , on = "bgid"]


plot(      bsample$lon, bsample$lat,     col = 'white', pch = '.', 
           main = "Of 8 million census blocks, 2% are at least 1 square mile (gray), 5k are at least 4 sqmi (red)
           and ~650 are 9 sqmi (circles), 206 blocks are >=16 sqmi and 61 are 25 to 55 sqmi")
points(      bsqmi$lon, bsqmi$lat,       col = 'lightgray'  , pch = '.')
points(    bigonly$lon, bigonly$lat,     col = 'red'  , pch = '.')
points(biggestonly$lon, biggestonly$lat, col = 'blue')


############################# # 
# but how often is some EPA-regulated facility in such a huge block?
# pick random sample of FRS sites with latlon info:

library(EJAM)
library(data.table)
pts <- testpoints_n(5000, weighting = 'frs')
setDT(pts)
pts[ , PGM_SYS_ACRNMS := NULL]


# EJAM::state_from_latlon

x <- getblocksnearby(pts, radius = 5, avoidorphans = F)
x2 <- getblocksnearby(pts, radius = 1, avoidorphans = F)
x3 <- getblocksnearby(pts, radius = 1, avoidorphans = T)

 
setDT(pts)
pts[ , PGM_SYS_ACRNMS := NULL]
which(x$distance > 5)
# 1640826 etc

this_id <-  (x[1640826, ejam_uniq_id])
y <- x[ejam_uniq_id == this_id, ]

setorder(y, "distance")
x[, siteid := ejam_uniq_id]
y[, siteid := ejam_uniq_id]




