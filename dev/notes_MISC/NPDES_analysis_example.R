# npdes dischargers analysis example

library(EJAM)
library(data.table)

myprogram <- "NPDES"  # 384k
# myprogram <- "CAMDBS" # 739 sites
pts <- frs_from_program(myprogram)[ , .(lat, lon, REGISTRY_ID,  PRIMARY_NAME)]
pts[, ST := state_from_latlon(lon = lon, lat=lat)$ST]

nrow(pts) 
# cbind(sort(table(pts$ST)))  # how many under this program are in each state?
# AS     9
# MP    26
# DE   130
# GU   171
# VI   217
# DC   453  500 sites in DC
# ...
# MD  4124   4 thousand locations !
# MS  4229
# ...
# IN  9947
# TN 10763  10 thousand
# WV 10993
# UT 13312
# CA 14192
# LA 15187
# AL 15776
# FL 16345
# PA 20163
# TX 51923
# GA 55681   55 thousand sites

# view these points for one state only
mapfast(pts[ST == 'MD',], radius = 0.1) # 0.1 miles radius circles

x <- ejamit(pts[ST == 'DC', ], radius = 1)

