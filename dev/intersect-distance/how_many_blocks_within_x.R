
library(EJAM)
library(data.table)
testpoints_10k <- testpoints_n(10000,dt=FALSE)
sites2blocks10k <- getblocksnearby(testpoints_10k, 3.11)
setDT(sites2blocks10k)

setwd("~")

save(sites2blocks10k, file = "sites2blocks10k.rda")
p10k = copy(setDT(testpoints_10k))
p10k = p10k[, .(lat,lon, REGISTRY_ID)]
save(testpoints_10k, file = "testpoints_10k.rda")
x=copy(sites2blocks10k)

x[ , .N, by="siteid"]
