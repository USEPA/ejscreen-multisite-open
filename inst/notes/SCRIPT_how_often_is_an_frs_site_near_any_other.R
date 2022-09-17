# Idea of precalculated table for FRS sites, storing  siteid - blockid - distance 
#
# 
# see   2022-09-06 notes for ECHO-EJAM discussion.txt


# Approx 57% of a random group of 3,000 frs sites (of the 3.1 million) are within 10 km of any other in the group (6.2 miles)
# > frsnearothers(3000,6.2)
# 1718, or 58 percent are within 6.2 miles of any other on list.

# 1023, or 51 percent are within 6 miles of any other on list. 3 mile radius circles overlap for 51%
# > frsnearothers(3000, 4)
# 1364, or 45 percent are within 4 miles of any other on list. 2 miles radius circles overlap for 45%
# > frsnearothers(3000, 2)
# 764, or 25 percent are within 2 miles of any other on list.  1 mile radius circles overlap for 25%
# > frsnearothers(3000, 1)
# 340, or 11 percent are within 1 miles of any other on list.
# 

library(proxistat)

library(frsdata)
data(package='frsdata')
# frs is the table with newer data but less useful formatting so far. and many more sites.
# frs_naics_2016 is older, so fewer sites, but in long format, ie has multiple rows for a site (registry id) if it has multiple programs/program ids

# > names(frsdata::frs)
# [1] "REGISTRY_ID"    "PRIMARY_NAME"   "SITE_TYPE_NAME" "PGM_SYS_ACRNMS" "INTEREST_TYPES" "NAICS_CODES"    "SIC_CODES"      "LAT"            "LONG"          
# > names(frsdata::frs_naics_2016)
# [1] "PROGRAM"     "PROGRAM_ID"  "REGISTRY_ID" "NAICS"       "LAT"         "LONG"       
#
# > dim(frs) # 
# [1] 3,144,206       9  The latest frs has 3.1 mill sites while the older one had 1.8 mill. ? 
# > dim(frs_naics_2016)
# [1] 1,847,061       6

# > length(unique(frs$REGISTRY_ID))
# [1] 3,14,4206
# > length(unique(frs_naics_2016$REGISTRY_ID))  
# [1] 1,237,040


count_near_eachother <- function(pts, miles_lte, returnfullmatrix=FALSE, return_min_dist=FALSE, silent=FALSE) {
  # pts <- cbind(lat=lat, lon=lon)
  distancematrix <- proxistat::get.distances.all(pts, pts, return.crosstab = TRUE)
  n <- NROW(pts)
  # replace diagonal with NA values to ignore distance from itself to itself
  for (i in 1:n) distancematrix[i, i] <- NA
  if (returnfullmatrix) return(distancematrix)
  mindist <- apply(distancematrix, 1, min, na.rm=TRUE)
  if (return_min_dist) return(mindist)
  count_w_any_near <- sum(mindist <= miles_lte) 
  if (!silent) {
    cat(paste0(count_w_any_near, '/',n, ', or ', 100*round(count_w_any_near/n,2), ' percent are within ', miles_lte, ' miles of any other on list.\n'))
  }
  invisible(count_w_any_near)
}

samplefrslatlon <- function(n=10) {
  these <- sample(1:NROW(frsdata::frs), n, replace = FALSE)
  sites <- frsdata::frs[these, c('LAT', 'LONG')]
  names(sites) <- gsub('LAT', 'lat', names(sites))
  names(sites) <- gsub('LONG', 'lon', names(sites))
  return(sites)  
}


frsnearothers <- function(n, d, silent=FALSE, ...) {
  pts = samplefrslatlon(n)
  x= count_near_eachother(pts, d, silent=silent, ...)
  invisible(x)
}

# n=10
# d=2000
# # pts = samplefrslatlon(n)
# # x = count_near_eachother(pts, d) 
# 
# frsnearothers(10,2000)
# for (d in c(2000, 1000, 100, 10, 5, 1))

monte <- function(trials=10, n=1000, d=6.2,silent=FALSE,...) {
  x=vector()
  for (i in 1:trials) {
    x[i] <- frsnearothers(n,d,silent=silent,...)
  }
  z <- round(100*mean(x)/n,0)
  cat(paste0('\n', z, '% of a random set of ', n,' FRS sites will be within ', d, ' miles of at least 1 of the others, on average.\n\n'))
  invisible(z)
}

multimonte <- function(
    trials=30,   silent=TRUE,
    distances= c(1,2,3,4,5,6,7,8,9,10),
    sectorsizes=c(10,33,50,100,300,500,1000)
) {
  res=matrix(data = NA, nrow=length(distances), ncol = length(sectorsizes), dimnames = list(distance_2r=distances, sitecount=sectorsizes))
  for (i in 1:length(distances)) {
    for (ii in 1:length(sectorsizes)) {
      d=distances[i]
      n=sectorsizes[ii]
      res[i,ii] <- monte(n=n, d=d,trials=trials, silent=silent)
      # browser()
    }
  }
  return(res)
}



#########################################################################################

# Even if a rule has only 200 sites, 
# 14% of a random set of 200 FRS sites will be within 6.2 miles of at least 1 of the others, on average.
# So 14% of the sites may have at least some double-counted residents since 5km circle overlaps with another.
#  monte(n=200,d=6.2,trials = 30)


# With a larger rule, it is worse: 
# In a 5km proximity analysis if there are 5k sites randomly picked from FRS,
#  there are double-counted residents at most of those 5k sites (circles overlap). maybe even 2/3 which is how many slightly overlap with some other.

# sometimes a few sites out of a group of 5000 sites will have none within 50 (1% might) 
# or even 100 miles (just 9 sites out of 5000, say, might have none of other 4,999 within 100 miles)
# but normally 1/3 have none within 6.2 miles if there are 5k sites.
# 
x <- frsnearothers(5000,6.2,return_min_dist=TRUE)
png(filename = 'histo dist to nearest other FRS if 5k sites.png')
hist(log10(x),100, main='5000 sites, dist to nearest_other_site')
dev.off()
summary(x)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.473   3.468   7.554   8.731 575.098 

frsnearothers(5000,6.2,return_min_dist=FALSE)
# 3414/5000, or 68 percent are within 6.2 miles of any other on list.
# in other words, 1/3 have none of the other 4,999 within 6.2 miles (10km, or radius of 5km at each)

#########################################################################################

 multimonte(distances = c(1,), sectorsizes = c(100,1000,10000))


x <- multimonte() # long slow - should recode to check multiple distances within the same 
x
save(x, file = 'pct of set of n FRS sites thats expected to be within d miles of another.rda')


##########################
png(width=1000, height=800,filename = 'what pct of n FRS sites are near each other so residents in overlapping circular buffers.png')

plot(x[,1],type='l',
     main='What % of an analyzed group of FRS facilities are near each other', 
     xlab='miles from each other (half of this distance is when some residents can be near both sites as circles overlap if distance apart <= 2x Radius)', 
     ylab='percent of that group of sites that are within that distance of any other in group', 
     ylim=c(min(x),max(x)))# for (i in 1:10) 

for (i in 2:ncol(x)) {points(x[,i], type='b', col=colors()[i*4])}
legend(2,40,legend = rev(paste(colnames(x), 'sites')), fill = rev(colors()[1:ncol(x)*4]))

dev.off()

##########################



# > for (i in 1:10) print(frsnearothers(1000, 6.2))
# 375, or 38 percent are within 6.2 miles of any other on list.
# [1] 375
# 342, or 34 percent are within 6.2 miles of any other on list.
# [1] 342
# 370, or 37 percent are within 6.2 miles of any other on list.
# [1] 370
# 353, or 35 percent are within 6.2 miles of any other on list.
# [1] 353
# 396, or 40 percent are within 6.2 miles of any other on list.
# [1] 396
# 362, or 36 percent are within 6.2 miles of any other on list.
# [1] 362
# 380, or 38 percent are within 6.2 miles of any other on list.
# [1] 380
# 370, or 37 percent are within 6.2 miles of any other on list.
# [1] 370
# 385, or 38 percent are within 6.2 miles of any other on list.
# [1] 385
# 335, or 34 percent are within 6.2 miles of any other on list.
# [1] 335
# > for (i in 1:10) print(frsnearothers(2000, 6.2))
# 983, or 49 percent are within 6.2 miles of any other on list.
# [1] 983
# 976, or 49 percent are within 6.2 miles of any other on list.
# [1] 976
# 972, or 49 percent are within 6.2 miles of any other on list.
# [1] 972
# 1022, or 51 percent are within 6.2 miles of any other on list.
# [1] 1022
# 938, or 47 percent are within 6.2 miles of any other on list.
# [1] 938
# 1026, or 51 percent are within 6.2 miles of any other on list.
# [1] 1026
# 963, or 48 percent are within 6.2 miles of any other on list.
# [1] 963
# 971, or 49 percent are within 6.2 miles of any other on list.
# [1] 971
# 998, or 50 percent are within 6.2 miles of any other on list.
# [1] 998
# 1029, or 51 percent are within 6.2 miles of any other on list.
# [1] 1029
# 
#   
#   