# see also    EJAM/inst/SPEED/NOTES_ON_SPEEDING_UP_APP.R

# see also EJAM::speedtest()

# results initial/early:
  
  
# EJAM 1K SITES: 205 k/hr   27x as fast as ejscreenapi app function
# user  system elapsed 
#   17.33    1.00   17.53   18 seconds versus 478 seconds, or 
# 0.29 minutes
# 205k/hour
# 
# EJScreen 1K SITES:  7.5 k/hr
# Rate of 7,547 buffers per hour (1,000 lat/long pairs per 477 seconds)
# Time difference of 8 mins
# Results were unavailable for 1 out of these 1000 sites. 
#    user  system elapsed 
#   12.22    0.88  478.07
# 
# EJAM 12.3K SITES: 271 k/hr  using ejamit() at 1 mile radius.
# 2.7 minutes for 12.3k sites, 75.18976/second
# > system.time({  y = ejamit(frs_from_naics(325,children=T),1) })
# Finding blocks nearby.
# Analyzing 12286 points, radius of 1 miles.
# user  system elapsed 
# 157.43    3.95  163.36    
# 



# EJAM vs EJScreen api can provide results for 
#    100 sites in 7 seconds vs in 21 seconds.
#  1,000 sites in 18 seconds vs waiting 8 minutes.
# 10,000 sites in <<3 minutes vs waiting 1 and 1/2 hours.

# note it is a bit faster if ST is provided for each point

t1=system.time({  x1=getblocksnearby(testpoints_1000,1);  save(x1,file = 'x1.rda');rm(x1)})
t3=system.time({  x3=getblocksnearby(testpoints_1000,3);  save(x3,file = 'x3.rda');rm(x3)})
t6=system.time({  x6=getblocksnearby(testpoints_1000,6);  save(x6,file = 'x6.rda');rm(x6)})

testpoints_10k <- testpoints_n(10000,"frs")

t1=system.time({  x1=getblocksnearby(testpoints_1000,1);  save(x1,file = 'x1.rda');rm(x1)})
t3=system.time({  x3=getblocksnearby(testpoints_1000,3);  save(x3,file = 'x3.rda');rm(x3)})
t6=system.time({  x6=getblocksnearby(testpoints_1000,6);  save(x6,file = 'x6.rda');rm(x6)})
 
