
# USING PARALLELIZATION FOR DISTANCE CALCULATIONS ####

library(foreach) # handles a for loop in a certain way
library(parallel)  #  
library(doParallel) # registers R session to make all processors available

# The doParallel package is a “parallel backend” for the foreach package. It provides a mechanism
# needed to execute foreach loops in parallel. The foreach package must be used in conjunction
# with a package such as doParallel in order to execute code in parallel. The user must register a
# parallel backend to use, otherwise foreach will execute tasks sequentially, even when the %dopar%
#   operator is used.  The doParallel package acts as an interface between foreach and the parallel package of R
# 2.14.0 and later. The parallel package is essentially a merger of the 
# multicore package, which was written by Simon Urbanek, and the 
# snow package, which was written by Luke Tierney and others. 
# The multicore functionality supports multiple workers only on those operating systems
# that support the fork system call; this excludes Windows. 
# By default, doParallel uses 
#   multicore functionality on Unix-like systems and 
#   snow functionality on Windows. 
# Note that the multicore functionality only runs tasks on a single computer, not a cluster of computers. However, you can
# use the 
#   snow functionality to execute on a cluster... 

# SEE the doParallel package vignette on cran, gettingstartedParallel.pdf 

################################################################################################ #
# spatial tools: 
#
# Also, EJAM::getblocksnearby() is probably fastest since it preindexes blocks.
# The example uses st_distance() but it might be faster to 
# use is_within_distance() or  st_buffer() and st_intersect() first,
# to narrow down to nearby ones. 
# on sf package see https://r-spatial.github.io/sf/articles/
# https://r-spatial.github.io/sf/articles/sf1.html#crs
# browseURL( "https://r-spatial.github.io/sf/articles/sf7.html")

################################################################################################ #

# EXAMPLE using parallel processing  
# to get stats on 
# how many blockpoints are near each FRS site? ####

library(tidyverse) # dplyr:: for dplyr::filter(), dplyr::arrange(), dplyr::select(), distinct()
library(sf)
library(vroom)
library(foreach) # handles a for loop in a certain way
library(doParallel) # registers R session to make all processors available
library(parallel)  # this is where detectCores() is from, for example
library(data.table)

# register the Parallel back end ####
# To register doParallel to be used with foreach, you must call the registerDoParallel function.
# If you call this with no arguments, on Windows you will get three workers
#local cluster
useparallel <- TRUE
n_cores <- parallel::detectCores() - 1 # leave one free for other stuff the computer does
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)
# print(paste0("Starting iterations with ", n_cores, " cores"))
performance_summary <- data.frame()

################################################################################################ #

# set sample size and radius ####

radius = 5000 # 5km
sampleSize <- c(10, 100) # seq(1000,101000,10000) #  

# Specify sitepoints to analyze  ####

oldfolder <- getwd()
setwd("~/../Downloads")
mydata <-  data.table::setDF(data.table::copy(EJAM::testpoints_1000_dt[-1, ])) # first row is NA, not allowed in st_as_sf.data.frame() 
## convert to geo format for sf package ####
sf_all <- mydata %>% 
  # vroom("/work/projectname/mydata.csv") %>% 
  # dplyr::filter(year > 2015 & year < 2019) %>%
  dplyr::select(siteid, lon, lat) %>%
  dplyr::distinct() %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4269, remove = FALSE) %>% 
  sf::st_transform(3747)  # to use UTM 17N 
# https://r-spatial.github.io/sf/articles/sf1.html#crs

################################################################################################ #
to_blocks <- FALSE # DOES NOT WORK LIKE THIS
# Error in { : task 1 failed - "st_crs(x) == st_crs(y) is not TRUE"
# In addition: There were 15 warnings (use warnings() to see them)
if (to_blocks) {
  library(EJAMblockdata)
  end.pts <- data.table::setDF(data.table::copy( blockpoints))
  end.pts <- sf::st_as_sf(x = end.pts, coords = c("lon", "lat"), crs = 4269, remove = FALSE)
  # not sure if need st_transform  
}
################################################################################################ #


# foreach() creates loop over sites ####

logtime <- gsub(":","_",Sys.time())
logname <- paste0("inlooptable_",logtime,".csv")

# Try a few sample sizes, like 10 sites, 100 sites, 1000 sites
for (i in 1:length(sampleSize)) {
  
  s <- sample(seq(1, nrow(sf_all)), sampleSize[i], replace = FALSE)
  sf <- sf_all[s,]
  start.p <- Sys.time()
  
  if (useparallel) {
    
    df <- foreach::foreach(n=1:nrow(sf), .combine='rbind', .packages= c("tidyverse", "sf","vroom" )) %dopar% {
      
      start.pt <- sf[n,]  # get point to measure FROM
      
      # points measured TO could be all US blockpoints, or just others in this sample
      
      # if points are all US blockpoints:
      # 
      if (to_blocks) {
        distance <- as.numeric(sf::st_distance(start.pt,end.pts))
        # saves only summary stats for this frompoint, not distance to each topoint:
        newrow <- data.frame(
          siteid = start.pt$siteid, # only one here in this loop
          min_distance = min(distance),
          mean_distance = mean(distance),
          count_near = sum(distance <= radius)
        )
      } else {
        # points are just others in the sample:
        end.pts  <- sf  %>% 
          dplyr::filter(!siteid == start.pt$siteid)  # get points to measure TO
        end.pts$distance <- as.numeric(sf::st_distance(start.pt,end.pts) )
        # cat("\n units are ", attr(st_distance(start.pt,end.pts), "units"), '\n')
        # https://r-spatial.github.io/sf/articles/sf1.html#units
        ####################### # 
        #
       # How many of the blocks inside circle are part of a blockgroup on the edge?, i.e., 
       # not all blocks of that parent bg are in circle?
        
        
        
       
        ##################### #
        # JUST SAVE THE AVERAGE OF THE 5 SHORTEST DISTANCES?
        # order <- end.pts %>%
        #   dplyr::arrange(distance)   # shortest distances first
        # first.five <- order[1:5,]  #   if selecting only the 5 closest points
        #################### #
        newrow <- data.frame(
          siteid = start.pt$siteid, # only one here in this loop
          min_distance = min(end.pts$distance),
          # mean_distance = mean(first.five$distance),
          count_near = sum(end.pts$distance <= radius)
        )
      }
      # vroom::vroom_write(newrow, logname, delim=",", append=TRUE)
      newrow
    }

  } else { 
    # not parallelized
    print("NOT PARALLEL")
    for (n in 1:nrow(sf)) {
      distances_newrow <- list()
      start.pt <- sf[n,]  # get point to measure FROM
      end.pts  <- sf  %>% 
        dplyr::filter(!siteid == start.pt$siteid)  # get points to measure TO
      end.pts$distance <- as.numeric( sf::st_distance(start.pt,end.pts) )
      ####################### # 
      # JUST SAVE THE AVERAGE OF THE 5 SHORTEST DISTANCES 
      # order <- end.pts %>%
      #   dplyr::arrange(distance)   # shortest distances first
      # first.five <- order[1:5,]  # ? is that syntax ok???  if selecting only the 5 closest points
      #################### #
      distances_newrow[[n]] <- data.frame(
        siteid = start.pt$siteid, # only one here in this loop
        min_distance = min(end.pts$distance),
        # mean_distance = mean(first.five$distance),
        count_near = sum(end.pts$distance <= radius)
      )
    }
    df <- do.call(rbind, distances_newrow)
  }
  end.p <- Sys.time()
  
  # save distances for each sample
  write.csv(df, file = paste0("distances results_",nrow(sf),"_points at ", logtime, ".csv"), row.names = FALSE)
  
  # calculate summary stats for each sample but save compiled version across all samples
  time <- as.numeric(difftime(end.p, start.p, units = "secs"))
  print(paste0(
    "Completed ", nrow(sf), " iterations in ", 
    round(time), " seconds (", round(time/60,1), " minutes)  ",
    round(time/nrow(sf),5), " seconds per iteration - ",
    round(3600*nrow(sf)/time,0), " per hour, ",
    n_cores, " cores, ",
    "parallel= ", useparallel, ", ",
    radius, " is the radius in meters. "
  ))
  performance_summary_newrow <- data.frame(
    nIterations = nrow(sf), 
    seconds = time, 
    perhour= round((nrow(sf)/time) * 3600,0),  
    useparallel=useparallel, 
    n_cores=n_cores,
    radius = radius,
    mincountnear = min(df$count_near),
    meancountnear = mean(df$count_near),
    maxcountnear = max(df$count_near),
    min_distance_any = min(df$min_distance),
    # dist2avg_from_avg_pt = mean(df$mean_distance), # this was using mean of 5 closest only
    dist2nearest_from_avg_pt = mean(df$min_distance)
  )
  performance_summary <- rbind(performance_summary, performance_summary_newrow)
}
cat(" \n")
print(performance_summary)
write.csv(performance_summary, file =  paste0("performance checks summary ", logtime, ".csv"), row.names = FALSE)
setwd(oldfolder)
# parallel::stopCluster(cl)

