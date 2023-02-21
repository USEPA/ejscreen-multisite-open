# EXAMPLE OF USING PARALLELIZATION FOR DISTANCE CALCULATIONS

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




# see  https://usepa.sharepoint.com/sites/oei_Community/HESC/SitePages/Home.aspx

# webinar transcript I saved:  "transcript webinar PARALLEL PROCESSING via EPA HPCC 2023-02-17.txt"
# webinar video: https://usepa-my.sharepoint.com/personal/vega_ann_epa_gov/_layouts/15/stream.aspx?id=%2Fpersonal%2Fvega%5Fann%5Fepa%5Fgov%2FDocuments%2FRecordings%2FR%20user%20group%2Dmonthly%20webinar%20series%2D20230215%5F140239%2DMeeting%20Recording%2Emp4&ga=1 


# USING EPA ATMOS - HIGH PERFORMANCE CLUSTER 
# - you need an account.
# MOSTLY it is AVAIL FOR ORD AND OAR, IN RTP, BUT OTHER OFFICES MIGHT HAVE A WAY TO GET TIME ON IT.
# YOU APPLY FOR PROJECT TIME ON THE SHAREPOINT SITE. ANYONE IN ORD CAN APPLY FOR A TESTING PROJECT, UP TO 3K HOURS USE-TIME.
# YEARLY YOU SUBMIT PROPOSAL FOR TIME. <20K HOURS DO NOT NEED EXTRA FUNDING OR APPROVALS.

# NEED TO KNOW WHICH SETUP TO USE !! (or it is slower than not using it at all!)
# Each node is for a certain purpose- speed, memory, etc.
# "compute" node has 32 cores (high perform laptop has 16).
# largemem node has 72 cores or 4 processors per node, 1 node, but 1536 GB memory per node.
# for example, dataset of 6k wells, find 5 nearest neighbors and calc mean distance,
#  1 epa high perf laptop = 20 minutes (0.18 seconds each iteration)
#  same but 8 cores, 3 minutes.
#  Atmos- largemem, 1 core, WORSE:  48 minutes. and 71 cores is 39 minutes.
# Atmos - compute node, 1 core is 42 minutes.
# Atmos compute node with 31 cores, though, is 1.2 minutes !! NEED TO KNOW WHICH SETUP TO USE !! 
#
# STEPS in using Atmos High performance cluster: (see webinar)
# 
# get Atmos account &apply for test-project time via sharepoint site. 
# need download Mobaxterm free epa-approved software. This is the SSH client to use.
# set up ssh connection with EPA credentials. atmos1.nesc.epa.gov, port 22.
# config your login behavior, not simple.   The .tcshrc file in home dir does that, see template the speaker provided (R User webinar 2/17/23)
# keep your data and scripts in your work dir, not your home dir. 
# copy data and scripts to Atmos.  can click/drag in  SSH session. can pull from EPA github repo 
# test script in R
# send script to your selected Atmos node 
# use the debug node just to test your code - up to 4 hrs "debug node session"  From that node you launch an RStudio session on that server, but you CANNOT INSTALL R PACKAGES ON ATMOS - IT HAS 2K PKGS ALREADY BUT NOT YOUR OWN PKG. ASK SUPPORT STAFF TO INSTALL A CUSTOM PKG IF NEEDED. 
# Write the .sh file that tells Atmos (built on linux) what to do. you make that .sh file once and then can slightly modify it as needed. specifies amounts of memory need, which node, name of job, time needed, text files like mylog.err and mylog.out, to print() or cat() errors/msg to, stock list of modules you are using (try typing module help r) like rstudio, git, R, geos, gcc, etc.; and last line in .sh file is path to your R script you are running. 
# In cmd line of MobaXterm/Atmos session, to submit batch, type    sbatch R_User_Group/samplesize.sh    for example.  Remote host  is atmos1.nesc.epa.gov,  port 22. 
# type in file folder window,    home/username,   load modules,   then go to   work/username 
# check your data is on the server.
#  cd /work/myuserfolderprojectname 
#  cd /home/mcorrale  
#  srun --ntasks=32 --partition=debug --time=2:00:00 --xll --pty csh -l # a 2hour debug test session
#  rstudio  (typed on ssh command line) # to launch remote rstudio session
#  sbatch myfolder/samplesize_inloop.sh  # to submit job with file you made
#  squeue -u myusername  # to see what is running 

# on sf package see https://r-spatial.github.io/sf/articles/
# https://r-spatial.github.io/sf/articles/sf1.html#crs


########################################################################### # 


# The example uses st_distance() but it might be faster to use st_buffer() and st_intersect() first,
# to narrow down to nearby ones. 
# and EJAM::getblocksnearby() is probably fastest since it preindexes blocks.

## _* st_buffer() vs st_is_within_distance() ####

## _ * st_buffer() via R2 vs S2 (s2 pkg, spherical approach) ####

browseURL( "https://r-spatial.github.io/sf/articles/sf7.html")



# Example in R

library(tidyverse)
library(sf)
library(vroom)
library(foreach) # handles a for loop in a certain way
library(doParallel) # registers R session to make all processors available
library(parallel)  # this is where detectCores() is from, for example
library(data.table)

# also loads iterator package, etc.
# Conflicts ─────────────────
# ✖ purrr::accumulate() masks foreach::accumulate()
# ✖ dplyr::filter()     masks stats::filter()
# ✖ dplyr::lag()        masks stats::lag()
# ✖ purrr::when()       masks foreach::when()


radius = 5000 # 5km
sampleSize <- c(10, 100) # seq(1000,101000,10000) #  
useparallel <- TRUE
n_cores <- detectCores() - 1 # leave one free for other stuff the computer does

################################################################################################ #
to_blocks <- FALSE # DOES NOT WORK LIKE THIS
# Error in { : task 1 failed - "st_crs(x) == st_crs(y) is not TRUE"
# In addition: There were 15 warnings (use warnings() to see them)
# > warnings()
# Warning messages:
#   1: In for (i in seq_along(all_sfc_names)) df[[all_sfc_names[i]]] = x[[all_sfc_columns[i]]] :
#   closing unused connection 77 (<-LZ11MCORRALE.aa.ad.epa.gov:11769)
# 2: In for (i in seq_along(all_sfc_names)) df[[all_sfc_names[i]]] = x[[all_sfc_columns[i]]] :
#   closing unused connection 76 (<-LZ11MCORRALE.aa.ad.epa.gov:11769)
# 3: In for (i in seq_along(all_sfc_names)) df[[all_sfc_names[i]]] = x[[all_sfc_columns[i]]] :
#   closing unused connection 75 (<-LZ11MCORRALE.aa.ad.epa.gov:11769)
if (to_blocks) {
  library(EJAMblockdata)
    end.pts <- setDF(copy(EJAMblockdata::blockpoints))
    end.pts <- st_as_sf(x = end.pts, coords = c("lon", "lat"), crs = 4269, remove = FALSE)
# not sure if need st_transform  
}
################################################################################################ #

#  GET A DATASET OF POINTS 
oldfolder <- getwd()
setwd("~/../Downloads")
mydata <-  setDF(copy(EJAM::testpoints_1000_dt[-1, ])) # first row is NA, not allowed in st_as_sf.data.frame() 

# convert to geo format for sf package
sf_all <- mydata %>% 
  # vroom("/work/projectname/mydata.csv") %>% 
  # filter(year > 2015 & year < 2019) %>%
  select(siteid, lon, lat) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4269, remove = FALSE) %>% 
  st_transform(3747)  # to use UTM 17N 
# https://r-spatial.github.io/sf/articles/sf1.html#crs


# register parallel back end
# To register doParallel to be used with foreach, you must call the registerDoParallel function.
# If you call this with no arguments, on Windows you will get three workers
#local cluster
cl <- makeCluster(n_cores)
registerDoParallel(cl)
# print(paste0("Starting iterations with ", n_cores, " cores"))
performance_summary <- data.frame()

logtime <- gsub(":","_",Sys.time())
logname <- paste0("inlooptable_",logtime,".csv")

for (i in 1:length(sampleSize)) {
  s <- sample(seq(1, nrow(sf_all)), sampleSize[i], replace = FALSE)
  sf <- sf_all[s,]
  start.p <- Sys.time()
  
  if (useparallel) {
    df <- foreach(n=1:nrow(sf), .combine='rbind', .packages= c("tidyverse", "sf","vroom" )) %dopar% {
      start.pt <- sf[n,]  # get point to measure FROM
      
      # points measured TO could be all US blockpoints, or just others in this sample
      
      # if points are all US blockpoints:
      # 
      if (to_blocks) {
        distance <- as.numeric(st_distance(start.pt,end.pts))
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
          filter(!siteid == start.pt$siteid)  # get points to measure TO
        end.pts$distance <- as.numeric(st_distance(start.pt,end.pts) )
        # cat("\n units are ", attr(st_distance(start.pt,end.pts), "units"), '\n')
        # https://r-spatial.github.io/sf/articles/sf1.html#units
        
        ####################### # 
        # JUST SAVE THE AVERAGE OF THE 5 SHORTEST DISTANCES 
        order <- end.pts %>%
          arrange(distance)   # shortest distances first
        first.five <- order[1:5,]  #   if selecting only the 5 closest points
        #################### #
        
        newrow <- data.frame(
          siteid = start.pt$siteid, # only one here in this loop
          min_distance = min(end.pts$distance),
          mean_distance = mean(first.five$distance),
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
        filter(!siteid == start.pt$siteid)  # get points to measure TO
      end.pts$distance <- as.numeric( st_distance(start.pt,end.pts) )
      
      ####################### # 
      # JUST SAVE THE AVERAGE OF THE 5 SHORTEST DISTANCES 
      order <- end.pts %>%
        arrange(distance)   # shortest distances first
      first.five <- order[1:5,]  # ? is that syntax ok???  if selecting only the 5 closest points
      #################### #

      distances_newrow[[n]] <- data.frame(
        siteid = start.pt$siteid, # only one here in this loop
        min_distance = min(end.pts$distance),
        mean_distance = mean(first.five$distance),
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
    dist2nearest_from_avg_pt = mean(df$min_distance),
    dist2avg_from_avg_pt = mean(df$mean_distance)
    )
  performance_summary <- rbind(performance_summary, performance_summary_newrow)
}
cat(" \n")
print(performance_summary)
write.csv(performance_summary, file =  paste0("performance checks summary ", logtime, ".csv"), row.names = FALSE)

setwd(oldfolder)



# parallel::stopCluster(cl)


# 

















