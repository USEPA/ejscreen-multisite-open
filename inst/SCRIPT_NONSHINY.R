# script example of running proximity analysis without Shiny app
# ****  presumes that other data are in global environment, ****
# **** like blockgroupstats, quaddata, etc. ****
if (FALSE) {
  
  # DEMOG SUBGROUPS ARE NOT IN LOOKUP TABLES OF PERCENTILES YET, SO NEED TO BE.
  # state.pctile.  all NA values so far
  
  
  # should getblocksnearby() redirect now to getblocksnearbyviaQuadTree or to getblocksnearbyviaQuadTree2 ??
  
  # See details in help for ?EJAM
  
  # and facilities_prep may be obsolete or should be done before save that as dataset and build a package.
  
  
  # set up parameters, functions ####
  # includes library(EJAM) which provides datasets like blockgroupstats, facilities, etc.
  library(blockdata) # for 2020 data. 
  library(EJAM)
  library(data.table)
  # data("blockwts") # should lazy load from blockdata pkg
  
  # SLOW - takes about 5 seconds ------------
  # This must be done for each session?? - One cannot save it as .rda and just load via a pkg. 
  system.time({
    localtree <- SearchTrees::createTree(blockdata::quaddata, treeType = "quad", dataType = "point")
  })
  
  # CountCPU <- 2
  CountCPU <- parallel::detectCores()
  indexgridsize <- 10  # This does not seem to be used ever - it is just used to create buffer_indexdistance which is not used.
  
  # to use random test points (sites) ######
  # sitepoints <- EJAM::points100example %>% head(1)# data in this package
  
  # to use 100 testpoints
  sitepoints <- data.table::copy(EJAM::points100example) # [1:5, ])
  # sitepoints <- data.table::copy(EJAM::points1000example)  
  # NOTE the first point is far outside the continental US and returns no data using census 2010 blocks.
  sitepoints[ , siteid := .I] # .I JUST NUMBERS THE SITES
  data.table::setnames(sitepoints, 'LAT', 'lat')
  data.table::setnames(sitepoints, 'LONG', 'lon')
  data.table::setkey(sitepoints) #,  c('siteid', 'lat', 'lon'))
  # head(sitepoints)
  # lon      lat siteid
  # 1: -156.5141 20.88059     80
  # 2: -122.2602 37.97029     29
  # 3: -122.2134 37.47238     68
  # 4: -120.5462 46.60350     82
  # 5: -120.4014 34.86113     85
  
  # specify radius for circular buffer and other key parameters ####
  
  radius <- 1 # radius (miles)
  maxcutoff <- 31.07 # 50 km  # max distance to expand search to, if avoidorphans=TRUE
  avoidorphans <- TRUE  # Expand distance searched, when a facility has no census block centroid within selected buffer distance.
  
  uniqueonly <- FALSE   # The uniqueonly parameter will be removed from getblocksnearby  and handled in doaggregate() 
  
  ############################################################################ #
  
  # call function that finds nearby blocks  ####
  
  system.time({  # about under 1 second for 100 sites, but 7.25 seconds for 1,000 sites, or 500k/hour for this step
    sites2blocks <- EJAM::getblocksnearby(
      sitepoints =  sitepoints,
      cutoff = radius,
      maxcutoff = maxcutoff,
      # uniqueonly = uniqueonly,
      avoidorphans = avoidorphans,
      quadtree = localtree
    )
  }) # end of timed function
  
  #  head(sites2blocks)
  ##    blockid  distance siteid
  ## 1:  388798 0.9879380     29
  ## 2:  388799 0.8800507     29
  ## 3:  388809 0.6421582     29
  # save.image(file = 'saved image so far in testing.rda')
  # can use this example dataset of 99 sites, with 11,567 nearby blocks:
  # sites2blocks_example
  
  out <- doaggregate(sites2blocks = sites2blocks)
  names(out)
  # [1] "results_overall" "results_bysite"
  
  cbind(prettyNum((out$results_overall[ , ..names_all])))
  
  cbind(prettyNum((out$results_overall[ , ..names_e])))
  cbind(prettyNum((out$results_overall[ , ..names_d])))
  cbind(prettyNum((out$results_overall[ , ..names_d_subgroups])))
  cbind(prettyNum((out$results_overall[ , ..names_e_pctile])))
  cbind(prettyNum((out$results_overall[ , ..names_d_pctile])))
  cbind(prettyNum((out$results_overall[ , ..names_ej_pctile])))
  
  
  
  # view output of batch run aggregation ####
  cbind(prettyNum((out$results_overall)))
  
  # pop                                          "1569459"    
  # nonmins                                      "729112.9"   
  # mins                                         "840342.4"   
  # lowinc                                       "449458"     
  # povknownratio                                "1510796"    
  # lths                                         "134308.1"   
  # age25up                                      "1102262"    
  # lingiso                                      "53929.4"    
  # hhlds                                        "634408.5"   
  # under5                                       "80827"      
  # over64                                       "228583.8"   
  # unemployed                                   "46864.1"    
  # unemployedbase                               "870289.2"   
  # pre1960                                      "336183.9"   
  # builtunits                                   "705888"     
  # nhwa                                         "729112.9"   
  # hisp                                         "374840.7"   
  # nhba                                         "214512.4"   
  # nhaa                                         "189684.8"   
  # nhaiana                                      "3355.1"     
  # nhnhpia                                      "1932.9"     
  # nhotheralone                                 "5926"       
  # nhmulti                                      "45466.3"    
  # pm                                           "9.128339"   
  # o3                                           "43.05075"   
  # cancer                                       "34.84424"   
  # resp                                         "0.5053104"  
  # dpm                                          "0.8765034"  
  # pctpre1960                                   "0.4762567"  
  # traffic.score                                "1394.102"   
  # proximity.npl                                "0.2377933"  
  # proximity.rmp                                "0.8407776"  
  # proximity.tsdf                               "13.31841"   
  # proximity.npdes                              "139.1055"   
  # ust                                          "13.01984"   
  # EJ.DISPARITY.pm.eo                           "26.98502"   
  # EJ.DISPARITY.o3.eo                           "21.14005"   
  # EJ.DISPARITY.cancer.eo                       "35.8879"    
  # EJ.DISPARITY.resp.eo                         "35.94551"   
  # EJ.DISPARITY.dpm.eo                          "33.4167"    
  # EJ.DISPARITY.pctpre1960.eo                   "25.99354"   
  # EJ.DISPARITY.traffic.score.eo                "30.19309"   
  # EJ.DISPARITY.proximity.npl.eo                "31.01578"   
  # EJ.DISPARITY.proximity.rmp.eo                "25.82143"   
  # EJ.DISPARITY.proximity.tsdf.eo               "34.06108"   
  # EJ.DISPARITY.proximity.npdes.eo              "30.39377"   
  # EJ.DISPARITY.ust.eo                          "30.31742"   
  # pctover64                                    "0.145645"   
  # pctunder5                                    "0.05149993" 
  # pcthisp                                      "0.2388344"  
  # pctnhwa                                      "0.4645634"  
  # pctnhba                                      "0.1366792"  
  # pctnhaiana                                   "0.002137744"
  # pctnhaa                                      "0.12086"    
  # pctnhnhpia                                   "0.001231571"
  # pctnhotheralone                              "0.003775825"
  # pctnhmulti                                   "0.02896942" 
  # pctmin                                       "0.5354346"  
  # pctlowinc                                    "0.2974975"  
  # pctlths                                      "0.1218478"  
  # pctlingiso                                   "0.08500737" 
  # pctunemployed                                "0.05384888" 
  # VSI.eo                                       "0.416466"   
  # pctile.pm                                    "66"         
  # pctile.o3                                    "58"         
  # pctile.cancer                                "90"         
  # pctile.resp                                  "95"         
  # pctile.dpm                                   "96"         
  # pctile.pctpre1960                            "71"         
  # pctile.traffic.score                         "86"         
  # pctile.proximity.npl                         "88"         
  # pctile.proximity.rmp                         "71"         
  # pctile.proximity.tsdf                        "97"         
  # pctile.proximity.npdes                       "99"         
  # pctile.ust                                   "92"         
  # pctile.VSI.eo                                "66"         
  # pctile.pctmin                                "69"         
  # pctile.pctlowinc                             "54"         
  # pctile.pctlths                               "63"         
  # pctile.pctlingiso                            "82"         
  # pctile.pctunder5                             "51"         
  # pctile.pctover64                             "46"         
  # pctile.pctunemployed                         "61"         
  # pctile.pct.unemployed                        "NA"         
  # pctile.pctnhwa                               "NA"         
  # pctile.pcthisp                               "NA"         
  # pctile.pctnhba                               "NA"         
  # pctile.pctnhaa                               "NA"         
  # pctile.pctnhaiana                            "NA"         
  # pctile.pctnhnhpia                            "NA"         
  # pctile.pctnhotheralone                       "NA"         
  # pctile.pctnhmulti                            "NA"         
  # pctile.EJ.DISPARITY.pm.eo                    "75"         
  # pctile.EJ.DISPARITY.o3.eo                    "71"         
  # pctile.EJ.DISPARITY.cancer.eo                "73"         
  # pctile.EJ.DISPARITY.resp.eo                  "76"         
  # pctile.EJ.DISPARITY.dpm.eo                   "79"         
  # pctile.EJ.DISPARITY.pctpre1960.eo            "75"         
  # pctile.EJ.DISPARITY.traffic.score.eo         "76"         
  # pctile.EJ.DISPARITY.proximity.npl.eo         "79"         
  # pctile.EJ.DISPARITY.proximity.rmp.eo         "72"         
  # pctile.EJ.DISPARITY.proximity.tsdf.eo        "79"         
  # pctile.EJ.DISPARITY.proximity.npdes.eo       "80"         
  # pctile.EJ.DISPARITY.ust.eo                   "77"         
  # state.pctile.pm                              "NA"         
  # state.pctile.o3                              "NA"         
  # state.pctile.cancer                          "NA"         
  # state.pctile.resp                            "NA"         
  # state.pctile.dpm                             "NA"         
  # state.pctile.pctpre1960                      "NA"         
  # state.pctile.traffic.score                   "NA"         
  # state.pctile.proximity.npl                   "NA"         
  # state.pctile.proximity.rmp                   "NA"         
  # state.pctile.proximity.tsdf                  "NA"         
  # state.pctile.proximity.npdes                 "NA"         
  # state.pctile.ust                             "NA"         
  # state.pctile.VSI.eo                          "NA"         
  # state.pctile.pctmin                          "NA"         
  # state.pctile.pctlowinc                       "NA"         
  # state.pctile.pctlths                         "NA"         
  # state.pctile.pctlingiso                      "NA"         
  # state.pctile.pctunder5                       "NA"         
  # state.pctile.pctover64                       "NA"         
  # state.pctile.pctunemployed                   "NA"         
  # state.pctile.pct.unemployed                  "NA"         
  # state.pctile.pctnhwa                         "NA"         
  # state.pctile.pcthisp                         "NA"         
  # state.pctile.pctnhba                         "NA"         
  # state.pctile.pctnhaa                         "NA"         
  # state.pctile.pctnhaiana                      "NA"         
  # state.pctile.pctnhnhpia                      "NA"         
  # state.pctile.pctnhotheralone                 "NA"         
  # state.pctile.pctnhmulti                      "NA"         
  # state.pctile.EJ.DISPARITY.pm.eo              "NA"         
  # state.pctile.EJ.DISPARITY.o3.eo              "NA"         
  # state.pctile.EJ.DISPARITY.cancer.eo          "NA"         
  # state.pctile.EJ.DISPARITY.resp.eo            "NA"         
  # state.pctile.EJ.DISPARITY.dpm.eo             "NA"         
  # state.pctile.EJ.DISPARITY.pctpre1960.eo      "NA"         
  # state.pctile.EJ.DISPARITY.traffic.score.eo   "NA"         
  # state.pctile.EJ.DISPARITY.proximity.npl.eo   "NA"         
  # state.pctile.EJ.DISPARITY.proximity.rmp.eo   "NA"         
  # state.pctile.EJ.DISPARITY.proximity.tsdf.eo  "NA"         
  # state.pctile.EJ.DISPARITY.proximity.npdes.eo "NA"         
  # state.pctile.EJ.DISPARITY.ust.eo             "NA"  
  
  # DONE - can look at out$results_overall  and out$results_bysite
  
  
  stop('done')
  ############################################################################ #
  # in one step, 
  # getblocksnearby_and_doaggregate ####
  system.time({
    out2 <- getblocksnearby_and_doaggregate(
      sitepoints =  sitepoints,
      cutoff = radius,
      maxcutoff = maxcutoff,
      # uniqueonly = uniqueonly,
      avoidorphans = avoidorphans,
      quadtree = localtree
    )
  })
  stop('done')
  #################################################################################
  
  # RESULTS FROM EJScreen 2.0 API for comparison
  for (fname in list.files('~/R/mypackages/ejscreenapi/R', pattern = '\\.R$',full.names = TRUE)) source(fname )
  outapi <- ejscreenapi_plus(sitepoints$lon, sitepoints$lat, radius = radius)
  # ejscreenapi_all() ???
  
  # weblinks <- outapi[ , c("EJScreenPDF", "EJScreenMAP" )]
  # outapi <- outapi[ , which(!(names(outapi) %in% c("EJScreenPDF", "EJScreenMAP" )))]
  # t(outapi[1:5,])
  ##  function that aggregates in each buffer  ####
  # system.time(
  # results_by_site <- EJAM::doaggregate(facilities = sitepoints, facilityblocks = results) 
  # )
  
  
  
  #################################################################################
  
}

#################################################################################
#################################################################################
#################################################################################
#################################################################################




stop('stopped here') 

# from doaggregate , as a script to test:  copied this code to here as a snapshot of what was doaggregate at one point...



countcols <- c(
  "pop", 'nonmins', "mins", 
  "lowinc",   "povknownratio",   
  "lths",     "age25up", 
  "lingiso",  "hhlds", 
  "under5", "over64",
  "unemployed",   "unemployedbase", # new in 2022
  'pre1960',  'builtunits',
  "nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone", "nhmulti" # not in EJScreen 2.0 but will use here
)
calculatedcols <- c(
  # "VSI.eo", 
  "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
  "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
  "flagged"
)
popmeancols <- c(
  #    *** should EJ Index percentiles be here too? do we use the popwtd mean of the state percentiles, but the lookedup US percentile of the popwtd mean raw EJ score??? ****** e.g.,  state.pctile.EJ.DISPARITY.dpm.eo
  
  'VSI.eo',   # Demog.Index <- weighted.mean(Demog.Index, w = pop)
  "pm", "o3", "cancer", "resp", "dpm", 
  "pctpre1960", "traffic.score", 
  "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", 
  "ust", 
  "EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
  "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
  "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo", 
  "EJ.DISPARITY.ust.eo"
)

sites2blocks <- blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgfips), on='blockid']
sites2blocks[, sitecount_near_block := .N, by=blockid] # (must use the table with duplicate blocks, not unique only) 

data.table::setorder(sites2blocks, siteid, bgfips, blockid) # new

#table(sites2blocks$sitecount_near_block) 
sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt), by=c('bgfips', 'siteid')]

sites2blocks_overall <- unique(sites2blocks, by="blockid") 
sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt), by=bgfips]  
rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??

blockcount_by_site <- sites2blocks[, .(blockcount_near_site = .N), by=siteid] # new
bgcount_by_site <- sites2blocks[, .(bgcount_near_site = length(unique(bgfips))), by=siteid] # new
count_of_blocks_near_multiple_sites <- (NROW(sites2blocks) - NROW(sites2blocks_overall)) # NEW fraction is over /NROW(sites2blocks_overall)

sites2bgs_overall <- sites2blocks_overall[ , .(siteid, bgwt = sum(blockwt)), by=bgfips ]

sites2bgs_bysite  <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), by=.(siteid, bgfips)]
sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by=bgfips] # do we need/want this for overall summary??

# join to blockgroupdata from EJScreen or wherever

countcols      <- intersect(countcols,      names(blockgroupstats))
popmeancols    <- intersect(popmeancols,    names(blockgroupstats))
calculatedcols <- intersect(calculatedcols, names(blockgroupstats))

sites2bgs_plusblockgroupdate_bysite  <- merge(sites2bgs_bysite,  
                                              blockgroupstats[ , c('bgfips', 'ST', ..countcols, ..popmeancols, ..calculatedcols)], 
                                              all.x = TRUE, all.y=FALSE, by='bgfips')  
sites2bgs_plusblockgroupdate_overall <- merge(sites2bgs_overall, 
                                              blockgroupstats[ , c('bgfips', 'ST', ..countcols, ..popmeancols, ..calculatedcols)], 
                                              all.x = TRUE, all.y=FALSE, by='bgfips') 

results_overall <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols ]
results_bysite <- sites2bgs_plusblockgroupdate_bysite[ ,    lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols, by = .(siteid) ]
results_overall_popmeans <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), .SDcols = popmeancols ]
results_overall <- cbind(results_overall, results_overall_popmeans)
results_bysite_popmeans <- sites2bgs_plusblockgroupdate_bysite[ ,  lapply(.SD, FUN = function(x) stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), by = .(siteid), .SDcols = popmeancols ]
results_bysite <- merge(results_bysite, results_bysite_popmeans)

results_bysite <- merge(results_bysite, blockcount_by_site) # new
results_bysite <- merge(results_bysite, bgcount_by_site) # new

# hardcoded formulas for now
results_overall[ , `:=`(
  pctover64 = ifelse(pop==0, 0, over64 / pop),
  pctunder5 = ifelse(pop==0, 0, under5 / pop),
  pcthisp = ifelse(pop==0, 0, as.numeric(hisp ) / pop),
  pctnhwa = ifelse(pop==0, 0, as.numeric(nhwa ) / pop),
  pctnhba = ifelse(pop==0, 0, as.numeric(nhba ) / pop) ,
  pctnhaiana = ifelse(pop==0, 0, as.numeric(nhaiana ) / pop),
  pctnhaa = ifelse(pop==0, 0, as.numeric(nhaa ) / pop), 
  pctnhnhpia = ifelse(pop==0, 0, as.numeric(nhnhpia ) / pop),
  pctnhotheralone = ifelse(pop==0, 0, as.numeric(nhotheralone ) / pop), 
  pctnhmulti = ifelse(pop==0, 0, as.numeric(nhmulti ) / pop),
  pctmin = ifelse(pop==0, 0, as.numeric(mins ) / pop), 
  pctlowinc = ifelse( povknownratio==0, 0, lowinc / povknownratio),                                                                                                                      
  pctlths = ifelse(age25up==0, 0, as.numeric(lths ) / age25up), 
  pctlingiso = ifelse( hhlds==0, 0, lingiso / hhlds), 
  pctpre1960 = ifelse( builtunits==0, 0, pre1960 / builtunits),
  pctunemployed = ifelse(unemployedbase==0, 0, as.numeric(unemployed) / unemployedbase)
) ]
# cbind(sum = prettyNum(results_overall, big.mark = ','))

results_bysite[ , `:=`(
  pctover64 = ifelse(pop==0, 0, over64 / pop),
  pctunder5 = ifelse(pop==0, 0, under5 / pop),
  pcthisp = ifelse(pop==0, 0, as.numeric(hisp ) / pop),
  pctnhwa = ifelse(pop==0, 0, as.numeric(nhwa ) / pop),
  pctnhba = ifelse(pop==0, 0, as.numeric(nhba ) / pop) ,
  pctnhaiana = ifelse(pop==0, 0, as.numeric(nhaiana ) / pop),
  pctnhaa = ifelse(pop==0, 0, as.numeric(nhaa ) / pop), 
  pctnhnhpia = ifelse(pop==0, 0, as.numeric(nhnhpia ) / pop),
  pctnhotheralone = ifelse(pop==0, 0, as.numeric(nhotheralone ) / pop), 
  pctnhmulti = ifelse(pop==0, 0, as.numeric(nhmulti ) / pop),
  pctmin = ifelse(pop==0, 0, as.numeric(mins ) / pop), 
  pctlowinc = ifelse( povknownratio==0, 0, lowinc / povknownratio),                                                                                                                      
  pctlths = ifelse(age25up==0, 0, as.numeric(lths ) / age25up), 
  pctlingiso = ifelse( hhlds==0, 0, lingiso / hhlds), 
  pctpre1960 = ifelse( builtunits==0, 0, pre1960 / builtunits),
  pctunemployed = ifelse(unemployedbase==0, 0, as.numeric(unemployed) / unemployedbase)
) ]



# missing:  id, lat, lon, Demog.Index which is VSI.eo, state.avg., state.pctile., us.avg., pctile., 
#  ST, Statename, REGION, 
#  NUM_NPL, NUM_TSDF, 
#  StatLayerCount, StatLayerZeroPopCount, 
#  weightLayerCount which might be the count of blocks nearby???
# "timeSeconds", "radius.miles", "unit", "statlevel", "inputAreaMiles"

t(results_bysite[1:5,])
sum(results_bysite$pop)            ###############   PROBLEM IF THOSE DO NOT LOOK FAIRLY SIMILAR    *************
results_overall$pop

##################################################### #
# FIND PERCENTILES THOSE RAW SCORES REPRESENT  ####
#  VIA  lookup tables of US/State  percentiles
##################################################### #

# results_bysite
# results_overall

# Use the dataset called EJAM::usastats as the lookup table for USA percentiles and mean. 
#  was updated so it uses right variable names- replaced with the one from ejscreen pkg for EJScreen v2.1

# hard coded for now:
varsneedpctiles <- c(ejscreen::names.e, union(ejscreen::names.d, 'pctunemployed'), ejscreen::names.d.subgroups, ejscreen::names.ej)
varnames.us.pctile <- paste0('pctile.', varsneedpctiles)
varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)

us.pctile.cols_bysite <- data.frame(matrix(nrow = NROW(results_bysite), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite) <- varnames.us.pctile
state.pctile.cols_bysite <- data.frame(matrix(nrow = NROW(results_bysite), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite) <- varnames.state.pctile
us.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall) <- varnames.us.pctile
state.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_overall) <- varnames.state.pctile

for (i in seq_along(varsneedpctiles)) {
  # older code 2015 had used  EJAM::lookup.pctile.US(results_bysite[ , myvar], .....  i=1
  myvar <- varsneedpctiles[i]
  if (myvar %in% names(ejscreen::lookupUSA)) {
    us.pctile.cols_bysite[ , varnames.us.pctile[[i]]] <- ejanalysis::lookup.pctile(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = ejscreen::lookupUSA) 
    us.pctile.cols_overall[, varnames.us.pctile[[i]]] <- ejanalysis::lookup.pctile(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = ejscreen::lookupUSA) 
    # state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- ejanalysis::lookup.pctile(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = ejscreen::lookupStates, zone =  results_bysite$ST) 
    # It may not make sense to do state percentiles except for EJ Indexes, in the "overall" summary:
    # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- ejanalysis::lookup.pctile(results_overall[ , varsneedpctiles[i]], varname.in.lookup.table = varsneedpctiles[i], lookup = ejscreen::lookupStates, zone =  results_overall$ST)
  } else {
    us.pctile.cols_bysite[ , varnames.us.pctile[[i]]] <- NA
    us.pctile.cols_overall[, varnames.us.pctile[[i]]] <- NA
  }
}

##################
# possibly add the us.avg.  and the state.avg.  for each key variable repeated in each row(site), since the ejscreenAPI does that. 
##################



# shiny::runApp('~/R/mypackages/ejscreenapi')

