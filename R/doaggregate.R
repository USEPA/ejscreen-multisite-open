#' Summarize indicators in each buffer (given the blocks in each buffer and indicators for each block)
#'
#' @description This updated 2023 code takes a set of facilities and the set of blocks that are near each,
#'   (as identified previously, in other code that has identified which blocks are nearby)
#'   and combines those with indicator scores for block groups.
#'
#' @details
#'   For all examples, see [getblocksnearbyviaQuadTree()]
#'   
#'   This function aggregates the blockgroup scores to create a summary of each indicator,
#'    as a raw score and US percentile and State percentile,
#'    in each buffer (i.e., near each facility):
#'    
#'    - **SUMS OF COUNTS**: for population count, or number of households or Hispanics, etc.
#'      
#'    - **POPULATION-WEIGHTED MEANS**: for  Environmental indicators.
#'      
#'        ***EJ Indexes**:* These could be in theory recalculated via formula, but the way EJScreen 
#'          does this is apparently finding the pop wtd mean of EJ Index raw scores,
#'          not the EJ Index formula applied to the summarized demographic score and aggregated envt number.
#'          
#'    - **CALCULATED BY FORMULA**: Buffer or overall score calculated via formulas using aggregated counts, 
#'          such as percent low income = sum of counts low income / sum of counts of denominator, 
#'          which in this case is the count of those for whom the poverty ratio is known.
#'          
#'    - **LOOKED UP**: Aggregated scores are converted into percentile terms via lookup tables (US or State version).
#'
#'   This function requires the following as data lazy loaded for example from EJAMblockdata package:
#'   
#'    - blockwts: data.table with these columns: blockid , bgid, blockwt
#'    
#'    - quaddata data.table used to create localtree, a quad tree index of block points
#'      (and localtree that is created when package is loaded)
#'    
#'    - EJAM::blockgroupstats - A data.table (such as EJScreen demographic and environmental data by blockgroup?)
#'
#' @param sites2blocks data.table of distances in miles between all sites (facilities) and 
#'   nearby Census block internal points, with columns siteid, blockid, distance,
#'   created by getblocksnearby  function. 
#'   See sites2blocks_example dataset in package, as input to this function
#' @param sites2states_or_latlon data.table or just data.frame, with columns siteid (each unique one in sites2blocks) and ST (2-character State abbreviation) or lat and lon 
#' @param countcols character vector of names of variables  to aggregate within a buffer
#'   using a sum of counts, like, for example, the number of people for whom a 
#'   poverty ratio is known, the count of which is the exact denominator needed
#'   to correctly calculate percent low income. 
#' @param popmeancols character vector of names of variables to aggregate within a buffer
#'   using population weighted mean.
#' @param calculatedcols character vector of names of variables to aggregate within a buffer
#'   using formulas that have to be specified.
#' @param testing used while testing this function
#' @param updateProgress progress bar function used for shiny app
#' @param include_ejindexes not yet implemented 
#' @param need_proximityscore whether to calculate proximity scores
#' @param ... more to pass to another function? Not used currently.
#' @param silentinteractive Set to FALSE to prevent long output showing in console in RStudio when in interactive mode
#' @seealso [ejamit]   [getblocksnearby()]  
#' @import data.table
#' @import EJAMblockdata
#' @export
#' 
doaggregate <- function(sites2blocks, sites2states_or_latlon=NA, countcols=NULL, popmeancols=NULL, calculatedcols=NULL, testing=FALSE, include_ejindexes=FALSE, updateProgress = NULL, need_proximityscore=FALSE, silentinteractive=FALSE, ...) {
  
  # timed <- system.time({
  if (testing) {library(data.table); library(EJAMblockdata);     sites2blocks <- EJAM::sites2blocks_example }
  
  ##################################################### #  ##################################################### #
  # HARDCODED blockgroup dataset, FOR NOW ####
  # including the names of the variables.
  # 
  # This function could either take as input params these:
  # 
  # - lists of variable names and a data.frame (like now), 
  #   or 
  # - lists of the actual indicator values, 
  #   or   
  # - it could even be given a table of scores and a table of what to do with each indicator (their names, 
  #   formulas for them, etc.)
  # for each type of indicator (countcols vs popmeancols, etc.),
  ##################################################### #   
  ## Specify Which vars are sum of counts, vs wtd avg, vs via formula ####
  
  if (is.null(countcols)) {
    countcols <- unique(c(
      names_other, 
      names_d_count, 
      names_d_subgroups_count
    ))
  }
  # **** but we probably treat pctpre1960 as pop wtd mean like other Evars?
  # # note that names.d.count was not yet defined in ejscreen pkg, and would lack denominators if only based on pctxyz
  # names.d.count <- union( gsub('pct','', grep(pattern = 'pct', ejscreen::names.d, value=TRUE)),
  #  c('unemployed', 'unemployedbase'))
  # names.d.count <- gsub('^min$', 'mins', names.d.count) # since singular was used in pctmin but plural for count is mins
  # countcols <- unique(c('pop', 'nonmins', names.d.count,
  #                "povknownratio",  "age25up", "hhlds",  'pre1960', 'builtunits',
  #                ejscreen::names.d.subgroups.count)) 
  # "pop", 'nonmins', "mins", 
  # "lowinc",   "povknownratio",   
  # "lths",     "age25up", 
  # "lingiso",  "hhlds", 
  # "under5", "over64",
  # "unemployed",   "unemployedbase", # new in 2022
  # 'pre1960',  'builtunits',
  # "nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone", "nhmulti" # not in EJScreen 2.1 but will use here
  
  if (is.null(calculatedcols)) {
    calculatedcols <- unique(c(
      names_d,                  #   "lowlifex"    "Demog.Index.Supp",  # already in names_d
      names_d_subgroups,  
      'flagged'
    ))
  }
  # These must be calculated after aggregating count variables and using those at siteid level. 
  # e.g. Use ejscreen::ejscreenformulas$formula to calculate these.
  # calculatedcols <- c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged') # use formulas for these
  #  or to avoid depending on ejscreen package, 
  #  dput(c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged')) # but make sure pctunemployed got added
  # Demog.Index", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
  # "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
  # "flagged"
  #      and will have lowlifex and "Demog.Index.Supp", 
  
  if (is.null(popmeancols)) {
    popmeancols <- unique(c(
      names_e,
      names_ej  ### but,
      # ** verify that we want and are using the popwtd mean of PERCENTILES in the case of the EJ Index, unlike Envt or Demog!!
    ))
  }
  # "lowlifex"??   # new, not completely sure it should be via popwtd mean, or calculated via formula actually.
  # popmeancols <- c(ejscreen::names.e, ejscreen::names.ej)
  # or to avoid depending on ejscreen package, 
  # dput(c(ejscreen::names.e, ejscreen::names.ej) )
  # 'NUM_NPL', 'NUM_TSDF',
  # "pm", "o3", "cancer", "resp", "dpm", 
  # "pctpre1960", "traffic.score", 
  # "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", 
  # "ust", 
  # "EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
  # "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
  # "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo", 
  # "EJ.DISPARITY.ust.eo"
  # 
  # ** CHECK THIS:  EJScreen treats pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas. . . ratio of sums of counts pre1960 and denom builtunits  
  # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop. 
  #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
  # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer? 
  # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer, 
  # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).
  ##################################################### #  ##################################################### #
  
  ##################################################### #
  ## ...update progress bar in shiny app ####
  if(is.function(updateProgress)){
    boldtext <- paste0('Calculating indicators at each site and overall')
    updateProgress(message_main = boldtext, value = 0.2)
  }
  ##################################################### #
  
  #____________________________________________________   #  ##################################################### #  ######################################################
  
  
  # ____AGGREGATE by BLOCK across sites #############################################################################################
  
   
  ################################################################ #
  # FIRST, PREPARE TO AGGREGATE BY BLOCK 
  
  ## Get pop weights of nearby blocks ####
  # to know what fraction of each parent block group is considered inside the buffer
  
  # >>>> A BIT SLOW - CAN WE DO THE FOLLOWING LINE BY REFERENCE INSTEAD OF MAKING A COPY OF sites2blocks? just want to join and add  blockwt & bgid to sites2blocks, from blockwts dt
  sites2blocks <-  blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgid), on='blockid']
  # that does not aggregate at all, it retains all rows, including where a blockid appears twice if it is near 2 different sites.
  
  # sort rows
  data.table::setorder(sites2blocks, siteid, bgid, blockid) # new
  
  ################################################################ #
  # Just create some new columns in sites2blocks, but not aggregate rows for each block yet
  # Note:
  # Using                 DT[, newcolumn := min(xyz), by="blockid"] creates new column in existing DT, with repeat of the same info in each row for duplicate blockids, which is ok. Typically not a large % are duplicated so it is not much slower, and dupes are removed later for overall stats.
  # Using  rolledup_DT <- DT[, summarycol = sum(xyz), by="blockid"] creates a new DT with fewer rows, by summarizing over the 1-2 sites near a given block.
  # >>>>> A VERY VERY SLOW STEP TO OPTIMIZE ####
  ## _sitecount for each block ####
  ## _min distance to any site, for each block ####
  if (need_proximityscore) {
    ## _Proximity Score of block ####
    sites2blocks[, `:=`(
      sitecount = .N,
      distance_min = collapse::fmin(distance),   # or is it faster to later save only closest site distance of each blockid, via	 x <-  sites2blocks[, .SD[which.min(distance)], by = blockid]
      proximityscore =  1 / distance  # score here is for only 1 site per block. summed later across all sites near a given block, then get popwtd mean of block prox scores.
    ),
    by="blockid"]
    
    ## _Proximity Score of block to be fixed ####
    # **** TO BE FIXED: / WARNING:  doesnt the formula need the adjustment for small distance?? You need the block area to calculate its effective radius and adjust score if distance is <that? see EJScreen tech doc
    sites2blocks[distance < 0.063, proximityscore := 1/0.56]  # ROUGHLY 100 yards used as a placeholder until fixed
    # sites2blocks[is.infinite(proximityscore), proximityscore := NA]  # if distance==0, proximityscore is Inf, which sort of makes sense. but would NA make more sense than Inf ?
    # warning('proximityscore lacks small distance adjustment factor - not yet implemented')
    ## proxistat::blockpoints_area_pop[ , .(blockid, area)]  has this area info (in square meters!!) but need it in blockwts  
    ## area.sq.mi =   area / (meters_per_mile^2)   #  = convert_units(area, from = "sqm", towhat = "sqmi")
    # blockwts[ , effectiveradius :=  sqrt(area.sq.mi/pi)  ]  ## because  area=pi*radius^2 
    ##  sites2blocks$effectiveradius  needed or do calc during join
    #  sites2blocks[distance < effectiveradius, proximityscore :=  1/(0.9 * effectiveradius) ] #   EJScreen proximity score uses 0.9x that
  } else {
    sites2blocks[, `:=`(       
      sitecount = .N,   # this just notes if there are 1 or 2 sites near a given block. not sure it is essential 
      
      # distance_min = collapse::fmin(distance) #,     ##  SLOWEST STEP -- THIS TAKES ABOUT HALF THE TOTAL TIME OF ALL doaggregate() ****
      distance_min = 0 # temporarily to see if essential and how much does this slow it down?
      # proximityscore =  1 / distance  # score here is for only 1 site per block. summed later across all sites near a given block, then get popwtd mean of block prox scores.
    ),
    by="blockid"]
  }
  ###################################### #
  ## * Unique residents (blocks) only, used for Overall stats #### 
  ###################################### #
  # each block only once, and therefore each person only once even if near 2+ sites.
  # For each overall resident (blockid) near 1 or more sites, 
  # find and save the info for just the closest single siteid (distance_min)
  # ***    Just for now, the simplistic way to drop duplicate blocks (even if other columns are not duplicates), 
  #    which are residents near 2 or more sites, to avoid double-counting them, is unique() 
  # This seems fine for overall stats, since you are just dropping a duplicate block 
  # so you just need to realize one row with that block had info on site 1 and another row w that duplicated block had info on site 2.
  # So you actually need to sum the counts and proximity scores for the consolidated block, 
  # and use the min distance (which block-site had the closer site?)
  # sites2blocks_overall <- unique(sites2blocks, by=blockid)    # would keep all columns but only one nearby site would be kept for each block.
  # Slowest way, but could get all that explicitly maybe like specifying each as max or min 
  
  
  ##################################################################### #
  # AGGREGATE BY BLOCK (over the 1 or 2 sites it may be near)
  #
  # Using                 DT[, newcolumn := min(xyz), by="blockid"] creates the same info in each row for duplicate blockids, which is ok. Typically not a large % are duplicated so it is not much slower, and dupes are removed later for overall stats.
  # Using  rolledup_DT <- DT[, summarycol = sum(xyz), by="blockid"]  creates a new DT with fewer rows by summarizing over the 1-2 sites near a given block.
  #  So  sites2blocks_overall will have 1 row per block, slightly fewer than above in full sites2blocks.
  if (need_proximityscore) {
    sites2blocks_overall <- sites2blocks[ ,  .(bgid = bgid[1],  # otherwise it retains duplicate rows, same block twice if it is near 2!
                                               blockwt = blockwt[1],  
                                               
                                               proximityscore = proximityscore[1], # sum(proximityscore, na.rm = TRUE), # already did sum over all sites near a block.
                                               # distance_avg = stats::weighted.mean(distance, w = blockwt, na.rm = TRUE),
                                               distance_min = distance_min[1],  # already did min
                                               sitecount = .N  # typically some blocks are near 2 or more sites in sites2blocks
                                               # sitecount_avg = .N
    ),
    by="blockid"]
  } else {
    sites2blocks_overall <- sites2blocks[ ,  .(bgid = bgid[1],  # otherwise it retains duplicate rows, same block twice if it is near 2!
                                               blockwt = blockwt[1],  
                                               
                                               #     proximityscore = proximityscore[1], # sum(proximityscore, na.rm = TRUE), # already did sum over all sites near a block.
                                               # distance_avg = stats::weighted.mean(distance, w = blockwt, na.rm = TRUE),
                                               distance_min = distance_min[1],  # already did min
                                               sitecount = .N  # typically some blocks are near 2 or more sites in sites2blocks
                                               # sitecount_avg = .N
    ),
    by="blockid"]
  }
  #  length(sites2blocks_example$blockid)
  # [1] 11567
  #  length(unique(sites2blocks_example$blockid))
  # [1] 11334
  
  #***  ###################################### #
  ## ...update progress bar in shiny app ####
  if(is.function(updateProgress)){
    boldtext <- paste0('Analyzing blockgroups')
    updateProgress(message_main = boldtext, value = 0.4)
  }
  #***  ###################################### #
  #____________________________________________________   #  ##################################################### #  ######################################################
  
  ##################################################### #  ##################################################### #  ##################################################### #  
  
  
  ##################################################### #
  
  # ___AGGREGATE by BG, the Distances & Sitecounts___ ######
  
  
  ##################################################### #
  # How to get Distrib & avg in each Demog group, ####
  # * each Demog's DISTRIBUTION OF DISTANCES or ENVT  
  # for
  # - Envt indicators, and for 
  # - Dist/proximity/sitecount stats.
  # 
  # - also, want %D as function of distance. 
  # 
  # We have each blockgroup near each site, which means some small % of those bgs are duplicated in this table:
  #   sites2bgs_plusblockgroupdata_bysite
  #  and for those stats we would want to take only unique blockgroups from here, 
  #  using the shorter distance I think, so the distribution of distances does not doublecount people.
  # 
  # Mostly we want this OVERALL (not by site) 
  #  But perhaps we might also want to see that distribution of distances by D *for just 1 site??*
  #  And perhaps we might also want to see the %D as a function of continuous distance *at just 1 site??*
  #  So although it would be simpler, clearer to return just all unique blockgroups NOT by site,
  #   just to retain flexibility this function currently reports all instances of blockgroup-site pairing.
  # 
  # see  EJAM/inst/notes_MISC/NOTES_IDEA_OUT_BY_BG_SHOWS_DISTRIB_OVER_PEOPLE.R
  
  
  
  ##################################################### #
  # >>NEED TO CHECK THIS overall calcution here #### 
  # - not sure we want these distance/count items here like this: 
  # Was going to try to do join of weights and the aggregation by blockid all in this one step? but not tested 
  # and may need to do intermed step 1st, where 
  # sites2bg <- blockwts[sites2blocks, .(siteid, bgid, distance, bgwt = sum(blockwt, na.rm=TRUE)), on = 'blockid', by =.(siteid, bgid)] 
  #
  ## why do sum(blockwt) by bgid  here AGAIN, if already did it above?
  # rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??
  ## ########## #
  #
  ## *?? ______WHICH OF THESE VERSIONS WAS BETTER? BY REFERENCE should be faster 
  #
  # sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt),     by="bgid"]  # variable not used !
  # sites2blocks[        , bg_fraction_in_buffer_bysite  := sum(blockwt, na.rm = TRUE), by=c("siteid", "bgid")]
  #
  # VERSUS this below...
  #
  ##################################################### #
  
  # Distances, Proximity scores, & Site counts nearby ####
  #  >>>> A *VERY* SLOW STEP TO OPTIMIZE  - sites2bgs_bysite   <- sites2blocks[   ####
  if (need_proximityscore) {
    warning('proximityscore lacks small distance adjustment factor - not yet implemented')
    
    sites2bgs_bysite   <- sites2blocks[         , .(bgwt = sum(blockwt, na.rm = TRUE),     # 550 msec ??
                                                    
                                                    proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min   = collapse::fmin(distance),
                                                    distance_min_avgperson   = collapse::fmean(distance, w=blockwt), # na.rm=T is default
                                                    sitecount_avg    = collapse::fmean(sitecount, w= blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fnunique(siteid)
    ), by=.(siteid, bgid)]
    
    #  >>>> A SLOW STEP TO OPTIMIZE - starts with sites2bgs_overall  <- sites2blocks_overall[  ####
    # is this redundant since we have sites2bg_bysite and bgid already, and it will get rolled up by siteid only and by bg only, later
    sites2bgs_overall  <- sites2blocks_overall[ , .(bgwt = sum(blockwt, na.rm = TRUE),    # 318 msec
                                                    
                                                    proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min = collapse::fmin(distance_min),
                                                    distance_min_avgperson = collapse::fmean(distance_min, w = blockwt),
                                                    sitecount_avg  =  collapse::fmean(sitecount,  w = blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fmax(sitecount) # this is an underestimate - TO BE FIXED LATER
    ), by=         "bgid" ]
  } else {
    sites2bgs_bysite   <- sites2blocks[         , .(bgwt = sum(blockwt, na.rm = TRUE),     # 550 msec ??
                                                    
                                                    # proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min     = collapse::fmin(distance),
                                                    distance_min_avgperson   = collapse::fmean(distance, w=blockwt), # na.rm=T is default
                                                    sitecount_avg    = collapse::fmean(sitecount, w= blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fnunique(siteid)
    ), by=.(siteid, bgid)]
    
    #  >>>> A SLOW STEP TO OPTIMIZE - starts with sites2bgs_overall  <- sites2blocks_overall[  ####
    # is this redundant since we have sites2bg_bysite and bgid already, and it will get rolled up by siteid only and by bg only, later
    sites2bgs_overall  <- sites2blocks_overall[ , .(bgwt = sum(blockwt, na.rm = TRUE),    # 318 msec
                                                    
                                                    # proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min = collapse::fmin(distance_min),
                                                    distance_min_avgperson = collapse::fmean(distance_min, w = blockwt),
                                                    sitecount_avg  =  collapse::fmean(sitecount,  w = blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fmax(sitecount) # this is an underestimate - TO BE FIXED LATER
    ), by=         "bgid" ]
  }
  # sites2bgs_overall$sitecount_unique <- sites2bgs_bysite[, --- TO BE FINISHED LATER --- , by="bgid"] 
  
  
  ###################################### #
  ## _PROXIMITY/ DISTANCE/ SITECOUNT notes   #### 
  ###################################### #
  #
  #  Best overall summary numbers:
  # Site counts - avg resident has how many nearby (avg by D group),
  #  and max site count nearby (which wont vary by Demog)
  # Distances - avg resident's distance from site (or distribution by D group)
  #  and distance of closest site (which wont vary by demog).
  # 
  ## **NEED TO check/ fix these variable names to be consistent between bysite and overall:
  # 
  #         sitecount info to save:  
  # 
  # sitecount_avg  = how many sites are near the avg person in this bg/site/overall set?
  # sitecount_max  = up to how many sites at most are near anyone in this bg/site/overall set?
  # sitecount_unique  = how many unique sites are near anyone in this bg/site/overall set?
  #   for each bg, keep all (but remember it is how many are near the avg person in this bg)
  #   for each site, keep all (don't need but keep so cols are consistent)
  #   For overall,  keep all (but remember it is how many are near the avg person)
  
  #       distance stats to save:  
  # 
  # distance_min  = how close is the closest site to anyone in this bg/site/overall set?f = closest site's distance for any block included in this bg (among all blocks inside buffer)/ at site/ overall.
  # ?distance_avg??  = not really a useful metric. we care about 1 closest, not avg distance to all nearby.
  ##   Distance could be then summarized a couple different ways...  
  # distance_min_avgperson ## avg of mins: avg person has a site within x distance = closest site for avg person ;
  #    avg of mins by Demog group: save by bg (or maybe by bg by site) to summarize avg by demographic group, overall and possibly by site.
  # distance_min is for the nearest person # min of mins: at least some people have a site within just x distance = what is the min distance of any site to any person
  ##?? avg of avgs??? what is the avg persons distance from avg site ?? that is not useful , right?
  
  #        proximityscore stats to save:
  # 
  # what is the average persons proximity score for proximity to this set of sites,
  #   for the ones in the radius only?? should we just separately calculate regular proximity scores ignoring distance?
  #    and then report on proximity scores for prox to these sites just like other proximity scores we report here? eg., avg person within 3 miles of any of these sites has proximity score of x.
  # But really site counts and distance of closest are the best overall summary numbers.
  ################################### # 
  
  
  #____________________________________________________ #  ######################################################
  
  
  
  ##################################################### #
  #  *** JOIN  EJScreen indicators ####
  # joins midsized intermed table of sites & BGs to EJScreen/ blockgroupstats . . . sites2bgs_overall ??  
  ##################################################### #
  #
  # DO JOIN  OF **blockgroupstats**   200 columns, on bgid , 
  
  # and not sure I can calculate results at same time, since this kind of join is getting a subset of blockgroupstats but grouping by sites2bgs_bysite$siteid  and 
  # maybe cannot use blockgroupstats[sites2bgs_bysite,    by=.(siteid)    since siteid is in sites2bgs_bysite not in blockgroupstats table. 
  # So, first join blockgroupstats necessary variables to the shorter sites2bgs_bysite:   
  
  
  #   Remember that. . .
  # countcols     # like population count, add up within a buffer
  # popmeancols    # we want average persons raw score,  for Environmental (but maybe avg PERCENTILE for EJ indexes ??)
  # calculatedcols  # use formulas for these, like  sum of counts of lowincome divided by sum of counts of those with known poverty ratio (universe)
  countcols_inbgstats      <- intersect(countcols,      names(blockgroupstats))
  popmeancols_inbgstats    <- intersect(popmeancols,    names(blockgroupstats))
  calculatedcols_inbgstats <- intersect(calculatedcols, names(blockgroupstats))
  
  sites2bgs_plusblockgroupdata_bysite  <- merge(sites2bgs_bysite,  # has other cols like   "distance_avg" , "proximityscore"  etc. 
                                                blockgroupstats[ , c('bgid', 'ST', ..countcols_inbgstats, ..popmeancols_inbgstats, ..calculatedcols_inbgstats)], 
                                                all.x = TRUE, all.y=FALSE, by='bgid')
  
  
  # just be aware that this is not saving just unique blockgroups, but saves each bgid-siteid pairing???
  
  sites2bgs_plusblockgroupdata_overall <- merge(sites2bgs_overall, 
                                                blockgroupstats[ , c('bgid',       ..countcols_inbgstats, ..popmeancols_inbgstats, ..calculatedcols_inbgstats)], 
                                                all.x = TRUE, all.y=FALSE, by='bgid')
  # rm(sites2bgs_overall, sites2bgs_bysite); rm(blockgroupstats)
  
  
  ##################################################### #  ##################################################### #
  
  
  ######################### #
  ## ...update progress bar in shiny app ####
  if(is.function(updateProgress)){
    boldtext <- paste0('Joining blockgroups to EJScreen indicators')
    updateProgress(message_main = boldtext, 
                   value = 0.6)
  }
  ######################### #
  #____________________________________________________ #  ######################################################
  
  
  
  # ___AGGREGATE by SITE, the Indicators ___ ####
  
  
  ##################################################### #
  # TOTAL COUNT for each count indicator at EACH SITE & OVERALL ####  
  # AND ALSO SUBGROUPS IF WANT TO 
  #  USE FORMULAS TO GET EXACT %D AT EACH SITE AS SUM OF NUMERATORS / SUM OF DENOMINATORS)
  ##################################################### #
  
  ##  Counts Overall (all sites/ whole sector)  ###
  
  results_overall <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm=TRUE), 1)
  } ), .SDcols = countcols ]
  
  # to be sum of unique, BY SITE, TO RETURN: blockcount_by_site, bgcount_by_site, 
  
  # others for OVERALL:  count_of_blocks_near_multiple_sites, blockcount_overall, bgcount_overall
  
  
  ##  Counts by site/facility  ###
  
  results_bysite <- sites2bgs_plusblockgroupdata_bysite[ ,    lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm=TRUE), 1)
  } ), .SDcols = countcols, by = .(siteid) ]
  
  # results_bysite[1:100,1:8]
  # cbind(sum = prettyNum(results_overall,big.mark = ','))
  # versus if you did not remove duplicate places/people:
  # sites2bgs_plusblockgroupdata_bysite[ ,  .(sums = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), .SDcols = countcols][1,]
  # 1: 9,978,123
  # but sum(outapi_3mile_100sites$pop[outapi_3mile_100sites$statename != 'PUERTO RICO' ])
  # [1] 10,022,946
  
  
  ##################################################### #
  # POP WTD MEAN for some indicators ####   
  # ( ENVT, EJ index. . .. AND MAYBE ALL THE DEMOG TOO???)
  ##################################################### #
  #  >>>> A bit SLOW - TO OPTIMIZE **WEIGHTED.MEAN   ####
  ## mean by SITE ###
  results_bysite_popmeans <- sites2bgs_plusblockgroupdata_bysite[   ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop)   # stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)    # 100 msec
  }), .SDcols = popmeancols, by = .(siteid) ]
  # redo   in data.table:: style, for speed? this is just by site so only 1 row per site is not that many usually, but is a lot of columns (200?) *********************************
  results_bysite <- merge(results_bysite, results_bysite_popmeans)
  
  ## mean OVERALL ###
  ## later, for results_overall, will calc state pctiles once we have them for each site 
  
  results_overall_popmeans <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop) # stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)
  }), .SDcols = popmeancols  ]
  results_overall <- cbind(results_overall, results_overall_popmeans) # many columns (the popwtd mean cols)
  
  ##################################################### #
  # *MIN or MAX distance or sitecount ####
  ##################################################### #
  
  calculatedcols <- c(calculatedcols,  
                      "distance_min" ,  
                      "sitecount_max")
  
  # sitecount_avg  = how many sites are near the avg person in this bg/site/overall set?
  # sitecount_max  = up to how many sites at most are near anyone in this bg/site/overall set?
  # sitecount_unique  = how many unique sites are near anyone in this bg/site/overall set?
  # 
  # distance_min  = how close is the closest site to anyone in this bg/site/overall set?f = closest site's distance
  # distance_min_avgperson ## avg of mins: avg person has a site within x distance = closest site for avg person ;
  
  #XXX sites2bgs_plusblockgroupdata_bysite actually has each bgid despite the variable name
  # print(names(sites2bgs_plusblockgroupdata_bysite))
  # "proximityscore"                 
  # [5] "distance_min"  "distance_min_avgperson"         
  # [7] "sitecount_avg"  "sitecount_max"  "sitecount_unique"
  
  results_bysite_minmax <- sites2bgs_plusblockgroupdata_bysite[ , .(  
    distance_min = collapse::fmin(distance_min),
    distance_min_avgperson = collapse::fmean(distance_min_avgperson, w= pop),   # distance_min_avgperson = weighted.mean(distance_min_avgperson, w= pop, na.rm=TRUE),
    sitecount_max    = collapse::fmax(sitecount_max ) ,
    sitecount_unique = collapse::fnunique(siteid), ######## CHECK THIS
    sitecount_avg     = collapse::fmean(sitecount_avg, w= pop)  # sitecount_avg     =weighted.mean(sitecount_avg, w= pop, na.rm=TRUE)
  ), by = .(siteid) ]
  results_bysite <- merge(results_bysite, results_bysite_minmax, on = "siteid")
  
  results_overall_minmax <- sites2bgs_plusblockgroupdata_bysite[ , .(  
    distance_min = collapse::fmin(distance_min),
    distance_min_avgperson = collapse::fmean(distance_min_avgperson, w= pop),   # distance_min_avgperson = weighted.mean(distance_min_avgperson, w= pop, na.rm=TRUE),
    sitecount_max    = collapse::fmax(sitecount_max ) ,
    sitecount_unique = collapse::fnunique(siteid), ######## CHECK THIS
    sitecount_avg     = collapse::fmean(sitecount_avg, w= pop)   # sitecount_avg     =weighted.mean(sitecount_avg, w= pop, na.rm=TRUE)
  ) ]  # not by siteid
  results_overall <- cbind(results_overall, results_overall_minmax) # cbind not merge, since only 1 row not by siteid
  
  # note that max E or D score of any bg near a given site must be calculated later, 
  # outside of doaggregate(), using results_bybg_people table 
  # not here, since it needs the calculated E or D scores done below.
  
  # rm(results_overall_popmeans, sites2bgs_plusblockgroupdata_overall)
  # rm(results_bysite_popmeans,  sites2bgs_plusblockgroupdata_bysite)
  
  
  ##################################################### #  ##################################################### #
  
  # * COUNT BLOCKS OR BGS  ####
  # 
  # Is that at all useful really??
  # "blockcount_near_site"            "bgcount_near_site"
  blockcount_by_site <- sites2blocks[, .(blockcount_near_site = .N),         by=siteid] # new----------------------------------------------- -
  bgcount_by_site    <- sites2blocks[, .(bgcount_near_site = collapse::fnunique(bgid)), by=siteid] # new------------------------------------ -
  
  results_bysite <- merge(results_bysite, blockcount_by_site) # on="siteid" new ---------------------------------------------- -
  results_bysite <- merge(results_bysite, bgcount_by_site)    # on="siteid" new ---------------------------------------------- -
  
  ##################################################### # 
  
  # ____OVERALL ###
  
  ##################################################### #  ##################################################### #
  # * COUNT SITES NEARBY ####
  # * overall, HOW OFTEN ARE BLOCKS,BGS NEAR >1 SITE?  ###
  # Note this is NOT like the other metrics - this is just an overall stat to report once over the whole set of sites and bgs.
  count_of_blocks_near_multiple_sites <- (NROW(sites2blocks) - NROW(sites2blocks_overall)) # NEW fraction is over /NROW(sites2blocks_overall)
  blockcount_overall <-  sites2blocks[, collapse::fnunique( blockid)]
  bgcount_overall    <-  sites2blocks[, collapse::fnunique( bgid)]
  # how many blockgroups here were found near 1, 2, or 3 sites? 
  # e.g., 6k bg were near only 1/100 sites tested, 619 near 2, 76 bg had 3 of the 100 sites nearby.
  # table(table(sites2bgs_bysite$bgid))
  
  results_overall <- cbind(results_overall, blockcount_near_site = blockcount_overall) # 1 new col and then changed name to match it in results_bysite ---------------------------------------------- -
  results_overall <- cbind(results_overall, bgcount_near_site = bgcount_overall) # 1 new col and then changed name to match it in results_bysite ---------------------------------------------- -
  
  #***  ###################################### ##  ###################################### #
  
  ## ...update progress bar in shiny app ####
  ##################################################### #
  if(is.function(updateProgress)){
    boldtext <- paste0('Computing results')
    updateProgress(message_main = boldtext, value = 0.8)
  }
  ##################################################### #
  
  #____________________________________________________ #  ######################################################
  ##################################################### #
  # CALCULATE PERCENT DEMOGRAPHICS FROM SUMS OF COUNTS, via FORMULAS  [hardcoded here, for now]
  #
  # but should do that using a list of formulas like in ejscreen::ejscreenformulas 
  # and a function like analyze.stuff  calc.fields() 
  ##################################################### #
  
  #      NOTE ON PERCENTAGES AS 0 TO 1.00 RATHER THAN O TO 100.
  # 
  #  API returns demographic percent indicators like percent low income as 0-100, 
  #   (which doesn't make much sense but popups code needs to know)
  #  and EJAM doaggregate() was returning them as 0-100, but 1/23/23 changed those to 0 to 1.00, 
  #    so that lookups of demographics in percentile tables will work right.
  #  but 
  #  the lookup tables like EJAM::usastats store those variables as 0 to 1.00 . . .. see usastats[74:80,1:9]
  #  and the dataset of all US blockgroups (from EJScreen FTP site or in EJAM::blockgroupstats) stores that as 0 to 1.00
  
  ##################################################### #
  # CALC via FORMULAS 
  #  using the Rolled up Counts #### 
   # this was meant to handle multiple columns (formula for each new one) for many rows (and here in buffer results, one site is a row, not one blockgroup) 
  
  # "nonmins <- nhwa"
  # "mins <- pop - nhwa" 
  results_overall[ , `:=`(
    pctover64       = 1 * ifelse(pop==0, 0,            over64        / pop),
    pctunder5       = 1 * ifelse(pop==0, 0,            under5        / pop),
    pcthisp         = 1 * ifelse(pop==0, 0, as.numeric(hisp )        / pop),
    pctnhwa         = 1 * ifelse(pop==0, 0, as.numeric(nhwa )        / pop),
    pctnhba         = 1 * ifelse(pop==0, 0, as.numeric(nhba )        / pop),
    pctnhaiana      = 1 * ifelse(pop==0, 0, as.numeric(nhaiana)      / pop),
    pctnhaa         = 1 * ifelse(pop==0, 0, as.numeric(nhaa )        / pop), 
    pctnhnhpia      = 1 * ifelse(pop==0, 0, as.numeric(nhnhpia )     / pop),
    pctnhotheralone = 1 * ifelse(pop==0, 0, as.numeric(nhotheralone) / pop), 
    pctnhmulti      = 1 * ifelse(pop==0, 0, as.numeric(nhmulti )     / pop),
    pctmin          = 1 * ifelse(pop==0, 0, as.numeric(mins)         / pop), 
    pctlowinc       = 1 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio),
    pctlths         = 1 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up), 
    pctlingiso      = 1 * ifelse(hhlds          == 0, 0, lingiso                / hhlds), 
    pctpre1960      = 1 * ifelse(builtunits     == 0, 0, pre1960                / builtunits),
    pctunemployed   = 1 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)  # ,
  ) ]
  
  if (!("lowlifex" %in% names(results_overall))) {results_overall[ , lowlifex := 0] } # if not available yet, treat like zero for now
  
  results_overall[ , `:=`(
    Demog.Index = (pctlowinc + pctmin) / 2,
    # *** add supplemental indicator too, when possible. need lowlifeexpectancy etc. and Demog.Index.Supp needs to be in names_d  and lookup tables for usastats and statestats ####
    Demog.Index.Supp  = (pctlowinc + pctunemployed + pctlths + pctlingiso + lowlifex ) / ifelse(lowlifex == 0, 4, 5)
    # *** add supplemental indicator too, when possible. need lowlifeexpectancy etc. and Demog.Index.Supp needs to be in names_d  and lookup tables for usastats and statestats ####
    # # supplemental demographic index = (% low-income + % unemployed + % less than high school education + % limited English speaking + low life expectancy) / 5 
    # For block groups where low life expectancy data is missing, the formula will average the other four factors! 
    # Demog.Index.Supp = (pctlowinc + pctunemployed + pctlths + pctlingiso + lowlifex ) / ifelse(lowlifex == 0, 4, 5) # where is lowlifex available?
    # NOTE THAT EJScreen uses the term "Supplemental Indexes" to refer to 
    #  EJ Indexes that are based on the Supplemental Demographic Index
    # See details at  https://www.epa.gov/ejscreen/ejscreen-map-descriptions#supp 
  )]
  
  results_bysite[ , `:=`(
    pctover64       = 1 * ifelse(pop==0, 0,            over64        / pop),
    pctunder5       = 1 * ifelse(pop==0, 0,            under5        / pop),
    pcthisp         = 1 * ifelse(pop==0, 0, as.numeric(hisp )        / pop),
    pctnhwa         = 1 * ifelse(pop==0, 0, as.numeric(nhwa )        / pop),
    pctnhba         = 1 * ifelse(pop==0, 0, as.numeric(nhba )        / pop),
    pctnhaiana      = 1 * ifelse(pop==0, 0, as.numeric(nhaiana)      / pop),
    pctnhaa         = 1 * ifelse(pop==0, 0, as.numeric(nhaa )        / pop), 
    pctnhnhpia      = 1 * ifelse(pop==0, 0, as.numeric(nhnhpia )     / pop),
    pctnhotheralone = 1 * ifelse(pop==0, 0, as.numeric(nhotheralone) / pop), 
    pctnhmulti      = 1 * ifelse(pop==0, 0, as.numeric(nhmulti )     / pop),
    pctmin          = 1 * ifelse(pop==0, 0, as.numeric(mins)         / pop), 
    pctlowinc       = 1 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio),
    pctlths         = 1 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up), 
    pctlingiso      = 1 * ifelse(hhlds          == 0, 0, lingiso                / hhlds), 
    pctpre1960      = 1 * ifelse(builtunits     == 0, 0, pre1960                / builtunits),
    pctunemployed   = 1 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)  # ,
  )]
  
  if (!("lowlifex" %in% names(results_bysite)))  {results_bysite[ ,  lowlifex := 0] } # if not available yet, treat like zero for now
  
  results_bysite[ , `:=`(
    Demog.Index = (pctlowinc + pctmin) / 2,
    # *** add this supplemental indicator too, when possible. need lowlifeexpectancy etc. and Demog.Index.Supp needs to be in names_d  and lookup tables for usastats and statestats ####
    Demog.Index.Supp = (pctlowinc + pctunemployed + pctlths + pctlingiso + lowlifex ) / ifelse(lowlifex == 0, 4, 5)
  )]
  
  
  
  ##################################################### #  
  # Demog.Index.US = sum(mins) / sum(pop)  +  sum(lowinc) / sum(povknownratio) ) / 2, 
  # Demog.Index = (pctlowinc + pctmin) / 2, 
  #  or is it treated as envt var and just pop mean?  ********************
  # Demog.Index <- stats::weighted.mean(Demog.Index, w = pop)
  ### Demog.Index <- VSI.eo
  
  # # To be replaced with data available to this package
  # myformulas <- ejscreen::ejscreenformulas
  # # one row per buffer/site?  
  # results_bysite_formulas_done <- ejscreen::ejscreen.acs.calc(bg = results_bysite, keep.old = 'all', keep.new = 'all', formulas = myformulas)
  #
  # just one row?
  #
  # results_overall_formulas_done  <- ejscreen::ejscreen.acs.calc(bg = results_overall, keep.old = 'all', keep.new = 'all', formulas = myformulas)
  
  # cbind(prettyNum( (results_bysite[1,]),big.mark = ','))
  # t(usastats[usastats$PCTILE == 'mean', ])
  # save.image('~/R/mypackages/EJAM/inst/doagg2 so far just AFTER calculated vars made.rda')
  
  # missing:  id, lat, lon, Demog.Index which was VSI.eo, state.avg., state.pctile., us.avg., pctile., 
  #  ST, Statename, REGION, 
  #  NUM_NPL, NUM_TSDF, 
  #  StatLayerCount, StatLayerZeroPopCount, 
  #  weightLayerCount which might be the count of blocks nearby???
  # "timeSeconds", "radius.miles", "unit", "statlevel", "inputAreaMiles"
  
  # t(results_bysite[1:5,])
  # sum(results_bysite$pop)
  # results_overall$pop
  
  
  
  
  
  
  # THIS IS ACTUALLY BEST KEPT OUTSIDE OF doaggregate()...
  # can be calculated from outputs of doaggregate() later, via the table of blockgroup-specific results.
  
  #################### #
  # We mainly report AVERAGE D and E indicator score near each site, but 
  #   we could save MAX of each D and E indicator score near each Site.
  #  Comparing sites that way 
  #  (e.g., which site has poorest block group? i.e., what is worst site as measured by highest nearby blockgroup-level %poor?)
  #    need to calculate that MAX from raw bg data when you aggregate by siteid, doing MAX not just AVG or SUM.
  #################### #
  # WE COULD  insert code here to calc max raw score of all blockgroups at a given site. 
  # something like this?
  # maxneededcols <- c(names_e, names_d, names_d_subgroups) # not also EJ?? # NEED TO CONFIRM THIS IS RIGHT
  # results_bysite_minmax_ED <- sites2bgs_plusblockgroupdata_bysite[   ,  lapply(
  #   .SD,                                                  
  #   max(x, na.rm = TRUE)
  # ), .SDcols = maxneededcols, by = .(siteid) ]
  
  
  
  
  
  
  
  
  
  
  
  #____________________________________________________  #  ##################################################### #  ######################################################
  
  # PERCENTILES ####
  
  
  ##################################################### #  ##################################################### #
  # 
  ## WHAT STATE IS EACH SITE IN? ####
  # 
  # Assign state abbrev to each site!! (allows for state percentiles and averages to be looked up) (and statename, FIPS.ST, REGION?) 
  # Get ST (state) each site is in, to report scores as state percentiles 
  # 
  # This will be a data.frame with siteid, ST, (and maybe other columns).
  # Do prelim cleanup of that lookup table that is later used in 
  # reporting each site's scores as state percentiles, 
  #  and then popwtd avg of sites state pctiles will be used as overall state pctiles.
  #
  # sites2states lookup table can be used after rollup blocks to BGs, so you can 
  #  convert raw BG scores to state.pctiles, via statestats lookup 
  #  [OR, if fast/efficient, maybe every BG in blockgroupstats should already have its own state pctile scores columns, in which case wont need to look up those bg pctiles via raw scores!]
  #
  # * and an extra feature could be-- using BG-scale dataset (per site or at least overall) calc avg state pctile in each Demog group (per site or at least overall).
  #   (but probably dont want to just do bg-wtd-popwtd avg of those bg-specific pctiles to get overall pctiles per site??); 
  # and then do rollup of raw scores from bgs to just sites, 
  #  and using state of each site, [ignore blockgroups near site that are in a different state than the site's central point, right?]
  # *** aggregated raw scores at each site can be looked up and reported as percentile in that state; 
  # *** popwtd avg of sites state pctiles (not raw scores) will be used as the overall state pctiles.
  #  (Because each site has a different site, you cannot just convert overall raw scores to state pctiles).
  
  if (missing(sites2states_or_latlon) | !("ST" %in% names(sites2states_or_latlon))) { # must or should figure out state based on blockid -> blockfips -> ST
    sites2states <- ST_by_site_from_sites2blocks(sites2blocks)
    # returns a data.table with these columns:  siteid, ST
    if (!missing(sites2states_or_latlon)) {
      # add in the lat,lon columns - this is always available if ejamit() called this since it passes the pts as sites2states_or_latlon
      if ("siteid" %in% names(sites2states_or_latlon) & "siteid" %in% names(sites2states)) {
        sites2states <- merge(sites2states, sites2states_or_latlon, by = 'siteid') #  error if  siteid is not there
      } else {
        sites2states <- cbind(sites2states, sites2states_or_latlon) #   ***xxx  HAVE NOT CHECKED IF THIS WORKS OR IS CORRECT !
      }
    } else {
      # maybe get latlon of closest block?? no, just omit lat,lon in this case
    }
  } else { # hope it has ST, which is best, or latlon which is slowest, but in between was via blockid, done above!
    sites2states <- states_infer(sites2states_or_latlon) 
    # returns a data.FRAME with these columns (plus others in input):  lat,lon,siteid, ST, statename, FIPS.ST, REGION,  n 
  }
  # sites2states  is df or dt with just 1 row/site, and columns= siteid,ST ; and MIGHT have lat,lon and other info.
  
  results_bysite[sites2states,  ST := ST,  on = "siteid"] # check this, including when ST is NA 
  results_overall$ST <- NA
  
  ##################################################### #
  ## PERCENTILES - show raw scores (from results_bysite AND  results_overall) in percentile terms #### 
  #  VIA  lookup tables of US/State  percentiles, called EJAM::usastats   and statestats
  #  note: usastats is  like ejscreen::lookupUSA , and EJAM::pctile_from_raw_lookup is like ejanalysis::lookup.pctile()
  ##################################################### #
  # these lines about names of variables should be pulled out of here and defined as params or another way   xxx
  # specify which variables get converted to percentile form
  varsneedpctiles <- c(names_e,  names_d, names_d_subgroups   )
  if (include_ejindexes) {
    varsneedpctiles <- c(varsneedpctiles, names_ej)
  }
  varnames.us.pctile    <- paste0(      'pctile.', varsneedpctiles)
  varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)
  # set up empty tables to store the percentiles we find
  us.pctile.cols_bysite     <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite)     <- varnames.us.pctile
  state.pctile.cols_bysite  <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite)  <- varnames.state.pctile
  us.pctile.cols_overall    <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall)    <- varnames.us.pctile
  # state.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_overall) <- varnames.state.pctile
  
  # SURELY THERE IS A FASTER / VECTORIZED WAY TO DO THIS (this actually is noticeably slow, at least the line that starts with state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- pctile_from_raw_lookup( ):
  #  >>>> VERY SLOW STEP; also the function  pctile_from_raw_lookup()  may need to be optimized or avoid passing dt as param to it. ####
  for (i in seq_along(varsneedpctiles)) {
    myvar <- varsneedpctiles[i]
    if (myvar %in% names(usastats)) {  # use this function to look in the lookup table to find the percentile that corresponds to each raw score value:
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
        unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
        unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
      # (note it is a bit hard to explain using an average of state percentiles   in the "overall" summary)
    } else { # cannot find that variable in the percentiles lookup table
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]] <- NA
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]] <- NA
    }
    if (myvar %in% names(statestats)) {
      state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- pctile_from_raw_lookup(    ### VERY SLOW STEP 289 msec
        unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats, zone =  results_bysite$ST)
      ## These must be done later, as avg of sites:
      # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- pctile_from_raw_lookup(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats, zone =  results_overall$ST)
    } else {
      state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- NA
      # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- NA
    }
    
  }
  
  # Q: does this convert it from data.table to data.frame? I think not. xxx
  
  results_overall <- cbind(siteid=NA, results_overall, us.pctile.cols_overall ) # , state.pctile.cols_overall)
  results_bysite  <- cbind(           results_bysite,  us.pctile.cols_bysite,  state.pctile.cols_bysite )
  
  ############################################################################## #   
  ## EJ INDEXES if needed ####
  #  EJ Indexes need to be calculated AFTER the envt percentiles are done, 
  # but then the raw EJ indexes themselves need to be made into percentiles.  
  ###################### # #  
  if (include_ejindexes) {
    # The 2023 new EJ index formula: 
    # 1) IMPORTANT QUESTION FOR OEJ: DOES THE STATE PERCENTILES VERSION OF EJ INDEX USE STATE PERCENTILE IN ITS FORMULA?? 
    # 2)    also we need to name these columns carefully - EJ index is always shown as a percentile and variable name that was used for that percentile might have been without the pctile. prefix?
    # 3) For the state percentile,   WE ASSUME THE ENTIRE BUFFER IS MAINLY OR ALL IN ONE STATE (based on point in center of circle) AND 
    #   LOOK UP  RAW EJ INDEX IN THAT STATE'S LOOKUP TO ASSIGN THE PERCENTILE. 
    warning("if using EJ indexes here, confirm the formula for state percentile version")
    
    ### basic 2023 EJ Indexes = 2-factor demog index times the envt percentile ####
    results_bysite <- results_bysite[ , lapply(.SD, FUN = function(x) {
      Demog.Index * x
    }), .SDcols = names_e_pctile]
    ## supplementary EJ Indexes = 5-factor suppl demog index times the envt percentile  ####
    results_bysite <- results_bysite[ ,  lapply(
      .SD, FUN = function(x) {
        Demog.Index.Supp * x
      }), .SDcols = names_e_pctile]
    
    # #other package did them like this but formula not updated there yet
    # names_ej_state_pctile, names_ej_state_pctile
    # EJ.new <-   # or  EJ.supp 
    #   data.frame(
    #     ejanalysis::ej.indexes(
    #       env.df  = results_bysite[, names_e],
    #       demog   = results_bysite[, 'Demog.Index'], # or Demog.Index.Supp
    #       weights = results_bysite[, 'pop'],
    #       type = "new"  # or supplementary
    #     ),
    #     stringsAsFactors = FALSE
    #   ) # note this calculates overall Demog.Index.US   on the fly
    
    ## NEED TO ASSIGN PERCENTILES TO EJ INDEXES ####
    
    warning("percentiles need to be assigned to EJ raw index scores")
    
    
    
    
    # fix column names of ej percentiles?  
    
    
    ## Question on EJ Index State Percentiles! #### 
    #*# xxx 
    #*# WE need to calc  state pctile of the popwtd mean raw EJ index? 
    #*#   Does the formula itself use state percentile of envt in that case??
    # if so, we'd need to separate the variables into raw ej score versus state-specific raw ej score, like we already do with the percentiles.
    # That would require some more code. 
    
    
    
  }
  ############################################################################## #   
  #
  # ***Calc STATE PERCENTILES for results_overall ####
  
  #  (as popwtd mean of sites state pctiles - which seems bizarre but not sure how else you would do it)
  
  # now that site-specific percentiles have been calculated and looked up,
  # you can calculate that overall state percentiles from those as a pop wtd mean (not by looking them up from raw scores as would be done for US pctiles, since each site may be in its own state)
  # xxx
  state.pctile.cols_overall <-  results_bysite[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = pop)  # stats::weighted.mean(x, w = pop, na.rm = TRUE)
  }), .SDcols = varnames.state.pctile ]
  
  # redo these in data.table:: style, for speed? but it is just a 1-row result  *********************************
  results_overall <- cbind(results_overall, state.pctile.cols_overall)
  
  # sites2bgs_plusblockgroupdata_bysite$ST <-  
  
  ############################################################################## #   
  # US and STATE AVERAGES ####
  # INCLUDE STATE AVERAGE FOR EACH SITE AND INDICATOR ###
  #
  # THEN CALC OVERALL POPWTD MEAN OF THOSE PER INDICATOR 
  
  names_these              <- c(names_d,     names_d_subgroups,     names_e) 
  names_avg_these          <- c(names_d_avg, names_d_subgroups_avg, names_e_avg)                         # <- paste0("avg.",       names_these) # 
  names_state_avg_these    <- c(names_d_state_avg,    names_d_subgroups_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
  names_ratio_to_avg_these <- c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_avg_these )  
  names_ratio_to_state_avg_these <- c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg, names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_state_avg_these)  
  
  # pull averages from the statestats table (note using data.frame syntax here not data.table)
  
  # must be a cleaner way to do this part but did not have time to think about one
  stinfo <- setDT(statestats[ statestats$PCTILE == "mean" , c("REGION", names_these)])
  setnames(stinfo, "REGION", "ST")
  state.avg.cols_bysite <- stinfo[results_bysite[,.(ST)],  on = "ST"]
  
  # rename the colnames to be state.avg. instead of just basic names
  setnames(state.avg.cols_bysite,  names_these, names_state_avg_these )
  state.avg.cols_bysite[, ST := NULL]
  results_bysite <- cbind(results_bysite, state.avg.cols_bysite)  # cbind?? collapse:: has a faster way   ************
  
  # calc overall popwtd mean of each state avg !!
  
  state.avg.cols_overall <-  results_bysite[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = pop)   # stats::weighted.mean(x, w = pop, na.rm = TRUE)
  }), .SDcols = names_these]
  setnames(state.avg.cols_overall,  names_these,  names_state_avg_these)
  
  results_overall <- cbind(results_overall, state.avg.cols_overall)
  
  ############################################################################## #   
  #
  # INCLUDE US AVERAGE FOR EACH SITE AND INDICATOR AND OVERALL ###
  #  -  THIS IS WASTING TIME AND SPACE, BUT IS CONVENIENT I GUESS
  
  #  overall     
  avg.cols_overall <-   usastats[ usastats$PCTILE == "mean",  names_these] # not a data.table, or it would need to say  usastats[ PCTILE == "mean",  ..names_these]
  # rename the colnames to avg instead of just basic names?
  setnames(avg.cols_overall,  names_these,  names_avg_these)
  
  results_overall <- cbind(results_overall, avg.cols_overall)
  
  results_bysite <- cbind(results_bysite,  avg.cols_overall) # collapse:: has a faster way   ? want that single row data.table repeated once per site here
  
  ############################################################################## #   
  #
  # RATIO to AVERAGE - (do not duplicate IN server code!)  ####
  #
  #    app_server code is/was duplicating efforts and trying to calculate these itself !!
  
  ## RATIOS TO US AVG ###
  ratios_to_avg_bysite  <- 
    results_bysite[  , ..names_these] / 
      results_bysite[, ..names_avg_these]
    
  ratios_to_avg_overall <- 
    results_overall[  , ..names_these] /          # AVERAGE PERSON score OVERALL, RIGHT?
      results_overall[, ..names_avg_these]
    
  ## RATIOS TO STATE AVG ###
  ratios_to_state_avg_bysite  <- 
    results_bysite[  , ..names_these] / 
      results_bysite[, ..names_state_avg_these]
  
  ratios_to_state_avg_overall <- 
    results_overall[  , ..names_these] / 
      results_overall[, ..names_state_avg_these]
  
  # add those all to results tables
  colnames(ratios_to_avg_bysite)  <- names_ratio_to_avg_these
  colnames(ratios_to_avg_overall) <- names_ratio_to_avg_these
  colnames(ratios_to_state_avg_bysite)  <- names_ratio_to_state_avg_these
  colnames(ratios_to_state_avg_overall) <- names_ratio_to_state_avg_these
  
  results_bysite  <- cbind(results_bysite,  ratios_to_avg_bysite,  ratios_to_state_avg_bysite)   # collapse:: has a faster way than cbind here!
  results_overall <- cbind(results_overall, ratios_to_avg_overall, ratios_to_state_avg_overall)
  
  # ________________________________________________________________________############ #  ######################## 
  
  
  #***  ###################################### #
  # LATITUDE & LONGITUDE added to results  ####
  
  if ("lat" %in% names(sites2states)) {
    results_bysite[sites2states, lat := lat, on = "siteid"]
  } else {
    results_bysite[ , lat := NA]
  }
  if ("lon" %in% names(sites2states)) {
    results_bysite[sites2states, lon := lon, on = "siteid"]
  } else {
    results_bysite[ , lon := NA]
  }
  
  # add those columns to overall, so the format is same for overall and bysite tables
  results_overall[ , lat := NA]
  results_overall[ , lon := NA]
  #***  ###################################### #
  
  # Put results columns in a more useful/ convenient order ####
  {
    useful_column_order <- c(
      'id', 'siteid',
      'pop',           # '[or names_wts]',
      'sitename',
      'lon', 'lat',
      'ST', 'statename', 'REGION', 
      
      ## RATIOS to AVG in US or State ---------------
      # for D,Dsub,E
      
      names_ratio_to_avg_these,
      names_ratio_to_avg_these,
      names_ratio_to_state_avg_these,
      names_ratio_to_state_avg_these,
      
      ## DEMOGRAPHICS -----------------
      
      ### D RAW % ##
      # "Demog.Index.Supp", # supplemental demographic indicator now is in names_d
      names_d,   # Demog.Index, percent low income, etc.
      names_d_subgroups,   # percent hispanic etc.
      # low life expectancy score 
      # "lowlifex",   # now is in names_d 
      ###  D US RATIOS?  # (above)
      ### D US PCTILE ###
      # "pctile.Demog.Index", #now in names_d_pctile
      names_d_pctile,  names_d_subgroups_pctile, 
      ### D US AVERAGES ###
      names_d_avg, names_d_subgroups_avg, names_d_subgroups_state_avg,
      ### D STATE RATIOS?     # (above)
      ### D STATE PCTILE ##
      # "state.pctile.Demog.Index", #now in names_d_pctile
      names_d_state_pctile, names_d_subgroups_state_pctile, 
      ### D STATE AVERAGES ###
      #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg,# eg    state.avg.pctmin 
      names_d_state_avg,
      
      ## ENVIRONMENTAL  -----------------
      
      ### E RAW # ###
      names_e,  
      ### E US RATIOS?    # (above)
      ### E US PCTILE ###
      names_e_pctile, #(US) 
      ### E US AVERAGES   ###
      names_e_avg,
      ### E STATE RATIOS?    # (above)
      ### E STATE PCTILE ###
      names_e_state_pctile, 
      ### E STATE AVERAGES  ###
      names_e_state_avg,
      ### MISC E ###
      'NUM_NPL', 'NUM_TSDF', # Extra from EJScreen - essentially envt related
      
      ## EJ INDEXES  -----------------
      
      ### EJ PCTILE US ###
      names_ej_pctile, 
      ### EJ PCTILE STATE ###
      names_ej_state_pctile,  #  
      ### EJ RAW -NOT NEEDED?  ###
      names_ej, # raw scores not essential in output 
      
      ### D RAW COUNTS? -NOT NEEDED?  ###
      names_d_count, names_d_subgroups_count,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT
      names_other,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT # denominator counts but also pop which is already above
      
      ## BG AND BLOCK COUNTS ----
      #  # it will use whichever version of name is found
      'statLayerCount',      "bgcount_near_site", "bgcount_overall",    # count of blockgroups, as named in API vs in EJAM outputs
      'weightLayerCount', "blockcount_near_site", "blockcount_overall",  # count of blocks, as named in API vs in EJAM outputs     
      
      'radius', 'radius.miles' # it will use whichever version of name is found
      
    )
    useful_column_order <- collapse::funique(useful_column_order)
  }
  # retain all the columns now, but put 1st the ones specified by useful_column_order
  #  1 and 2  are used because/in case there are differences in how overall and by site refer to a stat like "bgcount_near_site" vs "bgcount_overall"
  
  useful_column_order1 <- c(useful_column_order[useful_column_order %in% names(results_overall)], setdiff(names(results_overall), useful_column_order))
  data.table::setcolorder(results_overall, neworder = useful_column_order1)
  
  # useful_column_order2 <- c(useful_column_order[useful_column_order %in% names(results_bysite)], setdiff(names(results_bysite), useful_column_order))
  data.table::setcolorder(results_bysite, neworder = useful_column_order1)
  
  useful_column_order3 <- c(useful_column_order[useful_column_order %in% names(sites2bgs_plusblockgroupdata_bysite)], setdiff(names(sites2bgs_plusblockgroupdata_bysite), useful_column_order))
  data.table::setcolorder(sites2bgs_plusblockgroupdata_bysite, neworder = useful_column_order3)
  # only 74 columns in that last, longer table
  ##################################################### #  ##################################################### #  ##################################################### #
  
  # DONE - Return list of results ####
  
  ## results list ###
  longnames <- EJAMejscreenapi::map_headernames$longname_tableheader[match(
    names(results_overall), EJAMejscreenapi::map_headernames$newnames_ejscreenapi
  )]
  # if no long name found, just use what was there already, otherwise will use the nicer longnames  
  longnames[is.na(longnames)] <- names(results_bysite)[is.na(longnames)]
  
  ########################### #
  ## temporarily get friendly versions of some ratio names, for longnames !!  (and could make friendly the headers in all tables?) ####
  renamer = data.frame(
    old=grep("ratio.to",names_all, value=TRUE), 
    new=grep("Ratio to",names_all, value=TRUE))  # map old varnames to new friendly versions
  # renamer_check <- data.frame(
  #   old = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg,  names_e_ratio_to_avg,  names_d_ratio_to_state_avg,  names_d_subgroups_ratio_to_state_avg,  names_e_ratio_to_state_avg ), 
  #   new = c(names_d_ratio_to_avg_friendly, names_d_subgroups_ratio_to_avg_friendly, names_e_ratio_to_avg_friendly, names_d_ratio_to_state_avg_friendly, names_d_subgroups_ratio_to_state_avg_friendly,  names_e_ratio_to_state_avg_friendly))
  # > all.equal(sort(renamer$new), sort(renamer_check$new))
  # [1] TRUE
  # > all.equal(sort(renamer$old), sort(renamer_check$old))
  # [1] TRUE
  longnames[ longnames %in% renamer$old] <- renamer$new[match(longnames[ longnames %in% renamer$old] , renamer$old)]
  ## temporarily get friendly versions of some average names ####
  renamer = data.frame(
    old=c(names_e_avg,       names_d_avg,       names_d_subgroups_avg, 
          names_e_state_avg, names_d_state_avg, names_d_subgroups_state_avg),
    new=c(names_e_avg_friendly,   names_d_avg_friendly, names_d_subgroups_avg_friendly, 
          names_e_state_avg_friendly,   names_d_state_avg_friendly, names_d_subgroups_state_avg_friendly)
  )  # map old varnames to new friendly versions (not all were there)
  longnames[ longnames %in% renamer$old] <- renamer$new[match(longnames[ longnames %in% renamer$old] , renamer$old)]
  renamer = data.frame(
    old=c(
      'distance_min',
      'distance_min_avgperson',
      'sitecount_max', 
      'sitecount_unique',
      'sitecount_avg'
    ),
    new=c(
      "Distance to Closest Site",
      "Distance to Closest Site for Avg Person",
      "Number of Sites Nearby (max)",
      "Number of Sites Nearby (total unique)",
      "Number of Sites Nearby (avg)"
    )
  )  
  longnames[ longnames %in% renamer$old] <- renamer$new[match(longnames[ longnames %in% renamer$old] , renamer$old)]
  
  ########################### #
  
  results <- list(
    results_overall = results_overall,  # each indicator
    results_bysite  = results_bysite,   # each indicator, at each site
    
    results_bybg_people = sites2bgs_plusblockgroupdata_bysite,  # each indicator, at each BG-site combo, not just each UNIQUE BG !!
    #  That allows one to see distrib within each demog at each site, not just overall, 
    #  but need be careful when looking at that stat overall to not count some bgs twice.
    
    longnames = longnames,
    
    # results_summarized gets added here later, by batch.summarize() in ejamit() or app_server()
    
    # formatted gets added here later also  
    
    
    # SEPARATE VARIABLES TO RETURN ALONE: 
    count_of_blocks_near_multiple_sites = count_of_blocks_near_multiple_sites #, 
    # blockcount_overall = blockcount_overall, # note already also in results_overall as a column now, so we dont need to duplicate it here
    # bgcount_overall = bgcount_overall        # note already also in results_overall as a column now, so we dont need to duplicate it here
  )
  # }) # finish system.time()
  
  if (interactive() & !silentinteractive) {  # false if using shiny web app
    # print(timed)
    # cat("count of blocks that are near more than 1 site:", results$count_of_blocks_near_multiple_sites, "\n")
    # cat("count of blocks total, near all sites:", results$blockcount_overall, "\n")
    # cat("count of block groups, near all sites:", results$bgcount_overall, "\n")
    
    ## Show simple overall stats list in console ####
    
    x <- as.list(results$results_overall)
    x <- data.frame(variable = names(x), overall = unlist(x))
    rownames(x) <- NULL
    x$longname <- results$longname  # EJAMejscreenapi::map_headernames$longname_tableheader[match(x$variable, map_headernames$newnames_ejscreenapi)]
    x$longname <- substr(x$longname, 1, 40)
    x$overall <- round(x$overall, 3)
    print(x) # print to console, 125 rows
    # cat("See viewer for datatable view site by site\n")
    # Show datatable view of each site by site in RStudio ####
    
    # bysite <- results$results_bysite
    # print(DT::datatable(bysite, options = list(paging=FALSE), colnames=results$longnames , escape=FALSE))
    cat('see ejamit() as well\n')
  }
  invisible(results)
  ##################################################### #  ##################################################### #  ##################################################### #
}

