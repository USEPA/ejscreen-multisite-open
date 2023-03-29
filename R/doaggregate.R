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
#'    - quaddata, and blockquadtree: data.table and quad tree, for indexes of block points
#'      (and localtree that is created when package is loaded)
#'    
#'    - EJAM::blockgroupstats - A data.table (such as EJSCREEN demographic and environmental data by blockgroup?)
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
#' @param ... more to pass to another function? Not used currently.
#' @seealso [ejamit]   [getblocksnearby()]  
#' @import data.table
#' @import EJAMblockdata
#' @export
#' 
doaggregate <- function(sites2blocks, sites2states_or_latlon=NA, countcols=NULL, popmeancols=NULL, calculatedcols=NULL, testing=FALSE, include_ejindexes=FALSE, updateProgress = NULL, ...) {
  
  # timed <- system.time({
    
    ##################################################### #  ##################################################### #
    # Get ST (state) each site is in, to report scores as state percentiles ####
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
    
    sites2states <- states_infer(sites2states_or_latlon) 
    
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
      # # note that names.d.count was not yet defined in ejscreen pkg, and would lack denominators if only based on pctxyz
      # names.d.count <- union( gsub('pct','', grep(pattern = 'pct', ejscreen::names.d, value=TRUE)),
      #  c('unemployed', 'unemployedbase'))
      # names.d.count <- gsub('^min$', 'mins', names.d.count) # since singular was used in pctmin but plural for count is mins
      # countcols <- unique(c('pop', 'nonmins', names.d.count,
      #                "povknownratio",  "age25up", "hhlds",  'pre1960', 'builtunits',
      #                ejscreen::names.d.subgroups.count)) 
      
      
      # blockcount_by_site, bgcount_by_site
      
      countcols <- unique(c(
        names_other, names_d_count, names_d_subgroups_count
        # "pop", 'nonmins', "mins", 
        # "lowinc",   "povknownratio",   
        # "lths",     "age25up", 
        # "lingiso",  "hhlds", 
        # "under5", "over64",
        # "unemployed",   "unemployedbase", # new in 2022
        # 'pre1960',  'builtunits',
        # "nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone", "nhmulti" # not in EJScreen 2.1 but will use here
      ))
    } 
    # **** but we probably treat pctpre1960 as pop wtd mean like other Evars?
    
    if (is.null(calculatedcols)) {
      # calculatedcols <- c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged') # use formulas for these
      #  or to avoid depending on ejscreen package, 
      #  dput(c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged')) # but make sure pctunemployed got added
      
      calculatedcols <- unique(c(
        names_d,  #   "lowlifex"    "Demog.Index.Supp",  # already in names_d
        names_d_subgroups,  
        'flagged'
        # Demog.Index", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
        # "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
        # "flagged"
        #      and will have lowlifex and "Demog.Index.Supp", 
      ))
      
      # These must be calculated after aggregating count variables and using those at siteid level. 
      # e.g. Use ejscreen::ejscreenformulas$formula to calculate these.
    }
    if (is.null(popmeancols)) {
      # popmeancols <- c(ejscreen::names.e, ejscreen::names.ej)
      # or to avoid depending on ejscreen package, 
      # dput(c(ejscreen::names.e, ejscreen::names.ej) )
      
      popmeancols <- unique(c(
        names_e,
        names_ej 
        
        # 'NUM_NPL', 'NUM_TSDF',
        
        # "lowlifex"   # new, not completely sure it should be via popwtd mean, or calculated via formula actually.
        
        # "pm", "o3", "cancer", "resp", "dpm", 
        # "pctpre1960", "traffic.score", 
        # "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", 
        # "ust", 
        # "EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
        # "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
        # "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo", 
        # "EJ.DISPARITY.ust.eo"
      ))
      
      # ** CHECK THIS:  EJScreen treats pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas. . . ratio of sums of counts pre1960 and denom builtunits  
      # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop. 
      #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
      # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer? 
      # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer, 
      # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).
    }  
    ##################################################### #  ##################################################### #
    
    
    ## update progress bar in shiny app
    if(is.function(updateProgress)){
      
      boldtext <- paste0('Calculating indicators at each site and overall')
      updateProgress(message_main = boldtext, 
                     value = 0.2)
    }
    
    # Start Aggregating #############################################################################################
    
    # Some steps are the same for overall and site-by-site so it is more efficient to do both together if want both. 
    if (testing) {library(data.table); library(EJAMblockdata);     sites2blocks <- EJAM::sites2blocks_example }
    # data.table::setkey(result, "blockid", "siteid", "distance") #  has been done by getblocksnearby  now
    # use blockid, not fips.   *********************  THIS was SLOW trying to do merge by blockfips or join on blockfips
    
    ## BLOCK RESOLUTION  #######################################
    
    ## Get pop weights of nearby blocks ####
    # to know what fraction of each parent block group is considered inside the buffer 
    # sites2blocks <- merge(sites2blocks, blockwts, by='blockid', all.x	=TRUE, all.y=FALSE) # incomparables=NA
    sites2blocks <- EJAMblockdata::blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgid), on='blockid']
    # note that this still has some blocks appearing more than once if near 2+ sites - each row has info on one site only
    # xxx
    
    # sort rows
    data.table::setorder(sites2blocks, siteid, bgid, blockid) # new
    
    ##################################################### #  ##################################################### #
    
    ###################################### #
    ## _PROXIMITY METRICS IDEAS   #### 
    #  Some kinds of info about 1) # of Sites and 2) their distances, or 3) proximity score** (combo of count and distance)
    #  near each Block (or person), by block group, by site, and then by Demographic group.  
    #  Useful potentially.
    # **  EJScreen style proximity score for each block group as a way to summarize the count and distance of each site.
    # 
    # Summarize count/distance/proximity score info for various groups/places: 
    #  - avg and worst-case resident overall (averages, and min dist, max count, max proximity score)
    #  - avg and worst-case resident in each Demographic group (flags notable disparities in Demog group proximities and site counts nearby)
    #  - worst-case sites? to flag notable sites. [usually just 1 site nearby, at avg distance of Radius * 0.67, and worst case distance zero and several sites nearby; 
    #       The cumulative effect of some of these sites being somewhat clustered near each other. 
    ###################################### #
    
    
    
    
    ##### *****  work in progress -   by=blockid    creates the same info in each row for duplicate blockids, which is ok. Typically not a large % are duplicated so it is not much slower, and dupes are removed later for overall stats.
    
    
    
    ## _min distance to any site, for each block ####
    # do we really need this here, or only when 1) summarizing by blockgroup and retaining both sites at one block, eg,
    #  and 2) when dropping the duplicate blocks to get overall stats?
    
    sites2blocks[, sitedistance_min := min(distance, na.rm = TRUE), by=blockid] # Presumably sometimes very close to zero or to effective radius of nearest block.
    
    ## _count of sites, for (near) each block ####
    sites2blocks[, sitecount := .N, by=blockid] # (for this, must use the table with duplicate blocks, not unique only)
    # what will we do with this stat, if anything?
    
    ## _Proximity Score of block ####
    sites2blocks[, proximityscore := sum(1 / distance, na.rm = TRUE), by=blockid]
    # sites2blocks[distance < effectiveradius, proximityscore := sum(1 / effectiveradius, na.rm = TRUE), by=blockid]
    # **** TO BE FIXED: / WARNING:  doesnt the formula need the adjustment for small distance?? You need the block area to calculate its effective radius and adjust score if distance is <that? see EJScreen tech doc
    warning('proximityscore lacks small distance adjustment factor - not yet implemented')
    
    
    
    ###################################### #
    ## * Unique residents (blocks) only, used for Overall stats #### 
    ###################################### #
    # each block only once, and therefore each person only once even if near 2+ sites.
    # For each overall resident (blockid) near 1 or more sites, 
    # find and save the info for just the closest single siteid (sitedistance_min)
    # ***    Just for now, the simplistic way to drop duplicate blocks (even if other columns are not duplicates), 
    #    which are residents near 2 or more sites, to avoid double-counting them, is unique() 
    # This seems fine for overall stats, since you are just dropping a duplicate block 
    # so you just need to realize one row with that block had info on site 1 and another row w that duplicated block had info on site 2.
    # So you actually need to sum the counts and proximity scores for the consolidated block, 
    # and use the min distance (which block-site had the closer site?)
    # sites2blocks_overall <- unique(sites2blocks, by=blockid)    # would keep all columns but only one nearby site would be kept for each block.
    # Slowest way, but could get all that explicitly maybe like specifying each as max or min 
    
    # done above: sites2blocks <- EJAMblockdata::blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgid), on='blockid']
    
    #####   THIS SEEMS REDUNDANT... It already created by blockid all these and now again summarizing them by blockid? 
    sites2blocks_overall <- sites2blocks[, list(sitedistance_min = min(sitedistance_min),
                                                sitecount_max = .N,
                                                proximityscore = sum(proximityscore),
                                                bgid,
                                                
                                                blockwt # ???
    ),
    by="blockid"]
    
    #__________________________ #################################################### #  ##################################################### #  ######################################################
    
    
    
    ##################################################### #
    # ___summarize by blockGROUP now___ ######
    
    ##################################################### #
    ## (check this idea) ####
    # was going to try to do join of weights and the aggregation by blockid all in this one step? but not tested 
    # and may need to do intermed step 1st, where 
    # sites2bg <- blockwts[sites2blocks, .(siteid, bgid, distance, bgwt = sum(blockwt, na.rm=TRUE)), on = 'blockid', by =.(siteid, bgid)] 
    #
    ## why do sum(blockwt) by bgid  here AGAIN, if already did it above?
    # rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??
    ##################################################### #
    
    ## Aggregate blocks into blockgroups, per siteid ***  #######################################
    
    ## Calc bgwt, the fraction of each (parent)blockgroup's censuspop that is in buffer #### 
    
    ## *?? WHICH OF THESE VERSIONS WAS BETTER? BY REFERENCE should be faster?  #### 
    # sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt),     by="bgid"]  # variable not used !
    # sites2blocks[        , bg_fraction_in_buffer_bysite  := sum(blockwt, na.rm = TRUE), by=c("siteid", "bgid")]
    # VERSUS
    sites2bgs_overall   <-    sites2blocks_overall[ , .(bgwt = sum(blockwt, na.rm = TRUE),
                                                        proximityscore = stats::weighted.mean(proximityscore,   w = blockwt, na.rm = TRUE),
                                                        distance_avg   = stats::weighted.mean(distance,         w = blockwt, na.rm = TRUE),
                                                        sitedistance_min = min(sitedistance_min, na.rm = TRUE),
                                                        sitecount_max    = max(sitecount,        na.rm = TRUE)
                                                        ), by=         "bgid" ]
    
    sites2bgs_bysite    <-    sites2blocks[         , .(bgwt = sum(blockwt, na.rm = TRUE), 
                                                        
                                                        proximityscore = stats::weighted.mean(proximityscore,   w = blockwt, na.rm = TRUE),
                                                        distance_avg   = stats::weighted.mean(distance,         w = blockwt, na.rm = TRUE),
                                                        sitedistance_min = min(sitedistance_min, na.rm = TRUE),
                                                        sitecount_max    = max(sitecount,        na.rm = TRUE)
                                                        ), by=.(siteid, bgid)]
    
    # "proximityscore"    "distance_avg"      "sitedistance_min"  "sitecount_max"     "sitecount_near_bg"
    #  THESE ARE NOT IN blockgroupstats so they cannot be defined here as part of these lists:
    # popmeancols <-  c(popmeancols, "distance_avg" , "proximityscore" )
    # countcols <-  c(countcols)
    # calculatedcols <- c(calculatedcols, "sitedistance_min" , "sitecount_max" ,    "sitecount_near_bg")
    
    ## * COUNT # unique sites near each bg? #### 
    # (and that is not same as max count for any block in the bg)
    # does this work, to summarize by bgid again, after already did so just above?
    sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by="bgid"]
    
    
    
    
    ## * AVG AND WORST DISTANCE TO SITE, for each bg? (avg person)
    # for use in distribution of distances across people by group? but can't say which people in bg had that distance.
    
    # ?
    
    ## * AVG and WORST PROXIMITY SCORE, for each bg? (avg person)
    
    # ? 
    
    ## update progress bar in shiny app
    if(is.function(updateProgress)){
      
      boldtext <- paste0('Analyzing blockgroups')
      updateProgress(message_main = boldtext, 
                     value = 0.4)
    }
    
    
    #***  ###################################### #
    
    # * Count # blocks or bgs near each SITE?  or 
    #  MAYBE FOR AVG PERSON,   OR   WORST CASE PERSON NEAR THAT SITE?? ####
    # Is that at all useful really??
    # "blockcount_near_site"            "bgcount_near_site"
    
    blockcount_by_site <- sites2blocks[, .(blockcount_near_site = .N), by=siteid] # new----------------------------------------------- -
    bgcount_by_site    <- sites2blocks[, .(bgcount_near_site = length(unique(bgid))), by=siteid] # new------------------------------------ -
    
    # * Count # blocks or bgs near any 2+ sites overall ####
    count_of_blocks_near_multiple_sites <- (NROW(sites2blocks) - NROW(sites2blocks_overall)) # NEW fraction is over /NROW(sites2blocks_overall)
    blockcount_overall <- length(unique(sites2blocks$blockid))
    bgcount_overall    <- length(unique(sites2blocks$bgid))
    
    # how many blockgroups here were found near 1, 2, or 3 sites? 
    # e.g., 6k bg were near only 1/100 sites tested, 619 near 2, 76 bg had 3 of the 100 sites nearby.
    # table(table(sites2bgs_bysite$bgid))
    
    ##################################################### #  ##################################################### #
    # * HOW TO GET MORE STATS ON DISTRIBUTION OF DISTANCES OR ENVT, BY GROUP? ####
    
    # see  EJAM/inst/notes_MISC/NOTES_IDEA_OUT_BY_BG_SHOWS_DISTRIB_OVER_PEOPLE.R
    
    # considered removing sites2blocks and sites2blocks_overall NOW TO FREE UP RAM, BUT THAT IS slow!:
    # if (!testing) {rm(sites2blocks); gc() }
    # And need save that to analyze distance distribution! or 
    # At least save avg and worst for each block group, if not each block.
    # *****  BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP, 
    #  HAVE TO BE CALCULATED FROM BG DATA BEFORE WE AGGREGATE WITHIN EACH SITE (BUFFER). . . 
    #  Same for sites: worst site as measured by highest nearby blockgroup-level %poor needs raw bg data before summarized by siteid.
    
    
    ##################################################### #  ##################################################### #
    
    
    
    
    
    
    
    
    
    
    
    
    ##################################################### #
    #  *** JOIN sites-BGs to EJScreen indicators ####
    # joins midsized intermed table of sites & BGs to EJScreen/ blockgroupstats . . . sites2bgs_overall ??  ############################### #
    ##################################################### #
    #
    # DO JOIN  OF **blockgroupstats**   200 columns, on bgid , 
    
    # and not sure I can calculate results at same time, since this kind of join is getting a subset of blockgroupstats but grouping by sites2bgs_bysite$siteid  and 
    # maybe cannot use blockgroupstats[sites2bgs_bysite,    by=.(siteid)    since siteid is in sites2bgs_bysite not in blockgroupstats table. 
    # So, first join blockgroupstats necessary variables to the shorter sites2bgs_bysite:   
   
    
    #   Remember that. . .
    # countcols     # like population count, add up within a buffer
    # popmeancols    # we want average persons raw score,  for Environmental and EJ indexes
    # calculatedcols  # use formulas for these, like  sum of counts of lowincome divided by sum of counts of those with known poverty ratio (universe)
    countcols      <- intersect(countcols,      names(blockgroupstats))
    popmeancols    <- intersect(popmeancols,    names(blockgroupstats))
    calculatedcols <- intersect(calculatedcols, names(blockgroupstats))
    
    sites2bgs_plusblockgroupdata_bysite  <- merge(sites2bgs_bysite,  # has other cols like   "distance_avg" , "proximityscore"  etc. 
                                                  blockgroupstats[ , c('bgid', 'ST', ..countcols, ..popmeancols, ..calculatedcols)], 
                                                  all.x = TRUE, all.y=FALSE, by='bgid')
    # just be aware that this is not saving just unique blockgroups, but saves each bgid-siteid pairing???
    
    sites2bgs_plusblockgroupdata_overall <- merge(sites2bgs_overall, 
                                                  blockgroupstats[ , c('bgid', ..countcols, ..popmeancols, ..calculatedcols)], 
                                                  all.x = TRUE, all.y=FALSE, by='bgid')
    # rm(sites2bgs_overall, sites2bgs_bysite); rm(blockgroupstats)
    
    
    
    
    
    
    
    #_________________________________ #  ######################################################
    
    # ___summarize by site now___ ####
    
    ## update progress bar in shiny app
    if(is.function(updateProgress)){
      
      boldtext <- paste0('Joining blockgroups to EJScreen indicators')
      updateProgress(message_main = boldtext, 
                     value = 0.6)
    }
    
    ##################################################### #
    # CALC TOTALS FOR COUNT VARIABLES at EACH SITE & OVERALL ####  
    # AND ALSO SUBGROUPS IF WANT TO 
    #  USE FORMULAS TO GET EXACT %D AT EACH SITE AS SUM OF NUMERATORS / SUM OF DENOMINATORS)
    ##################################################### #
    
    ##  Counts Overall (all sites/ whole sector)  ####
    
    results_overall <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
      round(sum(x * bgwt, na.rm=TRUE), 1)
    } ), .SDcols = countcols ]
    
    # to be sum of unique, BY SITE, TO RETURN: blockcount_by_site, bgcount_by_site, 
    
    # others for OVERALL:  count_of_blocks_near_multiple_sites, blockcount_overall, bgcount_overall
    
   
     
    ##  Counts by site/facility  ####
    
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
    # CALC POP WEIGHTED MEAN FOR SOME VARIABLES ####   
    # ( ENVT, EJ index. . .. AND MAYBE ALL THE DEMOG TOO???)
    ##################################################### #
    
    
    calculatedcols <- c(calculatedcols,  
                        "sitedistance_min" ,   "sitecount_max")
    #  sitedistance_min  := min(sitedistance_min)
      #  sitecount_max := max(sitecount_max)
    
    
    #  sitecount_avg  := wtdmean(sitecount_near_bg)
    #  proximityscore := wtdmean(proximityscore)
    #  distance_avg   := wtdmean(distance_avg)
  
    # OTHERs BY BG, TO do sum or avg or max,   
    popmeancols <-  c(popmeancols, "distance_avg" , "proximityscore", "sitecount_near_bg" )
    
    
    # POP wtd MEAN BY SITE ####
    
    results_bysite_popmeans <- sites2bgs_plusblockgroupdata_bysite[   ,  lapply(.SD, FUN = function(x) {
      stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)
    }), .SDcols = popmeancols, by = .(siteid) ]
    
    # redo these in data.table:: style, for speed? this is just by site so only 1 row per site is not that many usually, but is a lot of columns (200?) *********************************
    results_bysite <- merge(results_bysite, results_bysite_popmeans)
    
    # POP wtd MEAN OVERALL ####
    
    ## later, for results_overall, will calc state pctiles once we have them for each site 
    
    results_overall_popmeans <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
      stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)
    }), .SDcols = popmeancols ]
    
    
    ##################################################### #
    # CALCULATE  MIN OR MAX OF ALL PLACES ####
    
    results_bysite[ , sitedistance_min  := min(sitedistance_min)]
    results_bysite[ , sitecount_max     := max(sitecount_max)]
    
    ##################################################### #
    
    # # results_bysite[ , ] xxx
    # calculatedcols <- c(calculatedcols,  
    #                     "sitedistance_min" ,   "sitecount_max")
    
    
    # rm(results_overall_popmeans, sites2bgs_plusblockgroupdata_overall)
    # rm(results_bysite_popmeans,  sites2bgs_plusblockgroupdata_bysite)
    
    ##################################################### #
    # COMBINE RESULTS ####
    
    results_bysite <- merge(results_bysite, blockcount_by_site) # on="siteid" new ---------------------------------------------- -
    results_bysite <- merge(results_bysite, bgcount_by_site)    # on="siteid" new ---------------------------------------------- -
    
    # redo these in data.table:: style, for speed? but it is just a 1-row result  *********************************
    results_overall <- cbind(results_overall, results_overall_popmeans) # many columns (the popwtd mean cols)
    results_overall <- cbind(results_overall, blockcount_near_site = blockcount_overall) # 1 new col and then changed name to match it in results_bysite ---------------------------------------------- -
    results_overall <- cbind(results_overall, bgcount_near_site = bgcount_overall) # 1 new col and then changed name to match it in results_bysite ---------------------------------------------- -
    
    
    

    
    ##################################################### #
    # CALCULATE PERCENT DEMOGRAPHICS FROM SUMS OF COUNTS, via FORMULAS  [hardcoded here, for now]
    #
    # but should do that using a list of formulas like in ejscreen::ejscreenformulas 
    # and a function like analyze.stuff::calc.fields() 
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
    
    ## update progress bar in shiny app
    if(is.function(updateProgress)){
      
      boldtext <- paste0('Computing results')
      updateProgress(message_main = boldtext, 
                     value = 0.8)
    }
    
  
    # CALC via FORMULAS with Rolled up Counts #### 
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
    #________________________________  #  ##################################################### #  ######################################################
    
    
    
    
    ## SPECIFY EACH SITES STATE HERE ####
    # Assign state abbrev to each site!! (allows for state percentiles and averages to be looked up) (and statename, FIPS.ST, REGION?) 
    # 
    # sites2states  is one row per site, and may have latlon and/or ST
    
    
    # already did this: sites2states <- states_infer(sites2states_or_latlon) # states_infer() will just clean up that lookup table. Get cleaned table of US State etc. by siteid, from lat/lon or other info
    # 
    # siteid, ST are in sites2states lookup; siteid is in sites2bgs 
    #  but not sure if have/want  statename, FIPS.ST, REGION   and maybe even lat,lon
    # for example, if siteid is just rownumber of pts,   sites2states <- data.frame(siteid=1:length(pts$lat), state_from_latlon_compiled(lat = pts$lat, lon = pts$lon))
    
    # if (bad_sites2states) {
    #   #  xxx
    #   #  where should we try to assign based on lat lon or even FIPS of closest block or blockgroup if that is the only option? in sites2states
    #   # results_bysite[ , ST := EJAM::state_from_latlon(lat,lon)] # assuming that results_bysite actually has the lat,lon !!
    #   # # could use one of these:
    #   #  state_from_latlon_compiled(lat=, lon=, states_shapefile=)
    #   #  state_from_blocktable(dt_with_blockid=)
    #   #  state_from_blockid(blockid=)
    #   results_bysite[ , ST := NA] # verify that state pctile lookups function will return NA if ST is NA. 
    #   } else {
    
    # results_bysite[ ,  ST := ]
    
    # JUST GET STATE BY SITE BASED ON most FREQUENT ST among site's bgs, from blockgroup data joined already?
    
    # results_bysite <- sites2bgs_plusblockgroupdata_bysite[ , ST[1],   by = .(siteid) ]  # would take the first not necessarily the central or most common one.
    # results_bysite <- merge(results_bysite, blockcount_by_site) # on="siteid"
    
    # results_bysite[ ,  ST := .(sites2states$ST[match(siteid, sites2states$siteid)])] # wouldnt a merge or actual join be faster? 
    
    results_bysite[sites2states,  ST := ST,  on = "siteid"]
    results_bysite[sites2states, lat := lat, on = "siteid"]
    results_bysite[sites2states, lon := lon, on = "siteid"]
    results_overall$lat <- NA
    results_overall$lon <- NA
    # }
    #***  ###################################### #
    
    
    
    ##################################################### #
    # PERCENTILES - show raw scores (from results_bysite AND  results_overall) in percentile terms #### 
    #  VIA  lookup tables of US/State  percentiles, called EJAM::usastats   and statestats
    #  note: usastats is  like ejscreen::lookupUSA , and EJAM::pctile_from_raw_lookup is like ejanalysis::lookup.pctile()
    ##################################################### #
    
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
    
    # SURELY THERE IS A FASTER / VECTORIZED WAY TO DO THIS (but only worth fixing if this actually is noticeably slow):
    # browser()
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
        state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- pctile_from_raw_lookup(
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
    # EJ INDEXES if needed ####
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
      
      ## basic 2023 EJ Indexes = 2-factor demog index times the envt percentile ####
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
    
    # ***Calc STATE PERCENTILES for results_overall ####
    #  (as popwtd mean of sites state pctiles) 
    # now that site-specific percentiles have been calculated and looked up,
    # you can calculate that overall state percentiles from those as a pop wtd mean (not by looking them up from raw scores as would be done for US pctiles, since each site may be in its own state)
    # xxx
    state.pctile.cols_overall <-  results_bysite[ ,  lapply(.SD, FUN = function(x) {
      stats::weighted.mean(x, w = pop, na.rm = TRUE)
    }), .SDcols = varnames.state.pctile ]
    
    # redo these in data.table:: style, for speed? but it is just a 1-row result  *********************************
    results_overall <- cbind(results_overall, state.pctile.cols_overall)
    results_overall$ST <- NA
    # sites2bgs_plusblockgroupdata_bysite$ST <-  
    ##################################################### #  ##################################################### #  ##################################################### #
    # Put results columns in a more useful/ convenient order ####
    {
      useful_column_order <- c(
        'id', 'siteid',
        'radius', 'radius.miles', # it will use whichever version of name is found
        'pop',           # '[or names_wts]',
        'sitename',
        'lon', 'lat',
        'ST', 'statename', 'REGION', 
        
        ## DEMOGRAPHICS -----------------
        
        ### D RAW % ####
        "Demog.Index.Supp", # supplemental demographic indicator
        names_d,   # Demog.Index, percent low income, etc.
        names_d_subgroups,   # percent hispanic etc.
        # low life expectancy score 
        "lowlifex", 
        
        ###  D US RATIOS? TO BE CALCULATED could GO HERE ####
        
        
        
        
        
        
        ### D US PCTILE ####
        "pctile.Demog.Index", names_d_pctile,  names_d_subgroups_pctile, 
        ### D US AVERAGES? ####
        #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg,#"us.avg.Demog.Index" ,
        ### D STATE RATIOS? TO BE CALCULATED WILL GO HERE  ####
        
        ### D STATE PCTILE ####
        "state.pctile.Demog.Index", names_d_state_pctile, names_d_subgroups_state_pctile, 
        ### D STATE AVERAGES? ####
        #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg,# eg    state.avg.pctmin 
        
        ## ENVIRONMENTAL  -----------------
        
        ### E RAW # ####
        names_e,  
        ### E US RATIOS? TO BE CALCULATED could GO HERE]####
        
        ### E US PCTILE ####
        names_e_pctile, #(US) 
        ### E US AVERAGES?  ####
        #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg,# eg  us.avg.pm 
        
        ### E STATE RATIOS? TO BE CALCULATED WILL GO HERE] ####
        
        ### E STATE PCTILE ####
        names_e_state_pctile, 
        ### E STATE AVERAGES? ####
        #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg, # eg  state.avg.pm  
        
        ### MISC E ####
        'NUM_NPL', 'NUM_TSDF', # Extra from EJScreen - essentially envt related
        
        ## EJ INDEXES -----------------
        
        ### EJ PCTILE US ####
        names_ej_pctile, 
        ### EJ PCTILE STATE ####
        names_ej_state_pctile,  #  
        
        ### EJ RAW -NOT NEEDED? not essential in output ####
        names_ej, # raw scores not essential in output 
        
        ### D RAW COUNTS? -NOT NEEDED? not essential in output #### 
        names_d_count, names_d_subgroups_count,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT
        names_other,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT # denominator counts but also pop which is already above
        
        ## BG AND BLOCK COUNTS ----
        #  # it will use whichever version of name is found
        'statLayerCount',      "bgcount_near_site", "bgcount_overall",    # count of blockgroups, as named in API vs in EJAM outputs
        'weightLayerCount', "blockcount_near_site", "blockcount_overall"  # count of blocks, as named in API vs in EJAM outputs     
      )
      useful_column_order <- unique(useful_column_order)
    }
    # retain all the columns now, but put 1st the ones specified by useful_column_order
    #  1 and 2  are used because/in case there are differences in how overall and by site refer to a stat like "bgcount_near_site" vs "bgcount_overall"
    # browser()
    useful_column_order1 <- c(useful_column_order[useful_column_order %in% names(results_overall)], setdiff(names(results_overall), useful_column_order))
    data.table::setcolorder(results_overall, neworder = useful_column_order1)
    
    useful_column_order2 <- useful_column_order[useful_column_order %in% names(results_bysite)]
    data.table::setcolorder(results_bysite, neworder = useful_column_order2)
    
    useful_column_order3 <- useful_column_order[useful_column_order %in% names(sites2bgs_plusblockgroupdata_bysite)]
    data.table::setcolorder(sites2bgs_plusblockgroupdata_bysite, neworder = useful_column_order3)
    
    
    # names_all <- c(
    #   names_other, # includes pop and other denominator counts
    #   names_d,           names_d_pctile,           names_d_state_pctile,           names_d_count, 
    #   names_d_subgroups, names_d_subgroups_pctile, names_d_subgroups_state_pctile, names_d_subgroups_count, 
    #   names_e,           names_e_pctile,           names_e_state_pctile, 
    #   names_ej,          names_ej_pctile,          names_ej_state_pctile 
    # )
    # 
    # names(results_overall)                     <- gsub("VSI.eo", "Demog.Index", names(results_overall))
    # names(results_bysite)                      <- gsub("VSI.eo", "Demog.Index", names(results_bysite))
    # names(sites2bgs_plusblockgroupdata_bysite) <- gsub("VSI.eo", "Demog.Index", names(sites2bgs_plusblockgroupdata_bysite))
    
    # Note on Avg Distance and range of distances in each Demog group, & %D as function of distance ####
    # 
    # We have each blockgroup near each site, which means some small % of those bgs are duplicated in this table:
    #   sites2bgs_plusblockgroupdata_bysite
    # Mostly we want overall (not by site) to know avg and cum distrib of distances in each demog, 
    #  and also %D as a function of continuous distance,
    #  and for those stats we would want to take only unique blockgroups from here, using the shorter distance I think, so the distribution of distances does not doublecount people.
    # But we might also want to see that distribution of distances by D for just 1 site??
    # And we might also want to see the %D as a function of continuous distance at just 1 site??
    # So it would be simpler, clearer to return just all unique blockgroups,
    #  but to retain flexibility this function currently reports all instances of blockgroup-site pairing.
    
    ##################################################### #  ##################################################### #  ##################################################### #
    # DONE - Return list of results ####
    ## results list ###
    longnames <- EJAMejscreenapi::map_headernames$longname_tableheader[match(
      names(results_overall), EJAMejscreenapi::map_headernames$newnames_ejscreenapi
    )]
    # if no long name found, just use what was there already, otherwise will use the nicer longnames  
    longnames[is.na(longnames)] <- names(results_bysite)[is.na(longnames)]
    
    results <- list(
      results_overall = results_overall,  # each indicator
      results_bysite  = results_bysite,   # each indicator, at each site
      results_bybg_people = sites2bgs_plusblockgroupdata_bysite,  # each indicator, at each BG
      longnames = longnames,
      
      # TABLES OF DIAGNOSTIC STATS: BUT THESE ARE REDUNDANT - already are columns in the by_site table:
      # blockcount_by_site = blockcount_by_site, # x$results_bysite$blockcount_near_site == x$blockcount_by_site$blockcount_near_site
      # bgcount_by_site    = bgcount_by_site,    # x$results_bysite$bgcount_near_site    == x$bgcount_by_site$bgcount_near_site
      
      # SEPARATE VARIABLES TO RETURN ALONE: 
      count_of_blocks_near_multiple_sites = count_of_blocks_near_multiple_sites #, 
      # blockcount_overall = blockcount_overall, # note already also in results_overall as a column now, so we dont need to duplicate it here
      # bgcount_overall = bgcount_overall        # note already also in results_overall as a column now, so we dont need to duplicate it here
    )
  # }) # finish system.time()
  
  if (interactive()) {  # false if using shiny web app
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
    cat("See viewer for datatable view site by site\n")
    ## Show datatable view of each site by site in RStudio ####
    
    bysite <- results$results_bysite
    print(DT::datatable(bysite, options = list(paging=FALSE), colnames=results$longnames ))
  }
  
  invisible(results)
  ##################################################### #  ##################################################### #  ##################################################### #
}

