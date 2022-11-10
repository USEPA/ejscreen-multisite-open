#' Summarize indicators in each buffer (given the blocks in each buffer and indicators for each block)
#'
#' @description This updated 2022 code takes a set of facilities and the set of blocks that are near each,
#'   (as identified previously, in other code that has identified which blocks are nearby)
#'   and combines those with indicator scores for block groups.
#'
#'   It aggregates the blockgroup scores to create a summary of each indicator,
#'    as a raw score and US percentile and State percentile,
#'    in each buffer (i.e., near each facility):
#'      - SUMS of counts: such as for population or number of households or Hispanics
#'      - POPULATION-WEIGHTED MEANS: for  Environmental indicators.
#'      -  NOTE: *** EJ Indexes: These could be in theory recalculated via formula, but the way EJScreen 
#'          does this is apparently finding the pop wtd mean of EJ Index raw scores,
#'          not the EJ Index formula applied to the summarized demographic score and aggregated envt number.
#'      - CALCULATED BY FORMULA: Buffer or overall score calculated via formulas using aggregated counts, 
#'          such as percent low income = sum of counts low income / sum of counts of denominator, 
#'          which in this case is the count of those for whom the poverty ratio is known.
#'      - LOOKED UP VALUES: Aggregated scores are converted into percentile terms via lookup tables (US or State version).
#'
#' @details
#'  \preformatted{
#'
#'   requires the following as data lazy loaded for example from EJAMblockdata package:
#'   
#'    - blockwts: data.table with these columns: blockid , bgid, blockwt
#'    
#'    - quaddata, and blockquadtree: data.table and quad tree,  for indexes of block points
#'    
#'    - EJAM::blockgroupstats - A data.table (such as EJSCREEN demographic and environmental data by blockgroup?)
#'    
#'    - statesshp?    (had been used - a shapefile of state boundries to determine what state a point is in)
#'    
#'    - stateregions? (had been used - data.table lookup of EPA REGION given the ST - state code like AK)
#'  }
#'
#' @param sites2blocks data.table of distances in miles between all sites (facilities) and 
#'   nearby Census block internal points, with columns siteid, blockid, distance,
#'   created by getblocksnearby  function. 
#'   See sites2blocks_example dataset in package, as input to this function
#' @param countcols character vector of names of variables  to aggregate within a buffer
#'   using a sum of counts, like, for example, the number of people for whom a 
#'   poverty ratio is known, the count of which is the exact denominator needed
#'   to correctly calculate percent low income. 
#' @param popmeancols character vector of names of variables to aggregate within a buffer
#'   using population weighted mean.
#' @param calculatedcols character vector of names of variables to aggregate within a buffer
#'   using formulas that have to be specified.
#' @param testing used while testing this function
#' @param ... more to pass to another function? Not used currently.
#' @import data.table
#' @import EJAMblockdata
#' @export
#'
doaggregate <- function(sites2blocks, countcols=NULL, popmeancols=NULL, calculatedcols=NULL, testing=FALSE, ...) {
  
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
  
  ##################################################### #  ##################################################### #
  ## Specify Which vars are sum of counts, vs wtd avg, vs via formula ####
  
  if (is.null(countcols)) {
    # # note that names.d.count was not yet defined in ejscreen pkg, and would lack denominators if only based on pctxyz
    # names.d.count <- union( gsub('pct','', grep(pattern = 'pct', ejscreen::names.d, value=TRUE)),
    #  c('unemployed', 'unemployedbase'))
    # names.d.count <- gsub('^min$', 'mins', names.d.count) # since singular was used in pctmin but plural for count is mins
    # countcols <- unique(c('pop', 'nonmins', names.d.count,
    #                "povknownratio",  "age25up", "hhlds",  'pre1960', 'builtunits',
    #                ejscreen::names.d.subgroups.count)) 
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
      names_d, 
      names_d_subgroups, 
      'flagged'
      # "VSI.eo", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
      # "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
      # "flagged"
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
      # "pm", "o3", "cancer", "resp", "dpm", 
      # "pctpre1960", "traffic.score", 
      # "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", 
      # "ust", 
      # "EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
      # "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
      # "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo", 
      # "EJ.DISPARITY.ust.eo"
    ))      
      # WE NEED TO   ASSIGN   STATE PERCENTILES OF EJ INDEXES 
      # AND THEN ROLL UP IN BUFFER BY FINDING POPWTD MEAN STATE PERCENTILE ? 
      # or state pctile of the popwtd mean raw EJ index?...
      #   where WE ASSUME THE ENTIRE BUFFER IS MAINLY OR ALL IN ONE STATE AND 
      #   LOOK UP THE RAW EJ INDEX IN THAT STATE'S LOOKUP TO ASSIGN THE PERCENTILE. THE LATTER, I THINK. 

    
    # ** CHECK THIS:  EJScreen treats pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas... ratio of sums of counts pre1960 and denom builtunits  
    # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop. 
    #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
    # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer? 
    # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer, 
    # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).
  }  
  ##################################################### #  ##################################################### #
  
  
  
   
  # Start Aggregating #############################################################################################
  
  # NOTE ON HANDLING DOUBLE COUNTING
  # Some steps are the same for overall and site-by-site so it is more efficient to do both together if want both. 
  # (SEE NOTES ON HOW TO MAKE AGGREGATE...)
  
  if (testing) {
    # FOR TESTING
    library(data.table); library(EJAMblockdata)
    # data("blockwts"); data('sites2blocks_example') # it is called  sites2blocks_example
    sites2blocks <- sites2blocks_example
  }
  # data.table::setkey(result, "blockid", "siteid", "distance") #  has been done by getblocksnearby  now
  
  # use blockid, not fips.   *********************  THIS IS SLOW:
  # sites2blocks***** <- merge(EJAMblockdata::blockid2fips, sites2blocks, by='blockfips', all.x=FALSE, all.y=TRUE)
  # sites2blocks$blockid <- EJAMblockdata::blockid2fips[sites2blocks, .(blockid), on='blockfips']
  # sites2blocks[,blockfips := NULL]
  # data.table::setkey(sites2blocks, 'blockid', 'siteid')
  
  
  
  ## block weights  #######################################
  
  ## Get pop weights for nearby blocks ####
  # sites2blocks <- merge(sites2blocks, blockwts, by='blockid', all.x	=TRUE, all.y=FALSE) # incomparables=NA
  sites2blocks <- EJAMblockdata::blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgid), on='blockid']
  
 # sort rows
  data.table::setorder(sites2blocks, siteid, bgid, blockid) # new
  
  ###################################### #
  ## * # of Sites near each Block (or person): #### 
  ###################################### #
  # Useful but not sure it was provided in outputs?
  # Can use to calculate How many of these sites are near avg resident in a given bg,
  # or count of nearby sites for people near this given site (avg person near site, or max count for any person near the site)
  # or distribution of counts of nearby sites across all by demog group, 
  # or near avg resident overall? 
  sites2blocks[, sitecount_near_block := .N, by=blockid] # (for this, must use the table with duplicate blocks, not unique only)
  
  ###################################### #
  ## * Unique residents (blocks) only, used for Overall stats #### 
  ###################################### #
  # and thus each person only once even if near 2+ sites.
  # For each overall resident (blockid) near 1 or more sites, 
  # find and save the info for just the closest single siteid.
  # ***    Just for now, The simplistic way to drop duplicate blocks (even if other columns are not duplicates), 
  #    which are residents near 2 or more sites, to avoid double-counting them, is this:
  sites2blocks_overall <- unique(sites2blocks, by="blockid")  # ???

  ###################################### #
  ## * AVG & MIN DISTANCE stats useful ####
  ###################################### #
  ## * These would be useful - are they calculated yet or returned? ####
  #
  ## _Proximity (block to site) of AVG person in bg ####
  #    (i.e., the censuspop-wtd mean of block-specific distances, for each bg)
  # AND
  ## _Proximity (block to site) of CLOSEST person in bg ? #### 
  # AND
  ## _Proximity (block to site) of EVERY BLOCK (resident) to see full distribution of distances even within a bg ? ####

  # Note the unique() would just pick the first instance found of a given blockid, regardless of which siteid that was for,
  # so it retains whatever distance happened to be the one found first, and same for all site characteristics.
  # *** We actually instead want to save the shortest distance for that blockid, as the worst case proximity
  # to be able to have summary stats by group of distance to the closest site, not to a random site among those nearby. 
  # What is the efficient way to find that?

   
  # ????????????????????
  
  
  
  # block groups analysis:   #######################################
  
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
  
  ## *?? WHICH OF THESE VERSIONS WAS BETTER? BY REFERENCE?  #### 
     # sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt), by=bgid]  # variable not used !
     # sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt, na.rm = TRUE), by=c('bgid', 'siteid')]
  # VERSUS
     sites2bgs_overall <- sites2blocks_overall[ , .(bgwt = sum(blockwt)), by=bgid ]
     sites2bgs_bysite  <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), by=.(siteid, bgid)]
  
     
  ## * Count # unique sites near each bg #### 
  sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by=bgid] 
  
  # Count # blocks or bgs near each site ####
  blockcount_by_site <- sites2blocks[, .(blockcount_near_site = .N), by=siteid] # new----------------------------------------------- -
  bgcount_by_site    <- sites2blocks[, .(bgcount_near_site = length(unique(bgid))), by=siteid] # new------------------------------------ -
  count_of_blocks_near_multiple_sites <- (NROW(sites2blocks) - NROW(sites2blocks_overall)) # NEW fraction is over /NROW(sites2blocks_overall)
  # how many blockgroups here were found near 1, 2, or 3 sites? 
  # e.g., 6k bg were near only 1/100 sites tested, 619 near 2, 76 bg had 3 of the 100 sites nearby.
  # table(table(sites2bgs_bysite$bgid))
  
  ##################################################### #
  # * HOW TO GET MORE STATS ON DISTRIBUTION OF DISTANCES OR ENVT, BY GROUP? ####
  #
  # considered removing sites2blocks and sites2blocks_overall NOW TO FREE UP RAM, BUT THAT IS slow!:
  # if (!testing) {rm(sites2blocks); gc() }
  # And need save that to analyze distance distribution!
  #  Maybe want some extra summary stats across people and sites (about the distribution), one column per indicator. 
  # *****  BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP, 
  #  HAVE TO BE CALCULATED FROM BG DATA BEFORE WE AGGREGATE WITHIN EACH SITE (BUFFER)... 
  #  Same for sites: worst site as measured by highest nearby blockgroup-level %poor needs raw bg data before summarized by siteid.
  
  
  
  ##################################################### #  ##################################################### #
  
  
  ##################################################### #
  #  *** JOIN sites-BGs to EJScreen indicators ####
  # joins midsized intermed table of sites & BGs to EJScreen/ blockgroupstats ... sites2bgs_overall ??  ############################### #
  ##################################################### #
  #
  # DO JOIN  OF **blockgroupstats**   200 columns, on bgid , 
  # and not sure I can calculate results at same time, since this kind of join is getting a subset of blockgroupstats but grouping by sites2bgs_bysite$siteid  and 
  # maybe cannot use blockgroupstats[sites2bgs_bysite,    by=.(siteid)    since siteid is in sites2bgs_bysite not in blockgroupstats table. 
  # So, first join blockgroupstats necessary variables to the shorter sites2bgs_bysite:   
  # early on, about 29 bg did not match on bgfips due to changes in codes as used by bg21 vs blockpoints dataset used in EJAM as of May 2022. 
  # And bg22 will be different too. check this.
  
  #   Remember that...
  # countcols     # like population count, add up within a buffer
  # popmeancols    # we want average persons raw score,  for Environmental and EJ indexes
  # calculatedcols  # use formulas for these, like  sum of counts of lowincome divided by sum of counts of those with known poverty ratio (universe)
  countcols      <- intersect(countcols,      names(blockgroupstats))
  popmeancols    <- intersect(popmeancols,    names(blockgroupstats))
  calculatedcols <- intersect(calculatedcols, names(blockgroupstats))
  
  sites2bgs_plusblockgroupdate_bysite  <- merge(sites2bgs_bysite,  
                                                blockgroupstats[ , c('bgid', ..countcols, ..popmeancols, ..calculatedcols)], 
                                                all.x = TRUE, all.y=FALSE, by='bgid')  
  sites2bgs_plusblockgroupdate_overall <- merge(sites2bgs_overall, 
                                                blockgroupstats[ , c('bgid', ..countcols, ..popmeancols, ..calculatedcols)], 
                                                all.x = TRUE, all.y=FALSE, by='bgid') 
  # rm(sites2bgs_overall, sites2bgs_bysite); rm(blockgroupstats)
  
  ##################################################### #
  # CALC TOTALS FOR COUNT VARIABLES at EACH SITE & OVERALL ####  
  # AND ALSO SUBGROUPS IF WANT TO 
  #  USE FORMULAS TO GET EXACT %D AT EACH SITE AS SUM OF NUMERATORS / SUM OF DENOMINATORS)
  ##################################################### #
  
  ##  Counts Overall (all sites/ whole sector)  ####
  results_overall <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols ]
  
  ##  Counts by site/facility  ####
  results_bysite <- sites2bgs_plusblockgroupdate_bysite[ ,    lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols, by = .(siteid) ]
  
  # results_bysite[1:100,1:8]
  # cbind(sum = prettyNum(results_overall,big.mark = ','))
  # versus if you did not remove duplicate places/people:
  # sites2bgs_plusblockgroupdate_bysite[ ,  .(sums = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), .SDcols = countcols][1,]
  # 1: 9,978,123
  # but sum(outapi_3mile_100sites$pop[outapi_3mile_100sites$statename != 'PUERTO RICO' ])
  # [1] 10,022,946
  
  ##################################################### #
  # CALC POP WEIGHTED MEAN FOR SOME VARIABLES ####   
  # ( ENVT, EJ index.... AND MAYBE ALL THE DEMOG TOO???)
  ##################################################### #
  
  # POP wtd MEAN OVERALL ####
  results_overall_popmeans <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(
    .SD, FUN = function(x) stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), .SDcols = popmeancols ]
  results_overall <- cbind(results_overall, results_overall_popmeans)
  # cbind(sum = prettyNum(results_overall, big.mark = ','))
  
  # POP wtd MEAN BY SITE ####
  results_bysite_popmeans <- sites2bgs_plusblockgroupdate_bysite[   ,  lapply(
    .SD, FUN = function(x) stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), .SDcols = popmeancols, by = .(siteid) ]
  results_bysite <- merge(results_bysite, results_bysite_popmeans)
  results_bysite <- merge(results_bysite, blockcount_by_site) # new ---------------------------------------------- -
  results_bysite <- merge(results_bysite, bgcount_by_site) # new ---------------------------------------------- -
  
  # save.image('~/R/mypackages/EJAM/inst/doagg2 so far just before calculated vars made.rda')
  # rm(results_overall_popmeans, sites2bgs_plusblockgroupdate_overall)
  # rm(results_bysite_popmeans,  sites2bgs_plusblockgroupdate_bysite)
  
  
  ##################################################### #
  # CALCULATE PERCENT DEMOGRAPHICS FROM SUMS OF COUNTS, via FORMULAS  [hardcoded here, for now]
  #
  # but should do that using a list of formulas like in ejscreen::ejscreenformulas 
  # and a function like analyze.stuff::calc.fields() 
  ##################################################### #
  
  # CALC via FORMULAS with Rolled up Counts #### 
  # this was meant to handle multiple columns (formula for each new one) for many rows (and here in buffer results, one site is a row, not one blockgroup) 
  
  # "nonmins <- nhwa"
  # "mins <- pop - nhwa" 
  results_overall[ , `:=`(
    pctover64       = 100 * ifelse(pop==0, 0,            over64        / pop),
    pctunder5       = 100 * ifelse(pop==0, 0,            under5        / pop),
    pcthisp         = 100 * ifelse(pop==0, 0, as.numeric(hisp )        / pop),
    pctnhwa         = 100 * ifelse(pop==0, 0, as.numeric(nhwa )        / pop),
    pctnhba         = 100 * ifelse(pop==0, 0, as.numeric(nhba )        / pop),
    pctnhaiana      = 100 * ifelse(pop==0, 0, as.numeric(nhaiana)      / pop),
    pctnhaa         = 100 * ifelse(pop==0, 0, as.numeric(nhaa )        / pop), 
    pctnhnhpia      = 100 * ifelse(pop==0, 0, as.numeric(nhnhpia )     / pop),
    pctnhotheralone = 100 * ifelse(pop==0, 0, as.numeric(nhotheralone) / pop), 
    pctnhmulti      = 100 * ifelse(pop==0, 0, as.numeric(nhmulti )     / pop),
    pctmin          = 100 * ifelse(pop==0, 0, as.numeric(mins)         / pop), 
    pctlowinc       = 100 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio),
    pctlths         = 100 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up), 
    pctlingiso      = 100 * ifelse(hhlds          == 0, 0, lingiso                / hhlds), 
    pctpre1960      = 100 * ifelse(builtunits     == 0, 0, pre1960                / builtunits),
    pctunemployed   = 100 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)
  ) ]
  # cbind(sum = prettyNum(results_overall, big.mark = ','))
  results_overall[ , `:=`(
    VSI.eo = (pctlowinc + pctmin) / 2  # *** add supplemental indicator too, when possible. ####
  )]
  results_bysite[ , `:=`(
    pctover64       = 100 * ifelse(pop==0, 0,            over64        / pop),
    pctunder5       = 100 * ifelse(pop==0, 0,            under5        / pop),
    pcthisp         = 100 * ifelse(pop==0, 0, as.numeric(hisp )        / pop),
    pctnhwa         = 100 * ifelse(pop==0, 0, as.numeric(nhwa )        / pop),
    pctnhba         = 100 * ifelse(pop==0, 0, as.numeric(nhba )        / pop),
    pctnhaiana      = 100 * ifelse(pop==0, 0, as.numeric(nhaiana)      / pop),
    pctnhaa         = 100 * ifelse(pop==0, 0, as.numeric(nhaa )        / pop), 
    pctnhnhpia      = 100 * ifelse(pop==0, 0, as.numeric(nhnhpia )     / pop),
    pctnhotheralone = 100 * ifelse(pop==0, 0, as.numeric(nhotheralone) / pop), 
    pctnhmulti      = 100 * ifelse(pop==0, 0, as.numeric(nhmulti )     / pop),
    pctmin          = 100 * ifelse(pop==0, 0, as.numeric(mins)         / pop), 
    pctlowinc       = 100 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio),
    pctlths         = 100 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up), 
    pctlingiso      = 100 * ifelse(hhlds          == 0, 0, lingiso                / hhlds), 
    pctpre1960      = 100 * ifelse(builtunits     == 0, 0, pre1960                / builtunits),
    pctunemployed   = 100 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)
  ) ]
  results_bysite[ , `:=`(
    VSI.eo = (pctlowinc + pctmin) / 2
  )]
  ##################################################### #  ##################################################### #  ##################################################### #
  
  # VSI.eo.US = sum(mins) / sum(pop)  +  sum(lowinc) / sum(povknownratio) ) / 2, 
  # VSI.eo = (pctlowinc + pctmin) / 2, 
  #  or is it treated as envt var and just pop mean?  ********************
  # Demog.Index <- stats::weighted.mean(Demog.Index, w = pop)
  # Demog.Index <- VSI.eo
  
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
  
  # missing:  id, lat, lon, Demog.Index which is VSI.eo, state.avg., state.pctile., us.avg., pctile., 
  #  ST, Statename, REGION, 
  #  NUM_NPL, NUM_TSDF, 
  #  StatLayerCount, StatLayerZeroPopCount, 
  #  weightLayerCount which might be the count of blocks nearby???
  # "timeSeconds", "radius.miles", "unit", "statlevel", "inputAreaMiles"
  
  # t(results_bysite[1:5,])
  # sum(results_bysite$pop)
  # results_overall$pop
  ##################################################### #  ##################################################### #  ##################################################### #
  
  
  
  
  ##################################################### #
  # PERCENTILES - show raw scores as pctiles #### 
  #  VIA  lookup tables of US/State  percentiles
  ##################################################### #
  
  # results_bysite
  # results_overall
  
  # Use the dataset called EJAM::usastats as the lookup table for USA percentiles and mean. 
  # but update/ fix it so it uses right variable names, etc., or replace with the one from ejscreen pkg
  
  # hard coded for now:
  # varsneedpctiles <- c(ejscreen::names.e, union(ejscreen::names.d, 'pctunemployed'), ejscreen::names.d.subgroups, ejscreen::names.ej)
  varsneedpctiles <- c(names_e,  names_d, names_d_subgroups, names_ej)
  varnames.us.pctile <- paste0('pctile.', varsneedpctiles)
  varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)
  
  us.pctile.cols_bysite     <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite)     <- varnames.us.pctile
  state.pctile.cols_bysite  <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite)  <- varnames.state.pctile
  us.pctile.cols_overall    <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall)    <- varnames.us.pctile
  state.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_overall) <- varnames.state.pctile
  
  for (i in seq_along(varsneedpctiles)) {
    # using EJAM::usastats not ejscreen::lookupUSA now
    myvar <- varsneedpctiles[i]
    if (myvar %in% names(usastats)) { # just like ejscreen::lookupUSA , and EJAM::lookup.pctile.US is like ejanalysis::lookup.pctile()
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]]    <- lookup.pctile.US(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]]    <- lookup.pctile.US(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
      state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- lookup.pctile.US(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats, zone =  results_bysite$ST)
      state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- lookup.pctile.US(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats, zone =  results_overall$ST)
      # but it may not make sense to do state percentiles except for EJ Indexes, in the "overall" summary?
    } else {
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]] <- NA
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]] <- NA
      state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- NA
      state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- NA
    }
  }
  # setdiff(varsneedpctiles, names(usastats)) # like ejscreen::lookupUSA
  # [1] "ust"                 "pctunemployed"      "pctnhwa"             "pcthisp"            
  # [5] "pctnhba"             "pctnhaa"             "pctnhaiana"          "pctnhnhpia"         
  # [9] "pctnhotheralone"     "pctnhmulti"          "EJ.DISPARITY.ust.eo"
  
  results_overall <- cbind(siteid=NA, results_overall, us.pctile.cols_overall, state.pctile.cols_overall)
  results_bysite  <- cbind(           results_bysite,  us.pctile.cols_bysite,  state.pctile.cols_bysite )
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
    names_d, names_d_subgroups,
    ###  D US RATIOS? TO BE CALCULATED WILL GO HERE ####
    
    ### D US PCTILE ####
    "pctile.Demog.Index", names_d_pctile,  names_d_subgroups_pctile, 
    ### D US AVERAGES? ####
     #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg,#"us.avg.Demog.Index" ,
    ### D STATE RATIOS? TO BE CALCULATED WILL GO HERE  ####
    
    ### D STATE PCTILE
    "state.pctile.Demog.Index", names_d_state_pctile, names_d_subgroups_state_pctile, 
    ### D STATE AVERAGES? ####
    #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg,# eg    state.avg.pctmin 
    ### D RAW COUNTS? #### 
    #  names_d_count, names_d_subgroups_count,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT
    #  names_other,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT # denominator counts but also pop which is already above
    
    ## ENVIRONMENTAL  -----------------
    
    ### E RAW # ####
    names_e,  
    ### E US RATIOS? TO BE CALCULATED WILL GO HERE]####
    
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
    ### EJ RAW -NOT NEEDED? ####
    # names_ej, # raw scores not essential in output 
    ### EJ PCTILE US ####
    names_ej_pctile, 
    ### EJ PCTILE STATE ####
    names_ej_state_pctile,  #  
    
    ## BG AND BLOCK COUNTS ----
    #  # it will use whichever version of name is found
    'statLayerCount',      "bgcount_near_site", # count of blockgroups, as named in API vs in EJAM outputs
    'weightLayerCount', "blockcount_near_site"  # count of blocks, as named in API vs in EJAM outputs     
  )
  }
  useful_column_order <- useful_column_order[useful_column_order %in% names(results_overall)]
  data.table::setcolorder(results_overall, neworder = useful_column_order)
  
  useful_column_order <- useful_column_order[useful_column_order %in% names(results_bysite)]
  data.table::setcolorder(results_bysite, neworder = useful_column_order)
  
  # names_all <- c(
  #   names_other, # includes pop and other denominator counts
  #   names_d,           names_d_pctile,           names_d_state_pctile,           names_d_count, 
  #   names_d_subgroups, names_d_subgroups_pctile, names_d_subgroups_state_pctile, names_d_subgroups_count, 
  #   names_e,           names_e_pctile,           names_e_state_pctile, 
  #   names_ej,          names_ej_pctile,          names_ej_state_pctile 
  # )
  
  ##################################################### #  ##################################################### #  ##################################################### #
  # DONE - Return list of results ####
  
  results <- list(
    results_overall = results_overall, 
    results_bysite = results_bysite
  )
  return(results)
  ##################################################### #  ##################################################### #  ##################################################### #
}


