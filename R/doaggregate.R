#' Summarize indicators in each buffer (given the blocks in each buffer and indicators for each block)
#'
#' @description This updated 2022 code takes a set of facilities and the set of blocks that are near each,
#'   (as identified previously, in other code that has identified which blocks are nearby)
#'   and combines those with indicator scores for block groups.
#'
#'   It aggregates the blockgroup scores to create a summary of each indicator,
#'    in each buffer (i.e., near each facility):
#'      -Sums of counts, such as for population or number of households or Hispanics
#'      -Calculated variables for the buffer via formulas based on aggregated counts, 
#'        such as percent low income.
#'      -Population-Weighted means for EJ Indexes or Environmental indicators.
#'
#' @details
#'  \preformatted{
#'
#'   requires the following as data lazy loaded for example from EJAMblockdata package
#'    blockwts  data.table   with these columns: 
#'       blockid , bgid, blockwt
#'    quaddata, and blockquadtree data.table and quad tree,  for indexes of block points  
#'    
#'    and
#'    
#'    EJAM::blockgroupstats - A data.table (such as EJSCREEN demographic and environmental data by blockgroup?)
#'    
#'    statesshp   (a shapefile of state boundries to determine what state a point is in)
#'    stateregions  data.table lookup of EPA REGION given the ST (state code like AK)
#'  }
#'
#' @param sites2blocks data.table of distances in miles between all sites (facilities) and 
#'   nearby Census block internal points, with columns siteid, blockid, distance,
#'   created by getblocksnearby  function. 
#'   See sites2blocks_example dataset in package, as input to this function
#' @param ... more to pass
#' @import data.table
#' @import EJAMblockdata
#' @export
#'
doaggregate <- function(sites2blocks, countcols=NULL, popmeancols=NULL, calculatedcols=NULL, testing=FALSE, ...) {
  
  # HARDCODED blockgroup dataset, FOR NOW ####
  # including the names of the variables.
  
  ##################################################### #  ##################################################### #
  # Which vars are sum of counts, vs wtd avg, vs via formula ####
  
  if (is.null(countcols)) {
    # # note that names.d.count was not yet defined in ejscreen pkg, and would lack denominators if only based on pctxyz
    # names.d.count <- union( gsub('pct','', grep(pattern = 'pct', ejscreen::names.d, value=TRUE)),
    #  c('unemployed', 'unemployedbase'))
    # names.d.count <- gsub('^min$', 'mins', names.d.count) # since singular was used in pctmin but plural for count is mins
    # countcols <- unique(c('pop', 'nonmins', names.d.count,
    #                "povknownratio",  "age25up", "hhlds",  'pre1960', 'builtunits',
    #                ejscreen::names.d.subgroups.count)) 
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
  } 
  # **** but we probably treat pctpre1960 as pop wtd mean like other Evars?
  
  if (is.null(calculatedcols)) {
    # calculatedcols <- c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged') # use formulas for these
    #  or to avoid depending on ejscreen package, 
    #  dput(c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged')) # but make sure pctunemployed got added
    
    calculatedcols <- c(
      "VSI.eo", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
      "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
      "flagged"
    )
    
    # These must be calculated after aggregating count variables and using those at siteid level. 
    # e.g. Use ejscreen::ejscreenformulas$formula to calculate these.
  }
  if (is.null(popmeancols)) {
    # popmeancols <- c(ejscreen::names.e, ejscreen::names.ej)
    # or to avoid depending on ejscreen package, 
    # dput(c(ejscreen::names.e, ejscreen::names.ej) )
    
    popmeancols <- c(
      "pm", "o3", "cancer", "resp", "dpm", 
      "pctpre1960", "traffic.score", 
      "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", 
      "ust", 
      "EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
      "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
      "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo", 
      "EJ.DISPARITY.ust.eo"
      
      # QUESTION... DO WE CALCULATE THE STATE-FOCUSED EJ INDEXES USING THE STATE AVERAGE DEMOG?? ejscreen was not doing that as of mid 2022 even though it should have been?, 
      # & ASSIGN STATE PERCENTILES OF EJ INDEXES AND THEN ROLL UP IN BUFFER BY FINDING POPWTD MEAN STATE PERCENTILE???
      #  OR DO WE ASSUME THE ENTIRE BUFFER IS MAINLY OR ALL IN ONE STATE AND LOOK UP THE RAW EJ INDEX IN THAT STATE'S LOOKUP TO ASSIGN THE PERCENTILE. THE LATTER, I THINK. 
    )
    
    # ** CHECK THIS:  EJScreen treats pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas... ratio of sums of counts pre1960 and denom builtunits  
    # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop. 
    #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
    # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer? 
    # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer, 
    # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).
  }  
  ##################################################### #  ##################################################### #
  
  # HANDLING DOUBLE COUNTING
  # Some steps are the same for overall and site-by-site so it is more efficient to do both together if want both. 
  
  # SEE NOTES ON HOW TO MAKE AGGREGATE...
  
  # Get the block weights only for the blocks found near these analyzed sites
  # via merge, or possibly via a data.table-specific join that avoids making a copy
  # like blockwts[sites2blocks, ]
  
  # Start Aggregating #############################################################################################
  
  if (testing) {
    # FOR TESTING
    library(data.table); library(EJAMblockdata); data("blockwts")
    data('sites2blocks_example') # it is called  sites2blocks_example
    sites2blocks <- sites2blocks_example
  }
  # data.table::setkey(result, "blockid", "siteid", "distance") #  has been done by getblocksnearby  now
  
  # use blockid, not fips.   *********************  THIS IS SLOW:
  # sites2blocks***** <- merge(EJAMblockdata::blockid2fips, sites2blocks, by='blockfips', all.x=FALSE, all.y=TRUE)
  # sites2blocks$blockid <- EJAMblockdata::blockid2fips[sites2blocks, .(blockid), on='blockfips']
  # sites2blocks[,blockfips := NULL]
  # data.table::setkey(sites2blocks, 'blockid', 'siteid')
  
  # block weights  #######################################
  
  ## get weights for nearby blocks ####
  # sites2blocks <- merge(sites2blocks, blockwts, by='blockid', all.x	=TRUE, all.y=FALSE) # incomparables=NA
  sites2blocks <- EJAMblockdata::blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgid), on='blockid']
  
  ## optional: Calc # of sites nearby each block: #### 
  # How many of these sites are near this resident? this bg? avg resident overall? 
  sites2blocks[, sitecount_near_block := .N, by=blockid] # (must use the table with duplicate blocks, not unique only)
  
  ## *** Unique residents- Overall stats count each block once #### 
  # and thus each person only once even if near 2+ sites, 
  # For each overall resident (blockid) near 1 or more sites, 
  # find and save the info for just the closest single siteid.
  # ***    Just for now, The simplistic way to drop duplicate blocks (even if other columns are not duplicates), 
  #    which are residents near 2 or more sites, to avoid double-counting them, is this:
  sites2blocks_overall <- unique(sites2blocks, by="blockid")  # ???
  
  ## HOW TO GET AVG OR MIN DISTANCE ETC? ####
  # But note the unique() would just pick the first instance found of a given blockid, regardless of which siteid that was for,
  # so it retains whatever distance happened to be the one found first, and same for all site characteristics.
  # *** We actually instead want to save the shortest distance for that blockid, as the worst case proximity
  # to be able to have summary stats by group of distance to the closest site, not to a random site among those nearby. 
  # What is the efficient way to find that?
  
  data.table::setorder(sites2blocks, siteid, bgid, blockid) # new
  
  # block groups   #######################################
  ## Avg person's proximity (block-level), by bg: #### 
  #  censuspop-wtd mean of block-specific distances, for each bg 
  sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt), by=bgid]  
  sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt), by=c('bgid', 'siteid')]
  
  ##################################################### #
  ## check this ####
  # was going to try to do join of weights and the aggregation by blockid all in this one step? but not tested 
  # and may need to do intermed step 1st, where 
  # sites2bg <- blockwts[sites2blocks, .(siteid, bgid, distance, bgwt = sum(blockwt, na.rm=TRUE)), on = 'blockid', by =.(siteid, bgid)] 
  #
  ## why do sum(blockwt) by bgid  here AGAIN, if already did it above?
  # rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??
  
  ## rollup as blockgroups - Aggregate blocks into blockgroups, per siteid ***  #######################################
  
  ## Calc bgwt, the fraction of each (parent)blockgroup's censuspop that is in buffer #### 
  sites2bgs_overall <- sites2blocks_overall[ , .(bgwt = sum(blockwt)), by=bgid ]
  sites2bgs_bysite  <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), by=.(siteid, bgid)]
  
  ## Count # unique sites near each bg #### 
  sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by=bgid] 
  
  # Count # blocks or bgs near each site ####
  blockcount_by_site <- sites2blocks[, .(blockcount_near_site = .N), by=siteid] # new----------------------------------------------- -
  bgcount_by_site <- sites2blocks[, .(bgcount_near_site = length(unique(bgid))), by=siteid] # new------------------------------------ -
  count_of_blocks_near_multiple_sites <- (NROW(sites2blocks) - NROW(sites2blocks_overall)) # NEW fraction is over /NROW(sites2blocks_overall)
  # how many blockgroups here were found near 1, 2, or 3 sites? 
  # e.g., 6k bg were near only 1/100 sites tested, 619 near 2, 76 bg had 3 of the 100 sites nearby.
  # table(table(sites2bgs_bysite$bgid))
  
  ##################################################### #
  # HOW TO GET MORE STATS ON DISTRIBUTION OF DISTANCES OR ENVT, BY GROUP? ####
  #
  # considered removing sites2blocks and sites2blocks_overall NOW TO FREE UP RAM, BUT THAT IS slow!:
  if (!testing) {rm(sites2blocks); gc() }
  # And need save that to analyze distance distribution!
  #  Maybe want some extra summary stats across people and sites (about the distribution), one column per indicator. 
  # *****  BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP, 
  #  HAVE TO BE CALCULATED FROM BG DATA BEFORE WE AGGREGATE WITHIN EACH SITE (BUFFER)... 
  #  Same for sites: worst site as measured by highest nearby blockgroup-level %poor needs raw bg data before summarized by siteid.
  
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
  rm(sites2bgs_overall, sites2bgs_bysite)
  rm(blockgroupstats)
  
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
  results_overall_popmeans <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), .SDcols = popmeancols ]
  results_overall <- cbind(results_overall, results_overall_popmeans)
  # cbind(sum = prettyNum(results_overall, big.mark = ','))
  
  # POP wtd MEAN BY SITE ####
  results_bysite_popmeans <- sites2bgs_plusblockgroupdate_bysite[ ,  lapply(.SD, FUN = function(x) stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), by = .(siteid), .SDcols = popmeancols ]
  results_bysite <- merge(results_bysite, results_bysite_popmeans)
  results_bysite <- merge(results_bysite, blockcount_by_site) # new ---------------------------------------------- -
  results_bysite <- merge(results_bysite, bgcount_by_site) # new ---------------------------------------------- -
  
  # save.image('~/R/mypackages/EJAM/inst/doagg2 so far just before calculated vars made.rda')
  rm(results_overall_popmeans, sites2bgs_plusblockgroupdate_overall)
  rm(results_bysite_popmeans,  sites2bgs_plusblockgroupdate_bysite)
  
  ##################################################### #
  # and/or if some variables have to be calculated using formulas, 
  # can do that using the list of formulas and this function: ... 
  ##################################################### #
  
  # CALC via FORMULAS with Rolled up Counts #### 
  # this was meant to handle multiple columns (formula for each new one) for many rows (and here in buffer results, one site is a row, not one blockgroup) 
  
  # "nonmins <- nhwa"
  # "mins <- pop - nhwa" 
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
  results_overall[ , `:=`(
    VSI.eo = (pctlowinc + pctmin) / 2
  )]
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
  results_bysite[ , `:=`(
    VSI.eo = (pctlowinc + pctmin) / 2
  )]
  ##################################################### #  ##################################################### #  ##################################################### #
  
  # VSI.eo.US = sum(mins) / sum(pop)  +  sum(lowinc) / sum(povknownratio) ) / 2, 
  # VNI.eo = VSI.eo * pop, 
  # VSI.eo = (pctlowinc + pctmin) / 2, 
  #  or is it treated as envt var and just pop mean?  ********************
  # Demog.Index <- stats::weighted.mean(Demog.Index, w = pop)
  # Demog.Index <- VSI.eo
  
  # # to be replaced with data available to this package
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
  # BUT PROBABLY SHOULD REMOVE THIS percentiles code TO A SEPARATE FUNCTION !   ##################################################### #
  #  VIA  lookup tables of US/State  percentiles
  ##################################################### #
  
  # results_bysite
  # results_overall
  
  # Use the dataset called EJAM::usastats as the lookup table for USA percentiles and mean. 
  # but update/ fix it so it uses right variable names, etc., or replace with the one from ejscreen pkg
  
  # hard coded for now:
  # varsneedpctiles <- c(ejscreen::names.e, union(ejscreen::names.d, 'pctunemployed'), ejscreen::names.d.subgroups, ejscreen::names.ej)
  varsneedpctiles <- c(names_e, union( names_d, 'pctunemployed'),  names_d_subgroups, names_ej)
  varnames.us.pctile <- paste0('pctile.', varsneedpctiles)
  varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)
  
  us.pctile.cols_bysite     <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite)     <- varnames.us.pctile
  state.pctile.cols_bysite  <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite)  <- varnames.state.pctile
  us.pctile.cols_overall    <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall)    <- varnames.us.pctile
  state.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_overall) <- varnames.state.pctile
  
  for (i in seq_along(varsneedpctiles)) {
    # using EJAM::usastats not ejscreen::lookupUSA now but older code 2015 had used  EJAM::lookup.pctile.US(results_bysite[ , myvar], .....  i=1
    myvar <- varsneedpctiles[i]
    if (myvar %in% names(usastats)) { # just like ejscreen::lookupUSA , and EJAM::lookup.pctile.US is like ejanalysis::lookup.pctile()
      us.pctile.cols_bysite[ , varnames.us.pctile[[i]]] <-  lookup.pctile.US(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
      us.pctile.cols_overall[, varnames.us.pctile[[i]]] <-  lookup.pctile.US(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
      # state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- lookup.pctile.US(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats, zone =  results_bysite$ST) 
      # It may not make sense to do state percentiles except for EJ Indexes, in the "overall" summary:
      # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- lookup.pctile.US(results_overall[ , varsneedpctiles[i]], varname.in.lookup.table = varsneedpctiles[i], lookup = statestats, zone =  results_overall$ST)
    } else {
      us.pctile.cols_bysite[ , varnames.us.pctile[[i]]] <- NA
      us.pctile.cols_overall[, varnames.us.pctile[[i]]] <- NA
    }
  }
  
  # setdiff(varsneedpctiles, names(usastats)) # like ejscreen::lookupUSA
  # [1] "ust"                 "pctunemployed"      "pctnhwa"             "pcthisp"            
  # [5] "pctnhba"             "pctnhaa"             "pctnhaiana"          "pctnhnhpia"         
  # [9] "pctnhotheralone"     "pctnhmulti"          "EJ.DISPARITY.ust.eo"
  
  results_overall <- cbind(siteid=NA, results_overall, us.pctile.cols_overall, state.pctile.cols_overall)
  results_bysite  <- cbind(results_bysite,  us.pctile.cols_bysite,  state.pctile.cols_bysite)
  
  ##################################################### #  ##################################################### #  ##################################################### #
  # DONE - Return list of results ####
  
  results <- list(results_overall = results_overall, results_bysite = results_bysite)
  return(results)
  ##################################################### #  ##################################################### #  ##################################################### #
}
  
  
