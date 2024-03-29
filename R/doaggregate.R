#' Summarize environmental and demographic indicators at each location and overall
#'
#' @description getblocksnearby() and doaggregate() are the two key functions that run ejamit().
#'   `doaggregate()` takes a set of sites like facilities and the
#'   set of blocks that are near each,
#'   combines those with indicator scores for block groups, and
#'   aggregates the numbes within each place and across all overall.
#'
#' @details
#'   For all examples, see [getblocksnearbyviaQuadTree()]
#'
#'   `doaggregate()` is the code run after [getblocksnearby()] (or a related function for
#'   polygons or FIPS Census units) has identified which blocks are nearby.
#'
#'   `doaggregate()` aggregates the blockgroup scores to create a summary of each indicator,
#'    as a raw score and US percentile and State percentile,
#'    in each buffer (i.e., near each facility):
#'
#'    - **SUMS OF COUNTS**: for population count, or number of households or Hispanics, etc.
#'
#'    - **POPULATION-WEIGHTED MEANS**: for  Environmental indicators, but also any percentage indicator
#'      for which the universe (denominator) is population count (rather than households, persons age 25up, etc.)
#'
#'        ***EJ Indexes**:* The way EJScreen
#'          does this is apparently finding the pop wtd mean of EJ Index raw scores,
#'          not the EJ Index formula applied to the summarized demographic score and aggregated envt number.
#'
#'    - **CALCULATED BY FORMULA**: Buffer or overall score calculated via formulas using aggregated counts,
#'          such as percent low income = sum of counts low income / sum of counts of denominator,
#'          which in this case is the count of those for whom the poverty ratio is known. Assuming no rounding errors,
#'          this method should give the same result as using a weighted mean of percentages, where the weights are
#'          the correct denominator like count of those for whom the poverty ratio is known.
#'
#'    - **LOOKED UP**: Aggregated scores are converted into percentile terms via lookup tables (US or State version).
#'
#'   This function requires the following datasets:
#'
#'    - blockwts: data.table with these columns: blockid , bgid, blockwt
#'
#'    - quaddata data.table used to create localtree, a quad tree index of block points
#'      (and localtree that is created when package is loaded)
#'
#'    - blockgroupstats - A data.table (such as EJScreen demographic and environmental data by blockgroup?)
#'
#' @param sites2blocks data.table of distances in miles between all sites (facilities) and
#'   nearby Census block internal points, with columns ejam_uniq_id, blockid, distance,
#'   created by getblocksnearby  function.
#'   See [sites2blocks_example10pts_1miles] aka [testoutput_getblocksnearby_10pts_1miles] dataset in package, as input to this function
#' @param sites2states_or_latlon data.table or just data.frame, with columns ejam_uniq_id (each unique one in sites2blocks) and ST (2-character State abbreviation) or lat and lon
#' @param radius Optional radius in miles to limit analysis to. By default this function uses
#'   all the distances that were provided in the output of getblocksnearby(),
#'   and reports radius estimated as rounded max of distance values in inputs to doaggregate.
#'   But there may be cases where you want to run getblocksnearby() once for 10 miles, say,
#'   on a very long list of sites (1,000 or more, say), and then get summary results for
#'   1, 3, 5, and 10 miles without having to redo the getblocksnearby() part for each radius.
#'   This lets you just run getblocksnearby() once for the largest radius, and then query those
#'   results to get doaggregate() to summarize at any distance that is less than or equal to the
#'   original radius analyzed by getblocksnearby().
#' @param countcols character vector of names of variables  to aggregate within a buffer
#'   using a sum of counts, like, for example, the number of people for whom a
#'   poverty ratio is known, the count of which is the exact denominator needed
#'   to correctly calculate percent low income.
#' @param popmeancols character vector of names of variables to aggregate within a buffer
#'   using population weighted mean.
#' @param calculatedcols character vector of names of variables to aggregate within a buffer
#'   using formulas that have to be specified.
#' @param subgroups_type Optional (uses default). Set this to
#'   "nh" for non-hispanic race subgroups as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh;
#'   "alone" for EJScreen v2.2 style race subgroups as in    White Alone, wa and others in names_d_subgroups_alone;
#'   "both" for both versions. Possibly another option is "original" or "default" but work in progress.
#' @param include_ejindexes whether to calculate EJ Indexes and return that information
#' @param calculate_ratios whether to calculate and return ratio of each indicator to its US and State overall mean
#' @param extra_demog if should include more indicators from EJScreen v2.2 report,
#'    on language, more age groups, gender, percent with disability, poverty, etc.
#' @param need_proximityscore whether to calculate proximity scores
#' @param infer_sitepoints set to TRUE to try to infer the lat,lon of each site around which the blocks in sites2blocks were found.
#'   lat,lon of each site will be approximated as average of nearby blocks, although a more accurate slower way would
#'   be to use reported distance of each of 3 of the furthest block points and triangulate
#' @param called_by_ejamit Set to TRUE by ejamit() to suppress some outputs even if ejamit(silentinteractive=F)
#' @param updateProgress progress bar function used for shiny app
#' @param silentinteractive Set to TRUE to see results in RStudio console.
#'   Set to FALSE to prevent long output showing in console in RStudio when in interactive mode
#' @param testing used while testing this function
#' @param ... more to pass to another function? Not used currently.
#' @seealso [ejamit]   [getblocksnearby()]
#'
#' @return list with named elements:
#'
#'   * **`results_overall`**   one row data.table, like results_by_site, but just one row with
#'     aggregated results for all unique residents.
#'
#'   * **`results_by_site`**   results for individual sites (buffers) - a data.table of results,
#'     one row per ejam_uniq_id, one column per indicator
#'
#'   * **results_bybg_people**  results for each block group, to allow for showing the distribution of each
#'      indicator across everyone within each demographic group.
#'
#'   * **longnames**  descriptive long names for the indicators in the above outputs
#'
#'   * **count_of_blocks_near_multiple_sites**  additional detail
#'
#' @import data.table
#'
#' @export
#'
doaggregate <- function(sites2blocks, sites2states_or_latlon=NA,
                        radius=NULL,
                        countcols=NULL, popmeancols=NULL, calculatedcols=NULL, subgroups_type='nh',
                        include_ejindexes=FALSE, calculate_ratios = TRUE,
                        extra_demog=TRUE, need_proximityscore=FALSE,
                        infer_sitepoints=FALSE,
                        called_by_ejamit=FALSE, updateProgress = NULL,
                        silentinteractive=TRUE, testing=FALSE,
                         ...) {

  ###################################################### #

  # ERROR CHECK/ VALIDATE INPUTS ####

  if (include_ejindexes & !exists("bgej")) {
    dataload_from_pins('bgej') # load it on demand when needed
    # if failed to find it, give up
    if (!exists("bgej")) {
      warning("include_ejindexes was set to TRUE but the (very large) bgej file was not found, so EJ Indexes will not be returned")
      include_ejindexes <- FALSE
    } else {
      message('loaded bgej data because include_ejindexes = TRUE')
    }
  }
  if (include_ejindexes) {
    ejnames_raw <- c(names_ej, names_ej_supp, names_ej_state, names_ej_supp_state)
  }

  # But note that names_d_subgroups and related lists should already be defined in built package
  # as either the nh versions or alone versions by the datacreate_names_of_indicators.R script
  # and or in map_headernames metadata
  #
  subs_count = switch(subgroups_type,
                      alone    = names_d_subgroups_alone_count,
                      nh       = names_d_subgroups_nh_count,
                      both     = c(names_d_subgroups_alone_count, names_d_subgroups_nh_count),
                      original = names_d_subgroups_count)
  subs = switch(subgroups_type,
                alone    = names_d_subgroups_alone,
                nh       = names_d_subgroups_nh,
                both     = c(names_d_subgroups_alone, names_d_subgroups_nh),
                original = names_d_subgroups)
  # also see code below that starts with   names_these <-

  # timed <- system.time({

  # check if optional input params, when provided, are all valid ***



  if (NROW(sites2blocks) == 0){
    #if (shiny::isRunning()) {
      warning('No blocks found within that radius of your site(s). Try a larger radius')
      return(NULL)
    #} else{
    #  stop('No blocks found within that radius of your site(s). Try a larger radius')

    #}
    }

  # add input validation here - check if sites2blocks is valid format, etc.

  if (any(!(c('ejam_uniq_id', 'blockid' ) %in% names(sites2blocks)))) {
    #if (shiny::isRunning()) {
      warning("sites2blocks must contain columns named ejam_uniq_id, blockid, and should have distance")
      return(NULL)
    #} else {
    #  stop("sites2blocks must contain columns named ejam_uniq_id, blockid, and should have distance")
    #}
  }

  if (!data.table::is.data.table(sites2blocks)) {
    message('sites2blocks should be a data.table - converting into one')

    data.table::setDT(sites2blocks, key = c("blockid", "ejam_uniq_id", "distance"))
  }


  ###################################################### #

  ## RADIUS VALIDATE/ CHECK/ ADJUST ####

  #  Try to clean and/or infer and/or limit what the radius was meant to be or will be limited to for reporting here
  # *** revisit this section - if user picks radius < max getblocksnearby() reports, should we also restrict reported and filtered radius to the inferred radius??

  if (!("distance" %in% names(sites2blocks))) {
    warning("distance should be a column in sites2blocks passed to doaggregate but was missing, so distances set to zero")
    sites2blocks$distance <- 0 # just to have a value but not sure results make sense in this unlikely case except if using polygons or fips and somehow getblocksnearby_from_fips() failed to add distance = 0 as a column
  }
  if (all(sites2blocks$distance == 0)) {
    # seems like they must have used getblocksnearby_from_fips() to do query on block points within certain FIPS or polygons, not circular buffers using radius
    # so set radius here to 0 , and anyway it will not restrict analysis to distances <= any particular radius
    # Earlier step, in getblocksnearbyviaQuadTree, it would have adjusted small distances based on effective radius of block.
    # But here a distance of zero is OK, since we are modeling simple presence of people in a zone (a block, etc.), without proximity to any site point.
    # Some stats or plots analyzing proximity (i.e., 1/distance) will not work, but they are not supposed to work for an analysis where distance is not an issue.
    radius <- 0
  } else {
    if (is.null(radius)) {radius <- NA}
    if ( missing(radius) | is.na(radius) | (length(radius) != 1) | (!is.numeric(radius)) | (radius < 0)  ) {
      if (!is.numeric(sites2blocks$distance)) {
        warning('Values found in sites2blocks$distance were not but must be numeric - doaggregate() will treat them as zero values')
        radius <- 0
      } else {
        warning('radius passed to doaggregate() must be a single number, in miles, at least 0, but was not, so now
                inferring radius based on sites2blocks distances.')
       radius <- radius_inferred(sites2blocks)
       message('Inferring approximate radius is ', radius, ' miles, based on distances found.')
      }
    }

      if (radius >= 1.5 * max(sites2blocks$distance)) {
        warning('radius passed to doaggregate() is at least 1.5x any distance found in sites2blocks,
                suggesting it is larger than the radius that was analyzed by getblocksnearby() --
                changing the reported radius now to be the inferred radius')
        radius <- radius_inferred(sites2blocks)
      }

      if (any(sites2blocks$distance > radius)) {message(paste0(
        "Restricting this analysis to blocks (residents) at distances smaller than radius of ", radius, "\n",
"as specified in radius parameter passed to doaggregate(), or else inferred from distances reported to doaggregate()\n",
"even though some larger distances were found in sites2blocks table passed from getblocksnearby() to doaggregate()\n",
"which sometimes occurs if small radius is used where blocks are very large (low pop density)\n",
"so reported distance to avg person was > radius requested for analysis"))}
     # only reporting results for residents at distances <= that apparent cutoff in distance even if large block led to getblocksnearby() reporting distance > radius!')
    # *** note this may eliminate from analysis a site that is in a very large rural block if there are no other blocks nearby and the block very large
      # -- see notes elsewhere

      sites2blocks <- sites2blocks[distance <= radius, ]

      # is sites2blocks already keying on distance? that would speed it up! ***
      # and, can this subset rows be done faster by reference in data.table somehow?/avoiding copy?
      # maybe something similar to this:  ???
    # sites2blocks[distance > radius, .SD := NULL]  #???

  }
  # end of radius adjustments
  ###################################################### #


  ##################################################### #  ##################################################### #
  # HARDCODED blockgroup indicator names and formulas, FOR NOW, but... ####
  #
  # This function could either take as input params these:
  #
  # - lists of variable names and a data.frame (like now),
  #   or
  # - lists of the actual indicator values,
  #   or
  # - it could even be given a table of scores and a table of what to do with each indicator (their names,
  #   formulas for them, etc. LIKE IN map_headernames )
  # for each type of indicator (countcols vs popmeancols, etc.),
  ##################################################### #

  ## Specify Which vars are SUM OF COUNTS, vs WTD AVG, vs via FORMULA (usually ratio of sums of counts) ####
  # That info is sort of stored already as one of unique(map_headernames$calculation_type)
  # map_headernames[map_headernames$calculation_type == "popwtd mean",     c('varlist', "rname") ]
  # map_headernames[map_headernames$calculation_type == "percent formula", c('varlist', "rname") ]
  # map_headernames$rname[grepl("denom", map_headernames$names_friendly, ignore.case = TRUE)] # [1] "unemployedbase" "builtunits"
  #
  # see pdf documenting denominator (weights) for each indicator aggregated via weighted average.

  if (is.null(countcols)) {
    countcols <- unique(c(
      names_d_other_count,
      names_d_count,
      subs_count
        #
    ))
  if  (extra_demog) {
    countcols <-  c(countcols, c('LAN_UNIVERSE', 'LAN_SPANISH', 'LAN_ENG_NA', 'LAN_IE', 'LAN_API',
      'HLI_SPANISH_LI', 'HLI_IE_LI',  'HLI_API_LI', 'HLI_OTHER_LI',
      'AGE_LT18', 'AGE_GT17', 'MALES', 'FEMALES', 'OWNHU',  'OCCHU',
      'DISAB_UNIVERSE', 'DISABILITY', 'HH_BPOV'))
  }
  }
  if (is.null(calculatedcols)) {
    calculatedcols <- unique(c(
      names_d[names_d != 'lowlifex'],      #   "lowlifex"(use popwtd mean)   "Demog.Index.Supp",  # already in names_d
      subs,
      'flagged'

      #  xxx

    ))
  }

  # DEFAULT COLUMNS TO AGGREGATE VIA POPULATION WEIGHTED AVERAGE OF PARTIAL BLOCK GROUPS IN EACH PLACE

  if (is.null(popmeancols)) {
    popmeancols <- unique(c(
      'lowlifex',  # I think it is just pop wtd mean  - not completely sure it should be via popwtd mean, or calculated via formula actually.
      names_e,

        'percapincome',
        'lifexyears'
    ))
    if (include_ejindexes) {
      popmeancols <- c(popmeancols, ejnames_raw)
    }
  }

  # notes on formulas:
  #
  # ** CHECK THIS:  EJScreen treats pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas. . . ratio of sums of counts pre1960 and denom builtunits
  # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop.
  #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
  # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer?
  # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer,
  # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).

  # see ejscreen package file ejscreenformulas$formula to help calculate
  # calculatedcols <- c(ejscreen package file names.d, ejscreen package file names.d.subgroups, 'flagged') # use formulas for these
  # but avoid depending on ejscreen package,



  ##################################################### #
  ## ...update progress bar in shiny app ####
  if (is.function(updateProgress)) {
    boldtext <- paste0('Calculating indicators at each site and overall')
    updateProgress(message_main = boldtext, value = 0.2)
  }
  ##################################################### #

  #____________________________________________________   #  ##################################################### #  ######################################################


  # ____AGGREGATE by BLOCK across sites #############################################################################################


  ################################################################ #
  # FIRST, PREPARE TO AGGREGATE BY BLOCK

  ## Use pop weights of nearby blocks ####
  # to track what fraction of each parent block group is considered inside the buffer.
  #    getblocksnearby() already did join that added blockwt column
  # and block_radius_miles was already used to adjust short distances in sites2blocks.

  # sort rows


  data.table::setorder(sites2blocks, ejam_uniq_id, bgid, blockid) # new


  ################################################################ #
  # Just create some new columns in sites2blocks,
  #    by="blockid" here already

  # Using                 DT[, newcolumn := min(xyz), by="blockid"] creates new column in existing DT, with repeat of the same info in each row for duplicate blockids, which is ok. Typically not a large % are duplicated so it is not much slower, and dupes are removed later for overall stats.
  # Using  rolledup_DT <- DT[, summarycol = sum(xyz), by="blockid"] creates a new DT with fewer rows, by summarizing over the 1-2 sites near a given block.

  # >>>>> A VERY VERY SLOW STEP TO OPTIMIZE ####
  ##  SLOWEST STEP -- THIS TAKES ABOUT HALF THE TOTAL TIME OF ALL doaggregate() ****

  ## _sitecount for each block (ie each resident) ####
  ## _min distance to any site, for each block (each resident's distance from the nearest site) ####

  if (need_proximityscore) {

    ## _Proximity Score of block   ####
    # Note the distance was already adjusted to be the minimum possible value of 0.9 * effective radius of block_radius_miles, in getblocksnearbyviaQuadTree()

    sites2blocks[, `:=`(
      proximityscore =  1 / distance,  # score here is for only 1 site per block. summed later across all sites near a given block, then get popwtd mean of block prox scores.
      sitecount = .N,   # done again below, right?
      # How far is closest site, for each unique block (resident, essentially, or actually avg resident in the block)?
      # distance_min = collapse::fmin(distance) #,
      distance_min = distance[1]   # temporarily use first distance among sites near this block to see if essential and how much does this slow it down?
    ),
    by = "blockid"]

    if (anyNA(sites2blocks$proximityscore)) {message("Proximity scores were requested but set to Inf where distance=0 as when analyzing unbuffered polygons or FIPS")}

  } else {

    sites2blocks[, `:=`(
      sitecount = .N,  # done again below, right?
      # How far is closest site, for each unique block (resident, essentially, or actually avg resident in the block)?
      # distance_min = collapse::fmin(distance) #,
      distance_min = distance[1] # temporarily use first distance among sites near this block to see if essential and how much does this slow it down?
    ),
    by = "blockid"]
  }
  ################################################################ #

  ###################################### #
  ## * Unique residents (blocks) only, used for Overall stats ####
  ###################################### #
  # each block only once, and therefore each person only once even if near 2+ sites.
  # For each overall resident (blockid) near 1 or more sites,
  # find and save the info for just the closest single ejam_uniq_id (distance_min)
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
                                               distance_min = distance_min[1],  # already did min or else do only here but not both
                                               sitecount = .N  # typically some blocks are near 2 or more sites in sites2blocks
                                               # sitecount_avg = .N
    ),
    by = "blockid"]

  } else {

    sites2blocks_overall <- sites2blocks[ ,  .(bgid = bgid[1],  # otherwise it retains duplicate rows, same block twice if it is near 2!
                                               blockwt = blockwt[1],

                                               #     proximityscore = proximityscore[1], # sum(proximityscore, na.rm = TRUE), # already did sum over all sites near a block.
                                               # distance_avg = stats::weighted.mean(distance, w = blockwt, na.rm = TRUE),
                                               distance_min = distance_min[1],  # already did min or else do only here but not both
                                               sitecount = .N  # typically some blocks are near 2 or more sites in sites2blocks
                                               # sitecount_avg = .N
    ),
    by = "blockid"]
  }

  #  length(testoutput_getblocksnearby_10pts_1miles$blockid)
  # [1] 11567
  #  length(unique(testoutput_getblocksnearby_10pts_1miles$blockid))
  # [1] 11334

  #***  ###################################### #
  ## ...update progress bar in shiny app ####
  if (is.function(updateProgress)) {
    boldtext <- paste0('Analyzing blockgroups')
    updateProgress(message_main = boldtext, value = 0.4)
  }
  #***  ###################################### #
  #____________________________________________________   #  ##################################################### #  ######################################################

  ##################################################### #  ##################################################### #  ##################################################### #


  ##################################################### #

  # ___AGGREGATE by BG, the Distances and Sitecounts___ ######


  ##################################################### #
  # How to get Distrib and avg in each Demog group, ####
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
  # sites2bg <- blockwts[sites2blocks, .(ejam_uniq_id, bgid, distance, bgwt = sum(blockwt, na.rm=TRUE)), on = 'blockid', by =.(ejam_uniq_id, bgid)]
  #
  ## why do sum(blockwt) by bgid  here AGAIN, if already did it above?
  # rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??
  ## ########## #
  #
  ## *?? ______WHICH OF THESE VERSIONS WAS BETTER? BY REFERENCE should be faster
  #
  # sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt),     by="bgid"]  # variable not used !
  # sites2blocks[        , bg_fraction_in_buffer_bysite  := sum(blockwt, na.rm = TRUE), by=c("ejam_uniq_id", "bgid")]
  #
  # VERSUS this below...
  #
  ##################################################### #

  # Distances, Proximity scores, and Site counts nearby ####
  #  >>>> A *VERY* SLOW STEP TO OPTIMIZE  - sites2bgs_bysite   <- sites2blocks[   ####
  if (need_proximityscore) {
    warning('proximityscore lacks small distance adjustment factor - not yet implemented')

    sites2bgs_bysite   <- sites2blocks[         , .(bgwt = sum(blockwt, na.rm = TRUE),     # 550 msec ??

                                                    proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min   = collapse::fmin(distance),  # already did min or else do only here but not both
                                                    distance_min_avgperson   = collapse::fmean(distance, w = blockwt), # na.rm = T is default
                                                    sitecount_avg    = collapse::fmean(sitecount, w = blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fnunique(ejam_uniq_id)
    ), by = .(ejam_uniq_id, bgid)]
    #  >>>> A SLOW STEP TO OPTIMIZE - starts with sites2bgs_overall  <- sites2blocks_overall[  ####
    # THIS IS REDUNDANT / inefficient   since we have sites2bg_bysite and bgid already, ?! ***
    # and it will get rolled up by ejam_uniq_id only and by bg only, later

    sites2bgs_overall  <- sites2blocks_overall[ , .(bgwt = sum(blockwt, na.rm = TRUE),    # 318 msec

                                                    proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min = collapse::fmin(distance_min), # already did min or else do only here but not both, or get from _bysite which would be faster?
                                                    distance_min_avgperson = collapse::fmean(distance_min, w = blockwt),
                                                    sitecount_avg  =  collapse::fmean(sitecount,  w = blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fmax(sitecount) # this is an underestimate - TO BE FIXED LATER
    ), by =         "bgid" ]
  } else {
    sites2bgs_bysite   <- sites2blocks[         , .(bgwt = sum(blockwt, na.rm = TRUE),     # 550 msec ??

                                                    # proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min     = collapse::fmin(distance),
                                                    distance_min_avgperson   = collapse::fmean(distance, w = blockwt), # na.rm = T is default
                                                    sitecount_avg    = collapse::fmean(sitecount, w = blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fnunique(ejam_uniq_id)
    ), by = .(ejam_uniq_id, bgid)]

    #  >>>> A SLOW STEP TO OPTIMIZE - starts with sites2bgs_overall  <- sites2blocks_overall[  ####
    # is this redundant since we have sites2bg_bysite and bgid already, and it will get rolled up by ejam_uniq_id only and by bg only, later
    sites2bgs_overall  <- sites2blocks_overall[ , .(bgwt = sum(blockwt, na.rm = TRUE),    # 318 msec

                                                    # proximityscore = collapse::fmean(proximityscore,   w = blockwt),
                                                    distance_min = collapse::fmin(distance_min),
                                                    distance_min_avgperson = collapse::fmean(distance_min, w = blockwt),
                                                    sitecount_avg  =  collapse::fmean(sitecount,  w = blockwt),
                                                    sitecount_max    = collapse::fmax(sitecount ),
                                                    sitecount_unique = collapse::fmax(sitecount) # this is an underestimate - TO BE FIXED LATER
    ), by =         "bgid" ]
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
  # ?distance_avg??  = not really a useful metric? maybe useful to calc avg distance for each demog.
  #   we care about 1 closest, not avg distance to all nearby.
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
  # joins midsized intermed table of sites and BGs to EJScreen/ blockgroupstats . . . sites2bgs_overall ??
  ##################################################### #
  #
  # DO JOIN  OF **blockgroupstats**   200 columns, on bgid ,

  # and not sure I can calculate results at same time, since this kind of join is getting a subset of blockgroupstats but grouping by sites2bgs_bysite$ejam_uniq_id  and
  # maybe cannot use blockgroupstats[sites2bgs_bysite,    by=.(ejam_uniq_id)    since ejam_uniq_id is in sites2bgs_bysite not in blockgroupstats table.
  # So, first join blockgroupstats necessary variables to the shorter sites2bgs_bysite:

  if (include_ejindexes) { # was already set to FALSE if bgej not available
    #blockgroupstats <- merge(blockgroupstats, bgej, by=c('OBJECTID','bgfips','ST','pop','bgid'))
    setDT(bgej)
    blockgroupstats <- merge(blockgroupstats,  bgej[!is.na(bgid), c(
      "bgid", ejnames_raw
    ), with = FALSE], by = "bgid")
    rm(bgej)
  }
  #   Remember that. . .
  # countcols     # like population count, add up within a buffer
  # popmeancols    # we want average persons raw score,  for Environmental (but maybe avg PERCENTILE for EJ indexes ??)
  # calculatedcols  # use formulas for these, like  sum of counts of lowincome divided by sum of counts of those with known poverty ratio (universe)
  countcols_inbgstats      <- intersect(countcols,      names(blockgroupstats))
  popmeancols_inbgstats    <- intersect(popmeancols,    names(blockgroupstats))
  calculatedcols_inbgstats <- intersect(calculatedcols, names(blockgroupstats))

  sites2bgs_plusblockgroupdata_bysite  <- merge(sites2bgs_bysite,  #  but has other cols like   "distance_avg" , "proximityscore"  etc.
                                                blockgroupstats[ , c('bgid', 'ST', ..countcols_inbgstats, ..popmeancols_inbgstats, ..calculatedcols_inbgstats)],
                                                all.x = TRUE, all.y = FALSE, by = 'bgid')


  # just be aware that this is not saving just unique blockgroups, but saves each bgid-ejam_uniq_id pairing???

  sites2bgs_plusblockgroupdata_overall <- merge(sites2bgs_overall,
                                                blockgroupstats[ , c('bgid',       ..countcols_inbgstats, ..popmeancols_inbgstats, ..calculatedcols_inbgstats)],
                                                all.x = TRUE, all.y = FALSE, by = 'bgid')
  # rm(sites2bgs_overall, sites2bgs_bysite); rm(blockgroupstats)


  ##################################################### #  ##################################################### #


  ######################### #
  ## ...update progress bar in shiny app ####
  if (is.function(updateProgress)) {
    boldtext <- paste0('Joining blockgroups to EJScreen indicators')
    updateProgress(message_main = boldtext,
                   value = 0.6)
  }
  ######################### #
  #____________________________________________________ #  ######################################################


  # ___AGGREGATE by SITE, the Indicators ___ ####


  ##################################################### #

  # * SUM OF COUNTS for each count indicator at EACH SITE and OVERALL ####

  ##################################################### #

  ##  Counts Overall (all sites/ whole sector)  ###

  results_overall <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)
  } ), .SDcols = countcols_inbgstats ]

  # to be sum of unique, BY SITE, TO RETURN: blockcount_by_site, bgcount_by_site,

  # others for OVERALL:  count_of_blocks_near_multiple_sites, blockcount_overall, bgcount_overall


  ## results_bysite  Counts by site/facility  ###

  results_bysite <- sites2bgs_plusblockgroupdata_bysite[ ,    lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)

  } ), .SDcols = countcols_inbgstats, by = .(ejam_uniq_id) ]

  # results_bysite[1:100,1:8]
  # cbind(sum = prettyNum(results_overall,big.mark = ','))
  # versus if you did not remove duplicate places/people:
  # sites2bgs_plusblockgroupdata_bysite[ ,  .(sums = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), .SDcols = countcols_inbgstats][1,]
  # 1: 9,978,123
  # but sum(outapi_3mile_100sites$pop[outapi_3mile_100sites$statename != 'PUERTO RICO' ])
  # [1] 10,022,946


  ##################################################### #
  # * POP WTD MEAN for some indicators ####
  # ( ENVT, and if include_ejindexes=TRUE, the EJ indexes too )
  ##################################################### #
  #  >    >>> A bit SLOW - TO OPTIMIZE **WEIGHTED.MEAN   ####
  ## mean by SITE ###
  results_bysite_popmeans <- sites2bgs_plusblockgroupdata_bysite[   ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop)   # stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)    # 100 msec

   }), .SDcols = popmeancols_inbgstats, by = .(ejam_uniq_id) ]
# redo   in data.table:: style, for speed? this is just by site so only 1 row per site is not that many usually, but is a lot of columns (200?) *********************************
  results_bysite <- merge(results_bysite, results_bysite_popmeans, by = "ejam_uniq_id") # dont we need by = "ejam_uniq_id" or just to be clear?  It defaults to merging by the shared key columns between the two tables. If y has no key columns, this defaults to the key of x.

  ## mean OVERALL ###
  ## later, for results_overall, will calc state pctiles once we have them for each site

  results_overall_popmeans <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop) # stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)
  }), .SDcols = popmeancols_inbgstats  ]
  results_overall <- cbind(results_overall, results_overall_popmeans) # many columns (the popwtd mean cols)

  ##################################################### #
  # * MIN or MAX distance or sitecount ####
  ##################################################### #

  # this is actually not used currently:
  # calculatedcols <- c(calculatedcols_inbgstats,
  #                     "distance_min" ,
  #                     "sitecount_max")

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
    distance_min_avgperson = collapse::fmean(distance_min_avgperson, w = pop),   # distance_min_avgperson = weighted.mean(distance_min_avgperson, w= pop, na.rm=TRUE),
    sitecount_max    = collapse::fmax(sitecount_max ) ,

    sitecount_unique = collapse::fnunique(ejam_uniq_id), ######## CHECK THIS
    sitecount_avg    = collapse::fmean(sitecount_avg, w = pop)  # sitecount_avg     =weighted.mean(sitecount_avg, w= pop, na.rm=TRUE)

  ), by = .(ejam_uniq_id) ]
#
results_bysite <- merge(results_bysite, results_bysite_minmax, by = "ejam_uniq_id") # data.table uses by=, not on=, for merge() or groupingby, and uses on=  for JOINS!

  results_overall_minmax <- sites2bgs_plusblockgroupdata_bysite[ , .(
    distance_min = collapse::fmin(distance_min),
    distance_min_avgperson = collapse::fmean(distance_min_avgperson, w = pop),   # distance_min_avgperson = weighted.mean(distance_min_avgperson, w= pop, na.rm=TRUE),
    sitecount_max    = collapse::fmax(sitecount_max ) ,

    sitecount_unique = collapse::fnunique(ejam_uniq_id), ######## CHECK THIS
    sitecount_avg     = collapse::fmean(sitecount_avg, w = pop)   # sitecount_avg     =weighted.mean(sitecount_avg, w= pop, na.rm=TRUE)
  ) ]
  results_overall <- cbind(results_overall, results_overall_minmax) # cbind not merge, since only 1 row

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
  blockcount_by_site <- sites2blocks[, .(blockcount_near_site = .N),                    by = ejam_uniq_id]
  bgcount_by_site    <- sites2blocks[, .(bgcount_near_site = collapse::fnunique(bgid)), by = ejam_uniq_id]

  results_bysite <- merge(results_bysite, blockcount_by_site, by = "ejam_uniq_id")
  results_bysite <- merge(results_bysite, bgcount_by_site,    by = "ejam_uniq_id")

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

  ##################################################### #
  ## ........update progress bar in shiny app ####
  if (is.function(updateProgress)) {
    boldtext <- paste0('Computing results')
    updateProgress(message_main = boldtext, value = 0.8)
  }
  ##################################################### #

  ##################################################### #
  # CALCULATE PERCENT DEMOGRAPHICS FROM SUMS OF COUNTS, via FORMULAS  [hardcoded here, for now]
  #
  # but should do that using a list of formulas like in ejscreen package file ejscreenformulas
  # and a function like analyze.stuff  calc.fields()
  ##################################################### #

  #      NOTE ON PERCENTAGES AS 0 TO 1.00 RATHER THAN O TO 100.
  #
  #  API returns demographic percent indicators like percent low income as 0-100,
  #   (which doesn't make much sense but popups code needs to know)
  #  and EJAM doaggregate() was returning them as 0-100, but 1/23/23 changed those to 0 to 1.00,
  #    so that lookups of demographics in percentile tables will work right.
  #  but
  #  the lookup tables like usastats store those variables as 0 to 1.00 . . .. see usastats[74:80,1:9]
  #  and the dataset of all US blockgroups (from EJScreen FTP site or in blockgroupstats) stores that as 0 to 1.00

  ##################################################### #
  # * CALCULATE DEMOGRAPHICS using FORMULAS ####
  #  (using the Rolled up Counts)
  # this was meant to handle multiple columns (formula for each new one) for many rows (and here in buffer results, one site is a row, not one blockgroup)


  #### possible way to pull formulas out of doaggregate() is this:
  ## it would not be as fast as data.table approach though, and is not tested.
  # dcalculated_overall <- calc_ejam(results_overall, keep.old = "", formulas = formulas_d)
  # results_overall <- cbind(results_overall, dcalculated_overall)
  # dcalculated_bysite <- calc_ejam(results_bysite, keep.old = "", formulas = formulas_d)
  # results_bysite <- cbind(results_bysite, dcalculated_bysite)


  # "nonmins <- nhwa"
  # "mins <- pop - nhwa"

  #################################################### #
  ##  Demog formulas OVERALL ####
  #################################################### #

  results_overall[ , `:=`(
    pctover64       = 1 * ifelse(pop == 0, 0,            over64        / pop),
    pctunder5       = 1 * ifelse(pop == 0, 0,            under5        / pop)
  ) ]
  ##################################### #

  if ("nh" %in% subgroups_type | "both" %in% subgroups_type) {
    #  as in names_d_subgroups_nh or names_d_subgroups
    #  where they are all NONHISPANIC
    results_overall[ , `:=`(
      pcthisp         = 1 * ifelse(pop == 0, 0, as.numeric(hisp )        / pop),
      pctnhba         = 1 * ifelse(pop == 0, 0, as.numeric(nhba )        / pop),
      pctnhaiana      = 1 * ifelse(pop == 0, 0, as.numeric(nhaiana)      / pop),
      pctnhaa         = 1 * ifelse(pop == 0, 0, as.numeric(nhaa )        / pop),
      pctnhnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhnhpia )     / pop),
      pctnhotheralone = 1 * ifelse(pop == 0, 0, as.numeric(nhotheralone) / pop),
      pctnhmulti      = 1 * ifelse(pop == 0, 0, as.numeric(nhmulti )     / pop),
      pctnhwa         = 1 * ifelse(pop == 0, 0, as.numeric(nhwa )        / pop)
    )]
  }
  if ("alone" %in% subgroups_type | "both" %in% subgroups_type) {
    # as in names_d_alone
    #   they include hispanic within each racial subgroup here, so it is black alone (whether or not hispanic) not just non-hispanic black alone.
    results_overall[ , `:=`(
      pcthisp       = 1 * ifelse(pop == 0, 0, as.numeric(hisp )      / pop),
      pctba         = 1 * ifelse(pop == 0, 0, as.numeric(ba )        / pop),
      pctaiana      = 1 * ifelse(pop == 0, 0, as.numeric(aiana)      / pop),
      pctaa         = 1 * ifelse(pop == 0, 0, as.numeric(aa )        / pop),
      pctnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhpia )     / pop),
      pctotheralone = 1 * ifelse(pop == 0, 0, as.numeric(otheralone) / pop),
      pctmulti      = 1 * ifelse(pop == 0, 0, as.numeric(multi )     / pop),
      pctwa         = 1 * ifelse(pop == 0, 0, as.numeric(wa )        / pop)
    )]
  }

  ##################################### #
  results_overall[ , `:=`(
    pctmin          = 1 * ifelse(pop == 0, 0, as.numeric(mins)         / pop),
    pctlowinc       = 1 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio),
    pctlths         = 1 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up),
    pctlingiso      = 1 * ifelse(hhlds          == 0, 0, lingiso                / hhlds),
    # note pre1960 is actually an envt indicator, not demog - does ejscreen use popwtd mean for it or the ratio of sums of counts formula?? ***
    pctpre1960      = 1 * ifelse(builtunits     == 0, 0, pre1960                / builtunits),
    pctunemployed   = 1 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)
  ) ]


  results_overall[ , `:=`(
    Demog.Index = (pctlowinc + pctmin) / 2,
    # *** add supplemental indicator
    #
    Demog.Index.Supp  = (pctlowinc + pctunemployed + pctlths + pctlingiso + ifelse(is.na(lowlifex), 0, lowlifex) ) / ifelse(is.na(lowlifex), 4, 5)
    # *** add supplemental indicator too
    #

    # # supplemental demographic index = (% low-income + % unemployed + % less than high school education + % limited English speaking + low life expectancy) / 5
    # For block groups where low life expectancy data is missing (NA), the formula will average the other four factors!
    # NOTE THAT EJScreen uses the term "Supplemental Indexes" to refer to
    #  EJ Indexes that are based on the Supplemental Demographic Index
    # See details at  https://www.epa.gov/ejscreen/ejscreen-map-descriptions#supp
  )]

  if (extra_demog) {
  results_overall[ , `:=`(
    pctdisability  = ifelse(DISAB_UNIVERSE == 0, 0, DISABILITY / DISAB_UNIVERSE),
    pctunder18 =  ifelse(pop == 0, 0, AGE_LT18 / pop),
    pctover17  =  ifelse(pop == 0, 0, AGE_GT17 / pop),
    pctmale    =  ifelse(pop == 0, 0, MALES    / pop),
    pctfemale  =  ifelse(pop == 0, 0, FEMALES  / pop),

    p_own_occupied =  ifelse(OCCHU == 0, 0, OWNHU / OCCHU),

    PCT_HH_BPOV  =  ifelse(hhlds == 0, 0, HH_BPOV / hhlds),

    pct_lan_eng     = ifelse(LAN_UNIVERSE == 0, 0, LAN_ENG_NA   / LAN_UNIVERSE),  # need to add to map_headernames
    pct_lan_spanish = ifelse(LAN_UNIVERSE == 0, 0, LAN_SPANISH  / LAN_UNIVERSE),  # need to add to map_headernames
    pct_lan_ie      = ifelse(LAN_UNIVERSE == 0, 0, LAN_IE       / LAN_UNIVERSE),   # need to add to map_headernames
    pct_lan_api     = ifelse(LAN_UNIVERSE == 0, 0, LAN_API      / LAN_UNIVERSE),   # need to add to map_headernames

    PCT_HLI_SPANISH_LI = ifelse(lingiso == 0, 0, HLI_SPANISH_LI  /  lingiso),  # need to add to map_headernames
    PCT_HLI_IE_LI      = ifelse(lingiso == 0, 0, HLI_IE_LI       /  lingiso),  # need to add to map_headernames
    PCT_HLI_API_LI     = ifelse(lingiso == 0, 0, HLI_API_LI      /  lingiso),  # need to add to map_headernames
    PCT_HLI_OTHER_LI   = ifelse(lingiso == 0, 0, HLI_OTHER_LI    /  lingiso)   # need to add to map_headernames

    )]

  }

  #################################################### #
  ## Demog formulas BYSITE  ####
  #################################################### #


  results_bysite[ , `:=`(
    pctover64       = 1 * ifelse(pop == 0, 0,            over64        / pop),
    pctunder5       = 1 * ifelse(pop == 0, 0,            under5        / pop)
  )]

  ##################################### #
  # cbind(names_d_subgroups, names_d_subgroups_count)
  # [,1]              [,2]
  # [1,] "pcthisp"         "hisp"
  # [2,] "pctnhba"         "nhba"
  # [3,] "pctnhaa"         "nhaa"
  # [4,] "pctnhaiana"      "nhaiana"
  # [5,] "pctnhnhpia"      "nhnhpia"
  # [6,] "pctnhotheralone" "nhotheralone"
  # [7,] "pctnhmulti"      "nhmulti"
  # [8,] "pctnhwa"         "nhwa"
  #
  if ("nh" %in% subgroups_type | "both" %in% subgroups_type) {
    # original versions of demog subgroups, as in names_d_subgroups_nh or names_d_subgroups
    #  where they are all NONHISPANIC - MIGHT GET phased out - EJScreen 2.2 does NOT use this version of subgroups
    results_bysite[ , `:=`(
      pcthisp         = 1 * ifelse(pop == 0, 0, as.numeric(hisp )        / pop),
      pctnhba         = 1 * ifelse(pop == 0, 0, as.numeric(nhba )        / pop),
      pctnhaiana      = 1 * ifelse(pop == 0, 0, as.numeric(nhaiana)      / pop),
      pctnhaa         = 1 * ifelse(pop == 0, 0, as.numeric(nhaa )        / pop),
      pctnhnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhnhpia )     / pop),
      pctnhotheralone = 1 * ifelse(pop == 0, 0, as.numeric(nhotheralone) / pop),
      pctnhmulti      = 1 * ifelse(pop == 0, 0, as.numeric(nhmulti )     / pop),
      pctnhwa         = 1 * ifelse(pop == 0, 0, as.numeric(nhwa )        / pop)
    )]
  }
  if ("alone" %in% subgroups_type | "both" %in% subgroups_type) {
    # new versions of subgroups - as used by EJScreen 2.2 - as in names_d_alone
    #   they include hispanic within each racial subgroup here, so it is black alone (whether or not hispanic) not just non-hispanic black alone.
    results_bysite[ , `:=`(
      pcthisp       = 1 * ifelse(pop == 0, 0, as.numeric(hisp )      / pop),
      pctba         = 1 * ifelse(pop == 0, 0, as.numeric(ba )        / pop),
      pctaiana      = 1 * ifelse(pop == 0, 0, as.numeric(aiana)      / pop),
      pctaa         = 1 * ifelse(pop == 0, 0, as.numeric(aa )        / pop),
      pctnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhpia )     / pop),
      pctotheralone = 1 * ifelse(pop == 0, 0, as.numeric(otheralone) / pop),
      pctmulti      = 1 * ifelse(pop == 0, 0, as.numeric(multi )     / pop),
      pctwa         = 1 * ifelse(pop == 0, 0, as.numeric(wa )        / pop)
    )]
  }

  ##################################### #

  results_bysite[ , `:=`(
    pctmin          = 1 * ifelse(pop == 0, 0, as.numeric(mins)         / pop),
    pctlowinc       = 1 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio),
    pctlths         = 1 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up),
    pctlingiso      = 1 * ifelse(hhlds          == 0, 0, lingiso                / hhlds),
    pctpre1960      = 1 * ifelse(builtunits     == 0, 0, pre1960                / builtunits),
    pctunemployed   = 1 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)  # ,
  )]

  results_bysite[ , `:=`(
    Demog.Index = (pctlowinc + pctmin) / 2,
    Demog.Index.Supp = (pctlowinc + pctunemployed + pctlths + pctlingiso + lowlifex ) / ifelse(is.na(lowlifex), 4, 5)
  )]


  if (extra_demog) {
  results_bysite[ , `:=`(
    pctdisability  = ifelse(DISAB_UNIVERSE == 0, 0, DISABILITY / DISAB_UNIVERSE),
    pctunder18 =  ifelse(pop == 0, 0, AGE_LT18 / pop),
    pctover17  =  ifelse(pop == 0, 0, AGE_GT17 / pop),
    pctmale    =  ifelse(pop == 0, 0, MALES    / pop),
    pctfemale  =  ifelse(pop == 0, 0, FEMALES  / pop),

    p_own_occupied =  ifelse(OCCHU == 0, 0, OWNHU / OCCHU),

    PCT_HH_BPOV  =  ifelse(hhlds == 0, 0, HH_BPOV / hhlds),

    pct_lan_eng     = ifelse(LAN_UNIVERSE == 0, 0, LAN_ENG_NA   / LAN_UNIVERSE),  # need to add to map_headernames
    pct_lan_spanish = ifelse(LAN_UNIVERSE == 0, 0, LAN_SPANISH  / LAN_UNIVERSE),  # need to add to map_headernames
    pct_lan_ie      = ifelse(LAN_UNIVERSE == 0, 0, LAN_IE       / LAN_UNIVERSE),   # need to add to map_headernames
    pct_lan_api     = ifelse(LAN_UNIVERSE == 0, 0, LAN_API      / LAN_UNIVERSE),   # need to add to map_headernames

    PCT_HLI_SPANISH_LI = ifelse(lingiso == 0, 0, HLI_SPANISH_LI  /  lingiso),  # need to add to map_headernames
    PCT_HLI_IE_LI      = ifelse(lingiso == 0, 0, HLI_IE_LI       /  lingiso),  # need to add to map_headernames
    PCT_HLI_API_LI     = ifelse(lingiso == 0, 0, HLI_API_LI      /  lingiso),  # need to add to map_headernames
    PCT_HLI_OTHER_LI   = ifelse(lingiso == 0, 0, HLI_OTHER_LI    /  lingiso)   # need to add to map_headernames

  )]
  }
  ##################################################### #
  # Demog.Index.US = sum(mins) / sum(pop)  +  sum(lowinc) / sum(povknownratio) ) / 2,
  # Demog.Index = (pctlowinc + pctmin) / 2,
  #  or is it treated as envt var and just pop mean?  ********************
  # Demog.Index <- stats::weighted.mean(Demog.Index, w = pop)
  ### Demog.Index <- VSI.eo

  # # To be replaced with data available to this package
  # myformulas <- ejscreen package file ejscreenformulas
  # # one row per buffer/site?
  # results_bysite_formulas_done <- ejscreen package file ejscreen.acs.calc(bg = results_bysite, keep.old = 'all', keep.new = 'all', formulas = myformulas)
  #
  # just one row?
  #
  # results_overall_formulas_done  <- ejscreen package file ejscreen.acs.calc(bg = results_overall, keep.old = 'all', keep.new = 'all', formulas = myformulas)

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
  #    need to calculate that MAX from raw bg data when you aggregate by ejam_uniq_id, doing MAX not just AVG or SUM.
  #################### #
  # WE COULD  insert code here to calc max raw score of all blockgroups at a given site.
  # something like this?
  # maxneededcols <- c(names_e, names_d, names_d_subgroups) # not also EJ?? # NEED TO CONFIRM THIS IS RIGHT
  # results_bysite_minmax_ED <- sites2bgs_plusblockgroupdata_bysite[   ,  lapply(
  #   .SD,
  #   max(x, na.rm = TRUE)
  # ), .SDcols = maxneededcols, by = .(ejam_uniq_id) ]


  #____________________________________________________  #  ##################################################### #  ######################################################

  # PERCENTILES ####


  ##################################################### #  ##################################################### #
  #
  ## WHAT STATE IS EACH SITE IN? ####
  #
  # Assign state abbrev to each site!! (allows for state percentiles and averages to be looked up) (and statename, FIPS.ST, REGION?)
  # Get ST (state) each site is in, to report scores as state percentiles
  #
  # This will be a data.frame with ejam_uniq_id, ST, (and maybe other columns).
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
    # returns a data.table with these columns:  ejam_uniq_id, ST  (and only 1 row per ejam_uniq_id! It is just to know the ST of each unique ejam_uniq_id)
    if (!missing(sites2states_or_latlon)) {
      # add in the lat,lon columns - this is always available if ejamit() called this since it passes the pts as sites2states_or_latlon
      if ("ejam_uniq_id" %in% names(sites2states_or_latlon) & "ejam_uniq_id" %in% names(sites2states)) {

        sites2states <- merge(sites2states, sites2states_or_latlon, by = 'ejam_uniq_id') #  error if  ejam_uniq_id is not there
      } else {
        sites2states <- cbind(sites2states, sites2states_or_latlon) #   ***xxx  HAVE NOT CHECKED IF THIS WORKS OR IS CORRECT !
      }
    } else {
      # maybe get latlon of closest block?? no, just omit lat,lon in this case
    }
  } else { # hope it has ST, which is best, or latlon which is slowest, but in between was via blockid, done above!
    sites2states <- states_infer(sites2states_or_latlon)
  }
  # sites2states  is df or dt with just 1 row/site, and columns= ejam_uniq_id,ST ; and MIGHT have lat,lon and other info.

  results_bysite[sites2states,  ST := ST,  on = "ejam_uniq_id"] # check this, including when ST is NA
  results_overall$ST <- NA
  results_bysite[, statename :=  stateinfo$statename[match(ST, stateinfo$ST)]]
  results_overall$statename <- NA
  ## add blank ejam_uniq_id column to results_overall (no longer tied to include_ejindexes)
  results_overall$ejam_uniq_id <- NA
  #  ##################################################### #  ##################################################### #

  if (missing(radius)) {radius.miles <- round(max(sites2blocks$distance, na.rm = TRUE), 1)}

  ##################################################### #
  ## PERCENTILES - express raw scores (from results_bysite AND  results_overall) in percentile terms ####
  #  VIA  lookup tables of US/State  percentiles, called usastats   and statestats
  #  note: usastats is  like ejscreen package file lookupUSA , and pctile_from_raw_lookup is like ejanalysis package file lookup.pctile()
  #
  #  *** this should be extracted as a function (but keeping the efficiency of data.table changes by reference using := or set___)
  #
  ##################################################### #
  # these lines about names of variables should be pulled out of here and defined as params or another way   xxx
  # specify which variables get converted to percentile form

  varsneedpctiles <- c(names_e,  names_d, subs   ) # ONLY IF THESE ARE ALL IN LOOKUP TABLES AND blockgroupstats?
  # varsneedpctiles <- intersect(varsneedpctiles, names(blockgroupstats))
  varnames.us.pctile    <- paste0(      'pctile.', varsneedpctiles) # but EJ indexes do not follow that naming scheme and are handled with separate code
  varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles) # but EJ indexes do not follow that naming scheme and are handled with separate code

  # set up empty tables to store the percentiles we find
  us.pctile.cols_bysite     <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite)     <- varnames.us.pctile
  state.pctile.cols_bysite  <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite)  <- varnames.state.pctile
  us.pctile.cols_overall    <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall)    <- varnames.us.pctile
  # done later: state.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_overall) <- varnames.state.pctile

  # SURELY THERE IS A FASTER / VECTORIZED WAY TO DO THIS (this actually is noticeably slow, at least the line that starts with state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- pctile_from_raw_lookup( ):
  #  >>>> VERY SLOW STEP; also the function  pctile_from_raw_lookup()  may need to be optimized ####
  # here it loops over the 31 to 83 (if ej) indicators, and then the function itself loops over USA + up to 50+ states ! ***
  for (i in seq_along(varsneedpctiles)) {
    myvar <- varsneedpctiles[i]
    if ((myvar %in% names(usastats)) & (myvar %in% names(results_bysite)) & (myvar %in% names(results_overall))) {  # use this function to look in the lookup table to find the percentile that corresponds to each raw score value:
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
        unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats)
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
        unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats)
      # (note it is a bit hard to explain using an average of state percentiles   in the "overall" summary)
    } else { # cannot find that variable in the percentiles lookup table
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]] <- NA
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]] <- NA
    }
    if ((myvar %in% names(statestats)) & (myvar %in% names(results_bysite)) ) {
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


  results_overall <- cbind(results_overall, us.pctile.cols_overall ) # , state.pctile.cols_overall)
  results_bysite  <- cbind(           results_bysite,  us.pctile.cols_bysite,  state.pctile.cols_bysite )
  #  ##################################################### #  ##################################################### #


  ##____________________________________________________  #  ##################################################### #

  ############################################################################## #

  ## EJ INDEX PERCENTILES ####

  ###################### # #
  if (include_ejindexes) {

    # The 2023/early 2024 EJ index formula:
    # 0) The buffer raw EJ Index is probably just the pop wtd mean of the RAW scores of blockgroups in it, (but reported finally as pctile via lookup)
    #  NOT recalculated via formulas ... It would not make sense to calculate from formula.
    #  Note the POP WTD MEAN for EJ INDEXES WAS ALREADY CALCULATED earlier in doaggregate(), along with all OTHER POP WTD MEAN INDICATORS
    # 1) IMPORTANT QUESTION FOR OEJ: DOES THE STATE PERCENTILES VERSION OF EJ INDEX USE STATE PERCENTILE IN ITS FORMULA??  YES, seems so, so kept State version of each raw EJ score, to use for calculating State EJ scores in buffers.
    # 2)    also we needed to name these columns carefully, and calculate each pctile based on correct raw scores:
    #   Note that  EJ index pctiles have special naming scheme and need to be handled with separate code.
    #   for most indicators, each raw indicator gets reported as a US pctile and a State pctile.
    #   for EJ indexes, though, there are US versions and State versions of the raw scores! so
    #     each US raw EJ gets reported only as US pctile, and
    #     each state raw gets reported only as state pctile.
    # 3) For the state percentile,   WE ASSUME THE ENTIRE BUFFER IS MAINLY OR ALL IN ONE STATE (based on point in center of circle) AND
    #   LOOK UP  RAW EJ INDEX IN statestats based on THAT STATE'S identity TO ASSIGN THE PERCENTILE.
    # We have 4 sets of EJ indexes: US EO, US SUPP, STATE EO, STATE SUPP
    ## EO (executive order) is raw EJ Index basic = 2-factor demog index times the envt percentile
    #  Supp. is raw EJ index supplemental = 5-factor suppl demog index times the envt percentile
    # results_bysite already has the raw EJ indexes in it.
    # results_overall does too.
    # Already defined this list of names earlier:
    #  ejnames_raw <-   c(names_ej, names_ej_supp, names_ej_state, names_ej_supp_state)
    ######################################################################################### #

    varnames.us.pctile_EJ    <- c(names_ej_pctile,       names_ej_supp_pctile)
    varnames.state.pctile_EJ <- c(names_ej_state_pctile, names_ej_supp_state_pctile)
    ejnames_pctile <- c(varnames.us.pctile_EJ, varnames.state.pctile_EJ)

    # >>> interim and untested - MOSTLY A COPY/PASTE OF CODE ABOVE ####
    # This for now just is a copy of code used above to get percentiles, except it uses standalone table of raw scores not raw scores already placed into the results tables,
    # since we do not really need to keep or return the raw scores of EJ indexes, unlike all other indicators where we want the raw scores.
    # Ideally should have a function to use to do this, but for other variables server code did this, not a function.

    # set up empty tables to store the percentiles we find, NA values by default to start off.
    us.pctile.cols_bysite     <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varnames.us.pctile_EJ)));    colnames(us.pctile.cols_bysite)     <- varnames.us.pctile_EJ
    state.pctile.cols_bysite  <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varnames.state.pctile_EJ))); colnames(state.pctile.cols_bysite)  <- varnames.state.pctile_EJ
    us.pctile.cols_overall    <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varnames.us.pctile_EJ)));    colnames(us.pctile.cols_overall)    <- varnames.us.pctile_EJ
    # done later: state.pctile.cols_overall

    # SURELY THERE IS A FASTER / VECTORIZED WAY TO DO THIS
    for (i in seq_along(ejnames_pctile)) {
      myvar <- ejnames_raw[i]  ##
      if (myvar %in% names(usastats)) { # e.g., "EJ.DISPARITY.proximity.rmp.supp"
        # look in the lookup table to find the percentile that corresponds to each raw score value:

        us.pctile.cols_bysite[    , ejnames_pctile[i]]    <- pctile_from_raw_lookup(
          unlist( results_bysite[  , ..myvar]),
          varname.in.lookup.table = myvar, lookup = usastats)

        us.pctile.cols_overall[   , ejnames_pctile[i]]    <- pctile_from_raw_lookup(
          unlist(results_overall[  , ..myvar]),
          varname.in.lookup.table = myvar, lookup = usastats)
        # (note it is a bit hard to explain using an average of state percentiles   in the "overall" summary)
      } else { # cannot find that variable in the percentiles lookup table
        # us.pctile.cols_bysite[    , ejnames_pctile[i]] <- NA
        # us.pctile.cols_overall[   , ejnames_pctile[i]] <- NA
      }
      if (myvar %in% names(statestats)) { # e.g., "state.EJ.DISPARITY.proximity.rmp.supp"
        state.pctile.cols_bysite[ , ejnames_pctile[i]] <- pctile_from_raw_lookup(    ### VERY SLOW STEP 289 msec
          unlist(results_bysite[  , ..myvar]),
          # unlist(cbind(ej_bysite, ej_supp_bysite)[  , ..myvar]),
          varname.in.lookup.table = myvar, lookup = statestats, zone =  results_bysite$ST) # should be same count of rows in results_bysite$ST and ej_bysite !
        ##  must be done later, as avg of sites:
        # state.pctile.cols_overall[, ejnames_pctile[i]] <- pctile_from_raw_lookup(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats, zone =  results_overall$ST)
      } else {
        # state.pctile.cols_bysite[ , ejnames_pctile[i]] <- NA # why do this? already NA until filled.
        ##  must be done later, as avg of sites:
        # state.pctile.cols_overall[, ejnames_pctile[i]] <- NA
      }
    }

    # add EJ index percentiles to results compilation (except for overall state percentile which is created as a weighted average of percentiles further below)
    # Do not provide the raw EJ scores, just the percentiles?

    results_overall <- cbind( results_overall, us.pctile.cols_overall ) # , state.pctile.cols_overall)
    results_bysite  <- cbind(           results_bysite,  us.pctile.cols_bysite,  state.pctile.cols_bysite )


    #*# Then for overall results as EJ index State percentile, I guess we use the popwtd mean of the site-specific EJ index State PERCENTILES?
    #*#   (You cannot look up the average (overall) raw score since the US percentiles table is not applicable really.)
    # add certain variable names to the list of variables for which overall average state percentile will be calculated
    varnames.state.pctile <- c(varnames.state.pctile, varnames.state.pctile_EJ)


  } # done with case where EJ Indexes are needed

  ############################################################################## #
  #
  # *** OVERALL AVG of STATE PERCENTILES ####
  #  (as popwtd mean of sites state pctiles - which seems bizarre but not sure how else you would do it)
  # The overall state percentile is either simply the pop wtd mean of state percentiles of each site’s average person’s suppl demog index across all the sites,
  # or, other idea:
  #   maybe is the percentile of the raw average compared to adjusted national percentiles table,
  # where that table is adjusted based on what they would be if those states had the pop counts seen at the sites analyzed, which is more complicated
  # – essentially construct a nation that has the same state populations as the nearby populations analyzed? something like that.

  # now that site-specific percentiles have been calculated and looked up,
  # you can calculate that overall state percentiles from those as a pop wtd mean (not by looking them up from raw scores as would be done for US pctiles, since each site may be in its own state)
  # xxx
  state.pctile.cols_overall <-  results_bysite[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = pop)  # stats::weighted.mean(x, w = pop, na.rm = TRUE)
  }), .SDcols = varnames.state.pctile ]

  # redo these in data.table:: style, for speed? but it is just a 1-row result  *********************************
  results_overall <- cbind(results_overall, state.pctile.cols_overall)
  #____________________________________________________  #  ##################################################### #  ######################################################

  ############################################################################## #
  #
  # US and STATE AVERAGES ####
  #     FOR EACH INDICATOR (repeated for all site rows) ###



  # THESE CALCULATIONS AND OUTPUTS WILL BE LIMITED HERE TO THE INDICATORS THAT ARE ACTUALLY FOUND IN THE OUTPUTS SO FAR
  #  i.e. found in   results_bysite
  #   AND in LOOKUP TABLES (statestats, usastats)
  # To be extra careful here, restrict it to only the ones found in all 3 places:
  #  us and state percentile lookup tables, and in results_bysite
  perfectnames <- function(x) intersect(intersect(intersect(x, names(usastats)), names(statestats)), names(results_bysite))

  ## (dont need avg of raw EJ indexes)  ####
  ## (we dont want to show a ratio to avg for raw EJ indexes, and no other reason to find those averages, since those raw scores are harder to interpret than Demog or Envt raw scores)

  ######################################### #
  # **** names_these and related lists are defined by EJAM already,
  # but want to ensure it uses the right version(s) of subgroups !

  # names_these <- c(names_d,              names_d_subgroups,              names_e)
  names_these <- c(names_d, subs, names_e) # to use nh or alone or both!!
  # names_these  may not be the same while transitioning to newer subgroups definitions
  names_these <- perfectnames(names_these)
  # names_these_avg <- c(names_d_avg,          names_d_subgroups_avg,          names_e_avg)        # #  avg.x was changed to us.avg.x naming scheme
  # THESE ARE ALREADY IN EJAM package but this ensures they are also in usastats, statestats, and results_bysite

  names_these_avg               <- paste0(      "avg.", names_these) # allows for "both" nh and alone, and also doing it this way limits it to the subset found by perfectnames(names_these)
  names_these_state_avg         <- paste0("state.avg.", names_these) # c(names_d_state_avg,    names_d_subgroups_state_avg,    names_e_state_avg)  #
  names_these_ratio_to_avg       <- paste0("ratio.to.", names_these_avg )   # <- c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg, names_e_ratio_to_avg) #
  names_these_ratio_to_state_avg <- paste0("ratio.to.", names_these_state_avg) #  <- c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg, names_e_ratio_to_state_avg)  #
  ######################################### #


  # pull averages from statestats table (note using data.frame syntax here not data.table)
  #
  # must be a cleaner way to do this part but did not have time to think about one
  stinfo <- data.table::setDT(statestats[ statestats$PCTILE == "mean" , c("REGION", names_these)])
  setnames(stinfo, "REGION", "ST")
  state.avg.cols_bysite <- stinfo[results_bysite[,.(ST)],  on = "ST"]

  # rename the colnames to be state.avg. instead of just basic names
  setnames(state.avg.cols_bysite,  names_these, names_these_state_avg )
  state.avg.cols_bysite[, ST := NULL]
  results_bysite <- cbind(results_bysite, state.avg.cols_bysite)  # cbind?? collapse:: has a faster way   ************

  ############# #
  #
  # >>> calc overall popwtd mean of each state avg !!   ####
  # but isn't this available from statestats ?? ***
  state.avg.cols_overall <-  results_bysite[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = pop)   # stats::weighted.mean(x, w = pop, na.rm = TRUE)
  }), .SDcols = names_these_state_avg] # fixed now?

  results_overall <- cbind(results_overall, state.avg.cols_overall)

  ############################################################################## #
  #
  # INCLUDE US AVERAGE FOR EACH SITE AND INDICATOR AND OVERALL ###
  #  -  THIS IS WASTING TIME AND SPACE, BUT IS CONVENIENT I GUESS

  #  overall
  avg.cols_overall <-   usastats[ usastats$PCTILE == "mean",  names_these] # not a data.table, or it would need to say  usastats[ PCTILE == "mean",  ..names_these]
  # rename the colnames to avg instead of just basic names?
  setnames(avg.cols_overall,  names_these,  names_these_avg)

  results_overall <- cbind(results_overall, avg.cols_overall)

  results_bysite <- cbind(results_bysite,  avg.cols_overall) # collapse:: has a faster way   ? want that single row data.table repeated once per site here

  ############################################################################## #

  if (calculate_ratios) {
    # RATIO to AVERAGE - (do not duplicate IN server code!)  ####
    #
    #    app_server code is/was duplicating efforts and trying to calculate these itself !!

    ## RATIOS TO US AVG ###
    ratios_to_avg_bysite  <-
      results_bysite[  , ..names_these] /
      results_bysite[, ..names_these_avg]

    ratios_to_avg_overall <-
      results_overall[  , ..names_these] /          # AVERAGE PERSON score OVERALL, RIGHT?
      results_overall[, ..names_these_avg]

    ## RATIOS TO STATE AVG ###
    ratios_to_state_avg_bysite  <-
      results_bysite[  , ..names_these] /
      results_bysite[, ..names_these_state_avg]

    ratios_to_state_avg_overall <-
      results_overall[  , ..names_these] /
      results_overall[, ..names_these_state_avg]

    # add those all to results tables
    colnames(ratios_to_avg_bysite)  <- names_these_ratio_to_avg
    colnames(ratios_to_avg_overall) <- names_these_ratio_to_avg
    colnames(ratios_to_state_avg_bysite)  <- names_these_ratio_to_state_avg
    colnames(ratios_to_state_avg_overall) <- names_these_ratio_to_state_avg

    results_bysite  <- cbind(results_bysite,  ratios_to_avg_bysite,  ratios_to_state_avg_bysite)   # collapse:: has a faster way than cbind here!
    results_overall <- cbind(results_overall, ratios_to_avg_overall, ratios_to_state_avg_overall)
  }
  # ________________________________________________________________________############ #  ########################

  #***  ###################################### #
  # RADIUS (inferred or passed here) added to results  ####

  results_overall[ , radius.miles := radius]
  results_bysite[  , radius.miles := radius]
  sites2bgs_plusblockgroupdata_bysite[ , radius.miles := radius]
  # longnames will get all the names translated from colnames of results_overall

  #***  ###################################### #
  # LATITUDE and LONGITUDE added to results  ####

  if ("lat" %in% names(sites2states)) {

    results_bysite[sites2states, lat := lat, on = "ejam_uniq_id"]
  } else {
    results_bysite[ , lat := NA]
  }
  if ("lon" %in% names(sites2states)) {

    results_bysite[sites2states, lon := lon, on = "ejam_uniq_id"]
  } else {
    results_bysite[ , lon := NA]
  }

  # add those columns to overall and bybg, so the format is same for overall and bysite tables
  results_overall[ , lat := NA]
  results_overall[ , lon := NA]

  sites2bgs_plusblockgroupdata_bysite[ , lat := NA]
  sites2bgs_plusblockgroupdata_bysite[ , lon := NA]

  #***  ###################################### #

  # COLUMNS REORDERED  ####
  # Put results columns in a more useful/ convenient order
  ###  THEY COULD BE SORTED TO MATCH EJSCREEN COMMUNITY REPORT ORDER HERE OR ELSEWHERE
  ## TO DO SO HERE, COULD BE SORTED ONLY WITHIN EACH GROUP,
  ## *** e.g., names_e[table_order_variables(names_e)]
  ## or overall as well, e.g.:
   #    useful_column_order[table_order_variables(useful_column_order)]

  {
    useful_column_order <- c(
      'id', 'ejam_uniq_id',
      'pop',           # '[or names_wts]',
      'sitename',
      'lon', 'lat', # do we want to make consistently lat,lon not lon,lat ??? ***
      'ST', 'statename', 'REGION',

      ## RATIOS to AVG in US or State ---------------
      # if (calculate_ratios)
      # for D,Dsub,E
      # these already contain nh, alone, or both  as the subgroups types
      names_these_ratio_to_avg,
      names_these_ratio_to_avg,
      names_these_ratio_to_state_avg,
      names_these_ratio_to_state_avg,

      ## DEMOGRAPHICS -----------------

      ### D RAW % ##
      # "Demog.Index.Supp", # supplemental demographic indicator now is in names_d
      names_d,   # Demog.Index, percent low income, etc.

      subs, # e.g., subs <- names_d_subgroups_nh  # or subs <-  names_d_subgroups,  like percent hispanic etc.

      # low life expectancy score
      # "lowlifex",   # now is in names_d

      ## OTHER DEMOG (mostly PERCENT) IN NEW REPORT xxx----
      c("lifexyears",
        "P_DISABILITY",
        "PCT_HH_BPOV",
        "percapincome",
        "P_OWN_OCCUPIED",
        "pctunder18", "pctover17",  "pctmale",   "pctfemale" ,
        "PCT_HLI_SPANISH_LI", "PCT_HLI_IE_LI", "PCT_HLI_API_LI", "PCT_HLI_OTHER_LI"),

      c("DISAB_UNIVERSE", "DISABILITY",
        "HH_BPOV",
        'OWNHU', "OCCHU", "OWNHU",
        'AGE_LT18', 'AGE_GT17', 'MALES', 'FEMALES',
        'LAN_UNIVERSE', 'LAN_SPANISH', 'LAN_ENG_NA', 'LAN_IE', 'LAN_API',
        'HLI_SPANISH_LI', 'HLI_IE_LI',  'HLI_API_LI', 'HLI_OTHER_LI'),
      ###  D US RATIOS?  # (above)
      ### D US PCTILE ###
      # "pctile.Demog.Index", #now in names_d_pctile
      names_d_pctile,

      names_d_subgroups_nh_pctile,       names_d_subgroups_alone_pctile,

      ### D US AVERAGES ###
      names_d_avg,

      names_d_subgroups_nh_avg,       names_d_subgroups_alone_avg,
      names_d_subgroups_nh_state_avg ,names_d_subgroups_alone_state_avg,
      # need to make flexible to have nh, alone, ,or both ! ***

      ### D STATE RATIOS?     # (above)
      ### D STATE PCTILE ##
      # "state.pctile.Demog.Index", #now in names_d_pctile
      names_d_state_pctile,

      names_d_subgroups_nh_state_pctile, names_d_subgroups_alone_state_pctile, # only available ones get used so ok to list all here.  need to make flexible to have nh, alone, ,or both ! ***

      ### D STATE AVERAGES ###
      #could create: names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg,# eg    state.avg.pctmin
      names_d_state_avg,
      # names_d_subgroups_avg,   # need to make flexible to have nh, alone, ,or both ! *** # this was not there but should add something like it ?

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

      ## EJ INDEXES are here only if include_ejindexes=TRUE -----------------

      ### EJ PCTILE US ###
      names_ej_pctile, names_ej_supp_pctile, #  ejnames_pctile,
      ### EJ PCTILE STATE ###
      names_ej_state_pctile, names_ej_supp_state_pctile, #
      ### EJ RAW - NOT NEEDED?  ###
      # names_ej, # raw scores not essential in output - keep here during testing/ debugging at least?
      # Probably should not retain the raw EJ scores in outputs? - just need to report them as percentiles ***

      ### D RAW COUNTS? -NOT NEEDED?  ###
      names_d_count,
      names_d_subgroups_nh_count, names_d_subgroups_alone_count,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT
      names_d_other_count,  # were in EJAM output but NOT ESSENTIAL IN OUTPUT # denominator counts but also pop which is already above

      ## OTHER DEMOG COUNTS IN NEW REPORT ----



      ## BG AND BLOCK COUNTS ----
      #  # it will use whichever version of name is found
      'statLayerCount',      "bgcount_near_site", "bgcount_overall",    # count of blockgroups, as named in API vs in EJAM outputs
      'weightLayerCount', "blockcount_near_site", "blockcount_overall",  # count of blocks, as named in API vs in EJAM outputs

      "distance_min", "distance_min_avgperson",
      "sitecount_max", "sitecount_unique", "sitecount_avg",

      'radius', 'radius.miles' # it will use whichever version of name is found

    )
    useful_column_order <- collapse::funique(useful_column_order)
  }
  # retain all the columns now, but put 1st the ones specified by useful_column_order
  #  1 and 2  are used because/in case there are differences in how overall and by site refer to a stat like "bgcount_near_site" vs "bgcount_overall"

  useful_column_order1 <- c(useful_column_order[useful_column_order %in% names(results_overall)], setdiff(names(results_overall), useful_column_order))
  # results_overall
  data.table::setcolorder(results_overall, neworder = useful_column_order1)

  # useful_column_order2 <- c(useful_column_order[useful_column_order %in% names(results_bysite)], setdiff(names(results_bysite), useful_column_order))
  data.table::setcolorder(results_bysite, neworder = useful_column_order1)

  useful_column_order3 <- c(useful_column_order[useful_column_order %in% names(sites2bgs_plusblockgroupdata_bysite)], setdiff(names(sites2bgs_plusblockgroupdata_bysite), useful_column_order))
  data.table::setcolorder(sites2bgs_plusblockgroupdata_bysite, neworder = useful_column_order3)
  # only 74 columns in that last, longer table
  ##################################################### #  ##################################################### #  ##################################################### #

  # DONE - Return list of results ####

  ## results list ###

  # maybe now do all this renaming/friendly naming via map_headernames via fixcolnames() ... xxx *****

  longnames <- fixcolnames(names(results_overall), oldtype = 'r', newtype = 'long')

  # older renaming / friendly names code:

  # longnames2 <- map_headernames$longname_tableheader[match(
  #   names(results_overall), map_headernames$newnames_ejscreenapi
  # )]
  # if no long name found, just use what was there already, otherwise will use the nicer longnames
  # longnames[is.na(longnames)] <- names(results_bysite)[is.na(longnames)] # none will be NA if using fixcolnames

  ########################### #

  ## temporarily get friendly versions of some ratio names, for longnames !!  (and could make friendly the headers in all tables?) ####
  # renamer = data.frame(
  #   old=grep("ratio.to", names_all, value=TRUE),
  #   new=grep("Ratio to", names_all, value=TRUE))  # map old varnames to new friendly versions
  # # renamer_check <- data.frame(
  # #   old = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg,  names_e_ratio_to_avg,  names_d_ratio_to_state_avg,  names_d_subgroups_ratio_to_state_avg,  names_e_ratio_to_state_avg ),
  # #   new = c(names_d_ratio_to_avg_friendly, names_d_subgroups_ratio_to_avg_friendly, names_e_ratio_to_avg_friendly, names_d_ratio_to_state_avg_friendly, names_d_subgroups_ratio_to_state_avg_friendly,  names_e_ratio_to_state_avg_friendly))
  # # > all.equal(sort(renamer$new), sort(renamer_check$new))
  # # [1] TRUE
  # # > all.equal(sort(renamer$old), sort(renamer_check$old))
  # # [1] TRUE
  # longnames[ longnames %in% renamer$old] <- renamer$new[match(longnames[ longnames %in% renamer$old] , renamer$old)]

  ## temporarily get friendly versions of some average names ####
  # renamer = data.frame(
  #   old=c(names_e_avg,       names_d_avg,       names_d_subgroups_avg,
  #         names_e_state_avg, names_d_state_avg, names_d_subgroups_state_avg),
  #   new=c(names_e_avg_friendly,   names_d_avg_friendly, names_d_subgroups_avg_friendly,
  #         names_e_state_avg_friendly,   names_d_state_avg_friendly, names_d_subgroups_state_avg_friendly)
  # )  # map old varnames to new friendly versions (not all were there)
  # longnames[ longnames %in% renamer$old] <- renamer$new[match(longnames[ longnames %in% renamer$old] , renamer$old)]


  # not   in map_headernames (*** but is that still the case?) :

  renamer = data.frame(
    old = c(
      'distance_min',
      'distance_min_avgperson',
      'sitecount_max',
      'sitecount_unique',
      'sitecount_avg'
    ),
    new = c(
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
    #  but need be careful when looking at that stat overall to not count some bgs twice. ?

    longnames = longnames,

    # results_summarized gets added here later, by batch.summarize() in ejamit() or app_server()

    # formatted gets added here later also

    # SEPARATE VARIABLES TO RETURN ALONE:
    count_of_blocks_near_multiple_sites = count_of_blocks_near_multiple_sites #,
    # blockcount_overall = blockcount_overall, # note already also in results_overall as a column now, so we dont need to duplicate it here
    # bgcount_overall = bgcount_overall        # note already also in results_overall as a column now, so we dont need to duplicate it here
  )


  # if (NROW(results$results_bysite) == 1) {
  #   # If we analyzed only 1 place then overall is same as 1 site per row!
  #   results$results_overall[ , `:=`(
  #     `EJScreen Report` = results$results_bysite$`EJScreen Report`,   #  rep(NA,nrow(out$results_bysite)),
  #     `EJScreen Map`    = results$results_bysite$`EJScreen Map`,    # rep(NA,nrow(out$results_bysite)),
  #     # `ACS Report`      = out$results_bysite$,   #  rep(NA,nrow(out$results_bysite)),
  #     `ECHO report`     = results$results_bysite$`ECHO report`     # rep(NA,nrow(out$results_bysite))
  #   )]
  # } else {
  #   results$results_overall[ , `:=`(
  #     `EJScreen Report` = NA,   #  rep(NA,nrow(out$results_bysite)),
  #     `EJScreen Map`    = NA,    # rep(NA,nrow(out$results_bysite)),
  #     # `ACS Report`      = NA,   #  rep(NA,nrow(out$results_bysite)),
  #     `ECHO report`     = NA     # rep(NA,nrow(out$results_bysite))
  #   )]
  # }
  ########################### #
  # }) # finish system.time()


  if (interactive() & !silentinteractive) { # false if using shiny web app
    # print(timed)
    # cat("count of blocks that are near more than 1 site:", results$count_of_blocks_near_multiple_sites, "\n")
    # cat("count of blocks total, near all sites:", results$blockcount_overall, "\n")
    # cat("count of block groups, near all sites:", results$bgcount_overall, "\n")

    if (!called_by_ejamit) {

      ## show simple overall stats list in console ####

      x <- as.list(results$results_overall)
      x <- data.frame(variable = names(x), overall = unlist(x))
      rownames(x) <- NULL
      x$longname <- results$longname  #   map_headernames$longname_tableheader[match(x$variable, map_headernames$newnames_ejscreenapi)]
      x$longname <- substr(x$longname, 1, 40) # truncated only for dispaly in RStudio console
      x$overall <- round(x$overall, 3) # only for dispaly in RStudio console
      print(x) # print to console, 125 rows
      # cat("See viewer for datatable view site by site\n")
      ## show datatable view of each site by site in RStudio ####

      # bysite <- results$results_bysite
      # print(DT::datatable(bysite, options = list(paging=FALSE), colnames=results$longnames , escape=FALSE))
      # cat('see ejamit() \n')

      print(structure.of.output.list(x))
    }
  }
  invisible(results)
  ##################################################### #  ##################################################### #  ##################################################### #
}

