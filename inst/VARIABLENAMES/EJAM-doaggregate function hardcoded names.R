


# EXCERPTED FROM HARD CODED VERSION OF EJAM::doaggregate.R



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


-------------------------------------------------------------------------------



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









--------------------------------------------------------------------




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
  


