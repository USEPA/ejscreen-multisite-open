## example
# out <- ejamit_compare_types_of_places(testpoints_10[1:4, ], typeofsite = c("A", "B", "B", "C"))

### Note that it takes roughly the same amount of time to do the getblocksnearby() for all points at once
### versus doing it group by group, so it 
### would not be much more efficient to do that part of ejamit() all at once:
# system.time({x = list() ; ng = 10 ; for (i in 1:ng) x[[i]] = getblocksnearby(testpoints_1000[(1 + (i-1) * (1000/ng)):(i * (1000/ng)),])})
# system.time({x= getblocksnearby(testpoints_1000)})


#' Compare subsets (types) of places that are all from one list
#' @description *** DRAFT - May change but works as currently drafted.
#'  e.g., change output formats of results_bytype vs results_overall
#' 
#' @param sitepoints see [ejamit()]
#' @param typeofsite   vector of length same as NROW(sitepoints), where 
#'   each unique value defines a group of sites
#' @param silentinteractive passed to [ejamit()]
#' @param ...  see [ejamit()]
#'
#' @return similar to ejamit output but results_overall has one row per unique typeofsite
#' 
#' @export
#'
#' @examples
#'   out <- ejamit_compare_types_of_places(testpoints_10[1:4, ], 
#'     typeofsite = c("A", "B", "B", "C"))
#'   cbind(Rows_or_length = sapply(out, NROW))
#'   
#'   ejam2barplot_sitegroups(out, names_ratio_to_avg_these[1], topn = 3)
#'   
#'   ejam2barplot_sitegroups(out, "sitecount_unique", topn=3, sortby = F)
#'   
#'   ejam2barplot_sitegroups(out, "pop", topn = 3, sortby = F)
#'   
#'   # use calculated variable not in original table
#'   df <- out$results_bytype
#'   df$share <- df$pop / sum(df$pop)
#'   df$pop_per_site <- df$pop / df$sitecount_unique
#'   
#'   plot_barplot_sites(df,
#'     "share", ylab = "Share of Total Population",
#'     topn = 3, names.arg = out$types , sortby = F)
#'     
#'   plot_barplot_sites(df,
#'     "pop_per_site", ylab = "Pop. at Avg. Site in Group",
#'     topn = 3, main = "Nearby Residents per Site, by Site Type",
#'     names.arg = out$types , sortby = F)
#'   
#'   \dontrun{
#'     
#'   # Analyze by EPA Region
#'   
#'   pts <- data.frame(testpoints_1000)
#'   
#'   # Get State and EPA Region of each point from lat/lon
#'   
#'    x <- state_from_latlon(lat = pts$lat, lon = pts$lon)
#'    pts <- data.frame(pts, x)
#'    
#'    out_byregion <- ejamit_compare_types_of_places(
#'      pts, typeofsite = pts$REGION)
#'    
#'    dvarname <- names_d[3]
#'    ejam2barplot_sitegroups(out_byregion, dvarname)
#'    abline(h = usastats_means(dvarname))
#'    
#'    ejam2barplot_sitegroups(out_byregion, "ratio.to.avg.pctmin",
#'       main = "By EPA Region", ylim = c(0, 2))
#'    abline(h = 1)
#'      
#'    # Analyze by State (slow)
#'    
#'    out_bystate <- ejamit_compare_types_of_places(pts, typeofsite = pts$ST)
#'    
#'    ejam2barplot_sitegroups(out_bystate, "sitecount_unique", 
#'      names.arg = out_bystate$types, topn = 52, cex.names = 0.5,
#'      main = "Sites by State")
#'
#'   }
#'   
ejamit_compare_types_of_places <- function(sitepoints, typeofsite = NULL, 
                                           silentinteractive = TRUE,  ...) {
  
  ########################################################### 
  #  Revamp ejamit_compare_types_of_places()
  # 
  # We should do ejamit(sitepoints) once for the results_overall, results_bysite, results_bybg_people (ie bybgbysite),
  # and then to get only the results_bytype (ie by group of sites) separately in a streamlined way...
  # So the way to do all this could be the following:
  #  *** - modify ejamit() to use new params to ejamit(  groupby = )  -- seems like the best approach...
  #     that would as usual run getblocksnearby() only once, and call doaggregate() once, but 
  #     *** A) modify doaggregate() to use new params doaggregate( groupby = )
  #       it would do the usual summaries 
  #      PLUS (maybe call a new function that would..) 
  #       return a new "results_bytype" table .... that would   
  #       calculate a  results_overall  1-row table on each group (each site type),
  #       and assemble those as a special results_bytype table. 
  #       Where/ how to best fit that into doaggregate() ?
  #      AND modify  related functions like ejam2report, ejam2excel, other excel functions, and server/shiny
  #      to 1st just save and then be able to display the new by type info.
  # 
  #  ejam2report(sitenumber = ) works but would add also ejam2report(sitetype = x), rather than typenumber =x
  #     or B) a separate doaggregate_bygroup() that ONLY calculates and returns results_bytype
  #       (but NOT calculate or return a results_overall that ignores groups/types, etc.)
  # ### But, note it would be much more efficient if each group could be summarized based on just results_bysite 
  #  instead of needing to redo the getblocksnearby() and redo the join of blockgroupstats 
  #  and could simply use wtd mean or sum over sites in a group,
  #  ***which should be OK IF THOSE SITES DO NOT OVERLAP AT ALL WITHIN A GROUP !!*** Explore and confirm this.
  #  We could take note of which sites overlap any others in a new site-specific column created while aggregating sites2blocks,
  #  and for all the ones that do not overlap, aggregate by group from that 1 full results_bysite done once over the whole set of groups.
  #  and for the other ones that might overlap within a group, could you go back to using something like.... 
  #  results_bysite_bybg? since that tells you for each site what fraction of each bg to count.... 
  #  no, you need to go back to sites2blocks table!! 
  # The problem is still to count each person only once you have to know which blocks are near which site, 
  # for the blockgroups where parts of the bg are near one site and parts are near the other site.
  
  ########################################################### 
  # Find a way to make ejamit by group (by site type) work well with the ejam2xyz() functions.
  # 
  # ejamit_compare_types_of_places() as a 1st draft, for now,  
  # does not report out$results_bybg_people, to save time/space,
  # and
  # does not report a typical out$results_overall, since that would require run of all groups at once
  # and
  # calculates a  results_bytype  which has 1 row per type,
  # which is what you want to see in excel,
  # and initial draft compromise was to report a 
  # table actually containing "results_bytype", but named "results_overall"
  # But, then you have to do some things to prep output of ejamit_... for use in ejam2excel() 
  # since that expects  "results_overall" and "results_bysite" and "results_bybg_people"
  # It would be useful to have a way that ejamit_compare_types_of_places() or 
  # some bygroup version of ejamit could work easily with the ejam2xyz() functions.
  ########################################################### 
  
  # sitepoints_from_any() will 
  # accept sitepoints interactively or from filepath or from object, 
  # infer lat/lon cols,
  
  # *** and this would assign ejam_uniq_id 1:N just once for ALL sites, not once per group...
  # *** is that what we want? 
  # *** ejamit() below in a loop will again create ejam_uniq_id 1:n within each group, though, right?!
  
  # Note sitepoints_from_any() is able to check if sitepoints was missing 
  #  in expected params of ejamit_compare_types_of_places()
  sitepoints <- sitepoints_from_any(sitepoints, silentinteractive = silentinteractive)   
  sitepoints <- data.frame(sitepoints)
  # sitepoints$typeofsite <- typeofsite # that would not get used
  #################### # 
  if (is.null(typeofsite) || length(typeofsite) != NROW(sitepoints)) {
    stop("typeofsite must be a vector as long as NROW(sitepoints), each unique value defining a group of sites")
  }
  types <- unique(typeofsite)
  
  results_overall     <- list()
  results_bysite      <- list()
  # results_bybg_people <- list() # unused for now
  # results_summarized  <- list() # unused for now (see comment below on complicated format)
  longnames           <- list()
  typeofsite_list     <- list()
  sitecount_bytype    <- list()
  ########################################################### 
  
  began <- Sys.time()
  ndone <- 0
  
  for (i in seq_along(types)) {
    
    cat("Type", i, "of", length(types), "=", types[i], " -- ")
    
    out <- suppressWarnings({
      
      # *** WILL ejamit() ASSIGN ejam_uniq_id 1:N within each group of sites?
      # Do we want 1:N for the full set of sites only? ***
      
      ejamit(sitepoints = sitepoints[typeofsite == types[i], ], ..., quiet = TRUE, silentinteractive = TRUE)
    }) 
    
    results_overall[[i]] <- out$results_overall   # one row, for this group of places (this type) 
    results_bysite[[i]]  <- out$results_bysite    # one row per site, in this group
    
    # results_bybg_people[[i]] <- out$results_bybg_people  # 1 row/blockgroup near this group of sites
    
    # results_summarized[[i]]  <- out$results_summarized   # but this itself is a named list:
    #
    #   This would be made by batch.summarize()
    #   cols is a table for this group of sites, 1 row/site in this group, 
    #      with "Max.of.EJ.US.or.ST", "Number.of.EJ.US.or.ST.at.above.threshold.of.90" etc.
    #   rows is a table of 1 col/indicator, with rownames being 
    #    "Average site","Average person", "Median site","Median person","Min","Max","Sum"
    
    longnames[[i]] <- out$longnames
    
    # 1 item, the name of this type of site:
    typeofsite_list[[i]] <- typeofsite[typeofsite == types[i]]
    
    # 1 number, the count of unique sites in group, as a site Should be here only once/group:
    sitecount_bytype[[i]] <- NROW(results_bysite[[i]]) 
    
    ndone <- ndone + sitecount_bytype[[i]]
    cat("Finished", ndone, "of", NROW(sitepoints), "sites. ")
    junk <- speedreport(began, Sys.time(), ndone)
  }
  cat("\n\n")
  
  ## Be careful about how many were submitted in original list,
  ## vs  how many valid and included in results_bysite, etc.
  #    sapply(outall, NROW)
  # typeofsite       604629 all submitted to ejamit()
  # ejam_uniq_id     604629 same
  # results_bysite   603858  missing some results, some ejam_uniq_id are not there - not valid/no results
  # types                99
  # sitecount_bytype     99
  # results_bytype       99
  # results_overall      99
  # longnames           394
  # > max(outall$results_bysite$ejam_uniq_id)  # [1] 604629
  # > length(outall$results_bysite$ejam_uniq_id)  # [1] 603858
  # > length(outall$results_bysite$ejam_uniq_id) - max(outall$results_bysite$ejam_uniq_id)  # [1] -771
  # So the actual vector of site type with results reported is
  # not outall$typeofsite (which has all submitted)
  # but is only 
  # outall$typeofsite[outall$results_bysite$ejam_uniq_id]
  # (which is only those appearing in results_bysite)
  
  out <- list(
    
    types = types,
    sitecount_bytype = unlist(sitecount_bytype),
    results_bytype = data.table::rbindlist(results_overall), 
    # will be N rows instead of usual 1 row (where N is the number of types of site, ie groups)
    results_overall = NA, # will be replaced but this sets the order of the list
    # results_overall = ejamit(sitepoints = sitepoints, ...)$results_overall,
    ## This seems inefficient to run them all AGAIN but as a whole instead of by group:
    
    ejam_uniq_id = sitepoints$ejam_uniq_id,
    typeofsite = unlist(typeofsite_list),
    results_bysite = data.table::rbindlist(results_bysite),
    # Does not have a column with typeofsite, because we want it to be the
    #  same shape as normal ejamit()$results_bysite, so out$typeofsite has that info.
    
    # results_bybg_people  would be needed once per group to show x as func of distance within a typeofsite,
    # but cannot rbind them since they overlap where 2 groups of sites share some blockgroups.
    ## not sure how/if to report this... *** tbd
    
    # results_summarized <- 0, # format is different. ## not sure how/if to report this... *** tbd
    
    longnames = longnames[[1]]
  )
  
  # print(data.frame(sitecount = as.vector(out$sitecount_bytype), out$results_overall[ , c("typeofsite", "pop")]))
  
  out$results_overall <- out$results_bytype # overall is the name needed in ejam2excel() etc.
  out$results_overall$ejam_uniq_id <- out$types # NOTE: this is the code of each type, not typical ejam_uniq_id of 1:N
  names(out$results_overall) <- gsub("ejam_uniq_id", "typeofsite", names(out$results_overall))
  
  print(  
    data.frame(
      sitetype  = out$types,
      sitecount = out$sitecount_bytype,
      pop = round(out$results_bytype$pop, 0),
      round(out$results_bytype[, 
                               names_d_ratio_to_state_avg,
                               ## or else
                               # c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg), 
                               
                               with = FALSE], 1)
    )
  )
  cat("Use  ejam2excel(out)  to view results, and see the types of sites compared, one row each, in the Overall tab\n")
  cat("Use ejam2barplot_sitegroups() to plot results.\n\n")
  ended <- Sys.time()
  cat(paste0("\n ", NROW(sitepoints), " sites in ", length(unique(typeofsite)), " groups (types of sites).\n"))
  speedreport(began, ended, NROW(sitepoints))
  
  return(out)
}
#################################################################### #

