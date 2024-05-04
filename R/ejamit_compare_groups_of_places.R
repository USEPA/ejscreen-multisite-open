
# out = ejamit_compare_types_of_places(testpoints_10[1:4, ], typeofsite = c("A", "B", "B", "C"))


#' Compare subsets (types) of places that are all from one list
#'
#' @param sitepoints see [ejamit()]
#' @param typeofsite   vector of length same as NROW(sitepoints), where each unique value defines a group of sites
#' @param ...  see [ejamit()]
#'
#' @return similar to ejamit output but results_overall has one row per unique typeofsite
#' 
#' @export
#'
#' @examples
#'   out = ejamit_compare_types_of_places(testpoints_10[1:4, ], 
#'   typeofsite = c("A", "B", "B", "C"))
#'   
ejamit_compare_types_of_places <- function(sitepoints, typeofsite = NULL, ...) {
  
  if (is.null(typeofsite)) {stop("typeofsite must be a vector of length same as NROW(sitepoints), where each unique value defines a group of sites"
  )}
  types = unique(typeofsite)
  # z = list()
  results_overall <- list()
  results_bysite <- list()
  results_bybg_people <- list()
  results_summarized <- list()
  longnames <- list()
  typeofsite_list <- list()
  sitecount_bytype <- list()
  
  if ("ejam_uniq_id" %in% names(sitepoints)) {stop("sitepoints must not already have a column named ejam_uniq_id")}
  sitepoints <- data.frame(sitepoints)
  sitepoints$ejam_uniq_id <- seq_len(NROW(sitepoints)) # assign ID 1:N just once for ALL sites, not once per group
  # sitepoints$typeofsite <- typeofsite
  
  for (i in seq_along(types)) {
    cat("Type", i, "=", types[i], " -- ")
    # z[[i]] <- ejamit(sitepoints = sitepoints[typeofsite == types[i], ], ...)
    out <- suppressWarnings({
      ejamit(sitepoints = sitepoints[typeofsite == types[i], ], ..., quiet = TRUE, silentinteractive = TRUE)
    }) 
    
    results_overall[[i]] <- out$results_overall
    results_bysite[[i]] <- out$results_bysite
    results_bybg_people[[i]] <- out$results_bybg_people
    # results_summarized[[i]] <= out$results_summarized
    longnames[[i]] <- out$longnames
    typeofsite_list[[i]] <- typeofsite[typeofsite == types[i]]
    sitecount_bytype[[i]] <- NROW(results_bysite)
  }
  cat("\n\n")
  
  out = list(
    
    # sitepoints = sitepoints, 
    typeofsite = unlist(typeofsite_list),
    types = types,
    ejam_uniq_id = sitepoints$ejam_uniq_id,
    
    ## This seems inefficient to run them all AGAIN but as a whole instead of by group:
    # results_overall = ejamit(sitepoints = sitepoints, ...)$results_overall,
    
    
    sitecount_bytype = unlist(sitecount_bytype),
    results_bytype = data.table::rbindlist(results_overall), 
    # will be N rows instead of usual 1 row (where N is the number of types of site, ie groups)
    
    results_bysite = data.table::rbindlist(results_bysite),
    # lacks column with typeofsite (to stay same shape as normal ejamit()$results_bysite), and out$typeofsite has that info.
    
    # results_bybg_people  would be needed once per group to show x as func of distance within a typeofsite,
    # except you cant rbind them since they overlap where 2 groups of sites share some blockgroups.
    
     # results_summarized <- format is different
    
    longnames = longnames[[1]]
  )
  
  # print(data.frame(sitecount = as.vector(out$sitecount_bytype), out$results_overall[ , c("typeofsite", "pop")]))
  
  out$results_overall = out$results_bytype # overall is the name needed in ejam2excel() etc.
  out$results_overall$ejam_uniq_id <- out$types
  names(out$results_overall) <- gsub("ejam_uniq_id", "typeofsite", names(out$results_overall))
  
  print(  
    data.frame(
      sitetype = out$types,
      sitecount = out$sitecount_bytype,
      round(out$results_bytype[, c("pop", names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg), with = FALSE], 1))
  )
  cat("Use  ejam2excel(out)  to view results, and see the types of sites compared, one row each, in the Overall tab\n")
  return(out)
}
#################################################################### #

