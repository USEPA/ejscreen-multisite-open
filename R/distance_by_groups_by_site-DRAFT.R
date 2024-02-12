
#' Ratios at each site, of avg dist of group / avg dist of everyone else near site
#' 
#' Like [distance_by_group()] but for multiple sites - DRAFT FUNCTION
#'
#' @param bybg such as [ejamit()]$results_bybg_people
#'
#' @return table of ratios, one col per site, one row per indicator
#' @seealso [plot_distance_cdf_by_group()] [plot_distance_mean_by_group()] [distance_by_group()] [distance_mean_by_group()]
#' @example distance_by_group_by_site(testoutput_ejamit_10pts_1miles$results_bybg_people)
#' 
#' @export
#' 
distance_by_group_by_site <- function(bybg) {

  # out = ejamit(testpoints_100[1:10, ], radius = 6.2)
  # bybg = out$results_bybg_people[ejam_uniq_id < 11, ]
  
  ids <- bybg[ , unique(ejam_uniq_id)]
  x = list()
  for (i  in 1:length(ids)) {
    
    y = plot_distance_mean_by_group(bybg[ejam_uniq_id ==  ids[i]], graph = FALSE)
    
    # print(x[[i]])  # > str(x[[1]])
    # 'data.frame':	18 obs. of  6 variables:
    #   $ group                    : chr  "Demog.Index" "Demog.Index.Supp" "pctlowinc" "pctlingiso" ...
    # $ nearest                  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    # $ nearer                   : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    # $ ratio                    : num  1.08 1.06 1.09 1.1 1.07 ...
    # $ avg_distance_for_group   : num  3.49 3.49 3.5 3.67 3.54 3.53 3.41 3.26 3.11 3.48 ...
    # $ avg_distance_for_nongroup: num  3.24 3.3 3.21 3.33 3.31 3.3 3.31 3.34 3.43 3.27 ...
   
    x[[i]] <- y$ratio
    names(x[[i]]) <- rownames(y)
    
  }
  x = cbind.data.frame(x)
  colnames(x) <- ids
  cat("Ratios at each site, of avg dist of group / avg dist of everyone else near site: \n\n")
   return(x)
}
