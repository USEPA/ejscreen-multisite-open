#' plot_distance_mean_by_group
#' Barplot of Average Proximity to sites, by Demographic Group (vs everyone else)
#' 
#' Note ratio shown is ratio of distance among others to distance in group,
#'   so values below 1 mean the given demographic group lives closer to facilities.
#' @seealso [distance_by_group()]
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param demogvarname vector of column names like "pctlowinc" etc.
#' @param demoglabel vector of labels like "Low Income Residents" etc.
#' @seealso [distance_by_group_plot()]  [plot_distance_cdf_by_group()]
#' @return data.frame with group, ratio, avg_distance_for_group, avg_distance_for_nongroup
#' @inherit plot_distance_cdf_by_group examples 
#' @export
plot_distance_mean_by_group <- function(results_bybg_people, 
                               demogvarname=c(EJAM::names_d, EJAM::names_d_subgroups), # namez$d, namez$d_subgroups),  
                               demoglabel=NULL, graph=TRUE) {
  if (is.null(demoglabel) & missing(demogvarname)) {
    demoglabel <- c(EJAM::names_d_friendly, EJAM::names_d_subgroups_friendly)
    # demoglabel <- c(namez$d_friendly, namez$d_subgroups_friendly)
  }
  dlist <- demogvarname
  x <- list()
  for (i in 1:length(dlist)) {x[[i]] <- distance_by_group(results_bybg_people, dlist[i])}
  x <- do.call(rbind, x)
  x <- matrix(unlist(x), ncol = 2)
  x <- as.data.frame(x, stringsAsFactors = F)
  colnames(x) <- c("avg_distance_for_group","avg_distance_for_nongroup")
  x$group <-  dlist
  rownames(x) <- demoglabel
  x$ratio <- round( x$avg_distance_for_nongroup / x$avg_distance_for_group,3)
  x <- x[ , c("group", "ratio", "avg_distance_for_nongroup", "avg_distance_for_group")]
  # print(x)
  if (graph) {
    # x <- distance_by_groups(out)
    barplot(x$ratio, names.arg = substr(rownames(x),1,13), cex.names = 0.6, 
            main = "Ratio of avg distance from facilities among non-group vs group residents" )
    abline(h=1,col="gray")
  }
  invisible(x)
}
################################################################################# # 


#' distance_mean_by_group or plot_distance_mean_by_group
#' @inherit plot_distance_mean_by_group
#' @inherit plot_distance_cdf_by_group examples 
#' @export
distance_mean_by_group <- function(...) {plot_distance_mean_by_group(...)}

################################################################################# # 


#' distance_by_group
#' Get average distance for ONE demographic group versus everyone else
#' 
#' @details  
#'  Note on Avg Distance and range of distances in each Demog group, & %D as function of distance:
#'  
#'  We have info on each blockgroup near each site, which means some small % of those bgs are duplicated in this table:
#'  
#'     results_bybg_people
#'     
#'  Mostly we want overall (not by site) to know avg and cum distrib of distances in each demog,
#'     
#'  (and also %D as a function of continuous distance),
#'  
#'  and for those stats we would want to take only unique blockgroups from here, 
#'  using the shorter distance, so the distribution of distances does not doublecount people.
#'  
#' But we might also want to see that distribution of distances by D for just 1 site? 
#' 
#' And we might also want to see the %D as a function of continuous distance at just 1 site? 
#' 
#' So to retain flexibility doaggregate() reports all instances of blockgroup-site pairings.
#'  
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param demogvarname e.g., "pctlowinc"
#' @param demoglabel e.g., "Low Income Residents"
#' @seealso [plot_distance_mean_by_group()]  
#' @inherit plot_distance_cdf_by_group examples 
#' @export
#' @return list of 2 numbers: avg_distance_for_group and avg_distance_for_nongroup
#'
distance_by_group <- function( results_bybg_people, demogvarname="Demog.Index", demoglabel=demogvarname) {
  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
  # remove duplicated blockgroups, since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
  results_bybg_people[ , distance_avg := min(distance_min_avgperson, na.rm = TRUE), by = "bgid"] # bug? they seem to be identical so taking min does nothing here???
  results_bybg_people <- unique(results_bybg_people, by = "bgid")
  results_bybg_people$distance_avg[ is.infinite(results_bybg_people$distance_min_avgperson)]  <- NA
  distance_avg_d    <- results_bybg_people[ , sum(distance_min_avgperson * pop *      .SD,  na.rm = TRUE) / sum(pop *      .SD,  na.rm = TRUE), .SDcols=demogvarname]
  distance_avg_nond <- results_bybg_people[ , sum(distance_min_avgperson * pop * (1 - .SD), na.rm = TRUE) / sum(pop * (1 - .SD), na.rm = TRUE), .SDcols=demogvarname]
  # cat(
  #   round( distance_avg_d,   3), " vs ", 
  #   round( distance_avg_nond,3),
  # " is average distance among ", demoglabel, " vs others." , "\n") 
  invisible(list(
    avg_distance_for_group     = distance_avg_d, 
    avg_distance_for_nongroup = distance_avg_nond))
}


