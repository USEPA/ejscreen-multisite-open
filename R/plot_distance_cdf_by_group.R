#' plot_distance_cdf_by_group  - SLOW - needs to be optimized
#' CDF Line Plots of cumulative share of each demographic group, within each distance
#' Each groups distribution of distances
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param radius_miles miles radius that was max distance analyzed
#' @param demogvarname names of columns in results_bybg_people, e.g., "pctlowinc"
#' @param demoglabel friendly text names for labelling graphic, like "Low income residents"
#' @param colorlist colors like "red" etc. for the demographic groups of interest
#' @param coloroverall color like "gray" for everyone as a whole
#' @seealso [distance_by_groups()] [ejamit()] for examples
#' @return invisibly returns full table of sorted distances of blockgroups, cumulative count of demog groups at that block group's distance
#' @examples 
#'  y <- ejamit(testpoints_50, radius = 3)
#'  plot_distance_mean_by_group(y$results_bybg_people) # or distance_mean_by_group() synonym
#'  print(distance_by_group(y$results_bybg_people, 
#'    demogvarname = 'pctlowinc', demoglabel = 'Low Income'))
#'  distance_by_group_plots(y$results_bybg_people, 
#'    demogvarname = 'pctlowinc', demoglabel = 'Low Income')
#'  xyz = plot_distance_cdf_by_group(y$results_bybg_people) #  
#'  tail(round(xyz,3))
#'  tail(xyz[xyz$pctwa <= 0.501, ]) #  Median distance to nearest site here 
#'    for White Alone is 2.15 miles, but >60% of Black Alone have a site that close.
#'  tail(xyz[xyz$pctba <= 0.501, ]) #  Median distance to nearest site here 
#'    for Black Alone is 1.85 miles
#'  round(tail(xyz[xyz$dist <=1, ]), 3) #  11% of White have a site within 1 mile, 
#'    compared to 18.7% of Asian who do.
#' @export
#'
#'
plot_distance_cdf_by_group <- function(
    results_bybg_people, 
    radius_miles=round(max(
      results_bybg_people$distance_min_avgperson[!is.infinite(
        results_bybg_people$distance_min_avgperson)], na.rm = T), 1), 
    demogvarname= c( namez$d_subgroups),  # or namez$d 
    demoglabel= NULL,
    colorlist= colorspace::diverging_hcl(length(demogvarname)),  
    coloroverall="black",
    ...) {
  # colors()[!grepl('white',colors())][1:length(demogvarname)],
  
  if (is.null(demoglabel) & !missing(demogvarname)) {
    demoglabel <- fixcolnames(demogvarname, 'r', 'long') # renames those it is able to, using an EJAMejscreenapi function fixcolnames
  }  
  if (is.null(demoglabel) & missing(demogvarname)) {
    demoglabel <- namez$d_subgroups_friendly # c(namez$d_friendly, namez$d_subgroups_friendly)
  }
  
  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
  
  x <- results_bybg_people # not a full slow copy... done by reference using data.table::
  
  # plot is too slow for huge datasets and can just plot a random sample of points if so huge:
  if (NROW(x) > 5000) {
    x <- x[sample(1:NROW(x), 5000, replace = F), ]
    warning('plotting just a random sample of 5,000 of these block groups to show pattern quickly')
  }
  
  # if Inf distance in that min_avgperson column (not sure why it happens), just use distance_min which seems to have valid numbers.
  fixthese = (is.infinite(x$distance_min_avgperson) | is.na(x$distance_min_avgperson))
  if (any(fixthese)) {
    x$distance_min_avgperson[fixthese  ]  <-  x$distance_min[fixthese] 
  }
  data.table::setorder(x, distance_min_avgperson) 
  
  # for a bg near 2+ sites, use the distance that is shorter (the distance to the closest of those sites)
  x[ , distance_min_avgperson := min(distance_min_avgperson, na.rm = TRUE), by = "bgid"] #  very few duplicate bgid values should be here - just when 2 sites near a bg
  # remove duplicated blockgroups now that you saved distance to closest site for each bg. since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
  x <- unique(x, by = "bgid")
  
  # specify demographic groups and reference (overall)
  x[ , overall := 1] # this represents the entire population, or 100 percent of the bg, to compare to x percent in any given subgroup of interest
  demogvarname <- c("overall", demogvarname)
  
  # countvarname <- gsub("^min$","mins", gsub("pct","",demogvarname))
  # this will not work for other indicator names, only those like pctlowinc as percentage and lowinc as count
  # maybe instead use   names_d_count (but note none for Demog.Index, Demog.Index.Supp),   names_d_subgroups_count
  
  x[ , overall := pop] # the count not the percent
  demoglabel <- c("Everyone", demoglabel)
  
  # for each demog group, calculate the cumulative share of all people in the group, as distance increases among all the blockgroups
  # what is the most efficient way to do this  ?
  # we actually can just use counts that are in x, not calc counts from pop * pct
  x[ , pop := as.numeric(pop)]
  # x[ , .SD := lapply(.SD, FUN = as.numeric), .SDcols = demogvarname]  ???
  cumdata <- x[ , lapply(.SD, FUN = function(z) collapse::fcumsum(pop * z, fill=T) / sum(pop * z, na.rm = TRUE)),
                .SDcols = demogvarname]
  cumdata$dist <- x$distance_min_avgperson # has NA values
  
  
  plot(cumdata$dist, 100 * cumdata[ , overall], 
       col = coloroverall, 
       pch = NA_integer_, type = 'l', lty="dotted",
       main = "Share of each Demographic Group Residing at Various Distances from Sites",
       xlab = "Distance from nearest site (for the avg resident in the blockgroup)", 
       ylab = paste0("Of all the residents within ", radius_miles," miles, what % have a site within X miles?"),
       ylim = c(0, 100))
  
  for (i in 2:length(demogvarname)) {
    
    # distance_cdf_by_group_plot  is not written in a way that makes it easy to vectorize, so this could be rewritten
    
    data.table::setDF(cumdata)
    points(cumdata$dist, 100 *  cumdata[ , demogvarname[i]], 
           col=colorlist[i-1],
           pch = c(0:6,15:25, 7:14)[i], # various base R shapes for the points
           ...)
  }
  
  legend("topleft", legend =  demoglabel, lty =  c("dotted", rep("solid", length(colorlist))) , pt.bg = c(coloroverall, colorlist),  col = c(coloroverall, colorlist), pch =  c(NA_integer_, 1:6,15:25, 7:14)[1:length(demogvarname)])
  
  invisible(cumdata)
}
############################################################################################################# # 






#' distance_cdf_by_group_plot - SLOW - needs to be optimized
#' Plot a graphic showing cumulative shares of ONE demographic group that are within each distance
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param radius_miles miles radius that was max distance analyzed
#' @param demogvarname name of column in results_bybg_people, e.g., "pctlowinc"
#' @param demoglabel friendly text name for labelling graphic, like "Low income residents"
#' @param color1 color like "red" for demographic group of interest
#' @param color2 color like "gray" for everyone else
#' @seealso [distance_by_group()] [getblocksnearbyviaQuadTree()] for examples
#' @inherit plot_distance_cdf_by_group examples 
#' @export
#' @return invisibly returns full table of sorted distances of blockgroups, cumulative count of demog group at that block group's distance, 
#' and cumulative count of everyone else in that block group
#'
distance_cdf_by_group_plot <- function(results_bybg_people, radius_miles=round(max(x$distance_min_avgperson, na.rm = T), 1), 
                                       demogvarname="Demog.Index", demoglabel=demogvarname,
                                       color1="red", color2="black") {
  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
  
  x <- results_bybg_people # not a full slow copy... done by reference using data.table::
  data.table::setorder(x, distance_min_avgperson) 
  
  # remove duplicated blockgroups, since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
  x[ , distance_min_avgperson := min(distance_min_avgperson, na.rm = TRUE), by = "bgid"]
  x <- unique(x, by = "bgid")
  
  # should recode this to use directly the counts of D instead of recreating them via pop * .SD
  
  cumdata <- x[ , .(
    dist = distance_min_avgperson, 
    # count_d = pop *   .SD, 
    cumall_d    = collapse::fcumsum(pop *   .SD,       fill=TRUE) / sum(pop *        .SD,  na.rm = TRUE) , 
    cumall_nond = collapse::fcumsum(pop * (1 -   .SD), fill=TRUE) / sum(pop * (1 -   .SD), na.rm = TRUE)),
    .SDcols = demogvarname]
  
  plot(cumdata$dist, 100 * cumdata$cumall_d, 
       col = color1, 
       main = "Share of each Demographic Group Residing at Various Distances from Facilities",
       xlab = "Living within X miles of facilities", 
       ylab= paste0("% of all residents within ", radius_miles," miles"),
       ylim=c(0,100)
  )
  points(cumdata$dist, 100 * cumdata$cumall_nond, col=color2)
  legend("topleft", legend = c(demoglabel, paste0("All other residents")), fill = c(color1, color2))
  cat('This takes a very long time to plot for 1,000 sites, e.g.... please wait... \n\n')
  print(  distance_by_group(x, demogvarname=demogvarname, demoglabel=demoglabel) )
  invisible(cumdata)
}
############################################################################################################# # 


#' distance_by_group_plots or plot_distance_cdf_by_group
#' @export
#' @inherit plot_distance_cdf_by_group
distance_by_group_plots <- function(...) {plot_distance_cdf_by_group(...)}


