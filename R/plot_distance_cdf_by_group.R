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
#' @export
#'
#'
plot_distance_cdf_by_group <- function(results_bybg_people, radius_miles=round(max(x$distance_avg, na.rm = T), 1), 
                                   demogvarname= c(namez$d, namez$d_subgroups),  
                                   demoglabel= NULL,
                                   colorlist=colors()[1:length(demogvarname)], coloroverall="gray") {
 
  
  if (is.null(demoglabel) & missing(demogvarname)) {
    demoglabel <- c(namez$d_friendly, namez$d_subgroups_friendly)
  }
  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
  
  x <- results_bybg_people # not a full slow copy... done by reference using data.table::
  data.table::setorder(x, distance_avg) 
  x[ , distance_avg := min(distance_avg, na.rm = TRUE), by = "bgid"]
  x <- unique(x, by = "bgid")
  x[ , overall := 1]
  demogvarname <- c("overall", demogvarname)
  countvarname <- gsub("^min$","mins", gsub("pct","",demogvarname)) # this will not work for other indicator names, only those like pctlowinc as percentage and lowinc as count
  x[ , overall := pop] # the count not the percent
  demoglabel <- c("Everyone", demoglabel)
  # can drop all but count columns
  # x[ , ]
  warning('function not fully working yet')
# for each demog group, calculate the cumulative share of all people in the group, as distance increases among all the blockgroups
  # what is the most efficient way to do this  ?
  # we actually can just use counts that are in x, not calc counts from pop * pct
   
  # x[is.infinite(.SD), ]
 
  cumdata <- x[ , .(
    
    # collapse::fcumsum(pop *  .SD,  fill=TRUE) / sum(pop *  .SD,  na.rm = TRUE) ,  # if .SDcols=demogvarname
    collapse::fcumsum(  .SD,  fill=TRUE) / sum(  .SD,  na.rm = TRUE) ,   # if .SDcols=countvarname
     
    dist = distance_avg
    ),
    # .SDcols = countvarname] 
   .SDcols =  demogvarname]
  # names(cumdata)
  # [1] "overall"         "Demog.Index"     "pctlowinc"       "pctmin"          "pctlths"         "pctlingiso"      "pctunder5"      
  # [8] "pctover64"       "pctunemployed"   "pctnhwa"         "pcthisp"         "pctnhba"         "pctnhaa"         "pctnhaiana"     
  # [15] "pctnhnhpia"      "pctnhotheralone" "pctnhmulti"      "dist"           
  # colnames(cumdata) <- c(demogvarname,'dist') # they are already named like this automatically by data.table
  
  plot(cumdata$dist, 100 * cumdata[ , overall], 
       col = coloroverall, 
       main = "Share of each Demographic Group Residing at Various Distances from Facilities",
       xlab = "Living within X miles of facilities", 
       ylab= paste0("% of all residents within ", radius_miles," miles"),
       ylim=c(0,100))
  
    for (i in 2:length(demogvarname)) {
      
      
      # distance_cdf_by_group_plot  is not written in a way that makes it easy to vectorize, so this could be rewritten

      
      # remove duplicated blockgroups, since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
    

      data.table::setDF(cumdata)
      points(cumdata$dist, 100 *  cumdata[ , i], col=colorlist[i-1])
    }
  legend("topleft", legend =  demoglabel , fill = c(coloroverall, colorlist))
  
  # print(  distance_by_groups(x ) )
  invisible(cumdata)
  }
  
  
#' distance_cdf_by_group_plot - SLOW - needs to be optimized
#' Plot a graphic showing cumulative shares of ONE demographic group that are within each distance
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param radius_miles miles radius that was max distance analyzed
#' @param demogvarname name of column in results_bybg_people, e.g., "pctlowinc"
#' @param demoglabel friendly text name for labelling graphic, like "Low income residents"
#' @param color1 color like "red" for demographic group of interest
#' @param color2 color like "gray" for everyone else
#' @seealso [distance_by_group()] [getblocksnearbyviaQuadTree()] for examples
#' @return invisibly returns full table of sorted distances of blockgroups, cumulative count of demog group at that block group's distance, 
#' and cumulative count of everyone else in that block group
#'
distance_cdf_by_group_plot <- function(results_bybg_people, radius_miles=round(max(x$distance_avg, na.rm = T), 1), 
                       demogvarname="Demog.Index", demoglabel=demogvarname,
                       color1="red", color2="black") {
  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
 
   x <- results_bybg_people # not a full slow copy... done by reference using data.table::
  data.table::setorder(x, distance_avg) 
  
  # remove duplicated blockgroups, since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
  x[ , distance_avg := min(distance_avg, na.rm = TRUE), by = "bgid"]
  x <- unique(x, by = "bgid")
  
  # should recode this to use directly the counts of D instead of recreating them via pop * .SD
  
  cumdata <- x[ , .(
    dist = distance_avg, 
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
  
  print(  distance_by_group(x, demogvarname=demogvarname, demoglabel=demoglabel) )
  invisible(cumdata)
}


#' @export
distance_by_group_plots <- plot_distance_cdf_by_group


