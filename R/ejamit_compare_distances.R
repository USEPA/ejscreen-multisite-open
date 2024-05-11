

#' compare ejamit results for more than one radius
#' run ejamit() once per radius, get a table with a row per radius
#' 
#' @param sitepoints like for [ejamit()]
#' @param radii vector of radius values like 1:3 for [ejamit()]
#' @param ... passed to [ejamit()]
#' @param quiet  passed to [ejamit()]
#' @param silentinteractive  passed to [ejamit()]
#' @param myvars for plot, see default value
#' @param ylab for plot, see default value
#' @param ylim for plot, see default value
#' @param n how many of the indicators to report on (printed to console),
#'   when reporting which indicators most strongly increase as radius decreases.
#' 
#' @examples 
#'   radii <- c(1,2,3,6,10)
#'   radii <- c(1, 10)
#'   pts <- testpoints_100
#'   pts <- testpoints_10
#'   x <- ejamit_compare_distances(pts, radii = radii)
#'   
#'   names(x) <- fixcolnames(names(x),"r","shortlabel")
#'   
#'   
#'   
#'   
#' @seealso [plot_distance_by_pctd()], [distance_by_group()] and several other related functions
#' @return data.table like ejamit()$results_overall but with one row
#'   per radius.
#'   
#' @export
#'
ejamit_compare_distances <- function(sitepoints, radii = c(1,2,3), quiet = TRUE, silentinteractive = TRUE, 
                                     plot = TRUE, 
                                     myvars = names_d_subgroups_ratio_to_state_avg, 
                                     ylab = "Ratio of Avg. within X miles to Avg. Statewide or Nationwide",
                                     ylim = c(0, 5),
                                     n = 1,
                                     ...) {
  
  # Options for what format to return 
  # from this function or similarly ejamit_compare_types_of_places()
  #  (you can convert between these but the code is a little awkward, so one should be the primary output format, and helper functions might reformat between these)
  #
  # results_bydistance table,
  # like results_overall but 1 row per distance.
  # Creating that is a bit awkward, via rbindlist(a list of the extracted list for all i of out[[i]]$resultsoverall )
  # and 
  # results_bybg_bysite, 1 table already covers all the distances and all the sites.
  #
  # plus one of these formats for the rest:
  #
  # A) MATCHES ejamit() FORMAT, but for one DISTANCE at a time:
  # LIST OF DISTANCE-SPECIFIC ejamit() lists,
  # A list of objects, one per radius, 
  #   each being exactly like the normal ejamit() output list, 
  #  for that 1 distance.
  #
  # B) Easier way to focus on 1 DISTANCE at a time:
  # LIST OF DISTANCE-SPECIFIC results_bysite TABLES,
  # where ROWS ARE SITES
  #
  # C) Easy way to focus on 1 SITE at a time:
  #  LIST OF SITE-SPECIFIC TABLES, 
  #  where ROWS ARE DISTANCES
  #  for that one site
  
  if (length(radii) > 30 || max(radii, na.rm = T) > 31 || any(!is.numeric(radii)) || any(radii < 0.5)) {
    stop("radii must be numbers between 0.5 and 31, and 30 different radii is the max allowed.")
  }
  
  ################################################################################## #
  # note this overlaps or duplicates code in ejamit() and app_server.R
  #   for data_up_latlon() around lines 81-110 and data_up_frs() at 116-148
  
  # select file interactively in RStudio
  if (missing(sitepoints)) {
    if (interactive() & !silentinteractive & !in_shiny) {
      sitepoints <- rstudioapi::selectFile(caption = "Select xlsx or csv with FIPS column of Census fips values", path = '.' )
      # that returns the path to the file
      # Do not interactively ask for radii
      sitepoints <- latlon_any_format(sitepoints) # read file and infer colnames with lat lon
    } else {
      if (shiny::isRunning()) {
        warning("sitepoints (locations to analyze) is missing but required.")
        return(NULL)
        
      } else {
        stop("sitepoints (locations to analyze) is missing but required.")
      }
    }
  }
  
  # Do these steps here even though ejamit() has the same code, so it won't have to happen once per loop on radius:
  
  # If user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
  #  by using sitepoints <- latlon_from_anything(sitepoints) which gets done by getblocksnearby()
  sitepoints <- latlon_from_anything(sitepoints)
  stopifnot(is.data.frame(sitepoints), "lat" %in% colnames(sitepoints), "lon" %in% colnames(sitepoints), NROW(sitepoints) >= 1, is.numeric(sitepoints$lat))
  
  ## check for ejam_uniq_id column; warn and add if not present
  if (!("character" %in% class(sitepoints)) & !c('ejam_uniq_id') %in% names(sitepoints)) {
    # message('sitepoints did not contain a column named ejam_uniq_id, so one was added')
    sitepoints$ejam_uniq_id <- seq.int(length.out = NROW(sitepoints))
  }
  ################################################################################## #
  
  z = list()
  for (i in seq_along(radii)) {
    
      z[[i]] <- EJAM::ejamit(sitepoints = sitepoints, radius = radii[i], 
                             quiet = quiet, silentinteractive = silentinteractive, 
                             ...)$results_overall
  }
  z = rbindlist(z)
  
  # display some results in RStudio console
  shown = data.frame(round(t(z)[names_these_ratio_to_state_avg, ], 1))
  colnames(shown) <- radii
  rownames(shown) <- fixcolnames(rownames(shown), 'r', 'shortlabel')
  cat("\n")
  print(shown)
  
  if (plot) {
    cat("\n Indicators that most strongly get larger as you get closer: \n")
    print(distance_comparison_ejamit2plot(z,
                                          myvars = myvars,
                                          ylab = ylab,
                                          ylim = ylim,
                                          n = n
    ))
  }
  # return the actual full set of results (either one site or overall)
  return(z)
}
#################################################################### #


distance_trends = function(x, radii, n = 1) {
  
  # radii <- x$radius.miles
  
  # REPORT STRONGEST TREND
  # fit line to points and report which has the most negative slope, e.g.
  # cat("Indicators that most strongly get larger as you get closer: \n")
  slopes = coef(lm( as.matrix(x) ~ radii ))[2, ]
  topn = head(sort(slopes), n)
  topn = fixcolnames(names(topn), "r", "long")
  
  return(topn)
}
#################################################################### #


#' plot indicators as a function of distance from point(s)
#' plot results of ejamit_compare_distances()
#'
#' @param outall output of [ejamit_compare_distances()]
#' @param myvars optional, see [ejamit_compare_distances()]
#' @param ylab  optional, see [ejamit_compare_distances()]
#' @param ylim  optional, see [ejamit_compare_distances()]
#' @param n  optional, see [ejamit_compare_distances()]
#'
#' @return text vector length n, explaining which indicators most strongly
#'   increase as you get closer to the site(s)
#'   
#' @export
#'
distance_comparison_ejamit2plot <- function(outall, 
                                            myvars = names_d_subgroups_ratio_to_state_avg, 
                                            ylab = "Ratio of Avg. within X miles to Avg. Statewide or Nationwide",
                                            ylim = c(0, 5),
                                            n = 1) {
  
  outall <- data.frame(outall)
  radii <- outall$radius.miles
  
  x <- outall[, myvars]
  
  # REPORT STRONGEST n TREND(s)
  
  topn = distance_trends(x = x, radii = radii, n = n)
  
  # PLOT
  
  mycolors = palette.colors(NCOL(x))
  
  for (i in 1:(NCOL(x))) {
    if (i == 1) {
      plot(x = radii, y = x[, i], type = "b", col = mycolors[i],
           # x labels should be radii
           xlab = "Distance (radius) in miles",
           ylab = ylab,
           xlim = c(0, max(radii)),
           ylim = ylim)
    } else {
      points(x = radii, y = x[, i], type = "b", col = mycolors[i])
    }
  }
  
  legend("topleft", legend = paste0("", fixcolnames(names(x), "r", "shortlabel"),""), fill = palette.colors(NCOL(x)))
  abline(h = 1, col = "lightgray")
  
  return(
    topn
  )
}
#################################################################### #
