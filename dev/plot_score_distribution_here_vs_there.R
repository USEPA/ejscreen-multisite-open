#' plot_hist_score_across_places_by_zone
#' @importFrom ggblanket gg_density
#' @param scores_name e.g., "% low income"
#' @param scores_colname e.g., "pctlowinc"
#' @param scores_refzone Optional vector of scores in the reference zone,
#'   but default is nationwide based on scores_colname column of US blockgroupstats
#' @param scores_myzone vector of scores in places analyzed such as near these facilities
#' @param refzone_name e.g., "in USA overall"
#' @param myzone_name e.g., "Near these sites"
#' @param ... passed to  ggblanket::gg_density()
#' @return plot
#' @export
#'
#' @examples \dontrun{
#'   dcol = "pctlths"
#'   dlab = "% less than high school"
#' out <- testoutput_ejamit_1000pts_1miles
#' x <- out$results_bysite[, dcol, with = FALSE]
#'   plot_score_distribution_here_vs_there(scores_myzone = x, scores_name = dlab,
#'     scores_colname = dcol)
#'   
#'   dcol = "traffic.score"
#'   dlab = "Traffic Scores"
#'   out <- testoutput_ejamit_1000pts_1miles
#'   x2 <- out$results_bysite[, dcol, with = FALSE]
#'   plot_score_distribution_here_vs_there(scores_myzone = x2, scores_name = dlab, 
#'     scores_colname = dcol, x_trans = "log", y_limits = c(0, 0.3))
#' }
plot_score_distribution_here_vs_there <- function(
    scores_name = "% low income",
    scores_colname = "pctlowinc",
    scores_refzone = EJAM::blockgroupstats[, scores_colname, with = FALSE],
    scores_myzone, 
    refzone_name = "in USA overall",
    myzone_name = "Near these sites",
    ...
) {
  #   # 1 score (like names_e[1]),
  #   # across block groups, or across sites,
  #   #  in nearby zone vs in reference zone like USA 
  
  # scores_refzone <- EJAM::blockgroupstats[, scores_colname, with=FALSE]
  # out <- ejamit(testpoints_1000, cutoff = 1)
  # scores_myzone <- out$results_bysite[, scores_colname, with=FALSE]
  score <- zone <- NA # to avoid warnings using them unquoted
  scores_myzone  <- data.frame(score = scores_myzone,  zone = myzone_name)
  scores_refzone <- data.frame(score = scores_refzone, zone = refzone_name)
  scores_all <- rbind(scores_refzone, scores_myzone)
  colnames(scores_all) <- c("score", "zone")
  
  # ggplot2::ggplot(data = scores_all, mapping = aes(x = score, colour = zone)) ? something like that
  
   ggblanket::gg_density(
    data = scores_all, 
    x = score, # scores_colname,  # seems like this stores but is not equal to the colname
    x_title = scores_name, 
    col = zone, 
    title = paste0(scores_name, " among these sites vs ", refzone_name),
    ...
  )
  # print(g)
  # return(g)
}


#  density pdf of 1 E or of Distance, 
#  for 1+ demogr groups (e.g., nearby, or in US), 
# and maybe for all groups together (e.g., nearby, or in USA overall) 


# plot_pdf_score_across_places_by_score <- function() {
#   # less useful
# }

# plot_pdf_score_across_pop_by


# plot_pdf_scores_across_people 
# plot_pdf_e_by_demog
