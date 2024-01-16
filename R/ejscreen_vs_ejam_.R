
# List of functions ####

# ejscreen_vs_ejam()
# ejscreen_vs_ejam_alreadyrun()
# ejscreenapi2ejam_format()
# ejscreen_vs_ejam_summary()
# ejscreen_vs_ejam_summary_quantiles()
# ejscreen_vs_ejam_see1()
# ejscreen_vs_ejam_see1map()


######################################################################### # 



############################################################ #


#' EJAM/EJSCREEN comparisons - compare EJScreen API vs EJAM stats near site(s)
#'
#' @param latlon data.table or data.frame with colnames lat and lon, 
#'   and one row per site
#' @param radius in miles, used in ejamit() and ejscreenapi_plus()
#' @param x100fix whether to multiply x100 the names_d and names_d_subgroups 
#'   indicator scores to convert fractions 0 to 1 into percentages of 0 to 100,
#'   prior to rounding and reporting EJAM results here. 
#' @param x100varnames optional, if x100fix=T, a vector of colnames of x$EJAM to convert from 
#'   being scaled as 0 to 1 into rescaled values of 0 to 100, because some 
#'   outputs of EJSCREEN were reported as percentages 0 to 100 but as 0 to 1 in EJAM.
#' @param save_when_report see ejscreenapi_plus()
#' @param report_every_n see ejscreenapi_plus()
#' @param calculate_ratios passed to ejscreenapi_plus() and [ejamit()]
#' @param ... passed to ejamit() as any additional parameters,
#'    like include_ejindexes = FALSE
#'
#' @return a list of data frames, with names 
#'   EJSCREEN, EJAM, EJSCREEN_shown, EJAM_shown, same_shown, 
#'   ratio, diff, absdiff, pctdiff, abspctdiff
#'   
#'   diff is EJAM - EJSCREEN
#'   
#'   ratio is EJAM / EJSCREEN
#'   
#'   pctdiff is ratio - 1
#'   
#'   abs is absolute value
#'   
#'   For each data.frame, colnames are indicators like pop, blockcount_near_site, etc.
#'   and rows represent sites analyzed.
#'   
#' @export
#'
#' @examples
#'  \dontrun{
#'    pts <- testpoints_100[1:5, ]
#'    
#'   # This step can take a long time, almost 1 minute / 20 points, as it uses the EJScreen API:
#'   #z <- ejscreen_vs_ejam(
#'     testpoints_100[27, ], radius = 3, nadrop = T, include_ejindexes = TRUE)
#'   z <- ejscreen_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
#'   
#'   # see one site
#'   ejscreen_vs_ejam_see1(z, mysite = 1)
#'   
#'    # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, names_pctile, names_state_pctile)
#'    z$EJAM[z$EJAM$rname %in% keyreportnames &  z$same_shown, ]
#'    z[z$rname %in% keyreportnames & !z$same_shown & !is.na(z$EJSCREEN), ]
#'   
#'    # Reported (rounded) numbers match:
#'    z[z$same_shown , -1]
#'    # Reports disagree: 
#'    #  (and not just because of percentages being of 100 vs of 1.00)
#'    z[!z$same_shown & !is.na(z$EJSCREEN) & z$ratio != 0.01, -1] 
#'    # Reports disagree if percentages reported as 0-100 vs fractions 0-1.00
#'    z[z$ratio == 0.01 & !is.na(z$ratio), -1]
#'    
#'    
#'    
#'   }
#' @seealso [ejscreen_vs_ejam_alreadyrun()]
#' 
ejscreen_vs_ejam <- function(latlon, radius = 3, nadrop = FALSE,
                                save_when_report = TRUE, report_every_n = 250, # save every 10 minutes or so
                                calculate_ratios = FALSE, include_ejindexes = TRUE,
                                x100fix = TRUE, 
                                x100varnames = c(
                                  names_d, names_d_avg, names_d_state_avg,
                                  names_d_subgroups, names_d_subgroups_avg, names_d_subgroups_state_avg,
                                  "pctdisability",  "p_own_occupied", 
                                  "pctunder18", "pctover17", "pctmale", "pctfemale"), ...) {
  
  api1 <- ejscreenapi_plus(latlon, radius = radius, 
                           save_when_report = save_when_report, report_every_n = report_every_n, 
                           calculate_ratios = calculate_ratios)
  # or api1 <- ejscreenit(latlon, radius = radius)$table
  if (missing(latlon)) {latlon <- api1[ , c('id', 'lat', 'lon')]} # in case provided interactively above
  ejam1 <- ejamit(latlon, radius = radius, calculate_ratios = calculate_ratios, include_ejindexes = include_ejindexes, ...)$results_bysite
  z <- ejscreen_vs_ejam_alreadyrun(api1, ejam1, nadrop = nadrop, x100fix = x100fix)
  invisible(z)
}
############################################################ #


#' EJAM/EJSCREEN comparisons - compare EJScreen API vs EJAM stats near site(s) (if results already run)
#'
#' @param apisite table output of ejscreenit()$table,
#'   or ejscreenapi_plus()
#' @param ejamsite table output of ejamit()$results_bysite
#' @param nadrop optional, whether to drop indicators for which EJScreen API returns NA
#' 
#' @param x100fix optional, whether to multiply x100 the names_d and names_d_subgroups 
#'   indicator scores to convert fractions 0 to 1 into percentages of 0 to 100,
#'   prior to rounding and reporting EJAM results here. 
#'   
#' @param x100varnames optional, if x100fix=T, a vector of colnames of x$EJAM to convert from 
#'   being scaled as 0 to 1 into rescaled values of 0 to 100, because some 
#'   outputs of EJSCREEN were reported as percentages 0 to 100 but as 0 to 1 in EJAM.
#'
#' @return prints summary to console, but returns invisible a list of data frames, with names 
#'   EJSCREEN, EJAM, EJSCREEN_shown, EJAM_shown, same_shown, ratio, etc.
#'   
#'   For each data.frame, colnames are indicators like pop, blockcount_near_site, etc.
#'   and rows represent sites analyzed.
#'   
#' @export
#' @examples 
#'   blah = ejscreen_vs_ejam_alreadyrun(
#'     apisite = testoutput_ejscreenapi_plus_5,
#'     ejamsite = ejamit(testpoints_5, radius = 1, include_ejindexes = TRUE)$results_bysite)
#'   ejscreen_vs_ejam_see1(blah, mysite = 1)
#'  \dontrun{
#'  
#'  # requires data.table
#'  
#'  # analyze point(s) in both EJScreen and EJAM
#'   pts <- testpoints_100[1:5, ]
#'   #z <- ejscreen_vs_ejam(testpoints_100[27, ], radius = 3, include_ejindexes = TRUE)
#'   
#'   # just 1 point
#'   # z <- ejscreen_vs_ejam(pts[5, ], radius = 3, include_ejindexes = TRUE)
#'   
#'   # multiple points
#'   # This step can take a long time, almost 1 minute per 20 points, as it uses the EJScreen API:
#'   z <- ejscreen_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
#'     
#'    # same thing but step by step
#'    api1 <- ejscreenit(pts, radius = 3)
#'    api1 <- api1$table[5, ]
#'    ejam1 <- ejamit(pts, radius = 3, include_ejindexes = TRUE)
#'    ejam1 <- ejam1$results_bysite[5, ]
#'    zzz <- ejscreen_vs_ejam_alreadyrun(api1, ejam1)
#'    
#'    # to keep it as a list of data.frames but just 1 row each now:
#'    zlist1 <- lapply(z, function(x) x[myrow, ])
#'    names(zlist1); sapply(zlist1, dim)
#'    
#'    # look at a table about just 1 site among those analyzed, e.g., the first one:
#'    myrow <- 1
#'    z1 <- data.frame(sapply(z, function(x) x[myrow, ]))
#'    # but that creates a data.frame where each column is a list... 
#'    
#'    
#'    
#'    
#'    # **** WORK IN PROGRESS ... *** !!!!!  
#'    
#'    
#'    
#'    
#'    
#'    # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, names_pctile, names_state_pctile)
#'    rname <- colnames(z$EJAM)
#'    z1[rname %in% keyreportnames &  z1$same_shown, ]
#'    z1[rname %in% keyreportnames & !z1$same_shown & !is.na(z1$EJSCREEN), ]
#'   
#'    # Reported (rounded) numbers match:
#'    z[z$same_shown , ]
#'    # Reports disagree: 
#'    #  (and not just because of percentages being of 100 vs of 1.00)
#'    z[!z$same_shown & !is.na(z$EJSCREEN) & z$ratio != 0.01, ] 
#'    # Reports disagree if percentages reported as 0-100 vs fractions 0-1.00
#'    z[z$ratio == 0.01 & !is.na(z$ratio), ]
#'    
#'    
#'    # site number 1
#'    z1 <- sapply(z, function(x) (x[1,]))
#'    # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, names_pctile, names_state_pctile)
#'    z1[z1$rname %in% keyreportnames &  z1$same_shown, ]
#'    z1[z1$rname %in% keyreportnames & !z1$same_shown & !is.na(z1$EJSCREEN), ]
#'    
#'    # Reported (rounded) numbers match:
#'    z1[z1$same_shown , ]
#'    # Reports disagree: 
#'    #  (and not just because of percentages being of 100 vs of 1.00)
#'    z1[!z1$same_shown & !is.na(z1$EJSCREEN) & z1$ratio != 0.01, ] 
#'    # Reports disagree if percentages reported as 0-100 vs fractions 0-1.00
#'    z1[z1$ratio == 0.01 & !is.na(z1$ratio), ]
#'    
#'    # Comparing across sites:
#'    
#'    # .... to be continued ...
#'    
#'   }
#'    
#' @seealso [ejscreen_vs_ejam()]
#' 
ejscreen_vs_ejam_alreadyrun <- function(apisite, ejamsite, nadrop = FALSE, 
                                           x100fix = TRUE, 
                                           x100varnames = c(
                                             names_d, names_d_avg, names_d_state_avg,
                                             names_d_subgroups, names_d_subgroups_avg, names_d_subgroups_state_avg,
                                             "pctdisability",  "p_own_occupied", 
                                             "pctunder18", "pctover17", "pctmale", "pctfemale")) {
  
  # requires data.table
  # radius <- 1
  # pts <- testpoints_100[1:5, ]
  #    
  # apix <- ejscreenit(pts, radius = radius)
  # apix <- apix$table
  # 
  # ejamx <- ejamit(pts, radius = radius, include_ejindexes = TRUE)
  # ejamx <- ejamx$results_bysite
  # apisite <- apix
  # ejamsite <- ejamx
  
  ## all.equal(names(apix), names(ejamx))
  ## [1] TRUE
  
  n <- NROW(apisite)
  
  if (!is.data.frame(apisite) | NROW(apisite) != n) {
    warning("apisite must be a data.frame of ", n, " rows")
    return(NULL)  
  }
  if (!is.data.frame(ejamsite) | NROW(ejamsite) != n) {
    warning("ejamsite must be a data.frame of ", n, " rows")
    return(NULL)
  }
  
  apisite <- ejscreenapi2ejam_format(
    apisite, 
    fillmissingcolumns = TRUE, 
    ejamcolnames = colnames(ejamsite)
  )
  
  # return only numeric columns, to simplify comparisons and formatting
  setDF(apisite)
  setDF(ejamsite)
  
  apisite   <- apisite[ , !(names(apisite) %in% c('ST', 'statename', "REGION", "EJScreen Report", "EJScreen Map", "ECHO report"))]
  ejamsite <- ejamsite[ , !(names(ejamsite) %in% c('ST', 'statename', "REGION", "EJScreen Report", "EJScreen Map", "ECHO report"))]
  
  if (x100fix) {
    ejamsite[ , x100varnames] <- 100 * ejamsite[ , x100varnames]
  }
  
  EJSCREEN_shown <- table_round(apisite)  # SLOW! ***
  EJAM_shown     <- table_round(ejamsite)  # SLOW! ***
  
  apisite <- data.frame(sapply(apisite, as.numeric))
  ejamsite <- data.frame(sapply(ejamsite, as.numeric))
  EJSCREEN_shown <- data.frame(sapply(EJSCREEN_shown, as.character))
  EJAM_shown <- data.frame(sapply(EJAM_shown, as.character))
  
  z <- list(
    EJSCREEN = apisite,
    EJAM = ejamsite,
    EJSCREEN_shown = EJSCREEN_shown,
    EJAM_shown = EJAM_shown
  )
  
  z$same_shown <- data.frame(z$EJAM_shown == z$EJSCREEN_shown)
  
  z$ratio <- z$EJAM / z$EJSCREEN   # prettyNum(z$ratio, digits = 2, format = 'f')
  z$diff <- z$EJAM - z$EJSCREEN
  z$absdiff <- abs(z$diff)
  z$pctdiff <- z$ratio - 1
  z$abspctdiff <- abs(z$pctdiff)   
  
  # rname <- colnames(ejamsite) # same as colnames(z$EJAM) OR  names(z$EJAM)
  # longname <- fixcolnames(colnames(ejamsite), 'r', 'long')
  
  ejscreen_vs_ejam_summary(z)
  
  invisible(z) 
}
############################################################ #



#' EJAM/EJSCREEN comparisons - Convert output of ejscreenapi_plus to format of ejamit table of sites
#' Makes it easier to compare EJScreen and EJAM results
#' @param ejscreenapi_plus_out results of ejscreenapi_plus() or also 
#'   could be results of ejscreenit()$table even though the colnames differ,
#'   because they get converted here in that case.
#' @param fillmissingcolumns optional. set to TRUE if you want the output
#'   to have exactly all the same columns as the EJAM table would, and
#'   fill in with NA values all the columns not provided by EJScreen API.
#' @param ejamcolnames optional. if specified as vector of colnames, it 
#'   overrides the assumed colnames that would have been taken to be
#'   colnames(testoutput_ejamit_10pts_1miles$results_bysite). 
#'   Any colnames you specify here will be the colnames of the output
#'   if fillmissingcolumns = TRUE, or else those not in names(ejscreenapi_plus_out)
#'   will be omitted.
#' @return A data.table not just data.frame, with some or all of the columns
#'   found in output of ejamit()$results_bysite
#' @seealso [ejscreen_vs_ejam()]
#' @export
#'
#' @examples
#' \dontrun{
#'    x <- ejscreenapi_plus(testpoints_10[1:2, ], radius = 1)
#'    y <- ejscreenapi2ejam_format(x)
#'    ejamvars <- names(testoutput_ejamit_10pts_1miles$results_bysite)
#'    all.equal(
#'      names(y), 
#'      ejamvars[ejamvars %in% names(y)]
#'   )
#'   
#'   z <- ejscreenapi2ejam_format(x, fillmissingcolumns = T)
#'   all.equal(names(z), ejamvars) 
#'   }
#'   
ejscreenapi2ejam_format <- function(ejscreenapi_plus_out, fillmissingcolumns = FALSE, ejamcolnames=NULL) {
  if (!is.null(ejamcolnames)) {
    ejamvars <- ejamcolnames
  } else { 
    ejamvars <- colnames(testoutput_ejamit_10pts_1miles$results_bysite)
  } 
  x <- ejscreenapi_plus_out
  # Should already be rname format, but 
  # just in case, try to convert as if they were long names as in output of ejscreenit()
  colnames(x) <- fixcolnames(colnames(x), "long", "r")
  #manually fix a couple we know differ
  colnames(x) <- gsub("EJScreenPDF", "EJScreen Report", colnames(x)) 
  colnames(x) <- gsub("EJScreenMAP", "EJScreen Map", colnames(x))
  
  # Remove columns from API output that are not in the EJAM output format 
  
  keepthese <- which(colnames(x) %in% ejamvars)
  setDF(x) # but should already be a data.frame not data.table if coming from ejscreenapi_plus() 
  x <- x[ , keepthese]
  
  if (fillmissingcolumns) {
    # could be done more efficiently - this was drafted quickly
    y <- data.frame(matrix(NA_integer_, nrow = NROW(x), ncol = length(ejamvars)))
    colnames(y) <- ejamvars
    sharednames <- intersect(colnames(x) , ejamvars)
    y[ , sharednames] <- x[ , sharednames]
    x <- y
    setDT(x)
  } else {
    sharednames_in_ejam_order <- ejamvars[ejamvars %in% names(x)]
    setDT(x)
    setcolorder(x, sharednames_in_ejam_order)
  }
  return(x)
  
  ### test it:
  # x <- ejscreenapi_plus(testpoints_10[1:2, ], radius = 1)
  # y <- ejscreenapi2ejam_format(x)
  # ejamvars <- names(testoutput_ejamit_10pts_1miles$results_bysite)
  # all.equal(
  #   names(y), 
  #   ejamvars[ejamvars %in% names(y)]
  # )
  
}
############################################################ #



#' EJAM/EJSCREEN comparisons - see summary stats after using ejscreen_vs_ejam()
#'
#' @param z output of ejscreen_vs_ejam()
#' @param myvars optional to check just a subset of the colnames found in z$EJAM and z$EJSCREEN, 
#'   such as myvars = c(names_d, names_d_subgroups) or myvars = grep("pctile", colnames(z$EJAM), value = T)
#' @param tol optional, set this so that results can be said to agree with this tolerance
#'   if they differ by less than tol percent where tol is expressed as a fraction 0 to 1.
#' @param prob optional fraction of 1 representing percentile p to check for absolute percentage differences.
#'   See within.x.pct.at.p.pct.of.sites value that is returned.
#' @param na.rm needs testing, optional
#' 
#'
#' @return A data.frame of summary stats showing counts and percents of analyzed sites (or those with valid data 
#'   that are found in both EJAM and EJSCREEN outputs), indicating how many of the sites
#'   agree between EJSCREEN and EJAM estimates, exactly as reported or within some tolerance.
#'   Columns include 
#'   
#'    "indicator" (variable name)
#'    
#'    "sites.with.data.ejam" How many of the sites had a value from EJAM for the given indicator?
#'    
#'    "sites.with.data.neither" How many sites had NA from both EJAM and EJSCREEN?
#'   
#'    "sites.with.data.both"
#'   
#'    "sites.agree.rounded" How many sites agree (EJSCREEN vs EJAM) in the value shown on reports?
#'      i.e., the reported, rounded value.
#'   
#'    "sites.agree.within.tol" How many sites agree within tol? (i.e., with tol x 100 percent)
#'    
#'    "pct.of.sites.agree.rounded"  as a percent 0-100% of sites with data
#'    
#'    "pct.of.sites.agree.within.tol"  as a percent 0-100% of sites with data
#'    
#'    "median.abs.diff" Median over sites with data, of the absolute differences, EJAM - EJSCREEN
#'    
#'    "max.abs.diff"
#'    
#'    "mean.pct.diff" Percent difference 0-100% is absolute value of 100*(ratio - 1), and ratio is EJAM/EJSCREEN
#'    
#'    "median.pct.diff" 0-100%
#'    
#'    "max.pct.diff" 0-100%
#'    
#'    "within.x.pct.at.p.pct.of.sites"  X, where EJAM and EJSCREEN agree within X percent 0-100% or better
#'      at prob share of sites. Prob share as used in this last stat should mean prob (e.g. 0.95) share of sites have 
#'      an absolute percentage difference in estimated indicator values that is less than or equal to x 
#'      where x is one of the actual values of abspctdiff found * 100. 
#'      It uses 100 * quantile(y, probs = prob, type = 1)
#'    
#' @examples
#'   dontrun{
#'   pts <- testpoints_n(100, weighting = 'frs')
#'   
#'   # This step can take a long time, almost 1 minute per 20 points, as it uses the EJScreen API:
#'   vs100 <- ejscreen_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
#'   
#'   ejscreen_vs_ejam_see1(vs100, mysite = 1)
#'   vs100$diff$blockcount_near_site
#'   sum100 <- ejscreen_vs_ejam_summary(vs100, tol = 0.01)
#'   s100 <- sum100[ , c(1, 6:12)]
#'   
#'   s100[s100$indicator %in% names_e, ]
#'   s100[s100$indicator %in% names_d, ]
#'   s100[s100$indicator %in% names_these, ]
#'   s100[s100$indicator %in% c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile), ]
#'   
#'   sum100_within5pct <- ejscreen_vs_ejam_summary(vs100, tol = 0.05)
#'   sum100_within5pct[sum100_within5pct$indicator %in% names_these, ][ , c(1, 6:12)]
#'   
#'   ## longer analysis (45 minutes perhaps)
#'   # This step can take a long time, almost 1 minute per 20 points, as it uses the EJScreen API:
#'   # pts <- testpoints_n(1000, weighting = 'frs')
#'   # vs1000pts3miles <- ejscreen_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
#'   # sum_vs1000pts3miles <- ejscreen_vs_ejam_summary(vs1000pts3miles)
#'   
#'   }
#' @export
#'
ejscreen_vs_ejam_summary <- function(z = ejscreen_vs_ejam(), myvars = colnames(z$EJAM), tol = 0.01, prob = 0.95, na.rm = TRUE) {
  
  # tol Set tol so that results are said to agree if they differ by less than tol percent, where tol is a fraction 0 to 1. 
  # z is output of ejscreen_vs_ejam
  # na.rm <- TRUE # to see 0% etc. instead of NA for indicators that could not be compared in at least some sites that may lack data.
  ## for a subset of key indicators:
  # myvars <- c(names_these, names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
  
  z$EJSCREEN             <- z$EJSCREEN[ , myvars]
  z$EJAM                     <- z$EJAM[ , myvars]
  z$EJSCREEN_shown <- z$EJSCREEN_shown[ , myvars]
  z$EJAM_shown         <- z$EJAM_shown[ , myvars]
  z$same_shown         <- z$same_shown[ , myvars]
  z$ratio                   <- z$ratio[ , myvars]
  
  z$diff             <- z$diff[ , myvars]
  z$absdiff       <- z$absdiff[ , myvars]
  z$pctdiff       <- z$pctdiff[ , myvars]
  z$abspctdiff <- z$abspctdiff[ , myvars]
  
  # calculate each as count of sites that agree (and not NA), over count of sites with data ie that are not NA 
  # matrixes of valid/not
  ok_ejscreen <- !is.na(z$EJSCREEN) # dim is 100 x 389, e.g.
  ok_ejam     <- !is.na(z$EJAM)
  # vector of counts of sites, 1 count per indicator
  sites.with.data.ejscreen <- colSums(ok_ejscreen)
  sites.with.data.ejam     <- colSums(ok_ejam)
  sites.with.data.neither  <- colSums(!ok_ejam & !ok_ejscreen)
  sites.with.data.both     <- colSums(ok_ejam & ok_ejscreen)
  
  sites.agree.rounded      <- colSums(z$same_shown, na.rm = na.rm) # only counts if valid
  sites.agree.within.tol   <- colSums(z$abspctdiff < tol, na.rm = TRUE)
  
  pct_agree = data.frame(
    
    indicator = myvars, 
    
    sites.with.data.ejscreen = sites.with.data.ejam,
    sites.with.data.neither = sites.with.data.neither,
    sites.with.data.both    = sites.with.data.both,
    
    sites.agree.rounded    = sites.agree.rounded,
    sites.agree.within.tol = sites.agree.within.tol,
    
    pct.of.sites.agree.rounded    = round(100 * sites.agree.rounded    / sites.with.data.both, 6),
    pct.of.sites.agree.within.tol = round(100 * sites.agree.within.tol / sites.with.data.both, 6), 
    # test/check NA handling there ***
    
    median.abs.diff       = sapply(z$absdiff,     median, na.rm = TRUE),
    max.abs.diff          = sapply(z$absdiff,     max,    na.rm = TRUE), 
    
    mean.pct.diff   = 100 * sapply(z$abspctdiff,  mean,   na.rm = TRUE),
    median.pct.diff = 100 * sapply(z$abspctdiff,  median, na.rm = TRUE),
    max.pct.diff    = 100 * sapply(z$abspctdiff,  max,    na.rm = TRUE),
    
    within.x.pct.at.p.pct.of.sites = 100 * sapply(z$abspctdiff, quantile, type = 1, probs = prob, na.rm = TRUE)
  )
  pct_agree$median.pct.diff <- round(pct_agree$median.pct.diff, 2)
  pct_agree$within.x.pct.at.p.pct.of.sites <- round(pct_agree$within.x.pct.at.p.pct.of.sites, 2)
  pct_agree$max.pct.diff <- round(pct_agree$max.pct.diff, 2)
  rownames(pct_agree) <- NULL
  pct_agree <- pct_agree[order(pct_agree$pct.of.sites.agree.rounded, -pct_agree$within.x.pct.at.p.pct.of.sites, decreasing = T), ]
  
  # if (!missing(z)) {
  usefulvars <- c('blockcount_near_site', 'pop', names_e, names_d,
                  names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
  usefulstats <- c('indicator',
                   #  "sites.with.data.both",
                   #  "sites.agree.rounded", "sites.agree.within.tol",
                   'pct.of.sites.agree.rounded', 'pct.of.sites.agree.within.tol',
                   'median.pct.diff', 'max.pct.diff', 'within.x.pct.at.p.pct.of.sites')
  cat("\n\n")
  print(pct_agree[pct_agree$indicator %in% usefulvars, usefulstats])
  cat(paste0("\n\n Tolerance of ", tol, " was used, so difference of <", tol * 100, "% is within tolerance.\n\n"))
  cat(paste0("Probs (p) used was ", prob, ", so ", prob * 100, "% of sites are within absolute percentage difference reported in output of this function.\n\n" ))
  # }
  
  invisible(pct_agree)
}
######################################################################### # 




#' EJAM/EJSCREEN comparisons - see quantiles over tested sites, of a stat like ratio of EJAM/EJSCREEN
#'
#' @param z output of ejscreen_vs_ejam()
#' @param mystat just one (not more than 1) of these: "ratio", "diff", "absdiff", "pctdiff", "abspctdiff"
#' @param myvars optional vector of indicators, to check just a subset of the colnames found in z$EJAM and z$EJSCREEN, 
#'   such as myvars = c(names_d, names_d_subgroups) 
#'   or myvars = grep("pctile", colnames(z$EJAM), value = T)
#' @param probs optional vector of probabilities 0 to 1 to pass to quantile()
#' @param na.rm optional, leave it as default TRUE
#' @param digits round results to this many digits
#'
#' @return table of percentiles across analyzed locations, of a stat like the ratio of 
#'   EJAM estimates over EJSCREEN estimates, for specified list of indicators like in names_d
#' @export
#'
ejscreen_vs_ejam_summary_quantiles <- function(z, 
                                               mystat = c("ratio", "diff", "absdiff", "pctdiff", "abspctdiff")[1], 
                                               myvars = c('pop', names_these), probs = (0:20) / 20, na.rm = TRUE, digits = 4) {
  round(t(sapply(data.frame(z[[mystat]])[ , myvars, drop = FALSE], quantile, probs = probs, na.rm = na.rm)), digits)
}
######################################################################### # 




#'  EJAM/EJSCREEN comparisons - see results for 1 site after using ejscreen_vs_ejam()
#'
#' @param z output of ejscreen_vs_ejam()
#' @param myvars optional to check just a subset of the colnames found in z$EJAM and z$EJSCREEN, 
#'   such as 
#'   
#'   myvars = colnames(z$EJAM) or 
#'   
#'   myvars = c(names_d, names_d_subgroups) or 
#'   
#'   myvars = grep("pctile", colnames(z$EJAM), value = T)
#'   
#' @param mysite rownumber corresponding to site of interest, among 1:nrow(z$EJAM)
#'
#' @return a table showing one row per indicator, and columns like EJSCREEN, EJAM, ratio, etc.
#' @export
#'
#' @examples 
#'   dontrun{
#'   z <- ejscreen_vs_ejam(testpoints_10, radius = 3)
#'   mysite <- 9
#'   ejscreen_vs_ejam_see1(z, mysite = mysite, myvars = colnames(z$EJAM))[!is.na(z$EJSCREEN[mysite, ]) , 1:2]
#'   }
#'   
ejscreen_vs_ejam_see1 <- function(z, myvars = names_d, mysite = 1) {
  if (!is.list(z) | !("EJAM" %in% names(z))) {stop('z must be output of ejscreen_vs_ejam() or ejscreen_vs_ejam_alreadyrun()')}
  if (length(mysite) > 1 | mysite > NROW(z$EJAM)) {stop('mysite must be the row number of 1 site in the table z$EJAM')}
  if (!all(myvars %in% colnames(z$EJAM))) {stop('myvars must be among colnames of z$EJAM')}
  
  sapply(z, function(x) x[mysite, ])[myvars, ]
  
}
######################################################################### # 



#' EJAM/EJSCREEN comparisons - see map and tables of blocks near a site to explain discrepancy in pop and blocks
#'
#' @param n row number in x$EJAM of site to check
#' @param x results from  x <- ejscreen_vs_ejam(testpoints_10, radius =3, include_ejindexes = TRUE)
#' @param overlay_blockgroups optional, set TRUE to see overlay of boundaries of parent blockgroups,
#'   noting that you have to click to turn off the layer for block point info popups to work
#' @param ... passed to [plotblocksnearby()]
#' @return Just draws map and shows tables and returns output of ejscreen_vs_ejam_see1()
#' @export
#' @examples dontrun{
#'   z <- ejscreen_vs_ejam(testpoints_10, radius = 3, include_ejindexes = TRUE)
#'   ejscreen_vs_ejam_see1map(3, z, overlay_blockgroups = TRUE)
#'  }
#'  
ejscreen_vs_ejam_see1map <- function(n = 1, x, overlay_blockgroups = FALSE, ...) {
  # function to help explain discrepancy in pop and blocks  
  # n is the rownumber of the site analyzed, row of x$EJAM
  # x is from x <- ejscreen_vs_ejam(pts, radius = radius, include_ejindexes = TRUE)
  
  px <- plotblocksnearby(x$EJAM[n, 1:10], radius = radius, overlay_blockgroups = overlay_blockgroups, ...)
  px[blockgroupstats, bgpop := pop, on = 'bgid']
  px[, blockpop := bgpop * blockwt]
  these <- tail(px[order(distance), .(blockid, distance, blockpop)], 10) # see the 10 furthest sites
  these$cumpop = round(rev(cumsum(rev(these$blockpop))), 1) # cumulative starting from furthest site
  print(these)
  
  ejscreen_vs_ejam_see1(x,
                        mysite = n, 
                        myvars = c('pop', 'blockcount_near_site'))[ , c('EJSCREEN', 'EJAM', 'diff')]
}
######################################################################### # 









############################################################ #
############################################################ #
############################################################ #
############################################################ #

if (1 == 0) {
  ### Comparing what the functions provide:
  
  ### ejscreenapi outputs:
  # provides as rnames if ejscreenapi_plus() but as longnames if ejscreenit() 
  
  # all.equal(names(testoutput_ejscreenit_5$table), names(testoutput_ejscreenapi_plus_5))
  # api_table1 = ejscreenapi_plus(testpoints_10)  # provides as rnames
  api_all <- ejscreenit(testpoints_10, radius = 3) # provides as LONG NAMES for convenience, including  "EJScreenPDF" "EJScreenMAP" "Buffer distance (miles)" "% Low Income" etc.   
  api_table <- api_all$table
  # setdiff2(fixcolnames(names(api_table), 'long', 'r'), names(api_table1))
  ### These names are identical once converted from long to r type names.
  names(api_table) <- fixcolnames(names(api_table), 'long', 'r')
  #  all.equal(api_table, api_table1)
  ### [1] "Component “timeSeconds”: Mean relative difference: 0.1214217"
  data.table # :: # 
  setDT(api_table)
  rm(api_all)
  gc()
  
  ### EJAM outputs:
  # provides as rnames
  
  # names(testoutput_ejamit_100pts_1miles$results_bysite)
  ### analyze.stuff # :: # overlaps(names(api_table),  names(testoutput_ejamit_100pts_1miles$results_bysite))
  ### in.a in.a.only overlap in.b.only in.b in.one.only union
  ### unique  340       153     187       103  290         256   443  
  
  ejamout <- ejamit(testpoints_10, radius = 3, include_ejindexes = TRUE, calculate_ratios = T, extra_demog = TRUE) # provides as rnames
  ejam_table <- copy(ejamout$results_bysite)  #  data.table # ::
  rm(ejamout)
  gc()
  
  inboth <- intersect(names(ejam_table), names(api_table))
  onlyejam <- setdiff(names(ejam_table), names(api_table)) # not provided by API, or named differently
  onlyapi <- setdiff(names(api_table), names(ejam_table)) # 
  # > names_d_other_count %in% onlyejam
  #  Both have pop, hhld, nonmins, but
  ## only EJAM has: "povknownratio","age25up","unemployedbase", "pre1960","builtunits"
  
  # missing from API:
  ### ratios for demog subgroups
  ## raw EJ scores
  ### pctiles demog subgroups
  ### avg demog subgroups
  ### count demog subgroups and groups  
  okmissing <- c(
    c(names_d_subgroups_ratio_to_avg, names_d_subgroups_ratio_to_state_avg),
    c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state),
    c(names_d_subgroups_pctile, names_d_subgroups_state_pctile),
    c(names_d_subgroups_avg, names_d_subgroups_state_avg),
    names_d_subgroups_count,
    names_d_count 
  )
  all(okmissing %in% onlyejam)
  setdiff(onlyejam, okmissing)
  
  
  # WILL FIX: 
  
  # FOUND PROBLEM IN ejscreenapi_plus outputs: lat and lon each appear twice, if input is testpoints_10
  
  ################################################################## #
  
  # WILL FIX: 
  
  # ejscreenapi() outputs now differ from those names in testoutput_ejscreenapi_1pts_1miles - rebuild examples in EJAMejscreenapi pkg
  setdiff2(names(x), names(testoutput_ejscreenapi_1pts_1miles))
  
  # [1] "P_NHWHITE"      "P_NHBLACK"      "P_NHASIAN"      "P_NHAMERIND"    "P_NHHAWPAC"     "P_NHOTHER_RACE" "P_NHTWOMORE"    "TOTALPOP"      
  # [9] "P_WHITE"        "P_BLACK"        "P_ASIAN"        "P_AMERIND"      "P_HAWPAC"       "P_OTHER_RACE"   "P_TWOMORE"  
  ################################################################## #
  
  # setdiff2(names(x$EJAM), names(testoutput_ejscreenapi_plus_5))
  
  
  
  onlyejam <- onlyejam[!grepl("DISPARITY", onlyejam)]
  
  # unexplained missing
  
  setdiff(onlyejam, okmissing) 
  
  ## need to rename api outputs 
  #   "EJScreenPDF"     "EJScreenMAP"  
  # to
  #  "EJScreen Report"  "EJScreen Map"         
  
  ## but do not have, for some reason, these in API outputs: Some problem with renaming to/from r format, done or not in outputs
  # testoutput_ejscreenapi_1pts_1miles  has percentage but not counts like P_AGE_LT18 
  #  "AGE_LT18" "AGE_GT17" - counts are not in api output, only percents of these
  # "MALES" "FEMALES" - counts are not in api output, only percents of these like "P_MALES"
  # "OWNHU"  "OCCHU"  - counts are not in api output, only percents of these like "P_OWN_OCCUPIED"
  #   "DISAB_UNIVERSE"  "DISABILITY"  - counts are not in api output, only percents of these like "P_DISABILITY" 
  # "PCT_HH_BPOV"   "HH_BPOV" - NOT SURE WHY THESE ARE MISSING
  #
  #"LAN_UNIVERSE" 
  # "LAN_ENG_NA" 
  #     "LAN_SPANISH"        "LAN_IE"        "LAN_API"     
  #     "HLI_SPANISH_LI"     "HLI_IE_LI"     "HLI_API_LI"      "HLI_OTHER_LI"  
  # "PCT_HLI_SPANISH_LI" "PCT_HLI_IE_LI" "PCT_HLI_API_LI"  "PCT_HLI_OTHER_LI" # "p_hli_api_li"  is in api output once renamed to rnames?? 
  #  "pct_lan_eng"
  #  "pct_lan_spanish"    "pct_lan_ie"    "pct_lan_api"
  # 
  #  "distance_min"  "distance_min_avgperson"  "sitecount_max"  "sitecount_unique"  "sitecount_avg"
  
  
  
}



