
# List of functions ####

# ejscreen_vs_ejam()
# ejscreen_vs_ejam_alreadyrun()

# ejscreenapi2ejam_format()  - moved to other source file to be with ejscreenit_for_ejam

# ejscreen_vs_ejam_summary() ***************************
# ejscreen_vs_ejam_summary_quantiles()
# ejscreen_vs_ejam_1var()
# ejscreen_vs_ejam_see1()
# ejscreen_vs_ejam_see1map()

# ejscreen_vscript()  is an unexported function that is 
#  like a script that does all this and saves it locally in files
#   for interactive analysis
######################################################################### # 



############################################################ #


#' EJAM/EJSCREEN comparisons
#' 
#' This is the main function that facilitates comparing
#' EJScreen API vs EJAM stats near site(s)
#' 
#' @details 
#'   Note that the EJAM tool/ function called [ejamit()]
#'   does not rely on EJScreen to do the calculations
#'   and instead tries to replicate what EJScreen would do. 
#'   As a result, as of early 2024 at least, while 
#'   - *[ejamit()] is much, much faster than [ejscreenit_for_ejam()]* and
#'   - *provides additional information* (distribution of distances by group, etc.) 
#'   - *features* (histograms, spreadsheet with heatmaps, etc.)
#'   - *flexibility* (easy for analysts using R to customize analysis, etc.),
#'   *[ejamit()] does not always exactly replicate EJScreen* -- 
#'   does not provide 100% identical results (percentiles, etc.) for 
#'   every indicator in every analysis at every location.
#'   This is due to slight variations in 
#'   - details of the spatial calculations (which blocks are nearby,
#'   sometimes counting 1 extra block as 2.99 miles away while 
#'   EJScreen counts it as outside the 3 mile radius, e.g.)
#'   - rounding (how many digits are retained during calculations,
#'   and how many are shown in final reports) 
#'   - percentile assignment method (how percentile lookup tables are used,
#'   how ties are treated in percentile lookup tables, etc.), or 
#'   - weighted averages or other formulas used for aggregation of blockgroup scores
#'   (being updated in 2024 to more precisely match the formulas EJScreen uses)
#' 
#' @param latlon data.table or data.frame with colnames lat and lon, 
#'   and one row per site
#' @param radius in miles, used in ejamit() and ejscreenapi_plus()
#' @param fips FIPS code(s) of counties or blockgroups, if not using latlon and radius.
#' @param x100fix whether to multiply x100 in ejamit() outputs the names_d and names_d_subgroups 
#'   indicator scores to convert fractions 0 to 1 into percentages of 0 to 100,
#'   prior to rounding and reporting EJAM results here. 
#' @param x100varnames optional, if x100fix = TRUE, a vector of colnames of x$EJAM to convert from 
#'   being scaled as 0 to 1 into rescaled values of 0 to 100, because some 
#'   outputs of EJSCREEN were reported as percentages 0 to 100 but as 0 to 1 in EJAM.
#' @param save_when_report see [ejscreenapi_plus()], to save progress every so often just in case.
#' @param save_ejscreen_output set to NULL to avoid saving ejscreen results locally.
#'   If specified as a valid path and filename ending in .rda, it saves there.
#'   If missing, function will prompt in interactive R session
#'   for a folder to use for saving the .rda results of ejscreenapi_plus().
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
#' @examples
#'  \dontrun{
#'  # in RStudio, interactive:
#'  vs <- ejscreen_vscript()
#'  
#'  # vs filtered to just the ones where rounded pop disagrees
#'  table(vs$same_shown$pop)
#'  vspopoff <- lapply(vs, function(x) x[!vs$same_shown$pop, ])
#'  
#'  pts <- testpoints_100[1:5, ]
#'    
#'   # This step can take a long time, 
#'   # almost 1 minute / 20 points, as it uses the EJScreen API:
#'   #z <- ejscreen_vs_ejam(
#'     testpoints_100[27, ], radius = 3, nadrop = T, include_ejindexes = TRUE)
#'   z <- ejscreen_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
#'   
#'   EJAM:::ejscreen_vs_ejam_summary(z)
#'   
#'   # see one site
#'   ejscreen_vs_ejam_see1(z, mysite = 1)
#'   
#'    # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, 
#'      paste0('pctile.', c(names_these, names_ej, names_ej_supp)), 
#'      paste0('state.pctile.', c(names_these, names_ej, names_ej_supp))
#'    )
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
#'   }
#' @seealso [ejscreen_vs_ejam_alreadyrun()]
#' 
#' @keywords internal
#' @export
#' 
ejscreen_vs_ejam <- function(latlon, radius = 3, 
                             fips = NULL,
                             nadrop = FALSE,
                             save_ejscreen_output = "ejscreenapi_plus_out.rda",
                             save_when_report = FALSE, report_every_n = 250, # save every 10 minutes or so
                             calculate_ratios = FALSE, include_ejindexes = TRUE,
                             x100fix = TRUE, 
                             x100varnames = names_pct_as_fraction_ejamit, ...) {
  
  if ((!is.null(save_ejscreen_output) && !(FALSE %in% save_ejscreen_output))  ) {
    if (missing(save_ejscreen_output) & interactive()) {
      mydir = rstudioapi::selectDirectory("save ejscreen results in what folder?")
    } else {
      mydir = dirname(save_ejscreen_output)
      if (!dir.exists(mydir)) {stop(mydir, " folder not found")}
      save_ejscreen_output <- basename(save_ejscreen_output)
      if (tools::file_ext(save_ejscreen_output) != "rda") {stop("filename specified in save_ejscreen_output must end in .rda")}
    }
  }
  ################################### #
  # GET EJSCREEN RESULTS ####
  
  ## a) this is simplest - it just calculates ratios in its default, and adds URLs we dont need, but no map, no table view, no boxplot/etc.
  api1 <- ejscreenapi_plus(latlon, radius = radius,
                           fips = fips,
                           save_when_report = save_when_report, report_every_n = report_every_n,
                           calculate_ratios = calculate_ratios)
  
  ## b) this would work but after using ejscreenapi_plus()  it also can do map, boxplot, interactive DT viewer, etc.
  # api1 <- ejscreenit(latlon, radius = radius, 
  #                    fips = fips, 
  #                    calculate_ratios = calculate_ratios,
  #                    nosave = TRUE, nosee = TRUE, save_plot = FALSE, save_map = FALSE, see_map = FALSE, see_plot = FALSE
  #                    )$table
  
  ## c) this would work but is the highest level function - it uses ejscreenit() but then reformats columns/names which is also done by ejscreen_vs_ejam_alreadyrun() below
  # api1 <- ejscreenit_for_ejam(latlon, radius = radius, 
  #                             fips = fips, 
  #                             calculate_ratios = calculate_ratios,
  #                             nosave = TRUE, nosee = TRUE, save_plot = FALSE, save_map = FALSE, see_map = FALSE, see_plot = FALSE
  # )
  
  ################################### #
  # GET EJAM RESULTS ####
  
  if (missing(latlon)) {latlon <- api1[ , c('id', 'lat', 'lon')]} # in case provided interactively above
  ejam1 <- ejamit(latlon, radius = radius, calculate_ratios = calculate_ratios, include_ejindexes = include_ejindexes, ...)$results_bysite
  
  ################################### #
  if (!is.null(save_ejscreen_output)) {
    save(api1, file = file.path(mydir, save_ejscreen_output))
  }
  ################################### #
  z <- ejscreen_vs_ejam_alreadyrun(api1, ejam1, nadrop = nadrop, x100fix = x100fix, x100varnames = x100varnames)
  
  invisible(z)
}
############################################################ #


#' EJAM/EJSCREEN comparisons
#' 
#' Compare EJScreen API vs EJAM stats near site(s) (after results already run)
#'
#' @param apisite table output of ejscreenit()$table,
#'   or ejscreenapi_plus(), and also see [ejscreenit_for_ejam()]
#' @param ejamsite table output of ejamit()$results_bysite
#' @param nadrop optional, whether to drop indicators for which EJScreen API returns NA
#' 
#' @param x100fix optional, whether to multiply x100 the names_d and names_d_subgroups 
#'   indicator scores to convert fractions 0 to 1 into percentages of 0 to 100,
#'   prior to rounding and reporting EJAM results here. 
#'   
#' @param x100varnames optional, if x100fix = T, a vector of colnames of x$EJAM to convert from 
#'   being scaled as 0 to 1 into rescaled values of 0 to 100, because some 
#'   outputs of EJSCREEN were reported as percentages 0 to 100 but as 0 to 1 in EJAM.
#'
#' @return prints summary to console, but returns invisible a list of
#'   tables (data frames or matrix/arrays), with names 
#'   EJSCREEN, EJAM, EJSCREEN_shown, EJAM_shown, same_shown, ratio, 
#'   diff, absdiff, pctdiff, etc.
#'   
#'   For each table, colnames are indicators like pop, blockcount_near_site, etc.
#'   and rows represent sites analyzed.
#'   
#'   
#' @examples 
#'   vs = ejscreen_vs_ejam_alreadyrun(
#'     apisite = testoutput_ejscreenapi_plus_5,
#'     ejamsite = ejamit(testpoints_5, radius = 1, include_ejindexes = TRUE)$results_bysite)
#'   ejscreen_vs_ejam_see1(vs, mysite = 1)
#'   
#'   # To see format of output:
#'   str(vs)
#'   cbind(class.of.table =  sapply(vs,
#'    FUN = function(that) paste0(class(that), collapse = ", ")) )
#'  \dontrun{
#'  
#'  # requires data.table
#'  
#'  # analyze point(s) in both EJScreen and EJAM
#'   pts <- testpoints_100[1:5, ]
#'   #vs <- ejscreen_vs_ejam(testpoints_100[27, ], radius = 3, include_ejindexes = TRUE)
#'   
#'   # just 1 point
#'   # vs <- ejscreen_vs_ejam(pts[5, ], radius = 3, include_ejindexes = TRUE)
#'   
#'   # multiple points
#'   # This step can take a long time, almost 1 minute per 20 points, as it uses the EJScreen API:
#'   vs <- ejscreen_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
#'     
#'    # same thing but step by step
#'    api1 <- ejscreenit(pts, radius = 3)
#'    api1 <- api1$table[5, ]
#'    ejam1 <- ejamit(pts, radius = 3, include_ejindexes = TRUE)
#'    ejam1 <- ejam1$results_bysite[5, ]
#'    vs <- ejscreen_vs_ejam_alreadyrun(api1, ejam1)
#'    
#'    # to keep it as a list of data.frames but just 1 row each now:
#'    zlist1 <- lapply(vs, function(x) x[myrow, ])
#'    names(zlist1); sapply(zlist1, dim)
#'    
#'    # look at a table about just 1 site among those analyzed, e.g., the first one:
#'    myrow <- 1
#'    z1 <- data.frame(sapply(vs, function(x) x[myrow, ]))
#'    # but that creates a data.frame where each column is a list... 
#'    
#'    z <- vs
#'    
#'    
#'    # **** WORK IN PROGRESS ... *** !!!!!  
#'    
#'    # SEE [ejscreen_vscript()] for a newer, interactive version
#'    
#'    
#'    # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, 
#'      paste0('pctile.', c(names_these, names_ej, names_ej_supp)), 
#'      paste0('state.pctile.', c(names_these, names_ej, names_ej_supp))
#'    )  
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
#'     # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, 
#'      paste0('pctile.', c(names_these, names_ej, names_ej_supp)), 
#'      paste0('state.pctile.', c(names_these, names_ej, names_ej_supp))
#'    )
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
#' @keywords internal
#' 
ejscreen_vs_ejam_alreadyrun <- function(apisite, ejamsite, nadrop = FALSE, 
                                        x100fix = TRUE, 
                                        x100varnames = names_pct_as_fraction_ejamit) {
  
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
  # if it had not already been put in this format via ejscreenit_for_ejam() in ejscreen_vs_ejam()...
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
    ejamsite <- fix_pctcols_x100(ejamsite, cnames = x100varnames)
  }
  
  EJSCREEN_shown <- table_round(apisite)  # SLOW! ***
  EJAM_shown     <- table_round(ejamsite)  # SLOW! ***
  suppressWarnings({
    apisite               <- as.matrix(as.data.frame(lapply( apisite,    as.numeric))) # should now work if only 1 point (1 row) was analyzed
    ejamsite              <- as.matrix(as.data.frame(lapply(ejamsite,    as.numeric)))
    EJSCREEN_shown <- as.matrix(as.data.frame(lapply(EJSCREEN_shown, function(z) trimws(as.character(z)) )))
    EJAM_shown     <- as.matrix(as.data.frame(lapply(EJAM_shown, function(z) trimws(as.character(z)) )))
  })  
  z <- list(
    EJSCREEN = apisite,
    EJAM    = ejamsite,
    EJSCREEN_shown = EJSCREEN_shown,
    EJAM_shown         = EJAM_shown
  )
  
  z$same_shown <- data.frame(z$EJAM_shown == z$EJSCREEN_shown)
  
  z$ratio <- ifelse(z$EJSCREEN == 0 & z$EJAM == 0, 1, ifelse(z$EJSCREEN == 0, NA, z$EJAM / z$EJSCREEN))   # prettyNum(z$ratio, digits = 2, format = 'f')
  z$diff <- z$EJAM - z$EJSCREEN
  z$absdiff <- abs(z$diff)
  z$pctdiff <- z$ratio - 1
  z$abspctdiff <- abs(z$pctdiff)   
  
  z <- lapply(z, function(mymatrix) as.data.frame(mymatrix)) # convert each matrix array to data.frame so easier to refer to colnames in each table
  
  # rname <- colnames(ejamsite) # same as colnames(z$EJAM) OR  names(z$EJAM)
  # longname <- fixcolnames(colnames(ejamsite), 'r', 'long')
  
  # ejscreen_vs_ejam_summary(z) # done also by ejscreen_vscript() so it would be repeated here
  
  invisible(z) 
}
############################################################ #


#' EJAM/EJSCREEN comparisons - see 1 site/row, check estimates for 1 indicator
#'
#' @param vsout results of [ejscreen_vs_ejam()]
#' @param pts data.frame with lat, lon that was input to [ejscreen_vs_ejam()]
#' @param varname default is "blockcount_near_site" but can be "pop", "Demog.Index", etc.
#'
#' @return table
#' 
#' @keywords internal
#'
ejscreen_vs_ejam_bysite <- function(vsout, pts, varname = "blockcount_near_site") {
  if (missing(pts)) {pts = data.frame(n = 1:NROW(vsout$EJSCREEN))}
  cat("\n\n", varname, "\n\n")
  data.frame(
    n = 1:NROW(pts),
    pts[, intersect(c("sitenumber","lat","lon"), names(pts))], 
    ejscreen = vsout$EJSCREEN[,varname], 
    ejam = vsout$EJAM[,varname], 
    difference = vsout$diff[,varname]
  )
}
######################################################################################### # 


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
#' 
#' @keywords internal
#'
ejscreen_vs_ejam_summary <- function(z = NULL, 
                                     myvars = colnames(z$EJAM), tol = 0.05, 
                                     prob = 0.95, na.rm = TRUE ) {
  
  if (is.null(z)) {
    z <-  ejscreen_vs_ejam()
  }
  # make each a data.frame instead of matrix array
  # so it is easier to refer to columns by name like z$EJSCREEN[ , myvars]
  z = lapply(z, function(mydf) as.data.frame(mydf)) # duplicates earlier step but ok
  
  # tol Set tol so that results are said to agree if they differ by less than tol percent, where tol is a fraction 0 to 1. 
  # z is output of ejscreen_vs_ejam
  # na.rm <- TRUE # to see 0% etc. instead of NA for indicators that could not be compared in at least some sites that may lack data.
  ## for a subset of key indicators:
  # myvars <- c(names_these, names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
  
  # need pop and radius.miles for this to work as written
  myvars <- unique(c('pop', myvars, 'radius.miles'))
  
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
  
  z$same_round0 <- round(z$EJSCREEN[ , myvars], 0) == round(z$EJAM[ , myvars], 0)
  
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
  sites.agree.round0       <- colSums(z$same_round0, na.rm = na.rm)
  sites.agree.within.tol   <- colSums(z$abspctdiff < tol, na.rm = TRUE)
  
  pct_agree = data.frame(
    
    indicator = myvars, 
    varlist = varinfo(myvars, 'varlist')$varlist,
    
    sites.with.data.ejscreen = sites.with.data.ejam,
    sites.with.data.neither = sites.with.data.neither,
    sites.with.data.both    = sites.with.data.both,
    
    sites.agree.rounded    = sites.agree.rounded,
    sites.agree.round0     = sites.agree.round0,
    sites.agree.within.tol = sites.agree.within.tol,
    
    pct.of.sites.agree.rounded    = round(100 * sites.agree.rounded    / sites.with.data.both, 1),
    pct.of.sites.agree.round0     = round(100 * sites.agree.round0     / sites.with.data.both, 1),
    pct.of.sites.agree.within.tol = round(100 * sites.agree.within.tol / sites.with.data.both, 1), 
    # test/check NA handling there ***
    
    median.abs.diff       =  apply(z$absdiff,   MARGIN = 2, FUN = median, na.rm = TRUE),
    max.abs.diff          =  apply(z$absdiff,   MARGIN = 2, FUN = max,    na.rm = TRUE), 
    
    mean.pct.diff   = 100 * apply(z$abspctdiff, MARGIN = 2, FUN = mean,   na.rm = TRUE),
    median.pct.diff = 100 * apply(z$abspctdiff, MARGIN = 2, FUN = median, na.rm = TRUE),
    max.pct.diff    = 100 * apply(z$abspctdiff, MARGIN = 2, FUN = max,    na.rm = TRUE),
    
    within.x.pct.at.p.pct.of.sites = round(100 * apply(z$abspctdiff, MARGIN = 2, FUN = quantile, type = 1, probs = prob, na.rm = TRUE), 1)
  )
  pct_agree$median.pct.diff <- round(pct_agree$median.pct.diff, 0)
  pct_agree$within.x.pct.at.p.pct.of.sites <- round(pct_agree$within.x.pct.at.p.pct.of.sites, 0)
  pct_agree$max.pct.diff <- round(pct_agree$max.pct.diff, 0)
  rownames(pct_agree) <- NULL
  pct_agree <- pct_agree[order(pct_agree$varlist, pct_agree$pct.of.sites.agree.rounded, -pct_agree$within.x.pct.at.p.pct.of.sites, decreasing = T), ]
  
  usefulvars <- c('blockcount_near_site', 'pop', names_e, names_d, 
                  #names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile,
                  names_d_subgroups
  )
  usefulstats <- c('indicator', 'varlist',
                   #  "sites.with.data.both",
                   #  "sites.agree.rounded", "sites.agree.within.tol",
                   'pct.of.sites.agree.rounded', 'pct.of.sites.agree.round0', 'pct.of.sites.agree.within.tol',
                   # 'median.pct.diff', 'max.pct.diff', 
                   'within.x.pct.at.p.pct.of.sites')
  
  # if (!is.null(decimals)) {
  #   pct_agree[, names(pct_agree) != "indicator"] <- round(pct_agree[, names(pct_agree) != "indicator"], decimals)
  #   }
  # rownames(pct_agree) <- pct_agree$indicator  # right now they have original rownum but prints sorted by largest disagreement
  
  cat("\n\nComparison of results for", NROW(z$EJAM), "sites.\n")
  print(z$EJAM[1,'radius.miles'])
  cat("\n")
  printrounded = pct_agree[pct_agree$indicator %in% usefulvars, usefulstats]
  #printrounded$
  print(
    printrounded
  )
  
  cat("\n\n")
  print(pct_agree[pct_agree$indicator %in% c("pop", "blockcount_near_site"), usefulstats])
  
  cat(paste0("\n\n Tolerance of ", tol, " was used, so 'agree.within.tol' means a difference of <", tol * 100, "%.\n"))
  cat(paste0(" Probs (p) used was ", prob, ", so 'at.p.pct.of.sites' means at ", round(prob * 100, 0), "% of sites.\n\n" ))
  cat("\n\n")
  
  # see all sites for one indicator like "pop"
  print(head(ejscreen_vs_ejam_1var(z), 25))
  cat("\n\n")
  
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
#'
#' @keywords internal
#' 
ejscreen_vs_ejam_summary_quantiles <- function(z, 
                                               mystat = c("ratio", "diff", "absdiff", "pctdiff", "abspctdiff")[1], 
                                               myvars = c('pop', names_these), 
                                               probs = (0:20) / 20, na.rm = TRUE, digits = 4) {
  
  round(
    t(
      sapply(
        data.frame(z[[mystat]])[ , myvars, drop = FALSE], 
        quantile, probs = probs, na.rm = na.rm
      )
    ), 
    digits
  )
}
######################################################################### # 


#' EJAM/EJSCREEN comparisons - see results at every site, for 1 INDICATOR after using ejscreen_vs_ejam()
#'
#' @param vs output of ejscreen_vs_ejam() or ejscreen_vscript()
#' @param varname 
#' @param info 
#'
#' @return  data frame with colnames like 
#'   EJSCREEN, EJAM, EJSCREEN_shown, EJAM_shown, same_shown, ratio, 
#'   diff, absdiff, pctdiff, etc.
#'
#'   and rows represent sites analyzed.
#'   
#' @keywords internal  
#'
ejscreen_vs_ejam_1var = function(vs, varname = 'pop', # names_these[4], # "pctlingiso" 
                                 info = c("EJSCREEN", "EJAM", 
                                          "ratio", 
                                          "diff", "absdiff", 
                                          "pctdiff", "abspctdiff")[c(1,2,5,6)]) {
  cat("\nAlso try, e.g.,  ejscreen_vs_ejam_1var(vs, 'blockcount_near_site')")
  cat('\n\nComparison of results at a few sites, for the indicator', varname, paste0(' (', fixcolnames(varname, 'r', 'long'), ')\n\n'))
  y = data.frame(lapply(vs[info], function(x) x[,varname]))
  if ("pctdiff" %in% names(y)) {y$pctdiff = round(100 * y$pctdiff, 0)}
  y
}
######################################################################### # 

#'  EJAM/EJSCREEN comparisons - see results for 1 site after using ejscreen_vs_ejam()
#'
#' @param z output of ejscreen_vs_ejam() or ejscreen_vscript()
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
#' @return a table showing one row per indicator, and columns like EJSCREEN, EJAM, ratio, etc.,
#'   but see str() because it is a list in matrix form
#'
#' @examples 
#'   dontrun{
#'   z <- ejscreen_vs_ejam(testpoints_10, radius = 3)
#'   mysite <- 9
#'   ejscreen_vs_ejam_see1(z, mysite = mysite, myvars = colnames(z$EJAM))[!is.na(z$EJSCREEN[mysite, ]) , 1:2]
#'   }
#'
#' @keywords internal
#' 
ejscreen_vs_ejam_see1 <- function(z, myvars = c("ejam_uniq_id", 'pop', names_d), mysite = 1) {
  
  if (!is.list(z) | !("EJAM" %in% names(z))) {stop('z must be output of ejscreen_vs_ejam() or ejscreen_vs_ejam_alreadyrun()')}
  if (length(mysite) > 1 | mysite > NROW(z$EJAM)) {stop('mysite must be the row number of 1 site in the table z$EJAM')}
  if (!all(myvars %in% colnames(z$EJAM))) {stop('myvars must be among colnames of z$EJAM')}
  if (length(myvars) == 1) {myvars = c('ejam_uniq_id', myvars)} # quick fix since didn't handle just 1 var
  z = sapply(z, function(x) x[mysite, ])[myvars, , drop = FALSE]
  z = cbind(z, varlist = varinfo(myvars, 'varlist')$varlist) # now a matrix that is also actually a list so it can store both character and numeric elements
  z = z[order(as.vector(unlist(z[,'varlist'] ))), ]
  
  roundable = c( 'ratio', 'diff', 'absdiff', 'pctdiff', 'abspctdiff' )
  z[, roundable] <- round(as.numeric(z[, roundable]), 4)
  # if output is made a data.frame,   EJAM_shown etc. do not show quote marks to make clear it is text.
  # if output is kept a matrix/array (that is really a list here), you cannot easily refer to a column like z$ratio 
  z = data.frame( lapply(as.data.frame(z), unlist) ) # lets you do things like z$ratio or z[,c('EJSCREEN', 'EJAM')] or z[!z$same_shown, ]
  return(z)
}
######################################################################### # 


#' EJAM/EJSCREEN comparisons - see map and tables of blocks near a site to explain discrepancy in pop and blocks
#'
#' @param n row number in x$EJAM of site to check
#' @param x results from  x <- ejscreen_vs_ejam(testpoints_10, radius =3, include_ejindexes = TRUE)
#' @param overlay_blockgroups optional, set TRUE to see overlay of boundaries of parent blockgroups,
#'   noting that you have to click to turn off the layer for block point info popups to work
#' @param ... passed to [plotblocksnearby()]
#' @return Just draws map and shows tables and returns the table of blocks near the site
#'   and info on how much the distance and pop count differ from what EJAM thinks
#'   is the actual exact radius, with possible explanation of discrepancy
#'   between ejscreen api and ejam estimate,
#'   but you may also want the output of ejscreen_vs_ejam_see1()
#' @examples dontrun{
#'   z <- ejscreen_vs_ejam(testpoints_10, radius = 3, include_ejindexes = TRUE)
#'   ejscreen_vs_ejam_see1map(3, z, overlay_blockgroups = TRUE)
#'  }
#'
#' @keywords internal
#' 
ejscreen_vs_ejam_see1map <- function(n = 1, x, overlay_blockgroups = FALSE,
                                     radius = NULL, ...) {
  
  # function to help explain discrepancy in pop and blocks  
  # n is the rownumber of the site analyzed, row of x$EJAM
  # x is from x <- ejscreen_vs_ejam(pts, radius = radius, include_ejindexes = TRUE)
  
  differenceinfo = ejscreen_vs_ejam_see1(x,
                                         mysite = n, 
                                         myvars = c('pop', 'blockcount_near_site'))[ , c('EJSCREEN', 'EJAM', 'diff')]
  ######### #
  if (is.null(radius)) {radius = x$EJAM[n, "radius.miles"]}
  datf = data.frame(x$EJAM[n, 1:10, drop = FALSE], lat = x$EJAM[n, 'lat'], lon = x$EJAM[n, 'lon'])
  suppressWarnings({
    
    px <- plotblocksnearby(datf, 
                           radius = radius + 0.08, # 0.01 miles is 52.81 feet... 
                           #  to include a few that are slightly beyond the stated radius !
                           overlay_blockgroups = overlay_blockgroups, 
                           ...)
  })
  px[blockgroupstats, bgpop := pop, on = 'bgid']
  px[, blockpop := bgpop * blockwt]
  
  # see the 10 furthest sites, that are <= radius,
  
  these <- tail(px[distance <= radius, ][order(distance), .(blockid, distance, blockpop)], 10) 
  these$cumpop = round(rev(cumsum(rev(these$blockpop))), 0) # cumulative starting from furthest site
  
  # but then also include the next few beyond that radius
  # so add as many as the discrepancy and about 5 more
  
  these_extras <- head(px[distance > radius, ][order(distance), .(blockid, distance, blockpop)], 5 + differenceinfo['blockcount_near_site', 'diff']) 
  these_extras$blockpop = -1 * these_extras$blockpop
  these_extras$cumpop = round( (cumsum((these_extras$blockpop))), 0) 
  
  these <- rbind(these, 
                 data.frame(blockid = NA, distance = radius, blockpop = NA, cumpop = 0), # dividing line in list
                 these_extras)
  
  these$blockpop = round(these$blockpop, 1)
  these$feet = -1 * round((radius - these$distance) * 5281, 0) 
  these$meters = -1 * round((radius - these$distance) * meters_per_mile, 0)
  
  ########################## # 
  these$explanation = ""
  pdif = round(differenceinfo['pop', 'diff'], 0)
  bdif = differenceinfo['blockcount_near_site', 'diff']
  explained = FALSE
  if (pdif != 0) {
    # does a single block explain the discrepancy?########################## # 
    if (pdif %in% round(these$blockpop, 0) & bdif %in% c(-1, 1)) {
      these$explanation[round(these$blockpop, 0) == pdif] <- "pop of this block matches pop diff"
      explained = TRUE
    }
    if (bdif != 1) {
      if (!explained) {
        # could it be a combo of some exactly bdif number of blocks that explains the pdif? e.g., missed all 3 or all 3 are extra ########################## # 
        combs = combn(seq_along((these$blockpop)), m = bdif)
        possible_explanations <- combs[, which(round(colSums(combn((these$blockpop), m = bdif)), 0) == pdif)]
        # most likely guess is the set (if one exists) that are in order by distance, like the next 3 beyond radius, ranked by distance
        inorderbydistance = apply(possible_explanations, 2, function(x) setequal(x, min(x):(min(x) + length(x) - 1) ))
        if (any(inorderbydistance)) {
          if (sum(inorderbydistance) > 1) {
            bestone = possible_explanations[, inorderbydistance][,1]
          } else {
            bestone = possible_explanations[, inorderbydistance] 
          }
        } else {
          bestone = possible_explanations[,1] # just take the first one, since none are obviously most likely
        }
        these$explanation[bestone] <- "group of missing or extra?"
        explained <- TRUE
      }
    }
    
    # could it be bdif blocks but also 1 more missed and 1 more extra?
    #   would check as above with combn but where m  = bdif +2
    #   includes case where bdif == 0    
    if (!explained) {
      # could it be a combo of some exactly bdif +2 number of blocks that explains the pdif? e.g., missed all 3 or all 3 are extra ########################## # 
      combs = combn(seq_along((these$blockpop)), m = bdif + 2)
      possible_explanations <- combs[, which(round(colSums(combn((these$blockpop), m = bdif + 2)), 0) == pdif)]
      if (length(possible_explanations) != 0) {
        if (NCOL(possible_explanations) == 1) {
          bestone = possible_explanations
        } else {
          # most likely guess is the set (if one exists) that are in order by distance, like the next 3 beyond radius, ranked by distance
          inorderbydistance = apply(possible_explanations, 2, function(x) setequal(x, min(x):(min(x) + length(x) - 1) ))
          if (any(inorderbydistance)) {
            if (sum(inorderbydistance) > 1) {
              bestone = possible_explanations[, inorderbydistance][,1]
            } else {
              bestone = possible_explanations[, inorderbydistance] 
            }
          } else {
            bestone = possible_explanations[,1] # just take the first one, since none are obviously most likely
          }
        }
        these$explanation[bestone] <- "group of missing or extra?"
        explained <- TRUE
      }
    }
    # This case should've been found already by the combinations above
    # if (!explained) {
    #   # does an obvious combo of blocks explain the discrepancy? ########################## # 
    #   if (pdif %in% these$cumpop) {
    #     these$explanation[these$cumpop == pdif] <- "cumpop matches pop diff"
    #     explained <- TRUE
    #   }
    # }
    
    ########################## # 
  }
  these = these[ , c('blockid', 'distance', 'feet', 'meters', 'blockpop' ,'cumpop' ,'explanation')]
  
  cat(paste0("\nSite ", n, ", ejam_uniq_id = ", x$EJAM$ejam_uniq_id[n], '\n\n'))
  
  print(these)
  cat("blockpop shown here is rounded off\n\n")
  
  
  cat("\nThere should be",
      differenceinfo['blockcount_near_site', 'diff'],
      "block(s) whose population account(s) for the pop count discrepancy of ",
      pdif,
      "people. \n\n"
  )
  print(differenceinfo)
  
  invisible(these)
}
######################################################################### # 




######################################################################################### # 


#'  EJAM/EJSCREEN comparisons - for interactive RStudio use
#' @details
#'  THIS IS A SCRIPT FOR INTERACTIVE RSTUDIO CONSOLE USE 
#'   TO RUN A SET OF POINTS THROUGH BOTH EJSCREEN AND EJAM
#'   AND SAVE STATS ON THE DIFFERENCES
#'   -- including letting you use saved ejscreenapi results and input points
#'   so you can iterate and rerun just the EJAM portion and compare to the saved benchmark data.
#'  
#' 
#' @param defdir folder
#' @param n how many points
#' @param newpts logical, if need new set of random locations
#' @param pts data.frame of points with columns lat,lon
#' @param radius miles
#' @param savedejscreentableoutput is a data.table from ejscreenit()$table 
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
#' @examples
#' vs = ejscreen_vscript(pts = testpoints_100, radius = 3)
#' 
#' #'  # vs filtered to just the ones where rounded pop disagrees
#'  table(vs$same_shown$pop)
#'  vspopoff <- lapply(vs, function(x) x[!vs$same_shown$pop, ])
#'  
#' ## look only at places where blockcount was identical, to exclude that as source of difference
#' vs_blocksmatch = lapply(vs, function(df) df[vs$absdiff[, "blockcount_near_site"] == 0, ])
#' # vss = ejscreen_vs_ejam_summary(vs )
#' vssb = ejscreen_vs_ejam_summary(vs_blocksmatch)
#' # vss[vss$indicator %in% names_these, c(1,7,8)]
#' vssb[vssb$indicator %in% names_these, c(1,7,8)]
#' x = vssb[vssb$indicator %in% c('pop', names_these), c(1,7,8)] 
#' x[order(x$pct.of.sites.agree.within.tol), ]
#' 
#' xx = cbind(indicator = vssb$indicator[vssb$indicator %in% c('pop', names_these)],
#'  round(vssb[vssb$indicator %in% c('pop', names_these),
#'   c('pct.of.sites.agree.within.tol', 'max.abs.diff', 'median.abs.diff')], 0))
#' xx[xx$pct.of.sites.agree.within.tol < 100, ]
#' 
#' @export
#' 
ejscreen_vscript <- function(defdir = '.',
                             n, newpts, pts, radius, savedejscreentableoutput,
                             x100fix = TRUE, 
                             x100varnames = names_pct_as_fraction_ejamit
) {
  
  if (!interactive()) {
    stop("ejscreen_vscript() can only be used interactively, as in RStudio console")
  }
  
  ## params in ejscreen_vs_ejam()
  # latlon, radius = 3, nadrop = FALSE,
  # save_ejscreen_output = "ejscreenapi_plus_out.rda",
  # save_when_report = FALSE, report_every_n = 250, # save every 10 minutes or so
  # calculate_ratios = FALSE, include_ejindexes = TRUE,
  # x100fix = TRUE, 
  # x100varnames = names_pct_as_fraction_ejamit, ...
  
  oldir = getwd()
  on.exit(setwd(oldir))
  
  if ((missing(defdir) | !dir.exists(defdir))) {
    if (!dir.exists(defdir)) {message("specified defdir not found - please specify a valid folder")}
    defdir = ifelse(dir.exists(defdir), defdir, getwd())
    mydir = rstudioapi::selectDirectory("Folder for saving files?", path = defdir)
    if (is.na(mydir)) {stop('cancelled')}
  } else {
    mydir <- defdir
  }
  setwd(mydir)
  # setwd("~/../Desktop/EJAM Team Files/-0 TO DO LISTS AND ISSUES IN PACKAGES/comparisons")
  
  # # a list of points can be analyzed interactively like this: 
  if (missing(newpts) && missing(pts)) {
    newpts = askYesNo("Use a new set of random points? (instead of saved points)")
    if (is.na(newpts)) {stop("cancelled")}
  }
  
  
  if (missing(pts) && newpts) {
    if (missing(n)) {
      n = ask_number(default = 100, title = "Points", message = "How many points to run?")
      if (is.na(n)) {stop('cancelled')}
    }
    pts <- testpoints_n(n, weighting = 'frs')
  } else {
    ###  or reload existing saved points and run EJAM again
    if (missing(pts)) {
      myfile = rstudioapi::selectFile("Which file has saved pts as .csv, .xls, .xlsx?", path = mydir, filter = "csv or xls or xlsx (*.csv, *.xls, *.xlsx)")
      if (is.na(myfile)) {stop("cancelled")}
      pts <- read_csv_or_xl(myfile) # read.csv(myfile) # load(file.path(mydir, "pts.rda"))
    }
    n = NROW(pts)
  }
  save_ejscreen_output <- file.path(mydir, "ejscreenapi_plus_out.rda")
  save_ejscreen_output <- gsub("out.rda$", paste0("out for ", n, "points-", gsub(':','.',Sys.time()), ".rda"), save_ejscreen_output) 
  
  
  if (missing(radius)) {
    radius = ask_number()
    if (is.na(radius)) {stop('cancelled')}
  }
  ############################################ #
  
  ### run the comparison 
  if (missing(savedejscreentableoutput)) {
    usesavedejscreen <- askYesNo("Use saved EJScreen results from prior run?")
    if (is.na(usesavedejscreen)) {stop('cancelled')}
  } else {
    # user can specify they dont want to get asked about using saved results like this:
    if ((length(savedejscreentableoutput) == 1 && savedejscreentableoutput[1] == FALSE) || is.null(savedejscreentableoutput) || all(is.na(savedejscreentableoutput))) {
      usesavedejscreen <- FALSE
    } else {
      usesavedejscreen <- TRUE
    }
  }
  if (usesavedejscreen) {
    ###  to redo just the ejam part eg when iterating fixes but use already-run ejscreen numbers since that is slow.
    if (missing(savedejscreentableoutput)) {
      savedejscreentableoutput_FILENAME = rstudioapi::selectFile("Which file has saved ejscreenapi_plus_out results as .rda ?",  path = getwd(), filter = "rda files (*.rda)")
      if (is.na(savedejscreentableoutput_FILENAME)) {stop("cancelled")}
      nameofobject <- load(savedejscreentableoutput_FILENAME) # load(file.path(mydir, "ejscreenapi_plus_out.rda"))
      savedejscreentableoutput <- get(nameofobject)
      
    } else {
      # savedejscreentableoutput should be the data.frame that is the output of ejscreenapi_plus() 
    }
    if (!is.data.frame(savedejscreentableoutput)) {stop("savedejscreentableoutput should be a data.frame output from ejscreenit()$table or similar")}
    cat("Using saved ejscreen results and running new EJAM results to compare them... \n")
    vs <- ejscreen_vs_ejam_alreadyrun(apisite = savedejscreentableoutput,
                                      ejamsite = ejamit(pts, radius = radius, include_ejindexes = TRUE)$results_bysite,
                                      x100fix = x100fix, 
                                      x100varnames = x100varnames
    )
  } else {
    ## to do it all from scratch
    
    vs <- ejscreen_vs_ejam(pts, radius = radius, include_ejindexes = TRUE, 
                           save_ejscreen_output = save_ejscreen_output,
                           x100fix = x100fix, 
                           x100varnames = x100varnames)
  }
  ######################################################################################### # 
  
  #           save results in text file, csv, rda, etc.
  
  # mydir = rstudioapi::selectDirectory("Confirm folder/ where to save results files", path = getwd())
  
  # see some of the results and save all
  
  sumvs <- ejscreen_vs_ejam_summary(vs) # prints as it does this
  qqq <- ejscreen_vs_ejam_summary_quantiles(vs, mystat = 'ratio', myvars = c(names_these, 'pop'), digits = 2)
  
  fname = paste0("EJAM vs EJSCREEN results for ", n, " points")
  write.csv(sumvs, file = file.path(mydir, paste0(fname, "-fullsummary.csv")))
  write.csv(qqq,   file = file.path(mydir, paste0(fname, "-quantiles.csv")))
  
  tfile = file.path(mydir, paste0(fname, "-full-printout.txt"))
  
  sink(file = tfile)
  on.exit(sink(NULL))
  cat(fname, "\n\n"); print(Sys.Date())
  cat("\n\n-------------------------------------------------------------------\n\n")
  cat("ejscreen_vs_ejam_summary(vs) \n\n")
  sumvs <- ejscreen_vs_ejam_summary(vs) # just printout not full results
  cat("\n\n-------------------------------------------------------------------\n\n")
  cat("percentiles across analyzed locations, of the ratio of EJAM / EJSCREEN estimates\n")
  cat("showing median ratio and ratio hit by only 5% of places\n\n")
  cat( 'qqq[order(qqq[, "95%"], decreasing = F), c("50%", "95%")] \n\n' )
  print( qqq[order(qqq[, "95%"], decreasing = F), c("50%", "95%")]  )
  cat("\n\n-------------------------------------------------------------------\n\n")
  cat("ejscreen_vs_ejam_see1(vs, myvars = c('pop', names_these)) \n\n")
  print( ejscreen_vs_ejam_see1(vs, myvars = c('pop', names_these)) )
  cat("\n\n-------------------------------------------------------------------\n\n")
  cat("ejscreen_vs_ejam_see1(vs, myvars = c('lowlifex', varlist2names('names_d')[2])) \n\n")
  print( ejscreen_vs_ejam_see1(vs, myvars = c('lowlifex', varlist2names('names_d')[2])) )
  cat("\n\n-------------------------------------------------------------------\n\n")
  cat("ejscreen_vs_ejam_bysite(vs, pts) \n\n")
  print(  ejscreen_vs_ejam_bysite(vs, pts) )
  cat("\n\n-------------------------------------------------------------------\n\n")
  sink(NULL)
  
  # open results in RStudio
  rstudioapi::documentOpen(tfile)
  
  ######################################################################################### # 
  
  #  save data for R:
  write.csv(pts, row.names = FALSE, file = file.path(mydir, paste0("EJAM vs EJSCREEN latlon used for ", n, "points-", gsub(':','.',Sys.time()), ".csv")))
  # save(pts, file = file.path(mydir, paste0(fname, "-pts.rda")))
  save(vs, file = file.path(mydir, paste0(fname, "-vs-", gsub(':','.',Sys.time()), ".rda")))
  ############################ # 
  setwd(oldir)
  cat("\n\n  Files saved in ", mydir, "\n\n")
  browseURL(mydir)
  invisible(vs)
}
######################################################################################### # 





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
  api_all <- ejscreenit(testpoints_10, radius = 3) # provides as LONG NAMES for convenience, including  "EJScreen Report" "EJScreen Map" "Buffer distance (miles)" "% Low Income" etc.   
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
  
  # ejscreenapi() outputs now differ from those names in testoutput_ejscreenapi_1pts_1miles - rebuild examples for  ejscreenapi 
  setdiff2(names(x), names(testoutput_ejscreenapi_1pts_1miles))
  # ok in map_headernames
  # [1] "P_NHWHITE"      "P_NHBLACK"      "P_NHASIAN"      "P_NHAMERIND"    "P_NHHAWPAC"     "P_NHOTHER_RACE" "P_NHTWOMORE"    "TOTALPOP"      
  # [9] "P_WHITE"        "P_BLACK"        "P_ASIAN"        "P_AMERIND"      "P_HAWPAC"       "P_OTHER_RACE"   "P_TWOMORE"  
  ################################################################## #
  
  # setdiff2(names(x$EJAM), names(testoutput_ejscreenapi_plus_5))
  
  
  
  onlyejam <- onlyejam[!grepl("DISPARITY", onlyejam)]
  
  # unexplained missing
  
  setdiff(onlyejam, okmissing) 
  
  
  ## but do not have, for some reason, these in API outputs: Some problem with renaming to/from r format, done or not in outputs
  # testoutput_ejscreenapi_1pts_1miles  has percentage but not counts like P_AGE_LT18 
  #  "AGE_LT18" "AGE_GT17" - counts are not in api output, only percents of these
  # "MALES" "FEMALES" - counts are not in api output, only percents of these like "P_MALES"
  # "OWNHU"  "OCCHU"  - counts are not in api output, only percents of these like "P_OWN_OCCUPIED"  -- now "pctownedunits"
  #   "DISAB_UNIVERSE"  "DISABILITY"  - counts are not in api output, only percents of these like "P_DISABILITY" 
  # "PCT_HH_BPOV"   "HH_BPOV" - NOT SURE WHY THESE ARE MISSING
  #
  #"LAN_UNIVERSE" 
  # "LAN_ENG_NA" 
  #     "LAN_SPANISH"        "LAN_IE"        "LAN_API"     
  #     "HLI_SPANISH_LI"     "HLI_IE_LI"     "HLI_API_LI"      "HLI_OTHER_LI"  ###  but later edited map_headernames??
  # "PCT_HLI_SPANISH_LI" "PCT_HLI_IE_LI" "PCT_HLI_API_LI"  "PCT_HLI_OTHER_LI" ### but later edited map_headernames
  #  "pct_lan_eng"
  #  "pct_lan_spanish"    "pct_lan_ie"    "pct_lan_api"
  # 
  #  "distance_min"  "distance_min_avgperson"  "sitecount_max"  "sitecount_unique"  "sitecount_avg"
  
  
  
}



