# ejscreenapi_vs_ejam()
# ejscreenapi_vs_ejam_alreadyrun()
# ejscreenapi2ejam_format()
# ejscreenapi_vs_ejam_summary


#' see summary stats after using ejscreenapi_vs_ejam()
#'
#' @param z output of ejscreenapi_vs_ejam()
#' @param myvars optional to check just a subset of the colnames found in z$EJAM and z$EJSCREEN, 
#'   such as myvars = c(names_d, names_d_subgroups) or myvars = grep("pctile", colnames(z$EJAM), value = T)
#' @param tol optional, set this so that results can be said to agree with this tolerance
#'   if they differ by less than tol percent where tol is expressed as a fraction 0 to 1.
#' @param na.rm needs testing, optional, but 
#'
#' @return A data.frame of summary stats showing counts and percents of analyzed sites (or those with valid data 
#'   that are found in both EJAM and EJSCREEN outputs), indicating how many of the sites
#'   agree between EJSCREEN and EJAM estimates, exactly as reported or within some tolerance.
#'   Columns include 
#'   
#'    "indicator" (variable name)
#'   
#'    "sites.with.data.neither"
#'   
#'    "sites.with.data.both"
#'   
#'    "Rounded.Results.Agree"
#'   
#'    "Results.within.tol"
#'   
#'    "Rounded.Results.Agree.as.pct.of.sites.withdata"
#'   
#'    "Results.within.tol.as.pct.of.sites.withdata"
#'    
#' @export
#'
ejscreenapi_vs_ejam_summary <- function(z, myvars = colnames(z$EJAM), tol = 0.01, na.rm = TRUE) {
  
  # tol Set tol so that results are said to agree if they differ by less than tol percent, where tol is a fraction 0 to 1. 
  # z is output of ejscreenapi_vs_ejam
  # na.rm <- TRUE # to see 0% etc. instead of NA for indicators that could not be compared in at least some sites that may lack data.
  ## for a subset of key indicators:
  # myvars <- c(names_these, names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
  
  z$EJSCREEN <- z$EJSCREEN[ , myvars]
  z$EJAM <- z$EJAM[ , myvars]
  z$EJSCREEN_shown <- z$EJSCREEN_shown[ , myvars]
  z$EJAM_shown <- z$EJAM_shown[ , myvars]
  z$same_shown <- z$same_shown[ , myvars]
  z$ratio <- z$ratio[ , myvars]
  
  # calculate each as count of sites that agree (and not NA), over count of sites with data ie that are not NA 
  # matrixes of valid/not
  ok_ejscreen <- !is.na(z$EJSCREEN) # dim is 100 x 389, e.g.
  ok_ejam <- !is.na(z$EJAM)
  # vector of counts of sites, 1 count per indicator
  sites.with.data.ejscreen <- colSums(ok_ejscreen)
  sites.with.data.ejam <- colSums(ok_ejam)
  sites.with.data.both <- colSums(ok_ejam & ok_ejscreen)
  sites.with.data.neither <- colSums(!ok_ejam & !ok_ejscreen)
  
  Rounded.Results.Agree <- colSums(z$same_shown, na.rm = na.rm) # only counts if valid
  Results.within.tol <- colSums(abs(1 - z$ratio) < tol, na.rm = TRUE)
  
  pct_agree = data.frame(
    indicator = myvars, 
    sites.with.data.neither,
    sites.with.data.both = sites.with.data.both,
    Rounded.Results.Agree = Rounded.Results.Agree,
    Results.within.tol,
    Rounded.Results.Agree.as.pct.of.sites.withdata = round(
      100 * Rounded.Results.Agree / sites.with.data.both, 
      6),
    
    Results.within.tol.as.pct.of.sites.withdata = round(
      100 * Results.within.tol / sites.with.data.both, # test/check NA handling here *** 
      6)
  )
  rownames(pct_agree) <- NULL
  pct_agree <- pct_agree[order(pct_agree$Results.within.tol.as.pct.of.sites.withdata, decreasing = T), ]
  return(pct_agree)
}
############################################################ #


#' compare EJScreen API vs EJAM stats near one site
#'
#' @param latlon 1-row data.table or data.frame with colnames lat and lon
#' @param radius in miles, used in ejamit() and ejscreenapi_plus()
#' @param nadrop whether to drop indicators for which EJScreen API returns NA
#' @param x100fix whether to multiply x100 the names_d and names_d_subgroups 
#'   indicator scores to convert fractions 0 to 1 into percentages of 0 to 100,
#'   prior to rounding and reporting EJAM results here. 
#' @param x100varnames optional, if x100fix=T, a vector of colnames of x$EJAM to convert from 
#'   being scaled as 0 to 1 into rescaled values of 0 to 100, because some 
#'   outputs of EJSCREEN were reported as percentages 0 to 100 but as 0 to 1 in EJAM.
#' 
#' @param ... passed to ejamit() as any additional parameters,
#'    like include_ejindexes = FALSE
#'
#' @return a data.frame with columns EJSCREEN and EJAM, 
#'   rownames are indicators like pop, blockcount_near_site, etc.
#' @export
#'
#' @examples
#'  \dontrun{
#'    pts <- testpoints_100[1:5, ]
#'   #z <- ejscreenapi_vs_ejam(testpoints_100[27, ], radius = 3, nadrop = T, include_ejindexes = TRUE)
#'   z <- ejscreenapi_vs_ejam(pts[5, ], radius = 3, include_ejindexes = TRUE)
#'    
#'    # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, names_pctile, names_state_pctile)
#'    z[z$rname %in% keyreportnames &  z$same_shown, ]
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
#' @seealso [ejscreenapi_vs_ejam_alreadyrun()]
#' 
ejscreenapi_vs_ejam <- function(latlon, radius = 3, nadrop = FALSE, x100fix = TRUE, 
                                x100varnames = c(
                                  names_d, names_d_avg, names_d_state_avg,
                                  names_d_subgroups, names_d_subgroups_avg, names_d_subgroups_state_avg,
                                  "pctdisability",  "p_own_occupied", 
                                  "pctunder18", "pctover17", "pctmale", "pctfemale"), ...) {
  
  api1 <- ejscreenapi_plus(latlon, radius = radius)
  # or api1 <- ejscreenit(latlon, radius = radius)$table
  ejam1 <- ejamit(latlon, radius = radius, ...)$results_bysite
  ejscreenapi_vs_ejam1_alreadyrun(api1, ejam1, nadrop = nadrop, x100fix = x100fix)
}
############################################################ #


#' compare EJScreen API vs EJAM stats near one site (if results already run)
#'
#' @param apisite table output of ejscreenit()$table,
#'   or ejscreenapi_plus()
#' @param ejamsite table output of ejamit()$results_bysite
#' @param nadrop optional, whether to drop indicators for which EJScreen API returns NA
#' @param x100fix optional, whether to multiply x100 the names_d and names_d_subgroups 
#'   indicator scores to convert fractions 0 to 1 into percentages of 0 to 100,
#'   prior to rounding and reporting EJAM results here. 
#' @param x100varnames optional, if x100fix=T, a vector of colnames of x$EJAM to convert from 
#'   being scaled as 0 to 1 into rescaled values of 0 to 100, because some 
#'   outputs of EJSCREEN were reported as percentages 0 to 100 but as 0 to 1 in EJAM.
#'
#' @return a data.frame with columns EJSCREEN and EJAM, 
#'   rownames are indicators like pop, blockcount_near_site, etc.
#' @export
#' @examples 
#'  \dontrun{
#'  
#'  # requires data.table
#'    pts <- testpoints_100[1:5, ]
#'    api1 <- ejscreenit(pts, radius = 3)
#'    api1 <- api1$table[5, ]
#'    ejam1 <- ejamit(pts, radius = 3, include_ejindexes = TRUE)
#'    ejam1 <- ejam1$results_bysite[5, ]
#'    z <- ejscreenapi_vs_ejam1_alreadyrun(api1, ejam1)
#'    
#'    # Reported key indicators - which ones do or don't match
#'    # when comparing EJSCREEN and EJAM results?
#'    keyreportnames <- c('pop', names_these, names_pctile, names_state_pctile)
#'    z[z$rname %in% keyreportnames &  z$same_shown, ]
#'    z[z$rname %in% keyreportnames & !z$same_shown & !is.na(z$EJSCREEN), ]
#'    
#'    # Reported (rounded) numbers match:
#'    z[z$same_shown , -1]
#'    # Reports disagree: 
#'    #  (and not just because of percentages being of 100 vs of 1.00)
#'    z[!z$same_shown & !is.na(z$EJSCREEN) & z$ratio != 0.01, -1] 
#'    # Reports disagree if percentages reported as 0-100 vs fractions 0-1.00
#'    z[z$ratio == 0.01 & !is.na(z$ratio), -1]
#'  }
#' @seealso [ejscreenapi_vs_ejam1()]
#' 
ejscreenapi_vs_ejam_alreadyrun <- function(apisite, ejamsite, nadrop = FALSE, x100fix = TRUE, 
                                            x100varnames = c(
                                              names_d, names_d_avg, names_d_state_avg,
                                              names_d_subgroups, names_d_subgroups_avg, names_d_subgroups_state_avg,
                                              "pctdisability",  "p_own_occupied", 
                                              "pctunder18", "pctover17", "pctmale", "pctfemale")) {
  
  if (!is.data.frame(apisite) | NROW(apisite) != 1) {stop("apisite must be a data.frame of 1 row")}
  if (!is.data.frame(ejamsite) | NROW(ejamsite) != 1) {stop("ejamsite must be a data.frame of 1 row")}
  
  # compare 1 site for EJAM vs EJScreen results already run
  
  apisite <- ejscreenapi2ejam_format(
    apisite, 
    fillmissingcolumns = TRUE, 
    ejamcolnames = colnames(ejamsite)
  )
  sitenum <- 1 # input should  be a one row data.table or data.frame
  
  # return only numeric columns, to simplify comparisons and formatting
  setDF(apisite)
  setDF(ejamsite)
  apisite   <- apisite[ , !(names(apisite) %in% c('ST', 'statename', "REGION", "EJScreen Report", "EJScreen Map", "ECHO report"))]
  ejamsite <- ejamsite[ , !(names(ejamsite) %in% c('ST', 'statename', "REGION", "EJScreen Report", "EJScreen Map", "ECHO report"))]
  
  if (x100fix) {
    ejamsite[ , x100varnames] <- 100 * ejamsite[ , x100varnames]
  }
  
  EJSCREEN_shown <- table_round(apisite) 
  EJAM_shown     <- table_round(ejamsite)
  
  apisite <- apisite[sitenum, ]
  ejamsite <- ejamsite[sitenum, ]

  z <- data.frame(
    EJSCREEN = as.numeric(unlist(apisite)),
    EJAM = as.numeric(unlist(ejamsite)),
    EJSCREEN_shown = as.character(EJSCREEN_shown),
    EJAM_shown = as.character(EJAM_shown),
    rname = names(ejamsite),
    longname = fixcolnames(names(ejamsite), 'r', 'long')
  )
  z$same_shown <- z$EJAM_shown == z$EJSCREEN_shown
  z$ratio <- round(as.numeric(z$EJAM) / as.numeric(z$EJSCREEN), 2)  # prettyNum(z$ratio, digits = 2, format = 'f')
  z <- z[ , c('longname', 'rname', 'EJSCREEN_shown', 'EJAM_shown', 'same_shown', 'EJSCREEN', 'EJAM', 'ratio')]
  
  if (nadrop) {
    z <- z[!is.na(z$EJSCREEN), ]
  }
   
  if (interactive()) {
    keyreportnames <- c('pop', names_these, names_pctile, names_state_pctile)
    shortname <- substr(z$longname[z$rname %in% keyreportnames], 1, 40)
    forprint <- data.frame(shortname = shortname, z[z$rname %in% keyreportnames, -1])
    forprint$EJSCREEN <- prettyNum(forprint$EJSCREEN, big.mark = ',')
    forprint$EJAM <- prettyNum(forprint$EJAM, big.mark = ',')
    cat('\n')
    print(forprint)
    cat('\n')
    print(z[z$rname %in% c('blockcount_near_site', 'pop'), c('longname', 'rname', 'EJSCREEN', 'EJAM')])
    invisible(z)
  } else {
    return(z)
  }
  
}
############################################################ #




#' Convert output of ejscreenapi_plus to format of ejamit table of sites
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
#' @seealso [ejscreenapi_vs_ejam1()]
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
#'   
#'  y2 <- ejamit(testpoints_10[1:2, ], radius = 1)
#'  y2 <- y2$results_bysite
#'  ejamvars <- names(y2)
#'  all.equal(names(z), ejamvars)
#'  sitenum <- 1
#'  z <- data.frame(EJAM = t(y2[sitenum, -1:-3]), EJSCREEN = t(z[sitenum, -1:-3 ]))
#'  z
#'  z[!is.na(z$EJSCREEN), ]
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
  
  # FOUND PROBLEM IN EJAM OUTPUT NAMES FOR EJ INDEX PERCENTILES:
  # SHOULD BE NAMED LIKE names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile
  # pctile.EJ.DISPARITY.pm.eo, state.pctile.EJ.DISPARITY.pm.eo, pctile.EJ.DISPARITY.pm.supp, state.pctile.EJ.DISPARITY.pm.supp
  # but right now show up as:
  # "pctile.state.EJ.DISPARITY.pm.supp"                 
  # "state.pctile.state.EJ.DISPARITY.pm.eo"  
  ###   will fix this.
  ################################################################## #
  
  # WILL FIX: 
  
  # ejscreenapi() outputs now differ from those names in testoutput_ejscreenapi_1pts_1miles - rebuild examples in EJAMejscreenapi pkg
  setdiff2(names(x), names(testoutput_ejscreenapi_1pts_1miles))
  # [1] "P_NHWHITE"      "P_NHBLACK"      "P_NHASIAN"      "P_NHAMERIND"    "P_NHHAWPAC"     "P_NHOTHER_RACE" "P_NHTWOMORE"    "TOTALPOP"      
  # [9] "P_WHITE"        "P_BLACK"        "P_ASIAN"        "P_AMERIND"      "P_HAWPAC"       "P_OTHER_RACE"   "P_TWOMORE"  
  ################################################################## #
  
  
  
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



