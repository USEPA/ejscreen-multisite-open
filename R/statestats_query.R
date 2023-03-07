#' convenient way to see mean, pctiles of Env or Demog indicators from lookup table
#'
#' @param ST vector of state abbreviations, or USA
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param dig digits to round to
#'
#' @export
#'
#' @examples \dontrun{
#' ## in USA overall, see mean and key percentiles for all demog and envt indicators
#' usastats_query() # or statestats_query('us') # can say us or US or USA or usa etc.
#' usastats_query(PCTILES = 'mean')
#' usastats_means() # same but nicer looking format in console
#' usastats_means(dig=4)
#' usastats[!(usastats$PCTILE < 50), c("PCTILE", names_d)]
#' usastats[!(usastats$PCTILE < 50), c("PCTILE", names_e)]
#' ## in 1 state, see mean and key percentiles for all demog and envt indicators
#' statestats_query('MD')
#' ## in 1 state, see mean and key percentiles for just demog indicators
#' statestats_queryd('MD')
#' ## 1 indicator in 1 state, see a few key percentiles and mean
#' statestats_query('MD','proximity.tsdf')
#' ## mean of 1 indicator for each state
#' statestats_query(varnames = 'proximity.tsdf')
#' ## using full blockgroup dataset, not lookup tables of percentiles, 
#' blockgroupstats[,   lapply(.SD, function(x) mean(x, na.rm=T)),  .SDcols= c(names_d, names_e)]
#' ##   see all total counts (not just US means), 
#' ##   demographics including subgroups, 
#' ##   but not environmental indicators.
#' t(round(EJAMbatch.summarizer::ustotals2(bg = EJAM::blockgroupstats),2))
#' t(blockgroupstats[,   lapply(.SD, function(x) mean(x, na.rm=T)),  .SDcols= c(names_e, names_d)])
#' 
#' }
#' 
statestats_query <- function(ST=sort(unique(EJAM::statestats$REGION)), varnames=c(EJAM::names_e, EJAM::names_d), 
                             PCTILES=NULL, dig=2) {
  if (length(ST)==1) {
    if ( substr(tolower(ST),1,2) == "us") {
      if (is.null(PCTILES)) {PCTILES <- c("mean", 0,5,50,80,90,95,99,100)}
      x <-  with(usastats, usastats[  PCTILE %in% PCTILES, c('REGION', 'PCTILE', varnames)])
      x[ , varnames] <- round(x[ , varnames], dig)
      rownames(x) <- NULL
      return(x)
    }
  }
  
  if (is.null(PCTILES)) {
    if (missing(ST)) {
      # if all states by default, then just show means
      PCTILES <- "mean"
    } else {
      # if 1 or more states specified, but pctiles not specified, show these defaults:
      PCTILES <- c("mean", 0,5,50,80,90,95,99,100)
    }
  }
  x <-  with(statestats, statestats[REGION %in% ST & PCTILE %in% PCTILES, c('REGION', 'PCTILE', varnames)]) 
  x[ , varnames] <- round(x[ , varnames], dig)
  rownames(x) <- NULL
  x
}

#' convenient way to see mean, pctiles of DEMOG indicators from lookup table
#' @inherit statestats_query params return description details seealso examples
#' @export
#'
statestats_queryd <- function(ST=sort(unique(EJAM::statestats$REGION)), varnames=  EJAM::names_d , 
                              PCTILES=NULL, dig=2) { 
  if (is.null(PCTILES))  {
    statestats_query(ST = ST, varnames = varnames, PCTILES = NULL, dig = dig)
  } else {
    statestats_query(ST = ST, varnames = varnames, PCTILES = PCTILES, dig = dig)
  }
}

#' convenient way to see mean, pctiles of ENVIRONMENTAL indicators from lookup table
#' @inherit statestats_query return description details seealso examples
#' @export
#'
statestats_querye <- function(ST=sort(unique(EJAM::statestats$REGION)), varnames=  EJAM::names_e , 
                              PCTILES=NULL, dig=2) { 
  if (is.null(PCTILES))  {
    statestats_query(ST = ST, varnames = varnames, PCTILES = NULL, dig = dig)
  } else {
    statestats_query(ST = ST, varnames = varnames, PCTILES = PCTILES, dig = dig)
  }
}


#' convenient way to see USA mean, pctiles of Env and Demog indicators from lookup table
#' @inherit statestats_query return description details seealso examples
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param @dig how many digits to round to
#' @export
#'
usastats_query   <- function(varnames=c(EJAM::names_e, EJAM::names_d), PCTILES=NULL, dig=2) {
  statestats_query(ST="us", varnames = varnames, PCTILES = PCTILES, dig = dig)
  ## see all total counts too not just US means for just demographics not envt, including subgroups:
  # t(round(EJAMbatch.summarizer::ustotals2(bg = EJAM::blockgroupstats),2))
  # t(round(rbind(
  #   EJAMbatch.summarizer::ustotals2(bg=ejscreen::bg22), 
  #   EJAMbatch.summarizer::ustotals2(bg = EJAM::blockgroupstats)
  # ),3))
}


#' convenient way to see USA mean, pctiles of ENVIRONMENTAL indicators from lookup table
#' @inherit statestats_query return description details seealso examples
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param @dig how many digits to round to
#' @export
#'
usastats_querye  <- function(varnames=EJAM::names_e, PCTILES=NULL, dig=2) {
   statestats_query(ST="us", varnames = varnames, PCTILES = PCTILES, dig = dig)
}

#' convenient way to see USA mean, pctiles of DEMOGRAPHIC indicators from lookup table
#' @inherit statestats_query return description details seealso examples
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param @dig how many digits to round to
#' @export
#'
usastats_queryd  <- function(varnames=EJAM::names_d, PCTILES=NULL, dig=2) {
  statestats_query(ST="us", varnames = varnames, PCTILES = PCTILES, dig = dig)
}


#' convenient way to see USA MEANS of ENVIRONMENTAL and DEMOGRAPHIC indicators from lookup table
#' @inheritDotParams usastats_query
#' @export
#'
usastats_means <- function(...) {
  x = usastats_query(PCTILES = "mean", ...)
  x$REGION=NULL; x$PCTILE=NULL # so t(x) wont make everything into character class
  x=t(x)
  colnames(x)="us.avg"
  return(x)
  }
