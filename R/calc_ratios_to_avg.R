
#' get ratios of each site's scores to US means (using output of batch buffering)
#' 
#' Used to create input to[ boxplots_ratios()]
#' 
#' @details Should recode to use variable name defaults from package not hardcoded here.
#' @param out data.frame output from [ejscreenapi_plus()] or from [ejscreenapi()] or doaggregate(),
#'   one row per buffer or site, and 
#'   columns for indicators named in evarnames, dvarnames, avg.evarnames, avg.dvarnames
#' @param evarnames vector of variable names for environmental indicators in out, like pm, o3, etc.
#' @param dvarnames vector of variable names for demographic indicators in out, such as pctlowinc
#' @param zone.prefix us or state, must fit with colnames in out such as  avg.pctlowinc and state.avg.pctlowinc
#' @param avg.evarnames use only if user-specific variable names are in out, 
#'   with defaults like avg.pm
#' @param avg.dvarnames use only if user-specific variable names are in out, 
#'   with defaults like  avg.pctlowinc
#'
#' @return a list with ratios_d and ratios_e which are 
#'   vectors of numbers the lengths of dvarnames and evarnames
#'
#' @examples
#'  out <- testoutput_ejscreenit_5$table # has long names
#'  names(out) <- fixcolnames(names(out1),'long','r')
#'  out2 <- testoutput_ejscreenapi_plus_5 # has r names already
#'  
#'  # these are data.frames, but the colnames are different:
#'  #   names like "Demog.Index" but they are ratios
#'  calc_ratios_to_avg(out)$ratios_d
#'  #   names like "ratio.to.avg.Demog.Index"
#'  out[ , names_d_ratio_to_avg] # needs EJAM for names_d_ratio_to_avg
#'  
#'  boxplots_ratios(calc_ratios_to_avg(out)$ratios_d,"pctlowinc","% low income", 
#'      wheretext="Within a mile of")
#'   
#' @keywords internal
#' @export
#'   
calc_ratios_to_avg <- function(out, 
                          evarnames = map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_e"], #EJAM ::names_e, 
                          dvarnames = map_headernames$newnames_ejscreenapi[map_headernames$varlist %in% c("names_d", "names_d_subgroups" )], #EJAM ::names_d and names_d_subgroups
                          zone.prefix='', # can just specify "state." if the 
                          # indicators use state.avg.indicator (and blank if  avg.indicator as variable names)
                          avg.evarnames=paste0(zone.prefix, 'avg.', evarnames), 
                          avg.dvarnames=paste0(zone.prefix, 'avg.', dvarnames)) {
  
  ##   ratios of nearby indicator score to US overall, 
  ## try to get quick barplot(s) or PDF(density) or CDF
  ## at one site or average site or all in one graphic. 
   
  # # for examples, see non_shiny_example_script.R
 
  ## tedious part is specifying the variable names, ideally flexibly.
  ## And  this app uses Demog.Index instead of VSI.eo
   
  # just do some error checking here:
   if (is.data.table(out)) {setDF(out)}
  # confirm those colnames are there
  d_haveboth <- (avg.dvarnames %in% names(out)) & (dvarnames %in% names(out))
  e_haveboth <- (avg.evarnames %in% names(out)) & (evarnames %in% names(out))
  if (any(!e_haveboth, !d_haveboth)) {
    warning('some variable names are not found in out - ignoring those')
    cat('Missing from outputs of API, but expected to see here: \n\n')
    if (any(!d_haveboth)) cat("Demog: ", paste(setdiff(c(dvarnames, avg.dvarnames), names(out)), collapse = ", "), '\n\n')
    if (any(!e_haveboth)) cat("Envt: " , paste(setdiff(c(evarnames, avg.evarnames), names(out)), collapse = ", "), '\n\n')
    }
  avg.evarnames <- avg.evarnames[e_haveboth] # only use avg if it and its base are there
  evarnames     <-     evarnames[e_haveboth]
  avg.dvarnames <- avg.dvarnames[d_haveboth]
  dvarnames     <-     dvarnames[d_haveboth]
  
  allnames <- c(evarnames, dvarnames, avg.evarnames, avg.dvarnames)
  if (length( allnames) == 0) {stop("None of the specified (or expected default if not specified) Variable names were found in the provided dataset")}
  if (length(evarnames) == 0) {stop("None of the specified (or expected default if not specified) evarnames were found in the provided dataset")}
  if (length(dvarnames) == 0) {stop("None of the specified (or expected default if not specified) dvarnames were found in the provided dataset")}
  
  
  # blanks crashed it - make any non numeric into NA # ejscreenapi_plus() ADDS COMMAS AND MAKES NUMERIC pop COUNTS INTO A CHARACTER FIELD
  if (any(!is.numeric(out[ , allnames]))) {
    # warning('some values are not numeric - replacing those with NA')
    out[ , allnames] <- suppressWarnings(sapply(out[ , allnames], as.numeric))
  }
  
  # calculating ratio is simple
  ratios_e <- out[ , evarnames] / out[ , avg.evarnames]
  ratios_d <- out[ , dvarnames] / out[ , avg.dvarnames]
  
  if (any(sapply(ratios_d, is.infinite)) | any(sapply(ratios_e, is.infinite))) {
    warning('some averages probably were zero because ratio is infinite and replaced with NA')
    ratios_d[sapply(ratios_d, function(x) !is.na(x) & is.infinite(x))  ] <- NA
    ratios_e[sapply(ratios_e, function(x) !is.na(x) & is.infinite(x))  ] <- NA
    }
  if (any(is.na(c(ratios_d, ratios_e)))) {warning('some ratios are NA because a score or the average was NA or avg was zero')}
  
  return(list(ratios_d = ratios_d, ratios_e = ratios_e))
}
