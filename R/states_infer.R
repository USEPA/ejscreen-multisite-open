#' states_infer
#' Get cleaned table of US State etc. by siteid, from lat/lon or other info
#' @param x data.frame or data.table with either ST column or lat and lon columns, 
#'   and optionally a column with siteid or column called n
#'
#' @return data.frame with unique siteid, ST, etc.
#' @export
#'
states_infer <- function(x) {
  
  stop("states_infer() function is not finished yet")
  
  sites2states <- x # sites2states_or_latlon # could rename param or variable in code to avoid making this copy
  if (is.data.table(sites2states)) setDF(sites2states)
  if (missing(sites2states)) {
    bad_sites2states <- TRUE
  } else {
    bad_sites2states <- FALSE
    
    ### overly inflexible to require identical lists of siteid values -- might want to get state pctiles for all where possible even if 1 site lacks ST, and ignore extra info too not found in sites2blocks
    # if (!all(unique(sites2blocks$siteid) %in% sites2states$siteid)) {
    #   warning("cannot provide state percentiles unless all siteid values in sites2blocks are also in sites2stats")
    #   bad_sites2states <- TRUE
    # }
    # if (!all(unique(sites2states$siteid) %in% sites2blocks$siteid)) {
    #   warning("cannot provide state percentiles unless all siteid values in sites2stats are also in sites2blocks")
    #   bad_sites2states <- TRUE
    # }
    
    if (!("siteid" %in% names(sites2states))) {
      if ("n" %in% names(sites2states)) {  # use n or rownumber as siteid if not explicitly provided as siteid column
        sites2states$siteid <- sites2states$n
      } else {
        siteid <- 1:NROW(sites2states)
      }
    }
    
    stop("not done")
    if (all("lat" %in% names(sites2states), "lon" %in% names(sites2states))) {
      sites2states$ST <- state_from_latlon(lat = sites2states$lat, lon = sites2states$lon)[, "ST"]
    } else {
      
      
    }
    
    
    if (!all(c("siteid", "ST") %in% names(sites2states))) {
      warning("cannot provide state percentiles unless sites2states param is data.frame with colnames siteid and ST (or siteid and latlon).
            Try state_from_latlon(lat= pts$lat, lon= pts$lon) to identify the state containing each site based on lat/lon of each siteid.")
      bad_sites2states <- TRUE
    } 
    if (!all(sites2states$ST %in% EJAM::stateinfo$ST)) {
      warning("cannot provide state percentiles unless all sites2states$ST values are valid 2-character State abbreviations")
      bad_sites2states <- TRUE
    } 
  }
  if (bad_sites2states) {
    sites2states <- data.frame(siteid=1:length(unique(sites2blocks$siteid)), ST=NA)
  }
  
  return(sites2states)
}
