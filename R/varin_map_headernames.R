
#' utility to check if a variable or term is in map_headernames and where
#'
#' @param query variable name or fragment to look for in map_headernames
#'   columns, looking within just column names listed in cols_with_names
#' @param ignore.case optional, like in grepl()
#' @param cols_with_names optional, colnames of map_headernames to check in
#'
#' @return data.frame of info about where query was found
#' @examples
#' varin_map_headernames("lowinc")
#' varin_map_headernames("POV", ignore.case = T)
#' varin_map_headernames("POV", ignore.case = F)
#' varin_map_headernames("ratio.to")
#' @seealso [varinfo()]
#' 
#' @export
#' @keywords internal
#'
varin_map_headernames <- function(query = "lowinc", ignore.case = TRUE, cols_with_names = c("oldname",
                                                                                            "apiname", 
                                                                                            "api_synonym",
                                                                                            "acsname" ,
                                                                                            "csvname",
                                                                                            "ejscreen_csv",
                                                                                            "rname",
                                                                                            "topic_root_term",
                                                                                            "basevarname",
                                                                                            "denominator",
                                                                                            "shortlabel",
                                                                                            
                                                                                            "longname",
                                                                                            "description",
                                                                                            "csvlongname",
                                                                                            "api_description",
                                                                                            "acs_description",
                                                                                            "varlist"
)) {
  
  mh <- map_headernames[, cols_with_names]
  
  if (ignore.case) {
    exactmatch = sapply(mh, function(x) {
      tolower(query) %in% 
        tolower(x)
    })
  } else {
    exactmatch = sapply(mh, function(x) {
     query %in% x
    })
  }
    
  data.frame(
    exactmatch = exactmatch,
    
    # grepmatch.anycase = sapply(mh, function(x) {
    # any(grepl(query, as.vector(x), ignore.case = ignore.case))
    # }),
    # grepmatch.anycase.hitlist = sapply(mh, function(x) {
    #   paste0(grep(query, as.vector(x), ignore.case = ignore.case, value = T), collapse = ",")
    # }),
    
    grepmatch.hitcount = sapply(mh, function(x) {
      sum(grepl(query, as.vector(x), ignore.case = ignore.case))
    })
  )
}
