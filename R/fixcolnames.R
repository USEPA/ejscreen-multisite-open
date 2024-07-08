
#' helper function to rename variables that are colnames of data.frame
#' 
#' Changes variable names like colnames to long plain-English headers or short labels for plots
#' 
#' @details You specify a type like "api" or "longname" or "shortname"
#'   or one of `colnames(map_headernames)` like "rname" or "vartype"
#'   
#'   Also, you can use this to extract any info from `map_headernames` (which 
#'   here is called mapping_for_names).
#'   
#'   NOTE: if you ask to rename your words to a known type like rname or apiname, and 
#'   the namesnow is not found among the oldtype, then it is not renamed, and those are returned as unchanged.
#'   BUT, if you specify as newtype some column that is not a known type of name, like "varcategory" 
#'   then it will instead return an empty string for those in namesnow that are not found among the oldtype.
#'   That way if you are really seeking a new name, but it cannot rename, it keeps the old name
#'   while if you are really seeking metadata like what category it is in, 
#'   it returns a blank if the old name is not found at all.
#'   
#' @param namesnow vector of colnames to be renamed
#' @param oldtype "longname" or "shortname", or "csv" or "r" or "api", etc.
#'   or a colname of map_headernames, used if one of those known types was not specified.
#' @param newtype "longname" or "shortname", or "csv" or "r" or "api", etc.
#'   or a colname of map_headernames, used if one of those known types was not specified.
#' @param mapping_for_names default is a dataset already in the package.
#' 
#' @seealso [varinfo()]
#' 
#' @return Vector or new column names same length as input
#' @examples  # see package tests
#'   fixcolnames(c("RAW_D_INCOME", "S_D_LIFEEXP")) # assumes it was in original format as API outputs
#'   fixcolnames(c("RAW_D_INCOME", "S_D_LIFEEXP"), newtype = "longname")
#'   fixcolnames(c("resp","rsei"),  oldtype = "r", newtype = "longname")
#'   fixcolnames(c("resp","rsei"),  oldtype = "r", newtype = "api")
#'   fixcolnames(c("resp","pctlowinc"), oldtype = "r", newtype = "varlist")
#'   
#'   # the columns "newsort" and "reportsort" provide useful sort orders
#'   x <- map_headernames$rname[map_headernames$varlist == "names_d"] # EJAM package has names_d
#'   print("original order"); print(x) 
#'   x <-  sample(x, length(x), replace = FALSE)   
#'   print("out of order"); print(x)
#'   print("fixed order")
#'   x[ order(fixcolnames(x, oldtype = "r", newtype = "newsort")) ]
#'
#' @export
#' 
fixcolnames <-  function(namesnow, oldtype='csvname2.3', newtype='r', mapping_for_names) {
  
  if (missing(mapping_for_names)) {
    if (exists('map_headernames')) {
      mapping_for_names <- map_headernames
    } else {
      warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
      return(namesnow)
    }
  }
  
  fromcolname <- fixmapheadernamescolname(oldtype)
  tocolname   <- fixmapheadernamescolname(newtype)
  
  if (!(tocolname %in% colnames(mapping_for_names)))   {
    warning(paste('returning unchanged names because mapping_for_names has no column called ', tocolname))
    return(namesnow)
  }
  if (!(fromcolname %in% colnames(mapping_for_names))) {
    warning(paste('returning unchanged names because mapping_for_names has no column called ', fromcolname))
    return(namesnow)
  }
  
  oldnames <- mapping_for_names[, fromcolname]
  newnames <- mapping_for_names[, tocolname]
  newnames <- newnames[oldnames %in% namesnow]
  oldnames <- oldnames[oldnames %in% namesnow]
  
  # if no match found in oldnames, we will just return the name unfixed/unchanged,
  # if they asked for renaming to a name type like api, csv, original, etc.
  # BUT, if they asked to rename to some other column such as varlist or vartype, 
  #  then for the cases where there is no match found among oldnames, we should return NA or empty string
  # rather than returning the name unchanged, since it is confusing to ask for varlist and get back the input term.
  
  nametypes <- sort(c(
    # canonical terms
    names(eval(formals(fixmapheadernamescolname)$alias_list)), 
    # synonyms for those
    as.vector(unlist(eval(formals(fixmapheadernamescolname)$alias_list)))
  ))
  
  # nametypes <- c("api", "apiname", 
  #   "csv", "csvname", "csvname2.2", "csvname2.3"
  #   "description", "full", 
  #   "long", "longname", "longname_tableheader", "longnames", "newnames_ejscreenapi",
  #   "old", "oldname", "oldnames", "original",
  #   "r", "rname",    "friendly", 
  #   "label", "labels",
  #   "short", "shortlabel", "shortname", "shortnames")
  # older:
  # nametypes <- c(
  #   'api', 'apiname',
  #   'csv','csvname2.2',
  #   'r', 'rname',
  #   'original', 'oldnames', 
  #   'friendly',   'newnames_ejscreenapi', 
  #   'shortlabel' , "shortlabel",
  #   'long', 'longname_tableheader'
  # )
  
  if (!(newtype %in% nametypes)) {
    namesnow[!(namesnow %in% oldnames)] <- ''
    # so that when there is no match below, it is left like this and is returned as ''
  }
  
  namesnow[match(oldnames, namesnow)] <- newnames
  
  return(namesnow)
}
