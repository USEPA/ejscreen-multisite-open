
#' helper function to rename variables that are colnames of data.frame
#' 
#' Changes column names to friendly or long from original names in FTP site file
#' 
#' @details YOU SPECIFY A TYPE LIKE "api" or "long" 
#'   or a colnames from map_headernames like "rname" or "vartype"
#'   
#'   Note that you actually can use this to extract any info from map_headernames (which 
#'   here is called mapping_for_names). 
#'   
#'   NOTE: if you ask to rename your words to a known type like rname or apiname, and 
#'   the namesnow is not found among the oldtype, then it is not renamed, and those are returned as unchanged.
#'   BUT, if you specify as newtype some column that is not a known type of name, like "varcategory" 
#'   then it will instead return an empty string for those in namesnow that are not found among the oldtype.
#'   That way if you are really seeking a new name, but it cannot rename, it keeps the old name; while
#'   if you are really seeking metadata like what category it is in, it returns a blank if the old name is not found at all.
#' @param namesnow vector of colnames to be renamed
#' @param oldtype friendly or long or original, or csv or r or api,
#'   or a colname of map_headernames, used if one of those known types was not specified.
#' @param newtype friendly or long or original, or csv or r or api,
#'   or a colname of map_headernames, used if one of those known types was not specified.
#' @param mapping_for_names default is a dataset already in the package.
#'   A data.frame passed to [fixnames()] to do the work
#'   with columns oldnames (original), longname_tableheader (long), newnames_ejscreenapi (friendly)
#' @seealso [varinfo()]
#' @return Vector or new column names same length as input
#' @examples  # see package tests
#'   fixcolnames(c("RAW_D_INCOME", "S_D_LIFEEXP")) # assumes it was in original format as API outputs
#'   fixcolnames(c("RAW_D_INCOME", "S_D_LIFEEXP"), newtype = "long")
#'   fixcolnames(c("resp","rsei"),  oldtype = "r", newtype = "long")
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
fixcolnames <-  function(namesnow, oldtype='original', newtype='friendly', mapping_for_names) {
  
  if (missing(mapping_for_names)) {
    if (exists('map_headernames')) {
      mapping_for_names <- map_headernames
    } else {
      warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
      return(namesnow)
    }
  } else {
    # x <- try(exists(mapping_for_names)) # this does not work as intended
    # if (inherits(x, "try-error")) {
    #   warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
    #   return(namesnow)
    # } else {
    #   if (x & is.data.frame(mapping_for_names)) {
    #     # it is a df that exists, and valid colnames are checked for later
    #   } else {
    #     warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
    #     return(namesnow)
    #   }
    # }
  }
  
  # this later maybe could be recoded to just check all 3 as possible from column, so you dont have to specify it.
  # fromcolname <- oldtype # if no match here for what they provide as oldtype
  fromcolname <- switch(oldtype,
                         api = 'apiname', # if they say oldtype="api" then look up each of namesnow within the column map_headernames$apiname 
                         csv = 'csvname2.2',
                         r =   'rname',
                         original = 'oldnames',   # which might be csvname2.2 or apiname or rname
                        shortlabel = "shortlabel", 
                          friendly = 'newnames_ejscreenapi',  # like rname
                         long = 'longname_tableheader',      # similar to  csvlongname or gdb23longname or csv_descriptions_name, etc.
                         oldtype # default is exactly what they claim is a colname in map_headernames # if no match here for what they provide as oldtype
  )
  # tocolname <- newtype # if no match here for what they provide as newtype
  tocolname <- switch(newtype,
                       api = 'apiname',  # if they say newtype="api" then rename to names found in the column map_headernames$apiname, the names the API reports
                       csv = 'csvname2.2',
                       r =   'rname',
                       original = 'oldnames',   # should be apiname when available, but csvname2.2 if there is no apiname
                      shortlabel = "shortlabel",
                        friendly = 'newnames_ejscreenapi', # should be the same as rname
                       long = 'longname_tableheader'  ,
                       newtype # default is exactly what they claim is a colname in map_headernames # if no match here for what they provide as newtype
  )
  if (!(tocolname %in% colnames(mapping_for_names)))   {warning(paste('returning unchanged names because mapping_for_names has no column called ', tocolname))
    return(namesnow)}
  if (!(fromcolname %in% colnames(mapping_for_names))) {warning(paste('returning unchanged names because mapping_for_names has no column called ', fromcolname))
    return(namesnow)}
  
  
  oldnames <- mapping_for_names[,fromcolname]
  newnames <- mapping_for_names[,tocolname]
  newnames <- newnames[oldnames %in% namesnow]
  oldnames <- oldnames[oldnames %in% namesnow]
  
  # if no match found in oldnames, we will just return the name unfixed/unchanged,
  # if they asked for renaming to a name type like api, csv, original, etc.
  # BUT, if they asked to rename to some other column such as varlist or vartype, 
  #  then for the cases where there is no match found among oldnames, we should return NA or empty string
  # rather than returning the name unchanged, since it is confusing to ask for varlist and get back the input term.
  nametypes <- c(
    'api', 'apiname',
    'csv','csvname2.2',
    'r', 'rname',
    'original', 'oldnames', 
      'friendly',   'newnames_ejscreenapi', 
    'shortlabel' , "shortlabel",
    'long', 'longname_tableheader'
  )
  if (!(newtype %in% nametypes)) {
    namesnow[!(namesnow %in% oldnames)] <- '' # so that when there is no match below, it is left like this and is returned as ''
  }
  
  namesnow[match(oldnames, namesnow)] <- newnames

  return(namesnow)
}
