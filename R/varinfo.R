
#' varinfo gets metadata for a variable, like its type, definition, decimalS rounding, etc.
#' 
#' This is just a way to query map_headernames, which has info about each indicator or 
#' variable used in EJAM.
#' @details See map_headernames for what kind of information is available there. 
#' But if a variable appears twice+ in var or in map_headernames, info returned only for the 1st row of those
#' @param var vector of variable names such as c("pctlowinc", "cancer") or c(names_d, names_d_subgroups)
#'   (and must be found in the column of map_headernames indicated by varnametype parameter below).
#'   
#' @param info types of metadata/info needed, such as "decimals", "long", etc.
#'   which should be among colnames of map_headernames, 
#'   or alias like "long" as allowed by fixcolnames()
#'   
#' @param varnametype optional. colname of map_headernames to use when looking for var, 
#'   like "rname" or "api" or "long"
#'   
#' @seealso fixcolnames() [table_rounding_info()]  
#' @return data.frame of 1 or more rows, 1 or more columns, where
#'   
#'  rowsnames are var (indicators like "pctmin")
#'  
#'  colnames are info (metadata   like "decimals")
#'  
#'  Cells of table are metadata such as what type of indicator is that var, how many 
#'  decimal places of rounding should be displayed for it in tables, etc.
#'    
#'  Results can be character, numeric, etc. depending on what info is requested
#'  
#' @examples  
#' varinfo("traffic.score", "decimals")
#' varinfo(names_d, "long")
#' myvars <- c(names_d, names_d_subgroups, names_e)
#' myinfo <- "percentage"
#' cbind(  is.a.percentage = varinfo(myvars, myinfo) )
#' cbind(varinfo(names_all_r, "pctile."))
#' myinfo <- "long"
#' cbind(varinfo(myvars, myinfo) )
#' table_rounding_info(names_e)
#'  
#' varinfo(
#'  var = c(names_these, names_d_pctile),
#'  info = c(
#'  "topic_root_term", "varcategory", "vartype", "percentage", "pctile.", "calculation_type"
#' ))
#' 
#' varinfo(names_all_r, c("varcategory", "varlist", "in_api", "in_bgcsv"))
#' 
#' 
#' @keywords internal
#' 
varinfo <- function(var = map_headernames$rname, info=colnames(map_headernames), varnametype="rname") {
  
  # rows    are var  (indicators like "pctmin")
  # columns are info (metadata   like "decimals")
  
  info_or_alias <- info
  info_true_name <- fixmapheadernamescolname(info_or_alias)
  x <- map_headernames[match(var, map_headernames[ , varnametype]), info_true_name, drop = FALSE]
  rownames(x) <- make.unique(var) # BUT NOTE if a variable appears twice+ in map_headernames, info returned only for the 1st row of those
  colnames(x) <- info_or_alias
  return(x)
}
#################################################################### # 


# # NOT EXPORTED, NOT USED # REPLACED BY varinfo(), this is the older version that was awkward code
# 
# varinfo2 <- function(var, info, varnametype="rname") {
# 
#   # rows    are var  (indicators like "pctmin")
#   # columns are info (metadata   like "decimals")
#   
#   if (length(info) > 1) {
#     x <- sapply(info, function(info1) fixcolnames(var, oldtype = varnametype, newtype = info1))
#     if (length(var) == 1) x <- t(x)
#     x = as.data.frame(x, stringsAsFactors = FALSE)
#     rownames(x) <- var
#     colnames(x) <- info
#   } else {
#     x <- fixcolnames(var, oldtype = varnametype, newtype = info)
#     x <- data.frame(x, stringsAsFactors = FALSE)
#     rownames(x) <- var
#     colnames(x) <- info
#   }
#   return(x)
#   
#   #################################################################### # 
#   if (1 == 0) {
#     
#     ## tests/ examples:
#     t1 <- t2 <- t3 <- t4 <- tt1 <- tt2 <- tt3 <- tt4 <- NA
#     x = "rname"
#     x = "long" # fails if just using varinfo2
#     t1 <- varinfo(var =   'pctmin',       info = 'decimals')
#     t2 <- varinfo(var = c('pctmin','pm'), info = 'decimals')
#     t3 <- varinfo(var =   'pctmin',       info = c('decimals', x))
#     t4 <- varinfo(var = c('pctmin','pm'), info = c('decimals', x))
#     t1; t2; t3; t4
#     str(t1); str(t2); str(t3); str(t4)
#     
#     tt1 <- varinfo2(var =   'pctmin',       info = 'decimals')
#     tt2 <- varinfo2(var = c('pctmin','pm'), info = 'decimals')
#     tt3 <- varinfo2(var =   'pctmin',       info = c('decimals', x))
#     tt4 <- varinfo2(var = c('pctmin','pm'), info = c('decimals', x))
#     tt1; tt2; tt3; tt4
#     str(tt1); str(tt2); str(tt3); str(tt4)
#     
#     all.equal(t1, tt1)
#     all.equal(t2, tt2)
#     all.equal(t3, tt3)
#     all.equal(t4, tt4)
#     
#     ################ # 
#     
#     t1 <- t2 <- t3 <- t4 <- tt1 <- tt2 <- tt3 <- tt4 <- NA
#     x <- c( "rname",
#             'api' , 'apiname',
#             'csv' , 'csvname2.2',
#             'r' , 'rname',
#             'original' , 'oldnames',
#             'friendly' , 'newnames_ejscreenapi',
#             'long', 'longname_tableheader')
#     t1 <- varinfo(var =   'pctmin',       info = 'decimals')
#     t2 <- varinfo(var = names_all_r, info = 'decimals')
#     t3 <- varinfo(var =   'pctmin',       info = c('decimals', x))
#     t4 <- varinfo(var = names_all_r, info = c('decimals', x))
#     #t1; t2; t3; t4
#     #str(t1); str(t2); str(t3); str(t4)
#     
#     tt1 <- varinfo2(var =   'pctmin',       info = 'decimals')
#     tt2 <- varinfo2(var = names_all_r, info = 'decimals')
#     tt3 <- varinfo2(var =   'pctmin',       info = c('decimals', x))
#     tt4 <- varinfo2(var = names_all_r, info = c('decimals', x))
#     #tt1; tt2; tt3; tt4
#     #str(tt1); str(tt2); str(tt3); str(tt4)
#     
#     all.equal(t1, tt1)
#     all.equal(t2, tt2)
#     all.equal(t3, tt3)
#     all.equal(t4, tt4)
#     
#   }
#   
# }
# #################################################################### # 

