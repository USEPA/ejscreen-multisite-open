#' @name usastats
#' @docType data
#' @title usastats (DATA) data.frame of 100 percentiles and means
#' @description data.frame of 100 percentiles and means (about 100 rows)
#'   in the USA overall, across all locations (e.g., block groups in [blockgroupstats])
#'   for a set of indicators such as percent low income.
#'   Each column is one indicator (or specifies the percentile).
#'   
#'   This should be similar to the lookup tables in the gdb on the FTP site of EJScreen,
#'   except it also has data for the demographic race/ethnicity subgroups.
#'   
#'   For details on how the table was made, see source package files
#'   EJAM/data-raw/datacreate_usastats_2023-08-28.R 
#'   and possibly /EJAM/data-raw/usastats_subgroups.R
#'   
#'   See also [statestats]
NULL
