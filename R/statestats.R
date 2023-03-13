#' @name statestats
#' @docType data
#' @aliases percentiles
#' @title data.frame of 100 percentiles and means for each US State and PR and DC.
#' @description data.frame of 100 percentiles and means 
#'   for each US State and PR and DC (approx 5,300 rows)
#'   for all the block groups in that zone (e.g., block groups in [blockgroupstats])
#'   for a set of indicators such as percent low income.
#'   Each column is one indicator (or specifies the percentile).
#'   
#'   This should be similar to the lookup tables in the gdb on the FTP site of EJScreen,
#'   except it also has data for the demographic race/ethnicity subgroups.
#'   For details on how the table was made, see /EJAM/data-raw/usastats_subgroups.R
NULL
