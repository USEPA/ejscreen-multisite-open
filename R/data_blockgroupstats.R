#' @name blockgroupstats
#' @docType data
#' @title EJScreen demographic and environmental indicators for Census block groups
#'
#' @description 
#'   The EJScreen dataset (demographic, environmental indicators).
#'   
#'   For EJ Indexes, see [bgej]
#'   
#' @details 
#'   - As of 08/2023 it was the EJScreen 2.2 version of data, which used ACS 2017-2021.
#'   
#'   Each year this should be re-created as for the latest version.
#'   See attributes(blockgroupstats) 
#'   
#'   It is a data.table of US Census blockgroups (not blocks). 
#'   With PR, and Island Areas
#'   
#'   See <https://www.epa.gov/ejscreen>
#'   
#'   Column names include bgfips, bgid (for join to blockwt$bgid), pop, pctlowinc, etc.
#'   
#'   
NULL
