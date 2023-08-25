#' @name blockgroupstats
#' @docType data
#' @title EJScreen demographic and enviromental indicators for Census block groups
#'
#' @description 
#'   The EJScreen dataset (demographic, environmental, EJ indicators), 
#'   plus more demographic subgroups.
#'   
#' @details 
#'   - As of 08/2023 it was the EJScreen 2.2 version of data, which used ACS 2017-2021.
#'   - As of 10/2022 it was the EJScreen 2.1 version of data, which used ACS 2016-2020.
#'   
#'   Each year this should be re-created as for the latest version.
#'   See attributes(blockgroupstats) 
#'   
#'   NOTE: The race/ethnic subgroups are added to blockgroupstats for purposes of this package, 
#'   but are not in the main v2.2 files one can download from the EJScreen FTP site.
#'   
#'   It is a data.table of US Census blockgroups (not blocks). 
#'   With PR, 242,335 rows, approx 134 columns before the race/ethnic subgroups are added.
#'   See <https://www.epa.gov/ejscreen>
#'   
#'   Column names include bgfips, bgid (for join to blockwt$bgid), pop, pctlowinc, 
#'   plus the addition of pcthisp, etc.
#'   
#'   See source code and notes in EJAM/dev/notes_datasets/  or EJAM/data-raw/ 
#'   
NULL
