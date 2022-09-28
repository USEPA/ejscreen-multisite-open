#' @name blockgroupstats
#' @docType data
#' @title EJSCREEN demographic and enviromental indicators for Census block groups
#'
#' @description 
#' This is essentially the EJScreen dataset, plus more demographic subgroups.  
#'   It has demographic and environmental data from EJScreen.
#'   
#' @details 
#'   As of 10/2022 it will be EJScreen 2.1 version of data, which uses ACS 2016-2020.
#'   As of  4/2022 it was the EJScreen 2.0 version of data, which used ACS 2015-2019.
#'   EJScreen 2.0 was released 2/18/2022 (raw data download avail 2/22/2022).
#'   
#'   NOTE: It also has the race/ethnic subgroups that add up to minority or people of color 
#'   
#'   Each year this could be created as for the latest version.
#'   See attributes(blockgroupstats)
#'   It is also available in a similar form via the ejscreen package on github, 
#'   but there are differences in which columns are kept.
#'   
#'   It is a data.table of US Census blockgroups (not blocks). With PR, 242,335 rows, approx 175 columns.
#'   See \url{https:\\www.epa.gov\ejscreen}
#'   
#'   column names include bgfips, bgid (for join to blockwt$bgid), pop, pctlowinc, pcthisp, etc.
#'   
#'   See notes on cleaning up and changing the dataset starting from ejscreen::bg22plus
#'   
#'   see source code and notes in EJAM::create_blockgroupstats  
#'   
NULL
