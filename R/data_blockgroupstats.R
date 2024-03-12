#' @name blockgroupstats
#' @docType data
#' @title blockgroupstats (DATA) EJScreen demographic and environmental indicators for Census block groups
#'
#' @description
#'   The EJScreen dataset (demographic, environmental indicators).
#'
#'   For EJ Indexes, see [bgej]
#'
#' @details
#'   - For 08/2023 through early 2024, it was EJScreen 2.2 data, ACS 2017-2021.
#'   - For mid-2024, EJAM and EJScreen use ACS 2018-2022 data.
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
