#' @name frs
#' @docType data
#' @title EPA Facility Registry Service table of regulated sites
#' 
#' @description This is a data.table snapshot version of the EPA FRS. 
#'   You can look up sites by REGISTRY_ID in [frs], and get their location, etc.
#' @seealso  frs_by_programid  frs_by_naics frs_by_sic
#' @details 
#'  This dataset can be updated by a package maintainer by using 
#'     frs_update_datasets() (which is not an exported function)
#'
#'   The definitions of active/inactive here are not quite the 
#'   same as used in ECHO, as of 5/2023.
#'   
#'   Also, EJScreen has maps of EPA-regulated facilities of a few program types,
#'   as provided here: <https://www.epa.gov/ejscreen/ejscreen-map-descriptions#sites-reporting-to-epa>
#' \preformatted{
#' 
#'  As of June 2023
#'  
#'  Count of    all REGISTRY_ID rows:   7,441,086
#'  Count of unique REGISTRY_ID values: 4,705,744
#'  Clearly inactive unique IDs:        1,436,096
#'  Assumed   active unique IDs:        3,269,648
#' 
#' frs rows total:            3,456,042 
#' frs clearly inactive IDs:  1,436,096 
#' frs rows actives:          2,573,338 
#' frs_by_programid rows:     3,440,036 
#' frs_by_naics rows:           679,471 
#' frs_by_sic rows:           1,081,742
#' 
#'   Classes ‘data.table’ and 'data.frame':	 
#'   Retained only these columns for this package
#'   
#'   $ lat           : num  18.4 18.4 18.5 18.2 18.2 ...
#'   $ lon           : num  -66.1 -66.1 -66.8 -67.1 -67.2 ...
#'   $ REGISTRY_ID   : chr  "110000307668" "110000307695" "110000307739" "110000307757" ...
#'   $ PRIMARY_NAME  : chr  "HB FULLER COMPANY HBF PUERTO RICO" "RAMCO CHEMICALS INCORPORATED" 
#'   $ NAICS         : chr  "325520" "" "311119" "312120" ...
#'   $ SIC           : chr  "2842" "2048" "2047, 2048, 2091" ...
#'   $ PGM_SYS_ACRNMS: chr  "NCDB:I02#19880913A2001 2, RCRAINFO:PRD090122136
#'    
#'    }
NULL
