
#' Find EPA-regulated facilities in FRS by NAICS code (industrial category)
#'
#' Get lat lon, Registry ID, given NAICS industry code(s)
#' Find all EPA Facility Registry Service (FRS) sites with this exact NAICS code (not subcategories)
#' @details 
#'  The EPA also provides a [FRS Facility Industrial Classification Search tool](https://www.epa.gov/frs/frs-query#industrial)
#'  where you can find facilities based on NAICS or SIC.
#'  
#' NOTE: many FRS sites lack NAICS code!
#'
#'   Also, this function does not find the sites
#'   identified by FRS data as being in a child NAICS (subcategory of your exact query)!
#'
#'   Relies on  frs_by_naics (a data.table)
#'
#'   See info about NAICS industry codes at <https://www.naics.com/search>
#'   
#' @param naics a vector of naics codes, or
#'   a data.table with column named code, as with output of [EJAM::naics_from_any()]
#' @return A data.table (not just data.frame) with columns called
#'   lat, lon, REGISTRY_ID, NAICS (but see the id_only parameter)
#'   
#' @seealso [frs_from_naics()]  [frs_from_sic()] [latlon_from_sic()] [regid_from_naics()] [naics_from_any()]
#'   
#' @examples
#'   regid_from_naics(321114)
#'   latlon_from_naics(321114)
#'   latlon_from_naics(EJAM::naics_from_any("cheese")[,code] )
#'   head(latlon_from_naics(c(3366, 33661, 336611), id_only=TRUE))
#'   # mapfast(frs_from_naics(336611)) # simple map
#'
#' @export
#'
latlon_from_naics <- function(naics, id_only=FALSE) {

  if (missing(naics)) {return(NULL)}
  if (data.table::is.data.table(naics) & "code" %in% names(naics)) {naics <- naics$code} # flexible in case it was given output of EJAM::naics_from_any() which is a table not just code

  if (!exists("frs_by_naics")) dataload_from_pins("frs_by_naics")

  if (id_only) {
    return(frs_by_naics[NAICS %in% naics, REGISTRY_ID])
  } else {
    return(frs_by_naics[NAICS %in% naics, ])
  }
}
########################################## #


#' @inherit latlon_from_naics
#' @return see  [latlon_from_naics()]
#' @export
#'
regid_from_naics <- latlon_from_naics

######################################## #
