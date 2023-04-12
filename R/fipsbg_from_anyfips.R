#' fipsbg_from_anyfips
#' convert any FIPS codes to the FIPS of all the blockgroups that are
#'   among or within or containing those FIPS
#' @details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#' 
#' Takes a vector of one or more FIPS that could be State (2-digit), County (5-digit),
#'   Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips). 
#'   
#'   Returns unique vector of FIPS of all US blockgroups (including DC and Puerto Rico)
#'   that contain any specified blocks, are equal to any specified blockgroup fips, 
#'   or are contained within any provided tract/county/state FIPS. 
#'   
#'   stateinfo$ST[match( unique(substr(blockgroupstats$bgfips,1,2)), stateinfo$FIPS.ST)]
#' @param fips vector of US FIPS codes, as character or numeric,
#'   with or without their leading zeroes, each with as many characters
#' @seealso [fips_lead_zero()]
#' @return vector of blockgroup FIPS (or NA values) that may be much longer than the 
#'   vector of fips passed to this function.
#' @export
#'
#' @examples 
#'   # all blockgroups in one state
#'   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#'   length(fipsbg_from_anyfips("72"))
#'   # all blockgroups in this one county
#'   fipsbg_from_anyfips(30001)
#'   # all blockgroups that contain any of these 6 blocks (just one bg)
#'   fipsbg_from_anyfips( blockid2fips$blockfips[1:6])
#'   # 2 counties
#'   fipsbg_from_anyfips(c(36009,36011))
fipsbg_from_anyfips <- function(fips) {

  fips <- fips_lead_zero(fips) 
  
  # if smaller than bg (i.e., block fips), return just the parent bgs
  fips <- unique(substr(fips,1,12))
  
  # if bigger than bg, return all the child bgs
  all_us_bgfips <- blockgroupstats$bgfips
  
  # if nchar==2, state, so get all bg starting with that
  # if nchar is N, get all bg starting with those N characters
  
  len <- nchar(fips)
  bgfips <- fips[len == 12]
  nonbg <- fips[len !=  12]
  
  extrabgs <- sapply(nonbg, FUN = function(z) all_us_bgfips[startsWith(all_us_bgfips, z)])
  # extrabgs <- list(rep(NA, length(nonbg)))
  # for (thisone in nonbg) {
  #   extrabgs[[i]] <-   all_us_bgfips[startsWith(all_us_bgfips, thisone)]
  # }
  # extrabgs <- do.call(c,extrabgs)
  
  return(unlist(union(bgfips, extrabgs)))
}

