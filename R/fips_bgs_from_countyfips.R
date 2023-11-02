#' bg_from_county - Analyze US Counties as if they were sites, to get EJ indicators summary for each county
#'
#' @param fips County FIPS vector (ideally as character not numeric values)
#'
#' @return data.table with all pairs of county fips - bgid, and a unique siteid assigned to each county 
#' @export
#'
#' @examples 
#'  bg_from_county(c('01001','72153'))
#'  # Largest US Counties by ACS Population Totals:
#'  blockgroupstats[ , .(ST = ST[1], countypop = sum(pop)),
#'   by=.(FIPS = substr(bgfips,1,5))][order(-countypop),][1:20, .(
#'   CountyPopulation = prettyNum(countypop, big.mark = ","), FIPS, ST)]
#'  
bg_from_county <- function(fips) {
  if (any(is.numeric(fips))) {
    message("leading zeroes being inferred since FIPS was provided as numbers not character class")
    
    }
  # accept county fips vector
# return counties2bgs table of pairs so doaggregate_blockgroups() or whatever can take that and do full EJ stats.
 
  county2bg <- bgpts[substr(bgfips,1,5) %in% fips, .(countyfips = substr(bgfips,1,5), bgid) ]
  county2bg[, siteid := .GRP , by = "countyfips"]
  county2bg[, .(siteid, countyfips, bgid)]
}

#' counties_as_sites
#' @export
#' @inherit bg_from_county
counties_as_sites <- bg_from_county

#' fips_bgs_from_countyfips
#' @export
#' @inherit bg_from_county
fips_bgs_from_countyfips <- bg_from_county
