#' rmost - utility to rm(list=ls()) but not remove key datasets EJAM uses 
#'
#' @export
#'
rmost <- function(notremove=c(
  'rmost', 
  'localtree', 'blockgroupstats', 'usastats', 'statestats', 
  'bgid2fips', 'blockid2fips', 'blockpoints', 'blockwts', 'quaddata',
  'bgej'
)) {
  rm(list = setdiff(
    ls(envir = globalenv()),
    notremove
  ),
  envir = globalenv()
  )
}
