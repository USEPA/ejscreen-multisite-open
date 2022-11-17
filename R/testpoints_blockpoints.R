#' Get some random US locations as points to try out/ for testing
#'
#' @param n how many points do you want?
#' @param weighting geo means each block is equally likely, pop means the points
#'   are population weighted (Census 2020 pop) so they represent a random sample of 
#'   where US residents live - the average person.
#' @param ST can be a character vector of 2 letter State abbreviations to pick from only some States
#' @param as.dt if TRUE (default), a data.table, but if FALSE then a data.frame
#'
#' @return see as.dt paramter. It returns a table with columns blockid, lat, lon 
#' @export
#'
testpoints_blockpoints <- function(n=10, weighting='geo', ST=is.null, as.dt = TRUE) {
  
  filteredrownums <- EJAMblockdata::blockid2fips[substr(blockfips,1,2) %in% ST, which = TRUE]
  maxrow <- length(filteredrownums)
  
  if (weighting == 'geo') {
    probability <- NULL
  }
  if (weighting == 'pop') {
    probability <- EJAMblockdata::blockwts[filteredrownums, blockwt]
  }
  if (weighting == 'area') {
    stop('not implemented yet')
    # would use 
    # probability <- EJAMblockdata::blockwts[filteredrownums, area] # if that col were there
  }
  
  if (as.dt) {
    return(
      EJAMblockdata::blockpoints[filteredrownums, ][sample.int(maxrow, size = n, replace = FALSE, prob = probability), ]
    )
  } else {
    return(
      as.data.frame(
        EJAMblockdata::blockpoints[filteredrownums, ][sample.int(maxrow, size = n, replace = FALSE, prob = probability), ]
      )
    )
  }
}
