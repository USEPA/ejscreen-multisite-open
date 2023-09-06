#' @name sites2blocks_example
#' @docType data
#' @seealso [testdata_sites2blocks] is the same as [sites2blocks_example] 
#' @title  test data for 100 points to use as input to doaggregate()
#' @details data.table of output of [getblocksnearby()], using testpoints_100_dt
#'   each row is a unique site-block-distance.
#'   Can be used as input to [doaggregate()]
NULL

#' @name testdata_sites2blocks
#' @docType data
#' @seealso [testdata_sites2blocks] is the same as [sites2blocks_example] 
#' @title test data for 100 points to use as input to doaggregate()
#' @details data.table of output of [getblocksnearby()], using testpoints_100_dt
#'   each row is a unique site-block-distance.
#'   Can be used as input to [doaggregate()]
NULL

#################################### # 

#' @name testdata_sites2blocks_1000
#' @docType data
#' @seealso [testdata_sites2blocks] [testdata_sites2blocks_1000]  [testpoints_1000_dt]
#' @title test data for 1000 points to use as input to doaggregate()
#' @details data.table of output of [getblocksnearby()], using testpoints_1000_dt
#'   each row is a unique site-block-distance.
#'   Can be used as input to [doaggregate()]
NULL


########################################################### # 

#' @name test_regid
#' @docType data
#' @title test data, vector of EPA FRS Registry ID numbers
#' @details For testing, e.g., 
#' 
#'   frs_from_siteid(test_regid)
#' 
#'   mapfast(  frs_from_regid(test_regid)  )
#' 
NULL



