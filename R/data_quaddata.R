#' @name quaddata
#' @docType data
#' @title data.table used to create index of all US block point locations
#' @details 
#'    8,174,955 rows when non-populated blocks are kept. 
#'    5,806,512 rows have Census 2020 population (and blockwt) > 0. 
#'    This is the largest file used by the package, and is 168 MB as a file, for 2020 Census.
#'      - blockid 
#'      - BLOCK_X, BLOCK_Y, BLOCK_Z  (not lat, lon)
#'  
#'     localtree is the index made from quaddata  
#'     (QuadTree class, via SearchTrees pkg), not a data.table
#'     
#' @seealso [indexblocks()]
NULL
