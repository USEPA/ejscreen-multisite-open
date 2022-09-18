#' Get summary stats on counts of unique and non-unique blocks nearby
#'
#' @param x The output of getblocksnearby()
#'
#' @return A list of stats
#' @import data.table
#' @export
#'
summary_of_blockcount <- function(x) {
  # sitecount_in <- nRowsDf # perhaps not unique??
  sitecount_out <- x[, .N, by = 'siteid']
  sitecount_unique_out       <- data.table::uniqueN(x, by = 'siteid')
  blockcount_unique          <- data.table::uniqueN(x, by = 'blockid')
  blockcount_incl_dupes      <- data.table::uniqueN(x)
  count_block_site_distances <- blockcount_incl_dupes  # number of rows in output table of all block-site pairs with their distance.
  blockcount_avgsite         <- blockcount_incl_dupes / sitecount_out  # maybe not clear or appropriate if input sites were not all unique?
  pct_of_blocks_are_unique   <- blockcount_unique / blockcount_incl_dupes
  pct_of_blocks_are_in_multiple_buffers <- 1 - pct_of_blocks_are_unique
  
  x <- list(
    # sitecount_in=sitecount_in, 
    sitecount_out=sitecount_out,
    sitecount_unique_out=sitecount_unique_out, 
    blockcount_unique=blockcount_unique, 
    blockcount_incl_dupes=blockcount_incl_dupes, 
    count_block_site_distances=count_block_site_distances, 
    blockcount_avgsite=blockcount_avgsite, 
    pct_of_blocks_are_unique=pct_of_blocks_are_unique, 
    pct_of_blocks_are_in_multiple_buffers
  )
  # print(paste0(sitecount_in, ' input sites'))
  print(paste0(sitecount_out, ' total sites in output results by count of siteid'))
  print(paste0(sitecount_unique_out, ' unique output sites'))
  print(paste0(blockcount_unique , ' unique blocks (residents in only a single buffer, so do not have 2+ sites nearby, ie not in overlap of circular buffers)'))
  print(paste0(blockcount_incl_dupes, " blocks including duplicates in overlaps, in final row count (block-to-site pairs table)" ))
  print(paste0(round(blockcount_avgsite, 0), ' blocks (based on their block internal point, like a centroid) are near the avg site or in avg buffer'))
  print(paste0(100 * round(pct_of_blocks_are_in_multiple_buffers, 3), '% of blocks are duplicates because those residents are near two or more sites (assuming they live at the block internal point'))
  
  return(x)
}
