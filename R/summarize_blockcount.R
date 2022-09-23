#' Get summary stats on counts of blocks near various sites
#' @description Tells you # of blocks near avg site, 
#' how many sites have only 1 block nearby, or have <30 nearby, etc.
#' 
#' @param x The output of getblocksnearby()
#'
#' @return invisibly, a list of stats
#' @import data.table
#' @export
#'
summarize_blocks_per_site <- function(x, varname='siteid') {
  blocks_per_site_histo <- table(table(x[ , ..varname]))
  blocks_per_site_histo <- data.frame(
    blocks_nearby =  as.numeric(names(blocks_per_site_histo)), 
    freq_of_sites =  as.numeric(blocks_per_site_histo)
  )
  print('Range and mean of count of blocks nearby the various sites')
  print(summary(as.numeric(table(x[ , ..varname]))))
  invisible(blocks_per_site_histo)
}

#' Get summary stats on how many sites are near various blocks (residents)
#'
#' @param x The output of getblocksnearby()
#'
#' @return invisibly, a list of stats
#' @import data.table
#' @export
#'
summarize_sites_per_block <- function(x, varname='blockid') {
  table(table(x[ , ..varname]))
}

#' Get summary stats on counts of blocks (unique vs doublecounted) near sites
#'
#' @param x The output of getblocksnearby()
#'
#' @return A list of stats
#' @import data.table
#' @export
#'
summarize_blockcount <- function(x) {
  
  prit <- function(x) {prettyNum(x, big.mark = ',')}  
  
  sitecount_unique_out       <- data.table::uniqueN(x, by = 'siteid')
  blockcount_unique          <- data.table::uniqueN(x, by = 'blockid') # how many blocks are there, counting each once, not "how many blocks are unique" ie appear only once
  blockcount_incl_dupes      <- data.table::uniqueN(x)
  ratio_blocks_incl_dupes_to_unique <- blockcount_incl_dupes / blockcount_unique
  
  sites_per_block_histo <- summarize_sites_per_block(x) #table(table(x$blockid))
  z <- sites_per_block_histo['1']  
  uniqueblocks_near_only1site  <-  ifelse(is.na(z),0,z) 
  z <- sites_per_block_histo['2']  
  uniqueblocks_near_exactly2site <- ifelse(is.na(z),0,z) 
  z <- sites_per_block_histo['3']  
  uniqueblocks_near_exactly3site <- ifelse(is.na(z),0,z) 
  uniqueblocks_near_multisite  <- blockcount_unique - uniqueblocks_near_only1site
  pct_of_unique_blocks_in_overlaps <- uniqueblocks_near_multisite / blockcount_unique
  
  sites_withany_overlap   <- # print(as.numeric(summarize_sites_per_block(x)['2'])) 
    # that tells you how many blocks are near 2 sites, but not how many or which sites those were. 
    
    # summarize_blocks_per_site(x) # tells you # of blocks near avg site, how many sites have only 1 block nearby, or <30 nearby, etc.
  
  count_block_site_distances <- blockcount_incl_dupes # number of rows in output table of all block-site pairs with their distance.
  blockcount_avgsite         <- blockcount_incl_dupes / sitecount_unique_out
  

  
  x <- list(
    sitecount_unique_out = sitecount_unique_out, 
    # sites_withany_overlap = sites_withany_overlap,
    blockcount_avgsite = blockcount_avgsite, 
    blockcount_incl_dupes = blockcount_incl_dupes, 
    blockcount_unique = blockcount_unique, 
    ratio_blocks_incl_dupes_to_unique = ratio_blocks_incl_dupes_to_unique,
    uniqueblocks_near_only1site = uniqueblocks_near_only1site,
    uniqueblocks_near_multisite = uniqueblocks_near_multisite,
    pct_of_unique_blocks_in_overlaps = pct_of_unique_blocks_in_overlaps,
    uniqueblocks_near_exactly2site = uniqueblocks_near_exactly2site,
    uniqueblocks_near_exactly3site = uniqueblocks_near_exactly3site,
    count_block_site_distances = count_block_site_distances
  )
  
  cat(paste0(prit(sitecount_unique_out), ' unique output sites\n'))
  cat(paste0(prit(round(blockcount_avgsite, 0)), ' blocks are near the avg site or in avg buffer
             (based on their block internal point, like a centroid)\n'))
  # cat(paste0(prit(sites_withany_overlap), ' sites do not overlap with any others\n'))
  cat(paste0(prit(blockcount_incl_dupes), " blocks including doublecounting in overlaps, 
             in final row count (block-to-site pairs table)\n" ))
  cat(paste0(prit(blockcount_unique), " actual unique blocks total\n" ))
  cat(paste0(prit(uniqueblocks_near_only1site) , ' blocks /residents are in only a single buffer,
             so do not have 2+ sites nearby,
             ie not in overlap of circular buffers)\n'))
  cat(paste0(prit(uniqueblocks_near_exactly2site) , ' blocks are in exactly 2 buffers,
             ie have exactly 2 sites nearby\n'))
  cat(paste0(prit(uniqueblocks_near_exactly3site) , ' blocks are in exactly 3 buffers,
             ie have exactly 3 sites nearby\n'))
  cat(paste0(prit(ratio_blocks_incl_dupes_to_unique), ' is ratio of blocks including multicounting / actual count of unique blocks\n'))
  cat(paste0(prit(100 * round(pct_of_unique_blocks_in_overlaps, 3)), 
             '% of unique blocks could get counted more than once 
             because those residents are near two or more sites 
             (assuming they live at the block internal point\n'))
  
  invisible(x)
}
