#' helper- Get summary stats on counts of blocks near various sites
#' @description Tells you # of blocks near avg site, 
#' how many sites have only 1 block nearby, or have <30 nearby, etc.
#' 
#' @param x The output of getblocksnearby()
#' @param varname colname of variable in data.table x that is the one to summarize by
#' @return invisibly, a list of stats
#' @import data.table
#' @seealso [getblocks_diagnostics()]
#' @export
#'
getblocks_summarize_blocks_per_site <- function(x, varname='siteid') {
  blocks_per_site_histo <- table(table(x[ , ..varname]))
  blocks_per_site_histo <- data.frame(
    blocks_nearby =  as.numeric(names(blocks_per_site_histo)), 
    freq_of_sites =  as.numeric(blocks_per_site_histo)
  )
  cat('Range and mean of count of blocks nearby the various sites:\n\n')
  print(summary(as.numeric(table(x[ , ..varname]))))
  cat("\n")
  invisible(blocks_per_site_histo)
}
######################################################################################### # 


#' helper- Get summary stats on how many sites are near various blocks (residents)
#'
#' @param x The output of [getblocksnearby()] like testoutput_getblocksnearby_10pts_1miles
#' @param varname colname of variable in data.table x that is the one to summarize by
#' @return invisibly, a list of stats
#' @export
#' @import data.table
#' @seealso [getblocks_diagnostics()]
#'
getblocks_summarize_sites_per_block <- function(x, varname='blockid') {
  table(table(x[ , ..varname]))
}
######################################################################################### # 



#' Get summary stats on counts of blocks (unique vs doublecounted) near sites
#'
#' @param x The output of [getblocksnearby()] like testoutput_getblocksnearby_10pts_1miles
#' @param detailed if TRUE, also shows in console a long table of frequencies via [getblocks_summarize_blocks_per_site()]
#' @param see_plot set TRUE to draw for each site a boxplot of distances of nearby blocks
#' @return A list of stats 
#' @seealso This relies on  [getblocks_summarize_blocks_per_site()] and [getblocks_summarize_sites_per_block()]
#' @examples  getblocks_diagnostics(testoutput_getblocksnearby_10pts_1miles)
#' @import data.table
#' @export
#'
getblocks_diagnostics <- function(x, detailed=FALSE, see_plot=FALSE) {
  if (NROW(x) == 0) {warning('no blocks found nearby'); return(NA)}
  
  prit <- function(x) {prettyNum(x, big.mark = ',')}
  
  # Distances ####
  
  cat("Summary stats on distances reported from any sites to any nearby blocks\n\n")
  print(cbind(percentiles.of.distance = quantile(x$distance, probs = (0:20)/20)))
  cat("\n")
  cat("Mean distance: ", mean(x$distance, na.rm = TRUE), "\n")
  cat("\n")
  if (detailed) {
  # Counts of blocks nearby, frequency of x nearby ####
  print(getblocks_summarize_blocks_per_site(x))
  # returns table that gives Range and mean of count of blocks nearby the various sites,
  #   how many sites have only 1 block nearby, or <30 nearby, etc.
  cat("\n\n")
  }
  # calculate extra stats ####
  
  sitecount_unique_out       <- data.table::uniqueN(x, by = 'siteid')
  blockcount_unique          <- data.table::uniqueN(x, by = 'blockid') # how many blocks are there, counting each once, not "how many blocks are unique" ie appear only once
  blockcount_incl_dupes      <- data.table::uniqueN(x)
  ratio_blocks_incl_dupes_to_unique <- blockcount_incl_dupes / blockcount_unique
  
  sites_per_block_histo <- getblocks_summarize_sites_per_block(x) #table(table(x$blockid))
  z <- sites_per_block_histo['1']  
  uniqueblocks_near_only1site  <-  ifelse(is.na(z),0,z) 
  z <- sites_per_block_histo['2']  
  uniqueblocks_near_exactly2site <- ifelse(is.na(z),0,z) 
  z <- sites_per_block_histo['3']  
  uniqueblocks_near_exactly3site <- ifelse(is.na(z),0,z) 
  uniqueblocks_near_multisite  <- blockcount_unique - uniqueblocks_near_only1site
  pct_of_unique_blocks_in_overlaps <- uniqueblocks_near_multisite / blockcount_unique
  
  count_block_site_distances <- blockcount_incl_dupes # number of rows in output table of all block-site pairs with their distance.
  blockcount_avgsite         <- blockcount_incl_dupes / sitecount_unique_out
  
  sumstats <- list(
    sitecount_unique_out = sitecount_unique_out, 
    # sites_withany_overlap = as.numeric(getblocks_summarize_sites_per_block(x)['2']),
    # that tells you how many blocks are near 2 sites, but not how many or which sites those were. 
    
       blockcount_avgsite = blockcount_avgsite, 
    
    blockcount_incl_dupes = blockcount_incl_dupes, 
    blockcount_unique = blockcount_unique, 
    
    uniqueblocks_near_only1site = uniqueblocks_near_only1site,
    uniqueblocks_near_exactly2site = uniqueblocks_near_exactly2site,
    uniqueblocks_near_exactly3site = uniqueblocks_near_exactly3site,
    ratio_blocks_incl_dupes_to_unique = ratio_blocks_incl_dupes_to_unique,
    pct_of_unique_blocks_in_overlaps = pct_of_unique_blocks_in_overlaps,
    
    count_block_site_distances = count_block_site_distances,
    uniqueblocks_near_multisite = uniqueblocks_near_multisite
  )
  
  # Print those extra stats to console ####
  
  cat(paste0(prit(sitecount_unique_out), ' unique output sites\n'))
  cat(paste0(prit(round(blockcount_avgsite, 0)), ' blocks are near the avg site or in avg buffer
             (based on their block internal point, like a centroid)\n'))
  # cat(paste0(prit(sites_withany_overlap), ' sites do not overlap with any others\n'))
  cat(paste0(prit(blockcount_incl_dupes), " blocks including doublecounting in overlaps, 
             in final row count (block-to-site pairs table)\n" ))
  cat(paste0(prit(blockcount_unique), " actual unique blocks total\n" ))
  cat(paste0(prit(uniqueblocks_near_only1site) ,    ' blocks (and their residents) have exactly 1 site nearby \n'))
  cat(paste0(prit(uniqueblocks_near_exactly2site) , ' blocks (and their residents) have exactly 2 sites nearby \n'))
  cat(paste0(prit(uniqueblocks_near_exactly3site) , ' blocks (and their residents) have exactly 3 sites nearby \n'))
  cat(paste0(prit(ratio_blocks_incl_dupes_to_unique), ' is ratio of blocks including multicounting / actual count of unique blocks\n'))
  cat(paste0(prit(100 * round(pct_of_unique_blocks_in_overlaps, 3)), 
             '% of unique blocks could get counted more than once 
             because those residents are near two or more sites 
             (assuming they live at the block internal point\n'))
  # cat(prit(count_block_site_distances), ' = count_block_site_distances',  '\n')
  # cat(prit(uniqueblocks_near_multisite),' = uniqueblocks_near_multisite ', '\n')
  
  # PLOT ####
  ## it is not really useful
  # if (see_plot) {
  # boxplot(x$distance ~ x$siteid)
  # }
  invisible(sumstats)
}
######################################################################################### # 
