

# if you wanted to count nearby for 1 site at a time in a loop...(why?)
# 
# getblockcountsnearby <- function(n=10, radius = 3) {
#   num <- vector()
#   cat("\n")
#   for (i in 1:n) {
#     cat("\nTrial", i, "- ")
#     check <- function() { suppressWarnings( {length(unique(getblocksnearby(testpoints_n(1), radius = 100, quiet = T)$blockid))})}
#     num[i] <- check()
#     cat('                                                                  ', 
#         prettyNum( num[i], big.mark = ","), 'blocks nearby')
#   }
#   cat('\n')
#   
#   print(summary(num))
#   
#   hist(num, 
#        main = paste0("How many blocks are within", radius, "miles of random facility?"),
#        xlab = "# of nearby blocks", ylab = "how often (how many random facilities)")
#   
#   return(num)
# }
# ###########
# 
# 
# radius = 100; num100miles <- getblockcountsnearby(1000, radius); save(num100miles, file = "num of blocks within 100 miles checked for 1k sites.rda")
# # radius = 10 ; num10miles  <- getblockcountsnearby(1000, radius)
# # radius = 3.1; num3.1miles <- getblockcountsnearby(1000, radius)
# # radius = 1.0; num1miles  <-  getblockcountsnearby(1000, radius)


hist1 <- function(vector_of_blockcounts, radius, hn = 100) {
  avgcount <- mean(vector_of_blockcounts, na.rm = T)
  hist(vector_of_blockcounts, hn,
       main = paste0("Average facility has ", prettyNum(round(avgcount, 0), big.mark = ",")," blocks within ", radius, " miles"),
       xlab = paste0("# of blocks within ", radius, " miles"), ylab = "how often (how many random facilities)")
  abline(v = avgcount, col = "blue")
}

################################################ #

## cant we do that much faster like this?

n <- 1000 # trials, sites
radius <- 3.1

blockswithin <- function(radius = 3.1, n = 1000) {
  suppressWarnings( {y <-  getblocksnearby(testpoints_n(n), radius = radius, quiet = T)})
  countable <- y[ , .(blockcount = .N), keyby = "ejam_uniq_id"]
  hist1(countable$blockcount, radius)
  cat("Number of blocks near typical site:\n")
  print(summary(countable$blockcount))
  cat("Total rows in s2b, number of block-site pairs stored: ", prettyNum(NROW(y), big.mark = ","), '\n')
  invisible(countable[order(blockcount), ]$blockcount)
}

# for 1,000 facilities (sites), at various radius values

b1 = blockswithin(1)
b3.1 = blockswithin(3.1)
b6.2 = blockswithin(6.2)
b10 = blockswithin(10)
b15 = blockswithin(15)
b31 = blockswithin(31)

b50 = blockswithin(50)

# actual rows (blocks) counted near each of 1,000 facilities (sites), at various radius values

scalinginfo = data.frame(miles = c(1, 3.1, 6.2, 10, 15, 31, 50), 
                         avgblocks = c(128, 899, 2952, 6999, 12605, 33329, 65491), 
                         rows_s2b = c(126716,  896736,  2952312, 6992019, 12592435, 33329144, 65491375))
scalinginfo$millions.of.rows = scalinginfo$rows_s2b / 1e6
scalinginfo$rows_per_site = scalinginfo$rows_s2b / 1000

# actual rows (blocks) per facility (site)

plot( rows_per_site ~ miles, type = "b", data = scalinginfo, main = "Size of table storing sites2blocks, output of getblocksnearby()") 

# actual rows (blocks) per 1k facilities (sites), on avg

# fairly good fit to predict rows needed for data.table sites2blocks of 1,000 sites and 1:50 miles
plot( avgblocks ~ miles, type = "b", data = scalinginfo)
points(1:50,  -0.15 * (1:50)^3 + ((1:50)^1.54) * 200, col = "blue") 

# actual millions of rows (blocks), per 1k facilities (sites) 
plot( millions.of.rows ~ miles, type = "b", data = scalinginfo, main = "Size of table storing sites2blocks, output of getblocksnearby()") 
points(1:50,  -0.00015 * (1:50)^3 +  ((1:50)^1.54) * 0.2, col = "red") 


# PREDICTED rows (blocks) per N facilities, at radius of 1 to 50 miles

plot(1:50, 
     getblocks_predict_blocks_per_site(1000, 1:50)
)






# Analyzing 100 points, radius of 1 miles around each.
# 1,283,193 blocks
# > getblocks_summarize_blocks_per_site(y)
# Range and mean of count of blocks nearby the various sites:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    26.0    86.0   129.3   184.0  1278.0 

############################################## # 





