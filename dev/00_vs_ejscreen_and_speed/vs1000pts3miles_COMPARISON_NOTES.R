######################################################################################### # 

# SCRIPT TO COMPARE 1K FRS SITES EJAM VS EJSCREEN ####
# Functions are ejscreen_vs_ejam() etc.
# validation check 1st try -- 1,000 random points checked in EJScreen vs EJAM 
######################################################################################### # 

library(EJAM)
dataload_from_pins()
indexblocks()


######################################################################################### # 

##   to reload saved work:
#
load( './dev/00_vs_ejscreen_and_speed/vs1000pts3miles.rda')
load( './dev/00_vs_ejscreen_and_speed/sum_vs1000pts3miles.rda')



######################################################################################### # 

##    to do a new analysis: 
 
        # TAKES MAYBE 40 MINUTES *** 

 # pts <- testpoints_n(1000, weighting = 'frs')
 # vs1000pts3miles <- ejscreen_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
 # sum_vs1000pts3miles <- ejscreen_vs_ejam_summary(vs1000pts3miles)
 # 
 # save(vs1000pts3miles, file = './dev/00_vs_ejscreen_and_speed/vs1000pts3miles.rda')
 # save(sum_vs1000pts3miles, file = './dev/00_vs_ejscreen_and_speed/sum_vs1000pts3miles.rda')
 # all.equal(vs1000pts3miles$EJSCREEN[, c('lat','lon')], vs1000pts3miles$EJAM[, c('lat','lon')])
 ######################################################################################### # 
 
 
 z <- vs1000pts3miles # shorter variable name for convenience...  
 # since the earlier saved run did not include these that are in the output of the latest version of the functions:
 # z$diff <- z$EJAM - z$EJSCREEN
 # z$absdiff <- abs(z$diff)
 # z$pctdiff <- z$ratio - 1
 # z$abspctdiff <- abs(z$pctdiff)   
 x <- z # since code below used z not x
 rm(z)
 # save updated version
 # vs1000pts3miles <- x
 # save(vs1000pts3miles, file = './dev/00_vs_ejscreen_and_speed/vs1000pts3miles.rda')
 # sum_vs1000pts3miles <- ejscreen_vs_ejam_summary(vs1000pts3miles)
 # save(sum_vs1000pts3miles, file = './dev/00_vs_ejscreen_and_speed/sum_vs1000pts3miles.rda')
 ######################################################################################### # 
  
 # save just the ones with no diff in blocks counts, as x0
 x0 <- lapply(x, function(z) z[x$diff$blockcount_near_site == 0, ])
 
 
 
 
# Results summary:  ####
 
 ejscreen_vs_ejam_summary(x)
 ejscreen_vs_ejam_summary(x0)
 
# # most key indicators agree with EJScreen to within 1% at 95%+ of all sites for 3 mile radius.
# # no indicator reports the same rounded value on reports at all 1,000 sites
# # indicators that disagree most often or by more than 1% at most sites:  pctpre1960 EJ index percentiles, and these raw demographics: pctover64, Demog.Index.Supp, lowlifex, pctlths, pctunder5, pctunemployed, pctlingiso.

 
 ## BLOCK AND POP COUNTS ####
 
 # # blockcount_near_site  agrees exactly at 45% of sites, and always within max of 16 blocks difference. 
 # max of 50% difference in count. avg difference of 0.2%. 
 # half the time they have a 0 to 1 block difference. Rarely can differ by 5 to even 16 blocks.
 # 96.4% of sites have count estimates agreeing within 1% of each other. 95% of the time they agree within 0.82%
    grep('block', sum_vs1000pts3miles$indicator , value = T)
    t(sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in%  "blockcount_near_site", ])
    # indicator                      "blockcount_near_site"
    # sites.with.data.ejscreen       "1000"                
    # sites.with.data.neither        "0"                   
    # sites.with.data.both           "1000"                
    # sites.agree.rounded            "448"                 
    # sites.agree.within.tol         "964"                 
    # pct.of.sites.agree.rounded     "44.8"                
    # pct.of.sites.agree.within.tol  "96.4"                
    # median.abs.diff                "1"                   
    # max.abs.diff                   "16"                  
    # mean.pct.diff                  "0.2320132"           
    # median.pct.diff                "0.07"                
    # max.pct.diff                   "50"                  
    # within.x.pct.at.p.pct.of.sites "0.82"  
    
    table(x$diff$blockcount_near_site)
 # -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16 
 #  1   1   5   9  20  42 107 448 173  84  42  22  16   8   6   5   5   1   2   1   1   1 
 
 summary(x$EJAM$blockcount_near_site)
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 1.0   160.0   561.5   851.0  1202.2  6008.0 
 
 
 # # Population count is exactly same (rounded to zero decimals) at just 42% of sites (vs 45% having same block counts).
 # 95% of sites differ by <1% on population count. worst was 11% difference in count. 
 # within 12 people half the time.
    t(sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in%  "pop", ])
    # indicator                      "pop"      
    # sites.with.data.ejscreen       "1000"     
    # sites.with.data.neither        "0"        
    # sites.with.data.both           "1000"     
    # sites.agree.rounded            "415"      
    # sites.agree.within.tol         "949"      
    # pct.of.sites.agree.rounded     "41.5"     
    # pct.of.sites.agree.within.tol  "94.9"     
    # median.abs.diff                "11.9"     
    # max.abs.diff                   "4889.2"   
    # mean.pct.diff                  "0.2457056"
    # median.pct.diff                "0.05"     
    # max.pct.diff                   "11.04"    
    # within.x.pct.at.p.pct.of.sites "0.85"
    
  # Where block counts match exactly, (almost half the sites) 
  # only 9 places (of those 448 sites) had discrepancy of > 1% in population counts.
  #   (EJAM must be finding same number but slightly different set of blocks nearby than EJSCREEN does.)
  #   (and worst of those was a 2.6% overestimate but all but 3 were within 2%).
  
    
  ## DEMOGRAPHIC ####
 
 # # names_d do not have very good agreement - especially lowlifex and Demog indexes.
    # sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_d, ]
 # # pctile.lowlifex and  pctile.Demog.Index.Supp  are especially bad among names_d_pctile
 #     sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_d_pctile, ]
 # but if block counts match, 
    ejscreen_vs_ejam_summary(x0)
 ejscreen_vs_ejam_summary(x, myvars = names_d)
 #           indicator pct.of.sites.agree.rounded pct.of.sites.agree.within.tol median.pct.diff max.pct.diff within.x.pct.at.p.pct.of.sites
 # 8         pctunder5                   99.29364                     13.319879            4.21          Inf                          11.99
 # 5     pctunemployed                   98.78910                     11.705348            4.52          Inf                          15.94
 # 4        pctlingiso                   98.48638                      7.971746            8.06          Inf                            Inf
 # 9         pctover64                   98.28456                     34.409687            1.45        14.40                           3.95
 # 6           pctlths                   98.28456                     20.686176            2.49        48.92                          10.90
 # 3         pctlowinc                   96.97276                     59.636731            0.81         8.57                           3.15
 # 10           pctmin                   96.67003                     59.939455            0.73          Inf                           7.30
 # 1       Demog.Index                   86.47831                     57.315843            0.86        11.67                           3.54
 # 2  Demog.Index.Supp                   81.35765                     25.835866            2.12        34.39                           7.35
 # 7          lowlifex                   65.34954                     24.113475            2.05       751.28                          32.09
 # 
 
 ## ENVIRONMENTAL ####
 
 # # among names_e, traffic and rsei and maybe npdes are worst.
 # EJAM/EJS agree within about 5% at 95% of sites for npdes 
 # but others are better - a few agree within 3% at 95% of sites, nata/pm/o3 agree within <2% at 95%.
 #    sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_e, ]
 # # for names_e_pctile, resp and cancer are worst.
 #    sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_e_pctile, ]
 
 # # demog subgroups agree well
 #     sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_d_subgroups, ]
 
 
 
 ## EJ ####
 
 # # EJ percentiles of all 4 types agree on reported pctile at 95-99% of sites except for the pre1960 ones (and state NPDES ones)
 #
 # #  pctile.EJ.DISPARITY.pctpre1960.eo  is worst. pctile.EJ.DISPARITY.proximity.npdes.eo also can be way off sometimes.
 # # But in the few locations where others disagree they can be off by as much as >10 pct points: 
 ejscreen_vs_ejam_summary(x, myvars = c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile))
 ejstats = ejscreen_vs_ejam_summary(x, myvars = c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile))
 ejstats[ , c("indicator", "max.abs.diff")]
 
 ######################################################################################### # 
 
 # Results details: ####
 
 ## to see comparison of one specific site: ####
 mysite  <- 1
 myvars <- names_d
 sapply(x, function(z) z[mysite, ])[myvars, ]
 #                  EJSCREEN EJAM     EJSCREEN_shown EJAM_shown same_shown ratio    
 # Demog.Index      28       27.54839 "28"           "28"       TRUE       0.9838711
 # Demog.Index.Supp 11       11.02795 "11"           "11"       TRUE       1.002541 
 # pctlowinc        20       20.32485 "20"           "20"       TRUE       1.016243 
 # pctlingiso       4        4.207085 "4"            "4"        TRUE       1.051771 
 # pctunemployed    3        2.661679 "3"            "3"        TRUE       0.8872264
 # pctlths          8        8.156037 "8"            "8"        TRUE       1.019505 
 # lowlifex         19       19.7901  "19"           "20"       FALSE      1.041584 
 # pctunder5        10       10.01303 "10"           "10"       TRUE       1.001303 
 # pctover64        8        7.706334 "8"            "8"        TRUE       0.9632917
 # pctmin           35       34.77193 "35"           "35"       TRUE       0.9934837
 
 ######################################################################################### # 
 
  
  # BLOCK COUNTS and POPULATION  ####
  
  table(x$diff$blockcount_near_site)
  # -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16 
  #  1   1   5   9  20  42 107 448 173  84  42  22  16   8   6   5   5   1   2   1   1   1 
  
  # There are between 1 and 6,000 blocks near a given site within 3 miles.
  # Usually there are about 150 to 1500 blocks within 3 miles. 
  quantile(x$EJAM$blockcount_near_site, probs = (0:20)/20)
  # 0%      5%     10%     15%     20%     25%     30%     35%     40%     45%     50%     55%     60%     65%     70%     75%     80%     85%     90%     95%    100% 
  # 1.00   37.00   62.90   92.00  120.00  160.00  217.00  291.00  365.00  465.55  561.50  665.45  777.40  891.35 1035.90 1202.25 1407.20 1678.90 2083.90 2829.20 6008.00 
  
  barplot(table(x$diff$blockcount_near_site), main = 'Discrepancies in number of blocks identified as within 3 miles (EJAM - EJSCREEN)',
          ylab = "Number of sites with given disagreement (out of 1,000 FRS sites)", 
          xlab = "Excess count of blocks identified by EJAM vs EJSCREEN")
  
  #  97% of the time the count is within 1% of EJSCREEN's count. 
  # Only 3.2% of sites had error of >1% in population estimate.
  
  #  99% of the time the count is within 3% of EJSCreen's count. 
  # <1% (or just 9 out of 1,000 sites) had diff > 3% in pop count.
  
  #  Worst case among 1k sites was overestimate of 11%
  
  # ***  But if block counts match, only 9 places had diff > 1% in pop count.
  sum(abs(x0$ratio$pop - 1) > 0.01, na.rm = T)
  # [1] 9
  
  cbind(blockcount_discrepancies = quantile(abs(x$diff$blockcount_near_site), probs = (0:20)/20))
  # blockcount_discrepancies
  # 0%                          0
  # 5%                          0
  # 10%                         0
  # 15%                         0
  # 20%                         0
  # 25%                         0
  # 30%                         0
  # 35%                         0
  # 40%                         0 at least 40% of sites had exact match for block count ejam v ejscreen
  # 45%                         1
  # 50%                         1
  # 55%                         1
  # 60%                         1
  # 65%                         1
  # 70%                         1 another 30% or so were off by just 1 block 
  # 75%                         2
  # 80%                         2
  # 85%                         2
  # 90%                         3 almost 90% of sites were off by 2 or fewer blocks
  # 95%                         5
  # 100%                       16 worst site was off by 16 blocks (ejam found 16 more than ejscreen)
  
     ejscreen_vs_ejam_summary_quantiles(x, 'ratio', myvars = 'pop')
     #        0%     5%    10%   15%    20%    25% 30% 35% 40% 45% 50%    55%    60%    65%    70%   75%    80%    85%    90%    95%   100%
     # pop 0.931 0.9969 0.9984 0.999 0.9997 0.9999   1   1   1   1   1 1.0001 1.0001 1.0004 1.0006 1.001 1.0015 1.0022 1.0036 1.0064 1.1104
     
     cbind(ratio_pop_counts = round(quantile(abs(x$ratio$pop), probs = (0:20)/20, type = 1, na.rm = TRUE), 6))
  #      ratio_pop_counts (EJAM/EJSCREEN)
  # 0%           0.930995  within 7% (EJAM is too low by 7%). only 10 places/1k (1% of places) are low by > 1%
  # 5%           0.996879  within 0.3 %
  # 10%          0.998401
  # 15%          0.999031
  # 20%          0.999692
  # 25%          0.999871
  # 30%          0.999961
  # 35%          0.999990
  # 40%          1.000000
  # 45%          1.000004
  # 50%          1.000017
  # 55%          1.000054
  # 60%          1.000149
  # 65%          1.000358
  # 70%          1.000641
  # 75%          1.000990
  # 80%          1.001513
  # 85%          1.002213
  # 90%          1.003588
  # 95%          1.006425
  # 100%         1.110405 
     # >      ejscreen_vs_ejam_summary_quantiles(x, 'diff', myvars = 'pop', digits = 0)
     # 0%   5% 10% 15% 20% 25% 30% 35% 40% 45% 50% 55% 60% 65% 70% 75% 80% 85% 90% 95% 100%
     # pop -1750 -184 -79 -37   0   0   0   0   0   0   0   0   0   3  19  44  76 119 220 453 4889
    
  # > sum(abs(x$EJAM$pop / x$EJSCREEN$pop)[x$EJSCREEN$pop != 0] - 1 > 0.01)
  # [1] 32
  # > sum(abs(x$EJAM$pop / x$EJSCREEN$pop)[x$EJSCREEN$pop != 0] - 1 > 0.03)
  # [1] 9
  # > sum(abs(x$EJAM$pop / x$EJSCREEN$pop)[x$EJSCREEN$pop != 0] - 1 > 0.05)
  # [1] 5
  # > sum(abs(x$EJAM$pop / x$EJSCREEN$pop)[x$EJSCREEN$pop != 0] - 1 > 0.07)
  # [1] 5
  # > sum(abs(x$EJAM$pop / x$EJSCREEN$pop)[x$EJSCREEN$pop != 0] - 1 > 0.08)
  # [1] 3
  # > sum(abs(x$EJAM$pop / x$EJSCREEN$pop)[x$EJSCREEN$pop != 0] - 1 > 0.11)
  # [1] 1
  # 
     # see that large percentage diff in pop is not due to large discrepancy in abs count of blocks 
  table(round(x$ratio$pop, 2), x$diff$blockcount_near_site)
  #       -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
  
  # 0.93   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  # 0.94   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  # 0.97   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  
  # 0.98   0   0   0   0   0   2   0   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  # 0.99   0   1   0   1   0   6   5   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  #  1      1   0   5   8  20  34  97 422 149  72  36  18  12   7   5   4   3   1   2   1   0   1
  # 1.01   0   0   0   0   0   0   1   5  14   9   6   3   3   1   1   1   2   0   0   0   1   0
  # 1.02   0   0   0   0   0   0   0   3   2   0   0   1   1   0   0   0   0   0   0   0   0   0
  
  # 1.03   0   0   0   0   0   0   0   1   2   1   0   0   0   0   0   0   0   0   0   0   0   0 pop count rarely off by >2%
  # 1.04   0   0   0   0   0   0   0   0   3   0   0   0   0   0   0   0   0   0   0   0   0   0
  # 1.08   0   0   0   0   0   0   0   0   1   1   0   0   0   0   0   0   0   0   0   0   0   0 
  # 1.09   0   0   0   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0
  # 1.11   0   0   0   0   0   0   0   0   2   0   0   0   0   0   0   0   0   0   0   0   0   0

  
  plot(ecdf(x$diff$blockcount_near_site))
  plot(ecdf(abs(x$diff$blockcount_near_site)), 
       main = "EJAM and EJSCREEN differ by up to 16 blocks seen within 3 miles, but rarely off by even 5 blocks", 
       xlab = "number of blocks difference between estimates from 2 tools", ylab = "cumulative share of the 1,000 FRS sites tested")
  
  hist(x$ratio$pop, 1000, main = 'Discrepancies in population count of residents identified within 3 miles (EJAM - EJSCREEN)', ylab = "Number of sites with given disagreement (out of 1,000 FRS sites)", xlab = "Excess count of residents identified by EJAM vs EJSCREEN as a ratio" )
  hist(x0$ratio$pop, 1000, main = 'Discrepancies in population count of residents identified within 3 miles (EJAM - EJSCREEN)
       at sites where block counts match', ylab = "Number of sites with given disagreement (out of 1,000 FRS sites)", xlab = "Excess count of residents identified by EJAM vs EJSCREEN as a ratio" )
  quantile(abs(x$diff$blockcount_near_site), probs = (0:20)/20)
  table(abs(x$diff$blockcount_near_site))
  
  hist(x$ratio$pop, 1000, main = 'Discrepancies in population count of residents identified within 3 miles (EJAM - EJSCREEN)', ylab = "Number of sites with given disagreement (out of 1,000 FRS sites)", xlab = "Excess count of residents identified by EJAM vs EJSCREEN as a ratio",
       xlim = c(0.95, 1.05))
  
 ########################################################################################### # 
  
  # CLOSELY EXAMINE ONE SITE TO SEE ITS BLOCKS AND WHY EJAM MIGHT DIFFER FROM EJSCREEN ####
  
look <- ejscreen_vs_ejam_see1map # alias for that function
  
  # Which sites have large discrepancy in pop estimate, due to block counts too high?
  
  # ALL 13 SITES WHERE EJAM IS more than 1% HIGHER POP COUNT THAN EJSCREEN
  #  & EJAM counted 2+ blocks as inside more than did EJscreen
  #  ARE PERFECTLY EXPLAINED BY THE EXTRA BLOCKS EJAM CONSIDERED INSIDE THE RADIUS OF 3 MILES.
  #  USUALLY DISTANCES OF THOSE ARE >2.994270 miles 
  
  
  which(x$ratio$pop > 1.05 & x$diff$blockcount_near_site > 1)
  #  654 940
  
  # first site to look at
look(654)
  # two extra blocks and 936.5 people more are found by EJAM than by EJSCREEN here
  # Looks like EJAM must include the 2 blocks that are 0 pop and 936.48 pop, from the furthest 3 blocks.
  # That explains the discrepancy perfectly. 
#    blockid distance   blockpop cumpop
# 7: 6401777 2.983148 221.427053 1179.7
# 8: 6401715 2.994270 936.476043  958.3
# 9: 6397090 2.994647  21.842799   21.8
# 10: 6397693 2.997031   0.000000    0.0
#                      EJSCREEN EJAM    diff 
# pop                  12098    13034.5 936.5
# blockcount_near_site 565      567     2 
 

  # other site to look into
  n = 940
  look(940)
  # discrepancy is 2 blocks, 212.4 pop count, explained by the 2 furthest blocks:
  # > sum(tail(px[order(distance), .(blockid, distance, blockpop)], 2)$blockpop)
  # [1] 212.55
  
  # which(x$ratio$pop > 1.01 & x$diff$blockcount_near_site == 1)
  # [1]  36  60  67 121 225 325 400 478 631 635 766 911 921
  which(x$ratio$pop > 1.04 & x$diff$blockcount_near_site > 1)
  # 204
  n = 204
 look(204)
  # 442.7 people in 2 blocks is discrepancy
  sum(tail(px[order(distance), .(blockid, distance, blockpop)], 2)$blockpop)
  # 442.9 people in 2 blocks that are furthest away !
  
  
  which(x$ratio$pop > 1.02 & x$diff$blockcount_near_site > 1)
  look(905)
  # furthest 5 blocks explain the difference. they are 2.996572 miles away or more.

  which(x$ratio$pop > 1.01 & x$diff$blockcount_near_site > 1)
  # 204  337  341  417  558  654  734  859  905  940  964  990 1000
  
  look(337)
  # 7: 7651304 2.994741  11.99436  112.8
  # 8: 7651102 2.996073  34.87457  100.8
  # 9: 7651293 2.997108  65.96896   66.0
  # 10: 7651051 2.997833   0.00000    0.0
  # EJSCREEN EJAM diff
  # pop                  6313     6426 113  # perfect explanation, distances >= 2.99474 miles
  # blockcount_near_site 189      193  4 
  
  look(341)
  # 5: 2514564 2.995724  26.15415 1126.0
  # 6: 2514748 2.995777  21.63347 1099.9
  # 7: 2514273 2.997149 556.67751 1078.2
  # 8: 2516677 2.997671   0.00000  521.6
  # 9: 2514437 2.998110 448.20836  521.6
  # 10: 2514760 2.999938  73.35784   73.4
  # EJSCREEN EJAM    diff 
  # pop                  54418    54987.7 569.7
  # blockcount_near_site 842      846     4    
  # 521.6 + 21.63347 + 26.15415 # explained by blocks 5,6,9,10 on that list. d >= 2.995724 
  # 569.3876
  
  look(417) # sites 7 and 8 of the furthest 10  perfectly explain the diff.
  look(558) # furthest 8 blocks perfectly explain the discrepancy
  look(734) # furthest 5 blocks perfectly explain the diff
  look(859) # furthest 2 blocks perfectly explain the diff
  look(964) # furthest numbers 7,9,10  perfectly explain the diff
  look(990) # furthest 7 9 10  perfectly explain the diff
  look(1000)  # furthest 2 blocks perfectly explain the diff
  
  # stranger cases:
  which(x$ratio$pop > 1.02 & x$diff$blockcount_near_site < 1)
  # [1] 408 460 984
look(408) # has some blocks on edge of 3 miles that are 80-100 pop each, 
# so EJAM must mistakenly include 1 of those but miss one that EJSCREEN included of around 70 people or so.


