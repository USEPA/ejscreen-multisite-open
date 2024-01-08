
# validation check 1st try -- 1,000 random points checked in EJScreen vs EJAM
library(EJAM)
dataload_from_pins()

 pts <- testpoints_n(1000, weighting = 'frs')
 vs1000pts3miles <- ejscreenapi_vs_ejam(pts, radius = 3, include_ejindexes = TRUE)
 sum_vs1000pts3miles <- ejscreenapi_vs_ejam_summary(vs1000pts3miles)
  # save(sum_vs1000pts3miles, file = './dev/00_vs_ejscreen_and_speed/sum_vs1000pts3miles.rda')
  # save(vs1000pts3miles, file = './dev/00_vs_ejscreen_and_speed/vs1000pts3miles.rda')
 # all.equal(vs1000pts3miles$EJSCREEN[, c('lat','lon')], vs1000pts3miles$EJAM[, c('lat','lon')])
 ## to see comparison of one specific site:
  mysite  <- 1
  myvars <- names_d
  sapply(vs1000pts3miles, function(z) z[mysite, ])[myvars, ]
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
 
 
# Results: 
 
# # most key indicators agree with EJScreen to within 1% at 95%+ of all sites for 3 mile radius.
# # no indicator reports the same rounded value on reports at all 1,000 sites
# # indicators that disagree most often or by more than 1% at most sites:  pctpre1960 EJ index percentiles, and these raw demographics: pctover64, Demog.Index.Supp, lowlifex, pctlths, pctunder5, pctunemployed, pctlingiso.

 # # blockcount_near_site  agrees exactly at 45% of sites, and always within max of 16 blocks difference. 
 # max of 50% difference in count. avg difference of 0.2%. 
 # half the time they have a 0 to 1 block difference. 
 # 96.4% of sites have count estimates agreeing within 1% of each other. 95% of the time they agree within 0.82%
 #    grep('block', sum_vs1000pts3miles$indicator , value = T)
 #    sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in%  "blockcount_near_site", ]
 #    summary(vs1000pts3miles$EJAM$blockcount_near_site)
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 1.0   160.0   561.5   851.0  1202.2  6008.0 
 
 
 # # population count is exactly same (rounded to zero decimals) at only 42% of sites. 
 # 95% of sites differ by <1% on population count. worst was 11% difference in count. 
 # within 12 people half the time.
 #    sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in%  "pop", ]
 
 
 # # names_d do not have very good agreement - especially lowlifex and Demog indexes.
 #    sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_d, ]

 # # pctile.lowlifex and  pctile.Demog.Index.Supp  are especially bad among names_d_pctile
 #     sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_d_pctile, ]
 

 # # among names_e, traffic and rsei and maybe npdes are worst.
 # EJAM/EJS agree within about 5% at 95% of sites for npdes 
 # but others are better - a few agree within 3% at 95% of sites, nata/pm/o3 agree within <2% at 95%.
 #    sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_e, ]
 # # for names_e_pctile, resp and cancer are worst.
 #    sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_e_pctile, ]


 # # for names_ej_pctile, pctile.EJ.DISPARITY.pctpre1960.eo  is worst, but most never off by more than 1 pct point.  But pctile.EJ.DISPARITY.proximity.npdes.eo also can be way off sometimes.
 # # for names_ej_state_pctile, they agree on reported for 95-97% of sites except pre1960 one;
 # # but when they disagree most of the 13 can be off by as much as >10 pct points!
 #     sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_ej_pctile, ]
 #     sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_ej_state_pctile, ]



 # # demog subgroups agree well
 #     sum_vs1000pts3miles[sum_vs1000pts3miles$indicator %in% names_d_subgroups, ]


