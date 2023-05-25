##################################################### #
### SIGNIFICANT PROBLEMS WITH THE   state.pctile.proximity.npdes 


### DEMOG pctiles look good generally but note these:

# place 19 the raw demog indexes differ a lot.

# see lingiso in places 33  the raw scores are 1 vs 0 so the pctile is way off- 0 vs 70 !!
# pctunemployed raw in place 33 also differ a lot, so the pctiles also. 5 vs 9 raw, 22 points diff in pctile.
# pctunder 5 raw place 33 also differ a lot, so pctile way off. by 14

# t(out[33, ])


# see pctlths in place 29, 6 vs 5.2% raw is big diff in pctile.
# place 29 under5 is 2.9 vs 3% raw so pctile off by 5


##################################################### #


library(EJAM)
library(EJAMejscreenapi)
library(data.table)

# out_check <- ejscreenit(testpoints_50, radius = 1, save_map = F, save_plot = F, save_table = F)
# out2 <- out_check$table
# testoutput_ejscreenapi_plus_50 <- out2
# usethis::use_data(testoutput_ejscreenapi_plus_50)

out <- EJAMejscreenapi::testoutput_ejscreenapi_plus_50

vnames <- names_d; multiplier <- 0.01 #  STATE PERCENTILES, DEMOGRAPHIC (WITH 1/100 ADJUSTED)
# or   vnames <- names_e; multiplier <- 1 #  STATE PERCENTILES, envt 

vnames <- vnames[vnames %in% names(out)]

######################################################## #

x <- ejamit(testoutput_ejscreenapi_plus_50, 1)

cat("\n\nCOMPARE RAW AND STATE PERCENTILES FROM API VS EJAM code that assigns pctiles based on raw values\n\n")

for (i in 1:length(vnames)) {
  
  MYVAR = vnames[i]
  print(MYVAR)
  
  xapi <- out[ , paste0("state.pctile.", MYVAR )]
  xejam <- pctile_from_raw_lookup(myvector = 
                                    multiplier * 
                                    out[ , MYVAR], 
                                  varname.in.lookup.table = MYVAR, 
                                  lookup = statestats, 
                                  zone = out$ST)
  ejam.state.pctile <- unlist(x$results_bysite[, paste0("state.pctile.", MYVAR), with=FALSE])
    print(
    cbind(
      ejscreen.raw = out[ , MYVAR],
      ejam.raw = round(unlist(x$results_bysite[, ..MYVAR]),3),
   ejam.lookup.of.apiraw = xejam,     
   ejam.state.pctile = ejam.state.pctile,
      ejscreen.state.pctile = xapi, 
    
      diff = ejam.state.pctile - xapi
    )
  )
  
}

##################################################### #
# COMPARE RAW CALCULATION AS STATE PCTILE 
# FOR API VS EJAM

# out <- EJAMejscreenapi::testoutput_ejscreenapi_plus_50


##############

vnames <- names_d; multiplier <- 0.01 #  STATE PERCENTILES, DEMOGRAPHIC (WITH 1/100 ADJUSTED)

vnames <- vnames[vnames %in% names(out)]

  as.data.frame(x$results_bysite[ ,  paste0("state.pctile.", vnames) , with = FALSE])   - out[ ,  paste0("state.pctile.", vnames)]
# see place 33... a few are way off in state.pctile.  demog
# 
#   state.pctile.Demog.Index state.pctile.pctlowinc state.pctile.pctlingiso state.pctile.pctunemployed state.pctile.pctlths state.pctile.pctunder5 state.pctile.pctover64 state.pctile.pctmin
#   1                        -1                      0                       0                          0                    0                      0                      0                   0
#   2                         0                     -1                       1                          1                    1                      2                     -1                   1
#   3                         0                      0                       0                          1                    0                      0                      0                   0
#   4                        NA                     NA                      NA                         NA                   NA                     NA                     NA                  NA
#   5                        NA                     NA                      NA                         NA                   NA                     NA                     NA                  NA
#   6                         0                      0                       0                          0                    0                      0                      0                   0
#   7                         0                      0                       0                          0                    0                      0                      0                   0
#   8                        -1                      0                       0                          0                    0                      0                      0                   0
#   9                         0                      0                       0                          0                    0                      0                      0                   0
#   10                        0                      0                       0                          0                    0                      0                      0                   0
#   11                        0                     -1                       0                          0                    1                     -1                      1                   1
#   12                       -1                      0                       0                          0                    0                     -1                      0                   0
#   13                        0                      0                       0                          0                    0                      0                      0                   0
#   14                       -1                      0                       1                          0                    0                      0                      0                   0
#   15                        0                      0                       0                          0                    0                      0                      0                   0
#   16                        0                      0                       0                          0                    0                      0                      0                   0
#   17                        0                      0                       0                          0                    0                      0                      0                   0
#   18                        0                      0                      -1                          0                    0                      0                      0                   0
#   19                       10                      0                       0                          0                    0                      0                      0                   0
#   20                        0                      0                       0                          0                    0                      0                      0                   0
#   21                        0                      0                       0                         -1                    0                      0                      0                   0
#   22                        0                      0                       0                          0                    0                      0                      0                   0
#   23                        0                      0                       0                          0                    0                      0                      0                   0
#   24                        0                      0                       0                          0                    0                      0                      0                   0
#   25                        0                      0                       0                          0                    0                      0                      0                   0
#   26                       -1                      0                       0                          0                    0                      0                      0                   0
#   27                        0                      0                       0                          0                    0                      0                      0                   0
#   28                        0                      0                       1                          1                    0                      1                      0                   0
#   29                       -2                     -1                      -3                          3                   -4                     -5                     -2                  -1
#   30                       -1                      0                       0                          0                    0                      0                      0                   0
#   31                        0                      0                       0                          0                    0                      0                      0                   0
#   32                        0                      0                       0                          0                    0                      0                      0                   0
#   33                       -1                     -1                     -70                         22                   -3                     14                      6                   1
#   34                        0                      0                       0                          0                    0                     -1                      0                   0
#   35                        0                      0                       0                          0                    0                      0                      0                   0
#   36                        0                      0                       0                          0                    0                      0                      0                   0
#   37                        0                      0                       0                          0                    0                      0                      0                   0
#   38                        0                      0                       0                          0                    0                      0                      0                   0
#   39                        0                      0                       0                          0                    0                      0                      0                   0
#   40                        0                      0                       0                          0                    0                      0                      0                   0
#   41                        0                      0                       0                          0                    0                      0                      0                   0
#   42                        0                      0                       0                          0                    0                      0                      0                   0
#   43                        0                      1                      -1                          0                    0                      0                      0                   1
#   44                        2                      0                       0                          0                    0                      0                      0                   0
#   45                        0                      0                       0                          0                    0                      0                      0                   0
#   46                        0                      0                       0                          0                    0                      0                      0                   0
#   47                        0                      0                       0                         -1                    0                      0                      0                   0
#   48                       -1                      0                       0                          0                    0                      0                      0                   0
#   49                        0                      0                       0                          0                    0                      0                      0                   0
#   50                        0                      0                       0                          0                    0                      1                     -1                   1

################################
  
vnames <- names_e; multiplier <- 1 #  STATE PERCENTILES, envt 

vnames <- vnames[vnames %in% names(out)]
 as.data.frame(x$results_bysite[ ,  paste0("state.pctile.", vnames) , with = FALSE])   - out[ ,  paste0("state.pctile.", vnames)]

 statestats[statestats$REGION=="IA", 'cancer']  # (20 is  threshold in IA for all pctiles up to 90th or so ????)  same in 1/3 of FL.
 statestats[statestats$REGION=="GA", 'pctpre1960']  # about 30% of state has score of zero.
  
 ### SIGNIFICANT PROBLEMS WITH THE   state.pctile.proximity.npdes 
 
 # BUT OTHER SCORES ALL LOOK VERY GOOD. OFTEN OFF BY 0, SOMETIMES BY 1. 
  
#  t(out[22, ])

