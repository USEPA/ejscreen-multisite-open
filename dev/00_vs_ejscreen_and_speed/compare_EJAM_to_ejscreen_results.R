


# this file is old.  see newer code now, in ejscreenapi_vs_ejam() and see script at
#  "EJAM/dev/00_vs_ejscreen_and_speed/vs1000pts3miles_COMPARISON_NOTES.R"





############################### ############################### ###############################
# compare EJAM and ejscreenapi results for the variables that both have, for each site


library(data.table)
library(EJAM)
library(EJAMejscreenapi)


# ONE EFFORT TO QUICKLY LOOK AT A FEW VARIABLES ###############################

#   (testoutput_ejscreenit_10pts_1miles$table)
#   should be very similar to
#  testoutput_ejamit_10pts_1miles$results_bysite

### look at some key indicators from each tool to see some differences: 
tejam <- as.data.frame(testoutput_ejamit_10pts_1miles$results_bysite)
tapi  <- as.data.frame(testoutput_ejscreenit_10pts_1miles$table)
pctcols <- (1 == fixcolnames(names(tejam), 'r', 'percentage'))
tejam[ , pctcols] <- tejam[ , pctcols] * 100 # so all are 0-100 ?


round(tejam[ , c("siteid", "pop", "lat", "lon", names_d)], 0)
round(cbind(tapi[  , c("id",     "pop", "lat", "lon", names_d)]), 0 )

cbind(
  EJAM = round(tejam$pop, 0), 
  API = tapi$pop, 
  delta =  round(tejam$pop, 0) - tapi$pop, 
  pctdiff =  round(100 * ((tejam$pop) - tapi$pop) / tapi$pop, 0)
)

cbind(
  EJAM = round(tejam$Demog.Index, 0), 
  API = tapi$Demog.Index, 
  delta =  round(tejam$Demog.Index, 0) - tapi$Demog.Index, 
  pctdiff =  round(100 * ((tejam$Demog.Index) - tapi$Demog.Index) / tapi$Demog.Index, 0)
)








# SEPARATE WORK ###############################

x1full = ejscreenit(testpoints_100, radius = 3, save_map = F, save_plot = F, save_table = F)

x2full = ejamit(testpoints_100, radius = 3)


# names(x1full)
# names(x2full)
x1 <- x1full$table
x2 <- x2full$results_bysite
x2 <- setDF(copy(x2))
names(x2) <- gsub("", "", names(x2))

# AS OF NOW, AT LEAST... 

cat("In EJScreen API results but not in EJAM:\n")
setdiff(names(x1), names(x2))
cat("\n")
cat("In EJAM results but not in EJScreen API:\n")
setdiff_yx(names(x1), names(x2))
cat("\n\n\n\n")

both = intersect(names(x1), names(x2))
class(x1); class(x2)
x1 <- x1[, both]
x2 <- x2[, both]
tex <- sapply(x1, function(z) !is.numeric(z))
# names(x1)[tex]
# names(x2)[(sapply(x2, function(z) !is.numeric(z)))]
# [1] "ST"
x1 <- x1[,!tex]
x2 <- x2[,!tex]
# > all.equal( names(x1), names(x2))
# [1] TRUE
ratios <- x1 / x2
avgratio = colMeans(ratios)


############################################### # 
# lowlifex and Demog.Index.Supp WERE not yet in ejscreen API: ***************

nd <- names_d[!(names_d %in% c("lowlifex", "Demog.Index.Supp"))]
pnd <- paste0("state.avg.", nd)

############################################### # 
# EJAM names_d needED to be multiplied by 100x and rounded. ***************

x2[, c(pnd, nd)] <- round(  100 * x2[, c(pnd, nd)]  , 0 )

############################################### # 
# other issues:

ratios <- x1 / x2
avgratio = colMeans(ratios, na.rm = T)
t(round(ratios,2))[1:85,1:10]
round(cbind(avgratio[avgratio > 1.05 | avgratio < 0.95]),2)

############################################### # 
# these US pctiles tend to be wrong, somehow: ***************

# proximity.npdes              0.65
# pctile.pctlingiso            0.88
# pctile.proximity.npdes       0.91
# pctile.ust                   0.94

############################################## # 
# # EJAM state.pctile.... are all wrong somehow ***************
#
# > x1$state.pctile.o3
# [1] 45 95 64 91 77 38 16 74 53 35 79  0 15 21 78 15 83  5 19 56 20 48 41  7 46 20 83 70 60 10  3 35  3 19 73 61 98 80 53 53 35 25 68 15 74 98 16 72 48 55
# > x2$state.pctile.o3
# [1]  0  0  0  3  0  0  0  0  0  0  0  0  1  0  0  6  0  0  9  0  0  0  3  0  0  0  0 32  0  0 41  0  0  0  0 10 53  5  0 70  0  0  0  0  0  0 11  0  0  0
# > 







