#     ######################################################################################
#     # SUMMARY STATS ON DISPARITY BY GROUP BY ENVT ISSUE
#     #  See help for RR.table() in ejanalysis package
#     # (This is very slow right now)

stop('need bg22plus from ejscreen pkg')
bg <-  bg22plus  # bg=bg22plus
# names.d.subgroups.pct and names.d are from ejscreen pkg also
names.dplus <- c( names.d,  names.d.subgroups.pct)

###############################################
# NOT FIXED YET:
#
# Ratios <- ejanalysis package file RR.table(
#   bg, 
#   Enames = ejscreen package file names.e, 
#   Dnames = names.dplus, 
#   popcolname = 'pop', 
#   digits = 2
# )
# RRS.US  <- Ratios
# RRS.US
# attributes(RRS.US) <- c(attributes(RRS.US), metadata)
# usethis::use_data(RRS.US)
###############################################

# MeansByGroup_and_Ratios <- ejanalysis package file RR.means(
#   e = subset(bg, select=names.e), 
#   d = subset(bg, select = names.dplus), 
#   pop = bg$pop
# )
# MeansByGroup_and_Ratios 

# MeansByGroup_and_Ratios <- metadata_add(MeansByGroup_and_Ratios)
# or manually set attributes to store metadata on vintage
metadata <- list(
  census_version = 2020,
  acs_version = '2016-2020',
  acs_releasedate = '3/17/2022',
  ejscreen_version = '2.1',
  ejscreen_releasedate = 'October 2022',
  ejscreen_pkg_data = 'bg22'
)
# attributes(MeansByGroup_and_Ratios) <- c(attributes(MeansByGroup_and_Ratios), metadata)

# usethis::use_data(MeansByGroup_and_Ratios)

rm(bg)
