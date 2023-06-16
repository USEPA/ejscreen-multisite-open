 library(EJAM)
ejamout <- ejamit(   testpoints_50, silentinteractive = TRUE)
apiout <- ejscreenit(testpoints_50, interactiveprompt = FALSE, nosave = T, nosee = T)

# > names(apiout)
# [1] "table"     "map"       "plot"      "us.ratios"
# > names(ejamout)
# [1] "results_overall"                     "results_bysite"                      "results_bybg_people"                
# [4] "longnames"                           "count_of_blocks_near_multiple_sites" "results_summarized"                 
# [7] "formatted"   

library(data.table)
xejam <- data.table::setDF(data.table::copy(ejamout$results_bysite))
xapi  <- apiout$table
setdiff(names(xejam), names(xapi))
# 153 colnames in EJAM that were not in ejscreenapi table output
# 46 in API only
# 108 shared by both.


# related to Supplemental:  (these are being added as placeholders with NA values, in ejscreenapi outputs)
suppnames <- c(
 "Demog.Index.Supp",                           "lowlifex", 
 "pctile.Demog.Index.Supp",             "pctile.lowlifex", 
 "avg.Demog.Index.Supp",                   "avg.lowlifex" , 
 "state.pctile.Demog.Index.Supp", "state.pctile.lowlifex",
 "state.avg.Demog.Index.Supp",       "state.avg.lowlifex"    
)

# related to subgroups, not in api:  (will be in June/July release of EJScreen, but were not yet in API)
subnames <- c(
names_d_subgroups, 
names_d_subgroups_avg, names_d_subgroups_state_avg,
names_d_subgroups_pctile, names_d_subgroups_state_pctile ,
names_d_subgroups_ratio_to_avg, names_d_subgroups_ratio_to_state_avg
)

# adding these to ejscreenapi at least for basic Demog groups and Envt, but need to add for subgroups and 
# also need to add state average ratios not just US.
rationames <- grep("ratio\\.to", names(xejam), value = T)
# "ratio.to"

# Still need to double check naming conventions:  pctile.EJ.DISPARITY...  versus  EJ.DISPARITY...
# Mostly have added these to EJAM now. 
ejnames <- grep("EJ\\.DISPARITY", names(xejam), value = T)

# will not be in api outputs.
countnames <- c(names_d_count, names_d_subgroups_count )

###############################################################
# SEE WHAT ELSE BESIDES THOSE DOES NOT MATCH ###############################################################

names(xejam)[!(names(xejam) %in% c(names_other, rationames, subnames, suppnames, ejnames, countnames)) & !(names(xejam) %in% names(xapi))]

###############################################################
 
# names in api vs in ejam were:   Updated to match, by editing map_headernames file.
# "EJScreenPDF"    `EJScreen Report` 
# "EJScreenMAP"    `EJScreen Map` 
# "EJScreenACS"    `ACS Report`
# `ECHO report`   NOT IN API OUTPUT YET
 

# others also not possible in API:

# "distance_min"           "distance_min_avgperson"
# "sitecount_max"          "sitecount_unique"       "sitecount_avg"    


