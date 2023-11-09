
# names_these
#
# [1] "Demog.Index"      "Demog.Index.Supp" "pctlowinc"        "pctlingiso"       "pctunemployed"    "pctlths"         
# [7] "lowlifex"         "pctunder5"        "pctover64"        "pctmin"        
#
# "pcthisp"          "pctnhba"         
# [13] "pctnhaa"          "pctnhaiana"       "pctnhnhpia"       "pctnhotheralone"  "pctnhmulti"       "pctnhwa"    
# 
# [19] "pm"               "o3"               "cancer"           "resp"             "dpm"              "pctpre1960"      
# [25] "traffic.score"    "proximity.npl"    "proximity.rmp"    "proximity.tsdf"   "proximity.npdes"  "ust"             
# [31] "rsei"      

setdiff(names_these, names(usastats)) 
#  unique(union(names_these, c(names_d_subgroups_alone, names_d_subgroups_nh))) %in% names(usastats)
#  unique(union(names_these, c(names_d_subgroups_alone, names_d_subgroups_nh))) %in% names(statestats)

longlist <- unique(c(names_e, names_d, names_d_subgroups_nh, names_d_subgroups_alone))

# a data.frame of 1 row per variable:
usastats_means(varnames = longlist, dig = 6)

# a data.frame of 1 col per variable, and easier to view in RStudio console
avg.in.us <-   usastats[ usastats$PCTILE == "mean",  longlist ]

  setdiff(names_these, names(avg.in.us))
 
  

## if we wanted raw EJ index averages, but nobody ever looks at raw EJ, only at percentile, so ratio of raw/avgraw is not so useful to people
# ejrawnames <- c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state)
# manynames  <- c(names_these, ejrawnames)
# avg.in.us  <-   usastats[ usastats$PCTILE == "mean", intersect(manynames,   names( usastats))]

avg.in.us <- metadata_add(avg.in.us)

# note the regular name not avg. name is used in the usastats table
usethis::use_data(avg.in.us, overwrite = TRUE)

################################################################################ #

#  ALSO COULD USE A FUNCTION TO LOOK UP THE MEANS WHEN NEEDED. 

avg.in.us                # This is a data.frame, 1 row, where colnames are indicators
avg.in.us[names_e]          # subset is a data.frame!
unlist(avg.in.us[names_e])  # to make it a vector

usastats_means()        # This is a matrix, with 1 col, and indicator names are rownames 
usastats_means(names_e)     # subset is a matrix        and indicator names are rownames
usastats_means()[names_e, ] # subset is a named vector  and indicator names are  names

help(usastats_query)
