

datacreate_names_pct_as_fraction <- function(map_headernames)   {
  
##################### # 
# datacreate_names_pct_as_fraction.R

## this should happen after map_headernames is recreated if necessary
# map_headernames$pct_as_fraction_blockgroupstats is TRUE when  map_headernames$rname %in% names_pct_as_fraction_blockgroupstats
# map_headernames$pct_as_fraction_ejamit          is TRUE when  map_headernames$rname %in% names_pct_as_fraction_ejamit
# map_headernames$pct_as_fraction_ejscreenit      is TRUE when  map_headernames$rname %in% names_pct_as_fraction_ejscreenit

names_pct_as_fraction_blockgroupstats <- map_headernames$rname[map_headernames$pct_as_fraction_blockgroupstats]

names_pct_as_fraction_blockgroupstats <- names_pct_as_fraction_blockgroupstats[names_pct_as_fraction_blockgroupstats %in% names(blockgroupstats)]
# names_pct_as_fraction_blockgroupstats <- names_d

names_pct_as_fraction_ejamit <- map_headernames$rname[map_headernames$pct_as_fraction_ejamit]

# names_pct_as_fraction_ejamit <-  c(
#   names_d, 
#   names_d_avg, names_d_state_avg,
#   names_d_subgroups, 
#   names_d_subgroups_avg, names_d_subgroups_state_avg,
###########   what about subgroups_nh  and subgroups_alone 
#   "pctdisability",  "p_own_occupied", 
#   "pctunder18", "pctover17", 
#   "pctmale", "pctfemale"
# )

names_pct_as_fraction_ejscreenit <-  map_headernames$rname[map_headernames$pct_as_fraction_ejscreenit]
# names_pct_as_fraction_ejscreenit <- c("pctpre1960", "state.avg.pctpre1960", "avg.pctpre1960") # but maybe it gets reported like 0-1?

# had to put that info into map_headernames.xlsx first



### if we wanted to return all these and do metadata and use_data outside this function: 
# 
# x <- list(names_pct_as_fraction_ejamit = names_pct_as_fraction_ejamit,
#           names_pct_as_fraction_ejscreenit = names_pct_as_fraction_ejscreenit, 
#           names_pct_as_fraction_ejamit = names_pct_as_fraction_ejamit)
# return(x)


names_pct_as_fraction_blockgroupstats <- metadata_add(names_pct_as_fraction_blockgroupstats)
names_pct_as_fraction_ejscreenit      <- metadata_add(names_pct_as_fraction_ejscreenit)
names_pct_as_fraction_ejamit          <- metadata_add(names_pct_as_fraction_ejamit)

usethis::use_data(names_pct_as_fraction_blockgroupstats, overwrite = T)
usethis::use_data(names_pct_as_fraction_ejscreenit,      overwrite = T)
usethis::use_data(names_pct_as_fraction_ejamit,          overwrite = T)

return(NULL)


##################### # 

## how those were found in ejscreenit() outputs
### x = ejscreenit(testpoints_5)$table
# x = testoutput_ejscreenit_500$table
# x = cbind(summary(x)[6,])
# x = data.frame(rownames(x), x)
# rownames(x) <- NULL
# x = x[order(x[,2]), ]
# head(x, 30)
### 116       US average for Lead Paint Indicator (% pre-1960s housing)       Max.   :0.3  
### 44     State average for Lead Paint Indicator (% pre-1960s housing)    Max.   :0.5500  
### 21                       Lead Paint Indicator (% pre-1960s housing)   Max.   :0.90000  
###
###    pctpre1960, state.avg.pctpre1960, avg.pctpre1960

}

# USE FUNCTION ####
datacreate_names_pct_as_fraction(map_headernames = map_headernames)  
# That function does metadata_add and use_data
rm("datacreate_names_pct_as_fraction")
