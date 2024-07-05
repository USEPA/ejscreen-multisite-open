# datacreate_rename_blockgroupstats variables

# after an update of map_headernames, can update names of indicators as stored in blockgroupstats like this:
# but probably also need to check/update these:
# 
# usastats, statestats, avg
# avg.in.us
# high_pctiles_tied_with_min
# etc.

library(EJAM)

# done once to fix some names:
fixonce <- function(oldnames) {
  oldnames <- gsub("PCT_HLI_SPANISH_LI", "pctspanish_li", oldnames)
  oldnames <- gsub("PCT_HLI_IE_LI", "pctie_li", oldnames)
  oldnames <- gsub("PCT_HLI_API_LI", "pctapi_li", oldnames)
  oldnames <- gsub("PCT_HLI_OTHER_LI", "pctother_li", oldnames)
  
  oldnames <- gsub("DISABILITYPCT", "pctdisability", oldnames)
  oldnames <- gsub("LIMITEDBBPCT", "raw_cg_limitedbbpct", oldnames)
  oldnames <- gsub("NOHINCPCT", "raw_cg_nohincpct", oldnames)
  oldnames <- gsub("ASTHMA", "raw_hi_asthma", oldnames)
  oldnames <- gsub("HEARTDISEASE", "raw_hi_heartdisease", oldnames)
  
  return(oldnames)
}


names(blockgroupstats) <- EJAM::fixcolnames(names(blockgroupstats), "oldnames", "r") 
# pct_lan_spanish = ifelse(lan_universe == 0, 0, lan_spanish  / lan_universe),  # blockgroupstats has these counts
# pct_lan_ie      = ifelse(lan_universe == 0, 0, lan_ie       / lan_universe),  # blockgroupstats has these counts
# pct_lan_api     = ifelse(lan_universe == 0, 0, lan_api      / lan_universe),  # blockgroupstats has these counts
# pct_lan_eng_na  = ifelse(lan_universe == 0, 0, lan_eng_na   / lan_universe),    # blockgroupstats has these counts
# 
# pctspanish_li = ifelse(lingiso == 0, 0, spanish_li  /  lingiso), # blockgroupstats has
# pctie_li      = ifelse(lingiso == 0, 0, ie_li       /  lingiso), # blockgroupstats has
# pctapi_li     = ifelse(lingiso == 0, 0, api_li      /  lingiso), # blockgroupstats has
# pctother_li   = ifelse(lingiso == 0, 0, other_li    /  lingiso)    # blockgroupstats has

## missing from blockgroupstats 2.2, but in map_headernames and in EJScreen reports:
# "p_english"      "p_spanish"      "p_french"       "p_rus_pol_slav" "p_other_ie"     "p_vietnamese"   "p_other_asian"  "p_arabic"       "p_other"        "p_non_english" 

EJAM:::metadata_add(blockgroupstats)
usethis::use_data(blockgroupstats, overwrite = TRUE)

# [88,] "lan_universe"
# [89,] "lan_eng_na"              
# [90,] "lan_spanish"             
# [91,] "lan_ie"                  
# [92,] "lan_api"   

# [93,] "spanish_li"              
# [94,] "pctspanish_li"           
# [95,] "ie_li"                   
# [96,] "pctie_li"                
# [97,] "api_li"                  
# [98,] "pctapi_li"               
# [99,] "other_li"                
# [100,] "pctother_li"    

names(usastats) <- EJAM::fixcolnames(names(usastats), "oldnames", "r") 
names(usastats) <- fixonce(names(usastats))
EJAM:::metadata_add(usastats)
usethis::use_data(usastats, overwrite = TRUE)

names(statestats) <- EJAM::fixcolnames(names(statestats), "oldnames", "r") 
names(statestats) <- fixonce(names(statestats))
EJAM:::metadata_add(statestats)
usethis::use_data(statestats, overwrite = TRUE)

names(avg.in.us) <- EJAM::fixcolnames(names(avg.in.us), "oldnames", "r") 
names(avg.in.us) <- fixonce(names(avg.in.us))
EJAM:::metadata_add(avg.in.us)
usethis::use_data(avg.in.us, overwrite = TRUE)


for (i in 1:length(high_pctiles_tied_with_min)) {
  names(high_pctiles_tied_with_min[[i]]) <- EJAM::fixcolnames(
    names(high_pctiles_tied_with_min[[i]]), "oldnames", "r"
  )
  names(high_pctiles_tied_with_min[[i]]) <- fixonce(names(high_pctiles_tied_with_min[[i]]))
}
EJAM:::metadata_add(high_pctiles_tied_with_min)
usethis::use_data(high_pctiles_tied_with_min, overwrite = TRUE)


