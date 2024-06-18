# FORMULAS THAT WERE BEING USED IN EJAM::doaggregate()

## see ?calc_ejam() for examples showing how these can work

######################### #

# NOTE THIS SAME INFO IS MORE OR LESS IN
# map_headernames$is.wtdmean and 
# map_headernames$denominator  !!
#  see github issue about the formulas
# doaggregate() will probably move away from any of the info below and just use map_headernames$is.wtdmean and $denominator for most calculations??

# countcols ####
  
countcols = "pop"

# wtdmeancols ####

# wtdmeancols = names_these

wtdmeancols = c(
  
  "lowlifex", "pctunder5", "pctover64", "pctmin",
  names_d_subgroups_alone,
  names_d_subgroups_nh,        # remove hisp that is already in other list?
  
  "pctunder18", "pctover17", "pctmale", "pctfemale",  
  
  "pctlowinc",
  "pctlths",
  "pctlingiso",
  "pctpre1960",
  "pctunemployed",
  
  "pctdisability",
  "pctownedunits",   # ** check
  "pctpoor",
  
  "pct_lan_spanish",    # ** check
  "pct_lan_ie",      # ** check
  "pct_lan_api",     # ** check
  "pct_lan_eng_na",    # ** check
  
  "pctspanish_li",  # ** check
  "pctie_li",  # ** check
  "pctapi_li",  # ** check
  "pctother_li",   # ** check
  
  "Demog.Index",    #??  # ** check
  "Demog.Index.Supp" # ?  # ** check
)

## Define weights (denominators) ####

wtscols = c(
  
  rep("pop", 4), 
  rep("pop", length(names_d_subgroups_alone)),
  rep("pop", length(names_d_subgroups_nh)),
  
  rep("pop", 4), # ages and gender
  
  "povknownratio",
  "age25up",
  "hhlds",
  "builtunits",
  "unemployedbase",
  
  "disab_universe",
  "occupiedunits",
  "hhlds", #??   # "povknownratio",  # ???
  
  rep("lan_universe", 4),
  rep("lingiso", 4),
  
  "pop", #? pop?
  "pop"  #? pop?
) 

# pct_lan_spanish = ifelse(lan_universe == 0, 0, lan_spanish  / lan_universe),  # blockgroupstats has these counts
# pct_lan_ie      = ifelse(lan_universe == 0, 0, lan_ie       / lan_universe),  # blockgroupstats has these counts
# pct_lan_api     = ifelse(lan_universe == 0, 0, lan_api      / lan_universe),  # blockgroupstats has these counts
# # pct_lan_eng_na  = ifelse(lan_universe == 0, 0, lan_eng_na   / lan_universe),  # blockgroupstats has these counts

# pctspanish_li = ifelse(lingiso == 0, 0, spanish_li  /  lingiso), # blockgroupstats has
# pctie_li      = ifelse(lingiso == 0, 0, ie_li       /  lingiso), # blockgroupstats has
# pctapi_li     = ifelse(lingiso == 0, 0, api_li      /  lingiso), # blockgroupstats has
# pctother_li   = ifelse(lingiso == 0, 0, other_li    /  lingiso)  # blockgroupstats has

##   missing from blockgroupstats 2.2, but in map_headernames and in EJScreen reports:
# "p_english"      "p_spanish"      "p_french"       "p_rus_pol_slav" "p_other_ie"     "p_vietnamese"   "p_other_asian"  "p_arabic"       "p_other"        "p_non_english" 

################################ # 

## save this info in map_headernames columns 
#   is.wtdmean = TRUE  and 
#   denominator = "pop" etc. (ie the weight)
##
# usethis::use_data(wtdmeancols, overwrite = TRUE)
# usethis::use_data(wtscols, overwrite = TRUE)

cbind(wtdmeancols, wtscols)

# wtdmeancols        wtscols         
# [1,] "lowlifex"         "pop"           
# [2,] "pctunder5"        "pop"           
# [3,] "pctover64"        "pop"           
# [4,] "pctmin"           "pop"           
# [5,] "pcthisp"          "pop"           
# [6,] "pctba"            "pop"           
# [7,] "pctaa"            "pop"           
# [8,] "pctaiana"         "pop"           
# [9,] "pctnhpia"         "pop"           
# [10,] "pctotheralone"    "pop"           
# [11,] "pctmulti"         "pop"           
# [12,] "pctwa"            "pop"           
# [13,] "pcthisp"          "pop"           
# [14,] "pctnhba"          "pop"           
# [15,] "pctnhaa"          "pop"           
# [16,] "pctnhaiana"       "pop"           
# [17,] "pctnhnhpia"       "pop"           
# [18,] "pctnhotheralone"  "pop"           
# [19,] "pctnhmulti"       "pop"           
# [20,] "pctnhwa"          "pop"           
# [21,] "pctunder18"       "pop"           
# [22,] "pctover17"        "pop"           
# [23,] "pctmale"          "pop"           
# [24,] "pctfemale"        "pop"           
# [25,] "pctlowinc"        "povknownratio" 
# [26,] "pctlths"          "age25up"       
# [27,] "pctlingiso"       "hhlds"         
# [28,] "pctpre1960"       "builtunits"    
# [29,] "pctunemployed"    "unemployedbase"
# [30,] "pctdisability"    "disab_universe"
# [31,] "pctownedunits"    "occupiedunits" 
# [32,] "pctpoor"          "hhlds"         
# [33,] "pct_lan_spanish"  "lan_universe"  
# [34,] "pct_lan_ie"       "lan_universe"  
# [35,] "pct_lan_api"      "lan_universe"  
# [36,] "pct_lan_eng_na"   "lan_universe"  
# [37,] "pctspanish_li"    "lingiso"       
# [38,] "pctie_li"         "lingiso"       
# [39,] "pctapi_li"        "lingiso"       
# [40,] "pctother_li"      "lingiso"       
# [41,] "Demog.Index"      "pop"           
# [42,] "Demog.Index.Supp" "pop" 

######################### #
# formulas_all  ####
######################### #

formulas_sum     = c(paste0("aggregate_", countcols,    " = sum(", countcols, ",      na.rm = TRUE)"))
formulas_wtdmean = c(paste0("aggregate_", wtdmeancols , " = sum(", wtscols, " * ", wtdmeancols, ", na.rm = TRUE) / sum(", wtscols, ", na.rm = TRUE)"))
formulas_all     = c(formulas_sum, formulas_wtdmean)

# formulas_all
# [1] "aggregate_pop = sum(pop, na.rm = TRUE)"                                                                           
# [2] "aggregate_lowlifex = sum(pop * lowlifex, na.rm = TRUE) / sum(pop, na.rm = TRUE)"  
#  etc etc

######################### #
# formulas_d ####
######################### #

# THESE WOULD BE THE FORMULAS IF PERCENTAGE IS CALCULATED AS RATIO OF SUMS OF COUNTS, NOT A WTDMEAN:
## these were hardcoded in doaggregate() and here, but could be replaced by weighted means based on above info.

formulas_d <- c(
  
  # "poptotal =  sum(pop, na.rm = TRUE)", # just to make lists same length and name cannot be same as input name
  
  # lowlifex ??? was wtdmean, not by formula, already 
  
  "pctunder5       = 1 * ifelse(pop == 0, 0,            under5        / pop)",
  "pctover64       = 1 * ifelse(pop == 0, 0,            over64        / pop)",
  "pctmin          = 1 * ifelse(pop == 0, 0, as.numeric(mins)         / pop)",

    "pcthisp         = 1 * ifelse(pop == 0, 0, as.numeric(hisp )        / pop)", ##
  "pctnhba         = 1 * ifelse(pop == 0, 0, as.numeric(nhba )        / pop)",
  "pctnhaiana      = 1 * ifelse(pop == 0, 0, as.numeric(nhaiana)      / pop)",
  "pctnhaa         = 1 * ifelse(pop == 0, 0, as.numeric(nhaa )        / pop)",
  "pctnhnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhnhpia )     / pop)",
  "pctnhotheralone = 1 * ifelse(pop == 0, 0, as.numeric(nhotheralone) / pop)",
  "pctnhmulti      = 1 * ifelse(pop == 0, 0, as.numeric(nhmulti )     / pop)",
  "pctnhwa         = 1 * ifelse(pop == 0, 0, as.numeric(nhwa )        / pop)",

 #### "pcthisp         = 1 * ifelse(pop == 0, 0, as.numeric(hisp )        / pop)", # was not repeated
  "pctba         = 1 * ifelse(pop == 0, 0, as.numeric(ba )        / pop)",
  "pctaiana      = 1 * ifelse(pop == 0, 0, as.numeric(aiana)      / pop)",
  "pctaa         = 1 * ifelse(pop == 0, 0, as.numeric(aa )        / pop)",
  "pctnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhpia )     / pop)",
  "pctotheralone = 1 * ifelse(pop == 0, 0, as.numeric(otheralone) / pop)",
  "pctmulti      = 1 * ifelse(pop == 0, 0, as.numeric(multi )     / pop)",
  "pctwa         = 1 * ifelse(pop == 0, 0, as.numeric(wa )        / pop)",
  
  "pctunder18 =  ifelse(pop == 0, 0, age_lt18 / pop)",
  "pctover17  =  ifelse(pop == 0, 0, age_gt17 / pop)",
  "pctmale    =  ifelse(pop == 0, 0, male    / pop)",
  "pctfemale  =  ifelse(pop == 0, 0, female  / pop)",
  
  "pctlowinc       = 1 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio)",
  "pctlths         = 1 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up)",
  "pctlingiso      = 1 * ifelse(hhlds          == 0, 0, lingiso                / hhlds)",
  "pctpre1960      = 1 * ifelse(builtunits     == 0, 0, pre1960                / builtunits)",
  "pctunemployed   = 1 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)",

  "pctdisability  = ifelse(disab_universe == 0, 0, disability / disab_universe)",  # 
  "pctownedunits =  ifelse(occupiedunits == 0, 0, ownedunits / occupiedunits)",  ##  
  "pctpoor  =  ifelse(hhlds == 0, 0, poor / hhlds)",  ##  ?
  
  "pct_lan_spanish = ifelse(lan_universe == 0, 0, lan_spanish  / lan_universe)",  ## 
  "pct_lan_ie      = ifelse(lan_universe == 0, 0, lan_ie       / lan_universe)",  ##  
  "pct_lan_api     = ifelse(lan_universe == 0, 0, lan_api      / lan_universe)",  ## 
    "pct_lan_eng_na     = ifelse(lan_universe == 0, 0, lan_eng_na   / lan_universe)",  ##  

  "pctspanish_li = ifelse(lingiso == 0, 0, spanish_li  /  lingiso)",   ##  
  "pctie_li      = ifelse(lingiso == 0, 0, ie_li       /  lingiso)",   ## 
  "pctapi_li     = ifelse(lingiso == 0, 0, api_li      /  lingiso)",   ##  
  "pctother_li   = ifelse(lingiso == 0, 0, other_li    /  lingiso)",     ## 
  
  "Demog.Index = (pctlowinc + pctmin) / 2",
  "Demog.Index.Supp  = (pctlowinc + pctunemployed + pctlths + pctlingiso + ifelse(is.na(lowlifex), 0, lowlifex) ) / ifelse(is.na(lowlifex), 4, 5)"
  
)
    # pctfemale1849 = ifelse(pop == 0, 0, female1849  / pop),

######################### #
# metadata_add() & use_data() ####
######################### #

formulas_all <- EJAM:::metadata_add(formulas_all)

usethis::use_data(formulas_all, overwrite = TRUE)

formulas_d <- EJAM:::metadata_add(formulas_d)

usethis::use_data(formulas_d, overwrite = TRUE)


################################################ #
