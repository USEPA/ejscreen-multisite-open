
#  EJAM/data-raw/datacreate_names_of_indicators.R

# *** This was updated for the 2023 ver 2.2 
# and to be reconciled with or replaced with map_headernames approach.
## see EJAMejscreenapi::map_headernames$varlist which now has most or all of these in a table 
## see   EJAMejscreenapi/data-raw/update_to_ejscreenv2.2.R

# - friendly names differ somewhat

# ** change when ready to switch to using subgroups_alone like white alone not nonhispanic white alone:

subgroups_type <- c(  'nh', 'alone')[1]  # simple names without _nh or _alone should be usable/ what is used in code, and here get set same as nh for now and set to same as alone when want to switch 

#  *** EJScreen refers to EJ indexes without mentioning they are percentiles, since that is the only way they are reported.
# Should EJAM do same, or be more explicit with friendly names? 
# names_ej_friendly can be used instead for pctiles, probably, since it does not mention whether it is raw or percentile

# library(EJAMejscreenapi)
# map_headernames[grepl("ej", map_headernames$varlist), c('varlist', 'newnames_ejscreenapi', 'names_friendly')]
# map_headernames[grepl("_d", map_headernames$varlist), c('varlist', 'newnames_ejscreenapi', 'names_friendly')]
# map_headernames[map_headernames$varlist == 'names_e' | grepl("names_e_", map_headernames$varlist), c('varlist', 'newnames_ejscreenapi', 'names_friendly')]
# unique(map_headernames$varlist)


# for example...
#
# cbind( xlsnames = EJAMejscreenapi::map_headernames['names_ej' == EJAMejscreenapi::map_headernames$varlist, c( 'names_friendly')],  names_ej_friendly )
# xlsnames                              names_ej_friendly     
# [1 ,] "EJ: PM2.5 (raw)"                     "EJ: PM2.5"           
# [2, ] "EJ: Ozone (raw)"                     "EJ: Ozone"           
# [3, ] "EJ: Cancer risk (raw)"               "EJ: Cancer risk"     
# [4, ] "EJ: Respiratory (raw)"               "EJ: Respiratory"     
# [5, ] "EJ: Diesel PM (raw)"                 "EJ: Diesel PM"       
# [6, ] "EJ: % built pre-1960 (raw)"          "EJ: % built pre-1960"
# [7, ] "EJ: Traffic (raw)"                   "EJ: Traffic"         
# [8, ] "EJ: NPL (raw)"                       "EJ: NPL"             
# [9, ] "EJ: RMP (raw)"                       "EJ: RMP"             
# [10,] "EJ: TSDF (raw)"                      "EJ: TSDF"            
# [11,] "EJ: NPDES (raw)"                     "EJ: NPDES"           
# [12,] "EJ: Underground storage tanks (raw)" "EJ: UST" 
#      RSEI  


############################################################################## #
## code to prepare `names_of_indicators` dataset 
# Define lists of names of EJScreen-related variables for use here
############################################################################## #

# FOR NOW I AM putting these all in one list called namez, 
#  and also lists in the varlist column of EJAMejscreenapi::map_headernames
# AND ALSO AS multiple objects like names_d 
#
#  But could recode later to use namez$d_friendly instead of names_d_friendly etc.
#  and/or could recode later to use 1 big table, like A MERGED VERSION OF THESE:
# 
#  EJAMejscreenapi::map_headernames which is created from .xlsx using the script in /data-raw folder
# older  EJAMbatch.summarizer  ::  varnamesinfo22.rda  which is prob from EJAMbatch.summarizer/inst/map_batch_to_friendly_fieldnames_2022_EJAM.xlsx but needed a creation script in data-raw folder to clarify that
#  EJAM::namez 
#  ejscreen package file ejscreenformulas 

# In the short term, before some mapping file is used to track all variable names, we could simplify a bit by 
# recoding all functions to use namez$d_friendly instead of names_d_friendly, etc., so there is only 1 .rda file and can see all names in 1 command or file,
#  Would need to replace all   names_([a-z|_]*)  with  namez$\1   in all the .R files (functions in EJAM package)
#  and then code to create all the small .rda files via usethis::use_data() in \data-raw\names_of_indicators.R will be obsolete,
#  and we can get rid of all those small data objects in the /EJAM/data/ folder
#    via   something like this:
# file.remove( list.files(path = "./data/", pattern = "names_"))  #  
#
# paste(namesoflistsofnames, collapse = ", ")
#  "names_wts, names_d, names_d_pctile,etc

# names(namez)




# see also:  EJAMejscreenapi/map_headernames.csv/xlsx  
#         or EJAMejscreenapi::map_headernames$newnames_ejscreenapi[ mapheadernames$varlist == "names_d] 

# see also:  EJAM/data-raw/names_of_indicators.R (this file)
# see also:  EJAMbatch.summarizer/data-raw/names_of_variables.R
# see also: ejscreen package file names.e, ejscreenformulas, etc. 

############################################################################## #
# To rename indicator variables ####

#    to change names,   which function to use?
# e.g. this  works but relies on ejscreen package file  pkg, and see older  EJAMbatch.summarizer function change... and fixnames and fixcolnames and fixnamestype etc. 
# names(USA_2022)    <- ejscreen package file ejscreenformulas$Rfieldname[match(names(USA_2022),    ejscreen package file ejscreenformulas$gdbfieldname)] 
# names(States_2022) <- ejscreen package file ejscreenformulas$Rfieldname[match(names(States_2022), ejscreen package file ejscreenformulas$gdbfieldname)]

# similar to: EJAMbatch.summarizer pkg objects names_d_batch_friendly, names_e_batch_friendly 

############################################################################## #

# POPULATION COUNT (WEIGHTS) ####

# used for weighted means to get stats on avg person nearby any site
# 
names_wts <- "pop"

############################################################################## #

#  DEMOGRAPHICS - rawpct, friendlypct, and count; and other counts like denominators (not pctile) ####

# raw percents
names_d <- c(
  "Demog.Index",   "Demog.Index.Supp",
  
  "pctlowinc",  
  "pctlingiso",   
  "pctunemployed",
  "pctlths",   # note this sort order was fixed 3/2/23 to put lowinc before min, to match friendly order
  "lowlifex", 
  "pctunder5",  "pctover64", 
  
  "pctmin"
)

# friendly raw percents
names_d_friendly <- c(
  "Demog.Ind.",   "Suppl Demog Index", 
  
  "% Low-inc.", 
  "% Limited English",
  "% Unemployed",
  "% < High School", 
  "Low life expectancy",    # note this is not a percent, actually?
  "% < age 5", "% > age 64", 
  
  "% People of Color"
)

# no friendly version of counts?  e.g.,  
names_d_count_friendly <- paste0("Count of ", gsub("% ", "", names_d_friendly))

# counts with exceptions, and other counts
names_d_count <- gsub('pct', '', names_d); names_d_count <- gsub('min', 'mins', names_d_count)
dontuse = names_d_count %in% c('Demog.Index',  'Demog.Index.Supp', 'lowlifex') # there is no count for these
names_d_count <- names_d_count[!dontuse]
names_d_count_friendly <- names_d_count_friendly[!dontuse]
rm(dontuse)


names_d_other_count <- c("pop", "nonmins", "povknownratio", "age25up", "hhlds", "unemployedbase", "pre1960", "builtunits")

# this was not being used:
names_d_other_count_friendly <- c('Population', 
                                  'Count of non-POC', 
                                  'Count of hhlds with known poverty ratio (denominator for % low income)', 
                                  'Count of age 25+ (denominator for %<high school)',
                                  'Count of households (denominator for %limited English)',
                                  'Count of denominator for %unemployed', 
                                  'Count of housing units built pre-1960',
                                  'Count of housing units (denominator for %pre-1960)')

############################################################################## #

# DEMOG SUBGROUPS - rawpct, friendlypct, & count (not pctile) ####
# 
# ** As of v2.2, EJAM and EJAMejscreenapi  will use 
# names_d_subgroups_nh    for non-hispanic versions, and 
# names_d_subgroups_alone for black alone, white alone, etc. -- ie, 
#   new ones EJScreen v2.2 uses, which do not exclude hispanic from race groups,
# so  names_d_subgroups will change to  names_d_subgroups_nh, but will not be used by EJAM for now? 
# and names_d_subgroups_alone has to be created and used everywhere names_d_subgroups was being used,
# but friendly names and so on have to be revised to be correct.

# subgroups_type

# raw percents
names_d_subgroups_alone  <- c("pcthisp",  "pctba",   "pctaa",   "pctaiana",   "pctnhpia",   "pctotheralone",   "pctmulti",   "pctwa")  # BUT ONLY SOME ARE GENERATED BY EJSCREEN2.2
names_d_subgroups_nh    <- c("pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", "pctnhwa")

# counts
names_d_subgroups_alone_count <- c(   "hisp",      "ba",      "aa",      "aiana",      "nhpia",      "otheralone",      "multi",      "wa")
names_d_subgroups_nh_count    <- c(   "hisp",    "nhba",    "nhaa",    "nhaiana",    "nhnhpia",    "nhotheralone",    "nhmulti",    "nhwa")
# or  names_d_subgroups_count <-  gsub("pct", "", names_d_subgroups)

# friendly raw percents

names_d_subgroups_alone_friendly <- c(
  "% Hispanic or Latino (any race)", 
  "% Black or African American, single race", 
  "% Asian, single race",
  "% American Indian or Alaska Native, single race", 
  "% Native Hawaiian or Other Pacific Islander, single race", 
  "% Other race, single race",
  "% Multirace (two or more races)",
  "% White (single race, includes White Hispanic)"
)
names_d_subgroups_nh_friendly <- c(   # , non-Hispanic
  "% Hispanic or Latino (any race)", 
  "% Black or African American, single race, non-Hispanic", 
  "% Asian, single race, non-Hispanic",
  "% American Indian or Alaska Native, single race, non-Hispanic", 
  "% Native Hawaiian or Other Pacific Islander, single race, non-Hispanic", 
  "% Other race, single race, non-Hispanic",
  "% Multirace (two or more races), non-Hispanic",
  "% White, single race, non-Hispanic"
)
# Basic subgroup lists

if ('alone' %in% subgroups_type) {
  names_d_subgroups        <-   names_d_subgroups_alone  
  names_d_subgroups_count    <- names_d_subgroups_alone_count 
  names_d_subgroups_friendly <- names_d_subgroups_alone_friendly
} else {
  names_d_subgroups        <-   names_d_subgroups_nh
  names_d_subgroups_count    <- names_d_subgroups_nh_count
  names_d_subgroups_friendly <- names_d_subgroups_nh_friendly
}

#   friendly version of counts 
names_d_subgroups_count_friendly       <-  gsub("% ", "Count of ", names_d_subgroups_friendly)
names_d_subgroups_alone_count_friendly <-  gsub("% ", "Count of ", names_d_subgroups_alone_friendly)
names_d_subgroups_nh_count_friendly    <-  gsub("% ", "Count of ", names_d_subgroups_nh_friendly)



############################################################################## #

# ENVIRONMENTAL - raw and friendly (no pctiles) ####

names_e <- c(
  "pm", "o3", "cancer", "resp", "dpm", 
  "pctpre1960", "traffic.score",  "proximity.npl", "proximity.rmp", 
  "proximity.tsdf", "proximity.npdes", "ust",
  "rsei")

names_e_friendly  <- c(
  "PM2.5", "Ozone", "Cancer risk", "Respiratory", "Diesel PM", 
  "% built pre-1960", "Traffic", "NPL proximity", "RMP proximity", 
  "TSDF proximity", "NPDES proximity", "Underground storage tanks",
  "RSEI Air")

############################################################################## #

# EJ INDEXES - raw & friendly; + supp for raw & friendly; (not pctile) ####

#  4 types of EJ-related indicators: US2, US5, ST2, ST5: 
# x.eo versions,
# x.supp versions, 
# state.x.eo versions,  
# state.x.supp versions

# raw EJ actually has all 4 of these: US normal, State normal; US Suppl, State Suppl  versions (+friendly)


#####   NEED TO confirm distinction betwee raw vs pctile !    

names_ej <- paste0('EJ.DISPARITY.', names_e, '.eo')
# either drop the .eo or make sure .supp is replacing the .eo not just tacking onto it ***

names_ej_friendly <- c( # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED
  "EJ: PM2.5", 
  "EJ: Ozone", 
  "EJ: Cancer risk", 
  "EJ: Respiratory", 
  "EJ: Diesel PM", 
  "EJ: % built pre-1960", 
  "EJ: Traffic", 
  "EJ: NPL", 
  "EJ: RMP", 
  "EJ: TSDF", 
  "EJ: NPDES", 
  "EJ: UST",
  "EJ: RSEI")

names_ej_state          <- paste0('state.', names_ej)
names_ej_state_friendly <- paste0('State ', names_ej_friendly)

names_ej_supp       <- gsub("\\.eo$", ".supp", names_ej) # not just this: # paste0(          names_ej, '.supp')
names_ej_supp_friendly <- gsub("EJ", "Supp. EJ", names_ej_friendly)

names_ej_supp_state <- paste0('state.', names_ej_supp)
names_ej_supp_state_friendly <- gsub("EJ", "Supp. EJ", names_ej_state_friendly)

############################################################################## #

# PERCENTILES - D & sub; E; EJ (us & st, normal & supp) - friendly versions after that ####

names_d_pctile                 <- paste0(      'pctile.', names_d)
names_d_state_pctile           <- paste0('state.pctile.', names_d)


names_d_subgroups_alone_pctile       <- paste0(      'pctile.', names_d_subgroups_alone) # newer
names_d_subgroups_alone_state_pctile <- paste0('state.pctile.', names_d_subgroups_alone) # newer

names_d_subgroups_nh_pctile       <- paste0(      'pctile.', names_d_subgroups_nh) # newer
names_d_subgroups_nh_state_pctile <- paste0('state.pctile.', names_d_subgroups_nh) # newer

if ('alone' %in% subgroups_type) {
  names_d_subgroups_pctile            <- names_d_subgroups_alone_pctile
  names_d_subgroups_state_pctile      <- names_d_subgroups_alone_state_pctile
} else {
  names_d_subgroups_pctile            <- names_d_subgroups_nh_pctile
  names_d_subgroups_state_pctile      <- names_d_subgroups_nh_state_pctile
}


names_e_pctile                 <- paste0(      'pctile.', names_e)
names_e_state_pctile           <- paste0('state.pctile.', names_e)


names_ej_pctile                <- paste0(      'pctile.', names_ej)
names_ej_state_pctile          <- paste0('state.pctile.', names_ej)

names_ej_supp_pctile           <- paste0(      'pctile.', names_ej_supp) # most recently added
names_ej_supp_state_pctile     <- paste0('state.pctile.', names_ej_supp) # most recently added


# need no percentiles of counts like d_counts ***


################################# ############ #
# FRIENDLY PERCENTILES ??? ####
# - not sure I need or will use these, and whether named the same categories in map_headernames:

# note the description in map_headernames omitted the US and just said Percentile

names_d_pctile_friendly                 <- paste0(   'US percentile for ', names_d_friendly)
names_d_state_pctile_friendly           <- paste0('State percentile for ', names_d_friendly)


names_d_subgroups_alone_pctile_friendly       <- paste0(   'US percentile for ', names_d_subgroups_alone_friendly) # newer 
names_d_subgroups_alone_state_pctile_friendly <- paste0('State percentile for ', names_d_subgroups_alone_friendly) # newer  

names_d_subgroups_nh_pctile_friendly          <- paste0(   'US percentile for ', names_d_subgroups_nh_friendly) # newer 
names_d_subgroups_nh_state_pctile_friendly    <- paste0('State percentile for ', names_d_subgroups_nh_friendly) # newer 

if ('alone' %in% subgroups_type) {
  names_d_subgroups_pctile_friendly             <- names_d_subgroups_alone_pctile_friendly
  names_d_subgroups_state_pctile_friendly       <- names_d_subgroups_alone_state_pctile_friendly
} else {
  names_d_subgroups_pctile_friendly             <- names_d_subgroups_nh_pctile_friendly
  names_d_subgroups_state_pctile_friendly       <- names_d_subgroups_nh_state_pctile_friendly
}


names_e_pctile_friendly                 <- paste0(  'US percentile for ',  names_e_friendly)
names_e_state_pctile_friendly           <- paste0('State percentile for ', names_e_friendly)


#  *** EJScreen refers to EJ indexes without mentioning they are percentiles, since that is the only way they are reported.
# Should EJAM do same, or be more explicit with friendly names? 
# names_ej_friendly can be used instead for pctiles, probably, since it does not mention whether it is raw or percentile

# names_ej_pctile_friendly            <- names_ej_friendly
# names_ej_state_pctile_friendly      <- names_ej_friendly
# 
# names_ej_supp_pctile_friendly       <- names_ej_friendly
# names_ej_supp_state_pctile_friendly <- names_ej_friendly

# more explicit/ longer names: 
# raw EJ has all 4 of these: US normal, State normal; US Suppl, State Suppl  versions (+friendly)

names_ej_pctile_friendly                <- paste0(   'US percentile for ', names_ej_friendly)
names_ej_state_pctile_friendly          <- paste0('State percentile for ', names_ej_friendly)
names_ej_supp_pctile_friendly           <- paste0(   'US percentile for ', names_ej_supp_friendly) # most recently added
names_ej_supp_state_pctile_friendly     <- paste0('State percentile for ', names_ej_supp_friendly) # most recently added

################################# ############ #


# compiled list of raw, root indicators for which the US and State pctiles needing to be looked up ( no need for   friendly versions)

names_pctile <- c( # USA pctiles   # not including friendly version
  names_d_pctile,
  
  #  DO NOT INCLUDE 3 versions here (subgroups, subgroups_alone, subgroups_nh) - just the one we are actually going to use
  names_d_subgroups_pctile,     # only include one or the other type if at all, but note EJScreen 2.2 does NOT report these as percentiles - only raw percentage.
  # names_d_subgroups_nh_pctile,                   #  will start to use when ready  xxx
  # names_d_subgroups_alone_pctile,                #  will start to use when ready  xxx
  # 
  names_e_pctile,
  names_ej_pctile,      
  names_ej_supp_pctile 
)
# all STATE pctiles, but not friendly versions 
names_state_pctile <- c( # STATE pctiles   # not including friendly version
  gsub("pctile", "state.pctile", names_pctile)
)

# which base indicators need to be reported as percentiles? ####

names_need_pctile       <- gsub(      "pctile.", "", names_pctile)   # NOTE THIS IS US ONLY - NOT STATE
names_need_state_pctile <- gsub("state.pctile.", "", names_state_pctile)  # NOTE THIS IS STATE
# no friendly version need for that list
rm(names_pctile, names_state_pctile)
# varsneedpctiles <- c(names_e,  names_d, names_d_subgroups, names_ej)  # _nh   and  _alone  ?
# varnames.us.pctile <- paste0('pctile.', varsneedpctiles)
# varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)






############################################################################## #

# AVERAGE (not MEDIAN?) IN US AND STATE - raw & friendly versions - not for counts and not for EJ indexes ??  ####

names_d_avg                       <- paste0(      "avg.", names_d)
names_d_state_avg                 <- paste0("state.avg.", names_d)


names_d_subgroups_alone_avg       <- paste0(      "avg.", names_d_subgroups_alone)
names_d_subgroups_alone_state_avg <- paste0("state.avg.", names_d_subgroups_alone)

names_d_subgroups_nh_avg          <- paste0(      "avg.", names_d_subgroups_nh)  
names_d_subgroups_nh_state_avg    <- paste0("state.avg.", names_d_subgroups_nh)

names_d_subgroups_avg             <- paste0(      "avg.", names_d_subgroups)   # names_d_subgroups_nh_avg       or  names_d_subgroups_alone_avg
names_d_subgroups_state_avg       <- paste0("state.avg.", names_d_subgroups)   # names_d_subgroups_nh_state_avg  or names_d_subgroups_alone_state_avg
# if ('alone' == subgroups_type) {
# } else {
#   
# }

names_e_avg       <- paste0(      "avg.", names_e)
names_e_state_avg <- paste0("state.avg.", names_e)
# names_e_med, names_e_state_med,
# names_d_med, names_d_state_med,


names_d_avg_friendly       <- paste0("US Avg ",    names_d_friendly)
names_d_state_avg_friendly <- paste0("State Avg ", names_d_friendly) 


names_d_subgroups_nh_avg_friendly       <- paste0("US average ",    names_d_subgroups_nh_friendly)
names_d_subgroups_nh_state_avg_friendly <- paste0("State average ", names_d_subgroups_nh_friendly)

names_d_subgroups_alone_avg_friendly       <- paste0("US average ",    names_d_subgroups_alone_friendly)
names_d_subgroups_alone_state_avg_friendly <- paste0("State average ", names_d_subgroups_alone_friendly)

names_d_subgroups_avg_friendly       <- paste0("US average ",    names_d_subgroups_friendly)  #  
names_d_subgroups_state_avg_friendly <- paste0("State average ", names_d_subgroups_friendly)  # 

names_e_avg_friendly       <- paste0("US Avg ",    names_e_friendly)
names_e_state_avg_friendly <- paste0("State Avg ", names_e_friendly)

# no ratios used for raw EJ indexes ?

# no averages no ratios for counts like count of POC, count of low income, etc.

############################################################################## #

# RATIOS TO AVERAGE ####

names_d_ratio_to_avg  <- paste0("ratio.to.", names_d_avg) 
names_d_ratio_to_state_avg <- paste0("ratio.to.", names_d_state_avg) 


names_d_subgroups_nh_ratio_to_avg       <- paste0("ratio.to.", names_d_subgroups_nh_avg)  
names_d_subgroups_nh_ratio_to_state_avg <- paste0("ratio.to.", names_d_subgroups_nh_state_avg)

names_d_subgroups_alone_ratio_to_avg       <- paste0("ratio.to.", names_d_subgroups_alone_avg)  
names_d_subgroups_alone_ratio_to_state_avg <- paste0("ratio.to.", names_d_subgroups_alone_state_avg)

names_d_subgroups_ratio_to_avg       <- paste0("ratio.to.", names_d_subgroups_avg)            
names_d_subgroups_ratio_to_state_avg <- paste0("ratio.to.", names_d_subgroups_state_avg)    


names_e_ratio_to_avg       <- paste0("ratio.to.", names_e_avg)  
names_e_ratio_to_state_avg <- paste0("ratio.to.", names_e_state_avg)

names_d_ratio_to_avg_friendly       <- paste0("Ratio to ", names_d_avg_friendly)
names_d_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_d_state_avg_friendly) 


names_d_subgroups_nh_ratio_to_avg_friendly          <- paste0("Ratio to ", names_d_subgroups_nh_avg_friendly)
names_d_subgroups_nh_ratio_to_state_avg_friendly    <- paste0("Ratio to ", names_d_subgroups_nh_state_avg_friendly)

names_d_subgroups_alone_ratio_to_avg_friendly       <- paste0("Ratio to ", names_d_subgroups_alone_avg_friendly)
names_d_subgroups_alone_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_d_subgroups_alone_state_avg_friendly)

names_d_subgroups_ratio_to_avg_friendly             <- paste0("Ratio to ", names_d_subgroups_avg_friendly)      
names_d_subgroups_ratio_to_state_avg_friendly       <- paste0("Ratio to ", names_d_subgroups_state_avg_friendly)  


names_e_ratio_to_avg_friendly       <- paste0("Ratio to ", names_e_avg_friendly)
names_e_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_e_state_avg_friendly)

############################################################################## #
# these ####
## see doaggregate() where these are used:

#  Only one of the 3 ways can be used here:
#  DO NOT INCLUDE 3 versions here (subgroups, subgroups_alone, subgroups_nh) - just the one we are actually going to use
# 
# if (subgroups_type == 'alone') {
#   names_these                    <- c(names_d,              names_d_subgroups_alone,              names_e)   
#   names_these_avg                <- c(names_d_avg,          names_d_subgroups_alone_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
#   names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_alone_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
#   names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_alone_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
#   names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_alone_ratio_to_state_avg,   names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
# } else {
#   names_these                    <- c(names_d,              names_d_subgroups_nh,              names_e)   
#   names_these_avg                <- c(names_d_avg,          names_d_subgroups_nh_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
#   names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_nh_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
#   names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_nh_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
#   names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_nh_ratio_to_state_avg,      names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
# }
# if (subgroups_type == 'original') {  #  ONLY 
names_these                    <- c(names_d,              names_d_subgroups,              names_e)
names_these_avg                <- c(names_d_avg,          names_d_subgroups_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_ratio_to_state_avg,    names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
# }

# no need for friendly versions?

# counts and e and ej not included here in names_these  ***

############################################################################## #
#  compilations of names (these lack friendly version of the group)

# [14,] "names_wts"         

# [1,] "names_all_r"      

# [13,] "names_these"        
# [2,]  "names_these_avg"          
# [11,] "names_these_state_avg"         
# [9,]  "names_these_ratio_to_avg"      
# [10,] "names_these_ratio_to_state_avg"

# [6,] "names_need_pctile"             
# [7,] "names_need_state_pctile"       


############################################################################## #
# all_r ####
#   this is not used by the app but may be useful in maintaining map_headernames or namez, etc.

names_all_r =  c(
  names_d_other_count,
  names_d_count, 
  names_d, 
  names_d_avg,           names_d_state_avg, 
  names_d_pctile,        names_d_state_pctile,       
  names_d_ratio_to_avg,  names_d_ratio_to_state_avg,
  
  # subgroups_lists,   #   ONLY INCLUDE THE ONE BEING USED BY CODE ?  # subgroups   # not sure we need to include here all 3 versions of names of subgroups indicators
  names_d_subgroups,         
  # names_d_subgroups_friendly,
  names_d_subgroups_avg,         names_d_subgroups_state_avg,
  # names_d_subgroups_avg_friendly,names_d_subgroups_state_avg_friendly,
  names_d_subgroups_pctile,      names_d_subgroups_state_pctile, 
  # names_d_subgroups_pctile_friendly, names_d_subgroups_state_pctile_friendly, 
  names_d_subgroups_ratio_to_avg,          names_d_subgroups_ratio_to_state_avg,
  # names_d_subgroups_ratio_to_avg_friendly, names_d_subgroups_ratio_to_state_avg_friendly,
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  names_d_subgroups_count,  # no avg, no ratio, no pctiles, for counts
  
  
  c(names_e,  
    names_e_avg,          names_e_state_avg, 
    names_e_pctile,       names_e_state_pctile),
  names_e_ratio_to_avg, names_e_ratio_to_state_avg,
  
  # c(names_these_ratio_to_avg, names_these_ratio_to_state_avg), 
  
  c(names_ej,             names_ej_state,
    names_ej_supp,        names_ej_supp_state,
    names_ej_pctile,      names_ej_state_pctile,
    names_ej_supp_pctile, names_ej_supp_state_pctile)  # no ratios for EJ
)
names_all_r <- unique(names_all_r)

############################################################################## #
# sub_nh_text    <- c( 'names_d_subgroups_nh_count',           
#                   c('names_d_subgroups_nh',    'names_d_subgroups_nh_avg',    'names_d_subgroups_nh_state_avg',    'names_d_subgroups_nh_pctile',    'names_d_subgroups_nh_state_pctile') )
sub_nh_text    <- c('names_d_subgroups_nh',        
                    'names_d_subgroups_nh_friendly', 
                    'names_d_subgroups_nh_avg',             'names_d_subgroups_nh_state_avg',   
                    'names_d_subgroups_nh_avg_friendly',    'names_d_subgroups_nh_state_avg_friendly',  
                    'names_d_subgroups_nh_pctile',          'names_d_subgroups_nh_state_pctile',   
                    'names_d_subgroups_nh_pctile_friendly', 'names_d_subgroups_nh_state_pctile_friendly',   
                    'names_d_subgroups_nh_ratio_to_avg',          'names_d_subgroups_nh_ratio_to_state_avg',  
                    'names_d_subgroups_nh_ratio_to_avg_friendly', 'names_d_subgroups_nh_ratio_to_state_avg_friendly',   
                    # names_d_subgroups_nh_med,     names_d_subgroups_nh_state_med,       
                    'names_d_subgroups_nh_count'  # no avg, no ratio, no pctiles, for counts 
)

# sub_alone_text <- c( 'names_d_subgroups_alone_count',  
#                   c('names_d_subgroups_alone', 'names_d_subgroups_alone_avg', 'names_d_subgroups_alone_state_avg', 'names_d_subgroups_alone_pctile', 'names_d_subgroups_alone_state_pctile') )   
sub_alone_text <- c(
  'names_d_subgroups_alone',        
  'names_d_subgroups_alone_friendly', 
  'names_d_subgroups_alone_avg',             'names_d_subgroups_alone_state_avg',   
  'names_d_subgroups_alone_avg_friendly',    'names_d_subgroups_alone_state_avg_friendly',  
  'names_d_subgroups_alone_pctile',          'names_d_subgroups_alone_state_pctile',   
  'names_d_subgroups_alone_pctile_friendly', 'names_d_subgroups_alone_state_pctile_friendly',   
  'names_d_subgroups_alone_ratio_to_avg',          'names_d_subgroups_alone_ratio_to_state_avg',  
  'names_d_subgroups_alone_ratio_to_avg_friendly', 'names_d_subgroups_alone_ratio_to_state_avg_friendly',   
  # names_d_subgroups_alone_med,     names_d_subgroups_alone_state_med,       
  'names_d_subgroups_alone_count' # no avg, no ratio, no pctiles, for counts   
)

if ("alone" %in% subgroups_type) {
  sub_o_text <-  sub_alone_text
} else {
  sub_o_text <-  sub_nh_text
}

subgroups_lists_text <-  sub_o_text  



# 'names_d_subgroups',               
# 'names_d_subgroups_friendly',      
# 'names_d_subgroups_avg',             'names_d_subgroups_state_avg',      
# 'names_d_subgroups_avg_friendly',    'names_d_subgroups_state_avg_friendly',      
# 'names_d_subgroups_pctile',          'names_d_subgroups_state_pctile',       
# 'names_d_subgroups_pctile_friendly', 'names_d_subgroups_state_pctile_friendly',       
# 'names_d_subgroups_ratio_to_avg',          'names_d_subgroups_ratio_to_state_avg',      
# 'names_d_subgroups_ratio_to_avg_friendly', 'names_d_subgroups_ratio_to_state_avg_friendly',        
# # names_d_subgroups_med,     names_d_subgroups_state_med,          
# 'names_d_subgroups_count', # no avg, no ratio, no pctiles, for counts         


############################################################################## #

# **namez__ ####

# try putting these all in one list instead of multiple objects? could recode later to use namez$d_friendly instead of names_d_friendly etc.
# and/or    just store them in a big table

namesoflistsofnames = c(
  'names_all_r',  # and names_all will get created 
  'names_wts',
  ############################################ # 
  
  'names_d', 
  'names_d_friendly', # not for percentiles and counts?
  'names_d_avg',             'names_d_state_avg', 
  'names_d_avg_friendly',    'names_d_state_avg_friendly' ,
  'names_d_pctile',          'names_d_state_pctile',       
  'names_d_pctile_friendly', 'names_d_state_pctile_friendly' , # new
  'names_d_ratio_to_avg',          'names_d_ratio_to_state_avg',
  'names_d_ratio_to_avg_friendly', 'names_d_ratio_to_state_avg_friendly', 
  # names_d_med,  names_d_state_med,
  
  'names_d_count',     
  'names_d_count_friendly', 
  'names_d_other_count', # includes pop and other denominator counts. no avg, no ratio, no pctiles for this needed.
  'names_d_other_count_friendly',  # no avg, no ratios, no percentiles, for counts
  
  ############################################ # 
  # subgroups 
  
  # 'names_d_subgroups',
  # 'names_d_subgroups_nh',
  # 'names_d_subgroups_alone',
  # subgroups_lists_text, 
  # sub_nh_text,
  # sub_alone_text,
  
  #  plain subgroups,   subgroups_alone will match ejscreen v 2.2
  'names_d_subgroups',         
  'names_d_subgroups_friendly',
  'names_d_subgroups_avg',         'names_d_subgroups_state_avg',
  'names_d_subgroups_avg_friendly','names_d_subgroups_state_avg_friendly',
  'names_d_subgroups_pctile',      'names_d_subgroups_state_pctile', 
  'names_d_subgroups_pctile_friendly', 'names_d_subgroups_state_pctile_friendly', 
  'names_d_subgroups_ratio_to_avg',          'names_d_subgroups_ratio_to_state_avg',
  'names_d_subgroups_ratio_to_avg_friendly', 'names_d_subgroups_ratio_to_state_avg_friendly',
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  'names_d_subgroups_count',  # no avg, no ratio, no pctiles, for counts
  
  'names_d_subgroups_nh',         
  'names_d_subgroups_nh_friendly',
  'names_d_subgroups_nh_avg',         'names_d_subgroups_nh_state_avg',
  'names_d_subgroups_nh_avg_friendly','names_d_subgroups_nh_state_avg_friendly',
  'names_d_subgroups_nh_pctile',      'names_d_subgroups_nh_state_pctile', 
  'names_d_subgroups_nh_pctile_friendly', 'names_d_subgroups_nh_state_pctile_friendly', 
  'names_d_subgroups_nh_ratio_to_avg',          'names_d_subgroups_nh_ratio_to_state_avg',
  'names_d_subgroups_nh_ratio_to_avg_friendly', 'names_d_subgroups_nh_ratio_to_state_avg_friendly',
  # names_d_subgroups_nh_med,     names_d_subgroups_nh_state_med,
  'names_d_subgroups_nh_count',  # no avg, no ratio, no pctiles, for counts
  
  'names_d_subgroups_alone',         
  'names_d_subgroups_alone_friendly',
  'names_d_subgroups_alone_avg',         'names_d_subgroups_alone_state_avg',
  'names_d_subgroups_alone_avg_friendly','names_d_subgroups_alone_state_avg_friendly',
  'names_d_subgroups_alone_pctile',      'names_d_subgroups_alone_state_pctile', 
  'names_d_subgroups_alone_pctile_friendly', 'names_d_subgroups_alone_state_pctile_friendly', 
  'names_d_subgroups_alone_ratio_to_avg',          'names_d_subgroups_alone_ratio_to_state_avg',
  'names_d_subgroups_alone_ratio_to_avg_friendly', 'names_d_subgroups_alone_ratio_to_state_avg_friendly',
  # names_d_subgroups_alone_med,     names_d_subgroups_alone_state_med,
  'names_d_subgroups_alone_count',  # no avg, no ratio, no pctiles, for counts
  
  ############################################ # 
  
  'names_e',           
  'names_e_friendly',
  'names_e_avg',             'names_e_state_avg', 
  'names_e_avg_friendly',    'names_e_state_avg_friendly'  ,
  'names_e_pctile',          'names_e_state_pctile', 
  'names_e_pctile_friendly', 'names_e_state_pctile_friendly', # new
  'names_e_ratio_to_avg',          'names_e_ratio_to_state_avg',
  'names_e_ratio_to_avg_friendly', 'names_e_ratio_to_state_avg_friendly',
  # names_e_med, names_e_state_med,  
  ############################################ # 
  
  'names_ej',           'names_ej_state', # RAW EJ SCORE
  'names_ej_friendly',  'names_ej_state_friendly', # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED
  
  'names_ej_supp',          'names_ej_supp_state', 
  'names_ej_supp_friendly', 'names_ej_supp_state_friendly', 
  
  'names_ej_pctile',          'names_ej_state_pctile' ,
  'names_ej_pctile_friendly', 'names_ej_state_pctile_friendly',  # new
  
  'names_ej_supp_pctile',          'names_ej_supp_state_pctile',  ################################ # had been missing
  'names_ej_supp_pctile_friendly', 'names_ej_supp_state_pctile_friendly', # new
  ############################################ # 
  
  # 'names_pctile',  # all US pctile indicators #  - this is not used though
  # 'names_state_pctile', #  - this is not used though
  'names_need_pctile',  # base indicators that need to be reported as US percentiles not just raw scores
  'names_need_state_pctile', 
  
  'names_these',
  'names_these_avg',
  'names_these_state_avg',
  'names_these_ratio_to_avg',
  'names_these_ratio_to_state_avg'
)

namez <- lapply(namesoflistsofnames, get)
names(namez) <- gsub("^names_","", namesoflistsofnames)

# **names_all__ ####
# NOTE THIS IS VERY DIFFERENT THAN names_all_batch !!
names_all <- as.vector(unlist(namez))
names_all <- unique(names_all) # pop would appear twice

namesoflistsofnames <- c('names_all', namesoflistsofnames)

############################################################################## #
#   USE_DATA ####

usethis::use_data(namez, overwrite = TRUE)

############################################################################## #
# AND ALSO STORE EACH LITTLE OBJECT ? ####

usethis::use_data(
  
  names_all, names_all_r,
  names_wts, 
  ############################################ # 
  
  names_d, 
  names_d_friendly, # not for percentiles and counts?
  names_d_avg,          names_d_state_avg, 
  names_d_avg_friendly, names_d_state_avg_friendly ,
  names_d_pctile,       names_d_state_pctile,           
  names_d_pctile_friendly, names_d_state_pctile_friendly , # new
  names_d_ratio_to_avg,          names_d_ratio_to_state_avg,
  names_d_ratio_to_avg_friendly, names_d_ratio_to_state_avg_friendly, 
  # names_d_med,  names_d_state_med,
  names_d_count,
  names_d_count_friendly,
  names_d_other_count,  # includes pop and other denominator counts. no avg, no ratio, no pctiles for this needed.
  names_d_other_count_friendly, # no avg, no ratios, no percentiles, for counts
  
  ############################################ # 
  # subgroups 
  
  #  plain subgroups,   subgroups_alone will match ejscreen v 2.2
  names_d_subgroups,         
  names_d_subgroups_friendly,
  names_d_subgroups_avg,         names_d_subgroups_state_avg,
  names_d_subgroups_avg_friendly,names_d_subgroups_state_avg_friendly,
  names_d_subgroups_pctile,      names_d_subgroups_state_pctile, 
  names_d_subgroups_pctile_friendly, names_d_subgroups_state_pctile_friendly, 
  names_d_subgroups_ratio_to_avg,          names_d_subgroups_ratio_to_state_avg,
  names_d_subgroups_ratio_to_avg_friendly, names_d_subgroups_ratio_to_state_avg_friendly,
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  names_d_subgroups_count,  # no avg, no ratio, no pctiles, for counts
  
  names_d_subgroups_nh,         
  names_d_subgroups_nh_friendly,
  names_d_subgroups_nh_avg,         names_d_subgroups_nh_state_avg,
  names_d_subgroups_nh_avg_friendly,names_d_subgroups_nh_state_avg_friendly,
  names_d_subgroups_nh_pctile,      names_d_subgroups_nh_state_pctile, 
  names_d_subgroups_nh_pctile_friendly, names_d_subgroups_nh_state_pctile_friendly, 
  names_d_subgroups_nh_ratio_to_avg,          names_d_subgroups_nh_ratio_to_state_avg,
  names_d_subgroups_nh_ratio_to_avg_friendly, names_d_subgroups_nh_ratio_to_state_avg_friendly,
  # names_d_subgroups_nh_med,     names_d_subgroups_nh_state_med,
  names_d_subgroups_nh_count,  # no avg, no ratio, no pctiles, for counts
  
  names_d_subgroups_alone,         
  names_d_subgroups_alone_friendly,
  names_d_subgroups_alone_avg,         names_d_subgroups_alone_state_avg,
  names_d_subgroups_alone_avg_friendly,names_d_subgroups_alone_state_avg_friendly,
  names_d_subgroups_alone_pctile,      names_d_subgroups_alone_state_pctile, 
  names_d_subgroups_alone_pctile_friendly, names_d_subgroups_alone_state_pctile_friendly, 
  names_d_subgroups_alone_ratio_to_avg,          names_d_subgroups_alone_ratio_to_state_avg,
  names_d_subgroups_alone_ratio_to_avg_friendly, names_d_subgroups_alone_ratio_to_state_avg_friendly,
  # names_d_subgroups_alone_med,     names_d_subgroups_alone_state_med,
  names_d_subgroups_alone_count,  # no avg, no ratio, no pctiles, for counts
  
  ############################################ # 
  
  names_e,           
  names_e_friendly,
  names_e_avg,          names_e_state_avg, 
  names_e_avg_friendly, names_e_state_avg_friendly,
  names_e_pctile,       names_e_state_pctile, 
  names_e_pctile_friendly, names_e_state_pctile_friendly, # new
  names_e_ratio_to_avg,          names_e_ratio_to_state_avg,
  names_e_ratio_to_avg_friendly, names_e_ratio_to_state_avg_friendly,
  # names_e_med, names_e_state_med,  
  ############################################ # 
  
  names_ej,           names_ej_state, # RAW EJ SCORE
  names_ej_friendly,  names_ej_state_friendly, # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED
  
  names_ej_supp,          names_ej_supp_state, 
  names_ej_supp_friendly, names_ej_supp_state_friendly, 
  
  names_ej_pctile,          names_ej_state_pctile ,
  names_ej_pctile_friendly, names_ej_state_pctile_friendly,  # new
  
  names_ej_supp_pctile,          names_ej_supp_state_pctile,  ################################ # had been missing
  names_ej_supp_pctile_friendly, names_ej_supp_state_pctile_friendly, # new
  
  ############################################ # 
  
  # names_pctile,  # all US pctile indicators - this is not used though
  # names_state_pctile,  #  - this is not used though
  names_need_pctile,  # base indicators that need to be reported as US percentiles not just raw scores
  names_need_state_pctile,
  
  names_these,                # used in code, and was set up to contain only subgroups or _alone or _nh but only one of those types
  names_these_avg,
  names_these_state_avg,
  names_these_ratio_to_avg,
  names_these_ratio_to_state_avg,
  
  overwrite = TRUE
)

############################################################################## #

# just for conveniently referring to these in server code
notfound = setdiff(names_these, names(EJAM::usastats))
notfound_st = setdiff(names_these, names(EJAM::statestats))
notfound_bg = setdiff(names_these, names(EJAM::blockgroupstats))
if (length(notfound   ) > 0) {warning('some of names_these are not column names found in EJAM::usastats  ... ',        paste0(notfound,    collapse = ', '), '\n')} else {print('ok')}
if (length(notfound_st) > 0) {warning('some of names_these are not column names found in EJAM::statestats  ... ',      paste0(notfound_st, collapse = ', '), '\n')} else {print('ok')}
if (length(notfound_bg) > 0) {warning('some of names_these are not column names found in EJAM::blockgroupstats  ... ', paste0(notfound_bg, collapse = ', '), '\n')} else {print('ok')}
rm(notfound, notfound_st, notfound_bg)


# (first make sure all the Envt variables etc are actually in usastates dataset)

stop('make sure all the Demog and Envt variables etc are actually in the latest installed usastates dataset')
#   #   NOW BOTH TYPES ARE IN blockgroupstats and in the usastats and statestats pctile lookup tables - alone and nh types as well

# library(EJAM) # will replace the work above if have not resinstalled since that code script was run
# data(blockgroupstats, package = "EJAM")
# data("usastats",      package = "EJAM")
# data("statestats",    package = "EJAM")
# 
# library(data.table)
# if (subgroups_type == 'alone') {
# # replace nh versions with current version (e.g. alone)
#   stop('fix this or remove it')
# names(usastats)   <- gsub('pctnh', 'pct', names(usastats)  )  # this is badly written as one of sub nh is pctnhnhpia 
# names(statestats) <- gsub('pctnh', 'pct', names(statestats)  ) # this is badly written as ....
# 
# usethis::use_data(usastats,        overwrite = TRUE)
# usethis::use_data(statestats,      overwrite = TRUE)
# }
# setnames(blockgroupstats, names_d_subgroups_nh, names_d_subgroups)
# setnames(blockgroupstats, names_d_subgroups_nh_count , names_d_subgroups_count )
# 
# usethis::use_data(blockgroupstats, overwrite = TRUE)


